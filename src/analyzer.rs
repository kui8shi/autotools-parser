//! Provides the higher level on-time analyses to improve parsing

use crate::ast::minimal::{
    Command, CommandWrapper, CompoundCommand, Condition, GuardBodyPair, Word, WordFragment,
};
use crate::ast::{Arithmetic, Parameter, ParameterSubstitution, Redirect};
use crate::m4_macro::{M4Argument, M4Macro};
use crate::parse::MinimalParser;
use crate::token::Token;
use std::collections::{HashMap, HashSet};

/// Represents a node in the dependency graph
#[derive(Debug, PartialEq, Eq)]
struct Node {
    /// Index of the command in the original list
    index: usize,
    /// Variables defined by this command
    defines: HashSet<String>,
    /// Variables used by this command
    uses: HashSet<String>,
    /// Dependencies (indices of commands this command depends on)
    dependencies: HashSet<usize>,
    /// Commands that depend on this command
    dependents: HashSet<usize>,
}

/// Analyzer that constructs dependency graph by tracking variable usages
#[derive(Debug)]
pub struct DependencyAnalyzer {
    /// Commands parsed from the input
    commands: Vec<CommandWrapper<String>>,
    /// Nodes in the dependency graph
    nodes: Vec<Node>,
    /// Map of variable names to indices of commands that define them
    var_definitions: HashMap<String, HashSet<usize>>,
}

impl DependencyAnalyzer {
    /// Create a new analyzer from a MinimalParser
    pub fn new<I>(parser: MinimalParser<I>) -> Self
    where
        I: Iterator<Item = Token>,
    {
        let mut commands = Vec::new();

        // Collect all complete commands from the parser
        for result in parser {
            if let Ok(cmd) = result {
                commands.push(cmd);
            }
        }

        Self {
            commands,
            nodes: Vec::new(),
            var_definitions: HashMap::new(),
        }
    }

    /// Analyze commands and build the dependency graph
    pub fn analyze(&mut self) {
        // Initialize nodes for each command
        for (i, _) in self.commands.iter().enumerate() {
            self.nodes.push(Node {
                index: i,
                defines: HashSet::new(),
                uses: HashSet::new(),
                dependencies: HashSet::new(),
                dependents: HashSet::new(),
            });
        }

        // First pass: collect all defined variables
        for (i, cmd) in self.commands.iter().enumerate() {
            let defined_vars = self.extract_defined_variables(&cmd.cmd);
            self.nodes[i].defines = defined_vars.clone();

            // Update var_definitions map
            for var in defined_vars {
                self.var_definitions
                    .entry(var)
                    .or_insert_with(HashSet::new)
                    .insert(i);
            }
        }

        // Second pass: collect all used variables and build dependency edges
        for (i, cmd) in self.commands.iter().enumerate() {
            let used_vars = self.extract_used_variables(&cmd.cmd);
            self.nodes[i].uses = used_vars.clone();

            // Create dependency edges
            for var in used_vars {
                if let Some(def_indices) = self.var_definitions.get(&var) {
                    for &def_idx in def_indices {
                        if def_idx != i {
                            // Avoid self-dependency
                            self.nodes[i].dependencies.insert(def_idx);
                            self.nodes[def_idx].dependents.insert(i);
                        }
                    }
                }
            }
        }
    }

    /// Extract variables defined by a command
    fn extract_defined_variables(&self, cmd: &Command<String>) -> HashSet<String> {
        let mut vars = HashSet::new();

        match cmd {
            Command::Assignment(var_name, _) => {
                vars.insert(var_name.clone());
            }
            Command::Compound(compound) => {
                // Extract variables from compound commands like functions
                match compound {
                    CompoundCommand::Macro(m4_macro) => {
                        if let Some(effects) = &m4_macro.effects {
                            if let Some(shell_vars) = &effects.shell_vars {
                                for var in shell_vars {
                                    if var.is_defined() {
                                        vars.insert(var.0.clone());
                                    }
                                }
                            }
                        }
                        for arg in &m4_macro.args {
                            if let M4Argument::Commands(cmds) = arg {
                                for c in cmds {
                                    let defined = self.extract_defined_variables(&c.cmd);
                                    vars.extend(defined);
                                }
                            }
                        }
                    }
                    CompoundCommand::Brace(cmds) | CompoundCommand::Subshell(cmds) => {
                        for c in cmds {
                            let defined = self.extract_defined_variables(&c.cmd);
                            vars.extend(defined);
                        }
                    }
                    CompoundCommand::While(guard_body) | CompoundCommand::Until(guard_body) => {
                        for c in &guard_body.body {
                            let defined = self.extract_defined_variables(&c.cmd);
                            vars.extend(defined);
                        }
                    }
                    CompoundCommand::If {
                        conditionals,
                        else_branch,
                    } => {
                        for guard_body in conditionals {
                            for c in &guard_body.body {
                                let defined = self.extract_defined_variables(&c.cmd);
                                vars.extend(defined);
                            }
                        }
                        for c in else_branch {
                            let defined = self.extract_defined_variables(&c.cmd);
                            vars.extend(defined);
                        }
                    }
                    CompoundCommand::For {
                        var: _,
                        words: _,
                        body,
                    } => {
                        for c in body {
                            let defined = self.extract_defined_variables(&c.cmd);
                            vars.extend(defined);
                        }
                    }
                    CompoundCommand::Case { word: _, arms } => {
                        for arm in arms {
                            for c in &arm.body {
                                let defined = self.extract_defined_variables(&c.cmd);
                                vars.extend(defined);
                            }
                        }
                    }
                    CompoundCommand::And(_, c) | CompoundCommand::Or(_, c) => {
                        let defined = self.extract_defined_variables(&c.cmd);
                        vars.extend(defined);
                    }
                    CompoundCommand::Pipe(_, cmds) => {
                        for c in cmds {
                            let defined = self.extract_defined_variables(&c.cmd);
                            vars.extend(defined);
                        }
                    }
                    _ => {}
                }
            }
            _ => {}
        }

        vars
    }

    /// Extract variables used by a command
    fn extract_used_variables(&self, cmd: &Command<String>) -> HashSet<String> {
        let mut vars = HashSet::new();

        match cmd {
            Command::Assignment(_, word) => {
                self.extract_variables_from_word(word, &mut vars);
            }
            Command::Compound(compound) => {
                self.extract_variables_from_compound(compound, &mut vars);
            }
            Command::Cmd(words) => {
                for word in words {
                    self.extract_variables_from_word(word, &mut vars);
                }
            }
        }

        vars
    }

    /// Extract variables used in an arithmetic expression
    fn extract_variables_from_arithmetic(
        &self,
        arith: &Arithmetic<String>,
        vars: &mut HashSet<String>,
    ) {
        match arith {
            Arithmetic::Var(name)
            | Arithmetic::PostIncr(name)
            | Arithmetic::PostDecr(name)
            | Arithmetic::PreIncr(name)
            | Arithmetic::PreDecr(name) => {
                vars.insert(name.clone());
            }
            Arithmetic::UnaryPlus(x)
            | Arithmetic::UnaryMinus(x)
            | Arithmetic::LogicalNot(x)
            | Arithmetic::BitwiseNot(x) => {
                self.extract_variables_from_arithmetic(x, vars);
            }
            Arithmetic::Pow(x, y)
            | Arithmetic::Mult(x, y)
            | Arithmetic::Div(x, y)
            | Arithmetic::Modulo(x, y)
            | Arithmetic::Add(x, y)
            | Arithmetic::Sub(x, y)
            | Arithmetic::ShiftLeft(x, y)
            | Arithmetic::ShiftRight(x, y)
            | Arithmetic::Less(x, y)
            | Arithmetic::LessEq(x, y)
            | Arithmetic::Great(x, y)
            | Arithmetic::GreatEq(x, y)
            | Arithmetic::Eq(x, y)
            | Arithmetic::NotEq(x, y)
            | Arithmetic::BitwiseAnd(x, y)
            | Arithmetic::BitwiseXor(x, y)
            | Arithmetic::BitwiseOr(x, y)
            | Arithmetic::LogicalAnd(x, y)
            | Arithmetic::LogicalOr(x, y) => {
                self.extract_variables_from_arithmetic(x, vars);
                self.extract_variables_from_arithmetic(y, vars);
            }
            Arithmetic::Ternary(x, y, z) => {
                self.extract_variables_from_arithmetic(x, vars);
                self.extract_variables_from_arithmetic(y, vars);
                self.extract_variables_from_arithmetic(z, vars);
            }
            Arithmetic::Sequence(seq) => {
                for arith in seq {
                    self.extract_variables_from_arithmetic(arith, vars);
                }
            }
            // TODO: Arithmetic::Assign should be counted as defined varaibles
            // Arithmetic::Assign(name , x),
            _ => {}
        }
    }

    /// Extract variables used in a parameter
    fn extract_variables_from_parameter(
        &self,
        param: &Parameter<String>,
        vars: &mut HashSet<String>,
    ) {
        if let Parameter::Var(name) = param {
            vars.insert(name.clone());
        }
    }

    /// Extract variables used in a word
    fn extract_variables_from_word(
        &self,
        word: &Word<String, CommandWrapper<String>>,
        vars: &mut HashSet<String>,
    ) {
        match word {
            Word::Concat(fragments) => {
                for fragment in fragments {
                    self.extract_variables_from_fragment(fragment, vars);
                }
            }
            Word::Single(fragment) => {
                self.extract_variables_from_fragment(fragment, vars);
            }
            Word::Empty => {}
        }
    }

    /// Extract variables from a word fragment
    fn extract_variables_from_fragment(
        &self,
        fragment: &WordFragment<
            String,
            Word<String, CommandWrapper<String>>,
            CommandWrapper<String>,
        >,
        vars: &mut HashSet<String>,
    ) {
        match fragment {
            WordFragment::Param(Parameter::Var(name)) => {
                // Variable usage
                vars.insert(name.clone());
            }
            WordFragment::DoubleQuoted(fragments) => {
                for fragment in fragments {
                    self.extract_variables_from_fragment(fragment, vars);
                }
            }
            WordFragment::Macro(m4_macro) => {
                self.extract_variables_from_macro(m4_macro, vars);
            }
            WordFragment::Subst(subst) => {
                // Parameter substitution
                match subst.as_ref() {
                    ParameterSubstitution::Command(cmds) => {
                        for c in cmds {
                            let used = self.extract_used_variables(&c.cmd);
                            vars.extend(used);
                        }
                    }
                    ParameterSubstitution::Arith(Some(arith)) => {
                        self.extract_variables_from_arithmetic(&arith, vars);
                    }
                    ParameterSubstitution::Len(param) => {
                        self.extract_variables_from_parameter(&param, vars);
                    }
                    ParameterSubstitution::Default(_, param, word)
                    | ParameterSubstitution::Assign(_, param, word)
                    | ParameterSubstitution::Error(_, param, word)
                    | ParameterSubstitution::Alternative(_, param, word)
                    | ParameterSubstitution::RemoveSmallestSuffix(param, word)
                    | ParameterSubstitution::RemoveLargestSuffix(param, word)
                    | ParameterSubstitution::RemoveSmallestPrefix(param, word)
                    | ParameterSubstitution::RemoveLargestPrefix(param, word) => {
                        self.extract_variables_from_parameter(&param, vars);
                        if let Some(word) = word {
                            self.extract_variables_from_word(&word, vars);
                        }
                    }
                    _ => {}
                }
            }
            _ => {}
        }
    }

    /// Extract variables from a compound command
    fn extract_variables_from_compound(
        &self,
        compound: &CompoundCommand<Word<String, CommandWrapper<String>>, CommandWrapper<String>>,
        vars: &mut HashSet<String>,
    ) {
        match compound {
            CompoundCommand::Brace(commands) | CompoundCommand::Subshell(commands) => {
                for cmd in commands {
                    let used = self.extract_used_variables(&cmd.cmd);
                    vars.extend(used);
                }
            }
            CompoundCommand::While(guard_body) | CompoundCommand::Until(guard_body) => {
                self.extract_variables_from_guard_body(guard_body, vars);
            }
            CompoundCommand::If {
                conditionals,
                else_branch,
            } => {
                for cond in conditionals {
                    self.extract_variables_from_guard_body(cond, vars);
                }

                for cmd in else_branch {
                    let used = self.extract_used_variables(&cmd.cmd);
                    vars.extend(used);
                }
            }
            CompoundCommand::For {
                var: _,
                words,
                body,
            } => {
                for word in words {
                    self.extract_variables_from_word(word, vars);
                }

                for cmd in body {
                    let used = self.extract_used_variables(&cmd.cmd);
                    vars.extend(used);
                }
            }
            CompoundCommand::Case { word, arms } => {
                self.extract_variables_from_word(word, vars);

                for arm in arms {
                    for pattern in &arm.patterns {
                        self.extract_variables_from_word(pattern, vars);
                    }

                    for cmd in &arm.body {
                        let used = self.extract_used_variables(&cmd.cmd);
                        vars.extend(used);
                    }
                }
            }
            CompoundCommand::And(condition, cmd) | CompoundCommand::Or(condition, cmd) => {
                self.extract_variables_from_condition(condition, vars);
                let used = self.extract_used_variables(&cmd.cmd);
                vars.extend(used);
            }
            CompoundCommand::Pipe(_, commands) => {
                for cmd in commands {
                    let used = self.extract_used_variables(&cmd.cmd);
                    vars.extend(used);
                }
            }
            CompoundCommand::Redirect(cmd, redirects) => {
                let used = self.extract_used_variables(&cmd.cmd);
                vars.extend(used);

                for redirect in redirects {
                    match redirect {
                        Redirect::Read(_, word)
                        | Redirect::Write(_, word)
                        | Redirect::ReadWrite(_, word)
                        | Redirect::Append(_, word)
                        | Redirect::Clobber(_, word)
                        | Redirect::Heredoc(_, word)
                        | Redirect::DupRead(_, word)
                        | Redirect::DupWrite(_, word) => {
                            self.extract_variables_from_word(word, vars);
                        }
                    }
                }
            }
            CompoundCommand::Background(cmd) => {
                let used = self.extract_used_variables(&cmd.cmd);
                vars.extend(used);
            }
            CompoundCommand::FunctionDef { name: _, body } => {
                let used = self.extract_used_variables(&body.cmd);
                vars.extend(used);
            }
            CompoundCommand::Macro(m4_macro) => {
                self.extract_variables_from_macro(m4_macro, vars);
            }
        }
    }

    /// Extract variables from a guard-body pair
    fn extract_variables_from_guard_body(
        &self,
        guard_body: &GuardBodyPair<Word<String, CommandWrapper<String>>, CommandWrapper<String>>,
        vars: &mut HashSet<String>,
    ) {
        self.extract_variables_from_condition(&guard_body.condition, vars);

        for cmd in &guard_body.body {
            let used = self.extract_used_variables(&cmd.cmd);
            vars.extend(used);
        }
    }

    /// Extract variables from a condition
    fn extract_variables_from_condition(
        &self,
        condition: &Condition<Word<String, CommandWrapper<String>>, CommandWrapper<String>>,
        vars: &mut HashSet<String>,
    ) {
        match condition {
            Condition::Cond(operator) => match operator {
                crate::ast::minimal::Operator::Eq(w1, w2)
                | crate::ast::minimal::Operator::Neq(w1, w2)
                | crate::ast::minimal::Operator::Ge(w1, w2)
                | crate::ast::minimal::Operator::Gt(w1, w2)
                | crate::ast::minimal::Operator::Le(w1, w2)
                | crate::ast::minimal::Operator::Lt(w1, w2) => {
                    self.extract_variables_from_word(w1, vars);
                    self.extract_variables_from_word(w2, vars);
                }
                crate::ast::minimal::Operator::Empty(w)
                | crate::ast::minimal::Operator::NonEmpty(w)
                | crate::ast::minimal::Operator::Dir(w)
                | crate::ast::minimal::Operator::File(w)
                | crate::ast::minimal::Operator::NoExists(w) => {
                    self.extract_variables_from_word(w, vars);
                }
            },
            Condition::And(c1, c2) | Condition::Or(c1, c2) => {
                self.extract_variables_from_condition(c1, vars);
                self.extract_variables_from_condition(c2, vars);
            }
            Condition::Eval(commands) => {
                for cmd in commands {
                    let used = self.extract_used_variables(&cmd.cmd);
                    vars.extend(used);
                }
            }
            Condition::ReturnZero(cmd) => {
                let used = self.extract_used_variables(&cmd.cmd);
                vars.extend(used);
            }
        }
    }

    /// Extract variables from a macro
    fn extract_variables_from_macro(
        &self,
        m4_macro: &M4Macro<Word<String, CommandWrapper<String>>, CommandWrapper<String>>,
        vars: &mut HashSet<String>,
    ) {
        // Process arguments
        for arg in &m4_macro.args {
            match arg {
                M4Argument::Word(word) => {
                    self.extract_variables_from_word(word, vars);
                }
                M4Argument::Array(words) => {
                    for word in words {
                        self.extract_variables_from_word(word, vars);
                    }
                }
                M4Argument::Commands(commands) => {
                    for cmd in commands {
                        let used = self.extract_used_variables(&cmd.cmd);
                        vars.extend(used);
                    }
                }
                _ => {}
            }
        }

        // Process side effects
        if let Some(effects) = &m4_macro.effects {
            if let Some(shell_vars) = &effects.shell_vars {
                for var in shell_vars {
                    if var.is_used() {
                        vars.insert(var.0.clone());
                    }
                }
            }
        }
    }

    /// Get commands that define a variable
    pub fn get_definitions(&self, var_name: &str) -> Option<&HashSet<usize>> {
        self.var_definitions.get(var_name)
    }

    /// Get all variables defined by a command
    pub fn get_defined_variables(&self, cmd_index: usize) -> Option<&HashSet<String>> {
        self.nodes.get(cmd_index).map(|node| &node.defines)
    }

    /// Get all variables used by a command
    pub fn get_used_variables(&self, cmd_index: usize) -> Option<&HashSet<String>> {
        self.nodes.get(cmd_index).map(|node| &node.uses)
    }

    /// Get all commands this command depends on
    pub fn get_dependencies(&self, cmd_index: usize) -> Option<&HashSet<usize>> {
        self.nodes.get(cmd_index).map(|node| &node.dependencies)
    }

    /// Get all commands that depend on this command
    pub fn get_dependents(&self, cmd_index: usize) -> Option<&HashSet<usize>> {
        self.nodes.get(cmd_index).map(|node| &node.dependents)
    }

    /// Get the total number of commands analyzed
    pub fn command_count(&self) -> usize {
        self.commands.len()
    }

    /// Get a reference to the command at the specified index
    pub fn get_command(&self, index: usize) -> Option<&CommandWrapper<String>> {
        self.commands.get(index)
    }

    /// Get all commands that define or use a variable
    pub fn find_commands_with_variable(&self, var_name: &str) -> HashSet<usize> {
        let mut result = HashSet::new();

        // Add commands that define the variable
        if let Some(def_indices) = self.var_definitions.get(var_name) {
            result.extend(def_indices);
        }

        // Add commands that use the variable
        for (i, node) in self.nodes.iter().enumerate() {
            if node.uses.contains(var_name) {
                result.insert(i);
            }
        }

        result
    }
}

