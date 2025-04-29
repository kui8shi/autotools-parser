//! Provides the higher level on-time analyses to improve parsing

use crate::ast::minimal::{
    Command, CommandWrapper, CompoundCommand, Condition, GuardBodyPair, Word, WordFragment,
};
use crate::ast::{Arithmetic, Parameter, ParameterSubstitution, PatternBodyPair, Redirect};
use crate::lexer::Lexer;
use crate::m4_macro::{M4Argument, M4Macro};
use crate::parse::MinimalParser;
use crate::token::Token;
use std::collections::{HashMap, HashSet};

/// Represents a node in the dependency graph
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Node {
    /// Index of the node in the original list
    id: usize,
    /// trailing comments
    comment: Option<String>,
    /// Range of line numbers where the body is effectively referenced.
    ranges: Vec<(usize, usize)>,
    /// Body of commands referenced by this node.
    cmd: Command<String>,
    /// ID of the parent node if the commands were flattened
    parent: Option<usize>,
    /// IDs of the children if the commands were flattened
    children: Option<Vec<usize>>,
    /// Variables defined by this command
    defines: HashMap<String, Vec<Guard>>,
    /// Variables used by this command
    uses: HashMap<String, Vec<Guard>>,
    /// Dependencies (indices of nodes this command depends on by variables)
    var_dependencies: HashSet<usize>,
    /// Commands that depend on this node
    var_dependents: HashSet<usize>,
}

/// Represents a condition
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Guard {
    /// doc
    Not(Box<Self>),
    /// doc
    Cond(Condition<Word<String, CommandWrapper<String>>, CommandWrapper<String>>),
    /// doc
    Match(
        Word<String, CommandWrapper<String>>,
        Vec<Word<String, CommandWrapper<String>>>,
    ),
}

/// Analyzer that constructs dependency graph by tracking variable usages
#[derive(Debug)]
pub struct DependencyAnalyzer {
    /// Original contents of analyzed script
    lines: Vec<String>,
    /// Nodes in the dependency graph
    nodes: Vec<Node>,
    /// Map of variable names to indices of commands that define them
    var_definitions: HashMap<String, Vec<(usize, Vec<Guard>)>>,
}

impl DependencyAnalyzer {
    /// Analyze commands and build the dependency graph
    pub fn new<S: AsRef<str>>(contents: S) -> Self {
        let lexer = Lexer::new(contents.as_ref().chars());
        let parser = MinimalParser::new(lexer);

        let mut commands = Vec::new();

        // Collect all complete commands from the parser
        for result in parser {
            if let Ok(cmd) = result {
                commands.push((cmd, None));
            }
        }

        // `commands` becomes a stack to freely manipulate the head
        commands.reverse();
        let flatten_threshold = 100;

        let mut s = Self {
            lines: contents.as_ref().lines().map(|s| s.to_string()).collect(),
            nodes: Self::flatten_commands_to_nodes(commands, flatten_threshold),
            var_definitions: HashMap::new(),
        };

        // collect all defined and used variables in a single pass
        for i in 0..s.nodes.len() {
            // Create a VariableAnalyzer to extract both defined and used variables
            let analyzer = VariableAnalyzer::new(&s.nodes[i].cmd);

            // Get defined variables
            s.nodes[i].defines = analyzer.defines.clone();

            // Update var_definitions map
            for (var, guards) in &analyzer.defines {
                s.var_definitions
                    .entry(var.clone())
                    .or_insert_with(Vec::new)
                    .push((i, guards.clone()));
            }

            // Get used variables
            s.nodes[i].uses = analyzer.uses;
        }

        // Create dependency edges based on variable usage
        for i in 0..s.nodes.len() {
            for var in s.nodes[i].uses.clone().keys() {
                if let Some(def_idx) = s.get_definition(var, i) {
                    s.nodes[i].var_dependencies.insert(def_idx);
                    s.nodes[def_idx].var_dependents.insert(i);
                }
            }
        }

        s
    }

    fn flatten_commands_to_nodes(
        mut commands: Vec<(CommandWrapper<String>, Option<usize>)>,
        flatten_threshold: usize,
    ) -> Vec<Node> {
        let mut nodes = Vec::new();
        let mut last = None;

        // 1st pass: flatten large commands.
        while let Some((c, parent)) = commands.pop() {
            let id = nodes.len();
            let do_flatten = c.range.is_some_and(|(a, b)| b - a > flatten_threshold);
            let mut ranges = c.range.map_or(vec![], |range| vec![range]);
            let cmd = if let Command::Compound(compound) = c.cmd {
                let compound = match compound {
                    CompoundCommand::Brace(body) if do_flatten => {
                        commands.extend(body.into_iter().rev().map(|c| (c, parent)));
                        continue;
                    }
                    CompoundCommand::For { var, words, body } if do_flatten => {
                        ranges = vec![
                            (c.range.unwrap().0, body.first().unwrap().range.unwrap().0),
                            (body.last().unwrap().range.unwrap().1, c.range.unwrap().1),
                        ];
                        commands.extend(body.into_iter().rev().map(|c| (c, Some(id))));
                        CompoundCommand::For {
                            var,
                            words,
                            body: Vec::new(),
                        }
                    }
                    CompoundCommand::Case { word, arms } if do_flatten => {
                        let mut flattened = Vec::new();
                        // FIXME; Dirty codes.
                        let whole_range = ranges.pop().unwrap();
                        let header_range = (
                            whole_range.0,
                            arms.first().unwrap().body.first().unwrap().range.unwrap().0 - 1,
                        );
                        let footer_range = (
                            arms.last().unwrap().body.last().unwrap().range.unwrap().1 + 1,
                            whole_range.1,
                        );
                        ranges.push(footer_range);
                        for arm in arms.into_iter().rev() {
                            flattened.push(PatternBodyPair {
                                patterns: arm.patterns,
                                body: Vec::new(),
                            });
                            let range = arm
                                .body
                                .first()
                                .map(|c| c.range.unwrap().0)
                                .zip(arm.body.last().map(|c| c.range.unwrap().1));
                            ranges.push((range.unwrap().0 - 1, range.unwrap().0));
                            let body = CommandWrapper {
                                comment: None,
                                range,
                                cmd: Command::Compound(CompoundCommand::Brace(arm.body)),
                            };
                            commands.push((body, Some(id)));
                        }
                        ranges.push(header_range);
                        ranges.reverse();
                        CompoundCommand::Case {
                            word,
                            arms: flattened,
                        }
                    }
                    CompoundCommand::If {
                        conditionals,
                        else_branch,
                    } if do_flatten => {
                        let mut flattened = Vec::new();
                        for GuardBodyPair { condition, body } in conditionals.into_iter().rev() {
                            flattened.push(GuardBodyPair {
                                condition,
                                body: Vec::new(),
                            });
                            let range = body
                                .first()
                                .map(|c| c.range.unwrap().0)
                                .zip(body.last().map(|c| c.range.unwrap().1));
                            let body = CommandWrapper {
                                comment: None,
                                range,
                                cmd: Command::Compound(CompoundCommand::Brace(body)),
                            };
                            commands.push((body, Some(id)));
                        }
                        CompoundCommand::If {
                            conditionals: flattened,
                            else_branch, // TODO: may also flatten else_branch
                        }
                    }
                    _ => compound,
                };
                Command::Compound(compound)
            } else {
                c.cmd
            };
            let next = ranges.first().map(|r| r.0);
            if let (Some(p), Some(n)) = (last, next) {
                if !(p <= n) {
                    dbg!(&ranges, id, last);
                    panic!();
                }
            }
            last = next;
            nodes.push(Node {
                id,
                comment: c.comment,
                ranges,
                cmd,
                parent,
                children: None,                   // deferred initialization
                defines: HashMap::new(),          // deferred initialization
                uses: HashMap::new(),             // deferred initialization
                var_dependencies: HashSet::new(), // deferred initialization
                var_dependents: HashSet::new(),   // deferred initialization
            })
        }

        // 2nd pass: initialize children according to parent
        for i in 0..nodes.len() {
            let node = nodes.get(i).unwrap();
            if let Some(j) = node.parent {
                let parent = nodes.get_mut(j).unwrap();
                parent.children.get_or_insert(Vec::new()).push(i);
            }
        }

        nodes
    }

    /// Get command that define a variable before
    pub fn get_definition(&self, var_name: &str, cmd_index: usize) -> Option<usize> {
        if let Some(node) = self.nodes.get(cmd_index) {
            if let Some(user_guards) = node.uses.get(var_name) {
                return self.var_definitions.get(var_name).and_then(|v| {
                    v.iter()
                        .rev()
                        .filter_map(|(i, guards)| {
                            // dumb heulistic to compare two guard conditions
                            (*i < cmd_index && guards.len() <= user_guards.len()).then_some(i)
                        })
                        .next()
                        .cloned()
                });
            }
        }
        None
    }

    /// Get all variables defined by a command
    pub fn get_defined_variables(&self, cmd_index: usize) -> Option<HashSet<String>> {
        self.nodes
            .get(cmd_index)
            .map(|node| node.defines.keys().cloned().collect())
    }

    /// Get all variables used by a command
    pub fn get_used_variables(&self, cmd_index: usize) -> Option<HashSet<String>> {
        self.nodes
            .get(cmd_index)
            .map(|node| node.uses.keys().cloned().collect())
    }

    /// Get all commands this command depends on
    pub fn get_dependencies(&self, cmd_index: usize) -> Option<HashSet<usize>> {
        self.nodes.get(cmd_index).map(|node| {
            let mut deps = node.var_dependencies.clone();
            if let Some(parent) = node.parent {
                deps.insert(parent);
            }
            deps
        })
    }

    /// Get all commands that depend on this command
    pub fn get_dependents(&self, cmd_index: usize) -> Option<HashSet<usize>> {
        self.nodes.get(cmd_index).map(|node| {
            let mut deps = node.var_dependencies.clone();
            if let Some(children) = &node.children {
                deps.extend(children.iter().copied());
            }
            deps
        })
    }

    /// Get the total number of commands analyzed
    pub fn command_count(&self) -> usize {
        self.nodes.len()
    }

    /// Get a range of the command at the specified index
    pub fn get_ranges(&self, index: usize) -> Option<&[(usize, usize)]> {
        self.nodes.get(index).map(|n| n.ranges.as_ref())
    }
    /// Get a reference to the command at the specified index
    pub fn get_command(&self, index: usize) -> Option<&Command<String>> {
        self.nodes.get(index).map(|n| &n.cmd)
    }

    /// Get all commands that define or use a variable
    pub fn find_commands_with_variable(&self, var_name: &str) -> HashSet<usize> {
        let mut result = HashSet::new();

        // Add commands that define the variable
        if let Some(def_indices) = self.var_definitions.get(var_name) {
            result.extend(def_indices.iter().map(|(i, _)| i));
        }

        // Add commands that use the variable
        for (i, node) in self.nodes.iter().enumerate() {
            if node.uses.contains_key(var_name) {
                result.insert(i);
            }
        }

        result
    }

    /// Get original content of commands in specified node
    pub fn get_content(&self, node_id: usize) -> Option<Vec<String>> {
        self.nodes.get(node_id).map(|node| {
            node.ranges
                .iter()
                .map(|&(a, b)| {
                    // FIXME: Poorly working with this lines extraction logic.
                    // Will fix it later.
                    self.lines[a - 1..b - 1]
                        .iter()
                        .filter(|s| !s.is_empty())
                        .map(|s| s.as_str())
                        .collect::<Vec<_>>()
                        .join("\n")
                })
                .collect()
        })
    }
}

#[derive(Debug)]
struct VariableAnalyzer {
    pub defines: HashMap<String, Vec<Guard>>,
    pub uses: HashMap<String, Vec<Guard>>,
    conds: Vec<Guard>,
}

impl VariableAnalyzer {
    /// Analyze a command to extract defined/used variables
    pub fn new(cmd: &Command<String>) -> Self {
        let mut s = Self {
            defines: HashMap::new(),
            uses: HashMap::new(),
            conds: Vec::new(),
        };
        s.analyze_variables(cmd);
        s
    }

    /// Extract variables from a word
    fn extract_variables_from_word(&mut self, word: &Word<String, CommandWrapper<String>>) {
        match word {
            Word::Concat(fragments) => {
                for fragment in fragments {
                    self.extract_variables_from_fragment(fragment);
                }
            }
            Word::Single(fragment) => {
                self.extract_variables_from_fragment(fragment);
            }
            Word::Empty => {}
        }
    }

    /// Extract variables from a word fragment
    fn extract_variables_from_fragment(
        &mut self,
        fragment: &WordFragment<
            String,
            Word<String, CommandWrapper<String>>,
            CommandWrapper<String>,
        >,
    ) {
        match fragment {
            WordFragment::Param(Parameter::Var(name)) => {
                // Variable usage
                self.uses.insert(name.clone(), self.conds.clone());
            }
            WordFragment::DoubleQuoted(fragments) => {
                for fragment in fragments {
                    self.extract_variables_from_fragment(fragment);
                }
            }
            WordFragment::Macro(m4_macro) => {
                self.extract_variables_from_macro(m4_macro);
            }
            WordFragment::Subst(subst) => {
                // Parameter substitution
                match subst.as_ref() {
                    ParameterSubstitution::Command(cmds) => {
                        for c in cmds {
                            self.analyze_variables(&c.cmd);
                        }
                    }
                    ParameterSubstitution::Arith(Some(arith)) => {
                        self.extract_variables_from_arithmetic(arith);
                    }
                    ParameterSubstitution::Len(param) => {
                        self.extract_variables_from_parameter(param);
                    }
                    ParameterSubstitution::Default(_, param, word)
                    | ParameterSubstitution::Assign(_, param, word)
                    | ParameterSubstitution::Error(_, param, word)
                    | ParameterSubstitution::Alternative(_, param, word)
                    | ParameterSubstitution::RemoveSmallestSuffix(param, word)
                    | ParameterSubstitution::RemoveLargestSuffix(param, word)
                    | ParameterSubstitution::RemoveSmallestPrefix(param, word)
                    | ParameterSubstitution::RemoveLargestPrefix(param, word) => {
                        self.extract_variables_from_parameter(param);
                        if let Some(word) = word {
                            self.extract_variables_from_word(word);
                        }
                    }
                    _ => {}
                }
            }
            _ => {}
        }
    }

    /// Extract variables from a condition
    fn extract_variables_from_condition(
        &mut self,
        condition: &Condition<Word<String, CommandWrapper<String>>, CommandWrapper<String>>,
    ) {
        match condition {
            Condition::Cond(operator) => match operator {
                crate::ast::minimal::Operator::Eq(w1, w2)
                | crate::ast::minimal::Operator::Neq(w1, w2)
                | crate::ast::minimal::Operator::Ge(w1, w2)
                | crate::ast::minimal::Operator::Gt(w1, w2)
                | crate::ast::minimal::Operator::Le(w1, w2)
                | crate::ast::minimal::Operator::Lt(w1, w2) => {
                    self.extract_variables_from_word(w1);
                    self.extract_variables_from_word(w2);
                }
                crate::ast::minimal::Operator::Empty(w)
                | crate::ast::minimal::Operator::NonEmpty(w)
                | crate::ast::minimal::Operator::Dir(w)
                | crate::ast::minimal::Operator::File(w)
                | crate::ast::minimal::Operator::NoExists(w) => {
                    self.extract_variables_from_word(w);
                }
            },
            Condition::And(c1, c2) | Condition::Or(c1, c2) => {
                self.extract_variables_from_condition(c1);
                self.extract_variables_from_condition(c2);
            }
            Condition::Eval(commands) => {
                for cmd in commands {
                    self.analyze_variables(&cmd.cmd);
                }
            }
            Condition::ReturnZero(cmd) => {
                self.analyze_variables(&cmd.cmd);
            }
        }
    }

    /// Extract variables from an arithmetic expression
    fn extract_variables_from_arithmetic(&mut self, arith: &Arithmetic<String>) {
        match arith {
            Arithmetic::Var(name)
            | Arithmetic::PostIncr(name)
            | Arithmetic::PostDecr(name)
            | Arithmetic::PreIncr(name)
            | Arithmetic::PreDecr(name) => {
                self.uses.insert(name.clone(), self.conds.clone());
            }
            Arithmetic::UnaryPlus(x)
            | Arithmetic::UnaryMinus(x)
            | Arithmetic::LogicalNot(x)
            | Arithmetic::BitwiseNot(x) => {
                self.extract_variables_from_arithmetic(x);
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
                self.extract_variables_from_arithmetic(x);
                self.extract_variables_from_arithmetic(y);
            }
            Arithmetic::Ternary(x, y, z) => {
                self.extract_variables_from_arithmetic(x);
                self.extract_variables_from_arithmetic(y);
                self.extract_variables_from_arithmetic(z);
            }
            Arithmetic::Sequence(seq) => {
                for arith in seq {
                    self.extract_variables_from_arithmetic(arith);
                }
            }
            // TODO: Arithmetic::Assign should be counted as defined varaibles
            // Arithmetic::Assign(name , x),
            _ => {}
        }
    }

    /// Extract variables from a parameter
    fn extract_variables_from_parameter(&mut self, param: &Parameter<String>) {
        if let Parameter::Var(name) = param {
            self.uses.insert(name.clone(), self.conds.clone());
        }
    }

    /// Extract variables from a macro
    fn extract_variables_from_macro(
        &mut self,
        m4_macro: &M4Macro<Word<String, CommandWrapper<String>>, CommandWrapper<String>>,
    ) {
        // Process arguments
        for arg in &m4_macro.args {
            match arg {
                M4Argument::Word(word) => {
                    self.extract_variables_from_word(word);
                }
                M4Argument::Array(words) => {
                    for word in words {
                        self.extract_variables_from_word(word);
                    }
                }
                M4Argument::Commands(commands) => {
                    for cmd in commands {
                        self.analyze_variables(&cmd.cmd);
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
                        self.uses.insert(var.0.clone(), self.conds.clone());
                    }
                    if var.is_defined() {
                        self.defines.insert(var.0.clone(), self.conds.clone());
                    }
                }
            }
        }
    }

    /// Analyze a command to extract both defined and used variables in a single pass
    fn analyze_variables(&mut self, cmd: &Command<String>) {
        match cmd {
            Command::Assignment(var_name, word) => {
                // Variable definition
                self.defines.insert(var_name.clone(), self.conds.clone());

                // Variables used in the assignment value
                self.extract_variables_from_word(word);
            }
            Command::Compound(compound) => {
                match compound {
                    CompoundCommand::Macro(m4_macro) => {
                        // Process macro effects and arguments
                        self.extract_variables_from_macro(m4_macro);
                    }
                    CompoundCommand::Brace(cmds) | CompoundCommand::Subshell(cmds) => {
                        for c in cmds {
                            self.analyze_variables(&c.cmd);
                        }
                    }
                    CompoundCommand::While(guard_body) | CompoundCommand::Until(guard_body) => {
                        // Process condition
                        self.extract_variables_from_condition(&guard_body.condition);

                        // Process body with condition context
                        self.conds.push(Guard::Cond(guard_body.condition.clone()));
                        for c in &guard_body.body {
                            self.analyze_variables(&c.cmd);
                        }
                        self.conds.pop();
                    }
                    CompoundCommand::If {
                        conditionals,
                        else_branch,
                    } => {
                        let mut last_cond = None;

                        for guard_body in conditionals {
                            // Extract variables from condition
                            self.extract_variables_from_condition(&guard_body.condition);

                            // Process body with condition context
                            self.conds.push(Guard::Cond(guard_body.condition.clone()));
                            for c in &guard_body.body {
                                self.analyze_variables(&c.cmd);
                            }
                            last_cond = self.conds.pop();
                        }

                        // Process else branch with negated condition context
                        if let Some(last) = last_cond {
                            self.conds.push(Guard::Not(Box::new(last)));
                            for c in else_branch {
                                self.analyze_variables(&c.cmd);
                            }
                            self.conds.pop();
                        } else {
                            for c in else_branch {
                                self.analyze_variables(&c.cmd);
                            }
                        }
                    }
                    CompoundCommand::For { var, words, body } => {
                        // Loop variable is defined
                        self.defines.insert(var.clone(), self.conds.clone());

                        // Variables in the list
                        for word in words {
                            self.extract_variables_from_word(word);
                        }

                        // Process body
                        for c in body {
                            self.analyze_variables(&c.cmd);
                        }
                    }
                    CompoundCommand::Case { word, arms } => {
                        // Extract variables from the case expression
                        self.extract_variables_from_word(word);

                        for arm in arms {
                            // Extract variables from patterns
                            for pattern in &arm.patterns {
                                self.extract_variables_from_word(pattern);
                            }

                            // Process body with pattern match context
                            self.conds
                                .push(Guard::Match(word.clone(), arm.patterns.clone()));
                            for c in &arm.body {
                                self.analyze_variables(&c.cmd);
                            }
                            self.conds.pop();
                        }
                    }
                    CompoundCommand::And(cond, c) | CompoundCommand::Or(cond, c) => {
                        // Extract variables from condition
                        self.extract_variables_from_condition(cond);

                        // Process command with condition context
                        self.conds.push(Guard::Cond(cond.clone()));
                        self.analyze_variables(&c.cmd);
                        self.conds.pop();
                    }
                    CompoundCommand::Pipe(_, cmds) => {
                        for c in cmds {
                            self.analyze_variables(&c.cmd);
                        }
                    }
                    CompoundCommand::Redirect(cmd, redirects) => {
                        // Process the command
                        self.analyze_variables(&cmd.cmd);

                        // Extract variables from redirects
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
                                    self.extract_variables_from_word(word);
                                }
                            }
                        }
                    }
                    CompoundCommand::Background(cmd) => {
                        self.analyze_variables(&cmd.cmd);
                    }
                    CompoundCommand::FunctionDef { name: _, body } => {
                        self.analyze_variables(&body.cmd);
                    }
                }
            }
            Command::Cmd(words) => {
                // Extract variables from command words
                for word in words {
                    self.extract_variables_from_word(word);
                }
            }
        }
    }
}
