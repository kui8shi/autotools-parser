//! Defines structs that represent test commands parsed in detail

/// Operators used to compare words or check file properties.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Operator<W> {
    /// Equality operator (==) between two words.
    Eq(W, W),
    /// Inequality operator (!=) between two words.
    Neq(W, W),
    /// Greater than or equal operator (>=) between two words.
    Ge(W, W),
    /// Greater than operator (>) between two words.
    Gt(W, W),
    /// Less than or equal operator (<=) between two words.
    Le(W, W),
    /// Less than operator (<) between two words.
    Lt(W, W),
    /// Checks if the given word is empty.
    Empty(W),
    /// Checks if the given word is non-empty.
    NonEmpty(W),
    /// Checks if the given word represents an existing directory.
    Dir(W),
    /// Checks if the given word represents an existing file.
    File(W),
    /// Checks if the given word represents a non-existing path.
    NoExists(W),
}

impl<W> Operator<W> {
    /// negate the operator
    pub fn flip(self) -> Self {
        use Operator::*;
        match self {
            Eq(lhs, rhs) => Neq(lhs, rhs),
            Neq(lhs, rhs) => Eq(lhs, rhs),
            Ge(lhs, rhs) => Lt(lhs, rhs),
            Gt(lhs, rhs) => Le(lhs, rhs),
            Le(lhs, rhs) => Gt(lhs, rhs),
            Lt(lhs, rhs) => Ge(lhs, rhs),
            Empty(x) => NonEmpty(x),
            NonEmpty(x) => Empty(x),
            Dir(x) => NoExists(x),
            File(x) => NoExists(x),
            NoExists(x) => File(x),
        }
    }
}

/// Represents a condition for control flow, which can be a single operator-based
/// condition, a logical combination of conditions, or an evaluated word.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Condition<C, W> {
    /// A single condition based on an operator.
    Cond(Operator<W>),
    /// A negation of a condition.
    Not(Box<Self>),
    /// Logical AND of two conditions.
    And(Box<Self>, Box<Self>),
    /// Logical OR of two conditions.
    Or(Box<Self>, Box<Self>),
    /// Evaluates the commands first, then treat the result output as a condition.
    Eval(Box<C>),
    /// Evaluates the commands, then take the return code as a boolean condition.
    ReturnZero(Box<C>),
}

impl<C, W> Condition<C, W> {
    /// negate the condition
    pub fn flip(self) -> Self {
        use Condition::*;
        match self {
            Cond(op) => Cond(op.flip()),
            Not(cond) => *cond,
            And(lhs, rhs) => Or(Box::new(lhs.flip()), Box::new(rhs.flip())),
            Or(lhs, rhs) => And(Box::new(lhs.flip()), Box::new(rhs.flip())),
            c @ _ => Not(Box::new(c)),
        }
    }
}
