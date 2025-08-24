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

/// Represents a condition for control flow, which can be a single operator-based
/// condition, a logical combination of conditions, or an evaluated word.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Condition<C, W> {
    /// A single condition based on an operator.
    Cond(Operator<W>),
    /// Logical AND of two conditions.
    And(Box<Self>, Box<Self>),
    /// Logical OR of two conditions.
    Or(Box<Self>, Box<Self>),
    /// Evaluates the commands first, then treat the result output as a condition.
    Eval(Box<C>),
    /// Evaluates the commands, then take the return code as a boolean condition.
    ReturnZero(Box<C>),
}
