//! Defines abstract representations of the automake source.
use super::minimal::{Word, WordFragment};
use super::node::NodeId;

/// Wraps word fragment with fixing generics
pub type ShWordFragment = WordFragment<String, NodeId, ShWord>;

/// Wraps minimal word with fixing generics
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ShWord(pub Word<ShWordFragment>);

impl From<Word<ShWordFragment>> for ShWord {
    fn from(value: Word<ShWordFragment>) -> Self {
        Self(value)
    }
}

impl Into<Word<ShWordFragment>> for ShWord {
    fn into(self) -> Word<ShWordFragment> {
        self.0
    }
}

impl Into<Option<WordFragment<String, NodeId, ShWord>>> for ShWord {
    fn into(self) -> Option<WordFragment<String, NodeId, ShWord>> {
        use Word::*;
        match self.0 {
            Single(word) => Some(word),
            _ => None,
        }
    }
}

impl Into<Option<String>> for ShWordFragment {
    fn into(self) -> Option<String> {
        use WordFragment::*;
        match self {
            Literal(l) => Some(l.into()),
            _ => None,
        }
    }
}

impl Into<Option<String>> for ShWord {
    fn into(self) -> Option<String> {
        match self.0 {
            Word::Empty => Some(String::new()),
            Word::Concat(_) => None,
            Word::Single(word) => word.into(),
        }
    }
}

impl From<String> for ShWord {
    fn from(value: String) -> Self {
        value
            .is_empty()
            .then_some(Word::Empty)
            .unwrap_or(Word::Single(WordFragment::Literal(value.into())))
            .into()
    }
}
