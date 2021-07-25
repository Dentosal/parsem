use std::sync::atomic::AtomicUsize;

use crate::data::Value;

static LABEL_ID_COUNTER: AtomicUsize = AtomicUsize::new(0);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LabelId {
    id: usize,
}
impl LabelId {
    pub const FIRST: Self = Self { id: 0 };

    pub fn new() -> Self {
        Self {
            id: LABEL_ID_COUNTER.fetch_add(1, std::sync::atomic::Ordering::SeqCst),
        }
    }
}

/// Abstract machine instructions
#[derive(Debug, Clone)]
pub enum Instruction {
    /// Comment
    Comment(String),
    /// Push a constant to the stack
    Push { value: Value },
    /// Push a reference to a global
    PushRef { name: String },
    /// Reref a reference
    Load,
    /// Store to a reference
    /// `ref = pop(); *ref = pop();`
    Store,
    /// Remove value on depth
    Remove { depth: usize },
    /// Copy value on depth to top
    Copy { depth: usize },
    /// Swap top and value on depth
    Swap { depth: usize },
    /// Pops function reference and `argc` argument from the stack and performs the call
    CallFunction { argc: usize },
    /// Define a label
    Label { id: LabelId },
    /// Jump to label if stack top matches boolean
    JumpToIf { condition: bool, label: LabelId },
}
