pub mod parser;

#[derive(Debug, Clone)]
pub enum AstKind {
    Increment,
    Decrement,

    DerefIncrement,
    DerefDecrement,

    Write,
    Read,

    Loop(BasicBlock),
}

#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub instructions: Vec<AstKind>,
}

pub type Program = BasicBlock;
