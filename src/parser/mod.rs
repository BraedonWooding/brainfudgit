pub mod parser;

#[derive(Debug, Clone)]
pub enum AstKind {
    ShiftDataPointer(isize),

    DerefIncrement(u8),
    DerefDecrement(u8),

    Write(usize),
    Read(usize),

    Loop(BasicBlock),
}

#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub instructions: Vec<AstKind>,
}

pub type Program = BasicBlock;
