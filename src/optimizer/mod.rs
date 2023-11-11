use std::collections::HashSet;

use crate::{
    parser::{AstKind, BasicBlock, Program},
    Optimizations,
};

use self::{comment_block::comment_block, constant_folding::constant_folding};

pub mod comment_block;
pub mod constant_folding;

// Intermediate AST that contains a few optimizations
// This is just to keep the high level AST clean
#[derive(Debug, Clone)]
pub enum MirAstKind {
    ShiftDataPointer(isize),

    DerefIncrement(u8),
    DerefDecrement(u8),

    Write(usize),
    Read(usize),

    Loop(MirBasicBlock),
}

#[derive(Debug, Clone)]
pub struct MirBasicBlock {
    pub instructions: Vec<MirAstKind>,
}

fn map_instructions(block: &BasicBlock) -> MirBasicBlock {
    return MirBasicBlock {
        instructions: block
            .instructions
            .iter()
            .map(|inst| match inst {
                AstKind::Increment => MirAstKind::ShiftDataPointer(1),
                AstKind::Decrement => MirAstKind::ShiftDataPointer(-1),
                AstKind::DerefIncrement => MirAstKind::DerefIncrement(1),
                AstKind::DerefDecrement => MirAstKind::DerefDecrement(1),
                AstKind::Write => MirAstKind::Write(1),
                AstKind::Read => MirAstKind::Read(1),
                AstKind::Loop(inner_block) => MirAstKind::Loop(map_instructions(inner_block)),
            })
            .collect(),
    };
}

pub fn optimize(block: &Program, options: &HashSet<Optimizations>) -> MirBasicBlock {
    let mut copy = map_instructions(block);

    if options.contains(&Optimizations::CommentBlock) {
        comment_block(&mut copy);
    }

    if options.contains(&Optimizations::ConstantFolding) {
        constant_folding(&mut copy);
    }

    copy
}
