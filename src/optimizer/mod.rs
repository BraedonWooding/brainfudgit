use std::collections::HashSet;

use crate::{Optimizations, parser::BasicBlock};

use self::constant_folding::constant_folding;

pub mod constant_folding;

pub fn optimize(block: &BasicBlock, options: &HashSet<Optimizations>) -> BasicBlock {
    let mut copy = block.clone();

    if options.contains(&Optimizations::ConstantFolding) {
        constant_folding(&mut copy);
    }

    copy
}