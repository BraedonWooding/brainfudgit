use super::{MirBasicBlock, MirAstKind};

/// If the first instruction in the program is a `[` we can skip the first block
pub fn comment_block(block: &mut MirBasicBlock) {
    // this block is high level "program", if the first instruction is a loop
    // then it's a comment block and can be removed this is because the first cell
    // will always be 0 so any `[` will skip it.
    if block.instructions.len() > 0 {
        if let MirAstKind::Loop(_) = block.instructions[0] {
            block.instructions.remove(0);
        }
    }
}