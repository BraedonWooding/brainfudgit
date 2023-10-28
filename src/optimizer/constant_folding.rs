use crate::parser::{BasicBlock, AstKind};

macro_rules! constant_fold_instruction {
    ($pc: tt, $bytecode: tt, $pattern: path) => {
        let old_pos = $pc;
        // accumulate all the values into this "constant"
        let mut acc = 0;

        while $pc < $bytecode.len() {
            if let $pattern(i2) = $bytecode[$pc] {
                acc += i2;
                $pc += 1;
            } else {
                // no longer a series of constants
                break;
            }
        }

        // doing this slightly awkward way just to avoid having to shuffle a lot of elements twice
        $bytecode.drain(old_pos + 1..$pc);
        $bytecode[old_pos] = $pattern(acc);
    };
}

/// A form of constant folding that folds multiple adds/subs into one
pub fn constant_folding(block: &mut BasicBlock) {
    let mut pc = 0;
    let instructions = &mut block.instructions;
    while pc < instructions.len() {
        match instructions[pc] {
            AstKind::ShiftDataPointer(_) => {
                constant_fold_instruction!(pc, instructions, AstKind::ShiftDataPointer);
            }
            AstKind::DerefIncrement(_) => {
                constant_fold_instruction!(pc, instructions, AstKind::DerefIncrement);
            }
            // technically you could combine deref sub & add but I don't know why you would
            // i.e. + - + - is just nothing, but that is just irrelevant
            AstKind::DerefDecrement(_) => {
                constant_fold_instruction!(pc, instructions, AstKind::DerefDecrement);
            }
            // everything else is ignored
            _ => {}
        }

        pc += 1;
    }
}
