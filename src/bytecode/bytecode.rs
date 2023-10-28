use std::collections::HashSet;

use crate::{parser::{Program, BasicBlock, AstKind}, Optimizations, optimizer::optimize};

use super::ByteCode;

pub fn to_bytecode(program: &Program, optimizations: &HashSet<Optimizations>) -> Vec<ByteCode> {
    let mut instructions = vec![];
    bytecode_write_block(&mut instructions, program, optimizations);
    instructions
}

fn bytecode_write_block(instructions: &mut Vec<ByteCode>, block: &BasicBlock, optimizations: &HashSet<Optimizations>) {
    let block = optimize(block, optimizations);

    for instruction in block.instructions.iter() {
        match instruction {
            AstKind::ShiftDataPointer(i) => instructions.push(ByteCode::Add(*i)),
            AstKind::DerefIncrement(i) => instructions.push(ByteCode::DerefAdd(*i)),
            AstKind::DerefDecrement(i) => instructions.push(ByteCode::DerefSub(*i)),
            AstKind::Write(i) => instructions.push(ByteCode::Write(*i)),
            AstKind::Read(i) => instructions.push(ByteCode::Read(*i)),
            AstKind::Loop(inner_block) => {
                // because of recursion we need to count how many sub-instructions exist
                let current_len = instructions.len();
                bytecode_write_block(instructions, inner_block, optimizations);
                let end_len = instructions.len();
                let offset = end_len - current_len;

                // i.e. if we have just [+] then it'll be JumpForwardsIfZero(3) & JumpBackwardsIfNonZero(1)
                // we want to jump past the last instruction (that we've not added yet)
                instructions.insert(current_len, ByteCode::JumpForwardsIfZero(offset + 2));
                // we want to jump above the instruction above
                instructions.push(ByteCode::JumpBackwardsIfNonZero(offset));
            },
        }
    }
}
