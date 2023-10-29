use crate::optimizer::{MirBasicBlock, MirAstKind};

pub struct MirInterpreter {}

impl MirInterpreter {
    pub fn new() -> Self {
        Self {}
    }

    pub fn interpret(&mut self, runtime: &mut super::Runtime, block: &MirBasicBlock) {
        // written tihs way since the upper-most block (program) doesn't repeat
        for instruction in block.instructions.iter() {
            match instruction {
                MirAstKind::ShiftDataPointer(i) => runtime.shift_data_pointer(*i),
                MirAstKind::DerefIncrement(i) => runtime.deref_and_add_value(*i),
                MirAstKind::DerefDecrement(i) => runtime.deref_and_sub_value(*i),
                MirAstKind::Write(i) => runtime.write(*i),
                MirAstKind::Read(i) => runtime.read(*i),
                MirAstKind::Loop(sub_block) => {
                    while !runtime.value_is_zero() {
                        self.interpret(runtime, sub_block);
                    }
                }
            }
        }
    }
}
