use crate::parser;

pub struct AstInterpreter {}

impl AstInterpreter {
    pub fn new() -> Self {
        Self {}
    }

    pub fn interpret(&mut self, runtime: &mut super::Runtime, program: &crate::parser::Program) {
        self.interpret_block(runtime, program)
    }

    fn interpret_block(&mut self, runtime: &mut super::Runtime, block: &parser::BasicBlock) {
        // written tihs way since the upper-most block (program) doesn't repeat
        for instruction in block.instructions.iter() {
            match instruction {
                parser::AstKind::ShiftDataPointer(i) => runtime.shift_data_pointer(*i),
                parser::AstKind::DerefIncrement(i) => runtime.deref_and_add_value(*i),
                parser::AstKind::DerefDecrement(i) => runtime.deref_and_sub_value(*i),
                parser::AstKind::Write(i) => runtime.write(*i),
                parser::AstKind::Read(i) => runtime.read(*i),
                parser::AstKind::Loop(sub_block) => {
                    while !runtime.value_is_zero() {
                        self.interpret_block(runtime, sub_block);
                    }
                }
            }
        }
    }
}
