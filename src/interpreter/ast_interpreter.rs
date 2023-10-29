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
                parser::AstKind::Increment => runtime.shift_data_pointer(1),
                parser::AstKind::Decrement => runtime.shift_data_pointer(-1),
                parser::AstKind::DerefIncrement => runtime.deref_and_add_value(1),
                parser::AstKind::DerefDecrement => runtime.deref_and_sub_value(1),
                parser::AstKind::Write => runtime.write(1),
                parser::AstKind::Read => runtime.read(1),
                parser::AstKind::Loop(sub_block) => {
                    while !runtime.value_is_zero() {
                        self.interpret_block(runtime, sub_block);
                    }
                }
            }
        }
    }
}
