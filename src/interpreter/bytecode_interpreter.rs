use crate::bytecode::ByteCode;

pub struct ByteCodeInterpreter {}

impl ByteCodeInterpreter {
    pub fn new() -> Self {
        Self {}
    }

    pub fn run(&mut self, runtime: &mut super::Runtime, instructions: &Vec<ByteCode>) {
        let mut pc = 0;
        while pc < instructions.len() {
            match &instructions[pc] {
                ByteCode::Add(i) => runtime.shift_data_pointer(*i),
                ByteCode::DerefAdd(i) => runtime.deref_and_add_value(*i),
                ByteCode::DerefSub(i) => runtime.deref_and_sub_value(*i),
                ByteCode::Read(len) => runtime.read(*len),
                ByteCode::Write(len) => runtime.write(*len),
                ByteCode::JumpForwardsIfZero(offset) => {
                    if runtime.value_is_zero() {
                        pc = pc.wrapping_add(*offset);
                        // don't do the ++
                        continue;
                    }
                }
                ByteCode::JumpBackwardsIfNonZero(offset) => {
                    if !runtime.value_is_zero() {
                        pc = pc.wrapping_sub(*offset);
                        // don't do the ++
                        continue;
                    }
                }
            }
            pc += 1;
        }
    }
}
