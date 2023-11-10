use std::{cell::RefCell, rc::Rc};

use brainfudgit::asm_x86;

use crate::{jit::codegen::CodeGen, bytecode::ByteCode, optimizer::{MirBasicBlock, MirAstKind}};

use super::{
    instruction::{Displacement, Immediate, Instruction, self},
    operand_encoding::{MemoryBaseRegister, Offset, OperandEncoding},
    ops::{self, JumpOperator},
    registers::{self, RegisterAccess, Registers},
};

/*
    - RSP/ESP (Stack Pointer): Data Pointer
    - RBP/EBP (Base Pointer):

    http://ref.x86asm.net/coder64.html most instructions come from this
    https://uops.info/table.html is a good reference for performance
    https://www.felixcloutier.com/x86/ for some specific commands
    https://wiki.osdev.org/X86-64_Instruction_Encoding for more general stuff

    This code is not written to be performant!  It's instead written to be readable and easily composable.
    So often sub-functions will recheck enums and state rather than rely on having to be called in multiple ways.
*/

#[derive(Debug)]
pub struct Label {
    instructions: Rc<RefCell<Vec<Instruction>>>,

    /// The offset of the label in the instruction byte array
    label_offset: Option<usize>,

    /// The location of all the jumps that will jump to the specified label
    jump_offsets: Vec<usize>,
}

impl Drop for Label {
    fn drop(&mut self) {
        self.link_jumps();
    }
}

impl Label {
    pub fn new(instructions: Rc<RefCell<Vec<Instruction>>>) -> Label {
        Label {
            label_offset: None,
            jump_offsets: vec![],
            instructions,
        }
    }

    pub fn current_offset_as_label(&mut self) {
        self.label_offset = Some(self.instructions.borrow().len());
    }

    pub fn add_jump(&mut self, opcode: JumpOperator) {
        let mut instructions = self.instructions.as_ref().borrow_mut();

        // add a constant that we'll later on replace with the actual jump offset
        self.jump_offsets.push(instructions.len());
        instructions.push(ops::jump_op(
            opcode,
            OperandEncoding::Address(Offset::Immediate(Immediate::Imm32(0xDEADBEEF))),
        ));
    }

    /// Sets all the jump targets this is automatically called on drop() so you don't need to manually call it
    fn link_jumps(&self) {
        // presume 32 bit jumps
        let label_offset = self
            .label_offset
            .expect("Can't link jumps until we have the label defined");
        for jump_offset in self.jump_offsets.iter() {
            // calculate how many bytes the instructions are
            let (largest, smallest, sign) = if label_offset > *jump_offset {
                (label_offset, *jump_offset, 1)
            } else {
                (*jump_offset, label_offset, -1)
            };

            let distance = sign
                * self.instructions.borrow()[smallest..largest]
                    .iter()
                    .map(|inst| inst.len() as i32)
                    .reduce(|acc, i| acc + i)
                    .unwrap_or(0);

            // TODO: Support other stuff than Imm32
            //       just being lazy rn
            self.instructions.as_ref().borrow_mut()[*jump_offset].immediate =
                Some(Immediate::Imm32(u32::from_le_bytes(distance.to_le_bytes())));
        }
    }
}

pub struct X86_64Codegen {
    instructions: Rc<RefCell<Vec<Instruction>>>,
}

impl X86_64Codegen {
    fn current_offset(&self) -> usize {
        return self.instructions.borrow().len();
    }

    fn push(&mut self, inst: Instruction) {
        self.instructions.as_ref().borrow_mut().push(inst);
    }

    fn jump_if_memory(&mut self, register: Registers, opcode: JumpOperator, label: &mut Label) {
        // for optimization since we don't need the value of the read we can do a cmp reg[0], 0
        self.push(ops::cmp(OperandEncoding::MemoryImmediate(
            MemoryBaseRegister::Register((register, RegisterAccess::LowByte)),
            Immediate::Imm8(0),
        )));

        label.add_jump(opcode);
    }

    // [ ... ] => while (*dp != 0) { ... }
    // we more accurately translate it to this: if (*dp != 0) do {  } while (*dp != 0);
    // which avoids the double jump on the while loop.
    fn while_loop<T: FnOnce(&mut X86_64Codegen) -> ()>(&mut self, emit_loop: T) {
        
    }

    fn windows_call(&mut self) {
        asm_x86! {
            // For a windows call we need to make 32 bytes worth of stack space and 8 bytes to align to 16 byte boundary
            sub RSP, 32 + 8;

            // TODO:
            // self.mov(...)
            // self.call(...)

            // Clean up the stack from what we pushed above
            add RSP, 32 + 8;
        };
    }
}

impl CodeGen for X86_64Codegen {
    fn load(&mut self, block: MirBasicBlock) {
        for instruction in block.instructions {
            match instruction {
                MirAstKind::ShiftDataPointer(i) => {
                    asm_x86! {
                        add RSP, (i);
                    }
                },
                MirAstKind::DerefIncrement(_) => todo!(),
                MirAstKind::DerefDecrement(_) => todo!(),
                MirAstKind::Write(_) => todo!(),
                MirAstKind::Read(_) => todo!(),
                MirAstKind::Loop(block) => {
                    asm_x86! {
                        cmp [SPL], 0;
                        jumpIfZero loop_end;
                        loop_start:
            
                        $self.load(block);
            
                        cmp [SPL], 0;
                        jumpIfNotZero loop_start;
                        loop_end:
                    };
                },
            }
        }
    }
    
    fn to_vec_u8(&self) -> Vec<u8> {
        // ideally we could calculate the length whenever we generate each instruction so that we know the full length
        // (this would make jump recalcs cheaper anyways) so in future maybe this becomes static?
        let mut result = vec![];
        let mut buf = [0; 15];
        for instruction in self.instructions.as_ref().borrow().iter() {
            for i in 0..instruction.write_out(&mut buf).unwrap() {
                result.push(buf[i]);
            }
        }

        result
    }

    fn new() -> Self {
        X86_64Codegen {
            instructions: Rc::new(RefCell::new(vec![])),
        }
    }
}
