use std::collections::HashMap;

use crate::jit::codegen::CodeGen;

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

pub struct X86_64Codegen {
    bytes: Vec<u8>,
}

#[derive(Clone, Debug)]
pub struct Label {
    /// The offset of the label in the instruction byte array
    label_offset: Option<usize>,

    /// The location of all the jumps that will jump to the specified label
    jump_offsets: Vec<usize>,
}

impl Label {
    pub fn new() -> Label {
        Label {
            label_offset: None,
            jump_offsets: vec![],
        }
    }

    pub fn define_label(&mut self, label_offset: usize) {
        self.label_offset = Some(label_offset);
    }

    pub fn current_offset_as_label(&mut self, assembler: &X86_64Codegen) {
        self.define_label(assembler.current_offset());
    }

    pub fn add_jump(&mut self, assembler: &mut X86_64Codegen) {
        // add a constant that we'll later on replace with the actual jump offset
        self.jump_offsets.push(assembler.current_offset());
        assembler.emit32(0xDEADBEEF);
    }

    pub fn link_jumps(&self, assembler: &mut X86_64Codegen) {
        // presume 32 bit jumps
        let label_offset = self
            .label_offset
            .expect("Can't link jumps until we have the label defined");
        for jump_offset in self.jump_offsets.iter() {
            // just doing it this manual way to prevent having to do weird casts to isize prior to subtraction
            let offset: isize = if label_offset > *jump_offset {
                (label_offset - *jump_offset) as isize
            } else {
                -((*jump_offset - label_offset) as isize)
            };

            assembler.set(*jump_offset, &offset.to_le_bytes());
        }
    }
}

impl X86_64Codegen {
    fn current_offset(&self) -> usize {
        return self.bytes.len();
    }

    fn emit8(&mut self, byte: u8) {
        self.bytes.push(byte);
    }

    fn set(&mut self, offset: usize, value: &[u8]) {
        for i in 0..value.len() {
            self.bytes[offset + i] = value[i];
        }
    }

    fn emit32(&mut self, value: u32) {
        // TODO: Better abstraction
        let bytes = value.to_le_bytes();
        for i in 0..bytes.len() {
            self.bytes.push(bytes[i]);
        }
    }

    fn emit64(&mut self, value: u64) {
        let bytes = value.to_le_bytes();
        for i in 0..bytes.len() {
            self.bytes.push(bytes[i]);
        }
    }

    // fn emit_displacement(&self, displacement: Option<usize>) {
    //     if displacement == None || displacement == Some(0) {
    //         // explicitly nothing
    //         return;
    //     }

    //     let Some(displacement) = displacement;
    //     if X86_64Codegen::value_fits_in_i8(displacement) {
    //         self.emit8(displacement as u8);
    //     } else if X86_64Codegen::value_fits_in_i32(displacement) {
    //         self.emit32(displacement as u32);
    //     } else {
    //         unreachable!("Only supports i8 & i32");
    //     }
    // }

    // fn while_loop<T: FnOnce(&mut X86_64Codegen) -> ()>(&mut self, emit_loop: T) {
    //     let mut loop_start = Label::new();
    //     let mut loop_end = Label::new();

    //     // Jump *over* loop (loop end)
    //     self.jump_if_memory(Register::RSP, JumpOpCode::JumpIfZero, &mut loop_end);
    //     loop_start.current_offset_as_label(&self);

    //     emit_loop(self);

    //     // Jump back up to above (loop start)
    //     self.jump_if_memory(Register::RSP, JumpOpCode::JumpIfNotZero, &mut loop_start);
    //     loop_end.current_offset_as_label(self);
    // }

    // fn emit_binaryop_instruction(&mut self, dst: Operand, src: Operand) {}

    // pub fn encode(&mut self, op: Opcode, dst: Operand, src: Operand) {
    //     // Optimizations
    //     if op == Opcode::Mov {
    //         if let (Operand::Register(reg, None), Operand::Immediate(imm)) = (dst, src) {
    //             // MOV(r, 0) = XOR(r, r) since it's better pipelined
    //             if imm == 0 {
    //                 op = Opcode::Xor;
    //                 src = Operand::Register(reg, None);
    //             } else {
    //                 // MOV(r, imm) = B8 + r since byte instruction no opcode
    //                 if X86_64Codegen::value_fits_in_i32(imm) {
    //                     // TODO: This is messy but we want to basically remove the :W since we are emitting a 32 bit imm
    //                     self.emit8(
    //                         (RexPrefixEncoding::from_operand(reg) & !RexPrefixEncoding::W).as_u8(),
    //                     );
    //                     self.emit8(0xB8 | reg.encode_register());
    //                     self.emit32(imm);
    //                 } else {
    //                     // We emit .W to indicate it's a large 64 bit imm
    //                     self.emit8(RexPrefixEncoding::from_operand(reg).as_u8());
    //                     self.emit8(0xB8 | reg.encode_register());
    //                     self.emit64(imm);
    //                 }
    //                 return;
    //             }
    //         }
    //         if let (Operand::Register(reg1, _), Operand::Register(reg2, _)) = (dst, src) {
    //             // MOV(r, r) always is a no-op
    //             if reg1 == reg2 {
    //                 return;
    //             }
    //         }
    //     }

    //     let (primaryOpcode, opcodeExtension) = match (dst, src) {
    //         (Operand::Register(_, Some(_)), Operand::Register(_, None)) => match op {
    //             Opcode::Add => (0x01, None),
    //             Opcode::Sub => (0x29, None),
    //             Opcode::Mov => (0x89, None),
    //             Opcode::Cmp => (0x39, None),
    //         },
    //         (Operand::Register(_, None), Operand::Register(_, Some(_))) => match op {
    //             Opcode::Add => (0x03, None),
    //             OpCode::Sub => (0x2B, None),
    //             Opcode::Mov => (0x8B, None),
    //             Opcode::Cmp => (0x3B, None),
    //         },
    //         (Operand::Register(_, _), Operand::Immediate(imm)) => {
    //             if X86_64Codegen::value_fits_in_i8(imm) {
    //                 // imm8
    //                 match op {
    //                     Opcode::Add => (0x83, Some(0)),
    //                     Opcode::Sub => (0x83, Some(5)),
    //                     Opcode::Mov => (0xC7, Some(0)),
    //                 }
    //             } else if X86_64Codegen::value_fits_in_i32(imm) {
    //                 // imm32
    //                 match op {
    //                     Opcode::Add => (0x81, Some(0)),
    //                     Opcode::Sub => (0x81, Some(5)),
    //                     Opcode::Mov => (0xC7, Some(0)),
    //                 }
    //             } else {
    //                 unreachable!()
    //             }
    //         }
    //         _ => unreachable!(),
    //     };
    // }

    // fn encode_math_instruction(&mut self, opcode: OpCode, dst: Operand, src: Operand) {
    //     self.emit8(RexPrefixEncoding::from_operands(dst, src).as_u8());

    //     let addressing_mode = AddressingMode::from_operand(dst);
    //     match (dst, src) {
    //         // dst = Math(dst, src)
    //         // todo; we should also support Register(dst, None), Register(src, displacement)
    //         // seems to consistently just be +2 to the standard instruction but eh we can do it better than that
    //         (Operand::Register(dst, displacement), Operand::Register(src, None)) => {
    //             self.emit8(Mnemonic::mnemonic_for_opcode(opcode) as u8);
    //             self.emit8(
    //                 addressing_mode as u8 | (src.encode_register() << 3) | dst.encode_register(),
    //             );
    //             self.emit_displacement(displacement);
    //         }
    //         (Operand::Register(reg, displacement), Operand::Immediate(imm)) => {
    //             if X86_64Codegen::value_fits_in_i8(imm) {
    //                 self.emit8(Mnemonic::MathImm8 as u8);
    //                 self.emit8(
    //                     addressing_mode as u8 | ((opcode as u8) << 3) | reg.encode_register(),
    //                 );
    //                 self.emit_displacement(displacement);
    //                 self.emit8(imm as u8);
    //             } else if X86_64Codegen::value_fits_in_i32(imm) {
    //                 self.emit8(Mnemonic::MathImm32 as u8);
    //                 self.emit8(
    //                     addressing_mode as u8 | ((opcode as u8) << 3) | reg.encode_register(),
    //                 );
    //                 self.emit_displacement(displacement);
    //                 self.emit32(imm as u32);
    //             } else {
    //                 unreachable!("Only supports i8 & i32");
    //             }
    //             // otherwise if it's == 0/register direct then we don't need to do anything
    //         }
    //         _ => unreachable!("Invalid arms dst {:?}, src {:?}", dst, src),
    //     }
    // }

    // fn add(&mut self, dst: Operand, src: Operand) {
    //     self.encode_math_instruction(OpCode::Add, dst, src);
    // }

    // fn sub(&mut self, dst: Operand, src: Operand) {
    //     self.encode_math_instruction(OpCode::Sub, dst, src);
    // }

    // // [ ... ] => while (*dp != 0) { ... }
    // // we more accurately translate it to this: if (*dp != 0) do {  } while (*dp != 0);
    // // which avoids the double jump on the while loop.
    // fn jump_if_memory(&mut self, register: Register, opcode: JumpOpCode, label: &mut Label) {
    //     // for optimization since we don't need the value of the read we can do a cmp reg[0], 0
    //     self.encode_math_instruction(
    //         OpCode::Cmp,
    //         Operand::Register(register, Some(0)),
    //         Operand::Immediate(0),
    //     );

    //     // then we have our JZ label
    //     // TODO: Abstract this 0x0F, there must be a better way to represent this opocde...
    //     self.emit8(0x0F);
    //     self.emit8(opcode as u8);
    //     // TODO: Jump!
    //     label.add_jump(self);
    // }

    // fn mov(&mut self, dst: Operand, src: Operand) {
    //     match (dst, src) {
    //         // dst = Math(dst, src)
    //         (Operand::Register(dst, None), Operand::Register(src, None)) => {
    //             self.emit8(RexPrefixEncoding::from_registers(dst, src).as_u8());
    //             self.emit8(Mnemonic::Move as u8);
    //             self.emit8(
    //                 AddressingMode::RegisterDirect as u8
    //                     | (src.encode_register() << 3)
    //                     | dst.encode_register(),
    //             );
    //         }
    //         (Operand::Register(reg, displacement), Operand::Immediate(imm)) => {
    //             let addressing_mode = AddressingMode::from_operand(dst);

    //             self.emit8(RexPrefixEncoding::from_register(reg).as_u8());
    //             if X86_64Codegen::value_fits_in_i8(imm) {
    //                 self.emit8(Mnemonic::Move as u8);
    //                 self.emit8(addressing_mode as u8 | reg.encode_register());
    //                 self.emit_displacement(displacement);
    //                 self.emit8(imm as u8);
    //             } else if X86_64Codegen::value_fits_in_i32(imm) {
    //                 self.emit8(Mnemonic::Move as u8);
    //                 self.emit8(addressing_mode as u8 | reg.encode_register());
    //                 self.emit_displacement(displacement);
    //                 self.emit32(imm as u32);
    //             } else {
    //                 unreachable!("Only supports i8 & i32");
    //             }
    //             // otherwise if it's == 0/register direct then we don't need to do anything
    //         }
    //         _ => unreachable!("Invalid arms dst {:?}, src {:?}", dst, src),
    //     }
    // }

    // fn windows_call(&mut self) {
    //     // For a windows call we need to make 32 bytes worth of stack space and 8 bytes to align to 16 byte boundary
    //     self.sub(
    //         Operand::Register(Register::RSP, None),
    //         Operand::Immediate(32 + 8),
    //     );
    //     // self.mov(...)
    //     // self.call(...)

    //     // Clean up the stack from what we pushed above
    //     self.add(
    //         Operand::Register(Register::RSP, None),
    //         Operand::Immediate(32 + 8),
    //     );
    // }
}

impl CodeGen for X86_64Codegen {
    fn compile(executable: Vec<u8>, bytecode: Vec<crate::bytecode::ByteCode>) {}
}
