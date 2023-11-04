use std::collections::HashMap;

use super::CodeGen;

use arbitrary_int::u3;
use bitbybit::{bitfield, bitenum};
use bitflags::bitflags;

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

#[derive(Debug)]
#[bitenum(u4, exhaustive: true)]
pub enum Register {
    RAX = 0b0000,
    RCX = 0b0001,
    RDX = 0b0010,
    RBX = 0b0011,
    RSP = 0b0100,
    RBP = 0b0101,
    RSI = 0b0110,
    RDI = 0b0111,
    // The first bit becomes a rex prefix
    R8 = 0b1000,
    R9 = 0b1001,
    R10 = 0b1010,
    R11 = 0b1011,
    R12 = 0b1100,
    R13 = 0b1101,
    R14 = 0b1110,
    R15 = 0b1111,
}

#[repr(u8)]
#[derive(Clone, Copy, Debug)]
pub enum Mnemonic {
    /// We are using the r16/r32/r64 variant so we can use larger registers
    AddRegisters = 0x01,

    /// Perform a math opcode on an i32 to a r16/32/r64 register
    MathImm32 = 0x81,

    /// Perform a math opcode on an i8 to a r16/32/r64 register
    MathImm8 = 0x83,

    /// We are using the r16/r32/r64 variant so we can use larger registers
    SubRegisters = 0x29,

    CmpRegisters = 0x39,

    Move = 0x89,
}

pub enum Argument {
    Register,
    MemRegister,
    Immediate,
}

pub type OpcodeMap = HashMap<(Argument, Argument), (u32, Option<u32>)>;

#[derive(Clone, Debug, PartialEq)]
pub enum Opcode {
    Add,
    Sub,
    Mov,
    Cmp,
    Xor,
}

impl Mnemonic {
    pub fn mnemonic_for_opcode(opcode: OpCode) -> Mnemonic {
        match opcode {
            OpCode::Add => Mnemonic::AddRegisters,
            OpCode::Sub => Mnemonic::SubRegisters,
            OpCode::Cmp => Mnemonic::CmpRegisters,
        }
    }
}

#[repr(u8)]
#[derive(Clone, Copy, Debug)]
pub enum OpCode {
    Add = 0,
    Sub = 5,
    Cmp = 7,
}

#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum JumpOpCode {
    JumpIfZero = 84,
    JumpIfNotZero = 85,
}

/// The addressing mode of the registers, this is the first 2 bits of the ModRM
#[bitenum(u2, exhaustive: true)]
pub enum AddressingMode {
    /// Dereference the memory location at the register but there is no additional displacement
    ZeroByteDisplacement = 0b00,
    /// Dereference the memory location at the register and apply the displacement that is stored 1 byte after MOD R/M but before the constant/immediate
    OneByteDisplacement = 0b01,
    FourByteDisplacement = 0b10,
    /// No indirect lookups of memory locations just use the direct value stored in the register
    RegisterDirect = 0b11,
}

impl AddressingMode {
    pub fn from_operand(op: Operand) -> AddressingMode {
        match op {
            Operand::Register(_, None) => AddressingMode::RegisterDirect,
            Operand::Register(_, Some(0)) => AddressingMode::ZeroByteDisplacement,
            Operand::Register(_, Some(displacement)) =>
                if X86_64Codegen::value_fits_in_i8(displacement) { AddressingMode::OneByteDisplacement }
                else { AddressingMode::FourByteDisplacement },
            Operand::Immediate(_) => unreachable!("This shouldn't be reached can't get addressing mode from an immediate"),
        }
    }
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
        let label_offset = self.label_offset.expect("Can't link jumps until we have the label defined");
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

bitflags! {
    pub struct RexPrefixEncoding : u8 {
        const Base  = 0b0100_000;
        const W     = 0b0001_000;
        const R     = 0b0000_100;
        const X     = 0b0000_010;
        const B     = 0b0000_001;
    }
}

#[derive(Clone, Debug)]
pub enum Operand {
    Register(Register, Option<usize>),
    Immediate(usize),
}

impl Register {
    pub fn encode_register(&self) -> EncodedRegister {
        // https://wiki.osdev.org/X86-64_Instruction_Encoding#Registers
        // X.Reg, in this case our encoded register is just encoding the Y part of X.YYY
        // we'll need to set REX flags X & B when using dest & src registers
        return EncodedRegister::new_with_raw_value(u3::from((*self as u8) & 0x7));
    }
}

impl RexPrefixEncoding {
    pub fn as_u8(&self) -> u8 {
        self.bits() as u8
    }

    pub fn from_operands(dst: Operand, src: Operand) -> RexPrefixEncoding {
        let mut result = RexPrefixEncoding::Base | RexPrefixEncoding::W;

        if let Operand::Register(dst, _) = dst {
            if dst as u8 > 7 {
                result |= RexPrefixEncoding::B;
            }
        }
        if let Operand::Register(src, _) = src {
            if src as u8 > 7 {
                result |= RexPrefixEncoding::R;
            }
        }

        result
    }

    pub fn from_operand(dst: Operand) -> RexPrefixEncoding {
        // re-using above, so that we don't need to parse this into registers before we use it
        RexPrefixEncoding::from_operands(dst, Operand::Immediate(0))
    }
}

#[derive(Clone, Debug)]
enum OperandEncoding {
    /// "I", Operand 1 = AL/AX/EAX/RAX, Operand 2 = imm8/16/32
    /// has no MOD/RM byte, has separate opcode for AL/AX
    /// and can use REX.W to 64 bit extend to RAX.
    ImmediateEncoding(Immediate),

    /// "MI", Operand 1 = ModRM:r/m (r, w), Operand 2 = imm8/16/32
    MemoryImmediateEncoding(),
    
    /// "MR", Operand 1 = ModRM:r/m (r, w), Operand 2 = ModRM:reg (r)
    MemoryRegisterEncoding(),
    
    /// "RM", Operand 1 = ModRM:reg (r), Operand 2 = ModRM:r/m (r, w)
    RegisterMemoryEncoding(),

    /// "OI", Operand 1 = opcode + rd (w), Operand 2 = imm8/16/32/64
    OpcodeImmediate(Operand, Immediate),

    // TODO: FD & TD
}

const TWO_BYTE_OPCODE_VALUE: u8 = 0x0F;

/// The encodings that can be emitted prior to the instruction
/// written this way since there is an order to the prefixes
pub struct Prefix {
    mandatory_prefix: Option<u8>,
    /// If set it should just be set to `TWO_BYTE_OPCODE_VALUE`
    two_byte_opcode: Option<u8>,
    rex: Option<RexPrefixEncoding>,
}

pub struct Instruction {
    prefix: Prefix,

    primary_opcode: u8,
    displacement: Option<Displacement>,
    immediate: Option<Immediate>,
}

impl Instruction {
    pub fn new(primary_opcode: u8, encoding: OperandEncoding) -> Instruction {
        match encoding {
            OperandEncoding::OpcodeImmediate(reg, imm) => {
                if let Operand::Register(_, None) = reg {
                    let (encoded, rex) = EncodedRegister::from_register(reg);
                    Instruction {
                        prefix: Prefix { rex: Some(rex), mandatory_prefix: None, two_byte_opcode: None },
                        primary_opcode: primary_opcode | u8::from(encoded.raw_value()),
                        displacement: None,
                        immediate: Some(imm),
                    }
                } else {
                    // TODO: use result
                    panic!("Invalid instruction");
                }
            },
            OperandEncoding::ImmediateEncoding(imm) => todo!(),
            OperandEncoding::MemoryImmediateEncoding() => todo!(),
            OperandEncoding::MemoryRegisterEncoding() => todo!(),
            OperandEncoding::RegisterMemoryEncoding() => todo!(),
        }
    }
}

#[derive(Debug)]
#[bitenum(u3, exhaustive: true)]
pub enum EncodedRegister {
    AX = 0b000,
    CX = 0b001,
    DX = 0b010,
    BX = 0b011,
    /// Illegal argument for SIB byte but otherwise valid
    /// when used in MOD/RM specifies there will be an SIB byte
    SP = 0b100,
    /// Used to indicate displacement only mode also 
    BP = 0b101,
    SI = 0b110,
    DI = 0b111,
}

impl EncodedRegister {
    pub fn from_registers(dst: Operand, src: Operand) -> ((EncodedRegister, EncodedRegister), RexPrefixEncoding) {
        let rex = RexPrefixEncoding::from_operands(dst, src);
        let (Operand::Register(dst, _), Operand::Register(src, _)) = (dst, src);
        ((dst.encode_register(), src.encode_register()), rex)
    }
    
    pub fn from_register(dst: Operand) -> (EncodedRegister, RexPrefixEncoding) {
        let rex = RexPrefixEncoding::from_operands(dst);
        let Operand::Register(dst, _) = dst;
        (dst.encode_register(), rex)
    }
}

/// Special mode that is enabled if the register is 101 (EBP) & MOD (addressing mode) = 00
/// is a full byte.
#[bitfield(u8)]
struct ScaledIndexByte {
    #[bits(5..=7, r)]
    scale: u3,

    #[bits(3..=5, r)]
    register: EncodedRegister,

    #[bits(0..=2, r)]
    base: EncodedRegister,
}

/// This primarily specifies addressing mode, a source/destination register, and optionally an opcode extension
#[bitfield(u8)]
struct ModRM {
    #[bits(6..=7, r)]
    addressing_mode: AddressingMode,

    #[bits(3..=5, r)]
    register: ModRMRegister,

    /// REX.B extends this to access upper registers
    #[bits(0..=2, r)]
    register_memory: EncodedRegister,
}

/// This is like an "enum" but the fields are overlapped
/// which one is set will be based upon the opcode
#[bitfield(u3)]
struct ModRMRegister {
    #[bits(0..=2, r)]
    opcode_extension: u3,

    /// REX.R extends this to access upper registers
    #[bits(0..=2, r)]
    register: EncodedRegister,
}

#[derive(Clone, Debug)]
enum Displacement {
    ZeroByteDisplacement,
    OneByteDisplacement(u8),
    FourByteDisplacement(u32),
}

#[derive(Clone, Debug)]
enum Immediate {
    Imm8(u8),
    Imm32(u32),
    /// possible but not with a displacement
    Imm64(u64),
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

    fn value_fits_in_i8(value: usize) -> bool {
        value <= (i8::MAX as usize) || (((!value) & i8::MIN as usize) == 0)
    }

    fn value_fits_in_i32(value: usize) -> bool {
        value <= (i32::MAX as usize) || (((!value) & i32::MIN as usize) == 0)
    }

    fn emit_displacement(&self, displacement: Option<usize>) {
        if displacement == None || displacement == Some(0) {
            // explicitly nothing
            return;
        }

        let Some(displacement) = displacement;
        if X86_64Codegen::value_fits_in_i8(displacement) {
            self.emit8(displacement as u8);
        } else if X86_64Codegen::value_fits_in_i32(displacement) {
            self.emit32(displacement as u32);
        } else {
            unreachable!("Only supports i8 & i32");
        }
    }

    fn emit_binaryop_instruction(&mut self, dst: Operand, src: Operand) {
        
    }

    pub fn encode(&mut self, op: Opcode, dst: Operand, src: Operand) {
        // Optimizations
        if op == Opcode::Mov {
            if let (Operand::Register(reg, None), Operand::Immediate(imm)) = (dst, src) {
                // MOV(r, 0) = XOR(r, r) since it's better pipelined
                if imm == 0 {
                    op = Opcode::Xor;
                    src = Operand::Register(reg, None);
                } else {
                    // MOV(r, imm) = B8 + r since byte instruction no opcode
                    if X86_64Codegen::value_fits_in_i32(imm) {
                        // TODO: This is messy but we want to basically remove the :W since we are emitting a 32 bit imm
                        self.emit8((RexPrefixEncoding::from_operand(reg) & !RexPrefixEncoding::W).as_u8());
                        self.emit8(0xB8 | reg.encode_register());
                        self.emit32(imm);
                    } else {
                        // We emit .W to indicate it's a large 64 bit imm
                        self.emit8(RexPrefixEncoding::from_operand(reg).as_u8());
                        self.emit8(0xB8 | reg.encode_register());
                        self.emit64(imm);
                    }
                    return;
                }
            }
            if let (Operand::Register(reg1, _), Operand::Register(reg2, _)) = (dst, src) {
                // MOV(r, r) always is a no-op
                if reg1 == reg2 {
                    return;
                }
            }
        }

        let (primaryOpcode, opcodeExtension) = match (dst, src) {
            (Operand::Register(_, Some(_)), Operand::Register(_, None)) => match op {
                Opcode::Add => (0x01, None),
                Opcode::Sub => (0x29, None),
                Opcode::Mov => (0x89, None),
                Opcode::Cmp => (0x39, None),
            },
            (Operand::Register(_, None), Operand::Register(_, Some(_))) => match op {
                Opcode::Add => (0x03, None),
                OpCode::Sub => (0x2B, None),
                Opcode::Mov => (0x8B, None),
                Opcode::Cmp => (0x3B, None),
            },
            (Operand::Register(_, _), Operand::Immediate(imm)) => if X86_64Codegen::value_fits_in_i8(imm) {
                // imm8
                match op {
                    Opcode::Add => (0x83, Some(0)),
                    Opcode::Sub => (0x83, Some(5)),
                    Opcode::Mov => (0xC7, Some(0)),
                }
            } else if X86_64Codegen::value_fits_in_i32(imm) {
                // imm32
                match op {
                    Opcode::Add => (0x81, Some(0)),
                    Opcode::Sub => (0x81, Some(5)),
                    Opcode::Mov => (0xC7, Some(0)),
                }
            } else {
                unreachable!()
            },
            _ => unreachable!(),
        };


    }

    fn encode_math_instruction(&mut self, opcode: OpCode, dst: Operand, src: Operand) {
        self.emit8(RexPrefixEncoding::from_operands(dst, src).as_u8());
        
        let addressing_mode = AddressingMode::from_operand(dst);
        match (dst, src) {
            // dst = Math(dst, src)
            // todo; we should also support Register(dst, None), Register(src, displacement)
            // seems to consistently just be +2 to the standard instruction but eh we can do it better than that
            (Operand::Register(dst, displacement), Operand::Register(src, None)) => {
                self.emit8(Mnemonic::mnemonic_for_opcode(opcode) as u8);
                self.emit8(addressing_mode as u8 | (src.encode_register() << 3) | dst.encode_register());
                self.emit_displacement(displacement);
            },
            (Operand::Register(reg, displacement), Operand::Immediate(imm)) => {
                if X86_64Codegen::value_fits_in_i8(imm) {
                    self.emit8(Mnemonic::MathImm8 as u8);
                    self.emit8(addressing_mode as u8 | ((opcode as u8) << 3) | reg.encode_register());
                    self.emit_displacement(displacement);
                    self.emit8(imm as u8);
                } else if X86_64Codegen::value_fits_in_i32(imm) {
                    self.emit8(Mnemonic::MathImm32 as u8);
                    self.emit8(addressing_mode as u8 | ((opcode as u8) << 3) | reg.encode_register());
                    self.emit_displacement(displacement);
                    self.emit32(imm as u32);
                } else {
                    unreachable!("Only supports i8 & i32");
                }
                // otherwise if it's == 0/register direct then we don't need to do anything
            },
            _ => unreachable!("Invalid arms dst {:?}, src {:?}", dst, src),
        }
    }

    fn add(&mut self, dst: Operand, src: Operand) {
        self.encode_math_instruction(OpCode::Add, dst, src);
    }

    fn sub(&mut self, dst: Operand, src: Operand) {
        self.encode_math_instruction(OpCode::Sub, dst, src);
    }

    // [ ... ] => while (*dp != 0) { ... }
    // we more accurately translate it to this: if (*dp != 0) do {  } while (*dp != 0);
    // which avoids the double jump on the while loop.
    fn jump_if_memory(&mut self, register: Register, opcode: JumpOpCode, label: &mut Label) {
        // for optimization since we don't need the value of the read we can do a cmp reg[0], 0
        self.encode_math_instruction(OpCode::Cmp, Operand::Register(register, Some(0)), Operand::Immediate(0));

        // then we have our JZ label
        // TODO: Abstract this 0x0F, there must be a better way to represent this opocde...
        self.emit8(0x0F);
        self.emit8(opcode as u8);
        // TODO: Jump!
        label.add_jump(self);
    }

    fn mov(&mut self, dst: Operand, src: Operand) {
        match (dst, src) {
            // dst = Math(dst, src)
            (Operand::Register(dst, None), Operand::Register(src, None)) => {
                self.emit8(RexPrefixEncoding::from_registers(dst, src).as_u8());
                self.emit8(Mnemonic::Move as u8);
                self.emit8(AddressingMode::RegisterDirect as u8 | (src.encode_register() << 3) | dst.encode_register());
            },
            (Operand::Register(reg, displacement), Operand::Immediate(imm)) => {
                let addressing_mode = AddressingMode::from_operand(dst);

                self.emit8(RexPrefixEncoding::from_register(reg).as_u8());
                if X86_64Codegen::value_fits_in_i8(imm) {
                    self.emit8(Mnemonic::Move as u8);
                    self.emit8(addressing_mode as u8 | reg.encode_register());
                    self.emit_displacement(displacement);
                    self.emit8(imm as u8);
                } else if X86_64Codegen::value_fits_in_i32(imm) {
                    self.emit8(Mnemonic::Move as u8);
                    self.emit8(addressing_mode as u8 | reg.encode_register());
                    self.emit_displacement(displacement);
                    self.emit32(imm as u32);
                } else {
                    unreachable!("Only supports i8 & i32");
                }
                // otherwise if it's == 0/register direct then we don't need to do anything
            },
            _ => unreachable!("Invalid arms dst {:?}, src {:?}", dst, src),
        }
    }

    fn while_loop<T: FnOnce(&mut X86_64Codegen) -> ()>(&mut self, emit_loop: T) {
        let mut loop_start = Label::new();
        let mut loop_end = Label::new();

        // Jump *over* loop (loop end)
        self.jump_if_memory(Register::RSP, JumpOpCode::JumpIfZero, &mut loop_end);
        loop_start.current_offset_as_label(&self);

        emit_loop(self);

        // Jump back up to above (loop start)
        self.jump_if_memory(Register::RSP, JumpOpCode::JumpIfNotZero, &mut loop_start);
        loop_end.current_offset_as_label(self);
    }

    fn windows_call(&mut self) {
        // For a windows call we need to make 32 bytes worth of stack space and 8 bytes to align to 16 byte boundary
        self.sub(Operand::Register(Register::RSP, None), Operand::Immediate(32 + 8));
        // self.mov(...)
        // self.call(...)

        // Clean up the stack from what we pushed above
        self.add(Operand::Register(Register::RSP, None), Operand::Immediate(32 + 8));
    }
}

impl CodeGen for X86_64Codegen {
    fn compile(executable: Vec<u8>, bytecode: Vec<crate::bytecode::ByteCode>) {
    }
}