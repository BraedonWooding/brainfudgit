pub mod x86_64;

pub trait CodeGen {
    fn compile(&mut self, executable: Vec<u8>, bytecode: Vec<ByteCode>);

    fn new() -> Self;
}

use crate::bytecode::ByteCode;

#[cfg(target_arch = "x86_64")]
pub use self::x86_64::*;
