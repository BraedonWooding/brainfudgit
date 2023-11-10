pub mod x86_64;

use crate::optimizer::MirBasicBlock;

pub trait CodeGen {
    fn load(&mut self, program: MirBasicBlock);
    
    fn to_vec_u8(&self) -> Vec<u8>;

    fn new() -> Self;
}

#[cfg(target_arch = "x86_64")]
pub use self::x86_64::*;
