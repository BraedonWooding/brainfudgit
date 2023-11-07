pub mod codegen;

use crate::{bytecode::ByteCode, optimizer::MirBasicBlock};
use std::{ffi::c_void, mem};

pub enum JitMode {
    /// All: Fully compiles everything to ASM, works like an AOT mode
    All,

    /// None: Nothing is JIT'd and it'll compile it when the block is first executed (very inefficient for this language)
    None,

    /// Economy: Only keeps `threshold` blocks in memory at once,
    /// will clean up blocks that were compiled earlier when it runs out of space
    Economy(usize),

    /// ThresholdBlocks: Given a defined `threshold` will compile blocks that are smaller than it
    ThresholdBlocks(usize),
}

pub enum JitBlock<'a> {
    Compiled(Vec<u8>),
    Pending(&'a Vec<ByteCode>),
}

pub struct JitCompiler<'a> {
    pub mode: JitMode,

    blocks: Vec<JitBlock<'a>>,
}

impl<'a> JitCompiler<'a> {
    pub fn jit_block(&self, block: &MirBasicBlock) {}

    extern "C" fn jit_callback(&mut self, jit_block_id: usize) {
        let block = self.blocks.get(jit_block_id).expect("Invalid jit block id");

        match block {
            JitBlock::Compiled(executable) => {
                let func: extern "C" fn() -> c_void =
                    unsafe { mem::transmute(executable.as_ptr()) };
                func();
            }
            JitBlock::Pending(bytecode) => {
                let compiled_block = todo!();
                // self.blocks[jit_block_id] = &JitBlock::Compiled(compiled_block);
            }
        }
    }
}
