pub mod ast_interpreter;
pub mod bytecode_interpreter;
pub mod mir_interpreter;

use std::io::{Read, Write};

pub struct Runtime {
    /// Pointer into the heap
    data_pointer: usize,

    /// Our statically allocated heap
    heap: Vec<u8>,

    in_stream: Box<dyn Read>,
    out_stream: Box<dyn Write>,
}

impl Runtime {
    pub fn reset(&mut self) {
        // TODO: Reset IO
        self.heap = vec![0; self.heap.len()];
        self.data_pointer = 0;
    }

    /// Read from stream in runtime and write to data pointer
    pub fn read(&mut self, len: usize) {
        self.check_data_pointer();
        // a bit odd but is just an easy way to write directly into the vec by converting it to a slice
        self.in_stream
            .read_exact(&mut self.heap[self.data_pointer..self.data_pointer + len])
            .unwrap();
    }

    /// Write to stream in runtime from data pointer
    pub fn write(&mut self, len: usize) {
        self.check_data_pointer();
        self.out_stream
            .write(&self.heap[self.data_pointer..self.data_pointer + len])
            .unwrap();
    }

    pub fn deref_and_add_value(&mut self, by: u8) {
        self.check_data_pointer();
        self.heap[self.data_pointer] = self.heap[self.data_pointer].wrapping_add(by);
    }

    pub fn deref_and_sub_value(&mut self, by: u8) {
        self.check_data_pointer();
        self.heap[self.data_pointer] = self.heap[self.data_pointer].wrapping_sub(by);
    }

    pub fn shift_data_pointer(&mut self, by: isize) {
        self.data_pointer = self.data_pointer.wrapping_add_signed(by);
        // we don't check that the data pointer is valid on shifts
        // we donly check that it's valid once it's read/written to
    }

    /// is the value at the data pointer zero?
    pub fn value_is_zero(&self) -> bool {
        self.check_data_pointer();
        self.heap[self.data_pointer] == 0
    }

    /// check if the data pointer is within bounds
    fn check_data_pointer(&self) {
        if self.data_pointer >= self.heap.len() {
            panic!("Data pointer ({}) out of bounds (max length {})", self.data_pointer, self.heap.len());
        }
    }
}

impl Runtime {
    pub fn new(heap_size: usize, in_stream: Box<dyn Read>, out_stream: Box<dyn Write>) -> Self {
        Self {
            data_pointer: 0,
            heap: vec![0; heap_size],
            in_stream,
            out_stream,
        }
    }
}
