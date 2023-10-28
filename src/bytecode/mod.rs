pub mod bytecode;

/// A very simple bytecode set of instructions
#[derive(Clone, Debug, PartialEq)]
pub enum ByteCode {
    /// Add the given value to the data pointer
    Add(isize),

    /// Dereference the data pointer and add to the heap value
    DerefAdd(u8),

    /// Dereference the data pointer and subtract from the heap value
    DerefSub(u8),

    /// Read bytes into the heap from the data pointer
    Read(usize),

    /// Write bytes from the heap relative to the data pointer
    Write(usize),

    /// Jump instructions forwards if zero
    JumpForwardsIfZero(usize),

    /// Jump instructions backwards if zero
    JumpBackwardsIfNonZero(usize),
}
