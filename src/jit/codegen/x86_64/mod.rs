mod codegen;
mod instruction;
mod operand_encoding;
mod ops;
mod registers;

fn value_fits_in_i8(value: usize) -> bool {
    value <= (i8::MAX as usize) || (((!value) & i8::MIN as usize) == 0)
}

fn value_fits_in_i32(value: usize) -> bool {
    value <= (i32::MAX as usize) || (((!value) & i32::MIN as usize) == 0)
}
