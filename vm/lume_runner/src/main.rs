use std::fs::File;

use memmap2::Mmap;

const BYTECODE_LENGTH_SIZE: usize = std::mem::size_of::<u64>();

fn main() {
    let current_exe = std::env::current_exe().unwrap();
    let current_file = File::open(current_exe).unwrap();

    let mapped_file = unsafe { Mmap::map(&current_file).unwrap() };
    let bytecode_size_slice = mapped_file.last_chunk::<BYTECODE_LENGTH_SIZE>().unwrap();
    let bytecode_size = u64::from_ne_bytes(bytecode_size_slice.to_owned()) as usize;

    assert!(
        bytecode_size < mapped_file.len(),
        "no room for embedded bytecode ({bytecode_size} < {})",
        mapped_file.len()
    );

    let start_idx = mapped_file.len() - bytecode_size - BYTECODE_LENGTH_SIZE;
    let end_idx = start_idx + bytecode_size;

    let bytecode_byte_slice = mapped_file.get(start_idx..end_idx).unwrap();
    let bytecode_map: lume_mir::ModuleMap = postcard::from_bytes(bytecode_byte_slice).unwrap();

    let main_ptr = lume_jit::generate(bytecode_map).unwrap();
    let exit_code = main_ptr();

    std::process::exit(exit_code);
}
