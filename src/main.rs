mod memory;
mod registers;

mod cpu;
use cpu::CPU;

fn main() {
    let _ = CPU::new();
}
