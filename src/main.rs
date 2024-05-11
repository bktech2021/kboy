mod display;
mod memory;
mod registers;

mod machine;
use machine::Machine;

// neden herşey cpu.rs'te aq

fn main() {
    let file = include_bytes!("../test.bin");
    let mut a = Machine::new();
    a.mem.load_from(&file.to_vec(), 0);
    a.mem.write(0xFF01, 0xFF);
    loop {
        a.cpu_cycle();
        a.print();
    }
}
