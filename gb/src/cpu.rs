use core::panic;

use crate::memory::Memory;
use crate::registers::*;

// first make this work, then make this better

#[allow(dead_code)]
pub struct CPU {
    pc: u16,
    ime: bool,
    ime_scheduled: bool,
    mem: Memory,
    reg: Registers,
}

#[allow(dead_code)]
impl CPU {
    pub fn new() -> CPU {
        CPU {
            pc: 0x100,
            ime: false,
            ime_scheduled: false,
            mem: Memory::new(0xFFFF),
            reg: Registers::new(),
        }
    }

    pub fn cycle(&mut self) -> () {
        let opcode = self.fetch();
        if opcode == 0xCB {
            let opcode = self.fetch();
            self.execute_cb(opcode);
        } else {
            self.execute(opcode);
        }
    }

    fn fetch(&mut self) -> u8 {
        let data = self.mem.read(self.pc as usize);
        self.pc += 1;
        data
    }

    fn execute(&mut self, opcode: u8) {
        let x = (opcode & 0b11000000) >> 6;
        let y = (opcode & 0b00111000) >> 3;
        let z = opcode & 0b00000111;
        let p = y >> 1;
        let q = y & 0b00000001;

        // For unknown opcode
        macro_rules! unknown {
            ($opcode: expr) => {{
                panic!("The opcode '{:02X}' is not implemented!", opcode);
            }};
        }

        macro_rules! check_cc {
            ($cc: expr) => {{
                let cc = $cc;
                match cc {
                    0 => {
                        // Condition code Not-Z
                        let z = self.reg.get_flag(Flag::Z);
                        !z
                    }

                    1 => {
                        // Condition code Z
                        let z = self.reg.get_flag(Flag::Z);
                        z
                    }

                    2 => {
                        // Condition code Not-C
                        let c = self.reg.get_flag(Flag::C);
                        !c
                    }

                    3 => {
                        // Condition code C
                        let c = self.reg.get_flag(Flag::C);
                        c
                    }

                    _ => unknown!(opcode),
                }
            }};
        }

        macro_rules! match_rp {
            ($rr: expr) => {
                match $rr {
                    0 => Reg16::BC,
                    1 => Reg16::DE,
                    2 => Reg16::HL,
                    3 => Reg16::SP,
                    _ => unknown!(opcode),
                }
            };
        }

        macro_rules! match_rp2 {
            ($rr: expr) => {{
                match $rr {
                    0 => Reg16::BC,
                    1 => Reg16::DE,
                    2 => Reg16::HL,
                    3 => Reg16::AF,
                    _ => unknown!(opcode),
                }
            }};
        }

        match x {
            0 => {
                match z {
                    0 => {
                        match y {
                            0 => {
                                // NOP
                                // No operation. This opcode doesn't do anything
                            }

                            1 => {
                                // LD (nn), SP
                                // Load the data from SP register to 16 bit address (nn)

                                let nn_lsb = self.fetch();
                                let nn_msb = self.fetch();
                                let nn = (nn_msb as u16) << 8 | nn_lsb as u16;
                                self.mem.write(
                                    nn as usize,
                                    (self.reg.get_reg16(Reg16::SP) & 0x00FF) as u8,
                                );
                                self.mem.write(
                                    nn as usize,
                                    ((self.reg.get_reg16(Reg16::SP) & 0xFF00) >> 8) as u8,
                                );
                            }

                            2 => {
                                // STOP
                                // TODO: Implement that later
                                unimplemented!(
                                    "Opcode stop is still not implemented. Please do the work."
                                );
                            }

                            3 => {
                                // JR e
                                // Unconditional jump to relative address specified by signed
                                // 8-bit operand e
                                let e = self.fetch() as i8;
                                self.pc = self.pc.wrapping_add_signed(e as i16);
                            }

                            4..=7 => {
                                // JR cc, e
                                // Conditional jump to relative address specified by the 8-bit
                                // signed integer
                                //
                                // You should fetch e even if condition is false

                                let e = self.fetch() as i8;

                                // Check the condition code Not-Z, Z (Zero), Not-C, or C (Carry)
                                if check_cc!(4 - y) {
                                    self.pc = self.pc.wrapping_add_signed(e as i16);
                                }
                            }
                            _ => unknown!(opcode),
                        }
                    }

                    1 => {
                        match q {
                            0 => {
                                // LD rr, nn
                                // Load to 16 bit register rr from nn
                                let nn_lsb = self.fetch();
                                let nn_msb = self.fetch();
                                let nn = (nn_msb as u16) << 8 | nn_lsb as u16;

                                let reg = match_rp!(p);

                                self.reg.set_reg16(reg, nn);
                            }

                            1 => {
                                // ADD HL rr
                                // Add the value in 16-bit register rr to HL register

                                let reg = match_rp!(p);

                                let hl = self.reg.get_reg16(Reg16::HL);
                                let rr = self.reg.get_reg16(reg);

                                let sum = hl.overflowing_add(rr);

                                // Check half-carry
                                if ((hl & 0x0FF + rr & 0x0FFF) & 0x1000) == 0x1000 {
                                    self.reg.set_flag(Flag::H, true);
                                } else {
                                    self.reg.set_flag(Flag::H, false);
                                }

                                // Check carry
                                self.reg.set_flag(Flag::C, sum.1);

                                // Subtraction flag
                                self.reg.set_flag(Flag::N, false);

                                self.reg.set_reg16(Reg16::HL, rr);
                            }

                            _ => unknown!(opcode),
                        }
                    }

                    2 => {
                        match q {
                            // The opcodes where the source is A
                            0 => {
                                match p {
                                    // LD [BC], A
                                    0 => self.mem.write(
                                        self.reg.get_reg16(Reg16::BC) as usize,
                                        self.reg.get_reg8(Reg8::A),
                                    ),

                                    // LD [DE], A
                                    1 => self.mem.write(
                                        self.reg.get_reg16(Reg16::BC) as usize,
                                        self.reg.get_reg8(Reg8::A),
                                    ),

                                    // LD [HL+], A
                                    // Note: increment HL after memory operation
                                    2 => {
                                        self.mem.write(
                                            self.reg.get_reg16(Reg16::HL) as usize,
                                            self.reg.get_reg8(Reg8::A),
                                        );
                                        self.reg.set_reg16(
                                            Reg16::HL,
                                            self.reg.get_reg16(Reg16::HL) + 1,
                                        );
                                    }

                                    // LD [HL-], A
                                    // Note: decrement HL after memory operation
                                    3 => {
                                        self.mem.write(
                                            self.reg.get_reg16(Reg16::HL) as usize,
                                            self.reg.get_reg8(Reg8::A),
                                        );
                                        self.reg.set_reg16(
                                            Reg16::HL,
                                            self.reg.get_reg16(Reg16::HL) - 1,
                                        );
                                    }

                                    _ => unknown!(opcode),
                                }
                            }

                            // The opcodes where destination is A
                            1 => {
                                match p {
                                    // LD A, [BC]
                                    0 => self.reg.set_reg8(
                                        Reg8::A,
                                        self.mem.read(self.reg.get_reg16(Reg16::BC) as usize),
                                    ),

                                    // LD A, [DE]
                                    1 => self.reg.set_reg8(
                                        Reg8::A,
                                        self.mem.read(self.reg.get_reg16(Reg16::DE) as usize),
                                    ),

                                    // LD A, [HL+]
                                    // Note: increment HL after memory operation
                                    2 => {
                                        self.reg.set_reg8(
                                            Reg8::A,
                                            self.mem.read(self.reg.get_reg16(Reg16::HL) as usize),
                                        );
                                        self.reg.set_reg16(
                                            Reg16::HL,
                                            self.reg.get_reg16(Reg16::HL) + 1,
                                        );
                                    }

                                    // LD A, [HL-]
                                    // Note, decrement HL after memory operations
                                    3 => {
                                        self.reg.set_reg8(
                                            Reg8::A,
                                            self.mem.read(self.reg.get_reg16(Reg16::HL) as usize),
                                        );
                                        self.reg.set_reg16(
                                            Reg16::HL,
                                            self.reg.get_reg16(Reg16::HL) - 1,
                                        );
                                    }

                                    _ => unknown!(opcode),
                                }
                            }

                            _ => unknown!(opcode),
                        }
                    }

                    3 => {
                        let reg = match_rp!(p);

                        match q {
                            // INC rr
                            0 => self.reg.set_reg16(reg, self.reg.get_reg16(reg) + 1),

                            // DEC rr
                            1 => self.reg.set_reg16(reg, self.reg.get_reg16(reg) - 1),
                            _ => unknown!(opcode),
                        }
                    }

                    4 => {
                        // INC r
                        if y == 6 {
                            self.mem.write(
                                self.reg.get_reg16(Reg16::HL) as usize,
                                self.mem.read(self.reg.get_reg16(Reg16::HL) as usize) + 1,
                            );
                            return;
                        }

                        let reg = match y {
                            0 => Reg8::B,
                            1 => Reg8::C,
                            2 => Reg8::D,
                            3 => Reg8::E,
                            4 => Reg8::H,
                            5 => Reg8::L,
                            7 => Reg8::A,
                            _ => unknown!(opcode),
                        };

                        self.reg.set_reg8(reg, self.reg.get_reg8(reg) + 1);
                    }

                    5 => {
                        // DEC r
                        if y == 6 {
                            self.mem.write(
                                self.reg.get_reg16(Reg16::HL) as usize,
                                self.mem.read(self.reg.get_reg16(Reg16::HL) as usize) - 1,
                            );
                            return;
                        }

                        let reg = match y {
                            0 => Reg8::B,
                            1 => Reg8::C,
                            2 => Reg8::D,
                            3 => Reg8::E,
                            4 => Reg8::H,
                            5 => Reg8::L,
                            7 => Reg8::A,
                            _ => unknown!(opcode),
                        };

                        self.reg.set_reg8(reg, self.reg.get_reg8(reg) - 1);
                    }

                    6 => {
                        // LD r, n

                        let n = self.fetch();

                        if y == 6 {
                            self.mem.write(self.reg.get_reg16(Reg16::HL) as usize, n);
                            return;
                        }

                        let reg = match y {
                            0 => Reg8::B,
                            1 => Reg8::C,
                            2 => Reg8::D,
                            3 => Reg8::E,
                            4 => Reg8::H,
                            5 => Reg8::L,
                            7 => Reg8::A,
                            _ => unknown!(opcode),
                        };

                        self.reg.set_reg8(reg, n);
                    }

                    7 => {
                        match y {
                            // FIXME: probably something wrong down there
                            // not sure about carry flag
                            0 => {
                                // RLCA
                                let c: bool = self.reg.get_flag(Flag::C);
                                let new_a = self.reg.get_reg8(Reg8::A) << 1 | c as u8;
                                self.reg.set_reg8(Reg8::A, new_a);
                                self.reg.set_flag(Flag::C, c);
                                self.reg.set_flag(Flag::Z, false);
                                self.reg.set_flag(Flag::H, false);
                                self.reg.set_flag(Flag::N, false);
                            }

                            1 => {
                                // RRCA
                                let c: bool = self.reg.get_flag(Flag::C);
                                let new_a = self.reg.get_reg8(Reg8::A) >> 1 | (c as u8) << 7;
                                self.reg.set_reg8(Reg8::A, new_a);
                                self.reg.set_flag(Flag::C, c);
                                self.reg.set_flag(Flag::Z, false);
                                self.reg.set_flag(Flag::H, false);
                                self.reg.set_flag(Flag::N, false);
                            }

                            2 => {
                                // RLA
                                let c: bool = self.reg.get_flag(Flag::C);
                                let new_a = self.reg.get_reg8(Reg8::A) << 1
                                    | self.reg.get_flag(Flag::C) as u8;
                                self.reg.set_reg8(Reg8::A, new_a);
                                self.reg.set_flag(Flag::C, c);
                                self.reg.set_flag(Flag::Z, false);
                                self.reg.set_flag(Flag::H, false);
                                self.reg.set_flag(Flag::N, false);
                            }

                            3 => {
                                // RRA
                                let c: bool = self.reg.get_flag(Flag::C);
                                let new_a = self.reg.get_reg8(Reg8::A) >> 1
                                    | (self.reg.get_flag(Flag::C) as u8) << 7;
                                self.reg.set_reg8(Reg8::A, new_a);
                                self.reg.set_flag(Flag::C, c);
                                self.reg.set_flag(Flag::Z, false);
                                self.reg.set_flag(Flag::H, false);
                                self.reg.set_flag(Flag::N, false);
                            }

                            4 => {
                                // DAA
                                let mut correction = 0;
                                let src = self.reg.get_reg8(Reg8::A);
                                if (self.reg.get_flag(Flag::H))
                                    || !self.reg.get_flag(Flag::N) && ((src & 0x0F) > 0x09)
                                {
                                    correction |= 0x06;
                                }
                                if (self.reg.get_flag(Flag::C))
                                    || !self.reg.get_flag(Flag::N) && ((src & 0xFF) > 0x99)
                                {
                                    correction |= 0x60;
                                    self.reg.set_flag(Flag::C, true);
                                }
                                if !self.reg.get_flag(Flag::N) {
                                    self.reg.set_reg8(Reg8::A, src.wrapping_add(correction));
                                    self.reg
                                        .set_flag(Flag::Z, src.wrapping_add(correction) == 0);
                                } else {
                                    self.reg.set_reg8(Reg8::A, src.wrapping_sub(correction));
                                    self.reg
                                        .set_flag(Flag::Z, src.wrapping_sub(correction) == 0);
                                }
                                self.reg.set_flag(Flag::H, false);
                            }

                            5 => {
                                // CPL
                                self.reg.set_reg8(Reg8::A, !self.reg.get_reg8(Reg8::A));
                                self.reg.set_flag(Flag::H, true);
                                self.reg.set_flag(Flag::N, true);
                            }

                            6 => {
                                // SCF
                                self.reg.set_flag(Flag::C, true);
                                self.reg.set_flag(Flag::H, false);
                                self.reg.set_flag(Flag::N, false);
                            }

                            _ => unknown!(opcode),
                        }
                    }

                    _ => unknown!(opcode),
                }
            }
            1 => {
                if (z == 6) & (y == 6) {
                    // LD r, r
                    if y == 6 {
                        if z == 6 {
                            let n = self.mem.read(self.reg.get_reg16(Reg16::HL) as usize);
                            self.mem.write(self.reg.get_reg16(Reg16::HL) as usize, n);
                            return;
                        }
                        let src = self.mem.read(self.reg.get_reg16(Reg16::HL) as usize);
                        let dst = match z {
                            0 => Reg8::B,
                            1 => Reg8::C,
                            2 => Reg8::D,
                            3 => Reg8::E,
                            4 => Reg8::H,
                            5 => Reg8::L,
                            7 => Reg8::A,
                            _ => unknown!(opcode),
                        };
                        self.reg.set_reg8(dst, src);
                        return;
                    }

                    let src = match y {
                        0 => Reg8::B,
                        1 => Reg8::C,
                        2 => Reg8::D,
                        3 => Reg8::E,
                        4 => Reg8::H,
                        5 => Reg8::L,
                        7 => Reg8::A,
                        _ => unknown!(opcode),
                    };
                    let dst = match z {
                        0 => Reg8::B,
                        1 => Reg8::C,
                        2 => Reg8::D,
                        3 => Reg8::E,
                        4 => Reg8::H,
                        5 => Reg8::L,
                        7 => Reg8::A,
                        _ => unknown!(opcode),
                    };

                    self.reg.set_reg8(dst, self.reg.get_reg8(src));
                    return;
                }

                // HALT
                unimplemented!("halt lol");
            }
            2 => {
                // alu[y] r[z]
                let val: u8;
                let reg = match z {
                    0 => Reg8::B,
                    1 => Reg8::C,
                    2 => Reg8::D,
                    3 => Reg8::E,
                    4 => Reg8::H,
                    5 => Reg8::L,
                    7 => Reg8::A,
                    _ => unknown!(opcode),
                };

                if z != 6 {
                    val = self.reg.get_reg8(reg);
                } else {
                    val = self.mem.read(self.reg.get_reg16(Reg16::HL) as usize);
                }

                match y {
                    0 => {
                        // ADD A, r
                        let a = self.reg.get_reg8(Reg8::A);
                        let sum = a.overflowing_add(val);
                        self.reg.set_flag(Flag::C, sum.1);
                        self.reg
                            .set_flag(Flag::H, (a & 0x0F + val & 0x0F) & 0x10 == 0x10);
                        self.reg.set_flag(Flag::N, false);
                        self.reg.set_flag(Flag::Z, sum.0 == 0);
                    }

                    1 => {
                        // ADC A, r
                        let src = self.reg.get_reg8(reg);
                        let cy = self.reg.get_flag(Flag::C) as u8;
                        let add = self.reg.get_reg8(Reg8::A);
                        let sum = add.wrapping_add(src).wrapping_add(cy);
                        self.reg.set_reg8(Reg8::A, sum);
                        self.reg.set_flag(Flag::Z, sum == 0);
                        self.reg.set_flag(Flag::N, false);
                        self.reg
                            .set_flag(Flag::H, (((src & 0x0F) + (add & 0x0F) + cy) & 0x10) != 0);
                        self.reg.set_flag(
                            Flag::C,
                            ((src as u16 + add as u16 + cy as u16) & 0x0100) != 0,
                        );
                    }

                    2 => {
                        // SUB r
                        let a = self.reg.get_reg8(Reg8::A);
                        let sub = a.overflowing_sub(val);
                        self.reg.set_reg8(Reg8::A, sub.0);
                        self.reg.set_flag(Flag::Z, sub.0 == 0);
                        self.reg.set_flag(Flag::N, true);
                        self.reg
                            .set_flag(Flag::H, (a & 0x0F - val & 0x0F) & 0x10 == 0x10);
                        self.reg.set_flag(Flag::C, sub.1);
                    }

                    3 => {
                        // SBC A, r
                        let src = self.reg.get_reg8(reg);
                        let cy = self.reg.get_flag(Flag::C) as u8;
                        let sub = self.reg.get_reg8(Reg8::A);
                        let sum = sub.wrapping_sub(src).wrapping_sub(cy);
                        self.reg.set_reg8(Reg8::A, sum);
                        self.reg.set_flag(Flag::Z, sum == 0);
                        self.reg.set_flag(Flag::N, true);
                        self.reg
                            .set_flag(Flag::H, (((sub & 0x0F) - (src & 0x0F) - cy) & 0x10) != 0);
                        self.reg.set_flag(
                            Flag::C,
                            ((sub as u16 - src as u16 - cy as u16) & 0x0100) != 0,
                        );
                    }

                    4 => {
                        // AND r
                        let a = self.reg.get_reg8(Reg8::A);
                        let and = a & val;
                        self.reg.set_reg8(Reg8::A, and);
                        self.reg.set_flag(Flag::Z, and == 0);
                        self.reg.set_flag(Flag::N, false);
                        self.reg.set_flag(Flag::H, true);
                        self.reg.set_flag(Flag::C, false);
                    }

                    5 => {
                        // XOR r
                        let a = self.reg.get_reg8(Reg8::A);
                        let xor = a ^ val;
                        self.reg.set_reg8(Reg8::A, xor);
                        self.reg.set_flag(Flag::Z, xor == 0);
                        self.reg.set_flag(Flag::N, false);
                        self.reg.set_flag(Flag::H, false);
                        self.reg.set_flag(Flag::C, false);
                    }

                    6 => {
                        // OR r
                        let a = self.reg.get_reg8(Reg8::A);
                        let or = a | val;
                        self.reg.set_reg8(Reg8::A, or);
                        self.reg.set_flag(Flag::Z, or == 0);
                        self.reg.set_flag(Flag::N, false);
                        self.reg.set_flag(Flag::H, false);
                        self.reg.set_flag(Flag::C, false);
                    }

                    7 => {
                        // CP r
                        let a = self.reg.get_reg8(Reg8::A);
                        let sub = a.overflowing_sub(val);
                        self.reg.set_flag(Flag::Z, sub.0 == 0);
                        self.reg.set_flag(Flag::N, true);
                        self.reg
                            .set_flag(Flag::H, (a & 0x0F - val & 0x0F) & 0x10 == 0x10);
                        self.reg.set_flag(Flag::C, sub.1);
                    }

                    _ => unknown!(opcode),
                }
            }
            3 => {
                match z {
                    0 => {
                        match y {
                            0..=3 => {
                                // RET cc
                                // Return if condition code cc is true
                                if check_cc!(y) {
                                    let lsb = self.mem.read(self.reg.get_reg16(Reg16::SP) as usize);
                                    let msb =
                                        self.mem.read(self.reg.get_reg16(Reg16::SP) as usize + 1);
                                    self.pc = (msb as u16) << 8 | lsb as u16;
                                    self.reg
                                        .set_reg16(Reg16::SP, self.reg.get_reg16(Reg16::SP) + 2);
                                }
                            }

                            4 => {
                                // LD (0xFF00 + n), A
                                let a = self.reg.get_reg8(Reg8::A);
                                let n = self.fetch();
                                self.mem.write((n as u16 + 0xFF00) as usize, a);
                            }

                            5 => {
                                // ADD SP, d
                                let sp = self.reg.get_reg16(Reg16::SP);
                                let n = self.fetch();
                                let sum = sp.overflowing_add_signed(n as i16);

                                // Check half-carry
                                self.reg.set_flag(
                                    Flag::H,
                                    (((sp & 0xFFF) + (n as u16 & 0xFFF)) & 0x1000) == 0x1000,
                                );

                                // Check carry
                                self.reg.set_flag(Flag::C, sum.1);

                                // Subtraction flag
                                self.reg.set_flag(Flag::N, false);

                                self.reg.set_reg16(Reg16::SP, sum.0);
                            }

                            6 => {
                                // LD A, (0xFF00 + n)
                                let n = self.fetch();
                                self.reg
                                    .set_reg8(Reg8::A, self.mem.read((n as u16 + 0xFF00) as usize));
                            }

                            7 => {
                                // LD HL, SP + d
                                let sp = self.reg.get_reg16(Reg16::SP);
                                let n = self.fetch();
                                let sum = sp.overflowing_add_signed(n as i16);
                                self.reg.set_reg16(Reg16::HL, sum.0);
                                self.reg.set_flag(Flag::C, sum.1);
                                self.reg.set_flag(
                                    Flag::H,
                                    (((sp & 0xFFF) + (n as u16 & 0xFFF)) & 0x1000) == 0x1000,
                                );
                            }

                            _ => unknown!(opcode),
                        }
                    }
                    1 => {
                        match q {
                            0 => {
                                // POP rr
                                let sp = self.reg.get_reg16(Reg16::SP);
                                let lsb = self.mem.read(sp as usize);
                                let msb = self.mem.read(sp as usize + 1);
                                let rr = match_rp2!(p);
                                self.reg.set_reg16(rr, (msb as u16) << 8 | lsb as u16);
                                self.reg
                                    .set_reg16(Reg16::SP, self.reg.get_reg16(Reg16::SP) + 2);
                            }
                            1 => match p {
                                0 => {
                                    // RET
                                    let lsb = self.mem.read(self.reg.get_reg16(Reg16::SP) as usize);
                                    let msb =
                                        self.mem.read(self.reg.get_reg16(Reg16::SP) as usize + 1);
                                    self.pc = (msb as u16) << 8 | lsb as u16;
                                    self.reg
                                        .set_reg16(Reg16::SP, self.reg.get_reg16(Reg16::SP) + 2);
                                }

                                1 => {
                                    // RETI
                                    let lsb = self.mem.read(self.reg.get_reg16(Reg16::SP) as usize);
                                    let msb =
                                        self.mem.read(self.reg.get_reg16(Reg16::SP) as usize + 1);
                                    self.pc = (msb as u16) << 8 | lsb as u16;
                                    self.reg
                                        .set_reg16(Reg16::SP, self.reg.get_reg16(Reg16::SP) + 2);
                                    self.ime = true;
                                }

                                2 => {
                                    // JP HL
                                    self.pc = self.reg.get_reg16(Reg16::HL);
                                }

                                3 => {
                                    // LD SP, HL
                                    self.reg.set_reg16(Reg16::SP, self.reg.get_reg16(Reg16::HL));
                                }

                                _ => unknown!(opcode),
                            },
                            _ => unknown!(opcode),
                        }
                    }
                    2 => {
                        match y {
                            0..=3 => {
                                // JP cc, nn
                                let nn_lsb = self.fetch();
                                let nn_msb = self.fetch();
                                let nn = (nn_msb as u16) << 8 | nn_lsb as u16;
                                if check_cc!(y) {
                                    self.pc = nn;
                                }
                            }

                            4 => {
                                // LD (0xFF00 + C), A
                                let a = self.reg.get_reg8(Reg8::A);
                                let c = self.reg.get_reg8(Reg8::C);
                                self.mem.write((c as u16 + 0xFF00) as usize, a);
                            }

                            5 => {
                                // LD (nn), A
                                let nn_lsb = self.fetch();
                                let nn_msb = self.fetch();
                                let nn = (nn_msb as u16) << 8 | nn_lsb as u16;
                                let a = self.reg.get_reg8(Reg8::A);
                                self.mem.write(nn as usize, a);
                            }

                            6 => {
                                // LD A, (0xFF00 + C)
                                let c = self.reg.get_reg8(Reg8::C);
                                self.reg
                                    .set_reg8(Reg8::A, self.mem.read((c as u16 + 0xFF00) as usize));
                            }

                            7 => {
                                // LD A, nn
                                let nn_lsb = self.fetch();
                                let nn_msb = self.fetch();
                                let nn = (nn_msb as u16) << 8 | nn_lsb as u16;
                                self.reg.set_reg8(Reg8::A, self.mem.read(nn as usize));
                            }

                            _ => unknown!(opcode),
                        }
                    }
                    3 => {
                        match y {
                            0 => {
                                // JP nn
                                let nn_lsb = self.fetch();
                                let nn_msb = self.fetch();
                                let nn = (nn_msb as u16) << 8 | nn_lsb as u16;
                                self.pc = nn;
                            }

                            1 => {
                                // CB prefix
                                let opcode = self.fetch();
                                self.execute_cb(opcode);
                            }

                            6 => {
                                // DI
                                self.ime = false;
                            }

                            7 => {
                                // EI
                                self.ime_scheduled = true;
                            }

                            _ => unknown!(opcode),
                        }
                    }
                    4 => {
                        match y {
                            0..=3 => {
                                // CALL cc, nn
                                let nn_lsb = self.fetch();
                                let nn_msb = self.fetch();
                                let nn = (nn_msb as u16) << 8 | nn_lsb as u16;
                                if check_cc!(y) {
                                    let mut sp = self.reg.get_reg16(Reg16::SP);
                                    self.mem.write((sp - 1) as usize, (self.pc >> 8) as u8);
                                    self.mem.write((sp - 2) as usize, (self.pc & 0xFF) as u8);
                                    sp -= 2;
                                    self.reg.set_reg16(Reg16::SP, sp);
                                    self.pc = nn;
                                }
                            }
                            _ => unknown!(opcode),
                        }
                    }
                    5 => {
                        match q {
                            0 => {
                                // PUSH rr
                                let rr = match_rp2!(p);
                                let reg = self.reg.get_reg16(rr);
                                let mut sp = self.reg.get_reg16(Reg16::SP);
                                self.mem.write((sp - 1) as usize, (reg >> 8) as u8);
                                self.mem.write((sp - 2) as usize, (reg & 0xFF) as u8);
                                sp -= 2;
                                self.reg.set_reg16(Reg16::SP, sp);
                            }

                            1 => {
                                match p {
                                    0 => {
                                        // CALL nn
                                        let nn_lsb = self.fetch();
                                        let nn_msb = self.fetch();
                                        let nn = (nn_msb as u16) << 8 | nn_lsb as u16;
                                        let mut sp = self.reg.get_reg16(Reg16::SP);
                                        self.mem.write((sp - 1) as usize, (self.pc >> 8) as u8);
                                        self.mem.write((sp - 2) as usize, (self.pc & 0xFF) as u8);
                                        sp -= 2;
                                        self.reg.set_reg16(Reg16::SP, sp);
                                        self.pc = nn;
                                    }
                                    _ => unknown!(opcode),
                                }
                            }

                            _ => unknown!(opcode),
                        }
                    }
                    6 => {
                        let val = self.fetch();
                        match y {
                            0 => {
                                // ADD A, r
                                let a = self.reg.get_reg8(Reg8::A);
                                let sum = a.overflowing_add(val);
                                self.reg.set_flag(Flag::C, sum.1);
                                self.reg
                                    .set_flag(Flag::H, (a & 0x0F + val & 0x0F) & 0x10 == 0x10);
                                self.reg.set_flag(Flag::N, false);
                                self.reg.set_flag(Flag::Z, sum.0 == 0);
                            }

                            1 => {
                                // ADC A, r
                                let src = val;
                                let cy = self.reg.get_flag(Flag::C) as u8;
                                let add = self.reg.get_reg8(Reg8::A);
                                let sum = add.wrapping_add(src).wrapping_add(cy);
                                self.reg.set_reg8(Reg8::A, sum);
                                self.reg.set_flag(Flag::Z, sum == 0);
                                self.reg.set_flag(Flag::N, false);
                                self.reg.set_flag(
                                    Flag::H,
                                    (((src & 0x0F) + (add & 0x0F) + cy) & 0x10) != 0,
                                );
                                self.reg.set_flag(
                                    Flag::C,
                                    ((src as u16 + add as u16 + cy as u16) & 0x0100) != 0,
                                );
                            }

                            2 => {
                                // SUB r
                                let a = self.reg.get_reg8(Reg8::A);
                                let sub = a.overflowing_sub(val);
                                self.reg.set_reg8(Reg8::A, sub.0);
                                self.reg.set_flag(Flag::Z, sub.0 == 0);
                                self.reg.set_flag(Flag::N, true);
                                self.reg
                                    .set_flag(Flag::H, (a & 0x0F - val & 0x0F) & 0x10 == 0x10);
                                self.reg.set_flag(Flag::C, sub.1);
                            }

                            3 => {
                                // SBC A, r
                                let src = val;
                                let cy = self.reg.get_flag(Flag::C) as u8;
                                let sub = self.reg.get_reg8(Reg8::A);
                                let sum = sub.wrapping_sub(src).wrapping_sub(cy);
                                self.reg.set_reg8(Reg8::A, sum);
                                self.reg.set_flag(Flag::Z, sum == 0);
                                self.reg.set_flag(Flag::N, true);
                                self.reg.set_flag(
                                    Flag::H,
                                    (((sub & 0x0F) - (src & 0x0F) - cy) & 0x10) != 0,
                                );
                                self.reg.set_flag(
                                    Flag::C,
                                    ((sub as u16 - src as u16 - cy as u16) & 0x0100) != 0,
                                );
                            }

                            4 => {
                                // AND r
                                let a = self.reg.get_reg8(Reg8::A);
                                let and = a & val;
                                self.reg.set_reg8(Reg8::A, and);
                                self.reg.set_flag(Flag::Z, and == 0);
                                self.reg.set_flag(Flag::N, false);
                                self.reg.set_flag(Flag::H, true);
                                self.reg.set_flag(Flag::C, false);
                            }

                            5 => {
                                // XOR r
                                let a = self.reg.get_reg8(Reg8::A);
                                let xor = a ^ val;
                                self.reg.set_reg8(Reg8::A, xor);
                                self.reg.set_flag(Flag::Z, xor == 0);
                                self.reg.set_flag(Flag::N, false);
                                self.reg.set_flag(Flag::H, false);
                                self.reg.set_flag(Flag::C, false);
                            }

                            6 => {
                                // OR r
                                let a = self.reg.get_reg8(Reg8::A);
                                let or = a | val;
                                self.reg.set_reg8(Reg8::A, or);
                                self.reg.set_flag(Flag::Z, or == 0);
                                self.reg.set_flag(Flag::N, false);
                                self.reg.set_flag(Flag::H, false);
                                self.reg.set_flag(Flag::C, false);
                            }

                            7 => {
                                // CP r
                                let a = self.reg.get_reg8(Reg8::A);
                                let sub = a.overflowing_sub(val);
                                self.reg.set_flag(Flag::Z, sub.0 == 0);
                                self.reg.set_flag(Flag::N, true);
                                self.reg
                                    .set_flag(Flag::H, (a & 0x0F - val & 0x0F) & 0x10 == 0x10);
                                self.reg.set_flag(Flag::C, sub.1);
                            }

                            _ => unknown!(opcode),
                        }
                    }
                    7 => {
                        // RST y * 8
                        let mut sp = self.reg.get_reg16(Reg16::SP);
                        self.mem.write((sp - 1) as usize, (self.pc >> 8) as u8);
                        self.mem.write((sp - 2) as usize, (self.pc & 0xFF) as u8);
                        sp -= 2;
                        self.reg.set_reg16(Reg16::SP, sp);
                        self.pc = y as u16 * 8;
                    }
                    _ => unknown!(opcode),
                }
            }
            _ => unknown!(opcode),
        }
    }

    fn execute_cb(&mut self, opcode: u8) {
        let x = (opcode & 0b11000000) >> 6;
        let y = (opcode & 0b00111000) >> 3;
        let z = opcode & 0b00000111;

        macro_rules! unknown {
            ($opcode: expr) => {{
                panic!("Unknown opcode: {:02X}", $opcode);
            }};
        }

        match x {
            0 => {
                let reg = match z {
                    0 => Reg8::B,
                    1 => Reg8::C,
                    2 => Reg8::D,
                    3 => Reg8::E,
                    4 => Reg8::H,
                    5 => Reg8::L,
                    7 => Reg8::A,
                    _ => unknown!(opcode),
                };

                let regv = match z {
                    6 => self.mem.read(self.reg.get_reg16(Reg16::HL) as usize),
                    _ => self.reg.get_reg8(reg),
                };

                let n_regv = match y {
                    0 => {
                        // RLC r
                        let c = regv & 0x80 == 0x80;
                        let new_regv = regv << 1 | c as u8;
                        self.reg.set_flag(Flag::C, c);
                        self.reg.set_flag(Flag::Z, new_regv == 0);
                        self.reg.set_flag(Flag::H, false);
                        self.reg.set_flag(Flag::N, false);

                        new_regv
                    }

                    1 => {
                        // RRC
                        let c = regv & 0x01 == 0x01;
                        let new_regv = regv >> 1 | (c as u8) << 7;
                        self.reg.set_flag(Flag::C, c);
                        self.reg.set_flag(Flag::Z, new_regv == 0);
                        self.reg.set_flag(Flag::H, false);
                        self.reg.set_flag(Flag::N, false);

                        new_regv
                    }

                    2 => {
                        // RL
                        let c = regv & 0x80 == 0x80;
                        let new_regv = regv << 1 | self.reg.get_flag(Flag::C) as u8;
                        self.reg.set_flag(Flag::C, c);
                        self.reg.set_flag(Flag::Z, new_regv == 0);
                        self.reg.set_flag(Flag::H, false);
                        self.reg.set_flag(Flag::N, false);

                        new_regv
                    }

                    3 => {
                        // RR
                        let c = regv & 0x01 == 0x01;
                        let new_regv = regv >> 1 | (self.reg.get_flag(Flag::C) as u8) << 7;
                        self.reg.set_reg8(reg, new_regv);
                        self.reg.set_flag(Flag::C, c);
                        self.reg.set_flag(Flag::Z, new_regv == 0);
                        self.reg.set_flag(Flag::H, false);
                        self.reg.set_flag(Flag::N, false);

                        new_regv
                    }

                    4 => {
                        // SLA
                        let c = regv & 0x80 == 0x80;
                        let new_regv = regv << 1;
                        self.reg.set_flag(Flag::C, c);
                        self.reg.set_flag(Flag::Z, new_regv == 0);
                        self.reg.set_flag(Flag::H, false);
                        self.reg.set_flag(Flag::N, false);

                        new_regv
                    }

                    5 => {
                        // SRA
                        let c = regv & 0x01 == 0x01;
                        let new_regv = regv >> 1 | (regv & 0x80);
                        self.reg.set_flag(Flag::C, c);
                        self.reg.set_flag(Flag::Z, new_regv == 0);
                        self.reg.set_flag(Flag::H, false);
                        self.reg.set_flag(Flag::N, false);

                        new_regv
                    }

                    6 => {
                        // SWAP
                        let new_regv = (regv & 0x0F) << 4 | (regv & 0xF0) >> 4;
                        
                        new_regv
                    }

                    7 => {
                        let c = regv & 0x01 == 0x01;
                        let new_regv = regv >> 1;
                        self.reg.set_flag(Flag::C, c);
                        self.reg.set_flag(Flag::Z, new_regv == 0);
                        self.reg.set_flag(Flag::H, false);
                        self.reg.set_flag(Flag::N, false);

                        new_regv
                    }
                    _ => unknown!(opcode),
                
                };
            
                match z {
                    6 => self.mem.write(self.reg.get_reg16(Reg16::HL) as usize, n_regv),
                    _ => self.reg.set_reg8(reg, n_regv),
                }
            }
            
            1 => {
                // BIT y, r[z]
                let reg = match z {
                    0 => Reg8::B,
                    1 => Reg8::C,
                    2 => Reg8::D,
                    3 => Reg8::E,
                    4 => Reg8::H,
                    5 => Reg8::L,
                    7 => Reg8::A,
                    _ => unknown!(opcode),
                };

                let regv = match z {
                    6 => self.mem.read(self.reg.get_reg16(Reg16::HL) as usize),
                    _ => self.reg.get_reg8(reg),
                };

                let bit = regv & (1 << y) != 0;

                self.reg.set_flag(Flag::Z, bit);
                self.reg.set_flag(Flag::H, true);
                self.reg.set_flag(Flag::N, false);   
            }

            2 => {
                // RES y, r[z]
                let reg = match z {
                    0 => Reg8::B,
                    1 => Reg8::C,
                    2 => Reg8::D,
                    3 => Reg8::E,
                    4 => Reg8::H,
                    5 => Reg8::L,
                    7 => Reg8::A,
                    _ => unknown!(opcode),
                };

                let regv = match z {
                    6 => self.mem.read(self.reg.get_reg16(Reg16::HL) as usize),
                    _ => self.reg.get_reg8(reg),
                };

                let n_regv = regv & !(1 << y);

                match z {
                    6 => self.mem.write(self.reg.get_reg16(Reg16::HL) as usize, n_regv),
                    _ => self.reg.set_reg8(reg, n_regv),
                }
            }

            3 => {
                // SET y, r[z]
                let reg = match z {
                    0 => Reg8::B,
                    1 => Reg8::C,
                    2 => Reg8::D,
                    3 => Reg8::E,
                    4 => Reg8::H,
                    5 => Reg8::L,
                    7 => Reg8::A,
                    _ => unknown!(opcode),
                };

                let regv = match z {
                    6 => self.mem.read(self.reg.get_reg16(Reg16::HL) as usize),
                    _ => self.reg.get_reg8(reg),
                };

                let n_regv = regv | (1 << y);

                match z {
                    6 => self.mem.write(self.reg.get_reg16(Reg16::HL) as usize, n_regv),
                    _ => self.reg.set_reg8(reg, n_regv),
                }
            }

            _ => unknown!(opcode),
        }
    }

    fn m_cycle(&mut self) {}
}
