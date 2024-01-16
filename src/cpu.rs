use crate::memory::Memory;
use crate::registers::*;

// first make this work, then make this better

#[allow(dead_code)]
pub struct CPU {
    sp: u16,
    pc: u16,
    cc: usize,
    mem: Memory,
    reg: Registers,
}

#[allow(dead_code)]
impl CPU {
    pub fn new() -> CPU {
        CPU {
            sp: 0xFFFE,
            pc: 0x100,
            cc: 0,
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
        self.cc += 4;
        data
    }

    fn execute(&mut self, opcode: u8) {
        let x = (opcode & 0b11000000) >> 6;
        let y = (opcode & 0b00111000) >> 3;
        let z = opcode & 0b00000111;
        let p = y >> 1;
        let q = y & 0b00000001;

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
                                // Load the data from SP register to 16 bit adress (nn)

                                let nn_lsb = self.fetch();
                                let nn_msb = self.fetch();
                                let nn = (nn_msb as u16) << 8 | nn_lsb as u16;
                                self.mem.write(nn as usize, (self.sp & 0x00FF) as u8);
                                self.mem.write(nn as usize, ((self.sp & 0xFF00) >> 8) as u8);
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
                                // Unconditional jump to releative address specified by signed
                                // 8-bit opearnd e
                                let e = self.fetch() as i8;
                                self.pc = self.pc.wrapping_add_signed(e as i16);
                            }

                            4..=7 => {
                                // JR cc, e
                                // Conditional jump to releative adress specified by the 8-bit
                                // signed integer
                                //
                                // You should fetch e even if condition is false

                                let e = self.fetch() as i8;
                                let cc = opcode & 0b00011000;

                                // Check the condition code Not-Z, Z (Zero), Not-C, or C (Carry)
                                match cc {
                                    0 => {
                                        // Condition code Not-Z
                                        let z = self.reg.get_flag(Flag::Z);
                                        if !z {
                                            self.pc = self.pc.wrapping_add_signed(e as i16);
                                        }
                                    }

                                    1 => {
                                        // Condition code Z
                                        let z = self.reg.get_flag(Flag::Z);
                                        if z {
                                            self.pc = self.pc.wrapping_add_signed(e as i16);
                                        }
                                    }

                                    2 => {
                                        // Condition code Not-C
                                        let c = self.reg.get_flag(Flag::C);
                                        if !c {
                                            self.pc = self.pc.wrapping_add_signed(e as i16);
                                        }
                                    }

                                    3 => {
                                        // Condition code C
                                        let c = self.reg.get_flag(Flag::C);
                                        if c {
                                            self.pc = self.pc.wrapping_add_signed(e as i16);
                                        }
                                    }

                                    _ => panic!("The condition code {} used in opcode {} is not implemented!", cc, opcode),
                                }
                            }
                            _ => panic!("The opcode '{}' is not implemented!", opcode),
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

                                let reg = match p {
                                    0 => Reg16::BC,
                                    1 => Reg16::DE,
                                    2 => Reg16::HL,
                                    3 => Reg16::SP,
                                    _ => panic!(
                                        "The register {} used in opcode {} is not implemented!",
                                        p, opcode
                                    ),
                                };

                                self.reg.set_reg16(reg, nn);
                            }

                            1 => {
                                // ADD HL rr
                                // Add the value in 16-bit register rr to HL register

                                let reg = match p {
                                    0 => Reg16::BC,
                                    1 => Reg16::DE,
                                    2 => Reg16::HL,
                                    3 => Reg16::SP,
                                    _ => panic!(
                                        "The register {} used in opcode {} is not implemented!",
                                        p, opcode
                                    ),
                                };

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
                                if sum.1 {
                                    self.reg.set_flag(Flag::C, true);
                                } else {
                                    self.reg.set_flag(Flag::C, false);
                                }

                                // Subtraction flag
                                self.reg.set_flag(Flag::N, false);

                                self.reg.set_reg16(Reg16::HL, rr);
                            }

                            _ => panic!("The opcode {} is not implemented!", opcode),
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

                                    _ => panic!("The opcode {} is not implemented!", opcode),
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

                                    _ => panic!("The opcode {} is not implemented!", opcode),
                                }
                            }

                            _ => panic!("The opcode {} is not implemeted!", opcode),
                        }
                    }

                    3 => {
                        let reg = match p {
                            0 => Reg16::BC,
                            1 => Reg16::DE,
                            2 => Reg16::HL,
                            3 => Reg16::SP,
                            _ => panic!("The opcode {} is not implemented!", opcode),
                        };

                        match q {
                            // INC rr
                            0 => self.reg.set_reg16(reg, self.reg.get_reg16(reg) + 1),

                            // DEC rr
                            1 => self.reg.set_reg16(reg, self.reg.get_reg16(reg) - 1),
                            _ => panic!("The opcode {} is not implemented!", opcode),
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
                            _ => panic!("The opcode {} is not implemented!", opcode),
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
                            _ => panic!("The opcode {} is not implemented!", opcode),
                        };

                        self.reg.set_reg8(reg, self.reg.get_reg8(reg) - 1);
                    }

                    6 => {
                        // LD r, n

                        let n = self.fetch();

                        if y == 6 {
                            self.mem
                                .write(self.reg.get_reg16(Reg16::HL) as usize, n);
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
                            _ => panic!("The opcode {} is not implemented!", opcode),
                        };

                        self.reg.set_reg8(reg, n);
                    }

                    7 => {
                        match y {
                            0 => {
                                // RLCA
                                let c: bool = (self.reg.get_reg8(Reg8::A) & 0x80) != 0;
                                let new_a = self.reg.get_reg8(Reg8::A) << 1 | c as u8;
                                self.reg.set_reg8(Reg8::A, new_a);
                                self.reg.set_flag(Flag::C, c);
                                self.reg.set_flag(Flag::Z, false);
                                self.reg.set_flag(Flag::H, false);
                                self.reg.set_flag(Flag::N, false);
                            }

                            1 => {
                                // RRCA
                                let c: bool = (self.reg.get_reg8(Reg8::A) & 0x01) != 0;
                                let new_a = self.reg.get_reg8(Reg8::A) >> 1 | (c as u8) << 7;
                                self.reg.set_reg8(Reg8::A, new_a);
                                self.reg.set_flag(Flag::C, c);
                                self.reg.set_flag(Flag::Z, false);
                                self.reg.set_flag(Flag::H, false);
                                self.reg.set_flag(Flag::N, false);
                            }

                            2 => {
                                // RLA
                                let c: bool = (self.reg.get_reg8(Reg8::A) & 0x80) != 0;
                                let new_a = self.reg.get_reg8(Reg8::A) << 1 | self.reg.get_flag(Flag::C) as u8;
                                self.reg.set_reg8(Reg8::A, new_a);
                                self.reg.set_flag(Flag::C, c);
                                self.reg.set_flag(Flag::Z, false);
                                self.reg.set_flag(Flag::H, false);
                                self.reg.set_flag(Flag::N, false);
                            }

                            3 => {
                                // RRA
                                let c: bool = (self.reg.get_reg8(Reg8::A) & 0x01) != 0;
                                let new_a = self.reg.get_reg8(Reg8::A) >> 1 | (self.reg.get_flag(Flag::C) as u8) << 7;
                                self.reg.set_reg8(Reg8::A, new_a);
                                self.reg.set_flag(Flag::C, c);
                                self.reg.set_flag(Flag::Z, false);
                                self.reg.set_flag(Flag::H, false);
                                self.reg.set_flag(Flag::N, false);
                            }

                            _ => panic!("The opcode {} is not implemented!", opcode),
                        }
                    }

                    _ => panic!("The opcode '{}' is not implemented!", opcode),
                }
            }
            _ => panic!("The opcode '{}' is not implemented!", opcode),
        }
    }

    fn execute_cb(&self, opcode: u8) {}
}
