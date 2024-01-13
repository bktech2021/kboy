use crate::memory::Memory;
use crate::registers::*;

// TODO: Use the registers.rs

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
                                unimplemented!("Opcode stop is still not implemented. Please do the work.");
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

                                    _ => unimplemented!("The condition code {} used in opcode {} is not implemented!", cc, opcode),
                                }
                            }
                            _ => unimplemented!("The opcode '{}' is not implemented!", opcode),
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
                                match p {
                                    0 => {
                                        // BC register
                                        self.reg.set_reg16(Reg16::BC, nn);
                                    }

                                    1 => {
                                        // DE register
                                        self.reg.set_reg16(Reg16::DE, nn);
                                    }

                                    2 => {
                                        // HL register
                                        self.reg.set_reg16(Reg16::HL, nn);
                                    }

                                    3 => {
                                        // SP register
                                        self.sp = nn;
                                    }

                                    _ => unimplemented!("The register {} used in opcode {} is not implemented!", p, opcode),
                                }
                            }

                            1 => {
                                // ADD HL rr
                                // Add the value in 16-bit register rr to HL register
                                match p {
                                    // BC register
                                    0 => {}

                                    // DE register 
                                    1 => {}

                                    // HL register 
                                    2 => {}

                                    // SP register
                                    3 => {}
                                    _ => unimplemented!("The register {} used in opcode {} is not implemented!", p, opcode),
                                }
                            }

                            _ => unimplemented!("The opcode {} is not implemented!", opcode),
                        }
                    }

                    _ => unimplemented!("The opcode '{}' is not implemented!", opcode),
                }
            }
            _ => unimplemented!("The opcode '{}' is not implemented!", opcode),
        }
    }

    fn execute_cb(&self, opcode: u8) {}
}
