// TODO: Move SP to registers
#[derive(Copy, Clone)]
pub enum Reg8 {
    A,
    B,
    C,
    D,
    E,
    F,
    H,
    L,
}

#[derive(Copy, Clone)]
pub enum Reg16 {
    AF,
    BC,
    DE,
    HL,
    SP,
}

#[derive(Copy, Clone)]
pub enum Flag {
    Z,
    N,
    C,
    H,
}

#[derive(Copy, Clone)]
#[allow(non_snake_case)]
pub struct Registers {
    pub A: u8,
    pub B: u8,
    pub C: u8,
    pub D: u8,
    pub E: u8,
    pub F: u8,
    pub H: u8,
    pub L: u8,
    pub SP: u16,
}

impl Registers {
    pub fn new() -> Self {
        Registers {
            A: 0x01,
            B: 0x00,
            C: 0x13,
            D: 0x00,
            E: 0xD8,
            F: 0xB0,
            H: 0x01,
            L: 0x4D,
            SP: 0,
        }
    }

    pub fn get_reg8(&self, src: Reg8) -> u8 {
        match src {
            Reg8::A => self.A,
            Reg8::B => self.B,
            Reg8::C => self.C,
            Reg8::D => self.D,
            Reg8::E => self.E,
            Reg8::F => self.F,
            Reg8::H => self.H,
            Reg8::L => self.L,
        }
    }

    pub fn set_reg8(&mut self, dst: Reg8, src: u8) {
        match dst {
            Reg8::A => self.A = src,
            Reg8::B => self.B = src,
            Reg8::C => self.C = src,
            Reg8::D => self.D = src,
            Reg8::E => self.E = src,
            Reg8::F => self.F = src,
            Reg8::H => self.H = src,
            Reg8::L => self.L = src,
        }
    }

    pub fn get_reg16(&self, src: Reg16) -> u16 {
        match src {
            Reg16::AF => (self.A as u16) << 8 | self.F as u16,
            Reg16::BC => (self.B as u16) << 8 | self.C as u16,
            Reg16::DE => (self.D as u16) << 8 | self.E as u16,
            Reg16::HL => (self.H as u16) << 8 | self.L as u16,
            Reg16::SP => self.SP,
        }
    }

    pub fn set_reg16(&mut self, dst: Reg16, src: u16) {
        match dst {
            Reg16::AF => {
                self.A = (src >> 8) as u8;
                self.F = src as u8 & 0xF0
            }
            Reg16::BC => {
                self.B = (src >> 8) as u8;
                self.C = src as u8
            }
            Reg16::DE => {
                self.D = (src >> 8) as u8;
                self.E = src as u8
            }
            Reg16::HL => {
                self.H = (src >> 8) as u8;
                self.L = src as u8
            }
            Reg16::SP => self.SP = src,
        };
    }

    pub fn get_flag(&self, src: Flag) -> bool {
        let m = match src {
            Flag::Z => 0b10000000,
            Flag::N => 0b01000000,
            Flag::H => 0b00100000,
            Flag::C => 0b00010000,
        };

        if self.F & m != 0 {
            true
        } else {
            false
        }
    }

    pub fn set_flag(&mut self, src: Flag, dst: bool) {
        let m = match src {
            Flag::Z => 0b10000000,
            Flag::N => 0b01000000,
            Flag::H => 0b00100000,
            Flag::C => 0b00010000,
        };

        if dst {
            self.F |= m;
        } else {
            self.F &= !m;
        }
    }
}
