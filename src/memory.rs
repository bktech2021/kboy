pub struct Memory {
    pub data: Vec<u8>,
}

#[allow(dead_code)]
impl Memory {
    pub fn new(size: usize) -> Memory {
        Memory {
            data: vec![0; size],
        }
    }

    pub fn load(&mut self, mem: &Vec<u8>) -> () {
        if mem.len() > self.data.len() {
            panic!("Memory overflow");
        }

        for (i, data) in mem.iter().enumerate() {
            self.data[i] = *data;
        }
    }

    pub fn load_from(&mut self, mem: &Vec<u8>, offset: usize) -> () {
        if mem.len() + offset > self.data.len() {
            panic!("Memory overflow");
        }

        for (i, data) in mem.iter().enumerate() {
            self.data[i + offset] = *data;
        }
    }

    pub fn read(&self, addr: u16) -> u8 {
        self.data[addr as usize]
    }

    pub fn write(&mut self, addr: u16, data: u8) -> () {
        self.data[addr as usize] = data;
    }
}
