use std::{
    fs::OpenOptions,
    io::{Read, Write},
    path::{Path, PathBuf},
};

type Word = u16;

enum Inst {
    Nop(),
    Halt(),
    Load(),
    Store(),
    Add(),
    Subtract(),
    And(),
    Or(),
    Xor(),
    ShiftLogicalL(),
    ShiftLogicalR(),
    ShiftArithR(),
    Ret(),
    RetIfZero(),
    Drop(),
    Jump(Word),
    JumpIfZero(Word),
    Call(Word),
    CallIfZero(Word),
    Literal(Word),
    Get(Word),
}

const N_ADDRESSES: usize = 2usize.pow(16);
struct Fram {
    chip0: [Word; N_ADDRESSES / 2],
    chip1: [Word; N_ADDRESSES / 2],
}

impl Fram {
    pub fn new() -> Fram {
        Fram {
            chip0: [0; N_ADDRESSES / 2],
            chip1: [0; N_ADDRESSES / 2],
        }
    }

    pub fn write(&mut self, addr: &Word, datum: Word) {
        let l01 = (addr & 1) as usize;
        let h15 = (addr >> 1) as usize;
        if l01 == 0 {
            self.chip0[h15] = datum;
        } else {
            self.chip1[h15] = datum;
        }
    }

    pub fn read(&self, addr: &Word) -> &Word {
        let l01 = (addr & 1) as usize;
        let h15 = (addr >> 1) as usize;
        if l01 == 0 {
            &self.chip0[h15]
        } else {
            &self.chip1[h15]
        }
    }

    pub fn load_from_image(path: &Path) -> Option<Fram> {
        if !path.exists() {
            return None;
        }
        let mut f = OpenOptions::new().read(true).open(path).unwrap();
        let mut buf = [0u8; N_ADDRESSES * 2];
        f.read_exact(&mut buf).unwrap();

        let mut _addr = 0usize;
        let mut fram = Fram::new();
        while _addr < N_ADDRESSES {
            let addr = _addr as u16;
            let bytes: [u8; 2] = [buf[2 * _addr], buf[2 * _addr + 1]];
            let word = u16::from_be_bytes(bytes);
            fram.write(&addr, word);
            _addr += 1;
        }

        Some(fram)
    }

    pub fn save_to_image(&self, path: &Path) {
        let mut f = OpenOptions::new()
            .create(true)
            .truncate(true)
            .write(true)
            .open(path)
            .unwrap();

        let mut _addr = 0usize;
        while _addr < N_ADDRESSES {
            let addr = _addr as u16;
            let word = self.read(&addr);
            let bytes = word.to_be_bytes();
            f.write_all(&bytes).unwrap();
            _addr += 1;
        }

        f.flush().unwrap();
    }
}

fn main() {
    let path = PathBuf::from("image.bin");
    let mut fram = Fram::load_from_image(&path).unwrap_or(Fram::new());
    fram.save_to_image(&path);
}
