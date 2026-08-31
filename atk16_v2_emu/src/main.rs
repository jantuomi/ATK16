use std::env::Args;
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};
use std::{
    fs::OpenOptions,
    io::{Read, Write},
    path::{Path, PathBuf},
    process::exit,
};

use crate::Inst::{Halt, Nop};

type Word = u16;

#[derive(Debug)]
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

struct Cu {}

impl Cu {
    pub fn fetch(&self, fram: &Fram, pc: &Word) -> [u16; 2] {
        let a0 = pc;
        let a1 = ((*pc as usize + 1) % N_ADDRESSES) as u16;
        [*fram.read(a0), *fram.read(&a1)]
    }

    pub fn decode(&self, bytes: &[u16; 2]) -> Inst {
        match bytes {
            [0x00, _] => Inst::Nop(),
            [0x01, _] => Inst::Halt(),
            [0x02, _] => Inst::Load(),
            [0x03, _] => Inst::Store(),
            [0x04, _] => Inst::Add(),
            [0x10, _] => Inst::Subtract(),
            [0x11, _] => Inst::And(),
            [0x12, _] => Inst::Or(),
            [0x13, _] => Inst::Xor(),
            [0x14, _] => Inst::ShiftLogicalL(),
            [0x15, _] => Inst::ShiftLogicalR(),
            [0x16, _] => Inst::ShiftArithR(),
            [0x20, _] => Inst::Ret(),
            [0x21, _] => Inst::RetIfZero(),
            [0x22, _] => Inst::Drop(),
            [0x60, addr] => Inst::Jump(*addr),
            [0x61, addr] => Inst::JumpIfZero(*addr),
            [0x62, addr] => Inst::Call(*addr),
            [0x63, addr] => Inst::CallIfZero(*addr),
            [0x64, word] => Inst::Literal(*word),
            [0x65, index] => Inst::Get(*index),
            _ => panic!("Undecodeable instruction: {:#x} {:#x}", bytes[0], bytes[1]),
        }
    }

    pub fn execute(&self, fram: &mut Fram, pc: &mut u16, halted: &mut bool, inst: &Inst) {
        *pc += 1;
        match inst {
            Inst::Nop() => {}
            Inst::Halt() => {
                *halted = true;
            }
            other => todo!("{:?}", other),
        }
    }
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let path_arg = match &args[..] {
        [_, str] => str,
        _ => panic!("Invalid args: {:?}", args),
    };

    let path = PathBuf::from(path_arg);
    let mut fram = Fram::load_from_image(&path).unwrap_or(Fram::new());

    let cu = Cu {};
    let mut pc = 0u16;
    let mut halted = false;

    let interrupted = Arc::new(AtomicBool::new(false));
    let flag = interrupted.clone();
    ctrlc::set_handler(move || {
        flag.store(true, Ordering::SeqCst);
    })
    .unwrap();

    while !halted && !interrupted.load(Ordering::SeqCst) {
        let bytes = cu.fetch(&fram, &pc);
        let inst = cu.decode(&bytes);
        cu.execute(&mut fram, &mut pc, &mut halted, &inst);
    }

    println!("Halted, saving image to {}", path.display());
    fram.save_to_image(&path);
}
