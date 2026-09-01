use crossterm::event::{Event, KeyCode, KeyModifiers, poll, read};
use crossterm::terminal::{disable_raw_mode, enable_raw_mode};
use std::thread::sleep;
use std::time::Duration;
use std::{
    fs::OpenOptions,
    io::{Read, Write},
    path::{Path, PathBuf},
};

mod kernel;
mod utils;

type Word = u16;

#[derive(Debug, Clone)]
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
    Get(u8),
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

    pub fn read(&self, addr: &Word) -> Word {
        let l01 = (addr & 1) as usize;
        let h15 = (addr >> 1) as usize;
        if l01 == 0 {
            self.chip0[h15]
        } else {
            self.chip1[h15]
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

const STACK_SIZE: usize = 256;
struct Stack {
    pub tos: Word,
    pub nos: Word,
    pub rest: [Word; STACK_SIZE],
    sp: usize, // some small register in reality
}

impl Stack {
    pub fn new() -> Stack {
        Stack {
            tos: 0,
            nos: 0,
            rest: [0; STACK_SIZE],
            sp: 0,
        }
    }

    pub fn push(&mut self, datum: Word) {
        self.rest[self.sp] = self.nos;
        self.nos = self.tos;
        self.tos = datum;
        self.sp = (self.sp + 1) % STACK_SIZE;
    }

    pub fn pop(&mut self) -> Word {
        let ret = self.tos;

        self.sp = (self.sp + STACK_SIZE - 1) % STACK_SIZE;
        self.tos = self.nos;
        self.nos = self.rest[self.sp];

        ret
    }

    pub fn copy_to_top(&mut self, index: &Word) {
        let datum = match index {
            0 => self.tos,
            1 => self.nos,
            other if (*other as usize - 2) < STACK_SIZE => self.rest[*other as usize - 2],
            _ => panic!("Invalid copy_to_top index {:?}", index),
        };

        self.push(datum);
    }
}

struct Cu {}

impl Cu {
    pub fn fetch(&self, fram: &Fram, pc: &Word) -> [u16; 2] {
        let a0 = pc;
        let a1 = ((*pc as usize + 1) % N_ADDRESSES) as u16;
        [fram.read(a0), fram.read(&a1)]
    }

    pub fn decode(&self, words: &[u16; 2]) -> Inst {
        let bytes: Vec<u8> = words.iter().flat_map(|word| word.to_be_bytes()).collect();
        match bytes[..] {
            [0x00, _, _, _] => Inst::Nop(),
            [0x01, _, _, _] => Inst::Halt(),
            [0x02, _, _, _] => Inst::Load(),
            [0x03, _, _, _] => Inst::Store(),
            [0x04, _, _, _] => Inst::Add(),
            [0x10, _, _, _] => Inst::Subtract(),
            [0x11, _, _, _] => Inst::And(),
            [0x12, _, _, _] => Inst::Or(),
            [0x13, _, _, _] => Inst::Xor(),
            [0x14, _, _, _] => Inst::ShiftLogicalL(),
            [0x15, _, _, _] => Inst::ShiftLogicalR(),
            [0x16, _, _, _] => Inst::ShiftArithR(),
            [0x20, _, _, _] => Inst::Ret(),
            [0x21, _, _, _] => Inst::RetIfZero(),
            [0x22, _, _, _] => Inst::Drop(),
            [0x30, code, _, _] => Inst::Get(code),
            [0x60, _, msb, lsb] => Inst::Jump(u16::from_be_bytes([msb, lsb])),
            [0x61, _, msb, lsb] => Inst::JumpIfZero(u16::from_be_bytes([msb, lsb])),
            [0x62, _, msb, lsb] => Inst::Call(u16::from_be_bytes([msb, lsb])),
            [0x63, _, msb, lsb] => Inst::CallIfZero(u16::from_be_bytes([msb, lsb])),
            [0x64, _, msb, lsb] => Inst::Literal(u16::from_be_bytes([msb, lsb])),
            _ => panic!("Undecodeable instruction: {:#x} {:#x}", bytes[0], bytes[1]),
        }
    }

    pub fn execute(
        &self,
        config: &Config,
        mem: &mut Mem,
        pc: &mut u16,
        halted: &mut bool,
        inst: &Inst,
    ) {
        if config.debug {
            print!(
                "@ {:04x}  {:04x}  {:04x}  ${:?}\r\n",
                pc, mem.ds.nos, mem.ds.tos, inst
            );
        }

        *pc = ((*pc as usize + 1) % N_ADDRESSES) as Word;
        match inst {
            Inst::Nop() => {}
            Inst::Halt() => {
                *halted = true;
            }
            Inst::Load() => {
                let addr = mem.ds.pop();
                let datum = mem.read(&addr);
                mem.ds.push(datum);
            }
            Inst::Store() => {
                let addr = mem.ds.pop();
                let datum = mem.ds.pop();
                mem.write(&addr, datum);
            }
            Inst::Add() => {
                let lhs = mem.ds.pop();
                let rhs = mem.ds.pop();
                mem.ds.push(lhs.wrapping_add(rhs));
            }
            Inst::Subtract() => {
                let lhs = mem.ds.pop();
                let rhs = mem.ds.pop();
                mem.ds.push(lhs.wrapping_sub(rhs));
            }
            Inst::And() => {
                let lhs = mem.ds.pop();
                let rhs = mem.ds.pop();
                mem.ds.push(lhs & rhs);
            }
            Inst::Or() => {
                let lhs = mem.ds.pop();
                let rhs = mem.ds.pop();
                mem.ds.push(lhs | rhs);
            }
            Inst::Xor() => {
                let lhs = mem.ds.pop();
                let rhs = mem.ds.pop();
                mem.ds.push(lhs ^ rhs);
            }
            Inst::ShiftLogicalL() => {
                let lhs = mem.ds.pop();
                let rhs = mem.ds.pop();
                mem.ds.push(lhs << rhs);
            }
            Inst::ShiftLogicalR() => {
                let lhs = mem.ds.pop();
                let rhs = mem.ds.pop();
                mem.ds.push(lhs >> rhs);
            }
            Inst::ShiftArithR() => {
                let lhs = mem.ds.pop();
                let rhs = mem.ds.pop();
                let result = ((lhs as i16) >> rhs) as u16;
                mem.ds.push(result);
            }
            Inst::Ret() => {
                *pc = mem.cs.pop();
            }
            Inst::RetIfZero() => {
                let cond = mem.ds.pop();
                if cond == 0 {
                    *pc = mem.cs.pop();
                }
            }
            Inst::Drop() => _ = mem.ds.pop(),
            Inst::Get(index) => {
                mem.ds.copy_to_top(&(*index as u16));
            }
            Inst::Jump(addr) => {
                *pc = *addr;
            }
            Inst::JumpIfZero(addr) => {
                let cond = mem.ds.pop();
                if cond == 0 {
                    *pc = *addr;
                } else {
                    *pc += 1;
                }
            }
            Inst::Call(addr) => {
                mem.cs.push(*pc + 1); // already incremented once
                *pc = *addr;
            }
            Inst::CallIfZero(addr) => {
                let cond = mem.ds.pop();
                if cond == 0 {
                    mem.cs.push(*pc + 1); // already incremented once
                    *pc = *addr;
                } else {
                    *pc += 1;
                }
            }
            Inst::Literal(datum) => {
                mem.ds.push(*datum);
                *pc += 1;
            }
        }
    }
}

const MMIO_SECTION: Word = 0xFF00;
const MMIO_SERIAL_READ: Word = 0x00;
const MMIO_SERIAL_WRITE: Word = 0x01;

struct Mmio {
    pub ports: [Word; 256],
}

impl Mmio {
    pub fn new() -> Mmio {
        Mmio { ports: [0; 256] }
    }
}

struct Mem {
    pub fram: Fram,
    pub mmio: Mmio,
    pub ds: Stack,
    pub cs: Stack,
}

impl Mem {
    pub fn read(&self, addr: &Word) -> Word {
        if *addr < 0xff00u16 {
            self.fram.read(addr)
        } else {
            let reg_select = *addr & 0xff;
            self.mmio.ports[reg_select as usize]
        }
    }

    pub fn write(&mut self, addr: &Word, datum: Word) {
        if *addr < 0xff00u16 {
            self.fram.write(addr, datum);
        } else {
            let reg_select = *addr & 0xff;
            self.mmio.ports[reg_select as usize] = datum;
        }
    }
}

struct Config {
    debug: bool,
    slow: bool,
    gen_kernel: bool,
    path: PathBuf,
}

fn parse_args() -> Config {
    let args: Vec<String> = std::env::args().collect();
    let args: Vec<&str> = args.iter().map(String::as_str).collect();
    let mut i = 1;
    let mut path: Option<PathBuf> = None;
    let mut debug = false;
    let mut slow = false;
    let mut gen_kernel = false;
    while i < args.len() {
        match &args[i..] {
            ["--debug", ..] => {
                debug = true;
            }
            ["--slow", ..] => {
                slow = true;
            }
            ["--gen-kernel", ..] => {
                gen_kernel = true;
            }
            [str] => {
                path = Some(PathBuf::from(str));
            }
            _ => panic!("Invalid args: {:?}", args),
        };
        i += 1;
    }

    Config {
        slow,
        debug,
        gen_kernel,
        path: path.unwrap(),
    }
}

enum UserInputExc {
    CtrlC,
    Error(std::io::Error),
}

fn handle_console_input(mmio: &mut Mmio) -> Result<(), UserInputExc> {
    loop {
        let available = match poll(std::time::Duration::from_millis(0)) {
            Ok(avail) => avail,
            Err(err) => return Err(UserInputExc::Error(err)),
        };

        if !available {
            return Ok(());
        }

        match read() {
            Ok(Event::Key(event))
                if event.code == KeyCode::Char('c')
                    && event.modifiers.contains(KeyModifiers::CONTROL) =>
            {
                return Err(UserInputExc::CtrlC);
            }
            Ok(Event::Key(ev)) => {
                if let KeyCode::Char(c) = ev.code {
                    mmio.ports[MMIO_SERIAL_READ as usize] = c as u16
                } else {
                    continue;
                }
            }
            Ok(_) => continue,
            Err(err) => return Err(UserInputExc::Error(err)),
        }
    }
}

fn handle_console_output(mmio: &mut Mmio) {
    let serial_write = mmio.ports[MMIO_SERIAL_WRITE as usize];
    if serial_write != 0 {
        print!("{}", serial_write as u8 as char);
        std::io::stdout().flush().unwrap();
        mmio.ports[MMIO_SERIAL_WRITE as usize] = 0;
    }
}

struct RawModeGuard;
impl RawModeGuard {
    fn new() -> std::io::Result<Self> {
        enable_raw_mode()?;
        Ok(RawModeGuard)
    }
}
impl Drop for RawModeGuard {
    fn drop(&mut self) {
        let _ = disable_raw_mode();
    }
}

fn main() {
    let config = parse_args();
    if config.gen_kernel {
        kernel::gen_kernel_image(&config.path);
        return;
    }

    let _guard = RawModeGuard::new().unwrap();

    let fram = Fram::load_from_image(&config.path).unwrap_or(Fram::new());

    let cu = Cu {};
    let mut pc = 0u16;
    let mut halted = false;
    let mut mem = Mem {
        fram,
        mmio: Mmio::new(),
        ds: Stack::new(),
        cs: Stack::new(),
    };

    while !halted {
        match handle_console_input(&mut mem.mmio) {
            Ok(()) => {}
            Err(UserInputExc::CtrlC) => {
                halted = true;
                continue;
            }
            Err(UserInputExc::Error(err)) => panic!("{}", err),
        }
        handle_console_output(&mut mem.mmio);

        let bytes = cu.fetch(&mem.fram, &pc);
        let inst = cu.decode(&bytes);
        cu.execute(&config, &mut mem, &mut pc, &mut halted, &inst);

        if config.slow {
            sleep(Duration::from_secs(1));
        }
    }

    print!("Halted, saving image to {}\r\n", config.path.display());
    mem.fram.save_to_image(&config.path);
}

#[cfg(test)]
mod tests;
