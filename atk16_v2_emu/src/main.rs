use crossterm::event::{Event, KeyCode, KeyModifiers, poll, read};
use crossterm::terminal::{disable_raw_mode, enable_raw_mode};
use std::thread::sleep;
use std::time::Duration;
use std::{
    fs::OpenOptions,
    io::{Read, Write},
    path::{Path, PathBuf},
};

mod utils;

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
            Inst::Get(index) => {
                mem.ds.copy_to_top(index);
                *pc += 1;
            }
        }
    }
}

#[repr(usize)]
enum MmioCode {
    SerialRead = 0x00,
    SerialWrite = 0x01,
}

struct Mmio {
    pub regs: [Word; 256],
}

impl Mmio {
    pub fn new() -> Mmio {
        Mmio { regs: [0; 256] }
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
            self.mmio.regs[reg_select as usize]
        }
    }

    pub fn write(&mut self, addr: &Word, datum: Word) {
        if *addr < 0xff00u16 {
            self.fram.write(addr, datum);
        } else {
            let reg_select = *addr & 0xff;
            self.mmio.regs[reg_select as usize] = datum;
        }
    }
}

struct Config {
    debug: bool,
    step: bool,
    path: PathBuf,
}

fn parse_args() -> Config {
    let args: Vec<String> = std::env::args().collect();
    let args: Vec<&str> = args.iter().map(String::as_str).collect();
    let mut i = 1;
    let mut path: Option<PathBuf> = None;
    let mut debug = false;
    let mut step = false;
    while i < args.len() {
        match &args[i..] {
            ["--debug", ..] => {
                debug = true;
                i += 1;
            }
            ["--step", ..] => {
                step = true;
                i += 1;
            }
            [str] => {
                path = Some(PathBuf::from(str));
                i += 1;
            }
            _ => panic!("Invalid args: {:?}", args),
        };
    }

    Config {
        path: path.unwrap(),
        step,
        debug,
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
                    mmio.regs[MmioCode::SerialRead as usize] = c as u16
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
    let serial_write = mmio.regs[MmioCode::SerialWrite as usize];
    if serial_write != 0 {
        print!("{}", serial_write as u8 as char);
        std::io::stdout().flush().unwrap();
        mmio.regs[MmioCode::SerialWrite as usize] = 0;
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
    let _guard = RawModeGuard::new().unwrap();

    let config = parse_args();
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

        if config.step {
            sleep(Duration::from_secs(1));
        }
    }

    print!("Halted, saving image to {}\r\n", config.path.display());
    mem.fram.save_to_image(&config.path);
}

#[cfg(test)]
mod tests;
