use super::*;

pub fn encode(insts: &[Inst]) -> Vec<u16> {
    let mut words: Vec<u16> = vec![];
    for inst in insts {
        match inst {
            Inst::Nop() => words.push(0x0000),
            Inst::Halt() => words.push(0x0100),
            Inst::Load() => words.push(0x0200),
            Inst::Store() => words.push(0x0300),
            Inst::Add() => words.push(0x0400),
            Inst::Subtract() => words.push(0x1000),
            Inst::And() => words.push(0x1100),
            Inst::Or() => words.push(0x1200),
            Inst::Xor() => words.push(0x1300),
            Inst::ShiftLogicalL() => words.push(0x1400),
            Inst::ShiftLogicalR() => words.push(0x1500),
            Inst::ShiftArithR() => words.push(0x1600),
            Inst::Ret() => words.push(0x2000),
            Inst::RetIfZero() => words.push(0x2100),
            Inst::Drop() => words.push(0x2200),
            Inst::Get(i) => {
                let word = u16::from_be_bytes([0x30, *i]);
                words.push(word);
            }
            Inst::Jump(addr) => {
                words.push(0x6000);
                words.push(*addr);
            }
            Inst::JumpIfZero(addr) => {
                words.push(0x6100);
                words.push(*addr);
            }
            Inst::Call(addr) => {
                words.push(0x6200);
                words.push(*addr);
            }
            Inst::CallIfZero(addr) => {
                words.push(0x6300);
                words.push(*addr);
            }
            Inst::Literal(v) => {
                words.push(0x6400);
                words.push(*v);
            }
        }
    }
    words
}
