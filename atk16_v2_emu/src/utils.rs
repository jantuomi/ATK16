use super::*;

pub fn encode(insts: &[Inst]) -> Vec<u16> {
    let mut words = Vec::new();
    for inst in insts {
        match inst {
            Inst::Nop() => words.push(0x00),
            Inst::Halt() => words.push(0x01),
            Inst::Load() => words.push(0x02),
            Inst::Store() => words.push(0x03),
            Inst::Add() => words.push(0x04),
            Inst::Subtract() => words.push(0x10),
            Inst::And() => words.push(0x11),
            Inst::Or() => words.push(0x12),
            Inst::Xor() => words.push(0x13),
            Inst::ShiftLogicalL() => words.push(0x14),
            Inst::ShiftLogicalR() => words.push(0x15),
            Inst::ShiftArithR() => words.push(0x16),
            Inst::Ret() => words.push(0x20),
            Inst::RetIfZero() => words.push(0x21),
            Inst::Drop() => words.push(0x22),
            Inst::Jump(addr) => {
                words.push(0x60);
                words.push(*addr);
            }
            Inst::JumpIfZero(addr) => {
                words.push(0x61);
                words.push(*addr);
            }
            Inst::Call(addr) => {
                words.push(0x62);
                words.push(*addr);
            }
            Inst::CallIfZero(addr) => {
                words.push(0x63);
                words.push(*addr);
            }
            Inst::Literal(v) => {
                words.push(0x64);
                words.push(*v);
            }
            Inst::Get(i) => {
                words.push(0x65);
                words.push(*i);
            }
        }
    }
    words
}
