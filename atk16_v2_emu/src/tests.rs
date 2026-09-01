use super::*;

fn run_program(insts: &[Inst]) -> Mem {
    let config = Config {
        debug: false,
        slow: false,
        gen_kernel: false,
        path: PathBuf::new(),
    };
    let words = utils::encode(insts);
    let mut fram = Fram::new();
    for (i, &word) in words.iter().enumerate() {
        fram.write(&(i as u16), word);
    }
    let cu = Cu {};
    let mut pc = 0u16;
    let mut halted = false;
    let mut mem = Mem {
        fram,
        mmio: Mmio::new(),
        ds: Stack::new(),
        cs: Stack::new(),
    };
    let mut cycles = 0;
    while !halted && cycles < 10_000 {
        let bytes = cu.fetch(&mem.fram, &pc);
        let inst = cu.decode(&bytes);
        cu.execute(&config, &mut mem, &mut pc, &mut halted, &inst);
        cycles += 1;
    }
    assert!(halted, "program did not halt within 10000 cycles");
    mem
}

// Single LIT then HALT: tos should hold the literal value.
#[test]
fn test_literal() {
    let mem = run_program(&[Inst::Literal(42), Inst::Halt()]);
    assert_eq!(mem.ds.tos, 42);
}

// Two LITs: tos holds last pushed, nos holds first.
#[test]
fn test_two_literals() {
    let mem = run_program(&[Inst::Literal(1), Inst::Literal(2), Inst::Halt()]);
    assert_eq!(mem.ds.tos, 2);
    assert_eq!(mem.ds.nos, 1);
}

// NOP leaves the stack unchanged.
#[test]
fn test_nop() {
    let mem = run_program(&[Inst::Literal(5), Inst::Nop(), Inst::Halt()]);
    assert_eq!(mem.ds.tos, 5);
}

// ADD: lhs=pop() (TOS), rhs=pop() (NOS), push(lhs+rhs).
#[test]
fn test_add() {
    let mem = run_program(&[
        Inst::Literal(3),
        Inst::Literal(5),
        Inst::Add(),
        Inst::Halt(),
    ]);
    assert_eq!(mem.ds.tos, 8);
}

// ADD wraps around on overflow.
#[test]
fn test_add_wrapping() {
    let mem = run_program(&[
        Inst::Literal(0xFFFF),
        Inst::Literal(1),
        Inst::Add(),
        Inst::Halt(),
    ]);
    assert_eq!(mem.ds.tos, 0);
}

// SUB: lhs=pop() (TOS=10), rhs=pop() (NOS=3), push(10-3)=7.
#[test]
fn test_subtract() {
    let mem = run_program(&[
        Inst::Literal(3),
        Inst::Literal(10),
        Inst::Subtract(),
        Inst::Halt(),
    ]);
    assert_eq!(mem.ds.tos, 7);
}

// SUB wraps around on underflow.
#[test]
fn test_subtract_wrapping() {
    let mem = run_program(&[
        Inst::Literal(1),
        Inst::Literal(0),
        Inst::Subtract(),
        Inst::Halt(),
    ]);
    assert_eq!(mem.ds.tos, 0xFFFF);
}

// AND: bitwise and of TOS and NOS.
#[test]
fn test_and() {
    let mem = run_program(&[
        Inst::Literal(0x0F0F),
        Inst::Literal(0xFF00),
        Inst::And(),
        Inst::Halt(),
    ]);
    assert_eq!(mem.ds.tos, 0x0F00);
}

// OR: bitwise or of TOS and NOS.
#[test]
fn test_or() {
    let mem = run_program(&[
        Inst::Literal(0xF0F0),
        Inst::Literal(0x0F0F),
        Inst::Or(),
        Inst::Halt(),
    ]);
    assert_eq!(mem.ds.tos, 0xFFFF);
}

// XOR: bitwise xor of TOS and NOS.
#[test]
fn test_xor() {
    let mem = run_program(&[
        Inst::Literal(0xFFFF),
        Inst::Literal(0x0F0F),
        Inst::Xor(),
        Inst::Halt(),
    ]);
    assert_eq!(mem.ds.tos, 0xF0F0);
}

// SLL: push(TOS << NOS). Push shift amount first, then value.
#[test]
fn test_sll() {
    // NOS=2 (shift), TOS=1 (value): 1 << 2 = 4
    let mem = run_program(&[
        Inst::Literal(2),
        Inst::Literal(1),
        Inst::ShiftLogicalL(),
        Inst::Halt(),
    ]);
    assert_eq!(mem.ds.tos, 4);
}

// SRL: push(TOS >> NOS), zero-fill. Sign bit is NOT propagated.
#[test]
fn test_srl() {
    // NOS=1 (shift), TOS=0x8000 (value): 0x8000 >> 1 = 0x4000
    let mem = run_program(&[
        Inst::Literal(1),
        Inst::Literal(0x8000),
        Inst::ShiftLogicalR(),
        Inst::Halt(),
    ]);
    assert_eq!(mem.ds.tos, 0x4000);
}

// SRA: arithmetic right shift; sign bit is propagated.
#[test]
fn test_sra() {
    // NOS=1 (shift), TOS=0x8000 (-32768 as i16): result = 0xC000
    let mem = run_program(&[
        Inst::Literal(1),
        Inst::Literal(0x8000),
        Inst::ShiftArithR(),
        Inst::Halt(),
    ]);
    assert_eq!(mem.ds.tos, 0xC000);
}

// SRL vs SRA differ when the sign bit is set.
#[test]
fn test_srl_vs_sra_sign_bit() {
    let mem_srl = run_program(&[
        Inst::Literal(1),
        Inst::Literal(0x8000),
        Inst::ShiftLogicalR(),
        Inst::Halt(),
    ]);
    let mem_sra = run_program(&[
        Inst::Literal(1),
        Inst::Literal(0x8000),
        Inst::ShiftArithR(),
        Inst::Halt(),
    ]);
    assert_eq!(mem_srl.ds.tos, 0x4000); // zero-fill
    assert_eq!(mem_sra.ds.tos, 0xC000); // sign-fill
}

// DROP discards TOS, exposing NOS as new TOS.
#[test]
fn test_drop() {
    let mem = run_program(&[
        Inst::Literal(1),
        Inst::Literal(2),
        Inst::Drop(),
        Inst::Halt(),
    ]);
    assert_eq!(mem.ds.tos, 1);
}

// GET 0 duplicates TOS.
#[test]
fn test_get_tos() {
    let mem = run_program(&[Inst::Literal(7), Inst::Get(0), Inst::Halt()]);
    assert_eq!(mem.ds.tos, 7);
    assert_eq!(mem.ds.nos, 7);
}

// GET 1 copies NOS to top without consuming it.
#[test]
fn test_get_nos() {
    // Stack before GET: tos=7, nos=3
    let mem = run_program(&[
        Inst::Literal(3),
        Inst::Literal(7),
        Inst::Get(1),
        Inst::Halt(),
    ]);
    assert_eq!(mem.ds.tos, 3); // NOS duplicated to top
    assert_eq!(mem.ds.nos, 7);
}

// JUMP skips over instructions between it and the target.
#[test]
fn test_jump() {
    // addr 0-1: Jump(4)
    // addr 2-3: Literal(99)  <- skipped
    // addr 4:   Halt
    let mem = run_program(&[Inst::Jump(4), Inst::Literal(99), Inst::Halt()]);
    assert_eq!(mem.ds.tos, 0); // Literal(99) was skipped
}

// JUMPZ jumps when TOS == 0 (condition consumed).
#[test]
fn test_jumpz_taken() {
    // addr 0-1: Literal(0)
    // addr 2-3: JumpIfZero(6)
    // addr 4-5: Literal(99)  <- skipped
    // addr 6:   Halt
    let mem = run_program(&[
        Inst::Literal(0),
        Inst::JumpIfZero(6),
        Inst::Literal(99),
        Inst::Halt(),
    ]);
    assert_eq!(mem.ds.tos, 0); // Literal(99) skipped; condition was consumed
}

// JUMPZ falls through when TOS != 0.
#[test]
fn test_jumpz_not_taken() {
    // addr 0-1: Literal(1)
    // addr 2-3: JumpIfZero(6)  <- not taken, falls to addr 4
    // addr 4-5: Literal(42)
    // addr 6:   Halt
    let mem = run_program(&[
        Inst::Literal(1),
        Inst::JumpIfZero(6),
        Inst::Literal(42),
        Inst::Halt(),
    ]);
    assert_eq!(mem.ds.tos, 42);
}

// CALL pushes return address onto CS and jumps; RET returns.
#[test]
fn test_call_ret() {
    // addr 0-1: Call(5)
    // addr 2-3: Literal(10)   <- resumed after RET
    // addr 4:   Halt
    // addr 5-6: Literal(42)   <- subroutine
    // addr 7:   Ret
    let mem = run_program(&[
        Inst::Call(5),
        Inst::Literal(10),
        Inst::Halt(),
        Inst::Literal(42),
        Inst::Ret(),
    ]);
    assert_eq!(mem.ds.tos, 10);
    assert_eq!(mem.ds.nos, 42);
}

// CALLZ calls only when TOS == 0 (condition consumed).
#[test]
fn test_callz_taken() {
    // addr 0-1: Literal(0)
    // addr 2-3: CallIfZero(5)  <- taken; return address = 4
    // addr 4:   Halt           <- return landing
    // addr 5-6: Literal(42)   <- subroutine
    // addr 7:   Ret
    let mem = run_program(&[
        Inst::Literal(0),
        Inst::CallIfZero(5),
        Inst::Halt(),
        Inst::Literal(42),
        Inst::Ret(),
    ]);
    assert_eq!(mem.ds.tos, 42);
}

// CALLZ falls through when TOS != 0.
#[test]
fn test_callz_not_taken() {
    // addr 0-1: Literal(1)
    // addr 2-3: CallIfZero(8)  <- not taken, falls to addr 4
    // addr 4-5: Literal(7)
    // addr 6:   Halt
    let mem = run_program(&[
        Inst::Literal(1),
        Inst::CallIfZero(8),
        Inst::Literal(7),
        Inst::Halt(),
    ]);
    assert_eq!(mem.ds.tos, 7);
}

// RETZ returns from subroutine only when TOS == 0.
#[test]
fn test_retz_taken() {
    // addr 0-1: Call(5)
    // addr 2-3: Literal(55)   <- resumed after RETZ
    // addr 4:   Halt
    // addr 5-6: Literal(0)    <- subroutine
    // addr 7:   RetIfZero      <- condition=0, returns
    let mem = run_program(&[
        Inst::Call(5),
        Inst::Literal(55),
        Inst::Halt(),
        Inst::Literal(0),
        Inst::RetIfZero(),
    ]);
    assert_eq!(mem.ds.tos, 55);
}

// RETZ does NOT return when TOS != 0; execution continues in subroutine.
#[test]
fn test_retz_not_taken() {
    // addr 0-1: Call(5)
    // addr 2-3: Literal(99)   <- resumed after RET
    // addr 4:   Halt
    // addr 5-6: Literal(1)    <- subroutine
    // addr 7:   RetIfZero      <- not taken (1 != 0)
    // addr 8-9: Literal(42)
    // addr 10:  Ret
    let mem = run_program(&[
        Inst::Call(5),
        Inst::Literal(99),
        Inst::Halt(),
        Inst::Literal(1),
        Inst::RetIfZero(),
        Inst::Literal(42),
        Inst::Ret(),
    ]);
    assert_eq!(mem.ds.tos, 99);
    assert_eq!(mem.ds.nos, 42);
}

// STORE writes a value to memory; LOAD reads it back.
#[test]
fn test_store_load() {
    // STORE: pops addr (TOS) then datum (NOS), writes datum to addr.
    // Push datum first, then addr.
    let mem = run_program(&[
        Inst::Literal(42),
        Inst::Literal(0x100),
        Inst::Store(),
        Inst::Literal(0x100),
        Inst::Load(),
        Inst::Halt(),
    ]);
    assert_eq!(mem.ds.tos, 42);
}
