from asm_ops import *

def expand_add(left: str, right: str, target: str) -> ExpandResult:
  return [["alr", "al_plus", left, right, target]]

def expand_sub(left: str, right: str, target: str) -> ExpandResult:
  return [["alr", "al_minus", left, right, target]]

def expand_addi(left: str, imm: str, target: str) -> ExpandResult:
  return [["ali", "al_plus", left, imm, target]]

def expand_subi(left: str, imm: str, target: str) -> ExpandResult:
  return [["ali", "al_minus", left, imm, target]]

def expand_not(reg: str, target: str) -> ExpandResult:
  return [["alr", "al_xor", reg, "0xFFFF", target]]

def expand_noti(imm: str, target: str) -> ExpandResult:
  return [["ali", "al_xor", imm, "0xFFFF", target]]

def expand_and(left: str, right: str, target: str) -> ExpandResult:
  return [["alr", "al_and", left, right, target]]

def expand_andi(left: str, imm: str, target: str) -> ExpandResult:
  return [["ali", "al_and", left, imm, target]]

def expand_or(left: str, right: str, target: str) -> ExpandResult:
  return [["alr", "al_or", left, right, target]]

def expand_ori(left: str, imm: str, target: str) -> ExpandResult:
  return [["ali", "al_or", left, imm, target]]

def expand_xor(left: str, imm: str, target: str) -> ExpandResult:
  return [["alr", "al_xor", left, imm, target]]

def expand_xori(left: str, right: str, target: str) -> ExpandResult:
  return [["ali", "al_xor", left, right, target]]

def expand_sll(left: str, right: str, target: str) -> ExpandResult:
  return [["alr", "al_sll", left, right, target]]

def expand_slr(left: str, right: str, target: str) -> ExpandResult:
  return [["alr", "al_slr", left, right, target]]

def expand_sar(left: str, right: str, target: str) -> ExpandResult:
  return [["alr", "al_sar", left, right, target]]

def expand_slli(left: str, imm: str, target: str) -> ExpandResult:
  return [["ali", "al_sll", left, imm, target]]

def expand_slri(left: str, imm: str, target: str) -> ExpandResult:
  return [["ali", "al_slr", left, imm, target]]

def expand_sari(left: str, imm: str, target: str) -> ExpandResult:
  return [["ali", "al_sar", left, imm, target]]

def expand_inc(reg: str) -> ExpandResult:
  return [["ali", "al_plus", reg, "1", reg]]

def expand_dec(reg: str) -> ExpandResult:
  return [["ali", "al_minus", reg, "1", reg]]

def expand_mov(from_reg: str, to_reg: str) -> ExpandResult:
  return [["ali", "al_plus", from_reg, "0", to_reg]]

def expand_nop() -> ExpandResult:
  return [["ali", "al_plus", "RA", "0", "RA"]]

def expand_spu(reg: str) -> ExpandResult:
  return [["str", reg, "__STACK_POINTER"]] + expand_inc("__STACK_POINTER")

def expand_spo(reg: str):
  return expand_dec("__STACK_POINTER") + [["ldr", "__STACK_POINTER", reg]]

def expand_csr(addr_reg: str):
  return [
    ["lpc", "__CSR_SCRATCH"],
    *expand_addi("__CSR_SCRATCH", "4", "__CSR_SCRATCH"),
    *expand_spu("__CSR_SCRATCH"),
    ["jpr", addr_reg]
  ]

def expand_csi(addr_imm: str):
  return [
    ["lpc", "__CSR_SCRATCH"],
    *expand_addi("__CSR_SCRATCH", "4", "__CSR_SCRATCH"),
    *expand_spu("__CSR_SCRATCH"),
    ["jpi", addr_imm]
  ]

def expand_rsr():
  return expand_spo("__CSR_SCRATCH") + [["jpr", "__CSR_SCRATCH"]]


def stack_stash(*rs: str):
  result: ExpandResult = []
  for r in rs:
    result += expand_spu(r)

  return result

def stack_restore(*rs: str):
  result: ExpandResult = []
  for r in rs:
    prefix = expand_spu(r)
    result = prefix + result

  return result

def set_graphics_mode(mode: str):
  return [
    ["ldi", "vt_gr_mode_addr", "RA"],
    ["ldr", "RA", "RA"],
    ["ldi", mode, "RB"],
    ["str", "RB", "RA"],
  ]

expansions: OpExpansionDict = {
  "add": expand_add,
  "sub": expand_sub,
  "addi": expand_addi,
  "subi": expand_subi,
  "not": expand_not,
  "noti": expand_noti,
  "and": expand_and,
  "andi": expand_andi,
  "or": expand_or,
  "ori": expand_ori,
  "xor": expand_xor,
  "xori": expand_xori,
  "sll": expand_sll,
  "slr": expand_slr,
  "sar": expand_sar,
  "slli": expand_slli,
  "slri": expand_slri,
  "sari": expand_sari,
  "inc": expand_inc,
  "dec": expand_dec,
  "mov": expand_mov,
  "nop": expand_nop,
  "spu": expand_spu,
  "spo": expand_spo,
  "csr": expand_csr,
  "csi": expand_csi,
  "rsr": expand_rsr,
  "stack_stash": stack_stash,
  "stack_restore": stack_restore,
  "set_graphics_mode": set_graphics_mode,
}
