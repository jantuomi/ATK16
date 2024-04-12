from typing import Callable
from dataclasses import dataclass
from .asm_eval import *

ExpandResult = list[list[str]]
ExpandFn = Callable[..., ExpandResult]
OpExpansionDict = dict[str, ExpandFn]

@dataclass
class Meta:
  address: int

generate_unique_label_counter = 0
def generate_unique_label(prefix: str) -> str:
  global generate_unique_label_counter
  label = f"{prefix}_{generate_unique_label_counter}"
  generate_unique_label_counter += 1
  return label

def make_alr(meta: Meta, symbols: Symbols, alu_op: str, left: str, right: str, target: str) -> int:
  """ALR 0000 TTTL LLRR RSSS"""
  target_e = eval_expr(symbols, target, bits=3)
  left_e = eval_expr(symbols, left, bits=3)
  right_e = eval_expr(symbols, right, bits=3)
  alu_op_e = eval_expr(symbols, alu_op, bits=3)
  word = (0b0000 << 12) + \
              (target_e << 9) + \
              (left_e << 6) + \
              (right_e << 3) + \
              alu_op_e
  return word

def make_ali(meta: Meta, symbols: Symbols, alu_op: str, left: str, imm: str, target: str) -> int:
  """ALI 0001 TTTL LLII ISSS"""
  target_e = eval_expr(symbols, target, bits=3)
  left_e = eval_expr(symbols, left, bits=3)
  imm_e = eval_expr(symbols, imm, bits=3)
  alu_op_e = eval_expr(symbols, alu_op, bits=3)
  word = (0b0001 << 12) + \
              (target_e << 9) + \
              (left_e << 6) + \
              (imm_e << 3) + \
              alu_op_e
  return word

def make_ldr(meta: Meta, symbols: Symbols, addr_reg: str, to_reg: str) -> int:
  """LDR 0010 TTTR RRXX XXXX"""
  to_reg_e = eval_expr(symbols, to_reg, bits=3)
  addr_reg_e = eval_expr(symbols, addr_reg, bits=3)
  word = (0b0010 << 12) + \
              (to_reg_e << 9) + \
              (addr_reg_e << 6)
  return word

def make_str(meta: Meta, symbols: Symbols, from_reg: str, addr_reg: str) -> int:
  """STR 0011 XXXL LLRR RXXX"""
  addr_reg_e = eval_expr(symbols, from_reg, bits=3)
  from_reg_e = eval_expr(symbols, addr_reg, bits=3)
  word = (0b0011 << 12) + \
              (from_reg_e << 6) + \
              (addr_reg_e << 3)
  return word

def make_ldi(meta: Meta, symbols: Symbols, imm: str, to_reg: str) -> int:
  """LDI 0100 TTTI IIII IIII"""
  to_reg_e = eval_expr(symbols, to_reg, bits=3)
  imm_e = eval_expr(symbols, imm, bits=9)
  word = (0b0100 << 12) + \
              (to_reg_e << 9) + \
              imm_e
  return word

def make_jpr(meta: Meta, symbols: Symbols, addr_reg: str) -> int:
  """JPR 0101 XXXR RRXX XXXX"""
  addr_reg_e = eval_expr(symbols, addr_reg, bits=3)
  word = (0b0101 << 12) + \
              (addr_reg_e << 6)
  return word

def make_jpi(meta: Meta, symbols: Symbols, imm: str) -> int:
  """JPI 0110 XXXI IIII IIII"""
  imm_e = eval_expr(symbols, imm, bits=9)
  imm_e = imm_e - meta.address - 1
  imm_e = imm_e & (0b111111111)
  #print("symbols:", symbols)
  #print(meta, imm, f"0x{imm_e:>0x}")
  word = (0b0110 << 12) + \
              imm_e
  return word

def make_brr(meta: Meta, symbols: Symbols, flag_s: str, addr_reg: str) -> int:
  """brr  0111 XFFR RRXX XXXX"""
  flag_s_e = eval_expr(symbols, flag_s, bits=2)
  addr_reg_e = eval_expr(symbols, addr_reg, bits=3)
  word = (0b0111 << 12) + \
              (flag_s_e << 9) + \
              (addr_reg_e << 6)
  return word

def make_bri(meta: Meta, symbols: Symbols, flag_s: str, imm: str) -> int:
  """BRI 1000 XFFI IIII IIII"""
  flag_s_e = eval_expr(symbols, flag_s, bits=2)
  imm_e = eval_expr(symbols, imm, bits=9)
  imm_e = imm_e - meta.address - 1
  imm_e = imm_e & (0b111111111)
  word = (0b1000 << 12) + \
              (flag_s_e << 9) + \
              imm_e
  return word

def make_lpc(meta: Meta, symbols: Symbols, target: str) -> int:
  """LPC 1001 TTTX XXXX XXXX"""
  target_e = eval_expr(symbols, target, bits=3)
  word = (0b1001 << 12) + \
              (target_e << 9)
  return word

def make_rti(meta: Meta, symbols: Symbols) -> int:
  """RTI 1110 XXXX XXXX XXXX"""
  word = (0b1110 << 12)
  return word

def make_hlt(meta: Meta, symbols: Symbols) -> int:
  """HLT 1111 XXXX XXXX XXXX"""
  word = (0b1111 << 12)
  return word

OpWordDict = dict[str, Callable[..., int]]
operations: OpWordDict = {
  "alr": make_alr,
  "ali": make_ali,
  "ldr": make_ldr,
  "str": make_str,
  "ldi": make_ldi,
  "jpr": make_jpr,
  "jpi": make_jpi,
  "brr": make_brr,
  "bri": make_bri,
  "lpc": make_lpc,
  "rti": make_rti,
  "hlt": make_hlt,
}

def expand_id(*parts: str) -> ExpandResult:
  return [list(parts)]

def expand_add(left: str, right: str, target: str) -> ExpandResult:
  return [["alr", "al_plus", left, right, target]]

def expand_sub(left: str, right: str, target: str) -> ExpandResult:
  return [["alr", "al_minus", left, right, target]]

def expand_addi(left: str, imm: str, target: str) -> ExpandResult:
  return [["ali", "al_plus", left, imm, target]]

def expand_subi(left: str, imm: str, target: str) -> ExpandResult:
  return [["ali", "al_minus", left, imm, target]]

# FIXME: 0xFFFF does not fit in 3 bits
# def expand_not(reg: str, target: str) -> ExpandResult:
#   return [["alr", "al_xor", reg, "0xFFFF", target]]

# def expand_noti(imm: str, target: str) -> ExpandResult:
#   return [["ali", "al_xor", imm, "0xFFFF", target]]

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
  return [["str", reg, "SP"]] + expand_inc("SP")

def expand_spo(reg: str) -> ExpandResult:
  return expand_dec("SP") + [["ldr", "SP", reg]]

def expand_sinc(imm: str) -> ExpandResult:
  return expand_addi("SP", imm, "SP")

def expand_sdec(imm: str) -> ExpandResult:
  return expand_subi("SP", imm, "SP")

def expand_callr(addr_reg: str) -> ExpandResult:
  return [
    ["lpc", "RG"],
    *expand_addi("RG", "4", "RG"),
    *expand_spu("RG"),
    ["jpr", addr_reg],
    ["ldr", "SP", "RG"],
    *expand_dec("SP"),
  ]

def expand_calli(addr_imm: str) -> ExpandResult:
  ret_label = generate_unique_label(prefix="calli_return")
  return [
    ["ldi", ret_label, "RG"],
    *expand_spu("RG"),
    ["jpi", addr_imm],
    ["@label", ret_label],
    ["ldr", "SP", "RG"], # return leaves SP pointing at return value, read to reg
    *expand_dec("SP"),   # decrement SP to fix stack
  ]

def expand_return() -> ExpandResult:
  return [
    ["str", "RG", "SP"], # store returned value on stack but don't increment SP
    *expand_dec("SP"),   # decrement SP to access the return address
    ["ldr", "SP", "RG"], # load return address to RG
    *expand_inc("SP"),   # point SP to return value
    ["jpr", "RG"]        # jump back to call site
  ]

def stack_stash(*rs: str) -> ExpandResult:
  result: ExpandResult = []
  for r in rs:
    result += expand_spu(r)

  return result

def stack_restore(*rs: str) -> ExpandResult:
  result: ExpandResult = []
  for r in rs:
    prefix = expand_spo(r)
    result = prefix + result

  return result

def set_graphics_mode(mode: str) -> ExpandResult:
  return [
    ["ldi", "vt_gr_mode_addr", "RF"],
    ["ldr", "RF", "RF"],
    ["ldi", mode, "RG"],
    ["str", "RG", "RF"],
  ]

expansions: OpExpansionDict = {
  "alr": lambda *args: expand_id("alr", *args),
  "ali": lambda *args: expand_id("ali", *args),
  "ldr": lambda *args: expand_id("ldr", *args),
  "str": lambda *args: expand_id("str", *args),
  "ldi": lambda *args: expand_id("ldi", *args),
  "jpr": lambda *args: expand_id("jpr", *args),
  "jpi": lambda *args: expand_id("jpi", *args),
  "brr": lambda *args: expand_id("brr", *args),
  "bri": lambda *args: expand_id("bri", *args),
  "lpc": lambda *args: expand_id("lpc", *args),
  "rti": lambda *args: expand_id("rti", *args),
  "hlt": lambda *args: expand_id("hlt", *args),

  "add": expand_add,
  "sub": expand_sub,
  "addi": expand_addi,
  "subi": expand_subi,
  # "not": expand_not,
  # "noti": expand_noti,
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
  "sinc": expand_sinc,
  "sdec": expand_sdec,
  "callr": expand_callr,
  "calli": expand_calli,
  "return": expand_return,
  "stack_stash": stack_stash,
  "stack_restore": stack_restore,
  "set_graphics_mode": set_graphics_mode,
}
