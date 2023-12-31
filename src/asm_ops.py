from typing import Callable
from asm_eval import *
from dataclasses import dataclass

@dataclass
class Meta:
  address: int

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
  from_reg_e = eval_expr(symbols, from_reg, bits=3)
  addr_reg_e = eval_expr(symbols, addr_reg, bits=3)
  word = (0b0011 << 12) + \
              (addr_reg_e << 6) + \
              (from_reg_e << 3)
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

ExpandResult = list[list[str]]
ExpandFn = Callable[..., ExpandResult]
OpExpansionDict = dict[str, ExpandFn]

def expand_id(*parts: str) -> ExpandResult:
  return [list(parts)]

default_expansions: OpExpansionDict = {
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
}

