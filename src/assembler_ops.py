from typing import Callable
from assembler_eval import *

def make_alr(address: int, alu_op: str, left: str, right: str, target: str) -> list[int]:
  """ALR 0000 TTTL LLRR RSSS"""
  target_e = eval_expr(target)
  left_e = eval_expr(left)
  right_e = eval_expr(right)
  alu_op_e = eval_expr(alu_op)
  word = (0b0000 << 12) + \
              (target_e << 9) + \
              (left_e << 6) + \
              (right_e << 3) + \
              alu_op_e
  return [word]

def make_ali(address: int, alu_op: str, left: str, imm: str, target: str) -> list[int]:
  """ALI 0001 TTTL LLII ISSS"""
  target_e = eval_expr(target)
  left_e = eval_expr(left)
  imm_e = eval_expr(imm)
  alu_op_e = eval_expr(alu_op)
  word = (0b0001 << 12) + \
              (target_e << 9) + \
              (left_e << 6) + \
              (imm_e << 3) + \
              alu_op_e
  return [word]

def make_ldr(address: int, to_reg: str, addr_reg: str) -> list[int]:
  """LDR 0010 TTTR RRXX XXXX"""
  to_reg_e = eval_expr(to_reg)
  addr_reg_e = eval_expr(addr_reg)
  word = (0b0010 << 12) + \
              (to_reg_e << 9) + \
              (addr_reg_e << 6)
  return [word]

def make_str(address: int, from_reg: str, addr_reg: str) -> list[int]:
  """STR 0011 TTTR RRXX XXXX"""
  from_reg_e = eval_expr(from_reg)
  addr_reg_e = eval_expr(addr_reg)
  word = (0b0011 << 12) + \
              (from_reg_e << 9) + \
              (addr_reg_e << 6)
  return [word]

def make_ldi(address: int, to_reg: str, imm: str) -> list[int]:
  """LDI 0100 TTTI IIII IIII"""
  to_reg_e = eval_expr(to_reg)
  imm_e = eval_expr(imm)
  word = (0b0100 << 12) + \
              (to_reg_e << 9) + \
              imm_e
  return [word]

def make_jpr(address: int, addr_reg: str) -> list[int]:
  """JPR 0101 XXXR RRXX XXXX"""
  addr_reg_e = eval_expr(addr_reg)
  word = (0b0101 << 12) + \
              (addr_reg_e << 6)
  return [word]

def make_jpi(address: int, imm: str) -> list[int]:
  """JPI 0110 XXXI IIII IIII"""
  imm_e = eval_expr(imm)
  imm_e = imm_e - address - 1
  word = (0b0110 << 12) + \
              imm_e
  return [word]

def make_br(address: int, flag_s: str, addr_reg: str) -> list[int]:
  """brr  0111 XFFR RRXX XXXX"""
  flag_s_e = eval_expr(flag_s)
  addr_reg_e = eval_expr(addr_reg)
  word = (0b0111 << 12) + \
              (flag_s_e << 9) + \
              (addr_reg_e << 6)
  return [word]

def make_bri(address: int, flag_s: str, imm: str) -> list[int]:
  """BRI 1000 XFFI IIII IIII"""
  flag_s_e = eval_expr(flag_s)
  imm_e = eval_expr(imm)
  imm_e = imm_e - address - 1
  word = (0b1000 << 12) + \
              (flag_s_e << 9) + \
              imm_e
  return [word]

def make_hlt(address: int, ) -> list[int]:
  """HLT 1111 XXXX XXXX XXXX"""
  word = (0b1111 << 12)
  return [word]

operations: dict[str, Callable[..., list[int]]] = {
  "alr": make_alr,
  "alr": make_alr,
  "ali": make_ali,
  "ldr": make_ldr,
  "str": make_str,
  "ldi": make_ldi,
  "jpr": make_jpr,
  "jpi": make_jpi,
  "brr": make_br,
  "bri": make_bri,
  "hlt": make_hlt,
}