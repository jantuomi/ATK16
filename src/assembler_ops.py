from typing import Callable
from assembler_eval import *
from dataclasses import dataclass

@dataclass
class Meta:
  address: int
  options: dict[str, str]

def make_alr(meta: Meta, alu_op: str, left: str, right: str, target: str) -> list[int]:
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

def make_ali(meta: Meta, alu_op: str, left: str, imm: str, target: str) -> list[int]:
  """ALI 0001 TTTL LLII ISSS"""
  target_e = eval_expr(target)
  left_e = eval_expr(left)
  imm_e = eval_expr(imm)
  if imm_e >= 2**3:
    raise Exception("ALI immediate value does not fit in 3 bits: " + str(imm_e))
  alu_op_e = eval_expr(alu_op)
  word = (0b0001 << 12) + \
              (target_e << 9) + \
              (left_e << 6) + \
              (imm_e << 3) + \
              alu_op_e
  return [word]

def make_ldr(meta: Meta, to_reg: str, addr_reg: str) -> list[int]:
  """LDR 0010 TTTR RRXX XXXX"""
  to_reg_e = eval_expr(to_reg)
  addr_reg_e = eval_expr(addr_reg)
  word = (0b0010 << 12) + \
              (to_reg_e << 9) + \
              (addr_reg_e << 6)
  return [word]

def make_str(meta: Meta, from_reg: str, addr_reg: str) -> list[int]:
  """STR 0011 TTTR RRXX XXXX"""
  from_reg_e = eval_expr(from_reg)
  addr_reg_e = eval_expr(addr_reg)
  word = (0b0011 << 12) + \
              (from_reg_e << 9) + \
              (addr_reg_e << 6)
  return [word]

def make_ldi(meta: Meta, to_reg: str, imm: str) -> list[int]:
  """LDI 0100 TTTI IIII IIII"""
  to_reg_e = eval_expr(to_reg)
  imm_e = eval_expr(imm)
  word = (0b0100 << 12) + \
              (to_reg_e << 9) + \
              imm_e
  return [word]

def make_jpr(meta: Meta, addr_reg: str) -> list[int]:
  """JPR 0101 XXXR RRXX XXXX"""
  addr_reg_e = eval_expr(addr_reg)
  word = (0b0101 << 12) + \
              (addr_reg_e << 6)
  return [word]

def make_jpi(meta: Meta, imm: str) -> list[int]:
  """JPI 0110 XXXI IIII IIII"""
  imm_e = eval_expr(imm)
  imm_e = imm_e - meta.address - 1
  if imm_e >= 2**9:
    raise Exception("JPI relative value does not fit in 9 bits: " + str(imm_e))

  word = (0b0110 << 12) + \
              imm_e
  return [word]

def make_brr(meta: Meta, flag_s: str, addr_reg: str) -> list[int]:
  """brr  0111 XFFR RRXX XXXX"""
  flag_s_e = eval_expr(flag_s)
  addr_reg_e = eval_expr(addr_reg)
  word = (0b0111 << 12) + \
              (flag_s_e << 9) + \
              (addr_reg_e << 6)
  return [word]

def make_bri(meta: Meta, flag_s: str, imm: str) -> list[int]:
  """BRI 1000 XFFI IIII IIII"""
  flag_s_e = eval_expr(flag_s)
  imm_e = eval_expr(imm)
  imm_e = imm_e - meta.address - 1
  if imm_e >= 2**9:
    raise Exception("BRI relative value does not fit in 9 bits: " + str(imm_e))
  word = (0b1000 << 12) + \
              (flag_s_e << 9) + \
              imm_e
  return [word]

def make_lpc(meta: Meta, target: str) -> list[int]:
  """LPC 1001 TTTX XXXX XXXX"""
  target_e = eval_expr(target)
  word = (0b1001 << 12) + \
              (target_e << 9)
  return [word]

def make_hlt(meta: Meta, ) -> list[int]:
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
  "brr": make_brr,
  "bri": make_bri,
  "lpc": make_lpc,
  "hlt": make_hlt,
}
