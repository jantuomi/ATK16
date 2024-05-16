from typing import Callable
from dataclasses import dataclass
from .asm_eval import *

ExpandResult = list[list[str]]
ExpandFn = Callable[..., ExpandResult]
OpWordDict = dict[str, Callable[..., int]]
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

operations: OpWordDict = {}
expansions: OpExpansionDict = {}

def register_primitive(func):
  ident: str = func.__name__.replace("make_", "")
  operations[ident] = func
  return func

def register_macro(func):
  ident: str = func.__name__.replace("expand_", "")
  expansions[ident] = func
  return func

@register_primitive
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

@register_macro
def expand_alr(alu_op: str, left: str, right: str, target: str) -> ExpandResult:
  return [["alr", alu_op, left, right, target]]

@register_primitive
def make_ali(meta: Meta, symbols: Symbols, alu_op: str, left: str, imm: int | str, target: str) -> int:
  """ALI 0001 TTTL LLII ISSS"""
  target_e = eval_expr(symbols, target, bits=3)
  left_e = eval_expr(symbols, left, bits=3)
  imm_e = eval_expr(symbols, str(imm), bits=3)
  alu_op_e = eval_expr(symbols, alu_op, bits=3)
  word = (0b0001 << 12) + \
              (target_e << 9) + \
              (left_e << 6) + \
              (imm_e << 3) + \
              alu_op_e
  return word

@register_macro
def expand_ali(alu_op: str, left: str, imm: int | str, target: str) -> ExpandResult:
  return [["ali", alu_op, left, str(imm), target]]

@register_primitive
def make_ldr(meta: Meta, symbols: Symbols, addr_reg: str, to_reg: str) -> int:
  """LDR 0010 TTTR RRXX XXXX"""
  to_reg_e = eval_expr(symbols, to_reg, bits=3)
  addr_reg_e = eval_expr(symbols, addr_reg, bits=3)
  word = (0b0010 << 12) + \
              (to_reg_e << 9) + \
              (addr_reg_e << 6)
  return word

@register_macro
def expand_ldr(addr_reg: str, to_reg: str) -> ExpandResult:
  return [["ldr", addr_reg, to_reg]]

@register_primitive
def make_str(meta: Meta, symbols: Symbols, from_reg: str, addr_reg: str) -> int:
  """STR 0011 XXXL LLRR RXXX"""
  addr_reg_e = eval_expr(symbols, from_reg, bits=3)
  from_reg_e = eval_expr(symbols, addr_reg, bits=3)
  word = (0b0011 << 12) + \
              (from_reg_e << 6) + \
              (addr_reg_e << 3)
  return word

@register_macro
def expand_str(from_reg: str, addr_reg: str) -> ExpandResult:
  return [["str", from_reg, addr_reg]]

@register_primitive
def make_ldi(meta: Meta, symbols: Symbols, imm: int | str, to_reg: str) -> int:
  """LDI 0100 TTTI IIII IIII"""
  to_reg_e = eval_expr(symbols, to_reg, bits=3)
  imm_e = eval_expr(symbols, str(imm), bits=9)
  word = (0b0100 << 12) + \
              (to_reg_e << 9) + \
              imm_e
  return word

@register_macro
def expand_ldi(imm: int | str, to_reg: str) -> ExpandResult:
  return [["ldi", str(imm), to_reg]]

@register_primitive
def make_jpr(meta: Meta, symbols: Symbols, addr_reg: str) -> int:
  """JPR 0101 XXXR RRXX XXXX"""
  addr_reg_e = eval_expr(symbols, addr_reg, bits=3)
  word = (0b0101 << 12) + \
              (addr_reg_e << 6)
  return word

@register_macro
def expand_jpr(addr_reg: str) -> ExpandResult:
  return [["jpr", addr_reg]]

@register_primitive
def make_jpi(meta: Meta, symbols: Symbols, imm: int | str) -> int:
  """JPI 0110 XXXI IIII IIII"""
  imm_e = eval_expr(symbols, str(imm), bits=16)
  imm_e = imm_e - meta.address - 1

  if imm_e < -(2 ** 8) or imm_e >= 2 ** 8:
    raise Exception(f"jpi imm value is not representable in 9 bits: {imm_e}")

  imm_e = imm_e & (0b111111111)
  word = (0b0110 << 12) + \
              imm_e
  return word

@register_macro
def expand_jpi(imm: int | str) -> ExpandResult:
  return [["jpi", str(imm)]]

@register_primitive
def make_brr(meta: Meta, symbols: Symbols, flag_s: str, addr_reg: str) -> int:
  """brr  0111 XFFR RRXX XXXX"""
  flag_s_e = eval_expr(symbols, flag_s, bits=2)
  addr_reg_e = eval_expr(symbols, addr_reg, bits=3)
  word = (0b0111 << 12) + \
              (flag_s_e << 9) + \
              (addr_reg_e << 6)
  return word

@register_macro
def expand_brr(flag_s: str, addr_reg: str) -> ExpandResult:
  return [["brr", flag_s, addr_reg]]

@register_primitive
def make_bri(meta: Meta, symbols: Symbols, flag_s: str, imm: int | str) -> int:
  """BRI 1000 XFFI IIII IIII"""
  flag_s_e = eval_expr(symbols, flag_s, bits=2)
  imm_e = eval_expr(symbols, str(imm), bits=16)
  imm_e = imm_e - meta.address - 1

  if imm_e < -(2 ** 8) or imm_e >= 2 ** 8:
    raise Exception(f"jpi imm value is not representable in 9 bits: {imm_e}")

  imm_e = imm_e & (0b111111111)
  word = (0b1000 << 12) + \
              (flag_s_e << 9) + \
              imm_e
  return word

@register_macro
def expand_bri(flag_s: str, imm: int | str) -> ExpandResult:
  return [["bri", flag_s, str(imm)]]

@register_primitive
def make_lpc(meta: Meta, symbols: Symbols, target: str) -> int:
  """LPC 1001 TTTX XXXX XXXX"""
  target_e = eval_expr(symbols, target, bits=3)
  word = (0b1001 << 12) + \
              (target_e << 9)
  return word

@register_macro
def expand_lpc(target: str) -> ExpandResult:
  return [["lpc", target]]

@register_primitive
def make_rti(meta: Meta, symbols: Symbols) -> int:
  """RTI 1110 XXXX XXXX XXXX"""
  word = (0b1110 << 12)
  return word

@register_macro
def expand_rti() -> ExpandResult:
  return [["rti"]]

@register_primitive
def make_hlt(meta: Meta, symbols: Symbols) -> int:
  """HLT 1111 XXXX XXXX XXXX"""
  word = (0b1111 << 12)
  return word

@register_macro
def expand_hlt() -> ExpandResult:
  return [["hlt"]]

def expand_id(*parts: str) -> ExpandResult:
  return [list(parts)]

@register_macro
def expand_add(left: str, right: str, target: str) -> ExpandResult:
  return [["alr", "al_plus", left, right, target]]

@register_macro
def expand_sub(left: str, right: str, target: str) -> ExpandResult:
  return [["alr", "al_minus", left, right, target]]

@register_macro
def expand_addi(left: str, imm: int | str, target: str) -> ExpandResult:
  return [["ali", "al_plus", left, str(imm), target]]

@register_macro
def expand_subi(left: str, imm: int | str, target: str) -> ExpandResult:
  return [["ali", "al_minus", left, str(imm), target]]

# FIXME: 0xFFFF does not fit in 3 bits
# def expand_not(reg: str, target: str) -> ExpandResult:
#   return [["alr", "al_xor", reg, "0xFFFF", target]]

# def expand_noti(imm: int | str, target: str) -> ExpandResult:
#   return [["ali", "al_xor", imm, "0xFFFF", target]]

@register_macro
def expand_and(left: str, right: str, target: str) -> ExpandResult:
  return [["alr", "al_and", left, right, target]]

@register_macro
def expand_andi(left: str, imm: int | str, target: str) -> ExpandResult:
  return [["ali", "al_and", left, str(imm), target]]

@register_macro
def expand_or(left: str, right: str, target: str) -> ExpandResult:
  return [["alr", "al_or", left, right, target]]

@register_macro
def expand_ori(left: str, imm: int | str, target: str) -> ExpandResult:
  return [["ali", "al_or", left, str(imm), target]]

@register_macro
def expand_xor(left: str, imm: int | str, target: str) -> ExpandResult:
  return [["alr", "al_xor", left, str(imm), target]]

@register_macro
def expand_xori(left: str, right: str, target: str) -> ExpandResult:
  return [["ali", "al_xor", left, right, target]]

@register_macro
def expand_not(reg: str) -> ExpandResult:
  tmp = "RA"
  return [
    *expand_spu(tmp),
    ["ldi", "0", tmp],
    *expand_subi(tmp, "1", tmp),
    *expand_xor(reg, tmp, reg),
    *expand_spo(tmp),
  ]

@register_macro
def expand_sll(left: str, right: str, target: str) -> ExpandResult:
  return [["alr", "al_sll", left, right, target]]

@register_macro
def expand_slr(left: str, right: str, target: str) -> ExpandResult:
  return [["alr", "al_slr", left, right, target]]

@register_macro
def expand_sar(left: str, right: str, target: str) -> ExpandResult:
  return [["alr", "al_sar", left, right, target]]

@register_macro
def expand_slli(left: str, imm: int | str, target: str) -> ExpandResult:
  return [["ali", "al_sll", left, str(imm), target]]

@register_macro
def expand_slri(left: str, imm: int | str, target: str) -> ExpandResult:
  return [["ali", "al_slr", left, str(imm), target]]

@register_macro
def expand_sari(left: str, imm: int | str, target: str) -> ExpandResult:
  return [["ali", "al_sar", left, str(imm), target]]

@register_macro
def expand_inc(reg: str) -> ExpandResult:
  return [["ali", "al_plus", reg, "1", reg]]

@register_macro
def expand_dec(reg: str) -> ExpandResult:
  return [["ali", "al_minus", reg, "1", reg]]

@register_macro
def expand_mov(from_reg: str, to_reg: str) -> ExpandResult:
  return [["ali", "al_plus", from_reg, "0", to_reg]]

@register_macro
def expand_nop() -> ExpandResult:
  return [["ali", "al_plus", "RA", "0", "RA"]]

@register_macro
def expand_spu(reg: str) -> ExpandResult:
  return [["str", reg, "SP"]] + expand_inc("SP")

@register_macro
def expand_spo(reg: str) -> ExpandResult:
  return expand_dec("SP") + [["ldr", "SP", reg]]

@register_macro
def expand_sinc(imm: int | str) -> ExpandResult:
  return expand_addi("SP", imm, "SP")

@register_macro
def expand_sdec(imm: int | str) -> ExpandResult:
  return expand_subi("SP", imm, "SP")

@register_macro
def expand_callr(addr_reg: str) -> ExpandResult:
  return [
    ["lpc", "RG"],
    *expand_addi("RG", "4", "RG"),
    *expand_spu("RG"),
    ["jpr", addr_reg],
    ["ldr", "SP", "RG"],
    *expand_dec("SP"),
  ]

@register_macro
def expand_calli(addr_imm: int | str) -> ExpandResult:
  ret_label = generate_unique_label(prefix="calli_return")
  return [
    ["ldi", ret_label, "RG"],
    *expand_spu("RG"),
    ["jpi", str(addr_imm)],
    ["@label", ret_label],
    ["ldr", "SP", "RG"], # return leaves SP pointing at return value, read to reg
    *expand_dec("SP"),   # decrement SP to fix stack
  ]

@register_macro
def expand_return() -> ExpandResult:
  return [
    ["str", "RG", "SP"], # store returned value on stack but don't increment SP
    *expand_dec("SP"),   # decrement SP to access the return address
    ["ldr", "SP", "RG"], # load return address to RG
    *expand_inc("SP"),   # point SP to return value
    ["jpr", "RG"]        # jump back to call site
  ]

@register_macro
def stack_stash(*rs: str) -> ExpandResult:
  result: ExpandResult = []
  for r in rs:
    result += expand_spu(r)

  return result

@register_macro
def stack_restore(*rs: str) -> ExpandResult:
  result: ExpandResult = []
  for r in rs:
    prefix = expand_spo(r)
    result = prefix + result

  return result

@register_macro
def set_graphics_mode(mode: str) -> ExpandResult:
  return [
    ["ldi", "vt_gr_mode_addr", "RF"],
    ["ldr", "RF", "RF"],
    ["ldi", mode, "RG"],
    ["str", "RG", "RF"],
  ]
