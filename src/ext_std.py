from assembler_ops import *

def make_add(address:int, left: str, right: str, target: str) -> list[int]:
  return make_alr(address, "al_plus", left, right, target)

def make_sub(address: int, left: str, right: str, target: str) -> list[int]:
  return make_alr(address, "al_minus", left, right, target)

def make_inc(address: int, reg: str) -> list[int]:
  return make_ali(address, "al_plus", reg, "1", reg)

def make_dec(address: int, reg: str) -> list[int]:
  return make_ali(address, "al_minus", reg, "1", reg)

def make_mov(address: int, from_reg: str, to_reg: str) -> list[int]:
 return make_ali(address, "al_plus", from_reg, "0", to_reg)

operations = {
  "add": make_add,
  "sub": make_sub,
  "inc": make_inc,
  "dec": make_dec,
  "mov": make_mov,
}
