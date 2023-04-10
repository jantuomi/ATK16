from assembler_ops import *

def make_add(meta: Meta, left: str, right: str, target: str) -> list[int]:
  return make_alr(meta, "al_plus", left, right, target)

def make_sub(meta: Meta, left: str, right: str, target: str) -> list[int]:
  return make_alr(meta, "al_minus", left, right, target)

def make_addi(meta: Meta, left: str, imm: str, target: str) -> list[int]:
  return make_ali(meta, "al_plus", left, imm, target)

def make_subi(meta: Meta, left: str, imm: str, target: str) -> list[int]:
  return make_ali(meta, "al_minus", left, imm, target)

def make_and(meta: Meta, left: str, right: str, target: str) -> list[int]:
  return make_alr(meta, "al_and", left, right, target)

def make_or(meta: Meta, left: str, right: str, target: str) -> list[int]:
  return make_alr(meta, "al_or", left, right, target)

def make_xor(meta: Meta, left: str, right: str, target: str) -> list[int]:
  return make_alr(meta, "al_xor", left, right, target)

def make_sll(meta: Meta, left: str, right: str, target: str) -> list[int]:
  return make_alr(meta, "al_sll", left, right, target)

def make_slr(meta: Meta, left: str, right: str, target: str) -> list[int]:
  return make_alr(meta, "al_slr", left, right, target)

def make_sar(meta: Meta, left: str, right: str, target: str) -> list[int]:
  return make_alr(meta, "al_sar", left, right, target)

def make_slli(meta: Meta, left: str, imm: str, target: str) -> list[int]:
  return make_ali(meta, "al_sll", left, imm, target)

def make_slri(meta: Meta, left: str, imm: str, target: str) -> list[int]:
  return make_ali(meta, "al_slr", left, imm, target)

def make_sari(meta: Meta, left: str, imm: str, target: str) -> list[int]:
  return make_ali(meta, "al_sar", left, imm, target)

def make_inc(meta: Meta, reg: str) -> list[int]:
  return make_ali(meta, "al_plus", reg, "1", reg)

def make_dec(meta: Meta, reg: str) -> list[int]:
  return make_ali(meta, "al_minus", reg, "1", reg)

def make_mov(meta: Meta, from_reg: str, to_reg: str) -> list[int]:
 return make_ali(meta, "al_plus", from_reg, "0", to_reg)

def make_spu(meta: Meta, reg: str) -> list[int]:
  sp_reg = meta.options.stack_pointer
  str_word = make_str(meta, reg, sp_reg)
  inc_word = make_inc(meta, sp_reg)
  return str_word + inc_word

def make_spo(meta: Meta, reg: str) -> list[int]:
  sp_reg = meta.options.stack_pointer
  dec_word = make_dec(meta, sp_reg)
  ldr_word = make_ldr(meta, reg, sp_reg)
  return dec_word + ldr_word

def make_csr(meta: Meta, addr_reg: str) -> list[int]:
  lpc_word = make_lpc(meta, meta.options.csr_scratch)
  spu_word = make_spu(meta, meta.options.csr_scratch)
  jpr_word = make_jpr(meta, addr_reg)
  return lpc_word + spu_word + jpr_word

def make_csi(meta: Meta, addr_imm: str) -> list[int]:
  lpc_word = make_lpc(meta, meta.options.csr_scratch)
  spu_word = make_spu(meta, meta.options.csr_scratch)
  jpi_word = make_jpi(meta, addr_imm)
  return lpc_word + spu_word + jpi_word

def make_rsr(meta: Meta) -> list[int]:
  spo_word = make_spo(meta, meta.options.csr_scratch)
  jpr_word = make_jpr(meta, meta.options.csr_scratch)
  return spo_word + jpr_word

operations = {
  "add": make_add,
  "sub": make_sub,
  "addi": make_addi,
  "subi": make_subi,
  "and": make_and,
  "or": make_or,
  "xor": make_xor,
  "sll": make_sll,
  "slr": make_slr,
  "sar": make_sar,
  "slli": make_slli,
  "slri": make_slri,
  "sari": make_sari,
  "inc": make_inc,
  "dec": make_dec,
  "mov": make_mov,
  "spu": make_spu,
  "spo": make_spo,
  "csr": make_csr,
  "csi": make_csi,
  "rsr": make_rsr,
}
