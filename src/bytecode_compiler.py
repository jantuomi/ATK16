#!/usr/bin/env python3
# Generate .atk16 assembly from a subset of Python

import sys
import dis

from dataclasses import dataclass
from typing import Literal, Set, cast, Any
from collections import OrderedDict

if len(sys.argv) != 3:
  print("usage: compiler.py <infile.py> <outfile.atk16>")
  sys.exit(1)

infile_path = sys.argv[1]
outfile_path = sys.argv[2]

with open(infile_path, "r") as f:
  source_py = f.read()

compiled_code = compile(source_py, infile_path, 'exec')
disassembled = dis.get_instructions(compiled_code)

co_consts = compiled_code.co_consts
co_names = compiled_code.co_names
print("### Constants")
print(co_consts)
print("\n### Names")
print(co_names)
print("\n### Instructions")
instrs: list[tuple[str, int | None]] = []
for instr in disassembled:
  print(f"{instr.opname}\t{instr.arg}")
  instrs.append((instr.opname, instr.arg))

### compile
asm_out: list[str] = []
def emit(stmt: str) -> int:
  ret = len(asm_out)
  asm_out.append(stmt)
  return ret

_counter = 0
def get_unique_name(prefix: str):
  global _counter
  ret =  f"{prefix}_{_counter}"
  _counter += 1
  return ret

@dataclass
class Label:
  label: str

  def __str__(self):
    return f"@label {self.label}"

RegChar = Literal["A", "B", "C", "D", "E", "F", "G", "H"]
GENERIC_REGS_LST: list[RegChar] = ["A", "B", "C", "D", "E", "F"]
GENERIC_REGS: OrderedDict[RegChar, None] = OrderedDict()
for char in GENERIC_REGS_LST:
  GENERIC_REGS[cast(RegChar, char)] = None

@dataclass
class Reg:
  reg: RegChar

  def __str__(self):
    return f"R{self.reg}"

Value = Label | Reg

reserved_regs: OrderedDict[RegChar, None] = OrderedDict()
def alloc_reg() -> Reg:
  for reg in GENERIC_REGS:
    if not reg in reserved_regs:
      reserved_regs[reg] = None
      return Reg(reg)

  raise Exception("Ran out of registers, TODO use stack")

def free_reg(reg: Reg):
  reserved_regs.pop(reg.reg)

def emit_serialize_const(const: Any):
  match const:
    case int(v):
      emit(f"  {v}")
    case str(v):
      if len(v) > 1:
        raise Exception("Cannot serialize const string: " + v)

      emit(f"  {ord(v[0])}")
    case _:
      print(f"warn: cannot serialize {type(const)} ({const}), emitting zero")
      emit("  0")

STACK_POINTER_REG = Reg('G')
CSR_SCRATCH_REG = Reg('H')
emit(f"@opt stack_pointer {STACK_POINTER_REG}")
emit(f"@opt csr_scratch {CSR_SCRATCH_REG}")
emit("@use ext_std:*")
emit("@include bootstrap")

py_consts_addr = emit("@label py_consts")

for const_i, const in enumerate(co_consts):
  emit(f"; {const_i}: {const}")
  emit_serialize_const(const)

emit("@label atk_store")
_reg_value = alloc_reg()
_reg_addr = alloc_reg()
emit(f"  str RB RA")
emit(f"  rsr")
free_reg(_reg_value)
free_reg(_reg_addr)

emit("@label atk_enable_text_mode")
emit("  set_graphics_mode gr_text_mode")
emit("  rsr")

emit("@label main")

instr_i = 0

def instr_pop():
  global instr_i
  ret = instrs[instr_i]
  instr_i += 1
  return ret

def instr_pop_expect(opcode: str, arg: int | None = -1):
  ret_op, ret_arg = instr_pop()
  if ret_op != opcode:
    raise Exception(f"Expected opcode {opcode}, got {ret_op}")
  if arg != -1 and arg != ret_arg:
    raise Exception(f"Expected instruction arg {arg}, got {ret_arg}")
  return ret_op, ret_arg

def instr_peek():
  return instrs[instr_i]

def instr_peek_expect(opcode: str, arg: int | None = -1):
  ret_op, ret_arg = instr_peek()
  if ret_op != opcode:
    raise Exception(f"Expected {opcode}, got {ret_op}")
  if arg != -1 and arg != ret_arg:
    raise Exception(f"Expected instruction arg {arg}, got {ret_arg}")
  return ret_op, ret_arg

instr_pop_expect("RESUME")
instr_pop_expect("LOAD_CONST", 0)
instr_pop_expect("LOAD_CONST", 1)
instr_pop_expect("IMPORT_NAME", 0)
instr_pop_expect("IMPORT_STAR", None)

while instr_i < len(instrs):
  opcode, arg = instr_pop()
  emit(f"; {opcode} {arg}")
  match opcode:
    case "LOAD_CONST":
      reg = alloc_reg()
      emit(f"  ldi ${{py_consts + {cast(int, arg)}}} {reg}")
      emit(f"  ldr {reg} {reg}")
      emit(f"  spu {reg}")
      free_reg(reg)
    case "PUSH_NULL":
      reg = alloc_reg()
      emit(f"  ldi 0 {reg}")
      emit(f"  spu {reg}")
      free_reg(reg)
    case "LOAD_NAME":
      name = co_names[cast(int, arg)]
      reg = alloc_reg()
      emit(f"  ldi {name} {reg}")
      emit(f"  spu {reg}")
      free_reg(reg)
    case "CALL":
      arg_count = cast(int, arg)
      # if arg_count > len(GENERIC_REGS_LST):
      #   raise Exception(f"Function calls with > {len(GENERIC_REGS_LST)} arguments not supported")

      regs_to_restore: list[tuple[Reg, Reg]] = []
      for j in range(arg_count):
        i = arg_count - j - 1
        ith_reg_char = GENERIC_REGS_LST[i]
        ith_reg = Reg(ith_reg_char)
        if ith_reg_char in reserved_regs:
          reg = alloc_reg()
          emit(f"  mov {ith_reg} {reg}")
          regs_to_restore.append((reg, ith_reg))

        emit(f"  spo {ith_reg}")
        reserved_regs[ith_reg_char] = None # manually reserve ith_reg

      fn_reg = alloc_reg()
      emit(f"  spo {fn_reg}")
      # emit(f"  ldr {fn_reg} {fn_reg}")
      emit(f"  csr {fn_reg}")
      free_reg(fn_reg)

      for from_reg, ith_reg in regs_to_restore:
        emit(f"  mov {from_reg} {ith_reg}")

      for j in range(arg_count):
        i = arg_count - j - 1
        ith_reg_char = GENERIC_REGS_LST[i]
        ith_reg = Reg(ith_reg_char)
        free_reg(ith_reg)

    case "POP_TOP":
      reg = alloc_reg()
      emit(f"  spo {reg}")
      free_reg(reg)
    case _:
      print(f"Skipping not implemented opcode: {opcode}")

emit("@label keep_alive")
emit("  jpi keep_alive")

with open(outfile_path, "w") as f:
  f.write("\n".join(asm_out))
