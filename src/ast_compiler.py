#!/usr/bin/env python3
# Generate .atk16 assembly from a subset of Python

from _ast import AnnAssign, Expr, Module
import sys
import ast

from dataclasses import dataclass
from typing import Literal, Set, cast, Any, TypeAlias
from collections import OrderedDict

if len(sys.argv) != 3:
  print("usage: ast_compiler.py <infile.py> <outfile.atk16>")
  sys.exit(1)

infile_path = sys.argv[1]
outfile_path = sys.argv[2]

Addr = int
Label = str
RegChar = Literal["A", "B", "C", "D", "E", "F", "G", "H"]

@dataclass
class Reg:
  reg: RegChar

  def __str__(self):
    return f"R{self.reg}"

ALL_REGS: list[RegChar] = ["A", "B", "C", "D", "E", "F", "G", "H"]
STACK_POINTER_REG = "G"
CSR_SCRATCH_REG = "H"
SPECIAL_REGS: list[RegChar] = [STACK_POINTER_REG, CSR_SCRATCH_REG]
GENERIC_REGS: OrderedDict[RegChar, None] = OrderedDict()
for char in ALL_REGS:
  if char not in SPECIAL_REGS:
    GENERIC_REGS[cast(RegChar, char)] = None

def format_asm_row(asm: str) -> str:
  if not (asm.startswith("@") or asm.startswith(";")) and not asm.startswith("  ") and len(asm) > 0:
    return "  " + asm
  else:
    return asm

class Compiler(ast.NodeVisitor):
  def __init__(self):
    self.const_asm: list[str] = []
    self.program_asm: list[str] = [
      "@label main"
    ]
    self.const_bindings: dict[str, Label] = {}
    self.call_depth: int = 0
    self.unique_name_counter = 0

    self.reserved_regs: OrderedDict[RegChar, None] = OrderedDict()

  def get_unique_name(self, prefix: str):
    ret =  f"{prefix}_{self.unique_name_counter}"
    self.unique_name_counter += 1
    return ret

  def assign_const(self, name: str, value: int):
    self.const_asm.append(f"@label {name}")
    self.const_asm.append(f"  {value}")
    self.const_bindings[name] = name

  def emit(self, asm: str):
    asm = asm.strip()
    asm = format_asm_row(asm)

    self.program_asm.append(asm)

  def alloc_reg(self) -> Reg:
    for reg in GENERIC_REGS:
      if not reg in self.reserved_regs:
        self.reserved_regs[reg] = None
        return Reg(reg)

    raise Exception("Ran out of registers, TODO use stack")

  def free_reg(self, reg: Reg):
    self.reserved_regs.pop(reg.reg)

  def compile(self, bootstrap_asm: str, source: str) -> str:
    tree = ast.parse(source)
    print(ast.dump(tree, indent=4))
    self.visit(tree)
    return "\n".join([
      bootstrap_asm,
      "",
      "\n".join(self.const_asm),
      "",
      "\n".join(self.program_asm)
    ])

  def emit_builtin_call(self, name: str, args: list[ast.expr]):
    self.emit(f"; builtin call {name} {args}")
    match name:
      case "asm":
        match args:
          case [ast.Constant(value)]:
            if type(value) != str:
              raise Exception(f"asm: invalid arg type: {type(value)}")

            self.emit(value)
          case _: raise Exception(f"asm: invalid args: {args}")
      case "set_graphics_mode":
        match args:
          case [ast.Constant(value)]:
            if type(value) != int:
              raise Exception(f"set_graphics_mode: invalid arg type: {type(value)}")

            reg1 = self.alloc_reg()
            reg2 = self.alloc_reg()
            self.emit(f"ldi vt_gr_mode_addr {reg1}")
            self.emit(f"ldr {reg1} {reg1}")
            self.emit(f"ldi {value} {reg2}")
            self.emit(f"str {reg2} {reg1}")
            self.free_reg(reg1)
            self.free_reg(reg2)

          case _: raise Exception(f"set_graphics_mode: invalid args: {args}")
      case "store":
        if len(args) != 2:
          raise Exception("Invalid number of arguments to store: " + str(len(args)))

        self.eval_expr_and_spu(args[0])
        self.eval_expr_and_spu(args[1])
        arg1 = self.alloc_reg()
        arg2 = self.alloc_reg()
        self.emit(f"spo {arg2}")
        self.emit(f"spo {arg1}")
        self.emit(f"str {arg2} {arg1}")
        self.free_reg(arg1)
        self.free_reg(arg2)

  def eval_int_constant_and_spu(self, value: int):
    reg = self.alloc_reg()
    if value >= 0 and value < 8:
      self.emit(f"ldi {value} {reg}")
    else:
      name = self.get_unique_name("int")
      self.assign_const(name, value)
      self.emit(f"ldi {name} {reg}")
      self.emit(f"ldr {reg} {reg}")

    self.emit(f"spu {reg}")
    self.free_reg(reg)

  def eval_expr_and_spu(self, expr: ast.expr):
    match expr:
      case ast.Constant(int(value)):
        self.eval_int_constant_and_spu(value)
      case ast.Constant(str(value)):
        if len(value) > 1:
          raise Exception("Invalid string, only single char values allowed: " + value)

        c = value[0]
        int_value = ord(c)
        self.eval_int_constant_and_spu(int_value)


  def visit_Module(self, node: Module):
    for stmt in node.body:
      self.visit(stmt)

  def visit_Expr(self, expr: Expr):
    if self.call_depth == 0 and type(expr.value) != ast.Call:
      self.emit("; NOP top-level expression")
      return

    match expr.value:
      case ast.Call(func, args, keywords):
        match func:
          case ast.Attribute(ast.Name(id="atk16"), attr):
            print("FOUND CAPTURED ATK16 CALL: " + attr)
            self.emit_builtin_call(attr, args)
          case ast.Name(name):
            addr = self.const_bindings[name]
            self.emit(f"csi {addr}")
          case _:
            raise NotImplementedError("Unhandled Call: " + str(func))
      case _:
        raise NotImplementedError("Unhandled Expr.value: " + str(expr.value))

  def visit_AnnAssign(self, node: AnnAssign):
    match node:
      case ast.AnnAssign(
        target=ast.Name(name),
        annotation=ast.Attribute(
          value=ast.Name(id="atk16"),
          attr="ConstInt"),
        value=ast.Constant(value) # TODO: constant folding
      ):
        if type(value) == int:
          self.assign_const(name, value)
          return

    raise NotImplementedError("Unhandled AnnAssign:\n" + ast.dump(node, indent=4))

class Optimizer:
  def __init__(self):
    pass

  def optimize(self, asm_str: str):
    asm = asm_str.split("\n")
    asm = [row.strip() for row in asm]
    asm = [self.strip_comment(row) for row in asm]
    asm = [row for row in asm if not len(row) == 0]
    asm = [row.split() for row in asm]
    asm = self.compact_spu_spo_pattern(asm)
    asm = self.compact_load_mov_pattern(asm)
    asm = self.compact_load_mov_pattern(asm)
    asm = self.compact_spu_load_spo_pattern(asm)
    result = "\n".join([format_asm_row(" ".join(row)) for row in asm])

    return result

  def strip_comment(self, row: str):
    ret: str = ""
    for c in row:
      if c == ";": break
      ret += c

    return ret

  def compact_spu_spo_pattern(self, asm: list[list[str]]) -> list[list[str]]:
    i = 0
    result: list[list[str]] = []
    while i < len(asm):
      current = asm[i]
      next = asm[i + 1] if i + 1 < len(asm) else None
      i += 1
      if current[0].startswith("@") or next is None:
        result.append(current)
        continue

      if current[0] == "spu" and next[0] == "spo":
        arg_current = current[1]
        arg_next = next[1]

        if arg_current == arg_next:
          pass # remove both spu and spo
        else:
          mov = ["mov", arg_current, arg_next]
          result.append(mov)

        i += 1
        continue

      result.append(current)

    return result

  def compact_load_mov_pattern(self, asm: list[list[str]]) -> list[list[str]]:
    i = 0
    result: list[list[str]] = []
    while i < len(asm):
      current = asm[i]
      next = asm[i + 1] if i + 1 < len(asm) else None
      i += 1
      if current[0].startswith("@") or next is None:
        result.append(current)
        continue

      if (current[0] == "ldi" or current[0] == "ldr") and next[0] == "mov":
        load_op, load_from, load_target_reg = current[0], current[1], current[2]
        mov_from_reg, mov_to_reg = next[1], next[2]

        if load_target_reg == mov_from_reg:
          ret = [load_op, load_from, mov_to_reg]
          result.append(ret)
          i += 1
          continue

      result.append(current)

    return result

  def compact_spu_load_spo_pattern(self, asm: list[list[str]]) -> list[list[str]]:
    #  spu RA
    #  ldi int_7 RA
    #  ldr RA RB
    #  spo RA
    # OR
    #  spu RA
    #  ldi 3 RB
    #  spo RA

    i = 0
    result: list[list[str]] = []
    while i < len(asm):
      instr0 = asm[i]
      instr1 = asm[i + 1] if i + 1 < len(asm) else None
      instr2 = asm[i + 2] if i + 2 < len(asm) else None
      instr3 = asm[i + 3] if i + 3 < len(asm) else None
      i += 1
      if instr0[0].startswith("@") or instr1 is None or instr2 is None or instr3 is None:
        result.append(instr0)
        continue

      if instr0[0] == "spu" and instr1[0] == "ldi" and instr2[0] == "ldr" and instr3[0] == "spo":
        spu_op, spu_reg = instr0
        ldi_op, ldi_imm, ldi_target_reg = instr1
        ldr_op, ldr_from_reg, ldr_to_reg = instr2
        spo_op, spo_reg = instr3

        if spu_reg == spo_reg and ldi_target_reg == ldr_from_reg and ldr_from_reg != ldr_to_reg:
          ret0 = f"ldi {ldi_imm} {ldr_to_reg}".split()
          ret1 = f"ldr {ldr_to_reg} {ldr_to_reg}".split()
          result.append(ret0)
          result.append(ret1)
          i += 3
          continue

      elif instr0[0] == "spu" and instr1[0] == "ldi" and instr2[0] == "spo":
        spu_op, spu_reg = instr0
        ldi_op, ldi_imm, ldi_target_reg = instr1
        spo_op, spo_reg = instr2

        if spu_reg == spo_reg and ldi_target_reg != spu_reg:
          ret0 = f"ldi {ldi_imm} {ldi_target_reg}".split()
          result.append(ret0)
          i += 2
          continue

      result.append(instr0)

    return result

with open(infile_path, "r") as f:
  source_py = f.read()

with open("asm/ast_compiler_bootstrap.atk16", "r") as f:
  bootstrap_asm = f.read()

compiler = Compiler()
asm_out = compiler.compile(
  bootstrap_asm,
  source_py,
)

optimizer = Optimizer()
asm_out_optimized = optimizer.optimize(asm_out)

with open(outfile_path, "w") as f:
  f.write(asm_out)

with open(f"{outfile_path}_optimized", "w") as f:
  f.write(asm_out_optimized)
