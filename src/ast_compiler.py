#!/usr/bin/env python3
# Generate .atk16 assembly from a subset of Python

import sys
import ast

from dataclasses import dataclass
from typing import Literal, Set, cast, Any, TypeAlias
from collections import OrderedDict
from tokenizer import tokenize

if len(sys.argv) != 3:
  print("usage: ast_compiler.py <infile.py> <outfile.atk16>")
  sys.exit(1)

infile_path = sys.argv[1]
outfile_path = sys.argv[2]

Addr = int
Label = str
RegChar = Literal["A", "B", "C", "D", "E", "F", "G", "H"]

@dataclass
class Reg():
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
    self.function_def_asms: list[str] = []
    self.currently_emitting_asm_list = self.program_asm
    self.stack_pointer_offset: int | None = None
    self.bindings: dict[str, Label] = {}
    self.call_depth: int = 0
    self.unique_name_counter = 0
    self.latest_break_target: Label | None = None

    self.reserved_regs: OrderedDict[RegChar, None] = OrderedDict()

  def get_unique_name(self, prefix: str):
    ret =  f"{prefix}_{self.unique_name_counter}"
    self.unique_name_counter += 1
    return ret

  def assign_const(self, name: str, value: int):
    self.const_asm.append(f"@label {name}")
    self.const_asm.append(f"  {value}")
    self.bindings[name] = name

  def emit(self, asm: str):
    asm = asm.strip()
    asm = format_asm_row(asm)

    self.currently_emitting_asm_list.append(asm)

  def alloc_reg(self) -> Reg:
    for reg in GENERIC_REGS:
      if not reg in self.reserved_regs:
        self.reserved_regs[reg] = None
        return Reg(reg)

    raise Exception("Ran out of registers, TODO use stack")

  def free_reg(self, reg: Reg):
    self.reserved_regs.pop(reg.reg)

  class RegContextManager:
    def __init__(self, compiler):
      self.compiler = compiler

    def __enter__(self):
      self.reg = self.compiler.alloc_reg()
      return self.reg

    def __exit__(self, exc_type, exc_value, exc_tb):
      self.compiler.free_reg(self.reg)

  def allocated_reg(self):
    return Compiler.RegContextManager(self)

  def compile(self, bootstrap_asm: str, source: str) -> str:
    tree = ast.parse(source)
    print(ast.dump(tree, indent=4))
    self.visit(tree)
    return "\n".join([
      bootstrap_asm,
      "",
      "\n".join(self.const_asm),
      "",
      "\n".join(self.function_def_asms),
      "",
      "\n".join(self.program_asm),
      ""
    ])

  def generic_visit(self, node: ast.AST) -> Any:
    raise NotImplementedError(f"type {type(node)}, value: {node}")

  def emit_builtin_call(self, name: str, args: list[ast.expr]):
    self.emit(f"; Builtin call {name} {args}")
    match name:
      case "asm":
        match args:
          case [ast.Constant(str(value))]:
            self.emit(value)

          case other: raise Exception(f"asm: invalid args: {other}")
      case "store":
        if len(args) != 2:
          raise Exception("Invalid number of arguments to store: " + str(len(args)))

        # Evaluate args before call
        for arg in args:
          self.visit(arg)

        with self.allocated_reg() as arg1, self.allocated_reg() as arg2:
          self.emit(f"spo {arg2}")
          self.emit(f"spo {arg1}")
          self.emit(f"str {arg2} {arg1}")

  def eval_int_constant_and_spu(self, value: int):
    with self.allocated_reg() as reg:
      if value >= 0 and value < 8:
        self.emit(f"ldi {value} {reg}")
      else:
        name = self.get_unique_name("int")
        self.assign_const(name, value)
        self.emit(f"ldi {name} {reg}")
        self.emit(f"ldr {reg} {reg}")

      self.emit(f"spu {reg}")

  def visit_Module(self, node: ast.Module):
    stmts = node.body
    stack_frame = self.collect_local_variables(stmts)

    self.emit(f"; stack frame: {stack_frame}")
    # Move stack pointer to accommodate local variables
    self.stack_pointer_offset = len(stack_frame)

    self.emit(f"addi SP {self.stack_pointer_offset} SP")

    prev_bindings = self.bindings
    self.bindings: dict[str, Label] = prev_bindings.copy()
    for idx, name in enumerate(stack_frame):
      offset = len(stack_frame) - idx
      offset_label = f"${{SP - {offset}}}"
      self.bindings[name] = offset_label

    for stmt in stmts:
      self.visit(stmt)

    self.bindings = prev_bindings
    self.emit(f"subi SP {self.stack_pointer_offset} SP")

  def visit_Expr(self, expr: ast.Expr):
    # if self.call_depth == 0 and type(expr.value) != ast.Call:
    #   self.emit("; NOP top-level expression")
    #   return

    self.visit(expr.value)

  def visit_UnaryOp(self, node: ast.UnaryOp):
    self.emit(f"; {node}")
    self.visit(node.operand)
    match node.op:
      case ast.Not():
        with self.allocated_reg() as reg1, self.allocated_reg() as reg2:
          self.emit(f"spo {reg1}")
          self.emit(f"ldi 1 {reg2}")
          self.emit(f"andi {reg1} 1 {reg1}")
          self.emit(f"xor {reg1} {reg2} {reg1}")
          self.emit(f"spu {reg1}")

      case ast.Invert(): # aka bitwise not
        with self.allocated_reg() as reg1:
          self.emit(f"spo {reg1}")
          self.emit(f"not {reg1}")
          self.emit(f"spu {reg1}")

      case ast.UAdd(): # +a
        pass # nop

      case ast.USub(): # -a
        with self.allocated_reg() as reg1:
          self.emit(f"spo {reg1}")
          self.emit(f"not {reg1}")
          self.emit(f"addi {reg1} 1 {reg1}")
          self.emit(f"spu {reg1}")

      case other:
        raise NotImplementedError(f"Unhandled UnaryOp: {other}")

  def visit_BoolOp(self, node: ast.BoolOp):
    self.emit(f"; {node.op}")

    match node.op:
      case ast.And():
        label_short_circuit = self.get_unique_name("And_short_circuit")

        with self.allocated_reg() as reg:
          for arg in node.values:
            self.emit(f"; And operand {arg}")
            self.visit(arg)

            self.emit(f"spo {reg}")
            self.emit(f"addi {reg} 0 {reg}")
            self.emit(f"bri zero {label_short_circuit}")

          self.emit(f"@label {label_short_circuit}")
          self.emit(f"spu {reg}")

      case ast.Or():
        label_short_circuit = self.get_unique_name("Or_short_circuit")

        with self.allocated_reg() as reg1, self.allocated_reg() as reg2:
          for arg in node.values:
            self.emit(f"; Or operand {arg}")
            self.visit(arg)

            self.emit(f"spo {reg1}")
            self.emit(f"subi {reg1} 1 {reg2}")
            self.emit(f"bri carry {label_short_circuit}")

          self.emit(f"@label {label_short_circuit}")
          self.emit(f"spu {reg1}")

      case other:
        raise NotImplementedError(f"Unhandled BoolOp: {other}")

  def visit_BinOp(self, node: ast.BinOp):
    self.emit(f"; {node}")

    self.emit(f"; BinOp lhs {node}")
    self.visit(node.left)
    self.emit(f"; BinOp rhs {node}")
    self.visit(node.right)

    with self.allocated_reg() as reg1, self.allocated_reg() as reg2:
      self.emit(f"spo {reg2}")
      self.emit(f"spo {reg1}")

      match node.op:
        case ast.Add():
          self.emit(f"add {reg1} {reg2} {reg1}")
        case ast.Sub():
          self.emit(f"sub {reg1} {reg2} {reg1}")
        case ast.BitAnd():
          self.emit(f"and {reg1} {reg2} {reg1}")
        case ast.BitOr():
          self.emit(f"or {reg1} {reg2} {reg1}")
        case ast.BitXor():
          self.emit(f"xor {reg1} {reg2} {reg1}")
        case ast.LShift():
          self.emit(f"sll {reg1} {reg2} {reg1}")
        case ast.RShift():
          self.emit(f"slr {reg1} {reg2} {reg1}")
        case other:
          raise NotImplementedError(f"Unhandled BinOp: {other}")

      self.emit(f"spu {reg1}")

  def visit_Constant(self, node: ast.Constant):
    self.emit(f"; {node}")
    match node.value:
      case bool(value):
        int_value = 1 if value else 0
        self.eval_int_constant_and_spu(int_value)
      case int(value):
        self.eval_int_constant_and_spu(value)
      case str(value):
        if len(value) > 1:
          raise Exception("Invalid string, only single char values allowed: " + value)

        c = value[0]
        int_value = ord(c)
        self.eval_int_constant_and_spu(int_value)
      case other:
        raise NotImplementedError(f"Unhandled Constant: {other}")

  def visit_If(self, node: ast.If):
    self.emit(f"; {node}")

    self.visit(node.test)
    label_false = self.get_unique_name("If_false_branch")
    label_end = self.get_unique_name("If_end_branch")

    with self.allocated_reg() as reg:
      self.emit(f"spo {reg}")
      self.emit(f"addi {reg} 0 {reg}")
      self.emit(f"bri zero {label_false}")

    for true_branch_stmt in node.body:
      self.visit(true_branch_stmt)

    self.emit(f"jpi {label_end}")
    self.emit(f"@label {label_false}")

    for false_branch_stmt in node.orelse:
      self.visit(false_branch_stmt)

    self.emit(f"@label {label_end}")

  def visit_While(self, node: ast.While):
    self.emit(f"; {node}")

    label_test = self.get_unique_name("While_test")
    label_else = self.get_unique_name("While_else")
    label_end = self.get_unique_name("While_end")

    prev_break_target = self.latest_break_target
    self.latest_break_target = label_end

    with self.allocated_reg() as reg:
      self.emit(f"@label {label_test}")
      self.visit(node.test)
      self.emit(f"spo {reg}")
      self.emit(f"addi {reg} 0 {reg}")
      self.emit(f"bri zero {label_else}")

    for body_stmt in node.body:
      self.visit(body_stmt)

    self.emit(f"jpi {label_test}")

    self.emit(f"@label {label_else}")

    for else_stmt in node.orelse:
      self.visit(else_stmt)

    self.emit(f"@label {label_end}")

    self.latest_break_target = prev_break_target

  def visit_Break(self, node: ast.Break):
    self.emit(f"; {node}")

    if self.latest_break_target is None:
      raise Exception("Invalid break: no break target defined, i.e. no place to break out to")

    self.emit(f"jpi {self.latest_break_target}")

  def visit_For(self, node: ast.For):
    raise Exception("For loops not supported. Consider using a while loop instead.")

  def visit_Lambda(self, node: ast.Lambda):
    raise Exception("Lambda functions not supported. Consider using a named function instead.")

  def collect_local_variables(self, stmts: list[ast.stmt]):
    symbols: list[str] = []
    for stmt in stmts:
      match stmt:
        case ast.Assign(targets=[ast.Name(name)]):
          symbols.append(name)
        case ast.Assign(other):
          raise Exception(f"Unsupported assignment in function definition body: {other}")
        case ast.While(body=body):
          symbols += self.collect_local_variables(body)
        case ast.If(body=tb, orelse=fb):
          symbols += self.collect_local_variables(tb)
          symbols += self.collect_local_variables(fb)
        case other: pass

    # remove duplicates
    return list(set(symbols))

  def visit_FunctionDef(self, node: ast.FunctionDef):
    self.currently_emitting_asm_list = self.function_def_asms
    self.emit(f"; {node}")

    fn_name = node.name
    fn_params = [str(param.arg) for param in node.args.args]
    fn_stmts = node.body

    # Add a "return None" to the end to make sure there the function returns
    if len(fn_stmts) == 0 or type(fn_stmts[len(fn_stmts) - 1]) != ast.Return:
      fn_stmts.append(ast.Return(value=None))

    if len(node.args.kwonlyargs) > 0 or len(node.args.posonlyargs) > 0 or len(node.args.kw_defaults) > 0 or len(node.args.defaults) > 0:
      raise Exception("Only simple positional args are supported for now in function definitions.")

    self.emit(f"@label {fn_name}")
    stack_frame = fn_params + self.collect_local_variables(fn_stmts)
    self.emit(f"; stack frame: {stack_frame}")
    # Move stack pointer to accommodate local variables
    self.stack_pointer_offset = len(stack_frame) - len(fn_params)

    self.emit(f"addi SP {self.stack_pointer_offset} SP")

    prev_bindings = self.bindings
    self.bindings: dict[str, Label] = prev_bindings.copy()
    for idx, name in enumerate(stack_frame):
      offset = len(stack_frame) - idx
      offset_label = f"${{SP - {offset}}}"
      self.bindings[name] = offset_label

    for stmt in fn_stmts:
      self.visit(stmt)

    self.bindings = prev_bindings
    self.currently_emitting_asm_list = self.program_asm

  def visit_Return(self, node: ast.Return):
    self.emit(f"; {node}")
    if self.stack_pointer_offset is None:
      raise Exception("Encountered Return outside a function def context")

    ret = node.value

    if ret is not None:
      self.visit(ret)
    else:
      with self.allocated_reg() as reg:
        self.emit(f"ldi 0 {reg}")
        self.emit(f"spu {reg}")

    self.emit(f"subi SP {self.stack_pointer_offset} SP")
    self.emit("rsr")

  def visit_Call(self, node: ast.Call):
    self.emit(f"; {node}")

    match node.func:
      case ast.Attribute(ast.Name(id="atk16"), attr):
        self.emit_builtin_call(attr, node.args)
      case ast.Name(name):
        # Evaluate args before call
        for arg in node.args:
          self.visit(arg)

        self.emit(f"csi {name}")
      case other:
        raise NotImplementedError(f"Unhandled Call: {other}")

  def visit_Name(self, node: ast.Name):
    self.emit(f"; {node} ({node.id})")

    name = node.id
    if name not in self.bindings:
      raise Exception(f"{name} is unbound")

    addr = self.bindings[name]
    with self.allocated_reg() as reg:
      self.emit(f"ldi {addr} {reg}")
      self.emit(f"ldr {reg} {reg}")
      self.emit(f"spu {reg}")

  def visit_Assign(self, node: ast.Assign):
    targets = node.targets
    value = node.value
    match targets:
      case [ast.Name(name)]:
        if name not in self.bindings:
          raise Exception(f"{name} is not in bindings. Bindings:\n{self.bindings}")

        self.visit(value)
        addr = self.bindings[name]

        with self.allocated_reg() as reg1, self.allocated_reg() as reg2:
          self.emit(f"spo {reg1}")
          self.emit(f"ldi {addr} {reg2}")
          self.emit(f"str {reg1} {reg2}")
      case other:
        raise Exception(f"Unsupported assign targets: {other}")

  def visit_Import(self, node: ast.Import) -> Any:
    match node.names:
      case [ast.alias(name="atk16")]:
        return
      case other:
        raise NotImplementedError(f"Unsupported import {other}")

  def visit_AnnAssign(self, node: ast.AnnAssign):
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
    asm = [tokenize(row, retain_curlies=True) for row in asm]
    asm = self.compact_spu_spo_pattern(asm)
    asm = self.compact_target_mov_pattern(asm)
    asm = self.compact_target_mov_pattern(asm)
    asm = self.compact_mov_source_pattern(asm)
    asm = self.compact_mov_source_pattern(asm)
    asm = self.compact_spu_load_spo_pattern(asm)
    # asm = self.convert_alr_to_ali(asm)
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

  def compact_target_mov_pattern(self, asm: list[list[str]]) -> list[list[str]]:
    ops_with_target = ["ldi", "ldr", "add", "sub", "addi", "subi", "and", "or", "xor", "sll", "slr", "sar", "slli", "slri", "sari", "inc", "dec", "mov", "ali", "alr", "lpc"]
    i = 0
    result: list[list[str]] = []
    while i < len(asm):
      current = asm[i]
      next = asm[i + 1] if i + 1 < len(asm) else None
      i += 1
      if current[0].startswith("@") or next is None:
        result.append(current)
        continue

      if current[0] in ops_with_target and next[0] == "mov":

        op_op, op_operands, op_target_reg = current[0], current[1:len(current) - 1], current[len(current) - 1]
        mov_from_reg, mov_to_reg = next[1], next[2]

        if op_target_reg == mov_from_reg:
          ret = [op_op, *op_operands, mov_to_reg]
          result.append(ret)
          i += 1
          continue

      result.append(current)

    return result

  def compact_mov_source_pattern(self, asm: list[list[str]]) -> list[list[str]]:
    ops_with_source = ["str", "ldr", "add", "sub", "addi", "subi", "and", "or", "xor", "sll", "slr", "sar", "slli", "slri", "sari", "mov", "ali", "alr"]
    i = 0
    result: list[list[str]] = []
    while i < len(asm):
      current = asm[i]
      next = asm[i + 1] if i + 1 < len(asm) else None
      i += 1
      if current[0].startswith("@") or next is None:
        result.append(current)
        continue

      if current[0] == "mov" and next[0] in ops_with_source:
        mov_from_reg, mov_to_reg = current[1], current[2]
        op_op, op_source_reg, op_operands = next[0], next[1], next[2:]

        if op_source_reg == mov_to_reg:
          ret = [op_op, op_source_reg, *op_operands]
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
