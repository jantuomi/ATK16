#!/usr/bin/env python3
# Generate .atk16 assembly from a subset of Python

import sys
import ast

from dataclasses import dataclass
from typing import Literal, Set, cast, Any, TypeAlias, TypeVar
from collections import OrderedDict


if len(sys.argv) != 3:
  print("usage: ast_compiler.py <infile.py> <outfile.atk16>")
  sys.exit(1)

infile_path = sys.argv[1]
outfile_path = sys.argv[2]

Label = str
StackOffset = int
RegChar = Literal["A", "B", "C", "D", "E", "F", "G", "H"]

@dataclass
class Reg():
  reg: RegChar

  def __str__(self):
    return f"R{self.reg}"

ALL_REGS: list[RegChar] = ["A", "B", "C", "D", "E", "F", "G", "H"]
FRAME_POINTER_REG_CHAR: RegChar = "G" # points to base of stack frame
FRAME_POINTER_REG = Reg(FRAME_POINTER_REG_CHAR)
STACK_POINTER_REG_CHAR: RegChar = "H"
STACK_POINTER_REG = Reg(STACK_POINTER_REG_CHAR)
SPECIAL_REGS: list[RegChar] = [FRAME_POINTER_REG_CHAR, STACK_POINTER_REG_CHAR]
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

    self.local_bindings: dict[str, StackOffset] = {}
    self.const_bindings: dict[str, Label] = {}

    self.unique_name_counter = 0
    self.latest_break_target: Label | None = None

    self.reserved_regs: OrderedDict[RegChar, None] = OrderedDict()

  def get_unique_name(self, prefix: str):
    ret =  f"{prefix}_{self.unique_name_counter}"
    self.unique_name_counter += 1
    return ret

  def assign_const(self, name: str, value: int):
    prev_currently_emitting_asm_list = self.currently_emitting_asm_list
    self.currently_emitting_asm_list = self.const_asm

    self.emit_label(name)
    self.emit(f"{value}")

    self.const_bindings[name] = name

    self.currently_emitting_asm_list = prev_currently_emitting_asm_list

  def emit(self, asm: str):
    asm = asm.strip()
    asm = format_asm_row(asm)

    self.currently_emitting_asm_list.append(asm)

  def emit_label(self, label_name: str):
    label_name = label_name.lower()
    self.emit(f"@label {label_name}")

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

    def __enter__(self) -> Reg:
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

             # return None
            with self.allocated_reg() as arg:
              self.emit(f"ldi 0 {arg}")
              self.emit(f"spu {arg}")

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

          # return None
          self.emit(f"ldi 0 {arg1}")
          self.emit(f"spu {arg1}")

      case "load":
        if len(args) != 1:
          raise Exception("Invalid number of arguments to load: " + str(len(args)))

        self.visit(args[0])

        self.emit("; (load) dereferencing top of stack")
        with self.allocated_reg() as arg:
          self.emit(f"spo {arg}")
          self.emit(f"ldr {arg} {arg}")
          self.emit(f"spu {arg}")

      case "ord":
        if len(args) != 1:
          raise Exception("Invalid number of arguments to ord: " + str(len(args)))

        self.visit(args[0])

      case other:
        raise Exception(f"Unknown builtin: {other}")

  def eval_int_constant_and_spu(self, value: int):
    with self.allocated_reg() as reg:
      if value >= 0 and value < 8:
        self.emit(f"ldi {value} {reg}")
      else:
        name = self.get_unique_name("int")
        self.assign_const(name, value)
        self.emit(f"ldi {name} {reg}")
        self.emit(f"ldr {reg} {reg} ; {name} = {value}")

      self.emit(f"spu {reg}")

  def visit_Module(self, node: ast.Module):
    stmts = node.body
    frame_names = self.collect_local_variables(stmts)

    self.emit("; stack frame with offsets")
    for offset, name in enumerate(frame_names):
      self.local_bindings[name] = offset
      self.emit(f"; {name} {offset}")

    self.emit(f"addi SP {len(frame_names)} SP")

    for stmt in stmts:
      if type(stmt) == ast.Expr and type(stmt.value) != ast.Call:
        self.emit("; NOP top-level expression")
        continue

      self.visit(stmt)

    self.emit(f"mov FP SP")
    self.emit("hlt")

  def visit_Expr(self, expr: ast.Expr):
    self.visit(expr.value)

    with self.allocated_reg() as reg:
      self.emit("; popping free-standing Expr result from stack")
      self.emit(f"spo {reg}")

  def visit_UnaryOp(self, node: ast.UnaryOp):
    self.emit(f"; {node.op}")
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

          self.emit_label(label_short_circuit)
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

          self.emit_label(label_short_circuit)
          self.emit(f"spu {reg1}")

      case other:
        raise NotImplementedError(f"Unhandled BoolOp: {other}")

  def visit_BinOp(self, node: ast.BinOp):
    self.emit(f"; {node.op}")

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
      if type(true_branch_stmt) == ast.Expr and type(true_branch_stmt.value) != ast.Call:
        self.emit("; NOP top-level expression")
        continue

      self.visit(true_branch_stmt)

    self.emit(f"jpi {label_end}")
    self.emit_label(label_false)

    for false_branch_stmt in node.orelse:
      if type(false_branch_stmt) == ast.Expr and type(false_branch_stmt.value) != ast.Call:
        self.emit("; NOP top-level expression")
        continue

      self.visit(false_branch_stmt)

    self.emit_label(label_end)

  def visit_Compare(self, node: ast.Compare):
    self.emit(f"; {node}")

    if len(node.ops) > 1:
      raise Exception("Multiple compare ops not supported")

    if len(node.comparators) > 1:
      raise Exception("Multiple comparators not supported")

    op = node.ops[0]
    left = node.left
    right = node.comparators[0]

    label_true = self.get_unique_name("Compare_true")
    label_end = self.get_unique_name("Compare_end")

    self.visit(left)
    self.visit(right)

    self.emit(f"; Comparing {left} {op} {right}")
    with self.allocated_reg() as reg_lhs, self.allocated_reg() as reg_rhs:
      match op:
        case ast.Lt(): # lhs < rhs
          self.emit(f"spo {reg_rhs}")
          self.emit(f"spo {reg_lhs}")

          self.emit(f"sub {reg_lhs} {reg_rhs} {reg_lhs}")
          self.emit(f"bri carry {label_true}")

          # false branch
          self.emit(f"ldi 0 {reg_lhs}")
          self.emit(f"spu {reg_lhs}")
          self.emit(f"jpi {label_end}")

          # true branch
          self.emit_label(label_true)
          self.emit(f"ldi 1 {reg_lhs}")
          self.emit(f"spu {reg_lhs}")

          self.emit_label(label_end)

  def visit_While(self, node: ast.While):
    self.emit(f"; {node}")

    label_test = self.get_unique_name("While_test")
    label_else = self.get_unique_name("While_else")
    label_end = self.get_unique_name("While_end")

    prev_break_target = self.latest_break_target
    self.latest_break_target = label_end

    with self.allocated_reg() as reg:
      self.emit_label(label_test)
      self.visit(node.test)
      self.emit(f"spo {reg}")
      self.emit(f"addi {reg} 0 {reg}")
      self.emit(f"bri zero {label_else}")

    for body_stmt in node.body:
      if type(body_stmt) == ast.Expr and type(body_stmt.value) != ast.Call:
        self.emit("; NOP top-level expression")
        continue

      self.visit(body_stmt)

    self.emit(f"jpi {label_test}")

    self.emit_label(label_else)

    for else_stmt in node.orelse:
      self.visit(else_stmt)

    self.emit_label(label_end)

    self.latest_break_target = prev_break_target

  def visit_Pass(self, node: ast.Pass):
    self.emit(f"; {node}")
    pass

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
    result: list[str] = []
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

    for symbol in symbols:
      if symbol not in result:
        result.append(symbol)

    return result

  def visit_FunctionDef(self, node: ast.FunctionDef):
    prev_currently_emitting_asm_list = self.currently_emitting_asm_list
    self.currently_emitting_asm_list = self.function_def_asms
    self.emit(f"; {node}")

    fn_name = node.name
    fn_params = [str(param.arg) for param in node.args.args]
    fn_stmts = node.body

    # Add a "return None" to the end to make sure the function returns
    if len(fn_stmts) == 0 or type(fn_stmts[len(fn_stmts) - 1]) != ast.Return:
      fn_stmts.append(ast.Return(value=None))

    if len(node.args.kwonlyargs) > 0 or len(node.args.posonlyargs) > 0 or len(node.args.kw_defaults) > 0 or len(node.args.defaults) > 0:
      raise Exception("Only simple positional args are supported for now in function definitions.")

    self.emit_label(fn_name)
    local_var_names = self.collect_local_variables(fn_stmts)
    frame_names = fn_params + local_var_names

    # Move stack pointer to accommodate local variables
    if len(local_var_names) > 0:
      self.emit(f"addi SP {len(local_var_names)} SP")

    prev_local_bindings = self.local_bindings
    self.local_bindings = {}
    self.emit("; stack frame with offsets")
    for offset, name in enumerate(frame_names):
      self.local_bindings[name] = offset
      self.emit(f"; {name} {offset}")

    for stmt in fn_stmts:
      self.visit(stmt)

    self.local_bindings = prev_local_bindings
    self.currently_emitting_asm_list = prev_currently_emitting_asm_list

  def visit_Return(self, node: ast.Return):
    self.emit(f"; {node}")

    with self.allocated_reg() as reg1, self.allocated_reg() as reg2:
      if node.value is not None:
        self.visit(node.value)
      else:
        self.emit(f"ldi 0 {reg1}")
        self.emit(f"spu {reg1}")

      self.emit(f"; return value is on top of stack")

      self.emit(f"; return from function")
      self.emit(f"subi FP 1 FP")
      self.emit(f"ldr FP {reg2}")
      self.emit(f"jpr {reg2}")

  def visit_Call(self, node: ast.Call):
    self.emit(f"; {node}")

    match node.func:
      case ast.Attribute(ast.Name(id="atk16"), attr):
        self.emit_builtin_call(attr, node.args)
      case ast.Name(name):
        self.emit(f"; Call function {name}")

        # Push the current FP
        self.emit("spu FP")
        #self.emit("mov SP FP") # BUG: FP needs to be moved AFTER EVALING ARGS! args depend on FP in calling frame if there are names to resolve!

        # Reserve a stack slot for the return address
        # FP will point to this slot
        with self.allocated_reg() as reg:
          self.emit(f"ldi 0 {reg}")
          self.emit(f"spu {reg}")

        # Evaluate args before call, pushing them to stack
        for arg in node.args:
          self.visit(arg)

        self.emit(f"subi SP {1 + len(node.args)} FP")

        with self.allocated_reg() as reg:
          self.emit("; set up return address and jump to subroutine")
          self.emit(f"lpc {reg}")
          self.emit(f"addi {reg} 4 {reg}") # imm must equal number of primitive instrs from lpc until jpi
          self.emit(f"str {reg} FP")
          self.emit(f"addi FP 1 FP")
          self.emit(f"jpi {name}")
          # ...after return from call...
          self.emit(f"spo {reg}")

          self.emit("mov FP SP") # move stack pointer to base of stack frame, i.e. top of previous frame
          self.emit("spo FP") # restore FP of previous frame
          self.emit(f"spu {reg}")

      case other:
        raise NotImplementedError(f"Unhandled Call: {other}")

  def resolve_name(self, name: str) -> Label | StackOffset:
    if name in self.local_bindings:
      return self.local_bindings[name]

    if name in self.const_bindings:
      return self.const_bindings[name]

    raise Exception(f"{name} is unbound")

  def visit_Name(self, node: ast.Name):
    self.emit(f"; {node} ({node.id})")

    name = node.id.lower()
    addr = self.resolve_name(name)

    with self.allocated_reg() as reg:
      match addr:
        case Label(label):
          self.emit(f"ldi {label} {reg}")
        case StackOffset(offset):
          self.emit(f"ldi {offset} {reg}")
          self.emit(f"add FP {reg} {reg}")
        case other:
          raise Exception(f"Unsupported addr value: {other}")

      self.emit(f"ldr {reg} {reg}")
      self.emit(f"spu {reg}")

  def visit_Assign(self, node: ast.Assign):
    self.emit(f"; {node}")

    targets = node.targets
    value = node.value
    match targets:
      case [ast.Name(name)]:
        offset = self.resolve_name(name)

        if type(offset) != StackOffset:
          raise Exception(f"Invalid address {offset} for name {name}. Can only assign to stack offsets.")

        self.emit(f"; assigning {name} at FP + {offset}")

        with self.allocated_reg() as reg1, self.allocated_reg() as reg2:
          self.emit(f"; evaluating value to be assigned")
          self.visit(value)
          self.emit(f"; assigning stack address (FP + {offset}) := top of stack")
          self.emit(f"spo {reg1}") # value

          self.emit(f"ldi {offset} {reg2}")
          self.emit(f"add {reg2} FP {reg2}") # address
          self.emit(f"str {reg1} {reg2}")

      case other:
        raise Exception(f"Unsupported assign targets: {other}")

  def visit_AugAssign(self, node: ast.AugAssign):
    op = node.op
    lhs = node.target
    rhs = node.value

    if type(lhs) != ast.Name:
      raise Exception("AugAssign to non-Name lhs not yet supported!")

    # construct bin op and assignment
    bin_op = ast.BinOp(left=lhs, op=op, right=rhs)
    assign = ast.Assign(targets=[lhs], value=bin_op)

    self.visit(assign)

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
          self.assign_const(name.lower(), value)
          return

    raise NotImplementedError("Unhandled AnnAssign:\n" + ast.dump(node, indent=4))

  def get_module_exports(self, node: ast.Module):
    result: list[str] = []
    for stmt in node.body:
      match stmt:
        case ast.FunctionDef(name):
          result.append(name)
        case _:
          # only function exported since recognizing which assignments are constants is kinda hard
          pass

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

with open(outfile_path, "w") as f:
  f.write(asm_out)

