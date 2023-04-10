#!/usr/bin/env python3
# Assemble ATK16 assembly to bytecode

import sys
from typing import Callable
import importlib
from assembler_ops import *
from assembler_eval import *

if len(sys.argv) != 3:
  print("usage: assembler.py <infile> <outfile> # read from file")
  print("       assembler.py - <outfile>        # read from stdin")
  sys.exit(1)

infile_path = sys.argv[1]
outfile_path = sys.argv[2]

src = ""
if (infile_path == "-"):
  for line in sys.stdin:
    src += line
else:
  with open(infile_path, "r") as f:
    src = f.read()

src_lines = src.splitlines()

### Utils

def parse(line: str) -> list[str]:
  depth = 0
  result: list[str] = []
  acc: str = ""
  for c in line:
    if c.isspace() and depth == 0:
      result.append(acc)
      acc = ""
    elif c == "(":
      depth += 1
      acc += "("
    elif c == ")":
      depth -= 1
      acc += ")"
    else:
      acc += c

  result.append(acc)
  return list(filter(lambda x: len(x) > 0, result))

# 1st pass, gather labels, set options, eval operations

options: dict[str, str] = {
  "stack_pointer": "6" # RG
}
address: int = 0

for (lineNo, line) in enumerate(src_lines):
  line = line.split(";")[0].strip()
  if line == "": continue
  keyword, *args = line.lower().split()
  match keyword:
    case "@address":
      address = eval(args[0])
      continue
    case "@label":
      labels[args[0]] = address
      continue
    case "@opt":
      opt_name = args[0]
      opt_value = constants.get(args[1], args[1])
      options[opt_name] = opt_value
    case "@use":
      module_name, ops = args[0].split(":")
      ops_split = ops.split(",")
      module = importlib.import_module(module_name)
      mod_operations: dict[str, Callable[..., list[int]]] = module.operations
      for op in mod_operations:
        callable = mod_operations[op]
        if ops == "*" or op in ops_split:
            operations[op] = callable
    case _:
      pass

  address += 1

# 2nd pass

# def make_spu(reg: str) -> list[int]:
#   stack_pointer = options["stack_pointer"]
#   reg = eval_symbol(reg)
#   words_inc: list[int] = make_inc(stack_pointer, stack_pointer)
#   words_str: list[int] = make_str(reg, stack_pointer)
#   return words_inc + words_str

address: int = 0
nop = bytearray([0b1000_0000, 0])
result = bytearray()
# initially one nop
result.extend(nop)

for (lineNo, line) in enumerate(src_lines):
  line = line.split(";")[0].strip()
  if line == "": continue
  keyword, *args = parse(line.lower())

  if keyword in operations:
    words = operations[keyword](address, *args)
  else:
    match keyword:
      # Directives
      case "@address":
        address = eval_expr(args[0])
        continue
      case "@label" | "@opt" | "@use":
        continue

      # Default case: evaluate as is (e.g. data word)
      case _:
        try:
          words = [eval_expr(keyword)]
        except:
          raise Exception(f"Invalid assembly at {infile_path}:{lineNo + 1}\n\n{line}")

  if len(result) < 2 * address + 1:
    result.extend((2 * address + 1 - len(result)) * nop)

  for (label, label_addr) in labels.items():
    if address == label_addr:
      print(f"{label}:")

  for word in words:
    print(f"{address:>08x}  0x{word:>04x}  {line}")
    result[2 * address + 0] = ((word >> 8) & 0xff)
    result[2 * address + 1] = ((word >> 0) & 0xff)
    address += 1

with open(outfile_path, "wb") as f:
  f.write(result)

print(f"Wrote {len(result)} bytes to {outfile_path}")
