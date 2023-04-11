#!/usr/bin/env python3
# Assemble ATK16 assembly to bytecode

import sys
from assembler_ops import *
from assembler_eval import *
from asm_pass1 import pass_1
from asm_pass2 import pass_2
from asm_pass3 import pass_3
from asm_pass4 import pass_4

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

result1 = pass_1(src_lines)
result2 = pass_2(result1)
result3 = pass_3(result2)
result4 = pass_4(result3)

nop = bytearray([0b1000_0000, 0])
result = bytearray()
# initially one nop
result.extend(nop)

for line in result4.lines:
  for (label, label_addr) in result4.labels.items():
    if line.address == label_addr:
      print(f"{label}:")

  if len(result) < 2 * line.address + 1:
    result.extend((2 * line.address + 1 - len(result)) * nop)

  out_line = f"{line.address:>08x}  0x{line.word:>04x}  {line.text}"
  out_spaces_n = (42 - len(out_line))
  out_spaces = out_spaces_n * " " if out_spaces_n > 0 else 4 * " "
  print(f"{out_line}{out_spaces}{line.original_text}")
  result[2 * line.address + 0] = ((line.word >> 8) & 0xff)
  result[2 * line.address + 1] = ((line.word >> 0) & 0xff)

with open(outfile_path, "wb") as f:
  f.write(result)

print(f"Wrote {len(result)} bytes to {outfile_path}")
