#!/usr/bin/env python3
# Assemble ATK16 assembly to bytecode

import sys
from .asm_ops import *
from .asm_eval import *
from .asm_pass0 import pass_0
from .asm_pass1 import pass_1
from .asm_pass2 import pass_2
from .asm_pass3 import pass_3
from .asm_pass4 import pass_4

def assemble(source: str, file_name: str) -> bytearray:
  src_lines = source.splitlines()

  result0 = pass_0(src_lines, file_name)
  result1 = pass_1(result0)
  result2 = pass_2(result1)
  result3 = pass_3(result2)
  result4 = pass_4(result3)

  nop = bytearray([0b1000_0000, 0])
  result = bytearray()
  # initially one nop
  result.extend(nop)

  for line in result4.lines:
    for (symbol, symbol_value) in result4.symbols.items():
      if line.address == symbol_value:
        print(f"{symbol}:")

    if len(result) < 2 * line.address + 1:
      result.extend((2 * line.address + 1 - len(result)) * nop)

    out_line = f"{line.address:>08x}  0x{line.word:>04x}  {line.text}"
    out_spaces_n = (42 - len(out_line))
    out_spaces = out_spaces_n * " " if out_spaces_n > 0 else 4 * " "
    print(f"{out_line}{out_spaces}{line.original_text}")
    result[2 * line.address + 0] = ((line.word >> 8) & 0xff)
    result[2 * line.address + 1] = ((line.word >> 0) & 0xff)

  return result

if __name__ == "__main__":
  if len(sys.argv) != 3:
    print("usage: assembler.py <infile> <outfile> # read from file")
    print("       assembler.py - <outfile>        # read from stdin")
    sys.exit(1)

  infile_path = sys.argv[1]
  outfile_path = sys.argv[2]

  src: str = ""
  if (infile_path == "-"):
    for line in sys.stdin:
      src += line
  else:
    with open(infile_path, "r") as f:
      src = f.read()

  result = assemble(src, infile_path)
  with open(outfile_path, "wb") as f:
    f.write(result)

  print(f"Wrote {len(result)} bytes to {outfile_path}")
