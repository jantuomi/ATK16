#!/usr/bin/env python3
# Assemble ATK16 assembly to bytecode

import sys
from .asm_ops import *
from .asm_eval import *
from .asm_pass0 import pass_0
from .asm_pass1 import pass_1
from .asm_pass2 import pass_2
from .asm_pass3 import pass_3, DbgLabelTable, DbgSourceTable
from .asm_pass4 import pass_4
from .pad_bin import pad_binary

@dataclass
class AssemblyResult:
  program: bytearray
  dbg_label_table: DbgLabelTable
  dbg_source_table: DbgSourceTable

def assemble(source: str, file_name: str) -> AssemblyResult:
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

  return AssemblyResult(
    program=result,
    dbg_label_table=result4.dbg_label_table,
    dbg_source_table=result4.dbg_source_table,
  )

def main():
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
    f.write(result.program)

  pad_binary(outfile_path, 64 * 1024)

  print(f"Wrote 64 KB to {outfile_path} ({len(result.program)} B without padding)")

  dbg_outfile_path = outfile_path + ".dbg"

  with open(dbg_outfile_path, "w") as f:
    for i in range(32 * 1024): # 32K words
      line = f"0x{i:>04x};"
      if i not in result.dbg_source_table:
        continue

      dbg_source_info = result.dbg_source_table[i]
      line += f"{dbg_source_info.src_file}:{dbg_source_info.line_num};{dbg_source_info.expanded_text};{dbg_source_info.original_text};"

      if i in result.dbg_label_table:
        line += f"{';'.join(result.dbg_label_table[i])}"

      f.write(line + "\n")

if __name__ == "__main__":
  main()
