#!/usr/bin/env python3
# Assemble ATK16 assembly to bytecode

import sys
import os
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

def show_help():
  print("usage: assembler.py [-h|--help] [-o outfile] <infile>")
  print("")
  print("Positional arguments:")
  print("  <infile>     input file in .atk16 assembler text format (use - for stdin)")
  print("")
  print("Options:")
  print("  -o outfile   output file in binary format")
  print("  -h|--help    show this help")

@dataclass
class CliOptions:
  infile: str
  outfile: str

def parse_cli_args(args: list[str]) -> CliOptions:
  i = 1
  infile: str | None = None
  outfile: str | None = None
  while i < len(args):
    if args[i] == "-o":
      if i + 1 >= len(args):
        print("Missing argument for -o")
        show_help()
        sys.exit(1)
      outfile = args[i + 1]
      i += 2
    elif args[i] == "-h" or args[i] == "--help":
      show_help()
      sys.exit(0)
    elif not infile:
      infile = args[i]
      i += 1
    else:
      raise Exception(f"Unknown argument: {args[i]}")

  if not infile or not outfile:
    show_help()
    sys.exit(1)

  return CliOptions(
    infile=infile,
    outfile=outfile,
  )

def main():
  options = parse_cli_args(sys.argv)

  src: str = ""
  if (options.infile == "-"):
    for line in sys.stdin:
      src += line
  else:
    with open(options.infile, "r") as f:
      src = f.read()

  result = assemble(src, options.infile)

  out_dirname = os.path.dirname(options.outfile)
  os.makedirs(out_dirname, exist_ok=True)

  with open(options.outfile, "wb") as f:
    f.write(result.program)

  pad_binary(options.outfile, 64 * 1024)

  print(f"Wrote 64 KB to {options.outfile} ({len(result.program)} B without padding)")

  dbg_outfile_path = options.outfile + ".dbg"

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
