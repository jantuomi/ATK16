from dataclasses import dataclass
from asm_ops import *
from asm_eval import *
from asm_pass2 import *

@dataclass
class Result3Line:
  line_num: int
  src_file: str
  address: int
  parts: list[str]
  original_parts: list[str]

@dataclass
class Result3:
  lines: list[Result3Line]
  options: Options
  operations: OpExpansionDict
  symbols: dict[str, int]

def pass_3(result2: Result2) -> Result3:
  result_lines: list[Result3Line] = []
  symbols: dict[str, int] = {}
  address = 0

  for line in result2.lines:
    keyword, *args = line.parts
    match keyword:
      case "@address":
        address = eval_expr(symbols, args[0])
        continue
      case "@label":
        symbols[args[0]] = address
        continue
      case "@let":
        symbols[args[0]] = eval_expr(symbols, args[1])
        continue
      case _:
        result_lines.append(Result3Line(
          line_num=line.line_num,
          src_file=line.src_file,
          parts=line.parts,
          address=address,
          original_parts=line.original_parts,
        ))
        address += 1

  result_lines.sort(key=lambda l: l.address)
  for result_line in result_lines:
    rows_with_same_addr = list(filter(lambda l: l.address == result_line.address, result_lines))
    n = len(rows_with_same_addr)
    if n > 1:
      formatted = "\n".join(map(lambda l: " ".join(l.parts), rows_with_same_addr))
      raise Exception(f"Overlapping segments: address 0x{result_line.address:>04x} has conflicting definitions:\n{formatted}")
  return Result3(
    operations=result2.operations,
    options=result2.options,
    lines=result_lines,
    symbols=symbols,
  )
