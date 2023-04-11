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
  labels: dict[str, int]

def pass_3(result2: Result2) -> Result3:
  result_lines: list[Result3Line] = []
  labels: dict[str, int] = {}
  address = 0

  for line in result2.lines:
    keyword, *args = line.parts
    match keyword:
      case "@address":
        address = eval(args[0])
        continue
      case "@label":
        labels[args[0]] = address
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

  return Result3(
    operations=result2.operations,
    options=result2.options,
    lines=result_lines,
    labels=labels,
  )
