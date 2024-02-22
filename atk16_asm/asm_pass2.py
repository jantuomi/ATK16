from dataclasses import dataclass
from .asm_ops import *
from .asm_eval import *
from .asm_pass1 import *

@dataclass
class Result2Line:
  line_num: int
  src_file: str
  parts: list[str]
  original_parts: list[str]

@dataclass
class Result2:
  lines: list[Result2Line]
  operations: OpExpansionDict

def pass_2(result1: Result1) -> Result2:
  result_lines: list[Result2Line] = []

  for line in result1.lines:
    keyword, *args = line.parts
    if keyword in result1.operations:
      fn = result1.operations[keyword]
      output: list[list[str]] = fn(*args)
    else:
      output = [line.parts]

    for (idx, parts) in enumerate(output):
      result_lines.append(Result2Line(
        line_num=line.line_num,
        src_file=line.src_file,
        parts=parts,
        original_parts=line.parts if idx == 0 else ["..."]
      ))

  return Result2(
    operations=result1.operations,
    lines=result_lines
  )
