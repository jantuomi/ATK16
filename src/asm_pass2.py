from dataclasses import dataclass
from assembler_ops import *
from assembler_eval import *
from asm_pass1 import *

@dataclass
class Result2Line:
  line_num: int
  parts: list[str]
  original_parts: list[str]

@dataclass
class Result2:
  lines: list[Result2Line]
  options: Options
  operations: OpExpansionDict

def pass_2(result1: Result1) -> Result2:
  options = Options()
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
        parts=parts,
        original_parts=line.parts if idx == 0 else ["..."]
      ))

  return Result2(
    operations=result1.operations,
    options=options,
    lines=result_lines
  )
