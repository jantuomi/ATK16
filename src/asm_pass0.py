from dataclasses import dataclass
import os.path
from asm_ops import *
from asm_eval import *

@dataclass
class Result0Line:
  line_num: int
  src_file: str
  line: str

@dataclass
class Result0:
  lines: list[Result0Line]
  options: Options
  operations: OpExpansionDict

def pass_0(lines: list[str], file_name: str) -> Result0:
  options = Options()
  result_lines: list[Result0Line] = []
  operations: OpExpansionDict = default_expansions.copy()

  for (line_num, line) in enumerate(lines):
    line = line.split(";")[0].strip()
    if line == "": continue
    keyword, *args = line.lower().split()
    match keyword:
      case "@include":
        asm_file_name = args[0]
        path = os.path.join(os.path.dirname(file_name), asm_file_name + ".atk16")
        with open(path, "r") as f:
          for incl_line in f.readlines():
            result_lines.append(Result0Line(
              src_file=asm_file_name,
              line_num=line_num,
              line=incl_line
            ))

      case _:
        result_lines.append(Result0Line(
          src_file=file_name,
          line_num=line_num,
          line=line
        ))

  return Result0(
    operations=operations,
    options=options,
    lines=result_lines
  )
