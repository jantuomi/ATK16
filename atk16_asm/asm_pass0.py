from dataclasses import dataclass
import os.path
from .asm_ops import *
from .asm_eval import *
from .tokenizer import *

@dataclass
class Result0Line:
  line_num: int
  src_file: str
  line: str

@dataclass
class Result0:
  lines: list[Result0Line]

def pass_0(lines: list[str], file_name: str) -> Result0:
  file_name = file_name if file_name.endswith(".atk16") else file_name + ".atk16"
  result_lines: list[Result0Line] = []

  for (line_num, line) in enumerate(lines):
    line_num += 1 # line numbers are 1-based

    line = line.split(";")[0].strip()
    if line == "": continue

    keyword, *args = tokenize(line)

    match keyword:
      case "@include":
        asm_file_name = args[0] if args[0].endswith(".atk16") else args[0] + ".atk16"
        path = os.path.join(os.path.dirname(file_name), asm_file_name)
        with open(path, "r") as f:
          incl_lines = f.readlines()

        incl_result0 = pass_0(incl_lines, asm_file_name)
        for incl_line in incl_result0.lines:
          result_lines.append(Result0Line(
            src_file=incl_line.src_file,
            line_num=incl_line.line_num,
            line=incl_line.line
          ))

      case _:
        result_lines.append(Result0Line(
          src_file=file_name,
          line_num=line_num,
          line=line
        ))

  return Result0(
    lines=result_lines
  )
