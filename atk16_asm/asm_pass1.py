import importlib
import sys
import os.path
from dataclasses import dataclass
from .asm_ops import *
from .asm_eval import *
from .asm_pass0 import *
from .tokenizer import tokenize

@dataclass
class Result1Line:
  line_num: int
  src_file: str
  parts: list[str]

@dataclass
class Result1:
  lines: list[Result1Line]
  operations: OpExpansionDict

def pass_1(result0: Result0) -> Result1:
  result_lines: list[Result1Line] = []
  operations: OpExpansionDict = expansions.copy()

  for line in result0.lines:
    keyword, *args = tokenize(line.line)
    match keyword:
      case "@use":
        module_name, ops = args[0].split(":")
        ops_split = ops.split(",")
        sys.path.append(os.path.dirname(line.src_file))
        module = importlib.import_module(module_name)
        mod_expansions: OpExpansionDict = module.expansions
        for op in mod_expansions:
          expansion = mod_expansions[op]
          if ops == "*" or op in ops_split:
            operations[op] = expansion
      case "%%data_segment":
        # drop the data_segment sentinel
        continue
      case _:
        result_lines.append(Result1Line(
          src_file=line.src_file,
          line_num=line.line_num,
          parts=[keyword, *args]
        ))

  return Result1(
    operations=operations,
    lines=result_lines
  )
