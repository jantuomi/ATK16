import importlib
import sys
import os.path
from dataclasses import dataclass
from asm_ops import *
from asm_eval import *
from asm_pass0 import *
from tokenizer import tokenize

@dataclass
class Result1Line:
  line_num: int
  src_file: str
  parts: list[str]

@dataclass
class Result1:
  lines: list[Result1Line]
  options: Options
  operations: OpExpansionDict

def pass_1(result0: Result0) -> Result1:
  options = Options()
  result_lines: list[Result1Line] = []
  operations: OpExpansionDict = default_expansions.copy()

  for line in result0.lines:
    keyword, *args = tokenize(line.line)
    match keyword:
      case "@opt":
        opt_name, opt_value = args
        match opt_name:
          case "stack_pointer": options.stack_pointer = opt_value
          case "csr_scratch": options.csr_scratch = opt_value
          case _: raise Exception("Unknown @opt: " + opt_name)
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
      case _:
        result_lines.append(Result1Line(
          src_file=line.src_file,
          line_num=line.line_num,
          parts=[keyword, *args]
        ))

  return Result1(
    operations=operations,
    options=options,
    lines=result_lines
  )
