from dataclasses import dataclass
from assembler_ops import *
from assembler_eval import *
from asm_pass3 import *

@dataclass
class Result4Line:
  line_num: int
  address: int
  word: int
  text: str

@dataclass
class Result4:
  lines: list[Result4Line]
  options: Options
  operations: OpExpansionDict
  labels: dict[str, int]

def translate_opt(arg: str, options: Options) -> str:
  match arg:
    case "__STACK_POINTER": return options.stack_pointer
    case "__CSR_SCRATCH": return options.csr_scratch
    case _: return arg

def pass_4(result3: Result3) -> Result4:
  result_lines: list[Result4Line] = []
  address = 0

  for line in result3.lines:
    keyword, *args = line.parts
    meta = Meta(
      address=address,
    )

    args = list(map(lambda a: translate_opt(a, result3.options), args))
    text = " ".join([keyword, *args])

    if keyword in operations:
      fn = operations[keyword]
      word = fn(meta, result3.labels, *args)
      result_lines.append(Result4Line(
        line_num=line.line_num,
        address=line.address,
        word=word,
        text=text,
      ))
    else:
      try:
        result_lines.append(Result4Line(
          line_num=line.line_num,
          address=line.address,
          word=eval_expr(result3.labels, keyword),
          text=text,
        ))
      except:
        raise Exception(f"Invalid assembly at {line.line_num + 1}\n\n{line}")

  return Result4(
    operations=result3.operations,
    labels=result3.labels,
    options=result3.options,
    lines=result_lines,
  )
