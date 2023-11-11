from dataclasses import dataclass
from asm_ops import *
from asm_eval import *
from asm_pass3 import *

@dataclass
class Result4Line:
  line_num: int
  src_file: str
  address: int
  word: int
  text: str
  original_text: str

@dataclass
class Result4:
  lines: list[Result4Line]
  options: Options
  operations: OpExpansionDict
  symbols: dict[str, int]

def translate_opt(arg: str, options: Options) -> str:
  match arg:
    case "SP": return options.stack_pointer
    case "CSR_SCRATCH": return options.csr_scratch
    case _: return arg

def pass_4(result3: Result3) -> Result4:
  result_lines: list[Result4Line] = []

  for line in result3.lines:
    keyword, *args = line.parts
    meta = Meta(
      address=line.address,
    )

    args = list(map(lambda a: translate_opt(a, result3.options), args))
    text = " ".join([keyword, *args])
    original_text = " ".join(line.original_parts)

    if keyword in operations:
      fn = operations[keyword]
      word = fn(meta, result3.symbols, *args)
      result_lines.append(Result4Line(
        line_num=line.line_num,
        src_file=line.src_file,
        address=line.address,
        word=word,
        text=text,
        original_text=original_text,
      ))
    else:
      try:
        result_lines.append(Result4Line(
          line_num=line.line_num,
          src_file=line.src_file,
          address=line.address,
          word=eval_expr(result3.symbols, keyword),
          text=text,
          original_text=original_text,
        ))
      except:
        raise Exception(f"Invalid assembly at {line.src_file}:{line.line_num + 1}\n\n{line}")

  return Result4(
    operations=result3.operations,
    symbols=result3.symbols,
    options=result3.options,
    lines=result_lines,
  )
