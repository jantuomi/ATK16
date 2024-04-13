from dataclasses import dataclass
from .asm_ops import *
from .asm_eval import *
from .asm_pass2 import *

@dataclass
class Result3Line:
  line_num: int
  src_file: str
  address: int
  parts: list[str]
  original_parts: list[str]

@dataclass
class DbgSourceInfo:
  src_file: str
  line_num: int
  expanded_text: str
  original_text: str

DbgLabelTable = dict[int, list[str]]
DbgSourceTable = dict[int, DbgSourceInfo]

@dataclass
class Result3:
  lines: list[Result3Line]
  operations: OpExpansionDict
  symbols: dict[str, int]
  dbg_label_table: DbgLabelTable
  dbg_source_table: DbgSourceTable

def pass_3(result2: Result2) -> Result3:
  result_lines: list[Result3Line] = []
  symbols: dict[str, int] = {}
  address = 0
  dbg_label_table: DbgLabelTable = {}
  dbg_source_table: DbgSourceTable = {}

  def save_dbg_label(addr: int, label: str):
    if addr in dbg_label_table:
      dbg_label_table[addr].append(label)
    else:
      dbg_label_table[addr] = [label]

  def save_dbg_source(addr: int, src_file: str, line_num: int, expanded_text: str, original_text: str):
    dbg_source_table[addr] = DbgSourceInfo(
      src_file=src_file,
      line_num=line_num,
      expanded_text=expanded_text,
      original_text=original_text,
    )

  # On address conflict, if override_mode is True, the last definition wins
  # otherwise, an exception is raised
  override_mode = False

  for line in result2.lines:
    keyword, *args = line.parts
    match keyword:
      case "@address":
        address = eval_expr(symbols, args[0])
        continue
      case "@begin_override":
        override_mode = True
        continue
      case "@end_override":
        override_mode = False
        continue
      case "@label":
        label = args[0]
        if label in symbols:
          raise Exception(f"When defining label {label} as {address:>04x}, symbol {label} already defined as {symbols[label]:>04x}")
        symbols[args[0]] = address
        save_dbg_label(address, label)
        continue
      case "@let":
        symbols[args[0]] = eval_expr(symbols, args[1])
        continue
      case _:
        if override_mode:
          # TODO this is a bit inefficient, but works
          result_lines = list(filter(lambda l: l.address != address, result_lines))

        dbg_original_text = " ".join(line.original_parts)
        dbg_expanded_text = " ".join(line.parts)
        save_dbg_source(address, line.src_file, line.line_num, dbg_expanded_text, dbg_original_text)
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
      formatted = format_overlapping_rows(rows_with_same_addr)
      raise Exception(f"Overlapping segments: address 0x{result_line.address:>04x} has conflicting definitions:\n{formatted}")

  return Result3(
    operations=result2.operations,
    lines=result_lines,
    symbols=symbols,
    dbg_label_table=dbg_label_table,
    dbg_source_table=dbg_source_table,
  )

def format_overlapping_rows(rows: list[Result3Line]) -> str:
  longest_val = 0
  for row in rows:
    joined = " ".join(row.parts)
    if len(joined) > longest_val:
      longest_val = len(joined)

  first_col_width = longest_val + 4

  results: list[str] = []
  for row in rows:
    joined = " ".join(row.parts)
    result = joined + (first_col_width - len(joined)) * " " + f" ({row.src_file}:{row.line_num})"
    results.append(result)

  return "\n".join(results)
