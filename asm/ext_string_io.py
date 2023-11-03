from asm_ops import *
from ext_std import expand_addi

def to_char_code(c: str):
  return ord(c)

_counter = 0
def get_unique_name(prefix: str):
  global _counter
  ret =  f"{prefix}_{_counter}"
  _counter += 1
  return ret

def expand_put_char(cursor_reg: str, scratch_reg: str, c: str) -> ExpandResult:
  c = c.strip("\"")
  return [
    ["ldi", str(to_char_code(c)), scratch_reg],
    ["str", scratch_reg, cursor_reg],
    *expand_addi(cursor_reg, "1", cursor_reg),
  ]

def expand_put_string(cursor_reg: str, scratch_reg: str, *rest: str) -> ExpandResult:
  string = " ".join(rest)
  string = string.strip("\"")
  result: ExpandResult = []
  for c in string:
    result += expand_put_char(cursor_reg, scratch_reg, c)

  return result

def expand_cursor_to_line(cursor_reg: str, line: int) -> ExpandResult:
  lbl_data = get_unique_name("cursor_to_line_data")
  lbl_jmpover = get_unique_name("cursor_to_line_jmpover")
  line_width = 64
  return [
    ["jpi", lbl_jmpover],
    ["@label", lbl_data],
    [f"text_mem + {line} * {line_width}"],
    ["@label", lbl_jmpover],
    ["ldi", lbl_data, cursor_reg],
    ["ldr", cursor_reg, cursor_reg],
  ]

expansions: OpExpansionDict = {
  "put_char": expand_put_char,
  "cursor_to_line": expand_cursor_to_line,
  "put_string": expand_put_string,
}