
constants: dict[str, str] = {
  # Registers
  "RA": "0",
  "RB": "1",
  "RC": "2",
  "RD": "3",
  "RE": "4",
  "RF": "5",
  "RG": "6",
  "RH": "7",
  # ALU instructions
  "al_plus": "0",
  "al_minus": "1",
  "al_and": "2",
  "al_or": "3",
  "al_xor": "4",
  "al_slr": "5",
  "al_sar": "6",
  "al_sll": "7",
  # ALU flags
  "carry": "0",
  "overflow": "1",
  "zero": "2",
  "sign": "3",
}

Symbols = dict[str, int]

def check_size(bits: int, val: int) -> None:
  if val >= 2 ** bits:
    raise Exception(f"Value does not fit in {bits} bits: 0x{val:>04x}")

def eval_symbol(symbols: Symbols, c: str) -> str:
  if c in symbols:
    return str(symbols[c])

  if c in constants:
    return constants[c]

  return c

def eval_expr(symbols: Symbols, expr: str, bits: int = 16) -> int:
  expr = eval_symbol(symbols, expr)
  ret = eval(expr, symbols.copy()) # eval as Python expr
  check_size(bits, ret)
  return ret
