constants: dict[str, str] = {
  # Registers
  "ra": "0",
  "rb": "1",
  "rc": "2",
  "rd": "3",
  "re": "4",
  "rf": "5",
  "rg": "6",
  "rh": "7",
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

Labels = dict[str, int]

def check_size(bits: int, val: int) -> None:
  if val >= 2 ** bits:
    raise Exception(f"Value does not fit in {bits} bits: {val}")

def eval_symbol(labels: Labels, c: str) -> str:
  if c in labels:
    return str(labels[c])

  if c in constants:
    return constants[c]

  return c

def eval_expr(labels: Labels, expr: str, bits: int = 16) -> int:
  expr = expr.lower()
  expr = eval_symbol(labels, expr)
  ret = eval(expr, labels.copy()) # eval as Python expr
  check_size(bits, ret)
  return ret
