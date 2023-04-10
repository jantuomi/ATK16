constants: dict[str, str] = {
  # Registers
  "rz": "0",
  "ra": "1",
  "rb": "2",
  "rc": "3",
  "rd": "4",
  "rf": "5",
  "rg": "6",
  # ALU instructions
  "al_plus": "0",
  "al_minus": "1",
  "al_and": "2",
  "al_or": "3",
  "al_xor": "4",
  "al_shift_lr": "5",
  "al_shift_ar": "6",
  "al_shift_ll": "7",
  # ALU flags
  "f_carry": "0",
  "f_overflow": "1",
  "f_zero": "2",
  "f_sign": "3",
}

labels: dict[str, int] = {}

def eval_symbol(c: str) -> str:
  if c in labels:
    return str(labels[c])

  if c in constants:
    return constants[c]

  return c

def eval_expr(expr: str) -> int:
  expr = eval_symbol(expr)
  return eval(expr, labels.copy()) # eval as Python expr
