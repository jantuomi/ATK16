#!/usr/bin/env python3
# Assemble ATK16 assembly to bytecode

import sys

if len(sys.argv) != 3:
  print("usage: assembler.py <infile> <outfile> # read from file")
  print("       assembler.py - <outfile>        # read from stdin")
  sys.exit(1)

infile_path = sys.argv[1]
outfile_path = sys.argv[2]

src = ""
if (infile_path == "-"):
  for line in sys.stdin:
    src += line
else:
  with open(infile_path, "r") as f:
    src = f.read()

src_lines = src.splitlines()

### Utils

def parse(line: str) -> list[str]:
  depth = 0
  result: list[str] = []
  acc: str = ""
  for c in line:
    if c.isspace() and depth == 0:
      result.append(acc)
      acc = ""
    elif c == "(":
      depth += 1
      acc += "("
    elif c == ")":
      depth -= 1
      acc += ")"
    else:
      acc += c

  result.append(acc)
  return list(filter(lambda x: len(x) > 0, result))

def eval_expr(expr: str) -> int:
  expr = eval_symbol(expr)
  return eval(expr, labels.copy()) # eval as Python expr

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

def eval_symbol(c: str):
  if c in labels:
    return str(labels[c])

  if c in constants:
    return constants[c]

  return c

# 1st pass, gather labels, set options

options: dict[str, str] = {
  "stack_pointer": "6" # RG
}
labels: dict[str, int] = {}
address: int = 0

for (lineNo, line) in enumerate(src_lines):
  line = line.split(";")[0].strip()
  if line == "": continue
  keyword, *args = line.lower().split()
  match keyword:
    case "@address":
      address = eval(args[0])
      continue
    case "@label":
      labels[args[0]] = address
      continue
    case "@opt":
      opt_name = args[0]
      opt_value = constants.get(args[1], args[1])
      options[opt_name] = opt_value
    case _:
      pass

  address += 1

# 2nd pass

def make_alr(alu_op: str, left: str, right: str, target: str) -> list[int]:
  """ALR 0000 TTTL LLRR RSSS"""
  target_e = eval_expr(target)
  left_e = eval_expr(left)
  right_e = eval_expr(right)
  alu_op_e = eval_expr(alu_op)
  word = (0b0000 << 12) + \
              (target_e << 9) + \
              (left_e << 6) + \
              (right_e << 3) + \
              alu_op_e
  return [word]

def make_ali(alu_op: str, left: str, imm: str, target: str) -> list[int]:
  """ALI 0001 TTTL LLII ISSS"""
  target_e = eval_expr(target)
  left_e = eval_expr(left)
  imm_e = eval_expr(imm)
  alu_op_e = eval_expr(alu_op)
  word = (0b0001 << 12) + \
              (target_e << 9) + \
              (left_e << 6) + \
              (imm_e << 3) + \
              alu_op_e
  return [word]

def make_ldr(to_reg: str, addr_reg: str) -> list[int]:
  """LDR 0010 TTTR RRXX XXXX"""
  to_reg_e = eval_expr(to_reg)
  addr_reg_e = eval_expr(addr_reg)
  word = (0b0010 << 12) + \
              (to_reg_e << 9) + \
              (addr_reg_e << 6)
  return [word]

def make_str(from_reg: str, addr_reg: str) -> list[int]:
  """STR 0011 TTTR RRXX XXXX"""
  from_reg_e = eval_expr(from_reg)
  addr_reg_e = eval_expr(addr_reg)
  word = (0b0011 << 12) + \
              (from_reg_e << 9) + \
              (addr_reg_e << 6)
  return [word]

def make_ldi(to_reg: str, imm: str) -> list[int]:
  """LDI 0100 TTTI IIII IIII"""
  to_reg_e = eval_expr(to_reg)
  imm_e = eval_expr(imm)
  word = (0b0100 << 12) + \
              (to_reg_e << 9) + \
              imm_e
  return [word]

def make_jpr(addr_reg: str) -> list[int]:
  """JPR 0101 XXXR RRXX XXXX"""
  addr_reg_e = eval_expr(addr_reg)
  word = (0b0101 << 12) + \
              (addr_reg_e << 6)
  return [word]

def make_jpi(address: int, imm: str) -> list[int]:
  """JPI 0110 XXXI IIII IIII"""
  imm_e = eval_expr(imm)
  imm_e -= address
  word = (0b0110 << 12) + \
              imm_e
  return [word]

def make_br(flag_s: str, addr_reg: str) -> list[int]:
  """BR  0111 XFFR RRXX XXXX"""
  flag_s_e = eval_expr(flag_s)
  addr_reg_e = eval_expr(addr_reg)
  word = (0b0111 << 12) + \
              (flag_s_e << 9) + \
              (addr_reg_e << 6)
  return [word]

def make_bri(address: int, flag_s: str, imm: str) -> list[int]:
  """BRI 1000 XFFI IIII IIII"""
  flag_s_e = eval_expr(flag_s)
  imm_e = eval_expr(imm)
  imm_e -= address
  word = (0b1000 << 12) + \
              (flag_s_e << 9) + \
              imm_e
  return [word]

def make_hlt() -> list[int]:
  """HLT 1111 XXXX XXXX XXXX"""
  word = (0b1111 << 12)
  return [word]

# def make_spu(reg: str) -> list[int]:
#   stack_pointer = options["stack_pointer"]
#   reg = eval_symbol(reg)
#   words_inc: list[int] = make_inc(stack_pointer, stack_pointer)
#   words_str: list[int] = make_str(reg, stack_pointer)
#   return words_inc + words_str

address = 0
nop = bytearray([0b1000_0000, 0])
result = bytearray()
# initially one nop
result.extend(nop)

for (lineNo, line) in enumerate(src_lines):
  line = line.split(";")[0].strip()
  if line == "": continue
  keyword, *args = parse(line.lower())
  match keyword:
    # Directives
    case "@address":
      address = eval_expr(args[0])
      continue
    case "@label" | "@opt":
      continue

    # Instructions
    case "alr":
      words = make_alr(*args)
    case "ali":
      words = make_ali(*args)
    case "ldr":
      words = make_ldr(*args)
    case "str":
      words = make_str(*args)
    case "ldi":
      words = make_ldi(*args)
    case "jpr":
      words = make_jpr(*args)
    case "jpi":
      words = make_jpi(address, *args)
    case "br":
      words = make_br(*args)
    case "bri":
      words = make_bri(address, *args)
    case "hlt":
      words = make_hlt()

    # Pseudoinstructions
    case "add":
      words = make_alr("al_plus", *args)
    case "sub":
      words = make_alr("al_minus", *args)
    case "inc":
      words = make_ali("al_plus", args[0], "1", args[0])
    case "dec":
      words = make_ali("al_minus", args[0], "1", args[0])
    case "mov":
      words = make_ali("al_plus", args[0], "0", args[1])
    # case "csr":
    #   reg = args[0]
    #   words_spu = make_spu(reg)
    #   words = []
    #   # todo finish this

    # Default case: evaluate as is (e.g. data word)
    case _:
      try:
        words = [eval_expr(keyword)]
      except:
        raise Exception(f"Invalid assembly at {infile_path}:{lineNo + 1}\n\n{line}")

  if len(result) < 2 * address + 1:
    result.extend((2 * address + 1 - len(result)) * nop)

  for (label, label_addr) in labels.items():
    if address == label_addr:
      print(f"{label}:")

  for word in words:
    print(f"{address:>08x}  0x{word:>04x}  {line}")
    result[2 * address + 0] = ((word >> 8) & 0xff)
    result[2 * address + 1] = ((word >> 0) & 0xff)
    address += 1

with open(outfile_path, "wb") as f:
  f.write(result)

print(f"Wrote {len(result)} bytes to {outfile_path}")
