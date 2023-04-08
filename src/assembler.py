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
  result = []
  acc = ""
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

def eval_symbol(c: str):
  if c in labels:
    return str(labels[c])

  match c:
    # Registers
    case "rz": return "0"
    case "ra": return "1"
    case "rb": return "2"
    case "rc": return "3"
    case "rd": return "4"
    case "pa": return "5"
    case "pb": return "6"
    # ALU instructions
    case "al_clear": return "0"
    case "al_b_minus_a": return "1"
    case "al_a_minus_b": return "2"
    case "al_a_plus_b": return "3"
    case "al_a_xor_b": return "4"
    case "al_a_or_b": return "5"
    case "al_a_and_b": return "6"
    case "al_preset": return "7"
    case "al_logical_shift_right": return "8"
    case "al_arithmetic_shift_right": return "9"
    case "al_logical_shift_left": return "10"
    # ALU flags
    case "f_carry": return "0"
    case "f_overflow": return "1"
    case "f_zero": return "2"
    case "f_sign": return "3"
    case _: return c

# 1st pass, gather labels

labels: dict[str, int] = {}
address = 0

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

  address += 1

# 2nd pass

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
    case "@label":
      continue

    # Instructions
    case "alu":
      # S (alu op)
      # L (lhs)
      # R (rhs)
      # T (target)
      word = (0b0000 << 12) + \
             (eval_expr(args[0]) << 3) + \
             (eval_expr(args[1]) << 9) + \
             (eval_expr(args[2]) << 6) + \
             (eval_expr(args[3]) << 0)
    case "als":
      # S (alu op)
      # L (lhs)
      # R (rhs)
      # T (target)
      word = (0b0001 << 12) + \
             (eval_expr(args[0]) << 3) + \
             (eval_expr(args[1]) << 9) + \
             (eval_expr(args[2]) << 6) + \
             (eval_expr(args[3]) << 0)
    case "ldr":
      # R (address)
      # T (target)
      word = (0b0010 << 12) + \
             (eval_expr(args[0]) << 9) + \
             (eval_expr(args[1]) << 0)
    case "str":
      # R (address)
      # T (value to store)
      word = (0b0011 << 12) + \
             (eval_expr(args[0]) << 9) + \
             (eval_expr(args[1]) << 6)
    case "ldi":
      # T (target)
      # I (immediate)
      word = (0b0100 << 12) + \
             (eval_expr(args[0]) << 0) + \
             (eval_expr(args[1]) << 3)
    case "jmp":
      # R (reg holding address)
      word = (0b0101 << 12) + \
             (eval_expr(args[0]) << 9)
    case "br":
      # F (flag selector)
      # R (reg holding address)
      word = (0b0110 << 12) + \
             (eval_expr(args[0]) << 9) + \
             (eval_expr(args[1]) << 6)
    case "hlt":
      word = 0b1111 << 12

    # Pseudoinstructions
    case "add":
      word = (0b0000                   << 12) + \
             (eval_expr(args[0])       << 9) + \
             (eval_expr(args[1])       << 6) + \
             (eval_expr("al_a_plus_b") << 3) + \
             (eval_expr(args[2])       << 0)
    case "sub":
      word = (0b0000                    << 12) + \
             (eval_expr(args[0])        << 9) + \
             (eval_expr(args[1])        << 6) + \
             (eval_expr("al_a_minus_b") << 3) + \
             (eval_expr(args[2])        << 0)
    case "mov":
      word = (0b0000                   << 12) + \
             (eval_expr("rz")          << 9) + \
             (eval_expr(args[0])       << 6) + \
             (eval_expr("al_a_plus_b") << 3) + \
             (eval_expr(args[1])       << 0)

    # Default case: evaluate as is (e.g. data word)
    case _:
      try:
        word = eval_expr(keyword)
      except:
        raise Exception(f"Invalid assembly at {infile_path}:{lineNo + 1}\n\n{line}")

  if len(result) < 2 * address + 1:
    result.extend((2 * address + 1 - len(result)) * nop)

  for (label, label_addr) in labels.items():
    if address == label_addr:
      print(f"{label}:")

  print(f"{address:>08x}  0x{word:>04x}  {line}")
  result[2 * address + 0] = ((word >> 8) & 0xff)
  result[2 * address + 1] = ((word >> 0) & 0xff)
  address += 1

with open(outfile_path, "wb") as f:
  f.write(result)

print(f"Wrote {len(result)} bytes to {outfile_path}")

with open(f"{outfile_path}.logisim.txt", "w") as f:
  f.write("v2.0 raw\n")
  run_length = 0
  run_last = ""
  for i in range(len(result) // 2):
    b0 = result[2 * i + 0]
    b1 = result[2 * i + 1]
    word = f"{b0:>02x}{b1:>02x}\n"

    if word != run_last and run_length <= 1:
      f.write(f"{run_last}")
      run_length = 1
      run_last = word
    elif word != run_last and run_length > 1:
      f.write(f"{run_length}*{run_last}")
      run_length = 1
      run_last = word
    else:
      run_length += 1

  if run_length <= 1:
    f.write(f"{run_last}")
  else:
    f.write(f"{run_length}*{run_last}")

print(f"Wrote Logisim image format to {outfile_path}.logisim.txt")

with open(f"{outfile_path}.ver.txt", "w") as f:
  f.write("addr/data: 15 16")
  run_length = 0
  run_last = ""
  written = -1

  def update_layout():
    if run_last == "": return
    if written % 8 == 0:
      f.write("\n")
    else:
      f.write(" ")

  for i in range(len(result) // 2):
    b0 = result[2 * i + 0]
    b1 = result[2 * i + 1]
    word = f"{b0:>02x}{b1:>02x}"

    if word != run_last and run_length <= 1:
      f.write(f"{run_last}")
      run_length = 1
      run_last = word
      written += 1
      update_layout()
    elif word != run_last and run_length > 1:
      f.write(f"{run_length}*{run_last}")
      run_length = 1
      run_last = word
      written += 1
      update_layout()
    else:
      run_length += 1

  if run_length <= 1:
    f.write(f"{run_last}")
  else:
    f.write(f"{run_length}*{run_last}")

print(f"Wrote verification test format to {outfile_path}.ver.txt")
