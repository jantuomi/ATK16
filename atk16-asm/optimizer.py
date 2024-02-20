from tokenizer import tokenize

def format_asm_row(asm: str) -> str:
  if not (asm.startswith("@") or asm.startswith(";")) and not asm.startswith("  ") and len(asm) > 0:
    return "  " + asm
  else:
    return asm

class Optimizer:
  def __init__(self):
    pass

  def optimize(self, asm_str: str):
    asm = asm_str.split("\n")
    asm = [row.strip() for row in asm]
    asm = [self.strip_comment(row) for row in asm]
    asm = [row for row in asm if not len(row) == 0]
    asm = [tokenize(row, retain_curlies=True) for row in asm]
    asm = self.compact_spu_spo_pattern(asm)

    # TODO: not safe when setting loading SP and FP
    #   ldr RA SP
    #   mov RA FP
    # gets optimized to
    #   ldr RA FP
    #asm = self.compact_target_mov_pattern(asm)
    #asm = self.compact_target_mov_pattern(asm)

    # TODO: not safe at all. E.g. breaks a while True: pass loop
    #asm = self.compact_mov_source_pattern(asm)
    #asm = self.compact_mov_source_pattern(asm)

    asm = self.compact_spu_load_spo_pattern(asm)
    # asm = self.convert_alr_to_ali(asm)
    result = "\n".join([format_asm_row(" ".join(row)) for row in asm])

    return result

  def strip_comment(self, row: str):
    ret: str = ""
    for c in row:
      if c == ";": break
      ret += c

    return ret

  def compact_spu_spo_pattern(self, asm: list[list[str]]) -> list[list[str]]:
    i = 0
    result: list[list[str]] = []
    while i < len(asm):
      current = asm[i]
      next = asm[i + 1] if i + 1 < len(asm) else None
      i += 1
      if current[0].startswith("@") or next is None:
        result.append(current)
        continue

      if current[0] == "spu" and next[0] == "spo":
        print("=== compact_spu_spo_pattern")
        print(current)
        print(next)

        arg_current = current[1]
        arg_next = next[1]

        print("=== arg_current:", arg_current)
        print("=== arg_next:", arg_next)

        if arg_current == arg_next:
          print("=== pass")
          pass # remove both spu and spo
        else:
          print("=== mov")
          mov = ["mov", arg_current, arg_next]
          result.append(mov)
          print("=== mov:", mov)

        print("=== last of result:", result[len(result) - 1])
        print()
        i += 1
        continue

      result.append(current)

    return result

  def compact_target_mov_pattern(self, asm: list[list[str]]) -> list[list[str]]:
    ops_with_target = ["ldi", "ldr", "add", "sub", "addi", "subi", "and", "or", "xor", "sll", "slr", "sar", "slli", "slri", "sari", "inc", "dec", "mov", "ali", "alr", "lpc"]
    i = 0
    result: list[list[str]] = []
    while i < len(asm):
      current = asm[i]
      next = asm[i + 1] if i + 1 < len(asm) else None
      i += 1
      if current[0].startswith("@") or next is None:
        result.append(current)
        continue

      if current[0] in ops_with_target and next[0] == "mov":

        op_op, op_operands, op_target_reg = current[0], current[1:len(current) - 1], current[len(current) - 1]
        mov_from_reg, mov_to_reg = next[1], next[2]

        if op_target_reg == mov_from_reg:
          ret = [op_op, *op_operands, mov_to_reg]
          result.append(ret)
          i += 1
          continue

      result.append(current)

    return result

  def compact_mov_source_pattern(self, asm: list[list[str]]) -> list[list[str]]:
    ops_with_source = ["str", "ldr", "add", "sub", "addi", "subi", "and", "or", "xor", "sll", "slr", "sar", "slli", "slri", "sari", "mov", "ali", "alr"]
    i = 0
    result: list[list[str]] = []
    while i < len(asm):
      current = asm[i]
      next = asm[i + 1] if i + 1 < len(asm) else None
      i += 1
      if current[0].startswith("@") or next is None:
        result.append(current)
        continue

      if current[0] == "mov" and next[0] in ops_with_source:
        mov_from_reg, mov_to_reg = current[1], current[2]
        op_op, op_source_reg, op_operands = next[0], next[1], next[2:]

        if op_source_reg == mov_to_reg:
          ret = [op_op, op_source_reg, *op_operands]
          result.append(ret)
          i += 1
          continue

      result.append(current)

    return result

  def compact_spu_load_spo_pattern(self, asm: list[list[str]]) -> list[list[str]]:
    #  spu RA
    #  ldi int_7 RA
    #  ldr RA RB
    #  spo RA
    # OR
    #  spu RA
    #  ldi 3 RB
    #  spo RA

    i = 0
    result: list[list[str]] = []
    while i < len(asm):
      instr0 = asm[i]
      instr1 = asm[i + 1] if i + 1 < len(asm) else None
      instr2 = asm[i + 2] if i + 2 < len(asm) else None
      instr3 = asm[i + 3] if i + 3 < len(asm) else None
      i += 1
      if instr0[0].startswith("@") or instr1 is None or instr2 is None or instr3 is None:
        result.append(instr0)
        continue

      if instr0[0] == "spu" and instr1[0] == "ldi" and instr2[0] == "ldr" and instr3[0] == "spo":
        spu_op, spu_reg = instr0
        ldi_op, ldi_imm, ldi_target_reg = instr1
        ldr_op, ldr_from_reg, ldr_to_reg = instr2
        spo_op, spo_reg = instr3

        if spu_reg == spo_reg and ldi_target_reg == ldr_from_reg and ldr_from_reg != ldr_to_reg:
          ret0 = f"ldi {ldi_imm} {ldr_to_reg}".split()
          ret1 = f"ldr {ldr_to_reg} {ldr_to_reg}".split()
          result.append(ret0)
          result.append(ret1)
          i += 3
          continue

      elif instr0[0] == "spu" and instr1[0] == "ldi" and instr2[0] == "spo":
        spu_op, spu_reg = instr0
        ldi_op, ldi_imm, ldi_target_reg = instr1
        spo_op, spo_reg = instr2

        if spu_reg == spo_reg and ldi_target_reg != spu_reg:
          ret0 = f"ldi {ldi_imm} {ldi_target_reg}".split()
          result.append(ret0)
          i += 2
          continue

      result.append(instr0)

    return result
