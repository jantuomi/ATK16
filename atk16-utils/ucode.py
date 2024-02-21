#!/usr/bin/env python3
# Generate .bin file with ATK16 CPU microcode

# Addressed by OOOO BUUU
# where O = opcode, B = branch flag, U = microsequencer value

import sys

if len(sys.argv) != 2:
  print("usage: ucode.py <outfile.bin>")
  sys.exit(1)

outfile_path = sys.argv[1]

PC_CO   = 1 << 0
PC_IE   = 1 << 1
PC_OE   = 1 << 2
MAR_IE  = 1 << 3
MEM_IE  = 1 << 4
MEM_OE  = 1 << 5
RW_IE   = 1 << 6
R1_OE   = 1 << 7
R2_OE   = 1 << 8
IR_IE   = 1 << 9
IM_M    = 1 << 10
LI_OE   = 1 << 11
ALU_OE  = 1 << 12
FR_IE   = 1 << 13
HALT    = 1 << 14
US_RS   = 1 << 15
ISRA_OE = 1 << 16
IM_DS   = 1 << 17
IM_EN   = 1 << 18
IPC_IE  = 1 << 19
IPC_OE  = 1 << 20
NOP5    = 1 << 21
NOP6    = 1 << 22
NOP7    = 1 << 23

BRANCH_FLAG_STATES_N = 2
UCODE_N: int = 2**3
CONTROL_WORD_SIZE = 3

def not_branch(bs: list[int]) -> list[list[int]]:
  return BRANCH_FLAG_STATES_N * [bs]

def branch(false_branch: list[int], true_branch: list[int]) -> list[list[int]]:
  return [false_branch, true_branch]

fetch = [PC_OE|MAR_IE, MEM_OE|IR_IE|PC_CO]

def nop():
  return not_branch([*fetch, US_RS, 0, 0, 0, 0, 0])

ucode = [
  # ALR 0000 TTTL LLRR RSSS
  not_branch([*fetch, ALU_OE|FR_IE|RW_IE, US_RS, 0, 0, 0, 0]),
  # ALI 0001 TTTL LLII ISSS
  not_branch([*fetch, IM_M|ALU_OE|FR_IE|RW_IE, US_RS, 0, 0, 0, 0]),
  # LDR 0010 TTTR RRXX XXXX
  not_branch([*fetch, R1_OE|MAR_IE, MEM_OE|RW_IE, US_RS, 0, 0, 0]),
  # STR 0011 XXXL LLRR RXXX
  not_branch([*fetch, R1_OE|MAR_IE, R2_OE|MEM_IE, US_RS, 0, 0, 0]),
  # LDI 0100 TTTI IIII IIII
  not_branch([*fetch, LI_OE|RW_IE, US_RS, 0, 0, 0, 0]),
  # JPR 0101 XXXR RRXX XXXX
  not_branch([*fetch, R1_OE|PC_IE, US_RS, 0, 0, 0, 0]),
  # JPI 0110 XXXI IIII IIII
  not_branch([*fetch, IM_M|LI_OE|PC_IE, US_RS, 0, 0, 0, 0]),
  # BRR  0111 XFFR RRXX XXXX
  branch([*fetch, US_RS, 0, 0, 0, 0, 0],
         [*fetch, R1_OE|PC_IE, US_RS, 0, 0, 0, 0]),
  # BRI 1000 XFFI IIII IIII
  branch([*fetch, US_RS, 0, 0, 0, 0, 0],
         [*fetch, IM_M|LI_OE|PC_IE, US_RS, 0, 0, 0, 0]),
  # LPC 1001 TTTX XXXX XXXX
  not_branch([*fetch, PC_OE|RW_IE, US_RS, 0, 0, 0, 0]),
  # NOP 1010 XXXX XXXX XXXX
  nop(),
  # NOP 1011 XXXX XXXX XXXX
  nop(),
  # ISRP0 1100 XXXX XXXX XXXX
  not_branch([IM_EN|PC_OE|IPC_IE, ISRA_OE|MAR_IE, MEM_OE|PC_IE, US_RS, 0, 0, 0, 0]),
  # ISRP1 1101 XXXX XXXX XXXX
  not_branch([IM_EN|PC_OE|IPC_IE, ISRA_OE|MAR_IE, MEM_OE|PC_IE, US_RS, 0, 0, 0, 0]),
  # RTI 1110 XXXX XXXX XXXX
  not_branch([*fetch, IM_DS|IPC_OE|PC_IE, US_RS, 0, 0, 0, 0]),
  # HLT 1111 XXXX XXXX XXXX
  not_branch([*fetch, HALT, 0, 0, 0, 0, 0]),
]

INST_N = len(ucode)
TOTAL_BYTEARRAY_SIZE = INST_N * BRANCH_FLAG_STATES_N * UCODE_N * CONTROL_WORD_SIZE

res_b = bytearray(TOTAL_BYTEARRAY_SIZE)

for i in range(INST_N):
  for j in range(BRANCH_FLAG_STATES_N):
    for k in range(UCODE_N):
      for l in range(CONTROL_WORD_SIZE):
        idx = l + \
            k * CONTROL_WORD_SIZE + \
            j * CONTROL_WORD_SIZE * UCODE_N + \
            i * CONTROL_WORD_SIZE * UCODE_N * BRANCH_FLAG_STATES_N
        cword = ucode[i][j][k]
        assert len(ucode[i][j]) == 8
        cbyte = (cword >> (8 * (CONTROL_WORD_SIZE - l - 1))) & 0xff
        print(f"inst: {i:>04b}, idx: {idx:>04x}, cbyte: {cbyte:>08b}")
        res_b[idx] = cbyte

with open(outfile_path, "wb") as f:
  f.write(res_b)
