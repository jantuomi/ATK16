from typing import Literal
from dataclasses import dataclass

Opcode = Literal[
  "ALR",
  "ALI",
  "LDR",
  "STR",
  "LDI",
  "JPR",
  "JPI",
  "BRR",
  "BRI",
  "LPC",
  "NOP",
  "ISRP0",
  "ISRP1",
  "RTI",
  "HLT",
]

@dataclass
class ALR():
  target: int
  left: int
  right: int
  alu_code: int

@dataclass
class ALI():
  target: int
  left: int
  imm: int
  alu_code: int

@dataclass
class HLT():
  pass
