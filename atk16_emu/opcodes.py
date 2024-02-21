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
class LDR():
  to_reg: int
  addr_reg: int

@dataclass
class STR():
  from_reg: int
  addr_reg: int

@dataclass
class LDI():
  to_reg: int
  imm: int

@dataclass
class JPR():
  addr_reg: int

@dataclass
class JPI():
  imm: int

@dataclass
class BRR():
  flag: int
  addr_reg: int

@dataclass
class BRI():
  flag: int
  addr_imm: int

@dataclass
class LPC():
  target_reg: int

@dataclass
class NOP():
  pass

@dataclass
class ISRP0():
  pass

@dataclass
class ISRP1():
  pass

@dataclass
class RTI():
  pass

@dataclass
class HLT():
  pass
