from typing import Callable, Literal, cast
from dataclasses import dataclass
import re

RegisterValue = Literal["RA", "RB", "RC", "RD", "RE", "RF", "RG", "RH"]
REGISTERS: list[RegisterValue] = ["RA", "RB", "RC", "RD", "RE", "RF", "RG", "RH"]
@dataclass
class RegisterRef:
  """Reference to a register, e.g. RA"""
  reg: RegisterValue

  def __str__(self) -> str:
    return self.reg

@dataclass
class Immediate:
  """Immediate value, e.g. 0x1234"""
  value: int
  base: Literal["dec", "hex", "bin"]

  def __str__(self) -> str:
    if self.base == "dec":
      return str(self.value)
    elif self.base == "hex":
      return f"0x{self.value:>04x}"
    elif self.base == "bin":
      return f"0b{self.value:>016b}"

    raise Exception("Invalid base: {self.base}")

@dataclass
class Label:
  """Label declaration, e.g. @loop"""
  label: str

  def __str__(self) -> str:
    return f"@{self.label}"

@dataclass
class LabelRef:
  """Label reference, e.g. &loop"""
  label: str

  def __str__(self) -> str:
    return f"&{self.label}"

FlagValue = Literal[0, 1, 2, 3]
FLAGS = ["carry", "overflow", "zero", "sign"]
@dataclass
class Flag:
  """ALU Flag, e.g. zero"""
  num: FlagValue

  @staticmethod
  def from_str(flag: str) -> "Flag":
    return Flag(cast(FlagValue, FLAGS.index(flag)))

  def __str__(self) -> str:
    return FLAGS[self.num]

ALUCodeValue = Literal[0, 1, 2, 3, 4, 5, 6, 7]
ALU_CODES = ["al_plus", "al_minus", "al_and", "al_or", "al_xor", "al_sll", "al_slr", "al_sar"]
@dataclass
class ALUCode:
  """ALU operation code, e.g. al_plus"""
  code: ALUCodeValue

  @staticmethod
  def from_str(code: str) -> "ALUCode":
    return ALUCode(cast(ALUCodeValue, ALU_CODES.index(code)))

  def __str__(self) -> str:
    return ALU_CODES[self.code]

@dataclass
class Expr:
  """Python expression that can be evaluated to an immediate value"""
  expr: str

  def __str__(self) -> str:
    return f"${{{self.expr}}}"

@dataclass
class Directive:
  """Directive, e.g. #address"""
  directive: str

  def __str__(self) -> str:
    return f"#${self.directive}"

@dataclass
class StringLiteral:
  """String literal, e.g. "hello world" """
  string: str

  def __str__(self) -> str:
    return f'"{self.string}"'

@dataclass
class Symbol:
  """Symbol, e.g. loop"""
  symbol: str

  def __str__(self) -> str:
    return self.symbol

Term = RegisterRef | Immediate | LabelRef | Label | Flag | ALUCode | Expr | Directive | StringLiteral | Symbol

symbol_pattern = "[a-zA-Z_][a-zA-Z0-9_]*"

def parse(tokens: list[str]) -> list[Term]:
  def parse_token(term: str, idx: int) -> Term:
    if term in REGISTERS:
      return RegisterRef(term)
    elif term in FLAGS:
      return Flag.from_str(term)
    elif term.startswith("${") and term.endswith("}"):
      return Expr(term[2:-1])
    elif term.startswith("\"") and term.endswith("\""):
      string_chars = term[1:-1]
      string_chars = string_chars.replace("\\n", "\n")
      string_chars = string_chars.replace("\\r", "\r")
      string_chars = string_chars.replace("\\t", "\t")
      return StringLiteral(string_chars)
    elif term.startswith("&"):
      return LabelRef(term[1:])
    elif term.startswith("@"):
      return Label(term[1:])
    elif term.startswith("#"):
      return Directive(term[1:])
    elif term.startswith("0x"):
      return Immediate(int(term.replace("_", ""), 16), "hex")
    elif term.startswith("0b"):
      return Immediate(int(term.replace("_", ""), 2), "bin")
    elif term.isdigit():
      return Immediate(int(term.replace("_", ""), 10), "dec")
    elif re.match(symbol_pattern, term) is not None:
      return Symbol(term)
    else:
      raise Exception(f"Invalid term \"{term}\" at index {idx} in {tokens}")

  return [parse_token(token, idx) for idx, token in enumerate(tokens)]
