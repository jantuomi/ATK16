import random
from typing import Literal
from dataclasses import dataclass
import sys
from .opcodes import *

class Register:
  def __init__(self, bits: int):
    self.bits = bits
    self.value = random.randint(0, 2 ** bits)

  def set_value(self, value: int):
    if value < 0 or value >= 2 ** self.bits:
      raise ValueError(f"Value {value} is out of range for {self.bits}-bit register")

    self.value = value

class Counter:
  def __init__(self, bits: int):
    self.bits = bits
    self.value = random.randint(0, 2 ** bits)

  def step(self):
    self.value = (self.value + 1) % (2 ** self.bits)

  def reset(self):
    self.value = 0

class ROM:
  def __init__(self, addr_bits: int, data_bits: int):
    self.addr_bits = addr_bits
    self.data_bits = data_bits
    self.memory = [random.randint(0, 2 ** data_bits) for _ in range(2 ** addr_bits)]

  def read(self, addr: int) -> int:
    if addr < 0 or addr >= 2 ** self.addr_bits:
      raise ValueError(f"Address {addr} is out of range for {self.addr_bits}-bit ROM")

    return self.memory[addr]

class RAM:
  def __init__(self, addr_bits: int, data_bits: int):
    self.addr_bits = addr_bits
    self.data_bits = data_bits
    self.memory = [random.randint(0, 2 ** data_bits) for _ in range(2 ** addr_bits)]

  def read(self, addr: int) -> int:
    if addr < 0 or addr >= 2 ** self.addr_bits:
      raise ValueError(f"Address {addr} is out of range for {self.addr_bits}-bit RAM")

    return self.memory[addr]

  def write(self, addr: int, value: int):
    if addr < 0 or addr >= 2 ** self.addr_bits:
      raise ValueError(f"Address {addr} is out of range for {self.addr_bits}-bit RAM")

    if value < 0 or value >= 2 ** self.data_bits:
      raise ValueError(f"Value {value} is out of range for {self.data_bits}-bit RAM")

    self.memory[addr] = value

@dataclass
class ALUFlags:
  carry: bool
  overflow: bool
  zero: bool
  sign: bool

@dataclass
class ALUResult:
  value: int
  flags: ALUFlags

class ALU:
  def __init__(self):
    pass

  def process(self, S: int, L: int, R: int):
    if L < 0 or L >= 2 ** 16:
      raise ValueError(f"Invalid ALU L: {L}")

    if R < 0 or R >= 2 ** 16:
      raise ValueError(f"Invalid ALU R: {R}")

    match S:
      case 0: # L + R
        py_sum = L + R
        result = py_sum & 0xFFFF
        return ALUResult(result, ALUFlags(
          carry = py_sum >= 2 ** 16,
          overflow = (L & 0x8000) == (R & 0x8000) and (L & 0x8000) != (result & 0x8000),
          zero = result == 0,
          sign = (result & 0x8000) != 0
        ))
      case 1: # L - R
        py_sum = L - R
        result = py_sum & 0xFFFF
        return ALUResult(result, ALUFlags(
          carry = py_sum < 0,
          overflow = (L & 0x8000) != (R & 0x8000) and (L & 0x8000) != (result & 0x8000),
          zero = result == 0,
          sign = (result & 0x8000) != 0
        ))
      case 2: # L and R
        raise NotImplementedError()
      case 3: # L or R
        raise NotImplementedError()
      case 4: # L xor R
        raise NotImplementedError()
      case 5: # L >> R logical
        raise NotImplementedError()
      case 6: # L >>> R arithmetic
        raise NotImplementedError()
      case 7: # L << R
        raise NotImplementedError()

    raise ValueError(f"Invalid ALU S: {S}")

class Machine:
  def __init__(self):
    self.rom = ROM(15, 16)
    self.ram = RAM(15, 16)
    self.alu = ALU()

    self.ra = Register(16)
    self.rb = Register(16)
    self.rc = Register(16)
    self.rd = Register(16)
    self.re = Register(16)
    self.rf = Register(16)
    self.rg = Register(16)
    self.rh = Register(16)

    self.pc = Counter(16)
    self.fr = ALUFlags(
      carry = False,
      overflow = False,
      zero = False,
      sign = False,
    )

    self.running = False

  def mem_read(self, addr: int):
    if addr < 2 ** 15:
      return self.rom.read(addr & 0x7FFF)
    else:
      # TODO: Implement memory-mapped I/O
      return self.ram.read(addr & 0x7FFF)

  def mem_write(self, addr: int, value: int):
    if addr < 2 ** 15:
      raise ValueError(f"Cannot write to ROM, addr: {addr:>04x}")
    else:
      # TODO: Implement memory-mapped I/O
      self.ram.write(addr & 0x7FFF, value)

  def get_nth_register(self, n: int) -> Register:
    if n < 0 or n >= 8:
      raise ValueError(f"Invalid register number: {n}")

    s = chr(ord("a") + n)
    return self.__getattribute__(f"r{s}")

  def load_rom_image(self, bytes: bytearray):
    if len(bytes) != 2 ** 16:
      raise ValueError("ROM image must be 64 KiB")

    # Loop over bytes, constructing two-byte words and storing them in ROM
    i = 0
    while i < len(bytes):
      high_byte = bytes[i]
      low_byte = bytes[i + 1]
      word = (high_byte << 8) | low_byte
      self.rom.memory[i // 2] = word
      i += 2

  def reset(self):
    self.pc.reset()
    self.running = False

  def run(self):
    "Set running = True."
    self.running = True

  def run_until_halted(self):
    "Run the self until HLT instruction is encountered"
    self.running = True
    while self.running:
      self.step()

  def check_nth_flag(self, n: int) -> bool:
    match n:
      case 0: return self.fr.carry
      case 1: return self.fr.overflow
      case 2: return self.fr.zero
      case 3: return self.fr.sign

    raise ValueError(f"Invalid flag number: {n}")

  def step(self):
    if not self.running:
      raise RuntimeError("Machine is not running")

    pc_addr = self.pc.value
    self.pc.step()

    instr = self.mem_read(pc_addr)
    instruction = self.decode(instr)

    try:
      match instruction:
        case ALR(target, left, right, alu_code):
          alu_result = self.alu.process(
            S = alu_code,
            L = self.get_nth_register(left).value,
            R = self.get_nth_register(right).value,
          )
          self.fr = alu_result.flags

          target_reg = self.get_nth_register(target)
          target_reg.value = alu_result.value

        case ALI(target, left, imm, alu_code):
          alu_result = self.alu.process(
            S = alu_code,
            L = self.get_nth_register(left).value,
            R = imm,
          )
          self.fr = alu_result.flags

          target_reg = self.get_nth_register(target)
          target_reg.value = alu_result.value

        case LDR(to_reg, addr_reg):
          addr = self.get_nth_register(addr_reg).value
          value = self.mem_read(addr)

          target_reg = self.get_nth_register(to_reg)
          target_reg.value = value

        case STR(addr_reg, from_reg):
          addr = self.get_nth_register(addr_reg).value
          value = self.get_nth_register(from_reg).value

          self.mem_write(addr, value)

        case LDI(to_reg, imm):
          target_reg = self.get_nth_register(to_reg)
          target_reg.value = imm

        case JPR(addr_reg):
          addr = self.get_nth_register(addr_reg).value
          self.pc.value = addr

        case JPI(imm):
          self.pc.value = (self.pc.value + imm) & 0xFFFF

        case BRR(flag, addr_reg):
          if self.check_nth_flag(flag):
            addr = self.get_nth_register(addr_reg).value
            self.pc.value = addr

        case BRI(flag, addr_imm):
          if self.check_nth_flag(flag):
            self.pc.value = (self.pc.value + addr_imm) & 0xFFFF

        case LPC(target_reg):
          target_reg = self.get_nth_register(target_reg)
          target_reg.value = self.pc.value

        case NOP():
          pass

        case ISRP0():
          # interrupt service routine, read store PC in IPC register, set PC to ISRA value
          raise NotImplementedError()

        case ISRP1():
          # interrupt service routine, read store PC in IPC register, set PC to ISRA value
          raise NotImplementedError()

        case RTI():
          # return from interrupt routine, read PC from IPC register
          raise NotImplementedError()

        case HLT():
          self.running = False

    except:
      print(f"Error while executing instruction {instr:>016b} ({instr:>04x}) at address {pc_addr:>04x}", file=sys.stderr)
      raise

  def decode(self, instr: int):
    opcode = (instr & 0xF000) >> 12
    opdata = instr & 0x0FFF
    match opcode:
      case 0b0000: return ALR(target=(opdata & 0b111000000000) >> 9,
                              left=(opdata & 0b000111000000) >> 6,
                              right=(opdata & 0b000000111000) >> 3,
                              alu_code=opdata & 0b000000000111)
      case 0b0001: return ALI(target=(opdata & 0b111000000000) >> 9,
                              left=(opdata & 0b000111000000) >> 6,
                              imm=(opdata & 0b000000111000) >> 3,
                              alu_code=opdata & 0b000000000111)
      case 0b0010: return LDR(to_reg=(opdata & 0b111000000000) >> 9,
                              addr_reg=(opdata & 0b000111000000) >> 6)
      case 0b0011: return STR(from_reg=(opdata & 0b000111000000) >> 6,
                              addr_reg=(opdata & 0b000000111000) >> 3)
      case 0b0100: return LDI(to_reg=(opdata & 0b111000000000) >> 9,
                              imm=opdata & 0b000111111111)
      case 0b0101: return JPR(addr_reg=(opdata & 0b000111000000) >> 6)
      case 0b0110: return JPI(imm=opdata & 0b000111111111)
      case 0b0111: return BRR(flag=(opdata & 0b011000000000) >> 9,
                              addr_reg=(opdata & 0b000111000000) >> 6)
      case 0b1000: return BRI(flag=(opdata & 0b011000000000) >> 9,
                              addr_imm=opdata & 0b000111111111)
      case 0b1001: return LPC(target_reg=(opdata & 0b111000000000) >> 9)
      case 0b1010: return NOP()
      case 0b1011: return NOP()
      case 0b1100: return ISRP0()
      case 0b1101: return ISRP1()
      case 0b1110: return RTI()
      case 0b1111: return HLT()

    raise ValueError(f"Invalid instruction: {instr:>016b} ({instr:>04x})")

  def print_state_summary(self):
    for i in range(8):
      reg_name = f"r{chr(ord('a') + i)}"
      value = getattr(self, reg_name).value
      print(f"{reg_name.upper()}: 0x{value:>04x} ({value})")

    print(f"PC: 0x{self.pc.value:>04x} ({self.pc.value})")
    print(f"FR: carry={self.fr.carry}\n    "
          f"overflow={self.fr.overflow}\n    "
          f"zero={self.fr.zero}\n    "
          f"sign={self.fr.sign}")
    for i in range(8):
      print(f"RAM[{i}]: 0x{self.ram.read(i):>04x} ({self.ram.read(i)})")