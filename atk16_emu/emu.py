import os
os.environ['PYGAME_HIDE_SUPPORT_PROMPT'] = "hide"

import sys
from dataclasses import dataclass

from .opcodes import *
from .memory import *
from .colors import C
from .peripherals import Graphics, DummyGraphics

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
        result = L & R
        return ALUResult(result, ALUFlags(
          carry = False,
          overflow = False,
          zero = result == 0,
          sign = (result & 0x8000) != 0
        ))
      case 3: # L or R
        result = L | R
        return ALUResult(result, ALUFlags(
          carry = False,
          overflow = False,
          zero = result == 0,
          sign = (result & 0x8000) != 0
        ))
      case 4: # L xor R
        result = L ^ R
        return ALUResult(result, ALUFlags(
          carry = False,
          overflow = False,
          zero = result == 0,
          sign = (result & 0x8000) != 0
        ))
      case 5: # L >> R logical
        result = L >> R
        return ALUResult(result, ALUFlags(
          carry = False,
          overflow = False,
          zero = result == 0,
          sign = (result & 0x8000) != 0
        ))
      case 6: # L >>> R arithmetic
        # shift right but keep the sign bit (16-bit)
        result = (L >> R) | (L & 0x8000)
        return ALUResult(result, ALUFlags(
          carry = False,
          overflow = False,
          zero = result == 0,
          sign = (result & 0x8000) != 0
        ))
      case 7: # L << R
        result = (L << R) & 0xFFFF
        return ALUResult(result, ALUFlags(
          carry = False,
          overflow = False,
          zero = result == 0,
          sign = (result & 0x8000) != 0
        ))

    raise ValueError(f"Invalid ALU S: {S}")

class Machine:
  def __init__(self, peripherals_enabled: bool = False):
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

    self.steps_taken = 0
    self.running = False

    self.peripherals_enabled = peripherals_enabled
    if peripherals_enabled:
      self.graphics = Graphics()
    else:
      self.graphics = DummyGraphics()

  def make_copy(self):
    new_machine = Machine()

    new_machine.peripherals_enabled = self.peripherals_enabled
    new_machine.graphics = self.graphics

    new_machine.rom = ROM(self.rom.addr_bits, self.rom.data_bits)
    new_machine.rom.memory = self.rom.memory.copy()

    new_machine.ram = RAM(self.ram.addr_bits, self.ram.data_bits)
    new_machine.ram.memory = self.ram.memory.copy()

    new_machine.alu = self.alu

    new_machine.ra = Register(self.ra.bits)
    new_machine.ra.value = self.ra.value

    new_machine.rb = Register(self.rb.bits)
    new_machine.rb.value = self.rb.value

    new_machine.rc = Register(self.rc.bits)
    new_machine.rc.value = self.rc.value

    new_machine.rd = Register(self.rd.bits)
    new_machine.rd.value = self.rd.value

    new_machine.re = Register(self.re.bits)
    new_machine.re.value = self.re.value

    new_machine.rf = Register(self.rf.bits)
    new_machine.rf.value = self.rf.value

    new_machine.rg = Register(self.rg.bits)
    new_machine.rg.value = self.rg.value

    new_machine.rh = Register(self.rh.bits)
    new_machine.rh.value = self.rh.value

    new_machine.pc = Counter(self.pc.bits)
    new_machine.pc.value = self.pc.value

    new_machine.fr = ALUFlags(
      carry = self.fr.carry,
      overflow = self.fr.overflow,
      zero = self.fr.zero,
      sign = self.fr.sign,
    )

    new_machine.running = self.running

    return new_machine

  def mem_read(self, addr: int):
    if addr < 2 ** 15:
      return self.rom.read(addr & 0x7FFF)
    else:
      # TODO: Implement memory-mapped I/O
      return self.ram.read(addr & 0x7FFF)

  def mem_write(self, addr: int, value: int):
    if addr < 2 ** 15:
      raise ValueError(f"Cannot write to ROM, addr: 0x{addr:>04x}")
    else:
      # TODO: Implement memory-mapped I/O
      if addr == 0xE002 and value == 0b00:
        self.graphics.deactivate()
      elif addr == 0xE002 and value == 0b01:
        self.graphics.activate_tpu()
      elif addr == 0xE002 and value == 0b10:
        self.graphics.activate_ppu()
      elif 0xF800 < addr <= 0xFFFF:
        self.graphics.write(addr, value)
      else:
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
    self.steps_taken = 0

  def run(self):
    "Set running = True."
    self.running = True

  def run_until_halted(self):
    "Run the self until HLT instruction is encountered"
    self.running = True
    while self.running:
      self.step()

      # TODO limit emulation speed to 393359.375 Hz ~= 2542 ns per instruction
      #      this is in order to have similar speed as the actual machine
      #      see: test_emulation_speed.py
      # naive time.sleep is slow and not accurate enough

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

    self.steps_taken += 1
    pc_addr = self.pc.value
    self.pc.step()

    instr = self.mem_read(pc_addr)
    instruction = self.decode(instr)

    #print(f"Executing instruction 0b{instr:>016b} (0x{instr:>04x}) at address 0x{pc_addr:>04x}")

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
          # convert imm from signed (twos complement) 9-bit to a python int
          imm = (imm & (0b011111111)) - (imm & 0b100000000)
          self.pc.value = (self.pc.value + imm) & 0xFFFF

        case BRR(flag, addr_reg):
          if self.check_nth_flag(flag):
            addr = self.get_nth_register(addr_reg).value
            self.pc.value = addr

        case BRI(flag, addr_imm):
          # convert imm from signed (twos complement) 9-bit to a python int
          addr_imm = (addr_imm & (0b011111111)) - (addr_imm & 0b100000000)
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
      print(f"Error while executing instruction 0b{instr:>016b} (0x{instr:>04x}) at address 0x{pc_addr:>04x}", file=sys.stderr)
      raise

    self.graphics.step()

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
    pc_hex = C.OKBLUE + f"0x{self.pc.value:>04x}" + C.ENDC
    print(f"PC:     {pc_hex} ({self.pc.value})")

    for i in range(8):
      reg_name = f"r{chr(ord('a') + i)}"
      value = getattr(self, reg_name).value
      reg_hex = C.OKBLUE + f"0x{value:>04x}" + C.ENDC
      print(f"{reg_name.upper()}:     {reg_hex} ({value})")

    for i in range(8):
      ram_hex = C.OKBLUE + f"0x{self.ram.read(i):>04x}" + C.ENDC
      print(f"RAM[{i}]: {ram_hex} ({self.ram.read(i)})")

    def as_num(b: bool) -> str:
      return ((C.OKGREEN + "1") if b else (C.WARNING + "0")) + C.ENDC

    print(f"FR: C: {as_num(self.fr.carry)}, "
          f"O: {as_num(self.fr.overflow)}, "
          f"Z: {as_num(self.fr.zero)}, "
          f"S: {as_num(self.fr.sign)}")

    print()