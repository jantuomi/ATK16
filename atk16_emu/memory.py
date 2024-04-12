import random

class Register:
  def __init__(self, bits: int):
    self.bits = bits
    self.value = random.randrange(0, 2 ** bits)

  def set_value(self, value: int):
    if value < 0 or value >= 2 ** self.bits:
      raise ValueError(f"Value {value} is out of range for {self.bits}-bit register")

    self.value = value

class Counter:
  def __init__(self, bits: int):
    self.bits = bits
    self.value = random.randrange(0, 2 ** bits)

  def step(self):
    self.value = (self.value + 1) % (2 ** self.bits)

  def reset(self):
    self.value = 0

class ROM:
  def __init__(self, addr_bits: int, data_bits: int):
    self.addr_bits = addr_bits
    self.data_bits = data_bits
    self.memory = [random.randrange(0, 2 ** data_bits) for _ in range(2 ** addr_bits)]

  def read(self, addr: int) -> int:
    if addr < 0 or addr >= 2 ** self.addr_bits:
      raise ValueError(f"Address {addr} is out of range for {self.addr_bits}-bit ROM")

    return self.memory[addr]

class RAM:
  def __init__(self, addr_bits: int, data_bits: int):
    self.addr_bits = addr_bits
    self.data_bits = data_bits
    self.memory = [random.randrange(0, 2 ** data_bits) for _ in range(2 ** addr_bits)]

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