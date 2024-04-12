import time
from atk16_asm import assemble
from atk16_emu import Machine
from test.utils import pad_bytearray

def run_mul(a: int, b: int) -> int:
  filename = "test/e2e/call_stack/run_mul.atk16"
  with open(filename, "r") as f:
    source = f.read()

  source = source.replace("%INPUT_A%", str(a))
  source = source.replace("%INPUT_B%", str(b))

  obj = assemble(source, filename)
  rom_image = pad_bytearray(obj.program)

  machine = Machine()
  machine.load_rom_image(rom_image)
  machine.reset()
  machine.run_until_halted()

  return machine.rg.value

def test_mul_0():
  expected = 0
  received = run_mul(0, -100)

  assert expected == received

def test_mul_1():
  expected = 30
  received = run_mul(5, 6)

  assert expected == received

def test_mul_2():
  a = -100
  b = 200
  expected = a * b
  received = run_mul(a, b)
  # interpret received as a 16 bit signed integer, using powers of two in the math
  received = received if received < 2**15 else received - 2**16

  assert expected == received

def run_fact(n: int) -> int:
  filename = "test/e2e/call_stack/run_fact.atk16"
  with open(filename, "r") as f:
    source = f.read()

  source = source.replace("%INPUT%", str(n))
  obj = assemble(source, filename)
  rom_image = pad_bytearray(obj.program)

  machine = Machine()
  machine.load_rom_image(rom_image)
  machine.reset()
  machine.run_until_halted()

  return machine.rg.value

def test_fact_0():
  expected = 1
  received = run_fact(0)

  assert expected == received

def test_fact_6():
  expected = 720
  received = run_fact(6)

  assert expected == received

