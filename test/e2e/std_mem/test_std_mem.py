from atk16_asm import assemble
from atk16_emu import Machine
from test.utils import pad_bytearray

def test_memset():
  filename = "test/e2e/std_mem/run_memset.atk16"
  with open(filename, "r") as f:
    source = f.read()

  obj = assemble(source, filename)
  rom_image = pad_bytearray(obj.program)

  machine = Machine()
  machine.load_rom_image(rom_image)
  machine.reset()
  machine.run_until_halted()
  machine.print_state_summary()

  addr = 0x9000 - 0x8000
  n = 5
  value = 3
  assert all([x == value for x in machine.ram.memory[addr:addr+n]])

def test_memcopy():
  filename = "test/e2e/std_mem/run_memcopy.atk16"
  with open(filename, "r") as f:
    source = f.read()

  obj = assemble(source, filename)
  rom_image = pad_bytearray(obj.program)

  machine = Machine()
  machine.load_rom_image(rom_image)
  machine.reset()
  machine.run_until_halted()
  machine.print_state_summary()

  addr_from = 0x9000 - 0x8000
  addr_to = 0x9100 - 0x8000
  n = 5

  mem_range_from = machine.ram.memory[addr_from:addr_from+n]
  mem_range_to = machine.ram.memory[addr_to:addr_to+n]

  assert [mem_range_from[i] == mem_range_to[i] for i in range(n)]
