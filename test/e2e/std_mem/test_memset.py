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
  expected_n = 5
  expected_value = 3
  assert all(map(lambda x: x == expected_value, machine.ram.memory[addr:addr+expected_n]))
