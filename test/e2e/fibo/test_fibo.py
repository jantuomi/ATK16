from atk16_asm import assemble
from atk16_emu import Machine
from test.utils import pad_bytearray

def test_fibo():
  filename = "test/e2e/fibo/fibo.atk16"
  with open(filename, "r") as f:
    source = f.read()

  obj = assemble(source, filename)
  rom_image = pad_bytearray(obj)

  machine = Machine()
  machine.load_rom_image(rom_image)
  machine.reset()
  machine.run_until_halted()
  machine.print_state_summary()

  assert machine.ra.value == 34
