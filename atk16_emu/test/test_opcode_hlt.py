from atk16_emu import Machine
from test.utils import make_rom

def test_hlt():
  machine = Machine()

  rom_image = make_rom([0b1111_0000_0000_0000]) # HLT
  machine.load_rom_image(rom_image)
  machine.reset()
  machine.run()

  assert machine.pc.value == 0
  assert machine.running == True

  machine.step()

  assert machine.pc.value == 1
  assert machine.running == False
