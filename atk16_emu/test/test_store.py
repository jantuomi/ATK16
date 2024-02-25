from atk16_emu import Machine
from test.utils import make_rom

def test_str():
  machine = Machine()

  rom_image = make_rom([0b0011_000_000_001_000]) # STR addr=RA data=RB
  machine.load_rom_image(rom_image)
  machine.reset()

  machine.mem_write(0x8000, 0x1234)
  machine.ra.value = 0x8000 # mem address
  machine.rb.value = 0x2345

  machine.run()
  machine.step()

  assert machine.mem_read(0x8000) == 0x2345
