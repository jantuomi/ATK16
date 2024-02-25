from atk16_emu import Machine
from test.utils import make_rom

def test_ldr():
  machine = Machine()

  rom_image = make_rom([0b0010_010_000_000_000]) # ALR target=RC reg=RA
  machine.load_rom_image(rom_image)
  machine.reset()

  machine.mem_write(0x8000, 0x1234)
  machine.ra.value = 0x8000 # mem address
  machine.rc.value = 0x2345

  machine.run()
  machine.step()

  assert machine.rc.value == 0x1234

def test_ldi():
  machine = Machine()

  rom_image = make_rom([0b0100_010_111111111]) # LDI target=RC imm=511
  machine.load_rom_image(rom_image)
  machine.reset()

  machine.rc.value = 0x123

  machine.run()
  machine.step()

  assert machine.rc.value == 511
