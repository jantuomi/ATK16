from atk16_emu.emu import Machine
from atk16_emu.test.utils import make_rom

def test_alr_s0_small_unsigned():
  machine = Machine()

  rom_image = make_rom([0b0000_010_000_001_000]) # ALR target=RC left=RA right=RB alu_code=0
  machine.load_rom_image(rom_image)
  machine.reset()

  machine.ra.value = 10
  machine.rb.value = 20

  machine.run()
  machine.step()

  assert machine.rc.value == 30
  assert machine.fr.carry == 0
  assert machine.fr.overflow == 0
  assert machine.fr.zero == 0
  assert machine.fr.sign == 0

def test_alr_s0_large_unsigned():
  machine = Machine()

  rom_image = make_rom([0b0000_010_000_001_000]) # ALR target=RC left=RA right=RB alu_code=0
  machine.load_rom_image(rom_image)
  machine.reset()

  machine.ra.value = 0xFFFF
  machine.rb.value = 20

  machine.run()
  machine.step()

  assert machine.rc.value == 19
  assert machine.fr.carry == 1
  assert machine.fr.overflow == 0
  assert machine.fr.zero == 0
  assert machine.fr.sign == 0

def test_alr_s0_small_signed():
  machine = Machine()

  rom_image = make_rom([0b0000_010_000_001_000]) # ALR target=RC left=RA right=RB alu_code=0
  machine.load_rom_image(rom_image)
  machine.reset()

  machine.ra.value = 0xFFFF # -1
  machine.rb.value = 0x0001 # 1

  machine.run()
  machine.step()

  assert machine.rc.value == 0
  assert machine.fr.carry == 1
  assert machine.fr.overflow == 0
  assert machine.fr.zero == 1
  assert machine.fr.sign == 0

def test_alr_s0_large_signed():
  machine = Machine()

  rom_image = make_rom([0b0000_010_000_001_000]) # ALR target=RC left=RA right=RB alu_code=0
  machine.load_rom_image(rom_image)
  machine.reset()

  machine.ra.value = 0x7FFF # 32767
  machine.rb.value = 0x0001 # 1

  machine.run()
  machine.step()

  assert machine.rc.value == 0x8000 # -32768
  assert machine.fr.carry == 0
  assert machine.fr.overflow == 1
  assert machine.fr.zero == 0
  assert machine.fr.sign == 1