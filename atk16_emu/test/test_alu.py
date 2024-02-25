from atk16_emu import Machine
from test.utils import make_rom

def test_alr_add_small_unsigned():
  machine = Machine()

  rom_image = make_rom([0b0000_010_000_001_000]) # ALR target=RC left=RA right=RB alu_code=0
  machine.load_rom_image(rom_image)
  machine.reset()

  machine.ra.value = 10
  machine.rb.value = 20

  machine.run()
  machine.step()

  assert machine.rc.value == 30
  assert machine.fr.carry == False
  assert machine.fr.overflow == False
  assert machine.fr.zero == False
  assert machine.fr.sign == False

def test_alr_add_large_unsigned():
  machine = Machine()

  rom_image = make_rom([0b0000_010_000_001_000]) # ALR target=RC left=RA right=RB alu_code=0
  machine.load_rom_image(rom_image)
  machine.reset()

  machine.ra.value = 0xFFFF
  machine.rb.value = 20

  machine.run()
  machine.step()

  assert machine.rc.value == 19
  assert machine.fr.carry == True
  assert machine.fr.overflow == False
  assert machine.fr.zero == False
  assert machine.fr.sign == False

def test_alr_add_small_signed():
  machine = Machine()

  rom_image = make_rom([0b0000_010_000_001_000]) # ALR target=RC left=RA right=RB alu_code=0
  machine.load_rom_image(rom_image)
  machine.reset()

  machine.ra.value = 0xFFFF # -1
  machine.rb.value = 0x0001 # 1

  machine.run()
  machine.step()

  assert machine.rc.value == 0
  assert machine.fr.carry == True
  assert machine.fr.overflow == False
  assert machine.fr.zero == True
  assert machine.fr.sign == False

def test_alr_add_large_signed():
  machine = Machine()

  rom_image = make_rom([0b0000_010_000_001_000]) # ALR target=RC left=RA right=RB alu_code=0
  machine.load_rom_image(rom_image)
  machine.reset()

  machine.ra.value = 0x7FFF # 32767
  machine.rb.value = 0x0001 # 1

  machine.run()
  machine.step()

  assert machine.rc.value == 0x8000 # -32768
  assert machine.fr.carry == False
  assert machine.fr.overflow == True
  assert machine.fr.zero == False
  assert machine.fr.sign == True

def test_alr_sub_small_unsigned():
  machine = Machine()

  rom_image = make_rom([0b0000_010_000_001_001]) # ALR target=RC left=RA right=RB alu_code=1
  machine.load_rom_image(rom_image)
  machine.reset()

  machine.ra.value = 10
  machine.rb.value = 20

  machine.run()
  machine.step()

  assert machine.rc.value == 0xFFF6 # -10
  assert machine.fr.carry == True
  assert machine.fr.overflow == False
  assert machine.fr.zero == False
  assert machine.fr.sign == True

def test_alr_sub_large_unsigned():
  machine = Machine()

  rom_image = make_rom([0b0000_010_000_001_001]) # ALR target=RC left=RA right=RB alu_code=1
  machine.load_rom_image(rom_image)
  machine.reset()

  machine.ra.value = 0xFFFF
  machine.rb.value = 0xFFFF

  machine.run()
  machine.step()

  assert machine.rc.value == 0x0000
  assert machine.fr.carry == False
  assert machine.fr.overflow == False
  assert machine.fr.zero == True
  assert machine.fr.sign == False

def test_alr_sub_small_signed():
  machine = Machine()

  rom_image = make_rom([0b0000_010_000_001_001]) # ALR target=RC left=RA right=RB alu_code=1
  machine.load_rom_image(rom_image)
  machine.reset()

  machine.ra.value = 0x0001 # 1
  machine.rb.value = 0xFFFF # -1

  machine.run()
  machine.step()

  assert machine.rc.value == 0x0002
  assert machine.fr.carry == True
  assert machine.fr.overflow == False
  assert machine.fr.zero == False
  assert machine.fr.sign == False

def test_alr_sub_large_signed():
  machine = Machine()

  rom_image = make_rom([0b0000_010_000_001_001]) # ALR target=RC left=RA right=RB alu_code=1
  machine.load_rom_image(rom_image)
  machine.reset()

  machine.ra.value = 0x7FFF # 32767
  machine.rb.value = 0xFFFF # -1

  machine.run()
  machine.step()

  assert machine.rc.value == 0x8000 # -32768
  assert machine.fr.carry == True
  assert machine.fr.overflow == True
  assert machine.fr.zero == False
  assert machine.fr.sign == True

def test_alr_and():
  machine = Machine()

  rom_image = make_rom([0b0000_010_000_001_010]) # ALR target=RC left=RA right=RB alu_code=2
  machine.load_rom_image(rom_image)
  machine.reset()

  machine.ra.value = 0b1010
  machine.rb.value = 0b1100

  machine.run()
  machine.step()

  assert machine.rc.value == 0b1000
  assert machine.fr.carry == False
  assert machine.fr.overflow == False
  assert machine.fr.zero == False
  assert machine.fr.sign == False

def test_alr_or():
  machine = Machine()

  rom_image = make_rom([0b0000_010_000_001_011]) # ALR target=RC left=RA right=RB alu_code=3
  machine.load_rom_image(rom_image)
  machine.reset()

  machine.ra.value = 0b1010
  machine.rb.value = 0b1100

  machine.run()
  machine.step()

  assert machine.rc.value == 0b1110
  assert machine.fr.carry == False
  assert machine.fr.overflow == False
  assert machine.fr.zero == False
  assert machine.fr.sign == False

def test_alr_xor():
  machine = Machine()

  rom_image = make_rom([0b0000_010_000_001_100]) # ALR target=RC left=RA right=RB alu_code=4
  machine.load_rom_image(rom_image)
  machine.reset()

  machine.ra.value = 0b1010
  machine.rb.value = 0b1100

  machine.run()
  machine.step()

  assert machine.rc.value == 0b0110
  assert machine.fr.carry == False
  assert machine.fr.overflow == False
  assert machine.fr.zero == False
  assert machine.fr.sign == False

def test_alr_slr_small():
  machine = Machine()

  rom_image = make_rom([0b0000_010_000_001_101]) # ALR target=RC left=RA right=RB alu_code=5
  machine.load_rom_image(rom_image)
  machine.reset()

  machine.ra.value = 0b1010
  machine.rb.value = 0b0001

  machine.run()
  machine.step()

  assert machine.rc.value == 0b0101
  assert machine.fr.carry == False
  assert machine.fr.overflow == False
  assert machine.fr.zero == False
  assert machine.fr.sign == False

def test_alr_slr_large():
  machine = Machine()

  rom_image = make_rom([0b0000_010_000_001_101]) # ALR target=RC left=RA right=RB alu_code=5
  machine.load_rom_image(rom_image)
  machine.reset()

  machine.ra.value = 0xFFFF
  machine.rb.value = 0x1

  machine.run()
  machine.step()

  assert machine.rc.value == 0x7FFF
  assert machine.fr.carry == False
  assert machine.fr.overflow == False
  assert machine.fr.zero == False
  assert machine.fr.sign == False

def test_alr_sar_small():
  machine = Machine()

  rom_image = make_rom([0b0000_010_000_001_110]) # ALR target=RC left=RA right=RB alu_code=6
  machine.load_rom_image(rom_image)
  machine.reset()

  machine.ra.value = 0b1010
  machine.rb.value = 0b0001

  machine.run()
  machine.step()

  assert machine.rc.value == 0b0101
  assert machine.fr.carry == False
  assert machine.fr.overflow == False
  assert machine.fr.zero == False
  assert machine.fr.sign == False

def test_alr_sar_large():
  machine = Machine()

  rom_image = make_rom([0b0000_010_000_001_110]) # ALR target=RC left=RA right=RB alu_code=6
  machine.load_rom_image(rom_image)
  machine.reset()

  machine.ra.value = 0xFFFF
  machine.rb.value = 0x1

  machine.run()
  machine.step()

  assert machine.rc.value == 0xFFFF
  assert machine.fr.carry == False
  assert machine.fr.overflow == False
  assert machine.fr.zero == False
  assert machine.fr.sign == True


def test_alr_sll_small():
  machine = Machine()

  rom_image = make_rom([0b0000_010_000_001_111]) # ALR target=RC left=RA right=RB alu_code=7
  machine.load_rom_image(rom_image)
  machine.reset()

  machine.ra.value = 0b1010
  machine.rb.value = 0b0001

  machine.run()
  machine.step()

  assert machine.rc.value == 0b10100
  assert machine.fr.carry == False
  assert machine.fr.overflow == False
  assert machine.fr.zero == False
  assert machine.fr.sign == False

def test_alr_sll_large():
  machine = Machine()

  rom_image = make_rom([0b0000_010_000_001_111]) # ALR target=RC left=RA right=RB alu_code=7
  machine.load_rom_image(rom_image)
  machine.reset()

  machine.ra.value = 0xFFFF
  machine.rb.value = 0x1

  machine.run()
  machine.step()

  assert machine.rc.value == 0xFFFE
  assert machine.fr.carry == False
  assert machine.fr.overflow == False
  assert machine.fr.zero == False
  assert machine.fr.sign == True

def test_ali_add_small_unsigned():
  machine = Machine()

  rom_image = make_rom([0b0001_010_000_001_000]) # ALI target=RC left=RA imm=0x000A alu_code=0
  machine.load_rom_image(rom_image)
  machine.reset()

  machine.ra.value = 10

  machine.run()
  machine.step()

  assert machine.rc.value == 11
  assert machine.fr.carry == False
  assert machine.fr.overflow == False
  assert machine.fr.zero == False
  assert machine.fr.sign == False
