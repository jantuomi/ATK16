import pygame
import sys
from .memory import RAM, ROM

class TPU:
  def __init__(self):
    self.surface = pygame.Surface((320, 240))
    self.surface.fill((255, 0, 0))
    self.text_mem = RAM(11, 8)
    self.char_mem = ROM(11, 8)

    with open("out/charmem.bin", "rb") as f:
      charmem = f.read()
      for i in range(len(charmem)):
        self.char_mem.memory[i] = int(charmem[i]) & 0xFF

  def frame(self):
    if not self.surface:
      return

    for cy in range(2 ** 3):
      for ty in range(2 ** 5):
        for tx in range(2 ** 6):
          text_addr = (ty << 6) + tx
          char_s = self.text_mem.read(text_addr)
          char_addr = (char_s << 3) + cy
          char_d = self.char_mem.read(char_addr)

          for cx in range(2 ** 3):
            sx = (tx << 3) + cx
            sy = (ty << 3) + cy
            #print("cy", cy, "ty", ty, "cx", cx, "tx", tx, "text_addr", text_addr, "char_s", char_s, "char_addr", char_addr)
            pixel_value_bit = (char_d >> cx) & 1

            self.surface.set_at((sx, sy), (255 * pixel_value_bit, 255 * pixel_value_bit, 255))

  def write_char(self, addr: int, char: int):
    # address: 5 bits y, 6 bits x
    self.text_mem.write(addr, char)

class Graphics:
  def __init__(self):
    pygame.init()
    pygame.display.init()

    surface_scale = 4
    self.screen = pygame.display.set_mode((320 * surface_scale, 240 * surface_scale))
    self.clock = pygame.time.Clock()
    self.tpu = TPU()
    self.active_picture_unit: TPU | None = None

  def activate_tpu(self):
    self.active_picture_unit = self.tpu

  def activate_ppu(self):
    raise NotImplementedError()

  def deactivate(self):
    self.active_picture_unit = None

  def step(self):
    if self.active_picture_unit:
      for event in pygame.event.get():
        if event.type == pygame.QUIT:
            sys.exit(0)

      self.active_picture_unit.frame()

      self.screen.blit(pygame.transform.scale(self.tpu.surface, self.screen.get_size()), (0, 0))
      pygame.display.flip()
      self.clock.tick(60)

  def write(self, addr: int, char: int):
    # take only the lower 11 bits
    self.tpu.write_char(addr & 0x7FF, char)

class DummyGraphics:
  def __init__(self):
    pass

  def activate_tpu(self):
    pass

  def activate_ppu(self):
    pass

  def deactivate(self):
    pass

  def step(self):
    pass
