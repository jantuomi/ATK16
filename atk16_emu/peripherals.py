import pygame
import sys
import random
from .memory import RAM, ROM
from typing import Callable

IRQ_LINE_KEYBOARD = 0

class Peripherals:
  def __init__(self, set_irq_line: Callable[[int], None]):
    pygame.init()
    self.graphics = Graphics()
    self.keyboard = Keyboard(set_irq_line=set_irq_line)
    self.terminal = Terminal()

    self.set_irq_line = set_irq_line

  def step(self):
    for event in pygame.event.get():
      if event.type == pygame.QUIT:
          sys.exit(0)
      elif event.type == pygame.KEYDOWN:
        self.keyboard.set_key_down(event.key)
      elif event.type == pygame.KEYUP:
        self.keyboard.set_key_up(event.key)

    self.graphics.step()

class DummyPeripherals:
  def __init__(self):
    self.graphics = DummyGraphics()
    self.keyboard = DummyKeyboard()
    self.terminal = Terminal()

  def step(self):
    pass

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
            pixel_value_bit = (char_d >> cx) & 1

            self.surface.set_at((sx, sy), (255 * pixel_value_bit, 255 * pixel_value_bit, 255))

  def write_char(self, addr: int, char: int):
    # address: 5 bits y, 6 bits x
    self.text_mem.write(addr, char & 0xFF)

class Graphics:
  def __init__(self):
    pygame.display.init()

    surface_scale = 4
    self.screen = pygame.display.set_mode((320 * surface_scale, 240 * surface_scale))
    pygame.display.set_caption("ATK16 Emulator")
    self.clock = pygame.time.Clock()
    self.tpu = TPU()
    self.active_picture_unit: TPU | None = None

    self.step_counter = 0
    self.draw_frame_every_n_steps = 500 # arbitrary, perf related

  def activate_tpu(self):
    self.active_picture_unit = self.tpu

  def activate_ppu(self):
    raise NotImplementedError()

  def deactivate(self):
    self.active_picture_unit = None

  def step(self):
    if self.active_picture_unit and self.step_counter >= self.draw_frame_every_n_steps:
      self.active_picture_unit.frame()

      self.screen.blit(pygame.transform.scale(self.tpu.surface, self.screen.get_size()), (0, 0))
      pygame.display.flip()
      self.clock.tick(60)

      self.step_counter = 0
    else:
      self.step_counter += 1

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

  def write(self, addr: int, char: int):
    pass

class Keyboard:
  def __init__(self, set_irq_line: Callable[[int], None]):
    # mimic electronic behaviour my assigning a random value at start
    self.latest_pressed: int = random.randrange(0, 2 ** 16)
    self.shift_pressed = False
    self.set_irq_line = set_irq_line

  def set_key_down(self, i: int):
    print("key:", i)

    if i == pygame.K_LSHIFT or i == pygame.K_RSHIFT:
      self.shift_pressed = True
      return

    if i == pygame.K_RETURN:
      self.latest_pressed = 10

    elif 0 <= i <= 0xFFFF:
      # sometimes the keycodes are out of range, don't know why
      if self.shift_pressed:
        self.latest_pressed = i - 32 # FIXME this only works for uppercase letters
                                     # for e.g. shift + 1 should be "!" but it's something else
      else:
        self.latest_pressed = i

    self.set_irq_line(IRQ_LINE_KEYBOARD)

  def set_key_up(self, i: int):
    if i == pygame.K_LSHIFT or i == pygame.K_RSHIFT:
      self.shift_pressed = False

  def read(self) -> int:
    return self.latest_pressed

class DummyKeyboard:
  def __init__(self):
    pass

  def read(self) -> int:
    print("warning: reading from dummy keyboard")
    return 0xFFFF

class Terminal:
  def __init__(self):
    pass

  def write(self, s: int):
    print(chr(s), end="")
    sys.stdout.flush()
