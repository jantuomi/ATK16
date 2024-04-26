import pygame
import sys
import os
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

    with open(os.path.join(os.path.dirname(__file__), "resources", "charmem.bin"), "rb") as f:
      charmem = f.read()
      for i in range(len(charmem)):
        self.char_mem.memory[i] = int(charmem[i]) & 0xFF

  def frame(self):
    if not self.surface:
      return

    for ty in range(2 ** 5):
      for tx in range(2 ** 6):
        text_addr = (ty << 6) + tx
        char_s = self.text_mem.read(text_addr)

        for cy in range(2 ** 3):
          char_addr = (char_s << 3) + cy
          char_d = self.char_mem.read(char_addr)
          sy = (ty << 3) + cy

          for cx in range(2 ** 3):
            sx = (tx << 3) + cx
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
    self.set_irq_line = set_irq_line

    self.shift_pressed = False
    self.meta_pressed = False

  def set_key_down(self, i: int):
    if i == pygame.K_LSHIFT or i == pygame.K_RSHIFT:
      self.shift_pressed = True
      return

    if i == pygame.K_LALT or i == pygame.K_RALT or i == pygame.K_LMETA or i == pygame.K_RMETA:
      self.meta_pressed = True
      return

    elif 0 <= i <= 0xFFFF:
      # sometimes the keycodes are out of range, don't know why

      # manually map some keys to the nordic MacOS layout
      # should work for some parts for a US layout as well
      if i == pygame.K_RETURN:
        self.latest_pressed = 10
      elif i == pygame.K_SPACE:
        self.latest_pressed = i
      elif self.shift_pressed and self.meta_pressed and i == 56: # 8
        self.latest_pressed = 123          # {
      elif self.shift_pressed and self.meta_pressed and i == 57: # 9
        self.latest_pressed = 125          # }
      elif self.shift_pressed and self.meta_pressed and i == 52: # 4
        self.latest_pressed = 2            # smiley opaque
      elif self.shift_pressed and i == 60: # <
        self.latest_pressed = 62           # >
      elif self.shift_pressed and i == 48: # 0
        self.latest_pressed = 61           # =
      elif self.shift_pressed and i == 43: # +
        self.latest_pressed = 63           # ?
      elif self.shift_pressed and i == 44: # ,
        self.latest_pressed = 59           # ;
      elif self.shift_pressed and i == 45: # -
        self.latest_pressed = 95           # _
      elif self.shift_pressed and i == 46: # .
        self.latest_pressed = 58           # :
      elif self.shift_pressed and i == 39: # '
        self.latest_pressed = 42           # *
      elif self.shift_pressed and i == 52: # 4
        self.latest_pressed = 1            # smiley transparent
      elif self.shift_pressed and i == 168: # ¨
        self.latest_pressed = 94           # ^
      elif self.shift_pressed and i < 65:  # number row
        self.latest_pressed = i - 16       # symbols above the number row
      elif self.shift_pressed:             # a - z
        self.latest_pressed = i - 32       # A - Z
      elif self.meta_pressed and i == 49:  # 1
        self.latest_pressed = 169          # ©
      elif self.meta_pressed and i == 50:  # 2
        self.latest_pressed = 64           # @
      elif self.meta_pressed and i == 51:  # 3
        self.latest_pressed = 163          # £
      elif self.meta_pressed and i == 52:  # 4
        self.latest_pressed = 36           # $
      elif self.meta_pressed and i == 55:  # 7
        self.latest_pressed = 124          # |
      elif self.meta_pressed and i == 56:  # 8
        self.latest_pressed = 91           # [
      elif self.meta_pressed and i == 57:  # 9
        self.latest_pressed = 93           # ]
      elif self.meta_pressed and i < 65:   # number row
        self.latest_pressed = i + 14       # symbols below the number row
      elif i == 168:                       # ¨
        self.latest_pressed = 126          # ~
      else:
        self.latest_pressed = i

    self.set_irq_line(IRQ_LINE_KEYBOARD)

  def set_key_up(self, i: int):
    if i == pygame.K_LSHIFT or i == pygame.K_RSHIFT:
      self.shift_pressed = False

    elif i == pygame.K_LALT or i == pygame.K_RALT or i == pygame.K_LMETA or i == pygame.K_RMETA:
      self.meta_pressed = False

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
