import pygame

class TPU:
  def __init__(self):
    self.surface: pygame.Surface | None = None
    self.text_mem = []

  def frame(self):
    # TODO
    pass

class Graphics:
  def __init__(self):
    pygame.init()
    pygame.display.init()

    self.surface = None
    self.clock = pygame.time.Clock()
    self.tpu = TPU()
    self.active_picture_unit: TPU | None = None

  def activate_tpu(self):
    if self.active_picture_unit:
      pygame.display.quit()

    pygame.display.init()
    self.surface = pygame.display.set_mode((320, 240))
    self.active_picture_unit = self.tpu
    self.active_picture_unit.surface = self.surface

  def activate_ppu(self):
    raise NotImplementedError()

  def deactivate(self):
    self.active_picture_unit = None
    pygame.display.quit()

  def step(self):
    if self.active_picture_unit:
      self.active_picture_unit.frame()

      pygame.display.flip()
      self.clock.tick(60)

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
