from getch import getche
from .emu import Machine

class Debugger:
  def __init__(self):
    self.rom_image = None
    self.breakpoints: set[int] = set()
    self.machine = Machine()
    self.machine.reset()
    self.machine.run()

  def load_rom_image(self, rom_image: bytearray) -> None:
    self.rom_image = rom_image
    self.machine.load_rom_image(rom_image)

    print("Loaded rom image.")

  def print_pc_context(self):
    print("=== Program context")
    for i in range(-4, 5):
      addr = self.machine.pc.value + i
      if addr < 0 or addr >= 64 * 2 ** 16:
        continue

      if i == 0:
        print(f"> 0x{addr:>04x}: 0x{self.machine.mem_read(addr):>04x}")
      else:
        print(f"  0x{addr:>04x}: 0x{self.machine.mem_read(addr):>04x}")

    print()

  def activate(self):
    print("=== ATK16 debugger ===")
    print("Press ? to show command help. Press q to quit.")
    print()

    while True:
      self.print_pc_context()
      print("dbg> ", end="", flush=True)
      cmd = getche()
      print()

      if cmd == "q":
        break
      elif cmd == "?":
        self.print_help()
      elif cmd == "r":
        while self.machine.running:
          self.machine.step()
          if self.machine.pc.value in self.breakpoints:
            print(f"Breakpoint hit at 0x{self.machine.pc.value:>04x}")
            break

        if not self.machine.running:
          print("Machine halted.")

      elif cmd == "b":
        print("Set breakpoints:")
        for addr in self.breakpoints:
          print(f"  0x{addr:>04x}")
        if len(self.breakpoints) == 0:
          print("<no breakpoints>")
        print()

        try:
          addr = eval(input("Breakpoint address: "), {})
        except:
          print("Cancelled")
          continue
        if type(addr) != int or addr < 0 or addr >= 64 * 2 ** 16:
          print("Invalid address")

        if addr in self.breakpoints:
          self.breakpoints.remove(addr)
          print(f"Removed breakpoint at 0x{addr:>04x}")
        else:
          self.breakpoints.add(addr)
      elif cmd == "n":
        if not self.machine.running:
          print("Machine halted.")
          continue

        self.machine.step()

        if not self.machine.running:
          print("Machine halted.")
      elif cmd == "s":
        self.machine.print_state_summary()
      elif cmd == "0":
        self.machine.reset()
        self.machine.run()
        print("Machine reset.")
      else:
        print(f"Unknown command: {cmd}")

  def print_help(self):
    print("Debugger commands:")
    print("  r     run until next breakpoint or until halted")
    print("  n     step forward")
    print("  b     step backward")
    print("  b     set or remove breakpoint")
    print("  s     show state summary")
    print("  0     reset the machine state")
    print("  q     quit")
    print("  ?     show this help")
    print()
