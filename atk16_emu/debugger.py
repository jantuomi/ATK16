from getch import getche
from .emu import Machine
from dataclasses import dataclass

@dataclass
class DbgAddrInfo:
  src_info: str
  text: str
  original_text: str
  labels: list[str]

class Debugger:
  def __init__(self):
    self.rom_image = None
    self.breakpoints: set[int] = set()
    self.machine = Machine()
    self.machine.reset()
    self.machine.run()
    self.dbg_addr_info: dict[int, DbgAddrInfo] = {}

  def load_rom_image(self, rom_image_path: str) -> None:
    self.rom_image_path = rom_image_path
    with open(rom_image_path, "rb") as f:
      self.rom_image = bytearray(f.read())

    self.machine.load_rom_image(self.rom_image)

    print(f"Loaded ROM image from \"{rom_image_path}\".")

    dbg_symbols_path = rom_image_path + ".dbg"
    try:
      with open(dbg_symbols_path, "r") as f:
        dbg_file_lines = f.readlines()
    except FileNotFoundError:
      print(f"Note: debug symbols file \"{dbg_symbols_path}\" not found.")
      return

    self.dbg_addr_info = {}
    self.dbg_text_col_width = 0
    self.dbg_original_text_col_width = 0
    for line in dbg_file_lines:
      addr, src_info, text, original_text, *labels_ = line.split(";")
      addr = int(addr, 16)

      if len(text) > self.dbg_text_col_width:
        self.dbg_text_col_width = len(text)

      if len(original_text) > self.dbg_original_text_col_width:
        self.dbg_original_text_col_width = len(original_text)

      labels = []
      for label in labels_:
        label = label.strip()
        if label != "":
          labels.append(label)

      self.dbg_addr_info[addr] = DbgAddrInfo(
        src_info=src_info,
        text=text,
        original_text=original_text,
        labels=labels,
      )

  def print_pc_context(self):
    print("=== Program context")
    for i in range(-4, 5):
      addr = self.machine.pc.value + i
      if addr < 0 or addr >= 64 * 2 ** 16:
        continue

      if i == 0:
        print(f"> 0x{addr:>04x}: 0x{self.machine.mem_read(addr):>04x}", end="")
      else:
        print(f"  0x{addr:>04x}: 0x{self.machine.mem_read(addr):>04x}", end="")

      if addr in self.dbg_addr_info:
        dbg_info = self.dbg_addr_info[addr]
        labels: str = f"[{' '.join(dbg_info.labels)}] " if len(dbg_info.labels) > 0 else ""
        print(f"  {dbg_info.text:<{self.dbg_text_col_width}} {dbg_info.original_text:<{self.dbg_original_text_col_width}} {labels}({dbg_info.src_info})")
      else:
        print()

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

      elif cmd == "l":
        load_path = input(f"Path to rom image [default: {self.rom_image_path}]: ")
        if load_path.strip() == "":
          load_path = self.rom_image_path

        self.load_rom_image(load_path)

        self.machine.reset()
        self.machine.run()
        print("Machine reset.")

      elif cmd == "r":
        while self.machine.running:
          self.machine.step()
          if self.machine.pc.value in self.breakpoints:
            for addr in self.breakpoints:
              if addr in self.dbg_addr_info and len(self.dbg_addr_info[addr].labels) > 0:
                label_suffix = f" [{' '.join(self.dbg_addr_info[addr].labels)}]"
              else:
                label_suffix = ""

            print(f"Breakpoint hit at 0x{self.machine.pc.value:>04x}{label_suffix}")
            break

        if not self.machine.running:
          print("Machine halted.")

      elif cmd == "b":
        print("Set breakpoints:")
        for addr in self.breakpoints:
          if addr in self.dbg_addr_info and len(self.dbg_addr_info[addr].labels) > 0:
            label_suffix = f" [{' '.join(self.dbg_addr_info[addr].labels)}]"
          else:
            label_suffix = ""
          print(f"  0x{addr:>04x}{label_suffix}")
        if len(self.breakpoints) == 0:
          print("<no breakpoints>")
        print()

        try:
          env: dict[str, int] = {}
          for addr in self.dbg_addr_info:
            for label in self.dbg_addr_info[addr].labels:
              env[label] = addr
          addr = eval(input("Breakpoint address: "), env)
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
    print("  0     reset machine state")
    print("  l     load ROM image and reset machine state")
    print("  q     quit")
    print("  ?     show this help")
    print()
