from getch import getche
import re
from typing import Literal
from .emu import Machine
from dataclasses import dataclass
from .colors import C

@dataclass
class DbgAddrInfo:
  src_info: str
  text: str
  original_text: str
  labels: list[str]

def try_parse_num(s: str) -> int | None:
  if s.startswith("0x"):
    return int(s[2:], 16)
  elif s.startswith("0b"):
    return int(s[2:], 2)
  elif s.isdigit():
    return int(s)
  else:
    return None

def without_colors(s: str):
  ansi_escape = re.compile(r'\x1b\[([0-9A-Za-z]+)(;[0-9A-Za-z]+)*m')
  return ansi_escape.sub('', s)

class Debugger:
  HISTORY_MAX_LEN = 1000

  def __init__(self, peripherals_enabled: bool):
    self.rom_image = None
    self.breakpoints: set[int] = set()
    self.machine = Machine(peripherals_enabled)
    self.machine.reset()
    self.machine.run()
    self.dbg_addr_info: dict[int, DbgAddrInfo] = {}

    self.time_traveling_enabled = False
    # Index complement, 0 is last element
    self.history_cmpl_index = 0
    self.history_buffer: list[Machine] = []

    self.inline_evaled_symbols_enabled: Literal["off", "dec", "hex"] = "off"

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

    print(f"Loaded debug info from \"{dbg_symbols_path}\".")

  def query_breakpoint(self) -> int | None:
    input_ = input(C.UNDERLINE + "Breakpoint address:" + C.ENDC + " ")
    as_number = try_parse_num(input_)
    if input_ == "":
      return None

    # address breakpoint
    if as_number is not None:
      return as_number

    # line breakpoint
    if ":" in input_:
      file_name, line_num = input_.strip().split(":")
      for addr, dbg_info in self.dbg_addr_info.items():
        if dbg_info.src_info == f"{file_name}:{line_num}":
          return addr

      return None

    # label breakpoint
    for addr, dbg_info in self.dbg_addr_info.items():
      if input_ in dbg_info.labels:
        return addr

  def print_breakpoints(self):
    for addr in self.breakpoints:
      if addr in self.dbg_addr_info and len(self.dbg_addr_info[addr].labels) > 0:
        label_suffix = " "
        label_suffix += C.OKCYAN + f"[{' '.join(self.dbg_addr_info[addr].labels)}] " + C.ENDC
        label_suffix += f"({self.dbg_addr_info[addr].src_info})"
      else:
        label_suffix = ""

      addr_prefix = C.OKGREEN + f"0x{addr:>04x}" + C.ENDC
      print(f"  {addr_prefix}{label_suffix}")

    if len(self.breakpoints) == 0:
        print("  <no breakpoints>")

    print()

  def inline_eval_symbols(self, dbg_text: str, base: Literal["dec", "hex"]) -> str:
    parts = dbg_text.split(" ")
    regs = ["RA", "RB", "RC", "RD", "RE", "RF", "RG", "RH"]
    result_parts = []
    for part in parts:
      if part in regs:
        reg_value = self.machine.__getattribute__(part.lower()).value
        reg_value = f"0x{reg_value:>04x}" if base == "hex" else str(reg_value)
        with_value = f"{part} " + C.WARNING + f"[{reg_value}]" + C.ENDC
        result_parts.append(with_value)
      else:
        result_parts.append(part)

    return " ".join(result_parts)

  def print_pc_context(self):
    print(C.UNDERLINE + "Program context" + C.ENDC)
    for i in range(-4, 5):
      addr = self.machine.pc.value + i
      if addr < 0 or addr >= 64 * 2 ** 16:
        continue

      addr_prefix = C.OKGREEN + f"0x{addr:>04x}" + C.ENDC
      word_infix = C.OKBLUE + f"0x{self.machine.mem_read(addr):>04x}" + C.ENDC
      if i == 0:
        print(f"> {addr_prefix}: {word_infix}", end="")
      else:
        print(f"  {addr_prefix}: {word_infix}", end="")

      if addr in self.dbg_addr_info:
        dbg_info = self.dbg_addr_info[addr]

        if self.inline_evaled_symbols_enabled != "off" and i == 0:
          dbg_text = self.inline_eval_symbols(dbg_info.text, base=self.inline_evaled_symbols_enabled)
          dbg_text_len = len(without_colors(dbg_text))
          if dbg_text_len < self.dbg_text_col_width:
            dbg_text += " " * (self.dbg_text_col_width - dbg_text_len)
        else:
          dbg_text = f"{dbg_info.text:<{self.dbg_text_col_width}}"

        dbg_orig = f"{dbg_info.original_text:<{self.dbg_original_text_col_width}}"

        labels: str = C.OKCYAN + f"[{' '.join(dbg_info.labels)}] " + C.ENDC if len(dbg_info.labels) > 0 else ""
        print(f"  {dbg_text} {dbg_orig} {labels}({dbg_info.src_info})")
      else:
        print()

    print()

  def record_history(self):
    self.history_buffer.append(self.machine.make_copy())
    if len(self.history_buffer) > Debugger.HISTORY_MAX_LEN:
      self.history_buffer.pop(0)

  def reset(self):
    self.machine.reset()
    self.machine.run()
    self.history_buffer = []
    self.history_cmpl_index = 0

  def activate(self):
    print(C.WARNING + "=== ATK16 debugger ===" + C.ENDC)
    print("Press ? to show command help. Press q to quit.")
    print()

    while True:
      if self.history_cmpl_index > 0:
        print(C.OKGREEN + f"Currently {self.history_cmpl_index} steps into history" + C.ENDC + "\n")

      self.print_pc_context()
      print("dbg> ", end="", flush=True)
      cmd = getche()
      print()

      if cmd == "q":
        break

      elif cmd == "?":
        self.print_help()

      elif cmd == "l":
        load_path = input(f"Path to ROM image [default: {self.rom_image_path}]: ")
        if load_path.strip() == "":
          load_path = self.rom_image_path

        self.load_rom_image(load_path)
        self.reset()

        print(C.WARNING + "Machine reset.\n" + C.ENDC)

      elif cmd == "r":
        if self.time_traveling_enabled:
          if self.history_cmpl_index - 1 > 0:
            self.history_cmpl_index -= 1
            self.machine = self.history_buffer[len(self.history_buffer) - self.history_cmpl_index]
            continue

          if self.history_cmpl_index == 1:
            self.history_cmpl_index = 0
            self.history_buffer.pop()

        while self.machine.running:
          if self.time_traveling_enabled:
            self.record_history()

          self.machine.step()

          if self.machine.pc.value in self.breakpoints:
            for addr in self.breakpoints:
              if addr in self.dbg_addr_info and len(self.dbg_addr_info[addr].labels) > 0:
                label_infix = C.OKCYAN + f"[{' '.join(self.dbg_addr_info[addr].labels)}]" + C.ENDC
                label_suffix = f" {label_infix} ({self.dbg_addr_info[addr].src_info})"
              else:
                label_suffix = ""

            pc_hex = C.OKGREEN + f"0x{self.machine.pc.value:>04x}" + C.ENDC
            print(C.WARNING + f"Breakpoint hit at " + C.ENDC + f"{pc_hex}{label_suffix}\n")
            break

        if not self.machine.running:
          print(C.FAIL + "Machine halted.\n" + C.ENDC)

      elif cmd == "b":
        if not self.time_traveling_enabled:
          print(C.WARNING + "Time traveling mode is not enabled." + C.ENDC + "\n")
          continue

        if self.history_cmpl_index == len(self.history_buffer):
          print(C.WARNING + "No more history available" + C.ENDC + "\n")
          continue

        self.history_cmpl_index += 1
        self.machine = self.history_buffer[len(self.history_buffer) - self.history_cmpl_index]

      elif cmd == "B":
        print(C.UNDERLINE + "Active breakpoints:" + C.ENDC)
        self.print_breakpoints()

        addr = self.query_breakpoint()
        if addr is None:
          print("Cancelled")
          continue

        if type(addr) != int or addr < 0 or addr >= 64 * 2 ** 16:
          print("Invalid address")

        addr_hex = C.OKGREEN + f"0x{addr:>04x}" + C.ENDC
        if addr in self.breakpoints:
          self.breakpoints.remove(addr)
          print(C.WARNING + f"Removed breakpoint at {addr_hex}" + C.ENDC)
        else:
          self.breakpoints.add(addr)
          print(C.WARNING + f"Set breakpoint at {addr_hex}" + C.ENDC)

        print()

      elif cmd == "n":
        if self.time_traveling_enabled:
          if self.history_cmpl_index - 1 > 0:
            self.history_cmpl_index -= 1
            self.machine = self.history_buffer[len(self.history_buffer) - self.history_cmpl_index]
            continue

          if self.history_cmpl_index == 1:
            self.history_cmpl_index = 0
            self.history_buffer.pop()

        if not self.machine.running:
          print(C.FAIL + "Machine halted.\n" + C.ENDC)
          continue

        if self.time_traveling_enabled:
          self.record_history()

        self.machine.step()

        if not self.machine.running:
          print(C.FAIL + "Machine halted.\n" + C.ENDC)

      elif cmd == "s":
        self.machine.print_state_summary()

      elif cmd == "0":
        self.reset()
        print(C.WARNING + "Machine reset.\n" + C.ENDC)

      elif cmd == "t":
        self.time_traveling_enabled = not self.time_traveling_enabled

        if not self.time_traveling_enabled:
          self.history_buffer = []
          self.history_cmpl_index = 0

        print(C.WARNING + f"Time traveling mode: {'on' if self.time_traveling_enabled else 'off'}" + C.ENDC + "\n")

      elif cmd == "e":
        if self.inline_evaled_symbols_enabled == "off":
          self.inline_evaled_symbols_enabled = "dec"
        elif self.inline_evaled_symbols_enabled == "dec":
          self.inline_evaled_symbols_enabled = "hex"
        else:
          self.inline_evaled_symbols_enabled = "off"

        print(C.WARNING + f"Inline symbol eval: {self.inline_evaled_symbols_enabled}" + C.ENDC + "\n")

      else:
        print(f"Unknown command: {cmd}")

  def print_help(self):
    print("Debugger commands:")
    print("  t     toggle time traveling mode (slow)")
    print("  r     run until next breakpoint or until halted")
    print("  n     step forward")
    print("  b     step backward (if in time traveling mode)")
    print("  B     set or remove breakpoint")
    print("  s     show state summary")
    print("  e     toggle inline symbol eval on current line")
    print("  0     reset machine state")
    print("  l     load ROM image and reset machine state")
    print("  q     quit")
    print("  ?     show this help")
    print()
