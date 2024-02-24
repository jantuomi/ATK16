import sys
from dataclasses import dataclass
from .emu import Machine
from .debugger import Debugger

@dataclass
class Options:
  rom_image_path: str | None
  debugger_enabled: bool

options = Options(
  rom_image_path=None,
  debugger_enabled=False,
)

def print_help():
  print("Usage: emu_cli.py [-d] [-h|--help] <rom_path>")
  print("")
  print("Options:")
  print("  <rom_path>: path to the rom image")
  print("  -h: print help")
  print("  -d: enable debugger")

for arg in sys.argv[1:]:
  if arg == "-d":
    options.debugger_enabled = True
  elif arg == "-h" or arg == "--help" or arg == "-?":
    print_help()
    sys.exit(0)
  elif arg[0] == "-":
    print(f"Unknown option: {arg}")
    print_help()
    sys.exit(1)
  else:
    options.rom_image_path = arg

if options.rom_image_path is None:
  print_help()
  sys.exit(1)

def load_rom_image_from_path(path: str) -> bytearray:
  with open(path, "rb") as f:
    return bytearray(f.read())

rom_image = load_rom_image_from_path(options.rom_image_path)

if not options.debugger_enabled:
  machine = Machine()
  machine.load_rom_image(rom_image)
  machine.reset()
  machine.run_until_halted()

  print("Machine halted.")
  print("===============")

  machine.print_state_summary()

else:
  debugger = Debugger()
  debugger.load_rom_image(rom_image)
  debugger.activate()
