import sys
from dataclasses import dataclass
from .emu import Machine
from .debugger import Debugger

@dataclass
class Options:
  rom_image_path: str | None
  debugger_enabled: bool
  peripherals_enabled: bool

options = Options(
  rom_image_path=None,
  debugger_enabled=False,
  peripherals_enabled=True,
)

def print_help():
  print("Usage: emu_cli.py [-d] [-n] [-h|--help] <rom_path>")
  print("")
  print("Options:")
  print("  <rom_path>: path to the rom image")
  print("  -h: print help")
  print("  -d: enable debugger")
  print("  -n: no peripherals (useful for automated testing)")

def main():
  for arg in sys.argv[1:]:
    if arg == "-d":
      options.debugger_enabled = True
    elif arg == "-n":
      options.peripherals_enabled = False
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

  if not options.debugger_enabled:
    with open(options.rom_image_path, "rb") as f:
      rom_image = bytearray(f.read())

    machine = Machine(options.peripherals_enabled)
    machine.load_rom_image(rom_image)
    machine.reset()
    machine.run_until_halted()

    print("Machine halted.")
    print("===============")

    machine.print_state_summary()
    machine.print_hotspot_summary()

  else:
    debugger = Debugger(options.peripherals_enabled)
    debugger.load_rom_image(options.rom_image_path)
    debugger.activate()

if __name__ == "__main__":
  main()
