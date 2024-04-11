import time
from atk16_asm import assemble
from atk16_emu import Machine
from test.utils import pad_bytearray

def test_fibo():
  filename = "test/e2e/fibo/fibo.atk16"
  with open(filename, "r") as f:
    source = f.read()

  obj = assemble(source, filename)
  rom_image = pad_bytearray(obj.program)

  machine = Machine()
  machine.load_rom_image(rom_image)
  machine.reset()
  start = time.perf_counter_ns()
  machine.run_until_halted()
  end = time.perf_counter_ns()
  machine.print_state_summary()

  elapsed = end - start
  print("Elapsed time:", elapsed, "ns")
  steps_taken = machine.steps_taken

  # VGA frequency is 25.175 MHz
  # Divided by 16, that's 1.5734375 MHz (main clock)
  # Instructions take 3–5 cycles, the average instruction takes around 4 cycles
  # 1.5734375 MHz / 4 = 393359.375 Hz
  # That is the goal instruction frequency
  # TODO limit emulation speed to 393359.375 Hz
  print("Instructions / second:", steps_taken / (elapsed / 1e9))

  assert machine.ra.value == 41443
