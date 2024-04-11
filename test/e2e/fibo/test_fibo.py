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
  print("Instructions / second:", steps_taken / (elapsed / 1e9))

  assert machine.ra.value == 41443
