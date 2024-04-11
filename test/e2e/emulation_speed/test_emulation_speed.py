import time
from atk16_asm import assemble
from atk16_emu import Machine
from test.utils import pad_bytearray

def test_emulation_speed():
  filename = "test/e2e/emulation_speed/long_loop.atk16"
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
  mean_ns_per_instr = elapsed / steps_taken
  print("Instructions / second:", steps_taken / (elapsed / 1e9))
  print("ns / instruction:", mean_ns_per_instr)

  # VGA frequency is 25.175 MHz
  # Divided by 16, that's 1.5734375 MHz (main clock)
  # Instructions take 3–5 cycles, the average instruction takes around 4 cycles
  # 1.5734375 MHz / 4 = 393359.375 Hz
  # That is the goal instruction frequency
  # TODO limit emulation speed to 393359.375 Hz ~= 2542 ns per instruction
  #      this is in order to have similar speed as the actual machine

  expected = 2542
  allowed_range = 0.05 # 0..1
  expected_min = expected * (1 - allowed_range)
  expected_max = expected * (1 + allowed_range)
  percentage = 100 * mean_ns_per_instr / expected
  assert expected_min < mean_ns_per_instr, f"Emulator seems to run too fast ({percentage:.2f}% of expected ns per instruction)"
  assert mean_ns_per_instr < expected_max, f"Emulator seems to run too slow ({percentage:.2f}% of expected ns per instruction)"
