from atk16_asm import assemble
from atk16_emu import Machine
from test.utils import pad_bytearray, assemble_and_run_until_halted

def test_bump_reset():
  machine = assemble_and_run_until_halted(
    "test/e2e/std_bump_alloc/run_bump_reset.atk16"
  )

  heap_next_p = 0xE7EF
  heap_next = machine.mem_read(heap_next_p)
  assert heap_next == heap_next_p - 1

def test_bump_alloc():
  machine = assemble_and_run_until_halted(
    "test/e2e/std_bump_alloc/run_bump_alloc.atk16"
  )

  heap_next_p = 0xE7EF
  heap_next = machine.mem_read(heap_next_p)
  assert heap_next == heap_next_p - 1 - 5

  returned_value = machine.rg.value
  assert returned_value == heap_next + 1
