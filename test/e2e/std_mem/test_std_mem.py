from test.utils import assemble_and_run_until_halted

def test_memset():
  machine = assemble_and_run_until_halted(
    "test/e2e/std_mem/run_memset.atk16"
  )

  addr = 0x9000 - 0x8000
  n = 5
  value = 3
  assert all([x == value for x in machine.ram.memory[addr:addr+n]])

def test_memcopy():
  machine = assemble_and_run_until_halted(
    "test/e2e/std_mem/run_memcopy.atk16"
  )

  addr_from = 0x9000 - 0x8000
  addr_to = 0x9100 - 0x8000
  n = 5

  mem_range_from = machine.ram.memory[addr_from:addr_from+n]
  mem_range_to = machine.ram.memory[addr_to:addr_to+n]

  assert [mem_range_from[i] == mem_range_to[i] for i in range(n)]
