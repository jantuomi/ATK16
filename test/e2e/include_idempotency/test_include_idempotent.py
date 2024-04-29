import os.path
from test.utils import assemble_and_run_until_halted

def test_include_idempotent():
  machine1 = assemble_and_run_until_halted(
    os.path.join(os.path.dirname(__file__), "p1.atk16")
  )

  machine2 = assemble_and_run_until_halted(
    os.path.join(os.path.dirname(__file__), "p2.atk16")
  )

  for i in range(len(machine1.rom.memory)):
    assert machine1.rom.memory[i] == machine2.rom.memory[i]
