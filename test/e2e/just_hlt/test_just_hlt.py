from test.utils import assemble_and_run_until_halted

def test_just_hlt():
  machine = assemble_and_run_until_halted(
    "test/e2e/just_hlt/just_hlt.atk16"
  )

  assert machine.pc.value == 1
