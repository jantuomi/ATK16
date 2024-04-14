from test.utils import assemble_and_run_until_halted

def test_fibo():
  machine = assemble_and_run_until_halted(
    "test/e2e/fibo/fibo.atk16"
  )

  assert machine.ra.value == 41443
