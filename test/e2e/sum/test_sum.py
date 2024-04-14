from test.utils import assemble_and_run_until_halted

def test_sum():
  machine = assemble_and_run_until_halted(
    "test/e2e/sum/sum.atk16"
  )

  assert machine.rb.value == 30
