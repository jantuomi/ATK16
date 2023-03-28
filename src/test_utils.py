import random
from dataclasses import dataclass
import tempfile
import sys
import os

def make_test_file(name: str, table: str) -> str:
  return f"""<?xml version="1.0" encoding="utf-8"?>
<circuit>
  <version>2</version>
  <attributes/>
  <visualElements>
    <visualElement>
      <elementName>Testcase</elementName>
      <elementAttributes>
        <entry>
          <string>Label</string>
          <string>{name}</string>
        </entry>
        <entry>
          <string>Testdata</string>
          <testData>
            <dataString>{table}</dataString>
          </testData>
        </entry>
      </elementAttributes>
      <pos x="200" y="200"/>
    </visualElement>
  </visualElements>
  <wires/>
  <measurementOrdering/>
</circuit>
  """

@dataclass
class Param:
  name: str
  min: int
  max: int

class TestGroup:
  def __init__(self, inputs: list[Param], outputs: list[str]):
    self.inputs = inputs
    self.outputs = outputs
    self.cases = []

  def generate_cases(self, n = 1000):
    for _ in range(n):
      result = {}
      for param in self.inputs:
        val = random.randrange(param.min, param.max + 1)
        result[param.name] = val

      self.cases.append(result)

  def process(self):
    raise NotImplemented()

  def to_table(self) -> str:
    names: list[str] = []
    for param in self.inputs:
      names.append(param.name)
    for output in self.outputs:
      names.append(output)

    ret = " ".join(names)
    ret += "\n"

    for case in self.cases:
      row_vals = []
      for param_name in names:
        row_vals.append(str(case[param_name]))

      ret += " ".join(row_vals)
      ret += "\n"

    return ret

def run_test(name: str, table: str):
  if len(sys.argv) != 2:
    print(f"usage: {sys.argv[0]} <circuit.dig>")
    sys.exit(1)

  circuit_path = sys.argv[1]
  _, test_path = tempfile.mkstemp(".dig")

  fc = make_test_file(name, table)
  with open(test_path, "w") as f:
    f.write(fc)

  stream = os.popen(f"make run-single-test circ={circuit_path} tests={test_path}")
  output = stream.read()
  print(output)
