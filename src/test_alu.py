#!/usr/bin/env python3

from test_utils import *

def make_alu_table() -> str:
  alu = ALU()
  n = 10000
  alu.generate_cases(n)
  alu.process()
  print(f"[test] running {n} ALU test cases")
  return alu.to_table().strip()

class ALU(TestGroup):
  def __init__(self):
    TestGroup.__init__(self, [
      Param("A", 0x0, 0xffff),
      Param("B", 0x0, 0xffff),
      Param("S", 0, 7),
    ], [
      "Y",
      "FLAGS",
    ])

  def process(self):
    new_cases = []
    for kase in self.cases:
      S = kase["S"]
      A = kase["A"]
      B = kase["B"]
      kase["FLAGS"] = "x"

      match S:
        case 0: Y = 0
        case 1: Y = (B - A) & 0xFFFF
        case 2: Y = (A - B) & 0xFFFF
        case 3: Y = (A + B) & 0xFFFF
        case 4: Y = (A ^ B) & 0xFFFF
        case 5: Y = (A | B) & 0xFFFF
        case 6: Y = (A & B) & 0xFFFF
        case 7: Y = 0xffff

      kase["Y"] = Y
      new_cases.append(kase)

    self.cases = new_cases

table = make_alu_table()
run_test("ALU", table)
