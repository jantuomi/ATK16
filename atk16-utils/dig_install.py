#!/usr/bin/env python3
# Install a generated .bin file into a memory chip in a .dig circuit file

import sys
from typing import cast
import xml.etree.ElementTree as ET

if len(sys.argv) != 4:
  print("usage: dig_install.py <infile.bin> <circuitfile.dig> <chip_label>")
  sys.exit(1)

infile = sys.argv[1]
circuitfile = sys.argv[2]
chip_label = sys.argv[3]

with open(infile, "rb") as f:
   program_bytes = f.read()

i = 0
program_words: list[str] = []
while i < len(program_bytes):
   hi_byte = program_bytes[i]
   lo_byte = program_bytes[i + 1]
   word = f"{hi_byte:>02x}{lo_byte:>02x}"
   #print(word)
   program_words.append(word)
   i += 2

program_data = ",".join(program_words)
print("program_data:", program_data)

tree = ET.parse(circuitfile)
root = tree.getroot()

# Iterate through each 'visualElement' element
def find_data_element(root: ET.Element) -> ET.Element:
  is_correct_visual_element = False
  for visual_element in root.findall('.//visualElement'):
      element_attributes = visual_element.find('elementAttributes')
      if element_attributes is None: continue

      entries = list(element_attributes)
      for entry in entries:
          strings = entry.findall("string")
          is_label_entry = len(strings) == 2 and strings[0].text == "Label" and strings[1].text == chip_label
          if is_label_entry and not is_correct_visual_element:
            is_correct_visual_element = True
            continue

          is_data_entry = len(strings) == 1 and strings[0].text == "Data"
          if is_data_entry and is_correct_visual_element:
            data_element = entry.findall("data")[0]
            return data_element

  raise Exception(f"No data entry matching label {chip_label} found in tree")

data_element = find_data_element(root)
data_element.text = program_data

tree.write(circuitfile)