#!/usr/bin/env python3
# Generate .bin file with ATK16 CPU TPU charmem data

# Addressed by _CCC CCCC CYYY
# Data width 8bit: ___X XXXX
# where _ = unused, C = 8bit character code, Y = character y coordinate, X = pixel value

import sys

if len(sys.argv) != 2:
  print("usage: charmem.py <outfile.bin>")
  sys.exit(1)

outfile_path = sys.argv[1]

with open("charset.txt", "r") as f:
  lines = [s.strip("\n") for s in f.readlines()]

header_chars = lines[0]

charset: list[list[str]] = []
CHAR_HEIGHT_BITS = 3
CHAR_HEIGHT = 2 ** CHAR_HEIGHT_BITS
CHAR_WIDTH = 8

i = CHAR_HEIGHT
while i < len(lines):
  char = lines[i:i+CHAR_HEIGHT]
  for row in char:
    assert len(row) == CHAR_WIDTH
  charset.append(char)
  i += CHAR_HEIGHT

CHAR_N = len(charset)

TOTAL_BYTEARRAY_SIZE = CHAR_N * CHAR_HEIGHT

def string_to_byte(input_string: str):
    # Replace spaces with '0' and '#' with '1'
    binary_string = input_string.replace(' ', '0').replace('█', '1')

    assert(len(input_string) == CHAR_WIDTH)

    # Check if the string contains only '0' or '1'
    if not all(c in '01' for c in binary_string):
        raise ValueError("Input string must contain only spaces and '█' characters.")

    # Convert the binary string to an unsigned integer byte
    return int(binary_string[::-1], 2)


res_b = bytearray(TOTAL_BYTEARRAY_SIZE)
for cn, char in enumerate(charset):
  for cy in range(0, CHAR_HEIGHT):
    addr = (cn << 3) + cy
    byte = string_to_byte(char[cy])
    print(f"{byte:>08b}")
    res_b[addr] = byte

with open(outfile_path, "wb") as f:
  f.write(res_b)
