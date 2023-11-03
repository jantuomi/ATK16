def gen_charset(file = "charset.txt"):
  with open(file, "r") as f:
    lines = [s.strip("\n") for s in f.readlines()]

  result: list[list[str]] = []
  i = 0
  while i < len(lines):
    char = lines[i:i+8]
    result.append(char)
    i += 8

  print(result)

gen_charset()
