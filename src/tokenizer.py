def tokenize(line: str) -> list[str]:
  cur: str = ""
  result: list[str] = []
  is_py_expr = False
  idx = 0
  while idx < len(line):
    c = line[idx]
    if not is_py_expr and c == "$":
      is_py_expr = True
      idx += 1
    elif is_py_expr and c == "$":
      raise Exception("Unexpected start of python expr while already parsing a python expression:\n" + line)
    elif not is_py_expr and c == "}":
      raise Exception("Unexpected end of python expr while not parsing a python expression:\n" + line)
    elif is_py_expr and c == "}":
      is_py_expr = False
      result.append(cur)
      cur = ""
    elif is_py_expr:
      cur += c
    elif c == " " or c == "\t":
      result.append(cur)
      cur = ""
    else:
      cur += c

    idx += 1

  if len(cur) > 0:
    result.append(cur)

  return [r for r in result if r != ""]