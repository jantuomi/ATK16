from dataclasses import dataclass
import os.path
from typing import Callable
from .asm_ops import *
from .asm_eval import *
from .tokenizer import *

@dataclass
class Result0Line:
  line_num: int
  src_file: str
  line: str

@dataclass
class Result0:
  lines: list[Result0Line]

InsertAtDataSegment = Callable[[str, list[str], str, int], None]
IncludeModule = Callable[[str], None]

def pass_0(lines: list[str], file_name: str,
           insert_at_data_segment: InsertAtDataSegment | None = None,
           include_module: IncludeModule | None = None) -> Result0:

  file_name = file_name if file_name.endswith(".atk16") else file_name + ".atk16"
  result_lines: list[Result0Line] = []
  included_module_abs_paths: list[str] = []

  if not insert_at_data_segment:
    def _insert_at_data_segment(label: str, values: list[str], context_file_name: str, context_line_num: int):
      data_segment_index = None
      for idx, result_line in enumerate(result_lines):
        if result_line.line == "%%data_segment":
          data_segment_index = idx
          break

      if data_segment_index is None:
        raise Exception(f"Error: @data before @data_segment in {context_file_name}:{context_line_num}")

      result_lines.insert(data_segment_index + 1, Result0Line(
        src_file=context_file_name,
        line_num=context_line_num,
        line=f"@label {label}"
      ))

      offset = 2
      for value in values:
        if value[0] == "\"" and value[-1] == "\"":
          string_chars = value[1:-1]
          string_chars = string_chars.replace("\\n", "\n")
          string_chars = string_chars.replace("\\r", "\r")
          string_chars = string_chars.replace("\\t", "\t")

          length = len(string_chars)
          result_lines.insert(data_segment_index + offset, Result0Line(
            src_file=context_file_name,
            line_num=context_line_num,
            line=f"  {length}"
          ))
          offset += 1

          for char in string_chars:
            result_lines.insert(data_segment_index + offset, Result0Line(
              src_file=context_file_name,
              line_num=context_line_num,
              line=f"  {ord(char)}"
            ))
            offset += 1
        else:
          result_lines.insert(data_segment_index + offset, Result0Line(
            src_file=context_file_name,
            line_num=context_line_num,
            line=f"  ${{{value}}}"
          ))
          offset += 1

    insert_at_data_segment = _insert_at_data_segment

  if not include_module:
    def _include_module(module_abs_path: str):
      if module_abs_path in included_module_abs_paths:
        return

      with open(module_abs_path, "r") as f:
        incl_lines = f.readlines()

      incl_result0 = pass_0(incl_lines, asm_file_name, insert_at_data_segment)
      for incl_line in incl_result0.lines:
        result_lines.append(Result0Line(
          src_file=incl_line.src_file,
          line_num=incl_line.line_num,
          line=incl_line.line
        ))

      included_module_abs_paths.append(module_abs_path)

    include_module = _include_module

  for (line_num, line) in enumerate(lines):
    line_num += 1 # line numbers are 1-based

    line = line.split(";")[0].strip()
    if line == "": continue

    keyword, *args = tokenize(line)

    match keyword:
      case "@data_segment":
        data_segment_sentinel = "%%data_segment"
        result_lines.append(Result0Line(
          src_file=file_name,
          line_num=line_num,
          line=data_segment_sentinel
        ))

      case "@data":
        if len(args) < 2:
          raise Exception(f"Error: @data missing arguments in {file_name}:{line_num}")

        name = args[0]
        data = args[1:]
        insert_at_data_segment(name, data, file_name, line_num)

      case "@include":
        asm_file_name = args[0] if args[0].endswith(".atk16") else args[0] + ".atk16"
        # if starts with %, use the path relative to this source file, not the current working directory
        if asm_file_name.startswith("%"):
          path = os.path.join(os.path.dirname(__file__), "builtin_asm", asm_file_name[1:])
        # if absolute, use the absolute path
        elif os.path.isabs(asm_file_name):
          path = asm_file_name
        else:
          path = os.path.join(os.path.dirname(file_name), asm_file_name)

        include_module(path)

      case _:
        result_lines.append(Result0Line(
          src_file=file_name,
          line_num=line_num,
          line=line
        ))

  return Result0(
    lines=result_lines
  )
