import os

def pad_binary(file_path: str, target_size: int) -> int:
  """
  Pads a binary file with zeros until it reaches a specified size in KiB.

  :param file_path: Path to the binary file to be padded.
  :param target_size: Target size in bytes.
  """

  # Check current file size
  current_size = os.path.getsize(file_path)

  # Calculate needed padding
  padding_size = target_size - current_size

  if padding_size < 0:
    raise ValueError(f"File is larger than the target size ({current_size} > {target_size}).")

  # Append zeros if needed
  if padding_size > 0:
    with open(file_path, 'ab') as file:
      file.write(b'\x00' * padding_size)

  return padding_size

def convert_size_to_bytes(size_str: str) -> int:
  """
  Converts a size string with K, M, or G suffix to bytes.

  :param size_str: Size string (e.g., "64K", "1M", "2G").
  :return: Size in bytes.
  """
  size_str = size_str.upper()
  if size_str.endswith('K'):
    return int(size_str[:-1]) * 1024
  elif size_str.endswith('M'):
    return int(size_str[:-1]) * 1024 ** 2
  elif size_str.endswith('G'):
    return int(size_str[:-1]) * 1024 ** 3
  else:
      return int(size_str)
