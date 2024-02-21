import os

def pad_binary(file_path, target_size) -> int:
  """
  Pads a binary file with zeros until it reaches a specified size in KiB.

  :param file_path: Path to the binary file to be padded.
  :param target_size: Target size in bytes.
  """

  # Check current file size
  current_size = os.path.getsize(file_path)

  # Calculate needed padding
  padding_size = target_size - current_size

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

if __name__ == "__main__":
  import argparse

  # Setup argument parser
  parser = argparse.ArgumentParser(description='Pad a binary file with zeros until it reaches a specified size.')
  parser.add_argument('file', type=str, help='Path to the binary file to be padded.')
  parser.add_argument('--to', dest='size', type=str, help='Target size (e.g., 64K, 1M, 2G).', required=True)

  args = parser.parse_args()

  # Convert size argument to bytes
  target_size_bytes = convert_size_to_bytes(args.size)

  padded_bytes_n = pad_binary(args.file, target_size_bytes)

  if padded_bytes_n > 0:
    print(f"File has been padded with {padded_bytes_n} zeros.")
  else:
    print("File is already equal to or larger than the target size. No padding added.")
