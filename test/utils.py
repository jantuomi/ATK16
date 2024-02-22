def pad_bytearray(ba: bytearray, to_length: int = 64 * 1024) -> bytearray:
  """
  Pads a bytearray with zeros until it reaches a specified length.

  :param to_length: Target length in bytes.
  :param ba: The bytearray to be padded.
  :return: The padded bytearray.
  """
  padding_size = to_length - len(ba)
  if padding_size > 0:
    ba.extend(b'\x00' * padding_size)
  return ba

def make_rom(words: list[int]) -> bytearray:
  """
  Constructs a ROM image from a list of 16-bit words.

  :param words: The list of 16-bit words.
  :return: The ROM image as a bytearray.
  """
  bytes = []
  for word in words:
    bytes.append((word >> 8) & 0xFF)
    bytes.append(word & 0xFF)
  return pad_bytearray(bytearray(bytes))
