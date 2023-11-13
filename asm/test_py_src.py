import atk16

TEXT_MODE: atk16.ConstInt            = 1
GRAPHICS_MODE_ADDR_P: atk16.ConstInt = 0x17
TEXT_MEM_ADDR_P: atk16.ConstInt      = 0x19

graphics_mode_addr = atk16.load(GRAPHICS_MODE_ADDR_P)
text_mem_addr = atk16.load(TEXT_MEM_ADDR_P)

atk16.store(graphics_mode_addr, TEXT_MODE)

def nth_letter(n: int) -> int:
    return atk16.ord('A') + n

i = 0
while i < 26:
    atk16.store(text_mem_addr + i, nth_letter(i))
    i = i + 1

while True:
    pass
