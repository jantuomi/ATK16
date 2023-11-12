import atk16


TEXT_MODE: atk16.ConstInt            = 1
GRAPHICS_MODE_ADDR_P: atk16.ConstInt = 0x17
TEXT_MEM_ADDR_P: atk16.ConstInt      = 0x19

graphics_mode_addr = atk16.load(GRAPHICS_MODE_ADDR_P)
text_mem_addr = atk16.load(TEXT_MEM_ADDR_P)

atk16.store(graphics_mode_addr, TEXT_MODE)


def nth_letter(n: int) -> int:
    return n + 65

i = 0
atk16.asm("ldi 0b111111111 RG")
while i < 26:
    # bug: stack frame offset for nth_letter is calculated
    # one off because text_mem_addr + i takes up one
    # stack slot
    atk16.store(text_mem_addr + i, nth_letter(i))
    #atk16.store(text_mem_addr + i, i + 65)
    i = i + 1

while True:
    pass
