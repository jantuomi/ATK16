# TODO: actual imports, not just `import atk16`
from atk16 import *

GRAPHICS_NO_MODE: ConstWord16     = 0
GRAPHICS_TEXT_MODE: ConstWord16   = 1
GRAPHICS_SPRITE_MODE: ConstWord16 = 2

GRAPHICS_MODE_PP: ConstWord16 = 0x17
TEXT_MEM_PP: ConstWord16      = 0x19

TEXT_MEM_PP: ConstWord16      = 0x19

# asm(
# "@label keyboard_isr"
# "  spu RA"

# "  spo RA"
# )

# set_isr(0, "keyboard_isr")

graphics_mode_p = load(GRAPHICS_MODE_PP)
text_mem_p      = load(TEXT_MEM_PP)

store(graphics_mode_p, GRAPHICS_TEXT_MODE)

def nth_letter(n: int) -> int:
    return ord('A') + n

i = 0
while i < 26:
    store(text_mem_p + i, nth_letter(i))
    i += 1

while True:
    pass
