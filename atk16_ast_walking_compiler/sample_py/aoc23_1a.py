import atk16

GRAPHICS_NO_MODE: atk16.ConstWord16     = 0
GRAPHICS_TEXT_MODE: atk16.ConstWord16   = 1
GRAPHICS_SPRITE_MODE: atk16.ConstWord16 = 2

GRAPHICS_MODE_PP: atk16.ConstWord16 = 0x17
TEXT_MEM_PP: atk16.ConstWord16      = 0x19

graphics_mode_p = atk16.load(GRAPHICS_MODE_PP)
text_mem_p      = atk16.load(TEXT_MEM_PP)

atk16.store(graphics_mode_p, GRAPHICS_TEXT_MODE)

INPUT_P: atk16.ConstWord16 = 0x9000
INPUT_ELEM_SIZE: atk16.ConstWord16 = 256

# 1a example

atk16.store(INPUT_P + 0,   "1")
atk16.store(INPUT_P + 1,   "a")
atk16.store(INPUT_P + 2,   "b")
atk16.store(INPUT_P + 3,   "c")
atk16.store(INPUT_P + 4,   "2")
atk16.store(INPUT_P + 5,   "\0")
# atk16.store_const_vec(INPUT_P + 0,   "1abc2\0")
# atk16.store_const_vec(INPUT_P + 256, "pqr3stu8vwx\0")
# atk16.store_const_vec(INPUT_P + 512, "a1b2c3d4e5f\0")
# atk16.store_const_vec(INPUT_P + 768, "treb7uchet\0")

row_p = INPUT_P
i = 0

first = atk16.load(row_p)
last = 0

while True:
    char_p = INPUT_P + i
    char = atk16.load(char_p)

    if char == "\0":
        break

    last = char
    i += 1

atk16.store(text_mem_p + 0, first)
atk16.store(text_mem_p + 1, last)