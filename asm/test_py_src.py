import atk16

def func(arg1: int, arg2: int) -> int:
    a = arg1
    return a + a

b = func(1, 3)

# TEXT_MODE: atk16.ConstInt          = 1
# GRAPHICS_MODE_ADDR: atk16.ConstInt = 0x17

# atk16.store(GRAPHICS_MODE_ADDR, TEXT_MODE)
# atk16.store(0xF800, 'H')
# atk16.store(0xF801, 'E')
# atk16.store(0xF802, 'L')
# atk16.store(0xF803, 'L')
# atk16.store(0xF804, 'O')

# a = atk16.call_inline(
#   atk16.load(0xF000)
# )

# atk16.asm("ldi 1 RA")
