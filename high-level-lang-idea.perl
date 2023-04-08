# Builtin types:
#   u8, u16, i8, i16, block[T], void

# allocate 16 words, returning block
# a block is similar to a C pointer but contains the allocated size as well
decl v :: block[u16] = alloc-words 16

# define a fn that zeroes a block of memory
decl zero-block :: block[u16] -> void
func zero-block block
  # for each address in block
  for-addr addr block
    # store the value 0 in memory at address addr
    store addr 0

# call fn zero-block in inlined mode
zero-block% v

# compute first 16 fibonacci numbers
decl a :: u16 = 0
decl b :: u16 = 1
for-index idx block
  # declarations in loop constructs are hoisted
  decl addr :: u16 = addr-at idx block
  decl v :: u16

  if idx == 0
    v = a
  else if idx == 1
    v = b
  else
    v = a + b

  store addr v
  a = b
  b = v
