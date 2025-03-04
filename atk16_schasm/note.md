# note about loads & stores

the indirection count tells the CPU how many times to dereference the address before reading or writing the value. in loads, the value can range from 0 to 3, and in stores, the value can range from 1 to 3. the indirection count is 0-indexed, so a value of 0 means no indirection, 1 means one level of indirection, and so on.

    ;; asm
    (ld R0 (u16 #x1234) indirect: 0)
    ;; equivalent pseudocode
    R0 := 0x1234

    ;; asm
    (ld R0 (u16 #x1234) indirect: 2)
    ;; equivalent pseudocode
    R0 := **(0x1234)

    ;; asm
    (ld R0 R1)
    ;; equivalent pseudocode
    R0 := R1

    ;; asm
    (ld R0 R1 indirect: 3)
    ;; equivalent pseudocode
    R0 := ***R1

    ;; asm
    (st R0 (u16 #x1234) indirect: 2)
    ;; equivalent pseudocode
    **(0x1234) := R0

    ;; asm
    (st R0 R1 indirect: 1)
    ;; equivalent pseudocode
    *R1 := R0

a load with an indirection count of 0 in reg mode is a `mov`.
