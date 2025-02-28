## Registers

There are 16 general-purpose registers, `R0` to `R15`.

`R13` is the flag/condition register.  
`R14` is the program counter.  
`R15` is the stack pointer.

Additionally, `R12` mey be clobbered by the assembler when expanding macros.

## Instructions

XXXX denotes ignored bits.

**HLT** is a halt operation. It stops the processor.

```python
HLT
0000 XXXX XXXX XXXX
```

**ALU** is a 16-bit ALU operation.

- SSS selects the operation to perform
- LLLL is the left operand and destination register.
- In register mode (M=0), RRRR is the right operand register.
- In immediate mode (M=1), the next word is interpreted as a 16-bit immediate value.

```python
ALU
# register
0001 SSSM LLLL RRRR
# immediate
0001 SSSM LLLL XXXX
IIII IIII IIII IIII
```

**LD** is a load operation.

- LLLL is the destination register.
- In register mode (M=0), RRRR is the register holding the address to load from.
- In immediate mode (M=1), the next word is interpreted as a 16-bit address to load from.
- In direct mode (D=0), the address is used as-is.
- In indirect mode (D=1), the address is used as a pointer to another address. An indirect load can be thought of as a pointer dereference.
- In pop mode (P = 1), the RRRR register is incremented before loading. This in conjunction with an indirect load can be used as a stack pop operation. POP is only valid in register mode.

```python
LD
# register
0010 DPXM LLLL RRRR
# immediate
0010 DPXM LLLL XXXX
IIII IIII IIII IIII
```

**MOV** is a move operation. It copies the value from one register to another.

- LLLL is the destination register.
- RRRR is the source register.

```python
MOV
0011 XXXX LLLL RRRR
```

**ST** is a store operation.

- LLLL is the source register.
- In register mode (M=0), RRRR is the register holding the address to store to.
- In immediate mode (M=1), the next word is interpreted as a 16-bit address to store to.
- In direct mode (D=0), the address is used as-is.
- In indirect mode (D=1), the address is used as a pointer to another address. An indirect store can be thought of as a pointer assignment.
- In push mode (P = 1), the RRRR register is decremented after storing. This in conjunction with an indirect store can be used as a stack push operation. PUSH is only valid in register mode.

```python
ST
# register
0100 DPXM LLLL RRRR
# immediate
0100 DPXM LLLL XXXX
IIII IIII IIII IIII
```

**BR** is a branch operation.

- FF selects the condition to branch on (carry, overflow, zero, sign).
- S determines if the selected flag should be set (1) or not set (0).
- The I octet is an 8-bit signed offset.

```python
BR
0101 FFXS IIII IIII
```

## Calling convention

Arguments are passed in registers R0..R10. The return value is stored in R0.
The return address is stored on the stack to support nested calls.

Registers `R0..R3` are caller-saved (called function can clobber these registers, calling code must save them on the stack or higher registers if needed).  
Registers `R4..R10` are callee-saved (called function must save these on the stack or lower registers).

## Example assembly

```java
@at 0x0
    LD  R0 0x1
    LD  R1 0x2
    ADD R0 R1     ; ADD = macro that expands to ALU 000

    BR SIGN UNSET $br_true
br_false:
    LD  R0 0xEE
    HLT
br_true:
    LD  R0 0xFF
    HLT
```

## Example macro assembly

```java
@macro ADD lhs rhs
    ALU 0 $lhs $rhs
@endmacro

@let threshold 0x80
@if R1 < $threshold ; expands into a SUB and a BR
    LD R0 0x1
    LD R1 0x2
    ADD R0 R1
    HLT
@else
    ; something
@endif

@let PC R14
@let SP R15

@macro SPUSH reg
    ST INDIRECT PUSH $reg $SP
@endmacro

@macro SPOP reg
    LD INDIRECT POP $reg $SP
@endmacro

@macro CALL1 fn_lbl arg
@let ret_addr $gen_uniq ; generate a unique label
    LD  R0 $arg         ; load argument to R0
    LD  R1 $ret_addr    ; load return address to R1
    SPUSH R1            ; push return address to stack
    LD  $PC $fn_lbl     ; jump to function address
$ret_addr:
    HLT
@endmacro

@at 0x0
main:
    CALL1 :fn 0x1
    HLT

; example of an absolute jump
    LD  $SP
    $abs_jump_addr

; example of a relative jump
    ADD $PC
    $rel_jump_offset

; idea: syntax for passing the immediate on the same line
    ADD $PC % $rel_jump_offset

; probably should just require the core instructions to be defined "correctly" and have convenience macros for the rest, such as

@macro LOAD reg from
@c_if immediate $from
    LD $reg
    $from
$c_else
    LD $reg $from
$c_endif
@endmacro
```

### Assembler concepts

- `R0` to `R15` are register literals.
- Rows with no indentation are either directives (`@` prefix) or labels (`:` suffix).
- Rows with indentation are instructions.
- Comments are prefixed with `;`.
- Labels can be literals (`my_label:`) or variables (`$my_label_var:`).
- Compile time variables are defined with `@let` and used with `$`.
