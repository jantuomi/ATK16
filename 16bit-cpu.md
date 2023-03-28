# 16-bit CPU design doc

## Features
16-bit data
16-bit address width
16-bit instruction width

MISC
4-bit opcodes

instruction format
xxxx xxxx xxxx xxxx - xxxx xxxx xxxx xxxx

General purpose registers:

    000: Zero register
    001: RA
    010: RB
    011: RC
    100: RD

Special registers: MAR (memory address), IR (instruction), FR (flag)

Serial peripheral ports:

    101: PA
    110: PB

for communicating with the outside world.

Expansion port that connects directly to the main bus

Addressing mode: indirect absolute (addresses are stored in registers and point to a complete address)

## Operations

### Memory operations
Format: [ 4 opcode . 3 rt . 3 rx . 6 unused ]

    ldr [rx] [rt]   # load value from memory at address (register value) rx to rt
    str [rx] [rt]   # store value in rt to memory address (register value) rx

### Immediate operations
Format: [ 4 opcode . 3 reg . 9 imm ]

    ldi [rt] [imm]          # load immediate value imm to rt

### Nullary operations
Format: [ 4 opcode . 12 unused ]

    hlt              # halt

### Arithmetic and logic operations
Format [ 4 opcode . 3 rt . 3 rl . 3 rr . 3 alu S ]

    alu [rl] [rr] [rt] [S] # compute rl ? rr where ? is the operation defined by
                           # 0SSS in the ALU, and store result in rt
                           # see datasheet of 74382 and ALU impl
    als [rl] [rr] [rt] [S] # compute rl ? rr where ? is the operation defined by
                           # 1SSS in the ALU, and store result in rt
                           # see datasheet of 74382 and ALU impl

Note: mul, div, gt, lt, eq, neq are implemented as pseudoinstructions in the assembler

### Jump
Format: [ 4 opcode . 3 rx . 9 unused ]

    jmp [rx] # Jump to address in register rx

### Branch
Format: [ 4 opcode . 1 unused . 2 flag . 9 addr ]  
Branch if flag is set (where flag 0b00 = Z, 0b01 = C, ...)

    br [flag] [addr]

Flags:
- C = ALU carry bit
- O = ALU overflow bit
- Z = ALU zero bit
- S = ALU sign bit

Interrupt lines: 1 (IRQ)
