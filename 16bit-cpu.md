# 16-bit CPU design doc

## Features
16-bit data
16-bit address width
16-bit instruction width
16-bit control word
Minimal instruction set computer (MISC)
4-bit opcodes
Memory mapped I/O

instruction format
xxxx xxxx xxxx xxxx - xxxx xxxx xxxx xxxx

General purpose registers:

    RA (0) .. RH (7)

Expansion port that connects directly to the main bus

Default addressing mode: indirect absolute (addresses are stored in registers and point to a complete address). Some instructions use PC-relative addressing.

## Operations

TODO rewrite bit about operations

ALU flags:
- C = carry bit
- O = overflow bit
- Z = zero bit
- S = sign bit

Interrupt lines: ???
