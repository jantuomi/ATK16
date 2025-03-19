#!/usr/bin/env python

def gen(name, message):
    for line in message.split('\n'):
        print(f'// {line}')

    print(f"`define LEN_{name} {len(message)}")
    print(f"`define STR_{name} \\")
    print(f"    reg [7:0] str_{name} [0:`LEN_{name}-1]; \\")
    print(f"    initial begin \\")
    for i, c in enumerate(message):
        print(f"        str_{name}[{i}] = 8'd{ord(c)}; \\")

    print(f"    end")
    print("")

gen("welcome", "Welcome to the ATK16 serial programmer. Press 'h' for help.\n")
gen("help",
    "Help:\n" +
    "    h - help\n" +
    "    p - program\n"
)
gen("program_ready",
    "Ready to receive program data. Reset device after transmission.\n"
)
