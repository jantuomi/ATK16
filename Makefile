.PHONY: all test install

py = /usr/bin/env python3
digital_path = $(HOME)/.local/share/Digital/digital.jar
PREFIX=/usr/local

start-digital:
	java -jar $(digital_path) &

# make assemble asm=src/program.atk16 out=out/program.bin
assemble:
	$(py) atk16_asm/assembler.py $(asm) $(out)

# make gen-ucode out=out/ucode.bin
gen-ucode:
	$(py) atk16_utils/ucode.py $(out)

# make gen-charmem out=out/charmem.bin
gen-charmem:
	$(py) atk16_utils/charmem.py $(out)

# make convert-ttf in=font.ttf out=charset.txt
convert-ttf:
	$(py) atk16_utils/convert_ttf.py $(in) $(out)

# make dig-install bin=out/program.bin dig=digital/atk16_mem.dig label=MEM_ROM
dig-install:
	$(py) atk16_utils/dig_install.py $(bin) $(dig) $(label)

# make ast-compile in=asm/test_py_src.py out=asm/test_py_src.atk16
ast-compile:
	$(py) atk16_ast_walking_compiler/compiler.py $(in) $(out)

# make bytecode-compile in=asm/test_py_src.py out=asm/test_py_src.atk16
bytecode-compile:
	$(py) atk16_bytecode_compiler/compiler.py $(in) $(out)

# make emu in=rom.bin
emu:
	$(py) -m atk16_emu.cli $(in)

# make test-digital-alu
test-digital-alu:
	$(py) atk16_asm/test_alu.py digital_diagrams/atk16_alu.dig

# make test
test:
	pytest --ignore resources

install:
	mkdir -p "$(PREFIX)/share/atk16/asm"
	cp atk16_asm/*.py "$(PREFIX)/share/atk16/asm/"
	printf "#!/bin/sh\ncd \"$(PREFIX)/share/atk16\"\n/usr/bin/env python -m asm.assembler \$$@\n" > "$(PREFIX)/bin/atk16c"
	chmod +x "$(PREFIX)/bin/atk16c"

run-single-test:
	java -cp $(digital_path) CLI test -verbose -circ $(circ) -tests $(tests)
