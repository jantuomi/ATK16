.PHONY: all

py = /usr/bin/env python3
digital_path = $(HOME)/.local/share/Digital/digital.jar

start-digital:
	java -jar $(digital_path) &

# make assemble asm=src/program.atk16 out=out/program.bin
assemble:
	$(py) atk16-asm/assembler.py $(asm) $(out)

# make gen-ucode out=out/ucode.bin
gen-ucode:
	$(py) atk16-utils/ucode.py $(out)

# make gen-charmem out=out/charmem.bin
gen-charmem:
	$(py) atk16-utils/charmem.py $(out)

# make convert-ttf in=font.ttf out=charset.txt
convert-ttf:
	$(py) atk16-utils/convert_ttf.py $(in) $(out)

# make dig-install bin=out/program.bin dig=digital/atk16_mem.dig label=MEM_ROM
dig-install:
	$(py) atk16-utils/dig_install.py $(bin) $(dig) $(label)

# make ast-compile in=asm/test_py_src.py out=asm/test_py_src.atk16
ast-compile:
	$(py) atk16-ast-walking-compiler/compiler.py $(in) $(out)

# make bytecode-compile in=asm/test_py_src.py out=asm/test_py_src.atk16
bytecode-compile:
	$(py) atk16-bytecode-compiler/compiler.py $(in) $(out)

# make test
test:
	$(py) atk16-asm/test_alu.py digital-diagrams/atk16_alu.dig

run-single-test:
	java -cp $(digital_path) CLI test -verbose -circ $(circ) -tests $(tests)
