.PHONY: all

py = /usr/bin/env python3
digital_path = $(HOME)/.local/share/Digital/digital.jar

start-digital:
	java -jar $(digital_path) &

# make assemble asm=src/program.atk16 out=out/program.bin
assemble:
	$(py) src/assembler.py $(asm) $(out)

# make gen-ucode out=out/ucode.bin
gen-ucode:
	$(py) src/ucode.py $(out)

# make gen-charmem out=out/charmem.bin
gen-charmem:
	$(py) src/charmem.py $(out)

# make convert-ttf in=font.ttf out=charset.txt
convert-ttf:
	$(py) src/convert_ttf.py $(in) $(out)

# make test
test:
	$(py) src/test_alu.py digital/atk16_alu.dig

run-single-test:
	java -cp $(digital_path) CLI test -verbose -circ $(circ) -tests $(tests)
