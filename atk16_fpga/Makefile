BIN ?= program.bin

hardware.bin: top.v
	apio verify
	apio test
	apio build

.PHONY: hw
hw: hardware.bin
	apio upload

.PHONY: sw
sw: $(BIN)
	iceprogduino -w -o 1M $(BIN)

.PHONY: graph
graph:
	yosys -f verilog -p "show -format dot -colors 1 -prefix hardware top" -q top.v
	dot -Tsvg hardware.dot -o hardware.svg
	open -a LibreWolf hardware.svg

.PHONY: clean
clean:
	apio clean
