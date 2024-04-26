EMULATOR = atk16emu
ASSEMBLER = atk16c
PROJ = monitor

out/$(PROJ).bin: $(PROJ).atk16
	$(ASSEMBLER) $< -o out/$(PROJ).bin

###

@PHONY: run
run: out/$(PROJ).bin
	$(EMULATOR) $<

@PHONY: debug
debug: out/$(PROJ).bin
	$(EMULATOR) -d $<
