.PHONY: all test start-digital

py = /usr/bin/env python3
digital_path = $(HOME)/.local/share/Digital/digital.jar
PREFIX=/usr/local

all:
	@echo "See Makefile for available commands"
	exit 1

# make start-digital
start-digital:
	java -jar $(digital_path) &

# make test
test:
	pytest --ignore resources

# DEPRECATED: focusing on emulator E2E tests for now instead of sim model tests
#run-single-test:
#	java -cp $(digital_path) CLI test -verbose -circ $(circ) -tests $(tests)
