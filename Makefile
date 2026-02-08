FC = gfortran
FFLAGS = -O2 -Wall -Wextra -std=f2008 -ffree-line-length-none
BIN = bin/gs-budget-variance-monitor
SRC = src/main.f90

.PHONY: build test clean fmt

all: build

build: $(BIN)

$(BIN): $(SRC)
	$(FC) $(FFLAGS) -o $@ $<

test: build
	./tests/run_tests.sh

clean:
	rm -f $(BIN) out/test_output.txt out/last_run.sql

fmt:
	@echo "No formatter configured for Fortran."
