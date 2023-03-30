# Simple top level makefile to build the entire system
RUSTC:=rustc

BASE_SYSTEM += bin/echo/echo


.PHONY: all clean

all: $(BASE_SYSTEM)
	

clean:
	rm -Rf $(BASE_SYSTEM)

bin/echo/echo : bin/echo/echo.rs

% : %.rs
	@echo "[RUSTC]\t" $<
	$(RUSTC) -o $@ $<