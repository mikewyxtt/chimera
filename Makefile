RUST_LIBS := lib/chimera 
#PROGS := bin/cat bin/echo bin/hello

PROGS := bin/hello


.PHONY: all clean

all:
	@echo "Building Java libraries..."
	@for java_prog in $(RUST_LIBS); do \
		make -I $(PWD)/include -C $$java_prog all; \
	done

	@echo "Building Java programs..."
	@for java_lib in $(PROGS); do \
		make -I $(PWD)/include -C $$java_lib all; \
	done

clean:
	@echo "Cleaning build files..."
	@for java_lib in $(RUST_LIBS); do \
		make -I $(PWD)/include -C $$java_lib clean; \
	done

	@for java_prog in $(PROGS); do \
		make -I $(PWD)/include -C $$java_prog clean; \
	done

	@echo "Cleaning done."
