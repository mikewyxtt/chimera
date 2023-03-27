JAVA_LIBS := lib/chimera 
#JAVA_PROGS := bin/cat bin/echo bin/hello

JAVA_PROGS := bin/hello


.PHONY: all clean

all:
	@echo "Building Java libraries..."
	@for java_prog in $(JAVA_LIBS); do \
		make -I $(PWD)/include -C $$java_prog all; \
	done

	@echo "Building Java programs..."
	@for java_lib in $(JAVA_PROGS); do \
		make -I $(PWD)/include -C $$java_lib all; \
	done

clean:
	@echo "Cleaning build files..."
	@for java_lib in $(JAVA_LIBS); do \
		make -I $(PWD)/include -C $$java_lib clean; \
	done

	@for java_prog in $(JAVA_PROGS); do \
		make -I $(PWD)/include -C $$java_prog clean; \
	done

	@echo "Cleaning done."
