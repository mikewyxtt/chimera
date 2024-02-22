.PHONY: all clean install

all:
	nasm -f elf32 start.asm -o start.o
	clang -c initializer.c -o initializer.o -ffreestanding -O2 -Wall -Wextra -target i386 -masm=intel
	ld -m elf_i386 -nostdlib -Ttext=0x1000 start.o initializer.o -o initializer

clean:
	rm start.o initializer initializer.o

install:
	cp initializer $(CHIMERA_SRC_ROOT)/iso/boot/initializer
