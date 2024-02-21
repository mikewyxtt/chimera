.PHONY: all clean install

all:
	nasm -f elf32 start.asm -o start.o
	@ld -m elf_i386 -nostdlib -Ttext=0x1000 start.o -o initializer

install:
	@cp initializer /home/mike/chimera/iso/boot/initializer
