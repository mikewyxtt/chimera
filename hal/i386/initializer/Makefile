.PHONY: all clean install

all:
	nasm -f elf32 start.asm -o start.o
	clang -c initializer.c -o initializer.o -ffreestanding -O2 -Wall -Wextra -target i386-pc-linux -masm=intel -mno-sse
	clang -c $(CHIMERA_SRC_ROOT)/hal/lib/early_log.c -o early_log.o -ffreestanding -O2 -Wall -Wextra -target i386-pc-linux -mno-sse -masm=intel
	clang -c $(CHIMERA_SRC_ROOT)/hal/lib/io.c -o io.o -ffreestanding -O2 -Wall -Wextra -target i386-pc-linux -mno-sse -masm=intel
	clang -c $(CHIMERA_SRC_ROOT)/hal/lib/string_utils.c -o string_utils.o -ffreestanding -O2 -Wall -Wextra -mno-sse -target i386-pc-linux -masm=intel
	ld -m elf_i386 -nostdlib -Ttext=0x1000 start.o initializer.o early_log.o io.o string_utils.o -o initializer

clean:
	rm start.o initializer initializer.o

install:
	cp initializer $(CHIMERA_SRC_ROOT)/iso/boot/initializer
