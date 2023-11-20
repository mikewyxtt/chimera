.PHONY: all clean qemu
all: loader.bin
	
loader.bin: loader.S
	@nasm -fbin -o loader.bin loader.S

qemu:
	qemu-system-x86_64 -hda loader.bin

clean:
	@rm a.out hello.o
