.PHONY: all clean qemu
all: mbr/mbr
	
mbr/mbr: mbr/mbr.S mbr/a20.inc mbr/gdt.inc drivers/console.inc
	@nasm -fbin -o mbr/mbr mbr/mbr.S

qemu:
	qemu-system-x86_64 -hda mbr/mbr

clean:
	@rm mbr/mbr
