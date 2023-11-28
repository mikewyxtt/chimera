
.PHONY: all clean install
all: bios/bootmgr bios/mbr



bios/bootmgr: bios/bootmgr.asm
	nasm -f bin -o bios/bootmgr bios/bootmgr.asm

bios/mbr: bios/mbr.asm
	nasm -f bin -o bios/mbr bios/mbr.asm


install:
	mkdir -p ../../../tmp/rootfs/boot
	cp loader/loader ../../../tmp/rootfs/boot/loader


clean:
	rm -f bios/mbr bios/bootmgr
	
