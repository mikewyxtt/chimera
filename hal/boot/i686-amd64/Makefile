
.PHONY: all clean
all: bios/bootmgr bios/mbr



bios/bootmgr: bios/bootmgr.asm
	@nasm -f bin -o bios/bootmgr bios/bootmgr.asm

bios/mbr: bios/mbr.asm
	@nasm -f bin -o bios/mbr bios/mbr.asm



clean:
	@rm -f bios/mbr bios/bootmgr
	
