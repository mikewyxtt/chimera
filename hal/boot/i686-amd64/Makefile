
MBR_SRCS:= mbr/mbr.S mbr/a20.inc mbr/gdt.inc mbr/vesa.inc

.PHONY: all clean qemu image
all: bios/bootmgr bios/mbr

bios/bootmgr: bios/bootmgr.asm
	@nasm -f bin -o bios/bootmgr bios/bootmgr.asm

bios/mbr: bios/mbr.asm
	@nasm -f bin -o bios/mbr bios/mbr.asm


#image:
#	# Create disk image
#	@mkdosfs -F 16 -C hdd.img 144000
	

#	# Install MBR to the image, being careful not to touch GPT partition data
#	@dd if=bios/bootmgr of=hdd.img conv=notrunc bs=1 count=432
#	@dd if=bios/bootmgr of=hdd.img conv=notrunc bs=1 count=2 skip=510 seek=510
	
#	@dd if=bios/mbr of=hdd.img conv=notrunc bs=512 count=2 seek=1050624	

#	# Copy loader and other files to the image
#	@mcopy -i hdd.img kernel.bin ::


qemu:
	@qemu-system-x86_64 -hda hdd.img

clean:
	@rm -f bios/mbr bios/bootmgr
	
