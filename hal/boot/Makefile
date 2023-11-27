
DRIVERS:= drivers/console.inc drivers/disk.inc
MBR_SRCS:= mbr/mbr.S mbr/a20.inc mbr/gdt.inc mbr/vesa.inc

.PHONY: all clean qemu image
all: bios/boot_manager/boot_manager mbr/mbr
#	@cat mbr/mbr > hdd.img

bios/boot_manager/boot_manager: bios/boot_manager/boot_manager.asm
	@nasm -f bin -o bios/boot_manager/boot_manager bios/boot_manager/boot_manager.asm

mbr/mbr: $(MBR_SRCS)
	@nasm -fbin -o mbr/mbr mbr/mbr.S

image:
#	# Create disk image
#	@mkdosfs -F 16 -C hdd.img 144000
	

#	# Install MBR to the image, being careful not to touch GPT partition data
	@dd if=bios/boot_manager/boot_manager of=hdd.img conv=notrunc bs=1 count=432
	@dd if=bios/boot_manager/boot_manager of=hdd.img conv=notrunc bs=1 count=2 skip=510 seek=510
	
	@dd if=mbr/mbr of=hdd.img conv=notrunc bs=512 count=1 seek=1050624	

#	# Copy loader and other files to the image
#	@mcopy -i hdd.img kernel.bin ::


qemu:
	@qemu-system-x86_64 -hda hdd.img

clean:
	@rm -f mbr/mbr bios/boot_manager/boot_manager
	
