
DRIVERS:= drivers/console.inc drivers/disk.inc
MBR_SRCS:= mbr/mbr.S mbr/a20.inc mbr/gdt.inc mbr/vesa.inc

.PHONY: all clean qemu image
all: mbr/mbr
#	@cat mbr/mbr > hdd.img
	
mbr/mbr: $(MBR_SRCS)
	@nasm -fbin -o mbr/mbr mbr/mbr.S

image:
#	# Create disk image
	@mkdosfs -C hdd.img 1440
	

#	# Install MBR to the image, being careful not to touch BIOS parameter block
	@dd if=mbr/mbr of=hdd.img conv=notrunc bs=1 count=3
	@dd if=mbr/mbr of=hdd.img conv=notrunc bs=1 count=448 skip=62 seek=62
	@dd if=mbr/mbr of=hdd.img conv=notrunc bs=1 count=2 skip=510 seek=510
	
#	# Write magic number
#	@dd if=bootload.bin of=hdd.img conv=notrunc bs=1 count=2 skip=510 seek=510

#	# Copy loader and other files to the image
#	@mcopy -i hdd.img kernel.bin ::


qemu:
	@qemu-system-x86_64 -hda hdd.img

clean:
	@rm -f mbr/mbr bootstrap/bootstrap hdd.img
	
