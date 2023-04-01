AS:= clang
LD:= ld.lld
LDFLAGS:= 	--oformat binary \
			--section-start=.text=0x7c00


.PHONY: all
all: mbr.bin

.PHONY: clean
clean:
	@rm -Rf entry.o mbr.bin cd.iso
	

# Assembles MBR
entry.o : entry.S
	@$(AS) -c entry.S -o entry.o --target=i386-unknown-linux-gnu


# Links MBR into flat binary
mbr.bin : entry.o
	@$(LD) -o mbr.bin -e init entry.o $(LDFLAGS)


## Creates ISO image
.PHONY: mkimage
mkimage: all
# Create dir for ISO files
	@mkdir -p ISO

# Make bootable 14.4MB floppy disk image(full size would be 2880 sectors but we use 2876 to account for 4 sector MBR)
	@dd if=mbr.bin of=bootdisk.img bs=512 count=1
	@dd if=/dev/zero of=bootdisk.img bs=512 count=2876 seek=1
	@cp bootdisk.img ISO

# Create the ISO, delete temporary files
	@mkisofs -o chimera.iso -V Chimera -b bootdisk.img ISO
	@rm -Rf ISO floppy.img bootdisk.img
