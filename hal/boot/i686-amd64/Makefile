TARGETS:= 	bios/bootmanager \
	  	bios/preloader \
		loader/src/bios/start.o

.PHONY: all clean install
all: $(TARGETS)


bios/bootmanager: bios/bootmanager.asm
	nasm -f bin -o $@ $<

bios/preloader: bios/preloader.asm
	nasm -f bin -o $@ $<

## Loader entry point for BIOS systems is written in assembly
loader/src/bios/start.o: loader/src/bios/start.asm
	nasm -f elf32 -o $@ $<

## UEFI version of loader
#loader/loader.efi: loader/uefi_entry.asm
#	nasm -f win32 -o loader/uefi_entry.o $<
#	lld-link /subsystem:efi_application /entry:START /base:0x100000 /out:$@ loader/uefi_entry.o

install:
#	chmod +x $(TARGETS)
	mkdir -p ../../../tmp/rootfs/boot
	cp bios/preloader ../../../tmp/rootfs/boot/preloader
	cp loader/loader ../../../tmp/rootfs/boot/loader
#	cp loader/loader.efi ../../../tmp/efi_sys/BOOT/EFI/BOOTX32.EFI


clean:
	rm -f $(TARGETS)
