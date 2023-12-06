TARGETS:= 	bios/bootmanager \
	  	bios/preloader \
		loader/loader.efi \
		loader/loader.bin

.PHONY: all clean install
all: $(TARGETS)


bios/bootmanager: bios/bootmanager.asm
	nasm -f bin -o $@ $<

bios/preloader: bios/preloader.asm
	nasm -f bin -o $@ $<

## UEFI version of loader
loader/loader.efi: loader/uefi/start.rs
	cargo -Z unstable-options -C loader build --target=i686-unknown-uefi
	lld-link /subsystem:efi_application /entry:start /base:0x100000 /out:$@ loader/target/i686-unknown-uefi/debug/libuefi.a loader/target/i686-unknown-uefi/debug/libloader.a /machine:x86

# BIOS version of loader
loader/loader.bin: loader/bios/start.asm
	mkdir -p loader/target/i686-unknown-linux-gnu
	nasm -f elf32 -o loader/target/i686-unknown-linux-gnu/start.o loader/bios/start.asm
	cargo -Z unstable-options -C loader build --target=i686-unknown-linux-gnu
	ld.lld -Tloader/link.lds -o loader.elf loader/target/i686-unknown-linux-gnu/start.o loader/target/i686-unknown-linux-gnu/debug/libloader.a
	objcopy -O binary --only-section=.text --only-section=.text.main loader.elf loader.bin

install:
#	chmod +x $(TARGETS)
	mkdir -p ../../../tmp/rootfs/boot
	cp bios/preloader ../../../tmp/rootfs/boot/preloader
	cp loader.bin ../../../tmp/rootfs/boot/loader
#	cp loader/loader.efi ../../../tmp/efi_sys/BOOT/EFI/BOOTX32.EFI


clean:
	rm -f $(TARGETS)
	rm -f loader/src/bios/start.o
	cargo -Z unstable-options -C loader clean
