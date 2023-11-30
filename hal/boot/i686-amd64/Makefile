TARGETS:= bios/bootmanager \
	  bios/preloader

.PHONY: all clean install
all: $(TARGETS)



bios/bootmanager: bios/bootmanager.asm
	nasm -f bin -o $@ $<

bios/preloader: bios/preloader.asm
	nasm -f bin -o $@ $<
	touch loader/loader

install:
	mkdir -p ../../../tmp/rootfs/boot
	cp bios/preloader ../../../tmp/rootfs/boot/preloader
	cp loader/loader ../../../tmp/rootfs/boot/loader


clean:
	rm -f $(TARGETS)
	
