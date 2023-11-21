
DRIVERS:= drivers/console.inc drivers/disk.inc

.PHONY: all clean qemu
all: mbr/mbr bootstrap/bootstrap
	@cat mbr/mbr bootstrap/bootstrap > hdd.img
	
mbr/mbr: mbr/mbr.S mbr/a20.inc $(DRIVERS)
	@nasm -fbin -o mbr/mbr mbr/mbr.S

bootstrap/bootstrap: bootstrap/bootstrap.S bootstrap/gdt.inc $(DRIVERS)
	@nasm -fbin -o bootstrap/bootstrap bootstrap/bootstrap.S
qemu:
	@qemu-system-x86_64 -hda hdd.img

clean:
	@rm mbr/mbr bootstrap/bootstrap hdd.img
	
