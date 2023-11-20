;; Functions to help us read from a disk
bits 16

;;
;; Function: hdd_read(dh: number of sectors to read, cl: sector to start from
;;			bx: memory buffer address) 
;; Purpose: Reads sectors from the primary hard disk
;;
hdd_read:
	mov     ah, 0x2         ; Tell BIOS we want to read from the disk
	mov     al, dl          ; Number of sectors to read
;	mov     cl, 0x2         ; Cylinder to read from
;	and     cl, 0xff        ; We need to & the cylinder w 0xff for some reason ?
	mov     dh, 0x0         ; Head to read from
;	mov     bx, 0x9C00      ; Memory buffer
	mov     dl, 0x80        ; Driver number, 0x80 is the primary disk or "C:" drive
	int     0x13            ; BIOS interrupt, begin reading from disk

; bits 32

