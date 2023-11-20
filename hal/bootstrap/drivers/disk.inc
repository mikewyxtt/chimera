;; Functions to help us read from a disk
bits 16

;;
;; Function: 
;;	hdd_read()
;;
;; Arguments: 
;;	DH: Number of sectors to read
;;	CL: Sector to start from
;;	BX: memory buffer address
;;
;; Purpose: Reads sectors from the primary hard disk into memory
;;
hdd_read:
	mov     ah, 0x2         ; Tell BIOS we want to read from the disk
	mov     al, dl          ; Number of sectors to read
	mov     dh, 0x0         ; Head to read from
	mov     dl, 0x80        ; Driver number, 0x80 is the primary disk or "C:" drive
	int     0x13            ; BIOS interrupt, begin reading from disk


