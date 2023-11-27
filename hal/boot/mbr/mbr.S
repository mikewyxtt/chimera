ORG 0x7C00			; Address in memory where  BIOS loads us at
BITS 16				; We start in 16 bit real mode

	JMP	START		; FAT32 puts a bunch of shit in the beginning of the MBR, so we need
	NOP			; to jump over it. JMP and NOP are standard, and total 3 bytes in length.

;TIMES 	90 DB 0x00		; BIOS parameter block + Fat32 extended boot record is 90 bytes.
				; We are filling with 0s so nasm doesn't put code here.

;; The computer has just been powered on, BIOS did all the stuff it needs
;; to do to get the computer to a running state and loaded this file into RAM.
;; Because of backwards compatitbility, the system is in the state it would have
;; been in back in the 80's. The rest of this code is us getting it into a more
;; useful state and eventually loading up the kernel and servers.
;;
;; Execution begins here:
START:
	CLI			; Disable interrupts before we do anything else

	;; Setup the stack
	MOV	AX, 0		; Set AX to 0, presumably bc you cant directly assign integers to those registers
	MOV	DS, AX		; ?
	MOV	ES, AX		; ?
	MOV	SS, AX		; Setup stack segment
	MOV	SP, 0x7C00	; Stack grows downwards from 0x7C00
	
	MOV	AH, 0x00	; Tell BIOS we want to set video mode
	MOV	AL, 0x02	; 720x480 resolution
	INT	0x10

	MOV 	SI, WelcomeMsg	; Move welcome message into SI
	CALL 	PRINTLN		; Display welcome message
	hlt
	
;	CALL 	ENABLE_A20	; Enable A20 gate
;	CALL	SETUP_GDT	; Setup GDT
	
	;; We are now going to use the BIOS to read the second sector from the disk.
	;; (we are currently on this first sector)
;	MOV	AL, 1		; Number of sectors to read, we want to read 1 sector (512 bytes)
;	MOV	CL, 0x2		; Cylinder to read from
;	MOV	DH, 0x0		; Head to read from
;	MOV	BX, 0x9C00	; Memory buffer
;	CALL	READ_DISK	; Disk read routine

;	JMP	0x0000:0x9C00	; Jump to the loader (Segment:Address)

	HLT			; Halt CPU. This should never execute.

LOAD_LOADER:
	


;;
;; FUNCTION:
;;	PROTECTED_MODE()
;; PURPOSE:
;; 	SETUP_GDT() returns here. We are now in protected mode and can finish up by jumping to the loader
;;
BITS 32
PROTECTED_MODE:
	HLT



;;
;; Data Section
;;
WelcomeMsg:			DB "Chimera MBR entered.", 0
;LoaderReadFailMsg:		DB "Failed to read disk."
;LoaderFilename:			DB "LOADER"

;;
;; Include Section
;;
;%INCLUDE "mbr/a20.inc"		; A20 gate setup code
;%INCLUDE "mbr/gdt.inc"		; GDT setup code
%INCLUDE "mbr/vesa.inc"		; Basic VESA driver
;%INCLUDE "mbr/disk.inc"		; Basic disk driver


;; Special BIOS stuff ;
TIMES 	510 - ($ - $$) DB 0	; BIOS expects bootsector to be 512 bytes, so we pad the file with 0's
DW 	0xAA55			; Magic number that tells the BIOS this is an executable block of code

