ORG 0x600
BITS 16

;;
;; When the computer first turns on, the BIOS loads the first sector of the boot disk into memory at address
;; 0x7C00. This is fine, but since this is a bootmanager we could possibly be loading someone else's
;; bootloader, which is expecting to be loaded to address 0x7C00. So, when we load them they need to be
;; loaded at 0x7C00. Obviously, we are at 0x7C00 so the first thing we are going to do is relocate ourselves
;; to 0x600 instead. This way we can load any bootloader we want (Windows, Linux, etc) without issue.
;;
ENTRY:	
	CLD			; String ops inc
	XOR	AX, AX		; Use XOR to zero out AX 
	MOV	ES, AX		; Address data
	MOV	DS, AX		; ^^
	MOV	SS, AX		; Setup stack
	MOV	SP, 0x7C00	; ^^

	MOV	SI, SP		; Source address for the transfer (0x7C00)
	MOV	DI, 0x600	; Destination address for the transfer (0x0600)
	MOV	CX, 0x100	; Word count (512 bytes in this file / 2 bytes per word = 0x100)
	REP	MOVSW		; Copy SI(0x7C00) into DI(0x0600) two bytes at a time until CX reaches 0 (256 times)

	;; Jump to newly relocated code, basically MAIN was originally +0x7C00 from 0, but we moved it to 
	;; +0x600 from 0. So to find MAIN we need to subtract 0x7C00 from it to then add 0x600 to it
	JMP	MAIN - 0x7C00 + 0x600 
	

MAIN:
	;; Clear the screen by setting screen resolution.
	MOV	AH, 0x00	; Tell BIOS we want to set video mode
	MOV	AL, 0x02	; Tell BIOS we want 720x480 (will change this later)
	INT	0x10		; BIOS modeset routine
	

	;; Print welcome message
	MOV	SI, WelcomeMSG	; Put WelcomeMSG string address into SI register
	CALL	PRINTLN

	;; Load bootsector from selected partition
	MOV	ESI, 0x100800	; Block number we want to loda
	MOV	BX, 0x7C00	; Address we want to load it to
	MOV	DL, 0x80	; Disk number we want to read from
	CALL	READ_SECTOR	; Read single sector routine
	
	JMP	0x0000:0x7c00	; Jump to bootloader (Segment:Address)

	HLT			; Should never get here.




;; ESI: LBA number
;; BX: Transfer buffer

READ_SECTOR:
	MOV	[DAP.addr], BX	; Copy the buffer address into the disk address packet
	MOV	[DAP.buff], ESI	; Copy the LBA into the DAP
	MOV	AH, 0X42	; BIOS extended read function, allows us to read disks > 8GB
	MOV	SI, DAP		; Move disk address packet into SI for the BIOS to read
	INT	0x13		; Call the BIOS
	JC	.ERROR		; Fail if there was an issue loading the sector
	RET			; Otherwise go back

.ERROR: MOV	SI, DiskErrorMSG ; Load error message into SI
	CALL	PRINTLN		; Call print function
	CLI			; We don't have error handling, so disable interrupts and halt
	HLT			; so the cpu can't do anything

;;
;; Disk Address Packet for BIOS interrupt 0x13 extended read function
;;
DAP:
	DB 0x10			; size of data packet, always should be set to 0x10
	DB 0x00			; reserved
	DW 0x0001		; number of sectors to read
.addr:	DW 0x0000		; 16 bit offset(address) of target buffer
	DW 0x0000		; 16 bit segment of target buffer
.buff:	DD 0x00000000		; Lower 32 bits of 48 bit LBA
	DD 0x00000000		; Upper 32 bits of 48 bit LBA


;;
;; Function:
;;      PRINTLN()
;;
;; Arguments:
;;      SI: String to print
;;
;; Purpose:
;;      Prints a string to the screen
;;
;; Clobbers:
;;      AX
;;
PRINTLN:
	MOV	AH, 0X0E	; Tell BIOS we are going to print chars

.LOOP	LODSB			; Load single byte from SI
	OR	AL, AL		; Check if we are at the end of the string
	JZ	.NEWL		; If we are at the strings end, print newline and exit
	INT	0x10		; Call BIOS print routine
	JMP	.LOOP		; Loop until done printing

.NEWL	MOV	AL, 0XA		; Print newline
	INT	0x10		; Call BIOS print routine
	MOV	AL, 0xD		; Print carriage return
	INT	0x10		; Call BIOS print routine
	RET			; Exit print routine


WelcomeMSG:	DB "Boot Manager entered.", 0xA, 0xD, "Searching for bootloader..."
DiskErrorMSG:	DB "DISK READ ERROR"

TIMES	64	DB	0	; We can't overwrite the partition table, fill with 0s
TIMES	510 - ($ - $$) DB 0x00	; Bootloader has to be 512 bytes long, pad the missing bytes with 0s
DW 	0xAA55			; But make the last two bytes 0xAA55

