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
	CLD							; String ops inc
	XOR	AX, AX						; Use XOR to zero out AX 
	MOV	ES, AX						; Address data
	MOV	DS, AX						; ^^
	MOV	SS, AX						; Setup stack
	MOV	SP, 0x7C00					; ^^

	MOV	SI, SP						; Source address for the transfer (0x7C00)
	MOV	DI, 0x600					; Destination address for the transfer (0x0600)
	MOV	CX, 0x100					; Word count (512 bytes in this file / 2 bytes per word = 0x100)
	REP	MOVSW						; Copy SI(0x7C00) into DI(0x0600) two bytes at a time until CX reaches 0 

	;; Jump to newly relocated code, basically MAIN was originally +0x7C00 from 0, but we moved it to 
	;; +0x600 from 0. So to find MAIN we need to subtract 0x7C00 from it to then add 0x600 to it
	JMP	MAIN - 0x7C00 + 0x600 
	

MAIN:
	;; Clear the screen by setting screen resolution.
	MOV	AH, 0x00					; Tell BIOS we want to set video mode
	MOV	AL, 0x02					; Tell BIOS we want 720x480 (will change this later)
	INT	0x10						; BIOS modeset routine
	

	;; Print welcome message
	MOV	SI, WelcomeMSG					; Put WelcomeMSG string address into SI register
	CALL	PRINTLN

	;; Find bootsector ;;
	
	;; In the future, this boot manager will give you option to select other operating
	;; systems and chainload bootloaders from different partitions. For the sake of
	;; simplicity to move things along, we will just assume the bootloader we want is
	;; on the second partition of the primary hard disk.

	;; To do so, we must load the GPT header and the partition table that follows it.
	;; These two data structures contain all the information we need to find the 
	;; first sector of the 2nd partition of the disk, which contains our second stage
	;; boot sector.

	;; Load GPT header
	MOV	ESI, 0x01					; GPT header is located in block 1
	MOV	BX, 0x800					; GPT header will reside in memory at 0x800:0x1000
	CALL	READ_SECTOR					; Read the GPT header into memory



	;; Load Partition entries
	MOV	ESI, [0x800 + GPTHEADER.TABLE_BLOCK] 		; Partition table location is listed in GPT header
	MOV	BX, 0x1000					; GPT partition table will reside in memory at 0x1000:0x1200
	CALL	READ_SECTOR					; Read the partition table entries into RAM



	;; Load the bootsector

	;; Find block number for 2nd partition on the disk
	MOV	DWORD EAX, [0x800 + GPTHEADER.ENTRY_SIZE] 	; Load partition entry size into EAX

	;; This next part looks complex but it is simple. We just figured out how many bytes
	;; are in each partion entry, now we need to find the 2nd partition. The first one is
	;; at 0x1000, so we need to seek ahead by the size of one entry (EAX) to find the 
	;; second partition.
	;;
	;; The starting block of each partition is found at offset 0x20 of each entry, thus
	;; giving us our LBA at [0x1000 + EAX + 0x20]
	MOV	DWORD ESI, [0x1000 + EAX + GPT.FIRST_BLOCK] 
	
	MOV	BX, 0x7C00					; Address we want to load it to
	CALL	READ_SECTOR					; Read single sector routine
	
	JMP	0x0000:0x7C00					; Jump to bootloader (Segment:Address)




;;
;; Function:
;;	READ_SECTOR()
;;
;; Arguments:
;; 	ESI: 	Block number to load
;; 	BX: 	Transfer buffer (Address)
;;	DL:	Disk to read from. Primary HDD is 0x80, ignore to use disk that was booted from.
;;
;; Purpose:
;;	Loads one sector from the selected disk into memeory
;;
READ_SECTOR:
	CLC							; Clear the carry flag, just in case. Not sure if needed tbh.
	MOV	DL, 0x80					; Primary hard disk, CHANGE THIS TO BOOT FROM OTHER MEDIA TYPES
	MOV	[DAP.addr], BX					; Copy the buffer address into the disk address packet
	MOV	[DAP.buff], ESI					; Copy the LBA into the DAP
	MOV	AH, 0X42					; BIOS extended read function, allows us to read disks > 8GB
	MOV	SI, DAP						; Move disk address packet into SI for the BIOS to read
	INT	0x13						; Call the BIOS
	JC	.ERROR						; Fail if there was an issue loading the sector
	RET							; Otherwise go back

.ERROR: MOV	SI, DiskErrorMSG				; Load error message into SI
	CALL	PRINTLN						; Call print function
	CLI							; We don't have error handling, so disable interrupts and halt
	HLT							; so the cpu can't do anything


;; Disk Address Packet for BIOS interrupt 0x13 extended read function
DAP:
	DB 0x10							; size of data packet, always should be set to 0x10
	DB 0x00							; reserved
	DW 0x0001						; number of sectors to read
.addr:	DW 0x0000						; 16 bit offset(address) of target buffer
	DW 0x0000						; 16 bit segment of target buffer
.buff:	DD 0x00000000						; Lower 32 bits of 48 bit LBA
	DD 0x00000000						; Upper 32 bits of 48 bit LBA






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
	MOV	AH, 0X0E					; Tell BIOS we are going to print chars

.LOOP	LODSB							; Load single byte from SI
	OR	AL, AL						; Check if we are at the end of the string
	JZ	.NEWL						; If we are at the strings end, print newline and exit
	INT	0x10						; Call BIOS print routine
	JMP	.LOOP						; Loop until done printing

.NEWL	MOV	AL, 0XA						; Print newline
	INT	0x10						; Call BIOS print routine
	MOV	AL, 0xD						; Print carriage return
	INT	0x10						; Call BIOS print routine
	RET							; Exit print routine

;;
;; GPT Data Structures
;;
;; (These are relative offsets, not absolute values) 
;;

;; GPT Header
GPTHEADER.SIGNATURE	EQU 0x00 				; (8 bytes): Magic number, should be equal to 'EFI PART' in hex
GPTHEADER.SIZE		EQU 0x0C 				; (4 bytes): Size of this header
GPTHEADER.CHECKSUM	EQU 0x10 				; (4 bytes): Checksum of this header
GPTHEADER.TABLE_BLOCK	EQU 0x48 				; (8 bytes): Block which contains the partition table, usually 0x02
GPTHEADER.ENTRIES_CNT	EQU 0X50 				; (4 bytes): Number of partitions on disk
GPTHEADER.ENTRY_SIZE	EQU 0x54 				; (4 bytes): contains the size (in bytes) of each partition entry


;; Partition Table
GPT.TYPE_GUID		EQU 0x00 				; (16 bytes): Partition type (microsoft, apple, etc), 0 means unused entry
GPT.FIRST_BLOCK		EQU 0x20 				; (8 bytes): Contains first sector number for the partition
GPT.LAST_BLOCK		EQU 0x28 				; (8 bytes): Contains last sector number for the partition
GPT.ATTRIBUTES		EQU 0x30 				; (8 bytes): Contains bit flags with important info e.g bootable or not
GPT.VOL_NAME		EQU 0x38 				; (72 bytes): Partition name, can be up to 72 bytes


;;
;; Strings
;;
WelcomeMSG:	DB "Boot Manager entered.", 0xA, 0xD, "Searching for bootloader...", 0x0
DiskErrorMSG:	DB "DISK READ ERROR", 0x0


;;
;; Compatibility stuff
;;
TIMES	64	DB	0					; We can't overwrite the partition table, fill with 0s
TIMES	510 - ($ - $$) DB 0x00					; Bootloader has to be 512 bytes long, pad the missing bytes with 0s
DW 	0xAA55							; But make the last two bytes 0xAA55

