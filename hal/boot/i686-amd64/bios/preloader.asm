ORG 0x600							; Address in memory where  BIOS loads us at
BITS 16								; We start in 16 bit real mode


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DATA STRUCTURE ADRESSES ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GPT_HEADER_ADDR			EQU	0x0A00			; GPT Header (512 bytes), contains crucial information about the disk and its partitions
GPT_TABLE_ADDR			EQU	0x0C00			; The partition table itself (512 bytes)

EXT2_SUPERBLOCK_ADDR		EQU	0x0E00			; EXT2 superblock (1024 bytes), contains crucial nformation about the filesystem
EXT2_BD_TABLE_ADDR		EQU	0x1200			; EXT2 Block Descriptor Table (32 bytes): there is a BDT for each block group on EXT2 but we only need the first one to find /boot/loader
EXT2_INODE_TABLE_ADDR		EQU	0x1220			; EXT2 Inode Table (128 bytes): There is also an inode table for each block group but as with before, we only need the first one
EXT2_ROOT_DIR_ADDR		EQU	0x12A0			; EXT2 Root directory (1 Logical Block ?)



	JMP	START						; I read somewhere that some machines wont load a bootsector that doesn't start with 
	NOP							; these two instructions... Pretty sure its FAT32 related but its only 3 bytes so who cares...


;; The computer has just been powered on, BIOS did all the stuff it needs
;; to do to get the computer to a running state and loaded this file into RAM.
;; Because of backwards compatitbility, the system is in the state it would have
;; been in back in the 80's. The rest of this code is us getting it into a more
;; useful state and eventually loading up the kernel and servers.
;;
;; Execution begins here:
START:
	;; Setup the stack
	cld
	XOR	AX, AX						; Set AX to 0, presumably bc you cant directly assign integers to those registers
	MOV	DS, AX						; Data segment
	MOV	ES, AX						; Extra segment
	MOV	SS, AX						; Stack segment
	MOV	SP, 0x7C00					; Stack grows downwards from 0x7C00

	MOV	SI, SP
	MOV	DI, 0x600
	MOV	CX, 0x100
	REP	MOVSW

	JMP	MAIN - 0x7C00 + 0x600

MAIN:
	;; The first thing we need to do is read all of the data structures that reside in known locations on the disk into memory. We need to read the GPT
	;; header and partition table to find the second partition, then we need to read the superblock, block descriptor table, and the inode table into memory
	;; from said partition before we can begin searching for /boot/loader.
	;;	
	;; TODO:
	;;	Make the implementation more robust by actually searching and finding the primary parititon in a precise way, rather than just hard coding the
	;;	second partition into the code. This way the user could partition the disk however they please and still be able to boot the system.

	;; Read GPT Header
	MOV	ESI, 0x01					; GPT header is located in block 1
	MOV	BX, GPT_HEADER_ADDR				; GPT header will reside in memory between 0x800 and 0x1000
	MOV	AX, 0x01					; GPT header is only 1 sector in length
	CALL	READ_SECTORS




	;; Read GPT Partition Table
	MOV	ESI, [GPT_HEADER_ADDR + GPTHEADER.TABLE_BLOCK]	; Partition table block location is listed in the GPT header
	MOV	BX, GPT_TABLE_ADDR				; Partition table will reside in memory at address specified at top of file
	MOV	AX, 0x01					; Partition table is only 1 sector in length
	CALL	READ_SECTORS


	;; Define ROOT_PART_OFFSET global variable
	;;
	;; NOTE:
	;;	This part is a bit of a hack bc we assume the primary partitoin is the second partition, hence why we seek into the table by one entry then
	;;	then add the first block. The correct way to do this would be to find the primary partition via a signature in the parition header, then read
	;;	the superblock from THAT partition.
	;;
	MOV	DWORD EAX, [GPT_HEADER_ADDR + GPTHEADER.ENTRY_SIZE]	; We need to know the size of each entry so we can select one
	MOV	DWORD ESI, [GPT_TABLE_ADDR + EAX + GPT.FIRST_BLOCK]	; We want to store the starting LBA(first block) of the second partition into this variable
	MOV	DWORD [ROOT_PART_OFFSET], ESI				; Store it to the variable



	;; Read second half of the preloader into memory
	MOV	DWORD ESI, [ROOT_PART_OFFSET]				; Second half is located immediately after the first half on disk, so the second sector
	ADD	ESI, 0x01
	MOV	BX, 0x800						; First half is at 0x600, takes up 512 bytes (0x200) so we read it to 0x800
	MOV	AX, 0x01						; We only want to read one sector
	CALL	READ_SECTORS

call SECOND_HALF_START

jmp $



	;; Read EXT2 superblock
;	MOV	DWORD ESI, [ROOT_PART_OFFSET]				; Read root partition offset into ESI so we can offset from "zero" of primary partition
;	ADD	ESI, 0x02						; We need the third and fourth blocks (first two blocks reserved for bootloader code)
;	MOV	AX, 0x02						; Load two sectors (Superblock is 1024 bytes)
;	MOV	BX, EXT2_SUPERBLOCK_ADDR				; Read it into the memory address set at the top of this file
;	CALL	READ_SECTORS

;	xor	edx, edx
;	mov	dword edx, [EXT2_SUPERBLOCK_ADDR + 0x38]


	;; Read EXT2 Block Descriptor Table
	;;
	;; Read EXT2 Inode Table

	;; Clear the screen, display welcome message
	MOV	AH, 0x00					; Tell BIOS we want to set video mode
	MOV	AL, 0x02					; 720x480 resolution
	INT	0x10
	
	MOV 	SI, WelcomeMsg					; Move welcome message into SI
	CALL 	PRINTLN						; Display welcome message

	;; Before we jump to the loader, we have to set up a basic 32 bit protected mode environment. We will enable the A20
	;; line which allows us to access memory above 1MB, and load a minimal GDT. This is not the GDT that will be used
	;; by the operating system; that GDT will be loaded by the loader itself.
;	CALL 	ENABLE_A20					; Enable A20 line
;	CALL	SETUP_GDT					; Setup GDT
	

.DONE:	HLT							; Infinite loop. This should never execute.
	JMP	.DONE





;;
;; FUNCTION:
;;	PROTECTED_MODE()
;; PURPOSE:
;; 	SETUP_GDT() returns here. We are now in protected mode and can finish up by jumping to the loader
;;
BITS 32
PROTECTED_MODE:
	HLT




BITS 16
;;;;;;;;;;;;;;;;;;;;;;;
;; SUPPORT FUNCTIONS ;;
;;;;;;;;;;;;;;;;;;;;;;;

;; FUNCTION:
;;	READ_SECTORS()
;;
;; ARGUMENTS:
;;	ESI: 	Starting block number to load
;;	BX:	Transfer buffer (Address to load sectors to)
;;	DL:	Disk to read from. Primary HDD is 0x80, ignore to use boot disk
;;	AX:	Number of sectors to read
;;
;; PURPOSE:
;;	Loads sectors from the selected disk into memory
READ_SECTORS:
	CLC							; Clear the carry flag, just in case.
;	XOR	AX, AX						; Zero AX, seems to have issues if you don't
	MOV	DL, 0x80					; Primary hard disk, CHANGE THIS TO BOOT FROM OTHER MEDIA
	MOV	[DAP.BUFF], BX					; Put the buffer address from BX into the Disk Address Packet
	MOV	[DAP.LBA], ESI					; Copy LBA from ESI into the DAP
;	MOV	[DAP.COUNT], AX					; Copy number of sectors to read into DAP
	MOV	AH, 0x42					; BIOS extended read function, allows us to read disks > 8 GB
	MOV	SI, DAP						; Move DAP into SI for BIOS to read
	INT	0x13						; Call the BIOS
	JC	.ERROR						; Fail if there was any issue loading the sector(s)
	RET							; Otherwise return to caller

.ERROR:	MOV	SI, DiskErrorMSG				; Load error message into SI
	CALL	PRINTLN						; Print error message
	CLI							; Disable interrupts and halt if we coulnd't load the sectors, fatal error.
	HLT							; ^^

;; Disk Address Packet for BIOS extended read function
DAP:
	DB 0x10							; Size of data packet, should always be 10
	DB 0x00							; Reserved
	DW 0x0001						; Number of sectors to read	
.BUFF:	DW 0x0000						; 16 bit offset (address) of target buffer
	DW 0x0000						; 16 bit segment of target buffer
.LBA	DD 0x00000000						; Lower 32 bits of 48 bit LBA
	DD 0x00000000						; Upper 32 bits of 48 but LBA (we do not use this currently)



;;
;; Function:
;;      PRINTLN()
;;
;; Arguments:
;;      SI: String to print
;;
;; Purpose:
;;      Print string to screen
;;
;; Clobbers:
;;      AH, AL
;;
PRINTLN:
        mov     ah, 0x0E        				; Tell Bios we are going to print chars

.loop   lodsb                   				; Load single byte from SI
        or      al, al          				; Check if end of string
        jz      .newl           				; Return if end of string
        int     0x10            				; BIOS putc routine
        jmp     .loop           				; Loop until done printing

.newl   mov     al, 0xA         				; Print newline
        int     0x10
        mov     al, 0xD         				; Print carriage return
        int     0x10
        ret




;;
;; Function: 
;;	ENABLE_A20()
;;
;; Purpose: 
;;	Enables A20 gate
;;
;; Arguments: 
;;	None
;;
ENABLE_A20:

	; We are assuming the user is using QEMU so we basically are just making sure
	; A20 is enabled. If it isn't we just fail. There was no reliable way of disabling
	; it to try and turn it back on at the time of writing. A20 is enabled if AX = 1.
	call	check_a20
	cmp 	ax, 1
	je 	.done
	jmp 	.fail

.fail	hlt				; Halt CPU. Nothing to do if we dom't have A20...

.done	ret				; A20 is enabled, continue on.
	

;; Function: check_a20 ;;
;  Purpose: Check if a20 gate sis enabled. Returns 1 in AX if enabled, 0 if not
;  Arguments: N/A
check_a20:
	; Preserve state of these registers before we begin.
	pushf
	push	ds
	push	es
	push 	di
	push	si

	; Basically the A20 gate allows memory accesses above 1MB, so we use a 
	; simple test that sees whether or not the memory wraps around or not when
	; we write/read something over 1MB
	xor	ax, ax
	mov 	es, ax
	mov	di, 0x0500
	mov	si, 0x0510

	mov 	al, byte [es:di]
	push 	ax

	mov 	al, byte [ds:si]
	push	ax

	mov	byte [es:di], 0x00
	mov	byte [ds:si], 0xFF
	
	cmp	byte [es:di], 0xFF	; Check if memory wraps arround

	; Put things back how they were
	pop	ax
	mov	byte [ds:si], al

	pop	ax
	mov	byte [es:di], al
	
	; If it's not enabled, set ax to 0 and leave, otherwise set it to 1
	mov	ax, 0
	je	.done

	mov	ax, 1			; If we get here, A20 is enabled :)

.done	pop 	si
	pop	di
	pop	es
	pop	ds
	popf
	ret

;; Data Section
;a20_fail_msg:	db "Error: Could not enable A20 gate. Please use QEMU."
;a20_success_msg: db "A20 Gate enabled.", 0






SETUP_GDT:
	cli				; Disable interrupts just to be sure

	xor	ax, ax			; ?
	mov	ds, ax			; ?
	lgdt	[gdt_desc]		; Load the GDT

	mov	eax, cr0		; Move contents of CR0 into EAX
	or	eax, 1			; Set bit 0 by making an OR operation with EAX and 1
	mov	cr0, eax		; Protected mode flag set

	jmp 	CODE_SEG:flush_gdt	; Far jump to protected mode

	ret				; Return to calling function

bits 32					; 32 bits, we are in protected mode :)
flush_gdt:
	mov	ax, DATA_SEG		; Update the segment registers!
	mov	ds, ax
	mov	ss, ax
	mov	es, ax
	mov	fs, ax
	mov	gs, ax

	mov	ebp, 0x90000		; Setup new stack
	mov	esp, ebp
	
	call	PROTECTED_MODE		; Go back to MBR main code file, this time in 32 bit protected mode



gdt:					
gdt_null:				; Null segment descriptor
	dq 0				; 64 bits containing '0'

gdt_code:				; Code segment descriptor
	dw 0x0FFFF			; Limit (16 bits)
	dw 0				; Base address (16 bits)
	db 0				; Base address (cont.) (8 bits)
	db 10011010b			; ? (8 bits)
	db 11001111b			; ? (8 bits)
	db 0				; ? (8 bits)

gdt_data:				; Data segment descriptor
	dw 0x0FFFF			; Limit (16 bits)
	dw 0				; Base address (16 bits)
	db 0				; Base address (cont.) (8 bits)
	db 10010010b			; ? (8 bits)
	db 11001111b			; ? (8 bits)
	db 0				; Segment base ? (8 bits)
gdt_end:
gdt_desc:
	dw gdt_end - gdt - 1
	dd gdt

CODE_SEG 		equ 	gdt_code - gdt
DATA_SEG 		equ 	gdt_data - gdt

bits 16


;;;;;;;;;;;;;;;;;;
;; Data Section ;;
;;;;;;;;;;;;;;;;;;

;; Strings
WelcomeMsg:			DB "Stage2 entered.", 0x00
DiskErrorMSG:			DB "DISK READ ERROR", 0x00
;LoaderFilename:		DB "LOADER", 0x00



;; Global Variables
ROOT_PART_OFFSET:		DD 0x00				; Double word to hold the root partition offset that we can use as a reference point to find data within the partition


;;
;; GPT Data Structures
;;
;; (These are relative offsets, not absolute values) 
;;

;; GPT Header
GPTHEADER.SIGNATURE     	EQU 	0x00                   	; (8 bytes): Magic number, should be equal to 'EFI PART' in hex
GPTHEADER.SIZE          	EQU 	0x0C                   	; (4 bytes): Size of this header
GPTHEADER.CHECKSUM      	EQU 	0x10                   	; (4 bytes): Checksum of this header
GPTHEADER.TABLE_BLOCK   	EQU 	0x48                   	; (8 bytes): Block which contains the partition table, usually 0x02
GPTHEADER.ENTRIES_CNT   	EQU 	0X50                   	; (4 bytes): Number of partitions on disk
GPTHEADER.ENTRY_SIZE    	EQU 	0x54                   	; (4 bytes): contains the size (in bytes) of each partition entry


;; Partition Table
GPT.TYPE_GUID           	EQU 	0x00                   	; (16 bytes): Partition type (microsoft, apple, etc), 0 means unused entry
GPT.FIRST_BLOCK         	EQU 	0x20                   	; (8 bytes): Contains first sector number for the partition
GPT.LAST_BLOCK          	EQU 	0x28                   	; (8 bytes): Contains last sector number for the partition
GPT.ATTRIBUTES          	EQU 	0x30                   	; (8 bytes): Contains bit flags with important info e.g bootable or not
GPT.VOL_NAME            	EQU 	0x38                   	; (72 bytes): Partition name, can be up to 72 bytes



;; Special BIOS stuff ;
PMBR_PARTITION_TABLE: TIMES 64 	DB 	0x00			; Pad MBR partition table with 0's
PADDING: TIMES 510 - ($ - $$) 	DB 	0x00			; BIOS expects bootsector to be 512 bytes, so we pad the space between the partition table and the magic number with 0's
MBR_BOOT_SIGNATURE:		DW 	0xAA55			; Magic number that tells the BIOS this is an executable block of code






;;;;;;;;;;;;;;;;;;;;;;;
;; BEGIN SECOND HALF ;;
;;;;;;;;;;;;;;;;;;;;;;;


SECOND_HALF_START:
	MOV 	SI, TEST_STR
	CALL	PRINTLN
jmp $

TEST_STR:			DB	"HELLO SECOND HALF", 0x00
TIMES 1024 - ($ -$$) 		DB 	0x00			; This entire bootloader combined cannot be more that 1024
