ORG 0x600							; Address in memory where  BIOS loads us at
BITS 16								; We start in 16 bit real mode


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DATA STRUCTURE ADRESSES ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GPT_HEADER_ADDR			EQU	0x0A00			; GPT Header (512 bytes), contains crucial information about the disk and its partitions
GPT_TABLE_ADDR			EQU	0x0C00			; The partition table itself (512 bytes)

EXT2_SUPERBLOCK_ADDR		EQU	0x0E00			; EXT2 superblock (1024 bytes), contains crucial nformation about the filesystem
EXT2_BGD_TABLE_ADDR		EQU	0x1200			; EXT2 Block Descriptor Table (4096? bytes): should be one block, we leave room for 3 4K blocks just in case.
EXT2_INODE_TABLE_ADDR		EQU	0x4220			; EXT2 Inode Table (128 bytes): There is also an inode table for each block group but as with before, we only need the first one
EXT2_ROOT_DIR_ADDR		EQU	0x6220			; EXT2 Root directory (1 Logical Block ?)



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

	;; Stack variables

	PUSH	DWORD	0x00		; [ESP + 18]	?
	PUSH	WORD	0x100		; [ESP + 16]	Inode size
	PUSH	DWORD	0x00000000	; [ESP + 12]	Inode table starting offset
	PUSH	DWORD 	8		; [ESP + 8 ]	Physical sectors per logical block
	PUSH	DWORD 	4096		; [ESP + 4 ]	ext2 logical block size
	PUSH	DWORD 	0x00000000	; [ESP + 0 ]	Primary partition physical block offset



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


	;; Find starting block of the primary partition
	;;
	;; NOTE:
	;;	This part is a bit of a hack bc we assume the primary partitoin is the second partition, hence why we seek into the table by one entry then
	;;	then add the first block. The correct way to do this would be to find the primary partition via a signature in the parition header, then read
	;;	the superblock from THAT partition.
	;;
	MOV	DWORD EAX, [GPT_HEADER_ADDR + GPTHEADER.ENTRY_SIZE]	; We need to know the size of each entry so we can select one
	MOV	DWORD ECX, [GPT_TABLE_ADDR + EAX + GPT.FIRST_BLOCK]	; We add the size of one entry (EAX) to get us to the second partition, then we find the starting block number within that entry
	MOV	DWORD [ESP + 0], ECX					; We want to store the physical starting block of the second partition in a variable so we can make relative offsets




	;; Read second half of the preloader into memory
	MOV	DWORD ESI, [ESP + 0]					; Copy physical starting block of boot partition in ESI so we can make relative offsets
	ADD	ESI, 0x01						; Second half is located immediately after the first half on disk, so one sector from 0
	MOV	BX, 0x800						; First half is at 0x600, takes up 512 bytes (0x200) so we read it to 0x800
	MOV	AX, 0x01						; We only want to read one sector
	CALL	READ_SECTORS




	;; Read EXT2 superblock
	MOV	DWORD ESI, [ESP + 0]					; Copy physical starting block of boot partition so we can make relative offsets
	ADD	ESI, 0x02						; We need the third and fourth blocks (first two blocks reserved for bootloader code)
	MOV	AX, 0x02						; Load two sectors (Superblock is 1024 bytes)
	MOV	BX, EXT2_SUPERBLOCK_ADDR				; Read it into the memory address set at the top of this file
	CALL	READ_SECTORS






	;; Find and read EXT2 Block Descriptor Table

	;; Calculate logical block size and sectors per block
	MOV	DWORD ECX, [EXT2_SUPERBLOCK_ADDR + EXT2.LOGICAL_BLOCK_SIZE]	; Read "s_log_block_size" from superblock so we can calculate the block size
	MOV	EDX, 0x400							; We need to bitshift by 1024
	SHL	EDX, CL								; blocksize = 1024 << s_log_block_size
	MOV	[ESP + 4], EDX							; Store block size in our stack variable

	;; if blocksize < 1024
	;; BDT = logical block 3 (0x02)
	;; else
	;; BDT = logical block 2 (0x01)
	MOV	EAX, [ESP + 4]					; Copy logical block size into EAX so we can compare to 1024
	CMP	EAX, 0x400					; Compare block size in EAX to 1024
	JL	.1KB						; if blocksize < 1024, jmp to .1KB

	XOR	EDX, EDX					; Zero out EDX
	MOV	EBX, 0x200					; To calculate physical blocks per logical block, we simply divide logical blocks / physical blocks
	DIV	EBX						; This stores the result in EAX
	MOV	[ESP + 8], EAX					; Store result in our variable for future use

	MOV	ESI, [ESP + 0]					; Put primary partition physical block offset in ESI so we can calculate relative offsets
	ADD	ESI, EAX					; Since we need the second logical block, we need only seek 1 logical block into the partition (block 2 = 0x01)

	JMP	.DONE_CALULATING

.1KB:	
	;; NOTE: The following never been tested, so if you have issues involving 1 KiB blocks on EXT2, look here.
	XOR	EDX, EDX					; Zero EDX, required to do division
	MOV	EBX, 0x200					; We are going to divide logical blocks / physical blocks (EAX / EBX)
	DIV	EBX						; Perform the division, result stored in EAX
	MOV	[ESP + 8], EAX					; Store result in our stack variable for future use

	MOV	ESI, [ESP + 0]					; Copy primary partition offset into ESI for relative offset calculations
	ADD	ESI, EAX					; It's the 3rd logical block, so we need to seek 2 logical blocks from 0, we do so by adding 2 times 
	ADD	ESI, EAX					; ^^

.DONE_CALULATING:
	MOV	AX, [ESP + 8]					; BGDT is 1 block in length (see note above) so we will read n number of sectors
	MOV	BX, EXT2_BGD_TABLE_ADDR				; We will load the BGD table to the address defined above
	CALL	READ_SECTORS





	;; Find and read EXT2 Inode Table into memory
	MOV	DWORD EAX, [EXT2_BGD_TABLE_ADDR + EXT2.BGDT.INODE_TABLE_OFFSET]	; Read the logical block id of the inode table into EAX
	MOV	ECX, [ESP + 8]							; Physical sectors per logical block
	MUL	ECX								; Multiply them, product is stored in EAX
	MOV	ESI, [ESP + 0]							; Next we need the boot partition starting sector
	ADD	ESI, EAX							; (Boot partition starting sector + Inode table offset) = Inode table physical starting sector
	MOV	AX, [ESP + 8]							; Inode table can be huge, we are loading 1 logical block for now since we only need the root directory to start
	MOV	BX, EXT2_INODE_TABLE_ADDR					; We will read the inode table to the address specificied above
	CALL	READ_SECTORS





	;; Calculate inode size
	MOV	WORD AX, [EXT2_SUPERBLOCK_ADDR + EXT2.INODE_SIZE_OFFSET]	; Copy inode size into AX
	MOV	WORD [ESP + 16], AX						; Store inode size in our stack variable




	;; Find root directory
	;; The root directory is always the 2nd entry in the inode table
	XOR	EAX, EAX							; Zero EAX bc we are going to go from using AX to EAX, something could be left over
	MOV	WORD AX, [ESP + 16]						; Copy inode size into AX
	MOV	EBX, EXT2_INODE_TABLE_ADDR					; Copy absolute address for inode table into EBX so we can do relative offsets
	ADD	EBX, EAX							; Root directory is the second entry, so we seek in by one to bring us to 0x02
	ADD	EBX, EXT2.INODE_BLOCK_ID_OFFSET					; Offset into the root directory inode to find the block ID
	MOV	ECX, [ebx]							; Copy the first block ID for the root directory into ECX

	;; Read root directory (Block ID * Sectors per Block) + Boot partition offset
	MOV	AX, [ESP + 8]							; Sectors per logical block
	MUL	ECX								; product in eax	
	MOV	ESI, [ESP + 0]							; Boot partition offset
	ADD	ESI, EAX							; Relative to absolute conversion
	MOV	AX, [ESP + 8]							; We want to read one entire block
	MOV	BX, EXT2_ROOT_DIR_ADDR						; Read the root directory into memory
	CALL	READ_SECTORS



	;; Find /boot/loader





jmp $


	

	;; Before we jump to the loader, we have to set up a basic 32 bit protected mode environment. We will enable the A20
	;; line which allows us to access memory above 1MB, and load a minimal GDT. This is not the GDT that will be used
	;; by the operating system; that GDT will be loaded by the loader itself.
;	CALL 	ENABLE_A20					; Enable A20 line
;	CALL	SETUP_GDT					; Setup GDT
	







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
	MOV	[DAP.COUNT], AX					; Copy number of sectors to read into DAP
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
.COUNT:	DW 0x0001						; Number of sectors to read	
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





;;;;;;;;;;;;;;;;;;
;; Data Section ;;
;;;;;;;;;;;;;;;;;;

;; Strings
DiskErrorMSG:			DB "DISK READ ERROR", 0x00
LoaderFilename:		DB "LOADER", 0x00



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


;; EXT2 
EXT2.LOGICAL_BLOCK_SIZE		EQU	0x18			; (4 bytes): Contains number to solve for logical block size: block_size = 1024 << EXT2.LOGICAL_BLOCK_SIZE
EXT2.BGDT.INODE_TABLE_OFFSET	EQU	0x08			; (4 bytes): Contains the logical block ID of the Inode Desc. Table
EXT2.INODE_SIZE_OFFSET		EQU	0x58			; (2 bytes): Contains the size of one inode
EXT2.INODE_BLOCK_ID_OFFSET	EQU	0X28			; (60 bytes): 15 32 bit entries containing block ids to the data blocks of the inode


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




TEST_STR:			DB	"HELLO SECOND HALF", 0x00
TIMES 1024 - ($ -$$) 		DB 	0x00			; This entire bootloader combined cannot be more that 1024
