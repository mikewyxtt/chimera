ORG 0x7C00							; Address in memory where  BIOS loads us at
BITS 16								; We start in 16 bit real mode

	JMP	START						; I read somewhere that some machines wont load a bootsector that doesn't start with 
	NOP							; these three instructions... Pretty sure its FAT32 related but its only 3 bytes so who cares...


;; The computer has just been powered on, BIOS did all the stuff it needs
;; to do to get the computer to a running state and loaded this file into RAM.
;; Because of backwards compatitbility, the system is in the state it would have
;; been in back in the 80's. The rest of this code is us getting it into a more
;; useful state and eventually loading up the kernel and servers.
;;
;; Execution begins here:
START:
	;; Setup the stack
	MOV	AX, 0						; Set AX to 0, presumably bc you cant directly assign integers to those registers
	MOV	DS, AX						; ?
	MOV	ES, AX						; ?
	MOV	SS, AX						; Setup stack segment
	MOV	SP, 0x7C00					; Stack grows downwards from 0x7C00


	;; Stage2 is 2 sectors (1024 bytes) in length, but boot managers only load 1 sector into memory. Fortunately due to the design of the filesyste,
	;; the second half of stage2 will always be the 2nd sector of the boot partition. We are running at 0x7C00 now, so if we load the other half to 
	;; 0x7E00 the code will just smoothly transition to the next 512 bytes, no extra steps required :)
	;;
	;; Just like the boot manager, we are going to hard code the 2nd partition as the boot partition for simplicity. First we must load the GPT header
	;; and partition table

	MOV	ESI, 0x01					; GPT header is located in block 1
	MOV	BX, 0x800					; GPT header will reside in memory between 0x800 and 0x1000
	MOV	AX, 0x01					; GPT header is only 1 sector in length
	CALL	READ_SECTORS

	MOV	ESI, [0x800 + GPTHEADER.TABLE_BLOCK]		; Partition table block location is listed in the GPT header
	MOV	BX, 0x1000					; Partition table will reside in memory between 0x1000 and 0x1200
	MOV	AX, 0x01					; Partition table is also only 1 sector
	CALL	READ_SECTORS


	;; Now that we have the GPT data structures loaded up, we can find the other half of stage2

	MOV	DWORD EAX, [0x800 + GPTHEADER.ENTRY_SIZE]	; We need to know the size of each entry so we can select one
	MOV	DWORD ESI, [0x1000 + EAX + GPT.FIRST_BLOCK]	; We want to load the second block of the second partition. This is explained in boot_manager.asm
	ADD	ESI, 0x01					; We need the second block
	MOV	AX, 0X01					; Load one sector
	MOV	BX, 0x7E00					; We want to load the sector directly after this one (in memory)
	CALL	READ_SECTORS


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
	XOR	AX, AX						; Zero AX, seems to have issues if you don't
	MOV	DL, 0x80					; Primary hard disk, CHANGE THIS TO BOOT FROM OTHER MEDIA
	MOV	[DAP.BUFF], BX					; Put the buffer address from BX into the Disk Address Packet
	MOV	[DAP.LBA], ESI					; Copy LBA from ESI into the DAP
;	MOV	[DAP.COUNT], AX					; Copy number of sectors to read into DAP
	MOV	AH, 0x42					; BIOS extended read function, allows us to read disks > 8 GB
	MOV	SI, DAP						; Move DAP into SI for BIOS to read
	INT	0x13						; Call the BIOS
	JC	.ERROR						; Fail if there was any issue loading the sector(s)
	RET							; Otherwise return to caller

.ERROR	MOV	SI, DiskErrorMSG				; Load error message into SI
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

.fail	mov 	si, a20_fail_msg
	call 	PRINTLN
	hlt				; Halt CPU. Nothing to do if we dom't have A20...

.done	mov	si, a20_success_msg
	call	PRINTLN
	ret				; A20 is enabled, continue on.
	

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
a20_fail_msg:	db "Error: Could not enable A20 gate. Please use QEMU."
a20_success_msg: db "A20 Gate enabled.", 0






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

CODE_SEG equ gdt_code - gdt
DATA_SEG equ gdt_data - gdt




;;;;;;;;;;;;;;;;;;
;; Data Section ;;
;;;;;;;;;;;;;;;;;;

WelcomeMsg:			DB "Stage2 entered.", 0x00
DiskErrorMSG:			DB "DISK READ ERROR", 0x00
;LoaderFilename:		DB "LOADER", 0x00


;;
;; GPT Data Structures
;;
;; (These are relative offsets, not absolute values) 
;;

;; GPT Header
GPTHEADER.SIGNATURE     EQU 0x00                                ; (8 bytes): Magic number, should be equal to 'EFI PART' in hex
GPTHEADER.SIZE          EQU 0x0C                                ; (4 bytes): Size of this header
GPTHEADER.CHECKSUM      EQU 0x10                                ; (4 bytes): Checksum of this header
GPTHEADER.TABLE_BLOCK   EQU 0x48                                ; (8 bytes): Block which contains the partition table, usually 0x02
GPTHEADER.ENTRIES_CNT   EQU 0X50                                ; (4 bytes): Number of partitions on disk
GPTHEADER.ENTRY_SIZE    EQU 0x54                                ; (4 bytes): contains the size (in bytes) of each partition entry


;; Partition Table
GPT.TYPE_GUID           EQU 0x00                                ; (16 bytes): Partition type (microsoft, apple, etc), 0 means unused entry
GPT.FIRST_BLOCK         EQU 0x20                                ; (8 bytes): Contains first sector number for the partition
GPT.LAST_BLOCK          EQU 0x28                                ; (8 bytes): Contains last sector number for the partition
GPT.ATTRIBUTES          EQU 0x30                                ; (8 bytes): Contains bit flags with important info e.g bootable or not
GPT.VOL_NAME            EQU 0x38                                ; (72 bytes): Partition name, can be up to 72 bytes


;;
;; Include Section
;;
;%INCLUDE "mbr/a20.inc"						; A20 gate setup code
;%INCLUDE "mbr/gdt.inc"						; GDT setup code


;; Special BIOS stuff ;
TIMES 	510 - ($ - $$) DB 0					; BIOS expects bootsector to be 512 bytes, so we pad the file with 0's
DW 	0xAA55							; Magic number that tells the BIOS this is an executable block of code

