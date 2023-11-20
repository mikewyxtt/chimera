org 0x7C00			; Address in memory where  BIOS loads us at
bits 16				; We start in 16 bit real mode

;; The computer has just been powered on, BIOS did all the stuff it needs
;; to do to get the computer to a running state and loaded this file into RAM.
;; Because of backwards compatitbility, the system is in the state it would have
;; been in back in the 80's. The rest of this code is us getting it into a more
;; useful state and eventually loading up the kernel and servers.
;;
;; Execution begins here:
start:
	cli			; Disable interrupts before we do anything else

	;; Setup a stack
	mov	ax, 0		; Set AX to 0, presumably bc you cant directly assign integers to those registers
	mov	ds, ax		; ?
	mov	es, ax		; ?
	mov	ss, ax		; Setup stack segment
	mov	sp, 0x7C00	; Stack grows downwards from 0x7C00
 
	call 	clear_screen	; Clear the screen
	
	mov 	si, welcome_msg
	call 	print_string	; Display welcome message
	
	call 	enable_a20	; Enable A20 gate

	
	;; We are now going to use the BIOS to read the second sector from the disk.
	;; (we are currently on this first sector)
	mov	al, 1		; Number of sectors to read, we want to read 1 sector (512 bytes)
	mov	cl, 0x2		; Cylinder to read from
	mov	dh, 0x0		; Head to read from
	mov	bx, 0x9C00	; Memory buffer
	call	hdd_read	; Disk read routine

	jmp	0x0000:0x9C00	; Jump to the bootstrap (Segment:Address)

	hlt			; Halt CPU, but this should never execute.




;; Data Section ;;
welcome_msg:			db "Chimera MBR entered.", 0
bootstrap_read_fail_msg:	db "Unable to read bootstrap from disk. Halting CPU."

;; Includes ;;
%include "mbr/a20.inc"		; Include stuff to enable A20 gate
%include "drivers/console.inc"	; Include stuff to do console I/O
%include "drivers/disk.inc"	; Include disk driver

;; Special BIOS stuff ;
times 	510 - ($ - $$) db 0	; BIOS expects bootsector to be 512 bytes, so we pad the file with 0's
dw 	0xAA55			; Magic number that tells the BIOS this is an executable block of code

