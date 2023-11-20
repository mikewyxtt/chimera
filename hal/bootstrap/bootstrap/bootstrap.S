org 0x9C00			; Address in memory where  BIOS loads us at
bits 16				; We are still in 16 bit real mode

;; MBR read us off the disk and loaded us into memory. We are no longer confined to the 440 byte
;; code size limit so we can continue initializing the hardware and eventually load the kernel.
;;
;; Execution begins here:
start:
	;; Setup the stack
	mov	ax, 0		; Set AX to 0, presumably bc you cant directly assign integers to those registers
	mov	ds, ax		; ?
	mov	es, ax		; ?
	mov	ss, ax		; Setup stack segment
	mov	sp, 0x9C00	; Stack grows downwards from 0x7C00
	
	mov 	si, welcome_msg
	call 	print_string	; Display welcome message
	
;	call	setup_gdt	; Setup/load GDT

	hlt			; Halt CPU, but this should never execute.


bits 32				; Tell NASM we are using 32 bit instructions moving forward
bootstrap_final:
	;; This function is called after setup_gdt complets. At this point we are in
	;; 32 bit protected mode with a20 gate enabled and the GDT loaded up. We will
	;; now pass control on to the second stage loader to continue the bootstrap 
	;; process since we will soon be over our 440 byte limit.
	;;
	;; Our work here is done, we just need to use Virtual 8086 to drop down into
	;; the BIOS so we can read the second stage loader from the disk.
	
	hlt			; Halt the CPU


;; Data Section ;;
welcome_msg:	db "Chimera bootstrap entered.", 0


;; Includes ;;
%include "drivers/console.inc"	; Include stuff to do console I/O
%include "bootstrap/gdt.inc"	; Include GDT setup functions

times 	512 - ($ - $$) db 0
