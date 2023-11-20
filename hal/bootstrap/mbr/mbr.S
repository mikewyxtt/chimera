org 0x7C00			; Address in memory where  BIOS loads us at
bits 16				; We start in 16 bit real mode

; Execution begins here
start:
	; Setup stack
	mov	ax, 0		; Set AX to 0, presumably bc you cant directly assign integers to those registers
	mov	ds, ax		; ?
	mov	es, ax		; ?
	mov	ss, ax		; Setup stack segment
	mov	sp, 0x7C00	; Stack grows downwards from 0x7C00
 
	cli			; Disable interrupts	
	call 	clear_screen	; Clear the screen
	
	mov 	si, welcome_msg
	call 	print_string	; Display welcome message
	
	call 	enable_a20	; Enable A20 gate
	
	call	setup_gdt	; Setup/load GDT

	hlt			; Halt CPU so it doesn't do weird things

bits 32
begin_bootstrap:
	
	hlt


; Data Section ;
welcome_msg:	db "Chimera bootloader entered.", 0


; Includes ;
%include "mbr/a20.inc"		; Include stuff to enable A20 gate
%include "drivers/console.inc"	; Include stuff to do console I/O
%include "mbr/gdt.inc"		; Include GDT setup functions


; Special BIOS stuff
times 	510 - ($ - $$) db 0	; BIOS expects bootsector to be 512 bytes, so we pad the file with 0's
dw 	0xAA55			; Magic number for BIOS

