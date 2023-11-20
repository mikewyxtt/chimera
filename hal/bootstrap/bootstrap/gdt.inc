;; Functions to help us setup and load a GDT
bits 16

setup_gdt:
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
	
	call	bootstrap_final



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

bits 32
