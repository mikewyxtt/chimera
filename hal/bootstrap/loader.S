org 0x7C00			; Address in memory where  BIOS loads us at
bits 16				; We start in 16 bit real mode

; Execution begins here
start:

	; Start off by disabling interrupts, clearing the screen, and printing the welcome message.
	cli				
	call 	clear_screen
	mov 	si, welcome_msg
	call 	print_string

	; Enable A20 gate
	call 	enable_a20
	
	; Halt CPU so it doesn't do weird things
	hlt

;; Function: enable_a20 ;;
;  Purpose: enables a20 gate
;  Arguments: None
enable_a20:

	; We are assuming the user is using QEMU so we basically are just making sure
	; A20 is enabled. If it isn't we just fail. There was no reliable way of disabling
	; it to try and turn it back on at the time of writing. A20 is enabled if AX = 1.
	call	check_a20
	cmp 	ax, 1
	je 	.done
	jmp 	.fail

.fail	mov 	si, a20_fail_msg
	call 	print_string
	hlt				; Halt CPU. Nothing to do if we dom't have A20...

.done	mov	si, a20_success_msg
	call	print_string
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

;; Function: print_string ;;
;  Purpose: Print string to screen
;  Arguments: SI: String to print
;  Clobbers: AH, AL
print_string:
	mov	ah, 0x0E	; Tell Bios we are going to print chars

.loop	lodsb			; Load single byte from SI
	or 	al, al		; Check if end of string
	jz 	.newl		; Return if end of string
	int 	0x10		; BIOS putc routine
	jmp 	.loop		; Loop until done printing

.newl	mov	al, 0xA		; Print newline
	int	0x10
	mov	al, 0xD		; Print carriage return
	int	0x10
	ret


;; Function: clear_screen ;;
;  Purpose: Clears the screen
;  Arguments: None
;  Clobbers: al, a
clear_screen:
	;; The simplest way to clear the screen in this context is to simply
	;; reset the screen resolution.
	mov 	ah, 0x00	; Tell BIOS we want to set video mode
	mov	al, 0x03	; Tell BIOS we want 80x25
	int	0x10		; Set video mode
	ret



;; Data Section
welcome_msg:	db "Chimera bootloader entered.", 0

a20_fail_msg:	db "Error: Could not enable A20 gate. Please use QEMU."
a20_success_msg: db "A20 Gate enabled.", 0

; Special BIOS stuff
times 	510 - ($ - $$) db 0	; BIOS expects bootsector to be 512 bytes, so we pad the file with 0's
dw 	0xAA55			; Magic number for BIOS

