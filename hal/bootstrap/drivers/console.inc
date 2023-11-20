; Functions that assist with console I/O
bits 16

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


;; Function: clear_screen() ;;
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
; bits 32

