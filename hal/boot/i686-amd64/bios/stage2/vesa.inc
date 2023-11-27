;; Functions that assist with console I/O

;;
;; Function: 
;;	PRINTLN()
;;
;; Arguments:
;;	SI: String to print
;;
;; Purpose:
;;	Print string to screen
;;
;; Clobbers:
;;	AH, AL
;;
PRINTLN:
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

;;
;; Function:
;;	CLEAR_SCREEN()
;;
;; Arugments:
;;	None.
;;
;; Purpose: 
;;	Clears the screen
;; 
;; Clobbers:
;;	AL, AH
;;
;CLEAR_SCREEN:
;	;; The simplest way to clear the screen in this context is to simply
;	;; reset the screen resolution.
;	mov 	ah, 0x00	; Tell BIOS we want to set video mode
;	mov	al, 0x03	; Tell BIOS we want 80x25
;	int	0x10		; Set video mode
;	ret

