;org 0x100000
;bits 32

extern main					; main() function of the loader


section .text

global _start
_start:
	call	CLEAR

	mov	byte [0xb8000], 'y'
	mov	byte [0xb8002], 'o'
	mov	byte [0xb8004], 'o'
	mov	byte [0xb8006], 'o'

	CALL	main

	jmp $



CLEAR:
	mov	ecx, 2000			; 80 * 25
	xor	eax, eax

.loop	mov 	byte [0xb8000 + eax], ' '	; fill with spaces
	add	eax, 0x02			; each character entry is 2 bytes
	loop	.loop				; loop through each char in vram

.done	mov	byte [cursor_pos], 0		; reset the cursor
	ret					; return to caller


cursor_pos	db	0x0


HELLOSTR:	db	"HELLOOOO!"
