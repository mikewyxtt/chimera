
extern main					; main() function of the loader


section .text

global _start
_start:
	mov	ESP, 0x100000
	mov	EBP, ESP

	CALL	main

	jmp $



section .data


