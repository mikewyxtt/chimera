org 0x8000
bits 16


start:
	mov	si, HELLOSTR
	mov	ah, 0x0E

.loop	lodsb
	or	al, al
	jz	.done
	int	0x10
	jmp	.loop

.done	jmp $


HELLOSTR:	db	"HELLOOOO!"
