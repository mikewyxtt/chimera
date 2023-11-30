section .text
    global _start

_start:
    ; Set up the stack (assuming you have a stack section defined)
;    mov esp, stack_top

    ; Call the main function
    call main

    ; Halt the system
    hlt

main:
	
	MOV 	ESI, HelloMsg
	CALL	PRINT

    	; Infinite loop
	JMP $

PRINT:
	MOV	EDI, 0xB8000			; VGA text console is memory mapped to 0xB8000

.LOOP:	LODSB					; Load byte from SI into AL
	OR	AL, AL				; If it's zero:
	JZ	.DONE				; end of string, we can leave
	MOV	BYTE [EDI], AL			; Otherwise load the char into the buffer
	ADD	EDI, 2				; Each entry is 16 bits, 8 bits color + 8 bits ascii, jump to next char
	JMP	.LOOP				; Loop

.DONE: RET

SECTION .data
HelloMsg: db "Hello", 0x00


section .bss
    ; Define a stack section (adjust size as needed)
    stack resb 8192

section .multiboot
    ; Multiboot header
    dd 0x1BADB002    ; Magic number
    dd 0x00          ; Flags
    dd - (0x1BADB002 + 0x00)  ; Checksum

section .header
    ; ELF header
    dd 0x464C457F    ; ELF "magic number"
    db 0x01          ; 32-bit
    db 0x01          ; Little endian
    db 0x01          ; ELF version
    times 9 db 0     ; Padding
    dw 2             ; Executable type
    dw 0x03          ; Machine architecture (x86)
    dd 1             ; ELF version
    dd _start        ; Entry point
    dd _start        ; Program header offset
    dd 0             ; Section header offset
    dd 0             ; Flags
    dw 0x34          ; ELF header size
    dw 0x20          ; Program header size
    dw 1             ; Number of program headers
    dw 0             ; Section header size
    dw 0             ; Number of section headers
    dw 0             ; Section header table index
