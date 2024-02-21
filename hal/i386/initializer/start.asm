; start.asm
; Multiboot2 compliant initializer entry stub

[BITS 32]
SECTION .multiboot
ALIGN 4
DD 0xE85250D6                           ; Magic Number
DD 0x00                                 ; Architecture (0 is 32 bit protected mode)
DD 0x10                                 ; Header length (16 bytes)
DD -(0xE85250D6 + 0x00 + 0x10)          ; Checksum

;; Tags

;; End tags
;DW 0x00         ; Tag type
;DW 0x00         ; Flags
;DD 0x08         ; Size


SECTION .text

GLOBAL _start
_start:
    cli                                 ; Disable Interupts
    mov     BYTE [0xB8000], 'H'
    mov     BYTE [0xB8002], 'E'
    mov     BYTE [0xB8004], 'L'
    mov     BYTE [0xB8006], 'L'
    mov     BYTE [0xB8008], 'O'
    mov     BYTE [0xB800A], '!'

.loop:                                  ; Infinite loop
    hlt
    jmp     .loop