ORG 0x9C00			; Address in memory where our MBR loads us
BITS 16				; We are still in 16 bit real mode

;; MBR read us off the disk and loaded us into memory. We are no longer confined to the 440 byte
;; code size limit so we can continue initializing the hardware and eventually load the kernel.
;; A20 gate was enabled by MBR.
;;
;; Execution begins here:
START:
	;; Setup the stack
	MOV	AX, 0x00	; Set AX to 0, presumably bc you cant directly assign integers to those registers
	MOV	DS, AX		; ?
	MOV	ES, AX		; ?
	MOV	SS, AX		; Setup stack segment
	MOV	SP, 0x9C00	; Stack grows downwards from 0x9C00
	
	MOV 	SI, WelcomeMSG
	CALL 	print_string	; Display welcome message
	
;	CALL	setup_gdt	; Setup/load GDT

	HLT			; Halt CPU, but this should never execute.

;;
;; FUNCTION: 
;;	LOAD_SYSTEM()
;;
;; ARGUMENTS:
;;	NONE.
;;
;; PURPOSE:
;;	LOADS KERNEL AND INITRD INTO MEMORY
;;
LOAD_SYSTEM:
	;; FIND THE KERNEL ON THE FILESYSTEM AND LOAD IT INTO MEMORY

	;; FIND THE INITRD ON THE FILESYSTEM AND LOAD IT INTO MEMORY

	RET



;;
;; FUNCTION:
;;	UNPACK_INITRD()
;;
;; ARGUMENTS:
;;	NONE.
;;
;; PURPOSE:
;;	UNPACKS INITRD AND PUTS SERVERS INTO APPROPRIATE MEMORY REGIONS
;;
UNPACK_INITRD:
	;; UNPACK INITRD ROUTINE. TBH, THIS DOESN'T REQUIRE ANY BIOS FUNCTIONS AND COULD EASILY BE DONE
	;; IN 32 BIT MODE


	RET




BITS 32				; Tell NASM we are using 32 bit instructions moving forward
bootstrap_final:
	;; This function is called after setup_gdt complets. At this point we are in
	;; 32 bit protected mode with a20 gate enabled and the GDT loaded up.
	;;
	
	HLT			; Halt the CPU

;;
;; Data Section 
;;
WelcomeMSG:	db "Chimera bootstrap entered.", 0


;; Includes ;;
%INCLUDE "drivers/console.inc"	; Include stuff to do console I/O
%INCLUDE "bootstrap/gdt.inc"	; Include GDT setup functions

;TIMES 	512 - ($ - $$) DB 0
