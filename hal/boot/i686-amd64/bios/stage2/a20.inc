; Functions to assist in dealing with the A20 gate

;;
;; Function: 
;;	ENABLE_A20()
;;
;; Purpose: 
;;	Enables A20 gate
;;
;; Arguments: 
;;	None
;;
ENABLE_A20:

	; We are assuming the user is using QEMU so we basically are just making sure
	; A20 is enabled. If it isn't we just fail. There was no reliable way of disabling
	; it to try and turn it back on at the time of writing. A20 is enabled if AX = 1.
	call	check_a20
	cmp 	ax, 1
	je 	.done
	jmp 	.fail

.fail	mov 	si, a20_fail_msg
	call 	PRINTLN
	hlt				; Halt CPU. Nothing to do if we dom't have A20...

.done	mov	si, a20_success_msg
	call	PRINTLN
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

;; Data Section
a20_fail_msg:	db "Error: Could not enable A20 gate. Please use QEMU."
a20_success_msg: db "A20 Gate enabled.", 0

