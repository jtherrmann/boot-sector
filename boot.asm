;;; https://www.cs.uaf.edu/2011/fall/cs301/lecture/11_18_bootblock.html
;;; compile:
;;; nasm -f bin -o boot.bin boot.asm

;;; https://opensourceforu.com/2017/06/hack-bootsector-write/
;;; test:
;;; qemu-system-x86_64 boot.bin

;;; for reference: https://wiki.osdev.org/Real_mode_assembly_I#So_where.27s_the_code.3F
	
	bits 16

	;; https://opensourceforu.com/2017/06/hack-bootsector-write/
	;; "Set DS (data segment base) as 0x7c0"
	mov ax, 0x7c0
	mov ds, ax
	;; Also see:
	;; https://www.cs.uaf.edu/2011/fall/cs301/lecture/11_18_bootblock.html
	;; "Segmented Memory" section at bottom of page.
	;;
	;; without the above lines, printing chars with the BIOS interrupt (see
	;; `print`) just outputs random chars, not the ones in our string

;;; ===========================================================================
;;; REPL
;;; ===========================================================================

;;; NEXT: compare strings procedure (di points to one, si to the other)
;;; use this to implement commands
;;; can have a command lookup table; series of pointer pairs; for each pair, first
;;; points to a pointer to a str, second points to a pointer to a procedure;
;;; for each first pointer, check if its string is the command, and if it is then
;;; call the second pointer's procedure, otherwise go to the next pointer pair
;;; can have a procedure for doing this, that just takes a pointer to the command string
;;; and promises to call the command matching that string (or a catch-all command for
;;; when the command string is invalid)

repl:
	call reset_input

	mov di, repl_prompt
	call print
	
	mov di, input		; start of input array
	mov bx, 0		; index

	.read_char:
	
	;; read a char to al
	mov ah, 0
	int 0x16

	;; check for carriage ret (enter)
	cmp al, 0x0d
	je .eval_print

	;; add the char to the input array
	;; note: only certain regs can be used for indexing, di and bx both
	;; work
	mov [di+bx], al

	;; print the char in al
	mov ah, 0x0e
	int 0x10
	
	inc bx
	jmp .read_char

	.eval_print:
	
	call print_newline

	mov di, input
	call print

	call print_newline
	call print_newline
	
	jmp repl

;;; ---------------------------------------------------------------------------
;;; REPL (end)
;;; ---------------------------------------------------------------------------


;;; ===========================================================================
;;; DATA
;;; ===========================================================================

	input times 32 db 0	; user input array
	repl_prompt db "> ",0	; repl prompt


;;; ===========================================================================
;;; PROCEDURES
;;; ===========================================================================

reset_input:
;;; Fill the input array with 0s.
	mov di, input
	mov bx, 0

	jmp .test

	.start:

	mov BYTE [di+bx], 0
	inc bx

	.test:

	cmp bx, 32
	jl .start

	ret

print:
;;; Print a string.
;;; Pre: di contains a pointer to the beginning of the string.
	mov ah, 0x0e

	jmp .test

	.start:

	mov al, [di]
	int 0x10
	inc di

	.test:

	cmp BYTE [di], 0
	jne .start
	
	ret

print_newline:
;;; Move the cursor to the beginning of the next line.
	mov ah, 0x0e
	mov al, 0x0d	; carriage ret
	int 0x10
	mov al, 0x0a	; newline
	int 0x10
	ret

;;; ---------------------------------------------------------------------------
;;; PROCEDURES (end)
;;; ---------------------------------------------------------------------------

	times 512-2-($-$$) db 0
	db 0x55
	db 0xaa
