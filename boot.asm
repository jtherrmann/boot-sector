;;; https://www.cs.uaf.edu/2011/fall/cs301/lecture/11_18_bootblock.html
;;; compile:
;;; nasm -f bin -o boot.bin boot.asm

;;; https://opensourceforu.com/2017/06/hack-bootsector-write/
;;; test:
;;; qemu-system-x86_64 boot.bin
	
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

	;; get char
	;; from https://wiki.osdev.org/Real_mode_assembly_I#So_where.27s_the_code.3F
	mov ah, 0
	int 0x16
	cmp al, 0x08
	jne call_print

print_back:
	mov di, backstr
	call print
	jmp hang
call_print:	
	;; does work:
	;; push str
	;; pop di

	mov di, str
	call print

hang:
	jmp hang

;;; ===========================================================================
;;; PRINT: start def
;;; ===========================================================================
;;;
;;; Print a string.
;;; Pre: di contains a pointer to the beginning of the string.
print:
	mov ah, 0x0e

	jmp print_test
print_start:	
	mov al, [di]
	int 0x10
	inc di
print_test:
	cmp BYTE [di], 0
	jne print_start
	
	ret
;;; ===========================================================================
;;; PRINT: end def
;;; ===========================================================================

str:
	db "Hello, world!",0
backstr:
	db "backspace",0

	times 512-2-($-$$) db 0
	db 0x55
	db 0xaa
