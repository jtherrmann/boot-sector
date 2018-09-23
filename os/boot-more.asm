	%include "boot-more1.asm"

	bits 16

	;; https://stackoverflow.com/q/4903906/10402025
	;; org 0x7c00

	mov ax, 0x7c0
	mov ds, ax
	;; mov ax, 0x0000
	;; mov ds, ax

	jmp code

	;; Write bootStr to boot sector.
	bootStr db "AAA"

code:	

	;; for int 0x10
	mov ah, 0x0e

	;; Print first char of bootStr.
	mov di, bootStr
	mov BYTE al, [di]
	int 0x10	; prints A
	
	;; Load next sector.
	;; adapted from: https://blog.benjojo.co.uk/post/interactive-x86-bootloader-tutorial
        mov ah, 0x02
        mov al, 1   
        mov ch, 0    
        mov cl, 2    
        mov dh, 0   
        mov bx, new 
        mov es, bx  
        xor bx, bx
        int 0x13
        jmp new:0

        new equ 0x0500

	;; Pad boot sector.
	times 510-($-$$) db 0 
	db 0x55
	db 0xaa
