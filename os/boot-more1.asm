	bits 16

	;; https://stackoverflow.com/q/4903906/10402025
	org 0x7c00

	;; mov ax, 0x7c0
	;; mov ds, ax
	mov ax, 0x0000
	mov ds, ax

nextSector:	

	;; for int 0x10
	mov ah, 0x0e

	;; Try to print first char of nextStr (defined below).
	mov di, nextStr
	mov BYTE al, [di]
	int 0x10	; should print B

	;; Move 'C' into nextStr and print it.
	mov BYTE [di], 'C'
	mov BYTE al, [di]
	int 0x10	; prints C

	;; Print first char of bootStr again.
	mov di, bootStr
	mov BYTE al, [di]
	int 0x10	; prints A

	hlt

	nextStr db "BBB"
