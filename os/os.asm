;;; LEFT OFF: incorporate changes from comments and answers (automatically
;;; compute kernel size, set up stack, etc.):
;;; https://stackoverflow.com/q/52463695/10402025
;;; Note: https://blog.benjojo.co.uk/post/interactive-x86-bootloader-tutorial
;;; has example of setting up the stack

;;; TODO: clean up, document; search for TODO in file
	
;;; https://www.cs.uaf.edu/2011/fall/cs301/lecture/11_18_bootblock.html
;;; compile:
;;; nasm -f bin -o boot.bin boot.asm

;;; https://opensourceforu.com/2017/06/hack-bootsector-write/
;;; test:
;;; qemu-system-x86_64 boot.bin

;;; for reference: https://wiki.osdev.org/Real_mode_assembly_I#So_where.27s_the_code.3F
	
	bits 16

	;; https://stackoverflow.com/q/52461308/10402025
	section boot, vstart=0x0000

	;; Load next sector.
	;; adapted from:
	;; https://blog.benjojo.co.uk/post/interactive-x86-bootloader-tutorial
        mov ah, 0x02
        mov al, 2
        mov ch, 0    
        mov cl, 2    
        mov dh, 0   
        mov bx, newsector 
        mov es, bx  
        xor bx, bx
        int 0x13
        jmp newsector:0

        newsector equ 0x0500

	times 510-($-$$) db 0
	db 0x55
	db 0xaa

	;; https://stackoverflow.com/q/52461308/10402025
	section os, vstart=0x0000
	mov ax, newsector
	mov ds, ax

	;; https://opensourceforu.com/2017/06/hack-bootsector-write/
	;; "Set DS (data segment base) as 0x7c0"
	;; mov ax, 0x7c0
	;; mov ds, ax
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

	mov di, repl_prompt
	mov BYTE [di+0], '>'
	mov BYTE [di+1], ' '
repl:
	mov di, repl_prompt
	call println

	mov di, input
	call getstr
	call execute_command
	jmp repl

;;; ---------------------------------------------------------------------------
;;; REPL (end)
;;; ---------------------------------------------------------------------------


;;; ===========================================================================
;;; USER COMMANDS
;;; ===========================================================================

;;; TODO: "help" command to list all commands

hello:
;;; Print "Hello, world!"
	jmp .print

	.str db "Hello, world!",0

	.print:
	mov di, .str
	call println
	ret

keymap:
;;; Toggle between QWERTY and Dvorak.
	jmp .start

	.qwertystr db "Layout: QWERTY",0
	.dvorakstr db "Layout: Dvorak",0

	.start:

	cmp BYTE [dvorak], 0
	je .dvorak

	mov di, .qwertystr
	call println
	mov BYTE [dvorak], 0
	ret

	.dvorak:

	mov di, .dvorakstr
	call println
	mov BYTE [dvorak], 1
	ret

me:
;;; Identify the user.
	jmp .start

	.str db "Who are you? ",0

	.start:

	mov di, .str
	call println

	mov di, input
	call getstr

	mov si, repl_prompt
	mov bx, 0
	jmp .test

	.loop:
	mov BYTE al, [di+bx]
	mov BYTE [si+bx], al
	inc bx

	.test:
	cmp BYTE [di+bx], 0
	jne .loop

	;; TODO: should stop writing at length 29 to leave room for:
	mov BYTE [si+bx+0], '>'
	mov BYTE [si+bx+1], ' '
	mov BYTE [si+bx+2], 0

	ret

reboot:
;;; Reboot.
	jmp .start

	.str1 db "See you soon!",0
	.str2 db "Press any key to reboot.",0

	.start:

	mov di, .str1
	call println

	mov di, .str2
	call print_newline
	call println

	;; Wait for a keypress.
	mov ah, 0
	int 0x16

	;; Reboot.
	;; source: https://stackoverflow.com/a/32686533
	db 0x0ea
	dw 0x0000
	dw 0xffff

;;; ---------------------------------------------------------------------------
;;; USER COMMANDS (end)
;;; ---------------------------------------------------------------------------


;;; ===========================================================================
;;; INTERNAL PROCEDURES
;;; ===========================================================================

getstr:
;;; Read a string from keyboard input.
;;; Pre: di points to an array.
;;; Post: di points to the same array, which now contains the string.
	mov bx, 0	; index
	.loop:
	
	;; read a char to al
	mov ah, 0
	int 0x16

	;; check for carriage ret (enter)
	cmp al, 0x0d
	je .return

	cmp BYTE [dvorak], 0
	je .skipdvorak

	push bx
	call convert_char
	pop bx

	.skipdvorak:

	;; add the char to the input array
	;; TODO: document:
	;; note: only certain regs can be used for indexing, di and bx both
	;; work
	;; also see: https://stackoverflow.com/a/12474190
	mov BYTE [di+bx], al

	;; print the char in al
	mov ah, 0x0e
	int 0x10

	inc bx
	jmp .loop

	.return:
	mov BYTE [di+bx], 0
	ret

convert_char:
;;; Convert a character from QWERTY to Dvorak.
;;; Pre: al contains the character as it was entered with QWERTY.
;;; Post: al contains the corresponding Dvorak character.

	;; chars <= 0x20 don't need conversion
	cmp al, 0x20
	jle .return

	;; movzx: https://stackoverflow.com/a/32836665/10402025
	movzx bx, al
	mov BYTE al, [dvorak_keymap+bx-0x21]

	.return:
	ret
	
execute_command:
;;; Call a user command.
;;; Pre: di contains a pointer to the command string.
	jmp .skipdata
	
	.hello_cmd db "hello",0
	.keymap_cmd db "keymap",0
	.me_cmd db "me",0
	.reboot_cmd db "reboot",0

	.skipdata:

	mov si, .hello_cmd
	call compare_strings
	cmp ax, 0
	je .skiphello
	call hello
	ret
	.skiphello:

	mov si, .keymap_cmd
	call compare_strings
	cmp ax, 0
	je .skipkeymap
	call keymap
	ret
	.skipkeymap:

	mov si, .me_cmd
	call compare_strings
	cmp ax, 0
	je .skipme
	call me
	ret
	.skipme:

	mov si, .reboot_cmd
	call compare_strings
	cmp ax, 0
	je .skipreboot
	call reboot
	ret
	.skipreboot:

	call invalid_command
	ret
	
invalid_command:
;;; Handle an invalid user command.
	jmp .print

	.str db "Invalid command.",0

	.print:
	mov di, .str
	call println
	ret

compare_strings:
;;; Compare two strings.
;;; Pre: di and si contain pointers to the strings.
;;; Post: ax contains 1 if the strings are equal and 0 otherwise.
	mov bx, 0

	.loop:

	mov BYTE al, [si+bx]
	cmp BYTE [di+bx], al
	jne .false

	cmp BYTE [di+bx], 0
	je .true

	inc bx
	jmp .loop

	.true:
	mov ax, 1
	ret

	.false:
	mov ax, 0
	ret

print:
;;; Print a string.
;;; Pre: di contains a pointer to the beginning of the string.
	mov ah, 0x0e

	mov bx, 0
	jmp .test

	.loop:

	mov BYTE al, [di+bx]
	int 0x10
	inc bx

	.test:

	cmp BYTE [di+bx], 0
	jne .loop
	
	ret

println:
;;; Print a string on a new line.
;;; Pre: di contains a pointer to the beginning of the string.
	call print_newline
	call print
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
;;; INTERNAL PROCEDURES (end)
;;; ---------------------------------------------------------------------------


;;; ===========================================================================
;;; DATA
;;; ===========================================================================

	;; TODO: extent input after implement automatic kernel size calculation
	input times 32 db 0
	repl_prompt times 32 db 0

	dvorak db 1

dvorak_keymap:
	db "!_#$%&-()*}w[vz0123456789SsW]VZ@AXJE>UIDCHTNMBRL",0x22,"POYGK<QF:/"
	db "\=^{`axje.uidchtnmbrl'poygk,qf;?|+~",0x7f
