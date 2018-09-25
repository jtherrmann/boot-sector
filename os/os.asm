;;; TODO: incorporate changes from comments and answers:
;;; https://stackoverflow.com/q/52463695/10402025

;;; TODO: clean up, document; search for TODO, FIXME in file; also search for other
;;; stuff to be addressed/cleaned up that's not marked with TODO or FIXME

;;; TODO: format docstrings like Args and Returns rather than pre/post

;;; TODO: rename REPL to shell; organize funcs into general internal utilities,
;;; single-procedure user commands, and multi-procedure user commands (or call
;;; them applications), such as calc (the main REPL plus all the helper funcs);
;;; the shell is just another application (that gets executed at startup); move
;;; REPL section into a shell procedure and execute it at startup

;;; TODO: check for correct usage of "operator" and "operand".

;;; TODO: use movzx for zero extension wherever there's a move from smaller reg into larger reg
;;; use movsx for sign extension, if needed
;;; https://stackoverflow.com/a/32836665/10402025

;;; TODO: convert all mov <reg>, 0 to xor <reg>, <reg>? what's the difference?

;;; TODO: comment all procedures thoroughly

;;; TODO: view in another editor, esp. for tab formatting, esp. for trailing
;;; ; comments, esp. those that are supposed to be 1 or 2 spaces away from
;;; the end of the line
	
;;; TODO: clean up
;;; https://www.cs.uaf.edu/2011/fall/cs301/lecture/11_18_bootblock.html
;;; compile:
;;; nasm -f bin -o boot.bin boot.asm

;;; TODO: clean up
;;; https://opensourceforu.com/2017/06/hack-bootsector-write/
;;; test:
;;; qemu-system-x86_64 boot.bin

;;; TODO:
;;; start with an insert mode key procedure lookup table; chars such as
;;; backspace and arrow keys are mapped to behaviors (delete back, move around)
;;; and when getstr gets one of these chars it looks up the corresponding
;;; procedure (hopefully just using char as index into procedure array) and
;;; calls it (otherwise just prints the char); note: as long as left arrow key
;;; corresponds to an ascii char then it could be implemented with a backspace
;;; (w/o delete); next step after the insert mode key table would be a normal
;;; mode key table for hjkl, etc.
;;; 
;;; https://wiki.osdev.org/Text_Mode_Cursor#Moving_the_Cursor int 0x10/ah=0x02
;;; looks like you probably have to get row & col ("Get Cursor Data") then move
;;; to e.g. the next col, same row (to move right) or prev col (to move left)
;;; 
;;; https://en.wikipedia.org/wiki/INT_10H "Set text-mode cursor shape"

	BITS 16

	;; TODO: apply Michael Petch's boot loader tips:
	;; https://stackoverflow.com/a/32705076/10402025
	;; linked from: https://stackoverflow.com/a/34095896/10402025
	;; also referenced from:
	;; https://blog.benjojo.co.uk/post/interactive-x86-bootloader-tutorial
	
;;; ===========================================================================
;;; Boot loader
;;; ===========================================================================

	;; Special thanks to Michael Petch for his help with the boot loader!
	;; https://stackoverflow.com/users/3857942/michael-petch

	;; General sources:
	;; - https://blog.benjojo.co.uk/post/interactive-x86-bootloader-tutorial
	;; - https://en.wikipedia.org/wiki/INT_13H#INT_13h_AH=02h:_Read_Sectors_From_Drive
	;; - https://wiki.osdev.org/Real_Mode#The_Stack
	;; - Michael Petch:
	;;   - https://stackoverflow.com/q/52461308/10402025
	;;   - https://stackoverflow.com/q/52463695/10402025

	section boot, vstart=0x0000

	os_load_start equ 0x0060

	;; Set up the stack above where the OS is loaded.
	;; 
	;; Set SS (the Stack Segment register) to os_load_start and SP
	;; (the Stack Pointer) to 0x0000 so that the stack grows down from
	;; os_load_start:0xFFFF toward the "top" of the stack at
	;; os_load_start:0x0000.
	mov ax, os_load_start
	mov ss, ax
	xor sp, sp

	;; Number of 512B sectors to read from the drive.
        mov al, (os_end-os_start+511)/512

        mov ch, 0  ; cylinder
        mov cl, 2  ; starting sector
        mov dh, 0  ; drive head

	;; Set ES (the Extra Segment register) to os_load_start and BX to
	;; 0x0000 so that we load the sectors into memory starting at
	;; os_load_start:0x0000.
	;; 
	;; https://stackoverflow.com/a/32705076/10402025
	;; http://www.c-jump.com/CIS77/ASM/Memory/lecture.html#M77_0120_reg_names
        mov bx, os_load_start 
        mov es, bx  
        xor bx, bx

	;; Read the sectors.
        mov ah, 0x02
        int 0x13

	;; Far jump to os_load_start:0x0000.
	;; 
	;; Set CS (the Code Segment register) to os_load_start and the
	;; instruction pointer to 0x0000, so the CPU begins executing
	;; instructions at os_load_start:0x0000.
	;; 
	;; https://wiki.osdev.org/Segmentation#Far_Jump
	;; https://stackoverflow.com/a/47249973/10402025
	;; http://www.c-jump.com/CIS77/ASM/Memory/lecture.html#M77_0120_reg_names
        jmp os_load_start:0x0000

	;; Pad boot sector to boot signature.
	times 510-($-$$) db 0
	db 0x55
	db 0xaa

;;; ---------------------------------------------------------------------------
;;; Boot loader (end)
;;; ---------------------------------------------------------------------------


	section os, vstart=0x0000
os_start:	

	;; Set DS (the Data Segment register) to os_load_start.
	;; 
	;; Michael Petch: https://stackoverflow.com/q/52461308/10402025
	;; http://www.c-jump.com/CIS77/ASM/Memory/lecture.html#M77_0120_reg_names
	mov ax, os_load_start
	mov ds, ax

;;; ===========================================================================
;;; REPL
;;; ===========================================================================

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


;;; ===========================================================================
;;; Shell commands
;;; ===========================================================================

;;; TODO: prefix names w/ something like cmd_? (or _cmd suffix)

calc:
;;; Start the calculator.
	call calculator
	ret

hello:
;;; Print "Hello, world!"
	push di  ; save
	jmp .print

	.str db "Hello, world!",0

	.print:
	mov di, .str
	call println

	pop di  ; restore
	ret

help:
;;; Print a list of commands.

	;; save
	push ax
	push bx
	push di

	xor ax, ax
	mov bx, command_table
	jmp .test

	.loop:

	mov WORD di, [bx]
	call println

	;; Advance to the next command string.
	add bx, 4
	inc ax

	.test:

	cmp ax, [help_list_len]
	jl .loop

	;; restore
	pop di
	pop bx
	pop ax

	ret

keymap:
;;; Toggle between QWERTY and Dvorak.
	push di  ; save
	jmp .start

	.qwertystr db "Layout: QWERTY",0
	.dvorakstr db "Layout: Dvorak",0

	.start:

	cmp BYTE [dvorak], 0
	je .dvorak

	mov di, .qwertystr
	call println
	mov BYTE [dvorak], 0
	jmp .return

	.dvorak:

	mov di, .dvorakstr
	call println
	mov BYTE [dvorak], 1

	.return:
	pop di  ; restore
	ret

me:
;;; Identify the user.

	;; save
	push ax
	push bx
	push di
	push si

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

	mov BYTE [si+bx+0], '>'
	mov BYTE [si+bx+1], ' '
	mov BYTE [si+bx+2], 0

	;; restore
	pop si
	pop di
	pop bx
	pop ax

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
	;; https://stackoverflow.com/a/32686533
	db 0x0ea
	dw 0x0000
	dw 0xffff
	
invalid_command:
;;; Handle an invalid user command.
	push di  ; save
	jmp .print

	.str db "Invalid command.",0

	.print:
	mov di, .str
	call println

	pop di  ; restore
	ret

;;; ---------------------------------------------------------------------------
;;; Shell commands (end)
;;; ---------------------------------------------------------------------------

;;; TODO: better headings; maybe just exactly like RST:
;;; http://docutils.sourceforge.net/docs/user/rst/quickstart.html

;;; ===========================================================================
;;; Applications
;;; ===========================================================================

;;; TODO: Calculator heading
;;; TODO: diff name; vlc? (very lazy calculator; e.g. if no negatives, esp. if
;;; no nums outside 0-9

;;; TODO: method of exiting; welcome message w/ info about RPN/postfix
;;; in welcome message include limitations (e.g. max/min values for a number)
calculator:
;;; Calculator.
	push di  ; save

	.loop:

	mov di, calc_prompt
	call println

	mov di, input
	call getstr
	call calc_eval
	jmp .loop

	pop di  ; restore
	ret

;;; TODO: should not print result when too many operands
;;; TODO: double check all sizes, and in helper funcs
;;; TODO: detect & handle numbers out of bounds (e.g. there are conditional
;;; overflow jump instructions; maybe in lecture notes?)
calc_eval:
;;; Evaluate a calculator expression.
;;; Pre: di points to the input string.

	;; save
	push ax
	push bx
	push cx
	push di
	push dx
	push si

	;; Note that the stack stores one-word (two-byte) items.
	;; https://wiki.osdev.org/Real_Mode#The_Stack

	;; Stack offset (starts at 0B).
	;; Increases 2B with each push and decreases 2B with each pop.
	;; Should never be allowed to fall below 0B.
	xor bx, bx

	jmp .test

	.toofewstr db "Too few operands.",0
	.toomanystr db "Too many operands.",0

	.loop:

	;; Check if the current input char represents a number 0-9.
	;; If not, treat the char as an operator.
	cmp BYTE [di], 0x30
	jl .operator
	cmp BYTE [di], 0x39
	jg .operator

	;; Convert the number from char to int and push it, then jump to the
	;; end of the loop.
	;; 
	;; Use movzx to zero-extend the high bits of cx.
	;; https://stackoverflow.com/a/32836665/10402025
	movzx WORD cx, [di]
	sub cx, 0x30
	push cx
	add bx, 2  ; Stack offset increases by 2B.
	jmp .inc

	.operator:
	;; Treat the current input char as an operator.

	;; Exit the loop if we've encountered the operator while number of
	;; operands on the stack < 2 (stack offset < 4B).
	cmp bx, 4
	jl .toofew

	mov BYTE dl, [di]  ; operator

	pop si  ; second operand
	pop cx  ; first operand
	sub bx, 4  ; Stack offset decreases by 4B.

	push di  ; Save input string pointer.
	mov di, cx
	call apply_operator
	pop di  ; Restore input string pointer.

	;; Push the result.
	push ax
	add bx, 2  ; Stack offset increases by 2B.

	;; Advance to the next input char.
	.inc:
	inc di

	;; Check the input string for the null-terminator.
	.test:
	cmp BYTE [di], 0
	jne .loop

	;; Done reading the input string (exit the loop).

	;; Pop the final result and print it.
	pop ax
	sub bx, 2  ; Stack offset decreases by 2B.
	call println_num

	;; Error if number of operands remaining on the stack > 0 (stack offset
	;; > 0B).
	cmp bx, 0
	jg .toomany

	;; The stack offset was 0B as expected.
	jmp .return

	;; Encountered an operator with too few operands. Fix the stack and
	;; notify the user.
	.toofew:
	add sp, bx  ; Add the stack offset to the stack pointer.
	mov di, .toofewstr
	call println
	jmp .return

	;; The expression contains too many operands. Fix the stack and notify
	;; the user.
	.toomany:
	add sp, bx  ; Add the stack offset to the stack pointer.
	mov di, .toomanystr
	call println

	;; The stack offset is back to 0B, either because the expression
	;; contained properly balanced operators and operands or because we
	;; fixed the stack.
	.return:

	;; restore
	pop si
	pop dx
	pop di
	pop cx
	pop bx
	pop ax

	ret

println_num:
;;; Print a number preceded by a newline.
;;; Pre: ax contains the number.
;;;
;;; Only accurately prints numbers in the range -32768 <= n < 32768.
	call print_newline
	call print_num
	ret

print_num:
;;; Print a number.
;;; Pre: ax contains the number.
;;;
;;; Only accurately prints numbers in the range -32768 <= n < 32768.

	;; save
	push ax
	push bx
	push cx
	push dx

	cmp ax, 0
	jge .positive

	;; ax < 0

	;; Set ax to its absolute value.
	xor bx, bx
	sub bx, ax
	mov ax, bx

	;; Print a minus sign.
	push ax  ; Save our number.
	mov BYTE al, '-'
	mov ah, 0x0e
	int 0x10
	pop ax  ; Restore our number.

	.positive:

	;; ax >= 0

	;; Number of digits pushed onto the stack.
	xor cx, cx

	.parseloop:
	
	;; div divides dx:ax by the operand. ax stores the quotient and dx
	;; stores the remainder.
	;; source: https://stackoverflow.com/a/8022107/10402025

	;; Divide our number (ax) by 10.
	xor dx, dx
	mov bx, 10
	div bx

	;; Convert the remainder from int to char and push it.
	add dx, 0x30
	push dx
	inc cx  ; Number of digits pushed onto the stack.

	;; Continue the loop if the quotient != 0.
	cmp ax, 0
	jne .parseloop

	;; Done parsing the number. Now print it:

	jmp .test
	.printloop:

	;; Pop a digit and print it.
	pop dx
	mov BYTE al, dl
	mov ah, 0x0e
	int 0x10

	dec cx  ; Number of digits left on the stack.

	.test:
	cmp cx, 0
	jg .printloop

	;; restore
	pop dx
	pop cx
	pop bx
	pop ax

	ret

;;; TODO: operator lookup table
;;; TODO: more operators: / and * at least
apply_operator:
;;; Apply an operator to two operands.
;;; Pre: di contains the first operand, si the second operand, and dl the
;;; operator.
;;; Post: ax contains the result.
	push di  ; save
	jmp .start

	.errorstr db "Invalid operand.",0

	.start:

	cmp BYTE dl, '+'
	jne .skipadd
	call add_op
	jmp .return
	.skipadd:

	cmp BYTE dl, '-'
	jne .error
	call sub_op
	jmp .return

	;; TODO: why does it crash here?
	.error:
	mov di, .errorstr
	call println

	.return:
	pop di  ; restore
	ret

add_op:
;;; Addition operator.
;;; Pre: di contains the first operand and si the second operand.
;;; Post: ax contains di + si.
	mov ax, di
	add ax, si
	ret

sub_op:
;;; Subtraction operator.
;;; Pre: di contains the first operand and si the second operand.
;;; Post: ax contains di - si.
	mov ax, di
	sub ax, si
	ret

;;; ---------------------------------------------------------------------------
;;; Applications (end)
;;; ---------------------------------------------------------------------------


;;; ===========================================================================
;;; Internal procedures
;;; ===========================================================================

getstr:
;;; Read a string from keyboard input.
;;; Pre: di points to an array.
;;; Post: di points to the same array, which now contains the string.

	;; save
	push ax
	push bx

	;; TODO: add support for backspace; and ideally support for C-h
	;; (translate to backspace) and C-m (translate to carriage ret)

	mov bx, 0	; index
	.loop:
	
	;; read a char to al
	mov ah, 0
	int 0x16

	;; check for carriage ret (enter)
	cmp al, 0x0d
	je .return

	cmp BYTE [dvorak], 0
	je .skipconvert

	call convert_char

	.skipconvert:

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

	;; Append the null-terminator.
	mov BYTE [di+bx], 0

	;; restore
	pop bx
	pop ax

	ret

convert_char:
;;; Convert a character from QWERTY to Dvorak.
;;; Pre: al contains the character as it was entered with QWERTY.
;;; Post: al contains the corresponding Dvorak character.
	push bx  ; save

	;; chars < 0x21 don't need conversion
	cmp al, 0x21
	jl .return

	;; Use movzx to zero-extend the high bits of bx.
	;; https://stackoverflow.com/a/32836665/10402025
	movzx bx, al
	mov BYTE al, [dvorak_keymap+bx-0x21]

	.return:
	pop bx  ; restore
	ret

execute_command:
;;; Call a command given an input string.
;;; Pre: di points to the input string.

	;; save
	push bx
	push si

	mov bx, command_table
	jmp .test

	.loop:

	;; Advance to the next command string.
	add bx, 4

	.test:

	;; Compare the current command string with the input string.
	mov WORD si, [bx]
	call compare_strings

	;; Loop if the strings are not equal.
	cmp ax, 0
	je .loop

	;; The command and input strings are equal, so call the procedure that
	;; follows the command string in the table.
	add bx, 2
	call [bx]

	;; restore
	pop si
	pop bx

	ret

compare_strings:
;;; Compare two strings.
;;; Pre: di and si contain pointers to the strings.
;;; Post: ax contains 1 if the strings are equal and 0 otherwise.
	push bx  ; save

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
	jmp .return

	.false:
	mov ax, 0

	.return:
	pop bx  ; restore
	ret

print:
;;; Print a string.
;;; Pre: di contains a pointer to the beginning of the string.

	;; save
	push ax
	push bx
	
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

	;; restore
	pop bx
	pop ax
	
	ret

println:
;;; Print a string on a new line.
;;; Pre: di contains a pointer to the beginning of the string.
	call print_newline
	call print
	ret

print_newline:
;;; Move the cursor to the start of the next line.
	push ax  ; save

	mov ah, 0x0e
	mov al, 0x0d  ; carriage ret
	int 0x10

	mov al, 0x0a  ; newline
	int 0x10

	pop ax  ; restore
	ret

;;; ---------------------------------------------------------------------------
;;; Internal procedures (end)
;;; ---------------------------------------------------------------------------


;;; ===========================================================================
;;; Data
;;; ===========================================================================

	input times 256 db 0
	repl_prompt times 32 db 0
	calc_prompt db "calc> ",0

	dvorak db 1

dvorak_keymap:
	db "!_#$%&-()*}w[vz0123456789SsW]VZ@AXJE>UIDCHTNMBRL",0x22,"POYGK<QF:/"
	db "\=^{`axje.uidchtnmbrl'poygk,qf;?|+~",0x7f

	;; TODO: double-check that this value is correct
	;; The help command prints the first help_list_len commands from the
	;; command table. Commands located after this position are meant to be
	;; discovered by the user. :)
	help_list_len dw 6

;;; Command strings:

	calc_str db "cc",0	; TODO
	hello_str db "hello",0
	help_str db "help",0
	keymap_str db "keymap",0
	me_str db "me",0
	reboot_str db "reboot",0

	;; Commands not listed by help:

	sos_str db "...---...",0

command_table:
	dw calc_str
	dw calc

	dw hello_str
	dw hello

	dw help_str
	dw help

	dw keymap_str
	dw keymap

	dw me_str
	dw me

	dw reboot_str
	dw reboot

	;; Commands not listed by help:

	dw sos_str
	dw help

	;; Allows execute_command to always call invalid_command if the input
	;; string does not match any of the above command strings.
	dw input
	dw invalid_command

os_end:	
