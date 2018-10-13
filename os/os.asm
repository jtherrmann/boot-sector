	%define break 0x0d, 0x0a
	%define line(str) db str,break

	BITS 16
	
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

	section os, vstart=0x0000
os_start:	

	;; Set DS (the Data Segment register) to os_load_start.
	;; 
	;; Michael Petch: https://stackoverflow.com/q/52461308/10402025
	;; http://www.c-jump.com/CIS77/ASM/Memory/lecture.html#M77_0120_reg_names
	mov ax, os_load_start
	mov ds, ax

	call shell


;;; ===========================================================================
;;; Shell
;;; ===========================================================================

shell:	
;;; Run the shell.
	jmp .start

	.welcome_str:
	line("Welcome!")
	line("Run 'help' for a list of commands.")
	db 0

	.start:

	mov di, .welcome_str
	call println

	mov di, shell_prompt
	mov BYTE [di+0], '>'
	mov BYTE [di+1], ' '

	.loop:

	mov di, shell_prompt
	call println

	mov di, input_buffer
	call getstr

	;; Check for empty input.
	cmp BYTE [di], 0
	je .loop

	call execute_command
	jmp .loop

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

;;; ---------------------------------------------------------------------------
;;; Shell commands
;;; ---------------------------------------------------------------------------

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

	;; Print the current command string.
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

	cmp BYTE [dvorak_mode], 0
	je .dvorak

	mov di, .qwertystr
	call println
	mov BYTE [dvorak_mode], 0
	jmp .return

	.dvorak:

	mov di, .dvorakstr
	call println
	mov BYTE [dvorak_mode], 1

	.return:
	pop di  ; restore
	ret

me:
;;; Identify the user by changing the shell prompt to their name.

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

	;; Get their name.
	mov di, input_buffer
	call getstr

	mov si, shell_prompt
	mov bx, 0
	jmp .test

	;; Copy their name from the input buffer to the shell prompt.
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
;;; Shell data
;;; ---------------------------------------------------------------------------

	shell_prompt times 32 db 0

	;; The help command prints the first help_list_len commands from the
	;; command table. Commands located after this position are meant to be
	;; discovered by the user. :)
	help_list_len dw 6

;;; Command strings:

	calc_str db "calc",0
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
	dw input_buffer
	dw invalid_command


;;; ===========================================================================
;;; Applications
;;; ===========================================================================

;;; ---------------------------------------------------------------------------
;;; Somewhat Lazy Calculator (SLC)
;;; ---------------------------------------------------------------------------

calculator:
;;; Run SLC.
	push di  ; save

	jmp .start

	.welcome_str:
	line("Welcome to the Somewhat Lazy Calculator (SLC)!")
	line("Run 'help' for help.")
	db 0

	.exit_str db "exit",0
	.help_str db "help",0

	.start:

	mov di, .welcome_str
	call println

	.loop:

	mov di, calc_prompt
	call println

	mov di, input_buffer
	call getstr

	;; Check for empty input.
	cmp BYTE [di], 0
	je .loop

	;; Check for the exit command.
	mov si, .exit_str
	call compare_strings
	cmp ax, 0
	jne .return

	;; Check for the help command.
	mov si, .help_str
	call compare_strings
	cmp ax, 0
	jne .help

	;; Assume the input is an expression and evaluate it.
	call calc_eval
	jmp .loop

	.help:
	call calc_help
	jmp .loop

	.return:

	pop di  ; restore
	ret

calc_help:
;;; Display the calculator help message.
	push di  ; save
	jmp .start

	.help_str:

	line("Operators: + - * / % ^")
	line("Operands: integers in the range -32768 to 32767 (inclusive).")
	db break

	line("Expressions use postfix notation:")
	db break

	line("  SLC> 6 2 /")
	line("  3")
	line("  SLC> 10 2 ^ 25 -")
	line("  75")
	db break

	line("Exit with 'exit'.")
	db break

	line("Please see the README for more information.")
	db 0

	.start:
	mov di, .help_str
	call println

	pop di  ; restore
	ret

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

	.toofewstr db "Too few operands for operator ' '.",0
	.toomanystr db "Too many operands.",0
	.invalidoperatorstr db "Invalid operator ' '.",0
	.operandoverflowstr db "Operand overflow.",0
	.operationoverflowstr db "Operation overflow.",0

	;; Loop through the input string. Upon encountering an operand, push it
	;; to the stack. Upon encountering an operator, pop two operands and
	;; apply the operator to them, then push the result to the stack.
	.loop:

	;; Jump to the bottom of the loop if the current input char is a space.
	cmp BYTE [di], ' '
	je .increment

	;; Check if the current input char represents a digit 0-9. If not,
	;; treat the char as an operator.
	cmp BYTE [di], 0x30
	jl .operator
	cmp BYTE [di], 0x39
	jg .operator

	;; We've encountered a char representing the first digit of an integer.

	;; Parse the integer and push its value. Note that parse_num also
	;; returns (in cx) the number of digits in the integer.
	call parse_num
	jo .operand_overflow
	push ax
	add bx, 2  ; Stack offset increases by 2B.

	;; Advance the input string pointer to the last digit of the integer
	;; and then jump to the bottom of the loop.
	add di, cx  ; Increment the string pointer by the number of digits.
	dec di  ; Go back to the last digit.
	jmp .increment

	.operator:
	;; Treat the current input char as an operator.

	;; Exit the loop if we've encountered the operator while number of
	;; operands on the stack < 2 (stack offset < 4B).
	cmp bx, 4
	jl .toofew

	;; Convert the operator char to an index into the operator table.
	mov BYTE dl, [di]  ; operator char
	call get_operator_index

	;; Exit the loop if the char does not represent an operator.
	cmp ax, -1
	je .invalid_operator

	;; Pop two operands.
	pop si  ; second operand
	pop cx  ; first operand
	sub bx, 4  ; Stack offset decreases by 4B.

	push bx  ; Save stack offset.
	push di  ; Save input string pointer.

	mov bx, ax  ; operator table index
	mov di, cx  ; first operand

	;; Call the operator procedure.
	call [operator_table+bx]

	pop di  ; Restore input string pointer.
	pop bx	; Restore stack offset.

	jo .operation_overflow

	;; Push the result.
	push ax
	add bx, 2  ; Stack offset increases by 2B.

	;; Advance to the next input char.
	.increment:
	inc di

	;; Check the input string for the null-terminator.
	.test:
	cmp BYTE [di], 0
	jne .loop

	;; Done reading the input string.

	;; Pop the final result.
	pop ax
	sub bx, 2  ; Stack offset decreases by 2B.

	;; Error if number of operands remaining on the stack > 0 (stack offset
	;; > 0B).
	cmp bx, 0
	jg .toomany

	;; The stack offset was 0B as expected, so print the final result and
	;; return.
	call println_num
	jmp .return

	;; Encountered an operator with too few operands. Fix the stack and
	;; notify the user.
	.toofew:
	add sp, bx  ; Add the stack offset to the stack pointer.
	mov BYTE dl, [di]  ; operator char
	mov di, .toofewstr
	mov BYTE [di+31], dl  ; Include the operator in the error message.
	call println
	jmp .return

	;; The expression contains too many operands. Fix the stack and notify
	;; the user.
	.toomany:
	add sp, bx  ; Add the stack offset to the stack pointer.
	mov di, .toomanystr
	call println
	jmp .return

	;; Encountered an invalid operator char. Fix the stack and notify the
	;; user.
	.invalid_operator:
	add sp, bx  ; Add the stack offset to the stack pointer.
	mov BYTE dl, [di]  ; operator char
	mov di, .invalidoperatorstr
	mov BYTE [di+18], dl  ; Include the operator in the error message.
	call println
	jmp .return

	;; Operand overflow. Fix the stack and notify the user.
	.operand_overflow:
	add sp, bx  ; Add the stack offset to the stack pointer.
	mov di, .operandoverflowstr
	call println
	jmp .return

	;; Operation overflow. Fix the stack and notify the user.
	.operation_overflow:
	add sp, bx  ; Add the stack offset to the stack pointer.
	mov di, .operationoverflowstr
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

get_operator_index:
;;; Convert an operator char to an index into the operator procedure array.
;;; Pre: dl contains the operator char.
;;; Post: ax contains the index, or -1 if the operator char is invalid.
	push bx  ; save

	mov bx, operator_chars	; string of valid operator chars
	xor ax, ax  ; the index

	.loop:

	;; Exit the loop if we've found the given operator char in the string
	;; of valid operator chars.
	cmp BYTE dl, [bx]
	je .shift

	;; Exit the loop if we've reached the end of the string of valid
	;; operator chars.
	cmp BYTE [bx], 0
	je .invalid

	inc bx  ; position in string of valid operator chars
	inc ax	; the index
	jmp .loop

	;; Signal that the given operator char is invalid by returning -1.
	.invalid:
	mov ax, -1
	jmp .return

	;; Shift the index left because the operator procedure array contains
	;; 2B pointers.
	.shift:
	shl ax, 1

	.return:
	pop bx  ; restore
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

mul_op:
;;; Multiplication operator.
;;; Pre: di contains the first operand and si the second operand.
;;; Post: ax contains di * si.
	mov ax, di
	imul ax, si
	ret

div_op:
;;; Division operator.
;;; Pre: di contains the first operand and si the second operand.
;;; Post: ax contains di / si (integer division).
	push dx  ; save
	call divide
	pop dx  ; restore
	ret

mod_op:	
;;; Modulo operator.
;;; Pre: di contains the first operand and si the second operand.
;;; Post: ax contains di % si.
	push dx  ; save
	call divide
	mov ax, dx  ; remainder
	pop dx  ; restore
	ret

pow_op:
;;; Power operator.
;;; Pre: di contains the first operand and si the second operand.
;;; Post: ax contains di raised to the power of si.
	call power
	ret


;;; ---------------------------------------------------------------------------
;;; Calculator data
;;; ---------------------------------------------------------------------------

	calc_prompt db "SLC> ",0

	operator_chars db "+-*/%^",0
operator_table:	
	dw add_op
	dw sub_op
	dw mul_op
	dw div_op
	dw mod_op
	dw pow_op


;;; ===========================================================================
;;; System utilities
;;; ===========================================================================

;;; ---------------------------------------------------------------------------
;;; Input
;;; ---------------------------------------------------------------------------

getstr:
;;; Read a string from keyboard input.
;;; Pre: di points to an array.
;;; Post: di points to the same array, which now contains the string.

	;; save
	push ax
	push bx

	mov bx, 0  ; input array index

	;; Get one char at a time, adding each one to the input array and
	;; printing it to the screen. Exit upon encountering a carriage return.
	.loop:
	
	;; Read a char to al.
	mov ah, 0
	int 0x16

	;; Check for backspace.
	cmp al, 0x08
	je .backspace

	;; Check for carriage ret (enter).
	cmp al, 0x0d
	je .return

	;; Skip unprintable chars.
	cmp al, 0x20
	jl .loop
	cmp al, 0x7e
	jg .loop

	;; Convert the char from QWERTY to Dvorak if Dvorak is enabled.
	cmp BYTE [dvorak_mode], 0
	je .skipconvert

	call convert_char

	.skipconvert:

	;; Add the char to the input array.
	mov BYTE [di+bx], al

	;; Print the char in al.
	mov ah, 0x0e
	int 0x10

	jmp .increment

	;; Handle a backspace.
	.backspace:

	cmp bx, 0
	je .loop

	dec bx
	mov BYTE [di+bx], 0
	call cursor_backspace
	jmp .loop

	.increment:
	
	inc bx
	jmp .loop

	.return:

	;; Append the null-terminator.
	mov BYTE [di+bx], 0

	;; restore
	pop bx
	pop ax

	ret

cursor_backspace:
;;; Move the cursor back one column and print a null character.

	;; save
	push ax
	push bx
	push cx
	push dx

	;; source:
	;; - https://wiki.osdev.org/Text_Mode_Cursor#Get_Cursor_Data
	;; - https://wiki.osdev.org/Text_Mode_Cursor#Moving_the_Cursor

	;; From source: bh is the "display page (usually, if not always 0)".
	xor bh, bh

	;; Get cursor data. Row and column are returned in dh and dl. (Values
	;; are also returned in ch and cl, which is why this procedure must
	;; preserve cx.)
	mov ah, 0x03
	int 0x10

	;; Move the cursor back one column. Row and column are passed in dh and
	;; dl.
	dec dl
	mov ah, 0x02
	int 0x10

	;; Print a null character. The cursor automatically moves forward one
	;; column.
	mov al, 0
	mov ah, 0x0e
	int 0x10

	;; Move the cursor back one column.
	mov ah, 0x02
	int 0x10

	;; restore
	pop dx
	pop cx
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

	;; Use the QWERTY char as an index into the Dvorak keymap.
	movzx bx, al
	mov BYTE al, [dvorak_keymap+bx-0x21]

	.return:
	pop bx  ; restore
	ret

dvorak_keymap:
	db "!_#$%&-()*}w[vz0123456789SsW]VZ@AXJE>UIDCHTNMBRL",0x22,"POYGK<QF:/"
	db "\=^{`axje.uidchtnmbrl'poygk,qf;?|+~",0x7f


;;; ---------------------------------------------------------------------------
;;; Output
;;; ---------------------------------------------------------------------------

println:
;;; Print a string on a new line.
;;; Pre: di points to the beginning of the string.
	call print_newline
	call print
	ret

print:
;;; Print a string.
;;; Pre: di points to the beginning of the string.

	;; save
	push ax
	push bx
	
	mov ah, 0x0e

	;; Print each char until encountering a null terminator.

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

println_num:
;;; Print a number preceded by a newline.
;;; Pre: ax contains the number.
	call print_newline
	call print_num
	ret

print_num:
;;; Print a number.
;;; Pre: ax contains the number.

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
;;; String operations
;;; ---------------------------------------------------------------------------

compare_strings:
;;; Compare two strings.
;;; Pre: di and si point to the strings.
;;; Post: ax contains 1 if the strings are equal and 0 otherwise.
	push bx  ; save

	mov bx, 0  ; index into each string

	;; Loop through the strings, comparing each pair of chars with the same
	;; index.
	.loop:

	;; Exit the loop and return 0 if the current two chars are not equal.
	mov BYTE al, [si+bx]
	cmp BYTE [di+bx], al
	jne .false

	;; The chars are equal, so exit the loop and return true if we've
	;; reached the null terminator.
	cmp BYTE [di+bx], 0
	je .true

	;; Advance to the next two chars and continue the loop.
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

parse_num:
;;; Get an integer from its string representation.
;;; 
;;; Pre: di points to a string that begins with a char in the range 0x30-0x39
;;; and terminates on any char outside of that range (e.g. whitespace or an
;;; arithmetic operator).
;;; 
;;; Post: ax contains the integer and cx its number of digits.

	;; save
	push bx
	push di
	push dx
	push si

	mov bx, di

	;; Advance to the end of the string by finding the first char that does
	;; not represent a digit 0-9.
	.loop_find_end:

	;; Advance to the next char.
	inc bx

	;; Exit the loop if the char does not represent a digit 0-9.
	cmp BYTE [bx], 0x30
	jl .exit
	cmp BYTE [bx], 0x39
	jg .exit

	jmp .loop_find_end

	.exit:

	;; Now go back through the string, adding up the values of the digits
	;; until we have our integer:

	;; Running total.
	xor si, si

	;; Current place in the number, starting at 0.
	;; 10 raised to the current place gives us the place value.
	xor cx, cx

	;; Points to 1B before the start of our string (so we know where to
	;; stop).
	dec di

	;; Start the loop.
	jmp .test

	.loop_add_digits:

	;; Convert the current digit from char to int.
	movzx dx, [bx]
	sub dx, 0x30

	push di  ; save string terminator
	push si	 ; save running total

	;; Calculate the current place value by finding 10 raised to the
	;; current place.
	mov di, 10
	mov si, cx  ; current place
	call power

	pop si  ; restore running total
	pop di  ; restore string terminator

	jo .return

	;; Multiply the current digit by the place value and add the result to
	;; our running total.
	imul dx, ax
	jo .return
	add si, dx
	jo .return

	;; Increment the current place.
	inc cx

	.test:

	;; Move back one char and continue the loop if we haven't reached the
	;; pointer to 1B before the start of our string.
	dec bx
	cmp bx, di
	jg .loop_add_digits

	;; The final value of our integer.
	mov ax, si

	.return:

	;; restore
	pop si
	pop dx
	pop di
	pop bx

	ret


;;; ---------------------------------------------------------------------------
;;; Arithmetic
;;; ---------------------------------------------------------------------------

divide:
;;; Divide the first operand by the second operand.
;;; Pre: di contains the first operand and si the second operand.
;;; Post: ax contains the quotient and dx the remainder.

	;; div divides dx:ax by the operand. ax stores the quotient and dx
	;; stores the remainder.
	;; source: https://stackoverflow.com/a/8022107/10402025

	;; idiv divides signed numbers in a similar manner. cwd sign-extends ax
	;; into dx:ax.
	;; source: https://stackoverflow.com/a/9073207/10402025

	mov ax, di  ; first operand
	cwd
	idiv si  ; second operand

	ret

power:
;;; Raise the first operand to the power of the second operand.
;;; Pre: di contains the first operand and si the second operand.
;;; Post: ax contains the result.
	push si  ; save

	;; Return 1 if the exponent is 0.
	mov ax, 1
	cmp si, 0
	je .return

	mov ax, di  ; running total

	.loop:

	;; Exit the loop if the exponent is 1.
	cmp si, 1
	je .return

	imul ax, di  ; Multiply running total by first operand.
	jo .return
	dec si	     ; Decrement the exponent.
	jmp .loop

	.return:
	pop si  ; restore
	ret


;;; ===========================================================================
;;; Global data
;;; ===========================================================================

	input_buffer times 256 db 0
	dvorak_mode db 0

os_end:	
