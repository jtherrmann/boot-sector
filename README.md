# Boot sector

Jake Herrmann\
CS 301: Assembly Language Programming\
Fall 2018\
Project 1

## Introduction

This is a boot sector that loads a simple shell for x86 real mode. The project
touches on most of the topics covered in class so far, particularly branching,
function calls, stack manipulation, integer overflow, register and data sizes,
pointers, and strings.

The first major challenge was getting the boot sector to load more sectors from
the disk. Fortunately I received help from a kind Stack Overflow user (Michael
Petch). Of course, the boot sector may still contain bugs that I haven't yet
discovered.

After working out the kinks in the boot sector, another significant challenge
was the inability to easily print values at arbitrary points in the program, as
that is my go-to debugging tool in higher level languages. In particular,
mixing up register and data sizes caused me significant frustration until I
learned to pay much closer attention to the sizes of values.

Overall, the experience has given me more confidence working with lower level
abstractions and was a valuable experience in code organization and debugging
in general.

## Getting started

Known to work on Debian GNU/Linux 9.5 (stretch).

1. Make sure `nasm` and `qemu` are installed.
2. Clone this repo, `cd` into `os/`, and run:

        nasm -f bin -o os.bin os.asm
        qemu-system-x86_64 os.bin

## Commands

Run `help` for a list of commands:

- `calc`: Run the calculator
- `hello`: Say hello to the computer
- `help`: Print a list of commands
- `keymap`: Switch between QWERTY and Dvorak
- `me`: Change your command prompt
- `reboot`: Reboot the computer

## Calculator

The calculator uses postfix notation:

    calc> 6 2 /
    3
    calc> 10 2 ^ 25 -
    75

Postfix notation is useful in computing because expressions written in postfix
notation are extremely simple to evaluate using a stack.

The calculator recognizes the following operators:

`+` (add)  
`-` (subtract)  
`*` (multiply)  
`/` (divide)  
`%` (mod)  
`^` (power)  

The calculator can only operate on integers in the range -32768 to 32767
(inclusive).

An *operand overflow* occurs when the calculator encounters an operand outside
of this range:

    calc> 32768
    Operand overflow.

An *operation overflow* occurs when the calculator encounters an operation
whose result falls outside of this range:

    calc> 32767 1 +
    Operation overflow.

Oh, and the calculator does not parse negative operands (e.g. -10). But there's
a simple workaround:

    calc> 0 10 -
    -10
    calc> 0 10 - 2 ^
    100
    calc> 0 10 - 3 ^
    -1000
