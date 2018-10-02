# Operating system for x86 real mode

Jake Herrmann  
CS 301 Fall 2018

TODO: table of contents

## Introduction

TODO

## Getting started

Known to work on Debian GNU/Linux 9.5 (stretch).

1. Make sure `nasm` and `qemu` are installed.
2. Clone this repo, `cd` into `os/`, and run:

        nasm -f bin -o os.bin os.asm
        qemu-system-x86_64 os.bin

## Applications

### The Somewhat Lazy Calculator (SLC)

Most calculators use infix notation; that is, operators fall in between their
operands (e.g. 3 - 1 = 2).

SLC uses postfix notation; that is, operators follow their operands:

    SLC> 6 2 /
    3
    SLC> 10 2 ^ 25 -
    75

Postfix notation is useful in computing because expressions written in postfix
notation are extremely simple to evaluate using a stack. While you might
consider infix notation more readable, SLC is lazy and prefers postfix
notation.

SLC recognizes the following operators:

`+` (add)  
`-` (subtract)  
`*` (multiply)  
`/` (divide)  
`%` (mod)  
`^` (power)  

SLC can only operate on integers in the range -32768 to 32767 (inclusive).

An *operand overflow* occurs when SLC encounters an operand outside of this
range:

    SLC> 32768
    Operand overflow.

An *operation overflow* occurs when SLC encounters an operation whose result
falls outside of this range:

    SLC> 32767 1 +
    Operation overflow.

Oh, and SLC is too lazy to parse negative operands (e.g. -10). But there's a
simple workaround:

    SLC> 0 10 -
    -10
    SLC> 0 10 - 2 ^
    100
    SLC> 0 10 - 3 ^
    -1000
