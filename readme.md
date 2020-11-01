
# ASM6d (v1.7)
## A 6502 assembler

ASM6d is a fork of ASM6f, itself a fork of ASM6. I started this project originally with the intention of making a VScode extension for ASM6f, but quickly realized that it would be much easier to leverage the assembler itself to generate most of the diagnosic info I needed. Most of the changes so far are for this purpose, as well as generally cleaning up and documenting the code. The grammar has also been changed to be more strict, requiring `.`'s for directives, `:`'s for labels, etc.

## Features
* [Macros](#macro) and [Rept loops](#rept).
* Local and directional [labels](#labels)
* Support for [undocumented instuctions](#undocumented).
* [iNES and iNES 2.0](#ines) header insertion.
* List file generation.
* Symbol file generation for FCEUX (.nl), Lua, .cdl, and Mesen (.mlb).

## Usage

Command Line:

    asm6d [-options] sourcefile [outputfile]

Options:

    -?        show this menu
    -q        quiet mode (no output unless error)
    -d<name>  define symbol with name
    -l[name]  create listing optionally with a name
    -L[name]  same as -l but verbose (expand REPT and MACRO calls)
    -n[name]  export FCEUX-compatible .nl files
    -f[name]  export Lua symbol file
    -c[name]  export .cdl file for use with FCEUX or Mesen
    -m[name]  export Mesen-compatible label file (.mlb)
        
Default names are given if any of the options with an optional name are given without a name.

## Numbers and expressions

Hexadecimal numbers begin with `$`. Binary numbers begin with `%`. Characters are surronded by single quotes and strings are surrounded by double quotes. The characters `'`, `"` and `\` must be escaped with a backslash when in quotes.

    12345
    $ABCD
    %01010101
    'L'
    '\''
    "text"
    "y\'ain\'t"

Supported operators (listed by precedence):

    ( )
    + - ~ ! < > (unary)
    * / %
    + -
    << >>
    < > <= >=
    == != 
    &
    ^
    |
    &&
    ||

The unary `<` and `>` operators give the lower and upper byte of a 16-bit word respectively. All other operators function like their C equivalents.

## Comments

Comments begin with a semicolon `;` and continue to the end of the line. Only single line comments are supported, no block comments.

    .org $C000 ; start at $C000
    LDA #$FF   ; store $FF into accumulator

<a id="labels"></a>

## Labels

Labels are case sensitive and must be followed by a colon. Labels beginning with `@` are local labels. They have limited scope, visible only between non-local labels. Names of local labels may be reused.

    label_1:
      @label_a:
      @label_b:
    label_2:
      @label_a:
      @label_b:

The special `$` label holds the current program address. 

Labels beginning with one or more `+` or `-` characters are directional labels. `+` labels are forward labels and can only be referecenced before they are defined. `-` labels are backward labels and can only be referenced after they are defined. They are especially useful for branching, when the name of the label doesn't matter. Also note that labels consisting of _only_ `+` or `-` are considered pure directional labels and do not require a colon.

        -- LDX #0
         - LDA $2002 ; loop
           BNE -
         - LDA $2002 ; directional labels can be reused
           BNE -
           CPX #69
           BEQ +     ; forward branch
           CPX #96
           BEQ +here ; use more characters to be more unique
           DEX
           JMP --    ; multiple --'s are handy for nested loops
    +here: INY
         + LDX #0

## Assembler Directives

All directives are case insensitive and must begin with a dot `.`

### INCLUDE (also INCSRC)

Assemble another source file as if it were part of the current
source.
    
    .include whatever.asm

### INCBIN

Add the raw contents of a file to the assembly output.

    moredata: .incbin whatever.bin

An optional file offset and size can be specified.

	.incbin foo.bin, $400 ; read foo.bin from $400 to end of file
	.incbin foo.bin, $200, $2000 ; read $2000 bytes, starting from $200

<a id="ines"></a>

### iNES directives

ASM6d supports automatic generation of an iNES header. Both the original iNES and the iNES 2.0 format is supported.
Note that using an iNES header is optional; it's only inserted if at least one of the following directives is used.

    .iNESPRG ; Number of PRG ROM banks
    .iNESCHR ; Number of CHR ROM banks
    .iNESMAP ; ROM mapper number
    .iNESMIR ; Mirroring mode

    .NES2CHRRAM  ; Amount of CHR RAM used
    .NES2PRGRAM  ; Amount of PRG RAM used
    .NES2SUB     ; Submapper number
    .NES2TV      ; TV mode: NTSC (0 or 'N'), PAL (1 or 'P'), or both (2 or 'B')
    .NES2VS      ; Use the Vs. Unisystem.
    .NES2BRAM    ; Amount of battery-packed PRG RAM.
    .NES2CHRBRAM ; Amount of battery-packed CHR RAM.

### EQU

For literal string replacement, similar to #define in C.
    
    one:  .equ 1
    plus: .equ +
    .db one plus one ; .db 1 + 1

### = (Assignment)

Unlike EQU, statements with = are evaluated to a number first. Also unlike EQU, symbols created with = can be reassigned. These statements also do not require a colon.

    i = 1
    j: .equ i+1
    k = i + 1 ; k = 1 + 1
    i = j + 1 ; i = i+1 + 1
    i = k + 1 ; i = 2 + 1

### IGNORENL, ENDINL

Suppresses output of any labels when exporting FCEUX .nl files.
Useful for defining labels that may conflict with zero page addresses.

    ; don't show these button masks in the .nl file
    .ignorenl
        PAD_A      = %10000000
        PAD_B      = %01000000
        PAD_SELECT = %00100000
        PAD_START  = %00010000
        PAD_UP     = %00001000
        PAD_DOWN   = %00000100
        PAD_LEFT   = %00000010
        PAD_RIGHT  = %00000001
    .endinl

### DB, DW

Output raw bytes or words. Multiple arguments are separated by commas. Strings can be "shifted" by adding a value to them (see example).

    .db $01, $02, $04, $08
    .db "ABCDE" + 1          ; equivalent to DB "BCDEF"
    .db "ABCDE" - "A" + 32   ; equivalent to DB 32,33,34,35,36

### DL, DH

Similar to DB, outputting only the LSB or MSB of a value.

    .dl a, b, c, d ; equivalent to .db <a, <b, <c, <d
    .dh a, b, c, d ; equivalent to .db >a, >b, >c, >d

### HEX

Compact way of laying out a table of hex values. Only raw hex values are allowed, no expressions. Spaces can be used to separate numbers.

    .hex 456789ABCD     ; equivalent to .db $45, $67, $89, $AB, $CD
    .hex 45 67 89 AB CD ; equivalent to .db $45, $67, $89, $AB, $CD
    .hex  0  1 23 45 67 ; equivalent to .db $00, $01, $23, $45, $67

### DSB, DSW

Define storage (bytes or words). The size argument may be followed by a fill value (default filler is 0).

    .dsb 4        ; equivalent to .db 0,0,0,0
    .dsb 8, 1     ; equivalent to .db 1,1,1,1,1,1,1,1
    .dsw 4, $ABCD ; equivalent to .dw $ABCD,$ABCD,$ABCD,$ABCD

### PAD

Fill memory from the current address to a specified address. A fill value may also be specified.

    .pad $FFFA      ; equivalent to .dsb $FFFA - $
    .pad $FFFA, $EA ; equivalent to .dsb $FFFA - $, $EA

### ORG

Set the starting address if it hasn't been assigned yet, otherwise functions like PAD.

    .org $E000     ; start assembling at $E000
    .org $FFFA,$80 ; equivalent to .pad $FFFA

### ALIGN

Fill memory from the current address to an N byte boundary. A fill value may also be specified.

    .align 256, $EA

### BASE

Set the program address. This is useful for relocatable code, multiple code banks, etc. The same can also be accomplished by assigning the '$' symbol directly (i.e. '$=9999').

    oldaddr = $
    .base $6000
    stuff:
        .
        .
        .
    .base oldaddr + $ - stuff

### FILLVALUE

Change the default filler for PAD, ALIGN, etc.

    .fillvalue $FF
    .pad $E000 ; equivalant to .pad $E000, $FF


### IF / ELSEIF / ELSE / ENDIF

Process a block of code if an expression is true (nonzero).

    .if j > 0
        .db i / j
    .else
        .db 0
    .endif

### IFDEF / IFNDEF

Process a block of code if a symbol has been defined / not defined.

    .ifdef __DEBUG
        BRK
    .endif

<a id="macro"></a>

### MACRO / ENDM

Define a macro. Macro parameters are comma separated. Labels defined inside macros are local (visible only to that macro).

    .macro setAXY x, y, z
        LDA #x
        LDX #y
        LDY #z
    .endm

    .setAXY $12,$34,$56
        ; expands to LDA #$12
        ;            LDX #$34
        ;            LDY #$56

<a id="rept"></a>

### REPT / ENDR

Repeat a block of code a specified number of times.
Labels defined inside REPT are local.

    i = 0
    .rept 256
        .db i
        i = i + 1
    .endr

### ENUM / ENDE

Reassign PC and suppress assembly output. Useful for defining variables in RAM.

    .enum $00
        foo1: .dsb 1
        foo2: .dsb 1
    .ende


### ERROR

Stop assembly and display a message.

    .if i > 100
        .error "i is too big :("
    .endif

<a id="undoc"></a>

### UNDOC

Enables use of undocumented instructions.

    .undoc
    LAX $0205
    DCP $07
    AXS #$EE
    SAX $0205

<a id="undocumented"></a>

## Undocumented Instructions

asm6d supports a number of undocumented or "illegal" instructions.
These instuctions require the use of the [`.undoc`](#undoc) directive, otherwise an error will be thrown.

More information about these instructions can be found in [Graham's 6502 Opcode document](http://www.oxyron.de/html/opcodes02.html)

The alternate `$EB` opcode for `SBC` immediate is not supported.

#### SLO - Shift Left, Or
Equivalant to `ASL`, `ORA`.  
Also known as `ASO`.

#### RLA - Rotate Left, And
Equivalant to `ROL`, `AND`.

#### SRE - Shift Right, Exclusive Or
Equivalant to `LSR`, `EOR`.  
Also known as `LSE`.

#### RRA - Rotate Right, Add with Carry
Equivalant to `ROR`, `ADC`.

#### SAX - Store A and X
Stores the bitwise and of A and X into the operand.
The A & X operation is a result of A and X put onto the bus at the same time.

#### LAX - Load A, Load X
`LDA` and `LDX` at the same time.

#### DCP - Decrement, Compare
Equivalant to `DEC`, `CMP`.

#### ISC - Increment, Subtract with Carry
Equivalant to `INC`, `SBC`.  
Also known as `ISB`.

#### ANC - And, Rotate Left
Performs an `AND` immediate, but bit 7 is put into the carry as if `ASL` or `ROL` were executed.

#### ALR - And, Shift Right
`AND` immediate, then `LSR`.  
Also known as `ASR`.

#### ARR - And, Rotate Right
Similar to `ALR` but also updates the V flag.  
Bit 0 does _not_ go into carry, but bit 7 is exchanged with the carry.  

#### AXS - A & X, Subtract Immediate
Stores (A & X) - #immediate into X.  
Performs `CMP` and `DEX` at the same time, so that the MINUS sets the flag like `CMP`, not `SBC`.  
Also known as `SBX`.

#### LAS
Stores the bitwise and of S and the opreand into A, X and S.  
Also known as `LAR`.

#### AHX - Store A & H & X
Stores A & X & H into the operand.  
Sometimes the H drops off. Page boundary crossing will not work as expected; the bank where the value is stored may not be equal to the value stored.

#### SHY - Store H & Y
Stores Y & H into the operand.  
Sometimes the &H drops off. Page boundary crossing will not work as expected; the bank where the value is stored may not be equal to the value stored.

#### SHX - Store H & X
Stores X & H into the operand.  
Sometimes the &H drops off. Page boundary crossing will not work as expected; the bank where the value is stored may not be equal to the value stored.

#### TAS
Stores A & X into S and A & X & H into the operand.

#### XAA
Bitwise and X and the operand.  
Very Unstable.  
Also known as `ANE`
