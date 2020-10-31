#ifndef ASM6F_H
#define ASM6F_H
#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

#define VERSION "1.7"

#define PC firstlabel.value  // '$' value
#define NOORIGIN -0x40000000 // nice even number so aligning works before origin is defined - what does this mean?
#define INITLISTSIZE 128     // initial label list size
#define INITCOMMENT 512      // initial comment list size
#define BUFFSIZE 8192        // file buffer (inputbuff, outputbuff) size
#define WORDMAX 128          // used with getword()
#define LINEMAX 2048         // plenty of room for nested equates
#define MAXPASSES 7          // number of tries before giving up
#define IFNESTS 32           // max nested IF levels
#define DEFAULTFILLER 0      // default fill value
#define LOCALCHAR '@'        // prefix for local labels
#define PATH_SEPERATOR '\\'  // file system path seperator
#define PATH_RELATIVE ".\\"  // relative file path specifier
#define LISTMAX 8            // number of output bytes to show in listing

typedef uint8_t byte;

enum cdltype
{
    NONE = 0,
    CODE = 1,
    DATA = 2,
};

// addressing modes
enum optype
{
    ACC, // Accumulator
    IMM, // Immediate
    IND, // Indirect
    IDX, // X-Indexed, Indirect
    IDY, // Indirect, Y-Inexed
    ZPX, // Zero Page, X-Indexed
    ZPY, // Zero Page, Y-Indexed
    ABX, // Absolute, X-Indexed
    ABY, // Absolute, Y-Indexed
    ZPG, // Zero Page
    ABS, // Absolute
    REL, // Relative
    IMP, // Implied
};

// size of the operand in bytes
const int opsize[] = {
    [ACC] = 0, // Accumulator
    [IMM] = 1, // Immediate
    [IND] = 2, // Indirect
    [IDX] = 1, // X-Indexed, Indirect
    [IDY] = 1, // Indirect, Y-Inexed
    [ZPX] = 1, // Zero Page, X-Indexed
    [ZPY] = 1, // Zero Page, Y-Indexed
    [ABX] = 2, // Absolute, X-Indexed
    [ABY] = 2, // Absolute, Y-Indexed
    [ZPG] = 1, // Zero Page
    [ABS] = 2, // Absolute
    [REL] = 1, // Relative
    [IMP] = 0, // Implied
};

// leading character for each operand type
const char ophead[] = {
    [ACC] = 0,   // Accumulator
    [IMM] = '#', // Immediate
    [IND] = '(', // Indirect
    [IDX] = '(', // X-Indexed, Indirect
    [IDY] = '(', // Indirect, Y-Inexed
    [ZPX] = 0,   // Zero Page, X-Indexed
    [ZPY] = 0,   // Zero Page, Y-Indexed
    [ABX] = 0,   // Absolute, X-Indexed
    [ABY] = 0,   // Absolute, Y-Indexed
    [ZPG] = 0,   // Zero Page
    [ABS] = 0,   // Absolute
    [REL] = 0,   // Relative
    [IMP] = 0,   // Implied
};

// trailing characters for each operand type
const char *optail[] = {
    [ACC] = "A",   // Accumulator
    [IMM] = "",    // Immediate
    [IND] = ")",   // Indirect
    [IDX] = ",X)", // X-Indexed, Indirect
    [IDY] = "),Y", // Indirect, Y-Inexed
    [ZPX] = ",X",  // Zero Page, X-Indexed
    [ZPY] = ",Y",  // Zero Page, Y-Indexed
    [ABX] = ",X",  // Absolute, X-Indexed
    [ABY] = ",Y",  // Absolute, Y-Indexed
    [ZPG] = "",    // Zero Page
    [ABS] = "",    // Absolute
    [REL] = "",    // Relative
    [IMP] = "",    // Implied
};

// precedence level for operators in order of lowest to highest priority
enum prectype
{
    WHOLEEXP,
    ORORP,
    ANDANDP,
    ORP,
    XORP,
    ANDP,
    EQCOMPARE,
    COMPARE,
    SHIFT,
    PLUSMINUS,
    MULDIV,
    UNARY,
};

// all operators
enum operator
{
    NOOP,
    EQUAL,
    NOTEQUAL,
    GREATER,
    GREATEREQ,
    LESS,
    LESSEQ,
    PLUS,
    MINUS,
    MUL,
    DIV,
    MOD,
    AND,
    XOR,
    OR,
    ANDAND,
    OROR,
    LEFTSHIFT,
    RIGHTSHIFT,
};

// precedence of each operator
char prec[] = {
    [NOOP] = WHOLEEXP,
    [EQUAL] = EQCOMPARE,
    [NOTEQUAL] = EQCOMPARE,
    [GREATER] = COMPARE,
    [GREATEREQ] = COMPARE,
    [LESS] = COMPARE,
    [LESSEQ] = COMPARE,
    [PLUS] = PLUSMINUS,
    [MINUS] = PLUSMINUS,
    [MUL] = MULDIV,
    [DIV] = MULDIV,
    [MOD] = MULDIV,
    [AND] = ANDP,
    [XOR] = XORP,
    [OR] = ORP,
    [ANDAND] = ANDANDP,
    [OROR] = ORORP,
    [LEFTSHIFT] = SHIFT,
    [RIGHTSHIFT] = SHIFT,
};

enum symboltype
{
    LABEL,    // LABEL: known address
    VALUE,    // VALUE: defined with '='
    EQUATE,   // EQUATE: made with EQU
    MACRO,    // MACRO: macro (duh)
    RESERVED, // RESERVED: reserved word
};

typedef struct instruction
{
    enum optype type;
    byte code;
} instruction;

typedef struct linked_node
{
    struct linked_node *next;
    char *text;
} linked_node;

typedef struct symbol
{
    const char *name;
    enum symboltype type;

    union
    {
        long value;                             // PC (label), value (value), param count (macro),
        void (*func)(struct symbol *, char **); // directive (reserved)
    };

    int pos; // location in file; used to determine bank when exporting labels
             // TODO is this redundant?

    union
    {
        bool known;                 // LABEL or VALUE: is the value good?
        char *line;                 // EQUATE: the text
        linked_node *line_list;     // MACRO: a linked list of strings, the first <value> lines hold param names
        const instruction *opcodes; // RESERVED: for instructions this holds opcode definitions, unused for other RESERVED
    };

    bool used;           // EQUATE or MACRO: recursion check
    int pass;            // when symbol was last defined
    int scope;           // where the symbol is visible (0 is global)
    bool ignorenl;       // output this label in .nl files?
    struct symbol *link; // labels that share the same name are chained together, i.e. local labels in different scopes
} symbol;

typedef struct comment
{
    char *text;
    int pos;
} comment;

void initsymboltable();
symbol *findsymbol(const char *);
symbol *newlabel();
symbol *getreserved(char **);

void getword(char *, char **, bool);
int getvalue(char **);
enum operator getoperator(char **);
bool readlabel(char *, char **);

int eval(char **, enum prectype);
void processline(char *, char *, int);
void listline(char *, char *);
void expandmacro(symbol *, char **, int, char *);
void expandrept(int, char *);

/* directives */
void nothing(symbol *, char **);
void opcode(symbol *, char **);

void inesprg(symbol *, char **);
void ineschr(symbol *, char **);
void inesmir(symbol *, char **);
void inesmap(symbol *, char **);

void nes2chrram(symbol *, char **);
void nes2prgram(symbol *, char **);
void nes2sub(symbol *, char **);
void nes2tv(symbol *, char **);
void nes2vs(symbol *, char **);
void nes2bram(symbol *, char **);
void nes2chrbram(symbol *, char **);

void include(symbol *, char **);
void incbin(symbol *, char **);

void org(symbol *, char **);
void align(symbol *, char **);
void base(symbol *, char **);
void pad(symbol *, char **);
void fillval(symbol *, char **);

void equ(symbol *, char **);
void equal(symbol *, char **);

void dw(symbol *, char **);
void db(symbol *, char **);
void dl(symbol *, char **);
void dh(symbol *, char **);
void dsw(symbol *, char **);
void dsb(symbol *, char **);
void hex(symbol *, char **);

void _if(symbol *, char **);
void ifdef(symbol *, char **);
void ifndef(symbol *, char **);
void elseif(symbol *, char **);
void _else(symbol *, char **);
void endif(symbol *, char **);

void macro(symbol *, char **);
void endm(symbol *, char **);
void rept(symbol *, char **);
void endr(symbol *, char **);
void _enum(symbol *, char **);
void ende(symbol *, char **);
void ignorenl(symbol *, char **);
void endinl(symbol *, char **);

void make_error(symbol *, char **);
void undoc(symbol *, char **);

#endif