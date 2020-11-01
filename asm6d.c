#include "asm6d.h"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stddef.h>
#include <ctype.h>
#include <stdarg.h>

// TODO find a better way to handle errors
#define THROW(err)    \
    do                \
    {                 \
        errmsg = err; \
        return 0;     \
    } while (0)
#define THROW_VOID(err) \
    do                  \
    {                   \
        errmsg = err;   \
        return;         \
    } while (0)
#define PROPOGATE_ERROR \
    do                  \
    {                   \
        if (errmsg)     \
            return 0;   \
    } while (0)
#define PROPOGATE_ERROR_VOID \
    do                       \
    {                        \
        if (errmsg)          \
            return;          \
    } while (0)

static void *true_ptr = &true_ptr;

// '$' label
symbol firstlabel = {
    .name = "$",       // name
    .value = 0,        // value
    .pos = 0,          // pos
    .known = true,     // known
    .type = VALUE,     // type
    .used = false,     // used
    .pass = 0,         // pass
    .scope = 0,        // scope
    .ignorenl = false, // ignorenl
    .link = NULL,      // link
};

const instruction adc[] = {{IMM, 0x69}, {IDX, 0x61}, {IDY, 0x71}, {ZPX, 0x75}, {ABX, 0x7d}, {ABY, 0x79}, {ZPG, 0x65}, {ABS, 0x6d}, {-1}};
const instruction and[] = {{IMM, 0x29}, {IDX, 0x21}, {IDY, 0x31}, {ZPX, 0x35}, {ABX, 0x3d}, {ABY, 0x39}, {ZPG, 0x25}, {ABS, 0x2d}, {-1}};
const instruction asl[] = {{ACC, 0x0a}, {ZPX, 0x16}, {ABX, 0x1e}, {ZPG, 0x06}, {ABS, 0x0e}, {-1}};
const instruction bcc[] = {{REL, 0x90}, {-1}};
const instruction bcs[] = {{REL, 0xb0}, {-1}};
const instruction beq[] = {{REL, 0xf0}, {-1}};
const instruction bit[] = {{ZPG, 0x24}, {ABS, 0x2c}, {-1}};
const instruction bmi[] = {{REL, 0x30}, {-1}};
const instruction bne[] = {{REL, 0xd0}, {-1}};
const instruction bpl[] = {{REL, 0x10}, {-1}};
const instruction brk[] = {{IMM, 0x00}, {ZPG, 0x00}, {IMP, 0x00}, {-1}};
const instruction bvc[] = {{REL, 0x50}, {-1}};
const instruction bvs[] = {{REL, 0x70}, {-1}};
const instruction clc[] = {{IMP, 0x18}, {-1}};
const instruction cld[] = {{IMP, 0xd8}, {-1}};
const instruction cli[] = {{IMP, 0x58}, {-1}};
const instruction clv[] = {{IMP, 0xb8}, {-1}};
const instruction cmp[] = {{IMM, 0xc9}, {IDX, 0xc1}, {IDY, 0xd1}, {ZPX, 0xd5}, {ABX, 0xdd}, {ABY, 0xd9}, {ZPG, 0xc5}, {ABS, 0xcd}, {-1}};
const instruction cpx[] = {{IMM, 0xe0}, {ZPG, 0xe4}, {ABS, 0xec}, {-1}};
const instruction cpy[] = {{IMM, 0xc0}, {ZPG, 0xc4}, {ABS, 0xcc}, {-1}};
const instruction dec[] = {{ZPX, 0xd6}, {ABX, 0xde}, {ZPG, 0xc6}, {ABS, 0xce}, {-1}};
const instruction dex[] = {{IMP, 0xca}, {-1}};
const instruction dey[] = {{IMP, 0x88}, {-1}};
const instruction eor[] = {{IMM, 0x49}, {IDX, 0x41}, {IDY, 0x51}, {ZPX, 0x55}, {ABX, 0x5d}, {ABY, 0x59}, {ZPG, 0x45}, {ABS, 0x4d}, {-1}};
const instruction inc[] = {{ZPX, 0xf6}, {ABX, 0xfe}, {ZPG, 0xe6}, {ABS, 0xee}, {-1}};
const instruction inx[] = {{IMP, 0xe8}, {-1}};
const instruction iny[] = {{IMP, 0xc8}, {-1}};
const instruction jmp[] = {{IND, 0x6c}, {ABS, 0x4c}, {-1}};
const instruction jsr[] = {{ABS, 0x20}, {-1}};
const instruction lda[] = {{IMM, 0xa9}, {IDX, 0xa1}, {IDY, 0xb1}, {ZPX, 0xb5}, {ABX, 0xbd}, {ABY, 0xb9}, {ZPG, 0xa5}, {ABS, 0xad}, {-1}};
const instruction ldx[] = {{IMM, 0xa2}, {ZPY, 0xb6}, {ABY, 0xbe}, {ZPG, 0xa6}, {ABS, 0xae}, {-1}};
const instruction ldy[] = {{IMM, 0xa0}, {ZPX, 0xb4}, {ABX, 0xbc}, {ZPG, 0xa4}, {ABS, 0xac}, {-1}};
const instruction lsr[] = {{ACC, 0x4a}, {ZPX, 0x56}, {ABX, 0x5e}, {ZPG, 0x46}, {ABS, 0x4e}, {-1}};
const instruction nop[] = {{IMP, 0xea}, {-1}};
const instruction ora[] = {{IMM, 0x09}, {IDX, 0x01}, {IDY, 0x11}, {ZPX, 0x15}, {ABX, 0x1d}, {ABY, 0x19}, {ZPG, 0x05}, {ABS, 0x0d}, {-1}};
const instruction pha[] = {{IMP, 0x48}, {-1}};
const instruction php[] = {{IMP, 0x08}, {-1}};
const instruction pla[] = {{IMP, 0x68}, {-1}};
const instruction plp[] = {{IMP, 0x28}, {-1}};
const instruction rol[] = {{ACC, 0x2a}, {ZPX, 0x36}, {ABX, 0x3e}, {ZPG, 0x26}, {ABS, 0x2e}, {-1}};
const instruction ror[] = {{ACC, 0x6a}, {ZPX, 0x76}, {ABX, 0x7e}, {ZPG, 0x66}, {ABS, 0x6e}, {-1}};
const instruction rti[] = {{IMP, 0x40}, {-1}};
const instruction rts[] = {{IMP, 0x60}, {-1}};
const instruction sbc[] = {{IMM, 0xe9}, {IDX, 0xe1}, {IDY, 0xf1}, {ZPX, 0xf5}, {ABX, 0xfd}, {ABY, 0xf9}, {ZPG, 0xe5}, {ABS, 0xed}, {-1}};
const instruction sec[] = {{IMP, 0x38}, {-1}};
const instruction sed[] = {{IMP, 0xf8}, {-1}};
const instruction sei[] = {{IMP, 0x78}, {-1}};
const instruction sta[] = {{IDX, 0x81}, {IDY, 0x91}, {ZPX, 0x95}, {ABX, 0x9d}, {ABY, 0x99}, {ZPG, 0x85}, {ABS, 0x8d}, {-1}};
const instruction stx[] = {{ZPY, 0x96}, {ZPG, 0x86}, {ABS, 0x8e}, {-1}};
const instruction sty[] = {{ZPX, 0x94}, {ZPG, 0x84}, {ABS, 0x8c}, {-1}};
const instruction tax[] = {{IMP, 0xaa}, {-1}};
const instruction tay[] = {{IMP, 0xa8}, {-1}};
const instruction tsx[] = {{IMP, 0xba}, {-1}};
const instruction txa[] = {{IMP, 0x8a}, {-1}};
const instruction txs[] = {{IMP, 0x9a}, {-1}};
const instruction tya[] = {{IMP, 0x98}, {-1}};

// Undocumented Instructions (NMOS 6502 only)
// names and information taken from http://www.oxyron.de/html/opcodes02.html
const instruction ahx[] = {{IDY, 0x93}, {ABY, 0x9f}, {-1}};
const instruction alr[] = {{IMM, 0x4b}, {-1}};
const instruction anc[] = {{IMM, 0x0b}, {-1}};
const instruction arr[] = {{IMM, 0x6b}, {-1}};
const instruction axs[] = {{IMM, 0xcb}, {-1}};
const instruction dcp[] = {{ZPG, 0xc7}, {ZPX, 0xd7}, {IDX, 0xc3}, {IDY, 0xd3}, {ABS, 0xcf}, {ABX, 0xdf}, {ABY, 0xdb}, {-1}};
const instruction isc[] = {{ZPG, 0xe7}, {ZPX, 0xf7}, {IDX, 0xe3}, {IDY, 0xf3}, {ABS, 0xef}, {ABX, 0xff}, {ABY, 0xfb}, {-1}};
const instruction las[] = {{ABY, 0xbb}, {-1}};
const instruction lax[] = {{ZPG, 0xa7}, {ZPY, 0xb7}, {IDX, 0xa3}, {IDY, 0xb3}, {ABS, 0xaf}, {ABY, 0xbf}, {-1}};
const instruction rla[] = {{ZPG, 0x27}, {ZPX, 0x37}, {IDX, 0x23}, {IDY, 0x33}, {ABS, 0x2f}, {ABX, 0x3f}, {ABY, 0x3b}, {-1}};
const instruction rra[] = {{ZPG, 0x67}, {ZPX, 0x77}, {IDX, 0x63}, {IDY, 0x73}, {ABS, 0x6f}, {ABX, 0x7f}, {ABY, 0x7b}, {-1}};
const instruction sax[] = {{ZPG, 0x87}, {ZPY, 0x97}, {IDX, 0x83}, {ABS, 0x8f}, {-1}};
const instruction shx[] = {{ABY, 0x9e}, {-1}};
const instruction shy[] = {{ABX, 0x9c}, {-1}};
const instruction slo[] = {{ZPG, 0x07}, {ZPX, 0x17}, {IDX, 0x03}, {IDY, 0x13}, {ABS, 0x0f}, {ABX, 0x1f}, {ABY, 0x1b}, {-1}};
const instruction sre[] = {{ZPG, 0x47}, {ZPX, 0x57}, {IDX, 0x43}, {IDY, 0x53}, {ABS, 0x4f}, {ABX, 0x5f}, {ABY, 0x5b}, {-1}};
const instruction tas[] = {{ABY, 0x9b}, {-1}};
const instruction xaa[] = {{IMM, 0x8b}, {-1}};

const struct
{
    const char *name;
    const instruction *opcodes;
} instr_list[] = {
    {"ADC", adc},
    {"AND", and},
    {"ASL", asl},
    {"BCC", bcc},
    {"BCS", bcs},
    {"BEQ", beq},
    {"BIT", bit},
    {"BMI", bmi},
    {"BNE", bne},
    {"BPL", bpl},
    {"BRK", brk},
    {"BVC", bvc},
    {"BVS", bvs},
    {"CLC", clc},
    {"CLD", cld},
    {"CLI", cli},
    {"CLV", clv},
    {"CMP", cmp},
    {"CPX", cpx},
    {"CPY", cpy},
    {"DEC", dec},
    {"DEX", dex},
    {"DEY", dey},
    {"EOR", eor},
    {"INC", inc},
    {"INX", inx},
    {"INY", iny},
    {"JMP", jmp},
    {"JSR", jsr},
    {"LDA", lda},
    {"LDX", ldx},
    {"LDY", ldy},
    {"LSR", lsr},
    {"NOP", nop},
    {"ORA", ora},
    {"PHA", pha},
    {"PHP", php},
    {"PLA", pla},
    {"PLP", plp},
    {"ROL", rol},
    {"ROR", ror},
    {"RTI", rti},
    {"RTS", rts},
    {"SBC", sbc},
    {"SEC", sec},
    {"SED", sed},
    {"SEI", sei},
    {"STA", sta},
    {"STX", stx},
    {"STY", sty},
    {"TAX", tax},
    {"TAY", tay},
    {"TSX", tsx},
    {"TXA", txa},
    {"TXS", txs},
    {"TYA", tya},

    /* undocumented instructions */
    {"AHX", ahx},
    {"ALR", alr},
    {"ANC", anc},
    {"ARR", arr},
    {"AXS", axs},
    {"DCP", dcp},
    {"ISC", isc},
    {"LAS", las},
    {"LAX", lax},
    {"RLA", rla},
    {"RRA", rra},
    {"SAX", sax},
    {"SHX", shx},
    {"SHY", shy},
    {"SLO", slo},
    {"SRE", sre},
    {"TAS", tas},
    {"XAA", xaa},

    {NULL, NULL},
};

const char *undocumented_list[] = {
    "AHX", "ALR", "ANC", "ARR", "AXS", "DCP", "ISC", "LAS", "LAX", "RLA",
    "RRA", "SAX", "SHX", "SHY", "SLO", "SRE", "TAS", "XAA", NULL};

const struct
{
    const char *name;
    void (*func)(symbol *, char **);
} directives[] = {
    {"", nothing},
    {"=", equal},
    {".ALIGN", align},
    {".BASE", base},
    {".BIN", incbin},
    {".DB", db},
    {".DH", dh},
    {".DL", dl},
    {".DSB", dsb},
    {".DSW", dsw},
    {".DW", dw},
    {".ELSE", _else},
    {".ELSEIF", elseif},
    {".ENDE", ende},
    {".ENDIF", endif},
    {".ENDINL", endinl},
    {".ENDM", endm},
    {".ENDR", endr},
    {".ENUM", _enum},
    {".EQU", equ},
    {".ERROR", make_error},
    {".FILLVALUE", fillval},
    {".HEX", hex},
    {".IF", _if},
    {".IFDEF", ifdef},
    {".IFNDEF", ifndef},
    {".IGNORENL", ignorenl},
    {".INCBIN", incbin},
    {".INCLUDE", include},
    {".INCSRC", include},
    {".INESCHR", ineschr},
    {".INESMAP", inesmap},
    {".INESMIR", inesmir},
    {".INESPRG", inesprg},
    {".MACRO", macro},
    {".NES2BRAM", nes2bram},
    {".NES2CHRBRAM", nes2chrbram},
    {".NES2CHRRAM", nes2chrram},
    {".NES2PRGRAM", nes2prgram},
    {".NES2SUB", nes2sub},
    {".NES2TV", nes2tv},
    {".NES2VS", nes2vs},
    {".ORG", org},
    {".PAD", pad},
    {".REPT", rept},
    {".UNDOC", undoc},
    {NULL, NULL},
};

/* error messages */
const char *BadAddr = "Can't determine address.";
const char *BadIncbinSize = "INCBIN size is out of range.";
const char *BranchOutOfRange = "Branch out of range.";
const char *CantOpen = "Can't open file.";
const char *DivZero = "Divide by zero.";
const char *ExtraChars = "Extra characters on line.";
const char *ExtraELSE = "ELSE without IF.";
const char *ExtraELSEIF = "ELSEIF without IF.";
const char *ExtraENDE = "ENDE without ENUM.";
const char *ExtraENDIF = "ENDIF without IF.";
const char *ExtraENDINL = "ENDINL without IGNORENL.";
const char *ExtraENDM = "ENDM without MACRO.";
const char *ExtraENDR = "ENDR without REPT.";
const char *IfNestLimit = "Too many nested IFs.";
const char *Illegal = "Illegal instruction.";
const char *IncompleteExp = "Incomplete expression.";
const char *LabelDefined = "Label already defined.";
const char *MacroInExpression = "Can't use macro in expression.";
const char *MissingOperand = "Missing operand.";
const char *NeedName = "Need a name.";
const char *NoENDE = "Missing ENDE.";
const char *NoENDIF = "Missing ENDIF.";
const char *NoENDINL = "Missing ENDINL.";
const char *NoENDM = "Missing ENDM.";
const char *NoENDR = "Missing ENDR.";
const char *NotANumber = "Not a number.";
const char *OutOfRange = "Value out of range.";
const char *PCOutOfRange = "PC out of range.";
const char *RecurseEQU = "Recursive EQU not allowed.";
const char *RecurseMACRO = "Recursive MACRO not allowed.";
const char *SeekOutOfRange = "Seek position out of range.";
const char *UndefinedPC = "PC is undefined (use ORG first).";
const char *UnknownLabel = "Unknown label.";
const char *WriteFileError = "Error writing file.";
const char *CreateFileError = "Can't create file.";
const char *BadLabel = "Invalid label name.";

#define WHITESPACE " \t\v\r\n"
const char *whitespace = WHITESPACE;
const char *delimiters = "!^&|+-*/%()<>=,:";

static char tmpstr[LINEMAX]; // all purpose big string

FILE *outputfile = NULL;
FILE *listfile = NULL;
FILE *cdlfile = NULL;

char *currentfilepath = NULL; // path of current included file
char *inputfilename = NULL;   // full input file name
char *outputfilename = NULL;
char *listfilename = NULL;
char *cdlfilename = NULL;
char *nlfilename = NULL;
char *mesenfilename = NULL;
char *luafilename = NULL;

bool verboselisting = false; // expand REPT and MACRO in list file
bool genlist = false;        // generate list file
bool gencdl = false;         // generate CDL file
bool gennl = false;          // generate FCEUX .nl files for symbolic debugging
bool genmesen = false;       // generate label files for use with Mesen
bool genlua = false;         // generate lua symbol file
int filepos = 0;

int pass = 0;            // current assembly pass
int scope;               // current scope, 0 = global
int nextscope;           // next nonglobal scope (increment on each new block of localized code)
bool lastchance = false; // set on final attempt
bool needanotherpass;    // still need to take care of some things..
bool verbose = true;     // controls if message() outputs anything

int iflevel = 0;       // index into ifdone[], skipline[]
int ifdone[IFNESTS];   // nonzero if current IF level has been true
int skipline[IFNESTS]; // 1 on an IF statement that is false

// these fuckers get set checked and reset all over the god damn place
bool dependant;         // set to true if symbol couldn't be resolved
bool harderror = false; // hard error (stop assembly after this pass)
const char *errmsg = NULL;
const char *listerr = NULL; // error message to show in the list file

size_t outcount; // bytes waiting in outputbuff
byte outputbuff[BUFFSIZE];
byte inputbuff[BUFFSIZE];

symbol **symbol_table; // array of symbol pointers (list starts from center and grows outward)
int symbol_count;      // number of symbols in symbol_table
int max_symbols;       // max number of unique symbols symbol_table can hold
int symbol_start;      // index of first symbol
int symbol_end;        // index of last symbol

symbol *lastlabel;        // last label created
symbol *labelhere = NULL; // points to the label being defined on the current line (for EQU, =, etc)

comment **comments;
int commentcount;
int commentcapacity;
int lastcommentpos = -1;

linked_node **macro_next = NULL; // (during macro creation) where next macro line will go. true_ptr to skip past macro
linked_node **rept_tail = NULL;  // like macro_next.. points to end of string chain
linked_node *rept_head;          // start of the rept chain
int insidemacro = 0;             // macro/rept is being expanded
int reptcount = 0;               // counts nested rept statements during rept string storage
int rept_loops;

int defaultfiller;               // default fill value
bool inside_enum = false;        // flag if we're inside an enum definition, supress output
bool nonl = false;               // supress output to .nl files
bool allow_undocumented = false; // allow undocumented instructions

// nicklausw ines stuff
bool ines_include = false;
int inesprg_num = 0;
int ineschr_num = 0;
int inesmir_num = 0;
int inesmap_num = 0;

bool use_nes2 = false;
int nes2chr_num = 0;
int nes2prg_num = 0;
int nes2sub_num = 0;
int nes2tv_num = 0;
int nes2vs_num = 0;
int nes2wram_num = 0;
int nes2bram_num = 0;
int nes2chrbram_num = 0;

// Prints printf-style message to stderr, then exits.
// Closes and deletes output file.
static void fatal_error(const char fmt[], ...)
{
    va_list args;

    if (outputfile != NULL)
    {
        fclose(outputfile);
        remove(outputfilename);
    }

    va_start(args, fmt);
    fprintf(stderr, "\nError: ");
    vfprintf(stderr, fmt, args);
    fprintf(stderr, "\n\n");
    va_end(args);

    exit(EXIT_FAILURE);
}

// sets harderror, prints errmsg with file and line number info,
// sets listerr if it is NULL
void showerror(char *errsrc, int errline)
{
    harderror = true;
    fprintf(stderr, "%s(%i): %s\n", errsrc, errline, errmsg);
    if (!listerr) // only list the first error for this line
        listerr = errmsg;
}

// Prints printf-style message if verbose mode is enabled.
static void message(const char fmt[], ...)
{
    if (verbose)
    {
        va_list args;
        va_start(args, fmt);
        vprintf(fmt, args);
        va_end(args);
    }
}

static void warn(const char *restrict format, ...)
{
    va_list args;
    va_start(args, format);
    fputs("Warning: ", stderr);
    vfprintf(stderr, format, args);
    va_end(args);
}

// TODO display error info more nicely
static void errorinfo(const char *restrict format, ...)
{
    va_list args;
    va_start(args, format);
    fputs("info: ", stderr);
    vfprintf(stderr, format, args);
    va_end(args);
}

// Same as malloc, but prints error and exits if allocation fails.
// A size of zero is an error
static void *my_malloc(size_t size)
{
    if (size == 0)
        fatal_error("zero size malloc");
    void *ptr = malloc(size);
    if (ptr == NULL)
        fatal_error("out of memory");
    return ptr;
}

// Same as realloc, but prints error and exits if allocation fails.
// A size of zero is an error
static void *my_realloc(void *ptr, size_t new_size)
{
    if (new_size == 0)
        fatal_error("zero size realloc");
    void *new = realloc(ptr, new_size);
    if (new == NULL)
        fatal_error("out of memory");
    return new;
}

// Same as regular strdup, but prints error and exits if allocation fails
static char *my_strdup(const char *s)
{
    size_t len = strlen(s) + 1;
    void *new = my_malloc(len);
    return (char *)memcpy(new, s, len);
}

// converts a string to uppercase, returns string
char *my_strupr(char *string)
{
    if (string == NULL)
        return NULL;

    for (char *s = string; *s; s++)
        *s = toupper(*s);

    return string;
}

// Converts a single hexadecimal character to an integer
// Sets errmsg on error
int hexify(int i)
{
    if (i >= '0' && i <= '9')
        return i - '0';
    if (i >= 'a' && i <= 'f')
        return i - 'a' + 10;
    if (i >= 'A' && i <= 'F')
        return i - 'A' + 10;
    THROW(NotANumber);
}

// find end of str, excluding any chars in ignore
// points to the character after the last character not in ignore
char *strend(char *str, const char *ignore)
{
    char *end = str + strlen(str);
    const char *w = ignore;
    while (*w && end != str)
    {
        char c = end[-1];
        for (w = ignore; *w; w++)
        {
            if (c == *w)
            {
                end--;
                break;
            }
        }
    }
    return end;
}

// decode str into a number
// set errmsg on error
int getvalue(char **str)
{
    static char gvline[WORDMAX];
    getword(gvline, str, true);
    char *s = gvline;
    if (!*s)
        THROW(MissingOperand);

    int ret = 0;
    if (*s == '$')
    {
        s++;
        if (!*s)
        { // $ by itself is the PC
            ret = PC;
        }
        else
        { // hex
            int chars = 0;
            do
            {
                int j = hexify(*s);
                PROPOGATE_ERROR;
                s++;
                chars++;
                ret = (ret << 4) | j;
            } while (*s);
            if (chars > 8)
                THROW(OutOfRange);
        }
    }
    else if (*s == '%')
    { // binary
        s++;
        int chars = 0;
        do
        {
            int j = *s;
            s++;
            chars++;
            j -= '0';
            if (j > 1)
                THROW(NotANumber);
            ret = (ret << 1) | j;
        } while (*s);
        if (chars > 32)
            THROW(OutOfRange);
    }
    else if (*s == '\'')
    { // char
        s++;
        if (*s == '\\')
            s++;
        ret = *s;
        s++;
        if (*s != '\'')
            THROW(NotANumber);
    }
    else if (*s >= '0' && *s <= '9')
    { // decimal
        if (strspn(s, "0123456789") == strlen(s))
            ret = atoi(s);
        else
            THROW(NotANumber);
    }
    else
    { // symbol
        symbol *p = findsymbol(gvline);
        if (!p)
        { // symbol doesn't exist (yet?)
            needanotherpass = true;
            dependant = true;
            if (lastchance) // only error once we're certain symbol will never exist
                THROW(UnknownLabel);
        }
        else
        {
            if (!p->known)
            {
                dependant = true;
                needanotherpass = true;
            }

            if (p->type == LABEL || p->type == VALUE)
                ret = p->value;
            else if (p->type == MACRO)
                THROW(MacroInExpression);
            else
                THROW(UnknownLabel); // trying to use a reserved word as a label
        }
    }
    return ret;
}

// get binary operator from str and advance str
enum operator getoperator(char **str)
{
    *str += strspn(*str, whitespace);
    switch (*(*str)++)
    {
    case '&':
        if (**str == '&')
        {
            (*str)++;
            return ANDAND;
        }
        else
            return AND;
    case '|':
        if (**str == '|')
        {
            (*str)++;
            return OROR;
        }
        else
            return OR;
    case '^':
        return XOR;
    case '+':
        return PLUS;
    case '-':
        return MINUS;
    case '*':
        return MUL;
    case '%':
        return MOD;
    case '/':
        return DIV;
    case '>':
        if (**str == '=')
        {
            (*str)++;
            return GREATEREQ;
        }
        else if (**str == '>')
        {
            (*str)++;
            return RIGHTSHIFT;
        }
        else
            return GREATER;
    case '<':
        if (**str == '=')
        {
            (*str)++;
            return LESSEQ;
        }
        else if (**str == '<')
        {
            (*str)++;
            return LEFTSHIFT;
        }
        else
            return LESS;
    case '=':
        if (**str == '=')
        {
            (*str)++;
            return EQUAL;
        }
        break;
    case '!':
        if (**str == '=')
        {
            (*str)++;
            return NOTEQUAL;
        }
        break;
    }

    (*str)--;
    return NOOP;
}

// evaluate expression in str and advance str
// sets errmsg on error
// if errmsg is set, the return value is undefined
int eval(char **str, enum prectype precedence)
{
    int ret;

    *str += strspn(*str, whitespace);
    char *s = *str;
    char unary = s[0];
    switch (unary)
    {
    case '(':
        s++;
        ret = eval(&s, WHOLEEXP);
        s += strspn(s, whitespace);
        if (*s == ')')
            s++;
        else
        {
            *str = s;
            THROW(IncompleteExp);
        }
        break;
    case '~':
        s++;
        ret = ~eval(&s, UNARY);
        break;
    case '!':
        s++;
        ret = !eval(&s, UNARY);
        break;
    case '<':
        s++;
        ret = eval(&s, UNARY) & 0xff;
        break;
    case '>':
        s++;
        ret = eval(&s, UNARY) >> 8 & 0xff;
        break;
    case '+':
    case '-':;
        // careful, might be +/- label
        bool val2 = needanotherpass;
        bool temp = dependant; // eval is reentrant so don't mess up dependant

        char *s2 = s;
        dependant = false;
        ret = getvalue(&s2);
        if (errmsg == UnknownLabel)
            errmsg = NULL;
        s++;

        if (!dependant || s2 == s)
        { // found something or a single +/-
            s = s2;
            s2 = NULL; // flag that we got something
            if (temp)
                dependant = true;
        }
        else
        { // not a label after all
            dependant = temp;
            needanotherpass = val2;
        }

        if (s2)
        { // if it wasn't a +/- label
            ret = eval(&s, UNARY);
            if (unary == '-')
                ret = -ret;
        }
        break;
    case ':':
        // if the user tries to define a label with the same name as an
        // instruction or directive, the colon (possibly followed by text) will
        // be interperated as an operand or argument. it would then be
        // interpereted as a label in the call to getvalue, and the error
        // wouldn't be caught until the final pass when getvalue determines it's
        // an unknown label. Catching it here means the error will be shown on
        // the first pass, and a better diagnostic can be given.
        THROW("Unexpected colon.");
    default:
        ret = getvalue(&s);
    }

    enum operator op;
    do
    {
        *str = s;
        op = getoperator(&s);
        if (prec[op] > precedence)
        { // if the precedence of the op we just read is higher than our own, evaluate it first
            int val2 = eval(&s, prec[op]);

            if (!dependant)
            {
                switch (op)
                {
                case AND:
                    ret &= val2;
                    break;
                case ANDAND:
                    ret = ret && val2;
                    break;
                case OR:
                    ret |= val2;
                    break;
                case OROR:
                    ret = ret || val2;
                    break;
                case XOR:
                    ret ^= val2;
                    break;
                case PLUS:
                    ret += val2;
                    break;
                case MINUS:
                    ret -= val2;
                    break;
                case MUL:
                    ret *= val2;
                    break;
                case DIV:
                    if (val2 == 0)
                        THROW(DivZero);
                    else
                        ret /= val2;
                    break;
                case MOD:
                    if (val2 == 0)
                        THROW(DivZero);
                    else
                        ret %= val2;
                    break;
                case EQUAL:
                    ret = (ret == val2);
                    break;
                case NOTEQUAL:
                    ret = (ret != val2);
                    break;
                case GREATER:
                    ret = ret > val2;
                    break;
                case GREATEREQ:
                    ret = ret >= val2;
                    break;
                case LESS:
                    ret = ret < val2;
                    break;
                case LESSEQ:
                    ret = ret <= val2;
                    break;
                case LEFTSHIFT:
                    ret <<= val2;
                    break;
                case RIGHTSHIFT:
                    ret >>= val2;
                    break;
                }
            }
            else
            { // dependant
                ret = 0;
            }
        }
    } while (prec[op] > precedence && !errmsg);
    return ret;
}

// copy next word from src into dst and advance src
// also stop at mathy chars if mcheck is true (false for filenames, etc)
// uses strtok so its kinda quirky
// assumes dst is WORDMAX size
void getword(char *dst, char **src, bool mcheck)
{
    // TODO clean up the unneccecary copying
    *src += strspn(*src, whitespace); // eat leading whitespace
    *dst = '\0';
    strncat(dst, *src, WORDMAX);
    strtok(dst, whitespace); // cut off trailing whitespace
    if (mcheck)
        strtok(dst, delimiters);
    *src += strlen(dst);
    // TODO how does removing this affect listings?
    // if (**src == ':')
    //     (*src)++; // cheesy fix for rept/macro listing
}

// grab and resolve filename relative to current file
// TODO add an option for absolute paths
void getfilename(char *dst, char **next)
{
    *next += strspn(*next, whitespace);
    size_t len = strcspn(*next, WHITESPACE ",");
    strcpy(dst, currentfilepath);
    strncat(dst, *next, len);
    *next += len;
}

// get word in src, advance src, and return label* if
// the word is a valid reserved word or macro call
// sets errmsg and returns NULL if there is no valid label
// TODO why do we set error if NULL already signifies an error?
symbol *getreserved(char **src)
{
    char dst[WORDMAX];
    char upp[WORDMAX];
    symbol *p = NULL;

    *src += strspn(*src, whitespace);
    if (**src == '=')
    { // special '=' reserved word
        // TODO figure out what the hell this means
        // I think this is a hack to get around getword chopping off '=' characters
        upp[0] = '=';
        upp[1] = '\0';
        (*src)++;
    }
    else
    {
        getword(dst, src, true);
        strcpy(upp, dst);
        my_strupr(upp);
    }

    p = findsymbol(upp); // case insensitive reserved word
    if (!p)
        p = findsymbol(dst); // or case sensitive macro
    if (p)
    {
        if (p->type == MACRO)
        {
            if (p->pass != pass)
                p = NULL;
        }
        else if (p->type != RESERVED)
            p = NULL;
    }

    if (!p)
        THROW(Illegal);

    return p;
}

// returns true if the word is a valid label.
// only checks for valid syntax, not semantics.
// word must not contain leading or trailing whitespace.
bool labelvalid(const char *word)
{
    if (strlen(word) == 1)
    {
        if (*word == '$')
            return true; // PC label

        if (toupper(*word) == 'A')
            return false; // Accumulator cant be a label
    }

    const char *s = word;

    if (*word == '+' || *word == '-')
    { // direcitonal label
        do
            ++s;
        while (*s == *word);
        if (!*s) // pure directional label
            return true;
    }

    if (*s == LOCALCHAR || *s == '_' || isalpha(*s))
    { // label can start with these
        return true;
    }

    return false;
}

// copy word to dst and advance src
// return true if it is a valid label name
// otherwise set errmsg and return false
bool readlabel(char *dst, char **src)
{
    getword(dst, src, true);
    if (labelvalid(dst))
        return true;
    THROW(BadLabel);
}

// preprocessing step
// Expand all equates from src into dst, and remove comment
// returns a pointer to the comment in src if there is one, or null.
// sets errmsg if it encounters a recursive equate
// CRIPES what a mess...
char *expandline(char *dst, char *src)
{
    char *start;
    char *comment = NULL;
    char c2;
    symbol *p;
    bool def_skip = false;

    char c;
    do
    {
        c = *src;
        if (c == '$' || (c >= '0' && c <= '9'))
        { // read past numbers (could be mistaken for a symbol, i.e. $BEEF)
            do
            {
                *dst = c;
                src++;
                dst++;
                c = *src;
            } while ((c >= '0' && c <= '9') || (c >= 'A' && c <= 'F') || (c >= 'a' && c <= 'f'));
            c = true; // don't terminate yet
        }
        else if (c == '"' || c == '\'')
        { // read past quotes
            *dst++ = *src++;
            do
            {
                *dst++ = c2 = *src++;
                if (c2 == '\\')
                    *dst++ = *src++;

            } while (c2 && c2 != c);
            c = c2;
        }
        else if (c == '_' || c == '.' || c == LOCALCHAR || (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z'))
        { // symbol
            start = src;
            do
            { // scan to end of symbol
                src++;
                c = *src;
            } while (c == '_' || c == '.' || c == LOCALCHAR || (c >= '0' && c <= '9') || (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z'));
            c = *src;
            *src = '\0'; // terminate at end of word (temporarily)

            p = NULL;
            if (!def_skip)
            {
                /*
                ghey hack.
                expandline() is called quite early during parsing, so

                FOO .equ xxxx
                .ifdef FOO

                  becomes

                FOO .equ xxxx
                .ifdef xxxx

                rendering IFDEF useless, so we will bypass expansion in this special case.
                I'm avoiding getreserved() because it does searching and other unnecessary crap.
                */
                char upp[WORDMAX];
                strncpy(upp, start, WORDMAX);
                my_strupr(upp);
                if (strcmp(upp, ".IFDEF") == 0 || strcmp(upp, ".IFNDEF") == 0)
                    def_skip = true;
                else
                    p = findsymbol(start);
            }

            if (p)
            { // p should be considered for expansion

                // equates MUST be defined before being used otherwise they will be expanded in their own definition
                // i.e. (label equ whatever) gets translated to (whatever equ whatever)
                if (p->type == EQUATE && p->pass == pass)
                {
                    if (p->used)
                    {
                        p = NULL;
                        THROW(RecurseEQU);
                    }
                    // else expand it
                }
                else
                { // p is not an equate or it hasn't been defined yet this pass
                    p = NULL;
                }
            }

            if (p)
            { // p is an equate that should be expanded
                p->used = true;
                expandline(dst, p->line); // expand the equate into dst
                p->used = false;
            }
            else
            { // just copy the word over
                strcpy(dst, start);
            }
            dst += strlen(dst);
            *src = c; // unterminate at the end of the word
        }
        else
        {
            if (c == ';')
            { // comment
                c = '\0';
                comment = src;
            }
            *dst++ = c;
            src++;
        }
    } while (c);

    return comment;
}

//
bool eatchar(char **str, char c)
{
    if (c)
    {
        *str += strspn(*str, whitespace);
        if (**str == c)
        {
            (*str)++;
            return true;
        }
        else
            return false;
    }
    return true;
}

// reverse src into dst
void reverse(char *dst, const char *src)
{
    dst += strlen(src);
    *dst = '\0';
    while (*src)
        *--dst = *src++;
}

void export_nl()
{
    // iterate through all the labels and output FCEUX-compatible label info files
    // based on their type (LABEL's,EQUATE's,VALUE's), address (ram/rom), and position (bank)

    int bank;

    FILE *bankfiles[64];
    for (int i = 0; i < 64; i++)
        bankfiles[i] = NULL;

    // ram file: <outputfile base name>.ram.nl
    // bank files: <outputfile base name>.nes.bank<bank number in hex>.nl

    char filename[FILENAME_MAX];
    strcpy(filename, nlfilename);
    char *filename_ext = filename + strlen(filename);
    strcpy(filename_ext, ".ram.nl");

    FILE *ramfile = fopen(filename, "w");
    if (!ramfile)
        errmsg = CreateFileError;

    // the bank files are created ad-hoc before being written to.

    // iNES banks are 16kb. Subtracting the 16 byte header and dividing that by
    // $4000 will get us the bank number of a particular label.
    // this only applies to labels that are $8000 and up.
    // Anything below $8000 is assumed to be RAM.

    // todo: include EQUATES for other registers and variables

    char str[512];
    for (int i = symbol_start; i <= symbol_end; i++)
    {
        symbol *l = symbol_table[i];

        // ignore IGNORENL'd labels
        if (l->ignorenl)
            continue;

        if (l->value <= 0xFFFF &&
            (l->type == LABEL || ((l->type == EQUATE || l->type == VALUE) && strlen(l->name) > 1))) // TODO: improve filtering
        {
            sprintf(str, "$%04X#%s#\n", (unsigned)l->value, l->name);

            if (l->value < 0x8000)
            { // RAM
                fwrite(str, 1, strlen(str), ramfile);
            }
            else
            { // ROM
                bank = (l->pos - 16) / 0x4000;
                if (!bankfiles[bank])
                {
                    sprintf(filename_ext, ".%X.nl", bank);
                    bankfiles[bank] = fopen(filename, "w");
                    if (!bankfiles[bank])
                        errmsg = CreateFileError;
                }
                fwrite(str, 1, strlen(str), bankfiles[bank]);
                if (ferror(bankfiles[bank]))
                    errmsg = WriteFileError;
            }
        }
    }

    if (ramfile)
    {
        fclose(ramfile);
        message("%s.ram.nl written.\n", nlfilename);
    }
    for (int i = 0; i < 64; i++)
    {
        if (bankfiles[i])
        {
            fclose(bankfiles[i]);
            message("%s.%X.nl written.\n", nlfilename, i);
        }
    }
}

void export_lua()
{
    // iterate through all the labels and output Lua-compatible label info files

    FILE *lua_file = fopen(luafilename, "w");
    if (!lua_file)
        errmsg = CreateFileError;

    char str[512];
    for (int i = symbol_start; i <= symbol_end; i++)
    {
        symbol *l = symbol_table[i];
        if (
            (
                l->type == LABEL ||
                ((l->type == EQUATE || l->type == VALUE) && strlen(l->name) > 1))
            // no anonymous labels
            && l->name[0] != '-' && l->name[0] != '+')
        {
            sprintf(str, "%s = 0x%04X\n", l->name, (unsigned int)l->value);
            fwrite(str, 1, strlen(str), lua_file);
            if (ferror(lua_file))
                errmsg = WriteFileError;
        }
    }

    fclose(lua_file);
    message("%s written.\n", luafilename);
}

// used for sorting
int comparelabels(const void *arg1, const void *arg2)
{
    const symbol *a = *((symbol **)arg1);
    const symbol *b = *((symbol **)arg2);
    if (a->type > b->type)
        return 1;
    if (a->type < b->type)
        return -1;
    if (a->pos > b->pos)
        return 1;
    if (a->pos < b->pos)
        return -1;
    if (a->value > b->value)
        return 1;
    if (a->value < b->value)
        return -1;
    return strcmp(a->name, b->name);
}

// used for sorting
int comparecomments(const void *arg1, const void *arg2)
{
    const comment *a = *((comment **)arg1);
    const comment *b = *((comment **)arg2);
    if (a->pos > b->pos)
        return 1;
    if (a->pos < b->pos)
        return -1;
    return strcmp(a->text, b->text);
}

// TODO mesen labels are a bit of a mess... en :)
void export_mesen()
{
    // iterate through all the labels and output Mesen-compatible label files
    // based on their type (LABEL's,EQUATE's,VALUE's) and address (ram/rom)

    char *commenttext;
    symbol *l;
    char str[512];

    FILE *messen_file = fopen(mesenfilename, "w");
    if (!messen_file)
        errmsg = CreateFileError;

    int currentcomment = 0;
    qsort(symbol_table + symbol_start, symbol_end - symbol_start + 1, sizeof(symbol *), comparelabels);
    qsort(comments, commentcount, sizeof(comment *), comparecomments);

    for (int i = symbol_start; i <= symbol_end; i++)
    {
        l = symbol_table[i];

        if (l->value < 0 || l->value > 0xFFFF || l->name[0] == '+' || l->name[0] == '-')
        { // Ignore CHR & anonymous code labels
            continue;
        }

        if (l->type == LABEL)
        { // Labels in the actual code
            if (l->pos < 16)
            { // Ignore file header
                continue;
            }

            // Check if one or more comments match this address
            commenttext = NULL;
            while (currentcomment < commentcount)
            {
                comment *c = comments[currentcomment];

                if (c->pos < l->pos)
                { // This comment is for a line before the current code label, write it to the file right away
                    if (c->pos >= 16)
                    {
                        sprintf(str, "P:%04X::", (unsigned int)c->pos - 16);
                        fwrite((const void *)str, 1, strlen(str), messen_file);
                        fwrite((const void *)c->text, 1, strlen(c->text), messen_file);
                        fwrite("\n", 1, 1, messen_file);
                    }
                    currentcomment++;
                }
                else if (c->pos == l->pos)
                { // Same address, write it on the same line as the label
                    commenttext = c->text;
                    currentcomment++;
                    break;
                }
                else
                {
                    break;
                }
            }

            // Dump the label
            sprintf(str, "P:%04X:%s", (unsigned int)(l->pos - 16), l->name);
            fwrite((const void *)str, 1, strlen(str), messen_file);

            if (commenttext)
            {
                fwrite(":", 1, 1, messen_file);
                fwrite((const void *)commenttext, 1, strlen(commenttext), messen_file);
            }
            fwrite("\n", 1, 1, messen_file);
        }
        else if (l->type == VALUE || l->type == EQUATE)
        { // These are potentially aliases for variables in RAM, or read/write registers, etc.
            if (l->value < 0x2000)
            { // Assume nes internal RAM below $2000 (2kb)
                sprintf(str, "R:%04X:%s\n", (unsigned int)l->value, l->name);
            }
            else if (l->value >= 0x6000 && l->value < 0x8000)
            { // Assume save/work RAM ($6000-$7FFF), dump as both. (not the best solution - maybe add an option?)
                sprintf(str, "S:%04X:%s\n", (unsigned int)l->value - 0x6000, l->name);
                sprintf(str, "W:%04X:%s\n", (unsigned int)l->value - 0x6000, l->name);
            }
            else
            { // Assume a global register for everything else (e.g $8000 for mapper control, etc.)
                sprintf(str, "G:%04X:%s\n", (unsigned int)l->value, l->name);
            }
            fwrite((const void *)str, 1, strlen(str), messen_file);
        }
    }

    fclose(messen_file);
    message("%s written.\n", mesenfilename);
}

//local:
//  false: if label starts with LOCALCHAR, make it local, otherwise it's global
//  true: force label to be local (used for macros)
// if the label exists and is not a VALUE, errors LabelDefined
void addlabel(char *word, bool local)
{
    char c = *word;
    symbol *p = findsymbol(word);                        // check for exitsing labels
    if (p && local && p->scope == 0 && p->type != VALUE) // if it's global and we're local
        p = NULL;                                        // pretend we didn't see it (local label overrides global of the same name)
    // global labels advance scope
    if (c != LOCALCHAR && !local)
        scope = nextscope++;

    if (!p)
    { // new label
        labelhere = newlabel();
        if (!labelhere->name) // name already set if it's a duplicate
            labelhere->name = my_strdup(word);
        labelhere->type = LABEL; // assume it's a label.. could mutate into something else later
        labelhere->pass = pass;
        labelhere->value = PC;
        labelhere->known = (PC >= 0);
        labelhere->used = false;
        labelhere->pos = filepos;
        labelhere->ignorenl = nonl;

        if (c == LOCALCHAR || local)
        { // local
            labelhere->scope = scope;
        }
        else
        { // global
            labelhere->scope = 0;
        }
        lastlabel = labelhere;
    }
    else
    { // existing label
        labelhere = p;
        if (p->pass == pass && c != '-')
        { // if this label already encountered this pass and its not a back label
            if (p->type == VALUE)
                return; // VALUEs can be reassigned
            else
                THROW_VOID(LabelDefined);
        }
        else
        { // first time seen on this pass or a - label
            p->pass = pass;
            if (p->type == LABEL)
            {
                if (p->value != PC && c != '-')
                {
                    needanotherpass = true; // label position is still moving around
                    if (lastchance)
                        errmsg = BadAddr;
                }
                p->value = PC;
                p->pos = filepos;
                p->known = (PC >= 0);
                if (lastchance && PC < 0)
                    THROW_VOID(BadAddr);
            }
        }
    }
}

// initialize symbol table
// allocates table and adds opcodes and directives
void initsymboltable(void)
{
    symbol_count = 1;
    symbol_table = my_malloc(INITLISTSIZE * sizeof(symbol *));
    symbol_start = INITLISTSIZE / 2;
    symbol_end = symbol_start;
    symbol_table[symbol_start] = &firstlabel; // $ label
    max_symbols = INITLISTSIZE;

    // add reserved words to symbol list
    symbol *p;
    for (int i = 0; instr_list[i].name; i++)
    { // opcodes first
        findsymbol(instr_list[i].name);
        p = newlabel();
        p->name = instr_list[i].name;
        p->func = opcode;
        p->type = RESERVED;
        p->opcodes = instr_list[i].opcodes;
    }

    for (int i = 0; directives[i].name; i++)
    { // other reserved words now
        findsymbol(directives[i].name);
        p = newlabel();
        p->name = directives[i].name;
        p->func = directives[i].func;
        p->type = RESERVED;
    }

    lastlabel = p;
}

void initcomments(void)
{
    commentcount = 0;
    commentcapacity = INITCOMMENT;
    comments = my_malloc(commentcapacity * sizeof(comment *));
}

void growcommentlist(void)
{
    if (commentcount == commentcapacity)
    {
        commentcapacity *= 2;
        my_realloc(comments, commentcapacity * sizeof(comment *));
    }
}

void addcomment(char *text)
{
    static int oldpass = 0;
    if (oldpass != pass)
    {
        oldpass = pass;
        commentcount = 0;
    }

    text++; // ignore the leading ";"

    if (lastcommentpos == filepos)
    { // File position hasn't changed since last comment was added
        // Append comment to the previous comment, since they are for the same address
        comment *c = comments[commentcount - 1];
        char *oldtext = c->text;
        int oldtextlen = strlen(oldtext);
        char *newtext = my_malloc(oldtextlen + strlen(text) + 4);
        strcpy(newtext, oldtext);
        strcpy(newtext + oldtextlen, "\\n");

        // Get rid of last character (newline \n)
        strcpy(newtext + oldtextlen + 2, text);
        newtext[strlen(newtext) - 1] = '\0';
        c->text = newtext;
    }
    else
    { // Add a new comment
        growcommentlist();

        comment *c = my_malloc(sizeof(comment));
        c->pos = filepos;
        c->text = my_malloc(strlen(text) + 1);
        strcpy(c->text, text);

        // Get rid of last character (newline \n)
        // TODO: what about EOF or \r\n?
        c->text[strlen(text) - 1] = '\0';

        comments[commentcount] = c;
        commentcount++;

        lastcommentpos = filepos;
    }
}

int findcmp;   // (these are used by newlabel)
int findindex; //
// find symbol with this name
// returns symbol* if found (and scope/pass is correct), returns NULL if nothing found
// if name wasn't found, findindex points to where name would be inserted (name<symbol_table[findindex])
// if name was found but with wrong scope/pass, findcmp = 0 and returns NULL
// don't call if list is empty!
symbol *findsymbol(const char *name)
{
    symbol *p, *global;

    int head = symbol_start;
    int tail = symbol_end;
    findindex = symbol_start + symbol_count / 2;
    do
    { // binary search
        findcmp = strcmp(name, (symbol_table[findindex])->name);
        if (findcmp < 0)
        {
            tail = findindex - 1;
            findindex -= (tail - head) / 2 + 1;
        }
        else if (findcmp > 0)
        {
            head = findindex + 1;
            findindex += (tail - head) / 2 + 1;
        }
    } while (findcmp && (tail - head) >= 0);

    if (findcmp != 0)
    {
        // position findindex so that it points to the where this symbol would be inserted
        if (findcmp < 0)
            findindex++;
        return NULL;
    }
    p = symbol_table[findindex];

    // check scope: symbol only visible if p->scope = (scope or 0)
    global = NULL;
    if (*name == '+')
    { // forward labels need special treatment :P
        do
        {
            if (p->pass != pass)
            { // only consider forward labels if they were defined on a previous pass
                if (p->scope == 0)
                    global = p;
                if (p->scope == scope)
                    return p;
            }
            p = p->link;
        } while (p);
    }
    else
    {
        do
        {
            if (p->scope == 0)
                global = p;
            if (p->scope == scope)
                return p;
            p = p->link;
        } while (p);
    }
    return global; // return global symbol only if no locals were found
}

// double list capacity
void growlist(void)
{
    max_symbols *= 2;
    int newhead = max_symbols / 2 - symbol_count / 2;
    symbol **tmp = my_malloc(max_symbols * sizeof(symbol *));
    memcpy(tmp + newhead, symbol_table + symbol_start, symbol_count * sizeof(symbol *));
    free(symbol_table);
    symbol_table = tmp;
    findindex = findindex - symbol_start + newhead;
    symbol_start = newhead;
    symbol_end = newhead + symbol_count - 1;
}

// make new empty label and add it to list using result from last findsymbol
// ONLY use after calling findsymbol
// scope is always initialized to 0
// if findcmp = 0, name and link are initialized using symbol_table[findindex],
// otherwise both are NULL
// all other values are uninitialized (garbage)
symbol *newlabel(void)
{
    symbol *p = my_malloc(sizeof(symbol));
    p->name = NULL;
    p->scope = 0;
    p->link = NULL;

    if (findcmp == 0)
    { // new symbol with same name
        // link the old one to the new one and replace it
        p->name = symbol_table[findindex]->name;
        p->link = symbol_table[findindex];
        symbol_table[findindex] = p;
        return p;
    }

    if (symbol_start <= 0 || symbol_end >= max_symbols - 1) // make sure there's room to add
        growlist();

    symbol **start, **end;
    if (findindex > (symbol_start + symbol_count / 2))
    { // shift up
        start = &symbol_table[symbol_end];
        end = &symbol_table[findindex];
        for (; start >= end; start--)
            *(start + 1) = *start;
        symbol_end++;
    }
    else
    { // shift down
        start = &symbol_table[symbol_start];
        end = &symbol_table[findindex] - 1;
        for (; start <= end; start++)
            *(start - 1) = *start;
        symbol_start--;
    }
    *end = p;
    symbol_count++;
    return p;
}

// process the open file f
void processfile(FILE *f, char *filename)
{
}

void processmacroline(char *line, const char *comment)
{
    char *s = line;
    symbol *p = getreserved(&s);
    errmsg = NULL; // why?
    char *label_end = NULL;
    if (!p)
    { // there was a label, skip it, we're looking for .endm
        // s points into line between the label and the next token
        label_end = s;
        p = getreserved(&s);
    }

    if (p && p->type == RESERVED && p->func == endm)
    { // we have a .endm, possibly with a label
        comment = NULL;
        if (label_end)
        { // hide the .endm in case of label: .endm
            // this modifies line
            label_end[0] = '\n';
            label_end[1] = '\0';
        }
        else
        { // it's just .endm without a label, don't bother adding it
            macro_next = NULL;
        }
    }

    if (macro_next && macro_next != true_ptr)
    // macro_next will be true_ptr if we should skip over this macro
    {
        // add the line to the macro

        if (comment)
            // keep the comment for listing,
            // it'll get stripped off again when the macro gets processed
            strncat(line, comment, LINEMAX);

        // macro_next should always be a pointer to a NULL pointer
        *macro_next = my_malloc(sizeof(linked_node));
        (**macro_next).text = my_strdup(line);
        (**macro_next).next = NULL;
        macro_next = &((**macro_next).next);
    }

    if (p && p->type == RESERVED && p->func == endm)
        macro_next = NULL; // macro is done
}
// process single line
// src = source line
// filename = source file name
// lineno = source file line number
// noexcept garunteed
void processline(char *src, char *filename, int lineno)
{
    char line[LINEMAX]; // expanded line
    char word[WORDMAX];
    symbol *p;

    errmsg = NULL; // why?

    char *comment = expandline(line, src);
    if (errmsg)
    {
        showerror(filename, lineno);
        errmsg = NULL;
        return;
    }

    if (!insidemacro || verboselisting)
        listline(line, comment);

    char *s = line;

    if (macro_next)
    { // we're inside a macro definition
        processmacroline(line, comment);
        return;
    }

    if (reptcount)
    { // REPT definition is in progress?
        p = getreserved(&s);
        errmsg = NULL;
        char *endrept = NULL;
        if (!p)
        {
            endrept = s;
            p = getreserved(&s);
        }
        if (p)
        {
            if (p->func == rept)
            {
                ++reptcount; // keep track of nested repts
            }
            else if (p->func == endr)
            {
                if (!(--reptcount))
                {
                    comment = NULL;
                    if (endrept)
                    {
                        endrept[0] = '\n'; // hide "ENDR" in case of "label: ENDR"
                        endrept[1] = '\0';
                    }
                }
            }
        }
        if (reptcount || endrept)
        { // add this line to REPT body
            if (comment)
                strcat(line, comment); // keep comment for listing
            *rept_tail = my_malloc(sizeof(linked_node));
            (**rept_tail).text = my_strdup(line);
            (**rept_tail).next = NULL;
            rept_tail = &(**rept_tail).next;
        }
        if (!reptcount)
        { // end of REPT, expand the whole thing right now
            expandrept(lineno, filename);
        }
        return;
    }

    labelhere = NULL; // labelhere could still hold a label left over from a previous line.
                      // set to NULL in case there is directive that requires a label.
                      // i.e. any function that might set errmsg to NeedName.
    char *s2 = line;
    p = getreserved(&s);
    errmsg = NULL; // why?

    if (skipline[iflevel])
    { // conditional assembly.. no code generation
        if (!p)
        { // token wasnt a reserved word... ignore it and move on
            // it could have been a label, check the next token
            p = getreserved(&s);
            if (!p)
                return;
        }

        if (p->type != RESERVED)
            return; // p could be a macro and we don't want to type pun the param count

        if (p->func != _else &&
            p->func != elseif &&
            p->func != endif &&
            p->func != _if &&
            p->func != ifdef &&
            p->func != ifndef)
            return; // ignore any symbols that aren't conditional assembly directives
    }

    if (!p)
    { // not a reserved word, maybe a label?
        if (readlabel(word, &s2))
        {
            s2 += strspn(s2, whitespace);
            bool pure = strspn(word, "+") == strlen(word) || strspn(word, "-") == strlen(word);
            // require and eat : for label definitions, except for assignments and pure forward/backward labels
            if (pure || *s2 == '=' || *s2++ == ':')
                addlabel(word, insidemacro);
            else // not a reserved word and not a label
                errmsg = Illegal;
        }
        if (errmsg)
        { // fucked up label
            showerror(filename, lineno);
            return;
        }
        s = s2;
        p = getreserved(&s); // word after label:
    }

    if (p)
    { // p is a valid reserved word or macro call
        if (p->type == MACRO)
            expandmacro(p, &s, lineno, filename);
        else // p->type == RESERVED
            (p->func)(p, &s);
    }

    if (!errmsg)
    { // check for extra characters on the line
        s += strspn(s, whitespace);
        if (*s)
            errmsg = ExtraChars;
    }

    if (errmsg)
        showerror(filename, lineno);
    errmsg = NULL;
}

void showhelp(void)
{
    puts("\n"
         "asm6f " VERSION "\n"
         "Usage:  asm6f [-options] sourcefile [outputfile]\n"
         "\n"
         "    -?        show this help\n"
         "    -q        quiet mode (no output unless error)\n"
         "    -d<name>  define symbol with name\n"
         "    -l[name]  create listing optionally with a name\n"
         "    -L[name]  same as -l but verbose (expand REPT and MACRO calls)\n"
         "    -n[name]  export FCEUX-compatible .nl files\n"
         "    -f[name]  export Lua symbol file\n"
         "    -c[name]  export .cdl file for use with FCEUX or Mesen\n"
         "    -m[name]  export Mesen-compatible label file (.mlb)\n"
         "\n"
         "See README.MD for more info.\n");
}

// TODO unfuck the whole file path managment
int main(int argc, char **argv)
{
    if (argc < 2)
    {
        showhelp();
        return EXIT_FAILURE;
    }
    initsymboltable();
    initcomments();
    // process command line arguments
    int notoption = 0;
    for (int i = 1; i < argc; i++)
    {
        if (*argv[i] == '-' || (*argv[i] == '/' && strlen(argv[i]) == 2))
        {
            switch (argv[i][1])
            {
            case 'h':
            case '?':
                showhelp();
                return EXIT_FAILURE;
            case 'd':
                if (argv[i][2])
                {
                    if (!findsymbol(&argv[i][2]))
                    {
                        symbol *p = newlabel();
                        p->name = my_strdup(&argv[i][2]);
                        p->type = VALUE;
                        p->value = 1;
                        p->known = true;
                        p->pass = 0;
                    }
                }
                break;
            case 'q':
                verbose = false;
                break;
            case 'L':
                verboselisting = true;
                // fallthrough
            case 'l':
                genlist = true;
                if (argv[i][2])
                    listfilename = &argv[i][2];
                break;
            case 'c':
                gencdl = true;
                if (argv[i][2])
                    cdlfilename = &argv[i][2];
                break;
            case 'n':
                gennl = true;
                if (argv[i][2])
                    nlfilename = &argv[i][2];
                break;
            case 'm':
                genmesen = true;
                if (argv[i][2])
                    mesenfilename = &argv[i][2];
                break;
            case 'f':
                genlua = true;
                if (argv[i][2])
                    luafilename = &argv[i][2];
                break;
            default:
                fatal_error("unknown option: %s", argv[i]);
            }
        }
        else
        {
            if (notoption == 0)
                inputfilename = argv[i];
            else if (notoption == 1)
                outputfilename = argv[i];
            else
                fatal_error("extra argument: %s", argv[i]);
            notoption++;
        }
    }
    if (!inputfilename)
        fatal_error("No source file specified.");

    // TODO non-windows compatability

    // use default names for files that were asked for but no name was given
    char filename_temp[FILENAME_MAX];
    if (!strrchr(inputfilename, PATH_SEPERATOR)) // filename without a path
        strcpy(filename_temp, PATH_RELATIVE);    // assume relative path
    strcat(filename_temp, inputfilename);
    inputfilename = my_strdup(filename_temp);

    char *temp_ext = strrchr(filename_temp, '.');      // search for file extension
    if (!temp_ext || strchr(temp_ext, PATH_SEPERATOR)) // no extension on the file
        temp_ext = filename_temp + strlen(filename_temp);
    *temp_ext = '\0'; // cut off the extension if there is one

    if (!outputfilename)
    {
        strcpy(temp_ext, ".bin");
        outputfilename = my_strdup(filename_temp);
    }
    if (genlist && !listfilename)
    {
        strcpy(temp_ext, ".lst");
        listfilename = my_strdup(filename_temp);
    }
    if (gencdl && !cdlfilename)
    {
        strcpy(temp_ext, ".cdl");
        cdlfilename = my_strdup(filename_temp);
    }

    strrchr(filename_temp, PATH_SEPERATOR)[1] = '\0';
    currentfilepath = my_strdup(filename_temp);

    *filename_temp = '\0';
    if (!strrchr(outputfilename, PATH_SEPERATOR)) // filename without path
        strcpy(filename_temp, PATH_RELATIVE);     // assume relative path
    strcat(filename_temp, outputfilename);
    outputfilename = my_strdup(filename_temp);
    temp_ext = strrchr(filename_temp, '.');            // search for file extension
    if (!temp_ext || strchr(temp_ext, PATH_SEPERATOR)) // no extension on the file
        temp_ext = filename_temp + strlen(filename_temp);
    *temp_ext = '\0'; // cut off the extension if there is one

    if (gennl && !nlfilename)
    {
        strcpy(temp_ext, ".nes");
        nlfilename = my_strdup(filename_temp);
    }
    if (genmesen && !mesenfilename)
    {
        strcpy(temp_ext, ".mlb");
        mesenfilename = my_strdup(filename_temp);
    }
    if (genlua && !luafilename)
    {
        strcpy(temp_ext, ".lua");
        luafilename = my_strdup(filename_temp);
    }

    // main assembly loop
    symbol *p = NULL;
    do
    {
        filepos = 0;
        pass++;
        if (pass == MAXPASSES || p == lastlabel)
            lastchance = true; // give up on too many tries or no progress made
        if (lastchance)
            message("last try..\n");
        else
            message("pass %i..\n", pass);
        needanotherpass = false;
        skipline[0] = 0;
        scope = 1;
        nextscope = 2;
        defaultfiller = DEFAULTFILLER; // reset filler value
        PC = NOORIGIN;                 // undefine origin
        p = lastlabel;
        char *str = strrchr(inputfilename, PATH_SEPERATOR) + 1;
        include(NULL, &str); // start assembling srcfile
        if (errmsg)
        {                          // todo - should this set error?
            fputs(errmsg, stderr); // bad inputfile??
        }
        // while no hard errors, not final try, and labels are still unresolved
    } while (!harderror && !lastchance && needanotherpass);

    if (outputfile)
    {
        // Be sure last of output file is written properly
        if (fwrite(outputbuff, 1, outcount, outputfile) < outcount || fflush(outputfile))
            fatal_error(WriteFileError);

        long i = ftell(outputfile);

        int result = fclose(outputfile);
        outputfile = NULL; // prevent trying to close file again
        if (result)
            fatal_error(WriteFileError);

        if (!harderror)
            message("%s written (%i bytes).\n", outputfilename, i);
        else
            remove(outputfilename);
    }
    else
    {
        if (!harderror)
            fputs("nothing to do!", stderr);
        harderror = true;
    }

    if (listfile)
        listline(NULL, NULL);

    // only generate labelfiles if asked
    if (gennl)
        export_nl();
    if (genlua)
        export_lua();
    if (genmesen)
        export_mesen();

    return harderror ? EXIT_FAILURE : EXIT_SUCCESS;
}

byte listbuff[LISTMAX];
int listcount;
void output(const byte *p, int size, enum cdltype cdlflag)
{
    static int oldpass = 0;

    if (gencdl)
    {
        if (oldpass != pass)
        {
            if (cdlfile)
                fclose(cdlfile);
            cdlfile = fopen(cdlfilename, "wb");
        }

        if (cdlfile && (!ines_include || filepos >= 16)) // ignore first 16 bytes of iNES header
        {
            byte flag = (PC <= 0xFFFF)
                            ? cdlflag // PRG data
                            : NONE;   // CHR data
            int repeat = size;
            while (repeat--)
                fwrite(&flag, sizeof flag, 1, cdlfile);
        }
    }

    PC += size;

    if (inside_enum)
        return;

    if (oldpass != pass)
    {
        oldpass = pass;
        if (outputfile)
            fclose(outputfile);
        outputfile = fopen(outputfilename, "wb");
        filepos = 0;
        outcount = 0;
        if (!outputfile)
            THROW_VOID(CreateFileError);

        if (ines_include)
        { // insert iNES if needed
            const byte ines_header[16] = {
                'N', 'E', 'S', 0x1A,
                (byte)inesprg_num,
                (byte)ineschr_num,
                (byte)(inesmap_num << 4) | inesmir_num,
                (byte)(inesmap_num & 0xF0) | (use_nes2 << 3) | (nes2tv_num << 7),
                (byte)(inesmap_num >> 8) | (nes2sub_num << 4),
                (byte)(inesprg_num >> 8) | ((ineschr_num >> 8) << 4),
                (byte)(nes2bram_num << 4) | nes2prg_num,
                (byte)(nes2chrbram_num << 4) | nes2chr_num,
                (byte)nes2tv_num,
                0, 0, 0};

            fwrite(ines_header, sizeof(byte), 16, outputfile);
            if (ferror(outputfile))
                errmsg = WriteFileError;
            filepos += 16;
        }
    }
    if (!outputfile)
        return;

    while (size--)
    {
        if (listfile && listcount < LISTMAX)
            listbuff[listcount] = *p;
        listcount++;
        filepos++;
        outputbuff[outcount++] = *p;
        p++;
        if (outcount >= BUFFSIZE)
        {
            fwrite(outputbuff, sizeof(byte), BUFFSIZE, outputfile);
            if (ferror(outputfile))
                errmsg = WriteFileError;
            outcount = 0;
        }
    }
}

// Outputs an integer as little-endian
// if size is 1, only the lower byte is output
static void output_le(int n, int size, enum cdltype cdlflag)
{
    const byte b[2] = {n, n >> 8};
    output(b, size, cdlflag);
}

// end listing when src = NULL
void listline(char *src, char *comment)
{
    static char srcbuff[LINEMAX];
    static int oldpass = 0;
    if (!listfilename)
        return;
    if (oldpass != pass)
    { // new pass means new listfile
        oldpass = pass;
        if (listfile)
            fclose(listfile);
        listfile = fopen(listfilename, "w");
        if (!listfile)
        {
            listfilename = NULL; // stop trying
            // todo - if user wants a listing, this SHOULD be an error, otherwise
            // he might still have old listing and think it's the current one.
            // For example, he might have had it open in a text editor, preventing its
            // creation here.
            warn("Can't create list file.\n"); // not critical, just give a warning
            return;
        }
    }
    else
    { // finish previous line
        // why not output directly?
        int i = 0;
        for (; i < listcount && i < LISTMAX; i++)
            fprintf(listfile, " %02X", (int)listbuff[i]);
        for (; i < LISTMAX; i++)
            fprintf(listfile, "   ");
        fputs(listcount > LISTMAX ? ".. " : "   ", listfile);
        fputs(srcbuff, listfile);
        if (listerr)
        {
            fprintf(listfile, "*** %s\n", listerr);
            listerr = NULL;
        }
    }
    listcount = 0;

    if (src)
    {
        if (PC < 0)
            fprintf(listfile, "	 ");
        else
            fprintf(listfile, "%05X", (int)PC);
        strcpy(srcbuff, src); // make a copy of the original source line
        if (comment)
        {
            strcat(srcbuff, comment);
            if (genmesen && filepos > 0 && PC <= 0xFFFF)
            { // save this comment - needed for export
                addcomment(comment);
            }
        }
    }
    else
    {
        fclose(listfile);
        message("%s written.\n", listfilename);
    }
}

//------------------------------------------------------
// directive(symbol *id, char **next)
//
//  id = the reserved word used to call the directive
//       currently only used for opcodes and macros
//  **next = source line (gets moved past the directive and any arguments on return)
//------------------------------------------------------

void equ(symbol *id, char **next)
{
    char str[LINEMAX];
    char *s = *next;
    if (!labelhere)
        THROW_VOID(NeedName); // EQU without a name

    if (labelhere->type == LABEL)
    {                                              // new EQU.. good
        reverse(str, s + strspn(s, whitespace));   // eat whitespace off both ends
        reverse(s, str + strspn(str, whitespace)); // TODO why not use strtok?
        if (*s)
        {
            labelhere->line = my_strdup(s);
            labelhere->type = EQUATE;
        }
        else
        {
            errmsg = IncompleteExp;
        }
    }
    else if (labelhere->type != EQUATE)
    {
        errmsg = LabelDefined;
    }
    *s = '\0'; // end line
}

void equal(symbol *id, char **next)
{
    if (!labelhere)
        THROW_VOID(NeedName); // = without a name
    else
    {
        labelhere->type = VALUE;
        dependant = false;
        labelhere->value = eval(next, WHOLEEXP);
        labelhere->known = !dependant;
    }
}

void base(symbol *id, char **next)
{
    int val;
    dependant = false;
    val = eval(next, WHOLEEXP);
    if (!dependant && !errmsg)
        PC = val;
    else
        PC = NOORIGIN; // undefine origin
}

void nothing(symbol *id, char **next)
{
}

// TODO update currentfilepath to support multiple directories
void include(symbol *id, char **next)
{
    char filename[FILENAME_MAX];
    getfilename(filename, next);
    FILE *f = fopen(filename, "r+"); // read as text, the + makes recursion not possible

    if (!f)
    {
        harderror = true; // why?
        THROW_VOID(CantOpen);
    }

    static char line[LINEMAX];
    static int nest = 0;
    nest++; // count nested includes

    int lineno = 0;
    while (fgets(line, LINEMAX, f))
    {
        lineno++;
        processline(line, filename, lineno);
    }
    nest--;

    if (!nest)
    { // if main source file -- why not do this in main?
        if (iflevel)
            errmsg = NoENDIF;
        if (reptcount)
            errmsg = NoENDR;
        if (macro_next)
            errmsg = NoENDM;
        if (inside_enum)
            errmsg = NoENDE;
        if (nonl)
            errmsg = NoENDINL;
        if (errmsg)
            showerror(filename, lineno);
    }

    fclose(f);
    errmsg = NULL; // let main know file was ok -- why here?

    *next = filename + strlen(filename);
}

void incbin(symbol *id, char **next)
{
    // file open:
    char filename[FILENAME_MAX];
    getfilename(filename, next);
    FILE *f = fopen(filename, "rb");
    if (!f)
    {
        errmsg = CantOpen;
        goto fail;
    }
    fseek(f, 0, SEEK_END);
    int filesize = ftell(f);

    // file seek:
    int seekpos = 0;
    if (eatchar(next, ','))
        seekpos = eval(next, WHOLEEXP);
    if (!errmsg && !dependant)
        if (seekpos < 0 || seekpos > filesize)
            errmsg = SeekOutOfRange;
    if (errmsg)
        goto fail;
    fseek(f, seekpos, SEEK_SET);

    // get size:
    int bytesleft;
    if (eatchar(next, ','))
    {
        bytesleft = eval(next, WHOLEEXP);
        if (!errmsg && !dependant)
            if (bytesleft < 0 || bytesleft > (filesize - seekpos))
                errmsg = BadIncbinSize;
        if (errmsg)
            goto fail;
    }
    else
    {
        bytesleft = filesize - seekpos;
    }

    // read file:
    int i;
    while (bytesleft)
    {
        if (bytesleft > BUFFSIZE)
            i = BUFFSIZE;
        else
            i = bytesleft;
        fread(inputbuff, 1, i, f);
        output(inputbuff, i, DATA);
        bytesleft -= i;
    }

fail:
    if (f)
        fclose(f);
}

void hex(symbol *id, char **next)
{
    char buff[LINEMAX];
    char *src;
    int dst;
    char c1, c2;
    getword(buff, next, 0);
    if (!*buff)
        THROW_VOID(MissingOperand);
    else
    {
        do
        {
            src = buff;
            dst = 0;
            do
            {
                c1 = hexify(*src);
                src++;
                if (*src)
                {
                    c2 = hexify(*src);
                    src++;
                }
                else
                { // deal with odd number of chars
                    c2 = c1;
                    c1 = '\0';
                }
                buff[dst++] = (c1 << 4) + c2;
            } while (*src);
            output((byte *)buff, dst, DATA);
            getword(buff, next, 0);
        } while (*buff);
    }
}

void dw(symbol *id, char **next)
{
    int val;
    do
    {
        val = eval(next, WHOLEEXP);
        if (!errmsg)
        {
            if (val > 65535 || val < -65536)
                errmsg = OutOfRange;
            else
                output_le(val, 2, DATA);
        }
    } while (!errmsg && eatchar(next, ','));
}

void dl(symbol *id, char **next)
{
    byte val;
    do
    {
        val = eval(next, WHOLEEXP) & 0xff;
        if (!errmsg)
            output(&val, 1, DATA);
    } while (!errmsg && eatchar(next, ','));
}

void dh(symbol *id, char **next)
{
    byte val;
    do
    {
        val = eval(next, WHOLEEXP) >> 8;
        if (!errmsg)
            output(&val, 1, DATA);
    } while (!errmsg && eatchar(next, ','));
}

void db(symbol *id, char **next)
{
    int val, val2;
    byte *s, *start;
    char c, quote;

    do
    {
        *next += strspn(*next, whitespace);
        quote = **next;
        if (quote == '"' || quote == '\'')
        { // string
            s = start = (byte *)*next + 1;
            do
            {
                c = *s;
                s++;
                if (!c)
                    errmsg = IncompleteExp;
                if (c == '\\')
                    s++;
            } while (!errmsg && c != quote);
            if (errmsg)
                continue;
            s--; // point to the "
            *s = '0';
            *next = (char *)s;
            val2 = eval(next, WHOLEEXP);
            if (errmsg)
                continue;
            while (start != s)
            {
                if (*start == '\\')
                    start++;
                val = *start + val2;
                start++;
                output_le(val, 1, DATA);
            }
        }
        else
        {
            val = eval(next, WHOLEEXP);
            if (!errmsg)
            {
                if (val > 255 || val < -128)
                    errmsg = OutOfRange;
                else
                    output_le(val, 1, DATA);
            }
        }
    } while (!errmsg && eatchar(next, ','));
}

void dsw(symbol *id, char **next)
{
    int count, val = defaultfiller;
    dependant = false;
    count = eval(next, WHOLEEXP);
    if (dependant || (count < 0 && needanotherpass)) //unknown count! don't do anything
        count = 0;
    if (eatchar(next, ','))
        val = eval(next, WHOLEEXP);
    PROPOGATE_ERROR_VOID;
    if (!dependant)
        if (val > 65535 || val < -32768 || count < 0)
            THROW_VOID(OutOfRange);
    while (count--)
        output_le(val, 2, DATA);
}

void filler(int count, char **next)
{
    int val = defaultfiller;
    if (dependant || (count < 0 && needanotherpass)) //unknown count! don't do anything
        count = 0;
    if (eatchar(next, ','))
        val = eval(next, WHOLEEXP);
    PROPOGATE_ERROR_VOID;
    if (!dependant)
        if (val > 255 || val < -128 || count < 0 || count > 0x100000)
            THROW_VOID(OutOfRange);

    while (count--) //!#@$
        output_le(val, 1, NONE);
}

void dsb(symbol *id, char **next)
{
    dependant = false;
    int count = eval(next, WHOLEEXP);
    PROPOGATE_ERROR_VOID;
    filler(count, next);
}

void align(symbol *id, char **next)
{
    int count;
    dependant = false;
    count = eval(next, WHOLEEXP);
    if (count >= 0)
    {
        if ((unsigned int)PC % count)
            count -= (unsigned int)PC % count;
        else
            count = 0;
    }
    else
        count = 0;
    filler(count, next);
}

void pad(symbol *id, char **next)
{
    int count;
    if (PC < 0)
    {
        THROW_VOID(UndefinedPC);
    }
    else
    {
        dependant = false;
        count = eval(next, WHOLEEXP) - PC;
        filler(count, next);
    }
}

void org(symbol *id, char **next)
{
    if (PC < 0)
        base(id, next); //this is the first ORG; PC hasn't been set yet
    else
        pad(id, next);
}

void opcode(symbol *id, char **next)
{
    char *s;
    int val = 0;
    int oldstate = needanotherpass;
    int forceRel = 0;

    if (!allow_undocumented)
        for (int uns = 0; undocumented_list[uns]; uns++)
            if (!strcmp(id->name, undocumented_list[uns]))
                THROW_VOID("Undocumented instruction used without calling .undoc.");

    for (const instruction *op = id->opcodes; op->type != -1; op++)
    { // loop through all addressing modes for this instruction
        needanotherpass = oldstate;
        strcpy(tmpstr, *next);
        dependant = false;
        errmsg = NULL;
        s = tmpstr;

        // opsize will be zero if the optype takes no operand
        if (opsize[op->type] != 0)
        { // get the operand
            if (!eatchar(&s, ophead[op->type]))
                continue; // head doesn't match, try the next one

            val = eval(&s, WHOLEEXP);

            // bounds check the operand value
            if (op->type == REL)
            {
                if (!dependant)
                {
                    val -= PC + 2;
                    if (val > 127 || val < -128)
                    {
                        needanotherpass = true; // give labels time to sort themselves out..
                        if (lastchance)
                        {
                            errmsg = BranchOutOfRange;
                            forceRel = true;
                        }
                    }
                }
            } // op->type != REL
            else if (opsize[op->type] == 1)
            { // one byte operand
                if (!dependant)
                {
                    if (val > 255 || val < -128)
                        errmsg = OutOfRange;
                }
                else // TODO why are we checking for dependant here?
                {
                    if (op->type != IMM)
                        continue; // default to non-ZPG instruction
                }
            }
            else
            { // two byte operand
                if ((val < 0 || val > 0xffff) && !dependant)
                    errmsg = OutOfRange;
            }

            if (errmsg && !dependant && !forceRel)
                continue; // the value is bad for this address mode, try the next one
                          // if the value is bad because it is dependant, ignore the error
        }

        // check that the operand has the appropriate trailing characters
        my_strupr(s);
        const char *s2 = optail[op->type];
        while (*s2)
        {
            if (!eatchar(&s, *s2))
                break; // optail dosen't match, *s2 is the expected character
            s2++;
        }
        s += strspn(s, whitespace);
        if (*s || *s2)
            continue; // optail didn't match
                      // this could also mean there are extra characters on the line

        if (PC > 0xffff)
            errmsg = PCOutOfRange;

        // value and syntax ok, write the opcode and return
        output(&op->code, 1, CODE);
        output_le(val, opsize[op->type], CODE);
        *next += s - tmpstr;
        return;
    }
    // none of the optypes matched
    PROPOGATE_ERROR_VOID;
    THROW_VOID(Illegal);
}

void _if(symbol *id, char **next)
{
    int val;
    if (iflevel >= IFNESTS - 1)
        errmsg = IfNestLimit;
    else
        iflevel++;
    dependant = false;
    val = eval(next, WHOLEEXP);
    if (dependant || errmsg)
    { //don't process yet
        ifdone[iflevel] = 1;
        skipline[iflevel] = 1;
    }
    else
    {
        skipline[iflevel] = !val || skipline[iflevel - 1];
        ifdone[iflevel] = !skipline[iflevel];
    }
}

void ifdef(symbol *id, char **next)
{
    char s[WORDMAX];
    if (iflevel >= IFNESTS - 1)
        errmsg = IfNestLimit;
    else
        iflevel++;
    readlabel(s, next);
    skipline[iflevel] = !(ptrdiff_t)findsymbol(s) || skipline[iflevel - 1];
    ifdone[iflevel] = !skipline[iflevel];
}

void ifndef(symbol *id, char **next)
{
    char s[WORDMAX];
    if (iflevel >= IFNESTS - 1)
        errmsg = IfNestLimit;
    else
        iflevel++;
    readlabel(s, next);
    skipline[iflevel] = (ptrdiff_t)findsymbol(s) || skipline[iflevel - 1];
    ifdone[iflevel] = !skipline[iflevel];
}

void elseif(symbol *id, char **next)
{
    int val;
    if (iflevel)
    {
        dependant = false;
        val = eval(next, WHOLEEXP);
        if (!ifdone[iflevel])
        { // no previous true statements
            if (dependant || errmsg)
            { // don't process yet
                ifdone[iflevel] = 1;
                skipline[iflevel] = 1;
            }
            else
            {
                skipline[iflevel] = !val || skipline[iflevel - 1];
                ifdone[iflevel] = !skipline[iflevel];
            }
        }
        else
        {
            skipline[iflevel] = 1;
        }
    }
    else
        errmsg = ExtraELSEIF;
}

void _else(symbol *id, char **next)
{
    if (iflevel)
        skipline[iflevel] = ifdone[iflevel] || skipline[iflevel - 1];
    else
        errmsg = ExtraELSE;
}

void endif(symbol *id, char **next)
{
    if (iflevel)
        --iflevel;
    else
        THROW_VOID(ExtraENDIF);
}

void endm(symbol *id, char **next)
{ // ENDM is handled during macro definition (see processline)
    THROW_VOID(ExtraENDM);
}

void endr(symbol *id, char **next)
{ // ENDR is handled during macro definition (see processline)
    THROW_VOID(ExtraENDR);
}

void macro(symbol *id, char **next)
{
    macro_next = true_ptr; // tell processline to skip over the macro def,
                           // unless macro_next gets set

    if (labelhere)
        // if the user defines a macro like so
        //
        //      myCoolMacro: .macro param1
        //          ADC param1
        //      .endm
        //
        // they might expect a macro named .myCoolMacro
        warn("label %s does not name macro\n", labelhere->name);
    labelhere = NULL;

    char word[WORDMAX] = {'.'};
    if (readlabel(word + 1, next))
    {
        addlabel(word, false); // add the label with prepended '.'
        PROPOGATE_ERROR_VOID;
    }
    else
        THROW_VOID(NeedName);

    if (labelhere->type == LABEL)
    { // new macro
        labelhere->type = MACRO;
        labelhere->line_list = NULL;
        macro_next = &(labelhere->line_list);

        // build param list
        labelhere->value = 0; // param count

        *next += strspn(*next, whitespace);
        if (**next) // check if theres any params at all
        {
            do // add each param to the macro's line_list
            {
                readlabel(word, next);
                PROPOGATE_ERROR_VOID;
                (*macro_next) = my_malloc(sizeof(linked_node));
                (*macro_next)->text = my_strdup(word);
                (*macro_next)->next = NULL;
                macro_next = &((*macro_next)->next);
                labelhere->value++;
            } while (eatchar(next, ','));
        }
    }
    else if (labelhere->type == MACRO)
    { // macro was defined on a previous pass.. skip past params
        // what if the user tries to redefine a macro?
        macro_next = true_ptr;  // flag for processline to skip to ENDM
        *next += strlen(*next); // skip over param names
    }
    else
    { // label is already defined as something else
        THROW_VOID(LabelDefined);
    }
}

void expandmacro(symbol *id, char **next, int lineno, char *filename)
{
    if (id->used)
        THROW_VOID(RecurseMACRO);
    id->used = true;
    insidemacro++;
    int oldscope = scope;
    scope = nextscope++;

    char macrofilename[FILENAME_MAX + WORDMAX]; // The "filename" to display in an error
    snprintf(macrofilename, sizeof macrofilename, "%s(%i):%s", filename, lineno, id->name);
    linked_node *line = id->line_list;

    // define macro args
    char *s = *next;
    int args = id->value; // number of named args
    int arg = 0;
    do
    {
        // TODO test cases for strings in macro arguments
        s += strspn(s, whitespace);    // s = args start
        char *s2 = s;                  // s2 = arg end
        char *s3 = strpbrk(s2, ",\""); // stop at arg end or string definition
        if (!s3)
            s3 = strchr(s2, '\0');
        char c = *s3;
        if (c == '"')
        { // go to end of string
            s3++;
            char c2;
            do
            {
                c2 = *s3;
                s3++;
                if (c2 == '\\')
                    s3++;
            } while (c2 && c2 != c);
            if (!c2)
                s3--; // oops, too far
            c = *s3;
        }
        s2 = s3;
        *s2 = '\0';
        if (*s)
        { // arg not empty
            if (arg < args)
            { // make named arg
                char *temp = s;
                addlabel(line->text, true);
                PROPOGATE_ERROR_VOID;
                equ(NULL, &temp);
                PROPOGATE_ERROR_VOID;
                line = line->next; // next arg name
            }
            char argname[WORDMAX]; // indexed arg
            sprintf(argname, "\\%d", arg);
            addlabel(argname, true);
            equ(NULL, &s);
            PROPOGATE_ERROR_VOID;
            labelhere->ignorenl = true;
            arg++;
        }
        *s2 = c;
        s = s2;
    } while (eatchar(&s, ','));
    addlabel("\\?", true);
    labelhere->type = VALUE;
    labelhere->value = arg;
    labelhere->known = true;
    labelhere->ignorenl = true;

    *next = s;

    while (arg++ < args) // skip any unused param names
        line = line->next;

    int macrolineno = 1;
    while (line)
    {
        processline(line->text, macrofilename, macrolineno);
        line = line->next;
        macrolineno++;
    }
    errmsg = NULL; // why?

    id->used = false;
    insidemacro--;
    scope = oldscope;
}

void rept(symbol *id, char **next)
{
    dependant = false;
    rept_loops = eval(next, WHOLEEXP);
    if (dependant || errmsg || rept_loops < 0)
        rept_loops = 0;
    rept_head = NULL;
    rept_tail = &rept_head;
    reptcount++; // tell processline to start storing up rept lines
}

void expandrept(int errline, char *errsrc)
{
    char macroerr[FILENAME_MAX + WORDMAX];
    int oldscope;

    oldscope = scope;
    insidemacro++;
    for (int i = 0; i < rept_loops; i++)
    {
        scope = nextscope++;
        snprintf(macroerr, sizeof macroerr, "%s(%i):REPT", errsrc, errline);
        linked_node *line = rept_head;
        int lineno = 1;
        while (line)
        {
            processline(line->text, macroerr, lineno);
            line = line->next;
            lineno++;
        }
    }
    linked_node *line = rept_head;
    while (line)
    { // free everything
        linked_node *temp = line->next;
        free(line->text);
        free(line);
        line = temp;
    }
    errmsg = NULL; // why?
    scope = oldscope;
    insidemacro--;
}

int enum_saveaddr;
void _enum(symbol *id, char **next)
{
    int val = 0;
    dependant = false;
    val = eval(next, WHOLEEXP);
    //  if(!dependant && !errmsg) {
    //  }
    if (!inside_enum)
        enum_saveaddr = PC;
    PC = val;
    inside_enum = true;
}

void ende(symbol *id, char **next)
{
    if (inside_enum)
    {
        PC = enum_saveaddr;
        inside_enum = false;
    }
    else
    {
        errmsg = ExtraENDE;
    }
}

void ignorenl(symbol *id, char **next)
{
    nonl = true;
}

void endinl(symbol *id, char **next)
{
    if (nonl)
        nonl = false;
    else
        errmsg = ExtraENDINL;
}

void fillval(symbol *id, char **next)
{
    dependant = false;
    defaultfiller = eval(next, WHOLEEXP);
}

void make_error(symbol *id, char **next)
{
    char *s = *next;
    reverse(tmpstr, s + strspn(s, WHITESPACE "\"")); // eat whitespace, quotes off both ends
    reverse(s, tmpstr + strspn(tmpstr, WHITESPACE "\""));
    errmsg = s;
    harderror = true;
    *next = s + strlen(s);
}

void undoc(symbol *id, char **next)
{
    allow_undocumented = true;
}

//[nicklausw] ines stuff

void inesprg(symbol *id, char **next)
{
    inesprg_num = eval(next, WHOLEEXP);

    if (inesprg_num < 0 || inesprg_num > 0xFF)
        errmsg = OutOfRange;

    ines_include = true;
}

void ineschr(symbol *id, char **next)
{
    ineschr_num = eval(next, WHOLEEXP);

    if (ineschr_num < 0 || ineschr_num > 0xFF)
        errmsg = OutOfRange;

    ines_include = true;
}

void inesmir(symbol *id, char **next)
{
    inesmir_num = eval(next, WHOLEEXP);

    // force 4 bits
    if (inesmir_num > 16 || inesmir_num < 0)
        errmsg = OutOfRange;

    ines_include = true;
}

void inesmap(symbol *id, char **next)
{
    inesmap_num = eval(next, WHOLEEXP);

    // ines 2.0 allows for some big numbers...
    if (inesmap_num >= 4096 || inesmap_num < 0)
        errmsg = OutOfRange;

    ines_include = true;
}

void nes2chrram(symbol *id, char **next)
{
    nes2chr_num = eval(next, WHOLEEXP);

    if (nes2chr_num < 0 || nes2chr_num > 16)
        errmsg = OutOfRange;

    ines_include = true;
    use_nes2 = true;
}

void nes2prgram(symbol *id, char **next)
{
    nes2prg_num = eval(next, WHOLEEXP);

    if (nes2prg_num < 0 || nes2prg_num > 16)
        errmsg = OutOfRange;

    ines_include = true;
    use_nes2 = true;
}

void nes2sub(symbol *id, char **next)
{
    nes2sub_num = eval(next, WHOLEEXP);

    if (nes2sub_num < 0 || nes2sub_num > 16)
        errmsg = OutOfRange;

    ines_include = true;
    use_nes2 = true;
}

void nes2tv(symbol *id, char **next)
{
    nes2tv_num = eval(next, WHOLEEXP);

    // possible presets...
    if (nes2tv_num == 'N')
        nes2tv_num = 0;
    if (nes2tv_num == 'P')
        nes2tv_num = 1;

    // might just change to 'N', 'P' but eh...
    if (nes2tv_num == 'B')
        nes2tv_num = 2;

    if (nes2tv_num > 2 || nes2tv_num < 0)
        errmsg = OutOfRange;

    ines_include = true;
    use_nes2 = true;
}

void nes2vs(symbol *id, char **next)
{
    nes2vs_num = 1;
    ines_include = true;
    use_nes2 = true;
}

void nes2bram(symbol *id, char **next)
{
    nes2bram_num = eval(next, WHOLEEXP);

    if (nes2bram_num < 0 || nes2bram_num > 16)
        errmsg = OutOfRange;

    ines_include = true;
    use_nes2 = true;
}

void nes2chrbram(symbol *id, char **next)
{
    nes2chrbram_num = eval(next, WHOLEEXP);

    if (nes2chrbram_num < 0 || nes2chrbram_num > 16)
        errmsg = OutOfRange;

    ines_include = true;
    use_nes2 = true;
}