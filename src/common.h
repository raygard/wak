// common.h
// Copyright 2023 Ray Gardner
// vi: tabstop=2 softtabstop=2 shiftwidth=2

////////////////////
//// wak -- an awk implementation
////////////////////
#ifndef MONOLITHIC

#endif // MONOLITHIC

#ifndef FOR_TOYBOX
#ifndef __STDC_WANT_LIB_EXT2__
#define __STDC_WANT_LIB_EXT2__ 1  // for getline(), getdelim()
#endif
#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <stdarg.h>
#include <ctype.h>
#include <math.h>
#include <time.h>
#include <locale.h>
#include <assert.h>

// for getopt():
#include <unistd.h>
#include <regex.h>
#if defined(__unix__) || defined(linux)
#include <langinfo.h>
#endif

// __USE_MINGW_ANSI_STDIO will have MinGW use its own printf format system?
// Because "The vc6.0 msvcrt.dll that MinGW-w64 targets doesn't implement
// support for the ANSI standard format specifiers."
// https://www.msys2.org/wiki/Porting/
#if !(defined(__unix__) || defined(linux))
#  define __USE_MINGW_ANSI_STDIO            1
#endif

#endif  // FOR_TOYBOX
#ifdef __GNUC__
#ifndef MONOLITHIC
#define ATTR_UNUSED_FUNCTION __attribute__ ((unused))
#define ATTR_UNUSED_VAR __attribute__ ((unused))
#endif // MONOLITHIC
#define ATTR_FALLTHROUGH_INTENDED __attribute__ ((fallthrough))
#else
#ifndef MONOLITHIC
#define ATTR_UNUSED_FUNCTION
#define ATTR_UNUSED_VAR
#endif // MONOLITHIC
#define ATTR_FALLTHROUGH_INTENDED
#endif

#define MAX(a,b) ((a)>(b)?(a):(b))

////////////////////
////   declarations
////////////////////

struct scanner_state {
    char *p;
    char *progstring;
    int num_progfiles;
    char **progfiles;
    int cur_progfile;
    char *filename;
    char *line;
    size_t line_size;
    ssize_t line_len;
    int line_num;
    int ch;
    FILE *fp;

    // state includes latest token seen
    int tok;
    int tokbuiltin;
    int toktype;
    char *tokstr;
    // int maxtok;
    // int toklen;
    size_t maxtok;
    size_t toklen;
    // int symtab_entry;
    double numval;
    int error;  // Set if lexical error.
};

enum Toktypes {
    // EOF (use -1 from stdio.h)
    ERROR = 2, NEWLINE, VAR, NUMBER, STRING, REGEX, USERFUNC, BUILTIN, TOKEN,
    KEYWORD
    };

// NOTE: nextfile, fflush NOT POSIX or in book

enum Toks {
    tkunusedtoken, tkeof, tkerr, tknl,
    tkvar, tknumber, tkstring, tkregex, tkfunc, tkbuiltin,

// static char *ops = " ;  ,  [  ]  (  )  {  }  $  ++ -- ^  !  *  /  %  +  -     "
//        "<  <= != == >  >= ~  !~ && || ?  :  ^= %= *= /= += -= =  >> |  ";
    tksemi, tkcomma, tklbracket, tkrbracket, tklparen, tkrparen, tklbrace,
    tkrbrace, tkfield, tkincr, tkdecr, tkpow, tknot, tkmul, tkdiv, tkmod,
    tkplus, tkminus,
    tkcat, // !!! Fake operator for concatenation (just adjacent string exprs)
    tklt, tkle, tkne, tkeq, tkgt, tkge, tkmatchop, tknotmatch, tkand, tkor,
    tkternif, tkternelse, tkpowasgn, tkmodasgn, tkmulasgn, tkdivasgn,
    tkaddasgn, tksubasgn, tkasgn, tkappend, tkpipe,

// static char *keywords = " in        BEGIN     END       if        else      "
//    "while     for       do        break     continue  exit      function  "
//    "return    next      nextfile  delete    print     printf    getline   ";
    tkin, tkbegin, tkend, tkif, tkelse,
    tkwhile, tkfor, tkdo, tkbreak, tkcontinue, tkexit, tkfunction,
    tkreturn, tknext, tknextfile, tkdelete, tkprint, tkprintf, tkgetline,

// static char *builtins = " atan2     cos       sin       exp       "
//    "log       sqrt      int       rand      srand     length    "
//    "tolower   toupper   system    fflush    ";
    tkatan2, tkcos, tksin, tkexp, tklog, tksqrt, tkint, tkrand, tksrand,
    tklength, tktolower, tktoupper, tksystem, tkfflush,

// static char *specialfuncs = " close     index     match     split     "
//    "sub       gsub      sprintf   substr    ";
    tkclose, tkindex, tkmatch, tksplit,
    tksub, tkgsub, tksprintf, tksubstr, tklasttk
    };

enum Opcodes {
    opunusedop = tklasttk,
    opvarref, opmapref, opfldref, oppush, opdrop, opdrop_n, opnotnot,
    oppreincr, oppredecr, oppostincr, oppostdecr, opnegate, opjump, opjumptrue,
    opjumpfalse, opprepcall, opmap, opmapiternext, opmapdelete, opmatchrec,
    opquit, opprintrec, oprange1, oprange2, oprange3, oplastop
};

// Special variables (POSIX)
enum spec_var_names { ARGC=1, ARGV, CONVFMT, ENVIRON, FILENAME, FNR, FS, NF,
    NR, OFMT, OFS, ORS, RLENGTH, RS, RSTART, SUBSEP };

// zlist: expanding sequential list
typedef struct {
  char *base, *limit, *avail;
  size_t size;
} zlist;

typedef struct {    // global symbol table entry
  unsigned flags;
  int slotnum;
  char *name;
} globals_entry;

typedef struct {    // local symbol table entry
  unsigned flags;
  int slotnum;
  char *name;
} locals_entry;

// zstring: flexible string type.
// Capacity must be > size because we insert a NUL byte.
typedef struct {
  int refcnt;
  unsigned size;
  unsigned capacity;
  char str[];   // C99 flexible array member
} zstring;

// These two typedefs are here to deal w/ circular name refs in the struct decls
typedef struct zmap zmap;
typedef struct zvalue zvalue;

// zvalue: the main awk value type
// Can be number or string or both, or else map (array) or regex

// Flag bits for zvalue and symbol tables
#define ZF_MAYBEMAP (1u << 1)
#define ZF_MAP      (1u << 2)
#define ZF_SCALAR   (1u << 3)
#define ZF_NUM      (1u << 4)
#define ZF_RX       (1u << 5)
#define ZF_STR      (1u << 6)
#define ZF_NUMSTR   (1u << 7)   // "numeric string" per posix
#define ZF_REF      (1u << 9)   // for lvalues
#define ZF_MAPREF   (1u << 10)  // for lvalues
#define ZF_FIELDREF (1u << 11)  // for lvalues
#define ZF_EMPTY_RX (1u << 12)
#define ZF_ANYMAP   (ZF_MAP | ZF_MAYBEMAP)
struct zvalue {
  unsigned flags;
  double num;
  union { // anonymous union not in C99; not going to fix it now.
    zstring *vst;
    zmap *map;
    regex_t *rx;
  };
};

// Macro to help facilitate possible future change in zvalue layout.
#define ZVINIT(flags, num, ptr) {(flags), (double)(num), {(ptr)}}

#define is_str(zvalp) ((zvalp)->flags & ZF_STR)
#define is_rx(zvalp) ((zvalp)->flags & ZF_RX)
#define is_num(zvalp) ((zvalp)->flags & ZF_NUM)
#define is_map(zvalp) ((zvalp)->flags & ZF_MAP)
#define is_empty_rx(zvalp) ((zvalp)->flags & ZF_EMPTY_RX)

#define GLOBAL      ((globals_entry *)globals_table.base)
#define LOCAL       ((locals_entry *)locals_table.base)
#define FUNC_DEF    ((func_def_entry *)func_def_table.base)

#define LITERAL     ((zvalue *)literals.base)
#define STACK       ((zvalue *)stack.base)
#define FIELD       ((zvalue *)fields.base)

#define ZCODE       ((int *)zcode.base)

#define FUNC_DEFINED    (1u)
#define FUNC_CALLED     (2u)
typedef struct {    // function symbol table entry
  unsigned flags;
  int slotnum;
  char *name;
  zlist function_locals;
  int zcode_addr;
} func_def_entry;

// Elements of the hash table (key/value pairs)
typedef struct {
  int hash;       // store hash key to speed hash table expansion
  zstring *key;
  zvalue val;
} zmapslot;
#define ZMSLOTINIT(hash, key, val) {hash, key, val}

// zmap: Mapping data type for arrays; a hash table. Values in hash are either
// 0 (unused), -1 (marked deleted), or one plus the number of the zmapslot
// containing a key/value pair. The zlist slot entries are numbered from 0 to
// count-1, so need to add one to distinguish from unused.  The probe sequence
// is borrowed from Python dict, using the "perturb" idea to mix in upper bits
// of the original hash value.
struct zmap {
  unsigned mask;  // tablesize - 1; tablesize is 2 ** n
  int *hash;      // (mask + 1) elements
  int limit;      // 80% of table size ((mask+1)*8/10)
  int count;      // number of occupied slots in hash
  int deleted;    // number of deleted slots
  zlist slot;     // expanding list of zmapslot elements
};

#define MAPSLOT    ((zmapslot *)(m->slot).base)

#define ffatal(format, ...) zzfatal(__FILE__, __LINE__, format, __VA_ARGS__)
#define fatal(...) zzfatal(__FILE__, __LINE__, "%s\n", __VA_ARGS__)
#define xerr(format, ...) zzerr(__FILE__, __LINE__, format, __VA_ARGS__)

struct compiler_globals {
  int in_print_stmt;
  int in_function_body;
  int pstate;
  int paren_level;
  int funcnum;
  int nparms;
  int compile_error_count;
  int first_begin;
  int last_begin;
  int first_end;
  int last_end;
  int first_recrule;
  int last_recrule;
  int break_dest;
  int continue_dest;
  int stack_offset_to_fix;  // fixup stack if return in for(e in a)
  int range_pattern_num;
  int rule_type;  // tkbegin, tkend, or 0
};

#define NO_EXIT_STATUS  (9999987)  // value unlikely to appear in exit stmt

ssize_t getline(char **lineptr, size_t *n, FILE *stream);
ssize_t getdelim(char ** restrict lineptr, size_t * restrict n, int delimiter, FILE *stream);

#define EXTERN extern

// Common (global) data

EXTERN struct scanner_state *scs;

// Forward ref declarations
EXTERN zvalue *val_to_str(zvalue *v);
EXTERN char *escape_str(char *s);
EXTERN int rx_compile(regex_t *rx, char *pat);

#ifndef MONOLITHIC
EXTERN char *progname;
EXTERN FILE *debout;    // Only for debugging
EXTERN unsigned debug_flags;
EXTERN void dprimpl(unsigned df, char *format, ...);

EXTERN void dump_func_table(void);
EXTERN void dump_stack(char *label);
EXTERN void dump_zcode(void);
EXTERN void dump_tables(void);

EXTERN void dump_zlist(char *label, zlist *z);
EXTERN void dump_zstring(char *label, zstring *s);
EXTERN void dump_zstringx(char *label, zstring *s);
EXTERN void dumpstrx(char *s, int n);
EXTERN void dumpstr(char *s);
EXTERN void dump_zvalue(char *label, zvalue *v);

EXTERN int last_global;    // set in run.h; used only for dump_stack() info.
EXTERN int spec_var_limit; // used in compile.h and run.h
EXTERN int stkptr;         // used in run.h and (once) in compile.h (& dumputils.h)
EXTERN int zcode_last;     // used in common.h and compile.h

EXTERN zlist globals_table;  // global symbol table
EXTERN zlist locals_table;  // local symbol table
EXTERN zlist func_def_table;  // function symbol table

EXTERN zlist literals;
EXTERN zlist fields;
EXTERN zlist zcode;
EXTERN zlist stack;

EXTERN zvalue uninit_zvalue;
EXTERN zvalue uninit_string_zvalue;

EXTERN char *ops, *keywords, *builtins;
EXTERN char *toknames[];

EXTERN char *tokstr;
EXTERN int prevtok;            // For checking end of previous statement for termination

EXTERN int trace_sw;
EXTERN int opt_print_source;

EXTERN struct compiler_globals cgl;

EXTERN void zvalue_dup_zstring(zvalue *v);

EXTERN void zzfatal(char *fn, int lnum, char *format, ...);
EXTERN void zzerr(char *fn, int lnum, char *format, ...);

EXTERN void error_exit(char *format, ...);
EXTERN void xfree(void *p);
EXTERN void *xmalloc(size_t size);
EXTERN void *xrealloc(void *p, size_t size);
EXTERN void *xzalloc(size_t size);
EXTERN char *xstrdup(char *s);
EXTERN double str_to_num(char *s);
EXTERN int hexval(int c);
EXTERN char *rx_escape_str(char *s);
EXTERN zlist *zlist_init(zlist *p, size_t size);
EXTERN size_t zlist_append(zlist *p, void *obj);
EXTERN int zlist_len(zlist *p);
EXTERN void get_token_text(char *op, int tk);
EXTERN void zstring_release(zstring **s);
EXTERN void zstring_incr_refcnt(zstring *s);
EXTERN zstring *new_zstring_cap(int capacity);
EXTERN zstring *zstring_update(zstring *to, size_t at, char *s, size_t n);
EXTERN zstring *zstring_copy(zstring *to, zstring *from);
EXTERN zstring *zstring_extend(zstring *to, zstring *from);
EXTERN zstring *new_zstring(char *s, size_t size);
EXTERN zvalue new_str_val(char *s);
EXTERN void zvalue_release_zstring(zvalue *v);
EXTERN void push_val(zvalue *v);
EXTERN void zvalue_copy(zvalue *to, zvalue *from);
EXTERN void init_scanner(void);
EXTERN void scan(void);
EXTERN int find_global(char *s);
EXTERN void compile(void);
EXTERN int zstring_match(zstring *a, zstring *b);
EXTERN zvalue *zmap_find(zmap *m, zstring *key);
EXTERN void zvalue_map_init(zvalue *v);
EXTERN void zmap_delete_map_incl_slotdata(zmap *m);
EXTERN void zmap_delete_map(zmap *m);
EXTERN zmapslot *zmap_find_or_insert_key(zmap *m, zstring *key);
EXTERN void zmap_delete(zmap *m, zstring *key);
EXTERN void run(int optind, int argc, char **argv, char *sepstring, int num_assignments, char **assignments, char **envp);

#endif // MONOLITHIC
#undef EXTERN
#define EXTERN

