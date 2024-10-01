/* awk.c - An awk implementation.
 * vi: tabstop=2 softtabstop=2 shiftwidth=2
 *
 * Copyright 2024 Ray Gardner <raygard@gmail.com>
 *
 * See https://pubs.opengroup.org/onlinepubs/9799919799/utilities/awk.html
 *
 * Deviations from posix: Don't handle LANG, LC_ALL, etc.
 *   Accept regex for RS
 *   Bitwise functions (from gawk): and, or, xor, lshift, rshift
 *   Attempt to follow tradition (nawk, gawk) where it departs from posix
 *
 * TODO: Lazy field splitting; improve performance; more testing/debugging

USE_AWK(NEWTOY(awk, "F:v*f*bc", TOYFLAG_USR|TOYFLAG_BIN|TOYFLAG_LINEBUF))

config AWK
  bool "awk"
  default n
  help
    usage:  awk [-F sepstring] [-v assignment]... program [argument...]
      or:
            awk [-F sepstring] -f progfile [-f progfile]... [-v assignment]...
                  [argument...]
      also:
      -b : count bytes, not characters (experimental)
      -c : compile only, do not run
*/

#define FOR_awk
#include "toys.h"

GLOBALS(
  struct arg_list *f;
  struct arg_list *v;
  char *F;

  struct scanner_state {
      char *p;
      char *progstring;
      struct arg_list *prog_args;
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
      size_t maxtok;
      size_t toklen;
      double numval;
      int error;  // Set if lexical error.
  } *scs;
  char *tokstr;
  int prevtok;

  struct compiler_globals {
    int in_print_stmt;
    int paren_level;
    int in_function_body;
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
  } cgl;

  // zvalue: the main awk value type
  // Can be number or string or both, or else map (array) or regex
  struct zvalue {
    unsigned flags;
    double num;
    union { // anonymous union not in C99; not going to fix it now.
      struct zstring *vst;
      struct zmap *map;
      regex_t *rx;
    };
  } nozvalue;   // to shut up compiler warning TODO FIXME

  struct runtime_globals {
    struct zvalue cur_arg;
    FILE *fp;           // current data file
    int narg;           // cmdline arg index
    int nfiles;         // num of cmdline data file args processed
    int eof;            // all cmdline files (incl. stdin) read
    char *recptr;
    struct zstring *zspr;      // Global to receive sprintf() string value
  } rgl;

  // Expanding sequential list
  struct zlist {
    char *base, *limit, *avail;
    size_t size;
  } globals_table,  // global symbol table
    locals_table,     // local symbol table
    func_def_table;  // function symbol table
  // runtime lists
  struct zlist literals, fields, zcode, stack;

  char *progname;

  int spec_var_limit;
  int zcode_last;
  struct zvalue *stackp;  // top of stack ptr

  char *pbuf;   // Used for number formatting in num_to_zstring()
#define RS_MAX  64
  char rs_last[RS_MAX];
  regex_t rx_rs_default, rx_rs_last;
  regex_t rx_default, rx_last, rx_printf_fmt;
#define FS_MAX  64
  char fs_last[FS_MAX];
  char one_char_fs[4];
  int nf_internal;  // should match NF
  char range_sw[64];   // FIXME TODO quick and dirty set of range switches
  int file_cnt, std_file_cnt;

  struct zfile {
    struct zfile *next;
    char *fn;
    FILE *fp;
    char mode;  // w, a, or r
    char file_or_pipe;  // 1 if file, 0 if pipe
    char is_tty, is_std_file;
    char eof;
    int ro, lim, buflen;
    char *buf;
  } *zfiles, *cfile, *zstdout;
)

static void awk_exit(int status)
{
  toys.exitval = status;
  xexit();
}
