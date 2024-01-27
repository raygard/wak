/* awk.c - An awk implementation.
 * vi: tabstop=2 softtabstop=2 shiftwidth=2
 *
 * Copyright 2024 Ray Gardner <raygard@gmail.com>
 *
 * See https://pubs.opengroup.org/onlinepubs/9699919799/utilities/awk.html

USE_AWK(NEWTOY(awk, "F:v*f*r", TOYFLAG_USR|TOYFLAG_BIN))

config AWK
  bool "awk"
  default n
  help
    usage:  awk [-F sepstring] [-v assignment]... program [argument...]
       or:
            awk [-F sepstring] -f progfile [-f progfile]... [-v assignment]...
                   [argument...]
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
    //char *filename;     // UNUSED
    FILE *fp;           // current data file
    int narg;           // cmdline arg index
    int nfiles;         // num of cmdline data file args processed
    int eof;            // all cmdline files (incl. stdin) read
    char *recptr;
    char *recbuf;
    size_t recbufsize;
    char *recbuf_multx;
    size_t recbufsize_multx;
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
  int stkptr;

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
    char file_or_pipe;  // f or p
    char is_std_file;
    char *recbuf;
    size_t recbufsize;
    char *recbuf_multi;
    size_t recbufsize_multi;
    char *recbuf_multx;
    size_t recbufsize_multx;
    int recoffs, endoffs;
  } *zfiles, *cfile, *zstdout;
)

