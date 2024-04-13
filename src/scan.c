// scan.c
// Copyright 2024 Ray Gardner
// License: 0BSD
// vi: tabstop=2 softtabstop=2 shiftwidth=2

#include "common.h"

////////////////////
//// scan (lexical analyzer)
////////////////////

// TODO:
// IS line_num getting incr correctly? Newline counts as start of line!?
// Handle nuls in file better.
// Open files "rb" and handle CRs in program.
// Roll gch() into get_char() ?
// Deal with signed char (at EOF? elsewhere?)
//
// 2023-01-11: Allow nul bytes inside strings? regexes?

static void progfile_open(void)
{
  TT.scs->filename = TT.scs->prog_args->arg;
  TT.scs->prog_args = TT.scs->prog_args->next;
  TT.scs->fp = stdin;
  if (strcmp(TT.scs->filename, "-")) TT.scs->fp = fopen(TT.scs->filename, "r");
  if (!TT.scs->fp) error_exit("Can't open %s", TT.scs->filename);
  TT.scs->line_num = 0;
}

static int get_char(void)
{
  static char *nl = "\n";
  // On first entry, TT.scs->p points to progstring if any, or null string.
  for (;;) {
    int c = *(TT.scs->p)++;
    if (c) {
      return c;
    }
    if (TT.scs->progstring) {  // Fake newline at end of progstring.
      if (TT.scs->progstring == nl) return EOF;
      TT.scs->p = TT.scs->progstring = nl;
      continue;
    }
    // Here if getting from progfile(s).
    if (TT.scs->line == nl) return EOF;
    if (!TT.scs->fp) {
      progfile_open();
    // The "  " + 1 is to set p to null string but allow ref to prev char for
    // "lastchar" test below.
    }
    // Save last char to allow faking final newline.
    int lastchar = (TT.scs->p)[-2];
    TT.scs->line_len = getline(&TT.scs->line, &TT.scs->line_size, TT.scs->fp);
    if (TT.scs->line_len > 0) {
      TT.scs->line_num++;
      TT.scs->p = TT.scs->line;
      continue;
    }
    // EOF
    // FIXME TODO or check for error? feof() vs. ferror()
    fclose(TT.scs->fp);
    TT.scs->fp = 0;
    TT.scs->p = "  " + 2;
    if (!TT.scs->prog_args) {
      xfree(TT.scs->line);
      if (lastchar == '\n') return EOF;
      // Fake final newline
      TT.scs->line = TT.scs->p = nl;
    }
  }
}

static void append_this_char(int c)
{
  if (TT.scs->toklen == TT.scs->maxtok - 1) {
    TT.scs->maxtok *= 2;
    TT.scs->tokstr = xrealloc(TT.scs->tokstr, TT.scs->maxtok);
  }
  TT.scs->tokstr[TT.scs->toklen++] = c;
  TT.scs->tokstr[TT.scs->toklen] = 0;
}

static void gch(void)
{
  // FIXME probably not right place to skip CRs.
  do {
    TT.scs->ch = get_char();
  } while (TT.scs->ch == '\r');
}

static void append_char(void)
{
  append_this_char(TT.scs->ch);
  gch();
}

static int find_keyword_or_builtin(char *table,
    int first_tok_in_table)
{
  char s[16] = " ", *p;
  // keywords and builtin functions are spaced 10 apart for strstr() lookup,
  // so must be less than that long.
  if (TT.scs->toklen >= 10) return 0;
  strcat(s, TT.scs->tokstr);
  strcat(s, " ");
  p = strstr(table, s);
  if (!p) return 0;
  return first_tok_in_table + (p - table) / 10;
}

static int find_token(void)
{
  char s[6] = " ", *p;
  // tokens are spaced 3 apart for strstr() lookup, so must be less than
  // that long.
  strcat(s, TT.scs->tokstr);
  strcat(s, " ");
  p = strstr(ops, s);
  if (!p) return 0;
  return tksemi + (p - ops) / 3;
}

static int find_keyword(void)
{
  return find_keyword_or_builtin(keywords, tkin);
}

static int find_builtin(void)
{
  return find_keyword_or_builtin(builtins, tkatan2);
}

static void get_number(void)
{
  // Assumes TT.scs->ch is digit or dot on entry.
  // TT.scs->p points to the following character.
  // OK formats: 1 1. 1.2 1.2E3 1.2E+3 1.2E-3 1.E2 1.E+2 1.E-2 1E2 .1 .1E2
  // .1E+2 .1E-2
  // NOT OK: . .E .E1 .E+ .E+1 ; 1E .1E 1.E 1.E+ 1.E- parse as number
  // followed by variable E.
  // gawk accepts 12.E+ and 12.E- as 12; nawk & mawk say syntax error.
  char *leftover;
  int len;
  TT.scs->numval = strtod(TT.scs->p - 1, &leftover);
  len = leftover - TT.scs->p + 1;
  if (len == 0) {
    append_char();
    TT.scs->toktype = ERROR;
    TT.scs->tok = tkerr;
    TT.scs->error = 1;
    FFATAL("Unexpected token '%s'\n", TT.scs->tokstr);
    return;
  }
  while (len--)
    append_char();
}

static void get_string_or_regex(int endchar)
{
  gch();
  while (TT.scs->ch != endchar) {
    if (TT.scs->ch == '\n') {
      // FIXME Handle unterminated string or regex. Is this OK?
      // FIXME TODO better diagnostic here?
      XERR("%s\n", "unterminated string or regex");
      break;
    } else if (TT.scs->ch == '\\') {
      // \\ \a \b \f \n \r \t \v \" \/ \ddd
      char *p, *escapes = "\\abfnrtv\"/";
      gch();
      if (TT.scs->ch == '\n') {  // backslash newline is continuation
        gch();
        continue;
      } else if ((p = strchr(escapes, TT.scs->ch))) {
        // posix regex does not use these escapes,
        // but awk does, so do them.
        int c = "\\\a\b\f\n\r\t\v\"/"[p-escapes];
        append_this_char(c);
        // Need to double up \ inside literal regex
        if (endchar == '/' && c == '\\') append_this_char('\\');
        gch();
      } else if (TT.scs->ch == 'x') {
        gch();
        if (isxdigit(TT.scs->ch)) {
          int c = hexval(TT.scs->ch);
          gch();
          if (isxdigit(TT.scs->ch)) {
            c = c * 16 + hexval(TT.scs->ch);
            gch();
          }
          append_this_char(c);
        } else append_this_char('x');
      } else if (TT.scs->ch == 'u') {
        gch();
        if (isxdigit(TT.scs->ch)) {
          int i = 0, j = 0, c = 0;
          char codep[9] = {0};
          do {
            codep[j++] = TT.scs->ch;
            gch();
          } while (j < 8 && isxdigit(TT.scs->ch));
          c = strtol(codep, 0, 16);
          for (i = wctoutf8(codep, c), j = 0; j < i; j++)
            append_this_char(codep[j]);
        } else append_this_char('u');
      } else if (isdigit(TT.scs->ch)) {
        if (TT.scs->ch < '8') {
          int k, c = 0;
          for (k = 0; k < 3; k++) {
            if (isdigit(TT.scs->ch) && TT.scs->ch < '8') {
              c = c * 8 + TT.scs->ch - '0';
              gch();
            } else
              break;
          }
          append_this_char(c);
        } else {
          append_char();
        }
      } else {
        if (endchar == '/') {
          // pass \ unmolested if not awk escape,
          // so that regex routines can see it.
          if (!strchr(".[]()*+?{}|^$-", TT.scs->ch)) {
            XERR("warning: '\\%c' -- unknown regex escape\n", TT.scs->ch);
          }
          append_this_char('\\');
        } else {
          XERR("warning: '\\%c' treated as plain '%c'\n", TT.scs->ch, TT.scs->ch);
        }
      }
    } else if (TT.scs->ch == EOF) {
      FATAL("EOF in string or regex\n");
    } else {
      append_char();
    }
  }
  gch();
}

static void ascan_opt_div(int div_op_allowed_here)
{
  int n;
  for (;;) {
    TT.scs->tokbuiltin = 0;
    TT.scs->toklen = 0;
    TT.scs->tokstr[0] = 0;
    while (TT.scs->ch == ' ' || TT.scs->ch == '\t')
      gch();
    if (TT.scs->ch == '\\') {
      append_char();
      if (TT.scs->ch == '\n') {
        gch();
        continue;
      }
      TT.scs->toktype = ERROR;   // \ not last char in line.
      TT.scs->tok = tkerr;
      TT.scs->error = 3;
      FATAL("backslash not last char in line\n");
      return;
    }
    break;
  }
  // Note \<NEWLINE> in comment does not continue it.
  if (TT.scs->ch == '#') {
    gch();
    while (TT.scs->ch != '\n')
      gch();
    // Need to fall through here to pick up newline.
  }
  if (TT.scs->ch == '\n') {
    TT.scs->toktype = NEWLINE;
    TT.scs->tok = tknl;
    append_char();
  } else if (isalpha(TT.scs->ch) || TT.scs->ch == '_') {
    append_char();
    while (isalnum(TT.scs->ch) || TT.scs->ch == '_') {
      append_char();
    }
    if ((n = find_keyword()) != 0) {
      TT.scs->toktype = KEYWORD;
      TT.scs->tok = n;
    } else if ((n = find_builtin()) != 0) {
      TT.scs->toktype = BUILTIN;
      TT.scs->tok = tkbuiltin;
      TT.scs->tokbuiltin = n;
    } else if ((TT.scs->ch == '(')) {
      TT.scs->toktype = USERFUNC;
      TT.scs->tok = tkfunc;
    } else {
      TT.scs->toktype = VAR;
      TT.scs->tok = tkvar;
      // skip whitespace to be able to check for , or )
      while (TT.scs->ch == ' ' || TT.scs->ch == '\t')
        gch();
    }
    return;
  } else if (TT.scs->ch == '"') {
    TT.scs->toktype = STRING;
    TT.scs->tok = tkstring;
    get_string_or_regex('"');
  } else if (isdigit(TT.scs->ch) || TT.scs->ch == '.') {
    TT.scs->toktype = NUMBER;
    TT.scs->tok = tknumber;
    get_number();
  } else if (TT.scs->ch == '/' && ! div_op_allowed_here) {
    TT.scs->toktype = REGEX;
    TT.scs->tok = tkregex;
    get_string_or_regex('/');
  } else if (TT.scs->ch == EOF) {
    TT.scs->toktype = EOF;
    TT.scs->tok = tkeof;
  } else if (TT.scs->ch == '\0') {
    append_char();
    TT.scs->toktype = ERROR;
    TT.scs->tok = tkerr;
    TT.scs->error = 5;
    FATAL("null char\n");
  } else {
    // All other tokens.
    TT.scs->toktype = TT.scs->ch;
    append_char();
    // Special case for **= and ** tokens
    if (TT.scs->toktype == '*' && TT.scs->ch == '*') {
      append_char();
      if (TT.scs->ch == '=') {
        append_char();
        TT.scs->tok = tkpowasgn;
      } else TT.scs->tok = tkpow;
      TT.scs->toktype = TT.scs->tok + 200;
      return;
    }
    // Is it a 2-character token?
    if (TT.scs->ch != ' ' && TT.scs->ch != '\n') {
      append_this_char(TT.scs->ch);
      if (find_token()) {
        TT.scs->tok = find_token();
        TT.scs->toktype = TT.scs->tok + 200;
        gch();  // Eat second char of token.
        return;
      }
      TT.scs->toklen--;  // Not 2-character token; back off.
      TT.scs->tokstr[TT.scs->toklen] = 0;
    }
    TT.scs->tok = find_token();
    if (TT.scs->tok) return;
    TT.scs->toktype = ERROR;
    TT.scs->tok = tkerr;
    TT.scs->error = 4;
    FFATAL("Unexpected token '%s'\n", TT.scs->tokstr);
  }
}

static void scan_opt_div(int div_op_allowed_here)
{
  // TODO FIXME need better diags for bad tokens!
  // TODO Also set global syntax error flag.
  do ascan_opt_div(div_op_allowed_here); while (TT.scs->tok == tkerr);
}

EXTERN void init_scanner(void)
{
  TT.prevtok = tkeof;
  gch();
}

// POSIX says '/' does not begin a regex wherever '/' or '/=' can mean divide.
// Pretty sure if / or /= comes after these, it means divide:
static char div_preceders[] = {tknumber, tkstring, tkvar, tkgetline, tkrparen, tkrbracket, tkincr, tkdecr, 0};

// For checking end of prev statement for termination and if '/' can come next

EXTERN void scan(void)
{
  TT.prevtok = TT.scs->tok;
  if (TT.prevtok && strchr(div_preceders, TT.prevtok)) scan_opt_div(1);
  else scan_opt_div(0);
  TT.tokstr = TT.scs->tokstr;
}
