// lib.c
// Copyright 2024 Ray Gardner
// License: 0BSD
// vi: tabstop=2 softtabstop=2 shiftwidth=2

#include "common.h"

////////////////////
//// lib
////////////////////

EXTERN void xfree(void *p)
{
  free(p);
}

#ifndef FOR_TOYBOX
EXTERN void error_exit(char *format, ...)
{
  va_list args;
  fprintf(stderr, "FATAL: ");
  va_start(args, format);
  vfprintf(stderr, format, args);
  va_end(args);
  putc('\n', stderr);
  fflush(stderr);
  exit(2);
}

// Compile a regular expression into a regex_t
EXTERN void xregcomp(regex_t *preg, char *regex, int cflags)
{
  // Copied from Rob Landley's toybox
  // Copyright 2006 Rob Landley <rob@landley.net>
  // License: 0BSD
  int rc;
  char libbuf[256]; // Added by rdg
  // BSD regex implementations don't support the empty regex (which isn't
  // allowed in the POSIX grammar), but glibc does. Fake it for BSD.
  if (!*regex) {
    regex = "()";
    cflags |= REG_EXTENDED;
  }

  if ((rc = regcomp(preg, regex, cflags))) {
    regerror(rc, preg, libbuf, sizeof(libbuf));
    error_exit("bad regex '%s': %s", regex, libbuf);
  }
}

// Do regex matching with len argument to handle embedded NUL bytes in string
EXTERN int regexec0(regex_t *preg, char *string, long len, int nmatch,
  regmatch_t *pmatch, int eflags)
{
  // Copied from Rob Landley's toybox
  // Copyright 2006 Rob Landley <rob@landley.net>
  // License: 0BSD
  regmatch_t backup;

  if (!nmatch) pmatch = &backup;
  pmatch->rm_so = 0;
  pmatch->rm_eo = len;
  return regexec(preg, string, nmatch, pmatch, eflags|REG_STARTEND);
}

// Convert wc to utf8, returning bytes written. Does not null terminate.
EXTERN int wctoutf8(char *s, unsigned wc)
{
  // Copied from Rob Landley's toybox
  // Copyright 2006 Rob Landley <rob@landley.net>
  // License: 0BSD
  int len = (wc>0x7ff)+(wc>0xffff), i;

  if (wc<128) {
    *s = wc;
    return 1;
  } else {
    i = len;
    do {
      s[1+i] = 0x80+(wc&0x3f);
      wc >>= 6;
    } while (i--);
    *s = (((signed char) 0x80) >> (len+1)) | wc;
  }

  return 2+len;
}

// Convert utf8 sequence to a unicode wide character
// returns bytes consumed, or -1 if err, or -2 if need more data.
EXTERN int utf8towc(unsigned *wc, char *str, unsigned len)
{
  // Copied from Rob Landley's toybox
  // Copyright 2006 Rob Landley <rob@landley.net>
  // License: 0BSD
  unsigned result, mask, first;
  char *s, c;

  // fast path ASCII
  if (len && *str<128) return !!(*wc = *str);

  result = first = *(s = str++);
  if (result<0xc2 || result>0xf4) return -1;
  for (mask = 6; (first&0xc0)==0xc0; mask += 5, first <<= 1) {
    if (!--len) return -2;
    if (((c = *(str++))&0xc0) != 0x80) return -1;
    result = (result<<6)|(c&0x3f);
  }
  result &= (1<<mask)-1;
  c = str-s;

  // Avoid overlong encodings
  if (result<(unsigned []){0x80,0x800,0x10000}[c-2]) return -1;

  // Limit unicode so it can't encode anything UTF-16 can't.
  if (result>0x10ffff || (result>=0xd800 && result<=0xdfff)) return -1;
  *wc = result;

  return str-s;
}

EXTERN void *xmalloc(size_t size)
{
  void *p = malloc(size);
  if (!p) error_exit("xmalloc(%d)", size);
  return p;
}

EXTERN void *xrealloc(void *p, size_t size)
{
  p = realloc(p, size);
  if (!p) error_exit("xrealloc(%d)", size);
  return p;
}

EXTERN void *xzalloc(size_t size)
{
  void *p = calloc(1, size);
  if (!p) error_exit("xzalloc(%d)", size);
  return p;
}

EXTERN char *xstrdup(char *s)
{
  size_t n = strlen(s) + 1;
  char *p = xmalloc(n);
  memmove(p, s, n);
  return p;
}

#endif  // FOR_TOYBOX
EXTERN double str_to_num(char *s)
{
  return atof(s);
}

EXTERN int hexval(int c)
{
  // Assumes c is valid hex digit
  return isdigit(c) ? c - '0' : (c | 040) - 'a' + 10;
}
