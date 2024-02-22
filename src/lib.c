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
  // Borrowed from Rob Landley's toybox
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
  // Borrowed from Rob Landley's toybox
  // Copyright 2006 Rob Landley <rob@landley.net>
  // License: 0BSD
  regmatch_t backup;

  if (!nmatch) pmatch = &backup;
  pmatch->rm_so = 0;
  pmatch->rm_eo = len;
  return regexec(preg, string, nmatch, pmatch, eflags|REG_STARTEND);
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
  setlocale(LC_NUMERIC, "");
  return atof(s);
}

EXTERN int hexval(int c)
{
  // Assumes c is valid hex digit
  return isdigit(c) ? c - '0' : (c | 040) - 'a' + 10;
}
