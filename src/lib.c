// lib.c
// Copyright 2023 Ray Gardner
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
  fflush(stderr);
  exit(42);
}

EXTERN void *xmalloc(size_t size)
{
  void *p = malloc(size);
  if (!p) error_exit("%s\n", "Out of memory.");
  return p;
}

EXTERN void *xrealloc(void *p, size_t size)
{
  p = realloc(p, size);
  if (!p) error_exit("%s\n", "Out of memory.");
  return p;
}

EXTERN void *xzalloc(size_t size)
{
  void *p = calloc(1, size);
  if (!p) error_exit("%s\n", "Out of memory.");
  return p;
}

EXTERN char *xstrdup(char *s)
{
  char *p = strdup(s);
  if (!p) error_exit("%s\n", "Out of memory.");
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
