/* awk.c - An awk implementation.
 * vi: tabstop=2 softtabstop=2 shiftwidth=2
 *
 * Copyright 2023 Ray Gardner <raygard@gmail.com>
 *
 * See https://pubs.opengroup.org/onlinepubs/9699919799/utilities/awk.html

USE_AWK(NEWTOY(awk, "F:v*f*rp", TOYFLAG_USR|TOYFLAG_BIN))

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

  int unused;
)

#define FOR_TOYBOX

