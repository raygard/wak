void awk_main(void)
{
  char *sepstring = TT.F ? escape_str(TT.F) : " ";
  int optind = 0;
  char *progstring = NULL;

  TT.pbuf = toybuf;
  toys.exitval = 73;
  if (!TT.f) {
    if (*toys.optargs) progstring = toys.optargs[optind++];
    else error_exit("No program string\n");
  }
  TT.progname = toys.which->name;
  toys.exitval = awk(sepstring, progstring, TT.f, TT.v,
      optind, toys.optc, toys.optargs, !FLAG(r), environ, 0, 0);
}
