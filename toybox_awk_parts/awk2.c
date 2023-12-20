char **make_arg_array(struct arg_list *x, int *n)
{
  struct arg_list *xx = x;
  for (*n = 0; x; (*n)++, x = x->next) {}
  char **a = xmalloc(*n * sizeof(*a));
  for (*n = 0; xx; (*n)++, xx = xx->next)
    a[*n] = xx->arg;
  return a;
}

void awk_main(void)
{
  int num_assignments, num_progfiles;
  char **assignments = make_arg_array(TT.v, &num_assignments);
  char **progfiles = make_arg_array(TT.f, &num_progfiles);
  char *sepstring = TT.F ? escape_str(TT.F) : " ";
  int optind = 0;
  char *progstring = NULL;

  TT.pbuf = toybuf;
  toys.exitval = 73;
  if (! num_progfiles) {
    if (*toys.optargs) progstring = toys.optargs[optind++];
    else error_exit("No program string\n");
  }
  TT.progname = toys.which->name;
  toys.exitval = awk(sepstring, num_assignments, assignments, num_progfiles,
      progfiles, progstring, optind, toys.optc, toys.optargs, 0, 0,
      !FLAG(r), environ);
}
