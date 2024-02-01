// compile.c
// Copyright 2024 Ray Gardner
// License: 0BSD
// vi: tabstop=2 softtabstop=2 shiftwidth=2

#include "common.h"

////////////////////
//// compile
////////////////////

//  NOTES:
//  NL ok after , { && || do else OR after right paren after if/while/for
//  TODO:
//    see case tkgetline -- test more
//    case tkmatchop, tknotmatch -- fix ~ (/re/)

// Forward declarations -- for mutually recursive parsing functions
static int exprn(int rbp);
static void lvalue(void);
static int primary(void);
static void stmt(void);
static void action(int action_type);

#define CURTOK() (TT.scs->tok)
#define ISTOK(toknum) (TT.scs->tok == (toknum))

static int havetok(int tk)
{
  if (!ISTOK(tk)) return 0;
  scan();
  return 1;
}

//// code and "literal" emitters
static void gen2cd(int op, int n)
{
  zlist_append(&TT.zcode, &op);
  TT.zcode_last = zlist_append(&TT.zcode, &n);
}

static void gencd(int op)
{
  TT.zcode_last = zlist_append(&TT.zcode, &op);
}

static int make_literal_str_val(char *s)
{
  // Only if no nul inside string!
  struct zvalue v = new_str_val(s);
  return zlist_append(&TT.literals, &v);
}

static int make_literal_regex_val(char *s)
{
  regex_t *rx;
  rx = xmalloc(sizeof(*rx));
  if (rx_compile(rx, s)) XERR("regex seen as '%s'\n", s);
  struct zvalue v = ZVINIT(ZF_RX, 0, 0);
  v.rx = rx;
  // Flag empty rx to make it easy to identify for split() special case
  if (!*s) v.flags |= ZF_EMPTY_RX;
  return zlist_append(&TT.literals, &v);
}

static int make_literal_num_val(double num)
{
  struct zvalue v = ZVINIT(ZF_NUM, num, 0);
  return zlist_append(&TT.literals, &v);
}

static int make_uninit_val(void)
{
  struct zvalue v = uninit_zvalue;
  return zlist_append(&TT.literals, &v);
}
//// END code and "literal" emitters

//// Symbol tables functions
static int find_func_def_entry(char *s)
{
  for (int k = 1; k < zlist_len(&TT.func_def_table); k++)
    if (!strcmp(s, FUNC_DEF[k].name)) return k;
  return 0;
}

static int add_func_def_entry(char *s)
{
  struct functab_slot ent = {0, 0, 0, {0, 0, 0, 0}, 0};
  ent.name = xstrdup(s);
  int slotnum = zlist_append(&TT.func_def_table, &ent);
  FUNC_DEF[slotnum].slotnum = slotnum;
  return slotnum;
}

EXTERN int find_global(char *s)
{
  for (int k = 1; k < zlist_len(&TT.globals_table); k++)
    if (!strcmp(s, GLOBAL[k].name)) return k;
  return 0;
}

static int add_global(char *s)
{
  struct symtab_slot ent = {0, 0, 0};
  ent.name = xstrdup(s);
  int slotnum = zlist_append(&TT.globals_table, &ent);
  GLOBAL[slotnum].slotnum = slotnum;
  return slotnum;
}

static int find_local_entry(char *s)
{
  for (int k = 1; k < zlist_len(&TT.locals_table); k++)
    if (!strcmp(s, LOCAL[k].name)) return k;
  return 0;
}

static int add_local_entry(char *s)
{
  struct symtab_slot ent = {0, 0, 0};
  ent.name = xstrdup(s);
  int slotnum = zlist_append(&TT.locals_table, &ent);
  LOCAL[slotnum].slotnum = slotnum;
  return slotnum;
}

static int find_or_add_var_name(void)
{
  int slotnum = 0;    // + means global; - means local to function
  int globals_ent = 0;
  int locals_ent = find_local_entry(TT.tokstr);   // in local symbol table?
  if (locals_ent) {
    slotnum = -LOCAL[locals_ent].slotnum;
  } else {
    globals_ent = find_global(TT.tokstr);
    if (!globals_ent) globals_ent = add_global(TT.tokstr);
    slotnum = GLOBAL[globals_ent].slotnum;
    if (find_func_def_entry(TT.tokstr))
      // POSIX: The same name shall not be used both as a variable name
      // with global scope and as the name of a function.
      XERR("var '%s' used as function name\n", TT.tokstr);
  }
  return slotnum;
}

//// END Symbol tables functions

//// Initialization
static void init_locals_table(void)
{
  static struct symtab_slot locals_ent;
  zlist_init(&TT.locals_table, sizeof(struct symtab_slot));
  zlist_append(&TT.locals_table, &locals_ent);
}

static void init_tables(void)
{
  static struct symtab_slot global_ent;
  static struct functab_slot func_ent;

  // Append dummy elements in lists to force valid offsets nonzero.
  zlist_init(&TT.globals_table, sizeof(struct symtab_slot));
  zlist_append(&TT.globals_table, &global_ent);
  zlist_init(&TT.func_def_table, sizeof(struct functab_slot));
  zlist_append(&TT.func_def_table, &func_ent);
  init_locals_table();
  zlist_init(&TT.zcode, sizeof(int));
  gencd(tkeof);   // to ensure zcode offsets are non-zero
  zlist_init(&TT.literals, sizeof(struct zvalue));
  zlist_init(&TT.stack, sizeof(struct zvalue));
  zlist_init(&TT.fields, sizeof(struct zvalue));
  zlist_append(&TT.literals, &uninit_zvalue);
  zlist_append(&TT.stack, &uninit_zvalue);
  zlist_append(&TT.fields, &uninit_zvalue);
  FIELD[0].vst = new_zstring("", 0);
}

static void init_compiler(void)
{
  // Special variables (POSIX). Must align with enum spec_var_names
  static char *spec_vars[] = { "ARGC", "ARGV", "CONVFMT", "ENVIRON", "FILENAME",
      "FNR", "FS", "NF", "NR", "OFMT", "OFS", "ORS", "RLENGTH", "RS", "RSTART",
      "SUBSEP", 0};

  init_tables();
  for (int k = 0; spec_vars[k]; k++) {
    TT.spec_var_limit = add_global(spec_vars[k]);
    GLOBAL[TT.spec_var_limit++].flags |= (k == 1 || k == 3) ? ZF_MAP : ZF_SCALAR;
    push_val(&uninit_zvalue);
  }
}
//// END Initialization

//// Parsing and compiling to TT.zcode
// Left binding powers
static int lbp_table[] = {  // Must align with enum Toks
  0, 0, 0, 0,     // tkunusedtoken, tkeof, tkerr, tknl,
  250, 250, 250,  // tkvar, tknumber, tkstring,
  250, 250, 250,  // tkregex, tkfunc, tkbuiltin,
  0, 0, 210, 0, // tksemi, tkcomma, tklbracket, tkrbracket,
  200, 0, 0, 0, // tklparen, tkrparen, tklbrace, tkrbrace,
  190, 180, 180, 170, 160, // tkfield, tkincr, tkdecr, tkpow, tknot,
  150, 150, 150, 140, 140, // tkmul, tkdiv, tkmod, tkplus, tkminus,
  130, // tkcat, // FAKE (?) optor for concatenation (adjacent string exprs)
  110, 110, 110, 110, 110, 110, // tklt, tkle, tkne, tkeq, tkgt, tkge,
  100, 100, // tkmatchop, tknotmatch,
  80, 70, // tkand, tkor,
  60, 0, // tkternif, tkternelse,
  50, 50, 50, 50,   // tkpowasgn, tkmodasgn, tkmulasgn, tkdivasgn,
  50, 50, 50, // tkaddasgn, tksubasgn, tkasgn,
  0, 120, // tkappend, tkpipe,
  90 // tkin
};

static int getlbp(int tok)
{
  // FIXME: should tkappend be here too? is tkpipe needed?
  // In print statement outside parens: make '>' end an expression
  if (TT.cgl.in_print_stmt && ! TT.cgl.paren_level && (tok == tkgt || tok == tkpipe))
    return 0;
  return (0 <= tok && tok <= tkin) ? lbp_table[tok] :
    // getline is special, not a normal builtin.
    // close, index, match, split, sub, gsub, sprintf, substr
    // are really builtin functions though bwk treats them as keywords.
    (tkgetline <= tok && tok <= tksubstr) ? 240 : 0;     // FIXME 240 is temp?
}

// Get right binding power. Same as left except for right associative optors
static int getrbp(int tok)
{
  int lbp = getlbp(tok);
  // ternary (?:), assignment, power ops are right associative
  return (lbp <= 60 || lbp == 170) ? lbp - 1 : lbp;
}

static void unexpected_eof(void)
{
  error_exit("terminated with error(s)");
}

//// syntax error diagnostic and recovery (Turner's method)
// D.A. Turner, Error diagnosis and recovery in one pass compilers,
// Information Processing Letters, Volume 6, Issue 4, 1977, Pages 113-115
static int recovering = 0;

static void complain(int tk)
{
  char op[3], tkstr[10];
  if (recovering) return;
  recovering = 1;
  if (!strcmp(TT.tokstr, "\n")) TT.tokstr = "<newline>";
  if (tksemi <= tk && tk <= tkpipe) {
    get_token_text(op, tk);
    XERR("syntax near '%s' -- '%s' expected\n", TT.tokstr, op);
  } else if (tk >= tkin && tk <= tksubstr) {
    if (tk < tkatan2) memmove(tkstr, keywords + 1 + 10 * (tk - tkin), 10);
    else memmove(tkstr, builtins + 1 + 10 * (tk - tkatan2), 10);
    *strchr(tkstr, ' ') = 0;
    XERR("syntax near '%s' -- '%s' expected\n", TT.tokstr, tkstr);
  } else XERR("syntax near '%s'\n", TT.tokstr);
}

static void expect(int tk)
{
  if (recovering) {
    while (!ISTOK(tkeof) && !ISTOK(tk))
      scan();
    if (ISTOK(tkeof)) unexpected_eof();
    scan(); // consume expected token
    recovering = 0;
  } else if (!havetok(tk)) complain(tk);
}

static void skip_to(char *tklist)
{
  do scan(); while (!ISTOK(tkeof) && !strchr(tklist, CURTOK()));
  if (ISTOK(tkeof)) unexpected_eof();
}

//// END syntax error diagnostic and recovery (Turner's method)

static void optional_nl_or_semi(void)
{
  while (havetok(tknl) || havetok(tksemi))
    ;
}

static void optional_nl(void)
{
  while (havetok(tknl))
    ;
}

static void rparen(void)
{
  expect(tkrparen);
  optional_nl();
}

static int have_comma(void)
{
  if (!havetok(tkcomma)) return 0;
  optional_nl();
  return 1;
}

static void check_set_map(int slotnum)
{
  // POSIX: The same name shall not be used within the same scope both as
  // a scalar variable and as an array.
  if (slotnum < 0 && LOCAL[-slotnum].flags & ZF_SCALAR)
    XERR("scalar param '%s' used as array\n", LOCAL[-slotnum].name);
  if (slotnum > 0 && GLOBAL[slotnum].flags & ZF_SCALAR)
    XERR("scalar var '%s' used as array\n", GLOBAL[slotnum].name);
  if (slotnum < 0) LOCAL[-slotnum].flags |= ZF_MAP;
  if (slotnum > 0) GLOBAL[slotnum].flags |= ZF_MAP;
}

static void check_set_scalar(int slotnum)
{
  if (slotnum < 0 && LOCAL[-slotnum].flags & ZF_MAP)
    XERR("array param '%s' used as scalar\n", LOCAL[-slotnum].name);
  if (slotnum > 0 && GLOBAL[slotnum].flags & ZF_MAP)
    XERR("array var '%s' used as scalar\n", GLOBAL[slotnum].name);
  if (slotnum < 0) LOCAL[-slotnum].flags |= ZF_SCALAR;
  if (slotnum > 0) GLOBAL[slotnum].flags |= ZF_SCALAR;
}

static void map_name(void)
{
  int slotnum;
  check_set_map(slotnum = find_or_add_var_name());
  gen2cd(tkvar, slotnum);
}

static void expr(void)
{
  exprn(0);
}

static void check_builtin_arg_counts(int tk, int num_args, char *fname)
{
  static char builtin_1_arg[] = { tkcos, tksin, tkexp, tklog, tksqrt, tkint,
                                  tktolower, tktoupper, tkclose, tksystem, 0};
  static char builtin_2_arg[] = { tkatan2, tkmatch, tkindex, 0};
  static char builtin_2_3_arg[] = { tksub, tkgsub, tksplit, tksubstr, 0};
  static char builtin_0_1_arg[] = { tksrand, tklength, tkfflush, 0};

  if (tk == tkrand && num_args)
    XERR("function '%s' expected no args, got %d\n", fname, num_args);
  else if (strchr(builtin_1_arg, tk) && num_args != 1)
    XERR("function '%s' expected 1 arg, got %d\n", fname, num_args);
  else if (strchr(builtin_2_arg, tk) && num_args != 2)
    XERR("function '%s' expected 2 args, got %d\n", fname, num_args);
  else if (strchr(builtin_2_3_arg, tk) && num_args != 2 && num_args != 3)
    XERR("function '%s' expected 2 or 3 args, got %d\n", fname, num_args);
  else if (strchr(builtin_0_1_arg, tk) && num_args != 0 && num_args != 1)
    XERR("function '%s' expected no arg or 1 arg, got %d\n", fname, num_args);
}

static void builtin_call(int tk, char *builtin_name)
{
  int num_args = 0;
  expect(tklparen);
  TT.cgl.paren_level++;
  switch (tk) {
    case tksub:
    case tkgsub:
      if (ISTOK(tkregex)) {
        gen2cd(tkregex, make_literal_regex_val(TT.tokstr));
        scan();
      } else expr();
      expect(tkcomma);
      optional_nl();
      expr();
      if (have_comma()) {
        lvalue();
      } else {
        gen2cd(tknumber, make_literal_num_val(0));
        gen2cd(opfldref, tkeof);
      }
      num_args = 3;
      break;

    case tkmatch:
      expr();
      expect(tkcomma);
      optional_nl();
      if (ISTOK(tkregex)) {
        gen2cd(tkregex, make_literal_regex_val(TT.tokstr));
        scan();
      } else expr();
      num_args = 2;
      break;

    case tksplit:
      expr();
      expect(tkcomma);
      optional_nl();
      if (ISTOK(tkvar) && (TT.scs->ch == ',' || TT.scs->ch == ')')) {
        map_name();
        scan();
      } else {
        XERR("%s\n", "expected array name as split() 2nd arg");
        expr();
      }
      // FIXME some recovery needed here!?
      num_args = 2;
      if (have_comma()) {
        if (ISTOK(tkregex)) {
          gen2cd(tkregex, make_literal_regex_val(TT.tokstr));
          scan();
        } else expr();
        num_args++;
      }
      break;

    case tklength:
      if (ISTOK(tkvar) && (TT.scs->ch == ',' || TT.scs->ch == ')')) {
        gen2cd(tkvar, find_or_add_var_name());
        scan();
        num_args++;
      }
      ATTR_FALLTHROUGH_INTENDED;

    default:
      if (ISTOK(tkrparen)) break;
      do {
        expr();
        num_args++;
      } while (have_comma());
      break;
  }
  expect(tkrparen);
  TT.cgl.paren_level--;

  check_builtin_arg_counts(tk, num_args, builtin_name);

  gen2cd(tk, num_args);
}

static void function_call(void)
{
  // Function call: generate TT.zcode to:
  //  push placeholder for return value, push placeholder for return addr,
  //  push args, then push number of args, then:
  //      for builtins: gen opcode (e.g. tkgsub)
  //      for user func: gen (tkfunc, function location)
  //      if function not yet defined, location will be filled in when defined
  //          the location slots will be chained from the symbol table
  int functk = 0, funcnum = 0;
  char builtin_name[16];  // be sure it's long enough for all builtins
  if (ISTOK(tkbuiltin)) {
    functk = TT.scs->tokbuiltin;
    strcpy(builtin_name, TT.tokstr);
  } else if (ISTOK(tkfunc)) { // user function
    funcnum = find_func_def_entry(TT.tokstr);
    if (!funcnum) funcnum = add_func_def_entry(TT.tokstr);
    FUNC_DEF[funcnum].flags |= FUNC_CALLED;
    gen2cd(opprepcall, funcnum);
  } else error_exit("bad function %s!", TT.tokstr);
  scan();
  // length() can appear without parens
  int num_args = 0;
  if (functk == tklength && !ISTOK(tklparen)) {
    gen2cd(functk, 0);
    return;
  }
  if (functk) {   // builtin
    builtin_call(functk, builtin_name);
    return;
  }
  expect(tklparen);
  TT.cgl.paren_level++;
  if (ISTOK(tkrparen)) {
    scan();
  } else {
    do {
      if (ISTOK(tkvar) && (TT.scs->ch == ',' || TT.scs->ch == ')')) {
        // Function call arg that is a lone variable. Cannot tell in this
        // context if it is a scalar or map. Just add it to symbol table.
        gen2cd(tkvar, find_or_add_var_name());
        scan();
      } else expr();
      num_args++;
    } while (have_comma());
    expect(tkrparen);
  }
  TT.cgl.paren_level--;
  gen2cd(tkfunc, num_args);
}

static void var(void)
{
  // var name is in TT.tokstr
  // slotnum: + means global; - means local to function
  int slotnum = find_or_add_var_name();
  scan();
  if (havetok(tklbracket)) {
    check_set_map(slotnum);
    int num_subscripts = 0;
    do {
      expr();
      num_subscripts++;
    } while (have_comma());
    expect(tkrbracket);
    if (num_subscripts > 1) gen2cd(tkrbracket, num_subscripts);
    gen2cd(opmap, slotnum);
  } else {
    check_set_scalar(slotnum);
    gen2cd(tkvar, slotnum);
  }
}

//   Dollar $ tkfield can be followed by "any" expresson, but
//   the way it binds varies.
//   The following are valid lvalues:
//   $ ( expr )
//   $ tkvar $ tknumber $ tkstring $ tkregex
//   $ tkfunc(...)
//   $ tkbuiltin(...)
//   $ length   # with no parens after
//   $ tkclose(), ... $ tksubstr
//   $ tkgetline FIXME TODO TEST THIS
//   $ ++ lvalue
//   $ -- lvalue
//   $ + expression_up_to_exponentiation (also -, ! prefix ops)
//   $ $ whatever_can_follow_and_bind_to_dollar
//
//     tkvar, tknumber, tkstring, tkregex, tkfunc, tkbuiltin, tkfield, tkminus,
//     tkplus, tknot, tkincr, tkdecr, tklparen, tkgetline,
//     tkclose, tkindex, tkmatch, tksplit, tksub, tkgsub, tksprintf, tksubstr
//
// ray@radon:~$ awk 'BEGIN { $0 = "7 9 5 8"; k=2; print $k*k }'
// 18
// ray@radon:~$ awk 'BEGIN { $0 = "7 9 5 8"; k=2; print $+k*k }'
// 18
// ray@radon:~$ awk 'BEGIN { $0 = "7 9 5 8"; k=2; print $k^k }'
// 81
// ray@radon:~$ awk 'BEGIN { $0 = "7 9 5 8"; k=2; print $+k^k }'
// 8

static void field_op(void)
{
  // CURTOK() must be $ here.
  expect(tkfield);
  // tkvar, tknumber, tkstring, tkregex, tkfunc, tkbuiltin, tkfield, tkminus,
  // tkplus, tknot, tkincr, tkdecr, tklparen, tkgetline, tkclose, tkindex,
  // tkmatch, tksplit, tksub, tkgsub, tksprintf, tksubstr
  if (ISTOK(tkfield)) field_op();
  else if (ISTOK(tkvar)) var();
  else primary();
  // tkfield op has "dummy" 2nd word so that convert_push_to_reference(void)
  // can find either tkfield or tkvar at same place (ZCODE[TT.zcode_last-1]).
  gen2cd(tkfield, tkeof);
}

// Tokens that can start expression
static char exprstartsy[] = {tkvar, tknumber, tkstring, tkregex, tkfunc,
  tkbuiltin, tkfield, tkminus, tkplus, tknot, tkincr, tkdecr, tklparen,
  tkgetline, tkclose, tkindex, tkmatch, tksplit, tksub, tkgsub, tksprintf,
  tksubstr, 0};

// Tokens that can end statement
static char stmtendsy[] = {tknl, tksemi, tkrbrace, 0};

// Tokens that can follow expressions of a print statement
static char printexprendsy[] = {tkgt, tkappend, tkpipe, tknl, tksemi, tkrbrace, 0};

// !! Ensure this:
// ternary op is right associative, so
// a ? b : c ? d : e        evaluates as
// a ? b : (c ? d : e)      not as
// (a ? b : c) ? d : e

static void convert_push_to_reference(void)
{
  if (ZCODE[TT.zcode_last - 1] == tkvar) ZCODE[TT.zcode_last-1] = opvarref;
  else if (ZCODE[TT.zcode_last - 1] == opmap) ZCODE[TT.zcode_last - 1] = opmapref;
  else if (ZCODE[TT.zcode_last - 1] == tkfield) ZCODE[TT.zcode_last - 1] = opfldref;
  else error_exit("bad lvalue?");
}

static void lvalue(void)
{
  if (ISTOK(tkfield)) {
    field_op();
    convert_push_to_reference();
  } else if (ISTOK(tkvar)) {
    var();
    convert_push_to_reference();
  } else {
    XERR("syntax near '%s' (bad lvalue)\n", TT.tokstr);
  }
}

static int primary(void)
{
  //  On entry: CURTOK() is first token of expression
  //  On exit: CURTOK() is infix operator (for binary_op() to handle) or next
  //   token after end of expression.
  //  return -1 for field or var (potential lvalue);
  //      2 or more for comma-separated expr list
  //          as in "multiple subscript expression in array"
  //          e.g. (1, 2) in array_name, or a print/printf list;
  //      otherwise return 0
  //
  //  expr can start with:
  //      tkvar, tknumber, tkstring, tkregex, tkfunc, tkbuiltin, tkfield, tkminus,
  //      tkplus, tknot, tkincr, tkdecr, tklparen, tkgetline, tkclose, tkindex,
  //      tkmatch, tksplit, tksub, tkgsub, tksprintf, tksubstr
  //
  //  bwk treats these as keywords, not builtins: close index match split sub gsub
  //      sprintf substr
  //
  //  bwk builtins are: atan2 cos sin exp log sqrt int rand srand length tolower
  //      toupper system fflush
  //  NOTE: fflush() is NOT in POSIX awk
  //
  //  primary() must consume prefix and postfix operators as well as
  //      num, string, regex, var, var with subscripts, and function calls

  int num_exprs = 0;
  int nargs, modifier;
  int tok = CURTOK();
  switch (tok) {
    case tkvar:
    case tkfield:
      if (ISTOK(tkvar)) var();
      else field_op();
      if (ISTOK(tkincr) || ISTOK(tkdecr)) {
        convert_push_to_reference();
        gencd(CURTOK());
        scan();
      } else return -1;
      break;

    case tknumber:
      gen2cd(tknumber, make_literal_num_val(TT.scs->numval));
      scan();
      break;

    case tkstring:
      gen2cd(tkstring, make_literal_str_val(TT.tokstr));
      scan();
      break;

    case tkregex:
      // When an ERE token appears as an expression in any context other
      // than as the right-hand of the '~' or "!~" operator or as one of
      // the built-in function arguments described below, the value of
      // the resulting expression shall be the equivalent of: $0 ~ /ere/
      // FIXME TODO
      gen2cd(opmatchrec, make_literal_regex_val(TT.tokstr));
      scan();
      break;

    case tkbuiltin: // various builtins
    case tkfunc:    // user-defined function
      function_call();
      break;

    // Unary prefix ! + -
    case tknot:
    case tkminus:
    case tkplus:
      scan();
      exprn(getlbp(tknot));   // unary +/- same precedence as !
      if (tok == tknot) gencd(tknot);
      else gencd(opnegate);               // forces to number
      if (tok == tkplus) gencd(opnegate); // forces to number
      break;

      // Unary prefix ++ -- MUST take lvalue
    case tkincr:
    case tkdecr:
      scan();
      lvalue();
      if (tok == tkincr) gencd(oppreincr);
      else gencd(oppredecr);
      break;

    case tklparen:
      scan();
      TT.cgl.paren_level++;
      num_exprs = 0;
      do {
        expr();
        num_exprs++;
      } while (have_comma());
      expect(tkrparen);
      TT.cgl.paren_level--;
      if (num_exprs > 1) return num_exprs;
      break;

    case tkgetline:
      // getline may be (according to awk book):
      // getline [var [<file]]
      // getline <file
      // cmd | getline [var]
      // var must be lvalue (can be any lvalue?)
      scan();
      nargs = 0;
      modifier = tkeof;
      if (ISTOK(tkfield) || ISTOK(tkvar)) {
        lvalue();
        nargs++;
      }
      if (havetok(tklt)) {
        exprn(getrbp(tkcat));   // bwk "historical practice" precedence
        nargs++;
        modifier = tklt;
      }
      gen2cd(tkgetline, nargs);
      gencd(modifier);
      break;

    default:
      XERR("syntax near '%s'\n", TT.tokstr[0] == '\n' ? "\\n" : TT.tokstr);
      skip_to(stmtendsy);
      break;
  }
  return 0;
}

static void binary_op(int optor)  // Also for ternary ?: optor.
{
  int nargs, cdx = 0;  // index in TT.zcode list
  int rbp = getrbp(optor);
  if (optor != tkcat) scan();
  // CURTOK() holds first token of right operand.
  switch (optor) {
    case tkin:
      // right side of 'in' must be (only) an array name
      map_name();
      gencd(tkin);
      scan();
      // FIXME TODO 20230109 x = y in a && 2 works OK?
      // x = y in a + 2 does not; it's parsed as x = (y in a) + 2
      // The +2 is not cat'ed with (y in a) as in bwk's OTA.
      // Other awks see y in a + 2 as a syntax error. They (may)
      // not want anything after y in a except a lower binding operator
      // (&& || ?:) or end of expression, i.e. ')' ';' '}'
      break;

  case tkpipe:
      expect(tkgetline);
      nargs = 1;
      if (ISTOK(tkfield) || ISTOK(tkvar)) {
        lvalue();
        nargs++;
      }
      gen2cd(tkgetline, nargs);
      gencd(tkpipe);
      break;

  case tkand:
  case tkor:
      optional_nl();
      gen2cd(optor, -1);  // tkand: jump if false, else drop
      cdx = TT.zcode_last;   // tkor:  jump if true, else drop
      exprn(rbp);
      gencd(opnotnot);    // replace TT.stack top with truth value
      ZCODE[cdx] = TT.zcode_last - cdx;
      break;

  case tkternif:
      gen2cd(optor, -1);
      cdx = TT.zcode_last;
      expr();
      expect(tkternelse);
      gen2cd(tkternelse, -1);
      ZCODE[cdx] = TT.zcode_last - cdx;
      cdx = TT.zcode_last;
      exprn(rbp);
      ZCODE[cdx] = TT.zcode_last - cdx;
      break;

  case tkmatchop:
  case tknotmatch:
      exprn(rbp);
      if (ZCODE[TT.zcode_last - 1] == opmatchrec) ZCODE[TT.zcode_last - 1] = tkregex;
      gencd(optor);
      break;

  default:
      exprn(rbp);
      gencd(optor);
  }
}

static int cat_start_concated_expr(int tok)
{
  // concat'ed expr can start w/ var number string func builtin $ ! ( (or ++ if prev was not lvalue)
  static char exprstarttermsy[] = {tkvar, tknumber, tkstring, tkregex, tkfunc, tkbuiltin,
    tkfield, tknot, tkincr, tkdecr, tklparen, tkgetline, 0};

  // NOTE this depends on builtins (close etc) being >= tkgetline
  return !! strchr(exprstarttermsy, tok) || tok >= tkgetline;
}

#define CALLED_BY_PRINT 99987 // Arbitrary, different from any real rbp value

static int exprn(int rbp)
{
  // On entry: TT.scs has first symbol of expression, e.g. var, number, string,
  // regex, func, getline, left paren, prefix op ($ ++ -- ! unary + or -) etc.
  static char asgnops[] = {tkpowasgn, tkmodasgn, tkmulasgn, tkdivasgn,
    tkaddasgn, tksubasgn, tkasgn, 0};
  int prim_st = primary();
  // If called directly by print_stmt(), and found a parenthesized expression list
  //    followed by an end of print statement: any of > >> | ; } <newline>
  //    Then: return the count of expressions in list
  //    Else: continue parsing an expression
  if (rbp == CALLED_BY_PRINT) {
    if (prim_st > 0 && strchr(printexprendsy, CURTOK())) return prim_st;
    else rbp = 0;
  }

  // mult_expr_list in parens must be followed by 'in' unless it
  // immediately follows print or printf, where it may still be followed
  // by 'in' ... unless at end of statement
  if (prim_st > 0 && ! ISTOK(tkin))
    XERR("syntax near '%s'; expected 'in'\n", TT.tokstr);
  if (prim_st > 0) gen2cd(tkrbracket, prim_st);
  // primary() has eaten subscripts, function args, postfix ops.
  // CURTOK() should be a binary op.
  int optor = CURTOK();
  if (strchr(asgnops, optor)) {

    // TODO FIXME ?  NOT SURE IF THIS WORKS RIGHT!
    // awk does not parse according to POSIX spec in some odd cases.
    // When an assignment (lvalue =) is on the right of certain operators,
    // it is not treated as a bad lvalue (as it is in C).
    // Example: (1 && a=2) # no error; the assignment is performed.
    // This happens for ?: || && ~ !~ < <= ~= == > >=
    //
    static char odd_assignment_rbp[] = {59, 60, 70, 80, 100, 110, 0};
    if (prim_st < 0 && (rbp <= getrbp(optor) || strchr(odd_assignment_rbp, rbp))) {
      convert_push_to_reference();
      scan();
      exprn(getrbp(optor));
      gencd(optor);
      return 0;
    }
    XERR("syntax near '%s'\n", TT.tokstr[0] == '\n' ? "\\n" : TT.tokstr);
    skip_to(stmtendsy);
  }
  if (cat_start_concated_expr(optor)) optor = tkcat;
  while (rbp < getlbp(optor)) {
    binary_op(optor);
    // HERE tok s/b an operator or expression terminator ( ; etc.).
    optor = CURTOK();
    if (cat_start_concated_expr(optor)) optor = tkcat;
  }
  return 0;
}

static void print_stmt(int tk)
{
  static char outmodes[] = {tkgt, tkappend, tkpipe, 0};
  int num_exprs = 0, outmode;
  TT.cgl.in_print_stmt = 1;
  expect(tk); // tkprint or tkprintf
  if ((tk == tkprintf) || !strchr(printexprendsy, CURTOK())) {
    // printf always needs expression
    // print non-empty statement needs expression
    num_exprs = exprn(CALLED_BY_PRINT);
    if (num_exprs > 0 && !strchr(printexprendsy, CURTOK())) FATAL("print stmt bug");
    if (!num_exprs) {
      for (num_exprs++; have_comma(); num_exprs++)
        expr();
    }
  }
  outmode = CURTOK();
  if (strchr(outmodes, outmode)) {
    scan();
    expr(); // FIXME s/b only bwk term? check POSIX
    num_exprs++;
  } else outmode = 0;
  gen2cd(tk, num_exprs);
  gencd(outmode);
  TT.cgl.in_print_stmt = 0;
}

static void delete_stmt(void)
{
  expect(tkdelete);
  if (ISTOK(tkvar)) {
    int slotnum = find_or_add_var_name();
    check_set_map(slotnum);
    scan();
    if (havetok(tklbracket)) {
      int num_subscripts = 0;
      do {
        expr();
        num_subscripts++;
      } while (have_comma());
      expect(tkrbracket);
      if (num_subscripts > 1) gen2cd(tkrbracket, num_subscripts);
      gen2cd(opmapref, slotnum);
      gencd(tkdelete);
    } else {
      // delete entire map (elements only; var is still a map)
      gen2cd(opmapref, slotnum);
      gencd(opmapdelete);
    }
  } else expect(tkvar);
}

static void simple_stmt(void)
{
  if (strchr(exprstartsy, CURTOK())) {
    expr();
    gencd(opdrop);
    return;
  }
  switch (CURTOK()) {
    case tkprint:
    case tkprintf:
      print_stmt(CURTOK());
      break;

    case tkdelete:
      delete_stmt();
      break;

    default:
      XERR("syntax near '%s'\n", TT.tokstr[0] == '\n' ? "\\n" : TT.tokstr);
      skip_to(stmtendsy);
  }
}

static int prev_was_terminated(void)
{
  return !!strchr(stmtendsy, TT.prevtok);
}

static int is_nl_semi(void)
{
  return ISTOK(tknl) || ISTOK(tksemi);
}

static void if_stmt(void)
{
  expect(tkif);
  expect(tklparen);
  expr();
  rparen();
  gen2cd(tkif, -1);
  int cdx = TT.zcode_last;
  stmt();
  if (!prev_was_terminated() && is_nl_semi()) {
    scan();
    optional_nl();
  }
  if (prev_was_terminated()) {
    optional_nl();
    if (havetok(tkelse)) {
      gen2cd(tkelse, -1);
      ZCODE[cdx] = TT.zcode_last - cdx;
      cdx = TT.zcode_last;
      optional_nl();
      stmt();
    }
  }
  ZCODE[cdx] = TT.zcode_last - cdx;
}

static void save_break_continue(int *brk, int *cont)
{
  *brk = TT.cgl.break_dest;
  *cont = TT.cgl.continue_dest;
}

static void restore_break_continue(int *brk, int *cont)
{
  TT.cgl.break_dest = *brk;
  TT.cgl.continue_dest = *cont;
}

static void while_stmt(void)
{
  int brk, cont;
  save_break_continue(&brk, &cont);
  expect(tkwhile);
  expect(tklparen);
  TT.cgl.continue_dest = TT.zcode_last + 1;
  expr();
  rparen();
  gen2cd(tkwhile, 2);    // drop, jump if true
  TT.cgl.break_dest = TT.zcode_last + 1;
  gen2cd(opjump, -1);     // jump here to break
  stmt();
  gen2cd(opjump, -1);     // jump to continue
  ZCODE[TT.zcode_last] = TT.cgl.continue_dest - TT.zcode_last - 1;
  ZCODE[TT.cgl.break_dest + 1] = TT.zcode_last - TT.cgl.break_dest - 1;
  restore_break_continue(&brk, &cont);
}

static void do_stmt(void)
{
  int brk, cont;
  save_break_continue(&brk, &cont);
  expect(tkdo);
  optional_nl();
  gen2cd(opjump, 4);   // jump over jumps, to statement
  TT.cgl.continue_dest = TT.zcode_last + 1;
  gen2cd(opjump, -1);   // here on continue
  TT.cgl.break_dest = TT.zcode_last + 1;
  gen2cd(opjump, -1);   // here on break
  stmt();
  if (!prev_was_terminated()) {
    if (is_nl_semi()) {
      scan();
      optional_nl();
    } else {
      XERR("syntax near '%s' -- ';' or newline expected\n", TT.tokstr);
      // FIXME
    }
  }
  ZCODE[TT.cgl.continue_dest + 1] = TT.zcode_last - TT.cgl.continue_dest - 1;
  optional_nl();
  expect(tkwhile);
  expect(tklparen);
  expr();
  rparen();
  gen2cd(tkwhile, TT.cgl.break_dest - TT.zcode_last - 1);
  ZCODE[TT.cgl.break_dest + 1] = TT.zcode_last - TT.cgl.break_dest - 1;
  restore_break_continue(&brk, &cont);
}

static void for_not_map_iter(void)
{
  // Here after loop initialization, if any; loop condition
  int condition_loc = TT.zcode_last + 1;
  if (havetok(tksemi)) {
    // "endless" loop variant; no condition
    // no NL allowed here in OTA
    gen2cd(opjump, -1);     // jump to statement
  } else {
    optional_nl();                // NOT posix or awk book; in OTA
    expr();                 // loop while true
    expect(tksemi);
    gen2cd(tkwhile, -1);    // drop, jump to statement if true
  }
  optional_nl();                    // NOT posix or awk book; in OTA
  TT.cgl.break_dest = TT.zcode_last + 1;
  gen2cd(opjump, -1);
  TT.cgl.continue_dest = TT.zcode_last + 1;
  if (!ISTOK(tkrparen)) simple_stmt();  // "increment"
  gen2cd(opjump, condition_loc - TT.zcode_last - 3);
  rparen();
  ZCODE[TT.cgl.break_dest - 1] = TT.zcode_last - TT.cgl.break_dest + 1;
  stmt();
  gen2cd(opjump, TT.cgl.continue_dest - TT.zcode_last - 3);
  ZCODE[TT.cgl.break_dest + 1] = TT.zcode_last - TT.cgl.break_dest - 1;
}

static int valid_for_array_iteration(int first, int last)
{
  return ZCODE[first] == tkvar && ZCODE[first + 2] == tkvar
      && ZCODE[first + 4] == tkin && ZCODE[first + 5] == opdrop
      && first + 5 == last;
}

static void for_stmt(void)
{
  int brk, cont;
  save_break_continue(&brk, &cont);
  expect(tkfor);
  expect(tklparen);
  if (havetok(tksemi)) {
    // No "initialization" part
    for_not_map_iter();
  } else {
    int loop_start_loc = TT.zcode_last + 1;
    simple_stmt();  // initializaton part, OR varname in arrayname form
    if (!havetok(tkrparen)) {
      expect(tksemi);
      for_not_map_iter();
    } else {
      // Must be map iteration
      // Check here for varname in varname!
      // FIXME TODO must examine generated TT.zcode for var in array?
      if (!valid_for_array_iteration(loop_start_loc, TT.zcode_last))
        XERR("%s", "bad 'for (var in array)' loop\n");
      else {
        ZCODE[TT.zcode_last-5] = opvarref;
        ZCODE[TT.zcode_last-1] = tknumber;
        ZCODE[TT.zcode_last] = make_literal_num_val(-1);
        TT.cgl.continue_dest = TT.zcode_last + 1;
        gen2cd(opmapiternext, 2);
        TT.cgl.break_dest = TT.zcode_last + 1;
        gen2cd(opjump, -1);   // fill in with loc after stmt
      }
      optional_nl();
      // fixup TT.stack if return or exit inside for (var in array)
      TT.cgl.stack_offset_to_fix += 3;
      stmt();
      TT.cgl.stack_offset_to_fix -= 3;
      gen2cd(opjump, TT.cgl.continue_dest - TT.zcode_last - 3);
      ZCODE[TT.cgl.break_dest + 1] = TT.zcode_last - TT.cgl.break_dest - 1;
      gencd(opdrop);
      gencd(opdrop);
      gencd(opdrop);
    }
  }
  restore_break_continue(&brk, &cont);
}

static void stmt(void)
{
  switch (CURTOK()) {
    case tkeof:
      break;     // FIXME ERROR?

    case tkbreak:
      scan();
      if (TT.cgl.break_dest) gen2cd(tkbreak, TT.cgl.break_dest - TT.zcode_last - 3);
      else XERR("%s", "break not in a loop\n");
      break;

    case tkcontinue:
      scan();
      if (TT.cgl.continue_dest)
        gen2cd(tkcontinue, TT.cgl.continue_dest - TT.zcode_last - 3);
      else XERR("%s", "continue not in a loop\n");
      break;

    case tknext:
      scan();
      gencd(tknext);
      if (TT.cgl.rule_type) XERR("%s", "next inside BEGIN or END\n");
      if (TT.cgl.in_function_body) XERR("%s", "next inside function def\n");
      break;

    case tknextfile:
      scan();
      gencd(tknextfile);
      if (TT.cgl.rule_type) XERR("%s", "nextfile inside BEGIN or END\n");
      if (TT.cgl.in_function_body) XERR("%s", "nextfile inside function def\n");
      break;

    case tkexit:
      scan();
      if (strchr(exprstartsy, CURTOK())) {
        expr();
      } else gen2cd(tknumber, make_literal_num_val(NO_EXIT_STATUS));
      gencd(tkexit);
      break;

    case tkreturn:
      scan();
      if (TT.cgl.stack_offset_to_fix) gen2cd(opdrop_n, TT.cgl.stack_offset_to_fix);
      if (strchr(exprstartsy, CURTOK())) {
        expr();
      } else gen2cd(tknumber, make_literal_num_val(0.0));
      gen2cd(tkreturn, TT.cgl.nparms);
      if (!TT.cgl.in_function_body) XERR("%s", "return outside function def\n");
      break;

    case tklbrace:
      action(tklbrace);
      break;

    case tkif:
      if_stmt();
      break;

    case tkwhile:
      while_stmt();
      break;

    case tkdo:
      do_stmt();
      break;

    case tkfor:
      for_stmt();
      break;

    case tksemi:
      scan();
      break;
    default:
      simple_stmt();      // expression print printf delete
  }
}

static void add_param(int funcnum, char *s)
{
  if (!find_local_entry(s)) add_local_entry(s);
  else XERR("function '%s' dup param '%s'\n", FUNC_DEF[funcnum].name, s);
  TT.cgl.nparms++;

  // POSIX: The same name shall not be used as both a function parameter name
  // and as the name of a function or a special awk variable.
  // !!! NOTE seems implementations exc. mawk only compare param names with
  // builtin funcs; use same name as userfunc is OK!
  if (!strcmp(s, FUNC_DEF[funcnum].name))
    XERR("function '%s' param '%s' matches func name\n",
        FUNC_DEF[funcnum].name, s);
  if (find_global(s) && find_global(s) < TT.spec_var_limit)
    XERR("function '%s' param '%s' matches special var\n",
        FUNC_DEF[funcnum].name, s);
}

static void function_def(void)
{
  expect(tkfunction);
  int funcnum = find_func_def_entry(TT.tokstr);
  if (!funcnum) {
    funcnum = add_func_def_entry(TT.tokstr);
  } else if (FUNC_DEF[funcnum].flags & FUNC_DEFINED) {
    XERR("dup defined function '%s'\n", TT.tokstr);
  }
  FUNC_DEF[funcnum].flags |= FUNC_DEFINED;
  if (find_global(TT.tokstr)) {
    // POSIX: The same name shall not be used both as a variable name with
    // global scope and as the name of a function.
    XERR("function name '%s' previously defined\n", TT.tokstr);
  }

  gen2cd(tkfunction, funcnum);
  FUNC_DEF[funcnum].zcode_addr = TT.zcode_last - 1;
  TT.cgl.funcnum = funcnum;
  TT.cgl.nparms = 0;
  if (ISTOK(tkfunc)) expect(tkfunc); // func name with no space before (
  else expect(tkvar);  // func name with space before (
  expect(tklparen);
  if (ISTOK(tkvar)) {
    add_param(funcnum, TT.tokstr);
    scan();
    // FIXME is the the best way? what if TT.tokstr not a tkvar?
    while (have_comma()) {
      add_param(funcnum, TT.tokstr);
      expect(tkvar);
    }
  }
  rparen();
  if (ISTOK(tklbrace)) {
    TT.cgl.in_function_body = 1;
    action(tkfunc);
    TT.cgl.in_function_body = 0;
    // Need to return uninit value if falling off end of function.
    gen2cd(tknumber, make_uninit_val());
    gen2cd(tkreturn, TT.cgl.nparms);
  } else {
    XERR("syntax near '%s'\n", TT.tokstr);
    // FIXME some recovery needed here!?
  }
  // Do not re-init locals table for dup function.
  // Avoids memory leak detected by LeakSanitizer.
  if (!FUNC_DEF[funcnum].function_locals.base) {
    FUNC_DEF[funcnum].function_locals = TT.locals_table;
    init_locals_table();
  }
}

static void action(int action_type)
{
(void)action_type;
  // action_type is tkbegin, tkend, tkdo (every line), tkif (if pattern),
  //                  tkfunc (function body), tklbrace (compound statement)
  // Should have lbrace on entry.
  expect(tklbrace);
  for (;;) {
    if (ISTOK(tkeof)) unexpected_eof();
    optional_nl_or_semi();
    if (havetok(tkrbrace)) {
      break;
    }
    stmt();
    // stmt() is normally unterminated here, but may be terminated if we
    // have if with no else (had to consume terminator looking for else)
    //   !!!   if (ISTOK(tkrbrace) || prev_was_terminated())
    if (prev_was_terminated()) continue;
    if (!is_nl_semi() && !ISTOK(tkrbrace)) {
      XERR("syntax near '%s' -- newline, ';', or '}' expected\n", TT.tokstr);
      while (!is_nl_semi() && !ISTOK(tkrbrace) && !ISTOK(tkeof)) scan();
      if (ISTOK(tkeof)) unexpected_eof();
    }
    if (havetok(tkrbrace)) break;
    // Must be semicolon or newline
    scan();
  }
}

static void rule(void)
{
  //       pa_pat
  //     | pa_pat lbrace stmtlist '}'
  //     | pa_pat ',' opt_nl pa_pat
  //     | pa_pat ',' opt_nl pa_pat lbrace stmtlist '}'
  //     | lbrace stmtlist '}'
  //     | XBEGIN lbrace stmtlist '}'
  //     | XEND lbrace stmtlist '}'
  //     | FUNC funcname '(' varlist rparen  lbrace stmtlist '}'

  switch (CURTOK()) {
    case tkbegin:
      scan();
      if (TT.cgl.last_begin) ZCODE[TT.cgl.last_begin] = TT.zcode_last - TT.cgl.last_begin;
      else TT.cgl.first_begin = TT.zcode_last + 1;

      TT.cgl.rule_type = tkbegin;
      action(tkbegin);
      TT.cgl.rule_type = 0;
      gen2cd(opjump, -1);
      TT.cgl.last_begin = TT.zcode_last;
      break;

    case tkend:
      scan();
      if (TT.cgl.last_end) ZCODE[TT.cgl.last_end] = TT.zcode_last - TT.cgl.last_end;
      else TT.cgl.first_end = TT.zcode_last + 1;

      TT.cgl.rule_type = tkbegin;
      action(tkend);
      TT.cgl.rule_type = 0;
      gen2cd(opjump, -1);
      TT.cgl.last_end = TT.zcode_last;
      break;

    case tklbrace:
      if (TT.cgl.last_recrule)
        ZCODE[TT.cgl.last_recrule] = TT.zcode_last - TT.cgl.last_recrule;
      else TT.cgl.first_recrule = TT.zcode_last + 1;
      action(tkdo);
      gen2cd(opjump, -1);
      TT.cgl.last_recrule = TT.zcode_last;
      break;

    case tkfunction:
      function_def();
      break;
    default:
      if (TT.cgl.last_recrule)
        ZCODE[TT.cgl.last_recrule] = TT.zcode_last - TT.cgl.last_recrule;
      else TT.cgl.first_recrule = TT.zcode_last + 1;
      gen2cd(opjump, 1);
      gencd(tkeof);
      int cdx = 0, saveloc = TT.zcode_last;
      expr();
      if (!have_comma()) {
        gen2cd(tkif, -1);
        cdx = TT.zcode_last;
      } else {
        gen2cd(oprange2, ++TT.cgl.range_pattern_num);
        gencd(-1);
        cdx = TT.zcode_last;
        ZCODE[saveloc-2] = oprange1;
        ZCODE[saveloc-1] = TT.cgl.range_pattern_num;
        ZCODE[saveloc] = TT.zcode_last - saveloc;
        expr();
        gen2cd(oprange3, TT.cgl.range_pattern_num);
      }
      if (ISTOK(tklbrace)) {
        action(tkif);
        ZCODE[cdx] = TT.zcode_last - cdx;
      } else {
        gencd(opprintrec);   // print $0 ?
        ZCODE[cdx] = TT.zcode_last - cdx;
      }
      gen2cd(opjump, -1);
      TT.cgl.last_recrule = TT.zcode_last;
  }
}

static void diag_func_def_ref(void)
{
  int n = zlist_len(&TT.func_def_table);
  for (int k = 1; k < n; k++) {
    if ((FUNC_DEF[k].flags & FUNC_CALLED) &&
            !(FUNC_DEF[k].flags & FUNC_DEFINED)) {
      // Sorry, we can't tell where this was called from, for now at least.
      XERR("Undefined function '%s'", FUNC_DEF[k].name);
    }
  }
}

EXTERN void compile(void)
{
  init_compiler();
  init_scanner();
  scan();
  optional_nl_or_semi();        // Does posix allow NL or ; before first rule?
  while (! ISTOK(tkeof)) {
    rule();
    optional_nl_or_semi();        // NOT POSIX
  }


  if (TT.cgl.last_begin) ZCODE[TT.cgl.last_begin-1] = opquit;
  if (TT.cgl.last_end) ZCODE[TT.cgl.last_end-1] = opquit;
  if (TT.cgl.last_recrule) ZCODE[TT.cgl.last_recrule-1] = opquit;

  gen2cd(tknumber, make_literal_num_val(0.0));
  gencd(tkexit);
  gencd(opquit);
  // If there are only BEGIN and END or only END actions, generate actions to
  // read all input before END.
  if (TT.cgl.first_end && !TT.cgl.first_recrule) {
    gencd(opquit);
    TT.cgl.first_recrule = TT.zcode_last;
  }
  gencd(opquit);  // One more opcode to keep ip in bounds in run code.
  diag_func_def_ref();
}
