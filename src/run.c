// run.c
// Copyright 2024 Ray Gardner
// License: 0BSD
// vi: tabstop=2 softtabstop=2 shiftwidth=2

#include "common.h"

////////////////////
//// runtime
////////////////////

static void check_numeric_string(struct zvalue *v)
{
  if (v->vst) {
    char *end, *s = v->vst->str;
    // Significant speed gain with this test:
    // num string must begin space, +, -, ., or digit.
    if (strchr("+-.1234567890 ", *s)) {
      double num = strtod(s, &end);
      if (s == end || end[strspn(end, " ")]) return;
      v->num = num;
      v->flags |= ZF_NUM | ZF_STR | ZF_NUMSTR;
    }
  }
}

static struct zstring *num_to_zstring(double n, char *fmt)
{
  int k;
  if (n == (long long)n) k = snprintf(TT.pbuf, PBUFSIZE, "%lld", (long long)n);
  else k = snprintf(TT.pbuf, PBUFSIZE, fmt, n);
  if (k < 0 || k >= PBUFSIZE) FFATAL("error encoding %f via '%s'", n, fmt);
  return new_zstring(TT.pbuf, k);
}

////////////////////
//// regex routines
////////////////////

EXTERN char *escape_str(char *s, int is_regex)
{
  char *p, *escapes = is_regex ? "abfnrtv\"/" : "\\abfnrtv\"/";
  // FIXME TODO should / be in there?
  char *s0 = s, *to = s;
  while ((*to = *s)) {
    if (*s != '\\') { to++, s++;
    } else if ((p = strchr(escapes, *++s))) {
      // checking char after \ for known escapes
      int c = (is_regex?"\a\b\f\n\r\t\v\"/":"\\\a\b\f\n\r\t\v\"/")[p-escapes];
      if (c) *to = c, s++;  // else final backslash
      to++;
    } else if ('0' <= *s && *s <= '9') {
      int k, c = *s++ - '0';
      for (k = 0; k < 2 && '0' <= *s && *s <= '9'; k++)
        c = c * 8 + *s++ - '0';
      *to++ = c;
    } else if (*s == 'x') {
      if (isxdigit(s[1])) {
        int c = hexval(*++s);
        if (isxdigit(s[1])) c = c * 16 + hexval(*++s);
        *to++ = c, s++;
      }
    } else {
      if (is_regex) *to++ = '\\';
      *to++ = *s++;
    }
  }
  return s0;
}

static void force_maybemap_to_scalar(struct zvalue *v)
{
  if (!(v->flags & ZF_ANYMAP)) return;
  if (v->flags & ZF_MAP || v->map->count)
    FATAL("array in scalar context");
  v->flags = 0;
  v->map = 0; // v->flags = v->map = 0 gets warning
}

static void force_maybemap_to_map(struct zvalue *v)
{
  if (v->flags & ZF_MAYBEMAP) v->flags = ZF_MAP;
}

// fmt_offs is either CONVFMT or OFMT (offset in stack to zvalue)
static struct zvalue *to_str_fmt(struct zvalue *v, int fmt_offs)
{
  force_maybemap_to_scalar(v);
  // TODO: consider handling numstring differently
  if (v->flags & ZF_NUMSTR) v->flags = ZF_STR;
  if (IS_STR(v)) return v;
  else if (!v->flags) { // uninitialized
    v->vst = new_zstring("", 0);
  } else if (IS_NUM(v)) {
    zvalue_release_zstring(v);
    if (!IS_STR(&STACK[fmt_offs])) {
      zstring_release(&STACK[fmt_offs].vst);
      STACK[fmt_offs].vst = num_to_zstring(STACK[fmt_offs].num, "%.6g");
      STACK[fmt_offs].flags = ZF_STR;
    }
    v->vst = num_to_zstring(v->num, STACK[fmt_offs].vst->str);
  } else {
    FATAL("Wrong or unknown type in to_str_fmt\n");
  }
  v->flags = ZF_STR;
  return v;
}

static struct zvalue *to_str(struct zvalue *v)
{
  return to_str_fmt(v, CONVFMT);
}

// TODO FIXME Is this needed? (YES -- investigate) Just use to_str()?
#define ENSURE_STR(v) (IS_STR(v) ? (v) : to_str(v))

static void rx_zvalue_compile(regex_t **rx, struct zvalue *pat)
{
  if (IS_RX(pat)) *rx = pat->rx;
  else {
    zvalue_dup_zstring(to_str(pat));
    escape_str(pat->vst->str, 1);
    xregcomp(*rx, pat->vst->str, REG_EXTENDED);
  }
}

static void rx_zvalue_free(regex_t *rx, struct zvalue *pat)
{
  if (!IS_RX(pat) || rx != pat->rx) regfree(rx);
}

// Used by the match/not match ops (~ !~) and implicit $0 match (/regex/)
static int match(struct zvalue *zvsubject, struct zvalue *zvpat)
{
  int r;
  regex_t rx, *rxp = &rx;
  rx_zvalue_compile(&rxp, zvpat);
  if ((r = regexec(rxp, to_str(zvsubject)->vst->str, 0, 0, 0)) != 0) {
    if (r != REG_NOMATCH) {
      char errbuf[256];
      regerror(r, &rx, errbuf, sizeof(errbuf));
      // FIXME TODO better diagnostic here
      error_exit("regex match error %d: %s", r, errbuf);
    }
    rx_zvalue_free(rxp, zvpat);
    return 1;
  }
  rx_zvalue_free(rxp, zvpat);
  return 0;
}

static int rx_find(regex_t *rx, char *s, regoff_t *start, regoff_t *end, int eflags)
{
  regmatch_t matches[1];
  int r = regexec(rx, s, 1, matches, eflags);
  if (r == REG_NOMATCH) return r;
  if (r) FATAL("regexec error");  // TODO ? use regerr() to meaningful msg
  *start = matches[0].rm_so;
  *end = matches[0].rm_eo;
  return 0;
}

// Differs from rx_find() in that FS cannot match null (empty) string.
// See https://www.austingroupbugs.net/view.php?id=1468.
static int rx_find_FS(regex_t *rx, char *s, regoff_t *start, regoff_t *end, int eflags)
{
  int r = rx_find(rx, s, start, end, eflags);
  if (r || *start != *end) return r;  // not found, or found non-empty match
  // Found empty match, retry starting past the match
  char *p = s + *end;
  if (!*p) return REG_NOMATCH;  // End of string, no non-empty match found
  // Empty match not at EOS, move ahead and try again
  while (!r && *start == *end && *++p)
    r = rx_find(rx, p, start, end, eflags);
  if (r || !*p) return REG_NOMATCH;  // no non-empty match found
  *start += p - s;  // offsets from original string
  *end += p - s;
  return 0;
}

////////////////////
////   fields
////////////////////

#define FIELDS_MAX  102400 // Was 1024; need more for toybox awk test
#define THIS_MEANS_SET_NF 999999999

static int get_int_val(struct zvalue *v)
{
  if (IS_NUM(v)) return (int)v->num;
  if (IS_STR(v) && v->vst) return (int)atof(v->vst->str);
  return 0;
}

// A single-char FS is never a regex, so make it a [<char>] regex to
// match only that one char in case FS is a regex metachar.
// If regex FS is needed, must use > 1 char. If a '.' regex
// is needed, use e.g. '.|.' (unlikely case).
static char *fmt_one_char_fs(char *fs)
{
  if (strlen(fs) != 1) return fs;
  snprintf(TT.one_char_fs, sizeof(TT.one_char_fs), "[%c]", fs[0]);
  return TT.one_char_fs;
}

static regex_t *rx_fs_prep(char *fs)
{
  if (!strcmp(fs, " ")) return &TT.rx_default;
  if (!strcmp(fs, TT.fs_last)) return &TT.rx_last;
  if (strlen(fs) >= FS_MAX) FATAL("FS too long");
  strcpy(TT.fs_last, fs);
  regfree(&TT.rx_last);
  xregcomp(&TT.rx_last, fmt_one_char_fs(fs), REG_EXTENDED);
  return &TT.rx_last;
}

// Only for use by split() builtin
static void set_map_element(struct zmap *m, int k, char *val, size_t len)
{
  // Do not need format here b/c k is integer, uses "%lld" format.
  struct zstring *key = num_to_zstring(k, "");// "" vs 0 format avoids warning
  struct zmap_slot *zs = zmap_find_or_insert_key(m, key);
  zstring_release(&key);
  zs->val.vst = zstring_update(zs->val.vst, 0, val, len);
  zs->val.flags = ZF_STR;
  check_numeric_string(&zs->val);
}

static void set_zvalue_str(struct zvalue *v, char *s, size_t size)
{
  v->vst = zstring_update(v->vst, 0, s, size);
  v->flags = ZF_STR;
}

// All changes to NF go through here!
static void set_nf(int nf)
{
  STACK[NF].num = TT.nf_internal = nf;
  STACK[NF].flags = ZF_NUM;
}

static void set_field(struct zmap *unused, int fnum, char *s, size_t size)
{ (void)unused;
  if (fnum < 0 || fnum > FIELDS_MAX) FFATAL("bad field num %d\n", fnum);
  int nfields = zlist_len(&TT.fields);
  // Need nfields to be > fnum b/c e.g. fnum==1 implies 2 TT.fields
  while (nfields <= fnum)
    nfields = zlist_append(&TT.fields, &uninit_zvalue) + 1;
  set_zvalue_str(&FIELD[fnum], s, size);
  set_nf(fnum);
  check_numeric_string(&FIELD[fnum]);
}

// Split s via fs, using setter; return number of TT.fields.
// This is used to split TT.fields and also for split() builtin.
static int splitter(void (*setter)(struct zmap *, int, char *, size_t), struct zmap *m, char *s, struct zvalue *zvfs)
{
  regex_t *rx;
  regoff_t offs, end;
  int multiline_null_rs = !ENSURE_STR(&STACK[RS])->vst->str[0];
  int nf = 0, r = 0, eflag = 0;
  int one_char_fs = 0;
  char *s0 = s, *fs = "";
  if (!IS_RX(zvfs)) {
    to_str(zvfs);
    fs = zvfs->vst->str;
    one_char_fs = utf8cnt(zvfs->vst->str, zvfs->vst->size) == 1;
  }
  // Empty string or empty fs (regex).
  // Need to include !*s b/c empty string, otherwise
  // split("", a, "x") splits to a 1-element (empty element) array
  if (!*s || (IS_STR(zvfs) && !*fs) || IS_EMPTY_RX(zvfs)) {
    while (*s) {
      if (*s < 128) setter(m, ++nf, s++, 1);
      else {        // Handle UTF-8
        char cbuf[8];
        unsigned wc;
        int nc = utf8towc(&wc, s, strlen(s));
        if (nc < 2) FATAL("bad string for split: \"%s\"\n", s0);
        s += nc;
        nc = wctoutf8(cbuf, wc);
        setter(m, ++nf, cbuf, nc);
      }
    }
    return nf;
  }
  if (IS_RX(zvfs)) rx = zvfs->rx;
  else rx = rx_fs_prep(fs);
  while (*s) {
    // Find the next occurrence of FS.
    // rx_find_FS() returns 0 if found. If nonzero, the field will
    // be the rest of the record (all of it if first time through).
    if ((r = rx_find_FS(rx, s, &offs, &end, eflag))) offs = end = strlen(s);
    if (setter == set_field && multiline_null_rs && one_char_fs) {
      // Contra POSIX, if RS=="" then newline is always also a
      // field separator only if FS is a single char (see gawk manual)
      int k = strcspn(s, "\n");
      if (k < offs) offs = k, end = k + 1;
    }
    eflag |= REG_NOTBOL;

    // Field will be s up to (not including) the offset. If offset
    // is zero and FS is found and FS is ' ' (TT.rx_default "[ \t]+"),
    // then the find is the leading or trailing spaces and/or tabs.
    // If so, skip this (empty) field, otherwise set field, length is offs.
    if (offs || r || rx != &TT.rx_default) setter(m, ++nf, s, offs);
    s += end;
  }
  if (!r && rx != &TT.rx_default) setter(m, ++nf, "", 0);
  return nf;
}

static void build_fields(void)
{
  char *rec = FIELD[0].vst->str;
  // TODO test this -- why did I not want to split empty $0?
  // Maybe don't split empty $0 b/c non-default FS gets NF==1 with splitter()?
  set_nf(*rec ? splitter(set_field, 0, rec, to_str(&STACK[FS])) : 0);
}

static void rebuild_field0(void)
{
  struct zstring *s = FIELD[0].vst;
  int nf = TT.nf_internal;
  // uninit value needed for eventual reference to .vst in zstring_release()
  struct zvalue tempv = uninit_zvalue;
  zvalue_copy(&tempv, to_str(&STACK[OFS]));
  for (int i = 1; i <= nf; i++) {
    if (i > 1) {
      s = s ? zstring_extend(s, tempv.vst) : zstring_copy(s, tempv.vst);
    }
    if (FIELD[i].flags) to_str(&FIELD[i]);
    if (FIELD[i].vst) {
      if (i > 1) s = zstring_extend(s, FIELD[i].vst);
      else s = zstring_copy(s, FIELD[i].vst);
    }
  }
  FIELD[0].vst = s;
  FIELD[0].flags |= ZF_STR;
  zvalue_release_zstring(&tempv);
}

// get field ref (lvalue ref) in prep for assignment to field.
// [... assigning to a nonexistent field (for example, $(NF+2)=5) shall
// increase the value of NF; create any intervening TT.fields with the
// uninitialized value; and cause the value of $0 to be recomputed, with the
// TT.fields being separated by the value of OFS.]
// Called by setup_lvalue()
static struct zvalue *get_field_ref(int fnum)
{
  if (fnum < 0 || fnum > FIELDS_MAX) error_exit("bad field num %d", fnum);
  if (fnum > TT.nf_internal) {
    // Ensure TT.fields list is large enough for fnum
    // Need len of TT.fields to be > fnum b/c e.g. fnum==1 implies 2 TT.fields
    for (int i = TT.nf_internal + 1; i <= fnum; i++) {
      if (i == zlist_len(&TT.fields)) zlist_append(&TT.fields, &uninit_zvalue);
      zvalue_copy(&FIELD[i], &uninit_string_zvalue);
    }
    set_nf(fnum);
  }
  return &FIELD[fnum];
}

// Called by tksplit op
static int split(struct zstring *s, struct zvalue *a, struct zvalue *fs)
{
  return splitter(set_map_element, a->map, s->str, fs);
}

// Called by getrec_f0_f() and getrec_f0()
static void copy_to_field0(char *buf, size_t k)
{
  set_zvalue_str(&FIELD[0], buf, k);
  check_numeric_string(&FIELD[0]);
  build_fields();
}

// After changing $0, must rebuild TT.fields & reset NF
// Changing other field must rebuild $0
// Called by gsub() and assignment ops.
static void fixup_fields(int fnum)
{
  if (fnum == THIS_MEANS_SET_NF) {  // NF was assigned to
    int new_nf = get_int_val(&STACK[NF]);
    // Ensure TT.fields list is large enough for fnum
    // Need len of TT.fields to be > fnum b/c e.g. fnum==1 implies 2 TT.fields
    for (int i = TT.nf_internal + 1; i <= new_nf; i++) {
      if (i == zlist_len(&TT.fields)) zlist_append(&TT.fields, &uninit_zvalue);
      zvalue_copy(&FIELD[i], &uninit_string_zvalue);
    }
    set_nf(TT.nf_internal = STACK[NF].num);
    rebuild_field0();
    return;
  }
  // fnum is # of field that was just updated.
  // If it's 0, need to rebuild the TT.fields 1... n.
  // If it's non-0, need to rebuild field 0.
  to_str(&FIELD[fnum]);
  if (fnum) check_numeric_string(&FIELD[fnum]);
  if (fnum) rebuild_field0();
  else build_fields();
}

// Fetching non-existent field gets uninit string value; no change to NF!
// Called by tkfield op       // TODO inline it?
static void push_field(int fnum)
{
  if (fnum < 0 || fnum > FIELDS_MAX) error_exit("bad field num %d", fnum);
  // Contrary to posix, awk evaluates TT.fields beyond $NF as empty strings.
  if (fnum > TT.nf_internal) push_val(&uninit_string_zvalue);
  else push_val(&FIELD[fnum]);
}

////////////////////
////   END fields
////////////////////

#define STKP    TT.stackp   // pointer to top of stack

#ifndef FOR_TOYBOX
// Random number generator
// Extracted from http://www.cs.ucl.ac.uk/staff/d.jones/GoodPracticeRNG.pdf
// modified to encapsulate state and add seed function.
static struct jkiss32_state {
  unsigned x, y, z, w, c;
} jkst = {123456789, 234567891, 345678912, 456789123, 0};

static unsigned jkiss32(void)
{
  int t;
  jkst.y ^= (jkst.y<<5); jkst.y ^= (jkst.y>>7); jkst.y ^= (jkst.y<<22);
  t = jkst.z+jkst.w+jkst.c; jkst.z = jkst.w; jkst.c = t<0; jkst.w = t&2147483647;
  jkst.x += 1411392427;
  return jkst.x + jkst.y + jkst.w;
}

static void seed_jkiss32(unsigned n)
{
  if (!n) n = 1;
  jkst = (struct jkiss32_state){n*123456789, n*234567891, n*345678912, n*456789123, 0};
  if (n > 1) for (n = 10000; n--;) jkiss32();
}
// END Random number generator
#define random(x) (jkiss32(x) >> 1)
#define srandom(x) seed_jkiss32(x)
#endif  // FOR_TOYBOX
static double seedrand(double seed)
{
  static double prev_seed;
  double r = prev_seed;
  srandom(trunc(prev_seed = seed));
  return r;
}

static int popnumval(void)
{
  return STKP-- -> num;
}

static void drop(void)
{
  if (!(STKP->flags & (ZF_ANYMAP | ZF_RX))) zstring_release(&STKP->vst);
  STKP--;
}

static void drop_n(int n)
{
  while (n--) drop();
}

static void swap(void)
{
  struct zvalue tmp = STKP[-1];
  STKP[-1] = STKP[0];
  STKP[0] = tmp;
}

// Set and return logical (0/1) val of top TT.stack value; flag value as NUM.
static int get_set_logical(void)
{
  struct zvalue *v = STKP;
  force_maybemap_to_scalar(v);
  int r = 0;
  if (IS_NUM(v)) r = !! v->num;
  else if (IS_STR(v)) r = (v->vst && v->vst->str[0]);
  zvalue_release_zstring(v);
  v->num = r;
  v->flags = ZF_NUM;
  return r;
}


static double to_num(struct zvalue *v)
{
  force_maybemap_to_scalar(v);
  if (v->flags & ZF_NUMSTR) zvalue_release_zstring(v);
  else if (!IS_NUM(v)) {
    v->num = 0.0;
    if (IS_STR(v) && v->vst) v->num = atof(v->vst->str);
    zvalue_release_zstring(v);
  }
  v->flags = ZF_NUM;
  return v->num;
}

static void set_num(struct zvalue *v, double n)
{
  zstring_release(&v->vst);
  v->num = n;
  v->flags = ZF_NUM;
}

static void incr_zvalue(struct zvalue *v)
{
  v->num = trunc(to_num(v)) + 1;
}

static void push_int_val(ptrdiff_t n)
{
  struct zvalue v = ZVINIT(ZF_NUM, n, 0);
  push_val(&v);
}

static struct zvalue *get_map_val(struct zvalue *v, struct zvalue *key)
{
  struct zmap_slot *x = zmap_find_or_insert_key(v->map, to_str(key)->vst);
  return &x->val;
}

static struct zvalue *setup_lvalue(int ref_stack_ptr, int parmbase, int *field_num)
{
  // ref_stack_ptr is number of slots down in stack the ref is
  // for +=, *=, etc
  // Stack is: ... scalar_ref value_to_op_by
  // or ... subscript_val map_ref value_to_op_by
  // or ... fieldref value_to_op_by
  // for =, ++, --
  // Stack is: ... scalar_ref
  // or ... subscript_val map_ref
  // or ... fieldnum fieldref
  int k;
  struct zvalue *ref, *v = 0; // init v to mute "may be uninit" warning
  *field_num = -1;
  ref = STKP - ref_stack_ptr;
  if (ref->flags & ZF_FIELDREF) return get_field_ref(*field_num = ref->num);
  k = ref->num >= 0 ? ref->num : parmbase - ref->num;
  if (k == NF) *field_num = THIS_MEANS_SET_NF;
  v = &STACK[k];
  if (ref->flags & ZF_REF) {
    force_maybemap_to_scalar(v);
  } else if (ref->flags & ZF_MAPREF) {
    force_maybemap_to_map(v);
    if (!IS_MAP(v)) FATAL("scalar in array context");
    v = get_map_val(v, STKP - ref_stack_ptr - 1);
    swap();
    drop();
  } else FATAL("assignment to bad lvalue");
  return v; // order FATAL() and return to mute warning
}

static struct zfile *new_file(char *fn, FILE *fp, char mode, char file_or_pipe,
                              char is_std_file)
{
  struct zfile *f = xzalloc(sizeof(struct zfile));
  *f = (struct zfile){TT.zfiles, xstrdup(fn), fp, mode, file_or_pipe,
                isatty(fileno(fp)), is_std_file, 0, 0, 0, 0, 0};
  return TT.zfiles = f;
}

static int fflush_all(void)
{
  int ret = 0;
  for (struct zfile *p = TT.zfiles; p; p = p->next)
    if (fflush(p->fp)) ret = -1;
  return ret;
}

static int fflush_file(int nargs)
{
  if (!nargs) return fflush_all();

  to_str(STKP);   // filename at top of TT.stack
  // Null string means flush all
  if (!STKP[0].vst->str[0]) return fflush_all();

  // is it open in file table?
  for (struct zfile *p = TT.zfiles; p; p = p->next)
    if (!strcmp(STKP[0].vst->str, p->fn))
      if (!fflush(p->fp)) return 0;
  return -1;    // error, or file not found in table
}
static int close_file(char *fn)
{
  // !fn (null ptr) means close all (exc. stdin/stdout/stderr)
  int r = 0;
  struct zfile *np, **pp = &TT.zfiles;
  for (struct zfile *p = TT.zfiles; p; p = np) {
    np = p->next;   // save in case unlinking file (invalidates p->next)
    // Don't close std files -- wrecks print/printf (can be fixed though TODO)
    if ((!p->is_std_file) && (!fn || !strcmp(fn, p->fn))) {
      xfree(p->buf);
      xfree(p->fn);
      r = (p->fp) ? (p->file_or_pipe ? fclose : pclose)(p->fp) : -1;
      *pp = p->next;
      xfree(p);
      if (fn) return r;
    } else pp = &p->next; // only if not unlinking zfile
  }
  return -1;  // file not in table, or closed all files
}

static struct zfile badfile_obj, *badfile = &badfile_obj;

// FIXME TODO check if file/pipe/mode matches what's in the table already.
// Apparently gawk/mawk/nawk are OK with different mode, but just use the file
// in whatever mode it's already in; i.e. > after >> still appends.
static struct zfile *setup_file(char file_or_pipe, char *mode)
{
  to_str(STKP);   // filename at top of TT.stack
  char *fn = STKP[0].vst->str;
  // is it already open in file table?
  for (struct zfile *p = TT.zfiles; p; p = p->next)
    if (!strcmp(fn, p->fn)) {
      drop();
      return p;   // open; return it
    }
  FILE *fp = (file_or_pipe ? fopen : popen)(fn, mode);
  if (fp) {
    struct zfile *p = new_file(fn, fp, *mode, file_or_pipe, 0);
    drop();
    return p;
  }
  if (*mode != 'r') FFATAL("cannot open '%s'\n", fn);
  drop();
  return badfile;
}

// TODO FIXME should be a function?
#define stkn(n) ((int)(TT.stackp - (n) - (struct zvalue *)TT.stack.base))

static int getcnt(int k)
{
  if (k >= stkn(0)) FATAL("too few args for printf\n");
  return (int)to_num(&STACK[k]);
}

static int fsprintf(FILE *ignored, const char *fmt, ...)
{
  (void)ignored;
  va_list args, args2;
  va_start(args, fmt);
  va_copy(args2, args);
  int len = vsnprintf(0, 0, fmt, args); // size needed
  va_end(args);
  if (len < 0) FATAL("Bad sprintf format");
  // Unfortunately we have to mess with zstring internals here.
  if (TT.rgl.zspr->size + len + 1 > TT.rgl.zspr->capacity) {
      // This should always work b/c capacity > size
      unsigned cap = 2 * TT.rgl.zspr->capacity + len;
      TT.rgl.zspr = xrealloc(TT.rgl.zspr, sizeof(*TT.rgl.zspr) + cap);
      TT.rgl.zspr->capacity = cap;
    }
  vsnprintf(TT.rgl.zspr->str + TT.rgl.zspr->size, len+1, fmt, args2);
  TT.rgl.zspr->size += len;
  TT.rgl.zspr->str[TT.rgl.zspr->size] = 0;
  va_end(args2);
  return 0;
}

static void varprint(int(*fpvar)(FILE *, const char *, ...), FILE *outfp, int nargs)
{
  int k, nn, nnc, fmtc, holdc, cnt1 = 0, cnt2 = 0;
  char *s = 0;  // to shut up spurious warning
  regoff_t offs = -1, e = -1;
  char *pfmt, *fmt = to_str(STKP-nargs+1)->vst->str;
  k = stkn(nargs - 2);
  while (*fmt) {
    double n = 0;
    nn = strcspn(fmt, "%");
    if (nn) {
      holdc = fmt[nn];
      fmt[nn] = 0;
      fpvar(outfp, "%s", fmt);
      fmt[nn] = holdc;
    }
    fmt += nn;
    if (!*(pfmt = fmt)) break;
    nnc = strcspn(fmt+1, "aAdiouxXfFeEgGcs%");
    fmtc = fmt[nnc+1];
    if (!fmtc) FFATAL("bad printf format '%s'", fmt);
    holdc = fmt[nnc+2];
    fmt[nnc+2] = 0;
    if (rx_find(&TT.rx_printf_fmt, fmt, &offs, &e, 0))
      FFATAL("bad printf format <%s>\n", fmt);
    int nargsneeded = 1;
    for (char *p = strchr(fmt, '*'); p; p = strchr(p+1, '*'))
      nargsneeded++;
    nargsneeded -= fmtc == '%';

    switch (nargsneeded) {
      case 0:
        fpvar(outfp, fmt);
        break;
      case 3:
        cnt1 = getcnt(k++);
        ATTR_FALLTHROUGH_INTENDED;
      case 2:
        cnt2 = getcnt(k++);
        ATTR_FALLTHROUGH_INTENDED;
      case 1:
        if (k > stkn(0)) FATAL("too few args for printf\n");
        if (fmtc == 's') {
          s = to_str(&STACK[k++])->vst->str;
        } else if (fmtc == 'c' && !IS_NUM(&STACK[k])) {
          unsigned wch;
          struct zvalue *z = &STACK[k++];
          if (z->vst && z->vst->str[0])
            n = utf8towc(&wch, z->vst->str, z->vst->size) < 1 ? 0xfffd : wch;
        } else {
          n = to_num(&STACK[k++]);
        }
        if (strchr("cdiouxX", fmtc)) {
          pfmt = strcpy(TT.pbuf, fmt);
          if (pfmt[nnc] != 'l') {
            strcpy(pfmt+nnc+1, "l_");
            pfmt[nnc+2] = fmtc;
          }
        }
        if (fmtc == 'c' && n > 0x10ffff) n = 0xfffd;  // musl won't take larger "wchar"
        switch (nargsneeded) {
          case 1:
            if (fmtc == 's') fpvar(outfp, pfmt, s);
            else if (fmtc == 'c') fpvar(outfp, pfmt, (wint_t)n);
            else if (strchr("di", fmtc)) fpvar(outfp, pfmt, (long)n);
            else if (strchr("ouxX", fmtc)) fpvar(outfp, pfmt, (unsigned long)n);
            else fpvar(outfp, pfmt, n);
            break;
          case 2:
            if (fmtc == 's') fpvar(outfp, pfmt, cnt2, s);
            else if (fmtc == 'c') fpvar(outfp, pfmt, cnt2, (wint_t)n);
            else if (strchr("di", fmtc)) fpvar(outfp, pfmt, cnt2, (long)n);
            else if (strchr("ouxX", fmtc)) fpvar(outfp, pfmt, cnt2, (unsigned long)n);
            else fpvar(outfp, pfmt, cnt2, n);
            break;
          case 3:
            if (fmtc == 's') fpvar(outfp, pfmt, cnt1, cnt2, s);
            else if (fmtc == 'c') fpvar(outfp, pfmt, cnt1, cnt2, (wint_t)n);
            else if (strchr("di", fmtc)) fpvar(outfp, pfmt, cnt1, cnt2, (long)n);
            else if (strchr("ouxX", fmtc)) fpvar(outfp, pfmt, cnt1, cnt2, (unsigned long)n);
            else fpvar(outfp, pfmt, cnt1, cnt2, n);
            break;
        }
        break;
      default:
        FATAL("bad printf format\n");
    }
    fmt += nnc + 2;
    *fmt = holdc;
  }
}

static int is_ok_varname(char *v)
{
  char *ok = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_";
  if (!*v) return 0;
  for (int i = 0; v[i]; i++)
    if (i ? !strchr(ok, v[i]) : !strchr(ok + 10, v[i])) return 0;
  return 1;
}

// FIXME TODO return value never used. What if assign to var not in globals?
static int assign_global(char *var, char *value)
{
  if (!is_ok_varname(var)) FFATAL("Invalid variable name '%s'\n", var);
  int globals_ent = find_global(var);
  if (globals_ent) {
    struct zvalue *v = &STACK[globals_ent];
    if (IS_MAP(v)) error_exit("-v assignment to array");  // Maybe not needed?

// The compile phase may insert a var in global table with flag of zero.  Then
// init_globals() will assign a ZF_MAYBEMAP flag to it. If it is then assigned
// via -v option or by assignment_arg() it will here be assigned a string value.
// So first, remove all map data to prevent memory leak. BUG FIX // 2024-02-13.
    if (v->flags & ZF_ANYMAP) {
      zmap_delete_map_incl_slotdata(v->map);
      xfree(v->map);
      v->map = 0;
      v->flags &= ~ZF_ANYMAP;
    }

    zvalue_release_zstring(v);
    value = xstrdup(value);
    *v = new_str_val(escape_str(value, 0));
    xfree(value);
    check_numeric_string(v);
    return 1;
  }
  return 0;
}

// If valid assignment arg, assign the global and return 1;
// otherwise return 0.
// TODO FIXME This does not check the format of the variable per posix.
// Needs to start w/ _A-Za-z then _A-Za-z0-9
// If not valid assignment form, then nextfilearg needs to treat as filename.
static int assignment_arg(char *arg)
{
  char *val = strchr(arg, '=');
  if (val) {
    *val++ = 0;
    if (!is_ok_varname(arg)) {
      *--val = '=';
      return 0;
    }
    assign_global(arg, val);
    *--val = '=';
    return 1;
  } else return 0;
}

static char *nextfilearg(void)
{
  char *arg;
  do {
    if (++TT.rgl.narg >= (int)to_num(&STACK[ARGC])) return 0;
    struct zvalue *v = &STACK[ARGV];
    struct zvalue zkey = ZVINIT(ZF_STR, 0,
        num_to_zstring(TT.rgl.narg, to_str(&STACK[CONVFMT])->vst->str));
    arg = "";
    if (zmap_find(v->map, zkey.vst)) {
      zvalue_copy(&TT.rgl.cur_arg, to_str(get_map_val(v, &zkey)));
      arg = TT.rgl.cur_arg.vst->str;
    }
    zvalue_release_zstring(&zkey);
  } while (!*arg || assignment_arg(arg));
  TT.rgl.nfiles++;
  return arg;
}

static int next_fp(void)
{
  char *fn = nextfilearg();
  if (TT.cfile->fp && TT.cfile->fp != stdin) fclose(TT.cfile->fp);
  if ((!fn && !TT.rgl.nfiles && TT.cfile->fp != stdin) || (fn && !strcmp(fn, "-"))) {
    xfree(TT.cfile->buf);
    *TT.cfile = (struct zfile){0};
    TT.cfile->fp = stdin;
    TT.cfile->fn = "-";
    zvalue_release_zstring(&STACK[FILENAME]);
    STACK[FILENAME].vst = new_zstring("-", 1);
  } else if (fn) {
    xfree(TT.cfile->buf);
    *TT.cfile = (struct zfile){0};
    if (!(TT.cfile->fp = fopen(fn, "r"))) FFATAL("can't open %s\n", fn);
    TT.cfile->fn = fn;
    zvalue_copy(&STACK[FILENAME], &TT.rgl.cur_arg);
  } else {
    TT.rgl.eof = 1;
    return 0;
  }
  set_num(&STACK[FNR], 0);
  TT.cfile->is_tty = isatty(fileno(TT.cfile->fp));
  return 1;
}

static int rx_find_rs(regex_t *rx, char *s, long len,
                      regoff_t *start, regoff_t *end, int one_byte_rs)
{
  regmatch_t matches[1];
  if (one_byte_rs) {
    char *p = memchr(s, one_byte_rs, len);
    if (!p) return REG_NOMATCH;
    *start = p - s;
    *end = *start + 1;
  } else {
    int r = regexec0(rx, s, len, 1, matches, 0);
    if (r == REG_NOMATCH) return r;
    if (r) FATAL("regexec error");  // TODO ? use regerr() to meaningful msg
    *start = matches[0].rm_so;
    *end = matches[0].rm_eo;
  }
  return 0;
}

// get a record; return length, or -1 at EOF
// Does work for getrec_f() for regular RS or multiline
static ssize_t getr(struct zfile *zfp, int rs_mode)
{
  // zfp->buf (initially null) points to record buffer
  // zfp->buflen -- size of allocated buf
  // TT.rgl.recptr -- points to where record is being / has been read into
  // zfp->ro -- offset in buf to record data
  // zfp->lim -- offset to 1+last byte read in buffer
  // rs_mode nonzero iff multiline mode; reused for one-byte RS

  regex_t rsrx; // FIXME Need to cache and avoid rx compile on every record?
  long ret = -1;
  int r = -REG_NOMATCH;   // r cannot have this value after rx_findx() below
  regoff_t so = 0, eo = 0;
  size_t m = 0, n = 0;

  xregcomp(&rsrx, rs_mode ? "\n\n+" : fmt_one_char_fs(STACK[RS].vst->str),
      REG_EXTENDED);
  rs_mode = strlen(STACK[RS].vst->str) == 1 ? STACK[RS].vst->str[0] : 0;
  for ( ;; ) {
    if (zfp->ro == zfp->lim && zfp->eof) break; // EOF & last record; return -1

    // Allocate initial buffer, and expand iff buffer holds one
    //   possibly (probably) incomplete record.
    if (zfp->ro == 0 && zfp->lim == zfp->buflen)
      zfp->buf = xrealloc(zfp->buf,
          (zfp->buflen = maxof(512, zfp->buflen * 2)) + 1);

    if ((m = zfp->buflen - zfp->lim) && !zfp->eof) {
      // Read iff space left in buffer
      if (zfp->is_tty) m = 1;
      n = fread(zfp->buf + zfp->lim, 1, m, zfp->fp);
      if (n < m) {
        if (ferror(zfp->fp)) FFATAL("i/o error %d on %s!", errno, zfp->fn);
        zfp->eof = 1;
        if (!n && r == -REG_NOMATCH) break; // catch empty file here
      }
      zfp->lim += n;
      zfp->buf[zfp->lim] = 0;
    }
    TT.rgl.recptr = zfp->buf + zfp->ro;
    r = rx_find_rs(&rsrx, TT.rgl.recptr, zfp->lim - zfp->ro, &so, &eo, rs_mode);
    if (!r && so == eo) r = 1;  // RS was empty, so fake not found

    if (!zfp->eof && (r
          || (zfp->lim - (zfp->ro + eo)) < zfp->buflen / 4) && !zfp->is_tty) {
      // RS not found, or found near lim. Slide up and try to get more data
      // If recptr at start of buf and RS not found then expand buffer
      memmove(zfp->buf, TT.rgl.recptr, zfp->lim - zfp->ro);
      zfp->lim -= zfp->ro;
      zfp->ro = 0;
      continue;
    }
    ret = so;   // If RS found, then 'so' is rec length
    if (zfp->eof) {
      if (r) {  // EOF and RS not found; rec is all data left in buf
        ret = zfp->lim - zfp->ro;
        zfp->ro = zfp->lim; // set ro for -1 return on next call
      } else zfp->ro += eo; // RS found; advance ro
    } else zfp->ro += eo; // Here only if RS found not near lim

    if (!r || !zfp->is_tty) {
      // If is_tty then RS found; reset buffer pointers;
      // is_tty uses one rec per buffer load
      if (zfp->is_tty) zfp->ro = zfp->lim = 0;
      break;
    } // RS not found AND is_tty; loop to keep reading
  }
  regfree(&rsrx);
  return ret;
}

// get a record; return length, or -1 at EOF
static ssize_t getrec_f(struct zfile *zfp)
{
  int k;
  if (ENSURE_STR(&STACK[RS])->vst->str[0]) return getr(zfp, 0);
  // RS == "" so multiline read
  // Passing 1 to getr() forces multiline mode, which uses regex "\n\n+" to
  // split on sequences of 2 or more newlines. But that's not the same as
  // multiline mode, which never returns empty records or records with leading
  // or trailing newlines, which can occur with RS="\n\n+". So here we loop and
  // strip leading/trailing newlines and discard empty lines. See gawk manual,
  // "4.9 Multiple-Line Records" for info on this difference.
  do {
    k = getr(zfp, 1);
    if (k < 0) break;
    while (k && TT.rgl.recptr[k-1] == '\n') k--;
    while (k && TT.rgl.recptr[0] == '\n') k--, TT.rgl.recptr++;
  } while (!k);
  return k;
}

static ssize_t getrec(void)
{
  ssize_t k;
  if (TT.rgl.eof) return -1;
  if (!TT.cfile->fp) next_fp();
  do {
    if ((k = getrec_f(TT.cfile)) >= 0) return k;
  } while (next_fp());
  return -1;
}

static ssize_t getrec_f0_f(struct zfile *zfp)
{
  ssize_t k = getrec_f(zfp);
  if (k >= 0) {
    copy_to_field0(TT.rgl.recptr, k);
  }
  return k;
}

static ssize_t getrec_f0(void)
{
  ssize_t k = getrec();
  if (k >= 0) {
    copy_to_field0(TT.rgl.recptr, k);
    incr_zvalue(&STACK[NR]);
    incr_zvalue(&STACK[FNR]);
  }
  return k;
}

// source is tkeof (no pipe/file), tklt (file), or tkpipe (pipe)
// fp is file or pipe (is NULL if file/pipe could not be opened)
// FIXME TODO should -1 return be replaced by test at caller?
// v is NULL or an lvalue ref
static int awk_getline(int source, struct zfile *zfp, struct zvalue *v)
{
  ssize_t k;
  int is_stream = source != tkeof;
  if (is_stream && !zfp->fp) return -1;
  if (v) {
    if ((k = is_stream ? getrec_f(zfp) : getrec()) < 0) return 0;
    zstring_release(&v->vst);
    v->vst = new_zstring(TT.rgl.recptr, k);
    v->flags = ZF_STR;
    check_numeric_string(v);    // bug fix 20240514
    if (!is_stream) {
      incr_zvalue(&STACK[NR]);
      incr_zvalue(&STACK[FNR]);
    }
  } else k = is_stream ? getrec_f0_f(zfp) : getrec_f0();
  return k < 0 ? 0 : 1;
}

// Define GAWK_SUB to get the same behavior with sub()/gsub() replacement text
// as with gawk, goawk, and recent bwk awk (nawk) versions. Undefine GAWK_SUB
// to get the simpler POSIX behavior, but I think most users will prefer the
// gawk behavior. See the gawk (GNU Awk) manual,
// sec. 9.1.4.1 // More about '\' and '&' with sub(), gsub(), and gensub()
// for details on the differences.
//
#undef GAWK_SUB
#define GAWK_SUB

// sub(ere, repl[, in]) Substitute the string repl in place of the
// first instance of the extended regular expression ERE in string 'in'
// and return the number of substitutions.  An <ampersand> ( '&' )
// appearing in the string repl shall be replaced by the string from in
// that matches the ERE. (partial spec... there's more)
static void gsub(int opcode, int nargs, int parmbase)
{ (void)nargs;
  int field_num = -1;
  // compile ensures 3 args
  struct zvalue *v = setup_lvalue(0, parmbase, &field_num);
  struct zvalue *ere = STKP-2;
  struct zvalue *repl = STKP-1;
  regex_t rx, *rxp = &rx;
  rx_zvalue_compile(&rxp, ere);
  to_str(repl);
  to_str(v);

#define SLEN(zvalp) ((zvalp)->vst->size)
  char *p, *rp0 = repl->vst->str, *rp = rp0, *s = v->vst->str;
  int namps = 0, nhits = 0, is_sub = (opcode == tksub), eflags = 0;
  regoff_t so = -1, eo;
  // Count ampersands in repl string; may be overcount due to \& escapes.
  for (rp = rp0; *rp; rp++) namps += *rp == '&';
  p = s;
  regoff_t need = SLEN(v) + 1;  // capacity needed for result string
  // A pass just to determine needed destination (result) string size.
  while(!rx_find(rxp, p, &so, &eo, eflags)) {
    need += SLEN(repl) + (eo - so) * (namps - 1);
    if (!*p) break;
    p += eo ? eo : 1; // ensure progress if empty hit at start
    if (is_sub) break;
    eflags |= REG_NOTBOL;
  }

  if (so >= 0) {  // at least one hit
    struct zstring *z = xzalloc(sizeof(*z) + need);
    z->capacity = need;

    char *e = z->str; // result destination pointer
    p = s;
    eflags = 0;
    char *ep0 = p, *sp, *ep;
    while(!rx_find(rxp, p, &so, &eo, eflags)) {
      sp = p + so;
      ep = p + eo;
      memmove(e, ep0, sp - ep0);  // copy unchanged part
      e += sp - ep0;
      // Skip match if not at start and just after prev match and this is empty
      if (p == s || sp - ep0 || eo - so) {
        nhits++;
        for (rp = rp0; *rp; rp++) { // copy replacement
          if (*rp == '&') {
            memmove(e, sp, eo - so);  //copy match
            e += eo - so;
          } else if (*rp == '\\') {
            if (rp[1] == '&') *e++ = *++rp;
            else if (rp[1] != '\\') *e++ = *rp;
            else {
#ifdef GAWK_SUB
              if (rp[2] == '\\' && rp[3] == '&') {
                rp += 2;
                *e++ = *rp;
              } else if (rp[2] != '&') *e++ = '\\';
#endif
              *e++ = *++rp;
            }
          } else *e++ = *rp;
        }
      }
      ep0 = ep;
      if (!*p) break;
      p += eo ? eo : 1; // ensure progress if empty hit at start
      if (is_sub) break;
      eflags |= REG_NOTBOL;
    }
    // copy remaining subject string
    memmove(e, ep0, s + SLEN(v) - ep0);
    e += s + SLEN(v) - ep0;
    *e = 0;
    z->size = e - z->str;
    zstring_release(&v->vst);
    v->vst = z;
  }
  rx_zvalue_free(rxp, ere);
  if (!IS_RX(STKP-2)) zstring_release(&STKP[-2].vst);
  drop_n(3);
  push_int_val(nhits);
  if (field_num >= 0) fixup_fields(field_num);
}

// Initially set stackp_needmore at MIN_STACK_LEFT before limit.
// When stackp > stackp_needmore, then expand and reset stackp_needmore
static void add_stack(struct zvalue **stackp_needmore)
{
  int k = stkn(0);  // stack elements in use
  zlist_expand(&TT.stack);
  STKP = (struct zvalue *)TT.stack.base + k;
  *stackp_needmore = (struct zvalue *)TT.stack.limit - MIN_STACK_LEFT;
}

#define CLAMP(x, lo, hi) ((x) < (lo) ? (lo) : (x) > (hi) ? (hi) : (x))

// Main loop of interpreter. Run this once for all BEGIN rules (which
// have had their instructions chained in compile), all END rules (also
// chained in compile), and once for each record of the data file(s).
static int interpx(int start, int *status)
{
  int *ip = &ZCODE[start];
  int opcode, op2, k, r, nargs, nsubscrs, range_num, parmbase = 0;
  int field_num;
  double nleft, nright, d;
  double (*mathfunc[])(double) = {cos, sin, exp, log, sqrt, trunc};
  struct zvalue *v, vv,
        *stackp_needmore = (struct zvalue*)TT.stack.limit - MIN_STACK_LEFT;
  while ((opcode = *ip++)) {

    switch (opcode) {
      case opquit:
        return opquit;

      case tknot:
        (STKP)->num = ! get_set_logical();
        break;

      case opnotnot:
        get_set_logical();
        break;

      case opnegate:
        STKP->num = -to_num(STKP);
        break;

      case tkpow:         // FALLTHROUGH intentional here
      case tkmul:         // FALLTHROUGH intentional here
      case tkdiv:         // FALLTHROUGH intentional here
      case tkmod:         // FALLTHROUGH intentional here
      case tkplus:        // FALLTHROUGH intentional here
      case tkminus:
        nleft = to_num(STKP-1);
        nright = to_num(STKP);
        switch (opcode) {
          case tkpow: nleft = pow(nleft, nright); break;
          case tkmul: nleft *= nright; break;
          case tkdiv: nleft /= nright; break;
          case tkmod: nleft = fmod(nleft, nright); break;
          case tkplus: nleft += nright; break;
          case tkminus: nleft -= nright; break;
        }
        drop();
        STKP->num = nleft;
        break;

      // FIXME REDO REDO ?
      case tkcat:
        to_str(STKP-1);
        to_str(STKP);
        STKP[-1].vst = zstring_extend(STKP[-1].vst, STKP[0].vst);
        drop();
        break;

        // Comparisons (with the '<', "<=", "!=", "==", '>', and ">="
        // operators) shall be made numerically:
        // * if both operands are numeric,
        // * if one is numeric and the other has a string value that is a
        //   numeric string,
        // * if both have string values that are numeric strings, or
        // * if one is numeric and the other has the uninitialized value.
        //
        // Otherwise, operands shall be converted to strings as required and a
        // string comparison shall be made as follows:
        // * For the "!=" and "==" operators, the strings shall be compared to
        //   check if they are identical (not to check if they collate equally).
        // * For the other operators, the strings shall be compared using the
        //   locale-specific collation sequence.
        //
        // The value of the comparison expression shall be 1 if the relation is
        // true, or 0 if the relation is false.
      case tklt:          // FALLTHROUGH intentional here
      case tkle:          // FALLTHROUGH intentional here
      case tkne:          // FALLTHROUGH intentional here
      case tkeq:          // FALLTHROUGH intentional here
      case tkgt:          // FALLTHROUGH intentional here
      case tkge:
        ; int cmp = 31416;

        if (  (IS_NUM(&STKP[-1]) &&
              (STKP[0].flags & (ZF_NUM | ZF_NUMSTR) || !STKP[0].flags)) ||
              (IS_NUM(&STKP[0]) &&
              (STKP[-1].flags & (ZF_NUM | ZF_NUMSTR) || !STKP[-1].flags))) {
          switch (opcode) {
            case tklt: cmp = STKP[-1].num < STKP[0].num; break;
            case tkle: cmp = STKP[-1].num <= STKP[0].num; break;
            case tkne: cmp = STKP[-1].num != STKP[0].num; break;
            case tkeq: cmp = STKP[-1].num == STKP[0].num; break;
            case tkgt: cmp = STKP[-1].num > STKP[0].num; break;
            case tkge: cmp = STKP[-1].num >= STKP[0].num; break;
          }
        } else {
          cmp = strcmp(to_str(STKP-1)->vst->str, to_str(STKP)->vst->str);
          switch (opcode) {
            case tklt: cmp = cmp < 0; break;
            case tkle: cmp = cmp <= 0; break;
            case tkne: cmp = cmp != 0; break;
            case tkeq: cmp = cmp == 0; break;
            case tkgt: cmp = cmp > 0; break;
            case tkge: cmp = cmp >= 0; break;
          }
        }
        drop();
        drop();
        push_int_val(cmp);
        break;

      case opmatchrec:
        op2 = *ip++;
        int mret = match(&FIELD[0], &LITERAL[op2]);
        push_int_val(!mret);
        break;

      case tkmatchop:
      case tknotmatch:
        mret = match(STKP-1, STKP); // mret == 0 if match
        drop();
        drop();
        push_int_val(!mret == (opcode == tkmatchop));
        break;

      case tkpowasgn:     // FALLTHROUGH intentional here
      case tkmodasgn:     // FALLTHROUGH intentional here
      case tkmulasgn:     // FALLTHROUGH intentional here
      case tkdivasgn:     // FALLTHROUGH intentional here
      case tkaddasgn:     // FALLTHROUGH intentional here
      case tksubasgn:
        // Stack is: ... scalar_ref value_to_op_by
        // or ... subscript_val map_ref value_to_op_by
        // or ... fieldref value_to_op_by
        v = setup_lvalue(1, parmbase, &field_num);
        to_num(v);
        to_num(STKP);
        switch (opcode) {
          case tkpowasgn:
            // TODO
            v->num = pow(v->num, STKP->num);
            break;
          case tkmodasgn:
            // TODO
            v->num = fmod(v->num, STKP->num);
            break;
          case tkmulasgn:
            v->num *= STKP->num;
            break;
          case tkdivasgn:
            v->num /= STKP->num;
            break;
          case tkaddasgn:
            v->num += STKP->num;
            break;
          case tksubasgn:
            v->num -= STKP->num;
            break;
        }

        drop_n(2);
        v->flags = ZF_NUM;
        push_val(v);
        if (field_num >= 0) fixup_fields(field_num);
        break;

      case tkasgn:
        // Stack is: ... scalar_ref value_to_assign
        // or ... subscript_val map_ref value_to_assign
        // or ... fieldref value_to_assign
        v = setup_lvalue(1, parmbase, &field_num);
        force_maybemap_to_scalar(STKP);
        zvalue_copy(v, STKP);
        swap();
        drop();
        if (field_num >= 0) fixup_fields(field_num);
        break;

      case tkincr:        // FALLTHROUGH intentional here
      case tkdecr:        // FALLTHROUGH intentional here
      case oppreincr:     // FALLTHROUGH intentional here
      case oppredecr:
        // Stack is: ... scalar_ref
        // or ... subscript_val map_ref
        // or ... fieldnum fieldref
        v = setup_lvalue(0, parmbase, &field_num);
        to_num(v);
        switch (opcode) {
          case tkincr: case tkdecr:
            // Must be done in this order because push_val(v) may move v,
            // invalidating the pointer.
            v->num += (opcode == tkincr) ? 1 : -1;
            push_val(v);
            // Now reverse the incr/decr on the top TT.stack val.
            STKP->num -= (opcode == tkincr) ? 1 : -1;
            break;
          case oppreincr: case oppredecr:
            v->num += (opcode == oppreincr) ? 1 : -1;
            push_val(v);
            break;
        }
        swap();
        drop();
        if (field_num >= 0) fixup_fields(field_num);
        break;

      case tknumber:      // FALLTHROUGH intentional here
      case tkstring:      // FALLTHROUGH intentional here
      case tkregex:
        push_val(&LITERAL[*ip++]);
        break;

      case tkprint:
      case tkprintf:
        nargs = *ip++;
        int outmode = *ip++;
        struct zfile *outfp = TT.zstdout;
        switch (outmode) {
          case tkgt: outfp = setup_file(1, "w"); break;     // file
          case tkappend: outfp = setup_file(1, "a"); break; // file
          case tkpipe: outfp = setup_file(0, "w"); break;   // pipe
          default: nargs++; break;
        }
        nargs--;
        if (opcode == tkprintf) {
          varprint(fprintf, outfp->fp, nargs);
          drop_n(nargs);
          break;
        }
        if (!nargs) {
          fprintf(outfp->fp, "%s", to_str(&FIELD[0])->vst->str);
        } else {
          struct zvalue tempv = uninit_zvalue;
          zvalue_copy(&tempv, &STACK[OFS]);
          to_str(&tempv);
          for (int k = 0; k < nargs; k++) {
            if (k) fprintf(outfp->fp, "%s", tempv.vst->str);
            int sp = stkn(nargs - 1 - k);
            ////// FIXME refcnt -- prob. don't need to copy from TT.stack?
            v = &STACK[sp];
            to_str_fmt(v, OFMT);
            struct zstring *zs = v->vst;
            fprintf(outfp->fp, "%s", zs ? zs->str : "");
          }
          zvalue_release_zstring(&tempv);
          drop_n(nargs);
        }
        fputs(ENSURE_STR(&STACK[ORS])->vst->str, outfp->fp);
        break;

      case opdrop:
        drop();
        break;

      case opdrop_n:
        drop_n(*ip++);
        break;

        // Stack frame layout relative to parmbase:
#define RETURN_VALUE    -4
#define RETURN_ADDR     -3
#define PREV_PARMBASE   -2
#define ARG_CNT         -1
#define FUNCTION_NUM    0
        // Actual args follow, starting at parmbase + 1
      case tkfunction:    // function definition
        op2 = *ip++;    // func table num
        struct functab_slot *pfdef = &FUNC_DEF[op2];
        struct zlist *loctab = &pfdef->function_locals;
        int nparms = zlist_len(loctab)-1;

        nargs = popnumval();
        int newparmbase = stkn(nargs);
        STACK[newparmbase + PREV_PARMBASE].num = parmbase;
        parmbase = newparmbase;
        for ( ;nargs > nparms; nargs--)
          drop();
        for ( ;nargs < nparms; nargs++) {
          // Push additional "args" that were not passed by the caller, to
          // match the formal parameters (parms) defined in the function
          // definition. In the local var table we may have the type as scalar
          // or map if it is used as such within the function. In that case we
          // init the pushed arg from the type of the locals table.
          // But if a var appears only as a bare arg in a function call it will
          // not be typed in the locals table. In that case we can only say it
          // "may be" a map, but we have to assume the possibility and attach a
          // map to the var. When/if the var is used as a map or scalar in the
          // called function it will be converted to a map or scalar as
          // required.
          // See force_maybemap_to_scalar().
          struct symtab_slot *q = &((struct symtab_slot *)loctab->base)[nargs+1];
          vv = (struct zvalue)ZVINIT(q->flags, 0, 0);
          if (vv.flags == 0) {
            zvalue_map_init(&vv);
            vv.flags = ZF_MAYBEMAP;
          } else if (IS_MAP(&vv)) {
            zvalue_map_init(&vv);
          } else {
            vv.flags = 0;
          }
          push_val(&vv);
        }
        break;

      case tkreturn:
        nparms = *ip++;
        nargs = STACK[parmbase+ARG_CNT].num;
        force_maybemap_to_scalar(STKP); // Unneeded?
        zvalue_copy(&STACK[parmbase+RETURN_VALUE], STKP);
        drop();
        // Remove the local args (not supplied by caller) from TT.stack, check to
        // release any map data created.
        while (stkn(0) > parmbase + nargs) {
          if ((STKP)->flags & ZF_ANYMAP) {
            zmap_delete_map_incl_slotdata((STKP)->map);
            xfree((STKP)->map);
          }
          drop();
        }
        while (stkn(0) > parmbase + RETURN_VALUE)
          drop();
        ip = &ZCODE[(int)STACK[parmbase+RETURN_ADDR].num];
        parmbase = STACK[parmbase+PREV_PARMBASE].num;
        break;

      case opprepcall:    // function call prep
        if (STKP > stackp_needmore) add_stack(&stackp_needmore);
        push_int_val(0);      // return value placeholder
        push_int_val(0);      // return addr
        push_int_val(0);      // parmbase
        push_int_val(0);      // arg count
        push_int_val(*ip++);  // function tbl ref
        break;

      case tkfunc:        // function call
        nargs = *ip++;
        newparmbase = stkn(nargs);
        STACK[newparmbase+RETURN_ADDR].num = ip - &ZCODE[0];
        STACK[newparmbase+ARG_CNT].num = nargs;
        push_int_val(nargs);      // FIXME TODO pass this in a zregister?
        ip = &ZCODE[FUNC_DEF[(int)STACK[newparmbase+FUNCTION_NUM].num].zcode_addr];
        break;

      case tkrbracket:    // concat multiple map subscripts
        nsubscrs = *ip++;
        while (--nsubscrs) {
          swap();
          to_str(STKP);
          push_val(&STACK[SUBSEP]);
          to_str(STKP);
          STKP[-1].vst = zstring_extend(STKP[-1].vst, STKP->vst);
          drop();
          swap();
          to_str(STKP);
          STKP[-1].vst = zstring_extend(STKP[-1].vst, STKP->vst);
          drop();
        }
        break;

      case opmapdelete:
      case tkdelete:
        k = STKP->num;
        if (k < 0) k = parmbase - k;    // loc of var on TT.stack
        v = &STACK[k];
        force_maybemap_to_map(v);
        if (opcode == opmapdelete) {
          zmap_delete_map(v->map);
        } else {
          drop();
          zmap_delete(v->map, to_str(STKP)->vst);
        }
        drop();
        break;

      case opmap:
        op2 = *ip++;
        k = op2 < 0 ? parmbase - op2 : op2;
        v = &STACK[k];
        force_maybemap_to_map(v);
        if (!IS_MAP(v)) FATAL("scalar in array context");
        v = get_map_val(v, STKP);
        drop();     // drop subscript
        push_val(v);
        break;

      case tkin:
        if (!(STKP->flags & ZF_ANYMAP)) FATAL("scalar in array context");
        v = zmap_find(STKP->map, to_str(STKP-1)->vst);
        drop();
        drop();
        push_int_val(v ? 1 : 0);
        break;

      case opmapiternext:
        op2 = *ip++;
        v = STKP-1;
        force_maybemap_to_map(v);
        if (!IS_MAP(v)) FATAL("scalar in array context");
        struct zmap *m = v->map;   // Need for MAPSLOT macro
        int zlen = zlist_len(&m->slot);
        int kk = STKP->num + 1;
        while (kk < zlen && !(MAPSLOT[kk].key)) // skip deleted slots
          kk++;
        STKP->num = kk; // save index for next iteration
        if (kk < zlen) {
          struct zvalue *var = setup_lvalue(2, parmbase, &field_num);
          var->flags = ZF_STR;
          zstring_release(&var->vst);
          var->vst = MAPSLOT[kk].key;
          zstring_incr_refcnt(var->vst);
          ip += op2;
        }
        break;

      case tkvar:
        op2 = *ip++;
        k = op2 < 0 ? parmbase - op2 : op2;
        v = &STACK[k];
        push_val(v);
        break;

      case tkfield:
        // tkfield op has "dummy" 2nd word so that convert_push_to_reference(void)
        // can find either tkfield or tkvar at same place (ZCODE[TT.zcode_last-1]).
        ip++; // skip dummy "operand" instruction field
        push_field((int)(to_num(STKP)));

        swap();
        drop();
        break;

      case oppush:
        push_int_val(*ip++);
        break;

      case tkand:
        op2 = *ip++;
        if (get_set_logical()) drop();
        else ip += op2;
        break;

      case tkor:
        op2 = *ip++;
        if (!get_set_logical()) drop();
        else ip += op2;
        break;

      case tkwhile:
        (STKP)->num = ! get_set_logical();
        ATTR_FALLTHROUGH_INTENDED;
        // FALLTHROUGH to tkternif
      case tkif:
        // FALLTHROUGH to tkternif
      case tkternif:
        op2 = *ip++;
        int t = get_set_logical();  // FIXME only need to get, not set
        drop();
        if (!t) ip += op2;
        break;

      case tkelse:        // FALLTHROUGH intentional here
      case tkternelse:    // FALLTHROUGH intentional here
      case tkbreak:       // FALLTHROUGH intentional here
      case tkcontinue:    // FALLTHROUGH intentional here
      case opjump:
        op2 = *ip++;
        ip += op2;
        break;

      case opvarref:
        op2 = *ip++;
        vv = (struct zvalue)ZVINIT(ZF_REF, op2, 0);
        push_val(&vv);
        break;

      case opmapref:
        op2 = *ip++;
        vv = (struct zvalue)ZVINIT(ZF_MAPREF, op2, 0);
        push_val(&vv);
        break;

      case opfldref:
        to_num(STKP);
        (STKP)->flags |= ZF_FIELDREF;
        ip++; // skip dummy "operand" instruction field
        break;

      case opprintrec:
        puts(to_str(&FIELD[0])->vst->str);
        break;

      case oprange1:
        range_num = *ip++;
        op2 = *ip++;
        if (TT.range_sw[range_num]) ip += op2;
        break;

      case oprange2:
        range_num = *ip++;
        op2 = *ip++;
        t = get_set_logical();  // FIXME only need to get, not set
        drop();
        if (t) TT.range_sw[range_num] = 1;
        else ip += op2;
        break;

      case oprange3:
        range_num = *ip++;
        t = get_set_logical();  // FIXME only need to get, not set
        drop();
        if (t) TT.range_sw[range_num] = 0;
        break;

      case tkexit:
        r = popnumval();
        if (r != NO_EXIT_STATUS) *status = (int)r & 255;
        // TODO FIXME do we need NO_EXIT_STATUS at all? Just use 0?
        ATTR_FALLTHROUGH_INTENDED;
      case tknext:
      case tknextfile:
        return opcode;

      case tkgetline:
        nargs = *ip++;
        int source = *ip++;
        // TT.stack is:
        // if tkgetline 0 tkeof:   (nothing stacked; plain getline)
        // if tkgetline 1 tkeof:   (lvalue)
        // if tkgetline 1 tklt:    (filename_string)
        // if tkgetline 2 tklt:    (lvalue) (filename_string)
        // if tkgetline 1 tkpipe:  (pipe_command_string)
        // if tkgetline 2 tkpipe:  (pipe_command_string) (lvalue)
        // effect is to set:
        // if tkgetline 0 tkeof:   $0 NF NR FNR
        // if tkgetline 1 tkeof:   var NR FNR
        // if tkgetline 1 tklt:    $0 NF
        // if tkgetline 2 tklt:    var
        // if tkgetline 1 tkpipe:  $0 NF
        // if tkgetline 2 tkpipe:  var
        // Ensure pipe cmd on top
        if (nargs == 2 && source == tkpipe) swap();
        struct zfile *zfp = 0;
        if (source == tklt || source == tkpipe) {
          zfp = setup_file(source == tklt, "r");
          nargs--;
        }
        // now cases are:
        // nargs source  TT.stack
        //  0 tkeof:   (nothing; plain getline) from current data file
        //  1 tkeof:   (lvalue)  from current data file
        //  0 tklt:    (nothing) from named file in 'stream'
        //  1 tklt:    (lvalue)  from  named file in 'stream'
        //  0 tkpipe:  (nothing) from piped command in 'stream'
        //  1 tkpipe:  (lvalue)  from piped command in 'stream'
        v = nargs ? setup_lvalue(0, parmbase, &field_num) : 0;
        if (v) drop();
        // source is tkeof (no pipe/file), tklt (file), or tkpipe (pipe)
        // stream is name of file or pipe
        // v is NULL or an lvalue ref
        if (zfp != badfile) push_int_val(awk_getline(source, zfp, v));
        else push_int_val(-1);

        // fake return value for now
        break;

        ////// builtin functions ///////

      case tksplit:
        nargs = *ip++;
        if (nargs == 2) push_val(&STACK[FS]);
        struct zstring *s = to_str(STKP-2)->vst;
        force_maybemap_to_map(STKP-1);
        struct zvalue *a = STKP-1;
        struct zvalue *fs = STKP;
        zmap_delete_map(a->map);
        k = split(s, a, fs);
        drop_n(3);
        push_int_val(k);
        break;

      case tkmatch:
        nargs = *ip++;
        if (!IS_RX(STKP)) to_str(STKP);
        regex_t rx_pat, *rxp = &rx_pat;
        rx_zvalue_compile(&rxp, STKP);
        regoff_t rso = 0, reo = 0;  // shut up warning (may be uninit)
        k = rx_find(rxp, to_str(STKP-1)->vst->str, &rso, &reo, 0);
        rx_zvalue_free(rxp, STKP);
        // Force these to num before setting.
        to_num(&STACK[RSTART]);
        to_num(&STACK[RLENGTH]);
        if (k) STACK[RSTART].num = 0, STACK[RLENGTH].num = -1;
        else {
          reo = utf8cnt(STKP[-1].vst->str, reo);
          rso = utf8cnt(STKP[-1].vst->str, rso);
          STACK[RSTART].num = rso + 1, STACK[RLENGTH].num = reo - rso;
        }
        drop();
        drop();
        push_int_val(k ? 0 : rso + 1);
        break;

      case tksub:
      case tkgsub:
        gsub(opcode, *ip++, parmbase);  // tksub/tkgsub, args
        break;

      case tksubstr:
        nargs = *ip++;
        struct zstring *zz = to_str(STKP - nargs + 1)->vst;
        int nchars = utf8cnt(zz->str, zz->size);  // number of utf8 codepoints
        // Offset of start of string (in chars not bytes); convert 1-based to 0-based
        ssize_t mm = CLAMP(trunc(to_num(STKP - nargs + 2)) - 1, 0, nchars);
        ssize_t nn = nchars - mm;   // max possible substring length (chars)
        if (nargs == 3) nn = CLAMP(trunc(to_num(STKP)), 0, nn);
        mm = bytesinutf8(zz->str, zz->size, mm);
        nn = bytesinutf8(zz->str + mm, zz->size - mm, nn);
        struct zstring *zzz = new_zstring(zz->str + mm, nn);
        zstring_release(&(STKP - nargs + 1)->vst);
        (STKP - nargs + 1)->vst = zzz;
        drop_n(nargs - 1);
        break;

      case tkindex:
        nargs = *ip++;
        char *s1 = to_str(STKP-1)->vst->str;
        char *s3 = strstr(s1, to_str(STKP)->vst->str);
        ptrdiff_t offs = s3 ? utf8cnt(s1, s3 - s1) + 1 : 0;
        drop();
        drop();
        push_int_val(offs);
        break;

      case tkband:
      case tkbor:
      case tkbxor:
      case tklshift:
      case tkrshift:
        ; size_t acc = to_num(STKP);
        nargs = *ip++;
        for (int i = 1; i < nargs; i++) switch (opcode) {
          case tkband: acc &= (size_t)to_num(STKP-i); break;
          case tkbor:  acc |= (size_t)to_num(STKP-i); break;
          case tkbxor: acc ^= (size_t)to_num(STKP-i); break;
          case tklshift: acc = (size_t)to_num(STKP-i) << acc; break;
          case tkrshift: acc = (size_t)to_num(STKP-i) >> acc; break;
        }
        drop_n(nargs);
        push_int_val(acc);
        break;

      case tktolower:
      case tktoupper:
        nargs = *ip++;
        struct zstring *z = to_str(STKP)->vst;
        unsigned zzlen = z->size + 4; // Allow for expansion
        zz = zstring_update(0, zzlen, "", 0);
        char *p = z->str, *e = z->str + z->size, *q = zz->str;
        // Similar logic to toybox strlower(), but fixed.
        while (p < e) {
          unsigned wch;
          int len = utf8towc(&wch, p, e-p);
          if (len < 1) {  // nul byte, error, or truncated code
            *q++ = *p++;
            continue;
          }
          p += len;
          wch = (opcode == tktolower ? towlower : towupper)(wch);
          len = wctoutf8(q, wch);
          q += len;
          // Need realloc here if overflow possible
          if ((len = q - zz->str) + 4 < (int)zzlen) continue;
          zz = zstring_update(zz, zzlen = len + 16, "", 0);
          q = zz->str + len;
        }
        *q = 0;
        zz->size = q - zz->str;
        zstring_release(&z);
        STKP->vst = zz;
        break;

      case tklength:
        nargs = *ip++;
        v = nargs ? STKP : &FIELD[0];
        force_maybemap_to_map(v);
        if (IS_MAP(v)) k = v->map->count - v->map->deleted;
        else {
          to_str(v);
          k = utf8cnt(v->vst->str, v->vst->size);
        }
        if (nargs) drop();
        push_int_val(k);
        break;

      case tksystem:
        nargs = *ip++;
        fflush(stdout);
        fflush(stderr);
        r = system(to_str(STKP)->vst->str);
#ifdef WEXITSTATUS
        // WEXITSTATUS is in sys/wait.h, but I'm not including that.
        // It seems to also be in stdlib.h in gcc and musl-gcc.
        // No idea how portable this is!
        if (WIFEXITED(r)) r = WEXITSTATUS(r);
#endif
        drop();
        push_int_val(r);
        break;

      case tkfflush:
        nargs = *ip++;
        r = fflush_file(nargs);
        if (nargs) drop();
        push_int_val(r);
        break;

      case tkclose:
        nargs = *ip++;
        r = close_file(to_str(STKP)->vst->str);
        drop();
        push_int_val(r);
        break;

      case tksprintf:
        nargs = *ip++;
        zstring_release(&TT.rgl.zspr);
        TT.rgl.zspr = new_zstring("", 0);
        varprint(fsprintf, 0, nargs);
        drop_n(nargs);
        vv = (struct zvalue)ZVINIT(ZF_STR, 0, TT.rgl.zspr);
        push_val(&vv);
        break;

      // Math builtins -- move here (per Oliver Webb suggestion)
      case tkatan2:
        nargs = *ip++;
        d = atan2(to_num(STKP-1), to_num(STKP));
        drop();
        STKP->num = d;
        break;
      case tkrand:
        nargs = *ip++;
        push_int_val(0);
        // Get all 53 mantissa bits in play:
        // (upper 26 bits * 2^27 + upper 27 bits) / 2^53
        STKP->num =
          ((random() >> 5) * 134217728.0 + (random() >> 4)) / 9007199254740992.0;
        break;
      case tksrand:
        nargs = *ip++;
        if (nargs == 1) {
          STKP->num = seedrand(to_num(STKP));
        } else push_int_val(seedrand(time(0)));
        break;
      case tkcos: case tksin: case tkexp: case tklog: case tksqrt: case tkint:
        nargs = *ip++;
        STKP->num = mathfunc[opcode-tkcos](to_num(STKP));
        break;

      default:
        // This should never happen:
        error_exit("!!! Unimplemented opcode %d", opcode);
    }
  }
  return opquit;
}

// interp() wraps the main interpreter loop interpx(). The main purpose
// is to allow the TT.stack to be readjusted after an 'exit' from a function.
// Also catches errors, as the normal operation should leave the TT.stack
// depth unchanged after each run through the rules.
static int interp(int start, int *status)
{
  int stkptrbefore = stkn(0);
  int r = interpx(start, status);
  // If exit from function, TT.stack will be loaded with args etc. Clean it.
  if (r == tkexit) {
    // TODO FIXME is this safe? Just remove extra entries?
    STKP = &STACK[stkptrbefore];
  }
  if (stkn(0) - stkptrbefore)
    error_exit("!!AWK BUG stack pointer offset: %d", stkn(0) - stkptrbefore);
  return r;
}

static void insert_argv_map(struct zvalue *map, int key, char *value)
{
  struct zvalue zkey = ZVINIT(ZF_STR, 0, num_to_zstring(key, ENSURE_STR(&STACK[CONVFMT])->vst->str));
  struct zvalue *v = get_map_val(map, &zkey);
  zvalue_release_zstring(&zkey);
  zvalue_release_zstring(v);
  *v = new_str_val(value);
  check_numeric_string(v);
}

static void init_globals(int optind, int argc, char **argv, char *sepstring,
    struct arg_list *assign_args)
{
  // Global variables reside at the bottom of the TT.stack. Start with the awk
  // "special variables":  ARGC, ARGV, CONVFMT, ENVIRON, FILENAME, FNR, FS, NF,
  // NR, OFMT, OFS, ORS, RLENGTH, RS, RSTART, SUBSEP

  STACK[CONVFMT] = new_str_val("%.6g");
  // Init ENVIRON map.
  struct zvalue m = ZVINIT(ZF_MAP, 0, 0);
  zvalue_map_init(&m);
  STACK[ENVIRON] = m;
  for (char **pkey = environ; *pkey; pkey++) {
    char *pval = strchr(*pkey, '=');
    if (!pval) continue;
    struct zvalue zkey = ZVINIT(ZF_STR, 0, new_zstring(*pkey, pval - *pkey));
    struct zvalue *v = get_map_val(&m, &zkey);
    zstring_release(&zkey.vst);
    if (v->vst) FFATAL("env var dup? (%s)", pkey);
    *v = new_str_val(++pval);    // FIXME refcnt
    check_numeric_string(v);
  }

  // Init ARGV map.
  m = (struct zvalue)ZVINIT(ZF_MAP, 0, 0);
  zvalue_map_init(&m);
  STACK[ARGV] = m;
  insert_argv_map(&m, 0, TT.progname);
  int nargc = 1;
  for (int k = optind; k < argc; k++) {
    insert_argv_map(&m, nargc, argv[k]);
    nargc++;
  }

  // Init rest of the awk special variables.
  STACK[ARGC] = (struct zvalue)ZVINIT(ZF_NUM, nargc, 0);
  STACK[FILENAME] = new_str_val("");
  STACK[FNR] = (struct zvalue)ZVINIT(ZF_NUM, 0, 0);
  STACK[FS] = new_str_val(sepstring);
  STACK[NF] = (struct zvalue)ZVINIT(ZF_NUM, 0, 0);
  STACK[NR] = (struct zvalue)ZVINIT(ZF_NUM, 0, 0);
  STACK[OFMT] = new_str_val("%.6g");
  STACK[OFS] = new_str_val(" ");
  STACK[ORS] = new_str_val("\n");
  STACK[RLENGTH] = (struct zvalue)ZVINIT(ZF_NUM, 0, 0);
  STACK[RS] = new_str_val("\n");
  STACK[RSTART] = (struct zvalue)ZVINIT(ZF_NUM, 0, 0);
  STACK[SUBSEP] = new_str_val("\034");

  // Init program globals.
  //
  // Push global variables on the TT.stack at offsets matching their index in the
  // global var table.  In the global var table we may have the type as scalar
  // or map if it is used as such in the program. In that case we init the
  // pushed arg from the type of the globals table.
  // But if a global var appears only as a bare arg in a function call it will
  // not be typed in the globals table. In that case we can only say it "may be"
  // a map, but we have to assume the possibility and attach a map to the
  // var. When/if the var is used as a map or scalar in the called function it
  // will be converted to a map or scalar as required.
  // See force_maybemap_to_scalar(), and the similar comment in
  // 'case tkfunction:' above.
  //
  int gstx, len = zlist_len(&TT.globals_table);
  for (gstx = TT.spec_var_limit; gstx < len; gstx++) {
    struct symtab_slot gs = GLOBAL[gstx];
    struct zvalue v = ZVINIT(gs.flags, 0, 0);
    if (v.flags == 0) {
      zvalue_map_init(&v);
      v.flags = ZF_MAYBEMAP;
    } else if (IS_MAP(&v)) {
      zvalue_map_init(&v);
    } else {
      // Set SCALAR flag 0 to create "uninitialized" scalar.
      v.flags = 0;
    }
    push_val(&v);
  }

  // Init -v assignment options.
  for (struct arg_list *p = assign_args; p; p = p->next) {
    char *asgn = p->arg;
    char *val = strchr(asgn, '=');
    if (!val) error_exit("bad -v assignment format");
    *val++ = 0;
    assign_global(asgn, val);
  }

  TT.rgl.cur_arg = new_str_val("<cmdline>");
  uninit_string_zvalue = new_str_val("");
  zvalue_copy(&FIELD[0], &uninit_string_zvalue);
}

static void run_files(int *status)
{
  int r = 0;
  while (r != tkexit && *status < 0 && getrec_f0() >= 0)
    if ((r = interp(TT.cgl.first_recrule, status)) == tknextfile) next_fp();
}

static void free_literal_regex(void)
{
  int len = zlist_len(&TT.literals);
  for (int k = 1; k < len; k++)
    if (IS_RX(&LITERAL[k])) regfree(LITERAL[k].rx);
}

EXTERN void run(int optind, int argc, char **argv, char *sepstring,
    struct arg_list *assign_args)
{
  char *printf_fmt_rx = "%[-+ #0']*([*]|[0-9]*)([.]([*]|[0-9]*))?l?[aAdiouxXfFeEgGcs%]";
  init_globals(optind, argc, argv, sepstring, assign_args);
  TT.cfile = xzalloc(sizeof(struct zfile));
  xregcomp(&TT.rx_default, "[ \t\n]+", REG_EXTENDED);
  xregcomp(&TT.rx_last, "[ \t\n]+", REG_EXTENDED);
  xregcomp(&TT.rx_printf_fmt, printf_fmt_rx, REG_EXTENDED);
  new_file("-", stdin, 'r', 1, 1);
  new_file("/dev/stdin", stdin, 'r', 1, 1);
  new_file("/dev/stdout", stdout, 'w', 1, 1);
  TT.zstdout = TT.zfiles;
  new_file("/dev/stderr", stderr, 'w', 1, 1);
  seedrand(1);
  int status = -1, r = 0;
  if (TT.cgl.first_begin) r = interp(TT.cgl.first_begin, &status);
  if (r != tkexit)
    if (TT.cgl.first_recrule) run_files(&status);
  if (TT.cgl.first_end) r = interp(TT.cgl.first_end, &status);
  regfree(&TT.rx_printf_fmt);
  regfree(&TT.rx_default);
  regfree(&TT.rx_last);
  free_literal_regex();
  close_file(0);    // close all files
  if (status >= 0) awk_exit(status);
}
