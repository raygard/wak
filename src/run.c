// run.c
// Copyright 2023 Ray Gardner
// vi: tabstop=2 softtabstop=2 shiftwidth=2

#include "common.h"

static void check_numeric_string(struct zvalue *v)
{
  if (v->vst) {
    char *end, *s = v->vst->str;
    // Significant speed gain with this test:
    // num string must begin space, +, -, ., or digit.
    if (strchr("+-.1234567890 ", *s)) {
      setlocale(LC_NUMERIC, "");
      double num = strtod(s, &end);
      if (s == end || end[strspn(end, " ")]) return;
      v->num = num;
      v->flags |= ZF_NUM | ZF_STR | ZF_NUMSTR;
    }
  }
}

enum {pbufsize = 512};
static char pbuf[pbufsize];

static struct zstring *num_to_zstring(double n, char *fmt)
{
  int k;
  if (n == (long long)n) k = snprintf(pbuf, pbufsize, "%lld", (long long)n);
  else k = snprintf(pbuf, pbufsize, fmt, n);
  if (k < 0 || k >= pbufsize) ffatal("error encoding %f via '%s'", n, fmt);
  return new_zstring(pbuf, k);
}

////////////////////
//// regex routines
////////////////////

EXTERN char *rx_escape_str(char *s)
{
  char *p, *escapes = "abfnrtv\"/"; // FIXME TODO should / be in there?
  char *s0 = s, *to = s;
  while ((*to = *s)) {
    if (*s != '\\') { to++, s++;
    } else if ((p = strchr(escapes, *++s))) {
      // checking char after \ for known escapes
      int c = "\a\b\f\n\r\t\v\"/"[p-escapes];
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
    } else *to++ = '\\', *to++ = *s++;
  }
  return s0;
}

EXTERN int rx_compile(regex_t *rx, char *pat)
{
  int r;
  if ((r = regcomp(rx, pat, REG_EXTENDED)) != 0) {
    char errbuf[256];
    regerror(r, rx, errbuf, sizeof(errbuf));
    fprintf(stderr, "regex error %d: %s on '%s' -- ", r, errbuf, pat);
  }
  return r;
}

static void rx_compile_or_die(regex_t *rx, char *pat)
{
  if (rx_compile(rx, pat)) fatal("bad regex\n");
}

static void rx_zvalue_compile(regex_t **rx, struct zvalue *pat)
{
  if (is_rx(pat)) *rx = pat->rx;
  else {
    val_to_str(pat);
    zvalue_dup_zstring(pat);
    rx_escape_str(pat->vst->str);
    rx_compile_or_die(*rx, pat->vst->str);
  }
}

static void rx_zvalue_free(regex_t *rx, struct zvalue *pat)
{
  if (!is_rx(pat) || rx != pat->rx) regfree(rx);
}

// Used by the match/not match ops (~ !~) and implicit $0 match (/regex/)
static int match(struct zvalue *zvsubject, struct zvalue *zvpat)
{
  int r;
  regex_t rx, *rxp = &rx;
  val_to_str(zvsubject);
  rx_zvalue_compile(&rxp, zvpat);
  if ((r = regexec(rxp, zvsubject->vst->str, 0, 0, 0)) != 0) {
    if (r != REG_NOMATCH) {
      char errbuf[256];
      regerror(r, &rx, errbuf, sizeof(errbuf));
      // FIXME TODO better diagnostic here
      fprintf(stderr, "regex match error %d: %s\n", r, errbuf);
      exit(123);  // FIXME TODO use fatal() or ... ?
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
  if (r) fatal("regexec error");  // TODO ? use regerr() to meaningful msg
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

// Need more for toybox awk test
#define FIELDS_MAX  102400
#define THIS_MEANS_SET_NF 999999999

#define FS_MAX  128
static regex_t rx_default;
static regex_t rx_last;
static regex_t rx_multiline;
static char fs_last[FS_MAX] = "";
static char one_char_fs[4];

// Called by run:run()
static void init_field_rx(void)
{
  rx_compile_or_die(&rx_default, "[ \t\n]+");
  rx_compile_or_die(&rx_last, "[ \t\n]+"); // FIXME TODO what if empty ""
  rx_compile_or_die(&rx_multiline, "[ \t]+");
}

// Called by run:run()
static void free_field_rx(void)
{
  regfree(&rx_default);
  regfree(&rx_last);
  regfree(&rx_multiline);
}

static int get_int_val(struct zvalue *v)
{
  if (is_num(v)) return (int)v->num;
  if (is_str(v) && v->vst) return (int)str_to_num(v->vst->str);
  return 0;
}

static char *fmt_one_char_fs(char *fs)
{
  if (strlen(fs) != 1) return fs;
  snprintf(one_char_fs, sizeof(one_char_fs), "[%c]", fs[0]);
  return one_char_fs;
}

static regex_t *rx_prep(char *fs)
{
  if (!strcmp(fs, " ")) return &rx_default;
  if (!strcmp(fs, fs_last)) return &rx_last;
  if (strlen(fs) >= FS_MAX) fatal("FS too long");
  strcpy(fs_last, fs);
  regfree(&rx_last);
  rx_compile_or_die(&rx_last, fmt_one_char_fs(fs));
  return &rx_last;
}

// Only for use by split() builtin
static void set_map_element(struct zmap *m, int k, char *val, size_t len)
{
  // Do not need format here b/c k is integer, uses "%lld" format.
  struct zstring *key = num_to_zstring(k, 0);
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

static int nf_internal = 0;  // should match NF

// All changes to NF go through here!
static void set_nf(int nf)
{
  STACK[NF].num = nf_internal = nf;
  STACK[NF].flags = ZF_NUM;
}

static void set_field(struct zmap *unused, int fnum, char *s, size_t size)
{ (void)unused;
  if (fnum < 0 || fnum > FIELDS_MAX) ffatal("bad field num %d\n", fnum);
  int nfields = zlist_len(&fields);
  // Need nfields to be > fnum b/c e.g. fnum==1 implies 2 fields
  while (nfields <= fnum)
    nfields = zlist_append(&fields, &uninit_zvalue) + 1;
  set_zvalue_str(&FIELD[fnum], s, size);
  set_nf(fnum);
  check_numeric_string(&FIELD[fnum]);
}

// Split s via fs, using setter; return number of fields.
// This is used to split fields and also for split() builtin.


static int splitter(void (*setter)(struct zmap *, int, char *, size_t), struct zmap *m, char *s, struct zvalue *zvfs)
{
  regex_t *rx;
  regoff_t offs, end;
  if (!is_rx(zvfs)) val_to_str(zvfs);
  char *fs = is_str(zvfs) ? zvfs->vst->str : "";
  int nf = 0, r = 0, eflag = 0;
  // Empty string or empty fs (regex).
  // Need to include !*s b/c empty string, otherwise
  // split("", a, "x") splits to a 1-element (empty element) array
  if (!*s || (is_str(zvfs) && !*fs) || is_empty_rx(zvfs)) {
    for ( ; *s; s++) setter(m, ++nf, s, 1);
    return nf;
  }
  if (is_rx(zvfs)) rx = zvfs->rx;
  else rx = rx_prep(fs);
  while (*s) {
    // Find the next occurrence of FS.
    // rx_find_FS() returns 0 if found. If nonzero, the field will
    // be the rest of the record (all of it if first time through).
    if ((r = rx_find_FS(rx, s, &offs, &end, eflag))) offs = end = strlen(s);
    else {
      int k = strcspn(s, "\n");
      if (k < offs) offs = k, end = k + 1;
    }
    eflag |= REG_NOTBOL;

    // Field will be s up to (not including) the offset. If offset
    // is zero and FS is found and FS is ' ' (rx_default "[ \t]+"),
    // then the find is the leading or trailing spaces and/or tabs.
    // If so, skip this (empty) field, otherwise set field, length is offs.
    if (offs || r || rx != &rx_default) setter(m, ++nf, s, offs);
    s += end;
  }
  if (!r && rx != &rx_default) setter(m, ++nf, "", 0);
  return nf;
}

static void build_fields(void)
{
  val_to_str(&STACK[FS]);
  char *rec = FIELD[0].vst->str;
  // TODO test this -- why did I not want to split empty $0?
  // Maybe don't split empty $0 b/c non-default FS gets NF==1 with splitter()?
  set_nf(*rec ? splitter(set_field, 0, rec, &STACK[FS]) : 0);
}

static void rebuild_field0(void)
{
  struct zstring *s = FIELD[0].vst;
  int nf = nf_internal;
  // uninit value needed for eventual reference to .vst in zstring_release()
  struct zvalue tempv = uninit_zvalue;
  zvalue_copy(&tempv, &STACK[OFS]);
  val_to_str(&tempv);
  for (int i = 1; i <= nf; i++) {
    if (i > 1) {
      s = s ? zstring_extend(s, tempv.vst) : zstring_copy(s, tempv.vst);
    }
    if (FIELD[i].flags) val_to_str(&FIELD[i]);
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
// increase the value of NF; create any intervening fields with the
// uninitialized value; and cause the value of $0 to be recomputed, with the
// fields being separated by the value of OFS.]
// Called by run:setup_lvalue()
static struct zvalue *get_field_ref(int fnum)
{
  if (fnum < 0 || fnum > FIELDS_MAX) error_exit("bad field num %d\n", fnum);
  if (fnum > nf_internal) {
    // Ensure fields list is large enough for fnum
    // Need len of fields to be > fnum b/c e.g. fnum==1 implies 2 fields
    for (int i = nf_internal + 1; i <= fnum; i++) {
      if (i == zlist_len(&fields)) zlist_append(&fields, &uninit_zvalue);
      zvalue_copy(&FIELD[i], &uninit_string_zvalue);
    }
    set_nf(fnum);
  }
  return &FIELD[fnum];
}

// Called by tksplit op in run
static int split(struct zstring *s, struct zvalue *a, struct zvalue *fs)
{
  return splitter(set_map_element, a->map, s->str, fs);
}

// Called by run:getrec_f0_f() and run:getrec_f0()
static void copy_to_field0(char *buf, size_t k)
{
  set_zvalue_str(&FIELD[0], buf, k);
  check_numeric_string(&FIELD[0]);
  build_fields();
}

// After changing $0, must rebuild fields & reset NF
// Changing other field must rebuild $0
// Called in run by gsub() and assignment ops.
static void fixup_fields(int fnum)
{
  if (fnum == THIS_MEANS_SET_NF) {  // NF was assigned to
    int new_nf = get_int_val(&STACK[NF]);
    // Ensure fields list is large enough for fnum
    // Need len of fields to be > fnum b/c e.g. fnum==1 implies 2 fields
    for (int i = nf_internal + 1; i <= new_nf; i++) {
      if (i == zlist_len(&fields)) zlist_append(&fields, &uninit_zvalue);
      zvalue_copy(&FIELD[i], &uninit_string_zvalue);
    }
    set_nf(nf_internal = STACK[NF].num);
    rebuild_field0();
    return;
  }
  // fnum is # of field that was just updated.
  // If it's 0, need to rebuild the fields 1... n.
  // If it's non-0, need to rebuild field 0.
  val_to_str(&FIELD[fnum]);
  if (fnum) check_numeric_string(&FIELD[fnum]);
  if (fnum) rebuild_field0();
  else build_fields();
}

// Fetching non-existent field gets uninit string value; no change to NF!
// Called by tkfield op in run    // TODO inline it in run?
static void push_field(int fnum)
{
  if (fnum < 0 || fnum > FIELDS_MAX) error_exit("bad field num %d\n", fnum);
  // Contrary to posix, awk evaluates fields beyond $NF as empty strings.
  if (fnum > nf_internal) push_val(&uninit_string_zvalue);
  else push_val(&FIELD[fnum]);
}

////////////////////
////   run
////////////////////

static struct runtime_globals {
  struct zvalue cur_arg;
  FILE *fp;           // current data file
  int narg;           // cmdline arg index
  int nfiles;         // num of cmdline data file args processed
  int eof;            // all cmdline files (incl. stdin) read
  char *recbuf;
  size_t recbufsize;
  char *recbuf_multiline;
  size_t recbufsize_multiline;
  struct zstring *zspr;      // Global to receive sprintf() string value
} rgl;

#define STKP    (&STACK[stkptr])

static char range_sw[64];   // FIXME TODO quick and dirty set of range switches


// Random number generator
// Extracted from http://www.cs.ucl.ac.uk/staff/d.jones/GoodPracticeRNG.pdf
// modified to encapsulate state and add seed function.
static struct jkiss32_state {
  unsigned x, y, z, w, c, seed;
} jkst = {123456789, 234567891, 345678912, 456789123, 0, 1};

static unsigned jkiss32(void)
{
  int t;
  jkst.y ^= (jkst.y<<5); jkst.y ^= (jkst.y>>7); jkst.y ^= (jkst.y<<22);
  t = jkst.z+jkst.w+jkst.c; jkst.z = jkst.w; jkst.c = t<0; jkst.w = t&2147483647;
  jkst.x += 1411392427;
  return jkst.x + jkst.y + jkst.w;
}

static unsigned seed_jkiss32(unsigned n)
{
  unsigned r = jkst.seed;
  if (!n) n = 1;
  jkst = (struct jkiss32_state){n*123456789, n*234567891, n*345678912, n*456789123, 0, n};
  if (n > 1) for (n = 10000; n--;) jkiss32();
  return r;
}
// END Random number generator

static void check_not_map(struct zvalue *v)
{
  if (is_map(v)) fatal("array in scalar context");
}



static int popnumval(void)
{
  stack.avail -= sizeof(struct zvalue);
  return STACK[stkptr--].num;
}

static void drop(void)
{
  stack.avail -= sizeof(struct zvalue);
  struct zvalue *v = &STACK[stkptr--];
  zvalue_release_zstring(v);
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

static void force_maybemap_to_scalar(struct zvalue *v)
{
  if (v->flags & ZF_MAYBEMAP) {
    v->flags = 0;
    if (v->map->count) fatal("attempt to use array as scalar.");
    v->map = 0;   //// !!!! FIXME abandoning the maybe map
  }
}

static void force_maybemap_to_map(struct zvalue *v)
{
  if (v->flags & ZF_MAYBEMAP) v->flags = ZF_MAP;
}

// Set and return logical (0/1) val of top stack value; flag value as NUM.
static int get_set_logical(void)
{
  struct zvalue *v = STKP;
  check_not_map(v);
  force_maybemap_to_scalar(v);
  int r = 0;
  if (is_num(v)) r = !! v->num;
  else if (is_str(v)) r = (v->vst && v->vst->str[0]);
  zvalue_release_zstring(v);
  v->num = r;
  v->flags = ZF_NUM;
  return r;
}


static struct zvalue *val_to_str_fmt(struct zvalue *v, char *fmt)
{
  check_not_map(v);
  force_maybemap_to_scalar(v);
  // TODO: consider handling numstring differently
  // if string and ONLY string (not numstring)
  if (v->flags & ZF_NUMSTR) v->flags = ZF_STR;
  if (is_str(v)) return v;
  else if (!v->flags) { // uninitialized
    v->vst = new_zstring("", 0);
  } else if (is_num(v)) {
    zvalue_release_zstring(v);
    v->vst = num_to_zstring(v->num, fmt);
  } else {
    fatal("!!! Wrong or unknown type in val_to_str_fmt\n");
  }
  v->flags = ZF_STR;
  return v;
}

EXTERN struct zvalue *val_to_str(struct zvalue *v)
{
  force_maybemap_to_scalar(v);
  // chicken-egg problem here. Need to convert CONVFMT to string
  // but need it to be a string. So use default format.
  // Should only happen when user sets CONVFMT to not-a-string.
  if (!is_str(&STACK[CONVFMT])) {
    zstring_release(&STACK[CONVFMT].vst);
    STACK[CONVFMT].vst = num_to_zstring(STACK[CONVFMT].num, "%.6g");
    STACK[CONVFMT].flags = ZF_STR;
  }
  return val_to_str_fmt(v, STACK[CONVFMT].vst->str);
}
#define ensure_str(v) (is_str(v) ? (v) : val_to_str(v))

static double val_to_num(struct zvalue *v)
{
  check_not_map(v);
  force_maybemap_to_scalar(v);
  if (v->flags & ZF_NUMSTR) zvalue_release_zstring(v);
  else if (!(is_num(v))) {
    v->num = 0.0;
    if (is_str(v) && v->vst) v->num = str_to_num(v->vst->str);
    zvalue_release_zstring(v);
  }
  v->flags = ZF_NUM;
  return v->num;
}

static void set_string(struct zvalue *v, struct zstring *zs)
{
  zstring_release(&v->vst);
  v->vst = zs;
  v->flags = ZF_STR;
}

static void set_num(struct zvalue *v, double n)
{
  zstring_release(&v->vst);
  v->num = n;
  v->flags = ZF_NUM;
}

static void incr_zvalue(struct zvalue *v)
{
  v->num = trunc(val_to_num(v)) + 1;
}

static void push_int_val(ptrdiff_t n)
{
  struct zvalue v = ZVINIT(ZF_NUM, n, 0);
  push_val(&v);
}

static struct zvalue *get_map_val(struct zvalue *v, struct zvalue *key)
{
  val_to_str(key); // FIXME does this work always?
  struct zmap_slot *x = zmap_find_or_insert_key(v->map, key->vst);
  return &x->val;
}

static struct zvalue *setup_lvalue(int n, int parmbase, int *field_num)
{
  *field_num = -1;
  struct zvalue *v = &STACK[n], *vv = v;
  int k = v->num;
  if (v->flags & ZF_FIELDREF) {
    v = get_field_ref(*field_num = k);
  } else if (v->flags & (ZF_REF | ZF_MAPREF)) {
    if (k == NF) *field_num = THIS_MEANS_SET_NF;
    if (k < 0) k = parmbase - k;    // loc of var on stack
    v = &STACK[k];
    // Next line caused mem leaks and bad 'array in scalar context' errors
    // force_maybemap_to_map(v);
    // Next line caused ASAN errors; why?
    // if (vv->flags & ZF_MAPREF) v->flags = ZF_MAP;
    // Next line fixed the above errors.
    if (vv->flags & ZF_MAPREF) force_maybemap_to_map(v);
    if (is_map(v)) {
      v = get_map_val(v, &STACK[n - 1]);
      swap();
      drop();
    }
  } else fatal("assignment to bad lvalue");
  return v;
}

// TODO FIXME This is set pretty high (1010) to pass T.overflow test of 1000
// open files; files[] is 24240 bytes. Maybe should be allocated dynamically?
#define MAX_FILES 1010
struct zfile {
  struct zstring *fn;
  FILE *fp;
  char mode;  // w, a, or r
  char file_or_pipe;  // f or p
};

static int file_cnt = 0, std_file_cnt = 0;

static struct zfile files[MAX_FILES];


static void setup_std_file(char *fn, FILE *fp, char *mode)
{
  files[file_cnt++] = (struct zfile){new_zstring(fn, strlen(fn)), fp, *mode, 'f'};
  std_file_cnt = file_cnt;
}

static int fflush_all(void)
{
  for (int k = 0; k < file_cnt; k++) {
    struct zfile *zof = &files[k];
    if (fflush(zof->fp)) return -1;
  }
  return 0;
}

static int fflush_file(int nargs)
{
  if (!nargs) return fflush_all();
  val_to_str(STKP);   // filename at top of stack
  // Null string means flush all
  if (!STKP[0].vst->str[0]) return fflush_all();
  // is it open in file table?
  for (int k = 0; k < file_cnt; k++) {
    struct zfile *zof = &files[k];
    if (zstring_match(STKP[0].vst, zof->fn)) {
      // if (zof->file_or_pipe == 'f' && fflush(zof->fp) == 0)
      //   return 0;  // otherwise assume fail
      // return -1;
      if (fflush(zof->fp) == 0) return 0;
    }
  }
  return -1;    // file not found in table
}

static int close_file(void)
{
  val_to_str(STKP);   // filename at top of stack
  // is it open in file table?
  for (int k = 0; k < file_cnt; k++) {
    struct zfile *zof = &files[k];
    if (zstring_match(STKP[0].vst, zof->fn)) {
      if (!zof->fp || (zof->file_or_pipe == 'f' ? fclose : pclose)(zof->fp) < 0)
        return -1;  // otherwise assume successful close
      if (k < std_file_cnt) return 0;
      // close hole in table if not last slot
      if (k < --file_cnt) files[k] = files[file_cnt];
      return 0;
    }
  }
  return -1;    // file not found in table
}


// FIXME TODO check if file/pipe/mode matches what's in the table already.
// Apparently gawk/mawk/nawk are OK with different mode, but just use the file
// in whatever mode it's already in; i.e. > after >> still appends.
static FILE *setup_file(char *file_or_pipe, char *mode)
{
  val_to_str(STKP);   // filename at top of stack
  struct zfile *zof = 0;
  // is it already open in file table?
  for (int k = 0; k < file_cnt; k++) {
    zof = &files[k];
    if (zstring_match(STKP[0].vst, zof->fn)) {
      drop();
      return zof->fp;   // open; return it
    }
  }
  // Open and put in table
  if (file_cnt >= MAX_FILES) fatal("too many open output files!\n");
  zof = &files[file_cnt];
  file_cnt++;
  zof->fn = STKP[0].vst;
  zstring_incr_refcnt(zof->fn);
  drop();
  zof->fp = (*file_or_pipe == 'f' ? fopen : popen)(zof->fn->str, mode);
  if (*mode != 'r' && !zof->fp) ffatal("cannot open '%s'\n", zof->fn->str);
  zof->file_or_pipe = *file_or_pipe;
  zof->mode = *mode;
  return zof->fp;
}

static FILE *setup_outfile(char *file_or_pipe, char *mode)
{
  FILE *fp = setup_file(file_or_pipe, mode);
  return fp;
}

static int getcnt(int k)
{
  if (k >= stkptr) fatal("too few args for printf\n");
  return (int)val_to_num(&STACK[k]);
}

static int fsprintf(FILE *ignored, const char *fmt, ...)
{
  (void)ignored;
  va_list args, args2;
  va_start(args, fmt);
  va_copy(args2, args);
  int len = vsnprintf(0, 0, fmt, args); // size needed
  va_end(args);

  // Unfortunately we have to mess with zstring internals here.
  if (len > (int)(rgl.zspr->capacity - rgl.zspr->size) - 1) {
    size_t cap = 2 * rgl.zspr->capacity + len;
    rgl.zspr = xrealloc(rgl.zspr, sizeof(*rgl.zspr) + cap);
    rgl.zspr->capacity = cap;
  }
  vsnprintf(rgl.zspr->str + rgl.zspr->size, len+1, fmt, args2);
  rgl.zspr->size += len;
  rgl.zspr->str[rgl.zspr->size] = 0;

  va_end(args2);
  return 0;
}



static regex_t rx_printf_fmt;
static char *printf_fmt_rx = "%[-+ #0]*([*]|[0-9]*)([.]([*]|[0-9]*))?[aAdiouxXfFeEgGcs%]";

static void varprint(int(*fpvar)(FILE *, const char *, ...), FILE *outfp, int nargs)
{
  int k, nn, nnc, fmtc, holdc, cnt1 = 0, cnt2 = 0;
  double n = 0;
  char *s;
  regoff_t offs = -1, e = -1;
  val_to_str(&STACK[stkptr-nargs+1]);
  char *fmt = STACK[stkptr-nargs+1].vst->str;
  k = stkptr - nargs + 2;
  while (*fmt) {
    nn = strcspn(fmt, "%");
    if (nn) {
      holdc = fmt[nn];
      fmt[nn] = 0;
      fpvar(outfp, "%s", fmt);
      fmt[nn] = holdc;
    }
    fmt += nn;
    if (!*fmt) break;
    nnc = strcspn(fmt+1, "aAdiouxXfFeEgGcs%");
    fmtc = fmt[nnc+1];
    if (!fmtc) ffatal("bad printf format '%s'", fmt);
    holdc = fmt[nnc+2];
    fmt[nnc+2] = 0;
    if (rx_find(&rx_printf_fmt, fmt, &offs, &e, 0))
      ffatal("bad printf format <%s>\n", fmt);
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
        if (k > stkptr) fatal("not enough args for printf format\n");
        if (fmtc == 's') {
          val_to_str(&STACK[k]);
          s = STACK[k++].vst->str;
        } else if (fmtc == 'c' && !is_num(&STACK[k])) {
          n = STACK[k++].vst ? STACK[k-1].vst->str[0] : '!';
        } else {
          val_to_num(&STACK[k]);
          n = STACK[k++].num;
        }
        switch (nargsneeded) {
          case 1:
            if (fmtc == 's') fpvar(outfp, fmt, s);
            else if (strchr("cdi", fmtc)) fpvar(outfp, fmt, (int)n);
            else if (strchr("ouxX", fmtc)) fpvar(outfp, fmt, (unsigned)n);
            else fpvar(outfp, fmt, n);
            break;
          case 2:
            if (fmtc == 's') fpvar(outfp, fmt, cnt2, s);
            else if (strchr("cdi", fmtc)) fpvar(outfp, fmt, cnt2, (int)n);
            else if (strchr("ouxX", fmtc)) fpvar(outfp, fmt, cnt2, (unsigned)n);
            else fpvar(outfp, fmt, cnt2, n);
            break;
          case 3:
            if (fmtc == 's') fpvar(outfp, fmt, cnt1, cnt2, s);
            else if (strchr("cdi", fmtc)) fpvar(outfp, fmt, cnt1, cnt2, (int)n);
            else if (strchr("ouxX", fmtc)) fpvar(outfp, fmt, cnt1, cnt2, (unsigned)n);
            else fpvar(outfp, fmt, cnt1, cnt2, n);
            break;
        }
        break;
      default:
        fatal("bad printf format\n");
    }
    fmt += nnc + 2;
    *fmt = holdc;
  }
}

EXTERN char *escape_str(char *s)
{
  char *p, *escapes = "\\abfnrtv\"/"; // FIXME TODO should / be in there?
  char *s0 = s, *to = s;
  while ((*to = *s)) {
    if (*s != '\\') to++, s++;
    else if ((p = strchr(escapes, *++s))) {
      // checking char after \ for known escapes
      int c = "\\\a\b\f\n\r\t\v\"/"[p-escapes];
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
    } else *to++ = *s++;
  }
  return s0;
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
  if (!is_ok_varname(var)) ffatal("invalid variable name '%s'\n'", var);
  int globals_ent = find_global(var);
  if (globals_ent) {
    struct zvalue *v = &STACK[globals_ent];
    if (is_map(v)) error_exit("-v assignment to array\n");  // Maybe not needed?
    zvalue_release_zstring(v);
    value = xstrdup(value);
    *v = new_str_val(escape_str(value));
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
    *val++ = '\0';
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
    if (++rgl.narg >= (int)val_to_num(&STACK[ARGC])) return 0;
    struct zvalue *v = &STACK[ARGV];
    struct zvalue zkey = ZVINIT(ZF_STR, 0,
        num_to_zstring(rgl.narg, val_to_str(&STACK[CONVFMT])->vst->str));
    arg = "";
    if (zmap_find(v->map, zkey.vst)) {
      zvalue_copy(&rgl.cur_arg, val_to_str(get_map_val(v, &zkey)));
      arg = rgl.cur_arg.vst->str;
    }
    zvalue_release_zstring(&zkey);
  } while (!*arg || assignment_arg(arg));
  rgl.nfiles++;
  return arg;
}

static int next_fp(void)
{
  char *fn = nextfilearg();
  if (rgl.fp && rgl.fp != stdin) fclose(rgl.fp);
  if ((!fn && !rgl.nfiles && rgl.fp != stdin) || (fn && !strcmp(fn, "-"))) {
    rgl.fp = stdin;
    zvalue_release_zstring(&STACK[FILENAME]);
    STACK[FILENAME].vst = new_zstring("<stdin>", 7);
  } else if (fn) {
    if (!(rgl.fp = fopen(fn, "r"))) ffatal("can't open %s\n", fn);
    zvalue_copy(&STACK[FILENAME], &rgl.cur_arg);
    set_num(&STACK[FNR], 0);
  } else {
    rgl.eof = 1;
    return 0;
  }
  return 1;
}


static ssize_t getrec_multiline(FILE *fp)
{
  ssize_t k, kk;
  do {
    k = getdelim(&rgl.recbuf, &rgl.recbufsize, '\n', fp);
  } while (k > 0 && rgl.recbuf[0] == '\n');
  if (k < 0) return k;
  // k > 0 and recbuf is not only a \n. Prob. ends w/ \n
  // but may not at EOF (last line w/o newline)
  for (;;) {
    kk = getdelim(&rgl.recbuf_multiline, &rgl.recbufsize_multiline, '\n', fp);
    if (kk < 0 || rgl.recbuf_multiline[0] == '\n') break;
    // data is in rgl.recbuf[0..k-1]; append to it
    if ((size_t)(k + kk + 1) > rgl.recbufsize)
      rgl.recbuf = xrealloc(rgl.recbuf, rgl.recbufsize = k + kk + 1);
    memmove(rgl.recbuf + k, rgl.recbuf_multiline, kk+1);
    k += kk;
  }
  if (k > 1 && rgl.recbuf[k-1] == '\n') rgl.recbuf[--k] = '\0';
  return k;
}

static ssize_t getrec_f(FILE *fp)
{
  int rs = ensure_str(&STACK[RS])->vst->str[0] & 0xff;
  if (!rs) return getrec_multiline(fp);

  ssize_t k = getdelim(&rgl.recbuf, &rgl.recbufsize, rs, fp);
  if (k > 0 && rgl.recbuf[k-1] == rs) rgl.recbuf[--k] = 0;
  return k;
}

static ssize_t getrec(void)
{
  ssize_t k;
  if (rgl.eof) return -1;
  if (!rgl.fp) next_fp();
  do {
    if ((k = getrec_f(rgl.fp)) >= 0) return k;
  } while (next_fp());
  return -1;
}

static ssize_t getrec_f0_f(FILE *fp)
{
  ssize_t k = getrec_f(fp);
  if (k >= 0) {
    copy_to_field0(rgl.recbuf, k);
  }
  return k;
}

static ssize_t getrec_f0(void)
{
  ssize_t k = getrec();
  if (k >= 0) {
    copy_to_field0(rgl.recbuf, k);
    incr_zvalue(&STACK[NR]);
    incr_zvalue(&STACK[FNR]);
  }
  return k;
}

// source is tkeof (no pipe/file), tklt (file), or tkpipe (pipe)
// fp is file or pipe (is NULL if file/pipe could not be opened)
// FIXME TODO should -1 return be replaced by test at caller?
// v is NULL or an lvalue ref
static int awk_getline(int source, FILE *fp, struct zvalue *v)
{
  ssize_t k;
  int is_stream = source != tkeof;
  if (is_stream && !fp) return -1;
  if (v) {
    if ((k = is_stream ? getrec_f(fp) : getrec()) < 0) return 0;
    set_string(v, new_zstring(rgl.recbuf, k));
    if (!is_stream) {
      incr_zvalue(&STACK[NR]);
      incr_zvalue(&STACK[FNR]);
    }
  } else k = is_stream ? getrec_f0_f(fp) : getrec_f0();
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
  struct zvalue *v = setup_lvalue(stkptr, parmbase, &field_num);
  struct zvalue *ere = STKP-2;
  struct zvalue *repl = STKP-1;
  regex_t rx, *rxp = &rx;
  rx_zvalue_compile(&rxp, ere);
  val_to_str(repl);
  val_to_str(v);

#define mkint(x) ((int)(x))    // coerce to integer
#define slen(zvalp) ((zvalp)->vst->size)
  char *p, *rp0 = repl->vst->str, *rp = rp0, *s = v->vst->str;
  int namps = 0, nhits = 0, is_sub = (opcode == tksub), eflags = 0;
  regoff_t so = -1, eo;
  // Count ampersands in repl string; may be overcount due to \& escapes.
  for (rp = rp0; *rp; rp++) namps += *rp == '&';
  p = s;
  regoff_t need = slen(v) + 1;  // capacity needed for result string
  // A pass just to determine needed destination (result) string size.
  while(!rx_find(rxp, p, &so, &eo, eflags)) {
    need += slen(repl) + (eo - so) * (namps - 1);
    if (!*p) break;
    p += eo ? eo : 1; // ensure progress if empty hit at start
    if (is_sub) break;
    eflags |= REG_NOTBOL;
  }

  if (so >= 0) {  // at least one hit
    struct zstring *z = new_zstring_cap(need);
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
    memmove(e, ep0, s + slen(v) - ep0);
    e += s + slen(v) - ep0;
    *e = '\0';
    z->size = e - z->str;
    zstring_release(&v->vst);
    v->vst = z;
  }
  rx_zvalue_free(rxp, ere);
  if (!is_rx(STKP-2)) zstring_release(&STKP[-2].vst);
  drop_n(3);
  push_int_val(nhits);
  if (field_num >= 0) fixup_fields(field_num);
}

static long time_ms(void)
{
  struct timespec ts;
  clock_gettime(CLOCK_REALTIME, &ts);
  return ts.tv_sec*1000+ts.tv_nsec/1000000;
}

static double (*mathfunc[])(double) = {cos, sin, exp, log, sqrt};
static void math_builtin(int opcode, int nargs)
{
  double d;
  switch (opcode) {
    case tkint:
      STKP->num = trunc(val_to_num(STKP));
      break;
    case tkatan2:
      d = atan2(val_to_num(STKP-1), val_to_num(STKP));
      drop();
      STKP->num = d;
      break;
    case tkrand:
      push_int_val(0);
      // STKP->num = rand(); // Not good in most libc implementations
      // STKP->num = (double)jkiss32() / 4294967296.0;
      // The above doesn't get all 53 mantissa bits in play. This does:
      // (upper 26 bits * 2^27 + upper 27 bits) / 2^53
      double a = (jkiss32() >> 6) * 134217728.0;
      STKP->num = (a + (jkiss32() >> 5)) / 9007199254740992.0;
      break;
    case tksrand:
      if (nargs == 1) {
        STKP->num = seed_jkiss32((unsigned)trunc(val_to_num(STKP)));
      } else push_int_val(seed_jkiss32((unsigned)time_ms()));
      break;
    default:
      if (tkcos <= opcode && opcode <= tksqrt) {
        STKP->num = mathfunc[opcode-tkcos](val_to_num(STKP));
      }
  }
}


#define clamp(x, lo, hi) ((x) < (lo) ? (lo) : (x) > (hi) ? (hi) : (x))

// Main loop of interpreter. Run this once for all BEGIN rules (which
// have had their instructions chained in compile), all END rules (also
// chained in compile), and once for each record of the data file(s).
static int interpx(int start, int *status)
{
  int *ip = &ZCODE[start];
  int opcode, op2, k, r, nargs, nsubscrs, range_num, parmbase = 0;
  int field_num;
  double nleft, nright;
  struct zvalue *v, vv;
// looptop
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
        val_to_num(STKP);
        STKP->num = -STKP->num;
        break;

      case tkpow:         // FALLTHROUGH intentional here
      case tkmul:         // FALLTHROUGH intentional here
      case tkdiv:         // FALLTHROUGH intentional here
      case tkmod:         // FALLTHROUGH intentional here
      case tkplus:        // FALLTHROUGH intentional here
      case tkminus:
        nleft = val_to_num(STKP-1);
        nright = val_to_num(STKP);
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
        val_to_str(STKP-1);
        val_to_str(STKP);
        STKP[-1].vst = zstring_extend(STKP[-1].vst, STKP[0].vst);
        drop();
        break;

        // Comparisons (with the '<', "<=", "!=", "==", '>', and ">="
        // operators) shall be made numerically if both operands are numeric,
        // if one is numeric and the other has a string value that is a numeric
        // string, or if one is numeric and the other has the uninitialized
        // value. Otherwise, operands shall be converted to strings as required
        // and a string comparison shall be made as follows:
        //
        // For the "!=" and "==" operators, the strings should be compared to
        // check if they are identical but may be compared using the
        // locale-specific collation sequence to check if they collate equally.
        //
        // For the other operators, the strings shall be compared using the
        // locale-specific collation sequence.
        //
        // The value of the comparison expression shall be 1 if the relation is
        // true, or 0 if the relation is false.
#define CMPSTR(a, b)  (strcmp(a.vst->str, b.vst->str))
      case tklt:          // FALLTHROUGH intentional here
      case tkle:          // FALLTHROUGH intentional here
      case tkne:          // FALLTHROUGH intentional here
      case tkeq:          // FALLTHROUGH intentional here
      case tkgt:          // FALLTHROUGH intentional here
      case tkge:
        ; int cmp = 31416;
        
        if (  (is_num(&STKP[-1]) &&
              (STKP[0].flags & (ZF_NUM | ZF_NUMSTR) || !STKP[0].flags)) ||
              (is_num(&STKP[0]) &&
              (STKP[-1].flags & (ZF_NUM | ZF_NUMSTR) || !STKP[-1].flags))) {
          switch (opcode) {
            case tklt:
              cmp = STKP[-1].num < STKP[0].num;
              break;
            case tkle:
              cmp = STKP[-1].num <= STKP[0].num;
              break;
            case tkne:
              cmp = STKP[-1].num != STKP[0].num;
              break;
            case tkeq:
              cmp = STKP[-1].num == STKP[0].num;
              break;
            case tkgt:
              cmp = STKP[-1].num > STKP[0].num;
              break;
            case tkge:
              cmp = STKP[-1].num >= STKP[0].num;
              break;
          }
        } else {
          val_to_str(STKP-1);
          val_to_str(STKP);
          cmp = CMPSTR(STKP[-1], STKP[0]);
          switch (opcode) {
            case tklt:
              cmp = cmp < 0;
              break;
            case tkle:
              cmp = cmp <= 0;
              break;
            case tkne:
              cmp = cmp != 0;
              break;
            case tkeq:
              cmp = cmp == 0;
              break;
            case tkgt:
              cmp = cmp > 0;
              break;
            case tkge:
              cmp = cmp >= 0;
              break;
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
        v = setup_lvalue(stkptr-1, parmbase, &field_num);
        val_to_num(v);
        val_to_num(STKP);
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
        v = setup_lvalue(stkptr-1, parmbase, &field_num);
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
        v = setup_lvalue(stkptr, parmbase, &field_num);
        val_to_num(v);
        v->flags = ZF_NUM;
        switch (opcode) {
          case tkincr: case tkdecr:
            // Must be done in this order because push_val(v) may move v,
            // invalidating the pointer.
            v->num += (opcode == tkincr) ? 1 : -1;
            push_val(v);
            // Now reverse the incr/decr on the top stack val.
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
        FILE *outfp = stdout;
        switch (outmode) {
          case tkgt: outfp = setup_outfile("f", "w"); break;
          case tkappend: outfp = setup_outfile("f", "a"); break;
          case tkpipe: outfp = setup_outfile("p", "w"); break;
          default: nargs++; break;
        }
        nargs--;
        if (opcode == tkprintf) {
          varprint(fprintf, outfp, nargs);
          drop_n(nargs);
          break;
        }
        if (!nargs) {
          val_to_str(&FIELD[0]);
          fprintf(outfp, "%s", FIELD[0].vst->str);
        } else {
          struct zvalue tempv = uninit_zvalue;
          zvalue_copy(&tempv, &STACK[OFS]);
          val_to_str(&tempv);
          for (int k = 0; k < nargs; k++) {
            if (k) fprintf(outfp, "%s", tempv.vst->str);
            int sp = stkptr - nargs + 1 + k;
            ////// FIXME refcnt -- prob. don't need to copy from stack?
            v = &STACK[sp];
            val_to_str_fmt(v, val_to_str(&STACK[OFMT])->vst->str);
            struct zstring *zs = v->vst;
            fprintf(outfp, "%s", zs ? zs->str : "");
          }
          zvalue_release_zstring(&tempv);
          drop_n(nargs);
        }
        fputs(ensure_str(&STACK[ORS])->vst->str, outfp);
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
      case tkfunction:    // function definition
        op2 = *ip++;    // func table num
        struct functab_slot *pfdef = &FUNC_DEF[op2];
        struct zlist *loctab = &pfdef->function_locals;
        int nparms = zlist_len(loctab)-1;

        nargs = popnumval();
        int newparmbase = stkptr - nargs;
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
          // For now, if it is converted to a scalar, we just abandon the map.
          // This is sloppy but this situation should be rare and the maybe-map
          // should (!) be empty.
          // See force_maybemap_to_scalar().
          struct symtab_slot *q = &((struct symtab_slot *)loctab->base)[nargs+1];
          vv = (struct zvalue)ZVINIT(q->flags, 0, 0);
          if (vv.flags == 0) {
            zvalue_map_init(&vv);
            vv.flags = ZF_MAYBEMAP;
          } else if (is_map(&vv)) {
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
        //     should be copied! STACK[parmbase+RETURN_VALUE] = STKP[0];
        zvalue_copy(&STACK[parmbase+RETURN_VALUE], STKP);
        drop();


        // Remove the local args (not supplied by caller) from stack, check to
        // release any map data created.
        while (stkptr > parmbase + nargs) {
          if ((STKP)->flags & ZF_ANYMAP) {
            zmap_delete_map_incl_slotdata((STKP)->map);
            xfree((STKP)->map);
          }
          drop();
        }
        while (stkptr > parmbase + RETURN_VALUE)
          drop();
        ip = &ZCODE[(int)STACK[parmbase+RETURN_ADDR].num];
        parmbase = STACK[parmbase+PREV_PARMBASE].num;
        break;

      case opprepcall:    // function call prep
        push_int_val(0);      // return value placeholder
        push_int_val(0);      // return addr
        push_int_val(0);      // parmbase
        push_int_val(0);      // arg count
        push_int_val(*ip++);  // function tbl ref
        break;

      case tkfunc:        // function call
        nargs = *ip++;
        newparmbase = stkptr - nargs;
        STACK[newparmbase+RETURN_ADDR].num = ip - &ZCODE[0];
        STACK[newparmbase+ARG_CNT].num = nargs;
        push_int_val(nargs);      // FIXME TODO pass this in a zregister?
        ip = &ZCODE[FUNC_DEF[(int)STACK[newparmbase+FUNCTION_NUM].num].zcode_addr];
        break;

      case tkrbracket:    // concat multiple map subscripts
        nsubscrs = *ip++;
        while (--nsubscrs) {
        swap();
        val_to_str(STKP);
        push_val(&STACK[SUBSEP]);
        val_to_str(STKP);
        STKP[-1].vst = zstring_extend(STKP[-1].vst, STKP->vst);
        drop();
        swap();
        val_to_str(STKP);
        STKP[-1].vst = zstring_extend(STKP[-1].vst, STKP->vst);
        drop();
        }
        break;

      case opmapdelete:
        k = STKP->num;
        if (k < 0) k = parmbase - k;    // loc of var on stack
        v = &STACK[k];
        force_maybemap_to_map(v);
        zmap_delete_map(v->map);
        drop();
        break;

      case tkdelete:
        k = STKP->num;
        if (k < 0) k = parmbase - k;    // loc of var on stack
        v = &STACK[k];
        force_maybemap_to_map(v);
        drop();
        val_to_str(STKP);
        zmap_delete(v->map, STKP->vst);
        drop();
        break;

      case opmap:
        op2 = *ip++;
        k = op2 < 0 ? parmbase - op2 : op2;
        v = &STACK[k];
        force_maybemap_to_map(v);
        if (!(is_map(v))) fatal("scalar in array context");
        v = get_map_val(v, STKP);
        drop();     // drop subscript
        push_val(v);
        break;

      case tkin:
        val_to_str(STKP-1);
        if (!(STKP->flags & ZF_ANYMAP)) fatal("scalar in array context");
        v = zmap_find(STKP->map, STKP[-1].vst);
        drop();
        drop();
        push_int_val(v ? 1 : 0);
        break;

      case opmapiternext:
        op2 = *ip++;
        v = STKP-1;
        force_maybemap_to_map(v);
        if (!(is_map(v))) fatal("scalar in array context");
        struct zmap *m = v->map;   // Need for MAPSLOT macro (in map.h)
        int zlen = zlist_len(&m->slot);
        int kk = STKP->num + 1;
        while (kk < zlen && !(MAPSLOT[kk].key)) // skip deleted slots
          kk++;
        STKP->num = kk; // save index for next iteration
        if (kk < zlen) {
          struct zvalue *var = setup_lvalue(stkptr-2, parmbase, &field_num);
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
        // can find either tkfield or tkvar at same place (ZCODE[zcode_last-1]).
        ip++; // skip dummy "operand" instruction field
        val_to_num(STKP);
        push_field((int)((STKP)->num));
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
        val_to_num(STKP);
        (STKP)->flags |= ZF_FIELDREF;
        ip++; // skip dummy "operand" instruction field
        break;

      case opprintrec:
        val_to_str(&FIELD[0]);
        puts(FIELD[0].vst->str);
        break;

      case oprange1:
        range_num = *ip++;
        op2 = *ip++;
        if (range_sw[range_num]) ip += op2;
        break;

      case oprange2:
        range_num = *ip++;
        op2 = *ip++;
        t = get_set_logical();  // FIXME only need to get, not set
        drop();
        if (t) range_sw[range_num] = 1;
        else ip += op2;
        break;

      case oprange3:
        range_num = *ip++;
        t = get_set_logical();  // FIXME only need to get, not set
        drop();
        if (t) range_sw[range_num] = 0;
        break;

      case tkexit:
        r = popnumval();
        if (r != NO_EXIT_STATUS) *status = (int)r & 255;
        // TODO FIXME do we need NO_EXIT_STATUS at all? Just use 0?
        return tkexit;

      case tknext:
        return tknext;

      case tknextfile:
        return tknextfile;

      case tkgetline:
        nargs = *ip++;
        int source = *ip++;
        // stack is:
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
        FILE *fp = 0;
        if (source == tklt || source == tkpipe) {
          fp = setup_file(source == tklt ? "f" : "p", "r");
          nargs--;
        }
        // now cases are:
        // nargs source  stack
        //  0 tkeof:   (nothing; plain getline) from current data file
        //  1 tkeof:   (lvalue)  from current data file
        //  0 tklt:    (nothing) from named file in 'stream'
        //  1 tklt:    (lvalue)  from  named file in 'stream'
        //  0 tkpipe:  (nothing) from piped command in 'stream'
        //  1 tkpipe:  (lvalue)  from piped command in 'stream'
        v = nargs ? setup_lvalue(stkptr, parmbase, &field_num) : 0;
        if (v) drop();


        // source is tkeof (no pipe/file), tklt (file), or tkpipe (pipe)
        // stream is name of file or pipe
        // v is NULL or an lvalue ref
        push_int_val(awk_getline(source, fp, v));

        // fake return value for now
        break;

        ////// builtin functions ///////


      case tksplit:
        nargs = *ip++;
        if (nargs == 2) push_val(&STACK[FS]);
        struct zstring *s = val_to_str(STKP-2)->vst;
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
        val_to_str(STKP-1);
        if (!(is_rx(STKP))) val_to_str(STKP);
        regex_t rx_pat, *rxp = &rx_pat;
        rx_zvalue_compile(&rxp, STKP);
        regoff_t rso, reo;
        k = rx_find(rxp, STKP[-1].vst->str, &rso, &reo, 0);
        rx_zvalue_free(rxp, STKP);
        // Force these to num before setting.
        val_to_num(&STACK[RSTART]);
        val_to_num(&STACK[RLENGTH]);
        if (k) STACK[RSTART].num = 0, STACK[RLENGTH].num = -1;
        else STACK[RSTART].num = rso + 1, STACK[RLENGTH].num = reo - rso;
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
        struct zstring *zz = val_to_str(&STACK[stkptr-nargs+1])->vst;
        // Offset of start of string; convert 1-based to 0-based
        ssize_t mm = clamp(trunc(val_to_num(&STACK[stkptr-nargs+2]))-1, 0, zz->size);
        ssize_t nn = zz->size - mm;   // max possible substring length
        if (nargs == 3) nn = clamp(trunc(val_to_num(STKP)), 0, nn);
        struct zstring *zzz = new_zstring(zz->str + mm, nn);
        zstring_release(&STACK[stkptr-nargs+1].vst);
        STACK[stkptr-nargs+1].vst = zzz;
        drop_n(nargs - 1);
        break;

      case tkindex:
        nargs = *ip++;
        char *s1 = val_to_str(STKP-1)->vst->str;
        char *s3 = strstr(s1, val_to_str(STKP)->vst->str);
        ptrdiff_t offs = s3 ? s3 - s1 + 1 : 0;
        drop();
        drop();
        push_int_val(offs);
        break;

      case tktolower:
      case tktoupper:
        nargs = *ip++;
        int (*f)(int) = opcode == tktolower ? (tolower) : (toupper);
        val_to_str(STKP);
        // Need to dup the string to not modify original.
        zvalue_dup_zstring(STKP);
        struct zstring *z = STKP->vst;
        char *p = z->str, *e = z->str + z->size;
        for (; p < e; p++) *p = f(*p);
        break;

      case tklength:
        nargs = *ip++;
        v = nargs ? STKP : &FIELD[0];
        force_maybemap_to_map(v);
        if (is_map(v)) k = v->map->count - v->map->deleted;
        else k = val_to_str(v)->vst->size;
        if (nargs) drop();
        push_int_val(k);
        break;

      case tksystem:
        nargs = *ip++;
        fflush(stdout);
        fflush(stderr);
        r = system(val_to_str(STKP)->vst->str);
#ifdef WEXITSTATUS
        // WEXITSTATUS is in sys/wait.h, but I'm not including that.
        // It seems to also be in stdlib.h in gcc and musl-gcc.
        // No idea how portable this is!
        r = r >= 256 ? WEXITSTATUS(r) : r + 256;
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
        r = close_file();
        drop();
        push_int_val(r);
        break;

      case tksprintf:
        nargs = *ip++;
        zstring_release(&rgl.zspr);
        rgl.zspr = new_zstring("", 0);
        varprint(fsprintf, 0, nargs);
        drop_n(nargs);
        vv = (struct zvalue)ZVINIT(ZF_STR, 0, rgl.zspr);
        push_val(&vv);
        break;

      default:
        if (tkatan2 <= opcode && opcode <= tksrand) {
          math_builtin(opcode, *ip++);  // 2nd arg is number of args in call
          break;
        }

        fprintf(stderr, "!!! Unimplemented opcode %d\n", opcode);
        exit(127);
    }
  }
  return opquit;
}

// interp() wraps the main interpreter loop interpx(). The main purpose
// is to allow the stack to be readjusted after an 'exit' from a function.
// Also catches errors, as the normal operation should leave the stack
// depth unchanged after each run through the rules.
static int interp(int start, int *status)
{
  int stkptrbefore = stkptr;
  int r = interpx(start, status);
  // If exit from function, stack will be loaded with args etc. Clean it.
  if (r == tkexit) {
    stack.avail -= (stkptr - stkptrbefore) * stack.size;
    stkptr = stkptrbefore;
  }
  if (stkptr - stkptrbefore)
    fprintf(stderr, "!! stack pointer offset: %d\n", stkptr - stkptrbefore);
  return r;
}

static void insert_argv_map(struct zvalue *map, int key, char *value)
{
  struct zvalue zkey = ZVINIT(ZF_STR, 0, num_to_zstring(key, ensure_str(&STACK[CONVFMT])->vst->str));
  struct zvalue *v = get_map_val(map, &zkey);
  zvalue_release_zstring(&zkey);
  zvalue_release_zstring(v);
  *v = new_str_val(value);
  check_numeric_string(v);
}

static void init_globals(int optind, int argc, char **argv, char *sepstring, int num_assignments, char **assignments, char **envp)
{
  // Global variables reside at the bottom of the stack. Start with the awk
  // "special variables":  ARGC, ARGV, CONVFMT, ENVIRON, FILENAME, FNR, FS, NF,
  // NR, OFMT, OFS, ORS, RLENGTH, RS, RSTART, SUBSEP

  STACK[CONVFMT] = new_str_val("%.6g");
  // Init ENVIRON map.
  struct zvalue m = ZVINIT(ZF_MAP, 0, 0);
  zvalue_map_init(&m);
  STACK[ENVIRON] = m;
  for (char **pkey = envp; *pkey; pkey++) {
    char *pval = strchr(*pkey, '=');
    if (!pval) continue;
    *pval++ = '\0';
    struct zvalue zkey = ZVINIT(ZF_STR, 0, new_zstring(*pkey, strlen(*pkey)));
    struct zvalue *v = get_map_val(&m, &zkey);
    zstring_release(&zkey.vst);
    if (v->vst) ffatal("env var dup? (%s)", pkey);
    *v = new_str_val(pval);    // FIXME refcnt
    check_numeric_string(v);
  }

  // Init ARGV map.
  m = (struct zvalue)ZVINIT(ZF_MAP, 0, 0);
  zvalue_map_init(&m);
  STACK[ARGV] = m;
  insert_argv_map(&m, 0, progname);
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
  // Push global variables on the stack at offsets matching their index in the
  // global var table.  In the global var table we may have the type as scalar
  // or map if it is used as such in the program. In that case we init the
  // pushed arg from the type of the globals table.
  // But if a global var appears only as a bare arg in a function call it will
  // not be typed in the globals table. In that case we can only say it "may be"
  // a map, but we have to assume the possibility and attach a map to the
  // var. When/if the var is used as a map or scalar in the called function it
  // will be converted to a map or scalar as required.
  // For now, if it is converted to a scalar, we just abandon the map.  This is
  // sloppy but this situation should be rare and the maybe-map should (!) be
  // empty.
  // See force_maybemap_to_scalar(), and the similar comment in
  // 'case tkfunction:' above.
  //
  int gstx, len = zlist_len(&globals_table);
  for (gstx = spec_var_limit; gstx < len; gstx++) {
    struct symtab_slot gs = GLOBAL[gstx];
    struct zvalue v = ZVINIT(gs.flags, 0, 0);
    if (v.flags == 0) {
      zvalue_map_init(&v);
      v.flags = ZF_MAYBEMAP;
    } else if (is_map(&v)) {
      zvalue_map_init(&v);
    } else {
      // Set SCALAR flag 0 to create "uninitialized" scalar.
      v.flags = 0;
    }
    push_val(&v);
  }

  // Init -v assignment options.
  for (int k = 0; k < num_assignments; k++) {
    char *asgn = assignments[k];
    char *val = strchr(asgn, '=');
    if (!val) error_exit("bad -v assignment format\n");
    *val++ = '\0';
    assign_global(asgn, val);
  }

  rgl.cur_arg = new_str_val("<cmdline>");
  uninit_string_zvalue = new_str_val("");
  zvalue_copy(&FIELD[0], &uninit_string_zvalue);
}

static void run_files(int *status)
{
  int r = 0;
  while (r != tkexit && *status < 0 && getrec_f0() >= 0)
    if ((r = interp(cgl.first_recrule, status)) == tknextfile) next_fp();
}

static void free_literal_regex(void)
{
  int len = zlist_len(&literals);
  for (int k = 1; k < len; k++)
    if (is_rx(&LITERAL[k])) regfree(LITERAL[k].rx);
}

EXTERN void run(int optind, int argc, char **argv, char *sepstring, int num_assignments, char **assignments, char **envp)
{
  init_globals(optind, argc, argv, sepstring, num_assignments, assignments, envp);
  init_field_rx();
  rx_compile_or_die(&rx_printf_fmt, printf_fmt_rx);
  setup_std_file("-", stdin, "r");
  setup_std_file("/dev/stdin", stdin, "r");
  setup_std_file("/dev/stdout", stdout, "w");
  setup_std_file("/dev/stderr", stderr, "w");
  seed_jkiss32(123);
  int status = -1, r = 0;
  if (cgl.first_begin) r = interp(cgl.first_begin, &status);
  if (r != tkexit)
    if (cgl.first_recrule) run_files(&status);
  if (cgl.first_end) r = interp(cgl.first_end, &status);
  regfree(&rx_printf_fmt);
  free_field_rx();
  free_literal_regex();
  xfree(rgl.recbuf);
  xfree(rgl.recbuf_multiline);
  if (status >= 0) exit(status);
}
