// common.c
// Copyright 2024 Ray Gardner
// License: 0BSD
// vi: tabstop=2 softtabstop=2 shiftwidth=2

#include "common.h"

////////////////////
//// common defs
////////////////////
#ifndef FOR_TOYBOX

// Global data
EXTERN struct global_data TT;
EXTERN struct optflags optflags;
#endif  // FOR_TOYBOX

// These (ops, keywords, builtins) must align with enum tokens
EXTERN char *ops = " ;  ,  [  ]  (  )  {  }  $  ++ -- ^  !  *  /  %  +  -  .. "
        "<  <= != == >  >= ~  !~ && || ?  :  ^= %= *= /= += -= =  >> |  ";

EXTERN char *keywords = " in        BEGIN     END       if        else      "
    "while     for       do        break     continue  exit      function  "
    "return    next      nextfile  delete    print     printf    getline   ";

EXTERN char *builtins = " atan2     cos       sin       exp       log       "
    "sqrt      int       rand      srand     length    "
    "tolower   toupper   system    fflush    "
    "and       or        xor       lshift    rshift    "
    "close     index     match     split     "
    "sub       gsub      sprintf   substr    ";

EXTERN void zzerr(char *format, ...)
{
  va_list args;
  int fatal_sw = 0;
  fprintf(stderr, "%s: ", TT.progname);
  if (format[0] == '$') {
    fprintf(stderr, "FATAL: ");
    format++;
    fatal_sw = 1;
  }
  fprintf(stderr, "file %s line %d: ", TT.scs->filename, TT.scs->line_num);
  va_start(args, format);
  vfprintf(stderr, format, args);
  va_end(args);
  if (format[strlen(format)-1] != '\n') fputc('\n', stderr); // TEMP FIXME !!!
  fflush(stderr);
  if (fatal_sw) exit(2);
        // Don't bump error count for warnings
  else if (!strstr(format, "arning")) TT.cgl.compile_error_count++;
}

EXTERN void get_token_text(char *op, int tk)
{
  // This MUST ? be changed if ops string or tk... assignments change!
  memmove(op, ops + 3 * (tk - tksemi) + 1, 2);
  op[ op[1] == ' ' ? 1 : 2 ] = 0;
}

////////////////////
/// UTF-8
////////////////////

// Return number of bytes in 'cnt' utf8 codepoints
EXTERN int bytesinutf8(char *str, size_t len, size_t cnt)
{
  if (FLAG(b)) return cnt;
  unsigned wch;
  char *lim = str + len, *s0 = str;
  while (cnt-- && str < lim) {
    int r = utf8towc(&wch, str, lim - str);
    str += r > 0 ? r : 1;
  }
  return str - s0;
}

// Return number of utf8 codepoints in str
EXTERN int utf8cnt(char *str, size_t len)
{
  unsigned wch;
  int cnt = 0;
  char *lim;
  if (!len || FLAG(b)) return len;
  for (lim = str + len; str < lim; cnt++) {
    int r = utf8towc(&wch, str, lim - str);
    str += r > 0 ? r : 1;
  }
  return cnt;
}

////////////////////
////   zlist
////////////////////

EXTERN struct zlist *zlist_initx(struct zlist *p, size_t size, size_t count)
{
  p->base = p->avail = xzalloc(count * size);
  p->limit = p->base + size * count;
  p->size = size;
  return p;
}

EXTERN struct zlist *zlist_init(struct zlist *p, size_t size)
{
#define SLIST_MAX_INIT_BYTES 128
  return zlist_initx(p, size, SLIST_MAX_INIT_BYTES / size);
}

// This is called from zlist_append() and add_stack() in run
EXTERN void zlist_expand(struct zlist *p)
{
  size_t offset = p->avail - p->base;
  size_t cap = p->limit - p->base;
  size_t newcap = maxof(cap + p->size, ((cap / p->size) * 3 / 2) * p->size);
  if (newcap <= cap) error_exit("mem req error");
  char *base = xrealloc(p->base, newcap);
  p->base = base;
  p->limit = base + newcap;
  p->avail = base + offset;
}

EXTERN size_t zlist_append(struct zlist *p, void *obj)
{
  // Insert obj (p->size bytes) at end of list, expand as needed.
  // Return scaled offset to newly inserted obj; i.e. the
  // "slot number" 0, 1, 2,...
  void *objtemp = 0;
  if (p->avail > p->limit - p->size) {
    objtemp = xmalloc(p->size);     // Copy obj in case it is in
    memmove(objtemp, obj, p->size); // the area realloc might free!
    obj = objtemp;
    zlist_expand(p);
  }
  memmove(p->avail, obj, p->size);
  if (objtemp) xfree(objtemp);
  p->avail += p->size;
  return (p->avail - p->base - p->size) / p->size;  // offset of updated slot
}

EXTERN int zlist_len(struct zlist *p)
{
  return (p->avail - p->base) / p->size;
}

////////////////////
////   zstring
////////////////////

EXTERN void zstring_release(struct zstring **s)
{
  if (*s && (**s).refcnt-- == 0) xfree(*s); //free_zstring(s);
  *s = 0;
}

EXTERN void zstring_incr_refcnt(struct zstring *s)
{
  if (s) s->refcnt++;
}

// !! Use only if 'to' is NULL or its refcnt is 0.
static struct zstring *zstring_modify(struct zstring *to, size_t at, char *s, size_t n)
{
  size_t cap = at + n + 1;
  if (!to || to->capacity < cap) {
    to = xrealloc(to, sizeof(*to) + cap);
    to->capacity = cap;
    to->refcnt = 0;
  }
  memcpy(to->str + at, s, n);
  to->size = at + n;
  to->str[to->size] = '\0';
  return to;
}

// The 'to' pointer may move by realloc, so return (maybe updated) pointer.
// If refcnt is nonzero then there is another pointer to this zstring,
// so copy this one and release it. If refcnt is zero we can mutate this.
EXTERN struct zstring *zstring_update(struct zstring *to, size_t at, char *s, size_t n)
{
  if (to && to->refcnt) {
    struct zstring *to_before = to;
    to = zstring_modify(0, 0, to->str, to->size);
    zstring_release(&to_before);
  }
  return zstring_modify(to, at, s, n);
}

EXTERN struct zstring *zstring_copy(struct zstring *to, struct zstring *from)
{
  return zstring_update(to, 0, from->str, from->size);
}

EXTERN struct zstring *zstring_extend(struct zstring *to, struct zstring *from)
{
  return zstring_update(to, to->size, from->str, from->size);
}

EXTERN struct zstring *new_zstring(char *s, size_t size)
{
  return zstring_modify(0, 0, s, size);
}

////////////////////
////   zvalue
////////////////////

EXTERN struct zvalue uninit_zvalue = ZVINIT(0, 0.0, 0);

// This will be reassigned in init_globals() with an empty string.
// It's a special value used for "uninitialized" field vars
// referenced past $NF. See push_field().
EXTERN struct zvalue uninit_string_zvalue = ZVINIT(0, 0.0, 0);

EXTERN struct zvalue new_str_val(char *s)
{
  // Only if no nul inside string!
  struct zvalue v = ZVINIT(ZF_STR, 0.0, new_zstring(s, strlen(s)));
  return v;
}

EXTERN void zvalue_release_zstring(struct zvalue *v)
{
  if (v && ! (v->flags & (ZF_ANYMAP | ZF_RX))) zstring_release(&v->vst);
}

// push_val() is used for initializing globals (see init_compiler())
// but mostly used in runtime
// WARNING: push_val may change location of v, so do NOT depend on it after!
// Note the incr refcnt used to be after the zlist_append, but that caused a
// heap-use-after-free error when the zlist_append relocated the zvalue being
// pushed, invalidating the v pointer.
EXTERN void push_val(struct zvalue *v)
{
  if (IS_STR(v) && v->vst) v->vst->refcnt++;  // inlined zstring_incr_refcnt()
  *++TT.stackp = *v;
}

EXTERN void zvalue_copy(struct zvalue *to, struct zvalue *from)
{
  if (IS_RX(from)) *to = *from;
  else {
    zvalue_release_zstring(to);
    *to = *from;
    zstring_incr_refcnt(to->vst);
  }
}

EXTERN void zvalue_dup_zstring(struct zvalue *v)
{
  struct zstring *z = new_zstring(v->vst->str, v->vst->size);
  zstring_release(&v->vst);
  v->vst = z;
}

////////////////////
////   zmap (array) implementation
////////////////////

EXTERN int zstring_match(struct zstring *a, struct zstring *b)
{
  return a->size == b->size && memcmp(a->str, b->str, a->size) == 0;
}

static int zstring_hash(struct zstring *s)
{   // djb2 -- small, fast, good enough for this
  unsigned h = 5381;
  char *p = s->str, *lim = p + s->size;
  while (p < lim)
    h = (h << 5) + h + *p++;
  return h;
}

enum { PSHIFT = 5 };  // "perturb" shift -- see find_mapslot() below

static struct zmap_slot *find_mapslot(struct zmap *m, struct zstring *key, int *hash, int *probe)
{
  struct zmap_slot *x = 0;
  unsigned perturb = *hash = zstring_hash(key);
  *probe = *hash & m->mask;
  int n, first_deleted = -1;
  while ((n = m->hash[*probe])) {
    if (n > 0) {
      x = &MAPSLOT[n-1];
      if (*hash == x->hash && zstring_match(key, x->key)) {
        return x;
      }
    } else if (first_deleted < 0) first_deleted = *probe;
    // Based on technique in Python dict implementation. Comment there
    // (https://github.com/python/cpython/blob/3.10/Objects/dictobject.c)
    // says
    //
    // j = ((5*j) + 1) mod 2**i
    // For any initial j in range(2**i), repeating that 2**i times generates
    // each int in range(2**i) exactly once (see any text on random-number
    // generation for proof).
    //
    // The addition of 'perturb' greatly improves the probe sequence. See
    // the Python dict implementation for more details.
    *probe = (*probe * 5 + 1 + (perturb >>= PSHIFT)) & m->mask;
  }
  if (first_deleted >= 0) *probe = first_deleted;
  return 0;
}

EXTERN struct zvalue *zmap_find(struct zmap *m, struct zstring *key)
{
  int hash, probe;
  struct zmap_slot *x = find_mapslot(m, key, &hash, &probe);
  return x ? &x->val : 0;
}

static void zmap_init(struct zmap *m)
{
  enum {INIT_SIZE = 8};
  m->mask = INIT_SIZE - 1;
  m->hash = xzalloc(INIT_SIZE * sizeof(*m->hash));
  m->limit = INIT_SIZE * 8 / 10;
  m->count = 0;
  m->deleted = 0;
  zlist_init(&m->slot, sizeof(struct zmap_slot));
}

EXTERN void zvalue_map_init(struct zvalue *v)
{
  struct zmap *m = xmalloc(sizeof(*m));
  zmap_init(m);
  v->map = m;
  v->flags |= ZF_MAP;
}

EXTERN void zmap_delete_map_incl_slotdata(struct zmap *m)
{
  for (struct zmap_slot *p = &MAPSLOT[0]; p < &MAPSLOT[zlist_len(&m->slot)]; p++) {
    if (p->key) zstring_release(&p->key);
    if (p->val.vst) zstring_release(&p->val.vst);
  }
  xfree(m->slot.base);
  xfree(m->hash);
}

EXTERN void zmap_delete_map(struct zmap *m)
{
  zmap_delete_map_incl_slotdata(m);
  zmap_init(m);
}

static void zmap_rehash(struct zmap *m)
{
  // New table is twice the size of old.
  int size = m->mask + 1;
  unsigned mask = 2 * size - 1;
  int *h = xzalloc(2 * size * sizeof(*m->hash));
  // Step through the old hash table, set up location in new table.
  for (int i = 0; i < size; i++) {
    int n = m->hash[i];
    if (n > 0) {
      int hash = MAPSLOT[n-1].hash;
      unsigned perturb = hash;
      int p = hash & mask;
      while (h[p]) {
        p = (p * 5 + 1 + (perturb >>= PSHIFT)) & mask;
      }
      h[p] = n;
    }
  }
  m->mask = mask;
  xfree(m->hash);
  m->hash = h;
  m->limit = 2 * size * 8 / 10;
}

EXTERN struct zmap_slot *zmap_find_or_insert_key(struct zmap *m, struct zstring *key)
{
  int hash, probe;
  struct zmap_slot *x = find_mapslot(m, key, &hash, &probe);
  if (x) return x;
  // not found; insert it.
  if (m->count == m->limit) {
    zmap_rehash(m);         // rehash if getting too full.
    // rerun find_mapslot to get new probe index
    x = find_mapslot(m, key, &hash, &probe);
  }
  // Assign key to new slot entry and bump refcnt.
  struct zmap_slot zs = ZMSLOTINIT(hash, key, (struct zvalue)ZVINIT(0, 0.0, 0));
  zstring_incr_refcnt(key);
  int n = zlist_append(&m->slot, &zs);
  m->count++;
  m->hash[probe] = n + 1;
  return &MAPSLOT[n];
}

EXTERN void zmap_delete(struct zmap *m, struct zstring *key)
{
  int hash, probe;
  struct zmap_slot *x = find_mapslot(m, key, &hash, &probe);
  if (!x) return;
  zstring_release(&MAPSLOT[m->hash[probe] - 1].key);
  m->hash[probe] = -1;
  m->deleted++;
}
