// common.c
// Copyright 2023 Ray Gardner
// vi: tabstop=2 softtabstop=2 shiftwidth=2

#include "common.h"

////////////////////
//// common defs
////////////////////

EXTERN zlist globals_table;  // global symbol table
EXTERN zlist locals_table;  // local symbol table
EXTERN zlist func_def_table;  // function symbol table

EXTERN zlist literals;
EXTERN zlist fields;
EXTERN zlist zcode;
EXTERN zlist stack;

EXTERN char *ops = " ;  ,  [  ]  (  )  {  }  $  ++ -- ^  !  *  /  %  +  -  .. "
        "<  <= != == >  >= ~  !~ && || ?  :  ^= %= *= /= += -= =  >> |  ";

EXTERN char *keywords = " in        BEGIN     END       if        else      "
    "while     for       do        break     continue  exit      function  "
    "return    next      nextfile  delete    print     printf    getline   ";

EXTERN char *builtins = " atan2     cos       sin       exp       log       "
    "sqrt      int       rand      srand     length    "
    "tolower   toupper   system    fflush    "
    "close     index     match     split     "
    "sub       gsub      sprintf   substr    ";

EXTERN char *progname;
EXTERN struct compiler_globals cgl;
// Some global variables:
EXTERN int spec_var_limit = 0; // used in compile.h and run.h
EXTERN int stkptr = 0;         // used in run.h and (once) in compile.h (& dumputils.h)
EXTERN int zcode_last = 0;     // used in common.h and compile.h
// END global variables

EXTERN void zzerr(char *fn, int lnum, char *format, ...)
{
  va_list args;
  cgl.compile_error_count++;
  (void)fn; (void)lnum;
  fprintf(stderr, "%s: file %s line %d: ", progname, scs->filename, scs->line_num);
  va_start(args, format);
  vfprintf(stderr, format, args);
  va_end(args);
  fflush(stderr);
}


EXTERN void zzfatal(char *fn, int lnum, char *format, ...)
{
  va_list args;
  (void)fn; (void)lnum;
  fprintf(stderr, "%s: file %s line %d: ", progname, scs->filename, scs->line_num);
  va_start(args, format);
  vfprintf(stderr, format, args);
  va_end(args);
  fprintf(stderr, "%s: exit on fatal error\n", progname);
  fflush(stderr);
  exit(42);
}

// Used in compile.h but mostly in run.h
EXTERN void get_token_text(char *op, int tk)
{
  // This MUST ? be changed if ops string or tk... assignments change!
  memmove(op, ops + 3 * (tk - tksemi) + 1, 2);
  op[ op[1] == ' ' ? 1 : 2 ] = 0;
}

////////////////////
////   zlist
////////////////////

static zlist *zlist_initx(zlist *p, size_t size, size_t count)
{
  p->base = p->avail = xzalloc(count * size);
  p->limit = p->base + size * count;
  p->size = size;
  return p;
}

EXTERN zlist *zlist_init(zlist *p, size_t size)
{
#define SLIST_MAX_INIT_BYTES 128
  return zlist_initx(p, size, SLIST_MAX_INIT_BYTES / size);
}

static void zlist_expand(zlist *p)
{
  size_t offset = p->avail - p->base;
  size_t cap = p->limit - p->base;
  size_t newcap = MAX(cap + p->size, ((cap / p->size) * 3 / 2) * p->size);
  if (newcap <= cap)
    error_exit("bad memory request.\n");
  char *base = xrealloc(p->base, newcap);
  p->base = base;
  p->limit = base + newcap;
  p->avail = base + offset;
}

EXTERN size_t zlist_append(zlist *p, void *obj)
{
  // Insert obj (p->size bytes) at end of list, expand as needed.
  // Return scaled offset to newly inserted obj; i.e. the
  // "slot number" 0, 1, 2,...
  void *objtemp = NULL;
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

EXTERN int zlist_len(zlist *p)
{
  return (p->avail - p->base) / p->size;
}


////////////////////
////   zstring
////////////////////



EXTERN void zstring_release(zstring **s)
{
  if (*s && (**s).refcnt-- == 0) xfree(*s); //free_zstring(s);
  *s = NULL;
}

EXTERN void zstring_incr_refcnt(zstring *s)
{
  if (s) s->refcnt++;
}

EXTERN zstring *new_zstring_cap(int capacity)
{
  zstring *z = xzalloc(sizeof(*z) + capacity);
  z->capacity = capacity;
  return z;
}

// !! Use only if 'to' is NULL or its refcnt is 0.
static zstring *zstring_modify(zstring *to, size_t at, char *s, size_t n)
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
EXTERN zstring *zstring_update(zstring *to, size_t at, char *s, size_t n)
{
  if (to && to->refcnt) {
    zstring *to_before = to;
    to = zstring_modify(NULL, 0, to->str, to->size);
    zstring_release(&to_before);
  }
  return zstring_modify(to, at, s, n);
}

EXTERN zstring *zstring_copy(zstring *to, zstring *from)
{
  return zstring_update(to, 0, from->str, from->size);
}

EXTERN zstring *zstring_extend(zstring *to, zstring *from)
{
  return zstring_update(to, to->size, from->str, from->size);
}

EXTERN zstring *new_zstring(char *s, size_t size)
{
  return zstring_modify(NULL, 0, s, size);
}

////////////////////
////   zvalue
////////////////////


EXTERN zvalue uninit_zvalue = ZVINIT(0, 0.0, 0);

// This will be reassigned in init_globals() with an empty string.
// It's a special value used for "uninitialized" field vars
// referenced past $NF. See push_field().
EXTERN zvalue uninit_string_zvalue = ZVINIT(0, 0.0, 0);

EXTERN zvalue new_str_val(char *s)
{
  // Only if no nul inside string!
  zvalue v = ZVINIT(ZF_STR, 0.0, new_zstring(s, strlen(s)));
  return v;
}

EXTERN void zvalue_release_zstring(zvalue *v)
{
  if (v && ! (v->flags & (ZF_ANYMAP | ZF_RX))) zstring_release(&v->vst);
}

static size_t zlist_append_zvalue(zlist *p, zvalue *v)
{
  zvalue vtemp;
  if (p->avail > p->limit - sizeof(*v)) {
    vtemp = *v;
    v = &vtemp;
    zlist_expand(p);
  }
  *(zvalue *)p->avail = *v;
  p->avail += p->size;
  return (p->avail - p->base - p->size) / p->size;  // offset of updated slot
}

// push_val() is used for initializing globals (see compile:init_compiler())
// but mostly used in run.h
// WARNING: push_val may change location of v, so do NOT depend on it after!
// Note the incr refcnt used to be after the zlist_append, but that caused a
// heap-use-after-free error when the zlist_append relocated the zvalue being
// pushed, invalidating the v pointer.
EXTERN void push_val(zvalue *v)
{
  if (is_str(v)) zstring_incr_refcnt(v->vst);
  stkptr = zlist_append_zvalue(&stack, v);
}

EXTERN void zvalue_copy(zvalue *to, zvalue *from)
{
  if (from->flags & ZF_ANYMAP) fatal("attempt to copy array var");
  if (is_rx(from)) *to = *from;
  else {
    zvalue_release_zstring(to);
    *to = *from;
    zstring_incr_refcnt(to->vst);
  }
}

EXTERN void zvalue_dup_zstring(zvalue *v)
{
  zstring *z = new_zstring(v->vst->str, v->vst->size);
  zstring_release(&v->vst);
  v->vst = z;
}

////////////////////
////   zmap (array) implementation
////////////////////


EXTERN int zstring_match(zstring *a, zstring *b)
{
  return a->size == b->size && memcmp(a->str, b->str, a->size) == 0;
}

static int zstring_hash(zstring *s)
{   // djb2 -- small, fast, good enough for this
  unsigned h = 5381;
  for (size_t k = 0; k < s->size; k++)
    h = ((h << 5) + h) + s->str[k];
  return h;
}

enum { PSHIFT = 5 };  // "perturb" shift -- see find_mapslot() below

static zmapslot *find_mapslot(zmap *m, zstring *key, int *hash, int *probe)
{
  zmapslot *x = NULL;
  *hash = zstring_hash(key);
  unsigned perturb = *hash;
  *probe = *hash & m->mask;
  int n;
  while ((n = m->hash[*probe])) {
    if (n > 0) {
      x = &MAPSLOT[n-1];
      if (*hash == x->hash && zstring_match(key, x->key)) {
        return x;
      }
    }
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
  return NULL;
}

EXTERN zvalue *zmap_find(zmap *m, zstring *key)
{
  int hash, probe;
  zmapslot *x = find_mapslot(m, key, &hash, &probe);
  return x ? &x->val : NULL;
}

static void zmap_init(zmap *m)
{
  enum {INIT_SIZE = 8};
  m->mask = INIT_SIZE - 1;
  m->hash = xzalloc(INIT_SIZE * sizeof(*m->hash));
  m->limit = INIT_SIZE * 8 / 10;
  m->count = 0;
  m->deleted = 0;
  zlist_init(&m->slot, sizeof(zmapslot));
}

EXTERN void zvalue_map_init(zvalue *v)
{
  zmap *m = xmalloc(sizeof(*m));
  zmap_init(m);
  v->map = m;
  v->flags |= ZF_MAP;
}

EXTERN void zmap_delete_map_incl_slotdata(zmap *m)
{
  for (zmapslot *p = &MAPSLOT[0]; p < &MAPSLOT[zlist_len(&m->slot)]; p++) {
    if (p->key) zstring_release(&p->key);
    if (p->val.vst) zstring_release(&p->val.vst);
  }
  xfree(m->slot.base);
  xfree(m->hash);
}

EXTERN void zmap_delete_map(zmap *m)
{
  zmap_delete_map_incl_slotdata(m);
  zmap_init(m);
}


static void zmap_rehash(zmap *m)
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



EXTERN zmapslot *zmap_find_or_insert_key(zmap *m, zstring *key)
{
  int hash, probe;
  zmapslot *x = find_mapslot(m, key, &hash, &probe);
  if (x) return x;
  // not found; insert it.
  if (m->count == m->limit) {
    zmap_rehash(m);         // rehash if getting too full.
    // rerun find_mapslot to get new probe index
    x = find_mapslot(m, key, &hash, &probe);
  }
  // Assign key to new slot entry and bump refcnt.
  zmapslot zs = ZMSLOTINIT(hash, key, (zvalue)ZVINIT(0, 0.0, 0));
  zstring_incr_refcnt(key);
  int n = zlist_append(&m->slot, &zs);
  m->count++;
  m->hash[probe] = n + 1;
  return &MAPSLOT[n];
}

EXTERN void zmap_delete(zmap *m, zstring *key)
{
  int hash, probe;
  zmapslot *x = find_mapslot(m, key, &hash, &probe);
  if (! x) return;
  zstring_release(&MAPSLOT[m->hash[probe] - 1].key);
  m->hash[probe] = -1;
  m->deleted++;
}
