/* Copyright(C) 2000, Swedish Institute of Computer Science */
/************************************************************/
/* Profile management for cumulative and sweep algorithms.  */
/************************************************************/

#include "fd.h"

#if MULTI_SP_AWARE
#define profile_part(A1,A2,A3,A4) profile_part(HIDDEN_ARG, A1,A2,A3,A4)
#endif /* MULTI_SP_AWARE */



static struct {
  int a;
  int b;
  int c;
} crand;

static double zx = 1.0/30269.0;
static double zy = 1.0/30307.0;
static double zz = 1.0/30323.0;

static void creset(void)
{
  crand.a = 27134;
  crand.b = 9213;
  crand.c = 17773;
}

static double crandom(void)
{
  int x, y, z;
  double t;
  
  x = (crand.a*171) % 30269;
  y = (crand.b*172) % 30307;
  z = (crand.c*170) % 30323;
  t = x*zx + y*zy + z*zz;
  crand.a = x;
  crand.b = y;
  crand.c = z;
  return t-(int)t;
}    



#undef  PROFILE_STATISTICS

#if PROFILE_STATISTICS
static int profile_stat[1000];

static void clear_profile_stat(void)
{
  int i;

  for (i=0; i<1000; i++)
    profile_stat[i] = 0;
}


static void update_profile_stat(struct profile *p)
{
  int i;

  for (i=0; p; i++) {
    p = p->next;
  }
  
  profile_stat[i]++;
  /* printf("profile_length(%d).\n", i); */
}

#endif

void init_profile MAGIC (HIDDEN_PROTO_VOID)
{
  fd.profile_pool = NULL;
  creset();
}


struct profile *empty_profile(void)
{
#if PROFILE_STATISTICS
  update_profile_stat(NULL);
#endif
  return NULL;
}


/* Allocating a new segment */
struct profile *profile_cons MAGIC (HIDDEN_PROTO
				    long begin,
				    long end, /* begin < end */
				    long erg, /* erg > 0 */
				    struct profile *next) 
/* next==NULL || next->begin > end || next->erg != erg */
{
  struct profile *cons = fd.profile_pool;
  
  if (cons)
    fd.profile_pool = cons->next;
  else
    cons = Calloc(1,struct profile);

  cons->begin = begin;
  cons->end = end;
  cons->erg = erg;
  cons->next = next;
  return cons;
}


/* true if two segments overlap */
#define OVERLAP(B1,E1,B2,E2) \
((E1) > (B2) && (E2) > (B1)) \

/* Coalesce or else appending a new segment to tail of profile */
#define profile_emit(B,E,Y) \
      if (tail->end==(B) && tail->erg==(Y)) { \
        tail->end = (E); \
      } else if (Y) { \
      tail->next = profile_cons((B), (E), (Y), NULL); \
      tail = tail->next; \
      } \

#define profile_emit_fast(B,E,Y) { \
      tail->next = profile_cons((B), (E), (Y), NULL); \
      tail = tail->next; \
      } \


/* Compute the temporal relation between two segments. */
static INLINE unsigned int profile_cmp(long b1, long e1,
				       long b2, long e2)
{
  if (b1 < b2) {
    if (e1 <= b2)
      return FD_BEFORE;		/* or MEETS */
    else if (e1 < e2)
      return FD_OVERLAPS;
    else if (e1 == e2)
      return FD_FINISHED_BY;
    else return FD_CONTAINS;
  } else if (b1 == b2) {
    if (e1 < e2)
      return FD_STARTS;
    else if (e1 == e2)
      return FD_EQUALS;
    else
      return FD_STARTED_BY;
  } else {
    if (e1 < e2)
      return FD_DURING;
    else if (e1 == e2)
      return FD_FINISHES;
    else if (b1 < e2)
      return FD_OVERLAPPED_BY;
    else
      return FD_AFTER;		/* or MET_BY */
  }
}


/* Disposing of a list of segment */
void profile_dispose MAGIC (HIDDEN_PROTO
			    struct profile *cons)
{
  struct profile *tail = cons;

  if (tail) {
    while (tail->next)
      tail = tail->next;
    tail->next = fd.profile_pool;
    fd.profile_pool = cons;
  }
}


/* maximum energy of sub-profile */
long profile_maxerg(struct profile *h,
		    long b, long e)
{
  long erg = 0;
  
  for (; h && h->begin<e; h=h->next)
    if (h->erg > erg && OVERLAP(h->begin,h->end,b,e))
      erg = h->erg;
  return erg;
}


/* get subset of h overlapping [b2,e2) such that erg > limit */
static struct profile *profile_part MAGIC (HIDDEN_PROTO
					   struct profile *h,
					   long b2, long e2, long limit)
{
  struct profile part;
  struct profile *tail = &part;
  long b1, e1, y1;

  part.end = HighInt;
  part.erg = 0;
  part.next = NULL;

  for (; h; h=h->next) {
    b1 = h->begin;
    e1 = h->end;
    y1 = h->erg;
    switch (profile_cmp(b1,e1,b2,e2)) {
    case FD_BEFORE:		/* or MEETS */
      break;
    case FD_OVERLAPS:
      if (y1 > limit) {
	profile_emit_fast(b2,e1,y1);
      }
      break;
    case FD_FINISHED_BY:
      if (y1 > limit) {
	profile_emit_fast(b2,e1,y1);
      }
      goto out;
    case FD_CONTAINS:
      if (y1 > limit) {
	profile_emit_fast(b2,e2,y1);
      }
      goto out;
    case FD_STARTS:
    case FD_DURING:
      if (y1 > limit) {
	profile_emit_fast(b1,e1,y1);
      }
      b2 = e1;
      break;
    case FD_EQUALS:
    case FD_FINISHES:
      if (y1 > limit) {
	profile_emit_fast(b1,e1,y1);
      }
      goto out;
    case FD_STARTED_BY:
    case FD_OVERLAPPED_BY:
      if (y1 > limit) {
	profile_emit_fast(b1,e2,y1);
      }
      goto out;
    case FD_AFTER:		/* or MET_BY */
      goto out;
    }
  }
out:
  tail->next = NULL;
  return part.next;
}


/* Merge two non-empty profiles.
   No structure sharing!  New profiles is brand new.
   */
struct profile *profile_merge MAGIC (HIDDEN_PROTO
				     struct profile *h1,
				     struct profile *h2)
{
  long b1, e1, y1, b2, e2, y2;
  struct profile part;
  struct profile *tail = &part;

  part.end = HighInt;
  part.erg = 0;
  part.next = NULL;
  
list_and_list:
  if (h1==NULL)
    goto null_and_list;
  b1 = h1->begin; e1 = h1->end; y1 = h1->erg; h1 = h1->next;
head_tail_and_list:
  if (h2==NULL)
    goto head_tail_and_null;
  b2 = h2->begin; e2 = h2->end; y2 = h2->erg; h2 = h2->next;
  goto head_tail_and_head_tail;
list_and_head_tail:
  if (h1==NULL)
    goto null_and_head_tail;
  b1 = h1->begin; e1 = h1->end; y1 = h1->erg; h1 = h1->next;
head_tail_and_head_tail:
  switch (profile_cmp(b1,e1,b2,e2)) {
  case FD_BEFORE:		/* or MEETS */
    profile_emit(b1,e1,y1);
    goto list_and_head_tail;
  case FD_OVERLAPS:
    profile_emit(b1,b2,y1);
    profile_emit(b2,e1,y1+y2);
    b2 = e1;
    goto list_and_head_tail;
  case FD_FINISHED_BY:
    profile_emit(b1,b2,y1);
    profile_emit(b2,e1,y1+y2);
    goto list_and_list;
  case FD_CONTAINS:
    profile_emit(b1,b2,y1);
    profile_emit(b2,e2,y1+y2);
    b1 = e2;
    goto head_tail_and_list;
  case FD_STARTS:
    profile_emit(b1,e1,y1+y2);
    b2 = e1;
    goto list_and_head_tail;
  case FD_EQUALS:
    profile_emit(b1,e1,y1+y2);
    goto list_and_list;
  case FD_STARTED_BY:
    profile_emit(b1,e2,y1+y2);
    b1 = e2;
    goto head_tail_and_list;
  case FD_DURING:
    profile_emit(b2,b1,y2);
    profile_emit(b1,e1,y1+y2);
    b2 = e1;
    goto list_and_head_tail;
  case FD_FINISHES:
    profile_emit(b2,b1,y2);
    profile_emit(b1,e1,y1+y2);
    goto list_and_list;
  case FD_OVERLAPPED_BY:
    profile_emit(b2,b1,y2);
    profile_emit(b1,e2,y1+y2);
    b1 = e2;
    goto head_tail_and_list;
  case FD_AFTER:		/* or MET_BY */
    profile_emit(b2,e2,y2);
    goto head_tail_and_list;
  }
null_and_list:
  if (h2==NULL)
    goto null_and_null;
  b2 = h2->begin; e2 = h2->end; y2 = h2->erg; h2 = h2->next;
null_and_head_tail:
    profile_emit(b2,e2,y2);
    goto null_and_list;
list_and_null:
  if (h1==NULL)
    goto null_and_null;
  b1 = h1->begin; e1 = h1->end; y1 = h1->erg; h1 = h1->next;
head_tail_and_null:
    profile_emit(b1,e1,y1);
    goto list_and_null;
null_and_null:
  tail->next = NULL;
  return part.next;
}



/* Add an interval to a profile without structure sharing.
   New profile is brand new.
   */
struct profile *profile_add MAGIC (HIDDEN_PROTO
				   struct profile *p,
				   long b2, long e2,
				   long y2)
{
  TAGGED b1, e1;
  int y1;
  struct profile part;
  struct profile *tail = &part;

  part.end = CLPFD_MAXINT;
  part.erg = 0;
  part.next = NULL;
  
list:
  if (p==NULL)
    goto null_and_head_tail;
  b1 = p->begin;
  e1 = p->end;
  y1 = p->erg;
  p = p->next;
  switch (profile_cmp(b1,e1,b2,e2)) {
  case FD_BEFORE:		/* or MEETS */
    profile_emit(b1,e1,y1);
    goto list;
  case FD_OVERLAPS:
    profile_emit(b1,b2,y1);
    profile_emit(b2,e1,y1+y2);
    b2 = e1;
    goto list;
  case FD_FINISHED_BY:
    profile_emit(b1,b2,y1);
    profile_emit(b2,e1,y1+y2);
    goto list_and_null;
  case FD_CONTAINS:
    profile_emit(b1,b2,y1);
    profile_emit(b2,e2,y1+y2);
    b1 = e2;
    goto head_tail_and_null;
  case FD_STARTS:
    profile_emit(b1,e1,y1+y2);
    b2 = e1;
    goto list;
  case FD_EQUALS:
    profile_emit(b1,e1,y1+y2);
    goto list_and_null;
  case FD_STARTED_BY:
    profile_emit(b1,e2,y1+y2);
    b1 = e2;
    goto head_tail_and_null;
  case FD_DURING:
    profile_emit(b2,b1,y2);
    profile_emit(b1,e1,y1+y2);
    b2 = e1;
    goto list;
  case FD_FINISHES:
    profile_emit(b2,b1,y2);
    profile_emit(b1,e1,y1+y2);
    goto list_and_null;
  case FD_OVERLAPPED_BY:
    profile_emit(b2,b1,y2);
    profile_emit(b1,e2,y1+y2);
    b1 = e2;
    goto head_tail_and_null;
  case FD_AFTER:		/* or MET_BY */
    profile_emit(b2,e2,y2);
    goto head_tail_and_null;
  }
null_and_head_tail:
  profile_emit(b2,e2,y2);
  tail->next = NULL;
  return part.next;
head_tail_and_null:
  profile_emit(b1,e1,y1);
list_and_null:
  while (p) {
    b1 = p->begin;
    e1 = p->end;
    y1 = p->erg;
    p = p->next;
    profile_emit(b1,e1,y1);
  }
  tail->next = NULL;
  return part.next;
}


/* Add an interval to profile with structure sharing,
   i.e. dispose of any parts of profile that can't be shared.
   */
struct profile *profile_update MAGIC (HIDDEN_PROTO
				      struct profile *p,
				      long b2, long e2,
				      long y2)
{
  TAGGED b1, e1;
  int y1;
  struct profile *tmp;
  struct profile part;
  struct profile *tail = &part;

  part.end = CLPFD_MAXINT;
  part.erg = 0;
  part.next = NULL;
  
list:
  if (p==NULL)
    goto null_and_head_tail;
  b1 = p->begin;
  e1 = p->end;
  y1 = p->erg;
  tmp = p;
  p = p->next;
  tmp->next = fd.profile_pool;
  fd.profile_pool = tmp;
  switch (profile_cmp(b1,e1,b2,e2)) {
  case FD_BEFORE:		/* or MEETS */
    profile_emit(b1,e1,y1);
    goto list;
  case FD_OVERLAPS:
    profile_emit(b1,b2,y1);
    profile_emit(b2,e1,y1+y2);
    b2 = e1;
    goto list;
  case FD_FINISHED_BY:
    profile_emit(b1,b2,y1);
    profile_emit(b2,e1,y1+y2);
    goto list_and_null;
  case FD_CONTAINS:
    profile_emit(b1,b2,y1);
    profile_emit(b2,e2,y1+y2);
    b1 = e2;
    goto head_tail_and_null;
  case FD_STARTS:
    profile_emit(b1,e1,y1+y2);
    b2 = e1;
    goto list;
  case FD_EQUALS:
    profile_emit(b1,e1,y1+y2);
    goto list_and_null;
  case FD_STARTED_BY:
    profile_emit(b1,e2,y1+y2);
    b1 = e2;
    goto head_tail_and_null;
  case FD_DURING:
    profile_emit(b2,b1,y2);
    profile_emit(b1,e1,y1+y2);
    b2 = e1;
    goto list;
  case FD_FINISHES:
    profile_emit(b2,b1,y2);
    profile_emit(b1,e1,y1+y2);
    goto list_and_null;
  case FD_OVERLAPPED_BY:
    profile_emit(b2,b1,y2);
    profile_emit(b1,e2,y1+y2);
    b1 = e2;
    goto head_tail_and_null;
  case FD_AFTER:		/* or MET_BY */
    profile_emit(b2,e2,y2);
    goto head_tail_and_null;
  }
null_and_head_tail:
  profile_emit(b2,e2,y2);
  tail->next = NULL;
#if PROFILE_STATISTICS
  update_profile_stat(part.next);
#endif
  return part.next;
head_tail_and_null:
  profile_emit(b1,e1,y1);
list_and_null:
  tail->next = p;
#if PROFILE_STATISTICS
  update_profile_stat(part.next);
#endif
  return part.next;
}



/* TRUE iff the "energy" is zero somewhere inside */
BOOL profile_zero_at(struct profile *p,
		     long b1, long e1, long *wit)
{
  long b2, e2, bb = 0, ee = 0;
  int count = 0;

list:
  if (!p)
    goto before;
  b2 = p->begin;
  e2 = p->end;
  p = p->next;
  while (p && e2 == p->begin) {	/* extend nonzero segment [b2,e2) */
    e2 = p->end;
    p = p->next;
  }
  switch (profile_cmp(b1,e1,b2,e2)) {
  case FD_BEFORE:
  before:
    if (1.0 / ++count > crandom()) {
      bb = b1;
      ee = e1;
    }
    break;
  case FD_OVERLAPS:
  case FD_FINISHED_BY:
    if (1.0 / ++count > crandom()) {
      bb = b1;
      ee = b2;
    }
    break;
  case FD_CONTAINS:
    if (1.0 / ++count > crandom()) {
      bb = b1;
      ee = b2;
    }
    b1 = e2;
    goto list;
  case FD_STARTS:
  case FD_EQUALS:
  case FD_DURING:
  case FD_FINISHES:
    break;
  case FD_STARTED_BY:
  case FD_OVERLAPPED_BY:
    b1 = e2;
    goto list;
  case FD_MET_BY:
  case FD_AFTER:
    goto list;
  }
  if (bb==ee)
    return FALSE;
  if (bb+1 < ee)		/* otherwise, unit interval */
    bb += (long)((ee-bb)*crandom());
  *wit = bb;
  return TRUE;
}



/* TRUE iff the "energy" is nonzero somewhere inside */
BOOL profile_nonzero(struct profile *p,
		     long b2, long e2)
{
  long b1=e2-1, e1;

  while (p && b1<e2) {
    b1 = p->begin;
    e1 = p->end;
    p = p->next;
    if (b1<e2 && b2<e1)
      return TRUE;
  }
  return FALSE;
}


/* Compute sub-profile inside [b,e) where energy limit would be exceeded
   if a task with resource use r1 ran.
   Operationally:
   1. decrease erg level by -r1 in [b1,e1), which may be empty.
   2. return subset overlapping [b,e) such that erg > limit
   Precond: [b,e) includes [b1,e1).
   */
struct profile *profile_exclude_one MAGIC (HIDDEN_PROTO
					   struct profile *p,
					   long b, long e, long limit,
					   long b1, long e1, long r1)
{
  struct profile *tmp1, *tmp2;
  BOOL disposable = FALSE;

  tmp1 = p;
  if (b1 < e1) {
    tmp1 = profile_add(tmp1, b1, e1, -r1);
    disposable = TRUE;
  }
  tmp2 = profile_part(tmp1, b, e, limit);
  if (disposable)
    profile_dispose(tmp1);
  return tmp2;
}


/* Compute sub-profile inside [b,e) where energy limit would be exceeded
   if two tasks with resource use r1 and r2 both ran.
   Operationally:
   1. decrease erg level by -r1 in [b1,e1), which may be empty.
   2. decrease erg level by -r2 in [b2,e2), which may be empty.
   3. return subset overlapping [b,e) such that erg > limit
   Precond: [b,e) may not include [b1,e1) or [b2,e2).
   */
struct profile *profile_exclude_two MAGIC (HIDDEN_PROTO
					   struct profile *p,
					   long b, long e, long limit,
					   long b1, long e1, long r1,
					   long b2, long e2, long r2)
{
  struct profile *tmp1, *tmp2;
  BOOL disposable = FALSE;

  tmp1 = p;
  if (b1 < e1) {
    tmp1 = profile_add(tmp1, b1, e1, -r1);
    disposable = TRUE;
  }
  if (b2 < e2) {
    tmp2 = tmp1;
    tmp1 = profile_add(tmp2, b2, e2, -r2);
    if (disposable)
      profile_dispose(tmp2);
    disposable = TRUE;
  }
  tmp2 = profile_part(tmp1, b, e, limit);
  if (disposable)
    profile_dispose(tmp1);
  return tmp2;
}


BOOL profile_next(struct profile *prof,
		  long *bp, long *ep, long *hp,
		  struct profile **nextp)
{
  if (!prof)
    return FALSE;
  *bp = prof->begin;
  *ep = prof->end;
  *hp = prof->erg;
  *nextp = prof->next;
  return TRUE;
}

