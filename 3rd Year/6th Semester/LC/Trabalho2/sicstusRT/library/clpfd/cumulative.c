/* Copyright(C) 1999, Swedish Institute of Computer Science */

#include "fd.h"

#define CSTATE_ONLY 0

#if MULTI_SP_AWARE
#if 0                           /* not redefined. It uses FD_SETUP_SPENV to access the SPEnv */
#define cumulative_destructor(A1) cumulative_destructor(HIDDEN_ARG, A1)
#endif
#define cmp_asc_area(A1,A2) cmp_asc_area(HIDDEN_ARG, A1,A2)
#define qsort_asc_areaswap(A1,A2,A3,A4) qsort_asc_areaswap(HIDDEN_ARG, A1,A2,A3,A4)
#define qsort_asc_areamed3(A1,A2,A3) qsort_asc_areamed3(HIDDEN_ARG, A1,A2,A3)
#define qsort_asc_area(A1,A2) qsort_asc_area(HIDDEN_ARG, A1,A2)
#define cmp_desc_est(A1,A2) cmp_desc_est(HIDDEN_ARG, A1,A2)
#define qsort_desc_estswap(A1,A2,A3,A4) qsort_desc_estswap(HIDDEN_ARG, A1,A2,A3,A4)
#define qsort_desc_estmed3(A1,A2,A3) qsort_desc_estmed3(HIDDEN_ARG, A1,A2,A3)
#define qsort_desc_est(A1,A2) qsort_desc_est(HIDDEN_ARG, A1,A2)
#define cmp_asc_est(A1,A2) cmp_asc_est(HIDDEN_ARG, A1,A2)
#define qsort_asc_estswap(A1,A2,A3,A4) qsort_asc_estswap(HIDDEN_ARG, A1,A2,A3,A4)
#define qsort_asc_estmed3(A1,A2,A3) qsort_asc_estmed3(HIDDEN_ARG, A1,A2,A3)
#define qsort_asc_est(A1,A2) qsort_asc_est(HIDDEN_ARG, A1,A2)
#define cmp_asc_ect(A1,A2) cmp_asc_ect(HIDDEN_ARG, A1,A2)
#define qsort_asc_ectswap(A1,A2,A3,A4) qsort_asc_ectswap(HIDDEN_ARG, A1,A2,A3,A4)
#define qsort_asc_ectmed3(A1,A2,A3) qsort_asc_ectmed3(HIDDEN_ARG, A1,A2,A3)
#define qsort_asc_ect(A1,A2) qsort_asc_ect(HIDDEN_ARG, A1,A2)
#define cmp_asc_lct(A1,A2) cmp_asc_lct(HIDDEN_ARG, A1,A2)
#define qsort_asc_lctswap(A1,A2,A3,A4) qsort_asc_lctswap(HIDDEN_ARG, A1,A2,A3,A4)
#define qsort_asc_lctmed3(A1,A2,A3) qsort_asc_lctmed3(HIDDEN_ARG, A1,A2,A3)
#define qsort_asc_lct(A1,A2) qsort_asc_lct(HIDDEN_ARG, A1,A2)
#define cmp_desc_lst(A1,A2) cmp_desc_lst(HIDDEN_ARG, A1,A2)
#define qsort_desc_lstswap(A1,A2,A3,A4) qsort_desc_lstswap(HIDDEN_ARG, A1,A2,A3,A4)
#define qsort_desc_lstmed3(A1,A2,A3) qsort_desc_lstmed3(HIDDEN_ARG, A1,A2,A3)
#define qsort_desc_lst(A1,A2) qsort_desc_lst(HIDDEN_ARG, A1,A2)
#define qsort_asc_longswap(A1,A2,A3,A4) qsort_asc_longswap(HIDDEN_ARG, A1,A2,A3,A4)
#define qsort_asc_longmed3(A1,A2,A3) qsort_asc_longmed3(HIDDEN_ARG, A1,A2,A3)
#define qsort_asc_long(A1,A2) qsort_asc_long(HIDDEN_ARG, A1,A2)
#define wr_task(A1) wr_task(HIDDEN_ARG, A1)
#define profile_update_check(A1,A2,A3) profile_update_check(HIDDEN_ARG, A1,A2,A3)
#define propagate(A1,A2,A3) propagate(HIDDEN_ARG, A1,A2,A3)
#define plus_sub(A1,A2,A3,A4,A5) plus_sub(HIDDEN_ARG, A1,A2,A3,A4,A5)
#define tell_plus(A1,A2,A3) tell_plus(HIDDEN_ARG, A1,A2,A3)
#define check_plus(A1,A2,A3) check_plus(HIDDEN_ARG, A1,A2,A3)
#define order_tell_task(A1,A2,A3) order_tell_task(HIDDEN_ARG, A1,A2,A3)
#define order_tell_diff(A1,A2,A3) order_tell_diff(HIDDEN_ARG, A1,A2,A3)
#define order_tell_hole(A1,A2,A3) order_tell_hole(HIDDEN_ARG, A1,A2,A3)
#define static_sets_up() static_sets_up(HIDDEN_ARG)
#define static_sets_down() static_sets_down(HIDDEN_ARG)
#define tell_before(A1,A2) tell_before(HIDDEN_ARG, A1,A2)
#define not_first(A1,A2,A3) not_first(HIDDEN_ARG, A1,A2,A3)
#define not_last(A1,A2,A3) not_last(HIDDEN_ARG, A1,A2,A3)

#define min_overlap(A1,A2,A3,A4) min_overlap(HIDDEN_ARG, A1,A2,A3,A4)
#define max_overlap(A1,A2,A3,A4) max_overlap(HIDDEN_ARG, A1,A2,A3,A4)

#define total_min_overlap(A1,A2,A3,A4) total_min_overlap(HIDDEN_ARG, A1,A2,A3,A4)
#define edge_finding_up() edge_finding_up(HIDDEN_ARG)
#define edge_finding_down() edge_finding_down(HIDDEN_ARG)
#define total_dur_disj(A1,A2) total_dur_disj(HIDDEN_ARG, A1,A2)

#define total_min_overlap_disj(A1,A2,A3,A4,A5) total_min_overlap_disj(HIDDEN_ARG, A1,A2,A3,A4,A5)
#define edge_finding_disj_up() edge_finding_disj_up(HIDDEN_ARG)
#define edge_finding_disj_down() edge_finding_disj_down(HIDDEN_ARG)
#define shifted_domain(A1,A2,A3,A4,A5,A6,A7) shifted_domain(HIDDEN_ARG, A1,A2,A3,A4,A5,A6,A7)
#define isolated_source(A1) isolated_source(HIDDEN_ARG, A1)
#define decompose() decompose(HIDDEN_ARG)
#endif /* MULTI_SP_AWARE */

#define KNAPSACK 0

/* Optimization 5: O(N) time detection of sources that can no longer
   affect anything.

 SOURCE --> NONE:
   a) Compute bounding box of all targets.
   b) Remove SOURCE property of any source that can't interact with the
      bounding box.  Mark other sources.

 Optional O(N^2) full decomposition:
   c) Form sorted lists of start and end events corresponding to
      items' bounding boxes
   d) Sweep the sorted lists, detecting overlaps except when one box
      includes another one.  Overlapping items are marked.
   e) Sweep the sorted lists once more, handling each event as
      follows:

      U = the empty set.

      If START(unmarked(u)) is next, add u to U.
      If START(marked(m)) is next, for each u in U:
        if m and u overlap in the Y dimension,
	  mark u and remove it from U.
      If END(unmarked(u)) is next, remove u from U.
      If END(marked(m)) is next, do nothing.       
   f) Remove SOURCE property of any unmarked source, and unmark the rest.
*/
#define DECOMPOSITION 1

typedef long TASK;
typedef long DIFF;

/* The constraint frame. */
struct cumulative_data {
  void (SPCDECL *destructor)(void *);
#if MULTI_SP_AWARE
  SPEnv *spenv;
#endif /* MULTI_SP_AWARE */

  int nrefs;			/* static, 1 + 4*ntasks + ndiffs */
  SP_term_ref refbase;		/* static */
  SP_term_ref drefbase;		/* static, = refbase + 3*ntasks + 1 */
  long stamp;
  int time;			/* starts from 0 at each resumption */
  int ntasks;			/* static, #tasks */
  int ntargets;			/* #tasks that may be targets, := ntasks */
  int nsources;			/* #tasks that may be sources only, := 0 */
#if CSTATE_ONLY
  int ndone;			/* #tasks that are forgotten _forever_, := 0 */
#endif
  int flags;			/* static */
  long use_limit;		/* capacity limit */
  long use_threshold;		/* v incompatible with some if USE(v)>use_threshold */
  TASK tasks_area;		/* terminated by EOL */
  TASK queued;			/* terminated by EOL */
  TASK *target;			/* [ntasks] */
  TASK *sortarr;		/* [3*ntasks], volatile */
  struct {
    TAGGED *min;		/* atomic */
    TAGGED *max;		/* atomic */
    TAGGED *fdset;		/* volatile, GC-unsafe */
    long *mindur;
    long *maxdur;
    long *minuse;
    long *maxuse;
    long *area;
    long *checked_at;		/* volatile */
    long *told_at;		/* volatile */
    long *status;
    TASK *next_queued;		/* terminated by EOL, volatile */
    TASK *next_area;		/* terminated by EOL, volatile */
    TASK *next_elt;		/* terminated by EOL, volatile */
  } task;			/* each [ntasks] */
  struct {
    TAGGED *min;		/* volatile, atomic */
    TAGGED *max;		/* volatile, atomic */
    TAGGED *fdset;		/* volatile, GC-safe */
    long *checked_at;		/* volatile */
    long *told_at;		/* volatile */
    TASK *next_queued;		/* terminated by EOL, volatile */
  } diff;			/* each [ntasks*ntasks] */
  /* space for the above arrays */
};

static void SPCDECL cumulative_destructor(void *pdata_v)
{
  struct cumulative_data *pdata = (struct cumulative_data *)pdata_v;
  FD_SETUP_SPENV(pdata->spenv)

  SP_free_term_refs(pdata->refbase,pdata->nrefs);
  SP_free(pdata);
}


/* linked list terminator */
#define EOL (-1L)

/* next_queued!=PAST_EOL iff item is in the propagation queue */
#define PAST_EOL (-2L)

#define STATUS_SOURCE 1
#define STATUS_TARGET 2

#define TMIN(t) (pdata->task.min[t])
#define TMAX(t) (pdata->task.max[t])
#define TDOM(t) (pdata->task.fdset[t])
#define DUR(T) (pdata->task.mindur[T])
#define DURmax(T) (pdata->task.maxdur[T])
#define USE(T) (pdata->task.minuse[T])
#define USEmax(T) (pdata->task.maxuse[T])
#define AREA(t) (pdata->task.area[t])
#define TCHECKED_AT(t) (pdata->task.checked_at[t])
#define TTOLD_AT(t) (pdata->task.told_at[t])
#define STATUS(t) (pdata->task.status[t])
#define TNEXT_QUEUED(t) (pdata->task.next_queued[t])
#define NEXT_AREA(t) (pdata->task.next_area[t])
#define NEXT_ELT(t) (pdata->task.next_elt[t])
#define EST(T) GetSmall(pdata->task.min[T])
#define LaST(T) GetSmall(pdata->task.max[T])
#define ECT(T) (EST(T)+pdata->task.mindur[T])
#define LCT(T) (LaST(T)+pdata->task.mindur[T])
#define LCTmax(T) (LaST(T)+pdata->task.maxdur[T])
#define TRefOrigAttr(T) RefTerm(pdata->refbase + 4*(T) + 1)
#define TRefOrig(T) RefTerm(pdata->refbase + 4*(T) + 2)
#define TRefDurAttr(T) RefTerm(pdata->refbase + 4*(T) + 3)
#define TRefUseAttr(T) RefTerm(pdata->refbase + 4*(T) + 4)

/* -1 if j before i, 1 if i before j, 0 otherwise */
#define TSIGN(Ti,Tj) (LaST(Tj)<ECT(Ti) ? -1 : LaST(Ti)<ECT(Tj))

#define DMIN(t) (pdata->diff.min[t-ntasks])
#define DMAX(t) (pdata->diff.max[t-ntasks])
#define DDOM(t) (pdata->diff.fdset[t-ntasks])
#define DCHECKED_AT(t) (pdata->diff.checked_at[t-ntasks])
#define DTOLD_AT(t) (pdata->diff.told_at[t-ntasks])
#define DNEXT_QUEUED(t) (pdata->diff.next_queued[t-ntasks])
#define DSIGN(V) (Tgtz(DMIN(V))+Tgtz(DMAX(V))-1)
#define DRefMut(T) RefTerm(pdata->drefbase + (T))

/* ALWAYS I < J */

#define SV(I)   (pdata->target[I])
#define DV(I,J) (ntasks*((I)+1) + (J))

/* for qsorting by ascending area */
static int cmp_asc_area MAGIC (HIDDEN_PROTO
			       TASK *t1, TASK *t2)
{
  struct cumulative_data *pdata = fd.gdata;

#if MULTI_SP_AWARE
  (void)HIDDEN_ARG;             /* [PM] 3.9b5 avoid -Wunused */
#endif /* MULTI_SP_AWARE */

  return AREA(*t1) - AREA(*t2);
}


#define QType TASK
#define QCmp  cmp_asc_area
#define QSort qsort_asc_area
#include "qsort.ic"

/* for qsorting by descending EST */
static int cmp_desc_est MAGIC (HIDDEN_PROTO
			       TASK *t1, TASK *t2)
{
  struct cumulative_data *pdata = fd.gdata;
#if MULTI_SP_AWARE
  (void)HIDDEN_ARG;             /* [PM] 3.9b5 avoid -Wunused */
#endif /* MULTI_SP_AWARE */
  return EST(*t2) - EST(*t1);
}


#define QType TASK
#define QCmp  cmp_desc_est
#define QSort qsort_desc_est
#include "qsort.ic"

/* for qsorting by ascending EST */
static int cmp_asc_est MAGIC (HIDDEN_PROTO
			      TASK *t1, TASK *t2)
{
  struct cumulative_data *pdata = fd.gdata;
#if MULTI_SP_AWARE
  (void)HIDDEN_ARG;             /* [PM] 3.9b5 avoid -Wunused */
#endif /* MULTI_SP_AWARE */
  return EST(*t1) - EST(*t2);
}


#define QType TASK
#define QCmp  cmp_asc_est
#define QSort qsort_asc_est
#include "qsort.ic"

/* for qsorting by ascending ECT */
static int cmp_asc_ect MAGIC (HIDDEN_PROTO
			      TASK *t1, TASK *t2)
{
  struct cumulative_data *pdata = fd.gdata;
#if MULTI_SP_AWARE
  (void)HIDDEN_ARG;             /* [PM] 3.9b5 avoid -Wunused */
#endif /* MULTI_SP_AWARE */
  return ECT(*t1) - ECT(*t2);
}


#define QType TASK
#define QCmp  cmp_asc_ect
#define QSort qsort_asc_ect
#include "qsort.ic"

/* for qsorting by ascending LCT */
static int cmp_asc_lct MAGIC (HIDDEN_PROTO
			      TASK *t1, TASK *t2)
{
  struct cumulative_data *pdata = fd.gdata;
#if MULTI_SP_AWARE
  (void)HIDDEN_ARG;             /* [PM] 3.9b5 avoid -Wunused */
#endif /* MULTI_SP_AWARE */
  return LCT(*t1) - LCT(*t2);
}


#define QType TASK
#define QCmp  cmp_asc_lct
#define QSort qsort_asc_lct
#include "qsort.ic"

/* for qsorting by descending LaST */
static int cmp_desc_lst MAGIC (HIDDEN_PROTO
			       TASK *t1, TASK *t2)
{
  struct cumulative_data *pdata = fd.gdata;
#if MULTI_SP_AWARE
  (void)HIDDEN_ARG;             /* [PM] 3.9b5 avoid -Wunused */
#endif /* MULTI_SP_AWARE */
  return LaST(*t2) - LaST(*t1);
}


#define QType TASK
#define QCmp  cmp_desc_lst
#define QSort qsort_desc_lst
#include "qsort.ic"

/* for qsorting by ascending long */
static int cmp_asc_long(long *l1, long *l2)
{
  return *l1 - *l2;
}


#define QType long
#define QCmp  cmp_asc_long
#define QSort qsort_asc_long
#include "qsort.ic"


#if DBG>1

static void wr_task MAGIC (HIDDEN_ARG
                           TASK t)
{
  struct cumulative_data *pdata = fd.gdata;
  printf("index=%ld dur=%ld use=%ld est=%ld lst=%ld ect=%ld lct=%ld\n",
	 (long)t, DUR(t), USE(t), EST(t), LaST(t), ECT(t), LCT(t));
}
#endif

/* Add delta to the energy level for all time points in [a,b).
   Fail if global limit exceeded.
   Precond: a<b.
   */
static BOOL profile_update_check MAGIC (HIDDEN_PROTO
					long b, long e, long delta)
{
  struct cumulative_data *pdata = fd.gdata;
  
  fd.profile = profile_update(fd.profile, b, e, delta);
  if (delta>0 && profile_maxerg(fd.profile,b,e) > pdata->use_limit)
    return FALSE;
  return TRUE;
}


/* We have just pruned v, old bounds being min0 and max0.
   Refresh the histogram and enqueue v and any variables that could be 
   affected.
*/
static BOOL propagate MAGIC (HIDDEN_PROTO
			     TASK v, TAGGED min0, TAGGED max0)
{
  struct cumulative_data *pdata = fd.gdata;
  int ntasks = pdata->ntasks;
  int ntargets = pdata->ntargets;
  
  if (v < ntasks) {
    long dur = DUR(v);
    long a = LaST(v);
    long b = ECT(v);
    
    TTOLD_AT(v) = ++pdata->time;
    if (TNEXT_QUEUED(v)==PAST_EOL)
      TNEXT_QUEUED(v) = pdata->queued, pdata->queued = v;
    if (a<b) {
      long a0 = GetSmall(max0);
      long b0 = GetSmall(min0) + dur;
      long res = USE(v);
      long maxneed;
      int j;
      
      if (a0<b0) {		/* the previous reserved interval */
	if (  (a<a0 && !profile_update_check(a, a0, res))
	      || (b0<b && !profile_update_check(b0, b, res))
	      )
	  return FALSE;
      } else if (!profile_update_check(a, b, res))
	return FALSE;
      /* enqueue for rule [4] */
      maxneed = pdata->use_limit - profile_maxerg(fd.profile, a, b);
      for (j=0; j<ntargets; j++) {
	TASK sj = SV(j);
	
	if (TNEXT_QUEUED(sj)==PAST_EOL &&
	    (STATUS(sj) & STATUS_TARGET) &&
	    EST(sj)<b && LCT(sj)>a &&
	    DUR(sj)>0 &&
	    USE(sj)>maxneed)
	  TNEXT_QUEUED(sj) = pdata->queued, pdata->queued = sj;
      }
    }
  } else {
    DTOLD_AT(v) = ++pdata->time;
    if (DNEXT_QUEUED(v)==PAST_EOL && DSIGN(v)!=0)
      DNEXT_QUEUED(v) = pdata->queued, pdata->queued = v;
  }
  return TRUE;
}


static BOOL plus_sub MAGIC (HIDDEN_PROTO
			    TASK term1,
			    TAGGED newmin, TAGGED newmax,
			    TAGGED min1, TAGGED max1)
{
  struct cumulative_data *pdata = fd.gdata;
  int ntasks = pdata->ntasks;
  
  if (Tlt(min1,newmin) || Tgt(max1,newmax)) {
    if (term1 < ntasks) {
      if (!adjust_bounds(newmin, newmax,
			 TDOM(term1), &TMIN(term1), &TMAX(term1)))
	return FALSE;
      if (min1!=TMIN(term1) || max1!=TMAX(term1))
	return propagate(term1, min1, max1);
    } else if ((STATUS(term1%ntasks)|STATUS(term1/ntasks-1)) & STATUS_TARGET) {
      if (!adjust_bounds(newmin, newmax,
			 DDOM(term1), &DMIN(term1), &DMAX(term1)))
	return FALSE;
      if (min1!=DMIN(term1) || max1!=DMAX(term1))
	return propagate(term1, min1, max1);
    }
  }
  return TRUE;
}


/* implements parts of rules [1,2] */
/* maintains interval consistency of term1 + term2 = term3 */
static BOOL tell_plus MAGIC (HIDDEN_PROTO
			     TASK term1, TASK term2, TASK term3)
{
  struct cumulative_data *pdata = fd.gdata;
  int ntasks = pdata->ntasks;
  
  TAGGED newmin, newmax;
  TAGGED min1;
  TAGGED max1;
  TAGGED min2;
  TAGGED max2;
  TAGGED min3;
  TAGGED max3;

  if (term1<ntasks)
    min1 = TMIN(term1),
    max1 = TMAX(term1);
  else
    min1 = DMIN(term1),
    max1 = DMAX(term1);

  if (term2<ntasks)
    min2 = TMIN(term2),
    max2 = TMAX(term2);
  else
    min2 = DMIN(term2),
    max2 = DMAX(term2);

  if (term3<ntasks)
    min3 = TMIN(term3),
    max3 = TMAX(term3);
  else
    min3 = DMIN(term3),
    max3 = DMAX(term3);

  /* X in min(T) - max(Y)..max(T) - min(Y) */
  newmin = min3-max2+TaggedZero;
  newmax = max3-min2+TaggedZero;
  if (!plus_sub(term1,newmin,newmax,min1,max1))
    return FALSE;
      
  /* Y in min(T) - max(X)..max(T) - min(X) */
  newmin = min3-max1+TaggedZero;
  newmax = max3-min1+TaggedZero;
  if (!plus_sub(term2,newmin,newmax,min2,max2))
    return FALSE;

  /* T in min(X) + min(Y)..max(X) + max(Y) */
  newmin = min1+min2-TaggedZero;
  newmax = max1+max2-TaggedZero;
  if (!plus_sub(term3,newmin,newmax,min3,max3))
    return FALSE;

  return TRUE;
}



/* implements rule [1] */
static BOOL check_plus MAGIC (HIDDEN_PROTO
			      TASK si, DIFF dij, TASK sj)
{
  struct cumulative_data *pdata = fd.gdata;
  int ntasks = pdata->ntasks;
  long chk = DCHECKED_AT(dij);
  
  if (  (chk < DTOLD_AT(dij) || chk < TTOLD_AT(si) || chk < TTOLD_AT(sj))
     && !tell_plus(si, dij, sj)
     )
    return FALSE;
  DCHECKED_AT(dij) = pdata->time;
  return TRUE;
}



/* implements rules [3-4] */
static BOOL order_tell_task MAGIC (HIDDEN_PROTO
				   TASK sv,
				   long lobound, long hibound)
{
  struct cumulative_data *pdata = fd.gdata;
  TAGGED oldmin = TMIN(sv);
  TAGGED oldmax = TMAX(sv);
  TAGGED newmin = MakeSmall(lobound);
  TAGGED newmax = MakeSmall(hibound);
  
  if (Tgt(newmin,oldmin) || Tlt(newmax,oldmax)) {
    if (  !adjust_bounds(newmin, newmax, TDOM(sv), &TMIN(sv), &TMAX(sv))
       || !propagate(sv,oldmin,oldmax)
       )
      return FALSE;
  }
  return TRUE;
}



static BOOL order_tell_diff MAGIC (HIDDEN_PROTO
				   DIFF dv,
				   long lobound, long hibound)
{
  struct cumulative_data *pdata = fd.gdata;
  int ntasks = pdata->ntasks;
  TAGGED oldmin = DMIN(dv);
  TAGGED oldmax = DMAX(dv);
  TAGGED newmin = MakeSmall(lobound);
  TAGGED newmax = MakeSmall(hibound);
  
  if (Tgt(newmin,oldmin) || Tlt(newmax,oldmax)) {
    if (  !adjust_bounds(newmin, newmax, DDOM(dv), &DMIN(dv), &DMAX(dv))
       || !propagate(dv,oldmin,oldmax)
       )
      return FALSE;
  }
  return TRUE;
}



static BOOL order_tell_hole MAGIC (HIDDEN_PROTO /* (sv,lb,ub) */
				   TASK sv,
				   long lb, long ub)
{
  struct cumulative_data *pdata = fd.gdata;
  TAGGED lbt = MakeSmall(lb);
  TAGGED ubt = MakeSmall(ub);

  if (lb>ub) return TRUE;

  switch (fd_interval_cmp(lbt,ubt,TMIN(sv),TMAX(sv))) {
  case FD_BEFORE:
  case FD_MEETS:
  case FD_MET_BY:
  case FD_AFTER:
    return TRUE;
  case FD_OVERLAPS:
  case FD_STARTS:
    return order_tell_task(sv,ub+1,LaST(sv));
  case FD_DURING:
    if ((pdata->flags & 0x20) &&
	fd_compare_interval(TDOM(sv),lbt,ubt)!=FDI_DISJOINT) {
      TDOM(sv) = fd_subtract_interval(TDOM(sv),lbt,ubt);
      TTOLD_AT(sv) = ++pdata->time;
      /* not propagate(sv,TMIN(sv),TMAX(sv)) -- bounds are unchanged */
    }
    return TRUE;
  case FD_FINISHES:
  case FD_OVERLAPPED_BY:
    return order_tell_task(sv,EST(sv),lb-1);
  case FD_FINISHED_BY:
  case FD_CONTAINS:
  case FD_EQUALS:
  case FD_STARTED_BY:
    return FALSE;
  }
  return TRUE;			/* really unreachable */
}


static BOOL static_sets_up MAGIC (HIDDEN_PROTO_VOID)
{
  struct cumulative_data *pdata = fd.gdata;
  int flags = pdata->flags;
  TASK si, sj;
  DIFF dv;
  TASK *arr = pdata->sortarr;
  int ntasks = pdata->ntasks;
  int ntargets = pdata->ntargets;
  int n = ntargets + pdata->nsources;
  long limit = pdata->use_limit;
  int i, j, sign;
  long total, lobound, maxneed, bound;

  qsort_asc_est(arr, n);

  for (i=0; i<ntargets; i++) {
    si = arr[i];
    if (!(STATUS(si) & STATUS_TARGET) || AREA(si)==0)
      continue;
    maxneed = limit-USE(si);
    total = 0;
    lobound = EST(si);
    for (j=n-1; j>=0; j--) {
      sj = arr[j];
      if (i==j || AREA(sj)==0) continue;
      if (flags & 0x10) {
	if (sj<si) {
	  dv = DV(sj,si);
	  sign = DSIGN(dv);
	} else {
	  dv = DV(si,sj);
	  sign = -DSIGN(dv);
	}
      } else
	sign = -TSIGN(si,sj);
      if (USE(sj) > maxneed && sign==1) { /* j before i */
	total += DUR(sj);
	bound = EST(sj) + CEILDIV(total,limit);
	if (lobound < bound)
	  lobound = bound;
      }
    }
    if (lobound > EST(si) && !order_tell_task(si,lobound,LaST(si)))
      return FALSE;
  }
  return TRUE;
}


static BOOL static_sets_down MAGIC (HIDDEN_PROTO_VOID)
{
  struct cumulative_data *pdata = fd.gdata;
  int flags = pdata->flags;
  TASK si, sj;
  DIFF dv;
  TASK *arr = pdata->sortarr;
  int ntasks = pdata->ntasks;
  int ntargets = pdata->ntargets;
  int n = ntargets + pdata->nsources;
  long limit = pdata->use_limit;
  int i, j, sign;
  long total, hibound, maxneed, bound;

  qsort_asc_lct(arr, n);

  for (i=ntargets-1; i>=0; i--) {
    si = arr[i];
    if (!(STATUS(si) & STATUS_TARGET) || AREA(si)==0)
      continue;
    maxneed = limit-USE(si);
    total = 0;
    hibound = LCT(si);
    for (j=0; j<n; j++) {
      sj = arr[j];
      if (i==j || AREA(sj)==0) continue;
      if (flags & 0x10) {
	if (sj<si) {
	  dv = DV(sj,si);
	  sign = -DSIGN(dv);
	} else {
	  dv = DV(si,sj);
	  sign = DSIGN(dv);
	}
      } else
	sign = TSIGN(si,sj);
      if (USE(sj) > maxneed && sign==1) { /* j after i */
	total += DUR(sj);
	bound = LCT(sj) - CEILDIV(total,limit);
	if (hibound > bound)
	  hibound = bound;
      }
    }
    if (hibound < LCT(si) && !order_tell_task(si,EST(si),hibound-DUR(si)))
      return FALSE;
  }
  return TRUE;
}


static BOOL tell_before MAGIC (HIDDEN_PROTO
			       TASK t1, TASK t2)
{
  struct cumulative_data *pdata = fd.gdata;
  int flags = pdata->flags;
  int ntasks = pdata->ntasks;
  long dur1 = DUR(t1);
  long est1 = EST(t1);
  long lst2 = LaST(t2);
  
  /* first adjust bounds if possible */
  if (!order_tell_task(t1, est1, lst2-dur1) ||
      !order_tell_task(t2, est1+dur1, lst2))
    return FALSE;
  /* then fix sign of D12 */
  if (!(flags & 0x10))
    return TRUE;
  else if (t1<t2) {
    DIFF dv = DV(t1,t2);
    
    return order_tell_diff(dv, dur1, GetSmall(DMAX(dv)));
  } else {
    DIFF dv = DV(t2,t1);
    
    return order_tell_diff(dv, GetSmall(DMIN(dv)), -dur1);
  }
}



#define DUR_OR_AREA(t) (disj ? DUR(t) : AREA(t))

/* Find lb of start time of l s.t. slack>=0 is possible, where
   slack = (avail. space from start(l) to lct0)
         - (area demand of tasks starting >= start(l))
   Tasks with USE<=maxuse are not disjunctive with l and are ignored.
   If disj=TRUE, all tasks with USE>maxuse are disjunctive,
   and all reasoning is in terms of duration, otherwise area.
	 */
static BOOL not_first MAGIC (HIDDEN_PROTO
			     TASK s0, TASK l,
			     long maxuse)
{
  struct cumulative_data *pdata = fd.gdata;
  long lb0, lb, newlb, height;
  TASK *arr = pdata->sortarr + 2*pdata->ntasks;
  TASK s;
  long estlt, durlt, dureq, durgt, lcts;
  int i, j, n;
  BOOL disj, sorted = FALSE;
  
#if DBG>1
  printf("NOT_FIRST: task l\n");
  wr_task(l);
  printf("NOT_FIRST: set S\n");
  for (s=s0; s>EOL; s=NEXT_ELT(s)) {
    switch (TSIGN(l,s)) {
    case -1:			/* s before l */
      printf("< "); break;
    case  1:			/* s after l */
      printf("> "); break;
    case  0:			/* either way */
      printf("= "); break;
    }
    wr_task(s);
  }
#endif

  /* Disjunctive case or not? */
  durlt = durgt = CLPFD_MAXINT;
  for (s=s0, i=0; s>EOL; s=NEXT_ELT(s))
    if (s!=l && USE(s)>maxuse) {
      if (USE(s)<=durlt)
	durgt = durlt, durlt = USE(s);
      else if (USE(s)<=durgt)
	durgt = USE(s);
    }
  disj = (durgt==CLPFD_MAXINT || durlt+durgt>pdata->use_limit);
  height = disj ? 1 : pdata->use_limit;
  
  /* Initialize variables.  Build array of unordered s tasks. */
  lb = EST(l);
  durlt = dureq = durgt = 0;
  estlt = CLPFD_MAXINT;
  lcts = ECT(l);
  for (s=s0, i=0; s>EOL; s=NEXT_ELT(s))
    if (s==l || USE(s)<=maxuse)	/* ignore s==l or s not disjunctive with l */
      ;
    else
      switch (TSIGN(l,s)) {
      case -1:			/* s before l */
	durlt += DUR_OR_AREA(s);
	if (estlt>EST(s))
	  estlt = EST(s);
	if (lb<ECT(s))
	  lb = ECT(s);
	newlb = estlt+CEILDIV(durlt,height);
	if (lb<newlb)
	  lb = newlb;
	break;
      case  1:			/* s after l */
	durgt += DUR_OR_AREA(s);
	if (lcts<LCT(s)) lcts = LCT(s);
	break;
      case  0:			/* either way */
	dureq += DUR_OR_AREA(s);
	if (lcts<LCT(s)) lcts = LCT(s);
	arr[i++] = s;
	break;
      }
  n = i;
#if DBG>1
  if (EST(l)<lb)
    printf("NOT_FIRST: infer start(%d)>=%ld\n", l, lb);
#endif
  if (EST(l)<lb && !order_tell_task(l, lb, LaST(l)))
    return FALSE;

  /* Keep adjusting lb until fixpoint. */

  lb0 = EST(l)-1;
  while (lb0<EST(l) && n>0) {
    long avail = height*(lcts-ECT(l))-durgt;
      
    lb0 = lb = EST(l);
    /* identfy new ordered s tasks */
    for (i=0, j=0; i<n; i++) {
      TASK t = arr[i];
      int sign = TSIGN(l,t);
	
      if (DUR_OR_AREA(t)>avail || sign == -1) {
#if DBG>1
	if (sign>=0)
	  printf("NOT_FIRST: infer %d before %d", t, l);
#endif
	if (sign>=0 && !tell_before(t,l))
	  return FALSE;
	dureq -= DUR_OR_AREA(t);
	durlt += DUR_OR_AREA(t);
	if (estlt>EST(t))
	  estlt = EST(t);
	if (lb<ECT(t))
	  lb = ECT(t);
	newlb = estlt+CEILDIV(durlt,height);
	if (lb<newlb)
	  lb = newlb;
      } else
	arr[j++] = t;
    }
    n = j;

    /* if l can't be first, get a new lb */

    if (dureq>avail) {
      if (!sorted) {
	sorted = TRUE;
	qsort_asc_ect(arr, n);
      }
      for (i=0; i<n && dureq>avail; i++) {
	TASK t = arr[i];
      
	newlb = (lb>ECT(t) ? lb : ECT(t));
	avail -= height*(newlb-lb) - DUR_OR_AREA(t);
	lb = newlb;
      }
    }
#if DBG>1
    if (EST(l)<lb)
      printf("NOT_FIRST: infer start(%d)>=%ld\n", l, lb);
#endif
    if (EST(l)<lb && !order_tell_task(l, lb, LaST(l)))
      return FALSE;
#if DBG>1
    if (lb0==EST(l)) {		/* knapsack reasoning */
      long estle = estlt;
      char *sep = "[";
      long klb, kub;

      for (i=0; i<n; i++)
        if (estle > EST(arr[i]))
	  estle = EST(arr[i]);
      klb = durlt+dureq-height*(LaST(l)-estle);
      kub = height*(lcts-ECT(l))-durgt;
      if (dureq>kub) {
	printf("NOT_FIRST: knapsack %ld <= ", klb);
	for (i=0; i<n; i++)
	  printf("%s%d", sep, arr[i]), sep = ",";
	printf("] <= %ld\n", kub);
      }
    }
#endif
  }
	
  return TRUE;
}



/* Find ub of finish time of l s.t. slack>=0 is possible, where
   slack = (avail. space from est0 to finish(l))
         - (area demand of tasks finishing <= finish(l))
   Tasks with USE<=maxuse are not disjunctive with l and are ignored.
   If disj=TRUE, all tasks with USE>maxuse are disjunctive,
   and all reasoning is in terms of duration, otherwise area.
	 */
static BOOL not_last MAGIC (HIDDEN_PROTO
			    TASK s0, TASK l,
			    long maxuse)
{
  struct cumulative_data *pdata = fd.gdata;
  long ub0, ub, newub, height;
  TASK *arr = pdata->sortarr + 2*pdata->ntasks;
  TASK s;
  long lctgt, durgt, dureq, durlt, ests;
  int i, j, n;
  BOOL disj, sorted = FALSE;
  
#if DBG>1
  printf("NOT_LAST: task l\n");
  wr_task(l);
  printf("NOT_LAST: set S\n");
  for (s=s0; s>EOL; s=NEXT_ELT(s)) {
    switch (TSIGN(l,s)) {
    case -1:			/* s before l */
      printf("< "); break;
    case  1:			/* s after l */
      printf("> "); break;
    case  0:			/* either way */
      printf("= "); break;
    }
    wr_task(s);
  }
#endif

  /* Disjunctive case or not? */
  durlt = durgt = CLPFD_MAXINT;
  for (s=s0, i=0; s>EOL; s=NEXT_ELT(s))
    if (s!=l && USE(s)>maxuse) {
      if (USE(s)<=durlt)
	durgt = durlt, durlt = USE(s);
      else if (USE(s)<=durgt)
	durgt = USE(s);
    }
  disj = (durgt==CLPFD_MAXINT || durlt+durgt>pdata->use_limit);
  height = disj ? 1 : pdata->use_limit;
  
  /* Initialize variables.  Build array of unordered s tasks. */
  ub = LCT(l);
  durgt = dureq = durlt = 0;
  lctgt = -CLPFD_MAXINT;
  ests = LaST(l);
  for (s=s0, i=0; s>EOL; s=NEXT_ELT(s))
    if (s==l || USE(s)<=maxuse)	/* ignore s==l or s not disjunctive with l */
      ;
    else
      switch (TSIGN(l,s)) {
      case  1:			/* s after l */
	durgt += DUR_OR_AREA(s);
	if (lctgt<LCT(s))
	  lctgt = LCT(s);
	if (ub>LaST(s))
	  ub = LaST(s);
	newub = lctgt-FLOORDIV(durgt,height);
	if (ub>newub)
	  ub = newub;
	break;
      case -1:			/* s before l */
	durlt += DUR_OR_AREA(s);
	if (ests>EST(s)) ests = EST(s);
	break;
      case  0:			/* either way */
	dureq += DUR_OR_AREA(s);
	if (ests>EST(s)) ests = EST(s);
	arr[i++] = s;
	break;
      }
  n = i;
#if DBG>1
  if (LCT(l)>ub)
    printf("NOT_LAST: infer end(%d)<=%ld\n", l, ub);
#endif
  if (LCT(l)>ub && !order_tell_task(l, EST(l), ub-DUR(l)))
    return FALSE;

  /* Keep adjusting lb until fixpoint. */

  ub0 = LCT(l)+1;
  while (ub0>LCT(l) && n>0) {
    long avail = height*(LaST(l)-ests)-durlt;
      
    ub0 = ub = LCT(l);
    /* identfy new ordered s tasks */
    for (i=0, j=0; i<n; i++) {
      TASK t = arr[i];
      int sign = TSIGN(l,t);
	
      if (DUR_OR_AREA(t)>avail || sign == 1) {
#if DBG>1
	if (sign<=0)
	  printf("NOT_LAST: infer %d before %d", l, t);
#endif
	if (sign<=0 && !tell_before(l,t))
	  return FALSE;
	dureq -= DUR_OR_AREA(t);
	durgt += DUR_OR_AREA(t);
	if (lctgt<LCT(t))
	  lctgt = LCT(t);
	if (ub>LaST(t))
	  ub = LaST(t);
	newub = lctgt-FLOORDIV(durgt,height);
	if (ub>newub)
	  ub = newub;
      } else
	arr[j++] = t;
    }
    n = j;

    /* if l can't be last, get a new ub */

    if (dureq>avail) {
      if (!sorted) {
	sorted = TRUE;
	qsort_desc_lst(arr, n);
      }
      for (i=0; i<n && dureq>avail; i++) {
	TASK t = arr[i];
      
	newub = (ub<LaST(t) ? ub : LaST(t));
	avail -= height*(ub-newub) - DUR_OR_AREA(t);
	ub = newub;
      }
    }
#if DBG>1
    if (LCT(l)>ub)
      printf("NOT_LAST: infer end(%d)<=%ld\n", l, ub);
#endif
    if (LCT(l)>ub && !order_tell_task(l, EST(l), ub-DUR(l)))
      return FALSE;
#if DBG>1
    if (ub0==LCT(l)) {		/* knapsack reasoning */
      long lctge = lctgt;
      char *sep = "[";
      long klb, kub;

      for (i=0; i<n; i++)
        if (lctge < LCT(arr[i]))
	  lctge = LCT(arr[i]);
      klb = durgt+dureq-height*(lctge-ECT(l));
      kub = height*(LaST(l)-ests)-durlt;
      if (dureq>kub) {
	printf("NOT_LAST: knapsack %ld <= ", klb);
	for (i=0; i<n; i++)
	  printf("%s%d", sep, arr[i]), sep = ",";
	printf("] <= %ld\n", kub);
      }
    }
#endif
  }
	
  return TRUE;
}



/* compute area of s that must overlap [est,lct) */
static INLINE long min_overlap MAGIC (HIDDEN_PROTO
                                      TASK s,
                                      long est, long lct, long h)
{
  struct cumulative_data *pdata = fd.gdata;
  long ltmp1=ECT(s)-est, ltmp2=lct-LaST(s);
  long r = (ltmp1<ltmp2 ? ltmp1 : ltmp2);
  
  if (r>0)
    {
      if (r>DUR(s)) r = DUR(s);
      if (r>lct-est) r = lct-est;
      return r*h;
    }
  else
    return 0;
}



/* compute area of s that may overlap [est,lct) */
static INLINE long max_overlap MAGIC (HIDDEN_PROTO
                                      TASK s,
				      long est, long lct, long h)
{
  struct cumulative_data *pdata = fd.gdata;
  long ltmp1=LCT(s)-est, ltmp2=lct-EST(s);
  long r = (ltmp1<ltmp2 ? ltmp1 : ltmp2);

  if (r>0)
    {
      if (r>DUR(s)) r = DUR(s);
      if (r>lct-est) r = lct-est;
      return r*h;
    }
  else
    return 0;
}





/* compute total area of all tasks overlapping [est,lct) */
static long total_min_overlap MAGIC (HIDDEN_PROTO
				     TASK *arr,
				     int n,
				     long est, long lct)
{
  struct cumulative_data *pdata = fd.gdata;
  long total = 0;
  int i;

  for (i=0; i<n; i++)
    total += min_overlap(arr[i],est,lct,USE(arr[i]));
  return total;
}


/* Lean edge finding a la Martin/Shmoys/Wuertz,
   generalized to the cumulative case.
   Comparing tasks T with Si such that lct(T) > lct(Si) = seed,
   where seed is a unique LCT of the tasks.
   */
static BOOL edge_finding_up MAGIC (HIDDEN_PROTO_VOID)
{
  struct cumulative_data *pdata = fd.gdata;
  TASK tasks_by_area = pdata->tasks_area;
  long sorted_at = pdata->time-1;
  int n = pdata->ntargets + pdata->nsources; /* # ACTIVE tasks */
  TASK *arr = pdata->sortarr;
  long *seed = (long *)arr + n;
  long limit = pdata->use_limit;
  int i, j;

  for (i=0; i<n; i++)
    seed[i] = LCT(arr[i]);
  qsort_asc_long(seed, n);

  j = n-1;
  while (j>=0) {
    long lct0 = seed[j];
    long area = 0;
    TASK s = EOL;		/* the chain of subsets */
    TASK l;			/* the tasks, by descending area */
    long ests, slack1base;
    
    if (sorted_at!=pdata->time) {
      sorted_at = pdata->time;
      qsort_desc_est(arr, n);
    }
    for (i=0; i<n; i++) {
      TASK t1 = arr[i];
	  
      if (LCT(t1) <= lct0 && AREA(t1)>0) {
	area += AREA(t1);
	if (area > limit*(lct0-EST(t1)))
	  return FALSE;
	NEXT_ELT(t1) = s, s = t1;
	/* HERE: can enforce upper bounds on DUR(t1), USE(t1) */
      }
    }
    if (s==EOL)
      return TRUE;		/* nothing more to do */
#if 1
    /* My own invention, to get extra pruning for cases like:
       ?- O1 in 16..31, O2 in 12..39, O5 in 12..59, O6 in 32..47, 
          serialized([O1,O2,O5,O6],[16,8,8,20],[edge_finder(true)]).
          O1 in 16..31, % 20..31 with this fix
	  O2 in 12..39,
	  O5 in 12..59,
	  O6 in 32..47

       The problem is that O1 is never checked in the iteration below.
       For seed=67, no l's are checked.
       For seed=47, only O5 and O6 are checked.
    */
    for (l=s; l>EOL; l=NEXT_ELT(l))
      if ((STATUS(l) & STATUS_TARGET) &&
	  USE(l) > pdata->use_threshold) {
	long slack2 = limit*(lct0-EST(l));
	
	if (slack2<area && !not_first(s,l/*,slack2-area*/,limit-USE(l)/*,FALSE*/))
	  return FALSE;
      }
#endif
    /* Martin-Shmoys iteration: s is valid */
    ests = EST(s);
    slack1base = limit*(lct0-ests) - total_min_overlap(arr,n,ests,lct0);
    if (slack1base<0)		/* check necessary condition */
      return FALSE;
    l = tasks_by_area;
    if (j<n-1) {	/* otherwise no relevant l */
      while (s>EOL && l>EOL)
	if (AREA(l)==0 || EST(l)>=lct0 || LCT(l)<=lct0)
	  l = NEXT_AREA(l);
	else {
	  long slack1 = slack1base + min_overlap(l,ests,lct0,USE(l));
	  long slack2 = limit*(lct0-EST(l)) - area;

	  if (slack1>=AREA(l)) { /* task l fits completely */
	    if (slack2>=AREA(l)) /* may be inside and first */
	      l = NEXT_AREA(l);
	    else {		/* may be inside but not first */
	      if (USE(l) > pdata->use_threshold &&
		  !not_first(s,l/*,slack2-AREA(l)*/,limit-USE(l)/*,FALSE*/))
		return FALSE;
	    }
	    
				/* get next s */
	    
	    do
	      area -= AREA(s), s = NEXT_ELT(s);
	    while (s>EOL && EST(s)==ests);
	    if (s>EOL) {
	      ests = EST(s);
	      slack1base = limit*(lct0-ests) - total_min_overlap(arr,n,ests,lct0);
	      if (slack1base<0)		/* check necessary condition */
		return FALSE;
	    }
	    
	  } else {
	    
	    if (slack1 < max_overlap(l,ests,lct0,USE(l))) { /* must stick out at either end */
	      long delta = FLOORDIV(slack1,USE(l));
	      long ub = ests+delta-DUR(l);
	      long lb = lct0-delta;
	      
	      if (EST(l) > ub) { /* must stick out to the right */
		if (!order_tell_task(l, lb, LaST(l)))
		  return FALSE;
		if (USE(l) > pdata->use_threshold) {
		  long maxuse = limit-USE(l);
		  TASK s1;
		  
		  for (s1=s; s1>EOL; s1=NEXT_ELT(s1))
		    if (USE(s1) > maxuse && !tell_before(s1,l))
		      return FALSE;
		}
	      }
	    }
	    l = NEXT_AREA(l);
	  }
	}
    }
    while (j>=0 && seed[j]==lct0)
      j--;
  }
  return TRUE;
}


/* Lean edge finding a la Martin/Shmoys/Wuertz,
   generalized to the cumulative case.
   Comparing tasks T with Si such that est(T) < est(Si) = seed,
   where seed is a unique EST of the tasks.
   */
static BOOL edge_finding_down MAGIC (HIDDEN_PROTO_VOID)
{
  struct cumulative_data *pdata = fd.gdata;
  TASK tasks_by_area = pdata->tasks_area;
  long sorted_at = pdata->time-1;
  int n = pdata->ntargets + pdata->nsources; /* # ACTIVE tasks */
  TASK *arr = pdata->sortarr;
  long *seed = (long *)arr + n;
  long limit = pdata->use_limit;
  int i, j;

  for (i=0; i<n; i++)
    seed[i] = EST(arr[i]);
  qsort_asc_long(seed, n);

  j = 0;
  while (j<n) {
    long est0 = seed[j];
    long area = 0;
    TASK s = EOL;		/* the chain of subsets */
    TASK l;			/* the tasks, by descending area */
    long lcts, slack1base;
    
    if (sorted_at!=pdata->time) {
      sorted_at = pdata->time;
      qsort_asc_lct(arr, n);
    }
    for (i=0; i<n; i++) {
      TASK t1 = arr[i];
	  
      if (EST(t1) >= est0 && AREA(t1)>0) {
	area += AREA(t1);
	if (area > limit*(LCT(t1) - est0))
	  return FALSE;
	NEXT_ELT(t1) = s, s = t1;
	/* HERE: can enforce upper bounds on DUR(t1), USE(t1) */
      }
    }
    if (s==EOL)
      return TRUE;		/* nothing more to do */
#if 1
    /* My own invention.  See comments in edge_finding_up. */
    for (l=s; l>EOL; l=NEXT_ELT(l))
      if ((STATUS(l) & STATUS_TARGET) &&
	  USE(l) > pdata->use_threshold) {
	long slack2 = limit*(LCT(l)-est0);
	
	if (slack2<area && !not_last(s,l/*,slack2-area*/,limit-USE(l)/*,FALSE*/))
	  return FALSE;
      }
#endif
    /* Martin-Shmoys iteration: s is valid */
    lcts = LCT(s);
    slack1base = limit*(lcts-est0) - total_min_overlap(arr,n,est0,lcts);
    if (slack1base<0)		/* check necessary condition */
      return FALSE;
    l = tasks_by_area;
    if (j>0) {			/* otherwise no relevant l */
      while (s>EOL && l>EOL)
	if (AREA(l)==0 || EST(l)>=est0 || LCT(l)<=est0)
	  l = NEXT_AREA(l);
	else {
	  long slack1 = slack1base + min_overlap(l,est0,lcts,USE(l));
	  long slack2 = limit*(LCT(l)-est0) - area;

	  if (slack1>=AREA(l)) { /* task l fits completely */
	    if (slack2>=AREA(l)) /* may be inside and last */
	      l = NEXT_AREA(l);
	    else {		/* may be inside but not last */
	      if (USE(l) > pdata->use_threshold && 
		  !not_last(s,l/*,slack2-AREA(l)*/,limit-USE(l)/*,FALSE*/))
		return FALSE;
	    }
	    
				/* get next s */
	    
	    do
	      area -= AREA(s), s = NEXT_ELT(s);
	    while (s>EOL && LCT(s)==lcts);
	    if (s>EOL) {
	      lcts = LCT(s);
	      slack1base = limit*(lcts-est0) - total_min_overlap(arr,n,est0,lcts);
	      if (slack1base<0)		/* check necessary condition */
		return FALSE;
	    }
	    
	  } else {
	    
	    if (slack1 < max_overlap(l,est0,lcts,USE(l))) { /* must stick out at either end */
	      long delta = FLOORDIV(slack1,USE(l));
	      long ub = est0+delta-DUR(l);
	      long lb = lcts-delta;
	      
	      if (LaST(l) < lb) { /* must stick out to the left */
		if (!order_tell_task(l, EST(l), ub))
		  return FALSE;
		if (USE(l) > pdata->use_threshold) {
		  long maxuse = limit-USE(l);
		  TASK s1;
		  
		  for (s1=s; s1>EOL; s1=NEXT_ELT(s1))
		    if (USE(s1) > maxuse && !tell_before(l,s1))
		      return FALSE;
		}
	      }
	    }
	    l = NEXT_AREA(l);
	  }
	}
    }
    while (j<n && seed[j]==est0)
      j++;
  }
  return TRUE;
}


/* total duration of all tasks in s 
   that are disjunctive with l and with each other 
*/
static long total_dur_disj MAGIC (HIDDEN_PROTO
                                  TASK s, TASK l)
{
  struct cumulative_data *pdata = fd.gdata;
  long minh = USE(l);
  long minuse;
  long maxuse = pdata->use_limit - minh;
  long total = 0;
  TASK s0;

  if (maxuse < pdata->use_limit>>1)
    maxuse = pdata->use_limit>>1;

  for (s0=s; s0>EOL; s0 = NEXT_ELT(s0))
    if (s0!=l && USE(s0)>maxuse) {
      if (minh>USE(s0)) minh = USE(s0);
      total += DUR(s0);
    }

  /* look for a task t with limit-minh < USE(t) <= maxuse */
  minuse = pdata->use_limit - minh;
  if (minuse<maxuse) {
    long maxdur=0;

    for (s0=s; s0>EOL; s0 = NEXT_ELT(s0))
      if (s0!=l && minuse<USE(s0) && USE(s0)<=maxuse)
	if (maxdur<DUR(s0)) maxdur = DUR(s0);

    total += maxdur;    
  }
  return total;
}


/* compute total min. overlap with [est,lct} of all tasks
   that are disjunctive with l and with each other 
*/
static long total_min_overlap_disj MAGIC (HIDDEN_PROTO
					  TASK *arr,
					  int n,
					  TASK l,
					  long est,
					  long lct)
{
  struct cumulative_data *pdata = fd.gdata;
  long minh = USE(l);
  long minuse;
  long maxuse = pdata->use_limit - minh;
  long total = 0;
  int i;

  if (maxuse < pdata->use_limit>>1)
    maxuse = pdata->use_limit>>1;
  for (i=0; i<n; i++) {
    long use = USE(arr[i]);
    
    if (arr[i]!=l && use>maxuse) {
      if (minh>use) minh = use;
      total += min_overlap(arr[i],est,lct,1);
    }
  }

  /* look for a task t with limit-minh < USE(t) <= maxuse */
  minuse = pdata->use_limit - minh;
  if (minuse<maxuse) {
    long ov, maxov=0;

    for (i=0; i<n; i++) {
      long use = USE(arr[i]);
      
      if (arr[i]!=l && minuse<use && use<=maxuse) {
	ov = min_overlap(arr[i],est,lct,1);
	if (maxov<ov) maxov = ov;
      }
    }
    total += maxov;
  }

  return total;
}


/* Lean edge finding a la Martin/Shmoys/Wuertz,
   specialized to the disjunctive case.
   Comparing tasks T with Si such that lct(T) > lct(Si) = seed,
   where seed is a unique LCT of the tasks.
   */
static BOOL edge_finding_disj_up MAGIC (HIDDEN_PROTO_VOID)
{
  struct cumulative_data *pdata = fd.gdata;
  TASK tasks_by_area = pdata->tasks_area;
  long sorted_at = pdata->time-1;
  int n = pdata->ntargets + pdata->nsources; /* # ACTIVE tasks */
  TASK *arr = pdata->sortarr;
  long *seed = (long *)arr + n;
  long limit = pdata->use_limit;
  long s_threshold = pdata->use_threshold;
  long l_threshold = pdata->use_threshold;
  int i, j;

  /* this would delete useful tasks in the "stick out to the right" case
     if (s_threshold < pdata->use_limit>>1)
     s_threshold = pdata->use_limit>>1;
  */
  for (i=0; i<n; i++)
    seed[i] = LCT(arr[i]);
  qsort_asc_long(seed, n);

  /* extra check which is not subsumed by code below */
  /* TODO: is it still true? */
  {
    TASK l = EOL;
    TASK s = EOL;
    long ests = HighInt;

    for (i=0; i<n; i++) {
      TASK t1 = arr[i];
      
      if (ests>EST(t1))
	ests = EST(t1);
      if (l==EOL || USE(t1)>USE(l)) {
	if (l>EOL)
	  NEXT_ELT(l) = s, s = l;
	l = t1;
      } else
	NEXT_ELT(t1) = s, s = t1;
    }
    if (DUR(l)+total_dur_disj(s,l) > seed[n-1]-ests)
      return FALSE;
  }

  j = n-1;
  while (j>=0) {
    long lct0 = seed[j];
    TASK s = EOL;		/* the chain of subsets */
    TASK l;			/* the tasks, by descending area */
    
    if (sorted_at!=pdata->time) {
      sorted_at = pdata->time;
      qsort_desc_est(arr, n);
    }
    for (i=0; i<n; i++) {
      TASK t1 = arr[i];
	  
      if (LCT(t1) <= lct0 && USE(t1) > s_threshold && AREA(t1)>0)
	NEXT_ELT(t1) = s, s = t1;
    }
    /* Martin-Shmoys iteration */
    l = tasks_by_area;
    if (j<n-1) {	/* otherwise no relevant l */
      while (s>EOL && l>EOL)
	if (AREA(l)==0 || EST(l)>=lct0 || LCT(l)<=lct0 || USE(l) <= l_threshold)
	  l = NEXT_AREA(l);
	else {
	  long ests = EST(s);
	  long dur    = total_dur_disj(s,l);
	  long totdur = total_min_overlap_disj(arr,n,l,ests,lct0);
	  long slack1 = (lct0-ests) - totdur;
          long slack2 = (lct0-EST(l)) - dur;
	  long maxuse = limit-USE(l);
	  
	  if (maxuse < limit>>1)
	    maxuse = limit>>1;
	  if (slack1>=DUR(l)) {
	    if (slack2>=DUR(l)) /* may be inside and first */
	      l = NEXT_AREA(l);
	    else		/* may be inside but not first */
	      if (!not_first(s,l/*,slack2-DUR(l)*/,maxuse/*,TRUE*/))
		return FALSE;
	    do
	      s = NEXT_ELT(s);
	    while (s>EOL && EST(s)==ests);
	  } else {
	    if (slack1 < min_overlap(l,ests,lct0,1))
	      return FALSE;
	    else if (slack1 < max_overlap(l,ests,lct0,1)) { /* must stick out at either end */
	      long ub = ests+slack1-DUR(l);
	      long lb = lct0-slack1;

	      if (EST(l) > ub) { /* must stick out to the right */
		if (!order_tell_task(l, lb, LaST(l)))
		  return FALSE;
		{
		  long maxuse = limit-USE(l);
		  TASK s1;
			  
		  for (s1=s; s1>EOL; s1=NEXT_ELT(s1))
		    if (USE(s1) > maxuse && !tell_before(s1,l))
		      return FALSE;
		}
	      }
	    }
	    l = NEXT_AREA(l);
	  }
	}
    }
    while (j>=0 && seed[j]==lct0)
      j--;
  }
  return TRUE;
}


/* Lean edge finding a la Martin/Shmoys/Wuertz,
   specialized to the disjunctive case.
   Comparing tasks T with Si such that est(T) < est(Si) = seed,
   where seed is a unique EST of the tasks.
   */
static BOOL edge_finding_disj_down MAGIC (HIDDEN_PROTO_VOID)
{
  struct cumulative_data *pdata = fd.gdata;
  TASK tasks_by_area = pdata->tasks_area;
  long sorted_at = pdata->time-1;
  int n = pdata->ntargets + pdata->nsources; /* # ACTIVE tasks */
  TASK *arr = pdata->sortarr;
  long *seed = (long *)(arr + n);
  long limit = pdata->use_limit;
  long s_threshold = pdata->use_threshold;
  long l_threshold = pdata->use_threshold;
  int i, j;

  /* this would delete useful tasks in the "stick out to the left" case
     if (s_threshold < limit>>1)
     s_threshold = limit>>1;
  */
  for (i=0; i<n; i++)
    seed[i] = EST(arr[i]);
  qsort_asc_long(seed, n);

  j = 0;
  while (j<n) {
    long est0 = seed[j];
    TASK s = EOL;		/* the chain of subsets */
    TASK l;			/* the tasks, by descending area */
      
    if (sorted_at!=pdata->time) {
      sorted_at = pdata->time;
      qsort_asc_lct(arr, n);
    }
    for (i=0; i<n; i++) {
      TASK t1 = arr[i];
	  
      if (EST(t1) >= est0 && USE(t1) > s_threshold && AREA(t1)>0)
	NEXT_ELT(t1) = s, s = t1;
    }
    /* Martin-Shmoys iteration */
    l = tasks_by_area;
    if (j>0) {		/* otherwise no relevant l */
      while (s>EOL && l>EOL)
	if (AREA(l)==0 || EST(l)>=est0 || LCT(l)<=est0 || USE(l) <= l_threshold)
	  l = NEXT_AREA(l);
	else {
	  long lcts = LCT(s);
	  long dur    = total_dur_disj(s,l);
	  long totdur = total_min_overlap_disj(arr,n,l,est0,lcts);
	  long slack1 = (lcts-est0) - totdur;
          long slack2 = (lcts-EST(l)) - dur;
	  long maxuse = limit-USE(l);
	  
	  if (maxuse < limit>>1)
	    maxuse = limit>>1;	  
	  if (slack1>=DUR(l)) {
	    if (slack2>=DUR(l)) /* may be inside and last */
	      l = NEXT_AREA(l);
	    else		/* may be inside but not last */
	      if (!not_last(s,l/*,slack2-DUR(l)*/,maxuse/*,TRUE*/))
		return FALSE;
	    do
	      s = NEXT_ELT(s);
	    while (s>EOL && LCT(s)==lcts);
	  } else {
	    if (slack1 < min_overlap(l,est0,lcts,1))
	      return FALSE;
	    else if (slack1 < max_overlap(l,est0,lcts,1)) { /* must stick out at either end */
	      long ub = est0+slack1-DUR(l);
	      long lb = lcts-slack1;

	      if (LaST(l) < lb) { /* must stick out to the left */
		if (!order_tell_task(l, EST(l), ub))
		  return FALSE;
		{
		  long maxuse = limit-USE(l);
		  TASK s1;
			  
		  for (s1=s; s1>EOL; s1=NEXT_ELT(s1))
		    if (USE(s1) > maxuse && !tell_before(l,s1))
		      return FALSE;
		}
	      }
	    }
	    l = NEXT_AREA(l);
	  }
	}
    }
    while (j<n && seed[j]==est0)
      j++;
  }
  return TRUE;
}




#if DECOMPOSITION
/* true if d1/\(a1..b1) + amount INCLUDES d2/\(a2..b2) */
static BOOL shifted_domain MAGIC (HIDDEN_PROTO
				  TAGGED a1, TAGGED b1, TAGGED d1,
				  long amount,
				  TAGGED a2, TAGGED b2, TAGGED d2)
{
  if (a1+IStep(amount) != a2 || b1+IStep(amount) != b2)
    return FALSE;

  d1 = fd_lsh(d1,amount);
  d2 = fd_and_interval(d2,a2,b2);
  switch (fd_compare(d2,d1)) {
  case FDI_SUBSET:
  case FDI_EQUAL:
    return TRUE;
  default:
    return FALSE;
  }
}


static BOOL isolated_source MAGIC (HIDDEN_PROTO
				   TASK si)
{
  struct cumulative_data *pdata = fd.gdata;
  int ntasks = pdata->ntasks;
  int ntargets = pdata->ntargets;
  int j;
  long esti = EST(si);

  for (j=0; j<ntargets; j++) {
    TASK sj = SV(j);

    if (STATUS(sj) & STATUS_TARGET) {
      if (si<sj) {
	DIFF dij = DV(si,sj);
	
	if (!shifted_domain(DMIN(dij),DMAX(dij),DDOM(dij), esti,
			    TMIN(sj), TMAX(sj), TDOM(sj)))
	  return FALSE;
      } else {
	DIFF dji = DV(sj,si);
	TAGGED dom;
	
	*fd_neg_internal(DDOM(dji),TaggedZero,&dom) = EmptySet;
	if (!shifted_domain(2*TaggedZero-DMIN(dji), 2*TaggedZero-DMAX(dji), dom, esti,
			    TMIN(sj), TMAX(sj), TDOM(sj)))
	  return FALSE;
      }
    }
  }
  return TRUE;
}



static void decompose MAGIC (HIDDEN_PROTO_VOID)
{
  struct cumulative_data *pdata = fd.gdata;
  int flags = pdata->flags;
  long est = CLPFD_MAXINT;
  long lct = -CLPFD_MAXINT;
  int ntargets = pdata->ntargets;
  int nactive_items = ntargets + pdata->nsources;
  int i;
  
  for (i=0; i<nactive_items; i++) {
    TASK si = SV(i);
    
    if (STATUS(si)&STATUS_TARGET) {
      if (est>EST(si)) est = EST(si);
      if (lct<LCTmax(si)) lct = LCTmax(si);
    }
  }

  /* forget sources that can no longer prune */
  for (i=ntargets; i<nactive_items; i++) {
    TASK si = SV(i);
    
    if ((STATUS(si)&(STATUS_SOURCE+STATUS_TARGET))==STATUS_SOURCE &&
	(LCTmax(si)<=est || lct<=EST(si)) &&
	(!(flags & 0x10) || isolated_source(si)))
      STATUS(si) -= STATUS_SOURCE;
  }
}

#endif

/*
  '$fd_cumulative'(+State0, -State, -Actions).

Filtering algorithm for serialized/[2,3] and cumulative/[4,5].

State0 = State = f(N,Tasks,Diffs,Limit,Flags,NTargets,NSources,Handle,Stamp).

Tasks are the tasks task(Si,SMi,Di,DMi,Ri,RMi,i,_) to be scheduled,
where i in 0..N-1.

Diffs is a list of d(i,j,Mij) where Mij is a mutable whose value is a
domain (a..-Dj)\/(Di..b) if tasks i and j are disjunctive or have a
precedence relation; otherwise, 0 may be in the domain.

Flags include: 0x1 - path consistency over Dij variables
               0x2 - static sets rule
               0x4 - edge finding rule
               0x8 - decomposition rule
	       0x10 - Dij vars required (implied by 0x1)
	       0x20 - cut holes in domains

No unbounded domains!

The following invariants hold when a fixpoint is reached:

[0. cumulative]
        The total task load does not exceed limit anywhere.

[1. differences]
	Si + dij = Sj

[2. path consistency i.e. transitive closure
    Redundant, used only of Flags & 0x1]
	dij + djk = dik 

[3. mutual exclusion]

	If tasks i and j are exclusive, then
		Di=0 | Dj=0 | Si+Di =< Sj | Sj+Dj =< Si
	Hence
		Si+Di =< max(Sj) | Si >= min(Sj)+Dj
                   ##                            ##
		Sj+Dj =< max(Si) | Sj >= min(Si)+Di
                   ##                            ##

Can be tightened by replacing ## by values from relevant Dij variable.

[4. resource restriction]

	Bi0*R0 +...+ Bin*Rm =< L,  at all points in time i, where

	Bij = 1, if Sj =< i < Sj+Dj
	Bij = 0, otherwise

	Pruning:
	If task i can't run during [a,b),
	then [Si,Si+Di) /\ [a,b) must be empty.
	Hence Di=0 | Si+Di =< a | Si >= b.

	If tasks i and j can't overlap during [a,b),
	then [Si,Si+Di) /\ [Sj,Sj+Dj) /\ [a,b) must be empty.
        Hence Di=0 | Dj=0 | Si+Di=<Sj | Si+Di=<a | Si>=b | Sj+Dj=<Si | Sj+Dj=<a | Sj>=b.
                               ##                             ##
	Hence:
		(ect(i)>a & lst(i)<b)
		   -> (Sj+Dj =< max(lst(i),a) | Sj >= min(ect(i),b))
		(ect(j)>a & lst(j)<b)
		   -> (Si+Di =< max(lst(j),a) | Si >= min(ect(j),b))

Can be tightened by replacing ## by values from relevant Dij variable.

[5. "static sets", a redundant constraint due to non-overlapping tasks
    Used only of Flags & 0x2]
	dai>0, ..., dei>0 => Si >= min(Sa,...,Se)+ceil((Da+...+De)/Limit)
	dai<0, ..., dei<0 => Si =< max(Sa+Da,...,Se+De)-ceil((Da+...+De)/Limit)-Di

	where a...e are all disjunctive with i.

	We can't consider all subsets of a...e.
        Instead, for lower bounds, we use all subsets with EST above some threshold.
        For upper bounds, we use all subsets with LCT below some threshold.

[6. edge finding]
    Redundant, used only of Flags & 0x4]

	Derived from the O(n^2) time Martin-Shmoys algorithm.

	UP phase:
	For each unique lct0 in decreasing order:
	  let S = {t|LCT(t) <= lct0}
	  for each s in S:
	    if s can't be first in S, update its lower bound if possible
	  let L = {t|EST(t) < lct0 /\ LCT(t) > lct0}
	  while (S and L not empty)
	    let l = {t in L|area(t) is maximal};
	    if (S can't fit before lct0)
	      fail;
	    if (l can be between and before S)
              L = L\{l},
              S = S\{s in S|est(s)=est(S)};
            else if (l can be between but not before S)
              update lower bound of l;
	      if (l must be after S) 
	        L = L\{l};
	      else
                S = S\{s in S|est(s)=est(S)};
            else if (l cannot be between but can be before S) (*)
              L = L\{l};
            else (l must be after S)
              update lower bound of l;
	      L = L\{l};

	DOWN phase:
	For each unique est0 in increasing order:
	  let S = {t|EST(t) >= est0}
	  for each s in S:
	    if s can't be last in S, update its upper bound if possible
	  let L = {t|EST(t) < est0 /\ LCT(t) > est0}
	  while (S and L not empty)
	    let l = {t in L|area(t) is maximal};
	    if (S can't fit after est0)
	      fail;
	    if (l can be between and after S)
              L = L\{l},
              S = S\{s in S|lct(s)=lct(S)};
            else if (l can be between but not after S)
              update upper bound of l;
	      if (l must be before S) 
	        L = L\{l};
	      else
                S = S\{s in S|lct(s)=lct(S)};
            else if (l cannot be between but can be after S) (**)
              L = L\{l};
            else (l must be before S)
              update upper bound of l;
	      L = L\{l};

	(*)  True if l can overlap the lower part of [est(S),lct0)
	(**) True if l can overlap the upper part of [est0,lct(S))

Implementation: a constant space Waltz style algorithm using
interval consistency.  We maintain a queue of _variables_:

dij in the queue means that [1,2] must be checked.
    Is queued only if sign is determined.
Si  in the queue means that [1,3,4] must be checked.
    Is queued whenever it changes.
[5] and [6] are done last of all.
The whole thing is repeated until the queue is empty after [5] and [6].

Timestamping is used to avoid some useless recomputations:

[1] is known to hold if:
	Dij.checked_at >= max(Dij.told_at, Si.told_at, Sj.told_at)

Var is pruned => Var.told_at := (pdata->time+=1)
[1] is run => Dij.checked_at := pdata->time */
void SPCDECL
prolog_fd_cumulative MAGIC (HIDDEN_PROTO
			    SP_term_ref State0,
			    SP_term_ref State,
			    SP_term_ref Actions)
{
  WAMENV;
  TAGGED tasks, diffs, handle, use_limit_attr, tmp;
  long use_limit, use_threshold, minlst, maxect;
  int i, j, ntasks, ndiffs;
  TASK *arr;
  int ent = -1;			/* disentailed unless otherwise */
  BOOL committed;
  int flags;
  int nactive_items;		/* caches pdata->ntargets + pdata->nsources */
  long total_size, state_stamp;
  struct cumulative_data *pdata;
  char *ptr;

  (void)State0;                 /* [PM] 3.9b5 avoid -Wunused */

  w->numstack_end = NULL;
  fd.fd_overflow = FALSE;

/*    X(0) = RefTerm(State0); */
  X(2) = atom_nil;		/* actions list */
  RefTerm(State) = unify_output_state(w,&handle,&state_stamp,&committed);

  if (!IsVar(handle)) {		/* got [Flag | '$free'(Ptr)] */
    pdata = Pdata(struct cumulative_data,handle);
    fd.gdata = pdata;
    flags = pdata->flags;
    ntasks = pdata->ntasks;
    ndiffs = ntasks*ntasks;
    use_limit_attr = RefTerm(pdata->refbase);
    DerefAttribute(tmp,use_limit_attr);
    use_limit = GetSmall(DomainMax(tmp));
  } else {			/* build persistent state */
				/* compute flags, ntasks, use_limit */
    DerefArg(tmp,X(0),1);		/* get N */
    ntasks = GetSmall(tmp);
    DerefArg(tmp,X(0),4);		/* get Limit */
    use_limit_attr = check_argument(w,tmp,Inf,Sup,Sup);
    DerefAttribute(tmp,use_limit_attr);
    use_limit = GetSmall(DomainMax(tmp));
    DerefArg(tmp,X(0),5);		/* get Flags */
    flags = GetSmall(tmp);
    ndiffs = (flags&0x10) ? ntasks*ntasks : 0;

    total_size = ((18*ntasks + 6*ndiffs)<<LogSizeOfWord);
    pdata = Palloc(struct cumulative_data, total_size, handle);
    fd.gdata = pdata;
    ptr = (char *)(pdata+1);
    pdata->target = (TASK *)ptr;
    ptr = (char *)(pdata->target+ntasks);
    pdata->sortarr = (TASK *)ptr;
    ptr = (char *)(pdata->sortarr+3*ntasks);
    /* arrays for task variables */
    pdata->task.min = (TAGGED *)ptr;
    ptr = (char *)(pdata->task.min+ntasks);
    pdata->task.max = (TAGGED *)ptr;
    ptr = (char *)(pdata->task.max+ntasks);
    pdata->task.fdset = (TAGGED *)ptr;
    ptr = (char *)(pdata->task.fdset+ntasks);
    pdata->task.mindur = (long *)ptr;
    ptr = (char *)(pdata->task.mindur+ntasks);
    pdata->task.maxdur = (long *)ptr;
    ptr = (char *)(pdata->task.maxdur+ntasks);
    pdata->task.minuse = (long *)ptr;
    ptr = (char *)(pdata->task.minuse+ntasks);
    pdata->task.maxuse = (long *)ptr;
    ptr = (char *)(pdata->task.maxuse+ntasks);
    pdata->task.area = (long *)ptr;
    ptr = (char *)(pdata->task.area+ntasks);
    pdata->task.checked_at = (long *)ptr;
    ptr = (char *)(pdata->task.checked_at+ntasks);
    pdata->task.told_at = (long *)ptr;
    ptr = (char *)(pdata->task.told_at+ntasks);
    pdata->task.status = (long *)ptr;
    ptr = (char *)(pdata->task.status+ntasks);
    pdata->task.next_queued = (TASK *)ptr;
    ptr = (char *)(pdata->task.next_queued+ntasks);
    pdata->task.next_area = (TASK *)ptr;
    ptr = (char *)(pdata->task.next_area+ntasks);
    pdata->task.next_elt = (TASK *)ptr;
    ptr = (char *)(pdata->task.next_elt+ntasks);
    if (flags & 0x10) {
      pdata->diff.min = (TAGGED *)ptr;
      ptr = (char *)(pdata->diff.min+ndiffs);
      pdata->diff.max = (TAGGED *)ptr;
      ptr = (char *)(pdata->diff.max+ndiffs);
      pdata->diff.fdset = (TAGGED *)ptr;
      ptr = (char *)(pdata->diff.fdset+ndiffs);
      pdata->diff.checked_at = (long *)ptr;
      ptr = (char *)(pdata->diff.checked_at+ndiffs);
      pdata->diff.told_at = (long *)ptr;
      ptr = (char *)(pdata->diff.told_at+ndiffs);
      pdata->diff.next_queued = (long *)ptr;
      ptr = (char *)(pdata->diff.next_queued+ndiffs);
    }
    if (ptr != (char *)(pdata+1)+total_size)
      printf("SHOULDN'T HAPPEN: expected ptr=0x%p, got 0x%p\ntasks",
	     (char *)(pdata+1)+total_size, ptr);

    pdata->destructor = cumulative_destructor;
    FD_STORE_SPENV(pdata->spenv);
    pdata->nrefs = ndiffs + 4*ntasks + 1;
    pdata->refbase = SP_alloc_term_refs(pdata->nrefs);
    pdata->drefbase = pdata->refbase + 3*ntasks + 1;
    pdata->stamp = state_stamp-1; /* DON'T trust initially */
    pdata->ntasks = ntasks;
#if CSTATE_ONLY
    pdata->ntargets = ntasks;
    pdata->nsources = 0;
    pdata->ndone = 0;
#endif
    pdata->flags = flags;
    RefTerm(pdata->refbase) = use_limit_attr;
    pdata->use_limit = use_limit;

    DerefArg(tasks,X(0),2);
    for (i=0; i<ntasks; i++) {
      TASK si = SV(i) = i;
      TAGGED elt, tmp1;
    
      DerefCar(elt,tasks);
      DerefCdr(tasks,tasks);
      DerefArg(tmp1,elt,1);	/* start's domain variable */
      TRefOrig(si) = tmp1;
      DerefArg(tmp1,elt,2);	/* start's attribute */
      TRefOrigAttr(si) = tmp1;
      DerefArg(tmp1,elt,4);	/* duration's attribute */
      TRefDurAttr(si) = tmp1;
      DerefArg(tmp1,elt,6);	/* resource's attribute */
      TRefUseAttr(si) = tmp1;
      STATUS(si) = (STATUS_SOURCE+STATUS_TARGET)<<4;
    }
    
    DerefArg(diffs,X(0),3);
    while (TagIsLST(diffs))
      {
	TAGGED delt, tmp;
	int ix, jx;
      
	DerefCar(delt,diffs);	/* get d/3 */
	DerefCdr(diffs,diffs);
	DerefArg(tmp,delt,1);	/* get I */
	ix = GetSmall(tmp);
	DerefArg(tmp,delt,2);	/* get J */
	jx = GetSmall(tmp);
	DerefArg(tmp,delt,3);	/* get set mutable */
	if (ix<jx) {
	  DRefMut(DV(ix-1,jx-1)) = tmp;
	} else {
	  printf("SHOULDN'T HAPPEN: expected %d < %d\n", ix, jx);
	}
      }
  }

  /* RESUME HERE */

#if !CSTATE_ONLY
  DerefArg(tmp,X(0),6);
  pdata->ntargets = GetSmall(tmp);
  DerefArg(tmp,X(0),7);
  pdata->nsources = GetSmall(tmp);
  nactive_items = pdata->ntargets + pdata->nsources;
#endif
  pdata->time = 0;
  pdata->queued = EOL;
  arr = pdata->sortarr;
  init_profile();
  fd.profile = empty_profile();

  if (state_stamp != pdata->stamp ||
      use_limit != pdata->use_limit) { /* trust nothing */
    use_threshold = use_limit;

    /* refresh all task variables */
  
#if CSTATE_ONLY
    pdata->ntargets = nactive_items = ntasks - pdata->ndone;
    pdata->nsources = 0;
#endif
    for (i=0; i<nactive_items; i++)
      {
	TASK si = SV(i);
	TAGGED tmp1, *arg;
      
	arr[i] = si;
	TTOLD_AT(si) = 0;
	TCHECKED_AT(si) = -1;	/* not checked initially */
	STATUS(si) |= STATUS(si)>>4;
	if (STATUS(si) & STATUS_TARGET) {
	  tmp1 = TRefOrigAttr(si);
	  DerefAttribute(tmp1,tmp1); /* dom/4 term */
	  arg = TagToArg(tmp1,0);
	  TDOM(si) = arg[1];
	  TMIN(si) = arg[2];
	  TMAX(si) = arg[3];
	  tmp1 = TRefDurAttr(si);
	  DerefAttribute(tmp1,tmp1); /* dom/4 term */
	  arg = TagToArg(tmp1,0);
	  DUR(si) = GetSmall(arg[2]);
	  DURmax(si) = GetSmall(arg[3]);
	  tmp1 = TRefUseAttr(si);
	  DerefAttribute(tmp1,tmp1); /* dom/4 term */
	  arg = TagToArg(tmp1,0);
	  USE(si) = GetSmall(arg[2]);
	  USEmax(si) = GetSmall(arg[3]);
	  if (USE(si)>use_limit && DUR(si)>0)
	    goto ret;
	  if (use_threshold > use_limit-USE(si))
	    use_threshold = use_limit-USE(si);
	  AREA(si) = DUR(si)*USE(si);
	  TNEXT_QUEUED(si) = pdata->queued, pdata->queued = si;
	} else
	  TNEXT_QUEUED(si) = PAST_EOL;
      }
    
    pdata->use_threshold = use_threshold;
    
  } else {

    /* refresh all task variables */
    
#if CSTATE_ONLY
    nactive_items = pdata->nsources + pdata->ntargets;
#endif
    for (i=0; i<nactive_items; i++)
      {
	TASK si = SV(i);
	TAGGED tmp1, *arg;
     
	arr[i] = si;
	TTOLD_AT(si) = 0;
	TCHECKED_AT(si) = -1;	/* not checked initially */
	if (STATUS(si) & STATUS_TARGET) {
	  BOOL pruned = FALSE;
	  
	  tmp1 = TRefOrigAttr(si);
	  DerefAttribute(tmp1,tmp1); /* dom/4 term */
	  arg = TagToArg(tmp1,0);
	  TDOM(si) = arg[1];
	  if (TMIN(si)!=arg[2] || TMAX(si)!=arg[3]) {
	    pruned = TRUE;
	    TMIN(si) = arg[2];
	    TMAX(si) = arg[3];
	  }
	  tmp1 = TRefDurAttr(si);
	  DerefAttribute(tmp1,tmp1); /* dom/4 term */
	  arg = TagToArg(tmp1,0);
	  if (DUR(si)!=GetSmall(arg[2])) {
	    pruned = TRUE;
	    DUR(si) = GetSmall(arg[2]);
	  }
	  DURmax(si) = GetSmall(arg[3]);
	  tmp1 = TRefUseAttr(si);
	  DerefAttribute(tmp1,tmp1); /* dom/4 term */
	  arg = TagToArg(tmp1,0);
	  if (USE(si)!=GetSmall(arg[2])) {
	    pruned = TRUE;
	    USE(si) = GetSmall(arg[2]);
	  }
	  USEmax(si) = GetSmall(arg[3]);
	  if (pruned) {
	    AREA(si) = DUR(si)*USE(si);
	    pdata->time = -1;
	    (void)propagate(si, TMIN(si), TMAX(si));
	  }
	}
      }
  }
  
  /* enqueue active dij variables that have determined sign */

  if (flags & 0x10)
  for (i=0; i<nactive_items; i++) {
      TASK si = SV(i);
      
      for (j=i+1; j<nactive_items; j++) {
	TASK sj = SV(j);
	DIFF dij = si<sj ? DV(si,sj) : DV(sj,si);
       
	DCHECKED_AT(dij) = -1;	/* not checked initially */
	DTOLD_AT(dij) = 0;
	if ((STATUS(si)|STATUS(sj)) & STATUS_TARGET) {
	  TAGGED tmp = DRefMut(dij);
	  
	  tmp = CTagToArg(tmp,1);	/* get set */
	  DDOM(dij) = tmp;
	  DMIN(dij) = fd_min(tmp);
	  DMAX(dij) = fd_max(tmp);
	  if (DSIGN(dij)!=0 && ((STATUS(si)|STATUS(sj)) & STATUS_TARGET))
	    DNEXT_QUEUED(dij) = pdata->queued, pdata->queued = dij;
	  else
	    DNEXT_QUEUED(dij) = PAST_EOL;
	}
      }
    }

  /* sort TARGETS by ascending area, then build reverse list */
  qsort_asc_area(arr, pdata->ntargets);
  pdata->tasks_area = EOL;
  for (i=0; i<pdata->ntargets; i++) {
    NEXT_AREA(arr[i]) = pdata->tasks_area;
    pdata->tasks_area = arr[i];
  }

				/* enter all initial reserved intervals */
				/* avoid expensive O(N^2) behavior */
  qsort_desc_lst(arr, nactive_items);
  minlst = CLPFD_MAXINT;
  maxect = -CLPFD_MAXINT;
  for (i=0; i<nactive_items; i++) {
    TASK task = arr[i];
    long lst = LaST(task);
    long ect = ECT(task);
    long use = USE(task);
    
    if (lst < ect && use>0) {
      if (minlst > lst) minlst = lst;
      if (maxect < ect) maxect = ect;
      if (!profile_update_check(lst, ect, use))
	goto ret;
    }
  }

  if (fd.profile && state_stamp==pdata->stamp && use_limit==pdata->use_limit) {
    /* enqueue for rule [4] */
    long maxneed = use_limit - profile_maxerg(fd.profile,minlst,maxect);
    
    for (j=0; j<pdata->ntargets; j++) {
      TASK sj = SV(j);
	
      if (TNEXT_QUEUED(sj)==PAST_EOL &&
	  (STATUS(sj) & STATUS_TARGET) &&
	  EST(sj)<maxect && LCT(sj)>minlst &&
	  DUR(sj)>0 &&
	  USE(sj)>maxneed)
	TNEXT_QUEUED(sj) = pdata->queued, pdata->queued = sj;
    }
  }
  
  pdata->use_limit = use_limit;
  pdata->stamp = state_stamp+1;
  
  /* END OF RESUMPTION */

  /* the fixpoint algorithm */
fixpoint:
  while (pdata->queued != EOL)
    {
      TASK var = pdata->queued;
      
      if (var < pdata->ntasks)
	{
	  TASK si = var;
	  long duri = DUR(si);
	  long usei = USE(si);
	  long restuse = use_limit-usei;

	  pdata->queued = TNEXT_QUEUED(si);
	  TNEXT_QUEUED(si) = PAST_EOL; /* enable var to be enqueued again */
	  if (TMIN(si)==TMAX(si) &&
	      duri==DURmax(si) &&
	      usei==USEmax(si))
	    STATUS(si) -= STATUS_TARGET;
	    {
	      long esti = EST(si);
	      long lsti = LaST(si);
	      long ecti = esti+duri;
	      long lcti = lsti+duri;
	      long maxerg = profile_maxerg(fd.profile, esti, lcti);
	      
				/* rule [4] */
	      if (esti<lsti &&/* otherwise subsumed by [0] */
		  duri>0 && maxerg>restuse) /* otherwise no conflict */
		{
		  long begin, end, h;
		  PROFILE tmp1, tmp2;

		  tmp1 = profile_exclude_one(fd.profile, esti, lcti, restuse, lsti, ecti, usei);
		  for (tmp2=tmp1; profile_next(tmp2, &begin, &end, &h, &tmp2); )
		    {
		      if (!order_tell_hole(si, begin-duri+1, end-1))
			goto ret;
		    }
		  profile_dispose(tmp1);
		}

	      for (j=0; j<nactive_items; j++)
		{
		  TASK sj = SV(j);
		  
		  if (si!=sj && TNEXT_QUEUED(sj)==PAST_EOL)
		    {
		      long durj = DUR(sj);
		      long usej = USE(sj);
		      
				/* rule [1] */
		      if (!(flags & 0x10))
			;
		      else if (si<sj)
			{
			  DIFF dv = DV(si,sj);
			  
			  if (DNEXT_QUEUED(dv)==PAST_EOL &&
			      !check_plus(si, dv, sj))
			    goto ret;
			}
		      else
			{
			  DIFF dv = DV(sj,si);
			  
			  if (DNEXT_QUEUED(dv)==PAST_EOL &&
			      !check_plus(sj, dv, si))
			    goto ret;
			}
				/* rule [3] */
		      if (duri>0 && durj>0 && usej>restuse && TSIGN(si,sj)==0) {/* otherwise no conflict */
			long di = duri;
			long dj = DUR(sj);
			long dk;

			if (flags & 0x10) { /* tighten di, dj by looking at dij */
			  DIFF dv = sj<si ? DV(sj,si) : DV(si,sj);
			  TAGGED set = DDOM(dv);

			  if (DSIGN(dv)!=0) goto done_3;
			  while (set!=EmptySet &&
				 val_vs_range(TaggedZero,CTagToCar(set))==CMP_AFTER) {
			    dj = -GetSmall(RangeMax(CTagToCar(set)));
			    set = CTagToCdr(set);
			  }
			  if (set!=EmptySet)
			    di = GetSmall(RangeMin(CTagToCar(set)));

			  if (sj<si) { /* swap and negate */
			    dk = -di; di = -dj; dj = dk;
			  }
			}
			if (!order_tell_hole(si, LaST(sj)-di+1, EST(sj)+dj-1) ||
			    !order_tell_hole(sj, LaST(si)-dj+1, EST(si)+di-1))
			  goto ret;
		      }
		    done_3:
		      if (TNEXT_QUEUED(si)!=PAST_EOL) /* invalidates [4b] */
			break;

				/* rule [4b] */
		      if (duri>0 && durj>0 && maxerg+usej>restuse &&/* otherwise no conflict */
			  usej<=restuse && /* otherwise subsumed by [3] */
			  esti<lsti &&	/* otherwise subsumed by [4] */
			  Tlt(TMIN(sj),TMAX(sj))) /* otherwise subsumed by [4] */
			{
			  long estj = EST(sj);
			  long lstj = LaST(sj);
			  long ectj = estj+durj;
			  long lctj = lstj+durj;
			  long overlapa = esti > estj ? esti : estj;
			  long overlapb = lcti < lctj ? lcti : lctj;
			  long begin, end, h;
			  PROFILE tmp1, tmp2;

			  if (overlapa<overlapb)
			    {
			      tmp1 = profile_exclude_two(fd.profile,
							 overlapa, overlapb, restuse-usej,
							 lsti, ecti, usei,
							 lstj, ectj, usej);
			      for (tmp2=tmp1; profile_next(tmp2, &begin, &end, &h, &tmp2); )
				{
				  long a = begin;
				  long b = end;
				      
				  if (ecti>a && lsti<b &&
				      !order_tell_hole(sj,
						       (lsti>a ? lsti : a)-durj+1,
						       (ecti<b ? ecti : b)-1))
				    goto ret;
				  if (ectj>a && lstj<b &&
				      !order_tell_hole(si,
						       (lstj>a ? lstj : a)-duri+1,
						       (ectj<b ? ectj : b)-1))
				    goto ret;
				}
			      profile_dispose(tmp1);
			    }
			}




		    }
		}
	    }
	}
      else
	{			/* var is a dij variable */
	  int k;
	  TASK si, sj;
	  
	  pdata->queued = DNEXT_QUEUED(var);
	  DNEXT_QUEUED(var) = PAST_EOL; /* enable var to be enqueued again */
	  sj = var%ntasks;
	  si = var/ntasks-1;		/* si<sj of course */
	  
				/* rules [1],[2] */
	    {
	      if (TNEXT_QUEUED(si)==PAST_EOL &&
		  TNEXT_QUEUED(sj)==PAST_EOL &&
		  !check_plus(si, var, sj))
		goto ret;
	      if (flags & 0x1)
		for (k=0; k<nactive_items; k++) {
		  TASK sk = SV(k);
		  
		  if (sk!=si && sk!=sj)
		    {
		      if (sk<si)
			{
			  DIFF dv1 = DV(sk,si);
			  DIFF dv2 = DV(sk,sj);

			  if (DNEXT_QUEUED(dv1)==PAST_EOL && DSIGN(dv1)!=0 &&
			      DNEXT_QUEUED(dv2)==PAST_EOL && DSIGN(dv2)!=0 &&
			      !tell_plus(dv1, var, dv2))
			    goto ret;
			}
		      else if (sk<sj)
			{
			  DIFF dv1 = DV(si,sk);
			  DIFF dv2 = DV(sk,sj);

			  if (DNEXT_QUEUED(dv1)==PAST_EOL && DSIGN(dv1)!=0 &&
			      DNEXT_QUEUED(dv2)==PAST_EOL && DSIGN(dv2)!=0 &&
			      !tell_plus(dv1, dv2, var))
			    goto ret;
			}
		      else
			{
			  DIFF dv1 = DV(sj,sk);
			  DIFF dv2 = DV(si,sk);

			  if (DNEXT_QUEUED(dv1)==PAST_EOL && DSIGN(dv1)!=0 &&
			      DNEXT_QUEUED(dv2)==PAST_EOL && DSIGN(dv2)!=0 &&
			      !tell_plus(var, dv1, dv2))
			    goto ret;
			}
		    }
		}
	    }
	}
    }
				/* rule [5] */
  if (flags & 0x2)
    {
      if (!static_sets_up() || !static_sets_down())
	goto ret;
      else if (pdata->queued != EOL)
	goto fixpoint;
    }
				/* rule [6] */
  if (flags & 0x4)
    {
      if (!edge_finding_up() || !edge_finding_down())
	goto ret;
      if (pdata->use_limit>1)
	if (!edge_finding_disj_up() || !edge_finding_disj_down())
	  goto ret;
      if (pdata->queued != EOL)
	goto fixpoint;
    }

#if DECOMPOSITION
  if (flags & 0x8)
    decompose();
#endif
  
  for (i=0; i<pdata->ntargets; i++) {
    TASK si = SV(i);
  
    if (TTOLD_AT(si)>0) {
      TAGGED dom = TDOM(si);
      
      if (!OnHeap(TagToLST(dom)))
	TDOM(si) = fd_localize(w,fd_and_interval(dom,TMIN(si),TMAX(si)));
      else
	TDOM(si) = ERRORTAG;
    }
  }
    
  /* OK to GC from here */
  
  ent = 1;
  for (i=0; i<pdata->ntargets; i++) {
    TASK si = SV(i);

    if (STATUS(si) & STATUS_TARGET)
      ent = 0;
    if (TTOLD_AT(si)>0) {
      TAGGED dom = TDOM(si);
      
      if (dom!=ERRORTAG)
	request_tell(w, TRefOrigAttr(si), TRefOrig(si), dom, 2, 3);
      else
	request_tell_interval(w, TRefOrigAttr(si), TRefOrig(si), TMIN(si), TMAX(si), 2, 3);
    }
  }
  if (ent) goto ret;

  /* partition into SOURCE+TARGET and SOURCE */

  {
    int delta;
    int inf = 0;
    int sup = pdata->ntargets-1 /* was nactive_items - 1 */;
    TASK item = SV(sup); /* sup is the hole */
    
    while (inf<sup) {
      while (inf<sup && (STATUS(SV(inf)) & STATUS_TARGET)) inf++;
      if (inf==sup) break;
      SV(sup) = SV(inf); /* inf is the hole */
      sup--;
      while (inf<sup && !(STATUS(SV(sup)) & STATUS_TARGET)) sup--;
      if (inf==sup) break;
      SV(inf) = SV(sup); /* sup is the hole */
      inf++;
    }
    SV(sup) = item;
    if (STATUS(item) & STATUS_TARGET) sup++;
    delta = sup - pdata->ntargets;
    pdata->ntargets += delta;
    pdata->nsources -= delta;
  }
  
  /* partition into SOURCE and 0 */

#if DECOMPOSITION
  if (flags & 0x8)
    {
      int delta;
      int inf = pdata->ntargets;
      int sup = inf+pdata->nsources-1 /* was ntasks - pdata->ndone - 1 */;
      TASK item = SV(sup); /* sup is the hole */
    
      while (inf<sup) {
	while (inf<sup && (STATUS(SV(inf)) & STATUS_SOURCE)) inf++;
	if (inf==sup) break;
	SV(sup) = SV(inf); /* inf is the hole */
	sup--;
	while (inf<sup && !(STATUS(SV(sup)) & STATUS_SOURCE)) sup--;
	if (inf==sup) break;
	SV(inf) = SV(sup); /* sup is the hole */
	inf++;
      }
      SV(sup) = item;
      if (STATUS(item) & STATUS_SOURCE) sup++;
      delta = sup - nactive_items;
      pdata->nsources += delta;
      nactive_items += delta;
#if CSTATE_ONLY
      if (committed)
	pdata->ndone = ntasks - sup;
#endif
    }
#endif

  if (committed)
    for (i=pdata->ntargets; i<nactive_items; i++) {
      TASK si = SV(i);
      
      STATUS(si) &= ~(STATUS_TARGET<<4);
    }

#if KNAPSACK
  {
    long estall = CLPFD_MAXINT;
    long lctall = -CLPFD_MAXINT;
    long prevect, prevlct, prevdelta;
    long nextlst, nextest, nextect, nextdelta;
    long totdur = 0;
    long slack, lb, ub, cdur, mindur, thr;
    PROFILE p2;
    int cand[20];
    TAGGED dom[20];
    long part_slack[20];
    char *sep;
    int nc, part, npart, iter;
    BOOL change = TRUE;

    for (i=0; i<pdata->ntasks; i++) {
      totdur += DUR(i);
      if (estall>EST(i))
	estall = EST(i);
      if (lctall<LCT(i))
	lctall = LCT(i);
      if (!(dom[i] = TDOM(i)))
	dom[i] = fd_interval(TMIN(i),TMAX(i));
    }

    /* do we have any partitions? */
    for (npart=1, p2=profile;
	 profile_next(p2, &nextlst, &nextect, &nextdelta, &p2); )
      if (nextlst>estall && nextect<lctall)
	npart++;
    if (npart==1)
      goto ret;
    
    for (iter=0; change; iter++) {
      change = FALSE;
      slack = lctall-estall-totdur;
      for (part=0; part<npart; part++)
	if (iter>0)
	  slack -= part_slack[part];
        else
	  part_slack[part] = 0;
      prevect = prevlct = estall;
      for (part=0, p2=fd.profile;
	   profile_next(p2, &nextlst, &nextect, &nextdelta, &p2) ||
	     (prevect<lctall && (nextlst=nextest=nextect=lctall,TRUE)) ; ) {
	/* treat the interval [prevect,nextlst] */
	nextdelta = 0;
	if (prevect<nextlst) {
	  cdur = 0;
	  mindur = 0;
	  for (i=0, nc=0; i<pdata->ntasks; i++) {
	    if (LaST(i)==nextlst && ECT(i)==nextect)
	      nextdelta = LaST(i)-EST(i);
	    if (pdata->task.mindur[i]<=nextlst-prevect &&	/* not too long */
		LaST(i)>=ECT(i)) { /* no compulsory part */
	      switch (fd_compare_interval(dom[i],MakeSmall(prevect),MakeSmall(nextlst))) {
	      case FDI_DISJOINT:
		break;
	      case FDI_SUBSET:
	      case FDI_EQUAL:
		mindur += DUR(i);
		break;
	      case FDI_INTERSECT:
	      case FDI_SUPERSET:
		cand[nc++] = i;
		cdur += DUR(i);
		break;
	      }
	    }
	  }
	  nextest = nextlst-nextdelta;
	  lb = nextest-prevlct-(slack+part_slack[part])-mindur;
	  ub = nextlst-prevect-mindur;
	  thr = nextest-prevlct+(prevdelta<nextdelta ? prevdelta:nextdelta)-mindur;
	  if (lb>0 || ub<cdur) {
	    printf("*** knapsack iter=%d lb=%d ub=%d dur=", iter, lb, ub);
	    sep = "[";
	    for (i=0; i<nc; i++) {
	      printf("%s%d", sep, DUR(cand[i]));
	      sep = ",";
	    }
	    printf("]");
	    if (cdur>ub) cdur = ub;
	    if (cdur>thr) {
	      long more = nextest-prevlct-(cdur+mindur);
	      printf(" prune if >%d", thr);
	      if (more>0) {
		printf(" slack>=%d", more);
		if (part_slack[part]<more) {
		  part_slack[part] = more;
		  change = TRUE;
		}
	      }
	    }
	    printf("\n");
	  }
	  part++;
	}
	prevdelta = nextdelta;
	prevect = nextect;
	prevlct = nextect+nextdelta;
      }
    }
  }
#endif

 ret:
#if !CSTATE_ONLY
  CTagToArg(X(0),6) = MakeSmall(pdata->ntargets);
  CTagToArg(X(0),7) = MakeSmall(pdata->nsources);
#endif
  Pfree(ent==1,handle,pdata);
  RefTerm(Actions) = request_done(w, ent, 2, 3);
  if (!fd_check_overflow())
    SP_fail();
}

