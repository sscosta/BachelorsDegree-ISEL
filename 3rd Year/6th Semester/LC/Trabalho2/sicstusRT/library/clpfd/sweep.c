/* Copyright(C) 2000-2002, Swedish Institute of Computer Science */

/****************************************************************

 Non-overlapping 1D and 2D objects, with options.
 Based on Nicolas Beldiceanu's generic sweep algorithm.

 ****************************************************************/

#include "fd.h"

#define CSTATE_ONLY 0

#if MULTI_SP_AWARE
#define qsort_asc_longswap(A1,A2,A3,A4) qsort_asc_longswap(HIDDEN_ARG, A1,A2,A3,A4)
#define qsort_asc_longmed3(A1,A2,A3) qsort_asc_longmed3(HIDDEN_ARG, A1,A2,A3)
#define qsort_asc_long(A1,A2) qsort_asc_long(HIDDEN_ARG, A1,A2)
#if 0                           /* not redefined. It uses FD_SETUP_SPENV to access the SPEnv */
#define disjoint1_destructor(A1) disjoint1_destructor(HIDDEN_ARG, A1)
#endif
#define cmp_desc_dur1d(A1,A2) cmp_desc_dur1d(HIDDEN_ARG, A1,A2)
#define qsort_desc_dur1dswap(A1,A2,A3,A4) qsort_desc_dur1dswap(HIDDEN_ARG, A1,A2,A3,A4)
#define qsort_desc_dur1dmed3(A1,A2,A3) qsort_desc_dur1dmed3(HIDDEN_ARG, A1,A2,A3)
#define qsort_desc_dur1d(A1,A2) qsort_desc_dur1d(HIDDEN_ARG, A1,A2)
#define cmp_desc_est1d(A1,A2) cmp_desc_est1d(HIDDEN_ARG, A1,A2)
#define qsort_desc_est1dswap(A1,A2,A3,A4) qsort_desc_est1dswap(HIDDEN_ARG, A1,A2,A3,A4)
#define qsort_desc_est1dmed3(A1,A2,A3) qsort_desc_est1dmed3(HIDDEN_ARG, A1,A2,A3)
#define qsort_desc_est1d(A1,A2) qsort_desc_est1d(HIDDEN_ARG, A1,A2)
#define cmp_desc_lst1d(A1,A2) cmp_desc_lst1d(HIDDEN_ARG, A1,A2)
#define qsort_desc_lst1dswap(A1,A2,A3,A4) qsort_desc_lst1dswap(HIDDEN_ARG, A1,A2,A3,A4)
#define qsort_desc_lst1dmed3(A1,A2,A3) qsort_desc_lst1dmed3(HIDDEN_ARG, A1,A2,A3)
#define qsort_desc_lst1d(A1,A2) qsort_desc_lst1d(HIDDEN_ARG, A1,A2)
#define cmp_asc_ect1d(A1,A2) cmp_asc_ect1d(HIDDEN_ARG, A1,A2)
#define qsort_asc_ect1dswap(A1,A2,A3,A4) qsort_asc_ect1dswap(HIDDEN_ARG, A1,A2,A3,A4)
#define qsort_asc_ect1dmed3(A1,A2,A3) qsort_asc_ect1dmed3(HIDDEN_ARG, A1,A2,A3)
#define qsort_asc_ect1d(A1,A2) qsort_asc_ect1d(HIDDEN_ARG, A1,A2)
#define cmp_asc_lct1d(A1,A2) cmp_asc_lct1d(HIDDEN_ARG, A1,A2)
#define qsort_asc_lct1dswap(A1,A2,A3,A4) qsort_asc_lct1dswap(HIDDEN_ARG, A1,A2,A3,A4)
#define qsort_asc_lct1dmed3(A1,A2,A3) qsort_asc_lct1dmed3(HIDDEN_ARG, A1,A2,A3)
#define qsort_asc_lct1d(A1,A2) qsort_asc_lct1d(HIDDEN_ARG, A1,A2)
#define cmp_asc_start(A1,A2) cmp_asc_start(HIDDEN_ARG, A1,A2)
#define qsort_asc_startswap(A1,A2,A3,A4) qsort_asc_startswap(HIDDEN_ARG, A1,A2,A3,A4)
#define qsort_asc_startmed3(A1,A2,A3) qsort_asc_startmed3(HIDDEN_ARG, A1,A2,A3)
#define qsort_asc_start(A1,A2) qsort_asc_start(HIDDEN_ARG, A1,A2)
#define cmp_asc_end(A1,A2) cmp_asc_end(HIDDEN_ARG, A1,A2)
#define qsort_asc_endswap(A1,A2,A3,A4) qsort_asc_endswap(HIDDEN_ARG, A1,A2,A3,A4)
#define qsort_asc_endmed3(A1,A2,A3) qsort_asc_endmed3(HIDDEN_ARG, A1,A2,A3)
#define qsort_asc_end(A1,A2) qsort_asc_end(HIDDEN_ARG, A1,A2)
#define extend_forbidden_region1d(A1,A2,A3,A4) extend_forbidden_region1d(HIDDEN_ARG, A1,A2,A3,A4)
#define prune_item1d(A1,A2,A3) prune_item1d(HIDDEN_ARG, A1,A2,A3)
#define prune_before1d(A1,A2) prune_before1d(HIDDEN_ARG, A1,A2)
#define total_area1d(A1,A2,A3,A4,A5) total_area1d(HIDDEN_ARG, A1,A2,A3,A4,A5)
#define not_first(A1,A2) not_first(HIDDEN_ARG, A1,A2)
#define not_last(A1,A2) not_last(HIDDEN_ARG, A1,A2)
#define edge_finding1d_up(A1) edge_finding1d_up(HIDDEN_ARG, A1)
#define edge_finding1d_down(A1) edge_finding1d_down(HIDDEN_ARG, A1)
#define next_start1d(A1,A2,A3,A4,A5,A6) next_start1d(HIDDEN_ARG, A1,A2,A3,A4,A5,A6)
#define next_end1d(A1,A2,A3,A4) next_end1d(HIDDEN_ARG, A1,A2,A3,A4)
#define prev_start1d(A1,A2,A3) prev_start1d(HIDDEN_ARG, A1,A2,A3)
#define prev_end1d(A1,A2,A3,A4,A5) prev_end1d(HIDDEN_ARG, A1,A2,A3,A4,A5)
#define less_constrained1d(A1,A2,A3) less_constrained1d(HIDDEN_ARG, A1,A2,A3)
#define dominates1d(A1,A2,A3) dominates1d(HIDDEN_ARG, A1,A2,A3)
#define disjoint1_filter(A1,A2,A3,A4) disjoint1_filter(HIDDEN_ARG, A1,A2,A3,A4)
#define disjoint1_check(A1) disjoint1_check(HIDDEN_ARG, A1)
#define decompose1d(A1,A2,A3) decompose1d(HIDDEN_ARG, A1,A2,A3)
#define collect_source1d(A1) collect_source1d(HIDDEN_ARG, A1)
#endif /* MULTI_SP_AWARE */

/* Optimization 1: for the 2D case, each rectangle has a witness
   (feasible point) for each of its four sides.  The following
   invariant holds: 

     When a fixpoint is reached (the algorithm suspends),
     for every witness w of a TARGET rectangle r,
     either w is outside the "other" domain of r,
         or w is wit(max(V)) and min(V)=max(V) (w is ignored),
	 or w is not in any forbidden region of r wrt. other rectangles.

   Worth noting:
   - When a rectangle is pruned (by the constraint or on wakeup),
     the corresponding witness is explicitly invalidated.
   - When a rectangle is found safe by domination (Opt. 4),
     its witness may still be invalid and must be copied from the
     dominating rectangle.
   - When min(dur(X)) is increased, all witnesses of X become
     invalid, since the forbidden regions of X may engulf them
   - When the constraint wakes up on forward execution, the only
     endangered witnesses are those in reach of any newly pruned
     rectangles (i.e. TARGET and pruned_at > -1).     

   - On backtracking, there are several options.
     Recall that not all rectangles become TARGET.
     * Invalidate all witnesses so as to force a sweep, or
     * Make all rectangles TARGET so as to consider them when checking
       witnesses, or
     * When checking witnesses, consider all rectangles instead of
       the newly pruned one.

     We chose the third option.  This implies that in Opt. 3,
     we may safely drop the TARGET property on backtracking,
     but not on forward execution.
*/

/* Optimization 2: items have SOURCE and TARGET properties.
   SOURCE = can prune, TARGET = can be pruned.

   Initially, all items are SOURCE+TARGET.
   Transitions:

   SOURCE+TARGET --> SOURCE:
     when a ground target has been checked

   SOURCE --> NONE:
     a) compute bounding box of all targets.
     b) remove SOURCE property of any source that can't interact with the
	bounding box.

   To reduce scanning, items are stored in an array partitioned as:

   [ SOURCE+TARGET | SOURCE | NONE ]
*/

/* Optimization 3: check obligatory phase initially.
   
   a) Compute all obligatory parts.
   b) Check that there is no overlap.  Requires sorting, so it's O(N lg N).
   c) For all ground targets, remove TARGET property.
*/
#define INITIAL_CHECK 1

/* Optimization 4: detect when a target can't be pruned.
   Based on the fact that one rectangle dominates another one.
   Not applicable in the presence of margins or wrap-around.

   Let: ori_k(r) = est_k(r) if sweeping right or up, or
                   lst_k(r) otherwise.

   For each dimension k (0 or 1) and sweep direction s (+1 or -1),
   store the most dominating target that was not pruned.     Minimize
   by lexicographic order: 
   * s*ori_k(r)
   * -dim_k(r)
   * -dim_k'(r)
   * -(lst_k(r)-est_k(r))
   *  (lst_k'(r)-est_k'(r))

  Target r1 dominates target r2 iff:
  * ori_k(r1)=ori_k(r2)
  * no holes in dom_k'(r2)
  * dur_k(r1) >= dur_k(r2)
  * dur_k'(r1) >= dur_k'(r2)
  * est_k(r1) >= est_k(r2)
  * lst_k(r1) <= lst_k(r2)
  * forbidden region of r2 wrt. r1 must not constrain ori_k(r2)

  Now, if r1 was not pruned and r1 dominates r2,
  then r2 will not be pruned either, since the position found for r1
  is also valid for r2.  This is true since:

  * r2 is included within r1,
  * r1 and r2 have the same est_k or lst_k, dep. on direction
  * the value found in dom_k'(r1) = a value located in dom_k'(r2)
*/
#define DOMINATION 1

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

/* Optimization 6: incrementality, applicable up to backtracking only.  

   Level 1:
   a) Upon resumption, compute bounding box of all targets that were
      pruned (min(orig), max(orig), min(size)) after suspension.
   b) In the INITIAL_CHECK and sweep, ignore sources that don't
      intersect the bounding box. 

   Notes:
   1. In the 2D case, any target with holes in its domains, where
      some witness is now invalid, is also considered pruned.
   2. If wrap-around, assume infinite bb in that dimension.
   3. During the sweep, maintain a bounding box that includes all
      pruned targets.

   Level 2: if at most one target was pruned after suspension,
            and no margins, wrap-around or global reasoning.
   a) If no targets pruned, suspend.
   b) The initial iteration needs only condider the pruned target t0
      and any target t where the forbidden region of t wrt. r0
      affects t.
*/
#define INCREMENTAL 2

/* Global 2D adapted from edge-finding a la cumulative.

   Lower bounds:

   foreach target o {
       est_1 = EST_1(o);
       lct_1 = LCT_1(o);
       Omega = {w | EST_0(w) < LCT_0(o) & w!=o & minoverlap_1(w,o)>0}
	       by increasing EST_0;
       arr = {w | w in Omega with unique LCT_0} by increasing LCT_0;
       for j in |arr|-1..0 {
	   est_0 = EST_0(Omega);
	   lct_0 = LCT_0(arr[j]);
	   area = Sum_{w in Omega} minoverlap(w,est_0,lct_0,est_1,lct_1);
	   Omega_2 = Omega;
	   Omega_3 = Omega;
	   while Omega_2 != NULL {
	       infer min_0(o) from area;
	       Omega_2 = {w in Omega_2 | EST_0(w) > est_0};
	       Omega_3 = {w in Omega_3 | ECT_0(w) > est_0};
	       est_2 = EST_0(Omega_2);
	       for w in Omega_3 {
		   d1 = minoverlap(w,est_0,lct_0,est_1,lct_1);
		   d2 = minoverlap(w,est_2,lct_0,est_1,lct_1);
		   area -= d1-d2;
	       }
	       est_0 = est_2;
	   }
       }
   }

   Upper bounds:

   foreach target o {
       est_1 = EST_1(o);
       lct_1 = LCT_1(o);
       Omega = {w | LCT_0(w) > EST_0(o) & w!=o & minoverlap_1(w,o)>0}
	       by decreasing LCT_0;
       arr = {w | w in Omega with unique EST_0} by increasing EST_0;
       for j in 0..|arr|-1 {
	   est_0 = EST_0(arr[j]);
	   lct_0 = LCT_0(Omega);
	   area = Sum_{w in Omega} minoverlap(w,est_0,lct_0,est_1,lct_1);
	   Omega_2 = Omega;
	   Omega_3 = Omega;
	   while Omega_2 != NULL {
	       infer max_0(o) from area;
	       Omega_2 = {w in Omega_2 | LCT_0(w) < lct_0};
	       Omega_3 = {w in Omega_3 | LST_0(w) < lct_0};
	       lct_2 = LCT_0(Omega_2);
	       for w in Omega_3 {
		   d1 = minoverlap(w,est_0,lct_0,est_1,lct_1);
		   d2 = minoverlap(w,est_0,lct_2,est_1,lct_1);
		   area -= d1-d2;
	       }
	       lct_0 = lct_2;
	   }
       }
   }
*/
#define NEW_GLOBAL 1

/************************************************************/
/* Generic support                                          */
/************************************************************/

#define EV_START 1
#define EV_END 2

#define STATUS_SOURCE 1
#define STATUS_TARGET 2
#define STATUS_MARKED 4

/* for qsorting by ascending long */
static int cmp_asc_long(long *l1, long *l2)
{
  return *l1 - *l2;
}


#define QType long
#define QCmp  cmp_asc_long
#define QSort qsort_asc_long
#include "qsort.ic"

/* compute area of <est,lct,dur> that must overlap [lb,ub) */
static long min_overlap_interval(long est, long lct, long dur,
				 long lb, long ub)
{
  long ltmp1=(est+dur)-lb, ltmp2=ub-(lct-dur);
  long r = (ltmp1<ltmp2 ? ltmp1 : ltmp2);

  if (r<=0) return 0;
  if (r>dur) r = dur;
  if (r>ub-lb) r = ub-lb;
  return r;
}


/* compute (area of <est,lct,dur> that must overlap [lb0,ub)) -
           (area of <est,lct,dur> that must overlap [lb,ub)) */
static long min_overlap_delta_est(long est, long lct, long dur,
				  long lb0, long lb, long ub)
{
  long ltmp1=(est+dur)-lb0, ltmp2=ub-(lct-dur);
  long r1 = (ltmp1<ltmp2 ? ltmp1 : ltmp2);
  long r2 = (est+dur)-lb;

  if (r1<=0) return 0;
  if (r1>dur) r1 = dur;
  if (r1>ub-lb0) r1 = ub-lb0;
  if (r2>=ltmp2) r2 = ltmp2;
  if (r2<=0) return r1;
  if (r2>dur) r2 = dur;
  if (r2>ub-lb) r2 = ub-lb;

  return r1-r2;
}


/* compute (area of <est,lct,dur> that must overlap [lb,ub0)) -
           (area of <est,lct,dur> that must overlap [lb,ub)) */
static long min_overlap_delta_lct(long est, long lct, long dur,
				  long lb, long ub0, long ub)
{
  long ltmp1=(est+dur)-lb, ltmp2=ub0-(lct-dur);
  long r1 = (ltmp1<ltmp2 ? ltmp1 : ltmp2);
  long r2 = ub-(lct-dur);

  if (r1<=0) return 0;
  if (r1>dur) r1 = dur;
  if (r1>ub0-lb) r1 = ub0-lb;
  if (r2>=ltmp1) r2 = ltmp1;
  if (r2<=0) return r1;
  if (r2>dur) r2 = dur;
  if (r2>ub-lb) r2 = ub-lb;

  return r1-r2;
}


/* compute area of <est,lct,dur> that may overlap [lb,ub) */
static long max_overlap_interval(long est, long lct, long dur,
				 long lb, long ub)
{
  long ltmp1=lct-lb, ltmp2=ub-est;
  long r = (ltmp1<ltmp2 ? ltmp1 : ltmp2);
  
  if (r<=0) return 0;
  if (r>dur) r = dur;
  if (r>ub-lb) r = ub-lb;
  return r;
}


/************************************************************/
/* Generic support                                          */
/************************************************************/

/* terminates lists linked by index */
#define TERMIN (-1)

#define TARGET(i) (pdata->target[i])
#define STATUS(it) (pdata->item.status[it])
#define TYPE(it) (pdata->item.type[it])
#define NEXT(it) (pdata->item.next[it])
#define NEXT_AREA(it) (pdata->item.next_area[it])

#define MarkedItem(ev) \
  ((STATUS(pdata->event.item[ev])&(STATUS_SOURCE+STATUS_MARKED))==STATUS_SOURCE+STATUS_MARKED)
#define MarkItem(ev) \
  {STATUS(pdata->event.item[ev]) |= STATUS_MARKED; --unmarked;}

typedef long SHIFTED_ITEM;

typedef long ITEM;

typedef long EVENT;

typedef long MARGIN;

/************************************************************/
/* Support for 1D disjoint                                  */
/************************************************************/

/* The constraint frame. */
struct disjoint1_data {
  void (SPCDECL *destructor)(void*);
#if MULTI_SP_AWARE
  SPEnv *spenv;
#endif /* MULTI_SP_AWARE */

  SP_term_ref refbase;		/* static */
  long stamp;
  int iteration;		/* initially #pruned, then incremented */
  int nitems;			/* static */
  int nshifted_items;		/* nitems * (1 or 3), static */
  int ntargets;			/* #items that may be targets, := nitems */
  int nsources;			/* #items that may be sources only, := 0 */
#if CSTATE_ONLY
  int ndone;			/* #items that are forgotten _forever_, := 0 */
#endif
  int nmargs;			/* exact number */
  int nmargs_aligned;		/* power of 2, static */
  int flags;			/* static */
  long lborder;			/* static */
  long rborder;			/* static */
  long maxmargin;		/* static */
  long maxdur;			/* static */
#if INCREMENTAL
  long bbmin;			/* bounding box min */
  long bbmax;			/* bounding box min */
#endif
  ITEM items_by_area;		/* terminated by TERMIN */
  ITEM *sortarr;		/* [3*nitems], volatile */
  ITEM *target;			/* [nitems], qsorted */
  SHIFTED_ITEM *source;		/* [nshifted_items], perhaps qsorted, volatile */
  EVENT *start_event;		/* [nshifted_items], qsorted, volatile */
  EVENT *end_event;		/* [nshifted_items], qsorted, volatile */
  MARGIN *margtab;		/* [nmargs_aligned] */
  struct {
    TAGGED *min;
    TAGGED *max;
    TAGGED *fdset;		/* volatile, GC-safe */
    long *mindur;
    long *maxdur;
    TAGGED *type;		/* static */
    long *pruned_at;		/* volatile */
    long *status;
    ITEM *next;			/* terminated by TERMIN, volatile */
    ITEM *next_area;		/* terminated by TERMIN, volatile */
  } item;			/* each [nitem] */
  struct {
    ITEM *item;			/* volatile */
    long *amount;		/* volatile */
  } shifted_item;		/* each [nshifted_items] */
  struct {
    ITEM *item;			/* volatile */
    long *start;		/* inclusive start of forbidden region, volatile */
    long *end;			/* inclusive end of forbidden region, volatile */
    long *min_start;		/* mimimal start among this and later end events, volatile */
    long *max_end;		/* maximal end among this and earlier start events, volatile */
  } event;			/* each [nshifted_item] */
  struct {
    TAGGED *type1;
    TAGGED *type2;
    long *amount;
    MARGIN *next;		/* terminated by TERMIN */
  } margin;			/* each [nmargs] */
  /* space for the above arrays */
};

static void SPCDECL disjoint1_destructor(void *pdata_v)
{
  struct disjoint1_data *pdata = (struct disjoint1_data *)pdata_v;
  FD_SETUP_SPENV(pdata->spenv)

  SP_free_term_refs(pdata->refbase,4*pdata->nitems);
  SP_free(pdata);
}


#define PRUNED_AT(t) (pdata->item.pruned_at[t])
#define FDSET(t) (pdata->item.fdset[t])
#define SMIN(t) (pdata->item.min[t])
#define SMAX(t) (pdata->item.max[t])
#define DUR(T) (pdata->item.mindur[T])
#define DURmax(T) (pdata->item.maxdur[T])
#define EST(T) GetSmall(pdata->item.min[T])
#define LaST(T) GetSmall(pdata->item.max[T])
#define ECT(T) (EST(T)+pdata->item.mindur[(T)])
#define LCT(T) (LaST(T)+pdata->item.mindur[T])
#define LCTmax(T) (LaST(T)+pdata->item.maxdur[T])
#define SLACK(T) (LaST(T)-EST(T)-pdata->item.mindur[T])

/* zero-length target assumed */
#define BASIC_RELATIVE_FORBIDDEN_REGION1D(SOURCE, X1, X2) \
(X1) = LaST(SOURCE) + 1; \
(X2) = EST(SOURCE)  + pdata->item.mindur[SOURCE] - 1;

/* zero-length target assumed */
#define RELATIVE_FORBIDDEN_REGION1D(SOURCE, X1, X2) \
(X1) = LaST(pdata->shifted_item.item[(SOURCE)])+pdata->shifted_item.amount[(SOURCE)] + 1; \
(X2) = EST(pdata->shifted_item.item[(SOURCE)])+pdata->shifted_item.amount[(SOURCE)]  + pdata->item.mindur[(pdata->shifted_item.item[(SOURCE)])] - 1;

#define BASIC_FORBIDDEN_REGION1D(TARGET, SOURCE, X1, X2) \
(X1) = LaST(SOURCE) - pdata->item.mindur[(TARGET)] + 1; \
(X2) = EST(SOURCE)  + pdata->item.mindur[(SOURCE)] - 1;

#define FORBIDDEN_REGION1D(TARGET, SOURCE, X1, X2) \
(X1) = LaST(pdata->shifted_item.item[(SOURCE)])+pdata->shifted_item.amount[(SOURCE)] - pdata->item.mindur[(TARGET)] + 1; \
(X2) = EST(pdata->shifted_item.item[(SOURCE)])+pdata->shifted_item.amount[(SOURCE)]  + pdata->item.mindur[(pdata->shifted_item.item[(SOURCE)])] - 1;

/* for qsorting TARGETS by descending DUR */
static int cmp_desc_dur1d MAGIC (HIDDEN_PROTO
				 ITEM *t1, ITEM *t2)
{
  struct disjoint1_data *pdata = fd.gdata;
  return DUR(*t2) - DUR(*t1);
}


#define QType ITEM
#define QCmp  cmp_desc_dur1d
#define QSort qsort_desc_dur1d
#include "qsort.ic"

/* for qsorting by descending EST */
static int cmp_desc_est1d MAGIC (HIDDEN_PROTO
				 ITEM *t1, ITEM *t2)
{
  struct disjoint1_data *pdata = fd.gdata;
  return EST(*t2) - EST(*t1);
}


#define QType ITEM
#define QCmp  cmp_desc_est1d
#define QSort qsort_desc_est1d
#include "qsort.ic"

/* for qsorting by descending LaST */
static int cmp_desc_lst1d MAGIC (HIDDEN_PROTO
				 ITEM *t1, ITEM *t2)
{
  struct disjoint1_data *pdata = fd.gdata;
  return LaST(*t2) - LaST(*t1);
}


#define QType ITEM
#define QCmp  cmp_desc_lst1d
#define QSort qsort_desc_lst1d
#include "qsort.ic"

/* for qsorting by ascending ECT */
static int cmp_asc_ect1d MAGIC (HIDDEN_PROTO
				ITEM *t1, ITEM *t2)
{
  struct disjoint1_data *pdata = fd.gdata;
  return ECT(*t1) - ECT(*t2);
}


#define QType ITEM
#define QCmp  cmp_asc_ect1d
#define QSort qsort_asc_ect1d
#include "qsort.ic"

/* for qsorting by ascending LCT */
static int cmp_asc_lct1d MAGIC (HIDDEN_PROTO
				ITEM *t1, ITEM *t2)
{
  struct disjoint1_data *pdata = fd.gdata;
  return LCT(*t1) - LCT(*t2);
}


#define QType ITEM
#define QCmp  cmp_asc_lct1d
#define QSort qsort_asc_lct1d
#include "qsort.ic"

static int cmp_asc_start MAGIC (HIDDEN_PROTO
				EVENT *t1, EVENT *t2)
{
  struct disjoint1_data *pdata = fd.gdata;
  return pdata->event.start[(*t1)] - pdata->event.start[(*t2)];
}


#define QType EVENT
#define QCmp  cmp_asc_start
#define QSort qsort_asc_start
#include "qsort.ic"

static int cmp_asc_end MAGIC (HIDDEN_PROTO
			      EVENT *t1, EVENT *t2)
{
  struct disjoint1_data *pdata = fd.gdata;
  return pdata->event.end[(*t1)] - pdata->event.end[(*t2)];
}


#define QType EVENT
#define QCmp  cmp_asc_end
#define QSort qsort_asc_end
#include "qsort.ic"

/* subtract from *p1 margin from target to source;
   add to *p2 margin from source to target.
*/
static int extend_forbidden_region1d MAGIC (HIDDEN_PROTO
					    ITEM target, ITEM source,
					    long *p1, long *p2)
{
  struct disjoint1_data *pdata = fd.gdata;
  TAGGED t1 = TYPE(target);
  TAGGED t2 = TYPE(source);
  TAGGED key = ((t1^t2)>>LogSizeOfWord) & (pdata->nmargs-1);
  MARGIN m = key;
  int hits = 0;

  while (m > TERMIN && hits<2) {
    if ((pdata->margin.type1[m]==t1) && (pdata->margin.type2[m]==t2)) { /* target -> source margin found */
      hits++;
      *p1 -= pdata->margin.amount[m];
    }
    if ((pdata->margin.type1[m]==t2) && (pdata->margin.type2[m]==t1)) { /* source -> target margin found */
      hits++;
      *p2 += pdata->margin.amount[m];
    }
    m = pdata->margin.next[m];
  }
  return hits;
}



static BOOL prune_item1d MAGIC (HIDDEN_PROTO
				ITEM sv,
				long lobound, long hibound)
{
  struct disjoint1_data *pdata = fd.gdata;
  TAGGED oldmin = SMIN(sv);
  TAGGED oldmax = SMAX(sv);
  TAGGED newmin = MakeSmall(lobound);
  TAGGED newmax = MakeSmall(hibound);
  BOOL rc = TRUE;
  
  if (Tgt(newmin,oldmin) || Tlt(newmax,oldmax)) {
    PRUNED_AT(sv) = pdata->iteration;
    rc = adjust_bounds(newmin, newmax, FDSET(sv), &SMIN(sv), &SMAX(sv));
#if INCREMENTAL
    if (pdata->bbmin > EST(sv))
      pdata->bbmin = EST(sv);
    if (pdata->bbmax < LCT(sv))
      pdata->bbmax = LCT(sv);
#endif
  }
  return rc;
}


static BOOL prune_before1d MAGIC (HIDDEN_PROTO
				  ITEM t1, ITEM t2)
{
  struct disjoint1_data *pdata = fd.gdata;
  long dur1 = DUR(t1);
  long est1 = EST(t1);
  long lst2 = LaST(t2);
  
  if (LCT(t1)<=EST(t2))
    /* no pruning possible */
    return TRUE;
  /* adjust bounds if possible */
  return (prune_item1d(t1, est1, lst2-dur1) &&
	  prune_item1d(t2, est1+dur1, lst2));
}



/* compute total area of all tasks\{l} overlapping [est,lct) */
static long total_area1d MAGIC (HIDDEN_PROTO
				ITEM *arr, int n,
				ITEM l,
				long est, long lct)
{
  struct disjoint1_data *pdata = fd.gdata;
  long total = 0;
  int i;

  for (i=0; i<n; i++)
    {
      ITEM s = arr[i];
  
      if (s!=l)
	total += min_overlap_interval(EST(s),LCT(s),DUR(s),est,lct);
    }
  return total;
}


/* -1 if j before i, 1 if i before j, 0 otherwise */
#define TSIGN(Ti,Tj) (LaST(Tj)<ECT(Ti) ? -1 : LaST(Ti)<ECT(Tj))

/* Find lb of start time of l s.t. slack>=0 is possible, where
   slack = (avail. space from start(l) to lct0)
         - (area demand of items starting >= start(l))
	 */
static BOOL not_first MAGIC (HIDDEN_PROTO
			     ITEM s0, ITEM l)
{
  struct disjoint1_data *pdata = fd.gdata;
  long lb0, lb, newlb;
  ITEM *arr = pdata->sortarr + 2*pdata->nitems;
  ITEM s;
  long estlt, durlt, dureq, durgt, lcts;
  int i, j, n;
  BOOL sorted = FALSE;
  
  /* Initialize variables.  Build array of unordered s items. */
  lb = EST(l);
  durlt = dureq = durgt = 0;
  estlt = CLPFD_MAXINT;
  lcts = ECT(l);
  for (s=s0, i=0; s>TERMIN; s=NEXT(s))
    switch (TSIGN(l,s)) {
    case -1:			/* s before l */
      durlt += DUR(s);
      if (estlt>EST(s))
	estlt = EST(s);
      if (lb<ECT(s))
	lb = ECT(s);
      newlb = estlt+durlt;
      if (lb<newlb)
	lb = newlb;
      break;
    case  1:			/* s after l */
      durgt += DUR(s);
      if (lcts<LCT(s)) lcts = LCT(s);
      break;
    case  0:			/* either way */
      dureq += DUR(s);
      if (lcts<LCT(s)) lcts = LCT(s);
      arr[i++] = s;
      break;
    }
  n = i;
  if (EST(l)<lb && !prune_item1d(l, lb, LaST(l)))
    return FALSE;

  /* Keep adjusting lb until fixpoint. */

  lb0 = EST(l)-1;
  while (lb0<EST(l) && n>0) {
    long avail = (lcts-ECT(l))-durgt;
      
    lb0 = lb = EST(l);
    /* identfy new ordered s items */
    for (i=0, j=0; i<n; i++) {
      ITEM t = arr[i];
      int sign = TSIGN(l,t);
	
      if (DUR(t)>avail || sign == -1) {
	if (sign>=0 && !prune_before1d(t,l))
	  return FALSE;
	dureq -= DUR(t);
	durlt += DUR(t);
	if (estlt>EST(t))
	  estlt = EST(t);
	if (lb<ECT(t))
	  lb = ECT(t);
	newlb = estlt+durlt;
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
	qsort_asc_ect1d(arr, n);
      }
      for (i=0; i<n && dureq>avail; i++) {
	ITEM t = arr[i];
      
	newlb = (lb>ECT(t) ? lb : ECT(t));
	avail -= (newlb-lb) - DUR(t);
	lb = newlb;
      }
    }
    if (EST(l)<lb && !prune_item1d(l, lb, LaST(l)))
      return FALSE;
  }
	
  return TRUE;
}



/* Find ub of finish time of l s.t. slack>=0 is possible, where
   slack = (avail. space from est0 to finish(l))
         - (area demand of items finishing <= finish(l))
	 */
static BOOL not_last MAGIC (HIDDEN_PROTO
			    ITEM s0, ITEM l)
{
  struct disjoint1_data *pdata = fd.gdata;
  long ub0, ub, newub;
  ITEM *arr = pdata->sortarr + 2*pdata->nitems;
  ITEM s;
  long lctgt, durgt, dureq, durlt, ests;
  int i, j, n;
  BOOL sorted = FALSE;
  
  /* Initialize variables.  Build array of unordered s items. */
  ub = LCT(l);
  durgt = dureq = durlt = 0;
  lctgt = -CLPFD_MAXINT;
  ests = LaST(l);
  for (s=s0, i=0; s>TERMIN; s=NEXT(s))
    switch (TSIGN(l,s)) {
    case  1:			/* s after l */
      durgt += DUR(s);
      if (lctgt<LCT(s))
	lctgt = LCT(s);
      if (ub>LaST(s))
	ub = LaST(s);
      newub = lctgt-durgt;
      if (ub>newub)
	ub = newub;
      break;
    case -1:			/* s before l */
      durlt += DUR(s);
      if (ests>EST(s)) ests = EST(s);
      break;
    case  0:			/* either way */
      dureq += DUR(s);
      if (ests>EST(s)) ests = EST(s);
      arr[i++] = s;
      break;
    }
  n = i;
  if (LCT(l)>ub && !prune_item1d(l, EST(l), ub-DUR(l)))
    return FALSE;

  /* Keep adjusting lb until fixpoint. */

  ub0 = LCT(l)+1;
  while (ub0>LCT(l) && n>0) {
    long avail = (LaST(l)-ests)-durlt;
      
    ub0 = ub = LCT(l);
    /* identfy new ordered s items */
    for (i=0, j=0; i<n; i++) {
      ITEM t = arr[i];
      int sign = TSIGN(l,t);
	
      if (DUR(t)>avail || sign == 1) {
	if (sign<=0 && !prune_before1d(l,t))
	  return FALSE;
	dureq -= DUR(t);
	durgt += DUR(t);
	if (lctgt<LCT(t))
	  lctgt = LCT(t);
	if (ub>LaST(t))
	  ub = LaST(t);
	newub = lctgt-durgt;
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
	qsort_desc_lst1d(arr, n);
      }
      for (i=0; i<n && dureq>avail; i++) {
	ITEM t = arr[i];
      
	newub = (ub<LaST(t) ? ub : LaST(t));
	avail -= (ub-newub) - DUR(t);
	ub = newub;
      }
    }
    if (LCT(l)>ub && !prune_item1d(l, EST(l), ub-DUR(l)))
      return FALSE;
  }
	
  return TRUE;
}


/* Lean edge finding a la Martin/Shmoys/Wuertz,
   generalized to the cumulative case.
   Comparing items T with Si such that lct(T) > lct(Si) = seed,
   where seed is a unique LCT of the items.
   */
static BOOL edge_finding1d_up MAGIC (HIDDEN_PROTO
				     BOOL *change)
{
  struct disjoint1_data *pdata = fd.gdata;
  BOOL sorted = FALSE;
  ITEM *arr = pdata->sortarr;
  int n = pdata->ntargets + pdata->nsources;
  long *seed = (long *)(arr + pdata->nitems);
  int i, j;

  for (i=0; i<n; i++)
    seed[i] = LCT(arr[i]);
  qsort_asc_long(seed, n);

  /* the greatest seed is no good */
  for (j=n-2; j>=0 && seed[j]==seed[j+1]; j--)
    ;
  while (j>=0)
    {
      long lcts = seed[j];
      long area = 0;
      ITEM s = TERMIN; /* the chain of subsets */
      ITEM s1;
      ITEM l;	/* the items, by descending area */

      if (!sorted) {
	sorted = TRUE;
	qsort_desc_est1d(arr, n);
      }
      for (i=0; i<n; i++)
	{
	  ITEM t1 = arr[i];
	  
	  if (LCT(t1) <= lcts)
	    {
	      area += DUR(t1);
	      NEXT(t1) = s, s = t1;
	      /* HERE: can enforce upper bounds on DUR(t1), t1->sup */
	    }
	}
      l = pdata->items_by_area;
      while (s>TERMIN && l>TERMIN && DUR(l)>0)
       if (EST(l)>=lcts || LCT(l)<=lcts
	   || !(STATUS(l)&STATUS_TARGET)
	   )
	 l = NEXT_AREA(l);
       else
	{
	  long ests = EST(s);
	  long totarea = total_area1d(arr,n,l,ests,lcts);
	  long slack1 = (lcts-ests) - totarea;
          long slack2 = (lcts-EST(l)) - area;
	  long larea = DUR(l);

	  if (slack1>=larea)
	    {
	      if (slack2>=larea) /* may be inside and first */
		l = NEXT_AREA(l);
	      else if (!not_first(s,l/*,slack2-larea*/))
		return FALSE;
	      else if (sorted && PRUNED_AT(l)==pdata->iteration) {
		*change = TRUE;
		sorted = FALSE;
	      }
	      do {
		area -= DUR(s);
		s = NEXT(s);
	      } while (s>TERMIN && EST(s)==ests);
	    }
	  else
	    {
	      if (slack1 < min_overlap_interval(EST(l),LCT(l),larea,ests,lcts))
		return FALSE;
	      else if (slack1 < max_overlap_interval(EST(l),LCT(l),larea,ests,lcts)) /* must stick out at either end */
		{
		  long ub = ests+slack1-larea;
		  long lb = lcts-slack1;

		  if (EST(l) > ub) /* must stick out to the right */
		    {
		      if (!prune_item1d(l, lb, LaST(l)))
			return FALSE;
		      *change = TRUE;
		      sorted = FALSE;
		      for (s1=s; s1>TERMIN; s1=NEXT(s1)) /* added 001115 */
			if (!prune_before1d(s1,l))
			  return FALSE;
		    }
		}
	      l = NEXT_AREA(l);
	    }
	}
      while (j>=0 && seed[j]==lcts)
	j--;
    }
  return TRUE;
}


/* Lean edge finding a la Martin/Shmoys/Wuertz,
   generalized to the cumulative case.
   Comparing items T with Si such that est(T) < est(Si) = seed,
   where seed is a unique EST of the items.
   */
static BOOL edge_finding1d_down MAGIC (HIDDEN_PROTO
				       BOOL *change)
{
  struct disjoint1_data *pdata = fd.gdata;
  BOOL sorted = FALSE;
  ITEM *arr = pdata->sortarr;
  int n = pdata->ntargets + pdata->nsources;
  long *seed = (long *)(arr + pdata->nitems);
  int i, j;

  for (i=0; i<n; i++)
    seed[i] = EST(arr[i]);
  qsort_asc_long(seed, n);

  /* the smallest seed is no good */
  for (j=1; j<n && seed[j]==seed[j-1]; j++)
    ;
  while (j<n)
    {
      long ests = seed[j];
      long area = 0;
      ITEM s = TERMIN; /* the chain of subsets */
      ITEM s1;
      ITEM l;	/* the items, by descending area */
      
      if (!sorted) {
	sorted = TRUE;
        qsort_asc_lct1d(arr, n);
      }
      for (i=0; i<n; i++)
	{
	  ITEM t1 = arr[i];
	  
	  if (EST(t1) >= ests)
	    {
	      area += DUR(t1);
	      NEXT(t1) = s, s = t1;
	      /* HERE: can enforce upper bounds on DUR(t1), t1->sup */
	    }
	}
      l = pdata->items_by_area;
      while (s>TERMIN && l>TERMIN && DUR(l)>0)
       if (EST(l)>=ests || LCT(l)<=ests
	   || !(STATUS(l)&STATUS_TARGET)
	   )
	 l = NEXT_AREA(l);
       else
	{
	  long lcts = LCT(s);
	  long totarea = total_area1d(arr,n,l,ests,lcts);
	  long slack1 = (lcts-ests) - totarea;
	  long slack2 = (LCT(l)-ests) - area;
	  long larea = DUR(l);

	  if (slack1>=larea)
	    {
	      if (slack2>=larea) /* may be inside and last */
		l = NEXT_AREA(l);
	      else if (!not_last(s,l/*,slack2-larea*/))
		return FALSE;
	      else if (sorted && PRUNED_AT(l)==pdata->iteration) {
		*change = TRUE;
		sorted = FALSE;
	      }
	      do {
		area -= DUR(s);
		s = NEXT(s);
	      } while (s>TERMIN && LCT(s)==lcts);
	    }
	  else
	    {
	      if (slack1 < min_overlap_interval(EST(l),LCT(l),larea,ests,lcts))
		return FALSE;
	      else if (slack1 < max_overlap_interval(EST(l),LCT(l),larea,ests,lcts)) /* must stick out at either end */
		{
		  long ub = ests+slack1-larea;
		  long lb = lcts-slack1;

		  if (LaST(l) < lb) /* must stick out to the left */
		    {
		      if (!prune_item1d(l, EST(l), ub))
			return FALSE;
		      *change = TRUE;
		      sorted = FALSE;
		      for (s1=s; s1>TERMIN; s1=NEXT(s1)) /* added 001115 */
			if (!prune_before1d(l,s1))
			  return FALSE;
		    }
		}
	      l = NEXT_AREA(l);
	    }
	}
      while (j<n && seed[j]==ests)
	j++;
    }
  return TRUE;
}


static INLINE int next_start1d MAGIC (HIDDEN_PROTO
				      int js,int nsources,
				      ITEM titem,
				      long est0,long lst,
				      long *next)
{
  struct disjoint1_data *pdata = fd.gdata;
  EVENT ev = TERMIN;

  while (js<nsources &&
	 pdata->event.start[(ev = pdata->start_event[js])]-DUR(titem) <= lst &&
	 (pdata->event.item[ev] == titem ||
	  pdata->event.end[ev] < est0 || /* necessary test! completes filtering done at [*] */
	  pdata->event.start[ev]-DUR(titem) > pdata->event.end[ev]))
    js++;
  *next = js<nsources ? pdata->event.start[ev]-DUR(titem) : CLPFD_MAXINT-1L;
  return js;
}


static INLINE int next_end1d MAGIC (HIDDEN_PROTO
				    int je, int nsources,
				    ITEM titem,
				    long *next)
{
  struct disjoint1_data *pdata = fd.gdata;
  EVENT ev = TERMIN;

  while (je<nsources &&
	 (pdata->event.item[(ev = pdata->end_event[je])] == titem ||
	  pdata->event.start[ev]-DUR(titem) > pdata->event.end[ev]))
    je++;
  *next = je<nsources ? pdata->event.end[ev] : CLPFD_MAXINT-1L;
  return je;
}


static INLINE int prev_start1d MAGIC (HIDDEN_PROTO
				      int js,
				      ITEM titem,
				      long *next)
{
  struct disjoint1_data *pdata = fd.gdata;
  EVENT ev = TERMIN;

  while (js>0 &&
	 (pdata->event.item[(ev = pdata->start_event[js-1])] == titem ||
	  pdata->event.start[ev]-DUR(titem) > pdata->event.end[ev]))
    js--;
  *next = js>0 ? pdata->event.start[ev]-DUR(titem) : 1L-CLPFD_MAXINT;
  return js;
}


static INLINE int prev_end1d MAGIC (HIDDEN_PROTO
				    int je,
				    ITEM titem,
				    long lct0,long est,
				    long *next)
{
  struct disjoint1_data *pdata = fd.gdata;
  EVENT ev = TERMIN;

  while (je>0 &&
	 pdata->event.end[(ev = pdata->end_event[je-1])] >= est &&
	 (pdata->event.item[ev] == titem ||
	  pdata->event.start[ev] > lct0 || /* necessary test! completes filtering done at [**] */
	  pdata->event.start[ev]-DUR(titem) > pdata->event.end[ev]))
    je--;
  *next = je>0 ? pdata->event.end[ev] : 1L-CLPFD_MAXINT;
  return je;
}



#if DOMINATION
/* True if r2 is "larger" and "more constrained" than r1,
   in which case r2 will be cached as the most dominating so far
   for the given sweep direction (right, left).
   Minimize by lexicographic order:
   * ( est_k(r), if right or up
     (-lst_k(r), if left or down
   * -dim_k(r)
   * -(lst_k(r)-est_k(r))
*/
static BOOL less_constrained1d MAGIC (HIDDEN_PROTO
				      ITEM r1, ITEM r2, BOOL rightp)
{
  struct disjoint1_data *pdata = fd.gdata;
  long ori1, ori2, diff;
  
  if (r1==TERMIN)
    return TRUE;
  
  ori1 = (!rightp ? EST(r1) : -LaST(r1));
  ori2 = (!rightp ? EST(r2) : -LaST(r2));

  diff = ori1 - ori2;		/* minimize */
  if (diff > 0) return TRUE;
  if (diff < 0) return FALSE;

  diff = DUR(r1) - DUR(r2);	/* maximize */
  if (diff < 0) return TRUE;
  if (diff > 0) return FALSE;

  diff = SLACK(r1) - SLACK(r2); /* maximize */
  if (diff < 0) return TRUE;

  return FALSE;
}


/* True if r1 dominates r2, in which case r2's origin in the given
   sweep direction will not be pruned.
*/
static BOOL dominates1d MAGIC (HIDDEN_PROTO
			       ITEM r1, ITEM r2, BOOL rightp)
{
  struct disjoint1_data *pdata = fd.gdata;
  long start, end, ori;
  
  if (r1==TERMIN)
    return FALSE;
  if (!rightp) {
    ori = EST(r2);
    if (SMIN(r1) != SMIN(r2))
      return FALSE;
  } else {
    ori = LaST(r2);
    if (SMAX(r1) != SMAX(r2))
      return FALSE;
  }

  if (DUR(r1) < DUR(r2))
    return FALSE;

  /* R1 must not forbid the X min (or max) of R2 */
  BASIC_FORBIDDEN_REGION1D(r2, r1, start, end);

  if (start > end ||		/* empty region */
      ori > end ||
      ori < start)		/* no overlap */
    return TRUE;

  return FALSE;
}
#endif


/* New filtering algorithm.
   Form L1: list of relative starts of forbidden areas.
   Form L2: list of ends of forbidden areas.
   For each target:
     scan L1 and L2 for forbidden regions.
*/
static int disjoint1_filter MAGIC (HIDDEN_PROTO
				   int nsources,
				   ITEM *target0, int ntargets,
				   BOOL *change)
{
  struct disjoint1_data *pdata = fd.gdata;
  int flags = pdata->flags;
  int i, js, je, mid, sup;
  long dur_threshold = CLPFD_MAXINT; /* too small items can't be pruned */
  long min_start, max_end;
#if DOMINATION
  BOOL moved = FALSE;
  ITEM not_pruned[2];
#endif

  for (i=0; i<nsources; i++) {
    SHIFTED_ITEM src = pdata->source[i];
    EVENT ev = i;
    long start, end;
    
    RELATIVE_FORBIDDEN_REGION1D(src, start, end);
    if (flags & 0x4) {
      int hits =
	extend_forbidden_region1d(target0[0],
				  pdata->shifted_item.item[src],
				  &start, &end);
      if (hits==0 && DUR(target0[0])==0)
	start = end = CLPFD_MAXINT;
    }
    pdata->event.item[ev] = pdata->shifted_item.item[src];
    pdata->start_event[i] = pdata->end_event[i] = ev;
    pdata->event.start[ev] = start;
    pdata->event.end[ev] = end;
    if (dur_threshold > start-end)
      dur_threshold = start-end;
  }

				/* if no margins, ignore any targets with DUR=0 */
				/* if margins, we have filtered out irrelevant regions */
  if (!(flags & 0x4) && dur_threshold<1)
    dur_threshold = 1;
  
  qsort_asc_start(pdata->start_event, nsources);
  qsort_asc_end(pdata->end_event, nsources);
  
#if DOMINATION
  not_pruned[0] = TERMIN;
  not_pruned[1] = TERMIN;
#endif
  max_end = pdata->event.end[(pdata->start_event[0])];
  min_start = pdata->event.start[(pdata->end_event[nsources-1])];
  for (i=0; i<nsources; i++) {
    if (max_end < pdata->event.end[(pdata->start_event[i])])
      max_end = pdata->event.end[(pdata->start_event[i])];
    pdata->event.max_end[(pdata->start_event[i])] = max_end;
    if (min_start > pdata->event.start[(pdata->end_event[nsources-i-1])])
      min_start = pdata->event.start[(pdata->end_event[nsources-i-1])];
    pdata->event.min_start[(pdata->end_event[nsources-i-1])] = min_start;
  }
  
  /* adjust all mins and maxes */
  
  for (i=0; i<ntargets; i++) {
    ITEM titem = target0[i];
    long est, lst, dur, lct;
    long est0, lct0, current, previous, next_start, next_end;
    int status;

    if (DUR(titem) < dur_threshold)	/* the rest are all smaller */
      break;
    if (!(STATUS(titem)&STATUS_TARGET))
      continue;
    est = EST(titem);
    lst = LaST(titem);
    dur = DUR(titem);
    lct = lst+dur;
#if INCREMENTAL
    if (est>=pdata->bbmax || lct<=pdata->bbmin)
      continue;
#endif
#if DECOMPOSITION
    if (est==lst && dur==DURmax(titem))
      STATUS(titem) &= ~STATUS_TARGET;
#endif
    
    /* pruning min(X) */

#if DOMINATION
      if (!(flags & 0x6) &&
	  dominates1d(not_pruned[0], titem, 0))
	goto prune_max;		/* then titem will not be pruned either */
#endif
      
    /* dichotomic search for first start event whose end is not before X */
    /* [*] NOTE: this does not filter out all events that are before X! */
    js = 0;
    sup = nsources;
    while (js<sup) {
      mid = (js+sup)>>1;
      if (pdata->event.max_end[(pdata->start_event[mid])] < est)
	js = mid+1;
      else
	sup = mid;
    }
    js = next_start1d(js,nsources,titem,est,lst,&next_start);
    if (next_start <= est) {
      if (est==lst) return FALSE;
      /* dichotomic search for first end event that is not before X */
      je = 0;
      sup = nsources;
      while (je<sup) {
	mid = (je+sup)>>1;
	if (pdata->event.end[(pdata->end_event[mid])] < est)
	  je = mid+1;
	else
	  sup = mid;
      }
      je = next_end1d(je,nsources,titem,&next_end);
      /* scan the start and end events for the first feasible min(X) */
      status = 0;
      current = est0 = est;
      while (current <= lst) {
	if (next_start <= current) {
	  status++;
	  js = next_start1d(js+1,nsources,titem,est0,lst,&next_start);
	} else if (next_end < current) {
	  status--;
	  je = next_end1d(je+1,nsources,titem,&next_end);
	} else if (status>0) {
	  current = next_end+1;
	} else if (current > est) {
	  if (!prune_item1d(titem, current, LaST(titem)))
	    return FALSE;
	  previous = current;
	  est = current = EST(titem); /* refresh */
	  *change = TRUE;
#if DOMINATION
	  moved = TRUE;
#endif
	  if (previous==current) break; /* current is feasible */
	}
	else break;		/* current is feasible */
      }
      if (current > lst)
	return FALSE;
    }
#if DOMINATION
      if (!(flags & 0x6) && !moved &&
	  less_constrained1d(not_pruned[0], titem, 0))
				/* then titem is "bigger" and "more constrained" */
	not_pruned[0] = titem;
      moved = FALSE;
#endif

    /* prune max(X) */
  prune_max:
    if (est==lst) goto next_dim;
#if DOMINATION
      if (!(flags & 0x6) && dominates1d(not_pruned[1], titem, 1))
	continue;		/* then titem will not be pruned either */
#endif

    /* dichotomic search for first end event whose start is after X */
    /* [**] NOTE: this does not filter out all events that are after X! */
    je = 0;
    sup = nsources;
    while (je<sup) {
      mid = (je+sup)>>1;
      if (pdata->event.min_start[(pdata->end_event[mid])] <= lct)
	je = mid+1;
      else
	sup = mid;
    }
    je = prev_end1d(je,titem,lct,est,&next_end);
    if (next_end >= lst) {
      /* dichotomic search for first start event that is after X */
      js = 0;
      sup = nsources;
      while (js<sup) {
	mid = (js+sup)>>1;
	if (pdata->event.start[(pdata->start_event[mid])] <= lct)
	  js = mid+1;
	else
	  sup = mid;
      }
      js = prev_start1d(js,titem,&next_start);
      /* scan the start and end events for the first feasible max(X) */
      status = 0;
      current = lst;
      lct0 = lct;
      while (current >= est) {
	if (next_end >= current) {
	  status++;
	  je = prev_end1d(je-1,titem,lct0,est,&next_end);
	} else if (next_start > current) {
	  status--;
	  js = prev_start1d(js-1,titem,&next_start);
	} else if (status>0) {
	  current = next_start-1;
	} else if (current < lst) {

	  if (!prune_item1d(titem, EST(titem), current))
	    return FALSE;
	  previous = current;
	  lst = current = LaST(titem); /* refresh */
	  lct = lst + dur;
	  *change = TRUE;
#if DOMINATION
	  moved = TRUE;
#endif
	  if (previous==current) break; /* current is feasible */
	}
	else break;		/* current is feasible */
      }
      if (current < est)
	return FALSE;
    }
  next_dim:
#if DOMINATION
    if (!(flags & 0x6) && !moved &&
	less_constrained1d(not_pruned[1], titem, 1))
				/* then titem is "bigger" and "more constrained" */
      not_pruned[1] = titem;
    moved = FALSE;
#endif
  }
  return TRUE;
}


#if INITIAL_CHECK
static BOOL disjoint1_check MAGIC (HIDDEN_PROTO int nsources)
{
  struct disjoint1_data *pdata = fd.gdata;
  long next_start, next_end;
  int js, je, i;
  int status;
  EVENT *queues = pdata->start_event;
  EVENT *queuee = pdata->end_event;
  
  for (i=0; i<nsources; i++) {
    SHIFTED_ITEM src = pdata->source[i];
    ITEM sitem = pdata->shifted_item.item[src];
    EVENT ev = i;
    
    pdata->event.item[ev] = sitem;
    queues[i] = queuee[i] = ev;
    pdata->event.start[ev] = LaST(sitem);
    pdata->event.end[ev] = ECT(sitem);
  }
  
  qsort_asc_start(queues, nsources);
  qsort_asc_end(queuee, nsources);
  
  status = 0;
  js = je = 0;
  next_start = js<nsources ? pdata->event.start[(queues[js])] : CLPFD_MAXINT;
  next_end   = je<nsources ? pdata->event.end[(queuee[je])] : CLPFD_MAXINT;
  while (js<nsources || je<nsources) {
    if (next_start<next_end) {	/* prefer end events if it's a tie */
      if (status>0)
	return FALSE;
      status++;
      js++;
      next_start = js<nsources ? pdata->event.start[(queues[js])] : CLPFD_MAXINT;
    } else {
      status--;
      je++;
      next_end   = je<nsources ? pdata->event.end[(queuee[je])] : CLPFD_MAXINT;
    }
  }
  
#if DECOMPOSITION
  /* Finally, for all ground targets, remove TARGET property. */
  for (i=0; i<nsources; i++) {
    SHIFTED_ITEM src = pdata->source[i];
    ITEM item = pdata->shifted_item.item[src];
    
    if ((STATUS(item) & STATUS_TARGET) &&
	SMIN(item)==SMAX(item) &&
	DUR(item)==DURmax(item))
      STATUS(item) &= ~STATUS_TARGET;
  }
#endif
  return TRUE;
}
#endif

#if DECOMPOSITION
static void decompose1d MAGIC (HIDDEN_PROTO
			       int nsources, int ntargets, BOOL incremental)
{
  struct disjoint1_data *pdata = fd.gdata;
  int complete = ((pdata->flags & 0x1) && !incremental);
  long next_start, next_end;
  long est = CLPFD_MAXINT;
  long lct = -CLPFD_MAXINT;
  int js, je, i, top=0;
  int status;
  int unmarked = ntargets;
  EVENT ev;
  EVENT *queues = pdata->start_event;
  EVENT *queuee = pdata->end_event;
  EVENT *stack = (EVENT *)pdata->sortarr + pdata->ntargets;
  
  for (i=0; i<nsources; i++) {
    SHIFTED_ITEM src = pdata->source[i];
    ITEM sitem = pdata->shifted_item.item[src];

    ev = i;
    pdata->event.item[ev] = sitem;
    queues[i] = queuee[i] = ev;
    pdata->event.start[ev] = EST(sitem) + pdata->shifted_item.amount[src] - pdata->maxmargin;
    pdata->event.end[ev] = LCTmax(sitem) + pdata->shifted_item.amount[src] + pdata->maxmargin;
    if (STATUS(sitem)&STATUS_TARGET) {
      if (est>pdata->event.start[ev]) est = pdata->event.start[ev];
      if (lct<pdata->event.end[ev]) lct = pdata->event.end[ev];
    }
  }

  /* forget sources that can no longer prune */
  for (i=0; i<ntargets; i++) {
    ITEM item = TARGET(i);
    
    if (!(STATUS(item)&STATUS_TARGET) &&
	(LCTmax(item)<=est || lct<=EST(item))) {
      --unmarked;
      STATUS(item) &= ~STATUS_SOURCE;
    } else if (!complete)
      STATUS(item) |= STATUS_MARKED;
  }

  if (!complete)
    return;
  
  /* forget all items that can't interact */

  qsort_asc_start(queues, nsources);
  qsort_asc_end(queuee, nsources);
  
  status = 0;
  /* Phase 1: find all potential overlaps, except when one box includes
     another one.
  */
  js = je = 0;
  next_start = js<nsources ? pdata->event.start[(queues[js])] : CLPFD_MAXINT;
  next_end   = je<nsources ? pdata->event.end[(queuee[je])] : CLPFD_MAXINT;
  while (unmarked>0 && (js<nsources || je<nsources)) {
    if (next_start<next_end) {	/* prefer end events if it's a tie */
      ev = queues[js];
      if (!MarkedItem(ev) && status) {
	MarkItem(ev);
      }
      status++;
      js++;
      next_start = js<nsources ? pdata->event.start[(queues[js])] : CLPFD_MAXINT;
    } else {
      ev = queuee[je];
      status--;
      if (!MarkedItem(ev) && status) {
	MarkItem(ev);
      }
      je++;
      next_end   = je<nsources ? pdata->event.end[(queuee[je])] : CLPFD_MAXINT;
    }
  }

  /* Phase 2: find the cases when one unmarked box includes
     a marked one (in the X dimension).

     U = the empty set.

     If START(unmarked(u)) is next, add u to U.
     If START(marked(m)) is next, for each u in U:
         mark u and remove it from U.
     If END(unmarked(u)) is next, remove u from U.
     If END(marked(m)) is next, do nothing.
  */
  js = je = 0;
  next_start = js<nsources ? pdata->event.start[(queues[js])] : CLPFD_MAXINT;
  next_end   = je<nsources ? pdata->event.end[(queuee[je])] : CLPFD_MAXINT;
  while (unmarked>0 && (js<nsources || je<nsources)) {
    if (next_start<next_end) {	/* prefer end events if it's a tie */
      ev = queues[js];
      if (!MarkedItem(ev))
        stack[top++] = ev;
      else {
	for (i=0; i<top; i++) {
	  --unmarked;
	  STATUS(pdata->event.item[(stack[i])]) |= STATUS_MARKED;
	}
	top = 0;
      }
      js++;
      next_start = js<nsources ? pdata->event.start[(queues[js])] : CLPFD_MAXINT;
    } else {
      ev = queuee[je];
      if (!MarkedItem(ev)) {
	for (i=top; stack[i-1]!=ev; --i)
	  ;
	stack[i-1] = stack[--top];
      }
      je++;
      next_end   = je<nsources ? pdata->event.end[(queuee[je])] : CLPFD_MAXINT;
    }
  }
  /**** for debugging
  printf("bounding_boxes(");
  for (i=0; i<nsources; i++) {
    ev = event1d+i;
    printf("%sbb(%d,%d,%d,%d,%d)",
	   (i==0 ? "[" : ","),
	   pdata->event.start[ev][0], pdata->event.end[ev][0], pdata->event.start[ev][1], pdata->event.end[ev][1], STATUS(pdata->event.item[ev])&STATUS_MARKED);
  }
  printf("]).\n");
  ****/
}
#endif

static int collect_source1d MAGIC (HIDDEN_PROTO BOOL filtering)
{
  struct disjoint1_data *pdata = fd.gdata;
  long lbound = filtering ? pdata->lborder+pdata->maxdur+pdata->maxmargin : CLPFD_MAXINT;
  long rbound = filtering ? pdata->rborder-pdata->maxmargin : -CLPFD_MAXINT;
  int n = pdata->ntargets + pdata->nsources;
  int flags = pdata->flags;
  int i, j;

  for (i=j=0; i<n; i++) {
    ITEM var = TARGET(i);
    
    if (filtering && SLACK(var)+2 > pdata->maxdur+2*pdata->maxmargin)
      continue;		/* the item is too loose and cannot prune anything */
    if (filtering && !(flags & 0x4) && DUR(var)==0)
      continue;
    pdata->source[j] = j;
    pdata->shifted_item.item[j] = var;
    pdata->shifted_item.amount[j++] = 0;
    if (pdata->flags & 0x2) {
      if (ECT(var)>rbound) {
	pdata->source[j] = j;
	pdata->shifted_item.item[j] = var;
	pdata->shifted_item.amount[j++] = pdata->lborder - pdata->rborder;
      }
      if (LaST(var)<lbound) {
	pdata->source[j] = j;
	pdata->shifted_item.item[j] = var;
	pdata->shifted_item.amount[j++] = pdata->rborder - pdata->lborder;
      }
    }

  }
  return j;
}



/* '$fd_disjoint1'(+State0, -State, -Actions) :-
   State0 is f(N,Opt,Items,NTargets,NSources,Handle,Stamp),
   State  similarly,
   Actions is a list of prunings etc.
   Opt = opt(Flags,LBorder,RBorder,Margins) where
               0x1 denotes decomposition
               0x2 denotes wrap-around over the interval LBorder..RBorder
               0x4 denotes existence of margins
               0x8 denotes global reasoning (task intervals light)
               0x20 denotes lean (avoid heavy work in incremental case)
               Margins = list of margin(Type1,Type2,Diff) = list of extra margins
   Items is a list of N items item(X,XMut,XDur,XDurMut,Type).
*/
void SPCDECL
prolog_fd_disjoint1 MAGIC (HIDDEN_PROTO
			   SP_term_ref State0,
			   SP_term_ref State,
			   SP_term_ref Actions)
{
  WAMENV;
  int i, j, ent = -1;		/* disentailed */
  int nitems;			/* #items */
  int nshifted_items;		/* #shifted items if wrap-around */
  int nmargs = 0;
  int nmargs_aligned = 0;
  int flags;
  int nactive_items;		/* caches pdata->ntargets + pdata->nsources */
  long l, total_size, state_stamp;
  TAGGED tmp, opt, item, items, *arg;
  TAGGED handle;
  BOOL change;
  BOOL committed;
  BOOL incremental;
#if INCREMENTAL > 1
  int pruned1=1, pruned2=2;	/* must be != and != TERMIN */
#endif
  struct disjoint1_data *pdata;
  char *ptr;
  
  w->numstack_end = NULL;

/*    X(0) = RefTerm(State0); */
  (void)State0;
  X(2) = atom_nil;		/* actions list */
  RefTerm(State) = unify_output_state(w,&handle,&state_stamp,&committed);

  if (!IsVar(handle)) {		/* got [Flag | '$free'(Ptr)] */
    pdata = Pdata(struct disjoint1_data,handle);
    fd.gdata = pdata;
    flags = pdata->flags;
    nitems = pdata->nitems;
    nshifted_items = pdata->nshifted_items;
  } else {			/* build persistent state */
				/* compute flags, nitems, nshifted_items */
    DerefArg(tmp,X(0),1);		/* get N */
    nshifted_items = nitems = GetSmall(tmp);
    DerefArg(opt,X(0),2);		/* get Opt */
    DerefArg(tmp,opt,1);
    flags = GetSmall(tmp);
    if (flags & 0x2)
      nshifted_items *= 3;
    if (flags & 0x4) {
      DerefArg(items,opt,4);
      while (TagIsLST(items)) {
	nmargs++;
	DerefCar(item,items);
	DerefCdr(items,items);
      }
      for (nmargs_aligned=1; nmargs_aligned<nmargs; nmargs_aligned <<= 1)
	;
    }

    total_size =
      ((14*nitems + 10*nshifted_items + 4*nmargs + nmargs_aligned)<<LogSizeOfWord);
    pdata = Palloc(struct disjoint1_data, total_size, handle);
    fd.gdata = pdata;
    ptr = (char *)(pdata+1);
    pdata->sortarr = (ITEM *)ptr;
    ptr = (char *)(pdata->sortarr+3*nitems);
    pdata->target = (ITEM *)ptr;
    ptr = (char *)(pdata->target+nitems);
    pdata->source = (ITEM *)ptr;
    ptr = (char *)(pdata->source+nshifted_items);
    pdata->start_event = (ITEM *)ptr;
    ptr = (char *)(pdata->start_event+nshifted_items);
    pdata->end_event = (ITEM *)ptr;
    ptr = (char *)(pdata->end_event+nshifted_items);
    pdata->margtab = (MARGIN *)ptr;
    ptr = (char *)(pdata->margtab+nmargs_aligned);
    pdata->item.min = (TAGGED *)ptr;
    ptr = (char *)(pdata->item.min+nitems);
    pdata->item.max = (TAGGED *)ptr;
    ptr = (char *)(pdata->item.max+nitems);
    pdata->item.fdset = (TAGGED *)ptr;
    ptr = (char *)(pdata->item.fdset+nitems);
    pdata->item.mindur = (long *)ptr;
    ptr = (char *)(pdata->item.mindur+nitems);
    pdata->item.maxdur = (long *)ptr;
    ptr = (char *)(pdata->item.maxdur+nitems);
    pdata->item.type = (TAGGED *)ptr;
    ptr = (char *)(pdata->item.type+nitems);
    pdata->item.pruned_at = (long *)ptr;
    ptr = (char *)(pdata->item.pruned_at+nitems);
    pdata->item.status = (long *)ptr;
    ptr = (char *)(pdata->item.status+nitems);
    pdata->item.next = (ITEM *)ptr;
    ptr = (char *)(pdata->item.next+nitems);
    pdata->item.next_area = (ITEM *)ptr;
    ptr = (char *)(pdata->item.next_area+nitems);
    pdata->shifted_item.item = (ITEM *)ptr;
    ptr = (char *)(pdata->shifted_item.item+nshifted_items);
    pdata->shifted_item.amount = (long *)ptr;
    ptr = (char *)(pdata->shifted_item.amount+nshifted_items);
    pdata->event.item = (ITEM *)ptr;
    ptr = (char *)(pdata->event.item+nshifted_items);
    pdata->event.start = (long *)ptr;
    ptr = (char *)(pdata->event.start+nshifted_items);
    pdata->event.end = (long *)ptr;
    ptr = (char *)(pdata->event.end+nshifted_items);
    pdata->event.min_start = (long *)ptr;
    ptr = (char *)(pdata->event.min_start+nshifted_items);
    pdata->event.max_end = (long *)ptr;
    ptr = (char *)(pdata->event.max_end+nshifted_items);
    pdata->margin.type1 = (TAGGED *)ptr;
    ptr = (char *)(pdata->margin.type1+nmargs);
    pdata->margin.type2 = (TAGGED *)ptr;
    ptr = (char *)(pdata->margin.type2+nmargs);
    pdata->margin.amount = (long *)ptr;
    ptr = (char *)(pdata->margin.amount+nmargs);
    pdata->margin.next = (MARGIN *)ptr;
    ptr = (char *)(pdata->margin.next+nmargs);
    if (ptr != (char *)(pdata+1)+total_size)
      printf("SHOULDN'T HAPPEN: expected ptr=0x%p, got 0x%p\nitems",
	     (char *)(pdata+1)+total_size, ptr);

    pdata->destructor = disjoint1_destructor;
    FD_STORE_SPENV(pdata->spenv);
    pdata->refbase = SP_alloc_term_refs(4*nitems);
    pdata->stamp = state_stamp-1; /* DON'T trust initially */
    pdata->nitems = nitems;
    pdata->nshifted_items = nshifted_items;
#if CSTATE_ONLY
    pdata->ntargets = nitems;
    pdata->nsources = 0;
    pdata->ndone = 0;
#endif
    pdata->nmargs = nmargs;
    pdata->nmargs_aligned = nmargs_aligned;
    pdata->flags = flags;
    pdata->lborder = 0;
    pdata->rborder = 0;
    pdata->maxmargin = 0;
    pdata->maxdur = 0;
    if (flags & 0x2) {
      DerefArg(tmp,opt,2);
      pdata->lborder = GetSmall(tmp);
      DerefArg(tmp,opt,3);
      pdata->rborder = GetSmall(tmp);
    }
				/* wrap-around can triple the #forbidden regions */

    if (flags & 0x4) {		/* build margin table */
      DerefArg(items,opt,4);
      i = 0;
      while (TagIsLST(items)) {
	DerefCar(item,items);
	DerefCdr(items,items);
	DerefArg(pdata->margin.type1[i],item,1);
	DerefArg(pdata->margin.type2[i],item,2);
	DerefArg(tmp,item,3);
	pdata->margin.amount[i] = l =
	  (TagIsSmall(tmp) ? GetSmall(tmp) : CLPFD_MAXINT>>1); /* TODO: cheating */
	if (pdata->maxmargin<l) pdata->maxmargin = l;
	i++;
      }
      pdata->nmargs = i;
      for (nmargs_aligned=1; nmargs_aligned<i; nmargs_aligned <<= 1)
	;
      for (i=0; i<nmargs_aligned; i++)
	pdata->margtab[i] = TERMIN;
      for (i=0; i<nmargs; i++) {
	TAGGED key = ((pdata->margin.type1[i]^pdata->margin.type2[i])>>LogSizeOfWord) & (nmargs_aligned-1);
	MARGIN m = pdata->margtab[key];

	pdata->margtab[key] = i;
	pdata->margin.next[i] = m;
      }
    }
				/* transfer all the items */
    DerefArg(items,X(0),3);
    for (i=0; i<nitems; i++) {
      SP_term_ref ref = pdata->refbase+(i<<2);

      TARGET(i) = i;
      DerefCar(item,items);
      DerefCdr(items,items);
      DerefArg(tmp,item,1);	/* start's domain variable */
      RefTerm(ref+1) = tmp;
      DerefArg(tmp,item,2);	/* start's attribute */
      RefTerm(ref) = tmp;
      DerefArg(tmp,item,3);	/* duration's domain variable */
      RefTerm(ref+3) = tmp;
      DerefArg(tmp,item,4);	/* duration's attribute */
      RefTerm(ref+2) = tmp;
      DerefArg(TYPE(i),item,5);	/* type */
      STATUS(i) = (STATUS_SOURCE+STATUS_TARGET)<<4;
    }
  }

				/* RESUME HERE */

#if !CSTATE_ONLY
  DerefArg(tmp,X(0),4);
  pdata->ntargets = GetSmall(tmp);
  DerefArg(tmp,X(0),5);
  pdata->nsources = GetSmall(tmp);
  nactive_items = pdata->nsources + pdata->ntargets;
#endif
  if (state_stamp != pdata->stamp) { /* trust nothing */
    incremental = FALSE;
#if CSTATE_ONLY
    pdata->ntargets = nitems - pdata->ndone;
    pdata->nsources = 0;
    nactive_items = pdata->ntargets;
#endif
#if INCREMENTAL
    pdata->bbmin = -CLPFD_MAXINT;
    pdata->bbmax = CLPFD_MAXINT;
#endif
    for (i=0; i<pdata->ntargets; i++) {
      ITEM it = TARGET(i);
      
      PRUNED_AT(it) = -1;
      STATUS(it) |= STATUS(it)>>4;
      if (STATUS(it) & STATUS_TARGET) {
	SP_term_ref ref = pdata->refbase+(it<<2);
	tmp = RefTerm(ref);	/* start's attribute */
	DerefAttribute(tmp,tmp); /* dom/4 term */
	arg = TagToArg(tmp,0);
	FDSET(it) = arg[1];
	SMIN(it) = arg[2];
	SMAX(it) = arg[3];
	tmp = RefTerm(ref+2);	/* duration's attribute */
	DerefAttribute(tmp,tmp); /* dom/4 term */
	arg = TagToArg(tmp,0);
	DUR(it) = GetSmall(arg[2]);
	DURmax(it) = GetSmall(arg[3]);
	PRUNED_AT(it) = 0;
      }
    }
    for (; i<nactive_items; i++) { /* 3.9 */
      ITEM it = TARGET(i);

      STATUS(it) |= STATUS_SOURCE;
    }
  } else {			/* check if anything was pruned */
    incremental = ((flags & 0x20) == 0x20);
#if INCREMENTAL
    if (!(flags & 0x2)) {	/* wrap-around => use maximal bounding box */
      pdata->bbmin = CLPFD_MAXINT;
      pdata->bbmax = -CLPFD_MAXINT;
    }
#endif
#if INCREMENTAL > 1
    pruned1 = TERMIN;
#endif
    for (i=0; i<pdata->ntargets; i++) {
      ITEM it = TARGET(i);
      long at = -1;
      
      if (STATUS(it)&STATUS_TARGET) { /* always true? */
	SP_term_ref ref = pdata->refbase+(it<<2);
      
	tmp = RefTerm(ref);	/* start's attribute */
	DerefAttribute(tmp,tmp); /* dom/4 term */
	arg = TagToArg(tmp,0);
	FDSET(it) = arg[1];
	if (SMIN(it) != arg[2] || SMAX(it) != arg[3]) {
	  SMIN(it) = arg[2];
	  SMAX(it) = arg[3];
	  at = 0;
	}
	tmp = RefTerm(ref+2);	/* duration's attribute */
	DerefAttribute(tmp,tmp); /* dom/4 term */
	arg = TagToArg(tmp,0);
	if (DUR(it) != GetSmall(arg[2]) ||
	    DURmax(it) != GetSmall(arg[3])) {
	  DUR(it) = GetSmall(arg[2]);
	  DURmax(it) = GetSmall(arg[3]);
	  at = 0;
	}
      }
      PRUNED_AT(it) = at;
#if INCREMENTAL
      if (at==0) {
#if INCREMENTAL > 1
	if (pruned1 == TERMIN) pruned1 = it;
	pruned2 = it;
#endif
	if (pdata->bbmin > EST(it))
	  pdata->bbmin = EST(it);
	if (pdata->bbmax < LCT(it))
	  pdata->bbmax = LCT(it);
      }
#endif
    }
#if INCREMENTAL
    if ((flags & 0x4) && pdata->bbmax < CLPFD_MAXINT) {
      pdata->bbmin -= 2*pdata->maxmargin;
      pdata->bbmax += 2*pdata->maxmargin;
    }
#endif
  }
  pdata->stamp = state_stamp+1;

  /* sort targets by decreasing duration */

  qsort_desc_dur1d(pdata->target, pdata->ntargets);

#if INITIAL_CHECK
  if (!incremental &&
      !(flags & 0x4)) {		/* not applicable if there are margins */
				/* collect all sources&targets
				   that have compulsory parts */
    for (i=j=0; i<nactive_items; i++) {
      ITEM it = TARGET(i);
      
      if (/*!(STATUS(it) & STATUS_TARGET) || wrong */
	     SLACK(it) >= 0
#if INCREMENTAL
	  || EST(it)>=pdata->bbmax
	  || LCT(it)<=pdata->bbmin
#endif
	     )
	continue;		/* not target or no compulsory part */
      pdata->source[j] = j;
      pdata->shifted_item.item[j++] = it;
    }

    if (j>0 && !disjoint1_check(j))
      goto ret;
  }
#endif
  
  /* refresh maxdur; build linked list by descending area */
  
  pdata->maxdur = 0;
  pdata->items_by_area = TERMIN;
  for (i=pdata->ntargets-1; i>=0; i--) {
    ITEM item = TARGET(i);
    
    if (STATUS(item) & STATUS_TARGET) {
      pdata->item.next_area[item] = pdata->items_by_area;
      pdata->items_by_area = item;
      l = DUR(item);
      if (pdata->maxdur<l)
	pdata->maxdur = l;
    }
  }
  /* initialize sortarr for use in edge finding */

  for (i=0; i<nactive_items; i++)
    pdata->sortarr[i] = TARGET(i);
  
  change = TRUE;
  pdata->iteration = 1;
#if INCREMENTAL > 1
  if (pruned1==TERMIN) {
    ent = 0;
    goto ret;
  } else if (pruned1==pruned2 && !(flags & 0xe)) {
    /* no margins, wrap-around, or global reasoning */
    /* special case of first part of disjoint1_filter() */
    long start, end, dur_threshold;
    ITEM *target0 = pdata->sortarr + 2*nitems;
    int tix = 0;

    BASIC_RELATIVE_FORBIDDEN_REGION1D(pruned1, start, end);
    dur_threshold = start>end ? start-end : 1;

    for (i=0; i<pdata->ntargets; i++) {
      ITEM titem = TARGET(i);
      
      if (titem==pruned1) {
	target0[tix++] = titem;
      } else if (!(STATUS(titem)&STATUS_TARGET))
	continue;
      else if (DUR(titem) < dur_threshold)
	continue;
      else if (!(EST(titem)<=end && LaST(titem)>=start-DUR(titem)))
	continue;
      else
	target0[tix++] = titem;
    }
    change = FALSE;
    j = collect_source1d(TRUE);
    if (j>0)
      if (!disjoint1_filter(j,target0,tix,&change))
	goto ret;
  }
#endif

  for (; change; pdata->iteration++) {
    change = FALSE;
    if (!(flags & 0x4)) { /* no margins */
      j = collect_source1d(TRUE);
      if (j>0)
	if (!disjoint1_filter(j,pdata->target,pdata->ntargets,&change))
	  goto ret;
    } else {			/* margins */
      int ti;

      for (ti=0; ti<pdata->ntargets; ti++) {
	ITEM titem = TARGET(ti);
	
	if (!(STATUS(titem)&STATUS_TARGET))
	  continue;
#if INCREMENTAL
	if (EST(titem)>=pdata->bbmax || LCT(titem)<=pdata->bbmin)
	  continue;
#endif
	j = collect_source1d(TRUE);
	if (j>0)
	  if (!disjoint1_filter(j,pdata->target+ti,1,&change))
	    goto ret;
      }
    }
    if (!change && !incremental && (flags & 0x8))
      if (!edge_finding1d_up(&change) ||
	  !edge_finding1d_down(&change))
	goto ret;
  }

#if DECOMPOSITION
  j = collect_source1d(FALSE);
  decompose1d(j,nactive_items,incremental);

  for (i=0; i<nactive_items; i++) {
    ITEM it = TARGET(i);
    
    if (STATUS(it)&STATUS_MARKED) {
      STATUS(it) &= ~STATUS_MARKED;
    } else {
      STATUS(it) &= ~STATUS_SOURCE;
    }
  }
#endif

  ent = 1;
  for (i=0; i<pdata->ntargets; i++) {
    ITEM it = TARGET(i);
    SP_term_ref ref = pdata->refbase+(it<<2);

    if (STATUS(it)&STATUS_TARGET)
      ent = 0;
    if (PRUNED_AT(it)>0)
      request_tell_interval(w, RefTerm(ref), RefTerm(ref+1), SMIN(it), SMAX(it), 2, 3);
  }
  if (ent) goto ret;

#if DECOMPOSITION

  /* partition into SOURCE+TARGET and SOURCE */

  {
    int delta;
    int inf = 0;
    int sup = pdata->ntargets - 1;
    ITEM held = TARGET(sup); /* sup is the hole */
    ITEM current = TARGET(inf);
    
    while (inf<=sup) {
      if (STATUS(current) & STATUS_TARGET) {
	TARGET(inf) = current;
	inf++;
	current = (inf>=sup ? held : TARGET(inf));
      } else {
	TARGET(sup) = current;
	sup--;
	current = (inf>=sup ? held : TARGET(sup));
      }
    }
    delta = pdata->ntargets - inf;
    pdata->ntargets -= delta;
    pdata->nsources += delta;
  }
  
  /* find the last real SOURCE */

  {
    int delta, sup;
    
    for (sup=nactive_items; sup>pdata->ntargets; --sup)
      if (STATUS(TARGET(sup-1)) & STATUS_SOURCE)
	break;
    delta = nactive_items - sup;
    pdata->nsources -= delta;
    nactive_items -= delta;
  }

  if (committed)
    for (i=pdata->ntargets; i<nactive_items; i++) {
      ITEM it = TARGET(i);
      
      STATUS(it) &= ~(STATUS_TARGET<<4);
    }
#endif
  
ret:
#if CSTATE_ONLY
  if (committed)
    pdata->ndone = nitems - nactive_items;
#else
  CTagToArg(X(0),4) = MakeSmall(pdata->ntargets);
  CTagToArg(X(0),5) = MakeSmall(pdata->nsources);
#endif
  Pfree(ent==1,handle,pdata);
  RefTerm(Actions) = request_done(w, ent, 2, 3);
}


/************************************************************/
/* Support for 2D disjoint                                  */
/************************************************************/

#if MULTI_SP_AWARE
#if 0                           /* not redefined. It uses FD_SETUP_SPENV to access the SPEnv */
#define disjoint2_destructor(A1) disjoint2_destructor(HIDDEN_ARG, A1)
#endif
#define cmp_desc_dur2d0(A1,A2) cmp_desc_dur2d0(HIDDEN_ARG, A1,A2)
#define qsort_desc_dur2d0swap(A1,A2,A3,A4) qsort_desc_dur2d0swap(HIDDEN_ARG, A1,A2,A3,A4)
#define qsort_desc_dur2d0med3(A1,A2,A3) qsort_desc_dur2d0med3(HIDDEN_ARG, A1,A2,A3)
#define qsort_desc_dur2d0(A1,A2) qsort_desc_dur2d0(HIDDEN_ARG, A1,A2)
#define cmp_asc_est2d0(A1,A2) cmp_asc_est2d0(HIDDEN_ARG, A1,A2)
#define qsort_asc_est2d0swap(A1,A2,A3,A4) qsort_asc_est2d0swap(HIDDEN_ARG, A1,A2,A3,A4)
#define qsort_asc_est2d0med3(A1,A2,A3) qsort_asc_est2d0med3(HIDDEN_ARG, A1,A2,A3)
#define qsort_asc_est2d0(A1,A2) qsort_asc_est2d0(HIDDEN_ARG, A1,A2)
#define cmp_desc_est2d0(A1,A2) cmp_desc_est2d0(HIDDEN_ARG, A1,A2)
#define qsort_desc_est2d0swap(A1,A2,A3,A4) qsort_desc_est2d0swap(HIDDEN_ARG, A1,A2,A3,A4)
#define qsort_desc_est2d0med3(A1,A2,A3) qsort_desc_est2d0med3(HIDDEN_ARG, A1,A2,A3)
#define qsort_desc_est2d0(A1,A2) qsort_desc_est2d0(HIDDEN_ARG, A1,A2)
#define cmp_asc_lct2d0(A1,A2) cmp_asc_lct2d0(HIDDEN_ARG, A1,A2)
#define qsort_asc_lct2d0swap(A1,A2,A3,A4) qsort_asc_lct2d0swap(HIDDEN_ARG, A1,A2,A3,A4)
#define qsort_asc_lct2d0med3(A1,A2,A3) qsort_asc_lct2d0med3(HIDDEN_ARG, A1,A2,A3)
#define qsort_asc_lct2d0(A1,A2) qsort_asc_lct2d0(HIDDEN_ARG, A1,A2)
#define cmp_asc_est2d1(A1,A2) cmp_asc_est2d1(HIDDEN_ARG, A1,A2)
#define qsort_asc_est2d1swap(A1,A2,A3,A4) qsort_asc_est2d1swap(HIDDEN_ARG, A1,A2,A3,A4)
#define qsort_asc_est2d1med3(A1,A2,A3) qsort_asc_est2d1med3(HIDDEN_ARG, A1,A2,A3)
#define qsort_asc_est2d1(A1,A2) qsort_asc_est2d1(HIDDEN_ARG, A1,A2)
#define cmp_desc_est2d1(A1,A2) cmp_desc_est2d1(HIDDEN_ARG, A1,A2)
#define qsort_desc_est2d1swap(A1,A2,A3,A4) qsort_desc_est2d1swap(HIDDEN_ARG, A1,A2,A3,A4)
#define qsort_desc_est2d1med3(A1,A2,A3) qsort_desc_est2d1med3(HIDDEN_ARG, A1,A2,A3)
#define qsort_desc_est2d1(A1,A2) qsort_desc_est2d1(HIDDEN_ARG, A1,A2)
#define cmp_asc_lct2d1(A1,A2) cmp_asc_lct2d1(HIDDEN_ARG, A1,A2)
#define qsort_asc_lct2d1swap(A1,A2,A3,A4) qsort_asc_lct2d1swap(HIDDEN_ARG, A1,A2,A3,A4)
#define qsort_asc_lct2d1med3(A1,A2,A3) qsort_asc_lct2d1med3(HIDDEN_ARG, A1,A2,A3)
#define qsort_asc_lct2d1(A1,A2) qsort_asc_lct2d1(HIDDEN_ARG, A1,A2)
#define cmp_asc_start0(A1,A2) cmp_asc_start0(HIDDEN_ARG, A1,A2)
#define qsort_asc_start0swap(A1,A2,A3,A4) qsort_asc_start0swap(HIDDEN_ARG, A1,A2,A3,A4)
#define qsort_asc_start0med3(A1,A2,A3) qsort_asc_start0med3(HIDDEN_ARG, A1,A2,A3)
#define qsort_asc_start0(A1,A2) qsort_asc_start0(HIDDEN_ARG, A1,A2)
#define cmp_asc_end0(A1,A2) cmp_asc_end0(HIDDEN_ARG, A1,A2)
#define qsort_asc_end0swap(A1,A2,A3,A4) qsort_asc_end0swap(HIDDEN_ARG, A1,A2,A3,A4)
#define qsort_asc_end0med3(A1,A2,A3) qsort_asc_end0med3(HIDDEN_ARG, A1,A2,A3)
#define qsort_asc_end0(A1,A2) qsort_asc_end0(HIDDEN_ARG, A1,A2)
#define cmp_asc_start1(A1,A2) cmp_asc_start1(HIDDEN_ARG, A1,A2)
#define qsort_asc_start1swap(A1,A2,A3,A4) qsort_asc_start1swap(HIDDEN_ARG, A1,A2,A3,A4)
#define qsort_asc_start1med3(A1,A2,A3) qsort_asc_start1med3(HIDDEN_ARG, A1,A2,A3)
#define qsort_asc_start1(A1,A2) qsort_asc_start1(HIDDEN_ARG, A1,A2)
#define cmp_asc_end1(A1,A2) cmp_asc_end1(HIDDEN_ARG, A1,A2)
#define qsort_asc_end1swap(A1,A2,A3,A4) qsort_asc_end1swap(HIDDEN_ARG, A1,A2,A3,A4)
#define qsort_asc_end1med3(A1,A2,A3) qsort_asc_end1med3(HIDDEN_ARG, A1,A2,A3)
#define qsort_asc_end1(A1,A2) qsort_asc_end1(HIDDEN_ARG, A1,A2)
#define extend_forbidden_region2d(A1,A2,A3,A4) extend_forbidden_region2d(HIDDEN_ARG, A1,A2,A3,A4)
#define profile_init_complement(A1,A2) profile_init_complement(HIDDEN_ARG, A1,A2)
#define prune_item2d(A1,A2,A3,A4) prune_item2d(HIDDEN_ARG, A1,A2,A3,A4)
#define ori_unification_depth(A1,A2,A3) ori_unification_depth(HIDDEN_ARG, A1,A2,A3)
#define len_unification_depth(A1,A2,A3) len_unification_depth(HIDDEN_ARG, A1,A2,A3)
#define same_origins(A1,A2,A3,A4,A5) same_origins(HIDDEN_ARG, A1,A2,A3,A4,A5)
#define same_lengths(A1,A2,A3,A4,A5) same_lengths(HIDDEN_ARG, A1,A2,A3,A4,A5)
#define cmp_rectangles(A1,A2) cmp_rectangles(HIDDEN_ARG, A1,A2)
#define qsort_rectanglesswap(A1,A2,A3,A4) qsort_rectanglesswap(HIDDEN_ARG, A1,A2,A3,A4)
#define qsort_rectanglesmed3(A1,A2,A3) qsort_rectanglesmed3(HIDDEN_ARG, A1,A2,A3)
#define qsort_rectangles(A1,A2) qsort_rectangles(HIDDEN_ARG, A1,A2)
#define cmp_events_increasing_order(A1,A2) cmp_events_increasing_order(HIDDEN_ARG, A1,A2)
#define qsort_events_increasing_orderswap(A1,A2,A3,A4) qsort_events_increasing_orderswap(HIDDEN_ARG, A1,A2,A3,A4)
#define qsort_events_increasing_ordermed3(A1,A2,A3) qsort_events_increasing_ordermed3(HIDDEN_ARG, A1,A2,A3)
#define qsort_events_increasing_order(A1,A2) qsort_events_increasing_order(HIDDEN_ARG, A1,A2)
#define cmp_events_decreasing_order(A1,A2) cmp_events_decreasing_order(HIDDEN_ARG, A1,A2)
#define qsort_events_decreasing_orderswap(A1,A2,A3,A4) qsort_events_decreasing_orderswap(HIDDEN_ARG, A1,A2,A3,A4)
#define qsort_events_decreasing_ordermed3(A1,A2,A3) qsort_events_decreasing_ordermed3(HIDDEN_ARG, A1,A2,A3)
#define qsort_events_decreasing_order(A1,A2) qsort_events_decreasing_order(HIDDEN_ARG, A1,A2)
#define free_synchro_table(A1) free_synchro_table(HIDDEN_ARG, A1)
#define check_if_use_sweep_synchro(A1) check_if_use_sweep_synchro(HIDDEN_ARG, A1)
#define sweep_synchronization(A1,A2,A3) sweep_synchronization(HIDDEN_ARG, A1,A2,A3)
#define wake_sweep_synchro(A1,A2,A3) wake_sweep_synchro(HIDDEN_ARG, A1,A2,A3)
#define prune_before2d(A1,A2,A3) prune_before2d(HIDDEN_ARG, A1,A2,A3)
#define edge_finding2d_up(A1,A2) edge_finding2d_up(HIDDEN_ARG, A1,A2)
#define edge_finding2d_down(A1,A2) edge_finding2d_down(HIDDEN_ARG, A1,A2)
#define next_start2d(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10) next_start2d(HIDDEN_ARG, A1,A2,A3,A4,A5,A6,A7,A8,A9,A10)
#define next_end2d(A1,A2,A3,A4,A5,A6,A7,A8) next_end2d(HIDDEN_ARG, A1,A2,A3,A4,A5,A6,A7,A8)
#define prev_start2d(A1,A2,A3,A4,A5,A6,A7) prev_start2d(HIDDEN_ARG, A1,A2,A3,A4,A5,A6,A7)
#define prev_end2d(A1,A2,A3,A4,A5,A6,A7,A8,A9) prev_end2d(HIDDEN_ARG, A1,A2,A3,A4,A5,A6,A7,A8,A9)
#define trace_orig(A1,A2,A3,A4,A5,A6) trace_orig(HIDDEN_ARG, A1,A2,A3,A4,A5,A6)
#define feasible_minwit(A1,A2,A3,A4,A5,A6) feasible_minwit(HIDDEN_ARG, A1,A2,A3,A4,A5,A6)
#define feasible_maxwit(A1,A2,A3,A4,A5,A6) feasible_maxwit(HIDDEN_ARG, A1,A2,A3,A4,A5,A6)
#define less_constrained2d(A1,A2,A3,A4) less_constrained2d(HIDDEN_ARG, A1,A2,A3,A4)
#define dominates2d(A1,A2,A3,A4) dominates2d(HIDDEN_ARG, A1,A2,A3,A4)
#define disjoint2_filter(A1,A2,A3,A4,A5) disjoint2_filter(HIDDEN_ARG, A1,A2,A3,A4,A5)
#define disjoint2_check(A1,A2) disjoint2_check(HIDDEN_ARG, A1,A2)
#define decompose2d(A1,A2,A3) decompose2d(HIDDEN_ARG, A1,A2,A3)
#define collect_source2d(A1) collect_source2d(HIDDEN_ARG, A1)
#define invariants2d() invariants2d(HIDDEN_ARG)
#endif /* MULTI_SP_AWARE */

/* Temporary data structure created each time the constraint is woken and reinitialized before
   handling each task. */
/* ...........................................................................................*/
struct matching_frame {
                                            /* REPRESENTATION OF THE MATCHING */
  int  *assign        ;                     /* 1..max_nb_part, index (0..nitems-1) of assignment */
                                            /* var.associated to ith part of a task */
  int  *hidden        ;                     /* 1..nb_resource, 0 if a resource node is hidden, */
                                            /* 1 if not */
  int  *rmate         ;                     /* 1..max_nb_part, resource associated to an */
                                            /* assignment variable */
  int  *ymate         ;                     /* 1..nb_resource, assignment variable associated to */
                                            /* a resource */
  int  *mark          ;                     /* 1..nb_resource, status of a resource node: */
                                            /* UNREACHED, REACHED, REACHEDBUTHIDDEN */

#define UNREACHED        0                  /* status of a node */
#define REACHED          1
#define REACHEDBUTHIDDEN 2

  int  *father        ;                     /* 1..nb_resource, assignment var.from which reach */
                                            /* a given resource */

                                            /* THE NOT YET MATCHED ASSIGNMENT VARIABLES */
  int   first_free_var;                     /* first not yet matched assignment variable */
  int  *next_free_var ;                     /* 1..max_nb_part, next not yet matched */
                                            /* assignment variable */
  int  *prev_free_var ;                     /* 1..max_nb_part, previous not yet matched */
                                            /* assignment variable */

                                            /* THE QUEUE FOR THE BREATH FIRST SEARCH */
  int  *queue_of_nodes;                     /* 1..max_nb_part, the queue */

                                            /* START AND END EVENTS */
  int  *sort_event    ;                     /* 0..nitems+nitems-1, index of events sorted in */
                                            /* increasing order of their date */

                                            /* the next 3 array are for sweeping from min to max */
  int  *type_event1   ;                     /* 0..nitems+nitems-1, 0 if start event, 1 if end */
  long *date_event1   ;                     /* 0..nitems+nitems-1, date associated to the event */
  long *ress_event1   ;                     /* 0..nitems+nitems-1, resource assoc. to the event */

                                            /* the next 3 array are for sweeping from max to min */
  int  *type_event2   ;                     /* 0..nitems+nitems-1, 0 if start event, 1 if end */
  long *date_event2   ;                     /* 0..nitems+nitems-1, date associated to the event */
  long *ress_event2   ;                     /* 0..nitems+nitems-1, resource assoc. to the event */

                                            /* SWEEP-LINE STATUS */
  int  *count_forbid  ;                     /* 1..nb_resource, count number of forbidden regions */
                                            /* which overlap a given resource for the current */
                                            /* position of the sweep-line */
};

/* SWEEP SYNCHRONIZATION FRAME ALLOCATED WHEN THE CONSTRAINT IS CREATED */
/*............................................................................................... */
struct synchro_frame {
                                            /* ASSIGNMENT AND TEMPORAL DIMENSIONS */
                                            /* (INITIALIZED WHEN CREATE CONSTRAINT) */
                                            /* assignment dimension: dimension where all */
  int              adim                  ;  /* the rectangles sizes are equal to 1 (0 or 1) */
                                            /* temporal dimension: dimension different from */
  int              tdim                  ;  /* the assignment dimension */
 
                                            /* TASKS AND THEIR PARTS */
                                            /* (UPDATED EACH TIME WE WAKE THE CONSTRAINT) */
                                            /* smallest value for the assignment variables */
  long             min_resource          ;  /* of the part of the different tasks */
                                            /* largest value for the assignment variables */
  long             max_resource          ;  /* of the part of the different tasks */
  int              nb_resource           ;  /* max_resource - min_resource + 1 */
  int              max_nb_part           ;  /* maximum of number of parts of the different tasks */

                                            /* (UPDATED BEFORE HANDLING CURRENT TASKS) */
                                            /* smallest value for the assignment variables */
  long             min_resource_cur      ;  /* of the part of the task we currently handle */
                                            /* largest value for the assignment variables */
  long             max_resource_cur      ;  /* of the part of the task we currently handle */
  int              nb_resource_cur       ;  /* max_resource_cur - min_resource_cur + 1 */
  int              nb_part_cur           ;  /* number of parts of the current task */

                                            /* (UPDATED EACH TIME WE WAKE THE CONSTRAINT) */
                                            /* 0..nitems-1, depth of the temporal origin of each */
                                            /* rectangle, used for catching the fact that a */
  int             *ori_depth             ;  /* variable was unified or fixed to a value */
                                            /* 0..nitems-1, depth of the temporal duration of */
                                            /* each rectangle, used for catching the fact that */
  int             *len_depth             ;  /* a variable was unified or fixed to a value */
  int             *sort_task             ;  /* 0..nitems-1, index of rectangles sorted on their */
                                            /* temporal origin and temporal size */
  int              first_task            ;  /* index of first task of the list of tasks */
  int             *next_task             ;  /* 0..nitems-1, index of the task next to a task */
  int             *last_part             ;  /* 0..nitems-1, index of the last part of a task */
  int             *next_part             ;  /* 0..nitems-1, index of the task next to a task part */

                                            /* USED FOR PASSING INFORMATION BETWEEN CONSECUTIVE */
                                            /* CALLS OF synchronize_parts_of_a_task */
                                            /* a hint for matching the assignment variables */
                                            /* 0..nitems-1, for a task for which the first two */
                                            /* parts are rectangles r1 and r2, hint[r1]=hint[r2] */
  int            *hint                   ;  /* indicates that hint was not yet initialised */
                                            /* 1 if could find a y-perfect matching, */
  int             result_of_previous_call;  /* 0 if could not, -1 if no previous call */
                                            /* 1 if the forest is valid, 0 if invalid or */
                                            /* if does not exist */
                                            /* forest_is_valid=1 => result_of_previous_call=0 */
  int             forest_is_valid        ;  /* result_of_previous_call=1 => forest_is_valid=0 */
                                            /* >0 if have a source for extending the matching */
  int             have_a_source          ;  /* (index of resource node), 0 else */
  int             have_a_root            ;  /* index of main root of the forest */
  int             nactive                ;  /* number of active rectanlges */
  struct matching_frame *matching_table  ;  /* pointer to the temporary frame related to matching */
};

/* The constraint frame. */
struct disjoint2_data {
  void (SPCDECL *destructor)(void*);
#if MULTI_SP_AWARE
  SPEnv *spenv;
#endif /* MULTI_SP_AWARE */
  SP_term_ref refbase;		/* static */
  long stamp;
  int iteration;		/* initially #pruned, then incremented */
  int nitems;			/* static */
  int nshifted_items;		/* nitems * (1 or 3 or 9), static */
  int ntargets;			/* #items that may be targets, := nitems */
  int nsources;			/* #items that may be sources only, := 0 */
#if CSTATE_ONLY
  int ndone;			/* #items that are forgotten _forever_, := 0 */
#endif
  int nmargs;			/* exact number */
  int nmargs_aligned;		/* power of 2, static */
  int flags;			/* static */
  long lborder[2];		/* static */
  long rborder[2];		/* static */
  long maxmargin[2];		/* static */
  long maxdur[2];		/* static */
#if INCREMENTAL
  long bbmin[2];		/* bounding box min */
  long bbmax[2];		/* bounding box min */
#endif
  ITEM items_by_area;		/* terminated by TERMIN */
  ITEM *sortarr;		/* [4*nitems] */
  ITEM *target;			/* [nitems], partitioned as (targets, qsorted)+(sources)+(done) */
  SHIFTED_ITEM *source;		/* [nshifted_items], perhaps qsorted */
  EVENT *start_event[2];	/* [nshifted_items], qsorted */
  EVENT *end_event[2];		/* [nshifted_items], qsorted */
  EVENT *start_event_t[2];	/* [nshifted_items], qsorted */
  EVENT *end_event_t[2];	/* [nshifted_items], qsorted */
  MARGIN *margtab;		/* [nmargs_aligned] */
  struct {
    TAGGED *min[2];		/* of current domain */
    TAGGED *max[2];		/* of current domain */
    TAGGED *fdset[2];		/* of current domain, GC-safe */
    long *mindur[2];		/* of current duration */
    long *maxdur[2];		/* of current duration */
    TAGGED *minwit[2];		/* witness for min[] */
    TAGGED *maxwit[2];		/* witness for max[] */
    long *pruned_at[2];		/* >0 if pruned */
    TAGGED *type;		/* type, static */
    long *status;		/* see STATUS_... */
    ITEM *next;			/* for task intervals, terminated by TERMIN */
    ITEM *next_area;		/* for task intervals, terminated by TERMIN */
  } item;			/* each [nitem] */
  struct {
    ITEM *item;
    long *amount[2];		/* virtual shift amount */
  } shifted_item;		/* each [nshifted_items] */
  struct {
    ITEM *item;
    long *start[2];		/* inclusive start of forbidden region */
    long *end[2];		/* inclusive end of forbidden region */
    long *min_start[2];		/* mimimal start among this and later end_event */
    long *max_end[2];		/* maximal end among this and earlier start_event */
    long *min_start_t[2];	/* mimimal start among this and later end_event_t */
    long *max_end_t[2];		/* maximal end among this and earlier start_event_t */
  } event;			/* each [nshifted_item] */
  struct {
    TAGGED *type1;
    TAGGED *type2;
    long *amount[2];
    MARGIN *next;		/* terminated by TERMIN */
  } margin;			/* each [nmargs] */
  /* space for the above arrays */
    struct synchro_frame *synchro_table;    /* pointer to the frame related to */
                                            /* sweep-synchronization, */
                                            /* NULL if no assignment dimension or if too many */
                                            /* assignment values */
};

/*----------------------------------------------------------------------
-- Free synchronization frame if it was created.
----------------------------------------------------------------------*/
static void free_synchro_table MAGIC (HIDDEN_PROTO
  struct disjoint2_data *pdata        )     /* pointer to rectangles frame */
{
  struct synchro_frame  *synchro_table;     /* pointer to synchronization frame */

  synchro_table = pdata->synchro_table;
  if (synchro_table != NULL) {              /* if synchronization frame was created because */
    SP_free(synchro_table->ori_depth );     /* the sweep synchronization method was used */
    SP_free(synchro_table->len_depth );
    SP_free(synchro_table->sort_task );
    SP_free(synchro_table->next_task );
    SP_free(synchro_table->last_part );
    SP_free(synchro_table->next_part );
    SP_free(synchro_table->hint      );
    SP_free(synchro_table            );
  }
}

static void SPCDECL disjoint2_destructor(void *pdata_v)
{
  struct disjoint2_data *pdata = (struct disjoint2_data *)pdata_v;
  FD_SETUP_SPENV(pdata->spenv)

  free_synchro_table(pdata);
  SP_free_term_refs(pdata->refbase,8*pdata->nitems);
  SP_free(pdata);
}


#define PRUNED_AT2D(t,dim) (pdata->item.pruned_at[dim][t])
#define FDSET2D(t,dim) (pdata->item.fdset[dim][t])
#define MIN2D(T,DIM) (pdata->item.min[DIM][T])
#define MAX2D(T,DIM) (pdata->item.max[DIM][T])
#define MINWIT2D(T,DIM) (pdata->item.minwit[DIM][T])
#define MAXWIT2D(T,DIM) (pdata->item.maxwit[DIM][T])
#define DUR2D(T,DIM) (pdata->item.mindur[DIM][T])
#define DURmax2D(T,DIM) (pdata->item.maxdur[DIM][T])
#define EST2D(T,DIM) GetSmall(pdata->item.min[DIM][(T)])
#define LST2D(T,DIM) GetSmall(pdata->item.max[DIM][(T)])
#define ECT2D(T,DIM) (EST2D(T,DIM)+pdata->item.mindur[DIM][(T)])
#define LCT2D(T,DIM) (LST2D(T,DIM)+pdata->item.mindur[DIM][(T)])
#define LCT2Dmax(T,DIM) (LST2D(T,DIM)+pdata->item.maxdur[DIM][(T)])
#define SLACK2D(T,DIM) (LST2D(T,DIM)-EST2D(T,DIM)-pdata->item.mindur[DIM][(T)])
#define AREA2D(T)    (pdata->item.mindur[0][(T)] * pdata->item.mindur[1][(T)])
#define ISTARGET(T) (STATUS(T)&STATUS_TARGET)
#define mindom_origin(T,DIM) EST2D(T,DIM)
#define maxdom_origin(T,DIM) LST2D(T,DIM)
#define mindom_length(T,DIM) DUR2D(T,DIM)
#define maxdom_length(T,DIM) DURmax2D(T,DIM)

/* zero-length target assumed */
#define BASIC_RELATIVE_FORBIDDEN_REGION2D(SOURCE, P1, P2) \
(P1)[0] = LST2D(SOURCE,0) + 1; \
(P2)[0] = EST2D(SOURCE,0) + pdata->item.mindur[0][SOURCE] - 1; \
(P1)[1] = LST2D(SOURCE,1) + 1; \
(P2)[1] = EST2D(SOURCE,1) + pdata->item.mindur[1][SOURCE] - 1;

/* zero-length target assumed */
#define RELATIVE_FORBIDDEN_REGION2D(SOURCE, P1, P2) \
(P1)[0] = LST2D(pdata->shifted_item.item[(SOURCE)],0)+pdata->shifted_item.amount[0][(SOURCE)] + 1; \
(P2)[0] = EST2D(pdata->shifted_item.item[(SOURCE)],0)+pdata->shifted_item.amount[0][(SOURCE)]  + pdata->item.mindur[0][(pdata->shifted_item.item[(SOURCE)])] - 1; \
(P1)[1] = LST2D(pdata->shifted_item.item[(SOURCE)],1)+pdata->shifted_item.amount[1][(SOURCE)] + 1; \
(P2)[1] = EST2D(pdata->shifted_item.item[(SOURCE)],1)+pdata->shifted_item.amount[1][(SOURCE)]  + pdata->item.mindur[1][(pdata->shifted_item.item[(SOURCE)])] - 1;

#define BASIC_FORBIDDEN_REGION2D(TARGET, SOURCE, P1, P2) \
(P1)[0] = LST2D(SOURCE,0) - pdata->item.mindur[0][(TARGET)] + 1; \
(P2)[0] = EST2D(SOURCE,0) + pdata->item.mindur[0][(SOURCE)] - 1; \
(P1)[1] = LST2D(SOURCE,1) - pdata->item.mindur[1][(TARGET)] + 1; \
(P2)[1] = EST2D(SOURCE,1) + pdata->item.mindur[1][(SOURCE)] - 1;

#define FORBIDDEN_REGION2D(TARGET, SOURCE, P1, P2) \
(P1)[0] = LST2D(pdata->shifted_item.item[(SOURCE)],0)+pdata->shifted_item.amount[0][(SOURCE)] - pdata->item.mindur[0][(TARGET)] + 1; \
(P2)[0] = EST2D(pdata->shifted_item.item[(SOURCE)],0)+pdata->shifted_item.amount[0][(SOURCE)]  + pdata->item.mindur[0][(pdata->shifted_item.item[(SOURCE)])] - 1; \
(P1)[1] = LST2D(pdata->shifted_item.item[(SOURCE)],1)+pdata->shifted_item.amount[1][(SOURCE)] - pdata->item.mindur[1][(TARGET)] + 1; \
(P2)[1] = EST2D(pdata->shifted_item.item[(SOURCE)],1)+pdata->shifted_item.amount[1][(SOURCE)]  + pdata->item.mindur[1][(pdata->shifted_item.item[(SOURCE)])] - 1;

/* for qsorting by descending Dur[0] */
static int cmp_desc_dur2d0 MAGIC (HIDDEN_PROTO
				  ITEM *t1, ITEM *t2)
{
  struct disjoint2_data *pdata = fd.gdata;
  return DUR2D(*t2,0) - DUR2D(*t1,0);
}


#define QType ITEM
#define QCmp  cmp_desc_dur2d0
#define QSort qsort_desc_dur2d0
#include "qsort.ic"

/* for qsorting by ascending EST2D(0) */
static int cmp_asc_est2d0 MAGIC (HIDDEN_PROTO
				 ITEM *t1, ITEM *t2)
{
  struct disjoint2_data *pdata = fd.gdata;
  return EST2D(*t1,0) - EST2D(*t2,0);
}


#define QType ITEM
#define QCmp  cmp_asc_est2d0
#define QSort qsort_asc_est2d0
#include "qsort.ic"

/* for qsorting by descending EST2D(0) */
static int cmp_desc_est2d0 MAGIC (HIDDEN_PROTO
				  ITEM *t1, ITEM *t2)
{
  struct disjoint2_data *pdata = fd.gdata;
  return EST2D(*t2,0) - EST2D(*t1,0);
}


#define QType ITEM
#define QCmp  cmp_desc_est2d0
#define QSort qsort_desc_est2d0
#include "qsort.ic"

/* for qsorting by ascending LCT2D(0) */
static int cmp_asc_lct2d0 MAGIC (HIDDEN_PROTO
				 ITEM *t1, ITEM *t2)
{
  struct disjoint2_data *pdata = fd.gdata;
  return LCT2D(*t1,0) - LCT2D(*t2,0);
}


#define QType ITEM
#define QCmp  cmp_asc_lct2d0
#define QSort qsort_asc_lct2d0
#include "qsort.ic"

/* for qsorting by ascending EST2D(1) */
static int cmp_asc_est2d1 MAGIC (HIDDEN_PROTO
				 ITEM *t1, ITEM *t2)
{
  struct disjoint2_data *pdata = fd.gdata;
  return EST2D(*t1,1) - EST2D(*t2,1);
}


#define QType ITEM
#define QCmp  cmp_asc_est2d1
#define QSort qsort_asc_est2d1
#include "qsort.ic"

/* for qsorting by descending EST2D(1) */
static int cmp_desc_est2d1 MAGIC (HIDDEN_PROTO
				  ITEM *t1, ITEM *t2)
{
  struct disjoint2_data *pdata = fd.gdata;
  return EST2D(*t2,1) - EST2D(*t1,1);
}


#define QType ITEM
#define QCmp  cmp_desc_est2d1
#define QSort qsort_desc_est2d1
#include "qsort.ic"

/* for qsorting by ascending LCT2D(1) */
static int cmp_asc_lct2d1 MAGIC (HIDDEN_PROTO
				 ITEM *t1, ITEM *t2)
{
  struct disjoint2_data *pdata = fd.gdata;
  return LCT2D(*t1,1) - LCT2D(*t2,1);
}


#define QType ITEM
#define QCmp  cmp_asc_lct2d1
#define QSort qsort_asc_lct2d1
#include "qsort.ic"


static int cmp_asc_start0 MAGIC (HIDDEN_PROTO
				 EVENT *t1, EVENT *t2)
{
  struct disjoint2_data *pdata = fd.gdata;
  return pdata->event.start[0][(*t1)] - pdata->event.start[0][(*t2)];
}


#define QType EVENT
#define QCmp  cmp_asc_start0
#define QSort qsort_asc_start0
#include "qsort.ic"


static int cmp_asc_end0 MAGIC (HIDDEN_PROTO
			       EVENT *t1, EVENT *t2)
{
  struct disjoint2_data *pdata = fd.gdata;
  return pdata->event.end[0][(*t1)] - pdata->event.end[0][(*t2)];
}


#define QType EVENT
#define QCmp  cmp_asc_end0
#define QSort qsort_asc_end0
#include "qsort.ic"


static int cmp_asc_start1 MAGIC (HIDDEN_PROTO
				 EVENT *t1, EVENT *t2)
{
  struct disjoint2_data *pdata = fd.gdata;
  return pdata->event.start[1][(*t1)] - pdata->event.start[1][(*t2)];
}


#define QType EVENT
#define QCmp  cmp_asc_start1
#define QSort qsort_asc_start1
#include "qsort.ic"

static int cmp_asc_end1 MAGIC (HIDDEN_PROTO
			       EVENT *t1, EVENT *t2)
{
  struct disjoint2_data *pdata = fd.gdata;
  return pdata->event.end[1][(*t1)] - pdata->event.end[1][(*t2)];
}


#define QType EVENT
#define QCmp  cmp_asc_end1
#define QSort qsort_asc_end1
#include "qsort.ic"

/* subtract from *p1 margin from target to source;
   add to *p2 margin from source to target.
*/
static int extend_forbidden_region2d MAGIC (HIDDEN_PROTO
					    ITEM target, ITEM source,
					    long *p1, long *p2)
{
  struct disjoint2_data *pdata = fd.gdata;
  TAGGED t1 = TYPE(target);
  TAGGED t2 = TYPE(source);
  TAGGED key = ((t1^t2)>>LogSizeOfWord) & (pdata->nmargs-1);
  MARGIN m = key;
  int hits = 0;

  while (m > TERMIN && hits<2) {
    if ((pdata->margin.type1[m]==t1) && (pdata->margin.type2[m]==t2)) { /* target -> source margin found */
      hits++;
      p1[0] -= pdata->margin.amount[0][m];
      p1[1] -= pdata->margin.amount[1][m];
    }
    if ((pdata->margin.type1[m]==t2) && (pdata->margin.type2[m]==t1)) { /* source -> target margin found */
      hits++;
      p2[0] += pdata->margin.amount[0][m];
      p2[1] += pdata->margin.amount[1][m];
    }
    m = pdata->margin.next[m];
  }
  return hits;
}



static PROFILE profile_init_complement MAGIC (HIDDEN_PROTO
					      TAGGED head, TAGGED tail)
{
  if (tail==EmptySet)
    return empty_profile();
  else {
    TAGGED head1 = CTagToCar(tail);
    TAGGED tail1 = CTagToCdr(tail);
    return profile_cons(GetSmall(RangeMax(head))+1,
			GetSmall(RangeMin(head1)),
			1,
			profile_init_complement(head1,tail1));
  }
}

static BOOL prune_item2d MAGIC (HIDDEN_PROTO
				ITEM sv,
				long lobound, long hibound,
				int k)
{
  struct disjoint2_data *pdata = fd.gdata;
  TAGGED oldmin = MIN2D(sv,k);
  TAGGED oldmax = MAX2D(sv,k);
  TAGGED newmin = MakeSmall(lobound);
  TAGGED newmax = MakeSmall(hibound);
  BOOL rc = TRUE;
  
  if (Tgt(newmin,oldmin) || Tlt(newmax,oldmax)) {
    if (Tgt(newmin,oldmin))
      MINWIT2D(sv,k) = TaggedLow; /* maintain invariant! */
    if (Tlt(newmax,oldmax))
      MAXWIT2D(sv,k) = TaggedLow; /* maintain invariant! */
    PRUNED_AT2D(sv,k) = pdata->iteration;
    rc = adjust_bounds(newmin, newmax, FDSET2D(sv,k), &MIN2D(sv,k), &MAX2D(sv,k));
#if INCREMENTAL
    for (k=0; k<2; k++) {
      if (pdata->bbmin[k] > EST2D(sv,k))
	pdata->bbmin[k] = EST2D(sv,k);
      if (pdata->bbmax[k] < LCT2D(sv,k))
	pdata->bbmax[k] = LCT2D(sv,k);
    }
#endif
  }
  return rc;
}

/*----------------------------------------------------------------------
-- CODE FOR SYNCHRONIZATION SUPPORT.
----------------------------------------------------------------------*/

#if DBG > 1
/*----------------------------------------------------------------------
-- Print an error number and exit.
----------------------------------------------------------------------*/
static void synchro_err(int err_number)
{
  printf("synchro_err number %d\n",err_number);
  exit(0);
}
#endif

/*----------------------------------------------------------------------
-- Return TRUE if value val belongs to the origin in dimension dim 
-- of rectangle rect, FALSE otherwise.
----------------------------------------------------------------------*/
static BOOL isindom_origin(
  struct disjoint2_data *pdata,
  int                    rect ,
  int                    dim  ,
  long                   val  )
{
  TAGGED tval = MakeSmall(val);

  if (ISTARGET(rect))
    return (Tge(tval,MIN2D(rect,dim)) &&
	    Tle(tval,MAX2D(rect,dim)) &&
	    fd_member(tval,FDSET2D(rect,dim)));
  else
    return (tval==MIN2D(rect,dim));
}

/*----------------------------------------------------------------------
-- Return the smallest value greater than val that belongs to the origin
-- in dimension dim of rectangle rect; when val corresponds to the
-- maximum value of the origin in dimension dim of rectangle rect then
-- return val+1.
----------------------------------------------------------------------*/
static long nextval_origin(
  struct disjoint2_data *pdata,
  int                    rect ,
  int                    dim  ,
  long                   val  )
{
  TAGGED tval = MakeSmall(val);
  TAGGED tmax = MAX2D(rect,dim)+IStep(1);
  TAGGED set  = FDSET2D(rect,dim);
  TAGGED range;

  if (ISTARGET(rect)) {
    while (set != EmptySet) {
      range = CTagToCar(set);
      set = CTagToCdr(set);
      switch (val_vs_range(tval,range)) {
      case CMP_INSIDE:
	if (tval == RangeMax(range))
	  break;
	tval += IStep(1);
	goto out;
      case CMP_BEFORE:
	tval = RangeMin(range);
	goto out;
      }
    }
    tval = tmax;
  } else {
    if (Tlt(tval,tmax-IStep(1)))
      tval = tmax-IStep(1);
  }
 out:
  if (Tgt(tval,tmax))
    tval = tmax;
  return GetSmall(tval);
}

/*----------------------------------------------------------------------
-- Returns the length of the dereference chain of the origin in dimension
-- dim of rectangle rect.  The length is -1 if it's ground, >=0
-- otherwise.
----------------------------------------------------------------------*/
static int ori_unification_depth MAGIC (HIDDEN_PROTO
  struct disjoint2_data *pdata,
  int                    rect ,
  int                    dim  )
{
  WAMENV;
  SP_term_ref ref = pdata->refbase + (rect<<3) + (dim<<2);
  int depth = 0;
  TAGGED tmp;

  tmp = RefMutable(CTagToArg(RefTerm(ref),3));
  while (TagToHeadfunctor(tmp) == fd.functor_v4) {
    depth++;
    tmp = RefMutable(CTagToArg(tmp,3));
  }
  /* tmp is a dom/4 term */
  if (DomainSize(tmp) == TaggedOne)
    depth = -1;
  return depth;
}

/*----------------------------------------------------------------------
-- Returns the length of the dereference chain of the length in dimension
-- dim of rectangle rect.  The length is -1 if it's ground, >=0
-- otherwise.
----------------------------------------------------------------------*/
static int len_unification_depth MAGIC (HIDDEN_PROTO
  struct disjoint2_data *pdata,
  int                    rect ,
  int                    dim  )
{
  WAMENV;
  SP_term_ref ref = pdata->refbase + (rect<<3) + (dim<<2) + 2;
  int depth = 0;
  TAGGED tmp;

  tmp = RefMutable(CTagToArg(RefTerm(ref),3));
  while (TagToHeadfunctor(tmp) == fd.functor_v4) {
    depth++;
    tmp = RefMutable(CTagToArg(tmp,3));
  }
  /* tmp is a dom/4 term */
  if (DomainSize(tmp) == TaggedOne)
    depth = -1;
  return depth;
}

/*----------------------------------------------------------------------
-- Return TRUE if the origin in dimension dim of rectangle rect1 and the
-- origin in dimension dim of rectangle rect2 correspond to the same
-- variable; in this case set keep to 1 if it would be better (for
-- instance less dereferencing later) to keep origin of rect1, or keep
-- to 1 if should keep origin of rect2. Otherwise return FALSE.  Note
-- that one is not oblige to follow the keep recommendation.
----------------------------------------------------------------------*/
static BOOL same_origins MAGIC (HIDDEN_PROTO
  struct disjoint2_data *pdata,
  int                    rect1,
  int                    rect2,
  int                    dim  ,
  int                   *keep )
{
  WAMENV;
  TAGGED x1 = RefTerm(pdata->refbase + (rect1<<3) + (dim<<2) + 1);
  TAGGED x2 = RefTerm(pdata->refbase + (rect2<<3) + (dim<<2) + 1);
  TAGGED t1;

  DerefSwitch(x1,t1,;);
  DerefSwitch(x2,t1,;);
  *keep = (rect1<rect2 ? 1 : 2);

  return (x1 == x2);
}

/*----------------------------------------------------------------------
-- Return TRUE if the length in dimension dim of rectangle rect1 and the
-- length in dimension dim of rectangle rect2 correspond to the same
-- variable; in this case set keep to 1 if it would be better (for
-- instance less dereferencing later) to keep length of rect1, or keep
-- to 1 if should keep length of rect2. Otherwise return FALSE.  Note
-- that one is not oblige to follow the keep recommendation.
----------------------------------------------------------------------*/
static BOOL same_lengths MAGIC (HIDDEN_PROTO
  struct disjoint2_data *pdata,
  int                    rect1,
  int                    rect2,
  int                    dim  ,
  int                   *keep )
{
  WAMENV;
  TAGGED x1 = RefTerm(pdata->refbase + (rect1<<3) + (dim<<2) + 3);
  TAGGED x2 = RefTerm(pdata->refbase + (rect2<<3) + (dim<<2) + 3);
  TAGGED t1;

  DerefSwitch(x1,t1,;);
  DerefSwitch(x2,t1,;);
  *keep = (rect1<rect2 ? 1 : 2);

  return (x1 == x2);
}

/*----------------------------------------------------------------------
-- Let ori1 and len1 be the origin and the length in the temporal
-- dimension of rectangle r1.  Let ori2 and len2 be the origin and the
-- length in the temporal dimension of rectangle r2.  Return (-1,0,1)
-- if (ori1,len1) is less than, equal to, or greater than (ori2,len2).
-- (ori1,len1) is less than or equal to (ori2,len2) if: (ori1 < ori2)
-- or (ori1 = ori2 and len1 <= len2).  Comparing two domain variables
-- means comparing their address.
----------------------------------------------------------------------*/
static int cmp_rectangles MAGIC (HIDDEN_PROTO
				 int *rect1, int *rect2)
{
  struct disjoint2_data  *pdata        ;    /* pointer to rectangles frame */
  struct synchro_frame   *synchro_table;    /* pointer to synchronization frame */
  int                     tdim         ;    /* temporal dimension */
  TAGGED                  x1, x2, t1   ;
  WAMENV                               ;

  pdata         = fd.gdata            ;
  synchro_table = pdata->synchro_table;
  tdim          = synchro_table->tdim ;

  x1 = RefTerm(pdata->refbase + (*rect1<<3) + (tdim<<2) + 1);
  x2 = RefTerm(pdata->refbase + (*rect2<<3) + (tdim<<2) + 1);
  DerefSwitch(x1,t1,;);
  DerefSwitch(x2,t1,;);
  if (x1 < x2) return -1;
  if (x1 > x2) return  1;
  x1 = RefTerm(pdata->refbase + (*rect1<<3) + (tdim<<2) + 3);
  x2 = RefTerm(pdata->refbase + (*rect2<<3) + (tdim<<2) + 3);
  DerefSwitch(x1,t1,;);
  DerefSwitch(x2,t1,;);
  if (x1 < x2) return -1;
  if (x1 > x2) return  1;
  return 0;
}

#define QType int
#define QCmp  cmp_rectangles
#define QSort qsort_rectangles
#include "qsort.ic"

/*----------------------------------------------------------------------
-- Used when sweep from min to max.  Return -1 if the date of event1 is
-- strictly less than the date of event2.  Return 1 if the date of
-- event1 is strictly greater than the date of event2.  If both events
-- have the same date then priority is given to the start event since
-- this can avoid the counter of forbidden region to pass from 1 to 0
-- and to restore a node which will be hidden again when we handle the
-- start event: Return -1 if the type of event1 is strictly less than
-- the type of event2.  Return 1 if the type of event1 is strictly
-- greater than the type of event2.  Return 0 otherwise.
----------------------------------------------------------------------*/
static int cmp_events_increasing_order MAGIC (HIDDEN_PROTO
					      int *event1, int *event2)
{
  struct disjoint2_data  *pdata         ;   /* pointer to rectangles frame */
  struct synchro_frame   *synchro_table ;   /* pointer to synchronization frame */
  struct matching_frame  *matching_table;   /* pointer to the temporary frame related to matching*/

  long                    date1         ;   /* date associated to event1 */
  long                    date2         ;   /* date associated to event2 */
  int                     type1         ;   /* type associated to event1 */
  int                     type2         ;   /* type associated to event2 */

  pdata          = fd.gdata                     ;
  synchro_table  = pdata->synchro_table         ;
  matching_table = synchro_table->matching_table;
  date1 = matching_table->date_event1[*event1];
  date2 = matching_table->date_event1[*event2];
  if (date1 < date2) return -1;
  if (date1 > date2) return  1;
  type1 = matching_table->type_event1[*event1];
  type2 = matching_table->type_event1[*event2];
  if (type1 < type2) return -1;
  if (type1 > type2) return  1;
  return 0;
}

#define QType int
#define QCmp  cmp_events_increasing_order
#define QSort qsort_events_increasing_order
#include "qsort.ic"

/*----------------------------------------------------------------------
-- Used when sweep from max to min.  Return -1 if the date of event1 is
-- strictly greater than the date of event2.  Return 1 if the date of
-- event1 is strictly less than the date of event2.  If both events
-- have the same date then priority is given to the start event since
-- this can avoid the counter of forbidden region to pass from 1 to 0
-- and to restore a node which will be hidden again when we handle the
-- start event: Return -1 if the type of event1 is strictly less than
-- the type of event2.  Return 1 if the type of event1 is strictly
-- greater than the type of event2.  Return 0 otherwise.
----------------------------------------------------------------------*/
static int cmp_events_decreasing_order MAGIC (HIDDEN_PROTO
					      int *event1, int *event2)
{
  struct disjoint2_data  *pdata         ;   /* pointer to rectangles frame */
  struct synchro_frame   *synchro_table ;   /* pointer to synchronization frame */
  struct matching_frame  *matching_table;   /* pointer to the temporary frame related to matching*/

  long                    date1         ;   /* date associated to event1 */
  long                    date2         ;   /* date associated to event2 */
  int                     type1         ;   /* type associated to event1 */
  int                     type2         ;   /* type associated to event2 */

  pdata          = fd.gdata                     ;
  synchro_table  = pdata->synchro_table         ;
  matching_table = synchro_table->matching_table;
  date1 = matching_table->date_event2[*event1];
  date2 = matching_table->date_event2[*event2];
  if (date1 > date2) return -1;
  if (date1 < date2) return  1;
  type1 = matching_table->type_event2[*event1];
  type2 = matching_table->type_event2[*event2];
  if (type1 < type2) return -1;
  if (type1 > type2) return  1;
  return 0;
}

#define QType int
#define QCmp  cmp_events_decreasing_order
#define QSort qsort_events_decreasing_order
#include "qsort.ic"

/*----------------------------------------------------------------------
-- Check if there exists an assignment dimension (i.e. all the size of
-- the rectangles in this dimension are equal to 1) with not too many
-- values (else some tables would become too big).  If an assignment
-- dimension is found then create the frame related to sweep
-- synchronization and record a pointer to it in the main frame
-- related to the rectangle non-overlapping constraint.  Assumes the
-- basic non-overlapping frame to be already created and initialized,
-- except field synchro_frame.
----------------------------------------------------------------------*/
static void check_if_use_sweep_synchro MAGIC (HIDDEN_PROTO
  struct disjoint2_data *pdata          )   /* pointer to rectangles frame */
{
  int                    nitems         ;   /* total number of active rectangles */
  int                    nfree_var      ;   /* number of free variables among origin and length */
  struct synchro_frame  *synchro_table  ;   /* pointer to synchronization frame */
  int                    xlength_are_one;   /* 1 as long as length are equal to 1 in dimension 0 */
  int                    ylength_are_one;   /* 1 as long as length are equal to 1 in dimension 1 */
  int                    r              ;   /* used for scanning rectangles */
  long                   mins           ;   /* minimum value of size of r in a given dimension */
  long                   maxs           ;   /* maximum value of size of r in a given dimension */
  int                    nitems5        ;   /* (5 * nitems) + 10 */
  long                   mina           ;   /* minimum value of assignment variable of r */
  long                   maxa           ;   /* maximum value of assignment variable of r */
  long                   xmin_resource  ;   /* smallest possible resource on dimension 0 */
  long                   xmax_resource  ;   /* largest  possible resource on dimension 0 */
  long                   ymin_resource  ;   /* smallest possible resource on dimension 1 */
  long                   ymax_resource  ;   /* largest  possible resource on dimension 1 */
  int                    size_table     ;   /* used for allocating arrays */

  /* GET INFORMATION FROM pdata */
  /*............................*/
  nitems = pdata->ntargets+pdata->nsources;

  /*if (0 != 0) {*/
  if ((pdata->flags&0x48)==0x40 &&          /* synchronization(true),margins(false) */
      nitems > 1) {                         /* if at least two rectangles */
    nfree_var = 0;                          /* initially no free variables */

    /* CHECK IF THERE EXIST AN ASSIGNMENT DIMENSION */
    /*..............................................*/
    xlength_are_one = 1;                    /* by default assume size to be equal to 1 in dim. 0 */
    ylength_are_one = 1;                    /* by default assume size to be equal to 1 in dim. 1 */
    r               = 0;                    /* index of first rectangle */
    while ((xlength_are_one || ylength_are_one)
       &&  (r < nitems)) {                  /* continue while length=1 for at least one dimension*/
      mins = mindom_length(r,0);      /* get minimum value of length[r,0] */
      maxs = maxdom_length(r,0);      /* get maximum value of length[r,0] */
      nfree_var += (mins != maxs);
      if (xlength_are_one                   /* if all previous sizes were equal to 1 in dim. 0 */
      && ((mins != 1) || (maxs != 1))) {    /* and if current size in dim. 0 is not equal to 1 */
        xlength_are_one = 0;                /* reset to false if current length[r,0] is not one */
      }
      mins = mindom_length(r,1);      /* get minimum value of length[r,1] */
      maxs = maxdom_length(r,1);      /* get maximum value of length[r,1] */
      nfree_var += (mins != maxs);
      if (ylength_are_one                   /* if all previous sizes were equal to 1 in dim. 1 */
      && ((mins != 1) || (maxs != 1))) {    /* and if current size in dim. 1 is not equal to 1 */
        ylength_are_one = 0;                /* reset to false if current length[r,1] is not one */
      }
      r++;                                  /* get index of next rectangle */
    }

    /* COMPUTE SMALLEST AND LARGEST POSSIBLE RESOURCE AND CHECK IF DIFFERENCE IS NOT TOO BIG */
    /* IF DIFFERENCE IS TOO BIG THE SYNCHRONIZATION METHOD WILL NOT BE USED */
    /*.......................................................................................*/
    nitems5 = (nitems * 5) + 10;
    r = 0;
    xmin_resource =  CLPFD_MAXINT;
    xmax_resource = -CLPFD_MAXINT;
    ymin_resource =  CLPFD_MAXINT;
    ymax_resource = -CLPFD_MAXINT;
    while ((xlength_are_one || ylength_are_one)
       &&  (r < nitems)) {
      mina = mindom_origin(r,0);      /* get minimum value of origin[r,0] */
      maxa = maxdom_origin(r,0);      /* get maximum value of origin[r,0] */
      nfree_var += (mina != maxa);
      if (xlength_are_one) {                /* if assignment dimension is dimension 0 */
        if (mina < xmin_resource) xmin_resource = mina;
        if (maxa > xmax_resource) xmax_resource = maxa;
        if ((xmax_resource - xmin_resource) > nitems5) xlength_are_one = 0;
      }
      mina = mindom_origin(r,1);      /* get minimum value of yorigin[r,1] */
      maxa = maxdom_origin(r,1);      /* get maximum value of yorigin[r,1] */
      nfree_var += (mina != maxa);
      if (ylength_are_one) {                /* if assignment dimension is dimension 1 */
        if (mina < ymin_resource) ymin_resource = mina;
        if (maxa > ymax_resource) ymax_resource = maxa;
        if ((ymax_resource - ymin_resource) > nitems5) ylength_are_one = 0;
      }
      r++;                                  /* get index of next rectangle */
    }

    /* IF ASSIGNMENT DIMENSION EXISTS */
    /*................................*/
    if ((xlength_are_one || ylength_are_one)/* if there exist an assignment dimension */
    &&  (nfree_var > 1)) {                  /* and if there exist at least two free variables */
      synchro_table = SP_malloc(sizeof(struct synchro_frame));
      if (xlength_are_one) {                /* if all length are equal to 1 on dimension 0 */
        synchro_table->adim = 0;            /* record assignment dimension */
        synchro_table->tdim = 1;            /* record temporal   dimension */
      } else {                              /* if all length are equal to 1 on dimension 1 */
        synchro_table->adim = 1;            /* record assignment dimension */
        synchro_table->tdim = 0;            /* record temporal   dimension */
      }
                                            /* allocate space for ori_depth [0..nitems-1] */
                                            /* allocate space for len_depth [0..nitems-1] */
                                            /* allocate space for sort_task [0..nitems-1] */
                                            /* allocate space for next_task [0..nitems-1] */
                                            /* allocate space for last_part [0..nitems-1] */
                                            /* allocate space for next_part [0..nitems-1] */
                                            /* allocate space for hint      [0..nitems-1] */
      size_table = nitems * sizeof(int);
      synchro_table->ori_depth  = SP_malloc(size_table);
      synchro_table->len_depth  = SP_malloc(size_table);
      synchro_table->sort_task  = SP_malloc(size_table);
      synchro_table->next_task  = SP_malloc(size_table);
      synchro_table->last_part  = SP_malloc(size_table);
      synchro_table->next_part  = SP_malloc(size_table);
      synchro_table->hint       = SP_malloc(size_table);

                                            /* -1 since no previous call */
                                            /* forest does not exist */
                                            /* don't have a source */
                                            /* don't have a root */
                                            /* temporaray matching table not yet allocated */
      synchro_table->result_of_previous_call =-1   ;
      synchro_table->forest_is_valid         = 0   ;
      synchro_table->have_a_source           = 0   ;
      synchro_table->have_a_root             = 0   ;
      synchro_table->nactive                 = nitems;
      synchro_table->matching_table          = NULL;
    } else {                                /* do not use the synchronization method if we don't */
      synchro_table = NULL;                 /* have a dimension were all sizes are equal to 1 */
    }                                       /* and if not at least two free variables */
  } else {                                  /* if less than two rectangles than do not use */
    synchro_table = NULL;                   /* sweep synchronization */
  }
  pdata->synchro_table = synchro_table;     /* record pointer to synchronization frame */
}

/*----------------------------------------------------------------------
-- Hide a resource j for all the assignment variables of the parts of the
-- task we currently handle.
----------------------------------------------------------------------*/
static void hide_resource_sweep_synchro(
  struct disjoint2_data *pdata          ,   /* pointer to rectangles frame */
  int                    j              )   /* j is the index of the hidden resource node */
{
  struct synchro_frame  *synchro_table  ;   /* pointer to synchronization frame */
  struct matching_frame *matching_table ;   /* pointer to the temporary frame related to matching*/
  int                    nb_resource_cur;   /* max_resource_cur - min_resource_cur + 1 */
  int                   *hidden         ;   /* 1..nb_resource_cur, 0 if a resource node is hidden*/
                                            /* 1 if not */
  int                   *ymate          ;   /* 1..nb_resource_cur, assignment variable associated*/
                                            /* to a resource */
  int                    first_free_var ;   /* first not yet matched assignment variable */
  int                   *next_free_var  ;   /* 1..nb_part_cur, next not yet matched */
                                            /* assignment variable */
  int                   *prev_free_var  ;   /* 1..nb_part_cur, previous not yet matched */
                                            /* assignment variable */
  int                    i              ;   /* index of assignment var. matched to value j */

  /* GET INFORMATION FROM TABLES */
  /*.............................*/
  synchro_table  = pdata->synchro_table;

#if DBG > 1
  if (synchro_table == NULL) synchro_err(8);
#endif

  matching_table = synchro_table->matching_table;

#if DBG > 1
  if (matching_table == NULL) synchro_err(9);
#endif

  nb_resource_cur = synchro_table->nb_resource_cur;
  hidden          = matching_table->hidden        ;
  ymate           = matching_table->ymate         ;
  first_free_var  = matching_table->first_free_var;
  next_free_var   = matching_table->next_free_var ;
  prev_free_var   = matching_table->prev_free_var ;

#if DBG > 1
  if (!((1 <= j) && (j <= nb_resource_cur))) synchro_err(10);
#endif

  if (hidden[j] == 0) {                     /* if resource node j is not yet hidden */
    hidden[j] = 1;                          /* mark node j as a hidden node */
    if (ymate[j] != 0) {                    /* if resource node j is matched to a variable */

      /* INSERT i AS THE FIRST ELEMENT OF THE FREE NODES LIST */
      /*......................................................*/
      i = ymate[j];                         /* get index of assignment var. matched to value j */

#if DBG > 1
      {				/* i does not belong to the list of free nodes */
	int kk; kk = first_free_var;
	if (i == 0) synchro_err(11);
	while (kk) {
	  if (kk == i) synchro_err(12); kk = next_free_var[kk];
	}
      }
#endif

      next_free_var[i] = first_free_var;
      prev_free_var[i] = 0;
      if (first_free_var) {
        prev_free_var[first_free_var] = i;
      }
      first_free_var = i;

    }
  }

  /* RECORD FIELDS WHICH MAY HAVE BEEN MODIFIED IN matching_table */
  /*..............................................................*/
  matching_table->first_free_var = first_free_var;
}

/*----------------------------------------------------------------------
-- Restore a resource j for all the assignment variables of the parts of
-- the task we currently handle.
----------------------------------------------------------------------*/
static void restore_resource_sweep_synchro(
  struct disjoint2_data *pdata          ,   /* pointer to rectangles frame */
  int                    j              )   /* j is the index of the restored resource node */
{
  struct synchro_frame  *synchro_table  ;   /* pointer to synchronization frame */
  struct matching_frame *matching_table ;   /* pointer to the temporary frame related to matching*/
  int                    nb_resource_cur;   /* max_resource_cur - min_resource_cur + 1 */
  int                    forest_is_valid;   /* 1 if forest is valid,0 if invalid or doesn't exist*/
  int                    have_a_source  ;   /* >0 if have a source for extending matching, 0 else*/
  int                   *hidden         ;   /* 1..nb_resource_cur, 0 if a resource node is hidden*/
                                            /* 1 if not */
  int                   *rmate          ;   /* 1..nb_part_cur, resource associated to an */
                                            /* assignment variable */
  int                   *ymate          ;   /* 1..nb_resource_cur, assignment variable associated*/
                                            /* to a resource */
  int                   *mark           ;   /* 1..nb_resource_cur, status of a resource node */
  int                    first_free_var ;   /* first not yet matched assignment variable */
  int                   *next_free_var  ;   /* 1..nb_part_cur, next not yet matched */
                                            /* assignment variable */
  int                   *prev_free_var  ;   /* 1..nb_part_cur, previous not yet matched */
                                            /* assignment variable */
  int                    i              ;   /* index of assignment var. matched to value j */
  int                    next           ;   /* node just after node i in the list of free nodes */
  int                    prev           ;   /* node just before node i in the list of free nodes */

  /* GET INFORMATION FROM TABLES */
  /*.............................*/
  synchro_table = pdata->synchro_table;

#if DBG > 1
  if (synchro_table == NULL) synchro_err(13);
#endif

  matching_table  = synchro_table->matching_table ;

#if DBG > 1
  if (matching_table == NULL) synchro_err(14);
#endif

  nb_resource_cur = synchro_table->nb_resource_cur;
  forest_is_valid = synchro_table->forest_is_valid;
  have_a_source   = synchro_table->have_a_source  ;
  hidden          = matching_table->hidden        ;
  rmate           = matching_table->rmate         ;
  ymate           = matching_table->ymate         ;
  mark            = matching_table->mark          ;
  first_free_var  = matching_table->first_free_var;
  next_free_var   = matching_table->next_free_var ;
  prev_free_var   = matching_table->prev_free_var ;

#if DBG > 1
  if (!((1 <= j) && (j <= nb_resource_cur))) synchro_err(15);
#endif

  if (hidden[j]) {                          /* if resource node j is not yet restored */
    hidden[j] = 0;                          /* mark node j as an unhidden node */
    if (forest_is_valid                     /* when forest is invalid nodes are implicitly */
	&&  (mark[j] == REACHEDBUTHIDDEN)) {    /* marked as UNREACHED */

      /* REMOVE EDGE i-j FROM MATCHING */
      /*...............................*/
      if (ymate[j] != 0) {                  /* if node j was belonging to the matching */
                                            /* (before it was hidden) */
        i               = ymate[j];         /* get index of assignment var. matched to value j */
        ymate[j]        = 0;
        rmate[i]        = 0;
      }
      forest_is_valid = 0;                  /* declare forest as invalid */
      have_a_source   = j;                  /* in next synchronize_parts_of_a_task will first */
                                            /* try to extend matching from j */
    } else  {                               /* if node j is marked UNREACHED or REACHED */
                                            /* forest_is_valid=0 OR mark[j]<>REACHEDBUTHIDDEN */
      if (ymate[j] != 0) {                  /* if node j was belonging to the matching */

        /* REMOVE NODE i FROM THE LIST OF FREE NODES */
        /*...........................................*/
                                            /* get index of assignment variable matched to value */
        i = ymate[j];                       /* resource node j */

#if DBG > 1
	{			/* node i belongs to the list of free nodes */
	  int kk; kk = first_free_var;
	  if (i == 0) synchro_err(16);
	  while (kk != i) {
	    if (kk == 0) synchro_err(17); kk = next_free_var[kk];
	  }
	}
#endif

        next = next_free_var[i];
        prev = prev_free_var[i];
        if (prev == 0) {
          first_free_var = next;
        } else {
          next_free_var[prev] = next;
        }
        if (next != 0) {
          prev_free_var[next] = prev;
        }

      }
    }
  }

  /* RECORD FIELDS WHICH MAY HAVE BEEN MODIFIED IN synchro_table AND matching_table */
  /*................................................................................*/
  synchro_table->forest_is_valid = forest_is_valid;
  synchro_table->have_a_source   = have_a_source  ;
  matching_table->first_free_var = first_free_var ;
}

#if DBG > 2
static void print_matching_state(
  struct disjoint2_data *pdata                  )
{
  struct synchro_frame  *synchro_table          ;
  int                    nb_resource_cur        ;
  int                    nb_part_cur            ;
  int                    result_of_previous_call;
  int                    forest_is_valid        ;
  int                    have_a_source          ;
  int                    have_a_root            ;
  struct matching_frame *matching_table         ;
  int                   *hidden                 ;
  int                   *rmate                  ;
  int                   *ymate                  ;
  int                   *mark                   ;
  int                   *father                 ;
  int                    i                      ;
  int                    j                      ;

  synchro_table           = pdata->synchro_table                  ;
  nb_resource_cur         = synchro_table->nb_resource_cur        ;
  nb_part_cur             = synchro_table->nb_part_cur            ;
  result_of_previous_call = synchro_table->result_of_previous_call;
  forest_is_valid         = synchro_table->forest_is_valid        ;
  have_a_source           = synchro_table->have_a_source          ;
  have_a_root             = synchro_table->have_a_root            ;
  matching_table          = synchro_table->matching_table         ;
  hidden                  = matching_table->hidden                ;
  rmate                   = matching_table->rmate                 ;
  ymate                   = matching_table->ymate                 ;
  mark                    = matching_table->mark                  ;
  father                  = matching_table->father                ;

  printf("previous=%d  forest=%d  source=%d  root=%d\n",
         result_of_previous_call,forest_is_valid,have_a_source,have_a_root);

  printf("rmate : ");
  for (i=1; i<=nb_part_cur; i++) {
    j = rmate[i];
	printf("%d ",j);
  }
  printf("\n");

  printf("ymate : ");
  for (i=1; i<=nb_resource_cur; i++) {
    j = ymate[i];
	printf("%d ",j);
  }
  printf("\n");

  printf("mark  : ");
  for (i=1; i<=nb_resource_cur; i++) {
    j = mark[i];
	printf("%d ",j);
  }
  printf("\n");

  printf("hidden: ");
  for (i=1; i<=nb_resource_cur; i++) {
    j = hidden[i];
	printf("%d ",j);
  }
  printf("\n");

  printf("father: ");
  for (i=1; i<=nb_resource_cur; i++) {
    j = father[i];
	printf("%d ",j);
  }
  printf("\n");
}
static int cpt_synchronize_parts_of_a_task = 0;
static int cpt_sweep_synchronization       = 0;
#endif

/*----------------------------------------------------------------------
-- Call the bipartite matching algorithm for task.  Return FALSE if a
-- contradiction was found, TRUE otherwise.
----------------------------------------------------------------------*/
static BOOL synchronize_parts_of_a_task(
  struct disjoint2_data *pdata            , /* pointer to rectangles frame */
  int                    task             ) /* index of the first rectangle of current task */
                                            /* return 0 if don't have any y-perfect matching */
                                            /* return 1 if have a y-perfect matching */
{
  struct synchro_frame  *synchro_table    ; /* pointer to synchronization frame */
                                            /* assignment dimension: dimension where all */
  int                    adim             ; /* the rectangles sizes are equal to 1 (0 or 1) */
                                            /* smallest value for the assignment variables */
  long                   min_resource_cur ; /* of the part of the task we currently handle */
  int                    nb_resource_cur  ; /* max_resource_cur - min_resource_cur + 1 */
  int                    nb_part_cur      ; /* number of parts of the current task */
  int                   *next_task        ; /* 0..nitems-1, index of the task next to a task */
  int                   *next_part        ; /* 0..nitems-1, index of the task next to a task part*/
                                            /* a hint for matching the assignment variables */
                                            /* 0..nitems-1, for a task for which the first two */
                                            /* parts are rectangles r1 and r2, hint[r1]=hint[r2] */
  int                   *hint             ; /* indicates that hint was not yet initialised */
                                            /* 1 if could find a y-perfect matching, */
  int              result_of_previous_call; /* 0 if could not, -1 if no previous call */
  int                    forest_is_valid  ; /* 1 if forest valid,0 if invalid or doesn't exist */
  int                    have_a_source    ; /* >0 if have a source for extending the matching, 0 */
  int                    have_a_root      ; /* >0 if have a root, else 0 */
  struct matching_frame *matching_table   ; /* pointer to the temporary frame related to matching*/
  int                   *assign           ; /* 1..nb_part_cur, index (0..nitems-1) of assignment */
                                            /* var.associated to ith part of a task */
  int                   *hidden           ; /* 1..nb_resource_cur, 0 if a resource node is hidden*/
                                            /* 1 if not */
  int                   *rmate            ; /* 1..nb_part_cur, resource associated to an */
                                            /* assignment variable */
  int                   *ymate            ; /* 1..nb_resource_cur, assignment variable associated*/
                                            /* to a resource */
  int                   *mark             ; /* 1..nb_resource_cur, status of a resource node */
  int                   *father           ; /* 1..nb_resource_cur, assignment var. from which */
                                            /* reach a given resource */
  int                    first_free_var   ; /* first not yet matched assignment variable */
  int                   *next_free_var    ; /* 1..nb_part_cur, next not yet matched */
                                            /* assignment variable */
  int                   *prev_free_var    ; /* 1..nb_part_cur, previous not yet matched */
                                            /* assignment variable */
  int                   *queue_of_nodes   ; /* 1..nb_part_cur, queue for the breath first search */
  BOOL                   res              ; /* result returned by synchronize_parts_of_a_task */
  int                    j                ; /* a resource node */
  int                    father_j         ; /* assignment var.from which we reach resource j */
  int                    pj               ; /* rmate[father_j] */
  int                    next             ; /* next_free_var[father_j] */
  int                    prev             ; /* prev_free_var[father_j] */
  int                    i                ; /* used for scanning part of tasks or resources */
  int                    use_hint         ; /* 1 if can use hint array, 0 else */
  int                    next_i           ; /* next_free_var[i] */
  int                    ri               ; /* index of rectangle associated to part i */
  int                    found_mate       ; /* 1 if find a mate, 0 otherwise */
  int                    f                ; /* a free node */
  int                 oldest_elem_in_queue; /* index of oldest elem.of queue of nodes to explore */
  int                 newest_elem_in_queue; /* index of newest elem.of queue of nodes to explore */
  int                    could_extend     ; /* 1 if could extend the matching, 0 otherwise */
  int                    last_j           ; /* maximum value of assignment variable assoc. to ri */
  int                    not_finish       ; /* 1 as long as did not finish to scan values of ri */
  int                    r                ; /* index of the rectangles of current task */
  BOOL                   skip_greedy      ; /* TRUE if have a source for extending the matching */
                                            /* FALSE otherwise */
                                            /* can use greedy (and hint) only if no partial */
                                            /* matching exists (otherwise may assign same value */
                                            /* twice */
#if DBG > 2
  cpt_synchronize_parts_of_a_task++;
  printf("cpt_synchronize_parts_of_a_task=%d\n",cpt_synchronize_parts_of_a_task);
#endif

  /* GET INFORMATION FROM TABLES */
  /*.............................*/
  synchro_table           = pdata->synchro_table                  ;

#if DBG > 1
  if (synchro_table == NULL) synchro_err(18);
#endif

  adim                    = synchro_table->adim                   ;
  min_resource_cur        = synchro_table->min_resource_cur       ;
  nb_resource_cur         = synchro_table->nb_resource_cur        ;
  nb_part_cur             = synchro_table->nb_part_cur            ;
  next_task               = synchro_table->next_task              ;
  next_part               = synchro_table->next_part              ;
  hint                    = synchro_table->hint                   ;
  result_of_previous_call = synchro_table->result_of_previous_call;
  forest_is_valid         = synchro_table->forest_is_valid        ;
  have_a_source           = synchro_table->have_a_source          ;
  have_a_root             = synchro_table->have_a_root            ;
  matching_table          = synchro_table->matching_table         ;

#if DBG > 1
  if (!(nb_part_cur  > 1)                         ) synchro_err(19);
  if (nb_part_cur    >  synchro_table->max_nb_part) synchro_err(20);
  if (matching_table == NULL                      ) synchro_err(21);
#endif

  assign                  = matching_table->assign                ;
  hidden                  = matching_table->hidden                ;
  rmate                   = matching_table->rmate                 ;
  ymate                   = matching_table->ymate                 ;
  mark                    = matching_table->mark                  ;
  father                  = matching_table->father                ;
  first_free_var          = matching_table->first_free_var        ;
  next_free_var           = matching_table->next_free_var         ;
  prev_free_var           = matching_table->prev_free_var         ;
  queue_of_nodes          = matching_table->queue_of_nodes        ;

  /* USE INFORMATION FROM PREVIOUS CALL TO TRY TO AVOID TO RUN AGAIN THE MATCHING ALGORITHM */
  /*........................................................................................*/
  skip_greedy = FALSE;                      /* by default don't skip greedy method */
                                            /* if previous call exists and did not find a */
  if (result_of_previous_call == 0) {       /* y-perfect matching */
    if (forest_is_valid) {                  /* if the forest is still valid */
                                            /* result_of_previous_call, forest_is_valid and */
                                            /* have_a_source remain unchanged */
                                            /* then know that we still don't have any y-perfect */
      res = FALSE; goto end_;               /* matching */
    } else {                                /* since invalid forest should have a source for */
      j = rmate[have_a_root];
      if (j==0 || hidden[j] != 0) {         /* check that root is still free ! */
        j = have_a_source;                  /* extending matching */
        skip_greedy = TRUE;                 /* will skip greedy algorithm that use hint since */
                                            /* may assign a value twice */

#if DBG > 1
      if (!((0        <  j) && (j       <= nb_resource_cur ))) synchro_err(22);
      if (!((ymate[j] == 0) && (mark[j] == REACHEDBUTHIDDEN))) synchro_err(23);
#endif

        /* EXTEND MATCHING BY USING FATHER INFORMATION */
        /*.............................................*/
        do {
          father_j = father[j];             /* assignment var.from which we reach resource j */
                                            /* resource from which arrive to assignment var. */
          pj = rmate[father_j];             /* (0 if root of tree) */
          rmate[father_j] = j;              /* "shift" the matching */
          ymate[j] = father_j;
          j = pj;                           /* continue until father_j is a free node: */
        } while ((j != 0)                   /* father_j is the root of the tree or previous mate */
             &&  (hidden[j] == 0));         /* of father_j is hidden */
        if (j != 0) {
          ymate[j] = 0;
        }

        /* REMOVE NODE father_j FROM THE LIST OF FREE NODES */
        /*..................................................*/

#if DBG > 1
	{			/* father_j belongs to the list of free nodes */
	  int kk; kk = first_free_var;
	  if (father_j == 0) synchro_err(24);
	  while (kk != father_j) {
	    if (kk == 0) synchro_err(25); kk = next_free_var[kk];
	  }
	}
#endif

        next = next_free_var[father_j];
        prev = prev_free_var[father_j];
        if (prev == 0) {
          first_free_var = next;
        } else {
          next_free_var[prev] = next;
        }
        if (next != 0) {
          prev_free_var[next] = prev;
        }
      }
      have_a_source = 0;                      /* we don't have a source any more */
      have_a_root   = 0;                      /* an no root either */
    }
                                              /* if first call or previous call did find a */
                                              /* y-perfect matching then have to reinitialize */
  } else {                                    /* the matching process */

    /* CREATE THE DOUBLE LINKED LIST OF FREE VARIABLES */
    /*.................................................*/
    first_free_var = 1;
    for (i=1; i<nb_part_cur; i++) {
      next_free_var[i] = i + 1;
      prev_free_var[i] = i - 1;
    }
    next_free_var[nb_part_cur] = 0              ;
    prev_free_var[nb_part_cur] = nb_part_cur - 1;

    /* INITIALISATION RELATED TO THE ASSIGNMENT VARIABLES */
    /*....................................................*/
    r = task;                               /* index of first rectangle of the task */
    for (i=1; i<=nb_part_cur; i++) {
      assign[i] = r           ;             /* record index of an assignment variable of the task*/
      rmate [i] = 0           ;             /* no mate is yet assigned to the ith variable */
      r         = next_part[r];
    }

    /* INITIALISATION RELATED TO THE RESOURCE NODES */
    /*..............................................*/
    for (i=1; i<=nb_resource_cur; i++) {
      ymate[i] = 0;                         /* resource node i has no mate */
      mark [i] = UNREACHED;                 /* resource node i is not yet reached */
                                            /* resource node i is not yet reached from any */
                                            /* assignment variable */
    }
    if (result_of_previous_call == -1) {    /* if no previous call */
      for (i=1; i<=nb_resource_cur; i++) {
        hidden[i] = 0;                      /* resource node i is not hidden */
      }
    }

  }

  /* USE A GREEDY HEURISTIC FOR COMPUTING A FIRST PART OF THE BIPARTITE MATCHING */
  /* (LINEAR IN NUMBER OF ASSIGNMENT VARIABLES) */
  /*.............................................................................*/
  if (skip_greedy == FALSE) {               /* if no variable is yet matched */
                                            /* will use hint array if it was initialised */
    use_hint = (hint[task] != hint[next_part[task]]);

#if DBG > 1
    {				/* values of hint array are pairwise different */
      int p1, p2;		/* for those assignment variables associated to */
      if (use_hint) {		/* the parts of task t */
	p1 = task;
	while (p1 != -1) {
	  p2 = next_task[p1];
	  while (p2 != -1) {
	    if (p1 == p2) synchro_err(26);
	    p2 = next_task[p2];
	  }
	  p1 = next_task[p1];
	}
      }
    }
#endif

    i = first_free_var;
    j = CLPFD_MAXINT;			    /* can never be a valid value */
    while (i) {                             /* scan the different free assignment variables */
      next_i = next_free_var[i];            /* since i may be removed from the list, take the */
                                            /* next i initially */
      ri = assign[i];                       /* get the assignment var. associated to current part*/
      found_mate = 1;                       /* by default assumes that will find a mate */
      if (use_hint) {                       /* if use previous y-perfect matching store in hint[]*/
        j = hint[ri] - min_resource_cur + 1;/* value provided by hint[ri] */
        if ((isindom_origin(pdata,ri,adim,hint[ri]) == 0) || (ymate[j] != 0)) {
                                            /* if value hint[ri] can't be taken by assignment */
          found_mate = 0;                   /* var. ri or if value allready used by an other */
                                            /* assignment var.then skip ith assignment variable */
        }
      } else {
        if (j == CLPFD_MAXINT) {          /* if first var, match it to its minimum value */
          j = mindom_origin(ri,adim) - min_resource_cur + 1;
        } else if ((j+min_resource_cur-1) < maxdom_origin(ri,adim)) {
                                            /* if not first var.and if j is not the maximum value*/
                                            /* get value j which is greater than the previous */
                                            /* used resource */
          j = nextval_origin(pdata,ri,adim,j+min_resource_cur-1) - min_resource_cur + 1;
        } else {                            /* if will skip ith assignment variable */
          found_mate = 0;
        }
      }
      if (found_mate
      &&  (hidden[j] == 0)) {               /* UPDATE MATES AND REMOVE NODE i FROM FREE NODES */
        if (rmate[i] != 0) {                /* if a resource was assigned to the assignment var.i*/
          ymate[rmate[i]] = 0;              /* this resource is no more assigned to var.i */
        }
        rmate[i] = j;                       /* mate of ith assignment variable is resource j */
        ymate[j] = i;                       /* mate of resource j is the ith assignment variable */

#if DBG > 1
	{			/* node i belongs to the list of free nodes */
	  int kk; kk = first_free_var;
	  if (i == 0) synchro_err(27);
	  while (kk != i) {
	    if (kk == 0) synchro_err(28); kk = next_free_var[kk];
	  }
	}
#endif

        next = next_free_var[i];            /* get free node that is located before node i */
        prev = prev_free_var[i];            /* get free node that is located after  node i */
        if (prev == 0) {
          first_free_var      = next;
        } else {
          next_free_var[prev] = next;
        }
        if (next != 0) {
          prev_free_var[next] = prev;
        }
      }
      i = next_i;                           /* index of next free variable to consider */
    }

#if DBG > 1
    {
      int k1, k2;
      for (k1=1; k1<=nb_part_cur; k1++) {       /* all entries<>0 of rmate are between 1 */
	if ((rmate[k1] != 0)	                /* and nb_resource_cur */
	    &&  (hidden[rmate[k1]] == 0)) {
	  if ((rmate[k1] < 1) || (rmate[k1] > nb_resource_cur)) synchro_err(29);
	  k2 = rmate[k1];
	  if (ymate[k2] != k1) synchro_err(30); /* rmate[k1] = k2<>0   =>   ymate[k2] = k1 */
	}
      }
      for (k1=1; k1<nb_part_cur; k1++) {        /* all entries<>0 of rmate are pairwise different */
	if ((rmate[k1] != 0) && (hidden[rmate[k1]] == 0)) {
	  for (k2=k1+1; k2<=nb_part_cur; k2++) {
	    if (rmate[k1] == rmate[k2]) synchro_err(31);
	  }
	}
      }
      for (k1=1; k1<=nb_resource_cur; k1++) {   /* all entries<>0 of ymate are between 1 */
	if (ymate[k1] != 0) {                   /* and nb_part_cur */
	  if ((ymate[k1] < 1) || (ymate[k1] > nb_part_cur)) synchro_err(32);
	  k2 = ymate[k1];
	  if (rmate[k2] != k1) synchro_err(33); /* ymate[k1] = k2<>0   =>   rmate[k2] = k1 */
	}
      }
      for (k1=1; k1<nb_resource_cur; k1++) {    /* all entries<>0 of ymate are pairwise different */
	if (ymate[k1] != 0) {
	  for (k2=k1+1; k2<=nb_resource_cur; k2++) {
	    if (ymate[k1] == ymate[k2]) synchro_err(34);
	  }
	}
      }
    }
#endif

    /* EXIT IF GREEDY HEURISTIC COULD FOUND A y-PERFECT MATCHING BY USING THE hint[] ARRAY */
    /* (NO NEED TO COPY THE y-PERFECT MATCHING TO hint[]) */
    /*.....................................................................................*/
    if ((first_free_var == 0) && use_hint) {
      result_of_previous_call = 1;          /* set to 1 to indicate to the next call that we */
                                            /* found a matching */
      forest_is_valid         = 0;          /* indicate that no forest at all */
      have_a_source           = 0;          /* indicate that we don't have a source */
      have_a_root             = 0;          /* indicate that we don't have a root */
      res = TRUE; goto end_;
    }
  }

  /* TRY TO MATCH THE REMAINING FREE NODES */
  /*.......................................*/
  while (first_free_var) {                  /* for all free nodes f on the Y-side do */
                                            /* (will exit the procedure if can't incorporate f */
                                            /* in the current matching) */
    for (i=1; i<=nb_resource_cur; i++) {
      mark[i] = UNREACHED;                  /* resource node i is not yet reached */
                                            /* resource node i is not yet reached from any */
                                            /* assignment variable */
    }
    f = first_free_var;                     /* get first free node (will be removed later if can */
                                            /* extend matching) */
    oldest_elem_in_queue = 1;               /* initialise the queue of the nodes that we still */
    newest_elem_in_queue = 1;               /* have to explore so that it only contains the root */
                                            /* f of the tree */
    queue_of_nodes[1] = f;                  /* insert the free node we try to incorporate in the */
                                            /* queue */
    could_extend = 0;                       /* by default assume that can't incorporate f in the */
                                            /* matching */
    while ((could_extend == 0)              /* stop search if could extend matching or if empty */
                                            /* queue */
       && oldest_elem_in_queue <= newest_elem_in_queue) {
                                            /* try to find an alternating path starting from f */
                                            /* and ending at a free resource */
                                            /* extract index of the next "assignment variable" */
                                            /* node from the queue */
      i = queue_of_nodes[oldest_elem_in_queue];
      oldest_elem_in_queue = oldest_elem_in_queue + 1;
      ri = assign[i];                       /* get index of corresponding rectangle */
                                            /* get minimum value of assignment variable ri */
      j = mindom_origin(ri,adim) - min_resource_cur + 1;
                                            /* get maximum value of assignment variable ri */
      last_j = maxdom_origin(ri,adim) - min_resource_cur + 1;
      not_finish = 1;                       /* will be reset to 0 when finish to scan of values */
                                            /* of adim origin of rectangle ri */
      do {                                  /* scan the <> values j (i.e successors) of node i */
        if (mark[j] == UNREACHED) {         /* if node j was not encountered before */
          father[j] = i;                    /* record the backward links in order to extend */
                                            /* matching later */
          if (hidden[j]) {                  /* if node j was hidden then mark it as */
            mark[j] = REACHEDBUTHIDDEN;     /* REACHEDBUTHIDDEN */
          } else {                          /* if node j is not hidden */
            if (ymate[j] != 0) {            /* if node j (i.e. resource j) is already used in */
                                            /* the matching */
              mark[j] = REACHED;            /* mark node j as REACHED and insert in the queue */
              newest_elem_in_queue++;       /* his mate */
              queue_of_nodes[newest_elem_in_queue] = ymate[j];
            } else {                        /* if node j (i.e. resource j) is not used in the */
              could_extend = 1;             /* matching then we have found an alternating path */
                                            /* from f to j */
              do {                          /* extend current matching by using father info. */
                father_j = father[j];       /* assignment var. from which we reach to resource j */
                pj = rmate[father_j];       /* resource from which arrive to assignment variable */
                                            /* (0 if root of tree) */
                rmate[father_j] = j;        /* "shift" the matching */
                ymate[j] = father_j;
                j = pj;
              } while ((j != 0) && (hidden[j] == 0));   /* continue to shift matching until handle root of */
                                            /* the tree */
              if (j != 0) {
                ymate[j] = 0;
              }
              next = next_free_var[f];      /* REMOVE NODE f FROM THE LIST OF FREE NODES */
              prev = prev_free_var[f];
              if (prev == 0) {
                first_free_var      = next;
              } else {
                next_free_var[prev] = next;
              }
              if (next != 0) {
                prev_free_var[next] = prev;
              }
            }
          }                                 /* if possible get the smallest value greater than j */
        }                                   /* and belonging to the assignment variable of ri */
        if (could_extend || (j == last_j)) {
          not_finish = 0;
        } else {
          j = nextval_origin(pdata,ri,adim,j+min_resource_cur-1) - min_resource_cur + 1;
        }
      } while (not_finish);
    }
                                            /* if could not find an alternating path starting */
    if (could_extend == 0) {                /* from f and ending to a "free" resource node */
      result_of_previous_call = 0;          /* tell the next call that could not find a */
                                            /* y-perfect matching */
      forest_is_valid         = 1;          /* tell that we have a valid forest */
                                            /* (proof that no matching) */
      have_a_source           = 0;          /* indicate that we don't have a source */
      have_a_root             = f;          /* indicate that root of our forest is f */
      res = FALSE; goto end_;               /* return FALSE for indicating that we could not */
    }                                       /* find a y-perfect matching */
  }

  /* SINCE WE FOUND A y-PERFECT MATCHING RECORD IT IN hint[] FOR THE NEXT CALL */
  /*...........................................................................*/
  for (i=1; i<=nb_part_cur; i++) {
    hint[assign[i]] = rmate[i] + min_resource_cur - 1;
  }
  result_of_previous_call = 1;              /* set to 1 to indicate to the next call that we */
                                            /* found a matching */
  forest_is_valid         = 0;              /* indicate that no forest at all */
  have_a_source           = 0;              /* indicate that we don't have a source */
  have_a_root             = 0;              /* indicate that we don't have a root */
  res = TRUE; goto end_;                    /* indicate that we could found a y-perfect matching */

 end_:
  /* RECORD GLOBAL INFORMATION WHICH HAVE MAY BE MODIFIED IN TABLES */
  /*................................................................*/
  synchro_table->result_of_previous_call = result_of_previous_call;
  synchro_table->forest_is_valid         = forest_is_valid        ;
  synchro_table->have_a_source           = have_a_source          ;
  synchro_table->have_a_root             = have_a_root            ;
  matching_table->first_free_var         = first_free_var         ;

  return res;
}

/*----------------------------------------------------------------------
-- Update the earliest start of a task according to the sweep
-- synchronization method.
----------------------------------------------------------------------*/
static BOOL sweep_synchronization MAGIC (HIDDEN_PROTO
  struct disjoint2_data *pdata          ,   /* pointer to rectangles frame */
  int                    task           ,   /* index of the first rectangle of current task */
  BOOL                  *change         )
{
  struct synchro_frame  *synchro_table  ;   /* pointer to synchronization frame */
  struct matching_frame *matching_table ;   /* pointer to the temporary frame related to matching*/
                                            /* assignment dimension: dimension where all */
  int                    adim           ;   /* the rectangles sizes are equal to 1 (0 or 1) */
  int                    tdim           ;   /* temporal dimension */
  long                   amin           ;   /* smallest assignment value for the parts of task */
  long                   amax           ;   /* largest  assignment value for the parts of task */
  int                    nb_resource_cur;   /* max_resource_cur - min_resource_cur + 1 */
  int                   *next_task      ;   /* 0..nitems-1, index of task next to a given task */
  int                   *next_part      ;   /* 0..nitems-1, index of the task next to a task part*/
  int                   *sort_event     ;   /* 0..nitems+nitems-1, index of events sorted in */
                                            /* increasing or decreasing order of their date */

                                            /* USED FOR STORING EVENTS FOR THE SWEEP FROM THE MIN*/
  int                    nevent1        ;   /* number of events when sweep from min to max */
  int                   *type_event1    ;   /* 0..nitems+nitems-1, 0 if start event, 1 if end */
  long                  *date_event1    ;   /* 0..nitems+nitems-1, date associated to the event */
  long                  *ress_event1    ;   /* 0..nitems+nitems-1, resource assoc. to the event */

                                            /* USED FOR STORING EVENTS FOR THE SWEEP FROM THE MAX*/
  int                    nevent2        ;   /* number of events when sweep from max to min */
  int                   *type_event2    ;   /* 0..nitems+nitems-1, 0 if start event, 1 if end */
  long                  *date_event2    ;   /* 0..nitems+nitems-1, date associated to the event */
  long                  *ress_event2    ;   /* 0..nitems+nitems-1, resource assoc. to the event */

  int                    dir            ;   /* sweep direction: 1 if from min to max, 2 otherwise*/
  BOOL                 exist_start_event;   /* dir=1: exist_start_event_at_tmin */
                                            /* dir=2: exist_start_event_at_tmax */
  int                    nevent         ;   /* dir=1: nevent1    ;  dir=2: nevent2 */
  long                  *date_event     ;   /* dir=1: date_event1;  dir=2: date_event2 */
  int                   *type_event     ;   /* dir=1: type_event1;  dir=2: type_event2 */
  long                  *ress_event     ;   /* dir=1: ress_event1;  dir=2: ress_event2 */
  int                   *count_forbid   ;   /* 1..nb_resource, count number of forbidden regions */
                                            /* which overlap a given resource for the current */
                                            /* position of the sweep-line */
  BOOL         exist_start_event_at_tmin;   /* TRUE if at least one start event at date tmin */
  BOOL         exist_start_event_at_tmax;   /* TRUE if at least one start event at date tmax */
  long                   tmin           ;   /* earliest start of task on the temporal axis */
  long                   tmax           ;   /* latest   start of task on the temporal axis */
  long                   dmin           ;   /* minimum duration of task */
  int                    t              ;   /* index of a task (used for scanning list of tasks) */
  long                   tforbid_min    ;   /* start of a forbidden region on the time axis */
  long                   tforbid_max    ;   /* end   of a forbidden region on the time axis */
  int                    p              ;   /* used for scanning the different parts of a task */
  long                   aforbid_min    ;   /* start of a forbidden region on the assignment axis*/
  long                   aforbid_max    ;   /* end   of a forbidden region on the assignment axis*/
  int                    e              ;   /* used for sorting events */
  BOOL                   found_sol      ;   /* TRUE if find feasible position for the sweep-line */
  BOOL                   hide_or_restore;   /* for the current position of the sweep-line catch */
                                            /* the fact that we hide or restore at least one node*/
  int                    k              ;   /* index of an event: used for scanning events */
  long                   d=0            ;   /* date associated to an event */
  int                    typ            ;   /* type associated to an event: 0 if start, 1 if end */
  long                   r              ;   /* assignment value associated to an event */
  long                   rshifted       ;   /* r - amin + 1 */
  int                  ka, kamin, kamax ;   /* loop index and bounds for ass. dim. for wrap */
  int                  kt, ktmin, ktmax ;   /* loop index and bounds for temp. dim. for wrap */
  long                   shift[2]       ;   /* shift amount for wrap */

#if DBG > 2
  cpt_sweep_synchronization++;
  printf("cpt_sweep_synchronization=%d\n",cpt_sweep_synchronization);
#endif

  /* CHECK IF THERE EXIST A MATCHING FOR THE ASSIGNMENT VAR */
  /*........................................................*/
  synchro_table = pdata->synchro_table;
  synchro_table->result_of_previous_call = -1; /* -1 since no previous call */
  synchro_table->forest_is_valid         =  0; /* forest does not exist */
  synchro_table->have_a_source           =  0; /* don't have a source */
  synchro_table->have_a_root             =  0; /* don't have a root */
  if (!synchronize_parts_of_a_task(pdata,task)) {
    return FALSE;
  }

  /* GET INFORMATION FROM TABLES */
  /*.............................*/
  adim            = synchro_table->adim            ;
  tdim            = synchro_table->tdim            ;
  amin            = synchro_table->min_resource_cur;
  amax            = synchro_table->max_resource_cur;
  nb_resource_cur = synchro_table->nb_resource_cur ;
  next_task       = synchro_table->next_task       ;
  next_part       = synchro_table->next_part       ;
  matching_table  = synchro_table->matching_table  ;
  sort_event      = matching_table->sort_event     ;
  type_event1     = matching_table->type_event1    ;
  date_event1     = matching_table->date_event1    ;
  ress_event1     = matching_table->ress_event1    ;
  type_event2     = matching_table->type_event2    ;
  date_event2     = matching_table->date_event2    ;
  ress_event2     = matching_table->ress_event2    ;
  count_forbid    = matching_table->count_forbid   ;
  if ((adim==0 && (pdata->flags&0x2)) ||
      (adim==1 && (pdata->flags&0x4)))
    kamin = -1;
  else
    kamin = 0;
  kamax = -kamin;
  if ((tdim==0 && (pdata->flags&0x2)) ||
      (tdim==1 && (pdata->flags&0x4)))
    ktmin = -1;
  else
    ktmin = 0;
  ktmax = -ktmin;
  for (k=0; k<2; k++)
    shift[k] = pdata->rborder[k]-pdata->lborder[k];

  /* GET THE FORBIDDEN REGIONS FOR THE PARTS OF task */
  /*.................................................*/
  exist_start_event_at_tmin = FALSE;
  exist_start_event_at_tmax = FALSE;
  nevent1 = 0;                              /* number of events for the sweep from min to max */
  nevent2 = 0;                              /* number of events for the sweep from max to min */
  tmin = mindom_origin(task,tdim);    /* earliest start of task on the temporal axis */
  tmax = maxdom_origin(task,tdim);    /* latest   start of task on the temporal axis */
  dmin = mindom_length(task,tdim);    /* minimum duration of task, >0 */
  for (kt=ktmin; kt<=ktmax; kt++) {
    t    = synchro_table->first_task;         /* first task of the list of tasks */
    while (t != -1) {                         /* scan the different tasks */
      long tdmin = mindom_length(t,tdim);
      
      if (t != task && tdmin>0) {             /* skip task for which generate forbidden regions */
	/*  the matching takes care that the different parts */
	/*  of a task are assigned to different resources */
	tforbid_min = maxdom_origin(t,tdim) - dmin + 1 + kt*shift[tdim];
	tforbid_max = mindom_origin(t,tdim) + tdmin - 1 + kt*shift[tdim];
	if (tforbid_min < tmin) {             /* shrink eventually the lower part on the temporal */
	  tforbid_min = tmin;                 /* axis of the forbidden region according to the */
	}                                     /* earliest start of the task we want to prune */
	if (tforbid_max > tmax) {             /* shrink eventually the upper part on the temporal */
	  tforbid_max = tmax;                 /* axis of the forbidden region according to the */
	}                                     /* latest start of the task we want to prune */
	if (tforbid_min <= tforbid_max) {     /* if forbidden region exists on the temporal axis */
	  for (ka=kamin; ka<=kamax; ka++) {
	    p = t;                              /* first part of task t */
	    do {                                /* scan the different parts of task t */
	      aforbid_min = maxdom_origin(p,adim) + ka*shift[adim]; /* mindom_length(task,amin)==1 */
	      aforbid_max = mindom_origin(p,adim) + ka*shift[adim]; /* mindom_length(p,amin)==1 */
	      if (aforbid_min < amin) {         /* shrink eventually the lower part on the assignment*/
		aforbid_min = amin;             /* axis of the forbidden region according to the */
	      }                                 /* smallest resource of the task we want to prune */
	      if (aforbid_max > amax) {         /* shrink eventually the upper part on the assignment*/
		aforbid_max = amax;             /* axis of the forbidden region according to the */
	      }                                 /* biggest resource of the task we want to prune */
	      if (aforbid_min <= aforbid_max) { /* if forbidden region exists on the assignment axis */

		/* RECORD EVENTS FOR SWEEP FROM MIN TO MAX */
		type_event1[nevent1] = 0;       /* record a new start event */
		date_event1[nevent1] = tforbid_min;
		ress_event1[nevent1] = aforbid_min;
		nevent1++;                      /* increment after since event index start at 0 */
		if (tforbid_min == tmin) {
		  exist_start_event_at_tmin = TRUE;
		}
		if (tforbid_max < tmax) {       /* if forbidden region finish before tmax */
		  type_event1[nevent1] = 1;     /* record a new end event */
		  date_event1[nevent1] = tforbid_max+1;
		  ress_event1[nevent1] = aforbid_min;
		  nevent1++;
		}                               /* RECORD EVENTS FOR SWEEP FROM MAX TO MIN */
		type_event2[nevent2] = 0;       /* record a new start event */
		date_event2[nevent2] = tforbid_max;
		ress_event2[nevent2] = aforbid_min;
		nevent2++;                      /* increment after since event index start at 0 */
		if (tforbid_max == tmax) {
		  exist_start_event_at_tmax = TRUE;
		}
		if (tforbid_min > tmin) {       /* if forbidden region start after tmin */
		  type_event2[nevent2] = 1;     /* record a new end event */
		  date_event2[nevent2] = tforbid_min-1;
		  ress_event2[nevent2] = aforbid_min;
		  nevent2++;
		}
	      }
	      p = next_part[p];                 /* get next part (if exists) of task t */
	    } while (p != -1);                  /* stop when on last part of task t */
	  }
	}
      }
      t = next_task[t];                       /* get next task */
    }
  }
  if (((nevent1+nevent2) == 0)              /* no sweep if no forbidden regions */
  ||  ((exist_start_event_at_tmin == FALSE) && (exist_start_event_at_tmax == FALSE))) {
    return TRUE;
  }

  exist_start_event = exist_start_event_at_tmin;
  nevent            = nevent1                  ;
  date_event        = date_event1              ;
  type_event        = type_event1              ;
  ress_event        = ress_event1              ;
  for (dir=1; dir<=2; dir++) {              /* dir=1:sweep from min to max; dir=2 from max to min*/

#if DBG > 2
  printf("DIRECTION IS %d\n",dir);
#endif

    if (exist_start_event) {                /* no sweep if no event at tmin(dir=1) or tmax(dir=2)*/

      /* SORT THE EVENTS IN INCREASING ORDER OF THEIR DATE */
      /*...................................................*/
      for (e=0; e<nevent; e++) {            /* build an array of event index for preparing */
        sort_event[e] = e;                  /* the sort on the temporal axis */
      }
      if (dir == 1) {
        qsort_events_increasing_order(sort_event,nevent);
      } else {
        qsort_events_decreasing_order(sort_event,nevent);
      }

      /* PERFORM THE SWEEP OVER THE DIFFERENT EVENTS UNTIL FIND A FIRST FEASIBLE POSITION */
      /*..................................................................................*/
      for (k=1; k<=nb_resource_cur; k++) {  /* initialize to empty the status of the sweep-line */
        count_forbid[k] = 0;
      }
      found_sol = FALSE;                    /* we did not yet found a feasible position */
      k = 0;                                /* index of first event to handle */
      while ((k < nevent)                   /* continue while we have event and we did not */
         &&  !found_sol ) {                 /* find a feasible position for the sweep line */
        e = sort_event[k];                  /* event associated to position k */
        d = date_event[e];                  /* date of the next event */

#if DBG > 2
		printf("DATE is %ld\n",d); print_matching_state(pdata);
#endif

        hide_or_restore = FALSE;            /* set to true if really hide or restore a node */
        while ((k < nevent)                 /* handle all events with date d */
           &&  (date_event[sort_event[k]] == d)) {
          e        = sort_event[k];         /* event associated to position k */
          typ      = type_event[e];         /* type of event e */
          r        = ress_event[e];         /* assignment value associated to event e */
          rshifted = r - amin + 1 ;
          k++;                              /* get read of kth event */
          if (typ == 0) {                   /* if e is a start event */
            count_forbid[rshifted] = count_forbid[rshifted] + 1;
            if (count_forbid[rshifted]==1) {/* if transition from 0 to 1 */
              hide_or_restore = TRUE;       /* then hide resource r */
              hide_resource_sweep_synchro(pdata,rshifted);

#if DBG > 2
              printf("AFTER HIDING NODE %ld\n",rshifted); print_matching_state(pdata);
#endif

            }
          } else {                          /* if e is an end event */
            count_forbid[rshifted] = count_forbid[rshifted] - 1;
            if (count_forbid[rshifted]==0) {/* if transition from 1 to 0 */
              hide_or_restore = TRUE;       /* then restore resource r */
              restore_resource_sweep_synchro(pdata,rshifted);

#if DBG > 2
              printf("AFTER RESTORING NODE %ld\n",rshifted); print_matching_state(pdata);
#endif

            }
          }
        }
                                            /* call the incremental matching algorithm */
        if (hide_or_restore) {              /* if did hide or restore some nodes */
          found_sol = synchronize_parts_of_a_task(pdata,task);

#if DBG > 2
          printf("AFTER MATCHING RESULT IS %d\n",found_sol); print_matching_state(pdata);
#endif

        }                                   /* that a maximum matching exists then force to exit */
      }
      if (found_sol) {                      /* if found a feasible position for the sweep-line */
        if (dir==1 && d>EST2D(task,tdim)) { /* if sweep from min to max */
	  *change = TRUE;
          if (!prune_item2d(task,d,LST2D(task,tdim),tdim)) {
            return FALSE;                   /* update the earliest start of task to it */
          }
        } else if (dir==2 && d<LST2D(task,tdim)) { /* if sweep from max to min */
	  *change = TRUE;
          if (!prune_item2d(task,EST2D(task,tdim),d,tdim)) {
            return FALSE;                   /* update the latest start of task to it */
          }
        }
      } else {                              /* if did a complete sweep without finding any */
       return FALSE;                        /* feasible solution then fail */
      }
    }
    if (dir == 1) {

#if DBG > 2
      printf("CHANGE DIRECTION TO SWEEP FROM MAX TO MIN\n");
#endif

      exist_start_event = exist_start_event_at_tmax;
      nevent            = nevent2                  ;
      date_event        = date_event2              ;
      type_event        = type_event2              ;
      ress_event        = ress_event2              ;
                                            /* call matching again since has to reinitialize */
                                            /* structures before sweeping from max to min */
      if (exist_start_event_at_tmin) {      /* else did nothing at iteration dir=1 */
        synchro_table->result_of_previous_call = -1;
        synchro_table->forest_is_valid         =  0;
        synchro_table->have_a_source           =  0;
        synchro_table->have_a_root             =  0;
        if (!synchronize_parts_of_a_task(pdata,task)) {
          return FALSE;
	}
      }
    }
  }
  return TRUE;
}

/*----------------------------------------------------------------------
-- Call the sweep synchronization algorithm for those tasks which have
-- more than one part.  Returns FALSE if a contradiction was found,
-- and TRUE otherwise.
----------------------------------------------------------------------*/
static BOOL wake_sweep_synchro MAGIC (HIDDEN_PROTO
  struct disjoint2_data *pdata            , /* pointer to rectangles frame */
  int                    down             , /* down=1 if go down, 0 if backtrack or if first */
                                            /* time we call wake_constraint */
  BOOL                  *change           )
                                            /* return TRUE  if did not find any contradiction */
                                            /* return FALSE if did find a contradiction */
{
  int                    nactive          ; /* total number of rectangles */
  struct synchro_frame  *synchro_table    ; /* pointer to synchronization frame */
  int                    adim             ; /* assignment dimension */
  int                    tdim             ; /* temporal dimension */
  int                   *ori_depth        ; /* 0..nitems-1, depth of temporal origin variables */
  int                   *len_depth        ; /* 0..nitems-1, depth of temporal lenght variables */
  int                   *sort_task        ; /* 0..nitems-1, index of rectangles sorted on their */
                                            /* temporal origin and temporal size */
  int                    first_task       ; /* index of first task of the list of tasks */
  int                   *next_task        ; /* 0..nitems-1, index of task next to a given task */
  int                   *last_part        ; /* 0..nitems-1, index of last part of a given task */
  int                   *next_part        ; /* 0..nitems-1, index of task next to a task part */
  int                   *hint             ; /* a hint for matching the assignment variables */
                                            /* 0..nitems-1, for a task for which the first two */
                                            /* parts are rectangles r1 and r2, hint[r1]=hint[r2] */
                                            /* indicates that hint was not yet initialised */
  struct matching_frame *matching_table   ; /* pointer to the temporary frame related to matching*/
  int                    r                ; /* used for scanning rectangles */
  long                   min_var          ; /* minimum possible value for an assignment variable */
  long                   max_var          ; /* maximum possible value for an assignment variable */
  long                   min              ; /* smallest value for the assignment variables */
  long                   max              ; /* largest value for the assignment variables */
  int                    nb_resource      ; /* max_resource - min_resource + 1 */
  int                    unification      ; /* used for catching the fact that unification occurs*/
  int                    t                ; /* used for scanning the list of tasks */
  int                    ori_d            ; /* depth of temporal origin of current task t */
  int                    len_d            ; /* depth of temporal length of current task t */
  int                    prev             ; /* index of previous task */
  int                    cour             ; /* index of current task */
  int                    next             ; /* index of next task */
  int                    keep             ; /* indicate which variable to keep after a unif. */
  int                    max_nb_part      ; /* maximum number of part of a task */
  int                    nb_part          ; /* number of part of a task */
  int                 size_int_max_nb_part; /* required space for an array */
  int                 size_int_nb_resource; /* required space for an array */
  int                   *assign           ; /* 1..max_nb_part, index (0..nitems-1) of assignment */
                                            /* var.associated to ith part of a task */
  int                   *hidden           ; /* 1..nb_resource, 0 if a resource node is hidden, */
                                            /* 1 if not */
  int                   *rmate            ; /* 1..max_nb_part, resource associated to an */
                                            /* assignment variable */
  int                   *ymate            ; /* 1..nb_resource, assignment variable associated to */
                                            /* a resource */
  int                   *mark             ; /* 1..nb_resource, status of a resource node */
  int                   *father           ; /* 1..nb_resource, assignment var.from which reach a */
                                            /* given resource */
  int                   *next_free_var    ; /* 1..max_nb_part, next not yet matched assignment */
                                            /* variable */
  int                   *prev_free_var    ; /* 1..max_nb_part, previous not yet matched */
                                            /* assignment variable */
  int                   *queue_of_nodes   ; /* 1..max_nb_part, the queue itself */
  int                    size_event_int   ; /* size for allocating tables storing the events */
  int                    size_event_long  ; /* size for allocating tables storing the events */
  int                   *sort_event       ; /* 0..nitems+nitems-1, index of events sorted in */
                                            /* increasing order of their date */
                                            /* used when sweep from min to max */
  int                   *type_event1      ; /* 0..nitems+nitems-1, 0 if start event, 1 if end */
  long                  *date_event1      ; /* 0..nitems+nitems-1, date associated to the event */
  long                  *ress_event1      ; /* 0..nitems+nitems-1, resource assoc. to the event */
                                            /* used when sweep from max to min */
  int                   *type_event2      ; /* 0..nitems+nitems-1, 0 if start event, 1 if end */
  long                  *date_event2      ; /* 0..nitems+nitems-1, date associated to the event */
  long                  *ress_event2      ; /* 0..nitems+nitems-1, resource assoc. to the event */
  int                   *count_forbid     ; /* 1..nb_resource, count number of forbidden regions */
                                            /* which overlap a given resource for the current */
                                            /* position of the sweep-line */
  int                    evfac            ; /* 1 or 3 or 9, depending on wrap-around */
  BOOL                   result           ; /* result returned by the procedure */

  /* GET INFORMATION FROM pdata */
  /*............................*/
  synchro_table = pdata->synchro_table;     /* pointer to synchronization frame */

  /* GET INFORMATION FROM synchro_table */
  /*....................................*/
  nactive    = synchro_table->nactive   ;   /* number of active rectangles */
  adim       = synchro_table->adim      ;   /* assignment dimension */
  tdim       = synchro_table->tdim      ;   /* temporal dimension */
  ori_depth  = synchro_table->ori_depth ;   /* table of depth of temporal origin variables */
  len_depth  = synchro_table->len_depth ;   /* table of depth of temporal length variables */
  sort_task  = synchro_table->sort_task ;   /* table for sorting the tasks */
  first_task = synchro_table->first_task;   /* if down=1 then index of first task else nothing */
  next_task  = synchro_table->next_task ;   /* table of next task index */
  last_part  = synchro_table->last_part ;   /* table of last part of a task */
  next_part  = synchro_table->next_part ;   /* table of next part of a task */
  hint       = synchro_table->hint      ;   /* table of hint for the assignment variables */

  /* CHECK IF A UNIFICATION DID OCCUR SINCE LAST TIME WE WOKE THE CONSTRAINT */
  /*.........................................................................*/
  if (down) {                               /* if go down */
    unification = 0;
    t = first_task;                         /* scan tasks in order to find if unification did */
    while (t != -1) {                       /* occur or not */
      ori_d = ori_unification_depth(pdata,t,tdim);
      len_d = len_unification_depth(pdata,t,tdim);
      if ((ori_depth[t] != ori_d)           /* find that a unif.occurs for the origin of task t */
      ||  (len_depth[t] != len_d)) {        /* find that a unif.occurs for the duration of task t*/
        unification = 1;
      }
      ori_depth[t] = ori_d;                 /* when go down record current depth if there was */
      len_depth[t] = len_d;                 /* some change in the depth */
      t = next_task[t];                     /* get next task */
    }
  } else {                                  /* when backtrack assumes that a unif.did occurs */
    nactive = pdata->ntargets+pdata->nsources; /* total number of rectangles */
    unification = 1;                        /* has to resort the tasks */
    for (t=0; t<nactive; t++) {
      r = TARGET(t);
      ori_depth[r] = 0;                     /* reset depth of temporal origins to 0 */
      len_depth[r] = 0;                     /* reset depth of temporal lengths to 0 */
    }                                       /* reinitialize next three fields when backtrack */
                                            /* since if previous call did not find a matching */
                                            /* this does not means that will not find a matching */
    synchro_table->result_of_previous_call = -1; /* -1 since no previous call */
    synchro_table->forest_is_valid         =  0; /* forest does not exist */
    synchro_table->have_a_source           =  0; /* don't have a source */
    synchro_table->have_a_root             =  0; /* don't have a root */
    synchro_table->nactive                 =  nactive;
  }

  /* UPDATE THE FIXED INFORMATION RELATED TO ASSIGNMENT VARIABLES */
  /*..............................................................*/
  min =  CLPFD_MAXINT;
  max = -CLPFD_MAXINT;
  for (t=0; t<nactive; t++) {                /* scan all the rectangles */
    r = TARGET(t);
    min_var=mindom_origin(r,adim);
    max_var=maxdom_origin(r,adim);
    if (min_var < min) min = min_var;
    if (max_var > max) max = max_var;
  }
  nb_resource = max - min + 1;
  synchro_table->min_resource = min;        /* record smallest value for the assignment variables*/
  synchro_table->max_resource = max;        /* record largest value for the assignment variables */
  synchro_table->nb_resource  = nb_resource;/* record max_resource-min_resource+1 */
 
  /* REBUILD THE LIST OF TASKS IF AN UNIFICATION DID OCCUR SINCE TASKS MAY HAVE TO MERGE */
  /*.....................................................................................*/
  if (unification) {
    for (t=0; t<nactive; t++) {             /* build an array of rectangle index for preparing */
      sort_task[t] = TARGET(t);             /* the sort on the temporal origin and temporal size */
    }
    qsort_rectangles(sort_task,nactive);

                                            /* BUILD A LIST OF SORTED RECTANGLES */
    first_task = sort_task[0];              /* record index of first rectangle of the list */
    for (t=0; t<nactive; t++) {              /* scan the sorted array in rectangle index */
      r = sort_task[t];                     /* get index of current rectangle */
      if (t < (nactive-1)) {                 /* if not on the last entry */
        next_task[r] = sort_task[t+1];      /* then the next rectangle is at the next entry */
      } else {                              /* if on the last entry */
        next_task[r] = -1;                  /* then terminate the list */
      }
      last_part[r] =  r;                    /* first part of the task is the task itself */
      next_part[r] = -1;                    /* the task has only one single part */
      hint[r] = 0;                          /* reinitialize hint to empty */
    }

                                            /* MERGE ADJACENT RECTANGLES WHICH BELONG TO THE */
                                            /* SAME TASK */
    prev = -1;                              /* index of previous task (no previous task) */
    cour = first_task;                      /* index of current task */
    while ((cour            != -1)          /* continue to scan while not on the */
       &&  (next_task[cour] != -1)) {       /* last element of the list of tasks */
      next = next_task[cour];               /* get next task of the list */
                                            /* if the two task origins were unified */
                                            /* as well as their durations */
      if (same_origins(pdata,cour,next,tdim,&keep)
      &&  same_lengths(pdata,cour,next,tdim,&keep)) {
        if (keep == 1) {                    /* if should keep cour as the representative */
          next_task[cour] = next_task[next];/* skip over task next */
          next_part[last_part[cour]] = next;/* add parts of next after last part of cour */
          last_part[cour] = last_part[next];/* last part of cour is now last part of next */
                                            /* prev and cour are kept unchanged */
        } else {                            /* if should keep next as the representative */
                                            /* remove cour from the list of tasks */
          if (prev != -1) {                 /* if cour has a predecessor prev */
            next_task[prev] = next;         /* then successor of prev is next */
          } else {                          /* if cour has no predecessor */
            first_task = next;              /* then first task of the list is task next */
          }
          next_part[last_part[next]] = cour;/* add parts of cour after last part of next */
          last_part[next] = last_part[cour];/* last part of next if now last part of cour */
          cour = next;                      /* cour is set to next and prev is kept unchanged */
        }
      } else {                              /* if the two tasks origins are not unified */
        prev = cour;                        /* then move one task ahead */
        cour = next;
      }
    }
  }
  synchro_table->first_task = first_task;   /* record new head of list of tasks */

  /* COMPUTE MAXIMUM NUMBER OF PART OF A TASK AND RECORD IT IN synchro_table */
  /*.........................................................................*/
  max_nb_part = 0;                          /* maximum number of part of a task */
  cour = first_task;                        /* get index of first task of list of tasks */
  while (cour != -1) {                      /* scan the different tasks */
    next = next_task[cour];                 /* record index of next task */
    nb_part = 0;                            /* will count number of part of current task */
    do {                                    /* scan the parts of current task */
      nb_part++;                            /* count one more part */
      cour = next_part[cour];               /* get index of next part */
    } while (cour != -1);
    if (nb_part > max_nb_part) {            /* if current task has a larger number of part */
      max_nb_part = nb_part;                /* then record it */
    }
    cour = next;                            /* get index of next task */
  }
  synchro_table->max_nb_part = max_nb_part; /* record new head of list of tasks */

#if DBG > 1
  /* INVARIANT ON THE LIST OF TASKS */
  /*  . THE INDEX OF A TASK IS BETWEEN 1 AND nitems */
  /*  . THE ORIGINS AND DURATIONS OF THE DIFFERENT PARTS OF A TAKS ARE UNIFIED */
  /*  . THE TOTAL NUMBER OF PARTS IS EQUAL TO nactive */
  /*.................................................*/
  {
    int n, pp, p;
    n = 0;
    t = first_task;
    while (t != -1) {
      if (!((0 <= t) && (t < pdata->nitems))) synchro_err(1);
      pp = -1;
      p  =  t;
      do {
        if (pp != -1) {
          if (!same_origins(pdata,pp,p,tdim,&keep)) synchro_err(2);
          if (!same_lengths(pdata,pp,p,tdim,&keep)) synchro_err(3);
        }
        n++;
        if (!(n <= nactive)) synchro_err(4);
        p = next_part[p];
      } while (p != -1);
      t = next_task[t];
    }
    if (!(n == nactive)) synchro_err(5);
  }
#endif

#if DBG > 1
  /* . ALL THE TASKS INDEX ARE PAIRWISE DIFFERENT */
  /* . THE ORIGINS OR THE DURATIONS OF THE TASKS SHOULD NOT BE UNIFIED */
  /*...................................................................*/
  {
    int t1, t2;
    t1 = first_task;
    while (t1 != -1) {
      t2 = next_task[t1];
      if (t1 == t2) synchro_err(6);
      while (t2 != -1) {
        if (same_origins(pdata,t1,t2,tdim,&keep)
	    &&  same_lengths(pdata,t1,t2,tdim,&keep)
	    &&  (mindom_origin(pdata,t1,tdim)!=maxdom_origin(pdata,t1,tdim)))
	  synchro_err(7);
        t2 = next_task[t2];
      }
      t1 = next_task[t1];
    }
  }
#endif
 
  /* ALLOCATE TEMPORARY FRAME FOR STORING INFORMATION RELATED TO MATCHING */
  /*......................................................................*/
                                            /* allocate space for temporary frame and record it */
  matching_table = SP_malloc(sizeof(struct matching_frame));
  synchro_table->matching_table = matching_table;
                                            /* compute size of arrays to allocate */
  size_int_max_nb_part  = sizeof(int ) * max_nb_part;
  size_int_nb_resource  = sizeof(int ) * nb_resource;
  evfac = 1;
  if (pdata->flags&0x2) evfac *= 3;
  if (pdata->flags&0x4) evfac *= 3;
  size_event_int        = evfac * sizeof(int ) * (nactive + nactive - 1);
  size_event_long       = evfac * sizeof(long) * (nactive + nactive - 1);
                                            /* allocate space for the different arrays */
                                            /* decrement address since first entry has index 1 */
  assign         = SP_malloc(size_int_max_nb_part); assign--        ;
  hidden         = SP_malloc(size_int_nb_resource); hidden--        ;
  rmate          = SP_malloc(size_int_max_nb_part); rmate--         ;
  ymate          = SP_malloc(size_int_nb_resource); ymate--         ;
  mark           = SP_malloc(size_int_nb_resource); mark--          ;
  father         = SP_malloc(size_int_nb_resource); father--        ;
  next_free_var  = SP_malloc(size_int_max_nb_part); next_free_var-- ;
  prev_free_var  = SP_malloc(size_int_max_nb_part); prev_free_var-- ;
  queue_of_nodes = SP_malloc(size_int_max_nb_part); queue_of_nodes--;
  sort_event     = SP_malloc(size_event_int      );
  type_event1    = SP_malloc(size_event_int      );
  date_event1    = SP_malloc(size_event_long     );
  ress_event1    = SP_malloc(size_event_long     );
  type_event2    = SP_malloc(size_event_int      );
  date_event2    = SP_malloc(size_event_long     );
  ress_event2    = SP_malloc(size_event_long     );
  count_forbid   = SP_malloc(size_int_nb_resource); count_forbid--  ;

                                            /* record address of allocated arrays */
  matching_table->assign         = assign        ;
  matching_table->hidden         = hidden        ;
  matching_table->rmate          = rmate         ;
  matching_table->ymate          = ymate         ;
  matching_table->mark           = mark          ;
  matching_table->father         = father        ;
  matching_table->next_free_var  = next_free_var ;
  matching_table->prev_free_var  = prev_free_var ;
  matching_table->queue_of_nodes = queue_of_nodes;
  matching_table->sort_event     = sort_event    ;
  matching_table->type_event1    = type_event1   ;
  matching_table->date_event1    = date_event1   ;
  matching_table->ress_event1    = ress_event1   ;
  matching_table->type_event2    = type_event2   ;
  matching_table->date_event2    = date_event2   ;
  matching_table->ress_event2    = ress_event2   ;
  matching_table->count_forbid   = count_forbid  ;

  /* CALL THE SWEEP SYNCHRONIZATION ALGORITHM FOR EACH TASK WHICH CONTAINS MORE THAN ONE PART */
  /*..........................................................................................*/
  result = TRUE;                            /* by default no contradiction will be found */
  t = first_task;
  while (result                             /* while no contradiction was found */
	  &&  (t != -1)) {                  /* and not at the end of the list of tasks */
    if ((next_part[t] !=-1) &&	            /* if task t consists of more than one rectangle */
	ISTARGET(t) &&		            /* and task t is subject to pruning */
	(mindom_length(t,tdim) > 0)) {      /* and min. duration of t is greater than 0 */
      nb_part = 0;                          /* compute number of parts of task t as well as */
      r = t;                                /* the smallest and largest possible values for */
      min =  CLPFD_MAXINT;
      max = -CLPFD_MAXINT;
      do {                                  /* one of its assignment variables */
        min_var = mindom_origin(r,adim);
        max_var = maxdom_origin(r,adim);
        if (min_var < min) min = min_var;
        if (max_var > max) max = max_var;
        nb_part++;
        r = next_part[r];                   /* get index of next rectangle or -1 if on last rect.*/
      } while (r != -1);                    /* update frame for synchronize_parts_of_a_task */
      synchro_table->min_resource_cur        = min          ;
      synchro_table->max_resource_cur        = max          ;
      synchro_table->nb_resource_cur         = max - min + 1;
      synchro_table->nb_part_cur             = nb_part      ;
      if (!sweep_synchronization(pdata,t,change)) {
        result = FALSE;
        goto end_;
      }
    }
    t = next_task[t];                       /* get index of next task */
  }

  /* DEALLOCATE TEMPORARY ARRAYS AND MATCHING TABLE (increment add.since first entry has index 1)*/
  /*.............................................................................................*/
end_:
  assign++        ; SP_free(assign        );
  hidden++        ; SP_free(hidden        );
  rmate++         ; SP_free(rmate         );
  ymate++         ; SP_free(ymate         );
  mark++          ; SP_free(mark          );
  father++        ; SP_free(father        );
  next_free_var++ ; SP_free(next_free_var );
  prev_free_var++ ; SP_free(prev_free_var );
  queue_of_nodes++; SP_free(queue_of_nodes);
                    SP_free(sort_event    );
                    SP_free(type_event1   );
                    SP_free(date_event1   );
                    SP_free(ress_event1   );
                    SP_free(type_event2   );
                    SP_free(date_event2   );
                    SP_free(ress_event2   );
  count_forbid++  ; SP_free(count_forbid  );
                    SP_free(matching_table);
  synchro_table->matching_table = NULL;     /* reset pointer to matching table to NULL */
  return result;
}

/*----------------------------------------------------------------------
-- CODE FOR THE BASIC RECTANGLE NON-OVERLAPING CONSTRAINT.
----------------------------------------------------------------------*/

static BOOL prune_before2d MAGIC (HIDDEN_PROTO
				  ITEM t1, ITEM t2, int k)
{
  struct disjoint2_data *pdata = fd.gdata;
  long dur1 = DUR2D(t1,k);
  long est1 = EST2D(t1,k);
  long lst2 = LST2D(t2,k);

  if (ECT2D(t1,1-k)<=LST2D(t2,1-k) || ECT2D(t2,1-k)<=LST2D(t1,1-k) ||
      /* then they need not overlap in the 1-k dimension */
      LCT2D(t1,k)<=EST2D(t2,k)
      /* no pruning possible */
      )
    return TRUE;
  /* adjust bounds if possible */
  return (prune_item2d(t1, est1, lst2-dur1, k) &&
	  prune_item2d(t2, est1+dur1, lst2, k));
}



#if !NEW_GLOBAL

/* compute total area of all tasks\{l} overlapping [est,lct) */
static long total_area2d MAGIC (HIDDEN_PROTO
				ITEM *arr,
				int n,
				long *est, TAGGED *lct,
				TAGGED l)
{
  struct disjoint2_data *pdata = fd.gdata;
  long total = 0;
  int i;

  for (i=0; i<n; i++) {
    ITEM s = arr[i];
    long r0, r1;
    
    if (s!=l) {
      r0 = min_overlap_interval(EST2D(s,0),LCT2D(s,0),DUR2D(s,0),est[0],lct[0]);
      if (r0>0) {
	r1 = min_overlap_interval(EST2D(s,1),LCT2D(s,1),DUR2D(s,1),est[1],lct[1]);
	if (r1>0)
	  total += r0*r1;
      }
    }
  }
  return total;
}


static void bounding_box MAGIC (HIDDEN_PROTO
				ITEM s,
				long *ests, TAGGED *lcts, TAGGED lb, TAGGED ub,
				int dim)
{
  struct disjoint2_data *pdata = fd.gdata;
  ests[1-dim] = CLPFD_MAXINT;
  lcts[1-dim] = -CLPFD_MAXINT;
  
  while (s>TERMIN && ests[1-dim]>lb && lcts[1-dim]<ub) {
    if (ests[1-dim] > EST2D(s,1-dim)) ests[1-dim] = EST2D(s,1-dim);
    if (lcts[1-dim] < LCT2D(s,1-dim)) lcts[1-dim] = LCT2D(s,1-dim);
    s = NEXT(s);
  }
}



/* Lean edge finding a la Martin/Shmoys/Wuertz,
   generalized to the cumulative case.
   Comparing tasks T with Si such that lct(T) > lct(Si) = seed,
   where seed is a unique LCT of the tasks.
*/
static BOOL edge_finding2d_up MAGIC (HIDDEN_PROTO
				     BOOL *change,
				     int dim)
{
  struct disjoint2_data *pdata = fd.gdata;
  BOOL sorted = FALSE;
  ITEM *arr = pdata->sortarr;
  int n = pdata->ntargets + pdata->nsources;
  long *seed = (long *)(arr + pdata->nitems);
  int i, j;

  for (i=0; i<n; i++)
    seed[i] = LCT2D(arr[i],dim);
  qsort_asc_long(seed, n);

  /* the greatest seed is no good */
  for (j=n-2; j>=0 && seed[j]==seed[j+1]; j--)
    ;
  while (j>=0)
    {
      long lcts[2];
      long ests[2];
      long area = 0;
      ITEM s = TERMIN;		/* the chain of subsets */
      ITEM s1;
      ITEM l;			/* the items, by descending area */
      
      lcts[dim] = seed[j];
      if (!sorted) {
	sorted = TRUE;
	if (dim==0)
	  qsort_desc_est2d0(arr, n);
	else
	  qsort_desc_est2d1(arr, n);
      }
      ests[1-dim] = CLPFD_MAXINT;
      lcts[1-dim] = -CLPFD_MAXINT;
      for (i=0; i<n; i++) {
	ITEM t1 = arr[i];
	  
	if (ests[1-dim] > EST2D(t1,1-dim)) ests[1-dim] = EST2D(t1,1-dim);
	if (lcts[1-dim] < LCT2D(t1,1-dim)) lcts[1-dim] = LCT2D(t1,1-dim);
      }

      for (i=0; i<n; i++) {
	ITEM t1 = arr[i];
	
	if (LCT2D(t1,dim) <= lcts[dim]) {
	  area += DUR2D(t1,0)*DUR2D(t1,1);
	  NEXT(t1) = s, s = t1;
	  /* HERE: can enforce upper bounds on t1->mindur, t1->sup */
	}
      }
      l = pdata->items_by_area;
      while (s>TERMIN && l>TERMIN && DUR2D(l,0)>0 && DUR2D(l,1)>0) {
	long lminy = EST2D(l,1-dim);
	long lmaxy = LCT2D(l,1-dim);
	long min_ov_y = min_overlap_interval(lminy,lmaxy,DUR2D(l,1-dim),
					     ests[1-dim],lcts[1-dim]);

	if (  !(STATUS(l)&STATUS_TARGET)
	   || min_ov_y==0
	   || EST2D(l,dim)>=lcts[dim]
	   || LCT2D(l,dim)<=lcts[dim]) {
	 l = NEXT_AREA(l);
	} else {
	  long larea = DUR2D(l,0)*min_ov_y;
	  long slack1 = larea;
          long slack2;
	  long lb[2], ub[2];
	  
	  ests[dim] = EST2D(s,dim);
	  /* compute the smallest of the following two slacks:
	     - total minimal overlap of all items wrt. bounding box of S
	     - total minimal overlap of all items
	       wrt. (bounding box of S) /\ (bb of l extended along dim)
*/
	  lb[dim] = ests[dim];
	  ub[dim] = lcts[dim];
	  lb[1-dim] = ests[1-dim];
	  ub[1-dim] = lcts[1-dim];
	  if (lb[1-dim] < lminy) lb[1-dim] = lminy;
	  if (ub[1-dim] > lmaxy) ub[1-dim] = lmaxy;
	  if (lb[1-dim]<ub[1-dim])
	    slack1 = (ub[0]-lb[0])*(ub[1]-lb[1]) -
	             total_area2d(arr,n,l,lb,ub);
	  if (lb[1-dim] != ests[1-dim] || ub[1-dim] != lcts[1-dim]) {
	    slack2 = (lcts[0]-ests[0])*(lcts[1]-ests[1]) -
	             total_area2d(arr,n,l,ests,lcts);
	    if (slack1>slack2) slack1 = slack2;
	  }
	  slack2 = (lcts[dim]-EST2D(l,dim))*(lcts[1-dim]-ests[1-dim]) - area;
	  if (slack1>=larea) {
	      if (slack2>=larea) /* may be inside and first */
		l = NEXT_AREA(l);
	      do {
		area -= DUR2D(s,0)*DUR2D(s,1);
		s = NEXT(s);
	      } while (s>TERMIN && EST2D(s,dim)==ests[dim]);
	      bounding_box(s,ests,lcts,ests[1-dim],lcts[1-dim],dim); /* refresh */
	  } else {
	    long min_ov_x = min_overlap_interval(EST2D(l,dim),LCT2D(l,dim),DUR2D(l,dim),ests[dim],lcts[dim]);
	    long max_ov_x = max_overlap_interval(EST2D(l,dim),LCT2D(l,dim),DUR2D(l,dim),ests[dim],lcts[dim]);
	    
	    if (min_ov_x*min_ov_y > slack1)
	      return FALSE;
	    else if (max_ov_x*min_ov_y > slack1) { /* must stick out at either end */
	      long delta = FLOORDIV(slack1,min_ov_y);
	      long ub = ests[dim]+delta-DUR2D(l,dim);
	      long lb = lcts[dim]-delta;
		  
	      if (EST2D(l,dim) > ub) { /* must stick out to the right */
		if (!prune_item2d(l, lb, LST2D(l,dim), dim))
		  return FALSE;
		*change = TRUE;
		sorted = FALSE;
		for (s1=s; s1>TERMIN; s1=NEXT(s1))	/* added 001115 */
		  if (!prune_before2d(s1,l,dim))
		    return FALSE;
	      }
	    }
	    l = NEXT_AREA(l);
	  }
	}
      }
      while (j>=0 && seed[j]==lcts[dim])
	j--;
    }
  return TRUE;
}


/* Lean edge finding a la Martin/Shmoys/Wuertz,
   generalized to the cumulative case.
   Comparing items T with Si such that est(T) < est(Si) = seed,
   where seed is a unique EST of the items.
*/
static BOOL edge_finding2d_down MAGIC (HIDDEN_PROTO
				       BOOL *change,
				       int dim)
{
  struct disjoint2_data *pdata = fd.gdata;
  BOOL sorted = FALSE;
  ITEM *arr = pdata->sortarr;
  int n = pdata->ntargets + pdata->nsources;
  long *seed = (long *)(arr + pdata->nitems);
  int i, j;

  for (i=0; i<n; i++)
    seed[i] = EST2D(arr[i],dim);
  qsort_asc_long(seed, n);

  /* the smallest seed is no good */
  for (j=1; j<n && seed[j]==seed[j-1]; j++)
    ;
  while (j<n)
    {
      long lcts[2];
      long ests[2];
      long area = 0;
      ITEM s = TERMIN;		/* the chain of subsets */
      ITEM s1;
      ITEM l;			/* the items, by descending area */
      
      ests[dim] = seed[j];
      if (!sorted) {
	sorted = TRUE;
	if (dim==0)
	  qsort_asc_lct2d0(arr, n);
	else
	  qsort_asc_lct2d1(arr, n);
      }
      ests[1-dim] = CLPFD_MAXINT;
      lcts[1-dim] = -CLPFD_MAXINT;
      for (i=0; i<n; i++) {
	ITEM t1 = arr[i];
	  
	if (ests[1-dim] > EST2D(t1,1-dim)) ests[1-dim] = EST2D(t1,1-dim);
	if (lcts[1-dim] < LCT2D(t1,1-dim)) lcts[1-dim] = LCT2D(t1,1-dim);
      }

      for (i=0; i<n; i++) {
	ITEM t1 = arr[i];
	
	if (EST2D(t1,dim) >= ests[dim]) {
	  area += DUR2D(t1,0)*DUR2D(t1,1);
	  NEXT(t1) = s, s = t1;
	  /* HERE: can enforce upper bounds on t1->mindur, t1->sup */
	}
      }
      l = pdata->items_by_area;
      while (s>TERMIN && l>TERMIN && DUR2D(l,0)>0 && DUR2D(l,1)>0) {
	long lminy = EST2D(l,1-dim);
	long lmaxy = LCT2D(l,1-dim);
	long min_ov_y = min_overlap_interval(lminy,lmaxy,DUR2D(l,1-dim),
					     ests[1-dim],lcts[1-dim]);

	if (  !(STATUS(l)&STATUS_TARGET)
	   || min_ov_y==0
	   || EST2D(l,dim)>=ests[dim]
	   || LCT2D(l,dim)<=ests[dim]) {
	 l = NEXT_AREA(l);
	} else {
	  long larea = DUR2D(l,0)*min_ov_y;
	  long slack1 = larea;
          long slack2;
	  long lb[2], ub[2];
	  
	  lcts[dim] = LCT2D(s,dim);
	  /* compute the smallest of the following two slacks:
	     - total minimal overlap of all items wrt. bounding box of S
	     - total minimal overlap of all items
	       wrt. (bounding box of S) /\ (bb of l extended along dim)
*/
	  lb[dim] = ests[dim];
	  ub[dim] = lcts[dim];
	  lb[1-dim] = ests[1-dim];
	  ub[1-dim] = lcts[1-dim];
	  if (lb[1-dim] < lminy) lb[1-dim] = lminy;
	  if (ub[1-dim] > lmaxy) ub[1-dim] = lmaxy;
	  if (lb[1-dim]<ub[1-dim])
	    slack1 = (ub[0]-lb[0])*(ub[1]-lb[1]) -
	             total_area2d(arr,n,l,lb,ub);
	  if (lb[1-dim] != ests[1-dim] || ub[1-dim] != lcts[1-dim]) {
	    slack2 = (lcts[0]-ests[0])*(lcts[1]-ests[1]) -
	             total_area2d(arr,n,l,ests,lcts);
	    if (slack1>slack2) slack1 = slack2;
	  }
	  slack2 = (LCT2D(l,dim)-ests[dim])*(lcts[1-dim]-ests[1-dim]) - area;
	  if (slack1>=larea) {
	      if (slack2>=larea) /* may be inside and last */
		l = NEXT_AREA(l);
	      do {
		area -= DUR2D(s,0)*DUR2D(s,1);
		s = NEXT(s);
	      } while (s>TERMIN && LCT2D(s,dim)==lcts[dim]);
	      bounding_box(s,ests,lcts,ests[1-dim],lcts[1-dim],dim); /* refresh */
	  } else {
	    long min_ov_x = min_overlap_interval(EST2D(l,dim),LCT2D(l,dim),DUR2D(l,dim),ests[dim],lcts[dim]);
	    long max_ov_x = max_overlap_interval(EST2D(l,dim),LCT2D(l,dim),DUR2D(l,dim),ests[dim],lcts[dim]);
	    
	    if (min_ov_x*min_ov_y > slack1)
	      return FALSE;
	    else if (max_ov_x*min_ov_y > slack1) { /* must stick out at either end */
	      long delta = FLOORDIV(slack1,min_ov_y);
	      long ub = ests[dim]+delta-DUR2D(l,dim);
	      long lb = lcts[dim]-delta;
		  
	      if (LST2D(l,dim) < lb) { /* must stick out to the left */
		if (!prune_item2d(l, EST2D(l,dim), ub, dim))
		  return FALSE;
		*change = TRUE;
		sorted = FALSE;
		for (s1=s; s1>TERMIN; s1=NEXT(s1))	/* added 001115 */
		  if (!prune_before2d(l,s1,dim))
		    return FALSE;
	      }
	    }
	    l = NEXT_AREA(l);
	  }
	}
      }
      while (j<n && seed[j]==ests[dim])
	j++;
    }
  return TRUE;
}

#else

static BOOL edge_finding2d_up MAGIC (HIDDEN_PROTO
				     BOOL *change,
				     int dim)
{
  struct disjoint2_data *pdata = fd.gdata;
  ITEM *src_by_est = pdata->sortarr;
  ITEM *src_by_lct = src_by_est + pdata->nitems;
  long *seed = src_by_lct + pdata->nitems;
  long *yoverlap = seed + pdata->nitems;
  int n = pdata->ntargets + pdata->nsources;
  int i, j, k, mid, sup;
#if DOMINATION
  ITEM not_pruned = TERMIN;
#endif

  for (i=0; i<n; i++)
    src_by_est[i] = src_by_lct[i] = TARGET(i);

  if (dim==0) {
    qsort_asc_est2d0(src_by_est, n);
    qsort_asc_lct2d0(src_by_lct, n);
  } else {
    qsort_asc_est2d1(src_by_est, n);
    qsort_asc_lct2d1(src_by_lct, n);
  }

  for (i=0; i<pdata->ntargets; i++) {
    ITEM t = TARGET(i);
    
    if ((STATUS(t)&STATUS_TARGET) && DUR2D(t,0)>0 && DUR2D(t,1)>0) {
      long est0, lct0; /* current bounding box */
      ITEM omega, omega2, omega3; /* sets of sources */
      ITEM s;
      long estt = EST2D(t,dim);
      long durt = DUR2D(t,dim);
      long lstt = LST2D(t,dim);
      long lctt = LCT2D(t,dim);
      long est1 = EST2D(t,1-dim);
      long lct1 = LCT2D(t,1-dim);
      long heightt = DUR2D(t,1-dim);
      long larea = durt*heightt;
      long ysize = lct1-est1;
      BOOL tchange = FALSE;
#if INCREMENTAL
      long at = PRUNED_AT2D(t,0) + PRUNED_AT2D(t,1) + 2;
#endif
      
#if DOMINATION
      if (not_pruned>TERMIN &&
	  estt == EST2D(not_pruned,dim) &&
	  lctt == LCT2D(not_pruned,dim) &&
	  est1 == EST2D(not_pruned,1-dim) &&
	  lct1 == LCT2D(not_pruned,1-dim) &&
	  durt == DUR2D(not_pruned,dim) &&
	  heightt == DUR2D(not_pruned,1-dim))
	continue;
#endif
      
      /* build set omega */
      omega = TERMIN;
      for (j=n-1; j>=0; j--) {
	s = src_by_est[j];
	
	if (s!=t && EST2D(s,dim)<lctt && DUR2D(s,dim)>0)
	  yoverlap[s] = min_overlap_interval(EST2D(s,1-dim),LCT2D(s,1-dim),DUR2D(s,1-dim),est1,lct1);
	else
	  yoverlap[s] = 0;
	if (yoverlap[s]>0) {
#if INCREMENTAL
	  at += PRUNED_AT2D(s,0) + PRUNED_AT2D(s,1) + 2;
#endif
	  NEXT(s) = omega;
	  omega = s;
	}
      }
#if INCREMENTAL
      if (at==0) continue;
#endif
      
      lct0 = estt;
      /* dichotomic search for first seed > lct0 */
      j = 0;
      sup = n;
      while (j<sup) {
	mid = (j+sup)>>1;
	if (LCT2D(src_by_lct[mid],dim) <= lct0)
	  j = mid+1;
	else
	  sup = mid;
      }

      /* build array of k relevant lct's */
      for (k=0; j<n; j++) {
	long l = LCT2D(src_by_lct[j],dim);

	if (l>=lctt)		/* > yields slightly better pruning, slower */
	  break;
	if (l>lct0 && yoverlap[src_by_lct[j]]>0)
	  seed[k++] = lct0 = l;
      }

      for (k--; k>=0; k--) {
	long area = 0;
	long est2 = 0;
	
	est0 = EST2D(omega,dim);
	lct0 = seed[k];
	for (s=omega; s>TERMIN; s=NEXT(s))
	  area += min_overlap_interval(EST2D(s,dim),LCT2D(s,dim),DUR2D(s,dim),
				       est0,lct0) *
	          yoverlap[s];
	omega2 = omega3 = omega;
	while (omega2>TERMIN && est0<lct0) {
	  long slack1 = (lct0-est0)*ysize - area;
	  
	  if (slack1<larea) {
	    long min_ov_x = min_overlap_interval(estt,lctt,durt,est0,lct0);
	    long max_ov_x = max_overlap_interval(estt,lctt,durt,est0,lct0);
	    
	    if (min_ov_x*heightt > slack1)
	      return FALSE;
	    else if (max_ov_x*heightt > slack1) { /* must stick out at either end */
	      long delta = FLOORDIV(slack1,heightt);
	      long ub = est0+delta-durt;
	      long lb = lct0-delta;
		  
	      if (estt > ub) { /* must stick out to the right */
		if (!prune_item2d(t, lb, lstt, dim))
		  return FALSE;
		tchange = TRUE;
		for (s=omega2; s>TERMIN; s=NEXT(s)) {	/* added 001115 */
		  long lcts = LCT2D(s,dim);
		  
		  if (lcts<=lct0 && /* then s must be before t */
		      lcts>estt && /* then s could prune t */
		      !prune_before2d(s,t,dim))
		    return FALSE;
		}
		estt = EST2D(t,dim); /* refresh! */
	      }
	    }
	  }

	  while (omega2>TERMIN && (est2=EST2D(omega2,dim))==est0)
	    omega2 = NEXT(omega2);
	  if (omega2==TERMIN || est2>=lct0)
	    break;
	  while (omega3>TERMIN && ECT2D(omega3,dim)<=est0)
	    omega3 = NEXT(omega3);
	  for (s=omega3; s>TERMIN; s=NEXT(s)) {
	    long ests = EST2D(s,dim);
	    
	    if (ests>=est2)
	      break;
	    area -= min_overlap_delta_est(ests,LCT2D(s,dim),DUR2D(s,dim),
					  est0,est2,lct0) *
	            yoverlap[s];
	  }
	  est0 = est2;
	}
      }
      if (tchange) {
	*change = TRUE;
	/* both array must be resorted---prune_before2d can adjust LCT:s */
	if (dim==0) {
	  qsort_asc_est2d0(src_by_est, n);
	  qsort_asc_lct2d0(src_by_lct, n);
	} else {
	  qsort_asc_est2d1(src_by_est, n);
	  qsort_asc_lct2d1(src_by_lct, n);
	}
      }
#if DOMINATION
      else
	not_pruned = t;
#endif
    }
  }
  return TRUE;
}


static BOOL edge_finding2d_down MAGIC (HIDDEN_PROTO
				       BOOL *change,
				       int dim)
{
  struct disjoint2_data *pdata = fd.gdata;
  ITEM *src_by_est = pdata->sortarr;
  ITEM *src_by_lct = src_by_est + pdata->nitems;
  long *seed = src_by_lct + pdata->nitems;
  long *yoverlap = seed + pdata->nitems;
  int n = pdata->ntargets + pdata->nsources;
  int i, j, k, mid, sup;
#if DOMINATION
  ITEM not_pruned = TERMIN;
#endif
  
  for (i=0; i<pdata->ntargets; i++) {
    ITEM t = TARGET(i);

    if ((STATUS(t)&STATUS_TARGET) && DUR2D(t,0)>0 && DUR2D(t,1)>0) {
      long est0, lct0; /* current bounding box */
      ITEM omega, omega2, omega3; /* sets of sources */
      ITEM s;
      long estt = EST2D(t,dim);
      long durt = DUR2D(t,dim);
      long lctt = LCT2D(t,dim);
      long est1 = EST2D(t,1-dim);
      long lct1 = LCT2D(t,1-dim);
      long heightt = DUR2D(t,1-dim);
      long larea = durt*heightt;
      long ysize = lct1-est1;
      BOOL tchange = FALSE;
#if INCREMENTAL
      long at = PRUNED_AT2D(t,0) + PRUNED_AT2D(t,1) + 2;
#endif
      
#if DOMINATION
      if (not_pruned>TERMIN &&
	  estt == EST2D(not_pruned,dim) &&
	  lctt == LCT2D(not_pruned,dim) &&
	  est1 == EST2D(not_pruned,1-dim) &&
	  lct1 == LCT2D(not_pruned,1-dim) &&
	  durt == DUR2D(not_pruned,dim) &&
	  heightt == DUR2D(not_pruned,1-dim))
	continue;
#endif

      /* build set omega */
      omega = TERMIN;
      for (j=0; j<n; j++) {
	s = src_by_lct[j];
	
	if (s!=t && LCT2D(s,dim)>estt && DUR2D(s,dim)>0)
	  yoverlap[s] = min_overlap_interval(EST2D(s,1-dim),LCT2D(s,1-dim),DUR2D(s,1-dim),est1,lct1);
	else
	  yoverlap[s] = 0;
	if (yoverlap[s]>0) {
#if INCREMENTAL
	  at += PRUNED_AT2D(s,0) + PRUNED_AT2D(s,1) + 2;
#endif
	  NEXT(s) = omega;
	  omega = s;
	}
      }
#if INCREMENTAL
      if (at==0) continue;
#endif
      
      est0 = estt;		/* estt-1 yields slightly better pruning, slower */
      /* dichotomic search for first seed > est0 */
      j = 0;
      sup = n;
      while (j<sup) {
	mid = (j+sup)>>1;
	if (EST2D(src_by_est[mid],dim) <= est0)
	  j = mid+1;
	else
	  sup = mid;
      }

      /* build array of k relevant est's */
      for (k=0; j<n; j++) {
	long l = EST2D(src_by_est[j],dim);

	if (l>=lctt)
	  break;
	if (l>est0 && yoverlap[src_by_lct[j]]>0)
	  seed[k++] = est0 = l;
      }

      for (j=0; j<k; j++) {
	long area = 0;
	long lct2 = 0;
	
	est0 = seed[j];
	lct0 = LCT2D(omega,dim);
	for (s=omega; s>TERMIN; s=NEXT(s))
	  area += min_overlap_interval(EST2D(s,dim),LCT2D(s,dim),DUR2D(s,dim),
				       est0,lct0) *
	          yoverlap[s];
	omega2 = omega3 = omega;
	while (omega2>TERMIN && est0<lct0) {
	  long slack1 = (lct0-est0)*ysize - area;
	  
	  if (slack1<larea) {
	    long min_ov_x = min_overlap_interval(estt,lctt,durt,est0,lct0);
	    long max_ov_x = max_overlap_interval(estt,lctt,durt,est0,lct0);
	    
	    if (min_ov_x*heightt > slack1)
	      return FALSE;
	    else if (max_ov_x*heightt > slack1) { /* must stick out at either end */
	      long delta = FLOORDIV(slack1,heightt);
	      long ub = est0+delta-durt;
	      long lb = lct0-delta+durt;
		  
	      if (lctt < lb) { /* must stick out to the left */
		if (!prune_item2d(t, estt, ub, dim))
		  return FALSE;
		tchange = TRUE;
		for (s=omega2; s>TERMIN; s=NEXT(s)) {	/* added 001115 */
		  long ests = EST2D(s,dim);
		  
		  if (ests>=est0 && /* then s must be after t */
		      ests<lctt && /* then s could prune t */
		      !prune_before2d(t,s,dim))
		    return FALSE;
		}
		lctt = LCT2D(t,dim); /* refresh! */
	      }
	    }
	  }

	  while (omega2>TERMIN && (lct2=LCT2D(omega2,dim))==lct0)
	    omega2 = NEXT(omega2);
	  if (omega2==TERMIN || est0>=lct2)
	    break;
	  while (omega3>TERMIN && LST2D(omega3,dim)>=lct0)
	    omega3 = NEXT(omega3);
	  for (s=omega3; s>TERMIN; s=NEXT(s)) {
	    long lcts = LCT2D(s,dim);
	    
	    if (lcts<=lct2)
	      break;
	    area -= min_overlap_delta_lct(EST2D(s,dim),lcts,DUR2D(s,dim),
					  est0,lct0,lct2) *
	            yoverlap[s];
	  }
	  lct0 = lct2;
	}
      }
      if (tchange) {
	*change = TRUE;
	/* both array must be resorted---prune_before2d can adjust LCT:s */
	if (dim==0) {
	  qsort_asc_est2d0(src_by_est, n);
	  qsort_asc_lct2d0(src_by_lct, n);
	} else {
	  qsort_asc_est2d1(src_by_est, n);
	  qsort_asc_lct2d1(src_by_lct, n);
	}
      }
#if DOMINATION
      else
	not_pruned = t;
#endif
    }
  }
  return TRUE;
}
#endif

#define NOVERLAP2D(EV)										 \
(start[EV]-dur[k] > end[EV] || /* empty forbidden region in [0] */					 \
 pdata->event.start[1-k][EV]-dur[1-k] > pdata->event.end[1-k][EV] || /* empty forbidden region in [1] */ \
 /* est[k] > pdata->event.end[k][EV] ||  X after forbidden region in [k], guaranteed false */		 \
 est[1-k] > pdata->event.end[1-k][EV] || /* X after forbidden region in [1-k] */			 \
 /* lct[k] < pdata->event.start[k][EV] ||  X before forbidden region in [k], guaranteed false */	 \
 lct[1-k] < pdata->event.start[1-k][EV] /* X before forbidden region in [1-k] */			 \
 )


static INLINE int next_start2d MAGIC (HIDDEN_PROTO
				      int js,int nsources,
				      ITEM titem,long *dur,long *est,long *lct,
				      long est0,long stop,int k,long *next)
{
  struct disjoint2_data *pdata = fd.gdata;
  EVENT ev = TERMIN;
  long *start, *end;

  start = pdata->event.start[k];
  end = pdata->event.end[k];

  while (js<nsources &&
	 start[(ev = pdata->start_event[k][js])]-dur[k] <= stop &&
	 (pdata->event.item[ev] == titem ||
	  end[ev] < est0 || /* necessary test! completes filtering done at [*] */
	  NOVERLAP2D(ev)))
    js++;
  *next = js<nsources ? start[ev]-dur[k] : CLPFD_MAXINT-1L;
  return js;
}


static INLINE int next_end2d MAGIC (HIDDEN_PROTO
				    int je,int nsources,
				    ITEM titem,long *dur,long *est,long *lct,
				    int k,long *next)
{
  struct disjoint2_data *pdata = fd.gdata;
  EVENT ev = TERMIN;
  long *start, *end;

  start = pdata->event.start[k];
  end = pdata->event.end[k];

  while (je<nsources &&
	 (pdata->event.item[(ev = pdata->end_event[k][je])] == titem ||
	  NOVERLAP2D(ev)))
    je++;
  *next = je<nsources ? end[ev] : CLPFD_MAXINT-1L;
  return je;
}


static INLINE int prev_start2d MAGIC (HIDDEN_PROTO
				      int js,
				      ITEM titem,long *dur,long *est,long *lct,
				      int k,long *next)
{
  struct disjoint2_data *pdata = fd.gdata;
  EVENT ev = TERMIN;
  long *start, *end;

  start = pdata->event.start[k];
  end = pdata->event.end[k];

  while (js>0 &&
	 (pdata->event.item[(ev = pdata->start_event[k][js-1])] == titem ||
	  NOVERLAP2D(ev)))
    js--;
  *next = js>0 ? start[ev]-dur[k] : 1L-CLPFD_MAXINT;
  return js;
}


static INLINE int prev_end2d MAGIC (HIDDEN_PROTO
				    int je,
				    ITEM titem,long *dur,long *est,long *lct,
				    long lct0,long stop,int k,long *next)
{
  struct disjoint2_data *pdata = fd.gdata;
  EVENT ev = TERMIN;
  long *start, *end;

  start = pdata->event.start[k];
  end = pdata->event.end[k];

  while (je>0 &&
	 end[(ev = pdata->end_event[k][je-1])] >= stop &&
	 (pdata->event.item[ev] == titem ||
	  start[ev] > lct0 || /* necessary test! completes filtering done at [**] */
	  NOVERLAP2D(ev)))
    je--;
  *next = je>0 ? end[ev] : 1L-CLPFD_MAXINT;
  return je;
}


/*** trace origin vs. forbidden area
void trace_orig MAGIC (HIDDEN_PROTO
                       struct item2d *titem,
		       struct event2d *ev,
		       long current, int k, int dir, int delta)
{
  struct disjoint2_data *pdata = fd.gdata;
  char *prefix1 = (delta==1 ? "+" : "-");
  char *prefix2 = (dir==1 ? ">" : "<");
  long omin[2], omax[2], fmin[2], fmax[2];
  BOOL ok, current_ok;
  char *suffix;

  omin[0] = EST2D(titem,0);
  omax[0] = LST2D(titem,0);
  omin[1] = EST2D(titem,1);
  omax[1] = LST2D(titem,1);
  fmin[0] = pdata->event.start[0][ev]-DUR2D(titem,0);
  fmax[0] = pdata->event.end[0][ev];
  fmin[1] = pdata->event.start[1][ev]-DUR2D(titem,1);
  fmax[1] = pdata->event.end[1][ev];
  ok = !(fmin[0]>fmax[0] || fmin[1]>fmax[1] || omin[0]>fmax[0] || omin[1]>fmax[1] || omax[0]<fmin[0] || omax[1]<fmin[1]);
  suffix = ok ? "OK" : "NO";
  current_ok = current<fmin[k] || current>fmax[k];

  printf(" %s%s origin(%d..%d,%d..%d) forbidden(%d..%d,%d..%d) [ %s ] current_ok=%d \n",
	 prefix1,prefix2,omin[0],omax[0],omin[1],omax[1],fmin[0],fmax[0],fmin[1],fmax[1],suffix,current_ok);
  ;
}
***/



#if 0

static BOOL feasible_minwit MAGIC (HIDDEN_PROTO
				   int js, long next_start,int nsources,
				   ITEM titem,
				   long *dur,long *est,long *lct,int k)
{
  struct disjoint2_data *pdata = fd.gdata;
  EVENT ev;
  long ub = est[k];
  long wit = GetSmall(MINWIT2D(titem,k));

  if (wit < est[1-k] || wit > lct[1-k]-dur[1-k] || /* NOT covered by next line */
      !fd_member(MINWIT2D(titem,k),FDSET2D(titem,1-k)))
    return FALSE;
  while (next_start <= ub) {
    ev = pdata->start_event[k][js];
    if (wit >= pdata->event.start[1-k][ev]-dur[1-k] && wit <= pdata->event.end[1-k][ev])
      return FALSE;
    js = next_start2d(js+1,nsources,titem,dur,est,lct,ub,ub,k,&next_start);
  }
  return TRUE;
}


static BOOL feasible_maxwit MAGIC (HIDDEN_PROTO
				   int je, long next_end,
				   ITEM titem,
				   long *dur,long *est,long *lct,int k)
{
  struct disjoint2_data *pdata = fd.gdata;
  EVENT ev;
  long lb = lct[k]-dur[k];
  long wit = GetSmall(MAXWIT2D(titem,k));
  
  if (wit < est[1-k] || wit > lct[1-k]-dur[1-k] || /* NOT covered by next line */
      !fd_member(MAXWIT2D(titem,k),FDSET2D(titem,1-k)))
    return FALSE;
  while (next_end >= lb) {
    ev = pdata->end_event[k][je-1];
    if (wit >= pdata->event.start[1-k][ev]-dur[1-k] && wit <= pdata->event.end[1-k][ev])
      return FALSE;
    je = prev_end2d(je-1,titem,dur,est,lct,lct[k],lb,k,&next_end);
  }
  return TRUE;
}


#else

static BOOL feasible_minwit MAGIC (HIDDEN_PROTO
				   int nt,
				   ITEM titem,
				   long *dur,long *est,long *lct,int k)
{
  struct disjoint2_data *pdata = fd.gdata;
  EVENT ev=0;
  long wit = GetSmall(MINWIT2D(titem,k));
  int js=0, sup=nt, mid;
  long *start, *end, *max_end_t, *start_event_t;

  if (wit < est[1-k] || wit > lct[1-k]-dur[1-k] || /* NOT covered by next line */
      !fd_member(MakeSmall(wit),FDSET2D(titem,1-k)))
    return FALSE;

  start = pdata->event.start[k];
  end = pdata->event.end[k];
  max_end_t = pdata->event.max_end_t[k];
  start_event_t = pdata->start_event_t[k];
  
  /* dichotomic search for first start event whose end is not before X */
  /* [#] NOTE: this does not filter out all events that are before X! */
  while (js<sup) {
    mid = (js+sup)>>1;
    if (max_end_t[(start_event_t[mid])] < est[k])
      js = mid+1;
    else
      sup = mid;
  }
  
  for (;;) {
    while (js<nt &&
	   start[(ev = start_event_t[js])]-dur[k] <= est[k] &&
	   (pdata->event.item[ev] == titem ||
	    end[ev] < est[k] || /* necessary test! completes filtering done at [#] */
	    NOVERLAP2D(ev)))
      js++;
    if (js==nt || start[ev]-dur[k] > est[k])
      return TRUE;
    if (wit >= pdata->event.start[1-k][ev]-dur[1-k] && wit <= pdata->event.end[1-k][ev])
      return FALSE;
    js++;
  }
}


static BOOL feasible_maxwit MAGIC (HIDDEN_PROTO
				   int nt,
				   ITEM titem,
				   long *dur,long *est,long *lct,int k)
{
  struct disjoint2_data *pdata = fd.gdata;
  EVENT ev=0;
  long lb = lct[k]-dur[k];
  int je=0, sup=nt, mid;
  long wit = GetSmall(MAXWIT2D(titem,k));
  long *start, *end, *min_start_t, *end_event_t;
  
  if (wit < est[1-k] || wit > lct[1-k]-dur[1-k] || /* NOT covered by next line */
      !fd_member(MakeSmall(wit),FDSET2D(titem,1-k)))
    return FALSE;

  start = pdata->event.start[k];
  end = pdata->event.end[k];
  min_start_t = pdata->event.min_start_t[k];
  end_event_t = pdata->end_event_t[k];
  
  /* dichotomic search for first end event whose start is after X */
  /* [##] NOTE: this does not filter out all events that are after X! */
  while (je<sup) {
    mid = (je+sup)>>1;
    if (min_start_t[(end_event_t[mid])] <= lct[k])
      je = mid+1;
    else
      sup = mid;
  }

  for (;;) {
    while (je>0 &&
	   end[(ev = end_event_t[je-1])]>=lb &&
	   (pdata->event.item[ev] == titem ||
	    start[ev] > lct[k] || /* necessary test! completes filtering done at [##] */
	    NOVERLAP2D(ev)))
      je--;
    if (je==0 || end[ev] < lb)
      return TRUE;
    if (wit >= pdata->event.start[1-k][ev]-dur[1-k] && wit <= pdata->event.end[1-k][ev])
      return FALSE;
    je--;
  }
  return TRUE;
}
#endif


#if DOMINATION
/* True if r2 is "larger" and "more constrained" than r1,
   in which case r2 will be cached as the most dominating so far
   for the given sweep direction (right, left, up, down).
   See comments about DOMINATION.
*/
static BOOL less_constrained2d MAGIC (HIDDEN_PROTO
				      ITEM r1, ITEM r2, BOOL rightp, int k)
{
  struct disjoint2_data *pdata = fd.gdata;
  long ori1, ori2, diff;
  
  if (r1==TERMIN)
    return TRUE;
  
  ori1 = (!rightp ? EST2D(r1,k) : -LST2D(r1,k));
  ori2 = (!rightp ? EST2D(r2,k) : -LST2D(r2,k));

  diff = ori1 - ori2;		/* minimize */
  if (diff > 0) return TRUE;
  if (diff < 0) return FALSE;

  diff = DUR2D(r1,k) - DUR2D(r2,k);	/* maximize */
  if (diff < 0) return TRUE;
  if (diff > 0) return FALSE;

  diff = DUR2D(r1,1-k) - DUR2D(r2,1-k); /* maximize */
  if (diff < 0) return TRUE;
  if (diff > 0) return FALSE;

  diff = SLACK2D(r1,k) - SLACK2D(r2,k); /* maximize */
  if (diff < 0) return TRUE;
  if (diff > 0) return FALSE;

  diff = SLACK2D(r1,1-k) - SLACK2D(r2,1-k); /* minimize */
  if (diff > 0) return TRUE;

  return FALSE;
}


/* True if r1 dominates r2, in which case r2's origin in the given
   sweep direction will not be pruned.
*/
static BOOL dominates2d MAGIC (HIDDEN_PROTO
			       ITEM r1, ITEM r2, BOOL rightp, int k)
{
  struct disjoint2_data *pdata = fd.gdata;
  long start[2], end[2], ori;
  
  if (r1==TERMIN)
    return FALSE;
  if (!rightp) {
    ori = EST2D(r2,k);
    if (MIN2D(r1,k) != MIN2D(r2,k))
      return FALSE;
  } else {
    ori = LST2D(r2,k);
    if (MAX2D(r1,k) != MAX2D(r2,k))
      return FALSE;
  }

  if (CTagToCdr(FDSET2D(r2,1-k)) != EmptySet || /* no holes in y domain */
      DUR2D(r1,k) <  DUR2D(r2,k) ||
      DUR2D(r1,1-k) <  DUR2D(r2,1-k) ||
      EST2D(r1,1-k) <  EST2D(r2,1-k) ||
      LST2D(r1,1-k) >  LST2D(r2,1-k))
    return FALSE;

  /* Missing: R1 must not forbid the X min (or max) of R2 */
  BASIC_FORBIDDEN_REGION2D(r2, r1, start, end);

  if (start[0] > end[0] ||	/* empty region */
      start[1] > end[1] ||	/* empty region */
      ori > end[k] ||
      ori < start[k] ||
      EST2D(r2,1-k) > end[1-k] ||
      LST2D(r2,1-k) < start[1-k]) /* no overlap */
    return TRUE;

  return FALSE;
}
#endif




/* New filtering algorithm.
   Form L1: list of relative starts of forbidden areas.
   Form L2: list of ends of forbidden areas.
   For each target:
     scan L1 and L2 for forbidden regions.
*/
static int disjoint2_filter MAGIC (HIDDEN_PROTO
				   int nsources,
				   ITEM *target0, int ntargets,
				   BOOL incremental,
				   BOOL *change)
{
  struct disjoint2_data *pdata = fd.gdata;
  int flags = pdata->flags;
  int i, k, nt;
  long dur_threshold[2] = {CLPFD_MAXINT,CLPFD_MAXINT}; /* too small items can't be pruned */
  long min_start, max_end, wit;
  long start[2], end[2];
#if DOMINATION
  BOOL moved = FALSE;
  ITEM not_pruned[2][2];
#endif

  for (i=0, nt=0; i<nsources; i++) {
    SHIFTED_ITEM src = pdata->source[i];
    ITEM titem;
    EVENT ev = i;

    RELATIVE_FORBIDDEN_REGION2D(src, start, end);
    if (flags & 0x8) {
      int hits =
	extend_forbidden_region2d(target0[0],
				  pdata->shifted_item.item[src],
				  start, end);
      if (hits==0 &&
	  (DUR2D(target0[0],0)==0 ||
	   DUR2D(target0[0],1)==0))
	start[0] = end[0] = start[1] = end[1] = CLPFD_MAXINT;
    }
    pdata->event.item[ev] = titem = pdata->shifted_item.item[src];
    if (!incremental ||
	((STATUS(titem)&STATUS_TARGET) && /* for checking witness */
	 PRUNED_AT2D(titem,0)+PRUNED_AT2D(titem,1) > -2)) { /* wrt. recently pruned targets */
      pdata->start_event_t[0][nt] = pdata->end_event_t[0][nt] = ev;
      pdata->start_event_t[1][nt] = pdata->end_event_t[1][nt] = ev;
      nt++;
    }
    for (k=0; k<2; k++) {
      pdata->start_event[k][i] = pdata->end_event[k][i] = ev;
      pdata->event.start[k][ev] = start[k];
      pdata->event.end[k][ev] = end[k];
      if (dur_threshold[k] > start[k]-end[k])
	dur_threshold[k] = start[k]-end[k];
    }
#if DBG > 2
    printf("rel. forbidden region Ox=%ld Ex=%ld Oy=%ld Ey=%ld\n",
	   start[0], end[0], start[1], end[1]);
    printf(" non-empty for targets with Dx>=%ld and Dy>=%ld\n",
	   start[0]-end[0], start[1]-end[1]);
#endif
  }
  if (!(flags & 0x8)) {
    if (dur_threshold[0]<1)
      dur_threshold[0] = 1;
    if (dur_threshold[1]<1)
      dur_threshold[1] = 1;
  }
  
  qsort_asc_start0(pdata->start_event[0], nsources);
  qsort_asc_end0(pdata->end_event[0], nsources);
  qsort_asc_start1(pdata->start_event[1], nsources);
  qsort_asc_end1(pdata->end_event[1], nsources);
	
  if (nt>0) {			/* for checking witness */
    qsort_asc_start0(pdata->start_event_t[0], nt);
    qsort_asc_end0(pdata->end_event_t[0], nt);
    qsort_asc_start1(pdata->start_event_t[1], nt);
    qsort_asc_end1(pdata->end_event_t[1], nt);
  }
  
  for (k=0; k<2; k++) {
#if DOMINATION
    not_pruned[k][0] = TERMIN;
    not_pruned[k][1] = TERMIN;
#endif
    max_end = pdata->event.end[k][(pdata->start_event[k][0])];
    min_start = pdata->event.start[k][(pdata->end_event[k][nsources-1])];
    for (i=0; i<nsources; i++) {
      if (max_end < pdata->event.end[k][(pdata->start_event[k][i])])
	max_end = pdata->event.end[k][(pdata->start_event[k][i])];
      pdata->event.max_end[k][(pdata->start_event[k][i])] = max_end;
      if (min_start > pdata->event.start[k][(pdata->end_event[k][nsources-i-1])])
	min_start = pdata->event.start[k][(pdata->end_event[k][nsources-i-1])];
      pdata->event.min_start[k][(pdata->end_event[k][nsources-i-1])] = min_start;
    }
    if (nt>0) {			/* for checking witness */
      max_end = pdata->event.end[k][(pdata->start_event_t[k][0])];
      min_start = pdata->event.start[k][(pdata->end_event_t[k][nt-1])];
      for (i=0; i<nt; i++) {
	if (max_end < pdata->event.end[k][(pdata->start_event_t[k][i])])
	  max_end = pdata->event.end[k][(pdata->start_event_t[k][i])];
	pdata->event.max_end_t[k][(pdata->start_event_t[k][i])] = max_end;
	if (min_start > pdata->event.start[k][(pdata->end_event_t[k][nt-i-1])])
	  min_start = pdata->event.start[k][(pdata->end_event_t[k][nt-i-1])];
	pdata->event.min_start_t[k][(pdata->end_event_t[k][nt-i-1])] = min_start;
      }
    }
  }
  
  /* adjust all mins and maxes */
  
  for (i=0; i<ntargets; i++) {
    ITEM titem = target0[i];
    long est[2];
    long lst[2];
    long dur[2];
    long lct[2];
    BOOL ground;
    
#if DBG > 2
    printf("target Ox=%ld..%ld Dx=%ld Oy=%ld..%ld Dy=%ld\n",
	   EST2D(titem,0), LST2D(titem,0), DUR2D(titem,0), EST2D(titem,1), LST2D(titem,1), DUR2D(titem,1));
#endif
    if (DUR2D(titem,0) < dur_threshold[0]) /* the rest are all smaller */
      break;
    if (DUR2D(titem,1) < dur_threshold[1]) /* this can't be pruned */
      continue;
    if (!(STATUS(titem)&STATUS_TARGET))
      continue;
    
    ground = TRUE;
    for (k=0; k<2; k++) {
      est[k] = EST2D(titem,k);
      lst[k] = LST2D(titem,k);
      dur[k] = DUR2D(titem,k);
      lct[k] = lst[k]+dur[k];
#if INCREMENTAL
      if (est[k]>=pdata->bbmax[k] || lct[k]<=pdata->bbmin[k])
	goto next_target;
#endif
      ground &= (est[k]==lst[k] && dur[k]==DURmax2D(titem,k));
    }
#if DECOMPOSITION
    if (ground)
      STATUS(titem) &= ~STATUS_TARGET;
#endif

    for (k=0; k<2; k++) {
      long est0, lct0, current, previous, next_start, next_end;
      int js, je, mid, sup;
      TAGGED tdom = FDSET2D(titem,1-k);

      /* pruning min(X) */

#if DOMINATION
      /* it seems better to do this _before_ feasible_minwit() */
      if (!(flags & 0xe) &&
	  dominates2d(not_pruned[k][0], titem, 0, k)) {
	MINWIT2D(titem,k) = MINWIT2D(not_pruned[k][0],k); /* maintain invariant! */
	goto prune_max;	/* titem will not be pruned either */
      }
#endif
      /* scan the start and end events for the first feasible min(X) */
      if (!feasible_minwit(nt,titem,dur,est,lct,k)) {
	/* dichotomic search for first start event whose end is not before X */
	/* [*] NOTE: this does not filter out all events that are before X! */
	js = 0;
	sup = nsources;
	while (js<sup) {
	  mid = (js+sup)>>1;
	  if (pdata->event.max_end[k][(pdata->start_event[k][mid])] < est[k])
	    js = mid+1;
	  else
	    sup = mid;
	}
	js = next_start2d(js,nsources,titem,dur,est,lct,est[k],lst[k],k,&next_start);
	/* dichotomic search for first end event that is not before X */
	je = 0;
	sup = nsources;
	while (je<sup) {
	  mid = (je+sup)>>1;
	  if (pdata->event.end[k][(pdata->end_event[k][mid])] < est[k])
	    je = mid+1;
	  else
	    sup = mid;
	}
	je = next_end2d(je,nsources,titem,dur,est,lct,k,&next_end);
	fd.profile = profile_init_complement(CTagToCar(tdom),CTagToCdr(tdom));
	current = est0 = est[k];
	while (current <= lst[k]) {
	  if (next_start <= current) {
	    EVENT ev = pdata->start_event[k][js];
	    /* trace_orig(titem,ev,current,k,1,1); */
	    fd.profile = profile_update(fd.profile,
				     pdata->event.start[1-k][ev]-dur[1-k],
				     pdata->event.end[1-k][ev]+1,
				     1);
	    js = next_start2d(js+1,nsources,titem,dur,est,lct,est0,lst[k],k,&next_start);
	  } else if (next_end < current) {
	    EVENT ev = pdata->end_event[k][je];
	    /* trace_orig(titem,ev,current,k,1,-1); */
	    fd.profile = profile_update(fd.profile,
				     pdata->event.start[1-k][ev]-dur[1-k],
				     pdata->event.end[1-k][ev]+1,
				     -1);
	    je = next_end2d(je+1,nsources,titem,dur,est,lct,k,&next_end);
	  } else if (!profile_zero_at(fd.profile, est[1-k], lst[1-k]+1, &wit)) {
	    current = next_end+1;
	  } else if (current > est[k]) {
	    if (!prune_item2d(titem, current, LST2D(titem,k), k))
	      return FALSE;
	    previous = current;
	    est[k] = current = EST2D(titem,k); /* refresh */
	    *change = TRUE;
#if DOMINATION
	    moved = TRUE;
#endif
	    if (previous==current) break; /* current is feasible */
	  }
	  else break;		/* current is feasible */
	}
	profile_dispose(fd.profile);
	if (current > lst[k])
	  return FALSE;
	else
	  MINWIT2D(titem,k) = MakeSmall(wit);
      }
#if DOMINATION
      if (!(flags & 0xe) && !moved &&
	  less_constrained2d(not_pruned[k][0], titem, 0, k)) {
				/* then titem is "bigger" and "more constrained" */
	TAGGED wit = MINWIT2D(titem,k);
	not_pruned[k][0] = titem;
	/* Any invalid witnesses must be made invalid for ALL items,
	   so that they can be quickly copied to dominated items. */
	if (Tlt(wit,EST2D(titem,1-k)) ||
	    Tgt(wit,LST2D(titem,1-k)) ||
	    !fd_member(wit,FDSET2D(titem,1-k)))
	  MINWIT2D(titem,k) = TaggedLow;
      }
      moved = FALSE;
#endif

      /* pruning max(X) */
    prune_max:
      if (est[k]==lst[k])
	goto next_dim;

#if DOMINATION
      /* it seems better to do this _before_ feasible_maxwit() */
      if (!(flags & 0xe) &&
	  dominates2d(not_pruned[k][1], titem, 1, k)) {
	MAXWIT2D(titem,k) = MAXWIT2D(not_pruned[k][1],k); /* maintain invariant! */
	continue;		/* titem will not be pruned either */
      }
#endif
      /* scan the start and end events for the first feasible max(X) */
      if (!feasible_maxwit(nt,titem,dur,est,lct,k)) {
	/* dichotomic search for first end event whose start is after X */
	/* [**] NOTE: this does not filter out all events that are after X! */
	je = 0;
	sup = nsources;
	while (je<sup) {
	  mid = (je+sup)>>1;
	  if (pdata->event.min_start[k][(pdata->end_event[k][mid])] <= lct[k])
	    je = mid+1;
	  else
	    sup = mid;
	}
	je = prev_end2d(je,titem,dur,est,lct,lct[k],est[k],k,&next_end);
	/* dichotomic search for first start event that is after X */
	js = 0;
	sup = nsources;
	while (js<sup) {
	  mid = (js+sup)>>1;
	  if (pdata->event.start[k][(pdata->start_event[k][mid])] <= lct[k])
	    js = mid+1;
	  else
	    sup = mid;
	}
	js = prev_start2d(js,titem,dur,est,lct,k,&next_start);
	fd.profile = profile_init_complement(CTagToCar(tdom),CTagToCdr(tdom));
	current = lst[k];
	lct0 = lct[k];
	while (current >= est[k]) {
	  if (next_end >= current) {
	    EVENT ev = pdata->end_event[k][je-1];
	    /* trace_orig(titem,ev,current,k,-1,1); */
	    fd.profile = profile_update(fd.profile,
				     pdata->event.start[1-k][ev]-dur[1-k],
				     pdata->event.end[1-k][ev]+1,
				     1);
	    je = prev_end2d(je-1,titem,dur,est,lct,lct0,est[k],k,&next_end);
	  } else if (next_start > current) {
	    EVENT ev = pdata->start_event[k][js-1];
	    /* trace_orig(titem,ev,current,k,-1,-1); */
	    fd.profile = profile_update(fd.profile,
				     pdata->event.start[1-k][ev]-dur[1-k],
				     pdata->event.end[1-k][ev]+1,
				     -1);
	    js = prev_start2d(js-1,titem,dur,est,lct,k,&next_start);
	  } else if (!profile_zero_at(fd.profile, est[1-k], lst[1-k]+1, &wit)) {
	    current = next_start-1;
	  } else if (current < lst[k]) {
	    if (!prune_item2d(titem, EST2D(titem,k), current, k))
	      return FALSE;
	    previous = current;
	    lst[k] = current = LST2D(titem,k); /* refresh */
	    lct[k] = lst[k] + dur[k];
	    *change = TRUE;
#if DOMINATION
	    moved = TRUE;
#endif
	    if (previous==current) break; /* current is feasible */
	  }
	  else break;		/* current is feasible */
	}
	profile_dispose(fd.profile);
	if (current < est[k])
	  return FALSE;
	else
	  MAXWIT2D(titem,k) = MakeSmall(wit);
      }
    next_dim:
#if DOMINATION
      if (!(flags & 0xe) && !moved &&
	  less_constrained2d(not_pruned[k][1], titem, 1, k)) {
				/* then titem is "bigger" and "more constrained" */
	TAGGED wit = MAXWIT2D(titem,k);
	not_pruned[k][1] = titem;
	/* Any invalid witnesses must be made invalid for ALL items,
	   so that they can be quickly copied to dominated items. */
	if (Tlt(wit,EST2D(titem,1-k)) ||
	    Tgt(wit,LST2D(titem,1-k)) ||
	    !fd_member(wit,FDSET2D(titem,1-k)))
	  MAXWIT2D(titem,k) = TaggedLow;
      }
      moved = FALSE;
#endif
    }
  next_target:
    ;
  }
  return TRUE;
}


#if INITIAL_CHECK
/* New check algorithm.
   Form L1: list of starts of compulsory areas.
   Form L2: list of ends of compulsory areas.
   Sweep the lists, and fail if there is any overlap.
   Finally, for all ground targets, remove TARGET property.
*/
static BOOL disjoint2_check MAGIC (HIDDEN_PROTO
				   int nsources, BOOL incremental)
{
  struct disjoint2_data *pdata = fd.gdata;
  BOOL ok = FALSE;
  int i, k, js, je;
  long next_start, next_end, ymin, ymax;
  EVENT *queues = pdata->start_event[0];
  EVENT *queuee = pdata->end_event[0];
  
  for (i=0; i<nsources; i++) {
    SHIFTED_ITEM src = pdata->source[i];
    ITEM sitem = pdata->shifted_item.item[src];
    EVENT ev = i;
    
    pdata->event.item[ev] = sitem;
    queues[i] = queuee[i] = ev;
    for (k=0; k<2; k++) {
      pdata->event.start[k][ev] = LST2D(sitem,k);
      pdata->event.end[k][ev] = ECT2D(sitem,k);
    }
  }
  
  qsort_asc_start0(queues, nsources);
  qsort_asc_end0(queuee, nsources);
  
  fd.profile = empty_profile();
  js = je = 0;
  next_start = js<nsources ? pdata->event.start[0][(queues[js])] : CLPFD_MAXINT;
  next_end   = je<nsources ? pdata->event.end[0][(queuee[je])] : CLPFD_MAXINT;
  while (js<nsources || je<nsources) {
    if (next_start<next_end) {	/* prefer end events if it's a tie */
      EVENT ev = queues[js];
      
      ymin = pdata->event.start[1][ev];
      ymax = pdata->event.end[1][ev];
      if (profile_nonzero(fd.profile,ymin,ymax))
	goto fail;
      fd.profile = profile_update(fd.profile,ymin,ymax,1);
      js++;
      next_start = js<nsources ? pdata->event.start[0][(queues[js])] : CLPFD_MAXINT;
    } else {
      EVENT ev = queuee[je];
      
      ymin = pdata->event.start[1][ev];
      ymax = pdata->event.end[1][ev];
      fd.profile = profile_update(fd.profile,ymin,ymax,-1);
      je++;
      next_end   = je<nsources ? pdata->event.end[0][(queuee[je])] : CLPFD_MAXINT;
    }
  }

  ok = TRUE;

#if DECOMPOSITION
  /* Finally, for all ground targets, remove TARGET property. */
  /* Only in non-incrementalal case, lest it break the invariant! */
  if (!incremental)
    for (i=0; i<nsources; i++) {
      SHIFTED_ITEM src = pdata->source[i];
      ITEM item = pdata->shifted_item.item[src];
    
      if ((STATUS(item) & STATUS_TARGET) &&
	  MIN2D(item,0)==MAX2D(item,0) &&
	  DUR2D(item,0)==DURmax2D(item,0) &&
	  MIN2D(item,1)==MAX2D(item,1) &&
	  DUR2D(item,1)==DURmax2D(item,1))
	STATUS(item) &= ~STATUS_TARGET;
    }
#endif

 fail:
  profile_dispose(fd.profile);
  return ok;
}
#endif

#if DECOMPOSITION
static void decompose2d MAGIC (HIDDEN_PROTO
			       int nsources, int ntargets, BOOL incremental)
{
  struct disjoint2_data *pdata = fd.gdata;
  int complete = ((pdata->flags & 0x1) && !incremental);
  long ymin, ymax, next_start, next_end;
  long est[2] = {CLPFD_MAXINT,CLPFD_MAXINT};
  long lct[2] = {-CLPFD_MAXINT,-CLPFD_MAXINT};
  int js, je, i, j, k, top=0;
  int unmarked = ntargets;
  EVENT ev;
  EVENT *queues = pdata->start_event[0];
  EVENT *queuee = pdata->end_event[0];
  EVENT *stack = (EVENT *)pdata->sortarr + pdata->nitems;
  
  for (i=0; i<nsources; i++) {
    SHIFTED_ITEM src = pdata->source[i];
    ITEM sitem = pdata->shifted_item.item[src];

    ev = i;
    pdata->event.item[ev] = sitem;
    queues[i] = queuee[i] = ev;
    for (k=0; k<2; k++) {
      pdata->event.start[k][ev] = EST2D(sitem,k) + pdata->shifted_item.amount[k][src] - pdata->maxmargin[k];
      pdata->event.end[k][ev] = LCT2Dmax(sitem,k) + pdata->shifted_item.amount[k][src] + pdata->maxmargin[k];
      if ((STATUS(sitem)&(STATUS_SOURCE+STATUS_TARGET)) ==
	  (STATUS_SOURCE+STATUS_TARGET)) {
	if (est[k]>pdata->event.start[k][ev]) est[k] = pdata->event.start[k][ev];
	if (lct[k]<pdata->event.end[k][ev]) lct[k] = pdata->event.end[k][ev];
      }
    }
  }

  /* forget sources that can no longer prune */
  for (i=0; i<ntargets; i++) {
    ITEM item = TARGET(i);
    
    if (!(STATUS(item)&STATUS_TARGET) &&
	(LCT2Dmax(item,0)<=est[0] || lct[0]<=EST2D(item,0) ||
	 LCT2Dmax(item,1)<=est[1] || lct[1]<=EST2D(item,1))) {
      --unmarked;
      STATUS(item) &= ~STATUS_SOURCE;
    } else if (!complete)
      STATUS(item) |= STATUS_MARKED;
  }

  if (!complete)
    return;
  
  /* forget all items that can't interact */

  qsort_asc_start0(queues, nsources);
  qsort_asc_end0(queuee, nsources);
  
  fd.profile = empty_profile();
  /* Phase 1: find all potential overlaps, except when one box includes
     another one (in the X dimension).
  */
  js = je = 0;
  next_start = js<nsources ? pdata->event.start[0][(queues[js])] : CLPFD_MAXINT;
  next_end   = je<nsources ? pdata->event.end[0][(queuee[je])] : CLPFD_MAXINT;
  while (unmarked>0 && (js<nsources || je<nsources)) {
    if (next_start<next_end) {	/* prefer end events if it's a tie */
      ev = queues[js];
      ymin = pdata->event.start[1][ev];
      ymax = pdata->event.end[1][ev];
      if (!MarkedItem(ev) && profile_nonzero(fd.profile,ymin,ymax)) {
	MarkItem(ev);
      }
      fd.profile = profile_update(fd.profile,ymin,ymax,1);
      js++;
      next_start = js<nsources ? pdata->event.start[0][(queues[js])] : CLPFD_MAXINT;
    } else {
      ev = queuee[je];
      ymin = pdata->event.start[1][ev];
      ymax = pdata->event.end[1][ev];
      fd.profile = profile_update(fd.profile,ymin,ymax,-1);
      if (!MarkedItem(ev) && profile_nonzero(fd.profile,ymin,ymax)) {
	MarkItem(ev);
      }
      je++;
      next_end   = je<nsources ? pdata->event.end[0][(queuee[je])] : CLPFD_MAXINT;
    }
  }
  profile_dispose(fd.profile);

  /* Phase 2: find the cases when one unmarked box includes
     a marked one (in the X dimension).

     U = the empty set.

     If START(unmarked(u)) is next, add u to U.
     If START(marked(m)) is next, for each u in U:
       if m and u overlap in the Y dimension,
         mark u and remove it from U.
     If END(unmarked(u)) is next, remove u from U.
     If END(marked(m)) is next, do nothing.
  */
  js = je = 0;
  next_start = js<nsources ? pdata->event.start[0][(queues[js])] : CLPFD_MAXINT;
  next_end   = je<nsources ? pdata->event.end[0][(queuee[je])] : CLPFD_MAXINT;
  while (unmarked>0 && (js<nsources || je<nsources)) {
    if (next_start<next_end) {	/* prefer end events if it's a tie */
      ev = queues[js];
      if (!MarkedItem(ev))
        stack[top++] = ev;
      else {
	for (i=j=0; i<top; i++)
	  if (pdata->event.start[1][(stack[i])]<pdata->event.end[1][ev] && pdata->event.end[1][(stack[i])]>pdata->event.start[1][ev]) {
	    --unmarked;
	    STATUS(pdata->event.item[stack[i]]) |= STATUS_MARKED;
	  } else stack[j++] = stack[i];
	top = j;
      }
      js++;
      next_start = js<nsources ? pdata->event.start[0][(queues[js])] : CLPFD_MAXINT;
    } else {
      ev = queuee[je];
      if (!MarkedItem(ev)) {
	for (i=top; stack[i-1]!=ev; --i)
	  ;
	stack[i-1] = stack[--top];
      }
      je++;
      next_end   = je<nsources ? pdata->event.end[0][(queuee[je])] : CLPFD_MAXINT;
    }
  }
  /**** for debugging
  printf("bounding_boxes(");
  for (i=0; i<nsources; i++) {
    ev = event2d+i;
    printf("%sbb(%d,%d,%d,%d,%d)",
	   (i==0 ? "[" : ","),
	   pdata->event.start[0][ev], pdata->event.end[0][ev], pdata->event.start[1][ev], pdata->event.end[1][ev], ev->STATUS(item)&STATUS_MARKED);
  }
  printf("]).\n");
  ****/
}
#endif


static int collect_source2d MAGIC (HIDDEN_PROTO BOOL filtering)
{
  struct disjoint2_data *pdata = fd.gdata;
  long lbound[2], rbound[2], shift[2];
  int kmin = (pdata->flags&0x2) ? 0 : 1;
  int kmax = (pdata->flags&0x2) ? 3 : 2;
  int lmin = (pdata->flags&0x4) ? 0 : 1;
  int lmax = (pdata->flags&0x4) ? 3 : 2;
  int n = pdata->ntargets + pdata->nsources;
  int flags = pdata->flags;
  int i, j, k, l;

  if (filtering)
    for (j=0; j<2; j++) {
      lbound[j] = pdata->lborder[j]+pdata->maxdur[j]+pdata->maxmargin[j];
      rbound[j] = pdata->rborder[j]-pdata->maxmargin[j];
      shift[j]  = pdata->rborder[j]-pdata->lborder[j];
    }
    
  for (i=j=0; i<n; i++) {
    ITEM var = TARGET(i);
    
    if (filtering &&
	(SLACK2D(var,0)+2 > pdata->maxdur[0]+2*pdata->maxmargin[0] ||
	 SLACK2D(var,1)+2 > pdata->maxdur[1]+2*pdata->maxmargin[1]))
      continue;		/* the item is too loose and cannot prune anything */
    if (filtering && !(flags & 0x8) && (DUR2D(var,0)==0 || DUR2D(var,1)==0))
      continue;
    for (k=kmin; k<kmax; k++) {
      if (k==1 ? TRUE :
	  !filtering ? TRUE :
	  k==0 ? (ECT2D(var,0)>rbound[0]) :
	  LST2D(var,0)<lbound[0])
	for (l=lmin; l<lmax; l++)
	  if (l==1 ? TRUE :
	      !filtering ? TRUE :
	      l==0 ? (ECT2D(var,1)>rbound[1]) :
	      LST2D(var,1)<lbound[1]) {
	    pdata->source[j] = j;
	    pdata->shifted_item.item[j] = var;
	    pdata->shifted_item.amount[0][j] = (k-1)*shift[0];
	    pdata->shifted_item.amount[1][j] = (l-1)*shift[1];
	    j++;
	  }
    }
  }
  return j;
}


#if DBG > 1
/* Invariant: for every TARGET t and its witness wit,
   either wit is invalid wrt. t,
   or     wit is valid wrt. every SOURCE (including TARGETS) */
static void invariants2d MAGIC (HIDDEN_PROTO_VOID)
{
  struct disjoint2_data *pdata = fd.gdata;
  int flags = pdata->flags;
  int inv = 0;
  int i, k, culprit=TERMIN;
  long min_start, max_end;
  long start[2], end[2];
  int collected = collect_source2d(FALSE);
  int nsources = 0;

  for (i=0; i<collected; i++) {
    SHIFTED_ITEM src = pdata->source[i];
    ITEM titem;
    EVENT ev;

    titem = pdata->shifted_item.item[src];
    /***
    if (STATUS(titem) & STATUS_TARGET)
      continue;
    ***/
    ev = nsources++;
    RELATIVE_FORBIDDEN_REGION2D(src, start, end);
    if (flags & 0x8) {
      int hits =
	extend_forbidden_region2d(pdata->target[0],
				  pdata->shifted_item.item[src],
				  start, end);
      if (hits==0 &&
	  (DUR2D(pdata->target[0],0)==0 ||
	   DUR2D(pdata->target[0],1)==0))
	start[0] = end[0] = start[1] = end[1] = CLPFD_MAXINT;
    }
    pdata->event.item[ev] = titem;
    for (k=0; k<2; k++) {
      pdata->start_event[k][ev] = pdata->end_event[k][ev] = ev;
      pdata->event.start[k][ev] = start[k];
      pdata->event.end[k][ev] = end[k];
    }
  }
  if (nsources==0)
    goto ret;
  qsort_asc_start0(pdata->start_event[0], nsources);
  qsort_asc_end0(pdata->end_event[0], nsources);
  qsort_asc_start1(pdata->start_event[1], nsources);
  qsort_asc_end1(pdata->end_event[1], nsources);
  for (k=0; k<2; k++) {
    max_end = pdata->event.end[k][(pdata->start_event[k][0])];
    min_start = pdata->event.start[k][(pdata->end_event[k][nsources-1])];
    for (i=0; i<nsources; i++) {
      if (max_end < pdata->event.end[k][(pdata->start_event[k][i])])
	max_end = pdata->event.end[k][(pdata->start_event[k][i])];
      pdata->event.max_end[k][(pdata->start_event[k][i])] = max_end;
      if (min_start > pdata->event.start[k][(pdata->end_event[k][nsources-i-1])])
	min_start = pdata->event.start[k][(pdata->end_event[k][nsources-i-1])];
      pdata->event.min_start[k][(pdata->end_event[k][nsources-i-1])] = min_start;
    }
  }
  for (i=0; i<pdata->ntargets; i++) {
    ITEM titem = pdata->target[i];
    long est[2];
    long lst[2];
    long dur[2];
    long lct[2];
    
    if (!(STATUS(titem) & STATUS_TARGET))
      continue;
    for (k=0; k<2; k++) {
      est[k] = EST2D(titem,k);
      lst[k] = LST2D(titem,k);
      dur[k] = DUR2D(titem,k);
      lct[k] = lst[k]+dur[k];
    }
    for (k=0; k<2; k++) {
      long next_start, next_end, wit;
      int js, je, mid, sup;
      
      /* checking minwit(X) */

      wit = GetSmall(MINWIT2D(titem,k));
      next_start = est[k]+1;
      if (!(wit < est[1-k] || wit > lct[1-k]-dur[1-k] || /* NOT covered by next line */
	    !fd_member(MINWIT2D(titem,k),FDSET2D(titem,1-k)))) {
	/* dichotomic search for first start event whose end is not before X */
	/* [*] NOTE: this does not filter out all events that are before X! */
	js = 0;
	sup = nsources;
	while (js<sup) {
	  mid = (js+sup)>>1;
	  if (pdata->event.max_end[k][(pdata->start_event[k][mid])] < est[k])
	    js = mid+1;
	  else
	    sup = mid;
	}
	js = next_start2d(js,nsources,titem,dur,est,lct,est[k],est[k],k,&next_start);
	while (next_start <= est[k]) {
	  EVENT ev = pdata->start_event[k][js];

	  culprit = (int)pdata->event.item[ev];
	  if (wit >= pdata->event.start[1-k][ev]-dur[1-k] &&
	      wit <= pdata->event.end[1-k][ev])
	    break;
	  js = next_start2d(js+1,nsources,titem,dur,est,lct,est[k],est[k],k,&next_start);
	}
      }
      if (next_start <= est[k]) {
	long xor = (k==0 ? 0 : (est[k]^wit));
	inv++;
	printf("* invalid minwit for item=%d dim=%d point=(%ld,%ld) item(FR)=%d\n",
	       (int)titem, k, est[k]^xor, wit^xor, culprit);
      }

      /* checking maxwit(X) */

      if (est[k]==lst[k])
	goto next_dim;
      wit = GetSmall(MAXWIT2D(titem,k));
      next_end = lst[k]-1;
      if (!(wit < est[1-k] || wit > lct[1-k]-dur[1-k] || /* NOT covered by next line */
	    !fd_member(MAXWIT2D(titem,k),FDSET2D(titem,1-k)))) {
	/* dichotomic search for first end event whose start is after X */
	/* [**] NOTE: this does not filter out all events that are after X! */
	je = 0;
	sup = nsources;
	while (je<sup) {
	  mid = (je+sup)>>1;
	  if (pdata->event.min_start[k][(pdata->end_event[k][mid])] <= lct[k])
	    je = mid+1;
	  else
	    sup = mid;
	}
	je = prev_end2d(je,titem,dur,est,lct,lct[k],lst[k],k,&next_end);
	while (next_end >= lst[k]) {
	  EVENT ev = pdata->end_event[k][je-1];

	  culprit = (int)pdata->event.item[ev];
	  if (wit >= pdata->event.start[1-k][ev]-dur[1-k] &&
	      wit <= pdata->event.end[1-k][ev])
	    break;
	  je = prev_end2d(je-1,titem,dur,est,lct,lct[k],lst[k],k,&next_end);
	}
      }
      if (next_end >= lst[k]) {
	long xor = (k==0 ? 0 : (lst[k]^wit));
	inv++;
	printf("* invalid maxwit for item=%d dim=%d point=(%ld,%ld) item(FR)=%d\n",
	       (int)titem, k, lst[k]^xor, wit^xor, culprit);
      }
    next_dim:
      ;
    }
  }
 ret:
  if (inv==0)
    ; /* printf("%% all witnesses valid\n"); */
  else
    printf("* %d invalid witnesses found\n", inv);
}
#endif


/* '$fd_disjoint2'(+State0, -State, -Actions) :-
   State0 is f(N,Opt,Items,NTargets,NSources,Handle,Stamp),
   State  similarly,
   Actions is a list of prunings etc.
   Opt = opt(Flags,XMin,XMax,YMin,YMax,Margins) where
               0x1 denotes decomposition
               0x2 denotes wrap-around over the interval XMin..XMax
               0x4 denotes wrap-around over the interval YMin..YMax
               0x8 denotes existence of margins
               0x10 denotes global reasoning (task intervals light)
               0x20 denotes lean (avoid heavy work in incremental case)
               0x40 denotes sweep synchronization, incompatible with margins
               Margins = list of margin(Type1,Type2,XMarg,YMarg) = list of extra margins
   Items is a list of N items item(X,XMut,XDur,XDurMut,Y,YMut,YDur,YDurMut,Type).
*/
void SPCDECL
prolog_fd_disjoint2 MAGIC (HIDDEN_PROTO
			   SP_term_ref State0,
			   SP_term_ref State,
			   SP_term_ref Actions)
{
  WAMENV;
  int create_disjoint2_ctr;	/* TRUE iff create constraint for the first time */
  int i, j, k, ent = -1;	/* disentailed */
  int nitems;			/* #items */
  int nshifted_items;		/* #shifted items if wrap-around */
  int nmargs = 0;
  int nmargs_aligned = 0;
  int flags;
  int nactive_items;		/* caches pdata->ntargets + pdata->nsources */
  long l, total_size, state_stamp;
  TAGGED tmp, opt, item, items, *arg;
  TAGGED handle;
  BOOL change;
  BOOL committed;
  BOOL incremental;
#if INCREMENTAL > 1
  int pruned1=1, pruned2=2;	/* must be != and != TERMIN */
#endif
  struct disjoint2_data *pdata;
  char *ptr;
  
  w->numstack_end = NULL;
  init_profile();

/*    X(0) = RefTerm(State0); */
  (void)State0;
  X(2) = atom_nil;		/* actions list */
  RefTerm(State) = unify_output_state(w,&handle,&state_stamp,&committed);
  create_disjoint2_ctr = IsVar(handle);

  if (!IsVar(handle)) {		/* got [Flag | '$free'(Ptr)] */
    pdata = Pdata(struct disjoint2_data,handle);
    fd.gdata = pdata;
    flags = pdata->flags;
    nitems = pdata->nitems;
    nshifted_items = pdata->nshifted_items;
  } else {			/* build persistent state */
				/* compute flags, nitems, nshifted_items */
    DerefArg(tmp,X(0),1);		/* get N */
    nshifted_items = nitems = GetSmall(tmp);
    DerefArg(opt,X(0),2);		/* get Opt */
    DerefArg(tmp,opt,1);
    flags = GetSmall(tmp);
    if (flags & 0x2)
      nshifted_items *= 3;
    if (flags & 0x4)
      nshifted_items *= 3;
    if (flags & 0x8) {
      DerefArg(items,opt,6);
      while (TagIsLST(items)) {
	nmargs++;
	DerefCar(item,items);
	DerefCdr(items,items);
      }
      for (nmargs_aligned=1; nmargs_aligned<nmargs; nmargs_aligned <<= 1)
	;
    }

    total_size =
      ((25*nitems + 25*nshifted_items + 5*nmargs + nmargs_aligned)<<LogSizeOfWord);
    pdata = Palloc(struct disjoint2_data, total_size, handle);
    fd.gdata = pdata;
    ptr = (char *)(pdata+1);
    pdata->sortarr = (ITEM *)ptr;
    ptr = (char *)(pdata->sortarr+4*nitems);
    pdata->target = (ITEM *)ptr;
    ptr = (char *)(pdata->target+nitems);
    pdata->source = (ITEM *)ptr;
    ptr = (char *)(pdata->source+nshifted_items);
    for (k=0; k<2; k++) {
      pdata->start_event[k] = (ITEM *)ptr;
      ptr = (char *)(pdata->start_event[k]+nshifted_items);
      pdata->end_event[k] = (ITEM *)ptr;
      ptr = (char *)(pdata->end_event[k]+nshifted_items);
      pdata->start_event_t[k] = (ITEM *)ptr;
      ptr = (char *)(pdata->start_event_t[k]+nshifted_items);
      pdata->end_event_t[k] = (ITEM *)ptr;
      ptr = (char *)(pdata->end_event_t[k]+nshifted_items);
    }
    pdata->margtab = (MARGIN *)ptr;
    ptr = (char *)(pdata->margtab+nmargs_aligned);
    for (k=0; k<2; k++) {
      pdata->item.min[k] = (TAGGED *)ptr;
      ptr = (char *)(pdata->item.min[k]+nitems);
      pdata->item.max[k] = (TAGGED *)ptr;
      ptr = (char *)(pdata->item.max[k]+nitems);
      pdata->item.fdset[k] = (TAGGED *)ptr;
      ptr = (char *)(pdata->item.fdset[k]+nitems);
      pdata->item.mindur[k] = (long *)ptr;
      ptr = (char *)(pdata->item.mindur[k]+nitems);
      pdata->item.maxdur[k] = (long *)ptr;
      ptr = (char *)(pdata->item.maxdur[k]+nitems);
      pdata->item.minwit[k] = (TAGGED *)ptr;
      ptr = (char *)(pdata->item.minwit[k]+nitems);
      pdata->item.maxwit[k] = (TAGGED *)ptr;
      ptr = (char *)(pdata->item.maxwit[k]+nitems);
      pdata->item.pruned_at[k] = (long *)ptr;
      ptr = (char *)(pdata->item.pruned_at[k]+nitems);
    }
    pdata->item.type = (TAGGED *)ptr;
    ptr = (char *)(pdata->item.type+nitems);
    pdata->item.status = (long *)ptr;
    ptr = (char *)(pdata->item.status+nitems);
    pdata->item.next = (ITEM *)ptr;
    ptr = (char *)(pdata->item.next+nitems);
    pdata->item.next_area = (ITEM *)ptr;
    ptr = (char *)(pdata->item.next_area+nitems);
    pdata->shifted_item.item = (ITEM *)ptr;
    ptr = (char *)(pdata->shifted_item.item+nshifted_items);
    for (k=0; k<2; k++) {
      pdata->shifted_item.amount[k] = (long *)ptr;
      ptr = (char *)(pdata->shifted_item.amount[k]+nshifted_items);
    }
    pdata->event.item = (ITEM *)ptr;
    ptr = (char *)(pdata->event.item+nshifted_items);
    for (k=0; k<2; k++) {
      pdata->event.start[k] = (long *)ptr;
      ptr = (char *)(pdata->event.start[k]+nshifted_items);
      pdata->event.end[k] = (long *)ptr;
      ptr = (char *)(pdata->event.end[k]+nshifted_items);
      pdata->event.min_start[k] = (long *)ptr;
      ptr = (char *)(pdata->event.min_start[k]+nshifted_items);
      pdata->event.max_end[k] = (long *)ptr;
      ptr = (char *)(pdata->event.max_end[k]+nshifted_items);
      pdata->event.min_start_t[k] = (long *)ptr;
      ptr = (char *)(pdata->event.min_start_t[k]+nshifted_items);
      pdata->event.max_end_t[k] = (long *)ptr;
      ptr = (char *)(pdata->event.max_end_t[k]+nshifted_items);
    }
    pdata->margin.type1 = (TAGGED *)ptr;
    ptr = (char *)(pdata->margin.type1+nmargs);
    pdata->margin.type2 = (TAGGED *)ptr;
    ptr = (char *)(pdata->margin.type2+nmargs);
    for (k=0; k<2; k++) {
      pdata->margin.amount[k] = (long *)ptr;
      ptr = (char *)(pdata->margin.amount[k]+nmargs);
    }
    pdata->margin.next = (MARGIN *)ptr;
    ptr = (char *)(pdata->margin.next+nmargs);
    if (ptr != (char *)(pdata+1)+total_size)
      printf("SHOULDN'T HAPPEN: expected ptr=0x%p, got 0x%p\nitems",
	     (char *)(pdata+1)+total_size, ptr);

    pdata->destructor = disjoint2_destructor;
    FD_STORE_SPENV(pdata->spenv);
    pdata->refbase = SP_alloc_term_refs(8*nitems);
    pdata->stamp = state_stamp-1; /* DON'T trust initially */
    pdata->nitems = nitems;
    pdata->nshifted_items = nshifted_items;
#if CSTATE_ONLY
    pdata->ntargets = nitems;
    pdata->nsources = 0;
    pdata->ndone = 0;
#endif
    pdata->nmargs = nmargs;
    pdata->nmargs_aligned = nmargs_aligned;
    pdata->flags = flags;
    for (k=0; k<2; k++) {
      pdata->lborder[k] = 0;
      pdata->rborder[k] = 0;
      pdata->maxmargin[k] = 0;
      pdata->maxdur[k] = 0;
      if (flags & (0x2<<k)) {
	DerefArg(tmp,opt,(k<<1)+2);
	pdata->lborder[k] = GetSmall(tmp);
	DerefArg(tmp,opt,(k<<1)+3);
	pdata->rborder[k] = GetSmall(tmp);
      }
    }
				/* wrap-around can triple the #forbidden regions */

    if (flags & 0x8) {		/* build margin table */
      DerefArg(items,opt,6);
      i = 0;
      while (TagIsLST(items)) {
	DerefCar(item,items);
	DerefCdr(items,items);
	DerefArg(pdata->margin.type1[i],item,1);
	DerefArg(pdata->margin.type2[i],item,2);
	for (k=0; k<2; k++) {
	  DerefArg(tmp,item,k+3);
	  pdata->margin.amount[k][i] = l =
	    (TagIsSmall(tmp) ? GetSmall(tmp) : CLPFD_MAXINT>>1); /* TODO: cheating */
	  if (pdata->maxmargin[k]<l) pdata->maxmargin[k] = l;
	}
	i++;
      }
      pdata->nmargs = i;
      for (nmargs_aligned=1; nmargs_aligned<i; nmargs_aligned <<= 1)
	;
      for (i=0; i<nmargs_aligned; i++)
	pdata->margtab[i] = TERMIN;
      for (i=0; i<nmargs; i++) {
	TAGGED key = ((pdata->margin.type1[i]^pdata->margin.type2[i])>>LogSizeOfWord) & (nmargs_aligned-1);
	MARGIN m = pdata->margtab[key];

	pdata->margtab[key] = i;
	pdata->margin.next[i] = m;
      }
    }
				/* transfer all the items */
    DerefArg(items,X(0),3);
    for (i=0; i<nitems; i++) {
      SP_term_ref ref = pdata->refbase+(i<<3);

      TARGET(i) = i;
      DerefCar(item,items);
      DerefCdr(items,items);
      j = 0;
      for (k=0; k<2; k++) {
	DerefArg(tmp,item,j+1);	/* start's domain variable */
	RefTerm(ref+1) = tmp;
	DerefArg(tmp,item,j+2);	/* start's attribute */
	RefTerm(ref) = tmp;
	DerefArg(tmp,item,j+3);	/* duration's domain variable */
	RefTerm(ref+3) = tmp;
	DerefArg(tmp,item,j+4);	/* duration's attribute */
	RefTerm(ref+2) = tmp;
	ref += 4;
	j += 4;
	MINWIT2D(i,k) = MakeSmall(1); /* TENTATIVE */
	MAXWIT2D(i,k) = MakeSmall(1); /* TENTATIVE */
      }
      DerefArg(TYPE(i),item,9);	/* type */
      STATUS(i) = (STATUS_SOURCE+STATUS_TARGET)<<4;
    }
  }

				/* RESUME HERE */

#if !CSTATE_ONLY
  DerefArg(tmp,X(0),4);
  pdata->ntargets = GetSmall(tmp);
  DerefArg(tmp,X(0),5);
  pdata->nsources = GetSmall(tmp);
  nactive_items = pdata->nsources + pdata->ntargets;
#endif
  if (state_stamp != pdata->stamp) { /* trust nothing */
    incremental = FALSE;
#if CSTATE_ONLY
    pdata->ntargets = nitems - pdata->ndone;
    pdata->nsources = 0;
    nactive_items = pdata->ntargets;
#endif
#if INCREMENTAL
    for (k=0; k<2; k++) {
      pdata->bbmin[k] = -CLPFD_MAXINT;
      pdata->bbmax[k] = CLPFD_MAXINT;
    }
#endif
    for (i=0; i<pdata->ntargets; i++) {
      ITEM it = TARGET(i);
      
      PRUNED_AT2D(it,0) = -1;
      PRUNED_AT2D(it,1) = -1;
      STATUS(it) |= STATUS(it)>>4;
      if (STATUS(it) & STATUS_TARGET)
	for (k=0; k<2; k++) {
	  SP_term_ref ref = pdata->refbase+(it<<3)+(k<<2);
      
	  tmp = RefTerm(ref);	/* start's attribute */
	  DerefAttribute(tmp,tmp); /* dom/4 term */
	  arg = TagToArg(tmp,0);
	  FDSET2D(it,k) = arg[1];
	  MIN2D(it,k) = arg[2];
	  MAX2D(it,k) = arg[3];
	  tmp = RefTerm(ref+2);	/* duration's attribute */
	  DerefAttribute(tmp,tmp); /* dom/4 term */
	  arg = TagToArg(tmp,0);
	  DUR2D(it,k) = GetSmall(arg[2]);
	  DURmax2D(it,k) = GetSmall(arg[3]);
	  PRUNED_AT2D(it,k) = 0;
	}
    }
    for (; i<nactive_items; i++) { /* 3.9 */
      ITEM it = TARGET(i);

      STATUS(it) |= STATUS_SOURCE;
    }
    if (create_disjoint2_ctr) {             /* if we are on the first call which create the ctr  */
     check_if_use_sweep_synchro(pdata);     /* check if can use sweep synchro.and create frame   */
    }                                       /* if it is the case                                 */
  } else {			/* check if anything was pruned */
    incremental = TRUE;
#if INCREMENTAL
    if (!(flags & 0x2)) {	/* wrap-around => use maximal bounding box */
      pdata->bbmin[0] = CLPFD_MAXINT;
      pdata->bbmax[0] = -CLPFD_MAXINT;
    }
    if (!(flags & 0x4)) {	/* wrap-around => use maximal bounding box */
      pdata->bbmin[1] = CLPFD_MAXINT;
      pdata->bbmax[1] = -CLPFD_MAXINT;
    }
#endif
#if INCREMENTAL > 1
    pruned1 = TERMIN;
#endif
    for (i=0; i<pdata->ntargets; i++) {
      ITEM it = TARGET(i);
      long at[] = {-1,-1};
      
      if (STATUS(it)&STATUS_TARGET) { /* always true? */
	for (k=0; k<2; k++) {
	  SP_term_ref ref = pdata->refbase+(it<<3)+(k<<2);
      
	  tmp = RefTerm(ref);	/* start's attribute */
	  DerefAttribute(tmp,tmp); /* dom/4 term */
	  arg = TagToArg(tmp,0);
	  FDSET2D(it,k) = arg[1];
	  if (MIN2D(it,k) != arg[2]) {
	    MIN2D(it,k) = arg[2];
	    MINWIT2D(it,k) = TaggedLow;	/* maintain invariant */
	    at[k] = 0;
	  }
	  if (MAX2D(it,k) != arg[3]) {
	    MAX2D(it,k) = arg[3];
	    MAXWIT2D(it,k) = TaggedLow;	/* maintain invariant */
	    at[k] = 0;
	  }
	  if (at[k]!=0 &&
	      CTagToCdr(arg[1])!=EmptySet &&
	      (!fd_member(MINWIT2D(it,1-k),arg[1]) ||
	       !fd_member(MAXWIT2D(it,1-k),arg[1]))) {
	    at[k] = 0;
	  }
	  tmp = RefTerm(ref+2);	/* duration's attribute */
	  DerefAttribute(tmp,tmp); /* dom/4 term */
	  arg = TagToArg(tmp,0);
	  if (DUR2D(it,k) != GetSmall(arg[2]) ||
	      DURmax2D(it,k) != GetSmall(arg[3])) {
	    DUR2D(it,k) = GetSmall(arg[2]);
	    DURmax2D(it,k) = GetSmall(arg[3]);
	    /* maintain invariant -- all (?) witnesses endangered */
	    MINWIT2D(it,1-k) = TaggedLow;
	    MAXWIT2D(it,1-k) = TaggedLow;
	    at[1-k] = 0;
	    MINWIT2D(it,k) = TaggedLow;
	    MAXWIT2D(it,k) = TaggedLow;
	    at[k] = 0;
	  }
	  PRUNED_AT2D(it,k) = at[k];
	}
#if INCREMENTAL
	if (at[0]==0 || at[1]==0) {
#if INCREMENTAL > 1
	  if (pruned1==TERMIN) pruned1 = it;
	  pruned2 = it;
#endif
	  for (k=0; k<2; k++) {
	    if (pdata->bbmin[k] > EST2D(it,k))
	      pdata->bbmin[k] = EST2D(it,k);
	    if (pdata->bbmax[k] < LCT2D(it,k))
	      pdata->bbmax[k] = LCT2D(it,k);
	  }
	}
#endif
      }
    }
#if INCREMENTAL
    for (k=0; k<2; k++)
      if ((flags & 0x8) && pdata->bbmax[k] < CLPFD_MAXINT) {
	pdata->bbmin[k] -= 2*pdata->maxmargin[k];
	pdata->bbmax[k] += 2*pdata->maxmargin[k];
      }
#endif
  }
  pdata->stamp = state_stamp+1;

  /* sort targets by decreasing duration[0]; refresh maxdur */
  
  qsort_desc_dur2d0(pdata->target, pdata->ntargets);

#if INITIAL_CHECK
  if (!(incremental && (flags & 0x20)) && /* not in the incremental case */
      !(flags & 0x8)) {		/* not applicable if there are margins */
				/* collect all sources&targets
				   that have compulsory parts */
    for (i=j=0; i<nactive_items; i++) {
      ITEM it = TARGET(i);
      
      if (/*!(STATUS(it) & STATUS_TARGET) || wrong */
	     SLACK2D(it,0) >= 0
	  || SLACK2D(it,1) >= 0
#if INCREMENTAL
	  || EST2D(it,0)>=pdata->bbmax[0]
	  || LCT2D(it,0)<=pdata->bbmin[0]
	  || EST2D(it,1)>=pdata->bbmax[1]
	  || LCT2D(it,1)<=pdata->bbmin[1]
#endif
	  )
	continue;		/* not target or no compulsory part */
      pdata->source[j] = j;
      pdata->shifted_item.item[j++] = it;
    }

    if (j>0 && !disjoint2_check(j,incremental))
      goto ret;
  }
#endif
  
  /* refresh maxdur; build linked list by descending area */
  
  for (k=0; k<2; k++) {
    pdata->maxdur[k] = 0;
  }
  pdata->items_by_area = TERMIN;
  for (i=pdata->ntargets-1; i>=0; i--) {
    ITEM item = TARGET(i);
    
    if (STATUS(item) & STATUS_TARGET) {
      NEXT_AREA(item) = pdata->items_by_area;
      pdata->items_by_area = item;
      for (k=0; k<2; k++) {
	l = DUR2D(item,k);
	if (pdata->maxdur[k]<l)
	  pdata->maxdur[k] = l;
      }
    }
  }

  /* initialize sortarr for use in edge finding */

  for (i=0; i<nactive_items; i++)
    pdata->sortarr[i] = TARGET(i);

  change = TRUE;
  pdata->iteration=1;
#if INCREMENTAL > 1
  if (pruned1==TERMIN) {
    ent = 0;
    goto ret;
  } else if (pruned1==pruned2 && !(flags & 0x1e)) {
    /* no margins, wrap-around, or global reasoning */
    /* special case of first part of disjoint2_filter() */
    long start[2], end[2], dur_threshold[2];
    ITEM *target0 = pdata->sortarr + 2*nitems;
    int tix = 0;

    BASIC_RELATIVE_FORBIDDEN_REGION2D(pruned1, start, end);
    for (k=0; k<2; k++)
      dur_threshold[k] = start[k]>end[k] ? start[k]-end[k] : 1;
    
    for (i=0; i<pdata->ntargets; i++) {
      ITEM titem = TARGET(i);
      
      if (titem==pruned1) {
	target0[tix++] = titem;
      } else if (!(STATUS(titem)&STATUS_TARGET))
	continue;
      else if (DUR2D(titem,0) < dur_threshold[0] ||
	       DUR2D(titem,1) < dur_threshold[1])
	continue;
      else if (!(EST2D(titem,0)<=end[0] &&
		 LST2D(titem,0)>=start[0]-DUR2D(titem,0) &&
		 EST2D(titem,1)<=end[1] &&
		 LST2D(titem,1)>=start[1]-DUR2D(titem,1)))
	continue;
      else
	target0[tix++] = titem;
    }
    change = FALSE;
    if (tix>0) {
      j = collect_source2d(TRUE);
      if (j>0)
	if (!disjoint2_filter(j,target0,tix,incremental,&change))
	  goto ret;
      pdata->iteration++;
    }
  }
#endif

  for (; change; pdata->iteration++) {
    change = FALSE;
    if (!(flags & 0x8)) { /* no margins */
      j = collect_source2d(TRUE);
      if (j>0)
	if (!disjoint2_filter(j,pdata->target,pdata->ntargets,
			      (incremental || pdata->iteration>1),
			      &change))
	  goto ret;
      if (pdata->synchro_table &&
	  !wake_sweep_synchro(pdata,
			      (incremental || pdata->iteration>1),
			      &change))
        goto ret;
    } else {			/* margins */
      int ti;

      for (ti=0; ti<pdata->ntargets; ti++) {
	ITEM titem = TARGET(ti);
	
	if (!(STATUS(titem)&STATUS_TARGET))
	  continue;
#if INCREMENTAL
	if (EST2D(titem,0)>=pdata->bbmax[0]
	   || LCT2D(titem,0)<=pdata->bbmin[0]
	   || EST2D(titem,1)>=pdata->bbmax[1]
	   || LCT2D(titem,1)<=pdata->bbmin[1])
	  continue;
#endif

	j = collect_source2d(TRUE);
	if (j>0)
	  if (!disjoint2_filter(j,pdata->target+ti,1,
				(incremental || pdata->iteration>1),
				&change))
	    goto ret;
      }
    }
    if (!change && !(incremental && (flags & 0x20)) && (flags & 0x10))
      for (k=0; k<2; k++)
	if (!edge_finding2d_up(&change,k) ||
	    !edge_finding2d_down(&change,k))
	  goto ret;
  }

#if DECOMPOSITION
  j = collect_source2d(FALSE);
  decompose2d(j,nactive_items,(incremental && (flags & 0x20)));

  for (i=0; i<nactive_items; i++) {
    ITEM it = TARGET(i);
    
    if (STATUS(it)&STATUS_MARKED) {
      STATUS(it) &= ~STATUS_MARKED;
    } else {
      STATUS(it) &= ~STATUS_SOURCE;
    }
  }
#endif

  ent = 1;
  for (i=0; i<pdata->ntargets; i++) {
    ITEM it = TARGET(i);
    SP_term_ref ref = pdata->refbase+(it<<3);

    if (STATUS(it)&STATUS_TARGET)
      ent = 0;
    for (k=0; k<2; k++) {
      if (PRUNED_AT2D(it,k)>0)
	request_tell_interval(w, RefTerm(ref), RefTerm(ref+1), MIN2D(it,k), MAX2D(it,k), 2, 3);
      ref += 4;
    }
  }
  if (ent) goto ret;

#if DECOMPOSITION

  /* partition into SOURCE+TARGET and SOURCE */

  {
    int delta;
    int inf = 0;
    int sup = pdata->ntargets - 1;
    ITEM held = TARGET(sup); /* sup is the hole */
    ITEM current = TARGET(inf);
    
    while (inf<=sup) {
      if (STATUS(current) & STATUS_TARGET) {
	TARGET(inf) = current;
	inf++;
	current = (inf>=sup ? held : TARGET(inf));
      } else {
	TARGET(sup) = current;
	sup--;
	current = (inf>=sup ? held : TARGET(sup));
      }
    }
    delta = pdata->ntargets - inf;
    pdata->ntargets -= delta;
    pdata->nsources += delta;
  }
  
  /* find the last real SOURCE */

  {
    int delta, sup;
    
    for (sup=nactive_items; sup>pdata->ntargets; --sup)
      if (STATUS(TARGET(sup-1)) & STATUS_SOURCE)
	break;
    delta = nactive_items - sup;
    pdata->nsources -= delta;
    nactive_items -= delta;
  }

  if (committed)
    for (i=pdata->ntargets; i<nactive_items; i++) {
      ITEM it = TARGET(i);
      
      STATUS(it) &= ~(STATUS_TARGET<<4);
    }
#endif
  
ret:
#if CSTATE_ONLY
  if (committed)
    pdata->ndone = nitems - nactive_items;
#else
  CTagToArg(X(0),4) = MakeSmall(pdata->ntargets);
  CTagToArg(X(0),5) = MakeSmall(pdata->nsources);
#endif
#if DBG > 1
  if (ent>=0)
    invariants2d();
#endif
  Pfree(ent==1,handle,pdata);
  RefTerm(Actions) = request_done(w, ent, 2, 3);
}
