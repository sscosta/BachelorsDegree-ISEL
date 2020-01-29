/* Copyright(C) 2000, Swedish Institute of Computer Science */

/****************************************************************

 cumulatives(TASKS,MACHINES,OPTIONS).
 Based on Nicolas Beldiceanu's algoritm which sweeps an interval at a time
 + frontier reasoning + generalization + task intervals.

 ****************************************************************/

#include "fd.h"

#define CSTATE_ONLY 0

#if MULTI_SP_AWARE
#if 0                           /* not redefined. It uses FD_SETUP_SPENV to access the SPEnv */
#define symmcum_destructor(A1) symmcum_destructor(HIDDEN_ARG, A1)
#endif

#define cmp_asc_eventc(A1,A2) cmp_asc_eventc(HIDDEN_ARG, A1,A2)
#define qsort_asc_eventcswap(A1,A2,A3,A4) qsort_asc_eventcswap(HIDDEN_ARG, A1,A2,A3,A4)
#define qsort_asc_eventcmed3(A1,A2,A3) qsort_asc_eventcmed3(HIDDEN_ARG, A1,A2,A3)
#define qsort_asc_eventc(A1,A2) qsort_asc_eventc(HIDDEN_ARG, A1,A2)
#define cmp_asc_eventnc(A1,A2) cmp_asc_eventnc(HIDDEN_ARG, A1,A2)
#define qsort_asc_eventncswap(A1,A2,A3,A4) qsort_asc_eventncswap(HIDDEN_ARG, A1,A2,A3,A4)
#define qsort_asc_eventncmed3(A1,A2,A3) qsort_asc_eventncmed3(HIDDEN_ARG, A1,A2,A3)
#define qsort_asc_eventnc(A1,A2) qsort_asc_eventnc(HIDDEN_ARG, A1,A2)
#define cmp_asc_lctkey(A1,A2) cmp_asc_lctkey(HIDDEN_ARG, A1,A2)
#define qsort_asc_lctkeyswap(A1,A2,A3,A4) qsort_asc_lctkeyswap(HIDDEN_ARG, A1,A2,A3,A4)
#define qsort_asc_lctkeymed3(A1,A2,A3) qsort_asc_lctkeymed3(HIDDEN_ARG, A1,A2,A3)
#define qsort_asc_lctkey(A1,A2) qsort_asc_lctkey(HIDDEN_ARG, A1,A2)
#define virtual_machine(A1) virtual_machine(HIDDEN_ARG, A1)
#define task_state(A1) task_state(HIDDEN_ARG, A1)
#define cd_update(A1,A2,A3,A4,A5,A6,A7,A8) cd_update(HIDDEN_ARG, A1,A2,A3,A4,A5,A6,A7,A8)
#define prune_resource_consumption(A1,A2,A3,A4,A5) prune_resource_consumption(HIDDEN_ARG, A1,A2,A3,A4,A5)
#define prune_machine_value(A1,A2) prune_machine_value(HIDDEN_ARG, A1,A2)
#define fix_machine_value(A1,A2) fix_machine_value(HIDDEN_ARG, A1,A2)
#define prune_machine_values(A1,A2) prune_machine_values(HIDDEN_ARG, A1,A2)
#define fix_machine_values(A1,A2) fix_machine_values(HIDDEN_ARG, A1,A2)
#define prune_origin_interval(A1,A2,A3) prune_origin_interval(HIDDEN_ARG, A1,A2,A3)
#define fix_origin_interval(A1,A2,A3) fix_origin_interval(HIDDEN_ARG, A1,A2,A3)
#define fix_duration_interval(A1,A2,A3) fix_duration_interval(HIDDEN_ARG, A1,A2,A3)
#define prune_end_interval(A1,A2,A3) prune_end_interval(HIDDEN_ARG, A1,A2,A3)
#define fix_end_interval(A1,A2,A3) fix_end_interval(HIDDEN_ARG, A1,A2,A3)
#define prune_duration_interval(A1,A2,A3) prune_duration_interval(HIDDEN_ARG, A1,A2,A3)
#define prune_height_interval(A1,A2,A3) prune_height_interval(HIDDEN_ARG, A1,A2,A3)
#define fix_height_interval(A1,A2,A3) fix_height_interval(HIDDEN_ARG, A1,A2,A3)
#define task_renormalize(A1) task_renormalize(HIDDEN_ARG, A1)
#define prune_constrained_task(A1,A2,A3,A4,A5) prune_constrained_task(HIDDEN_ARG, A1,A2,A3,A4,A5)
#define prune_unconstrained_task(A1,A2,A3,A4,A5) prune_unconstrained_task(HIDDEN_ARG, A1,A2,A3,A4,A5)
#define prune_generalize(A1,A2,A3,A4) prune_generalize(HIDDEN_ARG, A1,A2,A3,A4)
#define check_resource(A1,A2,A3) check_resource(HIDDEN_ARG, A1,A2,A3)
#define update_status_of_frontier_tasks(A1,A2,A3,A4) update_status_of_frontier_tasks(HIDDEN_ARG, A1,A2,A3,A4)
#define try_to_prune_according_to_frontier(A1) try_to_prune_according_to_frontier(HIDDEN_ARG, A1)
#define get_first_event(A1,A2) get_first_event(HIDDEN_ARG, A1,A2)
#define get_next_event(A1,A2,A3,A4,A5) get_next_event(HIDDEN_ARG, A1,A2,A3,A4,A5)
#define add_compulsory_events(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10) add_compulsory_events(HIDDEN_ARG, A1,A2,A3,A4,A5,A6,A7,A8,A9,A10)
#define add_non_compulsory_events(A1,A2,A3,A4,A5,A6,A7,A8) add_non_compulsory_events(HIDDEN_ARG, A1,A2,A3,A4,A5,A6,A7,A8)
#define check_interval(A1,A2) check_interval(HIDDEN_ARG, A1,A2)
#define ti_min_length_intersection(A1,A2,A3) ti_min_length_intersection(HIDDEN_ARG, A1,A2,A3)
#define ti_max_length_intersection(A1,A2,A3) ti_max_length_intersection(HIDDEN_ARG, A1,A2,A3)
#define ti_force_to_cover_atleast(A1,A2,A3,A4,A5) ti_force_to_cover_atleast(HIDDEN_ARG, A1,A2,A3,A4,A5)
#define ti_forbid_to_cover_more(A1,A2,A3,A4,A5) ti_forbid_to_cover_more(HIDDEN_ARG, A1,A2,A3,A4,A5)
#define ti_task_interval(A1,A2,A3) ti_task_interval(HIDDEN_ARG, A1,A2,A3)
#define build_minest(A1,A2) build_minest(HIDDEN_ARG, A1,A2)
#define update_task_states(A1) update_task_states(HIDDEN_ARG, A1)
#endif /* MULTI_SP_AWARE */

typedef long TASK;
typedef long MACHINE;
typedef long EVENT;
#define NOTASK ((TASK)-1L)
#define NOMACHINE ((MACHINE)-1L)
#define NOEVENT ((EVENT)-1L)
#define NOPROFILE ((PROFILE)-1L)

#define EV_START 1
#define EV_END 2

#define EV_CHECK_GE 4
#define EV_CHECK_NONE 8
#define EV_CHECK_LT 16

#define EV_COMPULSORY_PART 32

#define EV_TYPE_PROFILE 64
#define EV_TYPE_PRUNING 128

/* task flags */
#define PRUNED_ORIGIN 1
#define PRUNED_END 2
#define PRUNED_DURATION 4
#define PRUNED_HEIGHT 8
#define PRUNED_MACHINE 16
#define STATUS_TARGET 32
#define STATUS_SOURCE 64

#define FDMIN(A,B) ((A)<=(B) ? (A) : (B))
#define FDMAX(A,B) ((A)>=(B) ? (A) : (B))

/* The constraint frame. */
/* ===================== */
/* Valid options: */
/* bound(lower|upper)   ==> 0x0|0x1 */
/* generalization(Bool) ==> 0x0|0x2 */
/* task_intervals(Bool) ==> 0x0|0x4 */
/* prune(all|next)      ==> 0x0|0x8 */

#define OPT_ATMOST 0x1
#define OPT_GENERALIZATION 0x2
#define OPT_TASK_INTERVALS 0x4
#define OPT_PRUNE_NEXT 0x8

struct symmcum_data {
  void (SPCDECL *destructor)(void*);
#if MULTI_SP_AWARE
  SPEnv *spenv;
#endif /* MULTI_SP_AWARE */

  SP_term_ref refbase;		/* static */
  int options;
  long stamp;
  int ntasks;			/* static */
  int nmachines;		/* static */
  int last_to_prune;		/* volatile: min{t | TARGET(t)} or ntargets */
  int ntargets;			/* # TARGET+SOURCE tasks */
  int nsources;			/* # SOURCE tasks */
#if CSTATE_ONLY
  int ntargets_back;		/* # TARGET+SOURCE tasks upon backtracking */
#endif
  int nfiltered;		/* # filtered tasks */
  TASK *target;			/* [2*ntasks] */
  TASK *filtered;		/* [ntasks], malloced */
  long *minest;			/* [ntasks] */
  EVENT *eventqc;		/* [4*ntasks], qsorted, malloced */
  EVENT *eventqnc;		/* [8*ntasks], qsorted, malloced */
  int top_prune;		/* stack top */
  TASK *stack_prune;		/* [ntasks], pruning stack, malloced */
  long capacity_min;		/* min of all capacities */
  long capacity_max;		/* max of all capacities */
  long max_duration;		/* max duration of the tasks for which
				   the height attribute may cause a problem */
  long min_min_height;		/* min value of min initial height of tasks */
  long max_min_height;		/* max value of min initial height of tasks */
  long min_max_height;		/* min value of max initial height of tasks */
  long max_max_height;		/* max value of max initial height of tasks */
  long nconstrained;		/* counter */
  long profile;		/* counter */
  /* valid values upon backtracking */
  long *initial_size;
  TAGGED *initial_est;
  TAGGED *initial_lct;
  /* valid values in forward execution */
  long *previous_size;
  TAGGED *previous_est;
  TAGGED *previous_lct;
  /* either one of the two above */
  long *table_size;
  TAGGED *table_est;
  TAGGED *table_lct;
  TAGGED queue;
  
  /* fields for the generalization */
  long *size_;			/* [ntasks], malloced even if !OPT_GENERALIZATION */
  PROFILE *origin_profile;	/* [ntasks], malloced */
  PROFILE *end_profile;		/* [ntasks], malloced */
  PROFILE *duration_profile;	/* [ntasks], malloced */
  PROFILE *height_profile;	/* [ntasks], malloced */
  PROFILE *origin_tempo;	/* [ntasks], malloced */
  PROFILE *end_tempo;		/* [ntasks], malloced */
  PROFILE *duration_tempo;	/* [ntasks], malloced */
  PROFILE *height_tempo;	/* [ntasks], malloced */
  long *origin_stamp;		/* [ntasks], malloced */
  long *end_stamp;		/* [ntasks], malloced */
  long *duration_stamp;		/* [ntasks], malloced */
  long *height_stamp;		/* [ntasks], malloced */
  int generalize_nvar;		/* volatile, used even if !OPT_GENERALIZATION */
  PROFILE **generalize_tempo_marray; /* [4*nfiltered], malloced */
  PROFILE **generalize_profile_marray; /* [4*nfiltered], malloced */
  TASK *generalize_itask;	/* [4*nfiltered], malloced */
  int *generalize_dvar;		/* [4*nfiltered], malloced */
  
  /* fields for task intervals */
  long sum_max_length_inter;
  TASK *try_to_prune;		/* [ntasks], malloced */
  BOOL *include_in_machines;	/* [ntasks], malloced */
  long *min_length_inter;	/* [ntasks], malloced */
  long *max_length_inter;	/* [ntasks], malloced */
  long *max_min_length_inter;	/* [nmachines], malloced */
  long *contribution;		/* [ntasks], malloced */
  long *hcontribution;		/* [ntasks], malloced */
  
  /* fields for frontier-based pruning */
  long *start_frontier;		/* [ntasks], malloced */
  long *end_frontier;		/* [ntasks], malloced */
  TASK *first_sweep_frontier;	/* [nmachines], malloced */
  TASK *next_sweep_frontier;	/* [ntasks], malloced */
  TASK *first_prune_frontier;	/* [nmachines], malloced */
  TASK *next_prune_frontier;	/* [ntasks], malloced */
  TASK *first_check_frontier;	/* [nmachines], malloced */
  TASK *next_check_frontier;	/* [ntasks], malloced */
  long *init_status_frontier;	/* [ntasks], malloced */
  long *best_status_frontier;	/* [ntasks], malloced */
  struct {
    MACHINE *real;		/* virtual mach -> real mach */
    long *capacity;		/* virtual mach -> min or max */
  } machine;			/* [nmachines] */
  struct {
    long *flags;		/* flags for the five dvars + in_profile */
    MACHINE *m_status;
    long *htask;
    /* task origin */
    TAGGED *minorg;		/* min(Origin) */
    TAGGED *maxorg;		/* max(Origin) */
    TAGGED *setorg;		/* set(Origin), volatile, GC-unsafe */
    /* task duration */
    TAGGED *mindur;		/* min(Duration) */
    TAGGED *maxdur;		/* max(Duration) */
    TAGGED *setdur;		/* set(Duration), volatile, GC-unsafe */
    /* task end */
    TAGGED *minend;		/* min(End) */
    TAGGED *maxend;		/* max(End) */
    TAGGED *setend;		/* set(End), volatile, GC-unsafe */
    long *lctkey;		/* LCT just after backtracking */
    /* task height */
    TAGGED *minheight;		/* min(Height) */
    TAGGED *maxheight;		/* max(Height) */
    TAGGED *setheight;		/* set(Height), volatile, GC-unsafe */
    /* task (real) machine */
    long *sizemach;		/* size(Machine) */
    TAGGED *setmach;		/* set(Machine), volatile, GC-unsafe */
  } task;			/* [ntask] */
  struct {
    long *date;			/* location in time, malloced */
    TASK *task;			/* generating task, malloced */
    long *height;		/* quantity to add to the profile, malloced */
    MACHINE *machine;		/* assigned machine, malloced */
    unsigned int *flags;	/* EV_* + EV_TYPE_* + EV_CHECK_*, malloced */
  } event;			/* [10*ntasks] */
  /* space for the above arrays */
};

#define TASK(I) (pdata->target[I])
#define MINEST(I) (pdata->minest[I])
#define FILTERED(I) (pdata->filtered[I])
#define SIZE_(I) (pdata->size_[I])
#define ORIGIN_PROFILE(I) (pdata->origin_profile[I])
#define END_PROFILE(I) (pdata->end_profile[I])
#define DURATION_PROFILE(I) (pdata->duration_profile[I])
#define HEIGHT_PROFILE(I) (pdata->height_profile[I])
#define ORIGIN_TEMPO(I) (pdata->origin_tempo[I])
#define END_TEMPO(I) (pdata->end_tempo[I])
#define DURATION_TEMPO(I) (pdata->duration_tempo[I])
#define HEIGHT_TEMPO(I) (pdata->height_tempo[I])
#define ORIGIN_STAMP(I) (pdata->origin_stamp[I])
#define END_STAMP(I) (pdata->end_stamp[I])
#define DURATION_STAMP(I) (pdata->duration_stamp[I])
#define HEIGHT_STAMP(I) (pdata->height_stamp[I])

#define REAL(M) (pdata->machine.real[M])
#define CAPACITY(M) (pdata->machine.capacity[M])

#define FLAGS(T) (pdata->task.flags[T])
#define M_STATUS(T) (pdata->task.m_status[T])
#define HTASK(T) (pdata->task.htask[T])

#define EST(T) GetSmall(pdata->task.minorg[T])
#define EST_T(T) (pdata->task.minorg[T])
#define LST(T) GetSmall(pdata->task.maxorg[T])
#define LST_T(T) (pdata->task.maxorg[T])
#define SETORG(T) (pdata->task.setorg[T])

#define ECT(T) GetSmall(pdata->task.minend[T])
#define ECT_T(T) (pdata->task.minend[T])
#define LCT(T) GetSmall(pdata->task.maxend[T])
#define LCT_T(T) (pdata->task.maxend[T])
#define SETEND(T) (pdata->task.setend[T])
#define LCTKEY(T) (pdata->task.lctkey[T])

#define MINDUR(T) GetSmall(pdata->task.mindur[T])
#define MINDUR_T(T) (pdata->task.mindur[T])
#define MAXDUR(T) GetSmall(pdata->task.maxdur[T])
#define MAXDUR_T(T) (pdata->task.maxdur[T])
#define SETDUR(T) (pdata->task.setdur[T])

#define MINHEIGHT(T) GetSmall(pdata->task.minheight[T])
#define MINHEIGHT_T(T) (pdata->task.minheight[T])
#define MAXHEIGHT(T) GetSmall(pdata->task.maxheight[T])
#define MAXHEIGHT_T(T) (pdata->task.maxheight[T])
#define SETHEIGHT(T) (pdata->task.setheight[T])

#define SIZEMACH(T) (pdata->task.sizemach[T])
#define SETMACH(T) (pdata->task.setmach[T])

#define TABLE_SIZE(T) (pdata->table_size[T])
#define TABLE_EST_T(T) (pdata->table_est[T])
#define TABLE_EST(T) GetSmall(pdata->table_est[T])
#define TABLE_LCT_T(T) (pdata->table_lct[T])
#define TABLE_LCT(T) GetSmall(pdata->table_lct[T])

#define TRefOrgAttr(T) RefTerm(pdata->refbase + 10*(T))
#define TRefOrgVar(T) RefTerm(pdata->refbase + 10*(T) + 1)
#define TRefEndAttr(T) RefTerm(pdata->refbase + 10*(T) + 2)
#define TRefEndVar(T) RefTerm(pdata->refbase + 10*(T) + 3)
#define TRefDurAttr(T) RefTerm(pdata->refbase + 10*(T) + 4)
#define TRefDurVar(T) RefTerm(pdata->refbase + 10*(T) + 5)
#define TRefHeightAttr(T) RefTerm(pdata->refbase + 10*(T) + 6)
#define TRefHeightVar(T) RefTerm(pdata->refbase + 10*(T) + 7)
#define TRefMachAttr(T) RefTerm(pdata->refbase + 10*(T) + 8)
#define TRefMachVar(T) RefTerm(pdata->refbase + 10*(T) + 9)

#define EV_DATE(E) (pdata->event.date[E])
#define EV_TASK(E) (pdata->event.task[E])
#define EV_HEIGHT(E) (pdata->event.height[E])
#define EV_MACHINE(E) (pdata->event.machine[E])
#define EV_FLAGS(E) (pdata->event.flags[E])

static void SPCDECL symmcum_destructor(void *pdata_v)
{
  struct symmcum_data *pdata = (struct symmcum_data *)pdata_v;
  FD_SETUP_SPENV(pdata->spenv)

  SP_free_term_refs(pdata->refbase,10*pdata->ntasks);
  SP_free(pdata);
}


#define HAS_COMPULSORY_PART(t) (LST(t)<ECT(t))

#define MUST_OVERLAP(t,low,up) (low<ECT(t) && LST(t)<up && MINDUR(t)>0)

#define MAY_OVERLAP(t,low,up) (low<LCT(t) && EST(t)<up && MAXDUR(t)>0)

#define ORIGIN_OVERLAPS(t,low,up) (low<LST(t) && EST(t)<up && EST(t)<LST(t))

#define END_OVERLAPS(t,low,up) (low<LCT(t) && ECT(t)<up && ECT(t)<LCT(t))

#define OVERFLOWS(h,vr)							   \
	  (!((pdata->options&OPT_ATMOST) ? ((h)<=0 && (h)<=CAPACITY(vr))   \
	                                 : ((h)>=0 && (h)>=CAPACITY(vr))))

/* sort by <ascending machine, ascending date> */
static int 
cmp_asc_eventc MAGIC (HIDDEN_PROTO
		      EVENT *e1, EVENT *e2)
{
  struct symmcum_data *pdata = fd.gdata;
  MACHINE m1 = EV_MACHINE(*e1);
  MACHINE m2 = EV_MACHINE(*e2);

  if (m1<m2) return -1;
  if (m1>m2) return 1;
  return EV_DATE(*e1) - EV_DATE(*e2);
}


#define QType EVENT
#define QCmp  cmp_asc_eventc
#define QSort qsort_asc_eventc
#include "qsort.ic"

/* sort by <ascending date, PROFILE before PRUNING> */
static int 
cmp_asc_eventnc MAGIC (HIDDEN_PROTO
		       EVENT *e1, EVENT *e2)
{
  struct symmcum_data *pdata = fd.gdata;

  long d1 = EV_DATE(*e1);
  long d2 = EV_DATE(*e2);

  if (d1<d2) return -1;
  if (d1>d2) return 1;
  return EV_FLAGS(*e1) - EV_FLAGS(*e2);
}


#define QType EVENT
#define QCmp  cmp_asc_eventnc
#define QSort qsort_asc_eventnc
#include "qsort.ic"

static int 
cmp_asc_lctkey MAGIC (HIDDEN_PROTO
		      TASK *e1, TASK *e2)
{
  struct symmcum_data *pdata = fd.gdata;

  return LCTKEY(*e1) - LCTKEY(*e2);
}


#define QType TASK
#define QCmp  cmp_asc_lctkey
#define QSort qsort_asc_lctkey
#include "qsort.ic"

static MACHINE 
virtual_machine MAGIC (HIDDEN_PROTO MACHINE real)
{
  int j, mid, sup;
  struct symmcum_data *pdata = fd.gdata;
  
  /* dichotomic search for j such that REAL(j)==real */
  j = 0;
  sup = pdata->nmachines;
  while (j<sup) {
    mid = (j+sup)>>1;
    if (REAL(mid)<real)
      j = mid+1;
    else
      sup = mid;
  }
  return j;
}


static long 
task_state MAGIC (HIDDEN_PROTO TASK t)
{
  struct symmcum_data *pdata = fd.gdata;

  if ((pdata->options&OPT_ATMOST))
    return (LST_T(t)-EST_T(t)) + (LCT_T(t)-ECT_T(t)) + (MAXDUR_T(t)-MINDUR_T(t)) + SIZEMACH(t) - MINHEIGHT_T(t);
  else
    return (LST_T(t)-EST_T(t)) + (LCT_T(t)-ECT_T(t)) + (MAXDUR_T(t)-MINDUR_T(t)) + SIZEMACH(t) + MAXHEIGHT_T(t);
}



/* Add a unit height interval [b,e] to a profile p */
static void
cd_update MAGIC (HIDDEN_PROTO
		 TASK t, MACHINE vr, int dvar,
		 PROFILE *profile, PROFILE *tempo, long *stamp,
		 long b, long e)
{
  struct symmcum_data *pdata = fd.gdata;
  int i;
  
  if (b<=e) {
    if (*stamp != vr && *tempo != NOPROFILE) {
      profile_dispose(*tempo);
      *tempo = NOPROFILE;
    }
    if (*tempo == NOPROFILE) {
      *tempo = empty_profile();
      *stamp = vr;

      /* record information needed to update marray_profile as soon as
	 we finish the sweep proess on resource vr */

      i = pdata->generalize_nvar++;
      pdata->generalize_tempo_marray[i] = tempo;
      pdata->generalize_profile_marray[i] = profile;
      pdata->generalize_itask[i] = t;
      pdata->generalize_dvar[i] = dvar;
    }
    *tempo = profile_update(*tempo,b,e+1,1);
  }
}


static BOOL 
prune_resource_consumption MAGIC (HIDDEN_PROTO
				  TASK t, MACHINE vr,
				  long low, long up,
				  long contrib_t /* contribution of t in profile */
				  )
{
  struct symmcum_data *pdata = fd.gdata;

  if (MUST_OVERLAP(t,low,up)) {

    /* forbid too big or small height for t */

    long limit = CAPACITY(vr) - (pdata->profile - contrib_t);

    if (SIZEMACH(t)==1) {
      if ((pdata->options&OPT_ATMOST)) {
	if (limit<MAXHEIGHT(t)) {
	  FLAGS(t) |= PRUNED_HEIGHT;
	  return adjust_upper_bound(MakeSmall(limit),
				    SETHEIGHT(t),
				    &MINHEIGHT_T(t), &MAXHEIGHT_T(t));
	}
      } else {
	if (limit>MINHEIGHT(t)) {
	  FLAGS(t) |= PRUNED_HEIGHT;
	  return adjust_lower_bound(MakeSmall(limit),
				    SETHEIGHT(t),
				    &MINHEIGHT_T(t), &MAXHEIGHT_T(t));
	}
      }
    } else if (SIZE_(t)>1) {
      if ((pdata->options&OPT_ATMOST))
	cd_update(t,vr,PRUNED_HEIGHT,
		  &HEIGHT_PROFILE(t), &HEIGHT_TEMPO(t),&HEIGHT_STAMP(t),
		  limit+1,MAXHEIGHT(t));
      else
	cd_update(t,vr,PRUNED_HEIGHT,
		  &HEIGHT_PROFILE(t), &HEIGHT_TEMPO(t),&HEIGHT_STAMP(t),
		  MINHEIGHT(t),limit-1);
    }
  }
  return TRUE;
}


static BOOL
prune_machine_value MAGIC (HIDDEN_PROTO
			   TASK t, TAGGED trr)
{
  struct symmcum_data *pdata = fd.gdata;
  
  if (fd_member(trr,SETMACH(t))) {
    if (--SIZEMACH(t) == 0)
      return FALSE;
    FLAGS(t) |= PRUNED_MACHINE;
    SETMACH(t) = fd_delete(SETMACH(t),trr);
  }
  return TRUE;
}


static BOOL
fix_machine_value MAGIC (HIDDEN_PROTO
			 TASK t, long r/* real */)
{
  struct symmcum_data *pdata = fd.gdata;
  TAGGED tmp = MakeSmall(r);
  
  if (!fd_member(tmp,SETMACH(t)))
    return FALSE;
  if (SIZEMACH(t)>1) {
    FLAGS(t) |= PRUNED_MACHINE;
    SIZEMACH(t) = 1;
    SETMACH(t) = fd_interval(tmp,tmp);
  }
  return TRUE;
}


static BOOL
prune_machine_values MAGIC (HIDDEN_PROTO
			    TASK t, TAGGED values)
{
  struct symmcum_data *pdata = fd.gdata;

  switch (fd_compare(SETMACH(t),values)) {
  case FDI_DISJOINT:
    return TRUE;
  case FDI_SUBSET:
  case FDI_EQUAL:
    return FALSE;
  }
    
  FLAGS(t) |= PRUNED_MACHINE;
  SETMACH(t) = fd_subtract(SETMACH(t),values);
  SIZEMACH(t) = fd_size(SETMACH(t));
  return TRUE;
}


static BOOL
fix_machine_values MAGIC (HIDDEN_PROTO
			  TASK t, TAGGED values)
{
  struct symmcum_data *pdata = fd.gdata;

  switch (fd_compare(SETMACH(t),values)) {
  case FDI_DISJOINT:
    return FALSE;
  case FDI_SUBSET:
  case FDI_EQUAL:
    return TRUE;
  }
    
  FLAGS(t) |= PRUNED_MACHINE;
  SETMACH(t) = fd_and(SETMACH(t),values);
  SIZEMACH(t) = fd_size(SETMACH(t));
  return TRUE;
}


static BOOL
prune_origin_interval MAGIC (HIDDEN_PROTO
			     TASK t, long lb, long ub)
{
  struct symmcum_data *pdata = fd.gdata;
  TAGGED lbt = MakeSmall(lb);
  TAGGED ubt = MakeSmall(ub);

  if (lb>ub) return TRUE;

  switch (fd_interval_cmp(lbt,ubt,EST_T(t),LST_T(t))) {
  case FD_BEFORE:
  case FD_MEETS:
  case FD_MET_BY:
  case FD_AFTER:
    return TRUE;
  case FD_OVERLAPS:
  case FD_STARTS:
    FLAGS(t) |= PRUNED_ORIGIN;
    return adjust_lower_bound(MakeSmall(ub+1), SETORG(t), &EST_T(t), &LST_T(t));
  case FD_DURING:
    if (fd_compare_interval(SETORG(t),lbt,ubt)!=FDI_DISJOINT) {
      FLAGS(t) |= PRUNED_ORIGIN;
      SETORG(t) = fd_subtract_interval(SETORG(t),lbt,ubt);
    }
    return TRUE;
  case FD_FINISHES:
  case FD_OVERLAPPED_BY:
    FLAGS(t) |= PRUNED_ORIGIN;
    return adjust_upper_bound(MakeSmall(lb-1), SETORG(t), &EST_T(t), &LST_T(t));
  case FD_FINISHED_BY:
  case FD_CONTAINS:
  case FD_EQUALS:
  case FD_STARTED_BY:
    return FALSE;
  }
  return TRUE;			/* really unreachable */
}


static BOOL
fix_origin_interval MAGIC (HIDDEN_PROTO
			   TASK t, long lb, long ub)
{
  struct symmcum_data *pdata = fd.gdata;
  TAGGED lbt = MakeSmall(lb);
  TAGGED ubt = MakeSmall(ub);
  
  if (lb>EST(t) || ub<LST(t)) {
    FLAGS(t) |= PRUNED_ORIGIN;
    if (!adjust_bounds(lbt, ubt, SETORG(t), &EST_T(t), &LST_T(t)))
      return FALSE;
  }
  return TRUE;
}


static BOOL
fix_duration_interval MAGIC (HIDDEN_PROTO
			     TASK t, long lb, long ub)
{
  struct symmcum_data *pdata = fd.gdata;
  TAGGED lbt = MakeSmall(lb);
  TAGGED ubt = MakeSmall(ub);
  
  if (lb>MINDUR(t) || ub<MAXDUR(t)) {
    FLAGS(t) |= PRUNED_DURATION;
    if (!adjust_bounds(lbt, ubt, SETDUR(t), &MINDUR_T(t), &MAXDUR_T(t)))
      return FALSE;
  }  
  return TRUE;
}




static BOOL
prune_end_interval MAGIC (HIDDEN_PROTO
			  TASK t, long lb, long ub)
{
  struct symmcum_data *pdata = fd.gdata;
  TAGGED lbt = MakeSmall(lb);
  TAGGED ubt = MakeSmall(ub);

  if (lb>ub) return TRUE;

  switch (fd_interval_cmp(lbt,ubt,ECT_T(t),LCT_T(t))) {
  case FD_BEFORE:
  case FD_MEETS:
  case FD_MET_BY:
  case FD_AFTER:
    return TRUE;
  case FD_OVERLAPS:
  case FD_STARTS:
    FLAGS(t) |= PRUNED_END;
    return adjust_lower_bound(MakeSmall(ub+1), SETEND(t), &ECT_T(t), &LCT_T(t));
  case FD_DURING:
    if (fd_compare_interval(SETEND(t),lbt,ubt)!=FDI_DISJOINT) {
      FLAGS(t) |= PRUNED_END;
      SETEND(t) = fd_subtract_interval(SETEND(t),lbt,ubt);
    }
    return TRUE;
  case FD_FINISHES:
  case FD_OVERLAPPED_BY:
    FLAGS(t) |= PRUNED_END;
    return adjust_upper_bound(MakeSmall(lb-1), SETEND(t), &ECT_T(t), &LCT_T(t));
  case FD_FINISHED_BY:
  case FD_CONTAINS:
  case FD_EQUALS:
  case FD_STARTED_BY:
    return FALSE;
  }
  return TRUE;			/* really unreachable */
}



static BOOL
fix_end_interval MAGIC (HIDDEN_PROTO
			TASK t, long lb, long ub)
{
  struct symmcum_data *pdata = fd.gdata;
  TAGGED lbt = MakeSmall(lb);
  TAGGED ubt = MakeSmall(ub);
  
  if (lb>ECT(t) || ub<LCT(t)) {
    FLAGS(t) |= PRUNED_END;
    if (!adjust_bounds(lbt, ubt, SETEND(t), &ECT_T(t), &LCT_T(t)))
      return FALSE;
  }
  return TRUE;
}



static BOOL
prune_duration_interval MAGIC (HIDDEN_PROTO
			       TASK t, long lb, long ub)
{
  struct symmcum_data *pdata = fd.gdata;
  TAGGED lbt = MakeSmall(lb);
  TAGGED ubt = MakeSmall(ub);

  if (lb>ub) return TRUE;

  switch (fd_interval_cmp(lbt,ubt,MINDUR_T(t),MAXDUR_T(t))) {
  case FD_BEFORE:
  case FD_MEETS:
  case FD_MET_BY:
  case FD_AFTER:
    return TRUE;
  case FD_OVERLAPS:
  case FD_STARTS:
    FLAGS(t) |= PRUNED_DURATION;
    return adjust_lower_bound(MakeSmall(ub+1), SETDUR(t), &MINDUR_T(t), &MAXDUR_T(t));
  case FD_DURING:
    if (fd_compare_interval(SETDUR(t),lbt,ubt)!=FDI_DISJOINT) {
      FLAGS(t) |= PRUNED_DURATION;
      SETDUR(t) = fd_subtract_interval(SETDUR(t),lbt,ubt);
    }
    return TRUE;
  case FD_FINISHES:
  case FD_OVERLAPPED_BY:
    FLAGS(t) |= PRUNED_DURATION;
    return adjust_upper_bound(MakeSmall(lb-1), SETDUR(t), &MINDUR_T(t), &MAXDUR_T(t));
  case FD_FINISHED_BY:
  case FD_CONTAINS:
  case FD_EQUALS:
  case FD_STARTED_BY:
    return FALSE;
  }
  return TRUE;			/* really unreachable */
}



static BOOL
prune_height_interval MAGIC (HIDDEN_PROTO
			     TASK t, long lb, long ub)
{
  struct symmcum_data *pdata = fd.gdata;
  TAGGED lbt = MakeSmall(lb);
  TAGGED ubt = MakeSmall(ub);

  if (lb>ub) return TRUE;

  switch (fd_interval_cmp(lbt,ubt,MINHEIGHT_T(t),MAXHEIGHT_T(t))) {
  case FD_BEFORE:
  case FD_MEETS:
  case FD_MET_BY:
  case FD_AFTER:
    return TRUE;
  case FD_OVERLAPS:
  case FD_STARTS:
    FLAGS(t) |= PRUNED_HEIGHT;
    return adjust_lower_bound(MakeSmall(ub+1), SETHEIGHT(t), &MINHEIGHT_T(t), &MAXHEIGHT_T(t));
  case FD_DURING:
    if (fd_compare_interval(SETHEIGHT(t),lbt,ubt)!=FDI_DISJOINT) {
      FLAGS(t) |= PRUNED_HEIGHT;
      SETHEIGHT(t) = fd_subtract_interval(SETHEIGHT(t),lbt,ubt);
    }
    return TRUE;
  case FD_FINISHES:
  case FD_OVERLAPPED_BY:
    FLAGS(t) |= PRUNED_HEIGHT;
    return adjust_upper_bound(MakeSmall(lb-1), SETHEIGHT(t), &MINHEIGHT_T(t), &MAXHEIGHT_T(t));
  case FD_FINISHED_BY:
  case FD_CONTAINS:
  case FD_EQUALS:
  case FD_STARTED_BY:
    return FALSE;
  }
  return TRUE;			/* really unreachable */
}



static BOOL
fix_height_interval MAGIC (HIDDEN_PROTO
			   TASK t, long lb, long ub)
{
  struct symmcum_data *pdata = fd.gdata;
  TAGGED lbt = MakeSmall(lb);
  TAGGED ubt = MakeSmall(ub);
  
  if (lb>MINHEIGHT(t) || ub<MAXHEIGHT(t)) {
    FLAGS(t) |= PRUNED_HEIGHT;
    if (!adjust_bounds(lbt, ubt, SETHEIGHT(t), &MINHEIGHT_T(t), &MAXHEIGHT_T(t)))
      return FALSE;
  }
  return TRUE;
}



static BOOL 
task_renormalize MAGIC (HIDDEN_PROTO TASK t)	/* origin + duration = end */
{
  struct symmcum_data *pdata = fd.gdata;
  for (;;) {
    long est = EST(t);
    long lst = LST(t);
    long ect = ECT(t);
    long lct = LCT(t);
    long mindur = MINDUR(t);
    long maxdur = MAXDUR(t);
    long lb1 = ect-maxdur;
    long ub1 = lct-mindur;
    long lb2 = ect-lst;
    long ub2 = lct-est;
    long lb3 = est+mindur;
    long ub3 = lst+maxdur;

    if (!(est<lb1 || lst>ub1 ||
	  mindur<lb2 || maxdur>ub2 ||
	  ect<lb3 || lct>ub3))
      return TRUE;
    
    /* origin in min(end)-max(dur) ... max(end)-min(dur) */
    if (!fix_origin_interval(t, lb1, ub1))
      return FALSE;
  
  /* dur in min(end)-max(origin) ... max(end)-min(origin) */
    if (!fix_duration_interval(t, lb2, ub2))
      return FALSE;
  
  /* end in min(origin)+min(dur) ... max(origin)-max(dur) */
    if (!fix_end_interval(t, lb3, ub3))
      return FALSE;
  }
}



static BOOL 
prune_constrained_task MAGIC (HIDDEN_PROTO
			      TASK t, /* the task */
			      MACHINE r, /* virtual machine */
			      long low, long up, /* compulsory part */
			      long contrib_t	/* contribution of t in profile */
			      )
{
  struct symmcum_data *pdata = fd.gdata;
  long optval = pdata->profile - contrib_t;

  /* if there would be a problem if task t overlaps low..up
     (no need to prune min height since max would already cause problem) */
  
  if ((pdata->options&OPT_ATMOST)
      ? optval+HTASK(t)>CAPACITY(r)
      : optval+HTASK(t)<CAPACITY(r)) {

    /* if task t overlaps low..up then forbid assignment on r */

    if (MUST_OVERLAP(t,low,up)) {
      return prune_machine_value(t,MakeSmall(REAL(r)));
    } else {
      long maxd, l2;

      maxd = low-EST(t);
      l2 = LCT(t)-up;
      if (maxd<l2) maxd = l2;
      if (maxd<0)  maxd = 0;

      /* if task t assigned on r then forbid overlap with low..up */

      if (SIZEMACH(t)==1) {
	if (MINDUR(t)>0 &&
	    !(prune_origin_interval(t, low-MINDUR(t)+1, up-1) &&
	      prune_end_interval(t, low+1, up-1+MINDUR(t))))
	  return FALSE;
	return
	  fix_duration_interval(t, MINDUR(t), maxd);
      } else if (SIZE_(t)>1) {

	/* virtual pruning for the generalization */

	if (MINDUR(t)>0) {
	  cd_update(t,r,PRUNED_ORIGIN,
		    &ORIGIN_PROFILE(t), &ORIGIN_TEMPO(t),&ORIGIN_STAMP(t),
		    low-MINDUR(t)+1,up-1);
	  cd_update(t,r,PRUNED_END,
		    &END_PROFILE(t), &END_TEMPO(t),&END_STAMP(t),
		    low+1,up+MINDUR(t)-1);
	}
	cd_update(t,r,PRUNED_DURATION,
		  &DURATION_PROFILE(t), &DURATION_TEMPO(t),&DURATION_STAMP(t),
		  maxd+1,MAXDUR(t));
      }
    }
  }
  
  return TRUE;
}




static BOOL 
prune_unconstrained_task MAGIC (HIDDEN_PROTO
				TASK t, /* the task */
				MACHINE r, /* virtual machine */
				long low, long up, /* compulsory part */
				long contrib_t	/* contribution of t in profile */
				)
{
  struct symmcum_data *pdata = fd.gdata;
  long optval = pdata->profile - contrib_t;
  long mind, l2;

  /* exit if do not need absolutely task t on interval low..up */

  if (pdata->nconstrained==0 ||
      ((pdata->options&OPT_ATMOST) ? optval<=CAPACITY(r) : optval>=CAPACITY(r)))
    return TRUE;

  /* assign task t to machine r */

  if (!fix_machine_value(t, REAL(r)))
    return FALSE;

  /* prune origin of t to enforce task t to overlap low..up */

  if (!fix_origin_interval(t, up-MAXDUR(t), low))
    return FALSE;

  /* prune end of t to enforce task t to overlap low..up */

  if (!fix_end_interval(t, up, low+MAXDUR(t)))
    return FALSE;

  /* prune duration of t to enforce task t to overlap low..up */

  mind = up-LST(t);
  l2 = ECT(t)-low;
  if (mind>l2) mind = l2;
  if (!fix_duration_interval(t, mind, MAXDUR(t)))
    return FALSE;

  return TRUE;    
}


static BOOL
prune_generalize MAGIC (HIDDEN_PROTO
			TASK t, long size, PROFILE p,
			BOOL (*fun)(HIDDEN_PROTO TASK, long, long))
{
  struct symmcum_data *pdata = fd.gdata;
  BOOL rc = TRUE;
  BOOL pruned = FALSE;
  long low, up, h, state=0;

  while (p && rc) {
    profile_next(p,&low,&up,&h,&p);
    if (h==size) {
      if (!pruned) {
	pruned = TRUE;
	state = task_state(t);
      }
      rc = (*fun)(HIDDEN_ARG_COMMA t,low,up-1);
    }
  }
  if (rc && pruned && task_state(t)!=state) {
    rc = task_renormalize(t);
    pdata->queue =
      fd_or_interval(pdata->queue,TABLE_EST_T(t),TABLE_LCT_T(t));      
  }
  return rc;
}



static BOOL 
check_resource MAGIC (HIDDEN_PROTO
		      MACHINE r, /* virtual machine */
		      long low, long up /* compulsory part */
		      )
{
  struct symmcum_data *pdata = fd.gdata;
  long status = pdata->profile;
  long capr = CAPACITY(r);
  TAGGED trr = MakeSmall(REAL(r));
  int i, newi;

  if (pdata->nconstrained>0)
    if ((pdata->options&OPT_ATMOST) ? status>capr : status<capr)
      return FALSE;

  /* scan the stack of tasks and compress the stack */

  newi = 0;
  for (i=0; i<pdata->top_prune; i++) {
    TASK t = pdata->stack_prune[i];

    if (LCT(t)>up)
      pdata->stack_prune[newi++] = t;	/* keep t for the next check */
    if (fd_member(trr,SETMACH(t))) {
      long previous_task_state = task_state(t);
      long h = ((pdata->options&OPT_ATMOST) ? MINHEIGHT(t) : MAXHEIGHT(t));
      long contrib_t = (M_STATUS(t)==r ? HTASK(t) : 0);

      /* if t is an unconstrained or a reserve task */
      
      if (((pdata->options&OPT_ATMOST) ? h<=0 : h>=0) &&
	  !prune_unconstrained_task(t,r,low,up,contrib_t))
	return FALSE;
      
      if (fd_member(trr,SETMACH(t))) { /* currently redundant test */

	/* if t is a constrained or a reserve task */

	if (((pdata->options&OPT_ATMOST) ? (h>0 || h>capr) : (h<0 || h<capr)) &&
	    !prune_constrained_task(t,r,low,up,contrib_t))
	  return FALSE;

	if (fd_member(trr,SETMACH(t))) { /* NECESSARY test */

	  /* if necessary, prune the resource consumption of task t */

	  if (!prune_resource_consumption(t,r,low,up,contrib_t))
	    return FALSE;
	}
      }
      /* handle saturation if there was some pruning */
      
      if (previous_task_state!=task_state(t)) {
	if (!task_renormalize(t))
	  return FALSE;
	pdata->queue = fd_or_interval(pdata->queue,TABLE_EST_T(t),
				      TABLE_LCT_T(t));
      }
    }
  }
  pdata->top_prune = newi;
  return TRUE;
}


/*** support for frontier-based pruning ***/

static void
update_status_of_frontier_tasks MAGIC (HIDDEN_PROTO
				       MACHINE vr,
				       long low,long up,
				       int status)
{
  struct symmcum_data *pdata = fd.gdata;
  TASK t = pdata->first_sweep_frontier[vr];
  TASK previous_t = NOTASK;
  TASK next_t;
  BOOL remove_t;

  while (t != NOTASK) {
    next_t = pdata->next_sweep_frontier[t];
    remove_t = FALSE;

    /* if frontier of task t intersects low..up
       then update status of task t if necessary */

    if (pdata->end_frontier[t]>=low && pdata->start_frontier[t]<up) {
      if (pdata->init_status_frontier[t]==0 ||
	  ((pdata->options&OPT_ATMOST)
	   ? pdata->best_status_frontier[t]>status
	   : pdata->best_status_frontier[t]<status)) {

	/* if margin in height is too big,
	   then can't prune according to frontier of task t;
	   so remove task t from the list of tasks
	   for which the status must be updated,
	   and don't transfer t to the list of tasks
	   according to which we will prune */

	if ((pdata->options&OPT_ATMOST) 
	    ? status+MINHEIGHT(t)+pdata->max_min_height <= CAPACITY(vr)
	    : status+MAXHEIGHT(t)+pdata->min_max_height >= CAPACITY(vr)) {
	  remove_t = TRUE;
	} else {

	  /* if margin in height is not too big, then update status */

	  pdata->best_status_frontier[t] = status;
	  pdata->init_status_frontier[t] = 1;
	}
      }
    }

    /* if end of frontier of task t is before up, then transfer task t
       FROM the list of tasks for which the status must be updated
       TO the list of tasks according to which we will prune */

    if (!remove_t && pdata->end_frontier[t]<up) {
      remove_t = TRUE;
      pdata->next_prune_frontier[t] = pdata->first_prune_frontier[vr];
      pdata->first_prune_frontier[vr] = t;
    }

    /* remove t if necessary and get next task */

    if (remove_t) {
      if (previous_t==NOTASK)
	pdata->first_sweep_frontier[vr] = next_t;
      else
	pdata->next_sweep_frontier[previous_t] = next_t;
    } else {
      previous_t = t;
    }
    t = next_t;
  }
}


static BOOL
try_to_prune_according_to_frontier MAGIC (HIDDEN_PROTO MACHINE vr)
{
  struct symmcum_data *pdata = fd.gdata;
  TASK t, t2;
  long start, end, size, delta;
  long min_start, max_end, min_size, best_delta;

  /* exit if no task with a small frontier according to which to prune */

  t = pdata->first_prune_frontier[vr];
  if (t==NOTASK)
    return TRUE;

  /* compute common characteristics of all the frontiers
     according to which we will try to prune (for filtering purpose):
     - min_start:  EST of the frontier parts
     - max_end:    LCT of the frontier parts
     - min_size:   size of the smallest frontier
     - best_delta: (cum_atmost ? smallest : largest) height
                   which can lead to some pruning */

  min_start = CLPFD_MAXINT;
  max_end = -CLPFD_MAXINT;
  min_size = CLPFD_MAXINT;
  best_delta = (pdata->options&OPT_ATMOST) ? CLPFD_MAXINT : -CLPFD_MAXINT;
  while (t!=NOTASK) {
    start = pdata->start_frontier[t];
    end = pdata->end_frontier[t];
    size = end-start+1;
    if ((pdata->options&OPT_ATMOST))
      delta = CAPACITY(vr)-pdata->best_status_frontier[t]-MINHEIGHT(t);
    else
      delta = CAPACITY(vr)-pdata->best_status_frontier[t]-MAXHEIGHT(t);
    if (min_start>start) min_start = start;
    if (max_end<end) max_end = end;
    if (min_size>size) min_size = size;
    if (best_delta>((pdata->options&OPT_ATMOST) ? delta : -delta))
      best_delta = delta;
    t = pdata->next_prune_frontier[t];
  }

  /* pruning loop thru the list of candidates to prune or machine vr */

  t2 = pdata->first_check_frontier[vr];
  while (t2!=NOTASK) {

    /* make a first filter in order to have a chance that task t2
       can be pruned according to one of the small frontiers */

    if (LCT(t2)>min_start &&	/* task t2 interferes in time with frontiers */
	EST(t2)<=max_end &&	/* smallest frontier can be covered by task t2 */
	MAXDUR(t2)>=min_size &&	/* height of task t2 can cause a problem
				   for at least one frontier */
	((pdata->options&OPT_ATMOST)
	 ? MINHEIGHT(t2)>best_delta
	 : MAXHEIGHT(t2)<best_delta)) {

      /* scan the different frontiers in order to prune task t2
	 according to these different frontier parts */

      t = pdata->first_prune_frontier[vr];
      while (t!=NOTASK) {
	if (t==t2) goto next;
	start = pdata->start_frontier[t];
	end = pdata->end_frontier[t];
	best_delta = pdata->best_status_frontier[t];

	/* if task t2 has no compulsory part
	   or has a compulsory part that ends before start of frontier of t
	   or has a compulsory part that starts after end of frontier of t
	   (no interference of task t2 with best_status_frontier[t]) */

	if (!HAS_COMPULSORY_PART(t2) ||
	    (HAS_COMPULSORY_PART(t2) && ECT(t2)-1 < start) ||
	    (HAS_COMPULSORY_PART(t2) && LST(t2)   > end) ||
	    ((pdata->options&OPT_ATMOST)
	     ? (pdata->min_min_height>=0 && MINHEIGHT(t)+MINHEIGHT(t2)>CAPACITY(vr))
	     : (pdata->max_max_height<=0 && MAXHEIGHT(t)+MAXHEIGHT(t2)<CAPACITY(vr)))) {

	  /* if there is a capacity overflow or underflow
	     then forbid task t2 to cover completely the frontier of task t */

	  if ((pdata->options&OPT_ATMOST)
	      ? best_delta+MINHEIGHT(t)+MINHEIGHT(t2) > CAPACITY(vr)
	      : best_delta+MAXHEIGHT(t)+MAXHEIGHT(t2) < CAPACITY(vr)) {

	    /* record current state of task t2 before pruning */

	    long previous_task_state = task_state(t2);

	    /* try to prune origin and end attributes of task t2 */

	    if (MINDUR(t2)>=end-start+1)
	      if (!prune_origin_interval(t2, end+1-MINDUR(t2), start) ||
		  !prune_end_interval(t2, end+1, start+MINDUR(t2)))
		return FALSE;

	    /* try to prune maximum duration of task t2 */

	    if (MAXDUR(t2)>=end-start+1 && EST(t2)<=start && LCT(t2)>=end+1) {
	      long maxd = end-EST(t2);
	      long l2 = LCT(t2)-start-1;

	      if (maxd<l2) maxd = l2;
	      if (maxd<0)  maxd = 0;
	      if (!fix_duration_interval(t2, MINDUR(t2), maxd))
		return FALSE;
	    }

	    /* handle saturation if there was some pruning */

	    if (previous_task_state != task_state(t2)) {
	      if (!task_renormalize(t2))
		return FALSE;
	      pdata->queue = fd_or_interval(pdata->queue,TABLE_EST_T(t2),
					    TABLE_LCT_T(t2));
	    }
	  }
	}
      next:
	t = pdata->next_prune_frontier[t];
      }
    }
    t2 = pdata->next_check_frontier[t2]; /* get next candidate to prune */
  }
  return TRUE;  
}



/*** support for scanning multiple event lists ***/

static int
get_first_event MAGIC (HIDDEN_PROTO
		       int maxevnc, MACHINE vr)
{
  struct symmcum_data *pdata = fd.gdata;
  int jnc = -1;
  TAGGED trr = MakeSmall(REAL(vr));
  EVENT evnc;
  
  do {
    jnc++;
    evnc = jnc==maxevnc ? NOEVENT : pdata->eventqnc[jnc];
  } while (evnc!=NOEVENT && !fd_member(trr,EV_MACHINE(evnc)));
  return jnc;
}




/* precond: either *jcp == firstnonvr or *jcp is a valid event for vr,
   where firstnonvr is the first event index that is invalid for vr */
/* precond: either *jncp == maxevnc or *jncp is a valid event for vr */
/* We must process PROFILE events before PRUNING events.
   Only non-compulsory events can be PRUNING events.
   Hence, break ties (same date) by choosing compulsory events. */
static EVENT
get_next_event MAGIC (HIDDEN_PROTO
		      int *jcp,
		      int maxevc,
		      int *jncp,
		      int maxevnc,
		      MACHINE vr)
{
  struct symmcum_data *pdata = fd.gdata;
  EVENT evc, evnc, evnc1;
  TAGGED trr = MakeSmall(REAL(vr));
  int jc = *jcp;
  int jnc = *jncp;

  evc = jc==maxevc ? NOEVENT : pdata->eventqc[jc];
  if (evc!=NOEVENT && EV_MACHINE(evc)!=vr)
    evc = NOEVENT;
  evnc = jnc==maxevnc ? NOEVENT : pdata->eventqnc[jnc];

  if (evc!=NOEVENT && (evnc==NOEVENT || EV_DATE(evc)<=EV_DATE(evnc))) {
    *jcp = jc+1;
    return evc;
  } else if (evnc!=NOEVENT) {
    do {
      jnc++;
      evnc1 = jnc==maxevnc ? NOEVENT : pdata->eventqnc[jnc];
    } while (evnc1!=NOEVENT && !fd_member(trr,EV_MACHINE(evnc1)));
    *jncp = jnc;
    return evnc;
  } else {
    return NOEVENT;
  }
}


static void
add_compulsory_events MAGIC (HIDDEN_PROTO
			     long from, long to,
			     TASK t, long h, MACHINE vr, int flags,
			     long low, long up, int *jcp, EVENT *evp)
{
  struct symmcum_data *pdata = fd.gdata;
  int jc = *jcp;
  EVENT ev = *evp;

  if (from<low) from = low;
  EV_DATE(ev) = from;
  EV_TASK(ev) = t;
  EV_HEIGHT(ev) = h;
  EV_MACHINE(ev) = vr;
  EV_FLAGS(ev) = flags|EV_START|EV_COMPULSORY_PART;
  pdata->eventqc[jc++] = ev++;
      
  if (to<up) {
    EV_DATE(ev) = to;
    EV_TASK(ev) = t;
    EV_HEIGHT(ev) = h;
    EV_MACHINE(ev) = vr;
    EV_FLAGS(ev) = flags|EV_END|EV_COMPULSORY_PART;
    pdata->eventqc[jc++] = ev++;
  }

  *jcp = jc;
  *evp = ev;
}


static void
add_non_compulsory_events MAGIC (HIDDEN_PROTO
				 long from, long to, TASK t, int flags,
				 long low, long up, int *jncp, EVENT *evp)
{
  struct symmcum_data *pdata = fd.gdata;
  int jnc = *jncp;
  EVENT ev = *evp;
  long h = HTASK(t);
  
  if (from<low) from = low;
  EV_DATE(ev) = from;
  EV_TASK(ev) = t;
  EV_HEIGHT(ev) = h;
  EV_MACHINE(ev) = SETMACH(t);
  EV_FLAGS(ev) = flags|EV_START;
  pdata->eventqnc[jnc++] = ev++;
      
  if (to<up) {
    EV_DATE(ev) = to;
    EV_TASK(ev) = t;
    EV_HEIGHT(ev) = h;
    EV_MACHINE(ev) = SETMACH(t);
    EV_FLAGS(ev) = flags|EV_END;
    pdata->eventqnc[jnc++] = ev++;
  }

  *jncp = jnc;
  *evp = ev;
}


static BOOL
check_interval MAGIC (HIDDEN_PROTO long low, long up)
{
  struct symmcum_data *pdata = fd.gdata;
  int i, jc, jnc, maxevc, maxevnc, nfiltered=0, rc=FALSE;
  long d;
  MACHINE vr;
  EVENT ev;
  TAGGED trr;
  int k, sup, iend, mid;
  int ntasks = pdata->ntasks;
  int nmachines = pdata->nmachines;
  TAGGED comp_ev_machines = EmptySet;
  FDITER it;

  if (low>=up)
    return TRUE;

  /* allocate volatile arrays */

  pdata->filtered = Malloc(ntasks,TASK);
  pdata->eventqc = Malloc(4*ntasks,EVENT);
  pdata->eventqnc = Malloc(8*ntasks,EVENT);
  pdata->stack_prune = Malloc(ntasks,TASK);
  pdata->size_ = Malloc(ntasks,long);
  if (pdata->options&OPT_GENERALIZATION) {
    pdata->origin_profile = Malloc(ntasks,PROFILE);
    pdata->end_profile = Malloc(ntasks,PROFILE);
    pdata->duration_profile = Malloc(ntasks,PROFILE);
    pdata->height_profile = Malloc(ntasks,PROFILE);
    pdata->origin_tempo = Malloc(ntasks,PROFILE);
    pdata->end_tempo = Malloc(ntasks,PROFILE);
    pdata->duration_tempo = Malloc(ntasks,PROFILE);
    pdata->height_tempo = Malloc(ntasks,PROFILE);
    pdata->origin_stamp = Malloc(ntasks,long);
    pdata->end_stamp = Malloc(ntasks,long);
    pdata->duration_stamp = Malloc(ntasks,long);
    pdata->height_stamp = Malloc(ntasks,long);
  }
  pdata->start_frontier = Malloc(ntasks,long);
  pdata->end_frontier = Malloc(ntasks,long);
  pdata->first_sweep_frontier = Malloc(nmachines,TASK);
  pdata->next_sweep_frontier = Malloc(ntasks,TASK);
  pdata->first_prune_frontier = Malloc(nmachines,TASK);
  pdata->next_prune_frontier = Malloc(ntasks,TASK);
  pdata->first_check_frontier = Malloc(nmachines,TASK);
  pdata->next_check_frontier = Malloc(ntasks,TASK);
  pdata->init_status_frontier = Malloc(ntasks,long);
  pdata->best_status_frontier = Malloc(ntasks,long);
  pdata->event.date = Malloc(10*ntasks,long);
  pdata->event.task = Malloc(10*ntasks,TASK);
  pdata->event.height = Malloc(10*ntasks,long);
  pdata->event.machine = Malloc(10*ntasks,MACHINE);
  pdata->event.flags = Malloc(10*ntasks,unsigned int);

  /* record tasks that may intersect at least one point of low..up
     (candidates for potential pruning) */

  for (k=0; k<2; k++) {

    /* dichotomic search for min i such that LCTKEY(TASK(i)) > low 
       and min iend such that EST(TASK(j | j>=i)) >= up */

    if (k==0) {
      i = 0;
      sup = iend = pdata->ntargets;
      while (i<sup) {
	mid = (i+sup)>>1;
	if (LCTKEY(TASK(mid))<=low)
	  i = mid+1;
	else
	  sup = mid;
      }
    } else {
      i = pdata->ntargets;
      iend = i + pdata->nsources;
    }


    while (i<iend && MINEST(i)<up) {
      TASK t = TASK(i++);

      if ((FLAGS(t)&STATUS_SOURCE) &&
	  MAY_OVERLAP(t,low,up)) {
	TAGGED setmach = SETMACH(t);
	
	FILTERED(nfiltered++) = t;
	comp_ev_machines = fd_merge_into(setmach,comp_ev_machines);
	SIZE_(t) = 1;
	if ((pdata->options&OPT_GENERALIZATION) && (FLAGS(t)&STATUS_TARGET)) {
	  if ((SIZE_(t) = SIZEMACH(t)) > 1) {
	    ORIGIN_PROFILE(t) = empty_profile();
	    END_PROFILE(t) = empty_profile();
	    DURATION_PROFILE(t) = empty_profile();
	    HEIGHT_PROFILE(t) = empty_profile();
	  } else {
	    ORIGIN_PROFILE(t) = NULL;
	    END_PROFILE(t) = NULL;
	    DURATION_PROFILE(t) = NULL;
	    HEIGHT_PROFILE(t) = NULL;
	  }
	  ORIGIN_TEMPO(t) = NOPROFILE;
	  END_TEMPO(t) = NOPROFILE;
	  DURATION_TEMPO(t) = NOPROFILE;
	  HEIGHT_TEMPO(t) = NOPROFILE;
	  ORIGIN_STAMP(t) = NOMACHINE;
	  END_STAMP(t) = NOMACHINE;
	  DURATION_STAMP(t) = NOMACHINE;
	  HEIGHT_STAMP(t) = NOMACHINE;
	}
      }
    }
  }

  for (vr=0; vr<nmachines; vr++) {
    pdata->first_sweep_frontier[vr] = NOTASK;
    pdata->first_prune_frontier[vr] = NOTASK;
    pdata->first_check_frontier[vr] = NOTASK;
  }

  pdata->generalize_nvar = 0;
  if (pdata->options&OPT_GENERALIZATION) {
    pdata->generalize_tempo_marray = Malloc(4*nfiltered,PROFILE *);
    pdata->generalize_profile_marray = Malloc(4*nfiltered,PROFILE *);
    pdata->generalize_itask = Malloc(4*nfiltered,TASK);
    pdata->generalize_dvar = Malloc(4*nfiltered,int);
  }

  /* initialize the event queue with all start and end events */

  jc = 0;
  jnc = 0;
  ev = 0;
  for (i=0; i<nfiltered; i++) {
    TASK t = FILTERED(i);
    long h = ((pdata->options&OPT_ATMOST) ? MINHEIGHT(t) : MAXHEIGHT(t));
    BOOL assigned_t = (SIZEMACH(t)==1);
    long capa, capb;

    trr = RangeMin(CTagToCar(SETMACH(t)));
    vr = virtual_machine(GetSmall(trr));
    M_STATUS(t) = NOMACHINE;
    HTASK(t) = h;

    /* generate pruning events for target tasks */

    if (t<=pdata->last_to_prune && (FLAGS(t)&STATUS_TARGET)) {
      add_non_compulsory_events(EST(t),LCT(t),t,
				EV_TYPE_PRUNING|EV_CHECK_NONE,
				low,up,&jnc,&ev);
    }

    if (assigned_t) {
      capa = capb = CAPACITY(vr);
    } else if ((pdata->options&OPT_ATMOST)) {
      capa = pdata->capacity_max;
      capb = pdata->capacity_min;
    } else {
      capa = pdata->capacity_min;
      capb = pdata->capacity_max;
    }

    /* if t is an unconstrained task on at least one machine */

    if ((pdata->options&OPT_ATMOST) ? (h<=0 && h<=capa) : (h>=0 && h>=capa)) {
      int chk = (assigned_t ? EV_CHECK_NONE : EV_CHECK_GE);
      
      /* generate an event for EST(t)..LCT(t) */
      
      add_non_compulsory_events(EST(t),LCT(t),t,
				EV_TYPE_PROFILE|chk,
				low,up,&jnc,&ev);
    } else

      /* if t is a constrained task that is assigned to a machine,
	 insert i in the list of tasks subject to frontier-based pruning */

      if (assigned_t && ((pdata->options&OPT_ATMOST) ? h>0 : h<0)) {
	pdata->next_check_frontier[t] = pdata->first_check_frontier[vr];
	pdata->first_check_frontier[vr] = t;

	/* if t definitely intersects low..up */

	if (MUST_OVERLAP(t,low,up)) {

	  /* if t has a compulsory part */

	  if (HAS_COMPULSORY_PART(t)) {
	    add_compulsory_events(LST(t),ECT(t),t,HTASK(t),vr,
				  EV_TYPE_PROFILE|EV_CHECK_NONE,
				  low,up,&jc,&ev);
	  } else

	    /* if t has a not too big frontier part
	       (which is included in low..up, already verified) */

	    if (LST(t)-ECT(t)+2 <= pdata->max_duration) {
	      pdata->start_frontier[t] = ECT(t)-1;
	      pdata->end_frontier[t] = LST(t);
	      pdata->next_sweep_frontier[t] = pdata->first_sweep_frontier[vr];
	      pdata->first_sweep_frontier[vr] = t;
	      pdata->first_prune_frontier[vr] = NOTASK;
	      pdata->init_status_frontier[t] = 0;

	    }
	}
      }

    /* if t may be a reserve task */

    if ((pdata->options&OPT_ATMOST) ? (h<=0 && h>capb) : (h>=0 && h<capb)) {
      int chk = (assigned_t ? EV_CHECK_NONE : EV_CHECK_LT);

      /* if t is assigned to a machine and has a compulsory part
	 which intersects low..up */

      if (assigned_t && HAS_COMPULSORY_PART(t) && MUST_OVERLAP(t,low,up))
	add_compulsory_events(LST(t),ECT(t),t,0,vr,
			      EV_TYPE_PROFILE|EV_CHECK_NONE,
			      low,up,&jc,&ev);
	
      add_non_compulsory_events(EST(t),LCT(t),t,
				EV_TYPE_PROFILE|chk,
				low,up,&jnc,&ev);
    }
  }
  
  /* sort the different lists of events: compulsory events associated
     to each resource, and non-compulsory events */

  pdata->nfiltered = nfiltered;
  maxevc = jc;
  maxevnc = jnc;
  qsort_asc_eventc(pdata->eventqc, maxevc);
  qsort_asc_eventnc(pdata->eventqnc, maxevnc);

  /* build the cumulative profile resource by resource */

  jc = 0;
  fditer_init(&it, comp_ev_machines);
  while (!fditer_empty(&it)) {
    trr = fditer_next(&it);
    vr = virtual_machine(GetSmall(trr));
    pdata->profile = 0;
    pdata->nconstrained = 0;
    pdata->top_prune = 0;
    d = low;
    jnc = get_first_event(maxevnc,vr);
    ev = get_next_event(&jc,maxevc,&jnc,maxevnc,vr);
    while (ev!=NOEVENT) {
      TASK t = EV_TASK(ev);
      unsigned int flags = EV_FLAGS(ev);
      long nextd = EV_DATE(ev);

      if (flags & EV_TYPE_PRUNING) {
	if (flags & EV_START)
	  pdata->stack_prune[pdata->top_prune++] = t;
      } else {
	long h = EV_HEIGHT(ev);
	int inc = ((flags & EV_START) ? 1 : -1);
	
	if (nextd>d) {
	  if (!check_resource(vr,d,nextd))
	    goto ret;
	  update_status_of_frontier_tasks(vr,d,nextd,pdata->profile);
	  d = nextd;
	}
	if ((flags & EV_CHECK_NONE) ||
	    ((flags & EV_CHECK_GE) && !OVERFLOWS(h,vr)) ||
	    ((flags & EV_CHECK_LT) && OVERFLOWS(h,vr))) {
	  pdata->profile += h*inc;
	  if (flags & EV_COMPULSORY_PART)
	    pdata->nconstrained += inc;
	  M_STATUS(t) = (flags & EV_START) ? vr : NOMACHINE;
	}
      }
      ev = get_next_event(&jc,maxevc,&jnc,maxevnc,vr);
    }
    if (!check_resource(vr,d,up))
      goto ret;
    update_status_of_frontier_tasks(vr,d,up,pdata->profile);

    /* Transfer the temporary marrays that were modified during the current
       sweep of the events of resource vr to the profile marray
       associated to each variable we try to prune 
       (for all nonzero values of marray_source, add 1 to marray_destination)
       If empty domain after sweep on a resource, prune machine attribute.
    */

    for (i=pdata->generalize_nvar-1; i>=0; --i) {
      PROFILE src  = *pdata->generalize_tempo_marray[i];
      PROFILE *dest = pdata->generalize_profile_marray[i];
      TASK t       = pdata->generalize_itask[i];
      int dvar     = pdata->generalize_dvar[i];
      TAGGED fdset=0;
      long b, e, h;

      switch (dvar) {
      case PRUNED_ORIGIN:
	fdset = fd_and_interval(SETORG(t),EST_T(t),LST_T(t));
	break;
      case PRUNED_END:
	fdset = fd_and_interval(SETEND(t),ECT_T(t),LCT_T(t));
	break;
      case PRUNED_DURATION:
	fdset = fd_and_interval(SETDUR(t),MINDUR_T(t),MAXDUR_T(t));
	break;
      case PRUNED_HEIGHT:
	fdset = fd_and_interval(SETHEIGHT(t),MINHEIGHT_T(t),MAXHEIGHT_T(t));
	break;
      }

      while (profile_next(src,&b,&e,&h,&src)) {
	*dest = profile_update(*dest,b,e,1);
	fdset = fd_subtract_interval(fdset,MakeSmall(b),MakeSmall(e-1));
      }

      if (fdset==EmptySet && fd_member(trr,SETMACH(t))) {
	if (!prune_machine_value(t,trr))
	  goto ret;
	pdata->queue = fd_or_interval(pdata->queue,TABLE_EST_T(t),
				      TABLE_LCT_T(t));
      }
    }
    pdata->generalize_nvar = 0;

    /* try to prune according to the "small" frontier part
       of the tasks assigned to machine vr */

    if (!try_to_prune_according_to_frontier(vr))
      goto ret;
  }
  
  for (i=0; i<nfiltered; i++) {
    TASK t = FILTERED(i);
    long size = SIZE_(t);

    if ((FLAGS(t) & STATUS_TARGET) && size>1) {
      if (!prune_generalize(t,size,ORIGIN_PROFILE(t),(prune_origin_interval)) ||
	  !prune_generalize(t,size,END_PROFILE(t),(prune_end_interval)) ||
	  !prune_generalize(t,size,DURATION_PROFILE(t),(prune_duration_interval)) ||
	  !prune_generalize(t,size,HEIGHT_PROFILE(t),(prune_height_interval)))
	goto ret;
    }
  }
  rc = TRUE;
 ret:
  if (pdata->options&OPT_GENERALIZATION)
    for (i=0; i<nfiltered; i++) {
      TASK t = FILTERED(i);

      if (FLAGS(t) & STATUS_TARGET) {
	if (SIZE_(t)>1) {
	  profile_dispose(ORIGIN_PROFILE(t));
	  profile_dispose(END_PROFILE(t));
	  profile_dispose(DURATION_PROFILE(t));
	  profile_dispose(HEIGHT_PROFILE(t));
	}
	if (ORIGIN_TEMPO(t)!=NOPROFILE)
	  profile_dispose(ORIGIN_TEMPO(t));
	if (END_TEMPO(t)!=NOPROFILE)
	  profile_dispose(END_TEMPO(t));
	if (DURATION_TEMPO(t)!=NOPROFILE)
	  profile_dispose(DURATION_TEMPO(t));
	if (HEIGHT_TEMPO(t)!=NOPROFILE)
	  profile_dispose(HEIGHT_TEMPO(t));
      }
    }
  Free(pdata->filtered);
  Free(pdata->eventqc);
  Free(pdata->eventqnc);
  Free(pdata->stack_prune);
  Free(pdata->size_);
  if (pdata->options&OPT_GENERALIZATION) {
    Free(pdata->origin_profile);
    Free(pdata->end_profile);
    Free(pdata->duration_profile);
    Free(pdata->height_profile);
    Free(pdata->origin_tempo);
    Free(pdata->end_tempo);
    Free(pdata->duration_tempo);
    Free(pdata->height_tempo);
    Free(pdata->origin_stamp);
    Free(pdata->end_stamp);
    Free(pdata->duration_stamp);
    Free(pdata->height_stamp);
    Free(pdata->generalize_tempo_marray);
    Free(pdata->generalize_profile_marray);
    Free(pdata->generalize_itask);
    Free(pdata->generalize_dvar);
  }
  Free(pdata->start_frontier);
  Free(pdata->end_frontier);
  Free(pdata->first_sweep_frontier);
  Free(pdata->next_sweep_frontier);
  Free(pdata->first_prune_frontier);
  Free(pdata->next_prune_frontier);
  Free(pdata->first_check_frontier);
  Free(pdata->next_check_frontier);
  Free(pdata->init_status_frontier);
  Free(pdata->best_status_frontier);
  Free(pdata->event.date);
  Free(pdata->event.task);
  Free(pdata->event.height);
  Free(pdata->event.machine);
  Free(pdata->event.flags);
  return rc;
}


/*** support for task intervals ***/

static long 
ti_min_length_intersection MAGIC (HIDDEN_PROTO long low,long up,TASK t)
{
  struct symmcum_data *pdata = fd.gdata;
  long j1, j2, j3, j4;

  j1 = MINDUR(t);
  j2 = up-low;
  j3 = ECT(t)-low;
  j4 = up-LST(t);
  if (j1>j2) j1 = j2;
  if (j1>j3) j1 = j3;
  if (j1>j4) j1 = j4;
  if (j1<0)  j1 = 0;
  return j1;
}


static long 
ti_max_length_intersection MAGIC (HIDDEN_PROTO long low,long up,TASK t)
{
  struct symmcum_data *pdata = fd.gdata;
  long mino = EST(t);
  long maxo = LST(t);
  long maxd = MAXDUR(t);
  long max_inter, start_max_inter, end_max_inter, min, max;
  TAGGED setorg;

  if (maxo<low-maxd+1 || mino>=up || maxd==0)
    return 0;
  if (up-low>maxd) {
    max_inter = maxd;
    start_max_inter = low;
  } else {
    max_inter = up-low;
    start_max_inter = up-maxd;
  }
  if (maxo<start_max_inter)
    return max_inter - (start_max_inter-maxo);
  end_max_inter = up-max_inter;
  if (mino>end_max_inter)
    return max_inter - (mino-end_max_inter);

  min = FDMAX(start_max_inter,mino);
  max = FDMIN(end_max_inter,maxo);
  if (min<=max &&
      fd_compare_interval(SETORG(t),MakeSmall(min),MakeSmall(max))!=
      FDI_DISJOINT)
    return max_inter;

  /* TODO: simplify this loop */
  setorg = SETORG(t);
  if (FLAGS(t) & PRUNED_ORIGIN)
    setorg = fd_and_interval(setorg,MakeSmall(mino),MakeSmall(maxo));
  for (max_inter--;
       max_inter>0 &&
       !fd_member(MakeSmall(start_max_inter-1),setorg) &&
       !fd_member(MakeSmall(end_max_inter+1),setorg);
       max_inter--) {
    start_max_inter--;
    end_max_inter++;
  }
  return max_inter;
}



#define ti_min_use(low,up,r) (pdata->max_min_length_inter[r])

#define ti_max_use(low,up,r) FDMIN((up)-(low),pdata->sum_max_length_inter)

/* Input: Prune the attributes of the task t in order that the surface
   of the intersection of task t with interval low..up
   is at least equal to the value delta (> 0).
   If cum_atmost=1 then h corresponds to -MINHEIGHT(t), otherwise to
   MAXHEIGHT(t); in either case, h > 0.
   Output: An indication as to whether a solution may exist.
*/
static BOOL
ti_force_to_cover_atleast MAGIC (HIDDEN_PROTO
				 long low,long up,
				 TASK t,long delta,long h)
{
  struct symmcum_data *pdata = fd.gdata;
  long maxd = MAXDUR(t);
  long T, limit, inter;

  /* Invariant: delta>0 and h>0 */
  /* Try to prune the origin, end and duration attributes of task t
     according to the required minimum intersection in time T
     of task t with interval low..up */

  T = (delta+h-1) / h;

  /* Restrict the range of the origin attribute of task t */

  if (!fix_origin_interval(t, low+T-maxd, up-T))
    return FALSE;

  /* Restrict the range of the end attribute of task t */

  if (!fix_end_interval(t, low+T, up-T+maxd))
    return FALSE;

  /* Adjust the minimum duration of task t */

  if (!fix_duration_interval(t, T, maxd) ||
      (LST(t)<low && !fix_duration_interval(t, low-LST(t)+T, maxd)) ||
      (ECT(t)>up  && !fix_duration_interval(t, ECT(t)-up+T, maxd)))
    return FALSE;

  /* Adjust the maximum or minimum height of task t */

  inter = ti_max_length_intersection(low,up,t);
  if (inter==0)
    return FALSE;
  limit = (delta+inter-1) / inter;
  if ((pdata->options&OPT_ATMOST)
      ? !fix_height_interval(t, MINHEIGHT(t), -limit)
      : !fix_height_interval(t, limit, MAXHEIGHT(t)))
    return FALSE;

  return TRUE;
}


/* Input: Prune the attributes of the task t in order that the surface
   of the intersection of task t with interval low..up
   is at most equal to the value delta (>= 0).
   If cum_atmost=1 then h corresponds to MINHEIGHT(t), otherwise to
   -MAXHEIGHT(t); in either case, h > 0.
   Output: An indication as to whether a solution may exist.
*/
static BOOL
ti_forbid_to_cover_more MAGIC (HIDDEN_PROTO
			       long low,long up,
			       TASK t,long delta,long h)
{
  struct symmcum_data *pdata = fd.gdata;
  long mind = MINDUR(t);
  long T, limit, inter;

  /* Invariant: delta>=0 and h>0 */
  /* Try to prune the origin, end and duration attributes of task t
     according to the maximum possible intersection in time T
     of task t with interval low..up */

  T = (delta+h) / h;
  if (T<=up-low) {
    
    /* Adjust the origin and end attributes of task t */

    if (mind>=T) {
      if (!prune_origin_interval(t, low+T-mind, up-T) ||
	  !prune_end_interval(t, low+T, up-T+mind))
	return FALSE;
    }

    /* Adjust the maximum duration of task t */

    if (EST(t)<=up-T && LCT(t)>=low+T && MAXDUR(t)>=T) {
      long limit = FDMAX(low+T-EST(t)-1,LCT(t)-up+T-1);

      if (limit<0) limit = 0;
      if (!fix_duration_interval(t, mind, limit))
	return FALSE;
    }
  }

  /* Adjust the maximum or minimum height of task t */

  inter = ti_min_length_intersection(low,up,t);
  if (inter>0) {
    limit = delta / inter;    
    if ((pdata->options&OPT_ATMOST)
	? !fix_height_interval(t, MINHEIGHT(t), limit)
	: !fix_height_interval(t, -limit, MAXHEIGHT(t)))
      return FALSE;
  }
  
  return TRUE;
}



/* Input: an interval low..up and the machines machines for which we do the check.
   Output: An indication as to whether a solution may exist.
*/
static BOOL
ti_task_interval MAGIC (HIDDEN_PROTO
			long low,long up,TAGGED machines)
{
  struct symmcum_data *pdata = fd.gdata;
  int ntasks = pdata->ntasks;
  int nmachines = pdata->nmachines;
  int ntry_to_prune = 0;
  long sum_slack, slack, capr, sum_contribution, sum_contribution_i, h, inter,
       delta, previous_task_state, absh;
  MACHINE vr;
  BOOL result = FALSE;
  int i, k, sup, iend, mid;
  FDITER it;

  if (low>=up)
    return TRUE;

  /* Allocate temporary arrays. */

  pdata->sum_max_length_inter = 0;
  pdata->try_to_prune = Malloc(ntasks,TASK);
  pdata->include_in_machines = Malloc(ntasks,BOOL);
  pdata->min_length_inter = Malloc(ntasks,long);
  pdata->max_length_inter = Malloc(ntasks,long);
  pdata->max_min_length_inter = Malloc(nmachines,long);
  pdata->contribution = Malloc(ntasks,long);
  pdata->hcontribution = Malloc(ntasks,long);

  /* Get tasks that interact with interval low..up and machines
     and compute required information. */

  for (vr=0; vr<nmachines; vr++)
    pdata->max_min_length_inter[vr] = 0;

  for (k=0; k<2; k++) {


  /* dichotomic search for min i such that LCTKEY(TASK(i)) > low 
                       and min iend such that EST(TASK(j | j>=i)) >= up */

    if (k==0) {
      i = 0;
      sup = iend = pdata->ntargets;
      while (i<sup) {
	mid = (i+sup)>>1;
	if (LCTKEY(TASK(mid))<=low)
	  i = mid+1;
	else
	  sup = mid;
      }
    } else {
      i = pdata->ntargets;
      iend = i + pdata->nsources;
    }

    while (i<iend && MINEST(i)<up) {
      TASK t = TASK(i++);
      int overlap = fd_compare(SETMACH(t),machines);

      if ((FLAGS(t)&STATUS_SOURCE) &&
	  overlap != FDI_DISJOINT &&
	  MAY_OVERLAP(t,low,up)) {
	pdata->try_to_prune[ntry_to_prune++] = t;
	pdata->include_in_machines[t] = (overlap==FDI_SUBSET || overlap==FDI_EQUAL);
	pdata->min_length_inter[t] = ti_min_length_intersection(low,up,t);
	pdata->max_length_inter[t] = ti_max_length_intersection(low,up,t);
	if (SIZEMACH(t)==1) {
	  vr = virtual_machine(GetSmall(RangeMin(CTagToCar(SETMACH(t)))));
	  if (pdata->max_min_length_inter[vr]<pdata->min_length_inter[t])
	    pdata->max_min_length_inter[vr] = pdata->min_length_inter[t];
	}
	pdata->sum_max_length_inter += pdata->max_length_inter[t];
      }
    }
  }

  /* Compute sum of available slack on the different machines
     and on interval low..up */

  sum_slack = 0;
  fditer_init(&it, machines);
  while (!fditer_empty(&it)) {
    vr = virtual_machine(GetSmall(fditer_next(&it)));
    capr = CAPACITY(vr);
    if ((pdata->options&OPT_ATMOST) == (capr>=0))
      slack = ti_max_use(low,up,vr) * capr;
    else
      slack = ti_min_use(low,up,vr) * capr;
    sum_slack += slack;
  }

  /* Compute sum of contributions of the tasks on interval low..up */

  sum_contribution = 0;
  for (i=0; i<ntry_to_prune; i++) {
    TASK t = pdata->try_to_prune[i];	/* MACHINE[t] intersects machines */
    
    h = ((pdata->options&OPT_ATMOST) ? MINHEIGHT(t) : MAXHEIGHT(t));
    pdata->hcontribution[t] = h;
    inter = 0;

    /* If task t can produce some free space then consume maximum free space */

    if ((pdata->options&OPT_ATMOST) ? h<=0 : h>=0)
      inter = pdata->max_length_inter[t];

    /* If task t consumes some available space then compute minimum used space */

    else if (pdata->include_in_machines[t])
      inter = pdata->min_length_inter[t];

    pdata->contribution[t] = inter*h;
    sum_contribution += pdata->contribution[t];
  }

  /* Fail of necessary condition does not hold */

  if ((pdata->options&OPT_ATMOST)
      ? sum_contribution>sum_slack
      : sum_contribution<sum_slack)
    goto ret;

  /* Try to prune the attributes of the tasks in order to try to avoid
     the necessary condition to be false */

  for (i=0; i<ntry_to_prune; i++) {
    TASK t = pdata->try_to_prune[i];

    if (FLAGS(t) & STATUS_TARGET) {
      h = pdata->hcontribution[t];
      absh = FDMAX(h,-h);
      sum_contribution_i = sum_contribution - pdata->contribution[t];
      delta = sum_contribution_i - sum_slack;
      if (delta<0)
	delta = -delta;
      previous_task_state = task_state(t);

      /* If task t can produce some free space */

      if ((pdata->options&OPT_ATMOST) ? h<=0 : h>=0) {

	/* If this free space is absolutely needed then:
	   - force task t to be assigned on one of machines,
	   - force task t to cover a minimum surface */

	if ((pdata->options&OPT_ATMOST)
	    ? sum_contribution_i>sum_slack
	    : sum_contribution_i<sum_slack)
	  if (!fix_machine_values(t,machines) ||
	      !ti_force_to_cover_atleast(low,up,t,delta,absh))
	    goto ret;
      } else {
	if (pdata->include_in_machines[t]) {
	  if (!ti_forbid_to_cover_more(low,up,t,delta,absh))
	    goto ret;
	} else {

	  /* If the minimum intersection is too important
	     then forbid to assign the task on one of machines
	     (since !include_in_machines[t], the contribution of task t
	     in sum_contribution was null) */

	  if ((pdata->options&OPT_ATMOST)
	       ? sum_contribution+pdata->min_length_inter[t]*h > sum_slack
	       : sum_contribution+pdata->min_length_inter[t]*h < sum_slack)
	    if (!prune_machine_values(t,machines))
	      goto ret;
	}
      }

      /* If task state did change then add an event for saturation */

      if (previous_task_state!=task_state(t)) {
	if (!task_renormalize(t))
	  goto ret;
	pdata->queue = fd_or_interval(pdata->queue,TABLE_EST_T(t),
				                   TABLE_LCT_T(t));
      }
    }
  }

  /* Deallocate temporary arrays and exit */
  
  result = TRUE;
 ret:
  Free(pdata->try_to_prune);
  Free(pdata->include_in_machines);
  Free(pdata->min_length_inter);
  Free(pdata->max_length_inter);
  Free(pdata->max_min_length_inter);
  Free(pdata->contribution);
  Free(pdata->hcontribution);

  return result;
}


/* Compute for each i in [low,up): MINEST(i) = EST(TASK(i...up-1)) */
static void
build_minest MAGIC (HIDDEN_PROTO int low, int up)
{
  struct symmcum_data *pdata = fd.gdata;
  long min, est;
  int i;

  if (low<up) {
    min = EST(TASK(up-1));
    MINEST(up-1) = min;
    for (i=up-2; i>=low; i--) {
      est = EST(TASK(i));
      if (min>est) min = est;
      MINEST(i) = min;
    }
  }
}


/* Repartition all tasks into [ TARGET+SOURCE | SOURCE | 0 ] */
/* Update pdata->ntargets, pdata->nsources */
/* 3.9 invariant: target partition sorted by ascending lctkey */
/* 3.9 invariant: source partition sorted by ascending age (of becoming source) */
/* 3.9 invariant: 0 partition sorted by ascending age, older than source part */
static void
update_task_states MAGIC (HIDDEN_PROTO struct symmcum_data *pdata)
{
  
  TAGGED timepoints = EmptySet;	/* projection of targets on time axis */
  /* 3.9 TAGGED machines = EmptySet;  projection of targets on machine axis */
  int ntargets = pdata->ntargets;
  int nactive = ntargets + pdata->nsources;
  int i, delta1, delta2, inf, sup;
  TASK *target = pdata->target;
  TASK held, t;

  /* Phase 1: filter targets */
  inf = 0;
  sup = ntargets-1;
  t = target[inf];
  held = target[sup];
  while (inf<=sup) {
    if (!(LST_T(t)==EST_T(t) && /* all attributes ground? */
	  LCT_T(t)==ECT_T(t) &&
	  MAXDUR_T(t)==MINDUR_T(t) &&
	  MAXHEIGHT_T(t)==MINHEIGHT_T(t) &&
	  SIZEMACH(t)==1)) {
      if (EST_T(t)!=LCT_T(t))
	timepoints = fd_or_interval(timepoints,
				    pdata->initial_est[t],
				    pdata->initial_lct[t]-IStep(1));
      /* 3.9 machines = fd_merge_into(SETMACH(t),machines); */
      target[inf] = t;
      inf++;
      t = (inf>=sup ? held : target[inf]);
    } else {
      FLAGS(t) &= ~STATUS_TARGET;
      target[sup] = t;
      sup--;
      t = (inf>=sup ? held : target[sup]);
    }
  }

  delta1 = ntargets-inf;
  if (delta1>0) {
    ntargets -= delta1;
    pdata->ntargets -= delta1;
    pdata->nsources += delta1;
    qsort_asc_lctkey(target, ntargets);
  }

  /* Phase 2: filter sources */
  for (i=ntargets, sup=i; i<nactive; i++) {
    t = target[i];
    if (!(MAXDUR(t)==0 ||
	  fd_compare_interval(timepoints,
			      pdata->initial_est[t],
			      pdata->initial_lct[t]-IStep(1))==FDI_DISJOINT
	  /* 3.9 || fd_compare(machines,SETMACH(t))==FDI_DISJOINT */))
      sup = i+1;
    else
      FLAGS(t) &= ~STATUS_SOURCE;
  }
  delta2 = i-sup;
  if (delta2>0) {
    pdata->nsources -= delta2;
    nactive -= delta2;		/* keep me AFTER the for loop */
  }

  /* Phase 3: rebuild MINEST array */

  if (delta1>0 || delta2>0) {
    build_minest(0, ntargets);
    build_minest(ntargets, nactive);
  }
}


/* '$fd_cumulatives'(+State0, -State, -Actions).

   State = f(NT,NM,Opt,Tasks,Machines,NTargets,NSources,_Handle,0)

*/
void SPCDECL 
prolog_fd_cumulatives MAGIC (HIDDEN_PROTO
			     SP_term_ref State0,
			     SP_term_ref State,
			     SP_term_ref Actions)
{
  WAMENV;
  TAGGED list, handle, tmp;
  int i, ntasks, nmachines;
  int ent = -1;			/* disentailed unless otherwise */
  BOOL committed;
  long total_size, absmin, absmax, maxdur, minmin, minmax, maxmin, maxmax;
  long state_stamp;
  struct symmcum_data *pdata;
  char *ptr;
  int ntargets, nactive;

  w->numstack_end = NULL;
  init_profile();

/*    X(0) = RefTerm(State0); */
  (void)State0;
  X(2) = atom_nil;		/* actions list */
  RefTerm(State) = unify_output_state(w,&handle,&state_stamp,&committed);

  if (!IsVar(handle)) {		/* got [Flag | '$free'(Ptr)] */
    pdata = Pdata(struct symmcum_data,handle);
    fd.gdata = pdata;
    ntasks = pdata->ntasks;
    nmachines = pdata->nmachines;
  } else {			/* build persistent state */
				/* compute flags, ntasks, use_limit */
    DerefArg(tmp,X(0),1);		/* get ntasks */
    ntasks = GetSmall(tmp);

    /* ensure heap space for integer attributes */

    X(3) = handle;
    RequireHeap(ntasks*5*INT_ATTRIBUTE_SIZE, 4);
    handle = X(3);
    
    DerefArg(tmp,X(0),2);		/* get nmachines */
    nmachines = GetSmall(tmp);
    total_size = ((27*ntasks + 2*nmachines)<<LogSizeOfWord);
    pdata = Palloc(struct symmcum_data, total_size, handle);
    fd.gdata = pdata;
    ptr = (char *)(pdata+1);
    pdata->target = (TASK *)ptr;
    ptr = (char *)(pdata->target+2*ntasks);
    pdata->minest = (long *)ptr;
    ptr = (char *)(pdata->minest+ntasks);
    pdata->initial_size = (long *)ptr;
    ptr = (char *)(pdata->initial_size+ntasks);
    pdata->initial_est = (TAGGED *)ptr;
    ptr = (char *)(pdata->initial_est+ntasks);
    pdata->initial_lct = (TAGGED *)ptr;
    ptr = (char *)(pdata->initial_lct+ntasks);
    pdata->previous_size = (long *)ptr;
    ptr = (char *)(pdata->previous_size+ntasks);
    pdata->previous_est = (TAGGED *)ptr;
    ptr = (char *)(pdata->previous_est+ntasks);
    pdata->previous_lct = (TAGGED *)ptr;
    ptr = (char *)(pdata->previous_lct+ntasks);
    /* arrays for machine variables */
    pdata->machine.real = (MACHINE *)ptr;
    ptr = (char *)(pdata->machine.real+nmachines);
    pdata->machine.capacity = (long *)ptr;
    ptr = (char *)(pdata->machine.capacity+nmachines);
    /* arrays for task variables */
    pdata->task.flags = (long *)ptr;
    ptr = (char *)(pdata->task.flags+ntasks);
    pdata->task.m_status = (MACHINE *)ptr;
    ptr = (char *)(pdata->task.m_status+ntasks);
    pdata->task.htask = (long *)ptr;
    ptr = (char *)(pdata->task.htask+ntasks);
    pdata->task.minorg = (TAGGED *)ptr;
    ptr = (char *)(pdata->task.minorg+ntasks);
    pdata->task.maxorg = (TAGGED *)ptr;
    ptr = (char *)(pdata->task.maxorg+ntasks);
    pdata->task.setorg = (TAGGED *)ptr;
    ptr = (char *)(pdata->task.setorg+ntasks);
    pdata->task.mindur = (TAGGED *)ptr;
    ptr = (char *)(pdata->task.mindur+ntasks);
    pdata->task.maxdur = (TAGGED *)ptr;
    ptr = (char *)(pdata->task.maxdur+ntasks);
    pdata->task.setdur = (TAGGED *)ptr;
    ptr = (char *)(pdata->task.setdur+ntasks);
    pdata->task.minend = (TAGGED *)ptr;
    ptr = (char *)(pdata->task.minend+ntasks);
    pdata->task.maxend = (TAGGED *)ptr;
    ptr = (char *)(pdata->task.maxend+ntasks);
    pdata->task.setend = (TAGGED *)ptr;
    ptr = (char *)(pdata->task.setend+ntasks);
    pdata->task.lctkey = (long *)ptr;
    ptr = (char *)(pdata->task.lctkey+ntasks);
    pdata->task.minheight = (TAGGED *)ptr;
    ptr = (char *)(pdata->task.minheight+ntasks);
    pdata->task.maxheight = (TAGGED *)ptr;
    ptr = (char *)(pdata->task.maxheight+ntasks);
    pdata->task.setheight = (TAGGED *)ptr;
    ptr = (char *)(pdata->task.setheight+ntasks);
    pdata->task.sizemach = (long *)ptr;
    ptr = (char *)(pdata->task.sizemach+ntasks);
    pdata->task.setmach = (TAGGED *)ptr;
    ptr = (char *)(pdata->task.setmach+ntasks);
    
    if (ptr != (char *)(pdata+1)+total_size)
      printf("SHOULDN'T HAPPEN: expected ptr=0x%p, got 0x%p\n",
	     (char *)(pdata+1)+total_size, ptr);

    pdata->destructor = symmcum_destructor;
    FD_STORE_SPENV(pdata->spenv);
    pdata->refbase = SP_alloc_term_refs(10*ntasks);
    DerefArg(tmp,X(0),3);		/* get cum_atmost */
    pdata->options = GetSmall(tmp);
    pdata->stamp = state_stamp-1; /* DON'T trust initially */
    pdata->ntasks = ntasks;
    pdata->nmachines = nmachines;
#if CSTATE_ONLY
    pdata->ntargets = ntasks;
    pdata->nsources = 0;
    pdata->ntargets_back = ntasks;
#endif

    absmin = CLPFD_MAXINT;
    absmax = -CLPFD_MAXINT;
    DerefArg(list,X(0),5);
    for (i=0; i<nmachines; i++) {
      TAGGED elt, tmp1;
      long c;
    
      DerefCar(elt,list);
      DerefCdr(list,list);
      DerefArg(tmp1,elt,1);
      REAL(i) = GetSmall(tmp1);
      DerefArg(tmp1,elt,2);
      c = GetSmall(tmp1);
      CAPACITY(i) = c;
      if (absmin>c)
	absmin = c;
      if (absmax<c)
	absmax = c;
    }
    pdata->capacity_min = absmin;
    pdata->capacity_max = absmax;

    minmin = CLPFD_MAXINT;
    minmax = CLPFD_MAXINT;
    maxmin = -CLPFD_MAXINT;
    maxmax = -CLPFD_MAXINT;
    maxdur = -CLPFD_MAXINT;
    DerefArg(list,X(0),4);
    for (i=0; i<ntasks; i++) {
      TASK t = TASK(i) = i;
      TAGGED elt, tmp1;
      long l, u;
    
      DerefCar(elt,list);
      DerefCdr(list,list);
      pdata->initial_size[t] = CLPFD_MAXINT;
      DerefArg(tmp1,elt,1);	/* origin */
      TRefOrgVar(t) = tmp1;
      TRefOrgAttr(t) = tmp1 = check_argument(w,tmp1,Inf,Sup,Sup);

      DerefAttribute(tmp1,tmp1); /* dom/4 term */
      pdata->initial_est[t] = DomainMin(tmp1);
      DerefArg(tmp1,elt,3);	/* end */
      TRefEndVar(t) = tmp1;
      TRefEndAttr(t) = tmp1 = check_argument(w,tmp1,Inf,Sup,Sup);
      DerefAttribute(tmp1,tmp1); /* dom/4 term */
      pdata->initial_lct[t] = DomainMax(tmp1);
      DerefArg(tmp1,elt,4);	/* height */
      TRefHeightVar(t) = tmp1;
      TRefHeightAttr(t) = tmp1 = check_argument(w,tmp1,Inf,Sup,Sup);
      DerefAttribute(tmp1,tmp1); /* dom/4 term */
      l = GetSmall(DomainMin(tmp1)); /* min. height */
      if (minmin>l) minmin = l;
      if (maxmin<l) maxmin = l;
      u = GetSmall(DomainMax(tmp1)); /* max. height */
      if (minmax>u) minmax = u;
      if (maxmax<u) maxmax = u;
      DerefArg(tmp1,elt,2);	/* duration */
      TRefDurVar(t) = tmp1;
      TRefDurAttr(t) = tmp1 = check_argument(w,tmp1,Inf,Sup,Sup);
      DerefAttribute(tmp1,tmp1); /* dom/4 term */
      if ((pdata->options&OPT_ATMOST)
	  ? (l>0 || l>pdata->capacity_min)
	  : (u<0 || u<pdata->capacity_max)) {
	l = GetSmall(DomainMax(tmp1)); /* max. dur */
	if (maxdur<l) maxdur = l;
      }
      DerefArg(tmp1,elt,5);	/* machine */
      TRefMachVar(t) = tmp1;
      TRefMachAttr(t) = check_argument(w,tmp1,Inf,Sup,Sup);
    }
    pdata->min_min_height = minmin;
    pdata->min_max_height = maxmin;
    pdata->max_min_height = minmax;
    pdata->max_max_height = maxmax;
    pdata->max_duration = maxdur;
  }

  /* RESUME HERE */
  /* Get the correct table according to whether we backtrack or not */

  if (state_stamp != pdata->stamp) {
    pdata->table_size = pdata->initial_size;
    pdata->table_est = pdata->initial_est;
    pdata->table_lct = pdata->initial_lct;
  } else {
    pdata->table_size = pdata->previous_size;
    pdata->table_est = pdata->previous_est;
    pdata->table_lct = pdata->previous_lct;
  }
#if CSTATE_ONLY
  if (state_stamp != pdata->stamp) {
    pdata->ntargets = pdata->ntargets_back;
    pdata->nsources = 0;
  }
#else
  DerefArg(tmp,X(0),6);
  pdata->ntargets = GetSmall(tmp);
  DerefArg(tmp,X(0),7);
  pdata->nsources = GetSmall(tmp);
#endif
  ntargets = pdata->ntargets;
  nactive = ntargets + pdata->nsources;
  
  /* refresh target tasks */
  
  pdata->last_to_prune = ntasks;
  for (i=0; i<ntargets; i++) {
    TASK t = TASK(i);      
    TAGGED tmp1, *arg;

    FLAGS(t) = STATUS_TARGET+STATUS_SOURCE;
    if ((pdata->options&OPT_PRUNE_NEXT) && /* prune(next) selected */
	state_stamp != pdata->stamp && /* not fwd execution */
	pdata->last_to_prune>t && /* found an earlier task */
	(LST_T(t)!=EST_T(t) || /* not all attributes ground */
	 LCT_T(t)!=ECT_T(t) ||
	 MAXDUR_T(t)!=MINDUR_T(t) ||
	 MAXHEIGHT_T(t)!=MINHEIGHT_T(t) ||
	 SIZEMACH(t)!=1))
      pdata->last_to_prune = t;
    /* refresh origin */
    tmp1 = TRefOrgAttr(t);
    DerefAttribute(tmp1,tmp1); /* dom/4 term */
    arg = TagToArg(tmp1,0);
    SETORG(t) = arg[1];
    EST_T(t) = arg[2];
    LST_T(t) = arg[3];
    /* refresh duration */
    tmp1 = TRefDurAttr(t);
    DerefAttribute(tmp1,tmp1); /* dom/4 term */
    arg = TagToArg(tmp1,0);
    SETDUR(t) = arg[1];
    MINDUR_T(t) = arg[2];
    MAXDUR_T(t) = arg[3];
    /* refresh end */
    tmp1 = TRefEndAttr(t);
    DerefAttribute(tmp1,tmp1); /* dom/4 term */
    arg = TagToArg(tmp1,0);
    SETEND(t) = arg[1];
    ECT_T(t) = arg[2];
    LCT_T(t) = arg[3];
    /* refresh height */
    tmp1 = TRefHeightAttr(t);
    DerefAttribute(tmp1,tmp1); /* dom/4 term */
    arg = TagToArg(tmp1,0);
    SETHEIGHT(t) = arg[1];
    MINHEIGHT_T(t) = arg[2];
    MAXHEIGHT_T(t) = arg[3];
    /* refresh machine */
    tmp1 = TRefMachAttr(t);
    DerefAttribute(tmp1,tmp1); /* dom/4 term */
    arg = TagToArg(tmp1,0);
    SETMACH(t) = arg[1];
    SIZEMACH(t) = GetSmall(arg[4]);
  }
  if (state_stamp != pdata->stamp) {
    for (i=0; i<ntargets; i++) {
      TASK t = TASK(i);
    
      LCTKEY(t) = LCT(t);
    }
    qsort_asc_lctkey(pdata->target, ntargets);
    build_minest(0, ntargets);
    build_minest(ntargets, nactive); /* 3.9 */
  }

  /* refresh source tasks */
  /* assumption: SETDUR(t), SETEND(t), SETHEIGHT(t) will not be accessed
     for source tasks */
  
  for (i=ntargets; i<nactive; i++) {
    TASK t = TASK(i);
    TAGGED tmp1, *arg;

    FLAGS(t) = STATUS_SOURCE;	/* 3.9 */
    /* refresh origin */
    tmp1 = TRefOrgAttr(t);
    DerefAttribute(tmp1,tmp1); /* dom/4 term */
    arg = TagToArg(tmp1,0);
    SETORG(t) = arg[1];
    /* refresh machine */
    tmp1 = TRefMachAttr(t);
    DerefAttribute(tmp1,tmp1); /* dom/4 term */
    arg = TagToArg(tmp1,0);
    SETMACH(t) = arg[1];
  }

  /* END OF RESUMPTION */
  
  pdata->stamp = state_stamp+1;
  pdata->queue = EmptySet;

  /* Generate intervals where to check */
  
  for (i=0; i<ntargets; i++) {
    TASK t = TASK(i);
    long state = task_state(t);

    if (pdata->table_size[t]!=state) {
      if (!task_renormalize(t))
	goto ret;
      pdata->queue =
	fd_or_interval(pdata->queue,TABLE_EST_T(t),TABLE_LCT_T(t));
    }
  }

  /* Check the merged intervals and saturate if necessary */

 fixpoint:
  while (pdata->queue!=EmptySet) {
    TAGGED r = CTagToCar(pdata->queue);
    pdata->queue = CTagToCdr(pdata->queue);

    if (!check_interval(GetSmall(RangeMin(r)),GetSmall(RangeMax(r))))
      goto ret;
  }
  
  if (pdata->options&OPT_TASK_INTERVALS) {
    for (i=0; i<ntargets; i++) {
      TASK t = TASK(i);
      long minh = MINHEIGHT(t);
      long maxh = MAXHEIGHT(t);

      /* Check task interval(s) of the modified tasks */
      /* Consider interval of placement of the task
	 (filter out tasks with compulsory part) */

      if ((pdata->options&OPT_ATMOST)
	  ? (minh>0 || minh>pdata->capacity_min)
	  : (maxh<0 || maxh<pdata->capacity_max)) {
	if ((SIZEMACH(t)>1 || !HAS_COMPULSORY_PART(t)) &&
	    !ti_task_interval(EST(t),LCT(t),SETMACH(t)))
	  goto ret;
      } else {
	if (!ti_task_interval(TABLE_EST(t),EST(t),SETMACH(t)) ||
	    !ti_task_interval(LCT(t),TABLE_LCT(t),SETMACH(t)))
	  goto ret;
      }
    }
    if (pdata->queue!=EmptySet)
      goto fixpoint;
  }
  
  /* Prepare for pruning - protect from GC */
  
  for (i=0; i<nactive; i++) {
    TASK t = TASK(i);

    /* NOTE: SETMACH(t) is used by update_task_states() too */
    SETMACH(t) = fd_localize(w,SETMACH(t));
  }
  for (i=0; i<ntargets; i++) {
    TASK t = TASK(i);
    unsigned long flags = FLAGS(t);

    if ((flags & ~(STATUS_TARGET+STATUS_SOURCE)) > 0) {
      if (flags & PRUNED_ORIGIN) {
	TAGGED dom = SETORG(t);
      
	if (!OnHeap(TagToLST(dom)))
	  SETORG(t) = fd_localize(w,fd_and_interval(dom,EST_T(t),LST_T(t)));
	else
	  SETORG(t) = ERRORTAG;
      }
      if (flags & PRUNED_END) {
	TAGGED dom = SETEND(t);
      
	if (!OnHeap(TagToLST(dom)))
	  SETEND(t) = fd_localize(w,fd_and_interval(dom,ECT_T(t),LCT_T(t)));
	else
	  SETEND(t) = ERRORTAG;
      }
      if (flags & PRUNED_DURATION) {
	TAGGED dom = SETDUR(t);
      
	if (!OnHeap(TagToLST(dom)))
	  SETDUR(t) = fd_localize(w,fd_and_interval(dom,MINDUR_T(t),MAXDUR_T(t)));
	else
	  SETDUR(t) = ERRORTAG;
      }
      if (flags & PRUNED_HEIGHT) {
	TAGGED dom = SETHEIGHT(t);
      
	if (!OnHeap(TagToLST(dom)))
	  SETHEIGHT(t) = fd_localize(w,fd_and_interval(dom,MINHEIGHT_T(t),MAXHEIGHT_T(t)));
	else
	  SETHEIGHT(t) = ERRORTAG;
      }
    }
  }

  /* Record the state of the variables after saturation, and prune */
  
  for (i=0; i<ntargets; i++) {
    TASK t = TASK(i);
    unsigned long flags = FLAGS(t);

    long state = task_state(t);
    long est = EST(t);
    long lct = LCT(t);

    pdata->previous_size[t] = state;
    pdata->previous_est[t] = MakeSmall(est);
    pdata->previous_lct[t] = MakeSmall(lct);
    if (committed) {
      pdata->initial_size[t] = state;
      pdata->initial_est[t] = MakeSmall(est);
      pdata->initial_lct[t] = MakeSmall(lct);
    }

    if ((flags & ~(STATUS_TARGET+STATUS_SOURCE)) > 0) {
      if (flags & PRUNED_ORIGIN) {
	TAGGED dom = SETORG(t);
      
	if (dom!=ERRORTAG)
	  request_tell(w, TRefOrgAttr(t), TRefOrgVar(t), dom, 2, 3);
	else
	  request_tell_interval(w, TRefOrgAttr(t), TRefOrgVar(t), EST_T(t), LST_T(t), 2, 3);
      }
      if (flags & PRUNED_END) {
	TAGGED dom = SETEND(t);
      
	if (dom!=ERRORTAG)
	  request_tell(w, TRefEndAttr(t), TRefEndVar(t), dom, 2, 3);
	else
	  request_tell_interval(w, TRefEndAttr(t), TRefEndVar(t), ECT_T(t), LCT_T(t), 2, 3);
      }
      if (flags & PRUNED_DURATION) {
	request_tell_interval(w, TRefDurAttr(t), TRefDurVar(t), MINDUR_T(t), MAXDUR_T(t), 2, 3);
      }
      if (flags & PRUNED_HEIGHT) {
	request_tell_interval(w, TRefHeightAttr(t), TRefHeightVar(t), MINHEIGHT_T(t), MAXHEIGHT_T(t), 2, 3);
      }
      if (flags & PRUNED_MACHINE) {
	request_tell(w, TRefMachAttr(t), TRefMachVar(t), SETMACH(t), 2, 3);
      }
    }
  }

  /* Update TARGET/SOURCE properties and repartition tasks */

  update_task_states(pdata);
  ent = (pdata->ntargets==0);
  
#if CSTATE_ONLY
  if (committed)
    pdata->ntargets_back = pdata->ntargets + pdata->nsources;
#else
  CTagToArg(X(0),6) = MakeSmall(pdata->ntargets);
  CTagToArg(X(0),7) = MakeSmall(pdata->nsources);
#endif

 ret:
  Pfree(ent==1,handle,pdata);
  RefTerm(Actions) = request_done(w, ent, 2, 3);
}
