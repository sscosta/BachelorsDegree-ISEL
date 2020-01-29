/* Copyright(C) 1999, Swedish Institute of Computer Science */

#include "fd.h"

#if MULTI_SP_AWARE
#if 0                           /* not redefined. It uses FD_SETUP_SPENV to access the SPEnv */
#define alldiff_destructor(A1) alldiff_destructor(HIDDEN_ARG, A1)
#endif
#define qsort_asc_valswap(A1,A2,A3,A4) qsort_asc_valswap(HIDDEN_ARG, A1,A2,A3,A4)
#define qsort_asc_valmed3(A1,A2,A3) qsort_asc_valmed3(HIDDEN_ARG, A1,A2,A3)
#define qsort_asc_val(A1,A2) qsort_asc_val(HIDDEN_ARG, A1,A2)
#endif /* !MULTI_SP_AWARE */

struct alldiff_data {
  void (SPCDECL *destructor)(void *);
#if MULTI_SP_AWARE
  SPEnv *spenv;
#endif /* MULTI_SP_AWARE */
  SP_term_ref refbase;
  long stamp;			/* increases up to backtracking */
  int nvars;			/* _original_ #terms */
  long *target;
  TAGGED *val;
};

#define TRefAttr(T) RefTerm(pdata->refbase + 2*(T))
#define TRefVar(T) RefTerm(pdata->refbase + 2*(T) + 1)
#define SV(T) (pdata->target[T])
#define VAL(T) (pdata->val[T])

static void SPCDECL alldiff_destructor(void *pdata_v)
{
  struct alldiff_data *pdata = (struct alldiff_data*)pdata_v;
  FD_SETUP_SPENV(pdata->spenv)

  SP_free_term_refs(pdata->refbase,pdata->nvars<<1);
  SP_free(pdata);
}

static int cmp_asc_val(TAGGED *t1, TAGGED *t2)
{
  return Tdifference(*t1,*t2);
}

#define QType TAGGED
#define QCmp  cmp_asc_val
#define QSort qsort_asc_val
#include "qsort.ic"



/*
  '$fd_all_different'(+State0, +State, -Actions).
  State = state(Vec,NDone,Handle,Stamp)
*/

/* ground instances now run in linear time */

void SPCDECL
prolog_fd_all_different MAGIC (HIDDEN_PROTO
			       SP_term_ref State0,
			       SP_term_ref State,
			       SP_term_ref Actions)
{
  int ent = -1;			/* initially disentailed */
  TAGGED handle, t1, telt, tvec;
  TAGGED feasible=0;
  long state_stamp;
  int i, ntargets, nint, nvars=0;
  struct alldiff_data *pdata;
  BOOL committed;
  int elt;
  WAMENV;

  (void)State0;                 /* [PM] 3.9b5 avoid -Wunused */


  w->numstack_end = NULL;
/*    X(0) = RefTerm(State0); */
  X(2) = atom_nil;		/* actions list */
  RefTerm(State) = unify_output_state(w,&handle,&state_stamp,&committed);
  if (!IsVar(handle)) {		/* got [Flag | '$free'(Ptr)] */
    pdata = Pdata(struct alldiff_data,handle);
    nvars = pdata->nvars;
  } else {			/* build persistent state */
    DerefArg(tvec,X(0),1);	/* get Vec */
    while (TagIsLST(tvec)) {	/* count terms, moving ground terms to RHS */
      DerefCdr(tvec,tvec);
      nvars++;
    }
    pdata = Palloc(struct alldiff_data,
		   2*nvars*sizeof(long),
		   handle);
    pdata->destructor = (alldiff_destructor);
    FD_STORE_SPENV(pdata->spenv);
    pdata->refbase = SP_alloc_term_refs(nvars<<1);
    pdata->stamp = state_stamp;	/* trust initially */
    pdata->nvars = nvars;
    pdata->target = (long *)(pdata+1);
    pdata->val = (TAGGED *)(pdata->target+nvars);

    DerefArg(tvec,X(0),1);		/* get Vec */
    for (elt=0; elt<nvars; elt++) {
      DerefCar(telt,tvec);
      DerefCdr(tvec,tvec);
      DerefArg(t1,telt,1);	/* get domain var */
      TRefVar(elt) = t1;
      DerefArg(t1,telt,2);	/* get attribute */
      TRefAttr(elt) = t1;
      SV(elt) = elt;
    }
  }

  /* RESUME HERE */
  DerefArg(telt,X(0),2);
  ntargets = nvars-GetSmall(telt);
  pdata->stamp = state_stamp+1;
  {
    int inf = 0;
    int sup = ntargets-1;
    int held = SV(sup);		/* sup is the hole */
    TAGGED min;
    
    elt = SV(inf);
    while (inf<=sup) {
      min = TRefVar(elt);
      DerefSwitch(min,telt,goto nonground;);
      VAL(sup) = min;
      SV(sup) = elt;
      sup--;
      elt = (inf>=sup ? held : SV(sup));
      continue;
    nonground:
      SV(inf) = elt;
      inf++;
      elt = (inf>=sup ? held : SV(inf));
    }
    nint = ntargets-inf;
    if (nint==1) {
      min = VAL(inf);
      feasible = fd_compl_interval(min,min);
    } else if (nint>1) {
      FDCONS cons;
      
      qsort_asc_val(&VAL(inf),nint);
      fdcons_init(&cons);
      min = VAL(inf);
      fdcons_add(&cons,min);
      for (i=inf+1; i<ntargets; i++) {
	if (min==VAL(i))
	  goto ret;
	min = VAL(i);
	fdcons_add(&cons,min);
      }
      feasible = fd_complement(fdcons_set(&cons));
    }
    ent = !inf;
    if (nint>0) {
      CTagToArg(X(0),2) = MakeSmall(nvars-inf);
      for (i=0; i<inf; i++) {
	elt = SV(i);

	request_tell(w, TRefAttr(elt), TRefVar(elt), feasible, 2, 3);
      }
    }
  }
ret:
  Pfree(ent==1,handle,pdata);
  RefTerm(Actions) = request_done(w, ent, 2, 3);
}

