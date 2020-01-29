/* Copyright(C) 1999, Swedish Institute of Computer Science */

#include "fd.h"

#define CSTATE_ONLY 0

#if MULTI_SP_AWARE
#define ord_list_to_set(A1,A2) ord_list_to_set(HIDDEN_ARG, A1,A2)
#define element_feasible(A1,A2,A3,A4,A5) element_feasible(HIDDEN_ARG, A1,A2,A3,A4,A5)
#define qsort_taggedswap(A1,A2,A3,A4) qsort_taggedswap(HIDDEN_ARG, A1,A2,A3,A4)
#define qsort_taggedmed3(A1,A2,A3) qsort_taggedmed3(HIDDEN_ARG, A1,A2,A3)
#define qsort_tagged(A1,A2) qsort_tagged(HIDDEN_ARG, A1,A2)
#if 0                           /* not redefined. It uses FD_SETUP_SPENV to access the SPEnv */
#define element_destructor(A1) element_destructor(HIDDEN_ARG, A1)
#endif
#endif /* MULTI_SP_AWARE */

static TAGGED ord_list_to_set MAGIC (HIDDEN_PROTO
				     TAGGED *vec,
				     int k)
{
  int i=0;
  TAGGED set=EmptySet;
  TAGGED b, e;
  TAGGED *tail = &set;

  while (i<k) {
    e = b = vec[i++];
    while (i<k && (e==vec[i] || e+IStep(1)==vec[i]))
      e = vec[i++];
    *tail = fd_interval(b,e);
    tail = TagToLST(*tail)+1;
  }
  return set;  
}


static int cmp_tagged(TAGGED *x, TAGGED *y)
{
  long d = Tdifference(*x,*y);

  return (d<0 ? -1 : d>0 ? 1 : 0);
}


#define QType TAGGED
#define QCmp  cmp_tagged
#define QSort qsort_tagged
#include "qsort.ic"



/*
   '$fd_relation'(+State0, -State, -Actions) :-
   State0 is f(N,X-XMut,Pairs,Y-YMut) where
   Pairs is a list of N possible X-Y values,
   State  similarly,
   Actions is a list of prunings etc.

   Max. heap need, except Actions:
   5     for State
   2N    for new Pairs
   ====
   2N+5 total
*/
void SPCDECL
prolog_fd_relation MAGIC (HIDDEN_PROTO
			  SP_term_ref State0,
			  SP_term_ref State,
			  SP_term_ref Actions)
{
  WAMENV;
  int k, n, ent = -1;		/* disentailed */
  int xcond, ycond;
  TAGGED pair, pairs;		/* Pair, Pairs */
  TAGGED xpair;			/* X-XMut */
  TAGGED xdom;			/* XDom */
  TAGGED *xvals;		/* filtered X values */
  TAGGED ypair;			/* Y-YMut */
  TAGGED ydom;			/* YDom */
  TAGGED *yvals;		/* filtered Y values */
  TAGGED *shared, *newtail, outarg, oldtail, newdomx, newdomy, *h, x, y, t1;
  BOOL yorder = TRUE;
  SP_term_ref refs = SP_new_term_refs(4);
  
  w->numstack_end = NULL;
/*    X(0) = RefTerm(State0); */
  (void)State0;
  X(2) = atom_nil;		/* actions list */
  DerefNonvar(X(0));
  DerefArg(t1,X(0),1);  n = GetSmall(t1);

  RequireHeap(2*n+5, 3);

  DerefArg(xpair,X(0),2)	/* get X-XMut */
  DerefArg(pairs,X(0),3)	/* get Pairs */
  DerefArg(ypair,X(0),4);	/* get Y-YMut */
  DerefArg(t1,xpair,1);		/* get and protect X */
  RefTerm(refs+1) = t1;
  DerefArg(t1,xpair,2);		/* get and protect XMut */
  RefTerm(refs  ) = t1;
  DerefAttribute(xdom,t1);	/* get dom/4 */
  xdom = DomainSet(xdom);	/* get FD set */
  DerefArg(t1,ypair,1);		/* get and protect Y */
  RefTerm(refs+3) = t1;
  DerefArg(t1,ypair,2);		/* get and protect YMut */
  RefTerm(refs+2) = t1;
  DerefAttribute(ydom,t1);	/* get dom/4 */
  ydom = DomainSet(ydom);	/* get FD set */

  xvals = Calloc(n, TAGGED);	/* get space for x values */
  yvals = Calloc(n, TAGGED);	/* get space for y values */
  k = 0;			/* k filtered pairs */
  shared = newtail = &outarg;
  oldtail = pairs;
  h = w->global_top;
  while (TagIsLST(pairs)) {
    DerefCar(pair,pairs);
    DerefCdr(pairs,pairs);
    DerefArg(x,pair,1);
    DerefArg(y,pair,2);
    if (fd_member(x,xdom) && fd_member(y,ydom)) { /* O(N^2) cost here! */
      xvals[k] = x;
      yvals[k] = y;
      if (k>0 && Tgt(yvals[k-1],y))
	yorder = FALSE;
      k++;
      *newtail = MakeList(h);
      *h = pair;
      newtail = h+1;
      h += 2;
    } else {
      oldtail = pairs;
      shared = newtail;
      w->global_top = h;
    }
  }
  if (k==0)
    goto ret;
  *shared = oldtail;
  h = w->global_top;
  X(0) = MakeStructure(h);
  *h++ = TagToHeadfunctor(X(0));
  *h++ = MakeSmall(k);
  *h++ = xpair;
  *h++ = outarg;
  *h++ = ypair;
  w->global_top = h;
  RefTerm(State) = X(0);
  
  newdomx = ord_list_to_set(xvals,k);
  if (!yorder)
    qsort_tagged(yvals, k);
  newdomy = ord_list_to_set(yvals,k);
  xcond = fd_compare(xdom,newdomx);
  ycond = fd_compare(ydom,newdomy);
  if (xcond != FDI_SUBSET && xcond != FDI_EQUAL) {
    request_tell(w, RefTerm(refs), RefTerm(refs+1), newdomx, 2, 3); /* may GC */
  }
  if (ycond != FDI_SUBSET && ycond != FDI_EQUAL) {
    request_tell(w, RefTerm(refs+2), RefTerm(refs+3), newdomy, 2, 3); /* may GC */
  }
  ent = 0;
  if (xvals[0]==xvals[k-1] || yvals[0]==yvals[k-1])
    ent = 1;
ret:
  SP_reset_term_refs(refs);
  RefTerm(Actions) = request_done(w, ent, 2, 3);
}



struct element_data {
  void (SPCDECL *destructor)(void *);
#if MULTI_SP_AWARE
  SPEnv *spenv;
#endif /* MULTI_SP_AWARE */

  long stamp;
  SP_term_ref refbase;		/* first 4 for XMut, X, YMut, Y */
  int nrefs;			/* including first 4 */
  int nelts;
  int first_feas;
  int first_infeas;
#if CSTATE_ONLY
  int first_feas_save;
  int first_infeas_save;
#endif
  struct {
    TAGGED *xmin, *xmax;
    TAGGED *ymin, *ymax;
    SP_term_ref *mutref;
  } elt;
};

#define XMIN(E) (pdata->elt.xmin[E])
#define XMAX(E) (pdata->elt.xmax[E])
#define YMIN(E) (pdata->elt.ymin[E])
#define YMAX(E) (pdata->elt.ymax[E])
#define MUTREF(E) (pdata->elt.mutref[E])


static void SPCDECL element_destructor(void *pdata_v)
{
  struct element_data *pdata = (struct element_data *)pdata_v;
  FD_SETUP_SPENV(pdata->spenv)

  SP_free_term_refs(pdata->refbase,pdata->nrefs);
  SP_free(pdata);
}


static BOOL element_feasible MAGIC (HIDDEN_PROTO
				    struct worker *w,
				    struct element_data *pdata,
				    int edata,
				    TAGGED xset,TAGGED yset)
{
  BOOL ycond;

  if (XMIN(edata)==XMAX(edata)) {
    ycond = fd_member(XMIN(edata),xset);
  } else {
    ycond = (fd_compare_interval(xset,XMIN(edata),XMAX(edata))!=FDI_DISJOINT);
  }
  if (ycond) {
    if (MUTREF(edata)) {		/* refresh */
      TAGGED edom;

      DerefAttribute(edom,RefTerm(MUTREF(edata)));
      YMIN(edata) = DomainMin(edom);
      YMAX(edata) = DomainMax(edom);
    }
    if (YMIN(edata)==YMAX(edata)) {
      ycond &= fd_member(YMIN(edata),yset);
    } else {
      ycond &= (fd_compare_interval(yset,YMIN(edata),YMAX(edata))!=FDI_DISJOINT);
    }
  }
  return ycond;
}




/* '$fd_element'(+State0, -State, -Actions) :-
   State0 is f(X-XMut,[E1,E2,...],Y-YMut,LeftInfeas,RightInfeas,Handle,Stamp) where
   State  is State0 with updated Stamp,
   Actions is a list of prunings etc.
*/
void SPCDECL
prolog_fd_element MAGIC (HIDDEN_PROTO
			 SP_term_ref State0,
			 SP_term_ref State,
			 SP_term_ref Actions)
{
  WAMENV;
  int hit, miss, ent = -1; /* disentailed */
  int nelts=0, nvars=0, edata=0;
  long state_stamp;
  BOOL committed;
  TAGGED elt, elts;		/* Elt, Elts */
  TAGGED xelt;			/* X-XMut */
  TAGGED xset, xmin, xmax;	/* XSet */
  TAGGED yelt;			/* Y-YMut */
  TAGGED yset;			/* YSet */
  TAGGED handle;		/* perm data handle */
  TAGGED emin=0, emax=0, edom, ymin, ymax;
  TAGGED t1, ti, elt0;
  SP_term_ref eref, refbase;
  struct element_data *pdata;
  int i, j, k;
  char *ptr;
  
  w->numstack_end = NULL;

/*    X(0) = RefTerm(State0); */
  (void)State0;
  X(2) = atom_nil;		/* actions list */
  RefTerm(State) = unify_output_state(w,&handle,&state_stamp,&committed);

  if (!IsVar(handle)) {		/* got [Flag | '$free'(Ptr)] */
    pdata = Pdata(struct element_data,handle);
    refbase = pdata->refbase;
    nelts = pdata->nelts;
    /* fetch X and Y data */
    xset = RefTerm(refbase);
    DerefAttribute(xset,xset);	/* get dom/4 */
    xmin = DomainMin(xset);	/* get min */
    xmax = DomainMax(xset);	/* get max */
    xset = DomainSet(xset);	/* get FD set */
    yset = RefTerm(refbase+2);
    DerefAttribute(yset,yset);	/* get dom/4 */
    ymin = DomainMin(yset);
    ymax = DomainMax(yset);
    yset = DomainSet(yset);	/* get FD set */
  } else {
    DerefArg(elts,X(0),2);	/* get Elts */
    elt0 = 0;
    while (TagIsLST(elts)) {
      DerefCar(elt,elts);
      DerefCdr(elts,elts);
      if (elt!=elt0) {
	nelts++;
	if (IsVar(elt)) nvars++;
	elt0 = elt;
      }
    }
    pdata = Palloc(struct element_data,
		   nelts*5*sizeof(TAGGED),
		   handle);
    /* store header data */
    pdata->destructor = element_destructor;
    FD_STORE_SPENV(pdata->spenv);
    pdata->nrefs = 2*nvars + 4;
    pdata->refbase = refbase = SP_alloc_term_refs(2*nvars + 4);
    pdata->stamp = state_stamp-1; /* DON'T trust initially */
    pdata->nelts = nelts;
#if CSTATE_ONLY
    pdata->first_feas = pdata->first_feas_save = 0;
    pdata->first_infeas = pdata->first_infeas_save = nelts;
#endif
    ptr = (char *)(pdata+1);
    pdata->elt.xmin = (TAGGED *)ptr; ptr += nelts*sizeof(TAGGED);
    pdata->elt.xmax = (TAGGED *)ptr; ptr += nelts*sizeof(TAGGED);
    pdata->elt.ymin = (TAGGED *)ptr; ptr += nelts*sizeof(TAGGED);
    pdata->elt.ymax = (TAGGED *)ptr; ptr += nelts*sizeof(TAGGED);
    pdata->elt.mutref = (SP_term_ref *)ptr; ptr += nelts*sizeof(SP_term_ref);
    /* decode and store X and Y data */
    DerefArg(xelt,X(0),1)	/* get X-XMut */
    DerefArg(yelt,X(0),3);	/* get Y-YMut */
    DerefArg(t1,xelt,1);	/* get and protect X */
    RefTerm(refbase+1) = t1;
    DerefArg(t1,xelt,2);	/* get and protect XMut */
    RefTerm(refbase  ) = t1;
    DerefAttribute(xset,t1);	/* get dom/4 */
    xmin = DomainMin(xset);	/* get FD set */
    xmax = DomainMax(xset);	/* get FD set */
    xset = DomainSet(xset);	/* get FD set */
    DerefArg(t1,yelt,1);	/* get and protect Y */
    RefTerm(refbase+3) = t1;
    DerefArg(t1,yelt,2);	/* get and protect YMut */
    RefTerm(refbase+2) = t1;
    DerefAttribute(yset,t1);	/* get dom/4 */
    ymin = DomainMin(yset);
    ymax = DomainMax(yset);
    yset = DomainSet(yset);	/* get FD set */
    /* decode and store Elts data */
    DerefArg(elts,X(0),2);	/* get Elts */
    eref = refbase+4;
    elt0 = 0;
    ti = TaggedZero;
    while (TagIsLST(elts)) {
      ti += IStep(1);
      DerefCar(elt,elts);
      DerefCdr(elts,elts);
      if (elt!=elt0) {
	elt0 = elt;
	XMIN(edata) = ti;
	XMAX(edata) = ti;
	if (IsVar(elt)) {
	  RefTerm(eref+1) = elt; /* protect elt var */
	  t1 = check_argument(w,elt,Inf,Sup,Sup);
	  RefTerm(eref) = t1;	/* protect elt attribute */
	  DerefAttribute(edom,t1);
	  MUTREF(edata) = eref;
	  eref += 2;
	} else {
	  YMIN(edata) = elt;
	  YMAX(edata) = elt;
	  MUTREF(edata) = 0;
	}
	edata++;
      } else {
	XMAX(edata-1) = ti;
      }
    }
  }
#if CSTATE_ONLY
  if (state_stamp != pdata->stamp) { /* trust nothing */
    pdata->first_feas = pdata->first_feas_save;
    pdata->first_infeas = pdata->first_infeas_save;
  }
#else
  DerefArg(elt,X(0),4);
  pdata->first_feas = GetSmall(elt);
  DerefArg(elt,X(0),5);
  pdata->first_infeas = nelts - GetSmall(elt);
#endif
  pdata->stamp = state_stamp+1;
  
  /* dichotomic search for first item that is not before X */
  i = pdata->first_feas;
  k = pdata->first_infeas;
  while (i<k) {
    j = (i+k)>>1;
    if (Tgt(xmin,XMAX(j)))
      i = j+1;
    else
      k = j;
  }
  pdata->first_feas = i;

  /* scan the feasible items */
  hit = -1;
  miss = i-1;
  for (k=pdata->first_infeas; i<k; i++) {
    edata = i;

    if (Tlt(xmax,XMIN(edata)))
      break;
    if (element_feasible(w,pdata,edata,xset,yset)) {
      /*
      if (XMIN(edata) == xmin)
	xmin = XMAX(edata) + IStep(1);
      else
	xset = fd_subtract_interval(xset,XMIN(edata),XMAX(edata));
      */
      if (hit == -1) {		/* form union(Edom) */
	emin = YMIN(edata);
	emax = YMAX(edata);
      } else {			/* form interval hull */
	if (FDgt(emin,YMIN(edata)))
	  emin = YMIN(edata);
	if (FDlt(emax,YMAX(edata)))
	  emax = YMAX(edata);
      }
      hit = i;
    } else {
      if (hit == -1) {
	pdata->first_feas = i+1;
      } else if (miss<hit) {
	if (XMIN(miss+1) == xmin)
	  xmin = XMIN(edata);
	else
	  xset = fd_subtract_interval(xset,
				      XMIN(miss+1),
				      XMIN(edata)-IStep(1));
      }
      miss = i;
    }
  }
  if (hit == -1) {
    goto ret;
  } else if (miss<hit) {
    if (XMIN(miss+1) == xmin)
      xmin = XMAX(hit)+IStep(1);
    else
      xset = fd_subtract_interval(xset, XMIN(miss+1), XMAX(hit));
  }
  pdata->first_infeas = hit+1;
#if CSTATE_ONLY
  if (committed) {
    pdata->first_feas_save = pdata->first_feas;
    pdata->first_infeas_save = hit+1;
  }
#else
  CTagToArg(X(0),4) = MakeSmall(pdata->first_feas);
  CTagToArg(X(0),5) = MakeSmall(nelts - pdata->first_infeas);
#endif  
  if (FDlt(emin,ymin)) emin = ymin;
  if (FDgt(emax,ymax)) emax = ymax;
  k = fd_compare_interval(yset,emin,emax);
  if (Tle(xmin,xmax)) {
    xset = fd_and_interval(xset,xmin,xmax);
    if (xset!=EmptySet) {
      request_tell(w, RefTerm(refbase), RefTerm(refbase+1), fd_complement(xset), 2, 3); /* may GC */
    }
  }
  if (k != FDI_SUBSET && k != FDI_EQUAL) {
    request_tell_interval(w, RefTerm(refbase+2), RefTerm(refbase+3), emin, emax, 2, 3); /* may GC */
  }
  ent = (emin==emax && pdata->first_feas==hit);
  if (pdata->first_feas==hit) {	/* can prune E var too */
    eref = MUTREF(hit);
    if (eref) {
      DerefAttribute(edom,RefTerm(eref));
      switch (fd_compare_interval(DomainSet(edom),emin,emax)) {
      case FDI_SUBSET:
      case FDI_EQUAL:
	break;
      default:
	request_tell_interval(w, RefTerm(eref), RefTerm(eref+1), emin, emax, 2, 3); /* may GC */
      }
      if (!ent) {
	request_rewrite_eq(w, RefTerm(eref+1), RefTerm(refbase+3), 2, 3);
	ent = 1;
      }
    }
  }
ret:
  Pfree(ent==1,handle,pdata);
  RefTerm(Actions) = request_done(w, ent, 2, 3);
}


