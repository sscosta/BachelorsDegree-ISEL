/* Copyright(C) 2000, Swedish Institute of Computer Science */

#include "fd.h"

#if MULTI_SP_AWARE
#if 0                           /* not redefined. It uses FD_SETUP_SPENV to access the SPEnv */
#define case_destructor(A1) case_destructor(HIDDEN_ARG, A1)
#endif
#define eval_dag(A1,A2,A3) eval_dag(HIDDEN_ARG, A1,A2,A3)
#endif /* MULTI_SP_AWARE */


#define CASE_DOM 0
#define CASE_MINMAX 1
#define CASE_MIN 2
#define CASE_MAX 3
#define CASE_VAL 4
#define CASE_NONE 5

struct case_data {
  void (SPCDECL *destructor)(void*);
#if MULTI_SP_AWARE
  SPEnv *spenv;
#endif /* MULTI_SP_AWARE */

  SP_term_ref refbase;
  long stamp;			/* increases up to backtracking */
  int nvars;
  int nnodes;
  unsigned long false_date;
  unsigned long true_date;
  TAGGED relevant_vars;		/* volatile, numstack data */
  TAGGED *prune_set;		/* for DOM, volatile, GC-unsafe */
  TAGGED *prune_min;		/* for MINMAX|MIN|VAL, volatile, atomic */
  TAGGED *prune_max;		/* for MINMAX|MAX|VAL, volatile, atomic */
  long   *prune_mask;		/* any=1, min=2, max=4, at least two=8, volatile */
  unsigned long *control;
  struct {
    unsigned long *date;	/* TRUE(FALSE) if = true_date(false_date), undefined otherwise */
    long *var_index;
    /* TAGGED *varset;		// mutable, whose value is an fdset */
    long *child_index;
    long *child_end;
  } dag_node;
  struct {
    TAGGED *min, *max;
    long *node_index;
  } child;
};

#define DATE(N) (pdata->dag_node.date[N])
#define VAR(N) (pdata->dag_node.var_index[N])
#define VARSET(N) RefTerm(pdata->refbase + 2*nvars + (N))
                  /* (pdata->dag_node.varset[N]) */
#define CHILD(N) (pdata->dag_node.child_index[N])
#define CHILD_END(N) (pdata->dag_node.child_end[N])
#define CMIN(C) (pdata->child.min[C])
#define CMAX(C) (pdata->child.max[C])
#define NODE(C) (pdata->child.node_index[C])

static void SPCDECL case_destructor(void *pdata_v)
{
  struct case_data *pdata = (struct case_data *)pdata_v;
  FD_SETUP_SPENV(pdata->spenv)

  SP_free_term_refs(pdata->refbase,2*pdata->nvars+pdata->nnodes);
  SP_free(pdata);
}



static int MAGIC eval_dag MAGIC (HIDDEN_PROTO Argdecl,
				 int node,
				 struct case_data *pdata)
{
  if (DATE(node) == pdata->true_date)
    return TRUE;
  if (DATE(node) == pdata->false_date)
    return FALSE;
  
  {
    int state = FALSE;
    int virtual = VAR(node);
    TAGGED tvirtual = MakeSmall(virtual);
    int child_index = CHILD(node);
    int child_end = CHILD_END(node);
    int nvars = pdata->nvars;
    int inf, mid, sup;		/* for dichotomic search */
    int child;
    TAGGED dom, set, min, max;
    
    DerefAttribute(dom,RefTerm(pdata->refbase+(virtual<<1))); /* dom/4 term */
    set = DomainSet(dom);
    min = DomainMin(dom);
    max = DomainMax(dom);
    
    /* dichotomic search for first child */
    inf = child_index;
    sup = child_end;
    while (inf<sup) {
      mid = (inf+sup)>>1;
      if (FDgt(min,CMAX(mid))) /* current variable #> child */
	inf = mid+1;
      else
	sup = mid;
    }
    child = child_index = inf;
    
    /* scan the compatible children */
    while (child_index < child_end &&
	   !FDgt(CMIN(child),max) &&
	   (state==FALSE ||
	    fd_compare(VARSET(node),pdata->relevant_vars)!=FDI_DISJOINT)) {
      int lstate = TRUE;

      if (CMIN(child)==CMAX(child)) { /* singleton interval special case */
	if (fd_member(CMIN(child),set))
	  goto intersect;
	else
	  goto disjoint;
      }
      switch (fd_compare_interval(set,CMIN(child),CMAX(child))) {
      case FDI_DISJOINT:
      disjoint:
	lstate = FALSE;
	break;
      case FDI_SUBSET:
      case FDI_EQUAL:
	if (NODE(child) > -1) {
	  /* TODO: assert RefTerm(pdata->refbase+(virtual<<1)+1) in CMIN(child) .. CMAX(child) */
	  lstate = eval_dag(w,NODE(child),pdata);
	  /* TODO: retract RefTerm(pdata->refbase+(virtual<<1)+1) in CMIN(child) .. CMAX(child) */
	}
	if (lstate!=FALSE && fd_member(tvirtual,pdata->relevant_vars))
	  pdata->relevant_vars = fd_delete(pdata->relevant_vars,tvirtual);
	break;
      default:
      intersect:
	if (NODE(child) > -1) {
	  /* TODO: assert RefTerm(pdata->refbase+(virtual<<1)+1) in CMIN(child) .. CMAX(child) */
	  lstate = eval_dag(w,NODE(child),pdata);
	  /* TODO: retract RefTerm(pdata->refbase+(virtual<<1)+1) in CMIN(child) .. CMAX(child) */
	}
	if (lstate!=FALSE && fd_member(tvirtual,pdata->relevant_vars)) {
	  pdata->prune_mask[virtual] |= 0x1; /* support for some domain element - see CASE_VAL */
	  if (!FDgt(CMIN(child),min)) /* min >= CMIN(child) */
	    pdata->prune_mask[virtual] |= 0x2;	/* support for min element */
	  if (!FDgt(max,CMAX(child))) /* max <= CMAX(child) */
	    pdata->prune_mask[virtual] |= 0x4;	/* support for max element */
	  switch (pdata->control[virtual]) {
	  case CASE_DOM:
	    pdata->prune_set[virtual] = set =
	      fd_subtract_interval(pdata->prune_set[virtual],CMIN(child),CMAX(child));
	    if (set==EmptySet)
	      pdata->relevant_vars = fd_delete(pdata->relevant_vars,tvirtual);
	    break;
	  case CASE_MIN:
	    if (pdata->prune_mask[virtual] & 0x2)
	      pdata->relevant_vars = fd_delete(pdata->relevant_vars,tvirtual);
	    else {
	      if (FDgt(pdata->prune_min[virtual],CMIN(child)))
		pdata->prune_min[virtual] = CMIN(child);
	    }
	    break;
	  case CASE_MAX:
	    if (pdata->prune_mask[virtual] & 0x4)
	      pdata->relevant_vars = fd_delete(pdata->relevant_vars,tvirtual);
	    else {
	      if (FDlt(pdata->prune_max[virtual],CMAX(child)))
		pdata->prune_max[virtual] = CMAX(child);
	    }
	    break;
	  case CASE_MINMAX:
	    if ((pdata->prune_mask[virtual] & 0x6) == 0x6)
	      pdata->relevant_vars = fd_delete(pdata->relevant_vars,tvirtual);
	    else {
	      if (FDgt(pdata->prune_min[virtual],CMIN(child)))
		pdata->prune_min[virtual] = CMIN(child);
	      if (FDlt(pdata->prune_max[virtual],CMAX(child)))
		pdata->prune_max[virtual] = CMAX(child);
	    }
	    break;
	  case CASE_VAL:
	    if ((pdata->prune_mask[virtual] & 0x6) == 0x6)
	      pdata->relevant_vars = fd_delete(pdata->relevant_vars,tvirtual);
	    else {		/* detect when we have support for at least two values */
	      /* TODO: fd_compare case analysis */
	      TAGGED set1 = fd_and_interval(set,CMIN(child),CMAX(child));
	      TAGGED min = fd_min(set1);
	      TAGGED max = fd_max(set1);
	      
	      if (FDgt(pdata->prune_min[virtual],min))
		pdata->prune_min[virtual] = min;
	      if (FDlt(pdata->prune_max[virtual],max))
		pdata->prune_max[virtual] = max;
	      if (pdata->prune_min[virtual] != pdata->prune_max[virtual])
		pdata->relevant_vars = fd_delete(pdata->relevant_vars,tvirtual);
	    }
	    break;
	  }
	}
      }
      /* update state of current node */
      if (state==FALSE)
	state = lstate;
      child_index++;
      child++;
    }
    /* record state */
    DATE(node) = (state ? pdata->true_date : pdata->false_date);
    return state;
  }
}

/*
   '$fd_case'(+State0, -State, -Actions) :-
   State0 = State = state(f(NVars,NNodes,_NChildren,TVars,Dag,Prune),_Handle,Stamp)
 */
void SPCDECL
prolog_fd_case MAGIC (HIDDEN_PROTO
		      SP_term_ref State0,
		      SP_term_ref State,
		      SP_term_ref Actions)
{
  WAMENV;
  struct case_data *pdata;
  int i, nvars, nnodes, nchildren, nnonground, ent = -1;
  TAGGED item, tmp, state, handle;
  long state_stamp;
  BOOL committed;		/* not used */
  FDITER it;
  
  w->numstack_end = NULL;

/*    X(0) = RefTerm(State0); */
  (void)State0;
  X(2) = atom_nil;		/* actions list */
  RefTerm(State) = unify_output_state(w,&handle,&state_stamp,&committed);

  if (!IsVar(handle)) {		/* got [Flag | '$free'(Ptr)] */
    pdata = Pdata(struct case_data,handle);
    nvars = pdata->nvars;
    nnodes = pdata->nnodes;
    pdata->stamp = state_stamp-1; /* DON'T trust initially */
  } else {			/* build persistent state */
    char *ptr;
    int child;
    TAGGED tvars, dag, prune;
    
    DerefArg(state,X(0),1);
    DerefArg(tmp,state,1); 
    nvars = GetSmall(tmp);
    DerefArg(tmp,state,2); 
    nnodes = GetSmall(tmp);
    DerefArg(tmp,state,3); 
    nchildren = GetSmall(tmp);
    pdata = Palloc(struct case_data,
		   nvars*5*sizeof(long)+
		   nnodes*4*sizeof(long)+
		   nchildren*3*sizeof(long),
		   handle);
    pdata->destructor = case_destructor;
    FD_STORE_SPENV(pdata->spenv);
    pdata->refbase = SP_alloc_term_refs(2*nvars+nnodes);
    pdata->nvars = nvars;
    pdata->nnodes = nnodes;
    pdata->false_date = 0L;
    pdata->true_date = 1L;
    ptr = (char *)(pdata+1);
    pdata->prune_set = (TAGGED *)ptr; ptr += nvars*sizeof(TAGGED);
    pdata->prune_min = (TAGGED *)ptr; ptr += nvars*sizeof(TAGGED);
    pdata->prune_max = (TAGGED *)ptr; ptr += nvars*sizeof(TAGGED);
    pdata->prune_mask = (long *)ptr;  ptr += nvars*sizeof(long);
    pdata->control = (unsigned long *)ptr; ptr += nvars*sizeof(long);
    pdata->dag_node.date = (unsigned long *)ptr; ptr += nnodes*sizeof(long);
    pdata->dag_node.var_index = (long *)ptr; ptr += nnodes*sizeof(long);
    /* pdata->dag_node.varset = (TAGGED *)ptr; ptr += nnodes*sizeof(TAGGED); */
    pdata->dag_node.child_index = (long *)ptr; ptr += nnodes*sizeof(long);
    pdata->dag_node.child_end = (long *)ptr; ptr += nnodes*sizeof(long);
    pdata->child.min = (TAGGED *)ptr; ptr += nchildren*sizeof(long);
    pdata->child.max = (TAGGED *)ptr; ptr += nchildren*sizeof(long);
    pdata->child.node_index = (long *)ptr; ptr += nchildren*sizeof(long);
    
    DerefArg(tvars,state,4); 
    DerefArg(dag,state,5); 
    DerefArg(prune,state,6); 
    for (i=0; i < nvars; i++) {
      SP_term_ref ref = pdata->refbase+(i<<1);

      DerefCar(item,tvars);
      DerefCdr(tvars,tvars);
      DerefArg(tmp,item,1);	/* var's domain variable */
      RefTerm(ref+1) = tmp;
      DerefArg(tmp,item,2);	/* var's attribute */
      RefTerm(ref) = tmp;
    }
    /* build the DAG */
    child = 0;
    for (i=0; i < nnodes; i++) {
      int node = i;
      TAGGED children, dom, min, cmin, cmax;
      int ix;

      DerefCar(item,dag);
      DerefCdr(dag,dag);
      DerefArg(tmp,item,1);	/* var index */
      DATE(node) = -1L;
      VAR(node) = ix = GetSmall(tmp);
      DerefAttribute(dom,RefTerm(pdata->refbase+(ix<<1))); /* dom/4 term */
      min = DomainMin(dom);
      DerefArg(tmp,item,2);	/* var set */
      VARSET(node) = tmp;
      CHILD(node) = child;
      DerefArg(children,item,3);	/* children */
      while (TagIsLST(children)) {
	DerefCar(item,children);
	DerefCdr(children,children);
	DerefArg(tmp,item,2);
	DerefArg(item,item,1);
	DerefArg(cmin,item,1);
	DerefArg(cmax,item,2);
	CMIN(child) = cmin;
	CMAX(child) = cmax;
	NODE(child) = (TagIsSmall(tmp) ? GetSmall(tmp) : -1);
	child++;
      }
      CHILD_END(node) = child;
    }
    /* build control info */
    for (i=0; i<nvars; i++) {
      DerefCar(item,prune);
      DerefCdr(prune,prune);
      item = SetArity(item,1);
      if (item==fd.functor_dom)
	pdata->control[i] = CASE_DOM;
      else if (item==fd.functor_min)
	pdata->control[i] = CASE_MIN;
      else if (item==fd.functor_max)
	pdata->control[i] = CASE_MAX;
      else if (item==fd.functor_minmax)
	pdata->control[i] = CASE_MINMAX;
      else if (item==fd.functor_val)
	pdata->control[i] = CASE_VAL;
      else
	pdata->control[i] = CASE_NONE;
    }
  }
  
  if (state_stamp != pdata->stamp) { /* all nodes must be recomputed */
    pdata->true_date += 2;
    pdata->false_date += 2;
  } else {			/* false nodes remain false */
    pdata->true_date += 2;
  }
  pdata->stamp = state_stamp+1;

  /* init each time */
  pdata->relevant_vars = EmptySet;
  nnonground = 0;
  for (i=0; i < nvars; i++) {
    SP_term_ref ref = pdata->refbase+(i<<1);
    
    DerefAttribute(tmp,RefTerm(ref)); /* dom/4 term */
    if (pdata->control[i]==CASE_DOM)
      pdata->prune_set[i] = DomainSet(tmp); /* FD set */
    else {
      pdata->prune_min[i] = Sup; /* smallest so far */
      pdata->prune_max[i] = Inf; /* greatest so far */
    }
    pdata->prune_mask[i] = 0;
    if (pdata->control[i]!=CASE_NONE && DomainSize(tmp)!=TaggedOne)
      pdata->relevant_vars = fd_insert_into(MakeSmall(i),pdata->relevant_vars);
    if (DomainSize(tmp)!=TaggedOne)
      nnonground++;
  }
  /* evaluate the DAG */
  if (!eval_dag(w,0,pdata))
    goto ret;

  /* compute pruning acc. to generalization */

  fditer_init(&it, pdata->relevant_vars);
  while (!fditer_empty(&it)) {
    tmp = fditer_next(&it);
    i = GetSmall(tmp);
    switch (pdata->control[i]) {
    case CASE_DOM:
      pdata->prune_set[i] = fd_complement(pdata->prune_set[i]); /* implies localize! */
      break;
    case CASE_MIN:
      pdata->prune_set[i] = fd_interval(pdata->prune_min[i],Sup);
      break;
    case CASE_MAX:
      pdata->prune_set[i] = fd_interval(Inf,pdata->prune_max[i]);
      break;
    case CASE_MINMAX:
    case CASE_VAL:
      pdata->prune_set[i] = fd_interval(pdata->prune_min[i],pdata->prune_max[i]);
      break;
    }
  }

  /* do actual pruning */
  fditer_init(&it, pdata->relevant_vars);
  while (!fditer_empty(&it)) {
    TAGGED set, elt;
    SP_term_ref ref;
    
    tmp = fditer_next(&it);
    i = GetSmall(tmp);
    ref = pdata->refbase+(i<<1);
    set = pdata->prune_set[i];
    request_tell(w, RefTerm(ref), RefTerm(ref+1), set, 2, 3);
    DerefAttribute(tmp,RefTerm(ref)); /* dom/4 term */
    fd_and_min2(DomainSet(tmp),set,&elt);
    if (!elt)			/* the intersection is a singleton */
      nnonground--;
  }
  ent = !nnonground;
 ret:
  Pfree(ent==1,handle,pdata);
  RefTerm(Actions) = request_done(w, ent, 2, 3);
}


