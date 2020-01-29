/* Copyright(C) 1999, Swedish Institute of Computer Science */

#include "fd.h"

#if MULTI_SP_AWARE
#define tell_eq(A1,A2,A3,A4,A5,A6,A7,A8) tell_eq(HIDDEN_ARG, A1,A2,A3,A4,A5,A6,A7,A8)
#define negdom(A1) negdom(HIDDEN_ARG, A1)
#endif /* MULTI_SP_AWARE */

static int tell_eq MAGIC (HIDDEN_PROTO
			  Argdecl,
			  TAGGED xmut, TAGGED x, TAGGED xdom,
			  TAGGED ymut, TAGGED y, TAGGED ydom,
			  int intersecting)
{
  TAGGED inters;
  
  switch (intersecting) {
  case FDI_DISJOINT:
    return -1;
  case FDI_EQUAL:
    if (!IsVar(x))
      return 1;
    else {
      request_rewrite_eq(w,x,y,2,3);
      return 1;
    }
  case FDI_SUBSET:
    if (!IsVar(x)) {
      request_tell_value(w,ymut,y,x,2,3);
      return 1;
    } else {
      request_rewrite_eq(w,x,y,2,3);
      return 1;
    }
  case FDI_SUPERSET:
    if (!IsVar(y)) {
      request_tell_value(w,xmut,x,y,2,3);
      return 1;
    } else {
      request_rewrite_eq(w,x,y,2,3);
      return 1;
    }
  default:
    inters = fd_and(xdom,ydom);
    if (fd_singleton(inters)) {
      TAGGED min = fd_min(inters);
      if (IsVar(x))
	request_tell_value(w,xmut,x,min,2,3);
      if (IsVar(y))
	request_tell_value(w,ymut,y,min,2,3);
      return 1;
    } else {
      request_rewrite_eq(w,x,y,2,3);
      return 1;
    }
  }
}

/* '$fd_in_set_iff'(+State, -NewState, -Actions) :- X in_set Set iff B.
   State is f(X,XMut,Set,B,BMut),
   Actions is a list of prunings etc.
*/
void SPCDECL
prolog_fd_in_set_iff MAGIC (HIDDEN_PROTO
			    SP_term_ref State,
			    SP_term_ref NewState,
			    SP_term_ref Actions)
{
  WAMENV;
  int ent = 0;		/* neither entailed nor dis- */
  int intersecting;
  TAGGED x, xmut, set, b, bmut, xdom;
  
  w->numstack_end = NULL;

/*    X(0) = RefTerm(State); */
  RefTerm(NewState) = RefTerm(State);
  X(2) = atom_nil;		/* actions list */
  DerefNonvar(X(0));
  DerefArg(xmut,X(0),2);
  DerefArg(set,X(0),3);
  DerefArg(b,X(0),4);
  DerefAttribute(xdom,xmut);	/* get dom/4 */
  xdom = DomainSet(xdom);	/* get FD set */
  intersecting = fd_compare(xdom,set);
  if (IsVar(b)) {
    DerefArg(bmut,X(0),5);
    switch (intersecting) {
    case FDI_SUBSET:		/* [B=1,exit] */
    case FDI_EQUAL:		/* [B=1,exit] */
      ent = 1;
      request_tell_value(w,bmut,b,TaggedOne,2,3);
      break;
    case FDI_DISJOINT:		/* [B=0,exit] */
      ent = 1;
      request_tell_value(w,bmut,b,TaggedZero,2,3);
      break;
    }
  } else if (Teqz(b)) {
    DerefArg(x,X(0),1);
    if (IsVar(x)) {
				/* [X in_set \set,exit] */
      switch (intersecting) {
      case FDI_SUBSET:
      case FDI_EQUAL:
	ent = -1; break;
      case FDI_DISJOINT:
	ent = 1; break;
      default:
	request_tell(w,xmut,x,fd_complement(set),2,3); /* localized */
	ent = 1;
      }
    } else if (fd_member(x,set)) {
      ent = -1;			/* [fail] */
    } else {
      ent = 1;			/* [exit] */
    }
  } else {
    DerefArg(x,X(0),1);
    if (IsVar(x)) {
				/* [X in_set set,exit] */
      switch (intersecting) {
      case FDI_SUBSET:
      case FDI_EQUAL:
	ent = 1; break;
      case FDI_DISJOINT:
	ent = -1; break;
      default:
	request_tell(w,xmut,x,fd_localize_if_holes(w,set),2,3);
	ent = 1;
      }
    } else if (fd_member(x,set)) {
      ent = 1; /* [exit] */
    } else {
      ent = -1; /* [fail] */
    }
  }
  RefTerm(Actions) = request_done(w, ent, 2, 3);
}


/* '$fd_eq_iff'(+State, -Actions) :- X#=Y iff B.
   State is f(X,XMut,Y,YMut,B,BMut),
   Actions is a list of prunings etc.
*/
void SPCDECL
prolog_fd_eq_iff MAGIC (HIDDEN_PROTO
			SP_term_ref State,
			SP_term_ref NewState,
			SP_term_ref Actions)
{
  WAMENV;
  int ent = 0;		/* neither entailed nor dis- */
  int intersecting;
  TAGGED x, xmut, y, ymut, b, bmut, xdom, ydom;
  
  w->numstack_end = NULL;

/*    X(0) = RefTerm(State); */
  RefTerm(NewState) = RefTerm(State);
  X(2) = atom_nil;		/* actions list */
  DerefNonvar(X(0));
  DerefArg(x,X(0),1);
  DerefArg(xmut,X(0),2);
  DerefArg(y,X(0),3);
  DerefArg(ymut,X(0),4);
  DerefArg(b,X(0),5);
  DerefAttribute(xdom,xmut);	/* get dom/4 */
  xdom = DomainSet(xdom);	/* get FD set */
  DerefAttribute(ydom,ymut);	/* get dom/4 */
  ydom = DomainSet(ydom);	/* get FD set */
  intersecting = fd_compare(xdom,ydom);
  if (IsVar(b)) {
    DerefArg(bmut,X(0),6);
    switch (intersecting) {
    case FDI_DISJOINT:		/* [B=0,exit] */
      ent = 1;
      request_tell_value(w,bmut,b,TaggedZero,2,3);
      break;
    case FDI_EQUAL:
      if (!IsVar(y)) {		/* [B=1,exit] */
	ent = 1;
	request_tell_value(w,bmut,b,TaggedOne,2,3);
      }
      break;
    }
  } else if (Teqz(b)) {
    if (intersecting==FDI_DISJOINT)
      ent = 1;
    else if (!IsVar(x) && !IsVar(y))
      ent = -1;
    else if (!IsVar(y)) {
      request_tell(w,xmut,x,fd_compl_interval(y,y),2,3); /* localized */
      ent = 1;
    } else if (!IsVar(x)) {
      request_tell(w,ymut,y,fd_compl_interval(x,x),2,3); /* localized */
      ent = 1;
    }
  } else      
    ent = tell_eq(w,xmut,x,xdom,ymut,y,ydom,intersecting);

  RefTerm(Actions) = request_done(w, ent, 2, 3);
}


/* '$fd_le_iff'(+State, -Actions) :- X #=< Y iff B.
   State is f(X,XMut,Y,YMut,B,BMut),
   Actions is a list of prunings etc.
*/
void SPCDECL
prolog_fd_le_iff MAGIC (HIDDEN_PROTO
			SP_term_ref State,
			SP_term_ref NewState,
			SP_term_ref Actions)
{
  WAMENV;
  int ent = 0;		/* neither entailed nor dis- */
  TAGGED x, xmut, y, ymut, b, bmut, xdom, xmin, xmax, ydom, ymin, ymax;
  
  w->numstack_end = NULL;

/*    X(0) = RefTerm(State); */
  RefTerm(NewState) = RefTerm(State);
  X(2) = atom_nil;		/* actions list */
  DerefNonvar(X(0));
  DerefArg(x,X(0),1);
  DerefArg(xmut,X(0),2);
  DerefArg(y,X(0),3);
  DerefArg(ymut,X(0),4);
  DerefArg(b,X(0),5);
  DerefAttribute(xdom,xmut);	/* get dom/4 */
  xmin = DomainMin(xdom);
  xmax = DomainMax(xdom);
  DerefAttribute(ydom,ymut);	/* get dom/4 */
  ymin = DomainMin(ydom);
  ymax = DomainMax(ydom);
  if (IsVar(b)) {
    DerefArg(bmut,X(0),6);
    if (FDlt(ymax,xmin)) {	/* [B=0,exit] */
      ent = 1;
      request_tell_value(w,bmut,b,TaggedZero,2,3);
    } else if (!FDgt(xmax,ymin)) { /* [B=1,exit] */
      ent = 1;
      request_tell_value(w,bmut,b,TaggedOne,2,3);
    }
  } else if (Teqz(b)) {		/* enforce X #> Y */
    if (FDlt(ymax,xmin))
      ent = 1;
    else if (!FDgt(xmax,ymin))
      ent = -1;
    else {
      TAGGED ymin1 = ymin+IStep(1);
      TAGGED xmax1 = xmax-IStep(1);
      
      if (TagIsSmall(ymin) && FDlt(xmin,ymin1))
	request_tell_interval(w,xmut,x,ymin1,Sup,2,3);
      if (TagIsSmall(xmax) && FDgt(ymax,xmax1))
	request_tell_interval(w,ymut,y,Inf,xmax1,2,3);
      ent = !IsVar(x) || !IsVar(y) || xmax==ymin1;
    }
  } else {			/* enforce X #=< Y */
    if (FDlt(ymax,xmin))
      ent = -1;
    else if (!FDgt(xmax,ymin))
      ent = 1;
    else {
      if (TagIsSmall(ymax) && FDgt(xmax,ymax))
	request_tell_interval(w,xmut,x,Inf,ymax,2,3);
      if (TagIsSmall(xmin) && FDlt(ymin,xmin))
	request_tell_interval(w,ymut,y,xmin,Sup,2,3);
      ent = !IsVar(x) || !IsVar(y) || ymax==xmin;
    }
  }
  RefTerm(Actions) = request_done(w, ent, 2, 3);
}



/* '$fd_oneof'(+State, -Actions) :- X#=Y #\/ X#=Z.
   State is f(X,XMut,Y,YMut,Z,ZMut),
   Actions is a list of prunings etc.
*/
void SPCDECL
prolog_fd_oneof MAGIC (HIDDEN_PROTO
		       SP_term_ref State,
		       SP_term_ref NewState,
		       SP_term_ref Actions)
{
  WAMENV;
  int ent = 0;		/* neither entailed nor dis- */
  int xcapz, ycapz;
  TAGGED x, xmut, y, ymut, z, zmut, xdom, ydom, zdom;
  
  w->numstack_end = NULL;

/*    X(0) = RefTerm(State); */
  RefTerm(NewState) = RefTerm(State);
  X(2) = atom_nil;		/* actions list */
  DerefNonvar(X(0));
  DerefArg(x,X(0),1);
  DerefArg(xmut,X(0),2);
  DerefArg(y,X(0),3);
  DerefArg(ymut,X(0),4);
  DerefArg(z,X(0),5);
  DerefArg(zmut,X(0),6);
  DerefAttribute(xdom,xmut);	/* get dom/4 */
  xdom = DomainSet(xdom);	/* get FD set */
  DerefAttribute(ydom,ymut);	/* get dom/4 */
  ydom = DomainSet(ydom);	/* get FD set */
  DerefAttribute(zdom,zmut);	/* get dom/4 */
  zdom = DomainSet(zdom);	/* get FD set */
  xcapz = fd_compare(xdom,zdom);
  ycapz = fd_compare(ydom,zdom);
  if (xcapz==FDI_DISJOINT)
    ent = tell_eq(w,ymut,y,ydom,zmut,z,zdom,ycapz);
  else if (ycapz==FDI_DISJOINT)
    ent = tell_eq(w,xmut,x,xdom,zmut,z,zdom,xcapz);
  else {
    TAGGED xcupy = fd_or(xdom,ydom);

    ent = fd_singleton(xcupy);	/* integer(Z) after pruning does NOT suffice */
    switch (fd_compare(zdom,xcupy)) {
    case FDI_INTERSECT:
      ent = fd_singleton(fd_and(zdom,xcupy));
    case FDI_SUPERSET:
      request_tell(w,zmut,z,fd_localize_if_holes(w,xcupy),2,3);
      break;
    case FDI_DISJOINT:
      ent = -1;
      break;
    }
  }
  RefTerm(Actions) = request_done(w, ent, 2, 3);
}


/* computes -dom(d2) */
/* ripped off from indexical.c */
static TAGGED negdom MAGIC (HIDDEN_PROTO
			    TAGGED d2)
{
  int j;
  TAGGED t2, r2, tail, *h, *array;

  for (j=0, t2=d2; t2!=EmptySet; j++)
    t2 = CTagToCdr(t2);

				/* create j intervals, then merge */

  array = numstack_alloc(4*j);
  h = array + 4*j;
  tail = EmptySet;
  for (t2=d2; t2!=EmptySet;) {
    h -= 4;
    r2 = CTagToCar(t2); t2 = CTagToCdr(t2);
    h[0] = MakeList(h+2);
    h[1] = tail;
    h[2] = safe_minus(TaggedZero,RangeMax(r2));
    h[3] = safe_minus(TaggedZero,RangeMin(r2));
    tail = MakeList(h);
  }
  return tail;
}


/* '$fd_abs'(+State, -Actions) :- abs(X) #= Y.
   State is f(X,XMut,Y,YMut),
   Actions is a list of prunings etc.
*/
void SPCDECL
prolog_fd_abs MAGIC (HIDDEN_PROTO
		     SP_term_ref State,
		     SP_term_ref NewState,
		     SP_term_ref Actions)
{
  WAMENV;
  int ent = 0;		/* neither entailed nor dis- */
  TAGGED x, xmut, y, ymut, xdom, ydom;
  
  w->numstack_end = NULL;

/*    X(0) = RefTerm(State); */
  RefTerm(NewState) = RefTerm(State);
  X(2) = atom_nil;		/* actions list */
  DerefNonvar(X(0));
  DerefArg(x,X(0),1);
  DerefArg(xmut,X(0),2);
  DerefArg(y,X(0),3);
  DerefArg(ymut,X(0),4);
  DerefAttribute(xdom,xmut);	/* get dom/4 */
  xdom = DomainSet(xdom);	/* get FD set */
  DerefAttribute(ydom,ymut);	/* get dom/4 */
  ydom = DomainSet(ydom);	/* get FD set */
  if (!FDlt(fd_min(xdom),TaggedZero))
    ent = tell_eq(w,xmut,x,xdom,ymut,y,ydom,fd_compare(xdom,ydom));
  else {
    TAGGED ydom0 = ydom;
    TAGGED xndom, yndom, xdom1, ydom1;
    
    if (FDlt(fd_min(ydom),TaggedZero))
      ydom0 = fd_and_interval(ydom0,TaggedZero,Sup);
    xndom = negdom(xdom);
    yndom = negdom(ydom0);
    xdom1 = fd_or(ydom0,yndom);
    ydom1 = fd_or(xdom,xndom);
    if (FDlt(fd_min(ydom),TaggedZero))
      ydom1 = fd_and_interval(ydom1,TaggedZero,Sup);
    switch (fd_compare(xdom,xdom1)) {
    case FDI_SUBSET:
    case FDI_EQUAL:
      break;
    case FDI_DISJOINT:
      ent = -1;
      goto ret;
    case FDI_INTERSECT:
      xdom1 = fd_and(xdom,xdom1);
    case FDI_SUPERSET:
      request_tell(w,xmut,x,fd_localize_if_holes(w,xdom1),2,3);
    }
    switch (fd_compare(ydom,ydom1)) {
    case FDI_SUBSET:
    case FDI_EQUAL:
      ent = fd_singleton(ydom1);
      break;
    case FDI_DISJOINT:
      ent = -1;
      break;
    case FDI_INTERSECT:
      ydom1 = fd_and(ydom,ydom1);
    case FDI_SUPERSET:
      ent = fd_singleton(ydom1);
      request_tell(w,ymut,y,fd_localize_if_holes(w,ydom1),2,3);
    }
  }
 ret:
  RefTerm(Actions) = request_done(w, ent, 2, 3);
}

