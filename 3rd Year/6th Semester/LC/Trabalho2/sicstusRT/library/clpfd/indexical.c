/* Copyright(C) 1999, Swedish Institute of Computer Science */

#include "fd.h"
#include "fd_insn.h"

#if MULTI_SP_AWARE
#define fd_neg_offset(A1,A2) fd_neg_offset(HIDDEN_ARG, A1,A2)
#define fd_merge(A1,A2,A3) fd_merge(HIDDEN_ARG, A1,A2,A3)
#define fd_plus(A1,A2) fd_plus(HIDDEN_ARG, A1,A2)
#define fd_minus(A1,A2) fd_minus(HIDDEN_ARG, A1,A2)
#define fd_setmod(A1,A2) fd_setmod(HIDDEN_ARG, A1,A2)
#define fd_prune_and_enqueue(A1,A2,A3,A4) fd_prune_and_enqueue(HIDDEN_ARG, A1,A2,A3,A4)
#define handle_global(A1,A2,A3,A4) handle_global(HIDDEN_ARG, A1,A2,A3,A4)
#define finish_pruning(A1,A2,A3,A4) finish_pruning(HIDDEN_ARG, A1,A2,A3,A4)
#define prune_set(A1,A2,A3,A4,A5) prune_set(HIDDEN_ARG, A1,A2,A3,A4,A5)
#define prune_set_compl(A1,A2,A3,A4,A5) prune_set_compl(HIDDEN_ARG, A1,A2,A3,A4,A5)
#define prune_interval(A1,A2,A3,A4,A5,A6) prune_interval(HIDDEN_ARG, A1,A2,A3,A4,A5,A6)
#define prune_interval_compl(A1,A2,A3,A4,A5,A6) prune_interval_compl(HIDDEN_ARG, A1,A2,A3,A4,A5,A6)
#define prune_value(A1,A2,A3,A4,A5) prune_value(HIDDEN_ARG, A1,A2,A3,A4,A5)
#define prune_value_compl(A1,A2,A3,A4,A5) prune_value_compl(HIDDEN_ARG, A1,A2,A3,A4,A5)
#define fd_store_token(A1,A2,A3) fd_store_token(HIDDEN_ARG, A1,A2,A3)
#define fd_store_literal(A1,A2) fd_store_literal(HIDDEN_ARG, A1,A2)

#define free_fd_info(A1) free_fd_info(HIDDEN_ARG, A1)

#define fd_install_indexicals(A1,A2,A3,A4) fd_install_indexicals(HIDDEN_ARG, A1,A2,A3,A4)
#define init_fd_constraint(A1) init_fd_constraint(HIDDEN_ARG, A1)
#define fd_install(A1,A2,A3,A4,A5) fd_install(HIDDEN_ARG, A1,A2,A3,A4,A5)
#define link_indexical(A1,A2) link_indexical(HIDDEN_ARG, A1,A2)
#endif /* MULTI_SP_AWARE */

#define PositiveBound(B) (Tgez(B) && TagIsSmall(B))

#define Top (top[-1])

#define Pop (*(--top))

#define Push(X) (*top++ = (X))

 
static BOOL fd_infinite(TAGGED d1)
{
  TAGGED r1;

  r1 = CTagToCar(d1); d1 = CTagToCdr(d1);
  if (RangeMin(r1)==Inf)
    return TRUE;
  while (d1!=EmptySet)
    r1 = CTagToCar(d1), d1 = CTagToCdr(d1);
  if (RangeMax(r1)==Sup)
    return TRUE;
  return FALSE;
}


static TAGGED fd_neg_offset MAGIC (HIDDEN_PROTO
				   TAGGED d1,
				   long offset)
{
  TAGGED value;
  
  *fd_neg_internal(d1,offset,&value) = EmptySet;
  return value;
}


static TAGGED fd_merge MAGIC (HIDDEN_PROTO
			      int n,
			      TAGGED *array, TAGGED **rest)
{
  if (n==1)
    {
      *rest = array+4;
      return MakeList(array);
    }
  else
    {
      TAGGED t1 = fd_merge(n>>1, array, &array);
      TAGGED t2 = fd_merge(n-(n>>1), array, rest);

      return fd_or(t1,t2);
    }
}



/* d1 x d2 -> {s+t | s in d1 & t in d2} */
static TAGGED fd_plus MAGIC (HIDDEN_PROTO
			     TAGGED d1, TAGGED d2)
{
  int i, j;
  TAGGED t1, t2, r1, r2, min1, max1, *h, *array;

  if (d1==EmptySet || d2==EmptySet)
    return EmptySet;
  for (i=0, t1=d1; t1!=EmptySet; i+=1)
    t1 = CTagToCdr(t1);
  for (j=0, t2=d2; t2!=EmptySet; j+=i)
    t2 = CTagToCdr(t2);

				/* create j intervals, then merge */

  array = h = numstack_alloc(4*j);
  for (t1=d1; t1!=EmptySet;)
    {
      r1 = CTagToCar(t1); t1 = CTagToCdr(t1);
      min1 = RangeMin(r1);
      max1 = RangeMax(r1);
      for (t2=d2; t2!=EmptySet;)
	{
	  r2 = CTagToCar(t2); t2 = CTagToCdr(t2);
	  h[0] = MakeList(h+2);
	  h[1] = EmptySet;
	  h[2] = safe_plus(min1,RangeMin(r2));
	  h[3] = safe_plus(max1,RangeMax(r2));
	  h += 4;
	}
    }
  return fd_merge(j, array, &array);
}



/* d1 x d2 -> {s-t | s in d1 & t in d2} */
static TAGGED fd_minus MAGIC (HIDDEN_PROTO
			      TAGGED d1, TAGGED d2)
{
  int i, j;
  TAGGED t1, t2, r1, r2, min1, max1, *h, *array;

  if (d1==EmptySet || d2==EmptySet)
    return EmptySet;
  for (i=0, t1=d1; t1!=EmptySet; i+=1)
    t1 = CTagToCdr(t1);
  for (j=0, t2=d2; t2!=EmptySet; j+=i)
    t2 = CTagToCdr(t2);

				/* create j intervals, then merge */

  array = h = numstack_alloc(4*j);
  for (t1=d1; t1!=EmptySet;)
    {
      r1 = CTagToCar(t1); t1 = CTagToCdr(t1);
      min1 = RangeMin(r1);
      max1 = RangeMax(r1);
      for (t2=d2; t2!=EmptySet;)
	{
	  r2 = CTagToCar(t2); t2 = CTagToCdr(t2);
	  h[0] = MakeList(h+2);
	  h[1] = EmptySet;
	  h[2] = safe_minus(min1,RangeMax(r2));
	  h[3] = safe_minus(max1,RangeMin(r2));
	  h += 4;
	}
    }
  return fd_merge(j, array, &array);
}


/* support for FD_SETMOD */
static TAGGED fd_setmod MAGIC (HIDDEN_PROTO
			       TAGGED t1, TAGGED t2)
{
  TAGGED value = EmptySet;
  WORD n1, n2;
  WORD modulus = GetSmall(t2);
  
  if (Tlez(t2))
    return ERRORTAG;
  while (t1!=EmptySet)
    {
      TAGGED min1, max1, r1, subset;

      r1 = CTagToCar(t1); t1 = CTagToCdr(t1);
      min1 = RangeMin(r1);
      max1 = RangeMax(r1);

      if (min1==Inf || max1==Sup || Tge(max1-min1+IStep(1),t2-TaggedZero))
	{
	  subset = fd_interval(TaggedZero, t2-IStep(1));
	  t1 = EmptySet;
	}
      else
	{
	  n1 = GetSmall(min1) % modulus;
	  n2 = GetSmall(max1) % modulus;
	  if (n1 <= n2)
	    subset = fd_interval(MakeSmall(n1), MakeSmall(n2));
	  else
	    subset = fd_or(fd_interval(TaggedZero,MakeSmall(n2)),
			   fd_interval(MakeSmall(n1),t2-IStep(1)));
	}
      value = fd_or(value,subset);
    }
  return value;
}


/*** telling ***/

int fd_tell_value MAGIC (HIDDEN_PROTO
			 Argdecl,
			 TAGGED old, TAGGED value, TAGGED dest_attribute)
{
  TAGGED *h, t1;
  TAGGED domain, mutable;
  int why = MASK_DOM+MASK_MINMAX+MASK_VAL;
  LetShadowregs;

  AttrToDomM(dest_attribute,mutable);
  domain = RefMutable(mutable);
  if (!TagIsSmall(value))
    fd.fd_overflow = TRUE;
  if (DomainMin(domain)!=value)
      why += MASK_MIN;
  if (DomainMax(domain)!=value)
      why += MASK_MAX;
  
  if (TagToSTR(domain) >= GLOBAL_UNCOND)
    {			/* can safely smash domain _and set_ */
      DomainMin(domain) = value;
      DomainMax(domain) = value;
      DomainSize(domain) = TaggedOne;
      CTagToCar(old) = MakeList(&DomainMin(domain));
      CTagToCdr(old) = EmptySet;
    }
  else
    {
      h = w->global_top;
      t1 = MakeList(h);
      *h++ = t1+WD(4);
      *h++ = EmptySet;
      *h++ = fd.functor_dom4;
      *h++ = t1;
      *h++ = value;
      *h++ = value;
      *h++ = TaggedOne;
      w->global_top = h;
      update_mutable(w,MakeStructure(h-5), mutable);
    }
  return why;
}


int fd_tell_interval MAGIC (HIDDEN_PROTO Argdecl,
			    TAGGED old, TAGGED min, TAGGED max,
			    TAGGED dest_attribute,
			    int why)
{
  TAGGED *h, t1;
  TAGGED card, domain, mutable;
  LetShadowregs;
  
  if (min==max)
    return fd_tell_value(w,old,min,dest_attribute);
  card = max-min+TaggedOne;
  if (!TagIsSmall(card))
    card = Sup;
  AttrToDomM(dest_attribute,mutable);
  domain = RefMutable(mutable);
  if (TagToSTR(domain) >= GLOBAL_UNCOND)
    {			/* can safely smash domain _and set_ */
      (void)fd_put_range(&DomainMin(domain), min, max);
      DomainSize(domain) = card;
      CTagToCar(old) = MakeList(&DomainMin(domain));
      CTagToCdr(old) = EmptySet;
    }
  else
    {
      h = w->global_top;
      t1 = MakeList(h);
      *h++ = t1+WD(4);
      *h++ = EmptySet;
      *h++ = fd.functor_dom4;
      *h++ = t1;
      h = fd_put_range(h, min, max);
      *h++ = card;
      w->global_top = h;
      update_mutable(w,MakeStructure(h-5), mutable);
    }
  return why;
}


/* Precondition:
   X(ar) = dest_attribute;
   X(ar+1) = dest_var;
   hence ar+2 live X regs

   Precondition: new is an interval, or does not point to the heap
*/
int fd_tell MAGIC (HIDDEN_PROTO
		   Argdecl,
		   TAGGED old, TAGGED new, TAGGED dest_attribute,
		   int ar)
{
  int why = MASK_DOM;
  TAGGED *h, t1;
  TAGGED min=0, max, card, domain, dmin, dmax, mutable;
  
  DerefAttribute(domain,dest_attribute);
  dmin = DomainMin(domain);
  dmax = DomainMax(domain);  
  if (CTagToCdr(new)==EmptySet)
    {				/* new domain is an interval */
      new = CTagToCar(new);
      min = RangeMin(new);
      max = RangeMax(new);
      if (min!=dmin) why |= MASK_MIN+MASK_MINMAX;
      if (max!=dmax) why |= MASK_MAX+MASK_MINMAX;
      return fd_tell_interval(w,old,min,max,dest_attribute,why);
    }
  else
    {
      TAGGED d2=new, r2, b2, e2;
      long size=0;
      LetShadowregs;

      do {
	r2 = CTagToCar(d2); d2 = CTagToCdr(d2);
	b2 = RangeMin(r2);  e2 = RangeMax(r2);
	if (size==0) min = b2;
	if (size>=0 && AreSmall(b2,e2))
	  size += GetSmall(e2)-GetSmall(b2)+1;
	else
	  size = -1;
      } while (d2!=EmptySet);
      max = e2;
      card = (size>=0 ? MakeSmall(size) : Sup);

      if (min!=dmin) why |= MASK_MIN+MASK_MINMAX;
      if (max!=dmax) why |= MASK_MAX+MASK_MINMAX;
      t1 = fd_globalize(w,new,5,ar+2); /* can GC */
      AttrToDomM(X(ar),mutable);
      domain = RefMutable(mutable);
      if (TagToSTR(domain) >= GLOBAL_UNCOND)
	{
	  TAGGED *arg = TagToArg(domain,0);
	  
	  arg[1] = t1;
	  arg[2] = min;
	  arg[3] = max;
	  arg[4] = card;
	}
      else
	{
	  h = w->global_top;
	  *h++ = fd.functor_dom4;
	  *h++ = t1;
	  *h++ = min;
	  *h++ = max;
	  *h++ = card;
	  w->global_top = h;
	  update_mutable(w,MakeStructure(h-5), mutable);
	}
    }
  return why;
}


/* This function performs two actions:
   1. On entry, why is a bitmask of the effect of the pruning just done.
      This is and:ed with the suspension list bitmask, and or:ed with
      MASK_SINGLETON if appropriate.
   2. If the domain is now a singleton, the variable in question is bound. 
*/
static int finish_pruning MAGIC (HIDDEN_PROTO
				 Argdecl,
				 int why,
				 TAGGED dest_attribute, TAGGED dest_var)
{
  TAGGED mutable, t1;
  int mask;
  LetShadowregs;
  
  fd.prunings++;
  AttrToSuspM(dest_attribute,mutable);
  mask = GetSmall(CTagToArg(RefMutable(mutable),2)); /* bitmask of susp. lists */
  if (why & MASK_VAL)
    {
      TAGGED value;

      DerefAttribute(value,dest_attribute);
      value = DomainMin(value);
				/* purify and bind argument */
      DerefSwitch(dest_var,t1,
		  {
		    delete_attributes(dest_var,fd.fd_module);
		    dest_var = CTagToPointer(CTagToPointer(dest_var));
		    if (TagIsHVA(dest_var))
		      {BindHVA(dest_var,value);}
		    else
		      {BindCVA(dest_var,value); Wake;}
		  });
      return (why & mask) + MASK_SINGLETON;
    }
  else
    return (why & mask);
}




/* dest.domain &= src
   caller wants ar X regs to be protected

   will only update fd.prunings
*/
static int prune_set MAGIC (HIDDEN_PROTO
			    Argdecl,
			    TAGGED dest_attribute, TAGGED dest_var,
			    TAGGED src, int ar)
{
  TAGGED domain, old;
  int why;
  
  DerefAttribute(domain,dest_attribute);
  old = DomainSet(domain);
  switch (fd_compare(old,src))
    {
    default:
      /*
    case FDI_SUBSET:
    case FDI_EQUAL:
      */
      return 0;
    case FDI_DISJOINT:
      return -1;
    case FDI_SUPERSET:
      X(ar) = dest_attribute;		/* preserve over GC */
      X(ar+1) = dest_var;
      why = fd_tell(w,old,fd_localize_if_holes(w,src),dest_attribute,ar);
      break;
    case FDI_INTERSECT:
      X(ar) = dest_attribute;		/* preserve over GC */
      X(ar+1) = dest_var;
      if (CTagToCdr(old)==EmptySet)
	{
	  TAGGED range = CTagToCar(old);
	  TAGGED min = RangeMin(range);
	  TAGGED max = RangeMax(range);
	  
	  why = fd_tell(w,old,fd_localize_if_holes(w,fd_and_interval(src,min,max)),
			dest_attribute,ar);
	}
      else
	why = fd_tell(w,old,fd_localize_if_holes(w,fd_and(old,src)),
		      dest_attribute,ar);
    }
  return finish_pruning(w,why,X(ar),X(ar+1));
}



/* dest.domain &= \(src)
   caller wants ar X regs to be protected

   will only update fd.prunings
*/
static int prune_set_compl MAGIC (HIDDEN_PROTO
				  Argdecl,
				  TAGGED dest_attribute, TAGGED dest_var,
				  TAGGED src, int ar)
{
  TAGGED domain, old;
  int why;
  
  DerefAttribute(domain,dest_attribute);
  old = DomainSet(domain);
  switch (fd_compare(old,src))
    {
    default:
      /*
    case FDI_SUBSET:
    case FDI_EQUAL:
      */
      return -1;
    case FDI_DISJOINT:
      return 0;
    case FDI_SUPERSET:
    case FDI_INTERSECT:
      X(ar) = dest_attribute;		/* preserve over GC */
      X(ar+1) = dest_var;
      if (CTagToCdr(old)==EmptySet)
	{
	  TAGGED range = CTagToCar(old);
	  TAGGED min = RangeMin(range);
	  TAGGED max = RangeMax(range);

	  src = fd_interval_subtract(min,max,src);
	}
      else
	src = fd_subtract(old,src);
      why = fd_tell(w,old,fd_localize_if_holes(w,src),dest_attribute,ar);
    }
  return finish_pruning(w,why,X(ar),X(ar+1));
}



/* an important special case: X in min..max */
static int prune_interval MAGIC (HIDDEN_PROTO
				 Argdecl,
				 TAGGED dest_attribute, TAGGED dest_var,
				 TAGGED min, TAGGED max, int ar)
{
  TAGGED domain, old, min1, max1;
  int why = 0;
  
  DerefAttribute(domain,dest_attribute);
  old = DomainSet(domain);
  switch (fd_compare_interval(old,min,max))
    {
    case FDI_SUBSET:
    case FDI_EQUAL:
      return 0;
    case FDI_DISJOINT:
      return -1;
    case FDI_INTERSECT:
      if (CTagToCdr(old)!=EmptySet) {
	X(ar) = dest_attribute;	/* preserve over GC */
	X(ar+1) = dest_var;
	why = fd_tell(w,old,fd_localize_if_holes(w,fd_and_interval(old,min,max)),
		      dest_attribute,ar);
	break;
      }
    case FDI_SUPERSET:
      X(ar) = dest_attribute;	/* preserve over GC */
      X(ar+1) = dest_var;
      min1 = DomainMin(domain);
      max1 = DomainMax(domain);
      if (min1!=min)
	{
	  if (!EmptyInterval(min1,min))
	    why |= MASK_MIN+MASK_MINMAX+MASK_DOM;
	  else
	    min = min1;
	}
      if (max!=max1)
	{
	  if (!EmptyInterval(max,max1))
	    why |= MASK_MAX+MASK_MINMAX+MASK_DOM;
	  else
	    max = max1;
	}
      why = fd_tell_interval(w,old,min,max,dest_attribute,why);
    }
  return finish_pruning(w,why,X(ar),X(ar+1));
}


/* an important special case: X in \(min..max) */
static int prune_interval_compl MAGIC (HIDDEN_PROTO
				       Argdecl,
				       TAGGED dest_attribute, TAGGED dest_var,
				       TAGGED min, TAGGED max, int ar)
{
  TAGGED domain, old;
  int why = 0;
  
  DerefAttribute(domain,dest_attribute);
  old = DomainSet(domain);
  switch (fd_compare_interval(old,min,max)) /* need precise result here! */
    {
    case FDI_SUBSET:
    case FDI_EQUAL:
      return -1;
    case FDI_DISJOINT:
      return 0;
    default:
      X(ar) = dest_attribute;	/* preserve over GC */
      X(ar+1) = dest_var;
      if (CTagToCdr(old)==EmptySet)
	{
	  TAGGED range, min1, max1;

	  range = CTagToCar(old);
	  min1 = RangeMin(range);
	  max1 = RangeMax(range);

	  if (InInterval(min1,min,max))
	    why |= MASK_MIN+MASK_MINMAX+MASK_DOM,
	    min1 = max+IStep(1);
	  if (InInterval(max1,min,max))
	    why |= MASK_MAX+MASK_MINMAX+MASK_DOM,
	    max1 = min-IStep(1);
	  if (why>0)		/* false if min..max did not prune any bound */
	    why = fd_tell_interval(w,old,min1,max1,dest_attribute,why);
	  else
	    why = fd_tell(w,old,fd_localize_if_holes(w,fd_subtract_interval(old,min,max)),
			  dest_attribute,ar);
	}
      else
	why = fd_tell(w,old,fd_localize_if_holes(w,fd_subtract_interval(old,min,max)),
		      dest_attribute,ar);
    }
  return finish_pruning(w,why,X(ar),X(ar+1));
}


/* an important special case: X in val..val */
static int prune_value MAGIC (HIDDEN_PROTO
			      Argdecl,
			      TAGGED dest_attribute, TAGGED dest_var,
			      TAGGED value, int ar)
{
  TAGGED domain, old;
  int why;

  (void)ar;
  DerefAttribute(domain,dest_attribute);
  old = DomainSet(domain);
  if (!fd_member(value,old))
    return -1;
  else
    why = fd_tell_value(w,old,value,dest_attribute); /* no GC possible */
  return finish_pruning(w,why,dest_attribute,dest_var);
}



/* an important special case: X in \(val..val) */
static int prune_value_compl MAGIC (HIDDEN_PROTO
				    Argdecl,
				    TAGGED dest_attribute, TAGGED dest_var,
				    TAGGED value, int ar)
{
  TAGGED domain, old, min, max, card, mutable;
  int why;
  
  DerefAttribute(domain,dest_attribute);
  old = DomainSet(domain);
  if (!fd_member(value,old))
    return 0;

  min = DomainMin(domain);
  max = DomainMax(domain);
  card = DomainSize(domain);
  if (TagIsSmall(card)) card -= IStep(1);
  if (min==value) why = MASK_MIN+MASK_MINMAX+MASK_DOM;
  else if (max==value) why = MASK_MAX+MASK_MINMAX+MASK_DOM;
  else why = MASK_DOM;

  if (CTagToCdr(old)==EmptySet && (why&MASK_MINMAX)) /* X is an interval, R is min/max */
    {
      if (why & MASK_MIN)
	  min += IStep(1);
      else
	  max -= IStep(1);
      why = fd_tell_interval(w,old,min,max,dest_attribute,why);
    }
  else
    {
      TAGGED *h, t1;
      TAGGED new = fd_delete(old,value);
      TAGGED r1 = CTagToCar(new);
      TAGGED d1 = CTagToCdr(new);
      LetShadowregs;

      if (d1==EmptySet)
	{				/* new domain is an interval */
	  if (why & MASK_MIN)
	    min = RangeMin(r1);
	  else if (why & MASK_MAX)
	    max = RangeMax(r1);
	  why = fd_tell_interval(w,old,min,max,dest_attribute,why);
	}
      else
	{
	  if (why & MASK_MIN)
	    min = RangeMin(r1);
	  else if (why & MASK_MAX)
	    {
	      while (d1!=EmptySet)
		r1 = CTagToCar(d1),
	        d1 = CTagToCdr(d1);
	      max = RangeMax(r1);
	    }
				/* N.B. dest_var is dead here! */
	  X(ar) = dest_attribute;
	  t1 = fd_globalize(w,fd_localize(w,new),5,ar+1); /* can GC */
	  AttrToDomM(X(ar),mutable); /* refresh */
	  domain = RefMutable(mutable);
	  if (TagToSTR(domain) >= GLOBAL_UNCOND)
	    {
	      TAGGED *arg = TagToArg(domain,0);

	      arg[1] = t1;
	      arg[2] = min;
	      arg[3] = max;
	      arg[4] = card;
	    }
	  else
	    {
	      h = w->global_top;
	      *h++ = fd.functor_dom4;
	      *h++ = t1;
	      *h++ = min;
	      *h++ = max;
	      *h++ = card;
	      w->global_top = h;
	      update_mutable(w,MakeStructure(h-5), mutable);
	    }
	}
    }
  return finish_pruning(w,why,dest_attribute,dest_var);
}


/*** predicates ***/

/* $fd_tell(Var, +FDSet, -Effect)
   Effect is (value returned by fd_tell())/\7.
   Similar to prune_set().
*/
long SPCDECL
prolog_fd_tell MAGIC (HIDDEN_PROTO
		      SP_term_ref VarR,
		      SP_term_ref SetR)
{
  WAMENV;
  TAGGED domain, old, dest_attribute, src, var, t1;
  int why = 0;

  var = RefTerm(VarR);
  src = RefTerm(SetR);
  DerefNonvar(src);
  DerefSwitch(var,t1,goto prune;);
  if (!TagIsSmall(var) || !fd_member(var,src))
    goto fail;
  else
    goto ret;
 prune:
  dest_attribute = check_argument(w,var,Inf,Sup,Sup);
  w->numstack_end = NULL;
  DerefAttribute(domain,dest_attribute);
  old = DomainSet(domain);
  switch (fd_compare(old,src))
    {
    case FDI_SUBSET:
    case FDI_EQUAL:
      break;
    case FDI_DISJOINT:
      goto fail;
    case FDI_SUPERSET:
      X(3) = dest_attribute;		/* preserve over GC */
      X(4) = var;
      why = fd_tell(w,old,fd_localize_if_holes(w,src),
		    dest_attribute,3);
    case FDI_INTERSECT:
      X(3) = dest_attribute;		/* preserve over GC */
      X(4) = var;
      why = fd_tell(w,old,fd_localize_if_holes(w,fd_and(old,src)),
		    dest_attribute,3);
    }
 ret:
  return why&7;
 fail:
  SP_fail();
  return 0;
}


/* Guts of below, used in fd_prune_and_enqueue().
*/
void
fd_told MAGIC (HIDDEN_PROTO Argdecl,
	       TAGGED dest_attribute, TAGGED dest_var, long why)
{
  TAGGED domain;
  
  DerefAttribute(domain,dest_attribute);
				/* merge in implied bits */
  if (why & (MASK_MIN | MASK_MAX))
    why |= MASK_MINMAX;
  if (DomainSize(domain)==TaggedOne)
    why |= MASK_VAL;
				/* maybe bind; filter why */
  why = finish_pruning(w,why,dest_attribute,dest_var);
  if (why > 0) {
    TAGGED mutable;

    AttrToSuspM(dest_attribute,mutable);
    fd_enqueue_all(w,why,atom_nil,mutable);
  }
}


static BOOL fd_prune_and_enqueue MAGIC (HIDDEN_PROTO Argdecl,
					SP_term_ref ActionsR,
					SP_term_ref GlobalR,
					SP_term_ref ItemR)
{
  TAGGED dest_var, glob;
  TAGGED statmut, status;
  int why;

  fd_sync(Arg);
  while (TagIsLST(X(0))) {
    TAGGED item;
    
    DerefCar(item,X(0));
    DerefCdr(X(0),X(0));
    if (TagIsATM(item)) {
      LetShadowregs;
      
      if (item==atom_fail) {
	return FALSE;
      }
      fd.entailments++;
      DEREF(glob,RefTerm(GlobalR)); /* get global/5 */
      DerefArg(glob,glob,4);	/* get Ent */
      BindHVA(glob,MakeSmall(1)); /* always HVA */
    } else {
      TAGGED functor = TagToHeadfunctor(item);
      
      if (functor == fd.functor_dom)
	why = 1;
      else if (functor == fd.functor_min)
	why = 3;
      else if (functor == fd.functor_max)
	why = 5;
      else if (functor == fd.functor_minmax)
	why = 7;
      else {
	RefTerm(ActionsR) = X(0);
	RefTerm(ItemR) = item;
	if (SP_query(fd.call_action1,ItemR)!=SP_SUCCESS) {
	  return FALSE;
	} else {
	  X(0) = RefTerm(ActionsR);
	  continue;
	}
      }
      DerefArg(dest_var,item,1);
      if (IsVar(dest_var)) {	/* otherwise, co-reference already dealt with */
	TAGGED dest_attribute = get_attributes(dest_var,fd.fd_module);
	fd_told(w, dest_attribute, dest_var, why);
      }
    }
  }
  DEREF(glob,RefTerm(GlobalR));	/* get global/5 */
  DerefArg(statmut,glob,3);	/* get status mutable */
  status = RefMutable(statmut);
  if (status==MakeSmall(3))	/* enable self-resumption if non-coref */
    update_mutable(w,MakeSmall(2),statmut);
  return TRUE;
}

/*
% prune_and_enqueue(+Actions, +Global, -RC, -Global1)

*/
void SPCDECL
prolog_fd_prune_and_enqueue MAGIC (HIDDEN_PROTO
				   SP_term_ref ActionsR,
				   SP_term_ref GlobalR)
{
  WAMENV;

  /*    X(0) = RefTerm(ActionsR); */
  DerefNonvar(X(0));
  if (!fd_prune_and_enqueue(Arg,ActionsR,GlobalR,SP_new_term_ref())) {
    fd.failures++;
    SP_fail();
  }
}



/* $fd_check_arguments(+Goal, -Attv) */
void SPCDECL
prolog_fd_check_arguments MAGIC (HIDDEN_PROTO
				 SP_term_ref GoalR,
				 SP_term_ref AttvR)
{
  WAMENV;
  int i, ar;
  TAGGED attr[ARITYLIMIT];
  TAGGED *h, v;

  X(0) = RefTerm(GoalR);
  DerefNonvar(X(0));
  if (!TagIsSTR(X(0))) {
    RefTerm(AttvR) = X(0);
  } else {
    attr[0] = TagToHeadfunctor(X(0));
    ar = Arity(attr[0]);
    RequireHeap(ar*(FD_ATTRIBUTE_SIZE+ARITYLIMIT+6), 2);
    for (i=1; i<=ar; i++)
      if (!(attr[i]=check_argument(w,CTagToArg(X(0),i),Inf,Sup,Sup))) {
	SP_fail();
	return;
      }
    h = w->global_top;
    v = MakeStructure(h);
    *h++ = attr[0];
    for (i=1; i<=ar; i++)
      *h++ = attr[i];
    w->global_top = h;
    RefTerm(AttvR) = v;
  }
}


static ix_byte *fd_store_token MAGIC (HIDDEN_PROTO
				      ix_byte *ptr,
				      TAGGED token,
				      TAGGED *literals)
{
  if (TagIsSIN(token))		/* opcode */
    {
      token = DispatchLabel((TAGGED)GetSmall(token));
    }
  else
    {
      TAGGED f = TagToHeadfunctor(token);
      TAGGED arg;
      int i;

      DerefArg(arg,token,1);
      i = GetSmall(arg);
      if (f==fd.token_a)		/* argreg offset */
	token = (ix_byte)i;
      else if (f==fd.token_t)	/* tagged literal offset */
	token = (ix_byte)literals[i];
      else if (f==fd.token_d)	/* ground domain */
	{
	  token = (ix_byte)literals[i];
	  if (TagIsSTR(token))
	    token = MakeList(TagToArg(token,1));
	}
      else if (f==fd.token_h)		/* hash table literal offset */
	token = (ix_byte)TermToPointer(literals[i]-1);
      else if (f==fd.token_l)		/* code offset, RELATIVE */
	token = (ix_byte)i;
    }
  *ptr++ = token;
  return ptr;
}



static TAGGED *fd_store_literal MAGIC (HIDDEN_PROTO
				       TAGGED *literals,
				       TAGGED token)
{
  switch (TagOf(token))
    {
    case TAG_SIN:			/* constant term or inf/sup */
    case TAG_ATM:
      break;
    case TAG_STR:
      {
	TAGGED f = TagToHeadfunctor(token);
	DerefArg(token, token, 1);
	if (f==fd.token_d)
	  {			/* domain */
	    int k = 1;
	    TAGGED p = token;
	    TAGGED *q, car, cdr;

	    if (p==atom_nil)
	      break;
	    while (TagIsLST(p))
	      {
		k += 4;
		DerefCdr(p,p);
	      }
	    q = sp_checkalloc(k*sizeof(TAGGED) /*MM_MISC*/);
	    q[0] = TagIndex(TAG_LIN,k);
	    k = 1;
	    p = token;
	    while (TagIsLST(p))
	      {
		DerefCar(car,p);
		DerefCdr(cdr,car);
		DerefCar(car,car);
		DerefCdr(p,p);
		q[k+0] = MakeList(q+k+2);
		q[k+1] = MakeList(q+k+4);
		q[k+2] = car;
		q[k+3] = cdr;
		k += 4;
	      }
	    q[k-3] = atom_nil;
	    token = MakeStructure(q);
	  }
	else			/* hash table */
	  {
	    struct sw_on_key *sw = new_switch_on_key(2,NULL);
	    TAGGED item, key, value;
	    
	    while (TagIsLST(token))
	      {
		DerefCar(item, token);
		DerefCdr(token, token);
		DerefArg(key, item, 1);
		DerefArg(value, item, 2);
		dyn_puthash(&sw,key)->value.arities = value;
	      }
	    token = PointerToTerm(sw)+1; /* mark for dealloc */
	  }
      }
      break;
    }
  *literals++ = token;
  return literals;
}

static void free_fd_info MAGIC (HIDDEN_PROTO
				struct indexical_info **infop)
{
  struct indexical_info *ixinfo;

  /* [PM] 3.9b4 note that *infop may be NULL (except when called from free_fd_info_hook)*/

  while ((ixinfo = (*infop)))
    { 
      if (ixinfo->linkage)
	sp_checkdealloc((TAGGED *)(ixinfo->linkage),
			ixinfo->length_of_linkage*sizeof(ix_byte)
			/*MM_MISC*/);
      if (ixinfo->code)
	sp_checkdealloc((TAGGED *)(ixinfo->code),
			ixinfo->length_of_bytecode*sizeof(ix_byte)
			/*MM_MISC*/);
      if (ixinfo->literals)
	{
	  int i, len = ixinfo->length_of_literals;
	  TAGGED *p = ixinfo->literals;
	  struct sw_on_key *htab;
	  TAGGED *q;

	  for (i=0; i<len; i++)
	    {
	      if (p[i]&1)	/* hash table */
		{
		  htab = (struct sw_on_key *)TermToPointer(p[i]-1);
		  sp_checkdealloc((TAGGED *)htab,
				  sizeof(struct sw_on_key)+
				  (SwitchSize(htab)-ANY)*sizeof(struct sw_on_key_node)
				  /*MM_MISC*/);
		}
	      else if (TagIsSTR(p[i])) { /* bignum */
		q = TagToSTR(p[i]);
		sp_checkdealloc(q, LargeArity(q[0])*sizeof(TAGGED) /*MM_MISC*/);
	      }
	    }
	  sp_checkdealloc(p, len*sizeof(TAGGED) /*MM_MISC*/);
	}
      (*infop) = ixinfo->next;
      sp_checkdealloc((TAGGED *)ixinfo, sizeof(struct indexical_info) /*MM_MISC*/);
    }
}


void SPCDECL free_fd_info_hook(struct indexical_info **infop)
{
  /* [PM] note that this is a little special compared to other
     destructors, needing an extra indirection */
  FD_SETUP_SPENV((*infop)->spenv)
  free_fd_info(infop);          /* passes the HIDDEN_ARG SPEnv extracted from (the non-NULL!) *infop */
}


static void fd_install_indexicals MAGIC (HIDDEN_PROTO
					 TAGGED Info,
					 int type, int no_indexicals,
					 struct definition *f)
{
    TAGGED Indexical, tmp, tmp2, tmp3;
    int i,j,length;
    struct indexical_info *tmp_ix=NULL;
    struct indexical_info *Ix;
    ix_byte *code;
    TAGGED *literals;

    Ix = (struct indexical_info *)sp_checkalloc(sizeof(struct indexical_info) /*MM_MISC*/); 
    f->proc.code.fdinfo->info[type] = 
#if 0                           /* [PM] 3.9b4 now info[] is indexical_info* */
      (unsigned long *)
#endif
      Ix;
    #if 1                       /* [PM] 3.9b4 */
    Ix->destructor_fun = fd.fd_destructor_fun;
    #else
    Ix->destructor = fd.fd_destructor;
    #endif
    FD_STORE_SPENV(Ix->spenv);

    for (i=0; i<no_indexicals; i++)
      {				/* For each of the indexicals */
	if (i == 0)
	  tmp_ix = Ix;
	else
	  tmp_ix = tmp_ix->next = (struct indexical_info *)sp_checkalloc(sizeof(struct indexical_info) /*MM_MISC*/);
        #if 1                   /* [PM] 3.9b4 */
	tmp_ix->destructor_fun = fd.fd_destructor_fun;
        #else
	tmp_ix->destructor = fd.fd_destructor;
        #endif
        FD_STORE_SPENV(tmp_ix->spenv);
	tmp_ix->pred = f;
	tmp_ix->next = NULL;
	tmp_ix->checking = type>>1;
	tmp_ix->truth_value = type;
	DerefCar(Indexical, Info);
	DerefCdr(Info, Info);
	DerefArg(tmp, Indexical, 2); /* get length of fd linkage */
	tmp_ix->length_of_linkage = length = GetSmall(tmp);
	if (!length)		/* If the linkage list is empty */
	  tmp_ix->linkage = NULL;
	else
	  {
	    tmp_ix->linkage = (ix_byte *)sp_checkalloc(length*sizeof(ix_byte) /*MM_MISC*/);
	    DerefArg(tmp, Indexical, 1); /* tmp is now the linkage list */
	    for(j = 0;j<length;j++)
	      {
		DerefCar(tmp2, tmp);
		DerefArg(tmp3, tmp2, 1);
		DerefArg(tmp2, tmp2, 2);
		tmp_ix->linkage[j] = (GetSmall(tmp3)<<8) + GetSmall(tmp2);
		DerefCdr(tmp, tmp);
	      }
	  }
	DerefArg(tmp, Indexical, 3); /* get pruned */
	tmp_ix->pruned = GetSmall(tmp);

	DerefArg(tmp, Indexical, 7); /* get length of literals */
	tmp_ix->length_of_literals = length = GetSmall(tmp);
	if (!length)
	  tmp_ix->literals = NULL;
	else
	  {
	    tmp_ix->literals = literals = sp_checkalloc(length*sizeof(TAGGED) /*MM_MISC*/);
	    DerefArg(tmp, Indexical, 6);
	    while (TagIsLST(tmp))
	      {
		DerefCar(tmp2, tmp);
		DerefCdr(tmp, tmp);
		literals = fd_store_literal(literals, tmp2);
	      }
	  }

	DerefArg(tmp, Indexical, 5); /* get length of bytecode */
	tmp_ix->length_of_bytecode = length = GetSmall(tmp);
	if (!length)
	  tmp_ix->code = NULL;
	else
	  {
	    tmp_ix->code = code = (ix_byte *)sp_checkalloc(length*sizeof(ix_byte) /*MM_MISC*/);
	    DerefArg(tmp, Indexical, 4);
	    while (TagIsLST(tmp))
	      {
		DerefCar(tmp2, tmp);
		DerefCdr(tmp, tmp);
		code = fd_store_token(code, tmp2, tmp_ix->literals);
	      }
	  }

	tmp_ix->next = NULL;
    }
}


static void init_fd_constraint MAGIC (HIDDEN_PROTO
				      struct definition *f)
{
#if 1                           /* [PM] 3.9 (EG/engine_global is not available to SP_INSIDERs */
  char *s = SP_string_from_atom(f->proc.printname);
#else
  char *s = AtomIenc(f->proc.printname);
#endif
  int i;
  
  SP_install_c_predicate(s,f->proc.arity,f->module,0 /* uninstall */,NULL,NULL);
  f->proc.code.fdinfo = (struct fd_info *)sp_checkalloc(sizeof(struct fd_info) /*MM_MISC*/);
  for (i=0; i<4; i++)
    f->proc.code.fdinfo->info[i] = NULL;
  update_exports(f,FD_CONSTRAINT);
}


static void fd_install MAGIC (HIDDEN_PROTO
			      TAGGED head, TAGGED Module,
			      long Type, long InfoLength,
			      TAGGED Info)
{
  TAGGED *junk;
  struct mod_def *m = find_module(Module,TRUE);
  struct definition *f = find_definition(m,head,&junk,TRUE);
    
  if (f->proc.properties.prop.predtyp!=FD_CONSTRAINT)
    init_fd_constraint(f);
  else
    free_fd_info((struct indexical_info **)&f->proc.code.fdinfo->info[Type]);
  fd_install_indexicals(Info, Type, InfoLength, f);
}



/* $fd_install(+Name/+Arity, +Module, +Type, +InfoLength, +Info) */
void SPCDECL
prolog_fd_install MAGIC (HIDDEN_PROTO
			 SP_term_ref PredSpecR,
			 SP_atom module,
			 long type,
			 long length,
			 SP_term_ref InfoR)
{
  WAMENV;
  TAGGED name, arity, spec, info;

  spec = RefTerm(PredSpecR);
  info = RefTerm(InfoR);
  DerefNonvar(spec);
  DerefNonvar(info);
  DerefArg(name, spec, 1);
  DerefArg(arity, spec, 2);
  if (Tnez(arity)) {
    int i = GetSmall(arity);
    TAGGED *h = w->global_top;
	
    *h++ = SetArity(name,i);
    for (; i>0; --i)
      Load0HVA(h);
    name = MakeStructure(w->global_top);
  }
  fd_install(name, module, type, length, info);
}


/* $fd_get_indexicals(+DefPtr, +Type, +Goal, -Ent, -ZeroOne, +Attv, -Ixs) */
/* Ensure: Ent is globalized. */
void SPCDECL
prolog_fd_get_indexicals MAGIC (HIDDEN_PROTO
				long ptr,
				long type,
				SP_term_ref Goal,
				SP_term_ref Ent,
				SP_term_ref ZeroOne,
				SP_term_ref Attv,
				SP_term_ref Ixs)
{
  WAMENV;
  struct indexical_info *ix, **p, **q;
  struct indexical_info *array[512];
  struct definition *def = (struct definition *)TermToPointer(MakeSmall(ptr));
  TAGGED *h, t1, list;
  int l;

  (void)(Goal+Ent+ZeroOne+Attv);
/*    X(2) = RefTerm(Goal); */
/*    X(3) = RefTerm(Ent); */
/*    X(4) = RefTerm(ZeroOne); */
/*    X(5) = RefTerm(Attv); */
  DerefNonvar(X(2));
  DEREF(X(3),X(3));
  DEREF(X(4),X(4));
  DerefNonvar(X(5));		/* domain vector */
  if (type==1) fd.constraints++;
  ix = (struct indexical_info *)def->proc.code.fdinfo->info[type];
  p = q = array+256;
  while (ix)
    {
      DerefArg(t1, X(2), ix->pruned+1);
      if (IsVar(t1))
	*(--p) = ix;
      else
	*q++ = ix;
      ix = ix->next;
    }
  RequireHeap((q-p)*13+1, 7);
  h = w->global_top;
  if (TagIsSVA(X(3))) {
    LetShadowregs;
	    
    BindSVA(X(3),TagHVA(h));
    X(3) = h[0] = TagHVA(h);
    h++;
  }
  list = atom_nil;
  while (p<q)
    {
      l = (*(--q))->pruned;
      h[0] = fd.functor_mutable;
      h[1] = TaggedOne;
      h[2] = TaggedZero;
      h[3] = fd.functor_ix7;
      h[4] = PointerToTerm(*q); /* pointer */
      h[5] = X(2);		/* goal */
      h[6] = MakeStructure(h);	/* status mutable */
      h[7] = X(3);		/* entailment flag, globalized */
      h[8] = X(4);		/* zero-one var, globalized */
      h[9] = X(5);		/* Attv */
      h[10] = CTagToArg(X(5),l+1); /* LHS attribute */
      h[11] = MakeStructure(h+3);
      h[12] = list;
      list = MakeList(h+11);
      h += 13;
    }
  w->global_top = h;
  RefTerm(Ixs) = list;
}


static TAGGED fd_mon_vars(Argdecl,
			  struct indexical_info *current,
			  TAGGED goal)
{
  unsigned int mask = (current->checking ? 16 : 8);
  int length = current->length_of_linkage;
  TAGGED list = atom_nil;
  TAGGED *h = w->global_top;
  int i;
  ix_byte tmp_link;
  TAGGED t1;
  
  for (i=0; i<length; i++)
    {
      tmp_link = current->linkage[i];
      if (tmp_link & mask)
	{
	  DerefArg(t1, goal, (tmp_link>>8)+1);
	  if (IsVar(t1))
	    {
	      HeapPush(h,t1);
	      HeapPush(h,list);
	      list = MakeList(h-2);
	    }
	}
    }
  w->global_top = h;
  return list;
}




/* $fd_mon_vars(+Indexical, -Vars) */
void SPCDECL
prolog_fd_mon_vars MAGIC (HIDDEN_PROTO
			  SP_term_ref Indexical,
			  SP_term_ref Vars)
{
  WAMENV;
  struct indexical_info *current;
  int length;
  TAGGED t1, list, goal;

  (void)Indexical;
/*    X(0) = RefTerm(Indexical); */
  DerefNonvar(X(0));
  t1 = CTagToArg(X(0),1);
  current = (struct indexical_info *)TermToPointer(t1);
  length = current->length_of_linkage;
  RequireHeap(length<<1, 2);
  goal = CTagToArg(X(0),2);
  list = fd_mon_vars(w,current,goal);
  RefTerm(Vars) = list;
}


#define EVAL_ARITY 3

/* link in the indexical X(0) upon initial suspension */
/* called by $fd_evaluate_indexical only */
static void link_indexical MAGIC (HIDDEN_PROTO
				  Argdecl,
				  struct indexical_info *current)
{				/* link indexical into suspension lists */
  TAGGED t1, t2;
  unsigned int tmp_link;
  int length, i;
  ix_byte mask;
  TAGGED ix, *args;

  mask = (current->checking ? 16 : 8);
  length = current->length_of_linkage;
  RequireHeap(length*(256+38),EVAL_ARITY);	/* max. 256 modules? */
  ix = X(0);
  args = TagToArg(CTagToArg(ix,2),1);	/* refreshed after GC */
  for (i=0; i<length; i++)
    {
      tmp_link = current->linkage[i];
      if (!(tmp_link & mask))
	{
	  t1 = args[tmp_link>>8];
	  DerefSwitch(t1,t2,fd_link(w,t1,fd.linkage_keys[tmp_link & 7],ix););
	}
    }
}


void SPCDECL
prolog_fd_global_told MAGIC (HIDDEN_PROTO
			     SP_term_ref LinkageR,
			     SP_term_ref TermR)
{
  WAMENV;
  TAGGED statmut, status;
  TAGGED request, var;

  (void)(LinkageR+TermR);
/*    X(0) = RefTerm(LinkageR); */
/*    X(1) = RefTerm(TermR); */
  fd.resumptions++;
  DerefNonvar(X(1));
  statmut = CTagToArg(X(1),3);
  status = RefMutable(statmut);
  if (status!=MakeSmall(3))	/* enable self-resumption if coref */
    update_mutable(w,
		   (status & IStep(4)) ? MakeSmall(6) : MakeSmall(3),
		   statmut);
  if (!(status & IStep(2))) {	/* link me if it's the first time */
    fd.constraints++;
    DerefNonvar(X(0));
    while (TagIsLST(X(0))) {
      RequireHeap(256+38,2);	/* max. 256 modules? */
      DerefCar(request,X(0));
      DerefCdr(X(0),X(0));
      DerefArg(var,request,1);
      if (!IsVar(var))
	continue;
      fd_link(w,var,TagToHeadfunctor(request),X(1));
    }
  }
}


/*
link_iff([], _, _, _).
link_iff([Ix|Ixs], B, Key, A) :-
	'$fd_mon_vars'(Ix, Vars),
	'$fd_link'([val(B)], Vars-iff(Ix,B,Key,A)),
	link_iff(Ixs, B, Key, A).
*/
void SPCDECL
prolog_fd_link_iff MAGIC (HIDDEN_PROTO
			  SP_term_ref Ixs,
			  SP_term_ref B,
			  SP_term_ref Key,
			  SP_term_ref A)
{
  WAMENV;
  int i;

/*    X(0) = RefTerm(Ixs); */
/*    X(1) = RefTerm(B); */
/*    X(2) = RefTerm(Key); */
/*    X(3) = RefTerm(A); */
  (void)(Ixs+B+Key+A);
  for (i=0; i<4; i++)
    DEREF(X(i),X(i));

  while (TagIsLST(X(0))) {
    TAGGED *h, vars, t1, ix, goal;
    struct indexical_info *current;

    RequireHeap(512+8+10,4);
    DerefCar(ix,X(0));
    DerefCdr(X(0),X(0));
    t1 = CTagToArg(ix,1);	/* get indexical ptr */
    current = (struct indexical_info *)TermToPointer(t1);
    goal = CTagToArg(ix,2);	/* get constraint goal */
    vars = fd_mon_vars(w,current,goal);	/* at most 512 words */
    h = w->global_top;
    h[0] = functor_minus;
    h[1] = vars;
    h[2] = MakeStructure(h+3);
    h[3] = fd.functor_iff4;
    h[4] = ix;
    h[5] = X(1);
    h[6] = X(2);
    h[7] = X(3);
    w->global_top = h+8;
    fd_link(w,X(1),fd.functor_val,MakeStructure(h)); /* at most 10 words */
  }
}


/*
'$fd_enqueue_first(+Ixs).
*/
void SPCDECL
prolog_fd_enqueue_first MAGIC (HIDDEN_PROTO
			       SP_term_ref Ixs0)
{
  WAMENV;
  fd_sync(Arg);

/*    X(0) = RefTerm(Ixs0); */
  (void)Ixs0;
  DerefNonvar(X(0));
  while (TagIsLST(X(0))) {
    TAGGED *h, vars, t1, ix, goal;
    struct indexical_info *current;

    RequireHeap(512+3+10,1);
    DerefCar(ix,X(0));
    DerefCdr(X(0),X(0));
    t1 = CTagToArg(ix,1);	/* get indexical ptr */
    current = (struct indexical_info *)TermToPointer(t1);
    goal = CTagToArg(ix,2);	/* get constraint goal */
    vars = fd_mon_vars(w,current,goal);	/* at most 512 words */
    if (TagIsLST(vars)) {	/* suspend the indexical */
      h = w->global_top;
      h[0] = functor_minus;
      h[1] = CTagToCdr(vars);
      h[2] = ix;
      w->global_top = h+3;
      fd_link(w,CTagToCar(vars),fd.functor_val,MakeStructure(h)); /* at most 10 words */
    } else {			/* enqueue it now */
      fd_enqueue(ix, 0x1);
    }
  }
}


/* $fd_find_definition(+Constraint, +Module, -DefPtr) */
long SPCDECL
prolog_fd_find_definition MAGIC (HIDDEN_PROTO
				 SP_term_ref Constraint,
				 SP_atom module)
{
  WAMENV;
  struct definition *f;
  TAGGED *junk;
  
/*    X(0) = RefTerm(Constraint); */
  (void)Constraint;
  DerefNonvar(X(0));
  f = find_definition(find_module(module,FALSE),X(0),&junk,FALSE);
  if (f && f->proc.properties.prop.predtyp!=FD_CONSTRAINT)
    f = NULL;

  return GetSmall(PointerToTerm(f));
}


/* $fd_indexical_data(+Ptr, -Type, -Module) */
SP_atom SPCDECL
prolog_fd_indexical_data MAGIC (HIDDEN_PROTO
				long ptr, long *type)
{
  struct indexical_info *current =
    (struct indexical_info *)TermToPointer(MakeSmall(ptr));

  (void)HIDDEN_ARG_OR_ZERO;
  *type = (current->checking<<1) + current->truth_value;
  return current->pred->module->name;
}


typedef void (SPCDECL *FdFilterFun)();

static BOOL handle_global MAGIC (HIDDEN_PROTO Argdecl,
				 TAGGED constraint,
				 SP_term_ref State,
				 SP_term_ref Actions)
{
  TAGGED f = TagToHeadfunctor(constraint);
  struct sw_on_key_node *hnode = incore_gethash(fd.dispatch,f);

  if (!hnode->value.arities)
    return FALSE;
  X(0) = RefTerm(State);
  (*(FdFilterFun)hnode->value.arities)(HIDDEN_ARG_COMMA State, State, Actions);
  return TRUE;
}

/* '$fd_evaluate_indexical'(-RC, -Global)

Indexical = ix(+Ptr,+Goal,+StatusM,?Ent,-ZeroOne,+Attv,+LAttr)

X(1) holds status mutable
X(2) holds entailment variable.

Xref EVAL_ARITY
*/
long SPCDECL
prolog_fd_evaluate_indexical MAGIC (HIDDEN_PROTO
				    SP_term_ref Global)
{
  WAMENV;
  struct indexical_info *current;
  int why, ground;
  TAGGED gtemp, *atts, *args, pruned_attribute;
  TAGGED truth_value=TaggedOne, min2=0, max2=0;
  ix_byte *code;
  TAGGED *top=&X(EVAL_ARITY);
  TAGGED filter=0;	/* for use in fd_enqueue_all */
  TAGGED qval[128];
  TAGGED statem, constraint, statmut;
  SP_term_ref State = SP_new_term_ref();
  SP_term_ref Actions = SP_new_term_ref();
  DispatchDef;
  
  fd_sync(Arg);
  fd.fd_overflow = FALSE;
 restart:
  if (OffHeaptop(w->global_top,w->heap_warn_soft))
    return 1;
  switch (fd_dequeue(&X(0))) {
  case 0:
    fd_end(Arg);
    return 0;
  case 2:
    RefTerm(Global) = X(0);
    if (fd.debugging)
      return 2;
    DerefArg(statem,X(0),1);
    DerefArg(constraint,X(0),2);
    RefTerm(State) = RefMutable(statem);
    statmut = CTagToArg(X(0),3);
    if (RefMutable(statmut)==MakeSmall(7))	/* enable self-resumption if coref */
      update_mutable(w, MakeSmall(6), statmut);
    if (!handle_global(Arg,constraint,State,Actions))
      return 2;
    X(1) = RefTerm(Global);
    DerefArg(statem,X(1),1);
    update_mutable(w,RefTerm(State),statem);
    fd.resumptions++;
    X(0) = RefTerm(Actions);
    /* X(1) = RefTerm(Global); */
    if (!fd_prune_and_enqueue(Arg,Actions,Global,State/*recycled ref*/))
      goto fail;
    goto restart;
  }      
  why = 0;
  ground = 1;
  {
    TAGGED *ix_args = TagToArg(X(0),1);
    
    current = (struct indexical_info *)TermToPointer(ix_args[0]); /* Ptr */
    args = TagToArg(ix_args[1],1); /* Goal */
    X(1) = ix_args[2];		/* StatusM */
    DEREF(X(2),ix_args[3]);	/* Ent, always a HVA */
    atts = TagToArg(ix_args[5],1); /* Attv */
    pruned_attribute = ix_args[6];	/* LAttr */
  }
  fd.resumptions++;
  w->numstack_end = NULL;
  code = current->code;
  DispatchFirst {
    DispatchHead {
    CaseX(FD_DUP_RANGE):	/* int x int -> set */
      {
	TAGGED t1 = Top;
	Prefetch;
	
	Top = fd_interval(t1,t1);
	Dispatch;
      }

    CaseX(FD_RANGE_OO):		/* -> set */
      {
	Prefetch;
	
	Push(fd_interval(Inf, Sup));
	Dispatch;
      }

    CaseX(FD_RANGE_OC):		/* int -> set */
      {
	TAGGED t2 = Top;
	Prefetch;
	
	Top = (t2==Inf ? EmptySet : fd_interval(Inf, t2));
	Dispatch;
      }

    CaseX(FD_RANGE_CO):		/* int -> set */
      {
	TAGGED t1 = Top;
	Prefetch;
	
	Top = (t1==Sup ? EmptySet : fd_interval(t1, Sup));
	Dispatch;
      }

    CaseX(FD_RANGE_CC):		/* int x int -> set */
      {
	TAGGED t2 = Pop;
	TAGGED t1 = Top;
	Prefetch;
	
	if (t1==t2 && !TagIsSmall(t1)) {
	  fd.fd_overflow = TRUE;
	  goto check_and_ret;
	}
	Top = (EmptyInterval(t1,t2) ? EmptySet : fd_interval(t1, t2));
	Dispatch;
      }

    CaseX(FD_SETADD):		/* set x int -> set */
      {
	TAGGED t2 = Pop;
	TAGGED t1 = Top;
	Prefetch;
	
	Top = fd_lsh(t1,GetSmall(t2));
	Dispatch;
      }

    CaseX(FD_SETSUB):		/* set x int -> set */
      {
	TAGGED t2 = Pop;
	TAGGED t1 = Top;
	Prefetch;
	
	Top = fd_lsh(t1,-GetSmall(t2));
	Dispatch;
      }

    CaseX(FD_SETNEG):		/* set x int -> set */
      {
	TAGGED t2 = Pop;
	TAGGED t1 = Top;
	Prefetch;
	
	Top = fd_neg_offset(t1,t2);
	Dispatch;
      }

    CaseX(FD_SETMOD):		/* set x int -> set */
      {
	TAGGED t2 = Pop;
	TAGGED t1 = Top;
	Prefetch;
	
	if ((Top = fd_setmod(t1,t2))==ERRORTAG)
	  goto ret_abort;
	Dispatch;
      }

    CaseX(FD_SETPLUS):		/* set x set -> set */
      {
	TAGGED t2 = Pop;
	TAGGED t1 = Top;
	Prefetch;
	
	Top = fd_plus(t1,t2);
	Dispatch;
      }

    CaseX(FD_SETMINUS):		/* set x set -> set */
      {
	TAGGED t2 = Pop;
	TAGGED t1 = Top;
	Prefetch;
	
	Top = fd_minus(t1,t2);
	Dispatch;
      }

    CaseX(FD_COMPL_T):		/* int -> set */
      {
	TAGGED t1 = Top;
	Prefetch;
	
	Top = fd_compl_interval(t1,t1);
	Dispatch;
      }

    CaseX(FD_COMPL_D):		/* set -> set */
      {
	Prefetch;
	
	Top = fd_complement(Top);
	Dispatch;
      }

    CaseX(FD_UNION_TT):		/* int x int -> set */
      {
	TAGGED t1 = Pop;
	TAGGED t2 = Top;
	Prefetch;
	
	Top = fd_pair(t1,t2);
	Dispatch;
      }

    CaseX(FD_UNION_TD):		/* int x set -> set */
      {
	TAGGED t1 = Pop;
	TAGGED t2 = Top;
	Prefetch;
	
	Top = fd_insert_into(t2,t1);
	Dispatch;
      }
      
    CaseX(FD_UNION_DT):		/* set x int -> set */
      {
	TAGGED t2 = Pop;
	TAGGED t1 = Top;
	Prefetch;
	
	Top = fd_insert_into(t2,t1);
	Dispatch;
      }

    CaseX(FD_UNION_DD):		/* set x set -> set */
      {
	TAGGED t2 = Pop;
	TAGGED t1 = Top;
	Prefetch;
	
	Top = fd_or(t1,t2);
	Dispatch;
      }

    CaseX(FD_INTER_TT):		/* int x int -> set */
      {
	TAGGED t1 = Pop;
	TAGGED t2 = Top;
	Prefetch;
	
	if (t1!=t2)
	  Top = EmptySet;
	else
	  Top = fd_interval(t1,t1);
	Dispatch;
      }

    CaseX(FD_INTER_TD):		/* int x set -> set */
      {
	TAGGED t1 = Pop;
	TAGGED t2 = Top;
	Prefetch;
	
	if (fd_member(t2,t1))
	  Top = fd_interval(t2,t2);
	else
	  Top = EmptySet;
	Dispatch;
      }
      
    CaseX(FD_INTER_DT):		/* set x int -> set */
      {
	TAGGED t2 = Pop;
	TAGGED t1 = Top;
	Prefetch;
	
	if (fd_member(t2,t1))
	  Top = fd_interval(t2,t2);
	else
	  Top = EmptySet;
	Dispatch;
      }

    CaseX(FD_INTER_DD):		/* set x set -> set */
      {
	TAGGED t2 = Pop;
	TAGGED t1 = Top;
	Prefetch;
	
	Top = fd_and(t1,t2);
	Dispatch;
      }

    CaseX(FD_SUBTRACT_TT):		/* int x int -> set */
      {
	TAGGED t2 = Pop;
	TAGGED t1 = Top;
	Prefetch;
	
	if (t1==t2)
	  Top = EmptySet;
	else
	  Top = fd_interval(t1,t1);
	Dispatch;
      }

    CaseX(FD_SUBTRACT_TD):		/* int x set -> set */
      {
	TAGGED t2 = Pop;
	TAGGED t1 = Top;
	Prefetch;
	
	if (fd_member(t1,t2))
	  Top = EmptySet;
	else
	  Top = fd_interval(t1,t1);
	Dispatch;
      }
      
    CaseX(FD_SUBTRACT_DT):		/* set x int -> set */
      {
	TAGGED t2 = Pop;
	TAGGED t1 = Top;
	Prefetch;
	
	Top = fd_delete(t1,t2);
	Dispatch;
      }

    CaseX(FD_SUBTRACT_DD):		/* set x set -> set */
      {
	TAGGED t2 = Pop;
	TAGGED t1 = Top;
	Prefetch;
	
	Top = fd_subtract(t1,t2);
	Dispatch;
      }

    CaseX(FD_CHECK_UNION):	/* set -> set (sort of) */
      {				/* (D1 ? (inf..sup) \/ D2) */
	TAGGED t1 = Pop;
	ix_byte *altcode = current->code + (*code++);
	
	if (t1!=EmptySet)
	  {
	    Push(fd_interval(Inf, Sup));
	    code = altcode;
	  }
	{
	  Prefetch;
	  Dispatch;
	}
      }

    CaseX(FD_CHECK):		/* set -> set (sort of) */
      {
	TAGGED t1 = Pop;
	ix_byte *altcode = current->code + (*code++);
	
	if (t1==EmptySet)
	  {
	    Push(t1);
	    code = altcode;
	  }
	{
	  Prefetch;
	  Dispatch;
	}
      }

    CaseX(FD_ADD):		/* int x int -> int */
      {
	TAGGED t2 = Pop;
	TAGGED t1 = Top;
	Prefetch;
	
	if (TagIsSmall(t1))
	  {
	    if (TagIsSmall(t2))
	      {
		t1 += (t2-TaggedZero);
		if (!TagIsSmall(t1))
		  t1 = (Tltz(t2) ? Inf : Sup);
	      }
	    else
	      t1 = t2;
	  }
	else if (t1!=t2 && !TagIsSmall(t2))
	  goto ret_abort;
	Top = t1;
	Dispatch;
      }

    CaseX(FD_SUB):		/* int x int -> int */
      {
	TAGGED t2 = Pop;
	TAGGED t1 = Top;
	Prefetch;
	
	if (TagIsSmall(t1))
	  {
	    if (TagIsSmall(t2))
	      {
		t1 -= (t2-TaggedZero);
		if (!TagIsSmall(t1))
		  t1 = (Tltz(t2) ? Sup : Inf);
	      }
	    else
	      t1 = (t2==Inf ? Sup : Inf);
	  }
	else if (t1==t2)
	  goto ret_abort;
	Top = t1;
	Dispatch;
      }

    CaseX(FD_MULT_IMM):		/* int -> int */
      {
	TAGGED t2 = *code++;
	TAGGED t1 = Top;
	Prefetch;
	
	t1 = safe_mul(t1,t2);
	Top = t1;
	Dispatch;
      }

    CaseX(FD_DIVD_IMM):		/* int -> int */
      {				/* floor of quotient */
	TAGGED t2 = *code++;
	TAGGED t1 = Top;
	Prefetch;

	t1 = safe_divd(t1,t2);
	if (t1==ERRORTAG) goto ret_abort;
	Top = t1;
	Dispatch;
      }

    CaseX(FD_DIVU_IMM):		/* int -> int */
      {				/* ceiling of quotient */
	TAGGED t2 = *code++;
	TAGGED t1 = Top;
	Prefetch;
	
	t1 = safe_divu(t1,t2);
	if (t1==ERRORTAG) goto ret_abort;
	Top = t1;
	Dispatch;
      }

    CaseX(FD_MULT_VAL):		/* int -> int */
      {
	int i = *code++;
	TAGGED t2;
	TAGGED t1 = Top;
	Prefetch;
	
	t2 = args[i];
	DerefNonvar(t2);
	/* ensured by term expansion
	if (!PositiveBound(t2))
	  fprintf(stderr, "* FD: non-monotone multiply\n");
	*/
	t1 = safe_mul(t1,t2);
	Top = t1;
	Dispatch;
      }

    CaseX(FD_DIVD_VAL):		/* int -> int */
      {				/* floor of quotient */
	int i = *code++;
	TAGGED t2;
	TAGGED t1 = Top;
	Prefetch;

	t2 = args[i];
	DerefNonvar(t2);
	/* ensured by term expansion
	if (!PositiveBound(t2))
	  fprintf(stderr, "* FD: non-monotone divide\n");
	*/
	t1 = safe_divd(t1,t2);
	if (t1==ERRORTAG) goto ret_abort;
	Top = t1;
	Dispatch;
      }

    CaseX(FD_DIVU_VAL):		/* int -> int */
      {				/* ceiling of quotient */
	int i = *code++;
	TAGGED t2;
	TAGGED t1 = Top;
	Prefetch;
	
	t2 = args[i];
	DerefNonvar(t2);
	/* ensured by term expansion
	if (!PositiveBound(t2))
	  fprintf(stderr, "* FD: non-monotone divide\n");
	*/
	t1 = safe_divu(t1,t2);
	if (t1==ERRORTAG) goto ret_abort;
	Top = t1;
	Dispatch;
      }

    CaseX(FD_MULT_QVAL):		/* int -> int */
      {
	int i = *code++;
	TAGGED t1 = Top;
	Prefetch;
	
	t1 = safe_mul(t1,qval[i]);
	Top = t1;
	Dispatch;
      }

    CaseX(FD_DIVD_QVAL):		/* int -> int */
      {				/* floor of quotient */
	int i = *code++;
	TAGGED t1 = Top;
	Prefetch;

	t1 = safe_divd(t1,qval[i]);
	if (t1==ERRORTAG) goto ret_abort;
	Top = t1;
	Dispatch;
      }

    CaseX(FD_DIVU_QVAL):		/* int -> int */
      {				/* ceiling of quotient */
	int i = *code++;
	TAGGED t1 = Top;
	Prefetch;
	
	t1 = safe_divu(t1,qval[i]);
	if (t1==ERRORTAG) goto ret_abort;
	Top = t1;
	Dispatch;
      }

    CaseX(FD_MOD):		/* int x int -> int */
      {
	TAGGED t2 = Pop;
	TAGGED t1 = Top;
	Prefetch;
	
	if (Teqz(t2) || !TagIsSmall(t1))
	  goto ret_abort;	/* inf mod any OR any mod 0 -> give up */
	else if (TagIsSmall(t2))
	  t1 = ((WORD)(t1-TaggedZero)%(WORD)(t2-TaggedZero)+TaggedZero);
	/*else
	  t1 = t1;*/
	Top = t1;
	Dispatch;
      }

    CaseX(FD_QVAL):		/* -> int */
      {
	TAGGED t1 = *code++;
	{
	  Prefetch;
	  Push(qval[t1]);
	  Dispatch;
	}
      }

    CaseX(FD_VAL):		/* -> int */
       				/* the domain is a singleton, and the arg. is
				   almost always ground, but after 
				   var-var-unifications, the variable binding
				   may not have propagated yet */
      {
	int i = *code++;
	Prefetch;
	
	gtemp = args[i];
	DerefNonvar(gtemp);
	Push(gtemp);
	Dispatch;
      }
    
    CaseX(FD_VAL_0):		/* -> int */
      {
	Prefetch;
	
	gtemp = args[0];
	DerefNonvar(gtemp);
	Push(gtemp);
	Dispatch;
      }

    CaseX(FD_VAL_1):		/* -> int */
      {
	Prefetch;
	
	gtemp = args[1];
	DerefNonvar(gtemp);
	Push(gtemp);
	Dispatch;
      }

    CaseX(FD_VAL_2):		/* -> int */
      {
	Prefetch;
	
	gtemp = args[2];
	DerefNonvar(gtemp);
	Push(gtemp);
	Dispatch;
      }

    CaseX(FD_DOM):		/* -> set */
      {
	gtemp = atts[*code++];
      fd_dom:
	{
	  TAGGED *pt1;
	  Prefetch;

	  DerefAttribute(gtemp,gtemp);
	  pt1 = TagToSTR(gtemp);
	  if (pt1[2]!=pt1[3]) ground = 0;
	  Push(pt1[1]);
	  Dispatch;
	}
      }

    CaseX(FD_DOM_0):		/* -> set */
      gtemp = atts[0];
      goto fd_dom;

    CaseX(FD_DOM_1):		/* -> set */
      gtemp = atts[1];
      goto fd_dom;

    CaseX(FD_DOM_2):		/* -> set */
      gtemp = atts[2];
      goto fd_dom;

    CaseX(FD_GDOM_1):		/* -> set */
	{
	  TAGGED *pt1;
	  Prefetch;
	    
	  DerefAttribute(gtemp,atts[1]);
	  pt1 = TagToSTR(gtemp);
	  Push(pt1[1]);
	  Dispatch;
	}


    CaseX(FD_MIN):		/* -> int */
      {
	gtemp = atts[*code++];
      fd_min:
	{
	  TAGGED *pt1;
	  Prefetch;
	  
	  DerefAttribute(gtemp,gtemp);
	  pt1 = TagToSTR(gtemp);
	  if (pt1[2]!=pt1[3]) ground = 0;
	  Push(pt1[2]);
	  Dispatch;
	}
      }

    CaseX(FD_MIN_0):		/* -> int */
      gtemp = atts[0];
      goto fd_min;

    CaseX(FD_MIN_1):		/* -> int */
      gtemp = atts[1];
      goto fd_min;

    CaseX(FD_MIN_2):		/* -> int */
      gtemp = atts[2];
      goto fd_min;

    CaseX(FD_MAX):		/* -> int */
      {
	gtemp = atts[*code++];
      fd_max:
	{
	  TAGGED *pt1;
	  Prefetch;

	  DerefAttribute(gtemp,gtemp);
	  pt1 = TagToSTR(gtemp);
	  if (pt1[2]!=pt1[3]) ground = 0;
	  Push(pt1[3]);
	  Dispatch;
	}
      }

    CaseX(FD_MAX_0):		/* -> int */
      gtemp = atts[0];
      goto fd_max;

    CaseX(FD_MAX_1):		/* -> int */
      gtemp = atts[1];
      goto fd_max;

    CaseX(FD_MAX_2):		/* -> int */
      gtemp = atts[2];
      goto fd_max;

    CaseX(FD_CARD):		/* -> int */
      {
	gtemp = atts[*code++];
      fd_card:
	{
	  TAGGED *pt1;
	  Prefetch;
	  
	  DerefAttribute(gtemp,gtemp);
	  pt1 = TagToSTR(gtemp);
	  if (pt1[2]!=pt1[3]) ground = 0;
	  Push(pt1[4]);
	  Dispatch;
	}
      }

    CaseX(FD_CARD_0):		/* -> int */
      gtemp = atts[0];
      goto fd_card;

    CaseX(FD_CARD_1):		/* -> int */
      gtemp = atts[1];
      goto fd_card;

    CaseX(FD_CARD_2):		/* -> int */
      gtemp = atts[2];
      goto fd_card;

    CaseX(FD_CONST):		/* -> int or set */
      {
	Push(*code++);
	{
	  Prefetch;
	  Dispatch;
	}
      }

    CaseX(FD_UNIONOF):		/* set -> set x set */
      {				/* FD_UNIONOF LabelB */
	TAGGED t1 = Top;
	TAGGED t2;
	
	Top = EmptySet;
	code = current->code + (*code);
	if (t1!=EmptySet)
	  {
	    if (fd_infinite(t1))
	      goto ret_abort;
	    t2 = code[-2];	  
	    code = current->code + code[-1];
	    Push(t1);
	    fd_first_and_rest(t1,qval+t2,&top[-1]);
	  }
	{
	  Prefetch;
	  Dispatch;
	}
      }

    CaseX(FD_UNIONOF_NEXT):	/* set x set x set -> set x set */
      {				/* FD_UNIONOF_NEXT Qvar LabelB */
	TAGGED t2 = Pop;
	TAGGED t1 = Pop;	/* remaining bits */
	ix_byte *altcode;
      
	Top = fd_or(Top,t2);	/* update union so far */
	t2 = *code++;
	altcode = current->code + (*code++);
	if (t1!=EmptySet)
	  {
	    Push(t1);
	    fd_first_and_rest(t1,qval+t2,&top[-1]);
	    code = altcode;
	  }
	{
	  Prefetch;
	  Dispatch;
	}
      }
	  
	  
    CaseX(FD_SWITCH):		/* -> set or label */
      {
	struct sw_on_key *sw =	/* get hash table */
	  (struct sw_on_key *)(*code++);
	ix_byte *altcode = current->code + (*code++); /* get join label */
	TAGGED t2 = incore_gethash(sw,Top)->value.arities;
	if (!t2)
	  {
	    Top = EmptySet;	/* default value is empty set */
	    code = altcode;	/* branch to join */
	  }
	else
	  {
	    Top = (TAGGED)altcode; /* push join label */
	    code = current->code+GetSmall(t2); /* and branch to case */
	  }
	{
	  Prefetch;
	  Dispatch;
	}
      }

    CaseX(FD_POPJ):		/* label x set -> set */
      {
	TAGGED t2 = Pop;
	code = (ix_byte *)Top;
	Top = t2;
	{
	  Prefetch;
	  Dispatch;
	}
      }
    
    CaseX(FD_ERROR):
      goto error;
    
    CaseX(FD_PRUNE_RANGE_OO):	/* inf..sup (weird) */
      goto ret_nonempty;
      
    CaseX(FD_PRUNE_RANGE_OC):	/* int -> */
				/* inf..Max */
      max2 = Pop;
      min2 = Inf;
      goto prune_range;

    CaseX(FD_PRUNE_RANGE_CO):	/* int -> */
				/* Min..sup */
      max2 = Sup;
      min2 = Pop;
      goto prune_range;

    CaseX(FD_PRUNE_RANGE_CC):	/* int x int -> */
				/* Min..Max */
      max2 = Pop;		
      min2 = Pop; 
    prune_range:
      if (min2==max2 && !TagIsSmall(min2)) {
	fd.fd_overflow = TRUE;
	goto check_and_ret;
      }
      else if (EmptyInterval(min2,max2))
	goto fail;
      {
	TAGGED pruned_dom, *pt1;
	
	DerefAttribute(pruned_dom,atts[current->pruned]);
	pt1 = TagToSTR(pruned_dom);
	if (pt1[2]==pt1[3])
	  {
	    if (InInterval(pt1[2],min2,max2))
	      goto ret_nonempty;
	    else
	      goto fail;
	  }
	else
	  goto prune_interval;
      }
	  
    CaseX(FD_PRUNE_TERM_COMPL):	/* int -> */
				/* unify LHS with complement */
      gtemp = Pop;
    {
      TAGGED pruned_dom, *pt1;
      
      DerefAttribute(pruned_dom,atts[current->pruned]);
      pt1 = TagToSTR(pruned_dom);
      if (pt1[2]==pt1[3])
	{
	  if (gtemp!=pt1[2])
	    goto ret_nonempty;
	  else
	    goto fail;
	}
      else
	goto prune_value_compl;
    }

	  
    CaseX(FD_PRUNE_TERM):	/* int -> */
				/* unify LHS with value */
      gtemp = Pop;
    {
      TAGGED pruned_dom, *pt1;
      
      DerefAttribute(pruned_dom,atts[current->pruned]);
      pt1 = TagToSTR(pruned_dom);
      if (pt1[2]==pt1[3])
	{
	  if (gtemp==pt1[2])
	    goto ret_nonempty;
	  else
	    goto fail;
	}
      else
	goto prune_value;
    }

    CaseX(FD_PRUNE_COMPL):	/* set -> */
      gtemp = Pop;
      if (gtemp==EmptySet)
	goto ret_nonempty;
    {
      TAGGED pruned_dom, *pt1;
      
      DerefAttribute(pruned_dom,atts[current->pruned]);
      pt1 = TagToSTR(pruned_dom);
      if (pt1[2]==pt1[3])
	{
	  if (!fd_member(pt1[2],gtemp))
	    goto ret_nonempty;
	  else
	    goto fail;
	}
      else goto prune_compl;
    }

    CaseX(FD_PRUNE):		/* set -> */
      gtemp = Pop;
      if (gtemp==EmptySet)
	goto fail;

    {
      TAGGED pruned_dom, *pt1;
      
      DerefAttribute(pruned_dom,atts[current->pruned]);
      pt1 = TagToSTR(pruned_dom);
      if (pt1[2]==pt1[3])
	{
	  if (fd_member(pt1[2],gtemp))
	    goto ret_nonempty;
	  else
	    goto fail;
	}
    }

				/* pruned /\= gtemp */
    if (CTagToCdr(gtemp)==EmptySet)
      {
	gtemp = CTagToCar(gtemp);
	min2 = RangeMin(gtemp);
	max2 = RangeMax(gtemp);
	goto prune_interval;
      }
    why = prune_set(w,pruned_attribute,args[current->pruned],gtemp,EVAL_ARITY); /* GC */
    goto ret_pruned;

  prune_compl:			/* pruned /\= \gtemp */
    if (CTagToCdr(gtemp)==EmptySet)
      {
	gtemp = CTagToCar(gtemp);
	min2 = RangeMin(gtemp);
	max2 = RangeMax(gtemp);
	goto prune_interval_compl;
      }
    why = prune_set_compl(w,pruned_attribute,args[current->pruned],gtemp,EVAL_ARITY); /* GC */
    goto ret_pruned;

  prune_interval:		/* pruned /\= min2..max2 */
    if (min2==max2)
      goto prune_min2;
    why = prune_interval(w,pruned_attribute,args[current->pruned],min2,max2,EVAL_ARITY); /* GC */
    goto ret_pruned_interval;

  prune_interval_compl:
    if (min2==max2)
      goto prune_min2_compl;
    why = prune_interval_compl(w,pruned_attribute,args[current->pruned],min2,max2,EVAL_ARITY); /* GC */
    goto ret_pruned;

  prune_min2:
    gtemp = min2;

  prune_value:			/* pruned /\= gtemp..gtemp */
    why = prune_value(w,pruned_attribute,args[current->pruned],gtemp,EVAL_ARITY); /* GC */
    goto ret_pruned;

  prune_min2_compl:
    gtemp = min2;

  prune_value_compl:			/* pruned /\= \(gtemp..gtemp) */
    why = prune_value_compl(w,pruned_attribute,args[current->pruned],gtemp,EVAL_ARITY); /* GC */
    goto ret_pruned;

	  
    CaseX(FD_TEST_RANGE_OO):	/* inf..sup (weird) */
      truth_value = MakeSmall(current->truth_value);
      goto ret_entailed;
      
    CaseX(FD_TEST_RANGE_OC):	/* int -> */
				/* inf..Max */
      max2 = Pop;
      min2 = Inf;
      goto test_range;

    CaseX(FD_TEST_RANGE_CO):	/* int -> */
				/* Min..sup */
      max2 = Sup;
      min2 = Pop;
      goto test_range;

    CaseX(FD_TEST_RANGE_CC):	/* int x int -> */
				/* Min..Max */
      max2 = Pop;		
      min2 = Pop; 
    test_range:
      truth_value = MakeSmall(current->truth_value);
      if (min2==max2)
	{
	  if (!TagIsSmall(min2)) {
	    /* bug in 3.8.3
	    fd.fd_overflow = TRUE;
	    goto check_and_ret;
	    */
	    goto ret_disentailed;
	  }
	  gtemp = min2;
	  goto test_term;
	}
      else if (EmptyInterval(min2,max2))
	goto ret_disentailed;
    {
      TAGGED pruned_dom, dmin, dmax;
      
      DerefAttribute(pruned_dom,atts[current->pruned]);
      dmin = DomainMin(pruned_dom);
      dmax = DomainMax(pruned_dom);
      if (dmin==dmax)
	{
	  if (InInterval(dmin,min2,max2))
	    goto ret_entailed;
	  else
	    goto ret_disentailed;
	}
      else
	{			/* need precise test here! */
	  switch (fd_compare_interval(DomainSet(pruned_dom),min2,max2))
	    {
	    case FDI_SUBSET:
	    case FDI_EQUAL:
	      goto ret_entailed;
	    case FDI_INTERSECT:
	    case FDI_SUPERSET:
	      goto ret_zero;
	    default: /* disjoint */
	      goto ret_disentailed;
	    }
	}
    }
	  
    CaseX(FD_TEST_TERM_COMPL):	/* int -> */
      truth_value = MakeSmall(current->truth_value)^IStep(1);
      gtemp = Pop;
      goto test_term;

    CaseX(FD_TEST_TERM):	/* int -> */
      truth_value = MakeSmall(current->truth_value);
      gtemp = Pop; 
    test_term:
    {
      TAGGED pruned_dom, *pt1;
      
      DerefAttribute(pruned_dom,atts[current->pruned]);
      pt1 = TagToSTR(pruned_dom);
      if (pt1[2]==pt1[3])
	{
	  if (gtemp!=pt1[2])
	    goto ret_disentailed;
	  else
	    goto ret_entailed;
	}
      else
	{
	  if (!fd_member(gtemp,DomainSet(pruned_dom)))
	    goto ret_disentailed;
	  else
	    goto ret_zero;
	}
    }

    CaseX(FD_TEST_COMPL):	/* set -> */
      truth_value = MakeSmall(current->truth_value);
      gtemp = Pop;
      if (gtemp==EmptySet)
	goto ret_entailed;
    {
      TAGGED pruned_dom, dmin, dmax;
      
      DerefAttribute(pruned_dom,atts[current->pruned]);
      dmin = DomainMin(pruned_dom);
      dmax = DomainMax(pruned_dom);
      if (dmin==dmax)
	{
	  if (!fd_member(dmin,gtemp))
	    goto ret_entailed;
	  else
	    goto ret_disentailed;
	}
      else
	{
	  switch (fd_compare(DomainSet(pruned_dom),gtemp))
	    {
	    case FDI_DISJOINT:
	      goto ret_entailed;
	    case FDI_INTERSECT:
	    case FDI_SUPERSET:
	      goto ret_zero;
	    default: /* subset */
	      goto ret_disentailed;
	    }
	}
    }

    CaseX(FD_TEST):		/* set -> */
      truth_value = MakeSmall(current->truth_value);
      gtemp = Pop;
      if (gtemp==EmptySet)
	goto ret_disentailed;
    {
      TAGGED pruned_dom, *pt1;
      
      DerefAttribute(pruned_dom,atts[current->pruned]);
      pt1 = TagToSTR(pruned_dom);
      if (pt1[2]==pt1[3])
	{
	  if (fd_member(pt1[2],gtemp))
	    goto ret_entailed;
	  else
	    goto ret_disentailed;
	}
      else
	{
	  switch (fd_compare(DomainSet(pruned_dom),gtemp))
	    {
	    case FDI_SUBSET:
	    case FDI_EQUAL:
	      goto ret_entailed;
	    case FDI_INTERSECT:
	    case FDI_SUPERSET:
	      goto ret_zero;
	    default: /* disjoint */
	      goto ret_disentailed;
	    }
	}
    }
    }
  }

ret_disentailed:
  truth_value ^= IStep(1);
  if (ground)
    {
      TAGGED zero_one, t2;
      TAGGED pruned_dom;
      
ret_entailed:
      fd.entailments++;
      {
	LetShadowregs;
	BindHVA(X(2),truth_value);
      }
				/* propagate to the zero_one variable */
      zero_one = CTagToArg(X(0),5);
      DerefSwitch(zero_one,t2,goto ret_entailed_2;);
      if (0)
	{
      ret_entailed_2:
	  pruned_attribute = get_attributes(zero_one,fd.fd_module);/* GC impossible */
	  DerefAttribute(pruned_dom,pruned_attribute);
	  why = fd_tell_value(w,DomainSet(pruned_dom),truth_value,pruned_attribute);
	  why = finish_pruning(w,why,pruned_attribute,zero_one);
	}
      goto check_and_ret;
    }

ret_pruned_interval:
  if (why<0)
    goto fail;
  else if (why>0) {
    pruned_attribute = CTagToArg(X(0),7); /* refresh after GC */
    filter = atom_nil;
    if (why>0 && !ground && code[0]) { /* activate filter */
      TAGGED *pt1;
	
      DerefAttribute(gtemp,pruned_attribute);
      pt1 = TagToSTR(gtemp);
      if ((!(why&(MASK_MIN+MASK_MINMAX)) || pt1[2]==min2) &&
	  (!(why&(MASK_MAX+MASK_MINMAX)) || pt1[3]==max2))
	filter = X(2);
    }
  }
  goto ret_nonempty;

ret_abort:
  top=&X(EVAL_ARITY);		/* reset stack pointer */
  goto ret_zero;

ret_pruned:
  if (why<0)
    goto fail;
  else if (why>0) {
    pruned_attribute = CTagToArg(X(0),7); /* refresh after GC */
    filter = atom_nil;
    if (why>0 && !ground && code[0]) /* activate filter */
      filter = X(2);
  }
ret_nonempty:
  if (ground) {
    LetShadowregs;
    fd.entailments++;
    BindHVA(X(2),truth_value);
    goto check_and_ret;
  }
ret_zero:
  gtemp = RefMutable(X(1));
  update_mutable(w,MakeSmall(2),X(1)); /* status mutable */
  if (gtemp < MakeSmall(2))
    link_indexical(w,current);
check_and_ret:
  if (!fd_check_overflow())
    goto error;
  if (why!=0) {
    AttrToSuspM(pruned_attribute,pruned_attribute);
    fd_enqueue_all(w,why,filter,pruned_attribute);
  }
  goto restart;
fail:
  fd.failures++;
error:
  SP_fail();
  return -1;
}


/* '$fd_in_interval'(+Var, +Min, +Max, +Init) */
/* X(1) temporarily holds Var's FD att */
/* implies $fd_begin if Init=1 */
void SPCDECL
prolog_fd_in_interval MAGIC (HIDDEN_PROTO
			     SP_term_ref Var,
			     SP_term_ref Min,
			     SP_term_ref Max,
			     long init)
{
  WAMENV;
  int why;
  TAGGED var, min, max, t1;

  if (init)
    SP_MANGLE(prolog_fd_begin)(HIDDEN_ARG);
  var = RefTerm(Var);
  min = RefTerm(Min);
  max = RefTerm(Max);
  DerefNonvar(min);
  DerefNonvar(max);
  DerefSwitch(var,t1,goto prune;);
  if (!InInterval(var,min,max))
    goto fail;
  else
    return;
prune:
  fd_sync(Arg);
  X(1) = check_argument(w,var,Inf,Sup,Sup);
  w->numstack_end = NULL;
  if (min==max)
    why = prune_value(w,X(1),var,min,2);
  else
    why = prune_interval(w,X(1),var,min,max,2);
  if (why < 0)
    goto fail;
  else if (why > 0) {
    TAGGED lists_loc;

    AttrToSuspM(X(1),lists_loc);    
    fd_enqueue_all(w,why,atom_nil,lists_loc);
  }
  return;

fail:
  fd.failures++;
  SP_fail();
}



/* '$fd_in_set'(+Var, +Domain, +Init) */
/* X(1) temporarily holds Var's FD att */
/* implies $fd_begin if Init=1 */
void SPCDECL
prolog_fd_in_set MAGIC (HIDDEN_PROTO
			SP_term_ref Var,
			SP_term_ref Domain,
			long init)
{
  WAMENV;
  int why=0;
  TAGGED var, domain, t1;
  
  var = RefTerm(Var);
  domain = RefTerm(Domain);
  DerefNonvar(domain);
  if (domain==EmptySet)
    goto fail;
  if (init)
    SP_MANGLE(prolog_fd_begin)(HIDDEN_ARG);
  DerefSwitch(var,t1,goto prune;);
  if (!TagIsSmall(var) || !fd_member(var,domain))
    goto fail;
  else if (0)
    {
prune:
      fd_sync(Arg);
      X(1) = check_argument(w,var,Inf,Sup,Sup);
      w->numstack_end = NULL;
      if (CTagToCdr(domain)==EmptySet)
	{
	  TAGGED range = CTagToCar(domain);
	  TAGGED min = RangeMin(range);
	  TAGGED max = RangeMax(range);

	  if (min==max)
	    why = prune_value(w,X(1),var,min,2);
	  else
	    why = prune_interval(w,X(1),var,min,max,2);
	}
      else
	why = prune_set(w,X(1),var,domain,2);
    }
  if (why < 0)
    goto fail;
  else if (why > 0)
    {
      TAGGED lists_loc;

      AttrToSuspM(X(1),lists_loc);      
      fd_enqueue_all(w,why,atom_nil,lists_loc);
    }
  return;

fail:
  fd.failures++;
  SP_fail();
}



/*** support for save/restore ***/

static int find_fdset(TAGGED fdset, TAGGED *lits)
{
  int i=0;
  TAGGED key = MakeStructure(TagToLST(fdset)-1);

  while (lits[i]!=key)
    i++;

  return i;
}


static int find_htab(struct sw_on_key *htab,
		     TAGGED *lits)
{
  int i=0;
  TAGGED key = PointerToTerm(htab)+1;

  while (lits[i]!=key)
    i++;

  return i;
}


/* Relocate FDset refs and hashcode refs in the bytecode */
static void relocate_bytecode(ix_byte *code,
			      int n,
			      TAGGED *lits,
			      BOOL mkrel) /* TRUE if make relative */
{
  ix_byte *code_end = code+n;

  code++;			/* flag for filter etc. */
  while (code<code_end)
    switch (*code++)
      {
      case FD_QVAL:
      case FD_MULT_IMM:
      case FD_DIVD_IMM:
      case FD_DIVU_IMM:
      case FD_MOD:
      case FD_VAL:
      case FD_DOM:
      case FD_MIN:
      case FD_MAX:
      case FD_MULT_VAL:
      case FD_DIVD_VAL:
      case FD_DIVU_VAL:
      case FD_CARD:
      case FD_CHECK_UNION:
      case FD_CHECK:
      case FD_UNIONOF:
      case FD_MULT_QVAL:
      case FD_DIVD_QVAL:
      case FD_DIVU_QVAL:
	code++;
	break;

      case FD_CONST:
	if (!IsAtomic(code[0]))
	  {
	    if (mkrel)
	      code[0] = find_fdset(code[0],lits);
	    else
	      code[0] = MakeList(TagToArg(lits[code[0]],1));
	  }	  
	code++;
	break;
	
      case FD_UNIONOF_NEXT:
	code+=2;
	break;

      case FD_SWITCH:
	if (mkrel)
	  code[0] = find_htab((struct sw_on_key *)code[0],lits);
	else
	  code[0] = (ix_byte)TermToPointer(lits[code[0]]-1);
	code+=2;
	break;
      }
}



static void
fd_save_literals(TAGGED *lits,
		 int n,
		 fwrite_fun *fwr, /* fwrite passed from the emulator */
		 FILE *f)
{
  int i;
  int saved = 0;

  /* First, save the skeletal structure. */
  for (i=0; i<n; i++)
    {
      TAGGED lit = lits[i];
      
      if (IsAtomic(lit) && !(lit & 1))
	continue;
      if (saved<i)
	(*fwr)(lits+saved, (i-saved)*sizeof(TAGGED), 1, f);
      if (lit & 1)		/* a hash table */
	{
	  struct sw_on_key *htab = (struct sw_on_key *)TermToPointer(lit-1);
	  TAGGED aux = sizeof(struct sw_on_key)+
	    (SwitchSize(htab)-ANY)*sizeof(struct sw_on_key_node) + 1;

	  (*fwr)(&aux, sizeof(TAGGED), 1, f);
	}
      else			/* an FD set */
	{
	  TAGGED aux = LargeArity(TagToHeadfunctor(lit))*sizeof(TAGGED) + 2;
	  
	  (*fwr)(&aux, sizeof(TAGGED), 1, f);
	}
      saved = i+1;
    }
  if (saved<i)
    (*fwr)(lits+saved, (i-saved)*sizeof(TAGGED), 1, f);
  
  /* Then, save the residues. */
  for (i=0; i<n; i++)
    {
      TAGGED lit = lits[i];
      
      if (lit & 1)		/* a hash table */
	{
	  struct sw_on_key *htab = (struct sw_on_key *)TermToPointer(lit-1);

	  (*fwr)(htab,
		 sizeof(struct sw_on_key)+
		 (SwitchSize(htab)-ANY)*sizeof(struct sw_on_key_node), 1, f);
	}
      else if (TagIsSTR(lit))	/* an FD set */
	(*fwr)(TagToSTR(lit),
	       LargeArity(TagToHeadfunctor(lit))*sizeof(TAGGED), 1, f);
    }
}


void SPCDECL fd_save_hook(
     SP_HOOKS_COOKIE_PROTO,
     struct saverest_record *record,
     struct definition *pred,
     fwrite_fun *fwr,		/* fwrite passed from the emulator */
     FILE *f
  )
{
  int i;
  struct fd_info *fdinfo = pred->proc.code.fdinfo;

  SP_HOOKS_COOKIE_USE;
  for (i=0; i<4; i++)
    {
      struct indexical_info *info = (struct indexical_info *)fdinfo->info[i];
      
      while (info)
	{
	  record->body.fd_data.pred = pred;
	  record->body.fd_data.info =
	    (info->pruned<<10) + (info->length_of_linkage<<2) +
	    (info->checking<<1) + (info->truth_value);
	  record->body.fd_data.length_of_bytecode = info->length_of_bytecode;
	  record->body.fd_data.length_of_literals = info->length_of_literals;
	  (*fwr)(record, sizeof(*record), 1, f);
	  if (info->linkage)
	    (*fwr)(info->linkage,
		   info->length_of_linkage*sizeof(ix_byte),
		   1, f);
	  if (info->literals)
	    fd_save_literals(info->literals,
			     info->length_of_literals,
			     fwr, f);
	  relocate_bytecode(info->code, info->length_of_bytecode, info->literals, TRUE);
	  (*fwr)(info->code, info->length_of_bytecode*sizeof(ix_byte), 1, f);
	  relocate_bytecode(info->code, info->length_of_bytecode, info->literals, FALSE);
	  info = info->next;
	}
    }
}


void SPCDECL fd_restore_hook(
     SP_HOOKS_COOKIE_PROTO,     /* [PM] special, always passed by call_fd_restorer */
     struct saverest_record *record,
     fread_longs_fun *frd,		/* fread_longs passed from the emulator */
     RESTORE_FILE *f
  )
{
  int i, type;
  struct definition *pred = record->body.fd_data.pred; /* assume relocated */
  struct indexical_info **tail;
  struct indexical_info *info = 
    (struct indexical_info *)sp_checkalloc(sizeof(struct indexical_info) /* [PM] 3.9 MM_MISC */);

  SP_HOOKS_COOKIE_USE;
  if (pred->proc.properties.prop.predtyp!=FD_CONSTRAINT)
    init_fd_constraint(pred);

  #if 1                       /* [PM] 3.9b4 */
  info->destructor_fun = fd.fd_destructor_fun;
  #else
  info->destructor = fd.fd_destructor;
  #endif
  FD_STORE_SPENV(info->spenv);

  info->pred = pred;
  info->next = NULL;
  info->pruned = record->body.fd_data.info>>10;
  info->checking = record->body.fd_data.info>>1;
  info->truth_value = record->body.fd_data.info;
  type = record->body.fd_data.info&3;
  tail = (struct indexical_info **)&pred->proc.code.fdinfo->info[type]; 
  while (*tail) tail = &(*tail)->next;
  *tail = info;
  info->length_of_linkage = (record->body.fd_data.info>>2) & 0xff;
  info->length_of_bytecode = record->body.fd_data.length_of_bytecode;
  info->length_of_literals = record->body.fd_data.length_of_literals;
  if (info->length_of_linkage)
    {
      int size = info->length_of_linkage*sizeof(ix_byte);
      
      info->linkage = (ix_byte *)sp_checkalloc(size /* [PM] 3.9 MM_MISC */);
      (*frd)(info->linkage, size, 1, f);
    }
  else
    info->linkage = NULL;
  if (info->length_of_literals)
    {
      int size = info->length_of_literals*sizeof(TAGGED);
      
      info->literals = (TAGGED *)sp_checkalloc(size /* [PM] 3.9 MM_MISC */);
      (*frd)(info->literals, size, 1, f);
      /* Restore the residues. */
      for (i=0; i<info->length_of_literals; i++)
	{
	  TAGGED lit = info->literals[i];
	  
	  if (lit & 1)		/* a hash table */
	    {
	      struct sw_on_key *htab =
		(struct sw_on_key *)sp_checkalloc(lit-1 /* [PM] 3.9 MM_MISC */);
	      
	      (*frd)(htab, lit-1, 1, f);
	      info->literals[i] = PointerToTerm(htab)+1;
	    }
	  else if (lit & 2)	/* an FD set */
	    {
	      TAGGED *fdset = sp_checkalloc(lit-2 /* [PM] 3.9 MM_MISC */);
	      int j;
	      int size = lit>>LogSizeOfWord;
	      
	      (*frd)(fdset, lit-2, 1, f);
	      info->literals[i] = MakeStructure(fdset);
	      for (j=1; j<size; j+=4) /* relocate the ranges */
		fdset[j]   = MakeList(fdset+j+2);
	      for (j=1; j<size-4; j+=4)	/* relocate the tails */
		fdset[j+1] = MakeList(fdset+j+4);
	    }
	}
    }
  else
    info->literals = NULL;
  {
    int size = info->length_of_bytecode*sizeof(ix_byte);
    
    info->code = (TAGGED *)sp_checkalloc(size /* [PM] 3.9 MM_MISC */);
    (*frd)(info->code, size, 1, f);
    relocate_bytecode(info->code, info->length_of_bytecode, info->literals,
		      FALSE);
  }
}

