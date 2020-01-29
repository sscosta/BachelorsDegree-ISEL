/* Copyright(C) 1999, Swedish Institute of Computer Science */

#include "fd.h"

#if MULTI_SP_AWARE
#define fd_enqueue_list_gc(A1,A2,A3,A4) fd_enqueue_list_gc(HIDDEN_ARG, A1,A2,A3,A4)
#define fd_enqueue_list(A1,A2,A3,A4) fd_enqueue_list(HIDDEN_ARG, A1,A2,A3,A4)
#define fd_enqueue_val(A1,A2,A3,A4) fd_enqueue_val(HIDDEN_ARG, A1,A2,A3,A4)
#define compress_trail(A1,A2) compress_trail(HIDDEN_ARG, A1,A2)
#endif /* MULTI_SP_AWARE */


/* Returns FD attribute for a dvar or integer.
   Heap consumption bounds:
   existing dvar - 0
   integer - INT_ATTRIBUTE_SIZE
   non-domain variable - FD_ATTRIBUTE_SIZE + ARITYLIMIT + 4
*/
TAGGED check_argument MAGIC (HIDDEN_PROTO
			     Argdecl,
			     TAGGED argument,
			     TAGGED min, TAGGED max, TAGGED size)
{
  TAGGED *h, t1, attr;
  int j;

  DerefSwitch(argument,t1,;);
  switch (TagOf(argument))
    {
    case TAG_CVA:
      if ((t1=get_attributes(argument,fd.fd_module)))
	return t1;
    case TAG_SVA:
    case TAG_HVA:
      h = w->global_top;
      attr = MakeStructure(h);
      for (j=0; j<FD_ATTRIBUTE_SIZE; j++)
	{
	  t1 = fd_attribute[j];
	  if (t1&1)
	    t1 += TagHVA(w->global_top)-1;
	  *h++ = t1;
	}
      w->global_top[FD_ATTR_MIN_OFFSET] = min;
      w->global_top[FD_ATTR_MAX_OFFSET] = max;
      w->global_top[FD_ATTR_SIZE_OFFSET] = size;
      w->global_top = h;
      put_attributes(argument,attr,fd.fd_module);
      return attr;
    case TAG_SIN:
      h = w->global_top;
      attr = MakeStructure(h);
      *h++ = fd_attribute[0];
      *h++ = fd_attribute[1];
      *h++ = fd_attribute[2];
      *h++ = attr + WD(5);
      *h++ = TaggedZero;
/* 5*/*h++ = fd.functor_mutable;
      *h++ = attr + WD(8);
      *h++ = TaggedZero;
/* 8*/*h++ = fd.functor_dom4;
      *h++ = MakeList(w->global_top+13);
/*10*/*h++ = argument;
      *h++ = argument;
      *h++ = TaggedOne;
/*13*/*h++ = MakeList(w->global_top+10);
      *h++ = atom_nil;
      w->global_top = h;
      return attr;
    default:
      return ERRORTAG;
    }
}


/* '$fd_arg_attribute'(+Var, +Finitep, -Attr)
*/
void SPCDECL
prolog_fd_arg_attribute MAGIC (HIDDEN_PROTO
			       SP_term_ref Var,
			       long finitep,
			       SP_term_ref Attr)
{
  WAMENV;
  TAGGED attr, domain;

  attr = check_argument(w,RefTerm(Var),Inf,Sup,Sup);
  if (attr && finitep) {
    DerefAttribute(domain,attr); /* dom/4 term */
    if (!TagIsSmall(DomainSize(domain)))
      attr = ERRORTAG;
  }
  if (attr)
    RefTerm(Attr) = attr;
  else
    SP_fail();  
}

/* '$fd_dvar_list'(+List, +Finitep)
*/
void SPCDECL
prolog_fd_dvar_list MAGIC (HIDDEN_PROTO
			   SP_term_ref List,
			   long finitep)
{
  WAMENV;
  TAGGED list, attr, domain;
  int i = 0;

  DEREF(list,RefTerm(List));
  while (TagIsLST(list)) {
    DerefCar(attr,list);
    DerefCdr(list,list);
    if (IsVar(attr))
      i++;
  }
  if (list!=atom_nil)		/* type error? */
    goto fail;
  if (i>0 && !finitep) {
    RequireHeap(i*(FD_ATTRIBUTE_SIZE + ARITYLIMIT + 4),0);
  }
  DEREF(list,RefTerm(List));
  while (TagIsLST(list)) {
    DerefCar(attr,list);
    DerefCdr(list,list);
    if (!TagIsSmall(attr)) {
      attr = check_argument(w,attr,Inf,Sup,Sup);
      if (!attr)
	goto fail;
      if (finitep) {
	DerefAttribute(domain,attr); /* dom/4 term */
	if (!TagIsSmall(DomainSize(domain)))
	  goto fail;
      }
    }
  }
  return;
 fail:
  SP_fail();
}

/* '$fd_coref'(+List)
   Succeeds if List contains F1(X) and F2(X) for some X.
*/
void SPCDECL
prolog_fd_coref MAGIC (HIDDEN_PROTO SP_term_ref List)
{
  WAMENV;
  TAGGED list, var, t1;

  DEREF(list,RefTerm(List));
  while (TagIsLST(list)) {
    DerefCar(var,list);
    DerefCdr(list,list);
    var = CTagToArg(var,1);
    DerefHeapSwitch(var,t1,goto bind;);
    if (var==atom_nil)		/* F(Var), 2nd occurrence */
      return;
    else
      continue;			/* F(Integer) */
  bind:				/* F(Var), 1st occurrence */
    TrailPushCheck(var);
    CTagToPointer(var) = atom_nil;
  }
  SP_fail();			/* No coreference found. */
}


BOOL fd_member(TAGGED x, TAGGED set)
{
  TAGGED range;

  while (set!=EmptySet)
    {
      range = CTagToCar(set); 
      set = CTagToCdr(set);   
      switch (val_vs_range(x,range))
	{
	case CMP_INSIDE: return TRUE;
	case CMP_BEFORE: return FALSE;
	}
    }
  return FALSE;
}



BOOL fd_check_overflow MAGIC (HIDDEN_PROTO_VOID)
{
  if (fd.fd_overflow)
    {
      SP_term_ref excp = SP_new_term_ref();
      SP_term_ref a1 = SP_new_term_ref();
      SP_term_ref a2 = SP_new_term_ref();
      SP_term_ref a3 = SP_new_term_ref();

      SP_put_string(a1, "clpfd");
      SP_put_integer(a2, 0);
      SP_put_string(a3, "integer_overflow");
      (void)SP_cons_functor(excp,SP_atom_from_string("representation_error"),3,
			    a1,a2,a3);
      SP_raise_exception(excp);
      SP_reset_term_refs(excp);
      return FALSE;
    }
  return TRUE;
}


TAGGED *fd_put_range MAGIC (HIDDEN_PROTO
			    TAGGED *h, TAGGED b, TAGGED e)
{
  if (b==Sup || e==Inf)		/* precond: b/e are Inf, Sup or integers */
    fd.fd_overflow = TRUE;
  *h++ = b;
  *h++ = e;
  return h;
}



/* Build a copy of old on the heap.
   Precondition: old is built entirely on the numstack.
*/
TAGGED fd_globalize MAGIC (HIDDEN_PROTO
			   Argdecl,
			   TAGGED old, long req, int ar)
{
  TAGGED d1, r1, *h, value;
  TAGGED *valuep = &value;

  d1 = old;
  while (d1!=EmptySet)
      req += 4,
      d1 = CTagToCdr(d1);
  RequireHeap(req,ar);
  h = w->global_top;
  d1 = old;
  while (d1!=EmptySet)
    {
      r1 = CTagToCar(d1); d1 = CTagToCdr(d1);
      *valuep = MakeList(h);
      *h++ = *valuep+WD(2);
      valuep = h++;
      h = fd_put_range(h,RangeMin(r1),RangeMax(r1));
    }
  *valuep = atom_nil;
  w->global_top = h;
  return value;
}



void update_mutable MAGIC (HIDDEN_PROTO
			   struct worker *w,
			   TAGGED new_value, TAGGED mutable)
{
  TAGGED *h, *arg, value;
  LetShadowregs;
  
  value=RefMutable(mutable);
  while (TagIsSTR(value) && TagToHeadfunctor(value)==fd.functor_mutable) {
    mutable=value; value=RefMutable(value);
  }							
  arg = TagToArg(mutable,0);
  if (arg[2] < TrailToInt(w->node->trail_top) &&
      arg < GLOBAL_UNCOND)
    {
      h = w->trail_top; /* trail mutable if need be */
      *h++ = mutable;
      *h++ = arg[1];
      *h++ = arg[2];
      arg[1] = new_value;	/* must come BEFORE choice_overflow */
      arg[2] = TrailToInt(w->trail_top);
      w->trail_top = h;
      if (ChoiceYounger(ChoiceOffset(w->node,CHOICEPAD),w->trail_top))
	choice_overflow(CHOICEPAD);
    }
  else
    arg[1] = new_value;
}

void fd_link MAGIC (HIDDEN_PROTO
		    struct worker *w,
		    TAGGED var,
		    long key,
		    TAGGED item)
{
  TAGGED mutable;
  TAGGED *h;
  TAGGED *s=NULL;
  TAGGED queues;
  long mask;
  LetShadowregs;

  AttrToSuspM(check_argument(w,var,Inf,Sup,Sup),mutable);
  queues = RefMutable(mutable);
  h = w->global_top;
  
  if (TagToSTR(queues) < GLOBAL_UNCOND)/* can't smash */
    {
      int i;
	  
      for (s = TagToSTR(queues), i=0; i<8; i++)
	*h++ = *s++;
      queues = MakeStructure(h-8);
      update_mutable(w,queues,mutable);
    }
  if (key==fd.functor_dom)
    mask = IStep(MASK_DOM),
    s = TagToArg(queues,3);
  else if (key==fd.functor_min)
    mask = IStep(MASK_MIN),
    s = TagToArg(queues,4);
  else if (key==fd.functor_max)
    mask = IStep(MASK_MAX),
    s = TagToArg(queues,5);
  else if (key==fd.functor_minmax)
    mask = IStep(MASK_MINMAX),
    s = TagToArg(queues,6);
  else
    {
				/* patch item for globals suspending on val */
      if (TagToHeadfunctor(item)!=functor_minus)
	{
	  HeapPush(h,functor_minus);
	  HeapPush(h,atom_nil);
	  HeapPush(h,item);
	  item = MakeStructure(h-3);
	}
      mask = IStep(MASK_VAL),
      s = TagToArg(queues,7);
    }
  CTagToArg(queues,1) += IStep(1);
  CTagToArg(queues,2) |= mask;
  HeapPush(h,item);
  HeapPush(h,*s);
  *s = MakeList(h-2);
  w->global_top = h;
}


/*** support for queues of indexicals and globals ***/

#define CLPFD_MUTABLE RefTerm(5) /* xref Emulator/sicstus.c */

void fd_sync MAGIC (HIDDEN_PROTO Argdecl)
{
  TAGGED ptr = RefMutable(CLPFD_MUTABLE) & -4L; /* clear GC bits */
  struct propagator *current = (struct propagator *)TermToPointerOrNull(ptr);
  struct propagator *cur;

  while ((cur=fd.current_propagator)!=current) {

    cur->ixfirst = cur->ixlast = 0;
    cur->glfirst = cur->gllast = 0;
    fd.current_propagator = cur->next;
    cur->next = fd.free_propagators;
    fd.free_propagators = cur;
  }
}


/* '$fd_begin'
*/
void SPCDECL
prolog_fd_begin MAGIC (HIDDEN_PROTO_VOID)
{
  WAMENV;
  struct propagator *cur;
  
  fd_sync(Arg);
  if (fd.free_propagators) {
    cur = fd.free_propagators;
    fd.free_propagators = cur->next;
  } else {
    cur = (struct propagator *)sp_checkalloc(sizeof(struct propagator) /*MM_MISC*/);
    cur->ixfirst = 0;
    cur->ixlast = 0;
    cur->ixsize = 4;
    cur->glfirst = 0;
    cur->gllast = 0;
    cur->glsize = 4;
    cur->indexical = sp_checkalloc(4*sizeof(TAGGED) /*MM_MISC*/);
    cur->global = sp_checkalloc(4*sizeof(TAGGED) /*MM_MISC*/);
  }
  cur->next = fd.current_propagator;
  fd.current_propagator = cur;
  update_mutable(w,PointerToTerm(cur),CLPFD_MUTABLE);
}


/* '$fd_end'
*/
void fd_end MAGIC (HIDDEN_PROTO Argdecl)
{
  struct propagator *cur;

  fd_sync(Arg);
  cur = fd.current_propagator;
  fd.current_propagator = cur->next;
  cur->next = fd.free_propagators;
  fd.free_propagators = cur;
  update_mutable(w,PointerToTermOrZero(fd.current_propagator),CLPFD_MUTABLE);
}


void
fd_dealloc MAGIC (HIDDEN_PROTO_VOID)
{
  WAMENV;
  struct propagator *cur;
  
  RefMutable(CLPFD_MUTABLE) = TaggedZero;
  fd_sync(Arg);
  while ((cur=fd.free_propagators)) {
    sp_checkdealloc(cur->indexical, cur->ixsize*sizeof(TAGGED) /*MM_MISC*/);
    sp_checkdealloc(cur->global, cur->glsize*sizeof(TAGGED) /*MM_MISC*/);
    fd.free_propagators = cur->next;
    sp_checkdealloc((TAGGED *)cur, sizeof(struct propagator) /*MM_MISC*/);
  }
}  


/* support function for stack shifting and gc */
/* defines copied from heapgc.c */

#define GC_FIRSTMASK 0x1
#define GC_MARKMASK 0x2

#define gc_UnmarkM(x)  ((x)&=(~GC_MARKMASK))

#define gc_PutValueFirst(p,x) Deposit(p,POINTERMASK|GC_FIRSTMASK,x)

#define intoRelocationChain(j,curr) \
(*(curr) = gc_PutValueFirst(*(j),*(curr)), \
 *(j)    = gc_PutValueFirst(AddrToTag(curr)|GC_FIRSTMASK,*(j)))

void SPCDECL
fd_manager_hook(SP_HOOKS_COOKIE_PROTO,Argdecl,int msg,TAGGED *ptr)
{
  long reloc;
  struct propagator *cur;
  TAGGED t, *p;
  int i;
  
  SP_HOOKS_COOKIE_USE;
  switch (msg) {
  case 1:			/* stack shifter */
    reloc = (long)ptr;
    fd_sync(Arg);
    cur = fd.current_propagator;
    while (cur) {
      for (i=cur->ixfirst; i!=cur->ixlast; ) {
	cur->indexical[i] += reloc;
	i++;
	if (i==cur->ixsize)
	  i = 0;
      }
      for (i=cur->glfirst; i!=cur->gllast; ) {
	cur->global[i] += reloc;
	i++;
	if (i==cur->glsize)
	  i = 0;
      }
      cur = cur->next;
    }
    break;
  case 2:			/* gc, mark phase */
    fd_sync(Arg);
    cur = fd.current_propagator;
    while (cur) {
      for (i=cur->ixfirst; i!=cur->ixlast; ) {
	markVariable(&cur->indexical[i]);
	i++;
	if (i==cur->ixsize)
	  i = 0;
      }
      for (i=cur->glfirst; i!=cur->gllast; ) {
	markVariable(&cur->global[i]);
	i++;
	if (i==cur->glsize)
	  i = 0;
      }
      cur = cur->next;
    }
    break;
  case 3:			/* gc, sweep phase */
    cur = fd.current_propagator;
    while (cur) {
      for (i=cur->ixfirst; i!=cur->ixlast; ) {
	gc_UnmarkM(cur->indexical[i]);
	t = cur->indexical[i];
	p = TagToPointer(t);
	if (OffHeaptop(p,ptr))
	  intoRelocationChain(p,&cur->indexical[i]);
	i++;
	if (i==cur->ixsize)
	  i = 0;
      }
      for (i=cur->glfirst; i!=cur->gllast; ) {
	gc_UnmarkM(cur->global[i]);
	t = cur->global[i];
	p = TagToPointer(t);
	if (OffHeaptop(p,ptr))
	  intoRelocationChain(p,&cur->global[i]);
	i++;
	if (i==cur->glsize)
	  i = 0;
      }
      cur = cur->next;
    }
    break;
  }
}


/* where=0x0 -- prepend to indexical queue
         0x1 -- append to indexical queue
         0x2 -- prepend to global queue
         0x3 -- append to global queue
*/
void fd_enqueue MAGIC (HIDDEN_PROTO
		       TAGGED item, int where)
{
  struct propagator *cur = fd.current_propagator;
  int i, pop, size;
  
  if (!(where & 0x2)) {
    /* room for item? */
    pop = cur->ixlast-cur->ixfirst;
    size = cur->ixsize;
    if (pop<0)
      pop += size;
    if (pop+1 >= size) { /* grow */
      cur->indexical = sp_checkrealloc(cur->indexical,
				       size*sizeof(TAGGED),
				       size*sizeof(TAGGED)<<1
				       /*MM_MISC*/);
      cur->ixsize += size;
      if (cur->ixfirst>cur->ixlast) {
	for (i=size-1; i>=cur->ixfirst; i--)
	  cur->indexical[i+size] = cur->indexical[i];
	cur->ixfirst += size;
      }
      size += size;
    }
    if (where & 0x1) {
      cur->indexical[cur->ixlast++] = item;
      if (cur->ixlast==size)
	cur->ixlast = 0;
    } else {
      if (cur->ixfirst==0)
	cur->ixfirst = size;
      cur->indexical[--cur->ixfirst] = item;
    }
  } else {
    /* room for item? */
    pop = cur->gllast-cur->glfirst;
    size = cur->glsize;
    if (pop<0)
      pop += size;
    if (pop+1 >= size) { /* grow */
      cur->global = sp_checkrealloc(cur->global,
				    size*sizeof(TAGGED),
				    size*sizeof(TAGGED)<<1
				    /*MM_MISC*/);
      cur->glsize += size;
      if (cur->glfirst>cur->gllast) {
	for (i=size-1; i>=cur->glfirst; i--)
	  cur->global[i+size] = cur->global[i];
	cur->glfirst += size;
      }
      size += size;
    }
    if (where & 0x1) {
      cur->global[cur->gllast++] = item;
      if (cur->gllast==size)
	cur->gllast = 0;
    } else {
      if (cur->glfirst==0)
	cur->glfirst = size;
      cur->global[--cur->glfirst] = item;
    }
  }
}


/* 0 -- empty queue
   1 -- dequeued indexical
   2 -- dequeued global
*/
int fd_dequeue MAGIC (HIDDEN_PROTO TAGGED *item)
{
  struct propagator *cur = fd.current_propagator;
  TAGGED t1, t2;

  for (;;)
    if (cur->ixfirst != cur->ixlast) {
      *item = t1 = cur->indexical[cur->ixfirst++];
      if (cur->ixfirst==cur->ixsize)
	cur->ixfirst = 0;
      t1 = CTagToArg(t1,4);	/* Ent, always a HVA */
      DerefSwitch(t1,t2,return 1;); /* skip if (dis)entailed */
    } else if (cur->glfirst != cur->gllast) {
      *item = t1 = cur->global[cur->glfirst++];
      if (cur->glfirst==cur->glsize)
	cur->glfirst = 0;
      t1 = CTagToArg(t1,4);	/* Ent, always a HVA */
      DerefSwitch(t1,t2,return 2;); /* skip if (dis)entailed */
    } else
      return 0;
}



static void fd_enqueue_list_gc MAGIC (HIDDEN_PROTO
				      struct worker *w,
				      int index,
				      TAGGED filter, TAGGED lists_loc)
{
  TAGGED *lists = TagToArg(RefMutable(lists_loc),0);
  TAGGED list, new, item, status, prev=0, t1, *h;
  int gc=0, decr=0;
  LetShadowregs;

  list = lists[index];
  new = list;
  h = w->global_top;
  while (TagIsLST(list))
    {
      item = CTagToCar(list);
      DerefArg(t1,item,4);
      if (!IsVar(t1))		/* entailed */
	{
	  list = CTagToCdr(list);
	  if (prev==0)
	    decr++, new = list;
	  else if (TagToLST(prev) >= GLOBAL_UNCOND)
	    decr++, CTagToCdr(prev) = list;
	  else
	    decr++, gc++;
	}
      else
	{
	  prev = list;
	  list = CTagToCdr(list);
	}
      if (IsVar(t1) && t1!=filter)
	{
	  t1 = CTagToArg(item,3); /* status mutable */
	  status = RefMutable(t1);
	  if (!(status & IStep(1)))
	    {
	      update_mutable(w,status+IStep(1),t1);
	      if (TagToHeadfunctor(item)==fd.functor_ix7)
		fd_enqueue(item, 0x1);
	      else
		fd_enqueue(item, 0x3);
	    }
	}
    }
  if (decr>0)
    {
      TAGGED next, *nextp = &next;
      
      while (gc>0)
	{
	  item = CTagToCar(new);
	  new = CTagToCdr(new);
	  DerefArg(t1,item,4);
	  if (!IsVar(t1))		/* entailed */
	    --gc;
	  else
	    {
	      *nextp = MakeList(h);
	      HeapPush(h,item);
	      nextp = h++;
	    }
	}
      *nextp = new;
      /* next is the new list, decrement count by decr */
      if (lists < GLOBAL_UNCOND)/* can't smash */
	{
	  int i;
	  
	  for (i=0; i<8; i++)
	    *h++ = lists[i];
	  lists = h-8;
	  update_mutable(w,MakeStructure(lists),lists_loc);
	}
      lists[1] -= IStep(decr);
      lists[index] = next;
      if (next==atom_nil)
	{
	  index = GetSmall(lists[2]) & MASK_VAL;
	  if (TagIsLST(lists[3])) index |= MASK_DOM;
	  if (TagIsLST(lists[4])) index |= MASK_MIN;
	  if (TagIsLST(lists[5])) index |= MASK_MAX;
	  if (TagIsLST(lists[6])) index |= MASK_MINMAX;
	  lists[2] = MakeSmall(index);
	}
    }
  w->global_top = h;
}


static void fd_enqueue_list MAGIC (HIDDEN_PROTO
				   struct worker *w,
				   int index,
				   TAGGED filter, TAGGED lists)
{
  TAGGED list, item, status, t1;

  list = CTagToArg(lists,index);
  while (TagIsLST(list))
    {
      item = CTagToCar(list);
      list = CTagToCdr(list);
      DerefArg(t1,item,4);
      if (IsVar(t1) && t1!=filter)
	{
	  t1 = CTagToArg(item,3); /* status mutable */
	  status = RefMutable(t1);
	  if (!(status & IStep(1)))
	    {
	      update_mutable(w,status+IStep(1),t1);
	      if (TagToHeadfunctor(item)==fd.functor_ix7)
		fd_enqueue(item, 0x0);
	      else
		fd_enqueue(item, 0x2);
	    }
	}
    }
}


static int fd_enqueue_val MAGIC (HIDDEN_PROTO
				 struct worker *w,
				 int index,
				 TAGGED filter, TAGGED lists)
{
  TAGGED list0, list, pair, item, functor, vars, var, t1, *h;
  int n=0;
  int iff_count=0;
  LetShadowregs;

  list = CTagToArg(lists,index);
  list0 = list;
  h = w->global_top;
  while (TagIsLST(list))
    {
      pair = CTagToCar(list);
      list = CTagToCdr(list);
      vars = CTagToArg(pair,1); DerefNonvar(vars);
      item = CTagToArg(pair,2); DerefNonvar(item);
      functor = TagToHeadfunctor(item);
      DerefArg(t1,item,4);
      if (IsVar(t1) && t1!=filter)
	{
	  var = atom_nil;
	  while (TagIsLST(vars) && !IsVar(var))
	    {
	      DerefCar(var,vars);
	      DerefCdr(vars,vars);
	    }
	  if (IsVar(var))
	    {			/* fd_val_link(t1, vars, item) later */
	      n++;
	      iff_count += (functor==fd.functor_iff4);
	      HeapPush(h,pair);
	      HeapPush(h,X(1));
	      X(1) = MakeList(h-2);
	    }
	  else if (functor==fd.functor_ix7)
	    {
	      fd_enqueue(item, 0x0);
	    }
	  else if (functor==fd.functor_iff4)
	    {			/* iff(Ix,B,Key,A) */
	      TAGGED bvar, key;

	      iff_count++;
	      bvar = CTagToArg(item,2);
	      key = CTagToArg(item,3);
	      DerefNonvar(bvar);
	      if (bvar==key)
		{
		  DerefArg(item,item,1);
		  fd_enqueue(item, 0x0);
		}
	    }
	  else
	    {
	      t1 = CTagToArg(item,3); /* status mutable */
	      var = RefMutable(t1);
	      if (!(var & IStep(1))) /* can be in queue if global */
		{
		  update_mutable(w,var+IStep(1),t1);
		  fd_enqueue(item, 0x2);
		}
	    }
	}
      if (iff_count==0)
	list0 = list;
    }
  w->global_top = h;
  /* now disable all iff(Ix,B,Key,A) items we encountered */
  while (TagIsLST(list0))
    {
      pair = CTagToCar(list0);
      list0 = CTagToCdr(list0);
      item = CTagToArg(pair,2); DerefNonvar(item);
      DerefArg(t1,item,4);
      if (IsVar(t1) && t1!=filter && TagToHeadfunctor(item)==fd.functor_iff4)
	{			/* iff(Ix,B,Key,A) */
	  TAGGED key = CTagToArg(item,3);

	  BindHVA(t1,key);
	}
    }
  return n;
}


/* assuming bits > 0 */
/* X(1), X(2) must survive over this */
void fd_enqueue_all MAGIC (HIDDEN_PROTO
			   Argdecl,
			   int bits,
			   TAGGED filter, TAGGED lists_loc)
{
  TAGGED t1;
  int nsusp;
  
				/* each head and tail of each list 
				   is dereferenced */
  t1 = RefMutable(lists_loc);	/* get suspension lists */
  t1 = CTagToArg(t1,1);		/* get suspension count */
  nsusp = GetSmall(t1);
  X(3) = filter;
  X(4) = lists_loc;
  RequireHeap((nsusp<<1)+8, 5); /* 2 * #suspensions + lists/7 */
  filter = X(3);
  lists_loc = X(4);
  if (bits & MASK_SINGLETON)
    {
      TAGGED lists = RefMutable(lists_loc);
      int n=0;

      /* 3.9: enqueue all constraints when ground,
	 (a) to maximize entailment detection,
	 (b) to handle co-references */
      bits |= GetSmall(CTagToArg(lists,2)); /* bitmask of susp. lists */
      X(1) = atom_nil;
      if (bits & MASK_VAL)
	n = fd_enqueue_val(w, 7, filter, lists);
      if (bits & MASK_MINMAX)
	fd_enqueue_list(w, 6, filter, lists);
      if (bits & MASK_MAX)
	fd_enqueue_list(w, 5, filter, lists);
      if (bits & MASK_MIN)
	fd_enqueue_list(w, 4, filter, lists);
      if (bits & MASK_DOM)
	fd_enqueue_list(w, 3, filter, lists);
      if (n>0)
	{
	  TAGGED pair, var, vars, item;
	  TAGGED *h;

	  RequireHeap(13*n, 4);
	  while (TagIsLST(X(1)))
	    {
	      pair = CTagToCar(X(1));  
	      X(1) = CTagToCdr(X(1)); 
	      vars = CTagToArg(pair,1); DerefNonvar(vars);
	      item = CTagToArg(pair,2); DerefNonvar(item);
			    /* unwrap any iff(Ix,B,Key,A) item */
	      if (TagToHeadfunctor(item)==fd.functor_iff4)
		{
		  TAGGED bvar, key;

		  bvar = CTagToArg(item,2);
		  key = CTagToArg(item,3);
		  DerefNonvar(bvar);
		  if (bvar!=key)
		    continue;
		  DerefArg(item,item,1);
		}

	      do
		{
		  DerefCar(var,vars);
		  DerefCdr(vars,vars);
		}
	      while (!IsVar(var));
	      h = w->global_top;
	      HeapPush(h,functor_minus);
	      HeapPush(h,vars);
	      HeapPush(h,item);
	      w->global_top = h;
	      fd_link(w, var, fd.functor_val, MakeStructure(h-3));
	    }
	}
    }
  else
    {
      if (bits & MASK_MINMAX)
	fd_enqueue_list_gc(w, 6, filter, lists_loc);
      if (bits & MASK_MAX)
	fd_enqueue_list_gc(w, 5, filter, lists_loc);
      if (bits & MASK_MIN)
	fd_enqueue_list_gc(w, 4, filter, lists_loc);
      if (bits & MASK_DOM)
	fd_enqueue_list_gc(w, 3, filter, lists_loc);
    }
}


/* implies $fd_begin */
void SPCDECL
prolog_fd_global_enqueue MAGIC (HIDDEN_PROTO SP_term_ref TermR)
{
  WAMENV;
  TAGGED term=RefTerm(TermR);
  TAGGED mutable;

  SP_MANGLE(prolog_fd_begin)(HIDDEN_ARG);
  DerefNonvar(term);
  mutable = CTagToArg(term,3);
  update_mutable(w,RefMutable(mutable)+IStep(1),mutable); /* status mutable, disable resumption */
  /*** done in Prolog
  fd_sync(Arg);
  fd_enqueue(term, 0x3);
  ***/
}


/* implies $fd_begin */
void SPCDECL
prolog_fd_enqueue_all MAGIC (HIDDEN_PROTO SP_term_ref ListsM)
{
  WAMENV;
  TAGGED t1;
  int bits;

/*    X(0) = RefTerm(ListsM); */
  (void)ListsM;
  SP_MANGLE(prolog_fd_begin)(HIDDEN_ARG);
  DerefNonvar(X(0));
  t1 = RefMutable(X(0));	/* get suspension lists */
  bits = GetSmall(CTagToArg(t1,2)); /* get filter */
  if (bits > 0) {
    fd_sync(Arg);
    fd_enqueue_all(w,bits+MASK_SINGLETON,atom_nil,X(0));
  }
}



/* Given lower and upper bounds lb0 = *minp and ub0 = *maxp
   such that both bounds are members of the non-empty FDset d,
   adjust the bounds to tmin and tmax respectively.

   Postconditions:
   *minp = smallest lb such that lb>=tmin and lb>=lb0 and lb in d.
   *maxp = greatest ub such that ub<=tmax and ub<=ub0 and ub in d and lb<=ub.
   returns FALSE if bounds can't be adjusted.
*/
BOOL adjust_bounds(TAGGED tmin, TAGGED tmax,
		   TAGGED d,
		   TAGGED *minp, TAGGED *maxp)
{
  TAGGED r, rnext, a, b;
  TAGGED lb0 = *minp;
  TAGGED ub0 = *maxp;
  
  r = CTagToCar(d), d = CTagToCdr(d);
  if (EmptyInterval(tmin,lb0)) {
    while (d!=atom_nil && point_vs_range(tmin,r)==CMP_AFTER)
      r = CTagToCar(d), d = CTagToCdr(d);
    a = RangeMin(r);
    if (EmptyInterval(a,tmin)) tmin = a; /* tmin = max(a,tmin) */
  } else
    tmin = lb0;
  if (EmptyInterval(ub0,tmax)) {  
    while (d!=atom_nil && point_vs_range(tmax,rnext=CTagToCar(d))!=CMP_BEFORE)
      r = rnext, d = CTagToCdr(d);
    b = RangeMax(r);
    if (EmptyInterval(tmax,b)) tmax = b; /* tmax = min(b,tmax) */
  } else
    tmax = ub0;
  if (EmptyInterval(tmin,tmax))
    return FALSE;
  *minp = tmin;
  *maxp = tmax;
  return TRUE;
}


BOOL adjust_lower_bound(TAGGED tmin,
			TAGGED d,
			TAGGED *minp, TAGGED *maxp)
{
  TAGGED r, a;
  TAGGED lb0 = *minp;
  
  r = CTagToCar(d), d = CTagToCdr(d);
  if (EmptyInterval(tmin,lb0)) {
    while (d!=atom_nil && point_vs_range(tmin,r)==CMP_AFTER)
      r = CTagToCar(d), d = CTagToCdr(d);
    a = RangeMin(r);
    if (EmptyInterval(a,tmin)) tmin = a; /* tmin = max(a,tmin) */
  } else
    tmin = lb0;
  if (EmptyInterval(tmin,*maxp))
    return FALSE;
  *minp = tmin;
  return TRUE;
}


BOOL adjust_upper_bound(TAGGED tmax,
			TAGGED d,
			TAGGED *minp, TAGGED *maxp)
{
  TAGGED r, rnext, b;
  TAGGED ub0 = *maxp;
  
  r = CTagToCar(d), d = CTagToCdr(d);
  if (EmptyInterval(ub0,tmax)) {  
    while (d!=atom_nil && point_vs_range(tmax,rnext=CTagToCar(d))!=CMP_BEFORE)
      r = rnext, d = CTagToCdr(d);
    b = RangeMax(r);
    if (EmptyInterval(tmax,b)) tmax = b; /* tmax = min(b,tmax) */
  } else
    tmax = ub0;
  if (EmptyInterval(*minp,tmax))
    return FALSE;
  *maxp = tmax;
  return TRUE;
}



/* Support functions for new global API */
/* Preconditions:
   - All new domains localized, or protected in term refs.
   - ar X regs live; X(ar-1) is the tail of the action list 
   - CONTPAD heap words guaranteed
   Postconditions:
   - CONTPAD heap words guaranteed
*/

/* -1 = fail, 0 = suspend, 1 = exit */
TAGGED request_done MAGIC (HIDDEN_PROTO
			   Argdecl,
			   int ent,
			   int outarg, int nlive)
{
  TAGGED *h;
  TAGGED action = atom_exit;
  
  switch (ent)
    {
    case -1:
      action = atom_fail;
    case 1:
      RequireHeap(2,nlive);
      h = w->global_top;
      h[0] = action;
      h[1] = X(outarg);
      w->global_top = h+2;
      X(outarg) = MakeList(h);
    }
  return X(outarg);
}


/* Assuming fdset is localized or interval. */
/* Assumption that fdset neither contains old domain nor that they are disjoint
   is unsafe in the context of co-references.
*/
void request_tell MAGIC (HIDDEN_PROTO
			 Argdecl,
			 TAGGED dest_attribute, TAGGED dest_var, TAGGED fdset,
			 int outarg, int nlive)
{
  TAGGED *h, functor, old, t1;
  int why;

  if (CTagToCdr(fdset)==EmptySet)
    {			/* FDset is an interval */
      TAGGED fdsetrange = CTagToCar(fdset);
      TAGGED min = RangeMin(fdsetrange);
      TAGGED max = RangeMax(fdsetrange);
      
      request_tell_interval(Arg, dest_attribute, dest_var,
			    min, max, outarg, nlive);
      return;
    }
  functor = 0;
  why = 0;
  X(nlive) = dest_attribute;		/* preserve over GC */
  X(nlive+1) = dest_var;
  if (fd.debugging) {
    fdset = fd_globalize(w,fdset,5,nlive+2);
    h = w->global_top;
    h[0] = fd.functor_in_set2;
    h[1] = X(nlive+1);
    h[2] = fdset;
    h[3] = MakeStructure(h);
    h[4] = X(outarg);
    w->global_top = h+5;
    X(outarg) = MakeList(h+3);
  } else {
    DerefAttribute(old,dest_attribute);
    old = DomainSet(old);
    switch (fd_compare(old,fdset)) {
    case FDI_SUBSET:
    case FDI_EQUAL:
#if DBG>1
      fprintf(stderr,"* FD: useless request_tell, co-reference?\n");
#endif
      break;
    case FDI_DISJOINT:
#if DBG>1
      fprintf(stderr,"* FD: failing request_tell, co-reference?\n");
#endif
      why = -1;
      break;
    case FDI_SUPERSET:
      why = fd_tell(w,old,fdset,dest_attribute,nlive);
      break;
    case FDI_INTERSECT:
      if (CTagToCdr(old)==EmptySet)
	{
	  TAGGED range = CTagToCar(old);
	  TAGGED min = RangeMin(range);
	  TAGGED max = RangeMax(range);

	  why = fd_tell(w,old,fd_and_interval(fdset,min,max),
			dest_attribute,nlive);
	}
      else
	why = fd_tell(w,old,fd_localize_if_holes(w,fd_and(old,fdset)),
		      dest_attribute,nlive);
    }
    if (why<0) {
      RequireHeap(2,nlive);
      h = w->global_top;
      h[0] = atom_fail;
      h[1] = X(outarg);
      w->global_top = h+2;
      X(outarg) = MakeList(h);
      return;
    } else if (why==0) {
      return;
    } else if (1) {
      DerefSwitch(dest_var,t1,fd_told(w, dest_attribute, dest_var, why&7););
      /* otherwise, co-reference already dealt with */
    } else {
      switch (why & 7) {
      case 1:
	functor = fd.functor_dom;
	break;
      case 2:
      case 3:
	functor = fd.functor_min;
	break;
      case 4:
      case 5:
	functor = fd.functor_max;
	break;
      case 6:
      case 7:
	functor = fd.functor_minmax;
      }
      RequireHeap(4,nlive+2);
      h = w->global_top;
      h[0] = functor;
      h[1] = X(nlive+1);
      h[2] = MakeStructure(h);
      h[3] = X(outarg);
      w->global_top = h+4;
      X(outarg) = MakeList(h+2);
    }
  }
}


void request_tell_interval MAGIC (HIDDEN_PROTO
				  Argdecl,
				  TAGGED dest_attribute, TAGGED dest_var,
				  TAGGED min, TAGGED max,
				  int outarg, int nlive)
{
  TAGGED *h, functor, old, min1, max1, dset, t1;
  int why;

  if (min==max) {
    request_tell_value(Arg, dest_attribute, dest_var, min, outarg, nlive);
    return;
  }
  functor = 0;
  why = 0;
  X(nlive) = dest_attribute;		/* preserve over GC */
  X(nlive+1) = dest_var;
  DerefAttribute(old,dest_attribute);
  dset = DomainSet(old);
  if (fd.debugging) {
    dset = fd_globalize(w,fd_interval(min,max),5,nlive+2);
    h = w->global_top;
    h[0] = fd.functor_in_set2;
    h[1] = X(nlive+1);
    h[2] = dset;
    h[3] = MakeStructure(h);
    h[4] = X(outarg);
    w->global_top = h+5;
    X(outarg) = MakeList(h+3);
  } else {
    switch (fd_compare_interval(dset,min,max))
      {
      case FDI_SUBSET:
      case FDI_EQUAL:
#if DBG>1
	fprintf(stderr,"* FD: useless request_tell_interval, co-reference?\n");
#endif
	break;
      case FDI_DISJOINT:
#if DBG>1
	fprintf(stderr,"* FD: failing request_tell_interval, co-reference?\n");
#endif
	why = -1;
	break;
      case FDI_INTERSECT:
	if (CTagToCdr(dset)!=EmptySet) {
	  why = fd_tell(w,dset,fd_localize_if_holes(w,fd_and_interval(dset,min,max)),
			dest_attribute,nlive);
	  break;
	}
      case FDI_SUPERSET:
	min1 = DomainMin(old);
	max1 = DomainMax(old);
	if (min1!=min) {
	  if (!EmptyInterval(min1,min))
	    why |= MASK_MIN+MASK_MINMAX+MASK_DOM;
	  else
	    min = min1;
	}
	if (max!=max1) {
	  if (!EmptyInterval(max,max1))
	    why |= MASK_MAX+MASK_MINMAX+MASK_DOM;
	  else
	    max = max1;
	}
	why = fd_tell_interval(w,dset,min,max,dest_attribute,why);
      }
    if (why<0) {
      RequireHeap(2,nlive);
      h = w->global_top;
      h[0] = atom_fail;
      h[1] = X(outarg);
      w->global_top = h+2;
      X(outarg) = MakeList(h);
      return;
    } else if (why==0) {
      return;
    } else if (1) {
      DerefSwitch(dest_var,t1,fd_told(w, dest_attribute, dest_var, why&7););
      /* otherwise, co-reference already dealt with */
    } else {
      switch (why & 7) {
      case 1:			/* impossible */
	functor = fd.functor_dom;
	break;
      case 2:
      case 3:
	functor = fd.functor_min;
	break;
      case 4:
      case 5:
	functor = fd.functor_max;
	break;
      case 6:
      case 7:
	functor = fd.functor_minmax;
      }
      RequireHeap(4,nlive+2);
      h = w->global_top;
      h[0] = functor;
      h[1] = X(nlive+1);
      h[2] = MakeStructure(h);
      h[3] = X(outarg);
      w->global_top = h+4;
      X(outarg) = MakeList(h+2);
    }
  }
}


void request_tell_value MAGIC (HIDDEN_PROTO
			       Argdecl,
			       TAGGED dest_attribute, TAGGED dest_var,
			       TAGGED value,
			       int outarg, int nlive)
{
  TAGGED *h, functor=0, old, dom, t1;
  int why = 0;

  X(nlive) = dest_attribute;		/* preserve over GC */
  X(nlive+1) = dest_var;
  DerefAttribute(dom,dest_attribute);
  if (fd.debugging) {
    RequireHeap(5,nlive);
    h = w->global_top;
    h[0] = fd.functor_eq;
    h[1] = X(nlive+1);
    h[2] = value;
    h[3] = MakeStructure(h);
    h[4] = X(outarg);
    w->global_top = h+5;
    X(outarg) = MakeList(h+3);
  } else {
    old = DomainSet(dom);
    if (!fd_member(value,old)) {
#if DBG>1
      fprintf(stderr,"* FD: failing request_tell_value, co-reference?\n");
#endif
      why = -1;
    } else if (DomainSize(dom)==MakeSmall(1)) {
#if DBG>1
      fprintf(stderr,"* FD: useless request_tell_value, co-reference?\n");
#endif
    } else {
      why = fd_tell_value(w,old,value,dest_attribute);
    }
    if (why<0) {
      RequireHeap(2,nlive);
      h = w->global_top;
      h[0] = atom_fail;
      h[1] = X(outarg);
      w->global_top = h+2;
      X(outarg) = MakeList(h);
      return;
    } else if (why==0) {
      return;
    } else if (1) {
      DerefSwitch(dest_var,t1,fd_told(w, dest_attribute, dest_var, why&7););
      /* otherwise, co-reference already dealt with */
    } else {
      switch (why & 7) {
      case 1:			/* impossible */
	functor = fd.functor_dom;
	break;
      case 2:
      case 3:
	functor = fd.functor_min;
	break;
      case 4:
      case 5:
	functor = fd.functor_max;
	break;
      case 6:
      case 7:
	functor = fd.functor_minmax;
      }
      RequireHeap(4,nlive+2);
      h = w->global_top;
      h[0] = functor;
      h[1] = X(nlive+1);
      h[2] = MakeStructure(h);
      h[3] = X(outarg);
      w->global_top = h+4;
      X(outarg) = MakeList(h+2);
    }
  }
}


void request_rewrite_eq MAGIC (HIDDEN_PROTO
			       Argdecl, TAGGED x_var, TAGGED y_var,
			       int outarg, int nlive)
{
  TAGGED *h;
  
  if (x_var==y_var) {
  } else if (!fd.debugging) {
    (void)cunify(w,x_var,y_var);
  } else {
    X(nlive) = x_var;
    X(nlive+1) = y_var;
    RequireHeap(7,nlive+2);
    h = w->global_top;
    h[0] = fd.functor_eq;
    h[1] = X(nlive);
    h[2] = X(nlive+1);
    h[3] = fd.functor_call;
    h[4] = MakeStructure(h);
    h[5] = MakeStructure(h+3);
    h[6] = X(outarg);
    w->global_top = h+7;
    X(outarg) = MakeList(h+5);
  }
}


/* Support for managing incumbents:
   '$fd_update_incumbent'(+Ptr, +Value, +Vertex).
	store a new incumbent vertex and value
   '$fd_incumbent_bound'(+Ptr, -Value).
	retrieve the current incumbent value.
*/

void SPCDECL
prolog_fd_update_incumbent MAGIC (HIDDEN_PROTO
				  long ptr, /* N.B. struct instance * does not work */
				  SP_term_ref ValueR,
				  SP_term_ref VertexR)
{
  WAMENV;
  struct instance *ins;
  int i, no_cells;
  TAGGED vertex;

  ins = TagToInstance(MakeSmall(ptr));

  no_cells = ((ins->objsize-sizeof(struct instance))>>LogSizeOfWord) + 1;
  /* value is at offset no_cells-4
     vertex is at offset no_cells-7, -9, ...
  */
  DEREF(ins->code[no_cells-4],RefTerm(ValueR));
  DEREF(vertex,RefTerm(VertexR));
  for (i=no_cells-7; TagIsLST(vertex); i-=2) {
    DerefCar(ins->code[i],vertex);
    DerefCdr(vertex,vertex);
  }
}


void SPCDECL
prolog_fd_incumbent_bound MAGIC (HIDDEN_PROTO
				 long ptr, /* N.B. struct instance * does not work */
				 SP_term_ref ValueR)
{
  WAMENV;
  struct instance *ins;
  int no_cells;

  ins = TagToInstance(MakeSmall(ptr));

  no_cells = ((ins->objsize-sizeof(struct instance))>>LogSizeOfWord) + 1;
  /* value is at offset no_cells-4
     vertex is at offset no_cells-7, -9, ...
  */
  RefTerm(ValueR) = ins->code[no_cells-4];
}


void SPCDECL prolog_fd_minint_maxint MAGIC (HIDDEN_PROTO
					    long *minp, long *maxp)
{
  (void)HIDDEN_ARG_OR_ZERO;
  *minp = -HighInt;
  *maxp = HighInt-1L;
}


/* Heap routines from Cormen et al. */

void heap_init(struct heap *h)
{
  h->size = 0;
}


void heap_insert(struct heap *h,
		 void *item,
		 HeapFun cmpfun)
{
  int i = ++h->size;

  while (i>1 && (*cmpfun)(h->item[i>>1],item) > 0) {
    h->item[i] = h->item[i>>1];
    i >>= 1;
  }
  h->item[i] = item;
}


/* Repair the heap property
   i.e. h->item[i>>1].key <= h->item[i].key
*/
void heapify(struct heap *h,
	     HeapFun cmpfun)
{
  int i = 1;

  for (;;) {
    int l = i<<1;
    int r = l+1;
    int smallest;
    
    if (l <= h->size && (*cmpfun)(h->item[l],h->item[i]) < 0)
      smallest = l;
    else
      smallest = i;
    if (r <= h->size && (*cmpfun)(h->item[r],h->item[smallest]) < 0)
      smallest = r;
    if (smallest != i) {
      void *tmp = h->item[i];
      
      h->item[i] = h->item[smallest];
      h->item[smallest] = tmp;
      i = smallest;
    } else
      break;
  }
}


void *heap_extract_min(struct heap *h,
		       HeapFun cmpfun)
{
  void *it = h->item[1];

  if (h->size==0)
    return NULL;
  h->item[1] = h->item[h->size];
  --h->size;
  heapify(h,cmpfun);

  return it;
}


#if FD_PERSISTENT

/* Support for Palloc/Pfree. */

void *fd_perm_alloc MAGIC (HIDDEN_PROTO
			   Argdecl,
			   int nbytes,
			   TAGGED handle) /* HVA to bind */
{
  void *ptr = SP_malloc(nbytes);
  TAGGED tptr = PointerToTerm(ptr);
  TAGGED *h = w->global_top; 
  TAGGED inner, outer;
  LetShadowregs;

  inner = MakeStructure(h);
  *h++ = fd.functor_Dfree;
  *h++ = tptr;
  outer = MakeList(h);
  Load0HVA(h);
  *h++ = inner;
  w->global_top = h;
  TrailPushCheck(outer);
  BindHVA(handle,outer);
  return ptr;
}


void *fd_perm_data(TAGGED handle) /* [Flag | '$free'(Ptr)] */
{
  handle = CTagToCdr(handle);
  handle = CTagToArg(handle,1);
  return TermToPointer(handle);
}


/* A mutable item on the trail is redundant if:
   1. the mutable is in H+, or
   2. there is a previous item in TR+, or
   3. the new and old values are the same.
   */
#define RedundantMutableItem(Mutp,Item) \
((Mutp) >= w->global_uncond || (Item)[2] >= TrailToInt(w->node->trail_top) || (Mutp)[1]==(Item)[1])

#define PrevMutItem(Item) \
(w->trail_start + GetSmall((Item)[2]))

/* Compress the trail from tr and up, removing all zeroes.
   Any mutable timestamps pointing into the compressed region
   must also be updated.
   RIPPED OFF FROM EMULATOR.
*/
static void compress_trail MAGIC (HIDDEN_PROTO Argdecl,
				  TAGGED *tr)
{
  TAGGED *trlim = w->trail_top;
  TAGGED *h = tr;
  TAGGED ref, *mutstr, *mutitem;

  while (TrailYounger(trlim,tr)) {
    ref = tr[0];
    if (TagIsSTR(ref) && TagToHeadfunctor(ref)==fd.functor_mutable) {
      /* About to move a mutable item.  First, remove any redundant items. */
      mutstr = TagToSTR(ref);
      mutitem = PrevMutItem(mutstr);
      while (RedundantMutableItem(mutstr,mutitem) && mutitem >= tr) {
	mutstr[2] = mutitem[2];
	mutitem[0] = mutitem[1] = mutitem[2] = 0;
	mutitem = PrevMutItem(mutstr);
      }
      if (mutitem >= tr) {	/* some mutitems still in the compressed region */
	while (mutstr[2] != TrailToInt(tr))
	  mutstr = PrevMutItem(mutstr);
	mutstr[2] -= WD(tr-h);	/* = TrailToInt(h);  */
	h[0] = ref;
	h[1] = tr[1];
	h[2] = tr[2];
	h+=3;
      }
      tr+=3;
    } else {
      if (ref) 
	*h++ = ref;
      tr++;
    }
  }
  w->trail_top = h;
}

void fd_perm_free MAGIC (HIDDEN_PROTO
			 Argdecl,
			 TAGGED handle)	/* [Flag | '$free'(Ptr)] */
{
  struct node *nd;
  TAGGED *ptr, t1;
  void (SPCDECL *destructor)(void *);
  ANYPOINTER frame;
  
  DerefHeapSwitch(handle,t1,;);
  ptr = TagToLST(handle);
  for (nd = w->node;
       ptr < nd->global_top;
       nd = ChoiceptPrevious(nd))
    ;
  if (nd==w->node) {
    for (ptr=w->trail_top; ptr[-1]!=handle; ptr--)
      ;
    ptr[-1] = 0;		/* for compress_trail */
    compress_trail(w,ptr-1);
    frame = TermToPointer(CTagToArg(CTagToCdr(handle),1));
    destructor = *(void (SPCDECL **)(void*))frame;
    (*destructor)(frame);
  } else {
    for (;
	 ChoiceYounger(nd,w->choice_start) && !ChoiceptTestCleanup(nd);
	 nd = ChoiceptPrevious(nd))
      ChoiceptMarkCleanup(nd);
    *ptr = TaggedOne;
    TrailPushCheck(TagHVA(ptr));
  }
}    
#endif

/* Most propagators have arguments (+State0, -State), ... where
   State0 = F(......,Handle,Stamp), is left dereferenced,
   State  = copy of State0 with Stamp incremented.
   Also, check if this execution step can be backtracked over or not.
*/
TAGGED unify_output_state(Argdecl,
			  TAGGED *phandle,
			  long *pstamp,
			  BOOL *pcommitted)
{
  TAGGED handle, t1, *s, *h;
  int ar, i;
  LetShadowregs;
  
  DerefNonvar(X(0));
  ar = Arity(TagToHeadfunctor(X(0)));
  DerefArg(handle,X(0),ar-1);
  *phandle = handle;
  DerefArg(t1,X(0),ar);
  *pstamp = GetSmall(t1);
  s = TagToArg(X(0),0);
  if (s >= GLOBAL_UNCOND) {
    s[ar] += IStep(1);		/* increment stamp */
  } else {
    h = w->global_top;
    for (i=0; i<ar+1; i++)
      h[i] = s[i];
    w->global_top = h+ar+1;
    h[ar] += IStep(1);		/* increment stamp */
    X(0) = MakeStructure(h);
  }
  *pcommitted = (IsVar(handle) ? TRUE : TagToLST(handle) >= GLOBAL_UNCOND);
  return X(0);
}

