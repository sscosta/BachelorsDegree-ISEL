/* Copyright(C) 1999, Swedish Institute of Computer Science */

#include "fd.h"

#if MULTI_SP_AWARE
#endif

/*** Support for saturate/2 etc. ***/

/* $fd_mark_variable(+M, +I)

   will mark the variable with domain mutable M with the integer I
*/
void SPCDECL
prolog_fd_mark_variable MAGIC (HIDDEN_PROTO
			       SP_term_ref MutR, long mark)
{
  WAMENV;
  
  DerefNonvar(RefTerm(MutR));
  TagToSTR(RefTerm(MutR))[2-FD_ATTR_DOM_OFFSET] = MakeSmall(mark);
}



/* $fd_trailed_mutables(+Cut, -L)
   will return a list of Index-Domain for the trailed mutables
*/
void SPCDECL
prolog_fd_trailed_mutables MAGIC (HIDDEN_PROTO
				  long mark, SP_term_ref ListR)
{
  WAMENV;
  TAGGED *tr0, *tr, *s, *h, t1, t2, list;
  TAGGED tmark = MakeSmall(mark);
  
 restart:
  h = w->global_top;
  list = atom_nil;
  tr0 = ChoiceFromInt(tmark)->trail_top;
  tr = w->trail_top;
  while (TrailYounger(tr,tr0) && HeapDifference(h,w->heap_end) >= 5+CONTPAD)
    {
      t1 = TrailPop(tr);
      if (TagIsSIN(t1))	/* found mutable */
	{
	  tr -= 2;
	  s = TagToSTR(tr[0]);
	  t1 = s[2-FD_ATTR_DOM_OFFSET];
	  t2 = s[1];
	  if (s[2]==TrailToInt(tr) /* most recent entry for mutable? */
	      && s[-FD_ATTR_DOM_OFFSET]==fd.functor_v4 
	      && TagIsSTR(t2)
	      && TagToHeadfunctor(t2)==fd.functor_dom4
	      && Tgtz(t1))
	    {
	      t2 = DomainSet(t2);
	      HeapPush(h,functor_minus);
	      HeapPush(h,t1);
	      HeapPush(h,t2);
	      t2 = MakeStructure(h-3);
	      HeapPush(h,t2);
	      HeapPush(h,list);
	      list = MakeList(h-2);
	    }
	} 
    }
  if (TrailYounger(tr,tr0))
    {
      RequireHeap(2*HeapDifference(w->global_top,w->heap_end), 2);
      goto restart;
    }
  w->global_top = h;
  RefTerm(ListR) = list;
}


