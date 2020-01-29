/* Copyright(C) 1999, Swedish Institute of Computer Science */

#include "fd.h"

#if MULTI_SP_AWARE
#endif /* MULTI_SP_AWARE */

/* '$fd_set_singleton(+Val, +Mutable) */
void SPCDECL
prolog_fd_set_singleton MAGIC (HIDDEN_PROTO
			       SP_term_ref ValR,
			       SP_term_ref MutR)
{
  WAMENV;
  TAGGED val, mutable, t1, *h;
  LetShadowregs;

  val = RefTerm(ValR);
  mutable = RefTerm(MutR);
  DerefNonvar(val);
  DerefNonvar(mutable);
  t1 = RefMutable(mutable);
  if (!fd_member(val, DomainSet(t1)))
    SP_fail();
  else if (TagToSTR(t1) >= GLOBAL_UNCOND) {
				/* can safely smash it */
    TAGGED *arg = TagToArg(t1,0);
      
    h = w->global_top;
    *h++ = MakeList(arg+2);
    *h++ = atom_nil;
    w->global_top = h;
    arg[1] = MakeList(h-2);
    arg[2] = val;
    arg[3] = val;
    arg[4] = TaggedOne;
  } else {
    h = w->global_top;
    *h++ = MakeList(w->global_top+4);
    *h++ = atom_nil;
    *h++ = fd.functor_dom4;
    *h++ = MakeList(w->global_top);
    *h++ = val;
    *h++ = val;
    *h++ = TaggedOne;
    w->global_top = h;
    update_mutable(w,MakeStructure(h-5), mutable);
  }
}

