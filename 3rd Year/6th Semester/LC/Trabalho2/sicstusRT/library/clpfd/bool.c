/* Copyright(C) 1999, Swedish Institute of Computer Science */

#include "fd.h"

#if MULTI_SP_AWARE
#endif /* MULTI_SP_AWARE */


#define CAND 0
#define CANDN 1
#define CNAND 2
#define COR 3
#define CORN 4
#define CNOR 5
#define CXOR 6
#define CXORN 7

#define COMB(F,X,Y,Z) (((((((F)*3)+(X))*3)+(Y))*3)+(Z))

#define TZ TaggedZero
#define TO TaggedOne
#define xm RefTerm(ref+0)
#define x  RefTerm(ref+1)
#define ym RefTerm(ref+2)
#define y  RefTerm(ref+3)
#define zm RefTerm(ref+4)
#define z  RefTerm(ref+5)

/* Solve the eight essential Boolean constraints. */
/* '$fd_bool'(Fun, X, XM, Y, YM, Z, ZM, Actions) */
void SPCDECL
prolog_fd_bool MAGIC (HIDDEN_PROTO
		      SP_term_ref State,
		      SP_term_ref NewState,
		      SP_term_ref Actions)
{
  WAMENV;
  int xsig, ysig, zsig;
  int ent=0;
  long fun;
  TAGGED xv, yv, zv, t;
  SP_term_ref ref = SP_new_term_refs(6);

  RefTerm(NewState) = RefTerm(State);
  X(2) = atom_nil;		/* actions list */
  DerefNonvar(X(0));
  DerefArg(xv,X(0),1);
  x = xv;
  DerefArg(yv,X(0),3);
  y = yv;
  DerefArg(zv,X(0),5);
  z = zv;
  DerefArg(t,X(0),2);
  xm = t;
  DerefArg(t,X(0),4);
  ym = t;
  DerefArg(t,X(0),6);
  zm = t;
  DerefArg(t,X(0),7);
  fun = GetSmall(t);
  xsig = (IsVar(xv) ? 2 : GetSmall(xv));
  ysig = (IsVar(yv) ? 2 : GetSmall(yv));
  zsig = (IsVar(zv) ? 2 : GetSmall(zv));
  switch (COMB(fun,xsig,ysig,zsig)) {
  case COMB(CAND,1,2,2):
  case COMB(COR,0,2,2):
  case COMB(CXOR,0,2,2):
  case COMB(CXORN,1,2,2):
    request_rewrite_eq(w, y, z, 2, 3);
    ent = 1;
    break;
  case COMB(CAND,2,1,2):
  case COMB(CANDN,2,0,2):
  case COMB(COR,2,0,2):
  case COMB(CORN,2,1,2):
  case COMB(CXOR,2,0,2):
  case COMB(CXORN,2,1,2):
  x_equals_z:
    request_rewrite_eq(w, x, z, 2, 3);
    ent = 1;
    break;
  case COMB(CXOR,2,2,0):
  case COMB(CXORN,2,2,1):
    request_rewrite_eq(w, x, y, 2, 3);
    ent = 1;
    break;
  case COMB(CAND,2,2,0):
  case COMB(CNAND,2,2,1):
    if (xv==yv) goto x_equals_0;
    else        break;
  case COMB(CAND,2,2,2):
  case COMB(COR,2,2,2):
    if (xv==yv) goto x_equals_z;
    else        break;
  case COMB(CANDN,2,2,0):
  case COMB(CORN,2,2,1):
    break;
  case COMB(CANDN,2,2,2):
    if (xv==yv) goto z_equals_0;
    else if (yv==zv) goto x_equals_0_y_equals_0;
    else        break;
  case COMB(CXOR,2,2,2):
    if (xv==yv) goto z_equals_0;
    else if (xv==zv) goto y_equals_0;
    else if (yv==zv) goto x_equals_0;
    else        break;
  case COMB(COR,2,2,1):
  case COMB(CNOR,2,2,0):
    if (xv==yv) goto x_equals_1;
    else        break;
  case COMB(CORN,2,2,2):
    if (xv==yv) goto z_equals_1;
    else if (yv==zv) goto x_equals_1_y_equals_1;
    else        break;
  case COMB(CXORN,2,2,2):
    if (xv==yv) goto z_equals_1;
    else if (xv==zv) goto y_equals_1;
    else if (yv==zv) goto x_equals_1;
    else        break;
  case COMB(CXOR,2,2,1):
  case COMB(CXORN,2,2,0):
    if (xv==yv) goto fail;
    else        break;
  case COMB(CNAND,2,1,2):
  case COMB(CNOR,2,0,2):
  case COMB(CXOR,2,1,2):
  case COMB(CXORN,2,0,2):
    if (xv==zv) goto fail;
    else        break;
  case COMB(CNAND,2,2,2):
    if (xv==zv) goto x_equals_1_y_equals_0;
    else if (yv==zv) goto x_equals_0_y_equals_1;
    else        break;
  case COMB(CNOR,2,2,2):
    if (xv==zv) goto x_equals_0_y_equals_1;
    else if (yv==zv) goto x_equals_1_y_equals_0;
    else        break;
  case COMB(CANDN,1,2,2):
  case COMB(CNAND,1,2,2):
  case COMB(CORN,0,2,2):
  case COMB(CNOR,0,2,2):
  case COMB(CXOR,1,2,2):
  case COMB(CXORN,0,2,2):
    if (yv==zv) goto fail;
    else        break;

  case COMB(CAND,0,0,0):
  case COMB(CAND,0,1,0):
  case COMB(CAND,0,2,0):
  case COMB(CAND,1,0,0):
  case COMB(CAND,1,1,1):
  case COMB(CAND,2,0,0):
  case COMB(CANDN,0,0,0):
  case COMB(CANDN,0,1,0):
  case COMB(CANDN,0,2,0):
  case COMB(CANDN,1,0,1):
  case COMB(CANDN,1,1,0):
  case COMB(CANDN,2,1,0):
  case COMB(CNAND,0,0,1):
  case COMB(CNAND,0,1,1):
  case COMB(CNAND,0,2,1):
  case COMB(CNAND,1,0,1):
  case COMB(CNAND,1,1,0):
  case COMB(CNAND,2,0,1):
  case COMB(COR,0,0,0):
  case COMB(COR,0,1,1):
  case COMB(COR,1,0,1):
  case COMB(COR,1,1,1):
  case COMB(COR,1,2,1):
  case COMB(COR,2,1,1):
  case COMB(CORN,0,0,1):
  case COMB(CORN,0,1,0):
  case COMB(CORN,1,0,1):
  case COMB(CORN,1,1,1):
  case COMB(CORN,1,2,1):
  case COMB(CORN,2,0,1):
  case COMB(CNOR,0,0,1):
  case COMB(CNOR,0,1,0):
  case COMB(CNOR,1,0,0):
  case COMB(CNOR,1,1,0):
  case COMB(CNOR,1,2,0):
  case COMB(CNOR,2,1,0):
  case COMB(CXOR,0,0,0):
  case COMB(CXOR,0,1,1):
  case COMB(CXOR,1,0,1):
  case COMB(CXOR,1,1,0):
  case COMB(CXORN,0,0,1):
  case COMB(CXORN,0,1,0):
  case COMB(CXORN,1,0,0):
  case COMB(CXORN,1,1,1):
    ent = 1;
    break;
  case COMB(CAND,0,0,1):
  case COMB(CAND,0,1,1):
  case COMB(CAND,0,2,1):
  case COMB(CAND,1,0,1):
  case COMB(CAND,1,1,0):
  case COMB(CAND,2,0,1):
  case COMB(CANDN,0,0,1):
  case COMB(CANDN,0,1,1):
  case COMB(CANDN,0,2,1):
  case COMB(CANDN,1,0,0):
  case COMB(CANDN,1,1,1):
  case COMB(CANDN,2,1,1):
  case COMB(CNAND,0,0,0):
  case COMB(CNAND,0,1,0):
  case COMB(CNAND,0,2,0):
  case COMB(CNAND,1,0,0):
  case COMB(CNAND,1,1,1):
  case COMB(CNAND,2,0,0):
  case COMB(COR,0,0,1):
  case COMB(COR,0,1,0):
  case COMB(COR,1,0,0):
  case COMB(COR,1,1,0):
  case COMB(COR,1,2,0):
  case COMB(COR,2,1,0):
  case COMB(CORN,0,0,0):
  case COMB(CORN,0,1,1):
  case COMB(CORN,1,0,0):
  case COMB(CORN,1,1,0):
  case COMB(CORN,1,2,0):
  case COMB(CORN,2,0,0):
  case COMB(CNOR,0,0,0):
  case COMB(CNOR,0,1,1):
  case COMB(CNOR,1,0,1):
  case COMB(CNOR,1,1,1):
  case COMB(CNOR,1,2,1):
  case COMB(CNOR,2,1,1):
  case COMB(CXOR,0,0,1):
  case COMB(CXOR,0,1,0):
  case COMB(CXOR,1,0,0):
  case COMB(CXOR,1,1,1):
  case COMB(CXORN,0,0,0):
  case COMB(CXORN,0,1,1):
  case COMB(CXORN,1,0,1):
  case COMB(CXORN,1,1,0):
  fail:
    ent = -1;
    break;
  case COMB(CAND,2,1,0):
  case COMB(CANDN,2,0,0):
  case COMB(CNAND,2,1,1):
  case COMB(COR,2,0,0):
  case COMB(CORN,2,1,0):
  case COMB(CNOR,2,0,1):
  case COMB(CXOR,2,0,0):
  case COMB(CXOR,2,1,1):
  case COMB(CXORN,2,0,1):
  case COMB(CXORN,2,1,0):
				/* do([x=0,exit]); */
  x_equals_0:
    request_tell_value(w, xm, x, TZ, 2, 3);
    ent = 1;
    break;
  case COMB(COR,2,2,0):
  case COMB(CNOR,2,2,1):
				/* do([x=0,y=0,exit]); */
  x_equals_0_y_equals_0:
    request_tell_value(w, xm, x, TZ, 2, 3);
    request_tell_value(w, ym, y, TZ, 2, 3);
    ent = 1;
    break;
  case COMB(CORN,2,2,0):
				/* do([x=0,y=1,exit]); */
  x_equals_0_y_equals_1:
    request_tell_value(w, xm, x, TZ, 2, 3);
    request_tell_value(w, ym, y, TO, 2, 3);
    ent = 1;
    break;
  case COMB(CAND,2,1,1):
  case COMB(CANDN,2,0,1):
  case COMB(CNAND,2,1,0):
  case COMB(COR,2,0,1):
  case COMB(CORN,2,1,1):
  case COMB(CNOR,2,0,0):
  case COMB(CXOR,2,0,1):
  case COMB(CXOR,2,1,0):
  case COMB(CXORN,2,0,0):
  case COMB(CXORN,2,1,1):
				/* do([x=1,exit]); */
  x_equals_1:
    request_tell_value(w, xm, x, TO, 2, 3);
    ent = 1;
    break;
  case COMB(CANDN,2,2,1):
				/* do([x=1,y=0,exit]); */
  x_equals_1_y_equals_0:
    request_tell_value(w, xm, x, TO, 2, 3);
    request_tell_value(w, ym, y, TZ, 2, 3);
    ent = 1;
    break;
  case COMB(CAND,2,2,1):
  case COMB(CNAND,2,2,0):
				/* do([x=1,y=1,exit]); */
  x_equals_1_y_equals_1:
    request_tell_value(w, xm, x, TO, 2, 3);
    request_tell_value(w, ym, y, TO, 2, 3);
    ent = 1;
    break;
  case COMB(CAND,1,2,0):
  case COMB(CANDN,1,2,1):
  case COMB(CNAND,1,2,1):
  case COMB(COR,0,2,0):
  case COMB(CORN,0,2,1):
  case COMB(CNOR,0,2,1):
  case COMB(CXOR,0,2,0):
  case COMB(CXOR,1,2,1):
  case COMB(CXORN,0,2,1):
  case COMB(CXORN,1,2,0):
				/* do([y=0,exit]); */
  y_equals_0:
    request_tell_value(w, ym, y, TZ, 2, 3);
    ent = 1;
    break;
  case COMB(CAND,1,2,1):
  case COMB(CANDN,1,2,0):
  case COMB(CNAND,1,2,0):
  case COMB(COR,0,2,1):
  case COMB(CORN,0,2,0):
  case COMB(CNOR,0,2,0):
  case COMB(CXOR,0,2,1):
  case COMB(CXOR,1,2,0):
  case COMB(CXORN,0,2,0):
  case COMB(CXORN,1,2,1):
				/* do([y=1,exit]); */
  y_equals_1:
    request_tell_value(w, ym, y, TO, 2, 3);
    ent = 1;
    break;
  case COMB(CAND,0,0,2):
  case COMB(CAND,0,1,2):
  case COMB(CAND,0,2,2):
  case COMB(CAND,1,0,2):
  case COMB(CAND,2,0,2):
  case COMB(CANDN,0,0,2):
  case COMB(CANDN,0,1,2):
  case COMB(CANDN,0,2,2):
  case COMB(CANDN,1,1,2):
  case COMB(CANDN,2,1,2):
  case COMB(CNAND,1,1,2):
  case COMB(COR,0,0,2):
  case COMB(CORN,0,1,2):
  case COMB(CNOR,0,1,2):
  case COMB(CNOR,1,0,2):
  case COMB(CNOR,1,1,2):
  case COMB(CNOR,1,2,2):
  case COMB(CNOR,2,1,2):
  case COMB(CXOR,0,0,2):
  case COMB(CXOR,1,1,2):
  case COMB(CXORN,0,1,2):
  case COMB(CXORN,1,0,2):
				/* do([z=0,exit]); */
  z_equals_0:
    request_tell_value(w, zm, z, TZ, 2, 3);
    ent = 1;
    break;
  case COMB(CAND,1,1,2):
  case COMB(CANDN,1,0,2):
  case COMB(CNAND,0,0,2):
  case COMB(CNAND,0,1,2):
  case COMB(CNAND,0,2,2):
  case COMB(CNAND,1,0,2):
  case COMB(CNAND,2,0,2):
  case COMB(COR,0,1,2):
  case COMB(COR,1,0,2):
  case COMB(COR,1,1,2):
  case COMB(COR,1,2,2):
  case COMB(COR,2,1,2):
  case COMB(CORN,0,0,2):
  case COMB(CORN,1,0,2):
  case COMB(CORN,1,1,2):
  case COMB(CORN,1,2,2):
  case COMB(CORN,2,0,2):
  case COMB(CNOR,0,0,2):
  case COMB(CXOR,0,1,2):
  case COMB(CXOR,1,0,2):
  case COMB(CXORN,0,0,2):
  case COMB(CXORN,1,1,2):
				/* do([z=1,exit]); */
  z_equals_1:
    request_tell_value(w, zm, z, TO, 2, 3);
    ent = 1;
    break;
  }
  RefTerm(Actions) = request_done(w, ent, 2, 3);
  SP_reset_term_refs(ref);
}
