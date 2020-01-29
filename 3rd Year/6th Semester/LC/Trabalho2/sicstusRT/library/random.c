#include <sicstus/sicstus.h>

/*  This is an implementation of algorithm AS 183 from the journal
    "Applied Statistics", recoded in C. 
*/

/* Exported functions */
#if 1
#include "random_glue.h"
#else
extern void cgetrand PROTOTYPE((long *X,long *Y,long *Z));
extern void cputrand PROTOTYPE((long X,long Y,long Z));
extern double crandom PROTOTYPE((void));
#endif

struct random_state {
  short A;
  short B;
  short C;
};

#if MULTI_SP_AWARE

/* [PM] 3.9b4 ensures local.foo works. Also avoids need for SP_CONTEXT_SWITCH_HOOK. */
#define local (*(struct random_state *)*SP_foreign_stash())

#else  /* !MULTI_SP_AWARE */

static struct random_state local;

#endif /* !MULTI_SP_AWARE */

/* [PM] 3.9b2 These are constants so they need not go in local state. */
static const double ZX = 1.0/30269.0;
static const double ZY = 1.0/30307.0;
static const double ZZ = 1.0/30323.0;


void SPCDECL rand_init(SPAPI_ARG_PROTO_DECL 
		       int when)
{
  (void)when;                   /* [PM] 3.9b5 avoid -Wunused */

#if MULTI_SP_AWARE
  (*SP_foreign_stash()) = (void*)SP_malloc(sizeof(struct random_state));
#endif/* MULTI_SP_AWARE */

  local.A = 27134;
  local.B = 9213;
  local.C = 17773;
}

void SPCDECL rand_deinit(SPAPI_ARG_PROTO_DECL 
			 int when)
{
  (void)when;                   /* [PM] 3.9b5 avoid -Wunused */

#if MULTI_SP_AWARE
  SP_free((void*)*SP_foreign_stash());
  (*SP_foreign_stash()) = NULL; /* not needed */
#endif
}


void SPCDECL cgetrand(SPAPI_ARG_PROTO_DECL 
		      long *X, long *Y, long *Z)
{
  *X = local.A;
  *Y = local.B;
  *Z = local.C;
}

void SPCDECL cputrand(SPAPI_ARG_PROTO_DECL 
		      long X, long Y, long Z)
{
  local.A = (short)X;
  local.B = (short)Y;
  local.C = (short)Z;
}

double SPCDECL crandom(SPAPI_ARG_PROTO_DECL0)
{
  long X,Y,Z;
  double T;
  
  X = (local.A*171) % 30269;
  Y = (local.B*172) % 30307;
  Z = (local.C*170) % 30323;
  T = X*ZX + Y*ZY + Z*ZZ;
  local.A = (short)X;
  local.B = (short)Y;
  local.C = (short)Z;
  return T-(int)T;
}


