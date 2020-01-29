/* Copyright(C) 1999, Swedish Institute of Computer Science */

#include "fd.h"

#define CSTATE_ONLY 0

#if MULTI_SP_AWARE

#define free_one_dp(A1,A2) free_one_dp(HIDDEN_ARG, A1,A2)
#define free_all_dp(A1) free_all_dp(HIDDEN_ARG, A1)

#if 0                           /* not redefined. It uses FD_SETUP_SPENV to access the SPEnv */
#define linear_destructor(A1) linear_destructor(HIDDEN_ARG, A1)
#endif

#define cmp_desc_maxdiff(A1,A2) cmp_desc_maxdiff(HIDDEN_ARG, A1,A2)

#define qsort_desc_maxdiffswap(A1,A2,A3,A4) qsort_desc_maxdiffswap(HIDDEN_ARG, A1,A2,A3,A4)
#define qsort_desc_maxdiffmed3(A1,A2,A3) qsort_desc_maxdiffmed3(HIDDEN_ARG, A1,A2,A3)

#define qsort_desc_maxdiff(A1,A2) qsort_desc_maxdiff(HIDDEN_ARG, A1,A2)

#define refresh_targets_gcd(A1,A2) refresh_targets_gcd(HIDDEN_ARG, A1,A2)
#define sliding_gcd_rule(A1,A2,A3) sliding_gcd_rule(HIDDEN_ARG, A1,A2,A3)
#define fd_linear_general(A1,A2,A3,A4,A5,A6) fd_linear_general(HIDDEN_ARG, A1,A2,A3,A4,A5,A6)
#define fd_linear_fast(A1,A2,A3,A4) fd_linear_fast(HIDDEN_ARG, A1,A2,A3,A4)
#define fd_linear_unit(A1,A2,A3,A4) fd_linear_unit(HIDDEN_ARG, A1,A2,A3,A4)
#define fd_linear_adjust(A1) fd_linear_adjust(HIDDEN_ARG, A1)

#define scalar_product_filter(A1,A2,A3,A4,A5) scalar_product_filter(HIDDEN_ARG, A1,A2,A3,A4,A5)
#define alloc_frame(A1,A2,A3,A4,A5) alloc_frame(HIDDEN_ARG, A1,A2,A3,A4,A5)
#define maybe_rehash(A1) maybe_rehash(HIDDEN_ARG, A1)
#define knapsack_filter_noninc(A1,A2,A3) knapsack_filter_noninc(HIDDEN_ARG, A1,A2,A3)
#define knapsack_filter_inc(A1,A2) knapsack_filter_inc(HIDDEN_ARG, A1,A2)
#define knapsack_filter(A1,A2,A3,A4,A5) knapsack_filter(HIDDEN_ARG, A1,A2,A3,A4,A5)

#endif /* !MULTI_SP_AWARE */


#define NegateLB(LB) ((LB)==Inf ? Sup : Tminus(LB))

#define NegateUB(UB) ((UB)==Sup ? Inf : Tminus(UB))

#define ABS(G) ((G)>=0 ? (G) : -(G))

#define GetSmallSafe(T)						\
(TagIsSmall(T) ? GetSmall(T) : (T)==Inf ? -CLPFD_MAXINT : CLPFD_MAXINT)

/*** PRM 2211
#define SUBCHK(A,B,C)				\
{ int m_chk = ((A)<0)^((B)<0);			\
  (C) = (A)-(B);				\
  if (m_chk && ((C)<0)==((B)<0))		\
    fd.fd_overflow = TRUE;				\
}
***/

#define SUBCHK(A,B,C,OFLO)			\
{ int m_diff = (A)-(B);				\
  if (IntIsSmall(m_diff)) (C) = m_diff;		\
  else OFLO					\
}

static INLINE long gcd (long c1,long c2)
{
  if (c1 > 1 && c2 > 1) {
    while (c1 != 0 && c2 != 0) {
      c1 %= c2;
      if (c1 != 0) c2 %= c1;
    }
    return c1+c2;
  } else {
    return 1;
  }
}




#define HALFSHIFT (4<<LogSizeOfWord)

/* returns TRUE on overflow */
/* ripped off from wamfunc.c */
static BOOL long_mult ( /* (x, y, productp) */
     WORD x, WORD y, WORD *productp)
{
  UWORD lower, tmp;
  UHALF x0, x1, y0, y1;
  int negated;
  
  if (x>>HALFSHIFT==x>>(HALFSHIFT-1) &&
      y>>HALFSHIFT==y>>(HALFSHIFT-1)) {
    *productp = x*y;
    return FALSE;
  }

  negated = 0;
  if (x<0)
    x = -x, negated ^= 1;
  if (y<0)
    y = -y, negated ^= 1;

  x0 = (UHALF)x;
  x1 = (UHALF)(x>>HALFSHIFT);
  y0 = (UHALF)y;
  y1 = (UHALF)(y>>HALFSHIFT);

  lower = 0L;
  if (x0 && y0) {
    lower = (UWORD)x0*(UWORD)y0;
  }
  if (x1 && y1) return TRUE;
  else if (x0 && y1) {
    tmp = (UWORD)x0*(UWORD)y1 + (lower>>HALFSHIFT);
    if (tmp>>HALFSHIFT)
      return TRUE;
    else
      lower = (tmp<<HALFSHIFT) + (UHALF)lower;
  }
  else if (x1 && y0) {
    tmp = (UWORD)x1*(UWORD)y0 + (lower>>HALFSHIFT);
    if (tmp>>HALFSHIFT)
      return TRUE;
    else
      lower = (tmp<<HALFSHIFT) + (UHALF)lower;
  }
  if ((WORD)lower<0L && (lower<<1L || !negated))
    return TRUE;
  if (negated) lower = -lower;
  *productp = lower;
  return FALSE;
}


/* precond: t1 and t2 are tagged integers */
static TAGGED safe_mul_val ( /* (t1,t2) */
     TAGGED t1, TAGGED t2)
{
  long st = GetSmall(t1);
  long su = GetSmall(t2);
  long prod;
  
  if (!long_mult(st,su,&prod) && IntIsSmall(prod))
    return MakeSmall(prod);
  else
    return (((st<0)^(su<0)) ? Inf : Sup);
}


TAGGED safe_mul ( /* (t1,t2) */
                TAGGED t1, TAGGED t2)
{
  if (TagIsSmall(t1))
    {
      if (TagIsSmall(t2))
	t1 = safe_mul_val(t1,t2);
      else if (Teqz(t1))
	return ERRORTAG;	/* 0*inf -> give up */
      else
	t1 = (Tltz(t1)^(t2==Inf) ? Inf : Sup);
    }
  else if (TagIsSmall(t2))
    {
      if (Teqz(t2))
	t1 = ERRORTAG;		/* inf*0 -> give up */
      else
	t1 = (Tltz(t2)^(t1==Inf) ? Inf : Sup);
    }
  else
    t1 = (t1!=t2 ? Inf : Sup);
  return t1;
}



/* ceiling of quotient */
TAGGED safe_divu ( /* (t1,t2) */
     TAGGED t1, TAGGED t2)
{
  if (TagIsSmall(t1))
    {
      if (TagIsSmall(t2))
	{
	  if (Tnez(t2))
	    {
	      WORD x = t1-TaggedZero;
	      WORD y = t2-TaggedZero;
	      WORD q = (  x==0 ? 0
		       : (x<0)^(y<0) ? x/y 
		       : x>0 ? (x+y-1)/y
		       : (x+y+1)/y
		       );
		    
	      t1 = MakeSmall(q);
	    }
	  else if (Tltz(t1))
	    t1 = Inf;
	  else if (Tgtz(t1))
	    t1 = Sup;
	  else
	    t1 = ERRORTAG;	/* 0/0 -> give up */
	}
      else if (Tnez(t1))	/* +/sup, -/inf -> 1 */
	t1 = (Tltz(t1)^(t2==Inf) ? TaggedZero : TaggedOne);
    }
  else if (TagIsSmall(t2))
    t1 = (Tltz(t2)^(t1==Inf) ? Inf : Sup);
  else
    t1 = ERRORTAG;		/* inf/inf -> give up */
  return t1;
}


/* floor of quotient */
TAGGED safe_divd ( /* (t1,t2) */
     TAGGED t1, TAGGED t2)
{
  if (TagIsSmall(t1))
    {
      if (TagIsSmall(t2))
	{
	  if (Tnez(t2))
	    {
	      WORD x = t1-TaggedZero;
	      WORD y = t2-TaggedZero;
	      WORD q = ( x==0 ? 0
		       : (x<0)^(y>0) ? x/y 
		       : x>0 ? (x-y-1)/y
		       : (x-y+1)/y
		       );
	      
	      t1 = MakeSmall(q);
	    }
	  else if (Tltz(t1))
	    t1 = Inf;
	  else if (Tgtz(t1))
	    t1 = Sup;
	  else
	    t1 = ERRORTAG;	/* 0/0 -> give up */
	}
      else if (Tnez(t1))	/* -/sup, +/inf -> -1 */
	t1 = (Tltz(t1)^(t2==Sup) ? TaggedZero : TaggedMinusOne);
    }
  else if (TagIsSmall(t2))
    t1 = (Tltz(t2)^(t1==Inf) ? Inf : Sup);
  else
    t1 = ERRORTAG;		/* inf/inf -> give up */
  return t1;
}



TAGGED safe_plus ( /* (t1,t2) */
     TAGGED t1, TAGGED t2)
{
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
  return t1;
}



TAGGED safe_minus ( /* (t1,t2) */
     TAGGED t1, TAGGED t2)
{
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
  return t1;
}


/* Optimization: early detection of cases where there is nothing to do. */
#define EARLY_BREAK 1

typedef long TERM;

#define EOL (-1L)

#define SV(I)   (pdata->target[I])

#define COEFF(t) (pdata->term.coeff[t])
#define ABSCOEFF(t) (pdata->term.abscoeff[t])
#define CMIN(t) (pdata->term.cmin[t])
#define CMAX(t) (pdata->term.cmax[t])
#define GCD(t) (pdata->term.gcd[t])
#define CMINSUM(t) (pdata->term.cminsum[t])
#define CMAXSUM(t) (pdata->term.cmaxsum[t])
#define MAXDIFF(t) (pdata->term.maxdiff[t])
#define GCDREST(t) (pdata->term.gcdrest[t])
#define TMIN(t) (pdata->term.min[t])
#define TMAX(t) (pdata->term.max[t])
#define TDOM(t) (pdata->term.domain[t])
#define PRUNED(t) (pdata->term.pruned[t])
#define FANIN(t) (pdata->term.fanin[t])
#define FANOUT(t) (pdata->term.fanout[t])
#define SUPPORT(t) (pdata->term.support[t])
#define OFFSET(t) (pdata->term.offset[t])
#define TRefAttr(T) RefTerm(pdata->refbase + 2*(T))
#define TRef(T) RefTerm(pdata->refbase + 2*(T) + 1)

struct linear_data {
  void (SPCDECL *destructor)(void *);
#if MULTI_SP_AWARE
  SPEnv *spenv;
#endif /* MULTI_SP_AWARE */

  SP_term_ref refbase;
  long stamp;			/* increases up to backtracking */
  long rhs;			/* RHS - valid up to backtracking */
  long gcdall;			/* GCD of all target coeffs */
  int prunings;			/* counter */
  int nvars;			/* #terms */
  int ntargets;			/* #terms that may be targets */
  BOOL fast;			/* TRUE if everything is finite, no oflo */
#if CSTATE_ONLY
  int nsources;			/* #terms that may be sources only */
  long rhs_done;		/* RHS at last commit */
  BOOL fast_done;		/* ditto at last commit */
#endif
  TERM *target;
  struct {
    long offset;
    long lb;
    long ub;
    int nvars;
    int fanout;
    TERM *var;
  } dp;
  struct {
    long *coeff;		/* ai */
    long *abscoeff;		/* abs(ai) */
    long *cmin;			/* min(ai*xi) */
    long *cmax;			/* max(ai*xi) */
    long *gcd;			/* gcd{aj | i!=j} */
    long *cminsum;		/* sum{min(aj*xj) | j>=i} */
    long *cmaxsum;		/* sum{max(aj*xj) | j>=i} */
    long *maxdiff;		/* max{max(aj*xj) - min(aj*xj) | j>=i} */
    long *gcdrest;		/* sliding GCD rule applicable at j>=i */
    long *pruned;		/* pruned this time, volatile */
    TAGGED *min;		/* min(xi), volatile, atomic */
    TAGGED *max;		/* max(xi), volatile, atomic */
    TAGGED *domain;		/* dom(xi), volatile, GC-unsafe in knapsack/3 */
    /* fields for knapsack filtering */
    struct sw_on_key **fanin;
    struct sw_on_key **fanout;
    struct sw_on_key **support;
    long *offset;
  } term;
  /*
  space for arrays
  */
};

static void free_one_dp MAGIC (HIDDEN_PROTO struct linear_data *pdata,TERM t)
{
  sp_checkdealloc((TAGGED *)FANIN(t),
		  sizeof(struct sw_on_key)+
		  (SwitchSize(FANIN(t))-ANY)*sizeof(struct sw_on_key_node)
		  /*MM_SWITCH*/);
  sp_checkdealloc((TAGGED *)FANOUT(t),
		  sizeof(struct sw_on_key)+
		  (SwitchSize(FANOUT(t))-ANY)*sizeof(struct sw_on_key_node)
		  /*MM_SWITCH*/);
  sp_checkdealloc((TAGGED *)SUPPORT(t),
		  sizeof(struct sw_on_key)+
		  (SwitchSize(SUPPORT(t))-ANY)*sizeof(struct sw_on_key_node)
		  /*MM_SWITCH*/);
}



static void free_all_dp MAGIC (HIDDEN_PROTO struct linear_data *pdata)
{
  int i;
  
  for (i=0; i<pdata->dp.nvars; i++)
    free_one_dp(pdata,pdata->dp.var[i]);
  pdata->dp.nvars = 0;
  pdata->dp.fanout = 0;
}




static void SPCDECL linear_destructor(void *pdata_v)
{
  struct linear_data *pdata = (struct linear_data *)pdata_v;
  FD_SETUP_SPENV(pdata->spenv)

  free_all_dp(pdata);
  SP_free_term_refs(pdata->refbase,(pdata->nvars<<1)+2);
  SP_free(pdata);
}


/* for qsorting by descending maxdiff */
static int cmp_desc_maxdiff MAGIC (HIDDEN_PROTO  /* (t1,t2) */
     TERM *t1, TAGGED *t2)
{
  struct linear_data *pdata = fd.gdata;
  long l1 = MAXDIFF(*t1);
  long l2 = MAXDIFF(*t2);

#if MULTI_SP_AWARE
  (void)HIDDEN_ARG;             /* [PM] 3.9b5 avoid -Wunused */
#endif /* MULTI_SP_AWARE */
  
  return (l2<l1 ? -1 : l2>l1);
}

#define QType TERM
#define QCmp  cmp_desc_maxdiff
#define QSort qsort_desc_maxdiff
#include "qsort.ic"

/* Partition targets into ground/nonground, refreshing pdata->rhs.
   Compute GCD of all target coefficients.
   if (eq) compute target[i]->gcd = gcd({a[j] | i!=j}).
*/
static void 
refresh_targets_gcd MAGIC (HIDDEN_PROTO BOOL eq, TAGGED lbt)
{
  struct linear_data *pdata = fd.gdata;
  BOOL gcdrest = FALSE;
  int nonunits = 0;
  long cminsum = 0;
  long cmaxsum = 0;
  long prevdiff = CLPFD_MAXINT;
  BOOL ordered = TRUE;
  long g, rhs;
  int i;
  int delta;
  int sup;
  
  for (sup=0; sup<pdata->ntargets; sup++) {
    TERM elt = SV(sup);

    if (AreSmall(TMIN(elt),TMAX(elt)))
      g = CMAX(elt) - CMIN(elt);
    else
      g = CLPFD_MAXINT;
    MAXDIFF(elt) = g;
    if (prevdiff<g)
      ordered = FALSE;
    prevdiff = g;
  }
  if (!ordered && sup>1)
    qsort_desc_maxdiff(pdata->target, sup);
  for (; sup>0; sup--) {
    TERM elt = SV(sup-1);
    
    if (TMIN(elt)!=TMAX(elt))
      break;
    else
      pdata->rhs -= CMIN(elt);
  }
  delta = sup - pdata->ntargets;
  pdata->ntargets = sup;
#if CSTATE_ONLY
  pdata->nsources -= delta;
#endif
  rhs = pdata->rhs + (eq ? GetSmall(lbt) : 0);
  if (sup>0 && MAXDIFF(SV(0))>1)
    for (i=0; i<sup; i++)
      if (ABSCOEFF(SV(i))>1)
	nonunits++;
  if (nonunits==0 ||
      sup-nonunits>1 ||
      (!eq && sup-nonunits>0)) {
    pdata->gcdall = 1;
    for (i=0; i<sup; i++)
      GCD(SV(i)) = 1;
  } else if (!eq) {		/* GCD(i)=1 but gcdall may be >1 */
    g = ABSCOEFF(SV(0));
    GCD(SV(0)) = 1;
    for (i=1; i<sup; i++) {
      TERM elt = SV(i);
      
      g = gcd(g,ABSCOEFF(elt));
      GCD(elt) = 1;
    }
    pdata->gcdall = g;
  } else if (sup-nonunits==1) {		/* GCD(i)=gcdall=1 except for one i */
    int ix=0;			/* avoid false alarm */
    pdata->gcdall = 1;
    g = -1;
    for (i=0; i<sup; i++) {
      TERM elt = SV(i);
      long ac = ABSCOEFF(elt);

      GCD(elt) = 1;      
      if (ac==1)
	ix = elt;
      else if (g== -1)
	g = ac;
      else
	g = gcd(g,ac);
    }
    GCD(ix) = g;
  } else {	/* for each i, compute GCD of all coefficients except i */
    g = ABSCOEFF(SV(0));
    for (i=1; i<sup; i++) {
      TERM elt = SV(i);
      
      GCD(elt) = g;
      g = gcd(g,ABSCOEFF(elt));
    }
    pdata->gcdall = g;
				/* now, SV(i)->gcd = gcd(a[0] ... a[i-1]) */
    g = ABSCOEFF(SV(sup-1));
    for (i=sup-2; i>0; i--) {
      TERM elt = SV(i);
      
      GCD(elt) = gcd(g,GCD(elt));
      g = gcd(g,ABSCOEFF(elt));
    }
    GCD(SV(0)) = g;
				/* now, SV(i)->gcd = gcd({a[j] | i!=j}) */
  }

  /* refresh gcdrest, cminsum, cmaxsum fields */
  
  for (i=sup-1; i>=0; i--) {
    TERM elt = SV(i);

    if (!gcdrest) {
      g = GCD(elt);
      gcdrest =
	(g>pdata->gcdall &&
	 ((CMIN(elt)-rhs) % g || (CMAX(elt)-rhs) % g));
    }
    GCDREST(elt) = gcdrest;
    if (cminsum > -CLPFD_MAXINT && CMIN(elt) > -CLPFD_MAXINT)
      cminsum += CMIN(elt);
    else
      cminsum = -CLPFD_MAXINT;
    CMINSUM(elt) = cminsum;
    if (cmaxsum < CLPFD_MAXINT && CMAX(elt) < CLPFD_MAXINT)
      cmaxsum += CMAX(elt);
    else
      cmaxsum = CLPFD_MAXINT;
    CMAXSUM(elt) = cmaxsum;
  }
}


/* Preconditions:
   0<coeff<mod, 0=<rhs<mod, 0<mod

   Solve min X such that coeff*X = rhs (modulo mod)
*/
static long solve_gcd(long coeff, long rhs, long mod)
{
  if (rhs==0)
    return 0;
  else {
    long rhs1 = rhs%coeff;
    if (rhs1==0)
      return rhs/coeff;
    else
      return (rhs + mod*solve_gcd(mod%coeff, coeff-rhs1, coeff)) / coeff;
  }
}


/* Preconditions: 
   0<mod, gcdall = gcd(coeff,mod)

   Adjusts minx up and maxx down s.t.
   Returns smallest s.t. coeff*minx = coeff*maxx = rhs (modulo mod)
*/
static void adjust_bounds_gcd(long coeff, long rhs, long mod, long gcdall,
			      long *minx, long *maxx)
{
  long minx0 = *minx, maxx0 = *maxx;
  long q = mod/gcdall;
  long r, x, s;

  if (coeff<0)
    rhs = -rhs, coeff = -coeff;
  coeff = coeff % mod;
  rhs = rhs % mod;
  if (rhs<0) rhs += mod;
  s = solve_gcd(coeff, rhs, mod);
  r = minx0 % q;
  if (r<0) r += q;
  x = minx0 - r + s;
  if (x<minx0) x += q;
  *minx = x;
  r = maxx0 % q;
  if (r<0) r += q;
  x = maxx0 - r + s;
  if (x>maxx0) x -= q;
  *maxx = x;
}


/* This function ensures that:

   Ai * min(Xi) - RHS = Ai * max(Xi) - RHS = 0 (modulo G)

   where G is the gcd of all coefficients excepts Ai.
   Fails if an empty domain is produced.
*/
static int sliding_gcd_rule MAGIC (HIDDEN_PROTO  /* (elt, rhs, g) */
     TERM elt,
     long rhs, long g)
{
  struct linear_data *pdata = fd.gdata;
  TAGGED tmin, tmax, tcmin, tcmax;
  long c = COEFF(elt);
  TAGGED tc = MakeSmall(c);
  long imin, imax;

  do {
    tmin = TMIN(elt);
    tmax = TMAX(elt);
    /*** iterative algorithm
    if (TagIsSmall(tmin)) {
      imin = c*GetSmall(tmin);
      while ((imin - rhs) % g != 0)
	imin+=c, tmin+=IStep(1);
    }
    if (TagIsSmall(tmax)) {
      imax = c*GetSmall(tmax);
      while ((imax - rhs) % g != 0)
	imax-=c, tmax-=IStep(1);
    }
    ***/
    if (AreSmall(tmin,tmax)) {
      tcmin = safe_mul_val(tc,tmin);
      tcmax = safe_mul_val(tc,tmax);
      if (AreSmall(tcmin,tcmax)) {
	if ((GetSmall(tcmin) - rhs) % g != 0 ||
	    (GetSmall(tcmax) - rhs) % g != 0) {
	  imin = GetSmall(tmin);
	  imax = GetSmall(tmax);
	  adjust_bounds_gcd(c, rhs, g, pdata->gcdall, &imin, &imax);
	  tmin = MakeSmall(imin);
	  tmax = MakeSmall(imax);
	  if (tmin==TMIN(elt) && tmax==TMAX(elt))
	    return 0;		/* no pruning */
	  if (!adjust_bounds(tmin, tmax, TDOM(elt), &TMIN(elt), &TMAX(elt)))
	    return -1;		/* failure */
	  pdata->prunings++;
	}
      }
    }
  } while (tmin!=TMIN(elt) || tmax!=TMAX(elt));
  return 1;			/* pruning took place */
}



/* Propagator for linear relations for #=<, #<, #>=, #>,
   and for #= when some variable has an infinite bound.
*/
/* -1=disentailed, 0=true, 1=entailed */
static int fd_linear_general MAGIC (HIDDEN_PROTO
     long lb0,
     int lbsup,
     long lb,
     long ub0,
     int ubsup,
     long ub)
{
  struct linear_data *pdata = fd.gdata;
  long gcdall = pdata->gcdall;
  long ntargets = pdata->ntargets;
  long min, max;
  int i;
  TERM elt;

  for (i=0; i<ntargets; i++) {
    BOOL minok, maxok;
      
    elt = SV(i);
    min = CMIN(elt);
    max = CMAX(elt);
    minok = (max< CLPFD_MAXINT ? (lbsup<0 || (min>=lb+max)) : (lbsup<-1 || (min>=lb)));
    maxok = (min>-CLPFD_MAXINT ? (ubsup>0 || (max<=ub+min)) : (ubsup> 1 || (max<=ub)));
    if (!minok || !maxok || GCD(elt)>gcdall) {
      long a, b, c = COEFF(elt);
      TAGGED tc = MakeSmall(c);
      TAGGED tp;
	  
      if (max>=CLPFD_MAXINT)
	lbsup++;
      else
	lb += max;
      if (min<=-CLPFD_MAXINT)
	ubsup--;
      else
	ub += min;
      if (!maxok) {		/* prune upper bound */
	if (c > 0) {
	  b = FLOORDIV(ub,c);
	  if (!adjust_upper_bound(MakeSmall(b), TDOM(elt), &TMIN(elt), &TMAX(elt)))
	    return -1;
	  pdata->prunings++;
	  if (GCD(elt)>gcdall && sliding_gcd_rule(elt, lb0, GCD(elt))<0)
	    return -1;
	  tp = safe_mul_val(tc,TMAX(elt));
	  max = GetSmallSafe(tp);
	} else {
	  b = FLOORDIV(ub,-c);
	  if (!adjust_lower_bound(MakeSmall(-b), TDOM(elt), &TMIN(elt), &TMAX(elt)))
	    return -1;
	  pdata->prunings++;
	  if (GCD(elt)>gcdall && sliding_gcd_rule(elt, lb0, GCD(elt))<0)
	    return -1;
	  tp = safe_mul_val(tc,TMIN(elt));
	  max = GetSmallSafe(tp);
	}
      }
      if (!minok) {		/* prune lower bound */
	if (c > 0) {
	  a = CEILDIV(lb,c);
	  if (!adjust_lower_bound(MakeSmall(a), TDOM(elt), &TMIN(elt), &TMAX(elt)))
	    return -1;
	  pdata->prunings++;
	  if (GCD(elt)>gcdall && sliding_gcd_rule(elt, lb0, GCD(elt))<0)
	    return -1;
	  tp = safe_mul_val(tc,TMIN(elt));
	  min = GetSmallSafe(tp);
	} else {
	  a = CEILDIV(lb,-c);
	  if (!adjust_upper_bound(MakeSmall(-a), TDOM(elt), &TMIN(elt), &TMAX(elt)))
	    return -1;
	  pdata->prunings++;
	  if (GCD(elt)>gcdall && sliding_gcd_rule(elt, lb0, GCD(elt))<0)
	    return -1;
	  tp = safe_mul_val(tc,TMAX(elt));
	  min = GetSmallSafe(tp);
	}
      }
      if (min<=-CLPFD_MAXINT)
	ubsup++;
      else
	ub -= min;
      if (max>=CLPFD_MAXINT)
	lbsup--;
      else
	lb -= max;
      if (CMIN(elt)!=min || CMAX(elt)!=max) { /* pruned */
	CMIN(elt) = min;
	CMAX(elt) = max;
	PRUNED(elt) = 1;
      }
    }
#if EARLY_BREAK
    else if (ubsup-lbsup==0 &&
	     MAXDIFF(elt)<=(-lb < ub ? -lb : ub) &&
	     !GCDREST(elt))
      break;
#endif
  }
  /* Entailed iff lb0 <= \sum \min(a_i x_i) AND \sum \max(a_i x_i) <= ub0
     I.e.     iff lb0 <= ub0-ub             AND ub0 >= lb0-lb
  */
  if (lb0<=-CLPFD_MAXINT)
    lbsup++;
  else
    lb -= lb0;
  if (ub0>=CLPFD_MAXINT)
    ubsup--;
  else
    ub -= ub0;
  return ((lb0<=-CLPFD_MAXINT || (ubsup==0 && ub<=-lb0)) &&
	  (ub0>= CLPFD_MAXINT || (lbsup==0 && lb>=-ub0)));
}


/* Propagator for #= when all variables have finite bounds,
   but some variable has a non-unit coefficient or a non-interval domain. */
/* -1=disentailed, 0=true, 1=entailed */
static BOOL fd_linear_fast MAGIC (HIDDEN_PROTO
     long lb0, long lb, long ub0, long ub)
{
  struct linear_data *pdata = fd.gdata;
  long gcdall = pdata->gcdall;
  long ntargets = pdata->ntargets;
  long min, max, threshold;
  int i;
  TERM elt;

  threshold = (-lb < ub ? -lb : ub);
  for (i=0; i<ntargets; i++) {
    elt = SV(i);
    min = CMIN(elt);
    max = CMAX(elt);
    if (max-min>threshold || GCD(elt)>gcdall) {
      long a, b, c = COEFF(elt);

      lb += max;
      ub += min;
      if (c > 0) {
	a = CEILDIV(lb,c);
	b = FLOORDIV(ub,c);
	if (!adjust_bounds(MakeSmall(a), MakeSmall(b), TDOM(elt), &TMIN(elt), &TMAX(elt)))
	  return -1;
	pdata->prunings++;
	if (GCD(elt)>gcdall && sliding_gcd_rule(elt, lb0, GCD(elt))<0)
	  return -1;
	min = c*GetSmall(TMIN(elt));
	max = c*GetSmall(TMAX(elt));
      } else {
	a = CEILDIV(lb,-c);
	b = FLOORDIV(ub,-c);
	if (!adjust_bounds(MakeSmall(-b), MakeSmall(-a), TDOM(elt), &TMIN(elt), &TMAX(elt)))
	  return -1;
	pdata->prunings++;
	if (GCD(elt)>gcdall && sliding_gcd_rule(elt, lb0, GCD(elt))<0)
	  return -1;
	max = c*GetSmall(TMIN(elt));
	min = c*GetSmall(TMAX(elt));
      }
      if (CMIN(elt)!=min || CMAX(elt)!=max) { /* pruned */
	CMIN(elt) = min;
	CMAX(elt) = max;
	PRUNED(elt) = 1;
      }
      lb -= max;
      ub -= min;
      threshold = (-lb < ub ? -lb : ub);
    }
#if EARLY_BREAK
    else if (MAXDIFF(elt)<=threshold && !GCDREST(elt))
      break;
#endif
  }
  /* Entailed iff lb0 <= \sum \min(a_i x_i) AND \sum \max(a_i x_i) <= ub0
     I.e.     iff lb0 <= ub0-ub             AND ub0 >= lb0-lb
  */
  lb -= lb0;
  ub -= ub0;
  return (ub<=-lb0 && lb>=-ub0);
}


/* Propagator for #= when all variables have finite bounds,
   unit coefficients, and interval domains. */   
/* -1=disentailed, 0=true, 1=entailed */
static BOOL fd_linear_unit MAGIC (HIDDEN_PROTO
     long lb0, long lb, long ub0, long ub)
{
  struct linear_data *pdata = fd.gdata;
  long ntargets = pdata->ntargets;
  long min, max, threshold;
  TERM elt;
  int i;

  threshold = (-lb < ub ? -lb : ub);
  for (i=0; i<ntargets; i++) {
    elt = SV(i);
    min = CMIN(elt);
    max = CMAX(elt);
    if (max-min > threshold) {
      long c = COEFF(elt);	/* 1 or -1 */
      BOOL pruned = FALSE;

      lb += max;
      ub += min;
      if (c > 0) {
	TAGGED tmin = MakeSmall(lb);
	TAGGED tmax = MakeSmall(ub);

	if (Tgt(tmin,TMIN(elt))) {
	  TMIN(elt) = tmin;
	  CMIN(elt) = min = lb;
	  pruned = 1;
	}
	if (Tlt(tmax,TMAX(elt))) {
	  TMAX(elt) = tmax;
	  CMAX(elt) = max = ub;
	  pruned = 1;
	}
      } else {
	TAGGED tmin = MakeSmall(-ub);
	TAGGED tmax = MakeSmall(-lb);

	if (Tgt(tmin,TMIN(elt))) {
	  TMIN(elt) = tmin;
	  CMAX(elt) = max = ub;
	  pruned = 1;
	}
	if (Tlt(tmax,TMAX(elt))) {
	  TMAX(elt) = tmax;
	  CMIN(elt) = min = lb;
	  pruned = 1;
	}
      }
      lb -= max;
      ub -= min;
      if (pruned) {
	if (Tgt(TMIN(elt),TMAX(elt)))
	  return -1;
	PRUNED(elt) = 1;
	threshold = (-lb < ub ? -lb : ub);
      }
    }
#if EARLY_BREAK
    else if (MAXDIFF(elt)<=threshold)
      break;
#endif
  }
  /* Entailed iff lb0 <= \sum \min(a_i x_i) AND \sum \max(a_i x_i) <= ub0
     I.e.     iff lb0 <= ub0-ub             AND ub0 >= lb0-lb
  */
  lb -= lb0;
  ub -= ub0;
  return (ub<=-lb0 && lb>=-ub0);
}

/* Propagator when all variables must be set to their min or max
   value.  Always entailed. */   
static BOOL fd_linear_adjust MAGIC (HIDDEN_PROTO
				    BOOL adjust_min)
{
  struct linear_data *pdata = fd.gdata;
  long ntargets = pdata->ntargets;
  int i;

  pdata->prunings += ntargets;
  for (i=0; i<ntargets; i++) {
    TERM elt = SV(i);
    
    PRUNED(elt) = 1;
    if (adjust_min)
      CMIN(elt) = CMAX(elt);
    else
      CMAX(elt) = CMIN(elt);
    if (adjust_min == (COEFF(elt)>0))
      TMIN(elt) = TMAX(elt);
    else
      TMAX(elt) = TMIN(elt);
  }
  return 1;
}

#define CMul(C,U) (C*GetSmall(U))

/* with speedups */
static int scalar_product_filter MAGIC (HIDDEN_PROTO Argdecl,
				 long state_stamp,
				 TAGGED lbt, TAGGED ubt,
				 int *max_prunedp)
{
  struct linear_data *pdata = fd.gdata;
  BOOL eq;			/* TRUE if it's an equation (more pruning possible) */
  BOOL neq;			/* TRUE if it's a disequation */
  BOOL unit;			/* TRUE if all coeffs in [-1,1] */
  BOOL convex;			/* TRUE if all domains without holes */
  BOOL gcdrest;			/* TRUE if sliding GCD rule applicable */
  int i, ent;
  long lb0, lb, ub0, ub, maxdiff, rhs;
  int lbsup, ubsup;
  TERM elt;
				/* refresh min, max, cmin, cmax, dom */
  eq = (lbt==ubt);
  neq = (!eq && !TagIsSmall(lbt) && !TagIsSmall(ubt));
  for (i=0; i<pdata->ntargets; i++) {
    TAGGED min, max, t1;
    long c;

    elt = SV(i);
    c = COEFF(elt);
    t1 = TRefAttr(elt);	/* get attribute */
    DerefAttribute(t1,t1);	/* get dom/4 */
    min = DomainMin(t1);
    max = DomainMax(t1);
    if (pdata->fast) {
      if (min!=TMIN(elt)) {
	TMIN(elt) = min;
	if (c>0)
	  CMIN(elt) = CMul(c,min);
	else
	  CMAX(elt) = CMul(c,min);
      }
      if (max!=TMAX(elt)) {
	TMAX(elt) = max;
	if (c>0)
	  CMAX(elt) = CMul(c,max);
	else
	  CMIN(elt) = CMul(c,max);
      }
    } else {
      TAGGED tc, tp;
      tc = MakeSmall(c);
      if (min!=TMIN(elt)) {
	TMIN(elt) = min;

	if (c>0) {
	  if (TagIsSmall(min)) {
	    tp = safe_mul_val(tc,min);
	    CMIN(elt) = GetSmallSafe(tp);
	  } else {
	    CMIN(elt) = -CLPFD_MAXINT;
	  }
	} else {
	  if (TagIsSmall(min)) {
	    tp = safe_mul_val(tc,min);
	    CMAX(elt) = GetSmallSafe(tp);
	  } else {
	    CMAX(elt) = CLPFD_MAXINT;
	  }
	}
      }
      if (max!=TMAX(elt)) {
	TMAX(elt) = max;

	if (c>0) {
	  if (TagIsSmall(max)) {
	    tp = safe_mul_val(tc,max);
	    CMAX(elt) = GetSmallSafe(tp);
	  } else {
	    CMAX(elt) = CLPFD_MAXINT;
	  }
	} else {
	  if (TagIsSmall(max)) {
	    tp = safe_mul_val(tc,max);
	    CMIN(elt) = GetSmallSafe(tp);
	  } else {
	    CMIN(elt) = -CLPFD_MAXINT;
	  }
	}
      }
    }
    PRUNED(elt) = 0;
    TDOM(elt) = DomainSet(t1);
  }
  pdata->stamp = state_stamp+1;
 restart:
  refresh_targets_gcd(eq,lbt);
  ent = -1;
  unit = convex = TRUE;
  rhs = pdata->rhs;
  maxdiff = 0;
  gcdrest = FALSE;
  
  if (TagIsSmall(lbt))
    lbsup = 0, lb0 = lb = rhs+GetSmall(lbt);
  else
    lbsup = -1, lb0 = -CLPFD_MAXINT, lb = 0;
  if (TagIsSmall(ubt))
    ubsup = 0, ub0 = ub = rhs+GetSmall(ubt);
  else
    ubsup = 1, ub0 = CLPFD_MAXINT, ub = 0;
  if (neq && pdata->ntargets>1) {		/* no pruning possible */
    ent = 0;
    goto ret;
  }
  				/* adjust the bounds; detect failure */
  if (pdata->gcdall>1) {
    lb = CEILDIV(lb,pdata->gcdall)*pdata->gcdall;
    ub = FLOORDIV(ub,pdata->gcdall)*pdata->gcdall;
    if (ubsup-lbsup==0 && lb>ub)
      goto ret;
  }
   				/* setup for fixpoint algorithm */
  for (i=0; i<pdata->ntargets; i++) {
    long c, d;
      
    elt = SV(i);
    if (MAXDIFF(elt)<CLPFD_MAXINT) { /* short cut the loop */
      if (maxdiff<CLPFD_MAXINT)
	maxdiff = MAXDIFF(elt);
      if (eq)
	gcdrest |= GCDREST(elt);
      if (pdata->fast) {
	ub -= CMINSUM(elt);
	lb -= CMAXSUM(elt);
      } else {
	SUBCHK(ub,CMINSUM(elt),ub,ubsup++;);
	SUBCHK(lb,CMAXSUM(elt),lb,lbsup--;);
      }
      while (i<pdata->ntargets && (unit || convex)) {
	elt = SV(i);
	if (MAXDIFF(elt)==1)
	  break;
	if (ABSCOEFF(elt)!=1)
	  unit = FALSE;
	if (CTagToCdr(TDOM(elt))!=EmptySet)
	  convex = FALSE;
	i++;
      }
      break;
    }
    c = COEFF(elt);
    if (pdata->fast) {
      if (c>0) {
	long min, max;
      
	CMIN(elt) = min = CMul(c,TMIN(elt));
	ub -= min;
	CMAX(elt) = max = CMul(c,TMAX(elt));
	lb -= max;
      } else {
	long min, max;

	CMAX(elt) = min = CMul(c,TMIN(elt));
	lb -= min;
	CMIN(elt) = max = CMul(c,TMAX(elt));
	ub -= max;
      }
    } else {
      TAGGED tc, tp;
      tc = MakeSmall(c);
      if (c>0) {
	long min, max;
      
	if (TagIsSmall(TMIN(elt))) {
	  tp = safe_mul_val(tc,TMIN(elt));
	  CMIN(elt) = min = GetSmallSafe(tp);
	  SUBCHK(ub,min,ub,ubsup++;);
	} else {
	  CMIN(elt) = -CLPFD_MAXINT;
	  ubsup++;
	}
	if (TagIsSmall(TMAX(elt))) {
	  tp = safe_mul_val(tc,TMAX(elt));
	  CMAX(elt) = max = GetSmallSafe(tp);
	  SUBCHK(lb,max,lb,lbsup--;);
	} else {
	  CMAX(elt) = CLPFD_MAXINT;
	  lbsup--;
	}
      } else {
	long min, max;

	if (TagIsSmall(TMIN(elt))) {
	  tp = safe_mul_val(tc,TMIN(elt));
	  CMAX(elt) = min = GetSmallSafe(tp);
	  SUBCHK(lb,min,lb,lbsup--;);
	} else {
	  CMAX(elt) = CLPFD_MAXINT;
	  lbsup--;
	}
	if (TagIsSmall(TMAX(elt))) {
	  tp = safe_mul_val(tc,TMAX(elt));
	  CMIN(elt) = max = GetSmallSafe(tp);
	  SUBCHK(ub,max,ub,ubsup++;);
	} else {
	  CMIN(elt) = -CLPFD_MAXINT;
	  ubsup++;
	}
      }
    }
    if (c!=1 && c!= -1)
      unit = FALSE;
    if (CTagToCdr(TDOM(elt))!=EmptySet)
      convex = FALSE;
    if (AreSmall(TMIN(elt),TMAX(elt))) {
      d = CMAX(elt) - CMIN(elt);
      if (maxdiff<d) maxdiff = d;
    } else
      maxdiff = CLPFD_MAXINT;
    if (eq)
      gcdrest |= GCDREST(elt);
  }
  if (ubsup==lbsup)
    pdata->fast = TRUE;
  
  if (neq) {			/* ntargets=0 or ntargets=1 */
    elt = SV(0);
    if (pdata->ntargets==0)
      ent = (rhs!=0 ? 1 : -1);	/* ground - RHS must not be 0 */
    else if (rhs % ABSCOEFF(elt) != 0) /* RHS not a multiple of coefficient */
      ent = 1;
    else {
      TAGGED tq = MakeSmall(rhs/COEFF(elt));

      if (fd_member(tq,TDOM(elt)))
	request_tell(w, TRefAttr(elt), TRef(elt), fd_compl_interval(tq,tq), 2, 3);
      ent = 1;
    }
  } else if ((lbsup==0 && lb>0) || (ubsup==0 && ub<0)) {
    ;
  } else if (!gcdrest &&
	     (lbsup<-1 || maxdiff <= -lb) &&
	     (ubsup> 1 || maxdiff <=  ub)) {
    /* check entailment */
    if (ubsup-lbsup==0) {
      ent = (ub-ub0<=-lb0 && lb-lb0>=-ub0);
    } else {
      if (lb0==-CLPFD_MAXINT)
	lbsup++;
      else
	lb -= lb0;
      if (ub0==CLPFD_MAXINT)
	ubsup--;
      else
	ub -= ub0;
      ent = ((lb0==-CLPFD_MAXINT || (ubsup==0 && ub<=-lb0)) &&
	     (ub0== CLPFD_MAXINT || (lbsup==0 && lb>=-ub0)));
    }
  } else {
    int prunings = pdata->prunings;

    /*** PRM 2211
    if (!IntIsSmall(lb) || !IntIsSmall(ub))
      fd.fd_overflow = TRUE;
    ***/
    if (lb==0 && lbsup==0)
      ent = fd_linear_adjust(TRUE);
    else if (ub==0 && ubsup==0)
      ent = fd_linear_adjust(FALSE);
    else if (ubsup-lbsup!=0)
      ent = fd_linear_general(lb0,lbsup,lb,ub0,ubsup,ub);
    else if (unit && convex)
      ent = fd_linear_unit(lb0,lb,ub0,ub);
    else
      ent = fd_linear_fast(lb0,lb,ub0,ub);
    if (*max_prunedp<pdata->ntargets)
      *max_prunedp = pdata->ntargets;
    if (eq && ent==0 && prunings!=pdata->prunings)
      goto restart;
  }
 ret:
  return ent;
}

static struct linear_data *
alloc_frame MAGIC (HIDDEN_PROTO Argdecl, int nvars, long state_stamp, TAGGED handle, TAGGED trhs)
{
  int total_size = 
    11*nvars*sizeof(long) +
    3*nvars*sizeof(TAGGED) +
    3*nvars*sizeof(int *) +
    2*nvars*sizeof(TERM);
  struct linear_data *pdata =
    Palloc(struct linear_data, total_size, handle);
  char *ptr = (char *)(pdata+1);
  
  pdata->target = (TERM *)ptr;
  ptr = (char *)(pdata->target+nvars);
  pdata->dp.var = (TERM *)ptr;
  ptr = (char *)(pdata->dp.var+nvars);
  /* arrays for term variables */
  pdata->term.coeff = (long *)ptr;
  ptr = (char *)(pdata->term.coeff+nvars);
  pdata->term.abscoeff = (long *)ptr;
  ptr = (char *)(pdata->term.abscoeff+nvars);
  pdata->term.cmin = (long *)ptr;
  ptr = (char *)(pdata->term.cmin+nvars);
  pdata->term.cmax = (long *)ptr;
  ptr = (char *)(pdata->term.cmax+nvars);
  pdata->term.gcd = (long *)ptr;
  ptr = (char *)(pdata->term.gcd+nvars);
  pdata->term.cminsum = (long *)ptr;
  ptr = (char *)(pdata->term.cminsum+nvars);
  pdata->term.cmaxsum = (long *)ptr;
  ptr = (char *)(pdata->term.cmaxsum+nvars);
  pdata->term.maxdiff = (long *)ptr;
  ptr = (char *)(pdata->term.maxdiff+nvars);
  pdata->term.gcdrest = (long *)ptr;
  ptr = (char *)(pdata->term.gcdrest+nvars);
  pdata->term.pruned = (long *)ptr;
  ptr = (char *)(pdata->term.pruned+nvars);
  pdata->term.min = (TAGGED *)ptr;
  ptr = (char *)(pdata->term.min+nvars);
  pdata->term.max = (TAGGED *)ptr;
  ptr = (char *)(pdata->term.max+nvars);
  pdata->term.domain = (TAGGED *)ptr;
  ptr = (char *)(pdata->term.domain+nvars);
  pdata->term.fanin = (struct sw_on_key **)ptr;
  ptr = (char *)(pdata->term.fanin+nvars);
  pdata->term.fanout = (struct sw_on_key **)ptr;
  ptr = (char *)(pdata->term.fanout+nvars);
  pdata->term.support = (struct sw_on_key **)ptr;
  ptr = (char *)(pdata->term.support+nvars);
  pdata->term.offset = (long *)ptr;
  ptr = (char *)(pdata->term.offset+nvars);
    if (ptr != (char *)(pdata+1)+total_size)
      printf("SHOULDN'T HAPPEN: expected ptr=0x%p, got 0x%p\nitems",
	     (char *)(pdata+1)+total_size, ptr);

  pdata->destructor = linear_destructor;
  FD_STORE_SPENV(pdata->spenv);
  pdata->refbase = SP_alloc_term_refs((nvars<<1)+2);
  pdata->stamp = state_stamp-1; /* don't trust initially */
  pdata->nvars = nvars;
  pdata->ntargets = nvars;
  pdata->rhs = TagIsSmall(trhs) ? GetSmall(trhs) : (fd.fd_overflow=TRUE, 0);
  pdata->fast = FALSE;
#if CSTATE_ONLY
  pdata->nsources = 0;
  pdata->rhs_done = pdata->rhs;
  pdata->fast_done = pdata->fast;
#endif
  pdata->dp.nvars = 0;
  pdata->dp.fanout = 0;
  
  return pdata;
}


/*
  '$fd_linear'(+State0, -State, -Actions).
  State = state(CX,LBoff,UBoff,RHS,Nground,Fast,Handle,Stamp) where CX are all non-ground
  The constraint to be maintained is: CX in [RHS+LBoff,RHS+UBoff]
*/
void SPCDECL
prolog_fd_linear MAGIC (HIDDEN_PROTO
                        SP_term_ref State0,
                        SP_term_ref State,
                        SP_term_ref Actions)
{
  WAMENV;
  TAGGED lboff, uboff, tvec, telt, handle, t1;
  int max_pruned = 0;		/* pdata->ntargets at last pruning */
  BOOL committed;		/* TRUE if state can't be backtracked over */
  int nvars=0, i;
  long state_stamp;
  int ent;
  TERM elt;
  struct linear_data *pdata;

  (void)State0;                 /* [PM] 3.9b5 avoid -Wunused */

  w->numstack_end = NULL;
  fd.fd_overflow = FALSE;
/*    X(0) = RefTerm(State0); */
  X(2) = atom_nil;		/* actions list */
  RefTerm(State) = unify_output_state(w,&handle,&state_stamp,&committed);
  DerefArg(lboff,X(0),2);
  DerefArg(uboff,X(0),3);

  if (!IsVar(handle)) {		/* got [Flag | '$free'(Ptr)] */
    pdata = Pdata(struct linear_data,handle);
    nvars = pdata->nvars;
  } else {			/* build persistent state */
    committed = 2;		/* NOTE!  Obsolete? */
    DerefArg(tvec,X(0),1);	/* get CX0 */
    while (TagIsLST(tvec)) {	/* count terms */
      DerefCdr(tvec,tvec);
      nvars++;
    }
    DerefArg(tvec,X(0),1);	/* get CX0 */
    DerefArg(telt,X(0),4);	/* get RHS */
    pdata = alloc_frame(w,nvars,state_stamp,handle,telt);
				/* xfer all info to the struct linear_terms */
    for (i=0; i<nvars; i++) {
      elt = i;
      DerefCar(telt,tvec);
      DerefCdr(tvec,tvec);
      DerefArg(t1,telt,1);
      COEFF(elt) = GetSmall(t1);
      ABSCOEFF(elt) = ABS(COEFF(elt));
      DerefArg(t1,telt,2);	/* get domain var */
      TRef(elt) = t1;
      DerefArg(t1,telt,3);	/* get attribute */
      TRefAttr(elt) = t1;
      TMIN(elt) = TMAX(elt) = TaggedZero;
      CMIN(elt) = CMAX(elt) = 0;
      GCDREST(elt) = FALSE;
      SV(i) = elt;
    }
  }

				/* RESUME HERE */
  fd.gdata = pdata;
  if (state_stamp != pdata->stamp) {
#if CSTATE_ONLY
    pdata->rhs = pdata->rhs_done;
    pdata->ntargets += pdata->nsources;
    pdata->nsources = 0;
    pdata->fast = pdata->fast_done;
#else
    DerefArg(telt,X(0),4);
    pdata->rhs = GetSmall(telt);
    DerefArg(telt,X(0),5);
    pdata->ntargets = nvars-GetSmall(telt);
    DerefArg(telt,X(0),6);
    pdata->fast = GetSmall(telt);
#endif
  }
  ent = scalar_product_filter(w,state_stamp,lboff,uboff,&max_pruned);
  if (ent<0) goto ret;
  for (i=0; i<max_pruned; i++) {
    elt = SV(i);
    if (PRUNED(elt))
      request_tell_interval(w, TRefAttr(elt), TRef(elt), TMIN(elt), TMAX(elt), 2, 3);
  }  /* check invariants
  for (i=0; i<pdata->nvars; i++) {
    long c;
    
    elt = i;
    c = COEFF(elt);
    if ((c>0 && MULT_LB(c,TMIN(elt)) != CMIN(elt)) ||
	(c>0 && MULT_UB(c,TMAX(elt)) != CMAX(elt)) ||
	(c<0 && MULT_LB(c,TMIN(elt)) != CMAX(elt)) ||
	(c<0 && MULT_UB(c,TMAX(elt)) != CMIN(elt)))
      printf("shouldnt happen\n");
  }
  */
#if CSTATE_ONLY
  if (committed) {
    pdata->nsources = 0;
    pdata->rhs_done = pdata->rhs;
    pdata->fast_done = pdata->fast;
  }
#else
  CTagToArg(X(0),4) = MakeSmall(pdata->rhs);
  CTagToArg(X(0),5) = MakeSmall(nvars-pdata->ntargets);
  CTagToArg(X(0),6) = MakeSmall(pdata->fast);
#endif
  /* rewrite "A*X1 - A*X2 = 0" to "X1 = X2" */
  if (ent==0 && lboff==uboff && pdata->ntargets==2 && pdata->rhs==0) {
    TERM elt0 = SV(0);
    TERM elt1 = SV(1);

    if (COEFF(elt0) == -COEFF(elt1)) {
      request_rewrite_eq(w, TRef(elt0), TRef(elt1), 2, 3);
      ent = 1;
    }
  }
 ret:
  RefTerm(Actions) = request_done(w, ent, 2, 3);
  Pfree(ent==1,handle,pdata);
  if (!fd_check_overflow())
    SP_fail();
}




/*** support for sparse arrays repr. as hash tables ***/

typedef struct ARITER_struct {
  int next, max;
  struct sw_on_key *sw;
  struct sw_on_key_node *cur;
} ARITER;


#define ariter_empty(it) ((it)->next>=(it)->max)

INLINE static void
ariter_init (ARITER *it, struct sw_on_key *sw)
{
  int i;

  it->sw = sw;
  it->cur = NULL;
  it->next = -1;
  it->max = SwitchSize(sw);
  for (i=0; i<it->max && sw->tab.asnode[i].value.arities==0; i++)
    ;
  it->next = i;
}


INLINE static struct sw_on_key_node *
ariter_next (ARITER *it)
{
  int i;

  it->cur = &it->sw->tab.asnode[it->next];
  for (i=it->next+1; i<it->max && it->sw->tab.asnode[i].value.arities==0; i++)
    ;
  it->next = i;
  return it->cur;
}


INLINE static void
clear_array (struct sw_on_key *sw)
{
  int i = SwitchSize(sw);

  for (--i; i>=0; --i) {
    sw->tab.asnode[i].key = 0;
    sw->tab.asnode[i].value.arities = 0;
  }
}


/* ripped off from expand_sw_on_key */
static void
maybe_rehash MAGIC (HIDDEN_PROTO struct sw_on_key **psw)
{
  struct sw_on_key_node *h1;
  int size = SwitchSize(*psw);
  int newsize = 4;
  int pop = 0;
  struct sw_on_key *newsw;
  int j;
  
  if (size==4)
    return;
  for (j=0; j<size; j++) {
    h1 = &(*psw)->tab.asnode[j];
    if (h1->key && h1->value.try)
      pop++;
  }
  for (j=3; pop>j; j<<=1)
    newsize<<=1;
  if (newsize==size)
    return;
  newsw = new_switch_on_key(newsize,NULL);
  newsw->count = pop;
  for (j=0; j<size; j++) {
    TAGGED t;
    /*
    struct try_node *p = NULL;
    struct sw_on_key_node *h2;
    */
    h1 = &(*psw)->tab.asnode[j];
    if ((t=h1->key) && (/*p=*/h1->value.try)) {
      *incore_gethash(newsw,t) = *h1;
      /*
	h2 = incore_gethash(newsw,t);
	h2->key = t;
	h2->value.try = p;
      */
    }
  }
  sp_checkdealloc((TAGGED *)(*psw),
		  sizeof(struct sw_on_key)+
		  (size-ANY)*sizeof(struct sw_on_key_node)
		  /*MM_SWITCH*/);
  *psw = newsw;
}



#define AREF(Array,Index) (incore_gethash(Array,MakeSmall(Index))->value.arities)
#define ASET(Array,Index,Value) (dyn_puthash(&(Array),MakeSmall(Index))->value.arities=(Value))
#define AINC(Array,Index,Inc) (dyn_puthash(&(Array),MakeSmall(Index))->value.arities+=(Inc))

static void knapsack_filter_noninc MAGIC (HIDDEN_PROTO long base, long lb, long ub)
{
  struct linear_data *pdata = fd.gdata;
  long nvars = pdata->ntargets;
  FDITER it;
  ARITER ai;
  long v;
  int i;
  TERM last = SV(nvars-1);

  free_all_dp(pdata);
  for (i=0; i<nvars; i++)
    pdata->dp.var[i] = SV(i);

  for (i=0; i<nvars; i++) {
    TERM elt = pdata->dp.var[i];
    
    FANIN(elt) = new_switch_on_key(4, NULL);
    FANOUT(elt) = new_switch_on_key(4, NULL);
    SUPPORT(elt) = new_switch_on_key(4, NULL);
    OFFSET(elt) = 0;
  }

  /* forward phase */
  for (i=0; i<nvars; i++) {
    TERM prev = pdata->dp.var[i-1];
    TERM elt = pdata->dp.var[i];
    long c = COEFF(elt);
    long d, cd;
    TAGGED td;
    TAGGED tmin=TMIN(elt);
    TAGGED tmax=TMAX(elt);
    long lb1 = lb - (i==nvars-1 ? 0 : CMAXSUM(SV(i+1)));
    long ub1 = ub - (i==nvars-1 ? 0 : CMINSUM(SV(i+1)));
    long lb2, ub2;

    if (i==0) {
      lb2 = CEILDIV(lb1-base,c);
      ub2 = FLOORDIV(ub1-base,c);
      fditer_init(&it, TDOM(elt));
      while (!fditer_empty(&it)) {
	td = fditer_next(&it);
	d = GetSmall(td);
	if (Tlt(td,tmin) || d<lb2) continue;
	if (Tgt(td,tmax) || d>ub2) break;
	cd = c*d;
	pdata->dp.fanout++;
	ASET(FANIN(elt),base+cd,1);
	ASET(SUPPORT(elt),d,1);
      }
    } else {
      ariter_init(&ai, FANIN(prev));
      while (!ariter_empty(&ai)) {
	v = GetSmall(ariter_next(&ai)->key);
	lb2 = CEILDIV(lb1-v,c);
	ub2 = FLOORDIV(ub1-v,c);
	fditer_init(&it, TDOM(elt));
	while (!fditer_empty(&it)) {
	  td = fditer_next(&it);
	  d = GetSmall(td);
	  if (Tlt(td,tmin) || d<lb2) continue;
	  if (Tgt(td,tmax) || d>ub2) break;
	  cd = c*d;
	  AINC(FANOUT(prev),v,1);
	  AINC(FANIN(elt),v+cd,1);
	  AINC(SUPPORT(elt),d,1);
	}
      }
    }
  }

  /* final fanouts */
  pdata->dp.lb = ub+1;
  pdata->dp.ub = lb-1;
  ariter_init(&ai, FANIN(last));
  while (!ariter_empty(&ai)) {
    v = GetSmall(ariter_next(&ai)->key);
    if (v>=lb && v<=ub) {
      ASET(FANOUT(last),v,1);	/* maintain invariant */
      if (pdata->dp.lb>v) pdata->dp.lb = v;
      if (pdata->dp.ub<v) pdata->dp.ub = v;
    }
  }
  pdata->dp.offset = base;
  pdata->dp.nvars = nvars;
}



static void knapsack_filter_inc MAGIC (HIDDEN_PROTO long lb, long ub)
{
  struct linear_data *pdata = fd.gdata;
  long nvars = pdata->dp.nvars;
  long base = pdata->dp.offset;
  FDITER it;
  ARITER ai;
  struct sw_on_key_node *keyval;
  long v;
  int i;
  TERM last = pdata->dp.var[nvars-1];
  long offset = OFFSET(last);

  /* delete arcs for pruned values */
  for (i=0; i<nvars; i++) {
    TERM prev = pdata->dp.var[i-1];
    TERM elt = pdata->dp.var[i];
    long c = COEFF(elt);
    long d, cd;
    TAGGED td;
    TAGGED tmin=TMIN(elt);
    TAGGED tmax=TMAX(elt);
    TAGGED told=EmptySet;
    TAGGED tdel;
    
    /* Compute old domain */
    ariter_init(&ai, SUPPORT(elt));
    while (!ariter_empty(&ai)) {
      v = GetSmall(ariter_next(&ai)->key);
      told = fd_insert_into(MakeSmall(v),told);
    }
    tdel = fd_subtract(told,fd_and_interval(TDOM(elt),tmin,tmax));

    if (i==0) {
      fditer_init(&it, tdel);
      while (!fditer_empty(&it)) {
	td = fditer_next(&it);
	d = GetSmall(td);
	--pdata->dp.fanout;
	ASET(FANIN(elt),base+c*d,0);
	ASET(SUPPORT(elt),d,0);
      }
    } else {
      long offset = OFFSET(prev);
      ariter_init(&ai, FANOUT(prev));
      while (!ariter_empty(&ai)) {
	keyval = ariter_next(&ai);
	v = GetSmall(keyval->key);
	if (AREF(FANIN(prev),v)>0) {
	  if (tdel==EmptySet) continue;
	  fditer_init(&it, tdel);
	} else
	  fditer_init(&it, told);
	while (!fditer_empty(&it)) {
	  td = fditer_next(&it);
	  d = GetSmall(td);
	  cd = c*d+offset;
	  if (v+cd > pdata->dp.ub)
	    break;
	  if (AREF(FANIN(elt),v+cd)>0) {
	    keyval->value.arities--;
	    AINC(FANIN(elt),v+cd,-1);
	    AINC(SUPPORT(elt),d,-1);
	  }
	}
      }
    }
  }

  /* final fanouts */
  clear_array(FANOUT(last));
  pdata->dp.lb = ub+1;
  pdata->dp.ub = lb-1;
  ariter_init(&ai, FANIN(last));
  while (!ariter_empty(&ai)) {
    v = GetSmall(ariter_next(&ai)->key);
    if (v>=lb-offset && v<=ub-offset) {
      ASET(FANOUT(last),v,1);
      if (pdata->dp.lb>v+offset) pdata->dp.lb = v+offset;
      if (pdata->dp.ub<v+offset) pdata->dp.ub = v+offset;
    }
  }
}


/* Dynamic programming based propagator. */
/* -1=disentailed, 0=true, 1=entailed */
static int knapsack_filter MAGIC (HIDDEN_PROTO
                                  long base, 
                                  BOOL incremental,
                                  long *newlb, long *newub, int *max_prunedp)
{
  struct linear_data *pdata = fd.gdata;
  int ent = -1, nvars;
  BOOL pruned = FALSE;
  long lb = *newlb;
  long ub = *newub;
  FDITER it;
  ARITER ai;
  struct sw_on_key_node *keyval;
  long v, oldub;
  int i;

  if (incremental) {
    oldub = pdata->dp.ub;
    knapsack_filter_inc(lb,ub);
  } else {
    oldub = ub;
    knapsack_filter_noninc(base,lb,ub);
  }
  *newlb = lb = pdata->dp.lb;
  *newub = ub = pdata->dp.ub;
  nvars = pdata->dp.nvars;
  base = pdata->dp.offset;
  
  /* backward phase */
  for (i=nvars-1; i>=0; i--) {
    TERM prev = pdata->dp.var[i-1];
    TERM elt = pdata->dp.var[i];
    long c = COEFF(elt);
    long offset = i==0 ? 0 : OFFSET(prev);
    long d, cd;
    TAGGED td;
    TAGGED tmin=TMIN(elt);
    TAGGED tmax=TMAX(elt);

    ariter_init(&ai, FANIN(elt));
    while (!ariter_empty(&ai)) {
      keyval = ariter_next(&ai);
      v = GetSmall(keyval->key);
      if (AREF(FANOUT(elt),v)==0) {
	keyval->value.arities = 0;
	fditer_init(&it, TDOM(elt));
	while (!fditer_empty(&it)) {
	  td = fditer_next(&it);
	  if (Tlt(td,tmin)) continue;
	  if (Tgt(td,tmax)) break;
	  d = GetSmall(td);
	  cd = c*d+offset;
	  if (v-cd<base) continue;
	  if (i==0) {
	    if (v-cd == base) {
	      ASET(SUPPORT(elt),d,0);
	      --pdata->dp.fanout;
	      break;
	    }
	  } else
	    if (AREF(FANOUT(prev),v-cd)>0) {
	      AINC(SUPPORT(elt),d,-1);
	      AINC(FANOUT(prev),v-cd,-1);
	    }
	}
      }
    }
  }
  if (pdata->dp.fanout==0)
    goto ret;

  /* pruning */
  for (i=0, nvars=0; i<pdata->dp.nvars; i++) {
    TERM prev = pdata->dp.var[nvars-1];
    TERM elt = pdata->dp.var[i];
    long offset = nvars==0 ? base : OFFSET(prev);
    long d, cd=0;
    TAGGED td;
    TAGGED tmin=TMIN(elt);
    TAGGED tmax=TMAX(elt);
    int size=0;

    fditer_init(&it, TDOM(elt));
    while (!fditer_empty(&it)) {
      td = fditer_next(&it);
      if (Tlt(td,tmin)) continue;
      if (Tgt(td,tmax)) break;
      d = GetSmall(td);
      if (AREF(SUPPORT(elt),d)==0) {
	pruned = TRUE;
	TDOM(elt) = fd_delete(TDOM(elt),td);
	if (*max_prunedp<pdata->ntargets)
	  *max_prunedp = pdata->ntargets;
      } else {
	size++;
	cd = d*COEFF(elt) + offset;
      }
    }
    if (size==1) {		/* shunt elt */
      if (nvars==0) {
	pdata->dp.offset = base = cd+OFFSET(elt);
	pdata->dp.fanout = AREF(FANOUT(elt),cd);
      } else {
	OFFSET(prev) = cd+OFFSET(elt);
	/* not needed - old entries nonzero iff new entries nonzero */
	/* clear_array(FANOUT(prev)); */
	ariter_init(&ai, FANOUT(elt));
	while (!ariter_empty(&ai)) {
	  keyval = ariter_next(&ai);
	  v = GetSmall(keyval->key);
	  ASET(FANOUT(prev),v-cd,keyval->value.arities);
	}
      }
      free_one_dp(pdata,elt);
    } else {
      pdata->dp.var[nvars++] = elt;
      maybe_rehash(&FANIN(elt));
      maybe_rehash(&FANOUT(elt));
      maybe_rehash(&SUPPORT(elt));
    }
  }
  pdata->dp.nvars = nvars;
  ent = (nvars==0);

#if DBG > 1
  /* invariant 1: preserving flow */
  for (i=0; i<nvars; i++) {
    TERM prev = pdata->dp.var[i-1];
    TERM elt = pdata->dp.var[i];
    long fanout=0, fanin=0, support=0;

    if (i==0)
      fanout = pdata->dp.fanout;
    else
      for (v=base; v<=ub; v++)
	fanout += AREF(FANOUT(prev),v);
    for (v=base; v<=ub; v++)
      fanin += AREF(FANIN(elt),v);
    for (v=0; v<=ub-base; v++)
      support += AREF(SUPPORT(elt),v);

    if (fanin!=fanout || fanin!=support) {
      printf("* FANOUT(%d)=%d, FANIN(%d)=%d, SUPPORT(%d)=%d\n",
	     (int)prev,(int)fanout,(int)elt,(int)fanin,(int)elt,(int)support);
    }
  }

  /* invariant 2: zero in iff zero out */
  for (i=0; i<nvars; i++) {
    TERM elt = pdata->dp.var[i];
    
    for (v=base; v<=ub; v++)
      if (((AREF(FANIN(elt),v)==0)^(AREF(FANOUT(elt),v)==0))
	  /*|| AREF(FANIN(elt),v)<0*/)
	printf("* FANIN(%d,%d)=%d, FANOUT(%d,%d)=%d\n",
	       (int)elt,(int)v,(int)AREF(FANIN(elt),v),(int)elt,(int)v,(int)AREF(FANOUT(elt),v));
  }

  /* invariant 3: correct fanin counts */
  for (i=0; i<nvars; i++) {
    TERM prev = pdata->dp.var[i-1];
    TERM elt = pdata->dp.var[i];
    TAGGED told = EmptySet;
    long c = COEFF(elt);
    long offset = i==0 ? 0 : OFFSET(prev);
    long d, cd;
    TAGGED td;
    
    for (v=0; v<=ub-base; v++)
      if (AREF(SUPPORT(elt),v)>0)
	told = fd_insert_into(MakeSmall(v),told);

    for (v=base; v<=ub; v++) {
      int fanin = 0;
      
      if (AREF(FANIN(elt),v)==0) continue;
      if (i==0) {
	if ((v-base)%c==0 && AREF(SUPPORT(elt),(v-base)/c)>0)
	  fanin++;
      } else {
	fditer_init(&it, told);
	while (!fditer_empty(&it)) {
	  td = fditer_next(&it);
	  d = GetSmall(td);
	  cd = c*d+offset;
	  if (v-cd>=base && AREF(FANOUT(prev),v-cd)>0)
	    fanin++;
	}
      }
      if (AREF(FANIN(elt),v)!=fanin)
	printf("* FANIN(%d,%d)=%d, #predecessors=%d\n",
	       (int)elt,(int)v,(int)AREF(FANIN(elt),v),fanin);
    }
  }    
#endif

  if (ent==0 && pruned)
    refresh_targets_gcd(TRUE,MakeSmall(lb));

 ret:
  return ent;
}


/*
  '$fd_knapsack'(+State0, -State, -Actions).
  State = state(CX,RHS,Con,Nground,Handle,Stamp) where CX are all non-ground
  The constraint to be maintained is: CX+Con in [min(RHS),max(RHS)]
*/
void SPCDECL
prolog_fd_knapsack MAGIC (HIDDEN_PROTO
                          SP_term_ref State0,
                          SP_term_ref State,
                          SP_term_ref Actions)
{
  WAMENV;
  TAGGED tvec, telt, handle, t1, tlb, tub;
  int max_pruned = 0;		/* pdata->ntargets at last pruning */
  BOOL committed;		/* TRUE if state can't be backtracked over */
  BOOL incremental;		/* TRUE if dom, min, max, cmin, cmax need refreshing */
  int nvars=0, i;
  long newlb, newub, state_stamp;
  int ent;
  TERM elt;
  struct linear_data *pdata;

  (void)State0;                 /* [PM] 3.9b5 avoid -Wunused */

  w->numstack_end = NULL;
  fd.fd_overflow = FALSE;

/*    X(0) = RefTerm(State0); */
  X(2) = atom_nil;		/* actions list */
  RefTerm(State) = unify_output_state(w,&handle,&state_stamp,&committed);

  if (!IsVar(handle)) {		/* got [Flag | '$free'(Ptr)] */
    pdata = Pdata(struct linear_data,handle);
    nvars = pdata->nvars;
  } else {			/* build persistent state */
    committed = 2;		/* NOTE!  Obsolete? */
    DerefArg(tvec,X(0),1);	/* get CX0 */
    while (TagIsLST(tvec)) {	/* count terms */
      DerefCdr(tvec,tvec);
      nvars++;
    }
    DerefArg(tvec,X(0),1);	/* get CX0 */
    DerefArg(telt,X(0),3);	/* get Con */
    pdata = alloc_frame(w,nvars,state_stamp,handle,telt);
    pdata->fast = TRUE;
				/* xfer all info to the struct linear_terms */
    for (i=0; i<nvars; i++) {
      elt = i;
      DerefCar(telt,tvec);
      DerefCdr(tvec,tvec);
      DerefArg(t1,telt,1);
      COEFF(elt) = GetSmall(t1);
      ABSCOEFF(elt) = ABS(COEFF(elt));
      DerefArg(t1,telt,2);	/* get domain var */
      TRef(elt) = t1;
      DerefArg(t1,telt,3);	/* get attribute */
      TRefAttr(elt) = t1;
      TMIN(elt) = TMAX(elt) = TaggedZero;
      CMIN(elt) = CMAX(elt) = 0;
      GCDREST(elt) = FALSE;
      SV(i) = elt;
    }
  }
  DerefArg(telt,X(0),2);	/* get RHS */
  DerefArg(t1,telt,1);		/* get domain var */
  TRef(nvars) = t1;
  DerefArg(telt,telt,2);	/* get attribute */
  TRefAttr(nvars) = telt;
  DerefAttribute(telt,telt);	/* get dom/4 term */

				/* RESUME HERE */
  fd.gdata = pdata;
  incremental = (state_stamp == pdata->stamp);
  if (!incremental) {
#if CSTATE_ONLY
    pdata->rhs = pdata->rhs_done;
    pdata->ntargets += pdata->nsources;
    pdata->nsources = 0;
#else
    DerefArg(telt,X(0),3);
    pdata->rhs = GetSmall(telt);
    DerefArg(telt,X(0),4);
    pdata->ntargets = nvars-GetSmall(telt);
#endif
  }
  ent = scalar_product_filter(w,state_stamp,
			      DomainMin(telt), DomainMax(telt),
			      &max_pruned);
  if (ent < 0)
    goto ret;
  else if (pdata->ntargets>0) {
    newlb = GetSmall(DomainMin(telt));
    newub = GetSmall(DomainMax(telt));
    ent = knapsack_filter(-pdata->rhs,
			  incremental,
			  &newlb, &newub, &max_pruned);
  } else {
    ent = 1;
    newlb = newub = -pdata->rhs;
  }
  for (i=0; i<max_pruned; i++) {
    elt = SV(i);
    if (!OnHeap(TagToLST(TDOM(elt)))) /* TODO: fd_compare case analysis */
      TDOM(elt) = fd_localize(w,fd_and_interval(TDOM(elt),TMIN(elt),TMAX(elt)));
    else
      TDOM(elt) = ERRORTAG;
  }

  tlb = MakeSmall(newlb);
  tub = MakeSmall(newub);
  if (Tgt(tlb,DomainMin(telt)) || Tlt(tub,DomainMax(telt)))
    request_tell_interval(w, TRefAttr(nvars), TRef(nvars), tlb, tub, 2, 3);
  for (i=0; i<max_pruned; i++) {
    elt = SV(i);
    if (TDOM(elt)!=ERRORTAG)
      request_tell(w, TRefAttr(elt), TRef(elt), TDOM(elt), 2, 3);
    else if (PRUNED(elt))
      request_tell_interval(w, TRefAttr(elt), TRef(elt), TMIN(elt), TMAX(elt), 2, 3);
  }
#if CSTATE_ONLY
  if (committed) {
    pdata->nsources = 0;
    pdata->rhs_done = pdata->rhs;
  }
#else
  CTagToArg(X(0),3) = MakeSmall(pdata->rhs);
  CTagToArg(X(0),4) = MakeSmall(nvars-pdata->ntargets);
#endif
 ret:
  Pfree(ent==1,handle,pdata);
  RefTerm(Actions) = request_done(w, ent, 2, 3);
  if (!fd_check_overflow())
    SP_fail();
}


#define CEILSQRT \
  { \
    long m = GetSmall(ymin); \
    if (m>=3) \
      {			/* m = ceiling(sqrt(m)) */ \
	long n1 = m>>1, n2 = m; \
	while (n1<m) \
	  m = n1, \
	  n1 = (n1 + n2/n1)>>1; \
	if (m<n1 || m*m < n2) \
	  m++; \
      } \
    sqrt_ymin = MakeSmall(m); \
    ymin_align = MakeSmall(m*m); \
  }

#define FLOORSQRT \
    { \
      long n = GetSmall(ymax); \
      if (n>=2) \
	{			/* n = floor(sqrt(n)) */ \
	  long n1 = n>>1, n2 = n; \
 \
	  while (n1<n) \
	    n = n1, \
	    n1 = (n1 + n2/n1)>>1; \
	} \
      sqrt_ymax = MakeSmall(n); \
    }

#define XMutR (r+0)
#define XR (r+1)
#define YMutR (r+2)
#define YR (r+3)
#define ZMutR (r+4)
#define ZR (r+5)

/*
  '$fd_square'(+X, +XMut, +Y, +YMut, -Actions).
*/
void SPCDECL
prolog_fd_square MAGIC (HIDDEN_PROTO
			SP_term_ref State,
			SP_term_ref NewState,
			SP_term_ref Actions)
{
  WAMENV;
  TAGGED x, xmut, y, ymut, *arg;
  TAGGED xmin, xmax, xdom, ymin, ymax, ydom, newmin=0, newmax=0;
  TAGGED sqrt_ymin, sqrt_ymax, ymin_align;
  SP_term_ref r = SP_new_term_refs(4);
  unsigned int xflag=0, yflag=0; /* 1=consistent, 2=pruned */
  int ent = -1;			/* disentailed unless otherwise */

  w->numstack_end = NULL;
  fd.fd_overflow = FALSE;
  RefTerm(NewState) = RefTerm(State);
  X(2) = atom_nil;		/* actions list */
  DerefNonvar(X(0));
  DerefArg(x,X(0),1);
  DerefArg(xmut,X(0),2);
  DerefArg(y,X(0),3);
  DerefArg(ymut,X(0),4);
  RefTerm(XR) = x;
  RefTerm(XMutR) = xmut;
  RefTerm(YR) = y;
  RefTerm(YMutR) = ymut;
  DerefNonvar(xmut);
  DerefAttribute(xmut,xmut);
  arg = TagToArg(xmut,0);
  xdom = arg[1];
  xmin = arg[2];
  xmax = arg[3];
  DerefNonvar(ymut);
  DerefAttribute(ymut,ymut);
  arg = TagToArg(ymut,0);
  ydom = arg[1];
  ymin = arg[2];
  ymax = arg[3];
  sqrt_ymin = ymin;
  sqrt_ymax = ymax;
  X(4) = atom_nil;		/* actions list */
    
  CEILSQRT;
  if (TagIsSmall(ymax))
    FLOORSQRT;

  while (!(xflag&yflag&1))
    {
      if (!(yflag&1))
	{			/* y /\= x^2 */
	  yflag++;
	  switch ((!TagIsSmall(xmin) ? 0x0 : Tltz(xmin) ? 0x4 : 0x8)+
		  (!TagIsSmall(xmax) ? 0x0 : Tltz(xmax) ? 0x1 : 0x2))
	    {
	    case 0x0:		/* inf..sup */
	    case 0x2:		/* inf..+b */
	    case 0x4:		/*  -a..sup */
	      newmin = ymin;
	      newmax = ymax;
	      break;
	    case 0x1:		/* inf..-b => b^2..sup */
	      newmin = safe_mul_val(xmax,xmax);
	      newmax = ymax;
	      break;
	    case 0x5:		/*  -a..-b => b^2..a^2 */
	      newmin = safe_mul_val(xmax,xmax);
	      newmax = safe_mul_val(xmin,xmin);
	      break;
	    case 0x6:		/*  -a..b => 0..max(a^2,b^2) */
	      if (Tgt(Tminus(xmin),xmax))
		newmax = safe_mul_val(xmin,xmin);
	      else
		newmax = safe_mul_val(xmax,xmax);
	      break;
	    case 0x8:		/* a..sup => a^2..sup */
	      newmin = safe_mul_val(xmin,xmin);
	      newmax = ymax;
	      break;
	    case 0xa:		/*  a..b => a^2..b^2 */
	      newmin = safe_mul_val(xmin,xmin);
	      newmax = safe_mul_val(xmax,xmax);
	      break;
	    }
	  if (EmptyInterval(ymin_align,newmin))
	    newmin = ymin_align;
	  if (EmptyInterval(newmax,ymax))
	    newmax = ymax;
	  while (newmin!=ymin || newmax!=ymax)
	    {
	      yflag |= 2, xflag &= ~1;
	      if (!adjust_bounds(newmin, newmax, ydom, &ymin, &ymax))
		goto fail_or_oflo;
	      if (newmin!=ymin)
		{
		  CEILSQRT;
		  newmin = ymin_align;
		}
	      if (newmax!=ymax)
		{
		  FLOORSQRT;
		  newmax = ymax;
		}
	    }
	}
      else if (!(xflag&1))
	{			/* x /\= sqrt(y) */
	  xflag++;
	  if (Tlez(xmax)) /* x must be negative */ 
	    newmin = NegateUB(sqrt_ymax),
	    newmax = NegateLB(sqrt_ymin);
	  else if (xmin!=Inf && Tgez(xmin)) /* x must be positive */
	    newmin = sqrt_ymin, newmax = sqrt_ymax;
	  else
	    newmin = NegateUB(sqrt_ymax), newmax = sqrt_ymax;
	  
	  /* dom(y) may not be consistent after pruning dom(x) */

	  if (EmptyInterval(xmin,newmin))
	    newmin = xmin;
	  else if (newmin!=xmin)
	    xflag|=2, yflag &= ~1;
	  if (EmptyInterval(newmax,xmax))
	    newmax = xmax;
	  else if (newmax!=xmax)
	    xflag|=2, yflag &= ~1;
	  if (newmin!=xmin || newmax!=xmax)
	    {
	      if (!adjust_bounds(newmin, newmax, xdom, &xmin, &xmax))
		goto fail_or_oflo;
	    }
	}
    }
				/* Compute prunings */
				/* GC OK from here */
  if (xflag&2)
    request_tell_interval(w, RefTerm(XMutR), RefTerm(XR), xmin, xmax, 2, 3);
  if (yflag&2)
    request_tell_interval(w, RefTerm(YMutR), RefTerm(YR), ymin, ymax, 2, 3);

  ent = (xmin==xmax);
 ret:
  RefTerm(Actions) = request_done(w, ent, 2, 3);
  if (!fd_check_overflow())
    SP_fail();
  SP_reset_term_refs(r);
  return;
  
fail_or_oflo:
  if (newmin==Sup || newmax==Inf) /* can't represent bounds */
    fd.fd_overflow = TRUE;
  goto ret;
}



/* -1=overflow, 0=fail, 2=no pruning, 1=pruning */
static int fd_product_inverse_dispatch(
     TAGGED xmin, TAGGED xmax,
     TAGGED ymin, TAGGED ymax,
     TAGGED zmin, TAGGED zmax,
     TAGGED zdom,
     TAGGED *tmin, TAGGED *tmax)
{
  TAGGED newmin, newmax;
  int rc = 2;
  int x_is_mon=0;
  long xl=0, xu=0, xsize=0;

  
     switch ((!TagIsSmall(xmin) ? 0x00 :
	      Tltz(xmin) ? 0x40 : Teqz(xmin) ? 0x80 : 0xc0)+
	     (!TagIsSmall(xmax) ? 0x30 :
	      Tltz(xmax) ? 0x00 : Teqz(xmax) ? 0x10 : 0x20)+
	     (!TagIsSmall(ymin) ? 0x00 :
	      Tltz(ymin) ? 0x04 : Teqz(ymin) ? 0x08 : 0x0c)+
	     (!TagIsSmall(ymax) ? 0x03 :
	      Tltz(ymax) ? 0x00 : Teqz(ymax) ? 0x01 : 0x02))
       {			/* automatically generated table! */
       case 0x90: /* 0-0 / inf- -(d) => 0-0 */
       case 0x94: /* 0-0 / -(c)- -(d) => 0-0 */
       case 0x9e: /* 0-0 / +(c)- +(d) => 0-0 */
       case 0x9f: /* 0-0 / +(c)-sup => 0-0 */
	   newmin = TaggedZero;
	   newmax = TaggedZero;
	   break;
       case 0xeb: /* +(a)- +(b) / 0-sup => 1-b */
	   x_is_mon = 1;
	   newmin = TaggedOne;
	   newmax = xmax;
	   break;
       case 0x0: /* inf- -(b) / inf- -(d) => 1-sup */
       case 0x1: /* inf- -(b) / inf-0 => 1-sup */
       case 0xfb: /* +(a)-sup / 0-sup => 1-sup */
       case 0xff: /* +(a)-sup / +(c)-sup => 1-sup */
	   newmin = TaggedOne;
	   newmax = zmax;
	   break;
       case 0x10: /* inf-0 / inf- -(d) => 0-sup */
       case 0x14: /* inf-0 / -(c)- -(d) => 0-sup */
       case 0xbe: /* 0-sup / +(c)- +(d) => 0-sup */
       case 0xbf: /* 0-sup / +(c)-sup => 0-sup */
	   newmin = TaggedZero;
	   newmax = zmax;
	   break;
       case 0x41: /* -(a)- -(b) / inf-0 => 1- -(a) */
	   x_is_mon = 1;
	   newmin = TaggedOne;
	   newmax = Tminus(xmin);
	   break;
       case 0x40: /* -(a)- -(b) / inf- -(d) => 1-a/d */
	   x_is_mon = 1;
	   newmin = TaggedOne;
	   newmax = MakeSmall(-CEILDIV(GetSmall(xmin),-GetSmall(ymax)));
	   break;
       case 0x50: /* -(a)-0 / inf- -(d) => 0-a/d */
       case 0x54: /* -(a)-0 / -(c)- -(d) => 0-a/d */
	   newmin = TaggedZero;
	   newmax = MakeSmall(-CEILDIV(GetSmall(xmin),-GetSmall(ymax)));
	   break;
       case 0xae: /* 0- +(b) / +(c)- +(d) => 0-b/c */
       case 0xaf: /* 0- +(b) / +(c)-sup => 0-b/c */
	   newmin = TaggedZero;
	   newmax = MakeSmall(FLOORDIV(GetSmall(xmax),GetSmall(ymin)));
	   break;
       case 0xef: /* +(a)- +(b) / +(c)-sup => 1-b/c */
	   x_is_mon = 1;
	   newmin = TaggedOne;
	   newmax = MakeSmall(FLOORDIV(GetSmall(xmax),GetSmall(ymin)));
	   break;
       case 0x9: /* inf- -(b) / 0-0 => 1-0 */
       case 0x49: /* -(a)- -(b) / 0-0 => 1-0 */
       case 0xe9: /* +(a)- +(b) / 0-0 => 1-0 */
       case 0xf9: /* +(a)-sup / 0-0 => 1-0 */
	   return 0;
       case 0x4b: /* -(a)- -(b) / 0-sup => a- -1 */
	   x_is_mon = 1;
	   newmin = xmin;
	   newmax = TaggedMinusOne;
	   break;
       case 0x42: /* -(a)- -(b) / inf- +(d) => a- -(a) */
       case 0x43: /* -(a)- -(b) / inf-sup => a- -(a) */
       case 0x46: /* -(a)- -(b) / -(c)- +(d) => a- -(a) */
       case 0x47: /* -(a)- -(b) / -(c)-sup => a- -(a) */
	   x_is_mon = 1;
	   newmin = xmin;
	   newmax = Tminus(xmin);
	   break;
       case 0x4a: /* -(a)- -(b) / 0- +(d) => a-b/d */
	   x_is_mon = 1;
	   newmin = xmin;
	   newmax = MakeSmall(FLOORDIV(GetSmall(xmax),GetSmall(ymax)));
	   break;
       case 0xf: /* inf- -(b) / +(c)-sup => inf- -1 */
       case 0xf0: /* +(a)-sup / inf- -(d) => inf- -1 */
	   newmin = zmin;
	   newmax = TaggedMinusOne;
	   break;
       case 0xb: /* inf- -(b) / 0-sup => inf- -1 */
       case 0xf1: /* +(a)-sup / inf-0 => inf- -1 */
	   newmin = zmin;
	   newmax = TaggedMinusOne;
	   break;
       case 0x1e: /* inf-0 / +(c)- +(d) => inf-0 */
       case 0x1f: /* inf-0 / +(c)-sup => inf-0 */
       case 0xb0: /* 0-sup / inf- -(d) => inf-0 */
       case 0xb4: /* 0-sup / -(c)- -(d) => inf-0 */
	   newmin = zmin;
	   newmax = TaggedZero;
	   break;
       case 0xf4: /* +(a)-sup / -(c)- -(d) => inf-a/c */
       case 0xf5: /* +(a)-sup / -(c)-0 => inf-a/c */
	   newmin = zmin;
	   newmax = MakeSmall(-CEILDIV(GetSmall(xmin),-GetSmall(ymin)));
	   break;
       case 0x70: /* -(a)-sup / inf- -(d) => inf-a/d */
       case 0x74: /* -(a)-sup / -(c)- -(d) => inf-a/d */
	   newmin = zmin;
	   newmax = MakeSmall(-CEILDIV(GetSmall(xmin),-GetSmall(ymax)));
	   break;
       case 0x2e: /* inf- +(b) / +(c)- +(d) => inf-b/c */
       case 0x2f: /* inf- +(b) / +(c)-sup => inf-b/c */
	   newmin = zmin;
	   newmax = MakeSmall(FLOORDIV(GetSmall(xmax),GetSmall(ymin)));
	   break;
       case 0xa: /* inf- -(b) / 0- +(d) => inf-b/d */
       case 0xe: /* inf- -(b) / +(c)- +(d) => inf-b/d */
	   newmin = zmin;
	   newmax = MakeSmall(FLOORDIV(GetSmall(xmax),GetSmall(ymax)));
	   break;
       case 0xe1: /* +(a)- +(b) / inf-0 => -(b)- -1 */
	   x_is_mon = 1;
	   newmin = Tminus(xmax);
	   newmax = TaggedMinusOne;
	   break;
       case 0xe2: /* +(a)- +(b) / inf- +(d) => -(b)-b */
       case 0xe3: /* +(a)- +(b) / inf-sup => -(b)-b */
       case 0xe6: /* +(a)- +(b) / -(c)- +(d) => -(b)-b */
       case 0xe7: /* +(a)- +(b) / -(c)-sup => -(b)-b */
	   x_is_mon = 1;
	   newmin = Tminus(xmax);
	   newmax = xmax;
	   break;
       case 0xe5: /* +(a)- +(b) / -(c)-0 => -(b)-a/c */
	   x_is_mon = 1;
	   newmin = Tminus(xmax);
	   newmax = MakeSmall(-CEILDIV(GetSmall(xmin),-GetSmall(ymin)));
	   break;
       case 0x4f: /* -(a)- -(b) / +(c)-sup => a/c- -1 */
	   x_is_mon = 1;
	   newmin = MakeSmall(CEILDIV(GetSmall(xmin),GetSmall(ymin)));
	   newmax = TaggedMinusOne;
	   break;
       case 0x5e: /* -(a)-0 / +(c)- +(d) => a/c-0 */
       case 0x5f: /* -(a)-0 / +(c)-sup => a/c-0 */
	   newmin = MakeSmall(CEILDIV(GetSmall(xmin),GetSmall(ymin)));
	   newmax = TaggedZero;
	   break;
       case 0x7e: /* -(a)-sup / +(c)- +(d) => a/c-sup */
       case 0x7f: /* -(a)-sup / +(c)-sup => a/c-sup */
	   newmin = MakeSmall(CEILDIV(GetSmall(xmin),GetSmall(ymin)));
	   newmax = zmax;
	   break;
       case 0x6e: /* -(a)- +(b) / +(c)- +(d) => a/c-b/c */
       case 0x6f: /* -(a)- +(b) / +(c)-sup => a/c-b/c */
	   newmin = MakeSmall(CEILDIV(GetSmall(xmin),GetSmall(ymin)));
	   newmax = MakeSmall(FLOORDIV(GetSmall(xmax),GetSmall(ymin)));
	   break;
       case 0x4e: /* -(a)- -(b) / +(c)- +(d) => a/c-b/d */
	   x_is_mon = 1;
	   newmin = MakeSmall(CEILDIV(GetSmall(xmin),GetSmall(ymin)));
	   newmax = MakeSmall(FLOORDIV(GetSmall(xmax),GetSmall(ymax)));
	   break;
       case 0xea: /* +(a)- +(b) / 0- +(d) => a/d-b */
	   x_is_mon = 1;
	   newmin = MakeSmall(CEILDIV(GetSmall(xmin),GetSmall(ymax)));
	   newmax = xmax;
	   break;
       case 0xfa: /* +(a)-sup / 0- +(d) => a/d-sup */
       case 0xfe: /* +(a)-sup / +(c)- +(d) => a/d-sup */
	   newmin = MakeSmall(CEILDIV(GetSmall(xmin),GetSmall(ymax)));
	   newmax = zmax;
	   break;
       case 0xee: /* +(a)- +(b) / +(c)- +(d) => a/d-b/c */
	   x_is_mon = 1;
	   newmin = MakeSmall(CEILDIV(GetSmall(xmin),GetSmall(ymax)));
	   newmax = MakeSmall(FLOORDIV(GetSmall(xmax),GetSmall(ymin)));
	   break;
       case 0x4: /* inf- -(b) / -(c)- -(d) => b/c-sup */
       case 0x5: /* inf- -(b) / -(c)-0 => b/c-sup */
	   newmin = MakeSmall(-FLOORDIV(GetSmall(xmax),-GetSmall(ymin)));
	   newmax = zmax;
	   break;
       case 0x45: /* -(a)- -(b) / -(c)-0 => b/c- -(a) */
	   x_is_mon = 1;
	   newmin = MakeSmall(-FLOORDIV(GetSmall(xmax),-GetSmall(ymin)));
	   newmax = Tminus(xmin);
	   break;
       case 0x44: /* -(a)- -(b) / -(c)- -(d) => b/c-a/d */
	   x_is_mon = 1;
	   newmin = MakeSmall(-FLOORDIV(GetSmall(xmax),-GetSmall(ymin)));
	   newmax = MakeSmall(-CEILDIV(GetSmall(xmin),-GetSmall(ymax)));
	   break;
       case 0xa0: /* 0- +(b) / inf- -(d) => b/d-0 */
       case 0xa4: /* 0- +(b) / -(c)- -(d) => b/d-0 */
	   newmin = MakeSmall(-FLOORDIV(GetSmall(xmax),-GetSmall(ymax)));
	   newmax = TaggedZero;
	   break;
       case 0xe0: /* +(a)- +(b) / inf- -(d) => b/d- -1 */
	   x_is_mon = 1;
	   newmin = MakeSmall(-FLOORDIV(GetSmall(xmax),-GetSmall(ymax)));
	   newmax = TaggedMinusOne;
	   break;
       case 0x20: /* inf- +(b) / inf- -(d) => b/d-sup */
       case 0x24: /* inf- +(b) / -(c)- -(d) => b/d-sup */
	   newmin = MakeSmall(-FLOORDIV(GetSmall(xmax),-GetSmall(ymax)));
	   newmax = zmax;
	   break;
       case 0xe4: /* +(a)- +(b) / -(c)- -(d) => b/d-a/c */
	   x_is_mon = 1;
	   newmin = MakeSmall(-FLOORDIV(GetSmall(xmax),-GetSmall(ymax)));
	   newmax = MakeSmall(-CEILDIV(GetSmall(xmin),-GetSmall(ymin)));
	   break;
       case 0x60: /* -(a)- +(b) / inf- -(d) => b/d-a/d */
       case 0x64: /* -(a)- +(b) / -(c)- -(d) => b/d-a/d */
	   newmin = MakeSmall(-FLOORDIV(GetSmall(xmax),-GetSmall(ymax)));
	   newmax = MakeSmall(-CEILDIV(GetSmall(xmin),-GetSmall(ymax)));
	   break;
       default:
	 return 2;
       }
    if (EmptyInterval(zmin,newmin))
      newmin = zmin;
    if (EmptyInterval(newmax,zmax))
      newmax = zmax;
    if (x_is_mon)
      {
	long m = GetSmall(newmin);
	long n = GetSmall(newmax);
	
	xl = GetSmall(xmin)-1;
	xu = GetSmall(xmax);
	xsize = xu-xl;
	if (xl<0)
	  xl++, xu++;

	while (m<=n && xsize<(m<0 ? -m : m) && xl/m == xu/m)
	  m++;
	while (m<=n && xsize<(n<0 ? -n : n) && xl/n == xu/n)
	  n--;
	
	newmin = MakeSmall(m);
	newmax = MakeSmall(n);
      }

    while (newmin!=zmin || newmax!=zmax)
      {
	rc = 1;
	if (!adjust_bounds(newmin, newmax, zdom, &zmin, &zmax))
	  return (newmin==Sup || newmax==Inf) ? -1 : 0;
	newmin = zmin; newmax = zmax;
	if (x_is_mon)
	  {
	    long m = GetSmall(newmin);
	    long n = GetSmall(newmax);
	    
	    while (m<=n && xsize<(m<0 ? -m : m) && xl/m == xu/m)
	      m++;
	    while (m<=n && xsize<(n<0 ? -n : n) && xl/n == xu/n)
	      n--;

	    newmin = MakeSmall(m);
	    newmax = MakeSmall(n);
	  }
      }
    *tmin = newmin; *tmax = newmax;
    return rc;
}




/*
  '$fd_product'(+X, +XMut, +Y, +YMut, +Z, +ZMut, -Actions).
*/
void SPCDECL
prolog_fd_product MAGIC (HIDDEN_PROTO
			 SP_term_ref State,
			 SP_term_ref NewState,
			 SP_term_ref Actions)
{
  WAMENV;
  TAGGED x, xmut, y, ymut, z, zmut, *arg;
  TAGGED xmin, xmax, xdom, ymin, ymax, ydom, zmin, zmax, zdom;
  TAGGED newmin, newmax;
  SP_term_ref r = SP_new_term_refs(6);
  unsigned int xflag=0, yflag=0, zflag=0; /* 1=consistent, 2=pruned */
  int ent = -1;			/* disentailed unless otherwise */

  w->numstack_end = NULL;
  fd.fd_overflow = FALSE;
  RefTerm(NewState) = RefTerm(State);
  X(2) = atom_nil;		/* actions list */
  DerefNonvar(X(0));
  DerefArg(x,X(0),1);
  DerefArg(xmut,X(0),2);
  DerefArg(y,X(0),3);
  DerefArg(ymut,X(0),4);
  DerefArg(z,X(0),5);
  DerefArg(zmut,X(0),6);
  RefTerm(XR) = x;
  RefTerm(XMutR) = xmut;
  RefTerm(YR) = y;
  RefTerm(YMutR) = ymut;
  RefTerm(ZR) = z;
  RefTerm(ZMutR) = zmut;
  DerefNonvar(xmut);
  DerefAttribute(xmut,xmut);
  arg = TagToArg(xmut,0);
  xdom = arg[1];
  xmin = arg[2];
  xmax = arg[3];
  DerefNonvar(ymut);
  DerefAttribute(ymut,ymut);
  arg = TagToArg(ymut,0);
  ydom = arg[1];
  ymin = arg[2];
  ymax = arg[3];
  DerefNonvar(zmut);
  DerefAttribute(zmut,zmut);
  arg = TagToArg(zmut,0);
  zdom = arg[1];
  zmin = arg[2];
  zmax = arg[3];
  X(6) = atom_nil;		/* actions list */

  while (!(xflag&yflag&zflag&1))
    {
      if (!(zflag&1))
	{			/* z /\= x*y */
	  zflag++;
	  switch ((!TagIsSmall(xmin) ? 0x00 :
		   Tltz(xmin) ? 0x40 : Teqz(xmin) ? 0x80 : 0xc0)+
		  (!TagIsSmall(xmax) ? 0x30 :
		   Tltz(xmax) ? 0x00 : Teqz(xmax) ? 0x10 : 0x20)+
		  (!TagIsSmall(ymin) ? 0x00 :
		   Tltz(ymin) ? 0x04 : Teqz(ymin) ? 0x08 : 0x0c)+
		  (!TagIsSmall(ymax) ? 0x03 :
		   Tltz(ymax) ? 0x00 : Teqz(ymax) ? 0x01 : 0x02))

	    {			/* automatically generated table! */
	    case 0x9: /* inf- -(b) * 0-0 => 0-0 */
	    case 0x19: /* inf-0 * 0-0 => 0-0 */
	    case 0x29: /* inf- +(b) * 0-0 => 0-0 */
	    case 0x39: /* inf-sup * 0-0 => 0-0 */
	    case 0x49: /* -(a)- -(b) * 0-0 => 0-0 */
	    case 0x59: /* -(a)-0 * 0-0 => 0-0 */
	    case 0x69: /* -(a)- +(b) * 0-0 => 0-0 */
	    case 0x79: /* -(a)-sup * 0-0 => 0-0 */
	    case 0x90: /* 0-0 * inf- -(d) => 0-0 */
	    case 0x91: /* 0-0 * inf-0 => 0-0 */
	    case 0x92: /* 0-0 * inf- +(d) => 0-0 */
	    case 0x93: /* 0-0 * inf-sup => 0-0 */
	    case 0x94: /* 0-0 * -(c)- -(d) => 0-0 */
	    case 0x95: /* 0-0 * -(c)-0 => 0-0 */
	    case 0x96: /* 0-0 * -(c)- +(d) => 0-0 */
	    case 0x97: /* 0-0 * -(c)-sup => 0-0 */
	    case 0x99: /* 0-0 * 0-0 => 0-0 */
	    case 0x9a: /* 0-0 * 0- +(d) => 0-0 */
	    case 0x9b: /* 0-0 * 0-sup => 0-0 */
	    case 0x9e: /* 0-0 * +(c)- +(d) => 0-0 */
	    case 0x9f: /* 0-0 * +(c)-sup => 0-0 */
	    case 0xa9: /* 0- +(b) * 0-0 => 0-0 */
	    case 0xb9: /* 0-sup * 0-0 => 0-0 */
	    case 0xe9: /* +(a)- +(b) * 0-0 => 0-0 */
	    case 0xf9: /* +(a)-sup * 0-0 => 0-0 */
	      newmin = TaggedZero;
	      newmax = TaggedZero;
	      break;
	    case 0x1: /* inf- -(b) * inf-0 => 0-sup */
	    case 0x5: /* inf- -(b) * -(c)-0 => 0-sup */
	    case 0x10: /* inf-0 * inf- -(d) => 0-sup */
	    case 0x11: /* inf-0 * inf-0 => 0-sup */
	    case 0x14: /* inf-0 * -(c)- -(d) => 0-sup */
	    case 0x15: /* inf-0 * -(c)-0 => 0-sup */
	    case 0x41: /* -(a)- -(b) * inf-0 => 0-sup */
	    case 0x50: /* -(a)-0 * inf- -(d) => 0-sup */
	    case 0x51: /* -(a)-0 * inf-0 => 0-sup */
	    case 0xab: /* 0- +(b) * 0-sup => 0-sup */
	    case 0xaf: /* 0- +(b) * +(c)-sup => 0-sup */
	    case 0xba: /* 0-sup * 0- +(d) => 0-sup */
	    case 0xbb: /* 0-sup * 0-sup => 0-sup */
	    case 0xbe: /* 0-sup * +(c)- +(d) => 0-sup */
	    case 0xbf: /* 0-sup * +(c)-sup => 0-sup */
	    case 0xeb: /* +(a)- +(b) * 0-sup => 0-sup */
	    case 0xfa: /* +(a)-sup * 0- +(d) => 0-sup */
	    case 0xfb: /* +(a)-sup * 0-sup => 0-sup */
	      newmin = TaggedZero;
	      newmax = zmax;
	      break;
	    case 0xaa: /* 0- +(b) * 0- +(d) => 0- +(b)* +(d) */
	    case 0xae: /* 0- +(b) * +(c)- +(d) => 0- +(b)* +(d) */
	    case 0xea: /* +(a)- +(b) * 0- +(d) => 0- +(b)* +(d) */
	      newmin = TaggedZero;
	      newmax = safe_mul_val(xmax,ymax);
	      break;
	    case 0x45: /* -(a)- -(b) * -(c)-0 => 0- -(a)* -(c) */
	    case 0x54: /* -(a)-0 * -(c)- -(d) => 0- -(a)* -(c) */
	    case 0x55: /* -(a)-0 * -(c)-0 => 0- -(a)* -(c) */
	      newmin = TaggedZero;
	      newmax = safe_mul_val(xmin,ymin);
	      break;
	    case 0xa: /* inf- -(b) * 0- +(d) => inf-0 */
	    case 0xb: /* inf- -(b) * 0-sup => inf-0 */
	    case 0x1a: /* inf-0 * 0- +(d) => inf-0 */
	    case 0x1b: /* inf-0 * 0-sup => inf-0 */
	    case 0x1e: /* inf-0 * +(c)- +(d) => inf-0 */
	    case 0x1f: /* inf-0 * +(c)-sup => inf-0 */
	    case 0x4b: /* -(a)- -(b) * 0-sup => inf-0 */
	    case 0x5b: /* -(a)-0 * 0-sup => inf-0 */
	    case 0x5f: /* -(a)-0 * +(c)-sup => inf-0 */
	    case 0xa0: /* 0- +(b) * inf- -(d) => inf-0 */
	    case 0xa1: /* 0- +(b) * inf-0 => inf-0 */
	    case 0xb0: /* 0-sup * inf- -(d) => inf-0 */
	    case 0xb1: /* 0-sup * inf-0 => inf-0 */
	    case 0xb4: /* 0-sup * -(c)- -(d) => inf-0 */
	    case 0xb5: /* 0-sup * -(c)-0 => inf-0 */
	    case 0xe1: /* +(a)- +(b) * inf-0 => inf-0 */
	    case 0xf1: /* +(a)-sup * inf-0 => inf-0 */
	    case 0xf5: /* +(a)-sup * -(c)-0 => inf-0 */
	      newmin = zmin;
	      newmax = TaggedZero;
	      break;
	    case 0xe0: /* +(a)- +(b) * inf- -(d) => inf- +(a)* -(d) */
	    case 0xf0: /* +(a)-sup * inf- -(d) => inf- +(a)* -(d) */
	    case 0xf4: /* +(a)-sup * -(c)- -(d) => inf- +(a)* -(d) */
	      newmin = zmin;
	      newmax = safe_mul_val(xmin,ymax);
	      break;
	    case 0x2a: /* inf- +(b) * 0- +(d) => inf- +(b)* +(d) */
	    case 0x2e: /* inf- +(b) * +(c)- +(d) => inf- +(b)* +(d) */
	    case 0xa2: /* 0- +(b) * inf- +(d) => inf- +(b)* +(d) */
	    case 0xe2: /* +(a)- +(b) * inf- +(d) => inf- +(b)* +(d) */
	      newmin = zmin;
	      newmax = safe_mul_val(xmax,ymax);
	      break;
	    case 0x47: /* -(a)- -(b) * -(c)-sup => inf- -(a)* -(c) */
	    case 0x57: /* -(a)-0 * -(c)-sup => inf- -(a)* -(c) */
	    case 0x74: /* -(a)-sup * -(c)- -(d) => inf- -(a)* -(c) */
	    case 0x75: /* -(a)-sup * -(c)-0 => inf- -(a)* -(c) */
	      newmin = zmin;
	      newmax = safe_mul_val(xmin,ymin);
	      break;
	    case 0xe: /* inf- -(b) * +(c)- +(d) => inf- -(b)* +(c) */
	    case 0xf: /* inf- -(b) * +(c)-sup => inf- -(b)* +(c) */
	    case 0x4f: /* -(a)- -(b) * +(c)-sup => inf- -(b)* +(c) */
	      newmin = zmin;
	      newmax = safe_mul_val(xmax,ymin);
	      break;
	    case 0xef: /* +(a)- +(b) * +(c)-sup => +(a)* +(c)-sup */
	    case 0xfe: /* +(a)-sup * +(c)- +(d) => +(a)* +(c)-sup */
	    case 0xff: /* +(a)-sup * +(c)-sup => +(a)* +(c)-sup */
	      newmin = safe_mul_val(xmin,ymin);
	      newmax = zmax;
	      break;
	    case 0xee: /* +(a)- +(b) * +(c)- +(d) => +(a)* +(c)- +(b)* +(d) */
	      newmin = safe_mul_val(xmin,ymin);
	      newmax = safe_mul_val(xmax,ymax);
	      break;
	    case 0xa4: /* 0- +(b) * -(c)- -(d) => +(b)* -(c)-0 */
	    case 0xa5: /* 0- +(b) * -(c)-0 => +(b)* -(c)-0 */
	    case 0xe5: /* +(a)- +(b) * -(c)-0 => +(b)* -(c)-0 */
	      newmin = safe_mul_val(xmax,ymin);
	      newmax = TaggedZero;
	      break;
	    case 0x24: /* inf- +(b) * -(c)- -(d) => +(b)* -(c)-sup */
	    case 0x25: /* inf- +(b) * -(c)-0 => +(b)* -(c)-sup */
	    case 0xa7: /* 0- +(b) * -(c)-sup => +(b)* -(c)-sup */
	    case 0xe7: /* +(a)- +(b) * -(c)-sup => +(b)* -(c)-sup */
	      newmin = safe_mul_val(xmax,ymin);
	      newmax = zmax;
	      break;
	    case 0xe4: /* +(a)- +(b) * -(c)- -(d) => +(b)* -(c)- +(a)* -(d) */
	      newmin = safe_mul_val(xmax,ymin);
	      newmax = safe_mul_val(xmin,ymax);
	      break;
	    case 0xa6: /* 0- +(b) * -(c)- +(d) => +(b)* -(c)- +(b)* +(d) */
	    case 0xe6: /* +(a)- +(b) * -(c)- +(d) => +(b)* -(c)- +(b)* +(d) */
	      newmin = safe_mul_val(xmax,ymin);
	      newmax = safe_mul_val(xmax,ymax);
	      break;
	    case 0x64: /* -(a)- +(b) * -(c)- -(d) => +(b)* -(c)- -(a)* -(c) */
	    case 0x65: /* -(a)- +(b) * -(c)-0 => +(b)* -(c)- -(a)* -(c) */
	      newmin = safe_mul_val(xmax,ymin);
	      newmax = safe_mul_val(xmin,ymin);
	      break;
	    case 0x4a: /* -(a)- -(b) * 0- +(d) => -(a)* +(d)-0 */
	    case 0x5a: /* -(a)-0 * 0- +(d) => -(a)* +(d)-0 */
	    case 0x5e: /* -(a)-0 * +(c)- +(d) => -(a)* +(d)-0 */
	      newmin = safe_mul_val(xmin,ymax);
	      newmax = TaggedZero;
	      break;
	    case 0x42: /* -(a)- -(b) * inf- +(d) => -(a)* +(d)-sup */
	    case 0x52: /* -(a)-0 * inf- +(d) => -(a)* +(d)-sup */
	    case 0x7a: /* -(a)-sup * 0- +(d) => -(a)* +(d)-sup */
	    case 0x7e: /* -(a)-sup * +(c)- +(d) => -(a)* +(d)-sup */
	      newmin = safe_mul_val(xmin,ymax);
	      newmax = zmax;
	      break;
	    case 0x6a: /* -(a)- +(b) * 0- +(d) => -(a)* +(d)- +(b)* +(d) */
	    case 0x6e: /* -(a)- +(b) * +(c)- +(d) => -(a)* +(d)- +(b)* +(d) */
	      newmin = safe_mul_val(xmin,ymax);
	      newmax = safe_mul_val(xmax,ymax);
	      break;
	    case 0x46: /* -(a)- -(b) * -(c)- +(d) => -(a)* +(d)- -(a)* -(c) */
	    case 0x56: /* -(a)-0 * -(c)- +(d) => -(a)* +(d)- -(a)* -(c) */
	      newmin = safe_mul_val(xmin,ymax);
	      newmax = safe_mul_val(xmin,ymin);
	      break;
	    case 0x4e: /* -(a)- -(b) * +(c)- +(d) => -(a)* +(d)- -(b)* +(c) */
	      newmin = safe_mul_val(xmin,ymax);
	      newmax = safe_mul_val(xmax,ymin);
	      break;
	    case 0x0: /* inf- -(b) * inf- -(d) => -(b)* -(d)-sup */
	    case 0x4: /* inf- -(b) * -(c)- -(d) => -(b)* -(d)-sup */
	    case 0x40: /* -(a)- -(b) * inf- -(d) => -(b)* -(d)-sup */
	      newmin = safe_mul_val(xmax,ymax);
	      newmax = zmax;
	      break;
	    case 0x44: /* -(a)- -(b) * -(c)- -(d) => -(b)* -(d)- -(a)* -(c) */
	      newmin = safe_mul_val(xmax,ymax);
	      newmax = safe_mul_val(xmin,ymin);
	      break;
	    case 0x66: /* -(a)- +(b) * -(c)- +(d) => min(-(a)* +(d),+(b)* -(c))-max(-(a)* -(c),+(b)* +(d)) */
	      {
		TAGGED minmax = safe_mul_val(xmin,ymax);
		TAGGED maxmin = safe_mul_val(xmax,ymin);
		TAGGED minmin = safe_mul_val(xmin,ymin);
		TAGGED maxmax = safe_mul_val(xmax,ymax);

		newmin = EmptyInterval(minmax,maxmin) ? maxmin : minmax;
		newmax = EmptyInterval(minmin,maxmax) ? minmin : maxmax;
	      }
	      break;
	    default:
	      continue;
	    }
	  if (EmptyInterval(zmin,newmin))
	    newmin = zmin;
	  else if (newmin!=zmin)
	    zflag|=2;
	  if (EmptyInterval(newmax,zmax))
	    newmax = zmax;
	  else if (newmax!=zmax)
	    zflag|=2;
	  if (newmin!=zmin || newmax!=zmax)
	    {
	      if (!adjust_bounds(newmin, newmax, zdom, &zmin, &zmax))
		goto fail_or_oflo;
	      if (newmin!=zmin || newmax!=zmax)
		xflag &= ~1, yflag &= ~1;
	    }
	}
      else if (!(xflag&1))
	{			/* x /\= z/y */
	  TAGGED tmin, tmax;
	  xflag++;
	  switch
	    (fd_product_inverse_dispatch(zmin, zmax, ymin, ymax, xmin, xmax,
					 xdom, &tmin, &tmax))
	    {
	    case -1: goto oflo;
	    case 0: goto ret;
	    case 1: xmin = tmin; xmax = tmax;
	            xflag|=2, yflag &= ~1, zflag &= ~1;
	    /* case 2: continue; */
	    }
	}
      else if (!(yflag&1))
	{			/* y /\= z/x */
	  TAGGED tmin, tmax;
	  yflag++;
	  switch
	    (fd_product_inverse_dispatch(zmin, zmax, xmin, xmax, ymin, ymax,
					 ydom, &tmin, &tmax))
	    {
	    case -1: goto oflo;
	    case 0: goto ret;
	    case 1: ymin = tmin; ymax = tmax;
	            yflag|=2, xflag &= ~1, zflag &= ~1;
	    /* case 2: continue; */
	    }
	}
    }
				/* Compute prunings */
				/* GC OK from here */
  if (xflag&2)
    request_tell_interval(w, RefTerm(XMutR), RefTerm(XR), xmin, xmax, 2, 3);
  if (yflag&2)
    request_tell_interval(w, RefTerm(YMutR), RefTerm(YR), ymin, ymax, 2, 3);
  if (zflag&2)
    request_tell_interval(w, RefTerm(ZMutR), RefTerm(ZR), zmin, zmax, 2, 3);

  ent = ((xmin==xmax)+(ymin==ymax)+(zmin==zmax) >= 2);
 ret:
  RefTerm(Actions) = request_done(w, ent, 2, 3);
  if (!fd_check_overflow())
    SP_fail();
  SP_reset_term_refs(r);
  return;
  
fail_or_oflo:
  if (newmin==Sup || newmax==Inf) /* can't represent bounds */
    {
    oflo:
      fd.fd_overflow = TRUE;
    }
  goto ret;
}



/*
  '$fd_quotient'(+X, +XMut, +K, +Y, +YMut, -Actions).
*/
void SPCDECL
prolog_fd_quotient MAGIC (HIDDEN_PROTO
			  SP_term_ref State,
			  SP_term_ref NewState,
			  SP_term_ref Actions)
{
  WAMENV;
  TAGGED x, xmut, tk, y, ymut, *arg;
  TAGGED xmin, xmax, xdom, ymin, ymax, ydom, newmin, newmax;
  SP_term_ref r = SP_new_term_refs(4);
  unsigned int xflag=0, yflag=0; /* 1=consistent, 2=pruned */
  long k;
  int ent = -1;			/* disentailed unless otherwise */

  w->numstack_end = NULL;
  fd.fd_overflow = FALSE;
  RefTerm(NewState) = RefTerm(State);
  X(2) = atom_nil;		/* actions list */
  DerefNonvar(X(0));
  DerefArg(x,X(0),1);
  DerefArg(xmut,X(0),2);
  DerefArg(tk,X(0),3);
  DerefArg(y,X(0),4);
  DerefArg(ymut,X(0),5);
  RefTerm(XR) = x;
  RefTerm(XMutR) = xmut;
  RefTerm(YR) = y;
  RefTerm(YMutR) = ymut;
  DerefNonvar(tk);
  k = GetSmall(tk);
  DerefNonvar(xmut);
  DerefAttribute(xmut,xmut);
  arg = TagToArg(xmut,0);
  xdom = arg[1];
  xmin = arg[2];
  xmax = arg[3];
  DerefNonvar(ymut);
  DerefAttribute(ymut,ymut);
  arg = TagToArg(ymut,0);
  ydom = arg[1];
  ymin = arg[2];
  ymax = arg[3];
  X(5) = atom_nil;		/* actions list */

  while (!(xflag&yflag&1))
    {
      if (!(yflag&1))
	{			/* y /\= x/k */
	  yflag++;
	  switch ((!TagIsSmall(xmin) ? 0x0 :
		   Tltz(xmin) ? 0x4 : Teqz(xmin) ? 0x8 : 0xc)+
		  (!TagIsSmall(xmax) ? 0x3 :
		   Tltz(xmax) ? 0x0 : Teqz(xmax) ? 0x1 : 0x2)+
		  (k>0 ? 0x0 : 0x10))
	    {
	    case 0x1:		/* k>0 inf..0 */
	    case 0x1b:		/* k<0   0..sup */
	      newmin = ymin;
	      newmax = TaggedZero;
	      break;
	    case 0x0:		/* k>0 inf..-b */
	    case 0x2:		/* k>0 inf..+b */
	      newmin = ymin;
	      newmax = MakeSmall(GetSmall(xmax)/k);
	      break;
	    case 0x5:		/* k>0  -a..0 */
	      newmin = MakeSmall(GetSmall(xmin)/k);
	      newmax = TaggedZero;
	      break;
	    case 0x9:		/* k>0   0..0 */
	    case 0x19:		/* k<0   0..0 */
	      newmin = TaggedZero;
	      newmax = TaggedZero;
	      break;
	    case 0xa:		/* k>0   0..+b */
	      newmin = TaggedZero;
	      newmax = MakeSmall(GetSmall(xmax)/k);
	      break;
	    case 0xb:		/* k>0   0..sup */
	    case 0x11:		/* k<0 inf..0 */
	      newmin = TaggedZero;
	      newmax = ymax;
	      break;
	    case 0x4:		/* k>0  -a..-b */
	    case 0x6:		/* k>0  -a..+b */
	    case 0xe:		/* k>0  +a..+b */
	      newmin = MakeSmall(GetSmall(xmin)/k);
	      newmax = MakeSmall(GetSmall(xmax)/k);
	      break;
	    case 0x7:		/* k>0  -a..sup */
	    case 0xf:		/* k>0  +a..sup */
	      newmin = MakeSmall(GetSmall(xmin)/k);
	      newmax = ymax;
	      break;
	    case 0x10:		/* k<0 inf..-b */
	    case 0x12:		/* k<0 inf..+b */
	      newmin = MakeSmall(GetSmall(xmax)/k);
	      newmax = ymax;
	      break;
	    case 0x15:		/* k<0  -a..0 */
	      newmin = TaggedZero;
	      newmax = MakeSmall(GetSmall(xmin)/k);
	      break;
	    case 0x1a:		/* k<0   0..+b */
	      newmin = MakeSmall(GetSmall(xmax)/k);
	      newmax = TaggedZero;
	      break;
	    case 0x14:		/* k<0  -a..-b */
	    case 0x16:		/* k<0  -a..+b */
	    case 0x1e:		/* k<0  +a..+b */
	      newmin = MakeSmall(GetSmall(xmax)/k);
	      newmax = MakeSmall(GetSmall(xmin)/k);
	      break;
	    case 0x17:		/* k<0  -a..sup */
	    case 0x1f:		/* k<0  +a..sup */
	      newmin = ymin;
	      newmax = MakeSmall(GetSmall(xmin)/k);
	      break;
	    default:		/* inf..sup */
	      continue;
	    }
	  
	  /* dom(x) may not be consistent after pruning dom(y) */
	  
	  if (EmptyInterval(ymin,newmin))
	    newmin = ymin;
	  else if (newmin!=ymin)
	    yflag|=2, xflag &= ~1;
	  if (EmptyInterval(newmax,ymax))
	    newmax = ymax;
	  else if (newmax!=ymax)
	    yflag|=2, xflag &= ~1;
	  if (newmin!=ymin || newmax!=ymax)
	    {
	      if (!adjust_bounds(newmin, newmax, ydom, &ymin, &ymax))
		goto fail_or_oflo;
	    }
	}
      else if (!(xflag&1))
	{			/* x /\= y*k (+ some slack) */
	  xflag++;
	  switch ((!TagIsSmall(ymin) ? 0x0 :
		   Tltz(ymin) ? 0x4 : Teqz(ymin) ? 0x8 : 0xc)+
		  (!TagIsSmall(ymax) ? 0x3 :
		   Tltz(ymax) ? 0x0 : Teqz(ymax) ? 0x1 : 0x2)+
		  (k>0 ? 0x0 : 0x10))
	    {
	    case 0x1:		/* k>0 inf..0 => inf..k-1 */
	      newmin = xmin;
	      newmax = MakeSmall(k-1);
	      break;
	    case 0x1b:		/* k<0   0..sup => inf..-k-1*/
	      newmin = xmin;
	      newmax = MakeSmall(-k-1);
	      break;
	    case 0x0:		/* k>0 inf..-b => inf..-bk */
	      newmin = xmin;
	      newmax = safe_mul_val(ymax,tk);
	      break;
	    case 0x2:		/* k>0 inf..+b => inf..+bk+k-1 */
	      newmin = xmin;
	      newmax = safe_mul_val(ymax+IStep(1),tk);
	      if (TagIsSmall(newmax))
		newmax -= IStep(1);
	      break;
	    case 0x5:		/* k>0  -a..0 => -ak-k+1..k-1 */
	      newmin = safe_mul_val(ymin-IStep(1),tk);
	      if (TagIsSmall(newmin))
		newmin += IStep(1);
	      newmax = MakeSmall(k-1);
	      break;
	    case 0x9:		/* k>0   0..0 => -k+1..k-1 */
	      newmin = MakeSmall(1-k);
	      newmax = MakeSmall(k-1);
	      break;
	    case 0x19:		/* k<0   0..0 =>  k+1..-k-1*/
	      newmin = MakeSmall(k+1);
	      newmax = MakeSmall(-k-1);
	      break;
	    case 0xa:		/* k>0   0..+b => -k+1..+bk+k-1*/
	      newmin = MakeSmall(1-k);
	      newmax = safe_mul_val(ymax+IStep(1),tk);
	      if (TagIsSmall(newmax))
		newmax -= IStep(1);
	      break;
	    case 0xb:		/* k>0   0..sup => -k+1..sup */
	      newmin = MakeSmall(1-k);
	      newmax = xmax;
	      break;
	    case 0x11:		/* k<0 inf..0   =>  k+1..sup */
	      newmin = MakeSmall(k+1);
	      newmax = xmax;
	      break;
	    case 0x4:		/* k>0  -a..-b => -ak-k+1..-bk */
	      newmin = safe_mul_val(ymin-IStep(1),tk);
	      if (TagIsSmall(newmin))
		newmin += IStep(1);
	      newmax = safe_mul_val(ymax,tk);
	      break;
	    case 0x6:		/* k>0  -a..+b => -ak-k+1..+bk+k-1 */
	      newmin = safe_mul_val(ymin-IStep(1),tk);
	      if (TagIsSmall(newmin))
		newmin += IStep(1);
	      newmax = safe_mul_val(ymax+IStep(1),tk);
	      if (TagIsSmall(newmax))
		newmax -= IStep(1);
	      break;
	    case 0xe:		/* k>0  +a..+b => +ak..+bk+k-1 */
	      newmin = safe_mul_val(ymin,tk);
	      newmax = safe_mul_val(ymax+IStep(1),tk);
	      if (TagIsSmall(newmax))
		newmax -= IStep(1);
	      break;
	    case 0x7:		/* k>0  -a..sup => -ak-k+1..sup */
	      newmin = safe_mul_val(ymin-IStep(1),tk);
	      if (TagIsSmall(newmin))
		newmin += IStep(1);
	      newmax = xmax;
	      break;
	    case 0xf:		/* k>0  +a..sup => +ak..sup */
	      newmin = safe_mul_val(ymin,tk);
	      newmax = xmax;
	      break;
	    case 0x10:		/* k<0 inf..-b => -bk..sup */
	      newmin = safe_mul_val(ymax,tk);
	      newmax = xmax;
	      break;
	    case 0x12:		/* k<0 inf..+b => +bk+k+1..sup */
	      newmin = safe_mul_val(ymax+IStep(1),tk);
	      if (TagIsSmall(newmin))
		newmin += IStep(1);
	      newmax = xmax;
	      break;
	    case 0x15:		/* k<0  -a..0 => k+1..-ak-k-1 */
	      newmin = MakeSmall(k+1);
	      newmax = safe_mul_val(ymin-IStep(1),tk);
	      if (TagIsSmall(newmax))
		newmax -= IStep(1);
	      break;
	    case 0x1a:		/* k<0   0..+b => +bk+k+1..-k-1 */
	      newmin = safe_mul_val(ymax+IStep(1),tk);
	      if (TagIsSmall(newmin))
		newmin += IStep(1);
	      newmax = MakeSmall(-k-1);
	      break;
	    case 0x14:		/* k<0  -a..-b => -bk..-ak-k-1 */
	      newmin = safe_mul_val(ymax,tk);
	      newmax = safe_mul_val(ymin-IStep(1),tk);
	      if (TagIsSmall(newmax))
		newmax -= IStep(1);
	      break;
	    case 0x16:		/* k<0  -a..+b => +bk+k+1..-ak-k-1*/
	      newmin = safe_mul_val(ymax+IStep(1),tk);
	      if (TagIsSmall(newmin))
		newmin += IStep(1);
	      newmax = safe_mul_val(ymin-IStep(1),tk);
	      if (TagIsSmall(newmax))
		newmax -= IStep(1);
	      break;
	    case 0x1e:		/* k<0  +a..+b => +bk+k+1..+ak*/
	      newmin = safe_mul_val(ymax+IStep(1),tk);
	      if (TagIsSmall(newmin))
		newmin += IStep(1);
	      newmax = safe_mul_val(ymin,tk);
	      break;
	    case 0x17:		/* k<0  -a..sup => inf..-ak-k-1*/
	      newmin = xmin;
	      newmax = safe_mul_val(ymin-IStep(1),tk);
	      if (TagIsSmall(newmax))
		newmax -= IStep(1);
	      break;
	    case 0x1f:		/* k<0  +a..sup => inf..+ak*/
	      newmin = xmin;
	      newmax = safe_mul_val(ymin,tk);
	      break;
	    default:		/* inf..sup */
	      continue;
	    }
	  
	  /* dom(y) is consistent after pruning dom(x) */

	  if (EmptyInterval(xmin,newmin))
	    newmin = xmin;
	  else if (newmin!=xmin)
	    xflag|=2;
	  if (EmptyInterval(newmax,xmax))
	    newmax = xmax;
	  else if (newmax!=xmax)
	    xflag|=2;
	  if (newmin!=xmin || newmax!=xmax)
	    {
	      if (!adjust_bounds(newmin, newmax, xdom, &xmin, &xmax))
		goto fail_or_oflo;
	      if (newmin!=xmin || newmax!=xmax)
		yflag &= ~1;
	    }
	}
    }
				/* Compute prunings */
				/* GC OK from here */
  if (xflag&2)
    request_tell_interval(w, RefTerm(XMutR), RefTerm(XR), xmin, xmax, 2, 3);
  if (yflag&2)
    request_tell_interval(w, RefTerm(YMutR), RefTerm(YR), ymin, ymax, 2, 3);

  ent = (xmin==xmax);
 ret:
  RefTerm(Actions) = request_done(w, ent, 2, 3);
  if (!fd_check_overflow())
    SP_fail();
  SP_reset_term_refs(r);
  return;
  
fail_or_oflo:
  if (newmin==Sup || newmax==Inf) /* can't represent bounds */
    fd.fd_overflow = TRUE;
  goto ret;
}



/*
  '$fd_modulo'(+X, +XMut, +K, +Y, +YMut, -Actions).
*/
void SPCDECL
prolog_fd_modulo MAGIC (HIDDEN_PROTO
			SP_term_ref State,
			SP_term_ref NewState,
			SP_term_ref Actions)
{
  WAMENV;
  TAGGED x, xmut, tk, y, ymut, *arg;
  TAGGED xmin, xmax, xdom, ymin, ymax, ydom, newmin, newmax;
  unsigned int xflag=0, yflag=0; /* 1=consistent, 2=pruned */
  SP_term_ref r = SP_new_term_refs(4);
  long k, amk, bmk, kmin, kmax;
  int ent = -1;			/* disentailed unless otherwise */

  w->numstack_end = NULL;
  fd.fd_overflow = FALSE;
  RefTerm(NewState) = RefTerm(State);
  X(2) = atom_nil;		/* actions list */
  DerefNonvar(X(0));
  DerefArg(x,X(0),1);
  DerefArg(xmut,X(0),2);
  DerefArg(tk,X(0),3);
  DerefArg(y,X(0),4);
  DerefArg(ymut,X(0),5);
  RefTerm(XR) = x;
  RefTerm(XMutR) = xmut;
  RefTerm(YR) = y;
  RefTerm(YMutR) = ymut;
  DerefNonvar(tk);
  k = GetSmall(tk);
  if (k<0) k = -k;
  kmin = MakeSmall(1-k);
  kmax = MakeSmall(k-1);
  DerefNonvar(xmut);
  DerefAttribute(xmut,xmut);
  arg = TagToArg(xmut,0);
  xdom = arg[1];
  xmin = arg[2];
  xmax = arg[3];
  DerefNonvar(ymut);
  DerefAttribute(ymut,ymut);
  arg = TagToArg(ymut,0);
  ydom = arg[1];
  ymin = arg[2];
  ymax = arg[3];
  X(5) = atom_nil;		/* actions list */

  while (!(xflag&yflag&1))
    {
      if (!(yflag&1))
	{			/* y /\= x mod k */
	  yflag++;
	  amk = MakeSmall(GetSmall(xmin) % k);
	  bmk = MakeSmall(GetSmall(xmax) % k);
	  switch ((!TagIsSmall(xmin) ? 0x0 :
		   Tltz(xmin) ? 0x4 : Teqz(xmin) ? 0x8 : 0xc)+
		  (!TagIsSmall(xmax) ? 0x3 :
		   Tltz(xmax) ? 0x0 : Teqz(xmax) ? 0x1 : 0x2))
	    {
	    case 0x0:		/* inf..-b */
	    case 0x1:		/* inf..0 */
	      newmin = kmin;
	      newmax = TaggedZero;
	      break;
	    case 0x2:		/* inf..+b */
	      newmin = kmin;
	      newmax = (xmax >= kmax ? kmax : bmk);
	      break;
	    case 0x4:		/*  -a..-b */
	      if (Tge(xmax-xmin+TaggedZero,kmax) || Tgt(amk,bmk))
		newmin = kmin, newmax = TaggedZero;
	      else
		newmin = amk, newmax = bmk;
	      break;
	    case 0x5:		/*  -a..0 */
	      newmin = (Tle(xmin,kmin) ? kmin : amk);
	      newmax = TaggedZero;
	      break;
	    case 0x6:		/*  -a..+b */
	      newmin = (Tle(xmin,kmin) ? kmin : amk);
	      newmax = (Tge(xmax,kmax) ? kmax : bmk);
	      break;
	    case 0x7:		/*  -a..sup */
	      newmin = (Tle(xmin,kmin) ? kmin : amk);
	      newmax = kmax;
	      break;
	    case 0x9:		/*   0..0 */
	      newmin = newmax = TaggedZero;
	      break;
	    case 0xa:		/*   0..+b */
	      newmin = TaggedZero;
	      newmax = (Tge(xmax,kmax) ? kmax : bmk);
	      break;
	    case 0xb:		/*   0..sup */
	      newmin = TaggedZero;
	      newmax = kmax;
	      break;
	    case 0xe:		/*  +a..+b */
	      if (Tge(xmax-xmin+TaggedZero,kmax) || Tgt(amk,bmk))
		newmin = TaggedZero, newmax = kmax;
	      else
		newmin = amk, newmax = bmk;
	      break;
	    case 0xf:		/*  +a..sup */
	      newmin = TaggedZero;
	      newmax = kmax;
	      break;
	    default:
	      continue;
	    }
	  
	  /* dom(x) may not be consistent after pruning dom(y) */
	  
	  if (EmptyInterval(ymin,newmin))
	    newmin = ymin;
	  else if (newmin!=ymin)
	    yflag|=2, xflag &= ~1;
	  if (EmptyInterval(newmax,ymax))
	    newmax = ymax;
	  else if (newmax!=ymax)
	    yflag|=2, xflag &= ~1;
	  if (newmin!=ymin || newmax!=ymax)
	    {
	      if (!adjust_bounds(newmin, newmax, ydom, &ymin, &ymax))
		goto fail_or_oflo;
	    }
	}
      else if (!(xflag&1))
	{			/* x /\= {x | x mod k = y} */
	  xflag++;
	  amk = MakeSmall(GetSmall(xmin) % k);
	  bmk = MakeSmall(GetSmall(xmax) % k);
	  if ((TagIsSmall(xmin) && (Tlt(amk,ymin) || Tgt(amk,ymax))) ||
	      (TagIsSmall(xmax) && (Tlt(bmk,ymin) || Tgt(bmk,ymax))))
	    {
	      newmin = Inf;
	      newmax = Sup;
	      while (newmin!=xmin || newmax!=xmax)
		{
		  xflag |= 2, yflag &= ~1;
		  if (!adjust_bounds(newmin, newmax, xdom, &xmin, &xmax))
		    goto fail_or_oflo;
		  amk = MakeSmall(GetSmall(xmin) % k);
		  bmk = MakeSmall(GetSmall(xmax) % k);
		  newmin = ( !TagIsSmall(xmin) ? xmin
			     : Tlt(amk,ymin) ? xmin-amk+ymin
			     : Tgt(amk,ymax) ? xmin-amk+ymin+IStep(k)
			     : xmin);
		  newmax = ( !TagIsSmall(xmax) ? xmax
			     : Tgt(bmk,ymax) ? xmax-bmk+ymax
			     : Tlt(bmk,ymin) ? xmax-bmk+ymax-IStep(k)
			     : xmax);
		}
	    }
	}
    }
				/* Compute prunings */
				/* GC OK from here */
  if (xflag&2)
    request_tell_interval(w, RefTerm(XMutR), RefTerm(XR), xmin, xmax, 2, 3);
  if (yflag&2)
    request_tell_interval(w, RefTerm(YMutR), RefTerm(YR), ymin, ymax, 2, 3);

  ent = (xmin==xmax || k==1);
 ret:
  RefTerm(Actions) = request_done(w, ent, 2, 3);
  if (!fd_check_overflow())
    SP_fail();
  SP_reset_term_refs(r);
  return;
  
fail_or_oflo:
  if (newmin==Sup || newmax==Inf) /* can't represent bounds */
    fd.fd_overflow = TRUE;
  goto ret;
}

