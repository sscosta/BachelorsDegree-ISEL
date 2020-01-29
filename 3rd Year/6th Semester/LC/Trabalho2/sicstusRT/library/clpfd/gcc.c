#include "fd.h"

/***
  Support for global_cardinality/2.
  Algorithms, see J-C. Regin: "Generalized Arc Consistency for
                               Global Cardinality Constraint".
  ***/

#define CSTATE_ONLY 0

#if MULTI_SP_AWARE
#define print_vertex(A1,A2) print_vertex(HIDDEN_ARG, A1,A2)
#define print_graph(A1,A2,A3) print_graph(HIDDEN_ARG, A1,A2,A3)
#define sccs_visit(A1) sccs_visit(HIDDEN_ARG, A1)
#define sccs(A1) sccs(HIDDEN_ARG, A1)
#if 0                           /* not redefined. It uses FD_SETUP_SPENV to access the SPEnv */
#define gcc_destructor(A1) gcc_destructor(HIDDEN_ARG, A1)
#endif

#define cmp_asc_val(A1,A2) cmp_asc_val(HIDDEN_ARG, A1,A2)
#define qsort_asc_valswap(A1,A2,A3,A4) qsort_asc_valswap(HIDDEN_ARG, A1,A2,A3,A4)
#define qsort_asc_valmed3(A1,A2,A3) qsort_asc_valmed3(HIDDEN_ARG, A1,A2,A3)
#define qsort_asc_val(A1,A2) qsort_asc_val(HIDDEN_ARG, A1,A2)
#define gcc_alloc(A1,A2,A3,A4) gcc_alloc(HIDDEN_ARG, A1,A2,A3,A4)
#define gcc_init(A1,A2,A3) gcc_init(HIDDEN_ARG, A1,A2,A3)
#define gcc_filter(A1) gcc_filter(HIDDEN_ARG, A1)
#define gcc_new_domain(A1,A2,A3,A4) gcc_new_domain(HIDDEN_ARG, A1,A2,A3,A4)
#endif /* MULTI_SP_AWARE */


typedef long VERTEX;
static long const VEOL = 0xffffff; /* should be a big value see scc_visit */
static long const CEOL = -1;     /* should be negative */

struct gcc_graph
{
  void (SPCDECL *destructor)(void*); /* [PM] 3.9b4 changed name to destructor for consistency */
#if MULTI_SP_AWARE
  SPEnv *spenv;
#endif /* MULTI_SP_AWARE */

  SP_term_ref varrefbase;
  SP_term_ref valrefbase;
  long stamp;
  int norefs;
  int nvars;
  int nvals;
  int nvartargets;
  int nvaltargets;
#if CSTATE_ONLY
  int nvars_back;
  int nvals_back;
#endif
  VERTEX *valtarget;            /* 2*nvals */
  VERTEX *vartarget;            /* 2*nvars */
  VERTEX *edge;                 /* 2*nvars*nvals */
  VERTEX *stack;                /* nvals+nvars+2 */
  TAGGED *dom;                  /* Domain term for variables, GC-safe */
  TAGGED *mincount;		/* min(Count) */
  TAGGED *maxcount;		/* max(Count) */
  TAGGED *setcount;		/* set(Count), volatile, GC-safe */
  long **flow;                  /* a matrix */

  struct {                      /* source, vals, vars, target */
    long *component;            /* auxiliary variable for some algorithms */
    long *visited;              /* auxiliary variable for some algorithms */
    long *val;                  /* the FD value for value vertices */
    VERTEX *dad;                /* aux. var.: previous vertex in a path */
    long *out_degree;           /* # outbound edges (forward edges) */
    long *in_degree;            /* #  inbound edges (backward edges) */
    VERTEX **neighs;            /* array of pointers to the neighbours */
  } vertex;

  /*
  TAGGED[nvars];                         // dom
  struct gcc_vertex[nvals+nvars+2];      // the vertices
  VERTEX[2*nvals]                        // valtarget
  VERTEX[2*nvars]                        // vartarget
  VERTEX[(nvals+nvals*nvars+nvars+1)*2]; // neighbours (edges)
  VERTEX[nvals+nvars+2];                 // stack or queue
  TAGGED[nvals];                         // mincount
  TAGGED[nvals];                         // maxcount
  TAGGED[nvals];                         // setcount
  long *[nvals+nvars+2];                 // flow pointers
  long[(nvals+nvars+2)*(nvals+nvars+2)]; // flow array
  */
};

#define TVAR(V) (pdata->vartarget[V])
#define VAL(V) (pdata->vertex.val[V])
#define DOM(V) (pdata->dom[(V)-pdata->nvals-1])
#define NEIGHS(V) (pdata->vertex.neighs[V])
#define GNEIGH(G,V,I) ((G)->vertex.neighs[V][I])
#define NEIGH(V,I) GNEIGH(pdata,V,I)
#define GOUT_DEGREE(G,V) ((G)->vertex.out_degree[V])
#define OUT_DEGREE(V) GOUT_DEGREE(pdata,V)
#define GIN_DEGREE(G,V) ((G)->vertex.in_degree[V])
#define IN_DEGREE(V) GIN_DEGREE(pdata,V)
#define GCOMPONENT(G,V) ((G)->vertex.component[V])
#define COMPONENT(V) GCOMPONENT(pdata,V)
#define GVISITED(G,V) ((G)->vertex.visited[V])
#define VISITED(V) GVISITED(pdata,V)
#define DAD(V) (pdata->vertex.dad[V])
#define TRefVarAttr(V) RefTerm(pdata->varrefbase + 2*(V))
#define TRefVarVar(V) RefTerm(pdata->varrefbase + 2*(V) + 1)

#define TVAL(I) (pdata->valtarget[I])
/* these are only valid if V is a value vertex */
#define LIMIT(V) GetSmall(pdata->mincount[(V)-1])
#define CAPACITY(V) GetSmall(pdata->maxcount[(V)-1])
#define MINCOUNT_T(V) (pdata->mincount[(V)-1])
#define MAXCOUNT_T(V) (pdata->maxcount[(V)-1])
#define SETCOUNT(V) (pdata->setcount[(V)-1])
#define TRefValAttr(V) RefTerm(pdata->valrefbase + 2*(V) - 2)
#define TRefValVar(V) RefTerm(pdata->valrefbase + 2*(V) - 1)

#define FLOW(U,V) (pdata->flow[U][V])

/* Invariant: VISITED(V)==VEOL and COMPONENT(V)==CEOL iff V is not a target.
   There is one small exception: after expand_vars or expand_vals
   the resurrected vertices will have EOLs in these fields.
*/

/*
  calculate the residual capacity of the edge u->v
  PRE: u and v are neighbours
*/
static long res(struct gcc_graph *pdata, VERTEX u, VERTEX v)
{
  if (u == 0)                   /* from start to target or to vals */
    return v > pdata->nvals ? FLOW(v, u) : CAPACITY(v) - FLOW(u, v);
  if (v == 0)                   /* to start from target or from vals */
    return u > pdata->nvals ? pdata->nvars - FLOW(u,v) : FLOW(v,u) - LIMIT(u);
  /* other forward or backward edges */
  return u < v ? 1 - FLOW(u, v) : FLOW(v, u);
}


#if DBG > 1
#define PRINT_GRAPH(print, where) print_graph(print, where, pdata)
static void print_vertex MAGIC (HIDDEN_PROTO
				struct gcc_graph *pdata, VERTEX v)
{
  int i, degree;
  SP_printf("%ld(%ld): ", v, COMPONENT(v));
  for (i = 0, degree = OUT_DEGREE(v); i < degree; ++i) {
    VERTEX u = NEIGH(v, i);
    SP_printf("o(%ld,%ld,%ld) ", u, FLOW(v,u), res(pdata,v,u));
  }
  for (i = degree, degree += IN_DEGREE(v); i < degree; ++i) {
    VERTEX u = NEIGH(v, i);
    SP_printf("i(%ld,%ld,%ld) ", u, FLOW(v,u), res(pdata,v,u));
  }
  SP_printf("\n");
}


static void print_graph MAGIC (HIDDEN_PROTO
			       BOOL print,
			       char const *where,
			       struct gcc_graph *pdata)
{
  int i;

  if (!print) return;

  SP_printf("%s\nvals:", where);
  for (i = 0; i < pdata->nvaltargets; ++i)
    SP_printf(" %ld", TVAL(i));
  SP_printf(" #vars: %d\n", pdata->nvartargets);
  print_vertex(pdata, 0);
  for (i = 0; i < pdata->nvaltargets; ++i)
    print_vertex(pdata, TVAL(i));
  for (i = 0; i < pdata->nvartargets; ++i)
    print_vertex(pdata, TVAR(i));
  print_vertex(pdata, pdata->nvals+pdata->nvars+1);
  SP_printf("\n");
}

#else
#define PRINT_GRAPH(print, where)
#endif

/* 
   look for a directed path from s to t (in the residual graph) that
   doesn't include the edge s->t (because this function is only called
   from feasible_flow, we know that s->t is not part of the residual graph
   (it is infeasible))

   PRE: s != t
*/
static BOOL augmenting_path(struct gcc_graph *pdata,
			    VERTEX s, VERTEX t)
{
  VERTEX *vqueue = pdata->stack;
  int top = 0, bottom = 0;
  long i, ilim;

  /* if (s == t) return FALSE; */

  /* Now we do a breadth first search to find t. */

  VISITED(0) = 0;
  ilim = pdata->nvaltargets;
  for (i = 0; i < ilim; ++i)
    VISITED(TVAL(i)) = 0;
  ilim = pdata->nvartargets;
  for (i = 0; i < ilim; ++i)
    VISITED(TVAR(i)) = 0;
  VISITED(pdata->nvals+pdata->nvars+1) = 0;

  DAD(s) = -1;                  /* s has no dad :( */
  VISITED(s) = 1;
  for (;;) {
    long degree = OUT_DEGREE(s) + IN_DEGREE(s);
    for (i = 0; i < degree; ++i) {
      VERTEX v = NEIGH(s,i);
      if (!VISITED(v) && 0 < res(pdata, s, v)) {
        DAD(v) = s;
        if (v == t) return TRUE; /* found a path */
        VISITED(v) = 1;
        vqueue[top++] = v;
      }
    }
    if (top == bottom) return FALSE; /* t is not reachable from s */
    s = vqueue[bottom++];
  }
}


/*
  calulate the minimum residual capacity along the path (given by the dad
  pointers) and adjust the flow accordingly
*/
static void adjust_flow(struct gcc_graph *pdata,
			VERTEX s, VERTEX t)
{
  VERTEX d, x = s;
  long f = res(pdata, s, t);

  /* decrease f to the minimum flow on the path if necessary */
  for (d = DAD(x); d >= 0; x = d, d = DAD(x)) {
    long fds = res(pdata, d, x);
    if (f > fds) f = fds;
  }

  /* now adjust the flow along the cycle */
  FLOW(t, s) = -(FLOW(s, t) += f);
  for (d = DAD(s); d >= 0; s = d, d = DAD(s)) /* if we didn't store the flow */
    FLOW(s, d) = -(FLOW(d, s) += f);     /*  for both ways, we should be */
}                                        /*  when modifying it */


/*
  try to create a feasible flow if possible and return true iff succeeded
*/
static BOOL feasible_flow(struct gcc_graph *pdata)
{
  long i, ilim = pdata->nvaltargets;
  for (i = 0; i < ilim; ++i) {
    VERTEX v = TVAL(i);
    long limit = LIMIT(v);
    while (FLOW(0,v) < limit) {
      if (!augmenting_path(pdata, v, 0)) return FALSE;
      adjust_flow(pdata, 0, v);
    }
  }
  return TRUE;
}


/*
  Partial breadth first search (in the residual graph) from the start:
  only vertices reachable from start are visited.  When the function is
  finised, the component field gives the depth from the start of the given
  vertex plus 1.  If this field is 0, then it isn't reachable from the start.
*/
static void bfs(struct gcc_graph *pdata)
{
  int top = 0, bottom = 0;
  long i, t = pdata->nvals + pdata->nvars + 1;
  VERTEX *vqueue = pdata->stack, x = 0;

  for (i = 0; i < pdata->nvaltargets; ++i)
    COMPONENT(TVAL(i)) = 0;
  for (i = 0; i < pdata->nvartargets; ++i)
    COMPONENT(TVAR(i)) = 0;
  COMPONENT(t) = 0;
  COMPONENT(0) = 1;
  for (;;) {
    long degree = OUT_DEGREE(x) + IN_DEGREE(x);
    for (i = 0; i < degree; ++i) {
      VERTEX v = NEIGH(x, i);
      if (!COMPONENT(v) && 0 < res(pdata, x, v)) {
        COMPONENT(v) = COMPONENT(x)+1;
        vqueue[top++] = v;
      }
    }
    if (top == bottom) return;
    x = vqueue[bottom++];
  }
}


/*
  A flow is blocking iff on each directed path from the start to the sink
  there is a saturated edge.

  PRE: sizeof(long) <= sizeof(VERTEX)

  the component fields give the breadth-first search depth of the
  vertices starting from the start vertex in the residual graph.

  The edges x->v where v->component == x->component+1 form a DAG.  This
  function augments the current flow until it blocks this DAG.
*/
static void blocking_flow(struct gcc_graph *pdata)
{
  VERTEX const t = pdata->nvals + pdata->nvars + 1;

  long i;

  for (i = 0; i < pdata->nvaltargets; ++i)
    VISITED(TVAL(i)) = 0;
  for (i = 0; i < pdata->nvartargets; ++i)
    VISITED(TVAR(i)) = 0;
  VISITED(t) = 0;

  DAD(0) = -1;                  /* the source vertex has no dad */
  for (;;) {
    VERTEX x = 0;   /* start a depth first search from 0 to t */
    long *stack = (long *)pdata->stack;
    i = 0;
    while (x != t) {
      VERTEX v=0;
      long degree = OUT_DEGREE(x) + IN_DEGREE(x);
      for (; i < degree; ++i) {
        v = NEIGHS(x)[i];
        if (COMPONENT(v) == COMPONENT(x)+1 && !VISITED(v) && 0<res(pdata,x,v))
          break;
      }
      if (i >= degree) {         /* there's no way out from x */
        if (x == 0) return;     /* x is the source */
        VISITED(x) = 1;
        x = DAD(x);             /* take one step back */
        i = *--stack+1;         /* restore the old neighbour index */
        continue;
      }
      DAD(v) = x;
      x = v;                    /* take one step forward */
      *stack++ = i;             /* save the old neighbour index */
      i = 0;                    /*   and start the new one */
    }
    adjust_flow(pdata, t, 0);
  }
}


/*
  Find the maximum flow (using Dinic's algorithm) and return its value.
*/
static long maximum_flow(struct gcc_graph *pdata)
{
  VERTEX target = pdata->nvals+pdata->nvars+1;
  IN_DEGREE(0) = 0;             /* pretend that there's no t->s edge */
  for (;;) {
    bfs(pdata);
    if (!COMPONENT(target)) {   /* target is not reachable */
      IN_DEGREE(0) = 1;         /* there's actually a t->s edge */  
      return FLOW(target,0);    /* flow of the edge t->s */
    }
    blocking_flow(pdata);
  }
}


/*
  I use global variables for the "static" parameters of sccs_visit() to use
  less stack space.  If it needs to be thread-safe, they could be all
  packed in a struct allocated in sccs() and passed to sccs_visit in the
  form of a pointer.
*/

/*
 A recursive function that finds strongly connected components [Tarjan'72].
 Code taken roughly from [Sedgewick, Algorithms in C, page 482] via
 alldistinct.c.
*/
static int sccs_visit MAGIC (HIDDEN_PROTO
			     VERTEX x)
{
  VERTEX v = -1;                /* different from all valid vertices */
  long i, degree = GOUT_DEGREE(fd.scc_graph, x) + GIN_DEGREE(fd.scc_graph, x);
  int min = ++fd.scc_visited;

  GVISITED(fd.scc_graph, x) = fd.scc_visited;
  fd.scc_stack[fd.scc_index++] = x;
  for (i = 0; i < degree; ++i) {
    v = GNEIGH(fd.scc_graph, x, i);
    if (0 < res(fd.scc_graph, x, v)) {
      int m = GVISITED(fd.scc_graph, v) ? GVISITED(fd.scc_graph, v) : sccs_visit(v);
      if (m < min) min = m;
    }
  }
  if (min == GVISITED(fd.scc_graph, x)) {
    ++fd.scc_component;
    while (v != x) {
      v = fd.scc_stack[--fd.scc_index];
      GCOMPONENT(fd.scc_graph, v) = fd.scc_component;
      GVISITED(fd.scc_graph, v) = VEOL; /* High value, so that
                                        this vertex will be ignored
                                        in the future search. */
    }
  }
  return min;
}


static void sccs MAGIC (HIDDEN_PROTO
			struct gcc_graph *pdata)
{
  long i;
  /* long t = pdata->nvals+pdata->nvars+1; */

  fd.scc_component = 0;
  fd.scc_visited = 0;
  fd.scc_index = 0;
  fd.scc_graph = pdata;
  fd.scc_stack = pdata->stack;

  for (i = 0; i < pdata->nvaltargets; ++i)
    VISITED(TVAL(i)) = 0;
  for (i = 0; i < pdata->nvartargets; ++i)
    VISITED(TVAR(i)) = 0;
/*    VISITED(t) = 0; */

  IN_DEGREE(0) = 0;             /* pretend that there's no t->s edge */
  sccs_visit(0);
  for (i = 0; i < pdata->nvaltargets; ++i) {
    VERTEX v = TVAL(i);
    if (!VISITED(v)) sccs_visit(v);
  }
  for (i = 0; i < pdata->nvartargets; ++i) {
    VERTEX v = TVAR(i);
    if (!VISITED(v)) sccs_visit(v);
  }
/*    if (!VISITED(t)) sccs_visit(t); */
  IN_DEGREE(0) = 1;             /* there's a t->s edge */
}


static void remove_var(struct gcc_graph *pdata,
		       VERTEX val, VERTEX var)
{
  int i, lim = OUT_DEGREE(val)--; /* lim = --OUT_DEGREE(val)+IN_DEGREE(val); */
  for (i = 0; NEIGH(val, i) != var; ++i);
  for (; i < lim; ++i)
    NEIGH(val, i) = NEIGH(val, i+1);
}


static void compress_edges(struct gcc_graph *pdata)
{
  long i, js, jd, ilim = pdata->nvaltargets, jlim;
  for (i = 0; i < ilim; ++i) {  /* first the values */
    VERTEX u = TVAL(i);
    jlim = OUT_DEGREE(u);
    for (jd = js = 0; js < jlim; ++js) {
      VERTEX v = NEIGH(u, js);
      if (COMPONENT(v) == COMPONENT(u) || FLOW(u, v) != 0)
        NEIGHS(u)[jd++] = v;
    }
    /* now move the inbound edge to its new place */
    NEIGH(u, jd) = NEIGH(u, js);
    OUT_DEGREE(u) = jd;
  }
  ilim = pdata->nvartargets;
  for (i = 0; i < ilim; ++i) {  /* then the variables */
    VERTEX u = TVAR(i);
    jlim = IN_DEGREE(u)+1;
    for (jd = js = 1; js < jlim; ++js) {
      VERTEX v = NEIGH(u, js);
      if (COMPONENT(u) == COMPONENT(v) || FLOW(v, u) != 0)
        NEIGHS(u)[jd++] = v;
    }
    IN_DEGREE(u) = jd-1;
  }
}


/*
  These are values that are NOT fd integers.  We set the value fields of
  the start, variable and target vertices to these values.  They are not
  essential, actually, we never test if a gien vertex has one of these
  values.
*/
static long const START_VERTEX  = CLPFD_MAXINT-2;
static long const VAR_VERTEX    = CLPFD_MAXINT-1;
static long const TARGET_VERTEX = CLPFD_MAXINT-0;

static void SPCDECL gcc_destructor(void *pdata_v)
{
  struct gcc_graph *pdata = (struct gcc_graph *)pdata_v;
  FD_SETUP_SPENV(pdata->spenv)

  SP_free_term_refs(pdata->varrefbase, pdata->norefs);
  SP_free(pdata);
}


static int cmp_asc_val MAGIC (HIDDEN_PROTO
			      VERTEX *t1, VERTEX *t2)
{
  struct gcc_graph *pdata = fd.gdata;
  return VAL(*t1) - VAL(*t2);
}

#define QType VERTEX
#define QCmp  cmp_asc_val
#define QSort qsort_asc_val
#include "qsort.ic"


/* At present, called only from one place. Could be INLINEd. */
static struct gcc_graph *gcc_alloc MAGIC (HIDDEN_PROTO Argdecl,
					  int novars,int novals,TAGGED handle)
{
  char *ptr;
  int i;
  int const nodes = novars+novals+2;
  int const extra =
    novars*sizeof(TAGGED) +
    nodes*(2*sizeof(int)+3*sizeof(long)+sizeof(VERTEX)+sizeof(VERTEX *)) +
    2*(novals+novars)*sizeof(VERTEX) +
    (novals+novals*novars+novars+1)*2*sizeof(VERTEX) +
    nodes*sizeof(VERTEX) +
    3*novals*sizeof(TAGGED) +
    nodes*sizeof(long *) +
    nodes*nodes*sizeof(long);

  struct gcc_graph *pdata =
    Palloc(struct gcc_graph,
           extra,
           handle);
  pdata->destructor = gcc_destructor;
  FD_STORE_SPENV(pdata->spenv);
  pdata->norefs = 2*novars + 2*novals;
  pdata->varrefbase = SP_alloc_term_refs(pdata->norefs);
  pdata->valrefbase = pdata->varrefbase + 2*novars;
  pdata->nvartargets = pdata->nvars = novars;
  pdata->nvaltargets = pdata->nvals = novals;
#if CSTATE_ONLY
  pdata->nvars_back = novars;
  pdata->nvals_back = novals;
#endif
  ptr = (char *)(pdata+1);
  pdata->dom = (TAGGED *)ptr;
  ptr += novars*sizeof(TAGGED);

  pdata->vertex.component = (long *)ptr;
  ptr += nodes*sizeof(long);
  pdata->vertex.visited = (long *)ptr;
  ptr += nodes*sizeof(long);
  pdata->vertex.val = (long *)ptr;
  ptr += nodes*sizeof(long);
  pdata->vertex.dad = (VERTEX *)ptr;
  ptr += nodes*sizeof(VERTEX);
  pdata->vertex.out_degree = (long *)ptr;
  ptr += nodes*sizeof(long);
  pdata->vertex.in_degree  = (long *)ptr;
  ptr += nodes*sizeof(long);
  pdata->vertex.neighs = (VERTEX **)ptr;
  ptr += nodes*sizeof(VERTEX *);

  pdata->valtarget = (VERTEX *)ptr;
  ptr += 2*novals*sizeof(VERTEX);
  pdata->vartarget = (VERTEX *)ptr;
  ptr += 2*novars*sizeof(VERTEX);

  pdata->edge = (VERTEX *)ptr;
  ptr += (novals+novals*novars+novars+1)*2*sizeof(VERTEX);
  pdata->stack = (VERTEX *)ptr;
  ptr += nodes*sizeof(VERTEX);
  pdata->mincount = (TAGGED *)ptr;
  ptr += novals*sizeof(TAGGED);
  pdata->maxcount = (TAGGED *)ptr;
  ptr += novals*sizeof(TAGGED);
  pdata->setcount = (TAGGED *)ptr;
  ptr += novals*sizeof(TAGGED);
  pdata->flow = (long **)ptr;
  ptr += nodes*sizeof(long *);
  memset(ptr, 0, nodes*nodes*sizeof(long)); /* 0 flow initially */
  for (i = 0; i < nodes; ++i) {
    pdata->flow[i] = (long *)ptr;
    ptr += nodes*sizeof(long);
  }
  return pdata;
}


/* At present, called only from one place. Could be INLINEd. */
static void gcc_init MAGIC (HIDDEN_PROTO Argdecl,
			    struct gcc_graph *pdata,
			    TAGGED vals)
{
  int i, vx = 0, nx = 0, novals = pdata->nvals, novars = pdata->nvars;
  long t = novals+novars+1;     /* the target vertex */

  /* source vertex */
  NEIGHS(0) = pdata->edge;
  IN_DEGREE(0) = 1;
  VAL(0) = START_VERTEX;
  OUT_DEGREE(0) = novals;
  for (i = 0; i < novals; ++i)
    NEIGH(0, i) = i+1;
  NEIGH(0, i) = t;
  ++vx;
  nx = novals+1;

  /* value vertices + capacity + limit */
  for (i = 0; i < novals; ++i) {
    TAGGED telt, t1;
    TVAL(i) = vx;
    NEIGHS(vx) = pdata->edge + nx;
    IN_DEGREE(vx) = 1;
    DerefCar(telt, vals);
    DerefCdr(vals, vals);
    DerefArg(t1, telt, 1);	/* get value to count */
    VAL(vx) = GetSmall(t1);
    ++vx;
    nx += novars+1;
    DerefArg(t1, telt, 2);	/* get count variable */
    TRefValVar(i+1) = t1;	/* protect FD variable */
    t1 = check_argument(w,t1,Inf,Sup,Sup); /* attribute */
    TRefValAttr(i+1) = t1;	/* protect attribute */
  }

  /* var vertices */
  for (i = 0; i < novars; ++i) {
    TVAR(i) = vx;
    NEIGHS(vx) = pdata->edge + nx;
    NEIGH(vx, 0) = t;
    OUT_DEGREE(vx) = 1;
    VAL(vx) = VAR_VERTEX;
    ++vx;
    nx += novals+1;
  }

  /* target vertex */
  NEIGHS(vx) = pdata->edge + nx;
  OUT_DEGREE(vx) = 1;
  VAL(vx) = TARGET_VERTEX;
  NEIGH(vx,0) = 0;
  IN_DEGREE(vx) = novars;
  for (i = 1; i <= novars; ++i)
    NEIGH(vx, i) = novals+i;
}


/*
  Should be called for an already non-existent edge val->var.  It will
  zero the flow on this edge and on the edge start->val.  The original
  flow is returned.
*/
static long fix_flow(struct gcc_graph *pdata,
		     VERTEX val, VERTEX var)
{
  long **flow = pdata->flow, *fp = &flow[val][var], f = *fp;
  if (f) {              /* nonzeo flow */
    flow[var][val] = *fp = 0;
    flow[val][0]  = -(flow[0][val]  -= f);
/*      flow[t][var] = -(flow[var][t] -= f); */
/*      flow[0][t]  = -(flow[t][0]  -= f); */
  }
  return f;
}


/*
  Recompute the variable part of the graph repr. from scratch.
*/
static void gcc_refresh(struct gcc_graph *pdata)
{
  int i, novars = pdata->nvartargets, novals = pdata->nvaltargets;
  VERTEX t = pdata->nvals+pdata->nvars+1;

  /* part I: initialise out_grade for value vertices */
  for (i = 0; i < novals; ++i)
    OUT_DEGREE(TVAL(i)) = 0;

  /* part II: variable vertices & second part for value vertices */
  for (i = 0; i < novars; ++i) {
    int in_degree = 0, valvertex = 0, varvertex = TVAR(i);
    long df = 0;
    FDITER it;
    
    fditer_init(&it, DomainSet(DOM(varvertex)));
    while (!fditer_empty(&it)) {
      long cur = GetSmall(fditer_next(&it));
      
      while (VAL(++valvertex) != cur)
	df += fix_flow(pdata, valvertex, varvertex);
      NEIGHS(valvertex)[OUT_DEGREE(valvertex)++] = varvertex;
      NEIGHS(varvertex)[++in_degree] = valvertex; /* preincrement!!! */
    }
    IN_DEGREE(varvertex) = in_degree;
    /* update the flow for values above the max. of the domain */
    while (++valvertex <= pdata->nvals)
      df += fix_flow(pdata, valvertex, varvertex);
    /* the matching statements are commented out in fix_flow */
    FLOW(t,varvertex) = -(FLOW(varvertex,t) -= df);
    FLOW(0,t) = -(FLOW(t,0) -= df);
  }

  /* part III: update the inbound edge for all value vertices */
  for (i = 0; i < novals; ++i) {
    VERTEX v = TVAL(i);
    NEIGH(v, OUT_DEGREE(v)) = 0;
  }
}


/*
  Update the graph repr., deleting edges that are gone.
  The flow needs to be updated as well.
*/
static void gcc_update(struct gcc_graph *pdata)
{
  int i, top = 0,  novars = pdata->nvartargets;
  VERTEX *stack = pdata->stack, t = pdata->nvals+pdata->nvars+1;

  /* We do the pruning in reversed order.  I don't know why. */
  for (i = 0; i < novars; ++i) {
    VERTEX varvertex = TVAR(i);
    if (GetSmall(DomainSize(DOM(varvertex))) < IN_DEGREE(varvertex))
      stack[top++] = varvertex;
  }

  while (0 < top) {
    VERTEX varvertex = stack[--top], *edges = NEIGHS(varvertex)+1;
    VERTEX valvertex;
    int j = 0, k = 0;
    long df = 0;
    FDITER it;

    fditer_init(&it, DomainSet(DOM(varvertex)));
    while (!fditer_empty(&it)) {
      long cur = GetSmall(fditer_next(&it));
      
      while (VAL(valvertex = edges[j++]) < cur) {/* dangerous use of macro */
	df += fix_flow(pdata, valvertex, varvertex);
	remove_var(pdata, valvertex, varvertex);
      }
      if (VAL(valvertex) == cur)
	edges[k++] = valvertex;
    }
    while (j < IN_DEGREE(varvertex)) {
      VERTEX valvertex = edges[j++];
      df += fix_flow(pdata, valvertex, varvertex);
      remove_var(pdata, valvertex, varvertex);
    }
    IN_DEGREE(varvertex) = k;
    /* the matching statements are commented out in fix_flow */
    FLOW(t,varvertex) = -(FLOW(varvertex,t) -= df);
    FLOW(0,t) = -(FLOW(t,0) -= df);
  }
}


static BOOL gcc_filter MAGIC (HIDDEN_PROTO
			      struct gcc_graph *pdata)
{
  if (!feasible_flow(pdata)) return FALSE;
  if (maximum_flow(pdata) != pdata->nvars) return FALSE;
  sccs(pdata);
  compress_edges(pdata);
  return TRUE;
}


/* maintain the variant res(S,Yi)>=0 for all values Yi */
static void
maintain_capacities(struct gcc_graph *pdata)
{
  int i, j;
  int ilim = pdata->nvaltargets;
  VERTEX t = pdata->nvals+pdata->nvars+1;
  
  for (i=0; i < ilim; i++) {
    VERTEX y = TVAL(i);
    long f = -res(pdata,0,y);

    if (f>0) {			/* capacity exceeded by f */
      FLOW(0,y) -= f;		/* repair source/sink flows */
      FLOW(y,0) += f;
      FLOW(t,0) -= f;
      FLOW(0,t) += f;
      for (j=0; f>0; j++) {	/* consider each neighbor */
	VERTEX x = NEIGH(y,j);
	long fyx = FLOW(y,x);

	if (0<fyx && fyx <= f) { /* consume some or all of f */
	  FLOW(y,x) = 0;
	  FLOW(x,y) = 0;
	  FLOW(x,t) = 0;
	  FLOW(t,x) = 0;
	  f -= fyx;
	} else if (0<fyx) {	/* consume rest of f */
	  FLOW(y,x) -= f;
	  FLOW(x,y) += f;
	  FLOW(x,t) -= f;
	  FLOW(t,x) += f;
	  f = 0;
	}
      }	  
    }
  }
}


/* adjust bounds of counts */
/* returns delay=0, iterate=1, or fail=2 */
static int
adjust_count_bounds(struct gcc_graph *pdata)
{
  int rc = 0;
  long minsum = 0;
  long maxsum = 0;
  long diff;
  int i, j, lb, ub, jlim, f;
  int ilim = pdata->nvaltargets;
  int nvars = pdata->nvartargets;

  /* Phase 1: infer changes form the value network */
  
  for (i=0; i < ilim; i++) {
    VERTEX y = TVAL(i);
    jlim = OUT_DEGREE(y);
    /* adjust lower bound */
    lb = 0;
    for (j=0; j<jlim; j++)
      if (IN_DEGREE(NEIGH(y,j))==1)
	lb++;
    if (LIMIT(y)<lb || CAPACITY(y)>jlim) {
      if (!adjust_bounds(MakeSmall(lb), MakeSmall(jlim),
			 SETCOUNT(y), &MINCOUNT_T(y), &MAXCOUNT_T(y)))
	return 2;
      f = FLOW(0,y);
      rc |= (LIMIT(y)>f || CAPACITY(y)<f ||
	     LIMIT(y)>lb || CAPACITY(y)<jlim /* 3.9 */);
    }
    minsum += LIMIT(y);
    maxsum += CAPACITY(y);
  }
  
  /* Phase 2: infer changes from C1+...+Cm = nvars */
  /* Iterated until fixpoint */
  
  do {
    diff = maxsum-minsum;
    for (i=0; i < ilim; i++) {
      VERTEX y = TVAL(i);
  
      minsum -= LIMIT(y);
      maxsum -= CAPACITY(y);
      lb = nvars - maxsum;
      ub = nvars - minsum;
      if (LIMIT(y)<lb || CAPACITY(y)>ub) {
	if (!adjust_bounds(MakeSmall(lb), MakeSmall(ub),
			   SETCOUNT(y), &MINCOUNT_T(y), &MAXCOUNT_T(y)))
	  return 2;
	f = FLOW(0,y);
	rc |= (LIMIT(y)>f || CAPACITY(y)<f);
      }
      minsum += LIMIT(y);
      maxsum += CAPACITY(y);
    }
  } while (diff != maxsum-minsum);

  return rc;
}



#define EMIT_INTERVAL(H,B,E) \
      H = numstack_alloc(4); \
      *valuep = MakeList(H); \
      H[0] = MakeList(H+2); \
      valuep = H+1; \
      H[2] = (B); \
      H[3] = (E);

/* precond: n>=1 */
static TAGGED gcc_new_domain MAGIC (HIDDEN_PROTO
				    struct gcc_graph *pdata,
				    int i, int n, VERTEX vertex)
{
  int last = i+n;
  TAGGED b, e, e1;
  TAGGED value, *h, *valuep = &value;

  b = e = MakeSmall(VAL(NEIGHS(vertex)[i++]));
  while (i<last) {
    e1 = e;
    e = MakeSmall(VAL(NEIGHS(vertex)[i++]));
    if (e!=e1+IStep(1)) {
      EMIT_INTERVAL(h,b,e1);
      b = e;
    }
  }
  EMIT_INTERVAL(h,b,e);
  *valuep = EmptySet;
  return value;
}


/* the partition needs to be _stable_ to maintain edge order */
static void contract_vars(struct gcc_graph *pdata)
{
  int imax = pdata->nvartargets;
  int n = pdata->nvars;
  VERTEX *target = pdata->vartarget;
  int i, j, k;
    
  /* separate into nonground (0..) and ground (n..) */
  for (i=0, j=0, k=n; i<imax; i++) {
    VERTEX current = target[i];
    
    if (IN_DEGREE(current)>1 /*new*/ || VISITED(NEIGH(current,1))!=VEOL)
      target[j++] = current;
    else {
      VISITED(current) = VEOL;	/* maintain invariant! */
      COMPONENT(current) = CEOL; /* maintain invariant! */
      target[k++] = current;
    }
  }
  if (i==j) return;

  pdata->nvartargets = j;

  while (n<k)
    target[j++] = target[n++];
}


/* the partition needs to be _stable_ to maintain edge order */
static void contract_vals(struct gcc_graph *pdata)
{
  int imax = pdata->nvaltargets;
  int n = pdata->nvals;
  VERTEX *target = pdata->valtarget;
  int i, j, k;
  
  /* separate into nonground (0..) and ground (n..) */
  for (i=0, j=0, k=n; i<imax; i++) {
    VERTEX current = target[i];
    BOOL active = FALSE;
    int neighbour = OUT_DEGREE(current);
    
    while (--neighbour >= 0)
      if (IN_DEGREE(NEIGH(current,neighbour)) > 1) {
        active = TRUE;
        break;
      }
    if (active) {
      VISITED(current) = 0;	/* maintain invariant! */
      target[j++] = current;
    } else {
      VISITED(current) = VEOL;	/* maintain invariant! */
      COMPONENT(current) = CEOL; /* maintain invariant! */
      target[k++] = current;
    }
  }
  if (i==j) return;
  
  pdata->nvaltargets = j;

  while (n<k)
    target[j++] = target[n++];
}

/*
  '$fd_gcc'(+State0, -State, -Actions) :-
  State0 is f(NVars,NVals,Vars,Vals,Handle,Stamp).
  Vals is a keysorted list of Val-CountVar terms.  
  It is assumed, that the domains of Vars
  have been restricted to the available values.

  State similarly,
  Actions is a list of prunings etc.
*/
void SPCDECL
prolog_fd_gcc MAGIC (HIDDEN_PROTO
		     SP_term_ref State0,
		     SP_term_ref State,
		     SP_term_ref Actions)
{
  WAMENV;
  int i, ent = -1;
  long state_stamp;
  TAGGED handle;
  struct gcc_graph *pdata;
  BOOL committed;
  int change = 1;

  w->numstack_end = NULL;

/*    X(0) = RefTerm(State0); */
  (void)State0;
  X(2) = atom_nil;		/* actions list */
  RefTerm(State) = unify_output_state(w,&handle,&state_stamp,&committed);

  if (!IsVar(handle)) {		/* got [Flag | '$free'(Ptr)] */
    fd.gdata = pdata = Pdata(struct gcc_graph, handle);
  } else {			/* build persistent state */
				/* compute novars, novals, all */
    TAGGED tlvec;
    int novals, novars;

    DerefArg(tlvec, X(0), 1);	/* get #vars */
    novars = GetSmall(tlvec);
    DerefArg(tlvec, X(0), 2);	/* get #vals */
    novals = GetSmall(tlvec);

    /* ensure heap space for integer attributes */

    X(3) = handle;
    RequireHeap((novars+novals)*INT_ATTRIBUTE_SIZE, 4);
    handle = X(3);
    
    fd.gdata = pdata = gcc_alloc(w, novars, novals, handle);
    pdata->stamp = state_stamp-1; /* DON'T trust initially */

    /* initialise the persistent parts */
    DerefArg(tlvec, X(0), 3);	/* get variables */
    for (i = 0; i < novars; ++i) {
      TAGGED telt;
      
      DerefCar(telt, tlvec);
      DerefCdr(tlvec, tlvec);
      TRefVarVar(i) = telt;	/* protect FD variable */
      telt = check_argument(w,telt,Inf,Sup,Sup);
      TRefVarAttr(i) = telt;	/* protect attribute */
    }

    DerefArg(tlvec, X(0), 4);	/* get values */
    gcc_init(w, pdata, tlvec);
  }

                                /* RESUME HERE */
  if (state_stamp != pdata->stamp) {
    int nvars1, nvals1;
#if CSTATE_ONLY
    nvars1 = pdata->nvars_back;
    nvals1 = pdata->nvals_back;
#else
    TAGGED telt;
    DerefArg(telt,X(0),1);
    nvars1 = GetSmall(telt);
    DerefArg(telt,X(0),2);
    nvals1 = GetSmall(telt);
#endif
    if (pdata->nvartargets < nvars1) {
      pdata->nvartargets = nvars1;
      qsort_asc_val(pdata->vartarget,nvars1);
    }
    if (pdata->nvaltargets < nvals1) {
      pdata->nvaltargets = nvals1;
      qsort_asc_val(pdata->valtarget,nvals1);
    }
  }

  /* Always refresh dom fields */
  for (i = 0; i < pdata->nvartargets; ++i) {
    VERTEX vertex = TVAR(i);
    TAGGED t1 = TRefVarAttr(vertex-pdata->nvals-1); /* get attribute */
    
    DerefAttribute(t1,t1);
    DOM(vertex) = t1;
  }

  for (i = 0; i < pdata->nvaltargets; ++i) {
    VERTEX vertex = TVAL(i);
    TAGGED t1 = TRefValAttr(vertex); /* get attribute */
    TAGGED *arg;

    DerefAttribute(t1,t1);
    arg = TagToArg(t1,0);
    SETCOUNT(vertex) = arg[1]; /* set */
    MINCOUNT_T(vertex) = arg[2]; /* min */
    MAXCOUNT_T(vertex) = arg[3]; /* max */
  }

  if (state_stamp != pdata->stamp) /* recompute the graph repr. */
    gcc_refresh(pdata);
  else                          /* update the graph repr. */
    gcc_update(pdata);
  pdata->stamp = state_stamp+1;

  PRINT_GRAPH(TRUE, "before filter");
  while (change>0) {
    if (change>1)
      goto ret;
    maintain_capacities(pdata);
    change = (!gcc_filter(pdata) ? 2 : adjust_count_bounds(pdata));
  }
  PRINT_GRAPH(TRUE, "after filter");

				/* Compute prunings. */
  ent = 1;			/* Entailed while ground */
  for (i = 0; i < pdata->nvartargets; ++i) {
    VERTEX varvertex = TVAR(i);
    int live = IN_DEGREE(varvertex);

    if (live > 1) ent = 0;
    if (live != GetSmall(DomainSize(DOM(varvertex))))
      request_tell(w,
                   TRefVarAttr(varvertex-pdata->nvals-1),
                   TRefVarVar(varvertex-pdata->nvals-1),
                   gcc_new_domain(pdata, 1, live, varvertex),
                   2, 3);
  }
  for (i = 0; i < pdata->nvaltargets; ++i) {
    VERTEX valvertex = TVAL(i);
    TAGGED lbt = MINCOUNT_T(valvertex);
    TAGGED ubt = MAXCOUNT_T(valvertex);
    TAGGED mut = TRefValAttr(valvertex); /* get attribute */
    TAGGED dom, *arg;

    DerefAttribute(dom,mut);
    arg = TagToArg(dom,0);
    if (lbt!=arg[2] || ubt!=arg[3])
      request_tell_interval(w,
			    mut, TRefValVar(valvertex),
			    lbt, ubt, 2, 3);
  }

  contract_vals(pdata);
  contract_vars(pdata);

#if CSTATE_ONLY
  if (committed) {
    pdata->nvars_back = pdata->nvartargets;
    pdata->nvals_back = pdata->nvaltargets;
  }
#else
  CTagToArg(X(0),1) = MakeSmall(pdata->nvartargets);
  CTagToArg(X(0),2) = MakeSmall(pdata->nvaltargets);
#endif

ret:
  Pfree(ent==1, handle, pdata);
  RefTerm(Actions) = request_done(w, ent, 2, 3);
}

