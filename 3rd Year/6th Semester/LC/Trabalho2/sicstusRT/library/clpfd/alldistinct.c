/* Copyright(C) 1999, Swedish Institute of Computer Science */
/***
  Support for all_distinct/[1,2], assignment/[2,3], circuit/[1,2].
  Algorithms, see Regin'94, Costa'94, Caseau&Laburthe '97, and more for circuit.
  ***/

#include "fd.h"

#define CSTATE_ONLY 0

#if MULTI_SP_AWARE
#if 0                           /* not redefined. It uses FD_SETUP_SPENV to access the SPEnv */
#define all_distinct_destructor(A1) all_distinct_destructor(HIDDEN_ARG, A1)
#endif

#define cmp_asc_val(A1,A2) cmp_asc_val(HIDDEN_ARG, A1,A2)
#define qsort_asc_valswap(A1,A2,A3,A4) qsort_asc_valswap(HIDDEN_ARG, A1,A2,A3,A4)
#define qsort_asc_valmed3(A1,A2,A3) qsort_asc_valmed3(HIDDEN_ARG, A1,A2,A3)
#define qsort_asc_val(A1,A2) qsort_asc_val(HIDDEN_ARG, A1,A2)

#define clear_mate(A1) clear_mate(HIDDEN_ARG, A1)
#define remove_neighbor(A1,A2) remove_neighbor(HIDDEN_ARG, A1,A2)
#define neighbor_of(A1,A2) neighbor_of(HIDDEN_ARG, A1,A2)
#define propagate_value(A1,A2) propagate_value(HIDDEN_ARG, A1,A2)
#define augment_path(A1,A2) augment_path(HIDDEN_ARG, A1,A2)
#define augment(A1) augment(HIDDEN_ARG, A1)
#define visit(A1) visit(HIDDEN_ARG, A1)
#define find_sccs(A1) find_sccs(HIDDEN_ARG, A1)
#define single_scc(A1,A2) single_scc(HIDDEN_ARG, A1,A2)
#define compress_edges(A1,A2) compress_edges(HIDDEN_ARG, A1,A2)
#define all_dist_filter(A1) all_dist_filter(HIDDEN_ARG, A1)
#define ass_filter(A1) ass_filter(HIDDEN_ARG, A1)
#define ass_update(A1) ass_update(HIDDEN_ARG, A1)

#define ass_refresh(A1) ass_refresh(HIDDEN_ARG, A1)
#define remove_edge(A1,A2) remove_edge(HIDDEN_ARG, A1,A2)
#define single_circuit(A1,A2) single_circuit(HIDDEN_ARG, A1,A2)
#define join_paths(A1) join_paths(HIDDEN_ARG, A1)
#define circuit_filter(A1) circuit_filter(HIDDEN_ARG, A1)
#define all_dist_alloc(A1,A2,A3,A4,A5) all_dist_alloc(HIDDEN_ARG, A1,A2,A3,A4,A5)

#endif /* !MULTI_SP_AWARE */

typedef long VERTEX;

#define EOL (-1L)

struct all_distinct_data {
  void (SPCDECL *destructor)(void*);
#if MULTI_SP_AWARE
  SPEnv *spenv;
#endif /* MULTI_SP_AWARE */
  SP_term_ref refbase;
  long stamp;			/* increases up to backtracking */
  BOOL dense;			/* TRUE if vars 1..n, vals 1..n */
  BOOL pruned;
  int nrefs;
  int nvars;
  int nvartargets;
#if CSTATE_ONLY
  int nvars_back;
#endif
  int nvals;
  int nvaltargets;
#if CSTATE_ONLY
  int nvals_back;
#endif
  int circuit_count;
  int slack;			/* #vals - #vars after filtering */
  VERTEX *vartarget;		/* var vertices: nvars*2 */
  VERTEX *valtarget;		/* val vertices: nvals*2 */
  VERTEX *dist_stack;		/* : 3*nvars+nvals, volatile */
  VERTEX *head_tail;		/* head val vertex -> tail var vertex: nvars */
  VERTEX *tail_head;		/* tail var vertex -> head val vertex: nvars */
  struct {
    TAGGED *val;		/* The value of the vertex.  1-based: nvars+nvals  */
    TAGGED *fdset;		/* Local domain, volatile, GC-safe */
    long *size;			/* Size of domain (needed for pruning), volatile */
    VERTEX *mate;		/* Mate of this vertex */
    long *live;			/* Degree i.e. next index in neighbor array */
    long *component;		/* Component number, volatile */
    long *visited;		/* VAL nodes: Nth visited in SCC search */
				/* VAR nodes: visited in augment_path */
  } vertex;			/* : nvars+nvals, volatile */
  /*
  TAGGED[nvars];
  struct dist_vertex[nvars+nvals];
  VERTEX [3*nvars+2*nvals];
  VERTEX [2*nvars*]; (* only for circuit *)
  */
};

#define VAL(V) (pdata->vertex.val[V])
#define FDSET(V) (pdata->vertex.fdset[V])
#define SIZE(V) (pdata->vertex.size[V])
#define NEIGHS(V) (pdata->vertex.neighs[V])
#define MATE(V) (pdata->vertex.mate[V])
#define LIVE(V) (pdata->vertex.live[V])
#define COMPONENT(V) (pdata->vertex.component[V])
#define VISITED(V) (pdata->vertex.visited[V])
#define TRefAttr(V) RefTerm(pdata->refbase + 2*(V))
#define TRefVar(V) RefTerm(pdata->refbase + 2*(V) + 1)
#define TVAR(V) (pdata->vartarget[V])
#define TVAL(V) (pdata->valtarget[V])

/* Invariant: VISITED(V)>EOL iff V is a target (not yet ground and checked) */

static void SPCDECL all_distinct_destructor(void *pdata_v)
{
  struct all_distinct_data *pdata = (struct all_distinct_data *)pdata_v;
  FD_SETUP_SPENV(pdata->spenv)

  SP_free_term_refs(pdata->refbase,pdata->nrefs);
  SP_free(pdata);
}

static int cmp_asc_val MAGIC (HIDDEN_PROTO
			      VERTEX *t1, VERTEX *t2)
{
  struct all_distinct_data *pdata = fd.gdata;

  return Tdifference(VAL(*t1),VAL(*t2));
}

#define QType VERTEX
#define QCmp  cmp_asc_val
#define QSort qsort_asc_val
#include "qsort.ic"

static INLINE void clear_mate MAGIC (HIDDEN_PROTO
                                     VERTEX vertex)
{
  struct all_distinct_data *pdata = fd.gdata;
  VERTEX mate = MATE(vertex);

  if (mate>EOL)
    MATE(mate) = MATE(vertex) = EOL;
}

#define set_mates(n1,n2) MATE(n1) = (n2), MATE(n2) = (n1)

#define exposed(vertex) (MATE(vertex) == EOL)

static int remove_neighbor MAGIC (HIDDEN_PROTO
				  VERTEX u, VERTEX v)
{
  struct all_distinct_data *pdata = fd.gdata;
  TAGGED tv = VAL(v);

  if (fd_member(tv,FDSET(u))) {
    FDSET(u) = fd_delete(FDSET(u),tv);
    LIVE(u)--;
    pdata->pruned = TRUE;
  }

  return LIVE(u);
}

static VERTEX neighbor_of MAGIC (HIDDEN_PROTO
                                 TAGGED tval, VERTEX u)
{
  struct all_distinct_data *pdata = fd.gdata;

  if (u>=pdata->nvars)
    return GetSmall(tval)-1;
  else if (pdata->dense)
    return GetSmall(tval)+pdata->nvars-1;
  else {
    /* dichotomic search for j such that VAL(TVAL(j))==tval */
    int j, mid, sup;
    
    j = 0;
    sup = pdata->nvaltargets;
    while (j<sup) {
      mid = (j+sup)>>1;
      if (Tlt(VAL(TVAL(mid)),tval)) /* ORDER */
	j = mid+1;
      else
	sup = mid;
    }
    return TVAL(j);
  }
}

static BOOL propagate_value MAGIC (HIDDEN_PROTO
				   int nvars, int nvals)
{/* Performs all possible pruning by propagating singletons.
    Also, build matching for such singletons. */
  int i, top=0;
  struct all_distinct_data *pdata = fd.gdata;
  VERTEX *dist_stack = pdata->dist_stack;
  FDITER it;

  for (i=0; i<nvars; i++) {
    VERTEX v = TVAR(i);
     
    if (LIVE(v) == 1)
      dist_stack[top++] = v;
  }
  for (i=0; i<nvals; i++) {
    VERTEX v = TVAL(i);
       
    if (LIVE(v) == 1)
      dist_stack[top++] = v;
  }

  while (top>0) {
    VERTEX vertex = dist_stack[--top];
    VERTEX rvertex = neighbor_of(fd_min(FDSET(vertex)), vertex);
    TAGGED tv = VAL(vertex);

    if (MATE(vertex) != rvertex) {
      clear_mate(vertex);
      clear_mate(rvertex);
      set_mates(vertex,rvertex); /* one matching done */
    }
    if (LIVE(rvertex) > 1) {
      fditer_init(&it, FDSET(rvertex));
      while (!fditer_empty(&it)) {
	VERTEX lvertex = neighbor_of(fditer_next(&it), rvertex);
	
	if (lvertex!=vertex)
	  switch (remove_neighbor(lvertex,rvertex)) {
	  case 0: return FALSE;
	  case 1: dist_stack[top++] = lvertex;
	  }
      }
      FDSET(rvertex) = fd_interval(tv,tv);
      LIVE(rvertex) = 1;
      pdata->pruned = TRUE;
    }
  }

  return TRUE;
} /* propagate_value */


static BOOL augment_path MAGIC (HIDDEN_PROTO 
				VERTEX vertex,
				int now)
   /* BFS algorithm for finding an augmenting path. 
      If successful, updates the graph to reflect the new matching.
      Return TRUE or FALSE, depending on whether an augmenting path is
      found.
      */
{
  int p=0, q=0;
  VERTEX newvertex, neighbor;
  struct all_distinct_data *pdata = fd.gdata;
  VERTEX *vstack = pdata->dist_stack;
  FDITER it;

  vstack[q++] = vertex;
  while (p<q) {
    vertex = vstack[p++];
    fditer_init(&it, FDSET(vertex));
    while (!fditer_empty(&it)) {
      neighbor = neighbor_of(fditer_next(&it), vertex);
      newvertex = MATE(neighbor);
      if (newvertex==EOL) {
	for (;;) {
	  newvertex = MATE(vertex);
	  set_mates(vertex,neighbor);
	  if (newvertex==EOL)
	    return TRUE;
	  neighbor = newvertex;
	  vertex = vstack[newvertex]; /* FATHER(newvertex) */
	}
      } else if (VISITED(newvertex)!=now) {
	VISITED(newvertex) = now;
	vstack[neighbor] = vertex; /* FATHER(neighbor) */
	vstack[q++] = newvertex;
      }
    }
  }
  return FALSE;
} /* augment_path */



static BOOL augment MAGIC (HIDDEN_PROTO
			   int nvars)
   /* This function returns TRUE and a new graph with a maximum matching
      if there is such a matching. Returns FALSE otherwise. The maximum
      matching is found by repeatedly finding augmenting paths in the
      graph until all variable vertices are matched.  */
{
  int i, first_exp;
  VERTEX vertex, rvertex;
  TAGGED seed=0;		/* avoid false alarm */
  struct all_distinct_data *pdata = fd.gdata;
  FDITER it;
				/* greedy phase */
  first_exp = nvars;
  for (i=0; i<nvars; i++) {
    vertex = TVAR(i);
    if (exposed(vertex)) {
      if (first_exp>i) {
	first_exp = i;
	seed = fd_min(FDSET(vertex));
	rvertex = neighbor_of(seed,vertex);
	if (exposed(rvertex))
	  set_mates(vertex,rvertex);
      } else {
	fditer_init(&it,FDSET(vertex));
	fditer_skip(&it,seed);
	if (!fditer_empty(&it)) {
	  seed = it.cur;
	  rvertex = neighbor_of(seed,vertex);
	  if (exposed(rvertex))
	    set_mates(vertex,rvertex);
	}
      }
    }
  }
				/* augmenting phase */
  for (i=first_exp; i<nvars; i++) {
    vertex = TVAR(i);
    if (exposed(vertex)) {
      if (!augment_path(vertex,i+1))
	return FALSE;
    }
  }
  return TRUE;
} /* augment */


static int visit MAGIC (HIDDEN_PROTO
			VERTEX vertex)
   /* A recursive function that finds strongly connected components
      [Tarjan'72]. Code taken roughly from [Sedgewick, Algorithms in C,
      page 482].
      One big difference is that we only step on value vertices, i.e. we
      jump directly from variable vertices using the mate ptr.
      */
{
  VERTEX newvertex=EOL, mate;
   int m, min, scc;
   struct all_distinct_data *pdata = fd.gdata;
   VERTEX *dist_stack = pdata->dist_stack;
   FDITER it;
   
   VISITED(vertex) = ++fd.scc_visited;		/* Mark vertex as the id'th visited */
   min = fd.scc_visited;
   dist_stack[fd.scc_index++] = vertex;
   mate = MATE(vertex);
   fditer_init(&it, FDSET(mate));
   while (!fditer_empty(&it)) {
     newvertex = neighbor_of(fditer_next(&it),mate);
     if (newvertex != vertex) {
       m = (VISITED(newvertex) == 0) ? 
	 visit(newvertex) : VISITED(newvertex);
       if (m < min)
	 min = m;
     }
   }
   if (min == VISITED(vertex)) {
     scc = ++fd.scc_component;
     do {
       /* Each descendant on the dist_stack is part of this vertex's SCC. */
       newvertex = dist_stack[--fd.scc_index];
       COMPONENT(newvertex) = scc;
       COMPONENT(MATE(newvertex)) = scc;
       VISITED(newvertex)=0xffffff; /* High value, so that
				       this vertex will be ignored
				       in the future search. */
     } while (newvertex != vertex);
   }
   return min;
} /* visit */


static BOOL single_scc MAGIC (HIDDEN_PROTO
			      TAGGED unvisited,
			      int sccs)
{
  TAGGED h, y;
  VERTEX yvertex;
  struct all_distinct_data *pdata = fd.gdata;

  (void)sccs;                   /* [PM] 3.9b5 avoid -Wunused */

  h = y = fd_min(unvisited);
  yvertex = neighbor_of(y,0);
  unvisited = fd_delete(unvisited,y);
  while (unvisited!=EmptySet) {
    TAGGED fdset = FDSET(MATE(yvertex));

    y = fd_and_min(unvisited,fdset);
    if (y==ERRORTAG)
      return FALSE;
    unvisited = fd_delete(unvisited,y);
    yvertex = neighbor_of(y,0);
  }
  if (!fd_member(h,FDSET(MATE(yvertex))))
    return FALSE;
  /*
  for (i=0; i<pdata->nvartargets; i++) {
    yvertex = TVAR(i);
    if (COMPONENT(yvertex)==0) {
      COMPONENT(yvertex) = sccs+1;
      COMPONENT(MATE(yvertex)) = sccs+1;
    }
  }
  */
  return TRUE;
}



/* Returns #non-singleton components. */
static int find_sccs MAGIC (HIDDEN_PROTO  /* (sccs,nvars) */
			    int sccs)
   /* Marks all edges belonging to any strongly connected component in
      the graph. The strongly connected components are calculated using
      Tarjan's depth-first search based algorithm from 1972. */
{
   int i;
   FDCONS cons;
   VERTEX vertex, mate;
   struct all_distinct_data *pdata = fd.gdata;
   int nvars = pdata->nvartargets;
   int nvals = pdata->nvaltargets;

   for (i=0; i<nvars; i++) {
     vertex = TVAR(i);
     mate = MATE(vertex);
     if (COMPONENT(vertex)!=0)	/* reachable from exposed node */
       VISITED(mate) = 0xffffff; /* ignore in SCC search */
     else if (LIVE(vertex)==1) {
       VISITED(mate) = 0xffffff; /* ignore in SCC search */
       COMPONENT(vertex) = ++sccs; /* singleton SCC */
       COMPONENT(mate) = sccs;
     }
   }
   fdcons_init(&cons);
   for (i=0; i<nvals; i++) {
     vertex = TVAL(i);
     mate = MATE(vertex);
     if (mate>EOL && !VISITED(vertex))
       fdcons_add(&cons,VAL(vertex)); /* ORDER */
   }
   if (fdcons_size(&cons)==0)
     return 0;
   else if (single_scc(fdcons_set(&cons),sccs))
     return 1;
   fd.scc_visited = 0;
   fd.scc_index = 0;
   fd.scc_component = sccs;
   for (i=0; i<nvars; i++) {
     vertex=MATE(TVAR(i));
     if (VISITED(vertex) == 0)
       visit(vertex);
   }
   return fd.scc_component - sccs;
} /* find_sccs */


static void compress_edges MAGIC (HIDDEN_PROTO
				  VERTEX *vertices,
				  int nvars)
{
  struct all_distinct_data *pdata = fd.gdata;
  int i, size;
  FDITER it;
  FDCONS cons;
  
  for (i=0; i<nvars; i++) {
    VERTEX rvertex;
    VERTEX vertex = vertices[i];
    int component = COMPONENT(vertex);

    fditer_init(&it, FDSET(vertex));
    fdcons_init(&cons);
    while (!fditer_empty(&it)) {
      rvertex = neighbor_of(fditer_next(&it),vertex);
      if (COMPONENT(rvertex) != component)
	fdcons_add(&cons, VAL(rvertex));
    }
    size = fdcons_size(&cons);
    if (size>0) {
      FDSET(vertex) = fd_subtract(FDSET(vertex),fdcons_set(&cons));
      pdata->pruned = TRUE;
      if ((LIVE(vertex) -= size)==0)
	--pdata->slack;
    }
  }
}


static BOOL all_dist_filter MAGIC (HIDDEN_PROTO
				   struct all_distinct_data *pdata)
{
  int i;
  VERTEX vertex, rvertex;
  int nvartargets = pdata->nvartargets;
  int nvaltargets = pdata->nvaltargets;
  int slack = nvaltargets-nvartargets;
  TAGGED fdset, feasible, singletons, oldfeas=0;
  
  if (slack<0)
    return FALSE;

  singletons = EmptySet;
  feasible = fd_complement(EmptySet);
  while (oldfeas!=feasible) {
    oldfeas = feasible;
    for (i=0; i<nvartargets; i++) {
      vertex = TVAR(i);
      fdset = FDSET(vertex);
      if (!fd_member(VAL(vertex),singletons))
	switch (fd_compare(fdset,feasible)) {
	case FDI_DISJOINT:
	  return FALSE;
	case FDI_INTERSECT:
	  fdset = fd_and(fdset,feasible);
	  goto chk;
	case FDI_SUPERSET:
	  fdset = feasible;
	chk:
	  FDSET(vertex) = fdset;
	  LIVE(vertex) = fd_size(fdset);
	  pdata->pruned = TRUE;
	case FDI_SUBSET:
	case FDI_EQUAL:
	  if (LIVE(vertex)==1) {
	    rvertex = neighbor_of(fd_min(fdset),vertex);
	    LIVE(rvertex) = 1;	/* used in SCC search */
	    if (MATE(vertex) != rvertex) {
	      clear_mate(vertex);
	      clear_mate(rvertex);
	      set_mates(vertex,rvertex); /* one matching done */
	    }
	    feasible = fd_subtract(feasible,fdset);
	    singletons = fd_or_interval(singletons,VAL(vertex),VAL(vertex));
	  }
	}
    }
  }
  for (i=0; i<nvartargets; i++) {
    vertex = TVAR(i);
    COMPONENT(vertex) = 0;
    VISITED(vertex) = 0;
  }
  for (i=0; i<nvaltargets; i++) {
    vertex = TVAL(i);
    COMPONENT(vertex) = 0;
    VISITED(vertex) = 0;
  }
  pdata->slack = slack;

  /* Find maximal matching. */
  if (!augment(nvartargets))
    return FALSE;
  
  /* Look for all even length paths starting at an exposed value vertex. */
  if (slack > 0) {
    FDCONS reach0;
    TAGGED oldreach=EmptySet;
    TAGGED reachable;

    fdcons_init(&reach0);
    for (i=0; i<nvaltargets; i++) {
      vertex = TVAL(i);
      if (exposed(vertex)) {
	COMPONENT(vertex) = 1;
	fdcons_add(&reach0, VAL(vertex)); /* ORDER */
      }
    }

    reachable = fdcons_set(&reach0);
    while (oldreach!=reachable) {
      oldreach = reachable;
      for (i=0; i<nvartargets; i++) {
	vertex = TVAR(i);
	if (COMPONENT(vertex)==0 &&
	    fd_compare(FDSET(vertex),reachable)!=FDI_DISJOINT) {
	  VERTEX mate = MATE(vertex);

	  COMPONENT(vertex) = 1;
	  COMPONENT(mate) = 1;
	  reachable = fd_or_interval(reachable,VAL(mate),VAL(mate));
	}
      }
    }
  }
  
  /* Look for SCCs */
  /* In the tight case, can't prune if there is only 1 SCC. */
  /* In the loose case, can't prune if there is only 0 SCC. */
  if (find_sccs(1) > (slack==0)) {  
    /* Delete edges */
    compress_edges(pdata->vartarget,nvartargets);
  }
  return TRUE;
} /* all_dist_filter */




static void all_dist_init(struct all_distinct_data *pdata,
			  TAGGED vals)
{
  VERTEX varvertex, valvertex;
  int i=0;
  FDITER it;
  TAGGED t;
  BOOL dense = TRUE;

  /* First, allocate all value vertices. */
  valvertex = pdata->nvars;
  fditer_init(&it, vals);
  while (!fditer_empty(&it)) {
    t = fditer_next(&it);
    VAL(valvertex) = t;
    MATE(valvertex) = EOL;
    /* redundant
       COMPONENT(valvertex) = 0;
       VISITED(valvertex) = 0;
    */
    TVAL(i++) = valvertex;
    valvertex++;
    if (GetSmall(t)!=i)
      dense = FALSE;
  }
  pdata->dense = dense;

  /* Then, allocate all var vertices and their edges */
  varvertex = 0;
  for (i=0; i<pdata->nvars; i++)
    {
      VAL(varvertex) = MakeSmall(i+1);
      MATE(varvertex) = EOL;
      /* redundant
      COMPONENT(varvertex) = 0;
      VISITED(varvertex) = 0;
      */
      TVAR(i) = varvertex;
      varvertex++;
    }
} /* all_dist_init */

/* Update the graph repr., deleting edges that are gone. */
static void ass_update MAGIC (HIDDEN_PROTO
			      struct all_distinct_data *pdata)
{
  VERTEX varvertex, valvertex, mate, *edges;
  int i, j, jmax;
  TAGGED dom;
  VERTEX *dist_stack = pdata->dist_stack;
  int top=0;
  FDITER it;

  /* Collect set of vertices to update before any pruning. */
  for (i=0; i<pdata->nvartargets; i++) {
    varvertex = TVAR(i);
    if (SIZE(varvertex) < LIVE(varvertex)) {
      LIVE(varvertex) = SIZE(varvertex);
      dist_stack[top++] = varvertex;
    }
  }
  for (i=0; i<pdata->nvaltargets; i++) {
    valvertex = TVAL(i);
    if (SIZE(valvertex) < LIVE(valvertex)) {
      LIVE(valvertex) = SIZE(valvertex);
      dist_stack[top++] = valvertex;
    }
  }

  while (top>0) {
    varvertex = dist_stack[--top];
    if (varvertex<pdata->nvars) { /* really a var vertex */
      edges = &TVAL(0);
      jmax = pdata->nvaltargets;
    } else {
      edges = &TVAR(0);      
      jmax = pdata->nvartargets;
    }
    /* Remove edges that don't correspond to current domains. */
    mate = MATE(varvertex);
    dom = FDSET(varvertex);
    j = 0;
    /* NOTE: dom is NOT necessarily a subset of edgelist. */
    /* Compute the intersection. */
    fditer_init(&it, dom);
    while (j<jmax) {		/* ORDER */
      TAGGED val = VAL(edges[j]);

      if (Tlt(it.cur,val))
	fditer_next(&it);
      else if (Tgt(it.cur,val)) {
	valvertex = edges[j++];
	remove_neighbor(valvertex,varvertex);
	if (valvertex==mate)
	  MATE(varvertex) = MATE(valvertex) = EOL;
      } else {
	j++;
	fditer_next(&it);
      }
    }
  }
} /* ass_update */

static void ass_init (struct all_distinct_data *pdata)
{
  VERTEX vertex = 0;
  int k;
  int n = pdata->nvars;

  pdata->dense = TRUE;

  for (k=0; k<n; k++) {
    VAL(vertex) = MakeSmall(k+1);
    MATE(vertex) = EOL;
    /* redundant
    COMPONENT(vertex) = 0;
    VISITED(vertex) = 0;
    */
    TVAR(k) = vertex;
    vertex++;
  }
  for (k=0; k<n; k++) {
    VAL(vertex) = MakeSmall(k+1);
    MATE(vertex) = EOL;
    /* redundant
    COMPONENT(vertex) = 0;
    VISITED(vertex) = 0;
    */
    TVAL(k) = vertex;
    vertex++;
  }
}


static void ass_refresh MAGIC (HIDDEN_PROTO
			       struct all_distinct_data *pdata)
{
  VERTEX lvertex, rvertex;
  int k, size, index;
  TAGGED tk, tv;
  int n = pdata->nvartargets;
  int nvars = pdata->nvars;
  FDITER it;
  FDCONS cons;
  WAMENV;

  for (k=0; k<2*n; k++) {
    lvertex = (k<n ? TVAR(k) : TVAL(k-n));
    tk = VAL(lvertex);
    rvertex = MATE(lvertex);
    if (rvertex>EOL && !fd_member(VAL(rvertex),FDSET(lvertex)))
      MATE(lvertex) = MATE(rvertex) = EOL;
    LIVE(lvertex) = SIZE(lvertex);
    fditer_init(&it, FDSET(lvertex));
    fdcons_init(&cons);
    while (!fditer_empty(&it)) {
      tv = fditer_next(&it);
      rvertex = neighbor_of(tv, lvertex);
      if (!fd_member(tk,FDSET(rvertex))) {
	fdcons_add(&cons, tv);
      }
    }
    size = fdcons_size(&cons);
    if (size>0) {
      FDSET(lvertex) = fd_subtract(FDSET(lvertex),fdcons_set(&cons)); 
      LIVE(lvertex) -= size;
      pdata->pruned = TRUE;
    }
  }

  /* for circuit/[1,2] */
  if (pdata->head_tail) {
    pdata->circuit_count = n;
    for (k=0; k<n; k++) {
      rvertex = TVAL(k);
      index = rvertex-nvars;
      lvertex = index;
      while (VISITED(lvertex)==EOL) { /* see invariant */
	tv = TRefVar(lvertex);
	DerefNonvar(tv);
	lvertex = GetSmall(tv)-1;
      }
      pdata->head_tail[index] = lvertex;
    }
    for (k=0; k<n; k++) {
      lvertex = TVAR(k);
      index = lvertex;
      rvertex = index+nvars;
      while (VISITED(rvertex)==EOL) { /* see invariant */
	tv = TRefVar(rvertex);
	DerefNonvar(tv);
	rvertex = GetSmall(tv)+nvars-1;
      }
      pdata->tail_head[index] = rvertex;
    }
  }
}

static BOOL ass_filter MAGIC (HIDDEN_PROTO
			      struct all_distinct_data *pdata)
{
  int i;
  VERTEX vertex;
  int nvartargets = pdata->nvartargets;
  int nvaltargets = pdata->nvaltargets;
  
  for (i=0; i<nvartargets; i++) {
    vertex = TVAR(i);
    COMPONENT(vertex) = 0;
    VISITED(vertex) = 0;
    if (LIVE(vertex)==0)
      return FALSE;
  }
  for (i=0; i<nvaltargets; i++) {
    vertex = TVAL(i);
    COMPONENT(vertex) = 0;
    VISITED(vertex) = 0;
    if (LIVE(vertex)==0)
      return FALSE;
  }
  pdata->slack = 0;

  /* Propagate ground values until fixpoint. This DOES pay off. */
  if (!propagate_value(nvartargets,nvaltargets))
    return FALSE;
  
  /* Find maximal matching. */
  if (!augment(nvartargets))
    return FALSE;
  
  /* Look for SCCs */
  /* In the tight case, can't prune if there is only 1 SCC. */
  /* In the loose case, can't prune if there is only 0 SCC. */
  if (find_sccs(1) > 1) {  
    /* Delete edges */
    compress_edges(pdata->vartarget,nvartargets);
    compress_edges(pdata->valtarget,nvaltargets);
  }
  return TRUE;
} /* ass_filter */




/* 0=noop, 1=pruned, 3=failure */
static int remove_edge MAGIC (HIDDEN_PROTO
			      VERTEX u, VERTEX v)
{
  int j, degree; 
  struct all_distinct_data *pdata = fd.gdata;

				/* handle var side */
  degree = LIVE(u);
  j = remove_neighbor(u,v);
  if (j==degree)		/* not found */
    return 0;
  if (j==0)			/* no edges left */
    return 3;
  if (MATE(u)==v)
    MATE(u) = MATE(v) = EOL;
				/* handle value side */
  j = remove_neighbor(v,u);
  if (j==0)			/* no edges left */
    return 3;
  return 1;
}



/* A feasibility check: a necessary condition for circuit/[1,2]
   is that the graph be a single, strongly connected component.
   Implementation: an iterative version of Tarjan's algorithm + 
   a necessary condition for Hamiltonicity.
   */
static BOOL single_circuit MAGIC (HIDDEN_PROTO
				  struct all_distinct_data *pdata,
				  VERTEX vertex)
{
  int id=0, top=0, min, m, vertid;
  int majormin=0, majormax=0;	/* first (last) id of latest major subtree */
  BOOL seen_major = TRUE;	/* seen edge to latest major subtree? */
  VERTEX child;
  VERTEX *vstack = pdata->dist_stack;
  long *lstack = (long *)vstack;
  TAGGED *tstack = (TAGGED *)vstack;
  int nvars = pdata->nvars;
  TAGGED tv;
  FDITER it;

start:
  VISITED(vertex) = vertid = min = ++id;
  fditer_init(&it, FDSET(vertex));
  while (!fditer_empty(&it)) {
    tv = fditer_next(&it);
    child = pdata->head_tail[neighbor_of(tv,vertex)-nvars /*VAL(neighs[j])-1*/];
    m = VISITED(child);
    if (m==0) {
      vstack[top] = vertex;
      tstack[top+1] = tv;
      lstack[top+2] = min;
      top += 3;
      vertex = child;
      goto start;
    } else if (vertid>1 && m>=majormin && m<=majormax)
      seen_major = TRUE;
  cont:
    if (min > m)
      min = m;
  }
  if (min==vertid)
    return (min==1 && id==pdata->circuit_count);
  m = min;
  top -= 3;
  vertex = vstack[top];
  tv = tstack[top+1];
  min = lstack[top+2];
				/* here, m is the minimal ID reachable from
				   some just visited child of vertex */
  if (VISITED(vertex)==1) {	/* just visited a major subtree */
    if (!seen_major && tv!=fd_min(FDSET(vertex))) /* which must have an edge into the subtree */
      return FALSE;		/* to its left */
    majormin = vertid;
    majormax = id;
    seen_major = FALSE;
  }
  vertid = VISITED(vertex);
  fditer_init(&it, FDSET(vertex));
  fditer_skip(&it, tv);
  goto cont;
}


/* Join paths in a circuit constraints.
   E.g. assume the paths

	x1-->x3 and x4-->x6

   and a determined edge x3-->x4.
   Note: x1 = head(x3), x4 = head(x6).

   If (graph has four nodes)
     infer edge x6-->x1
   else
     forbid edge x6-->x1
     head(x6) := x1.
     */
static int join_paths MAGIC (HIDDEN_PROTO
			     struct all_distinct_data *pdata)
{
  int n = pdata->nvartargets;
  int nvars = pdata->nvars;
  VERTEX *dist_stack = pdata->dist_stack;
   int top=0, flag=0, i, m;
   int bestval;
   VERTEX head1;	/* val vertex: head of P1 before join (e.g. x1) */
   VERTEX tail1;	/* var vertex: tail of P1 before join (e.g. x3) */
   VERTEX head2;	/* val vertex: head of P2 before join (e.g. x4) */
   VERTEX tail2;	/* var vertex: tail of P2 before join (e.g. x6) */
   VERTEX bestv;

   /* N.B. the following hold:
      VAL(headx)-1 == headx-nvars;
      VAL(tailx)-1 == tailx;
   */

   for (i=0; i<n; i++)
     if (LIVE(TVAR(i))==1)
       dist_stack[top++] = TVAR(i);
   while (top>0 && flag<2) {
     tail1 = dist_stack[--top]; /* e.g. x3 */
     head2 = neighbor_of(fd_min(FDSET(tail1)),tail1);
     tail2 = pdata->head_tail[head2-nvars /*VAL(head2)-1*/];
     if (tail2==EOL)
       continue;		/* already processed */
     head1 = pdata->tail_head[tail1 /*VAL(tail1)-1*/];
     if (head1==head2)
       continue;		/* last edge of circuit */
     pdata->head_tail[head2-nvars /*VAL(head2)-1*/] = EOL;
     pdata->head_tail[head1-nvars /*VAL(head1)-1*/] = tail2;
     pdata->tail_head[tail1 /*VAL(tail1)-1*/] = EOL;
     pdata->tail_head[tail2 /*VAL(tail2)-1*/] = head1;
     if (--pdata->circuit_count > 1) {
       m = remove_edge(tail2,head1);
       if (m==1 && LIVE(tail2)==1)
	 dist_stack[top++] = tail2;
       flag |= m;
     }
   }
   if (flag==0 && pdata->circuit_count>=4) {
     /* may crash if flag==1 */
     /* heuristic: choose vertex with max out-degree */
     bestv = EOL;
     bestval = 0;
     for (i=0; i<n; i++) {
       tail1 = TVAR(i);
       VISITED(tail1) = 0;
       if (LIVE(tail1) > bestval) {
	 bestv = tail1;
	 bestval = LIVE(tail1);
       }
     }
     if (!single_circuit(pdata,bestv))
       flag = 2;
   }
   return flag;
}


static BOOL circuit_filter MAGIC (HIDDEN_PROTO
				  struct all_distinct_data *pdata)
{
  int flag;
  
  do {
    if (!ass_filter(pdata))
      return FALSE;
    flag = join_paths(pdata);
  } while (flag==1);
  return (flag==0);
} /* circuit_filter */



static struct all_distinct_data *
all_dist_alloc MAGIC (HIDDEN_PROTO
		      Argdecl,
		      int nvars,int nvals,int nrefs,
		      TAGGED handle)
{
    struct all_distinct_data *pdata =
      Palloc(struct all_distinct_data,
	     14*nvars*sizeof(TAGGED) +
	     10*nvals*sizeof(TAGGED),
	     handle);
    char *ptr;
    
    pdata->destructor = all_distinct_destructor;
    FD_STORE_SPENV(pdata->spenv);
    pdata->refbase = SP_alloc_term_refs(2*nrefs);
    pdata->nrefs = 2*nrefs;
    pdata->nvars = nvars;
    pdata->nvartargets = nvars;
#if CSTATE_ONLY
    pdata->nvars_back = nvars;
#endif
    pdata->nvals = nvals;
    pdata->nvaltargets = nvals;
#if CSTATE_ONLY
    pdata->nvals_back = nvals;
#endif
    ptr = (char *)(pdata+1);
    pdata->vartarget = (VERTEX *)ptr; /* array + aux */
    ptr += 2*nvars*sizeof(VERTEX);
    pdata->valtarget = (VERTEX *)ptr; /* array + aux */
    ptr += 2*nvals*sizeof(VERTEX);
    pdata->dist_stack = (VERTEX *)ptr;
    ptr += (3*nvars+nvals)*sizeof(VERTEX);
    if (nvars==nrefs) {
      pdata->head_tail = NULL;
      pdata->tail_head = NULL;
    } else {
      pdata->head_tail = (VERTEX *)ptr;
      ptr += nvars*sizeof(VERTEX);
      pdata->tail_head = (VERTEX *)ptr;
      ptr += nvars*sizeof(VERTEX);
    }
    pdata->vertex.val = (TAGGED *)ptr;
    ptr += (nvars+nvals)*sizeof(VERTEX);
    pdata->vertex.fdset = (TAGGED *)ptr;
    ptr += (nvars+nvals)*sizeof(VERTEX);
    pdata->vertex.size = (long *)ptr;
    ptr += (nvars+nvals)*sizeof(VERTEX);
    pdata->vertex.mate = (VERTEX *)ptr;
    ptr += (nvars+nvals)*sizeof(VERTEX);
    pdata->vertex.live = (long *)ptr;
    ptr += (nvars+nvals)*sizeof(VERTEX);
    pdata->vertex.component = (long *)ptr;
    ptr += (nvars+nvals)*sizeof(VERTEX);
    pdata->vertex.visited = (long *)ptr;
    ptr += (nvars+nvals)*sizeof(VERTEX);

    return pdata;
}


/* the targets need to be ordered (see ORDER comments) */
static void contract_vars (struct all_distinct_data *pdata)
{
  int imax = pdata->nvartargets;
  int n = pdata->nvars;
  VERTEX *target = pdata->vartarget;
  int i, j, k;
    
  /* separate into nonground (0..) and ground (n..) */
  for (i=0, j=0, k=n; i<imax; i++) {
    VERTEX current = target[i];
    
    if (LIVE(current)>1)
      target[j++] = current;
    else {
      VISITED(current) = EOL;	/* maintain invariant! */
      target[k++] = current;
    }
  }
  if (i==j) return;
  
  pdata->nvartargets = j;

  while (n<k)
    target[j++] = target[n++];
}


/* the targets need to be ordered (see ORDER comments) */
static void contract_vals (struct all_distinct_data *pdata)
{
  int imax = pdata->nvaltargets;
  int n = pdata->nvals;
  VERTEX *target = pdata->valtarget;
  int threshold = (pdata->slack==0);
  int i, j, k;
    
  /* separate into nonground (0..) and ground (n..) */
  for (i=0, j=0, k=n; i<imax; i++) {
    VERTEX current = target[i];
    
    if (LIVE(current)>threshold)
      target[j++] = current;
    else {
      VISITED(current) = EOL;	/* maintain invariant! */
      target[k++] = current;
    }
  }
  if (i==j) return;
  
  pdata->nvaltargets = j;

  while (n<k)
    target[j++] = target[n++];
}

/* '$fd_all_distinct'(+State0, -State, -Actions) :-
   State0 is f(LVec,Handle,Stamp) where LVec is a list of Var-Attribute
   State  similarly,
   Actions is a list of prunings etc.
   */
void SPCDECL 
prolog_fd_all_distinct MAGIC (HIDDEN_PROTO
			      SP_term_ref State0,
			      SP_term_ref State,
			      SP_term_ref Actions)
{
  WAMENV;
  int nvars, nvals;
  int i;
  TAGGED tlvec, telt, t1;
  TAGGED all, handle;
  long state_stamp;
  int ent = -1;			/* disentailed unless otherwise */
  struct all_distinct_data *pdata;
  BOOL committed;
  BOOL ground = TRUE;

  (void)State0;                 /* [PM] 3.9b5 avoid -Wunused */

  
  w->numstack_end = NULL;

/*    X(0) = RefTerm(State0); */
  X(2) = atom_nil;		/* actions list */
  RefTerm(State) = unify_output_state(w,&handle,&state_stamp,&committed);

  if (!IsVar(handle)) {		/* got [Flag | '$free'(Ptr)] */
    fd.gdata = pdata = Pdata(struct all_distinct_data,handle);
    nvars = pdata->nvars;
    nvals = pdata->nvals;
  } else {			/* build persistent state */
				/* compute nvars, nvals, all */
    all = EmptySet;
    nvars = 0;
    DerefArg(tlvec,X(0),1);
    while (TagIsLST(tlvec)) {
      DerefCar(telt,tlvec);
      DerefCdr(tlvec,tlvec);
      DerefArg(t1,telt,2);	/* Attribute */
      DerefAttribute(t1,t1);
      all = fd_merge_into(DomainSet(t1),all);
      nvars++;
    }
    nvals = fd_size(all);

    fd.gdata = pdata = all_dist_alloc(w,nvars,nvals,nvars,handle);
    pdata->stamp = state_stamp-1; /* DON'T trust initially */
    DerefArg(tlvec,X(0),1);
    for (i=0; i<nvars; i++)
      {
	DerefCar(telt,tlvec);
	DerefCdr(tlvec,tlvec);
	DerefArg(t1,telt,1);	/* FD variable */
	TRefVar(i) = t1;	/* protect it */
	DerefArg(t1,telt,2);	/* attribute */
	TRefAttr(i) = t1;	/* protect it */
      }
    all_dist_init(pdata,all);
  }

				/* RESUME HERE */
  pdata->pruned = FALSE;
  if (state_stamp != pdata->stamp) {
    int nvars1, nvals1;
#if CSTATE_ONLY
    nvars1 = pdata->nvars_back;
    nvals1 = pdata->nvals_back;
#else
    DerefArg(telt,X(0),2);
    nvars1 = nvars-GetSmall(telt);
    DerefArg(telt,X(0),3);
    nvals1 = nvals-GetSmall(telt);
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
  for (i=0; i<pdata->nvartargets; i++) {
    VERTEX vertex = TVAR(i);
    VERTEX mate = MATE(vertex);
    
    t1 = TRefAttr(vertex); /* get attribute */
    DerefAttribute(t1,t1);
    FDSET(vertex) = DomainSet(t1);
    LIVE(vertex) = SIZE(vertex) = GetSmall(DomainSize(t1));
    if (LIVE(vertex)>1) ground = FALSE;
    VISITED(vertex) = 0;
    if (mate>EOL && !fd_member(VAL(mate),FDSET(vertex)))
      MATE(vertex) = MATE(mate) = EOL;
  }
  pdata->stamp = state_stamp+1;

  if (!all_dist_filter(pdata))
    goto ret;
  ent = ground;
  if (!pdata->pruned)
    goto ret;

				/* Compute prunings. */
  all = EmptySet;
  for (i=0; i<pdata->nvartargets; i++) {
    VERTEX vertex = TVAR(i);
    
    if (LIVE(vertex) > 1)
      all = fd_merge_into(FDSET(vertex),all);
    if (LIVE(vertex) != SIZE(vertex))
      FDSET(vertex) = fd_localize(w,FDSET(vertex));
  }
  all = fd_localize(w,all);
  for (i=0; i<pdata->nvartargets; i++) {
    VERTEX vertex = TVAR(i);
    
    if (LIVE(vertex) != SIZE(vertex))
      request_tell(w, TRefAttr(vertex), TRefVar(vertex),
		   FDSET(vertex),
		   2, 3);
  }

  contract_vars(pdata);
  
  for (i=0; i<pdata->nvaltargets; i++) {
    VERTEX vertex = TVAL(i);

    LIVE(vertex) = fd_member(VAL(vertex),all) ? 2 : 0;
  }
  contract_vals(pdata);

#if CSTATE_ONLY
  if (committed) {
    pdata->nvars_back = pdata->nvartargets;
    pdata->nvals_back = pdata->nvaltargets;
  }
#else
  CTagToArg(X(0),2) = MakeSmall(nvars-pdata->nvartargets);
  CTagToArg(X(0),3) = MakeSmall(nvals-pdata->nvaltargets);
#endif
  
  ent = (pdata->nvartargets<=1);
#if DBG > 1
  if (ent>=0) {			/* check invariants */
    printf("%% invariants\n");
    for (i=0; i<pdata->nvaltargets; i++) {
      VERTEX vertex = TVAL(i);
      VERTEX mate = MATE(vertex);

      if (mate>EOL && LIVE(mate)==1)
	printf("! singleton var=%d matched non-singleton val=%d\n", (int)mate, (int)vertex);
    }
  }
#endif
ret:
  Pfree(ent==1,handle,pdata);
  RefTerm(Actions) = request_done(w, ent, 2, 3);
}



/* '$fd_assignment'(+State0, -State, -Actions) :-
   State0 is f(LVec,RVec,Handle,Stamp) where LVec,RVec are lists of f(Var,DomainMut)
   State  similarly,
   Actions is a list of prunings etc.
   */
void SPCDECL 
prolog_fd_assignment MAGIC (HIDDEN_PROTO
			    SP_term_ref State0,
			    SP_term_ref State,
			    SP_term_ref Actions)
{
  WAMENV;
  int nvars, nvals;
  int i, j;
  TAGGED tlvec, telt, trvec, t1;
  TAGGED handle;
  long state_stamp;
  int ent = -1;			/* disentailed unless otherwise */
  struct all_distinct_data *pdata;
  BOOL committed;
  BOOL ground = TRUE;

  (void)State0;                 /* [PM] 3.9b5 avoid -Wunused */

  
  w->numstack_end = NULL;

/*    X(0) = RefTerm(State0); */
  X(2) = atom_nil;		/* actions list */
  RefTerm(State) = unify_output_state(w,&handle,&state_stamp,&committed);

  if (!IsVar(handle)) {		/* got [Flag | '$free'(Ptr)] */
    fd.gdata = pdata = Pdata(struct all_distinct_data,handle);
    nvars = pdata->nvars;
    nvals = pdata->nvals;
  } else {			/* build persistent state */
				/* compute nvars, nvals */
    nvars = 0;
    DerefArg(tlvec,X(0),1);
    while (TagIsLST(tlvec)) {
      DerefCdr(tlvec,tlvec);
      nvars++;
    }
    nvals = nvars;

    fd.gdata = pdata = all_dist_alloc(w,nvars,nvals,2*nvars,handle);
    pdata->stamp = state_stamp-1; /* DON'T trust initially */
    DerefArg(tlvec,X(0),1);
    DerefArg(trvec,X(0),2);
    for (i=0; i<nvars; i++)
      {
	DerefCar(telt,tlvec);
	DerefCdr(tlvec,tlvec);
	DerefArg(t1,telt,1);	/* FD variable */
	TRefVar(i) = t1;	/* protect it */
	DerefArg(t1,telt,2);	/* attribute */
	TRefAttr(i) = t1;	/* protect it */
	DerefCar(telt,trvec);
	DerefCdr(trvec,trvec);
	DerefArg(t1,telt,1);	/* FD variable */
	TRefVar(nvars+i) = t1;	/* protect it */
	DerefArg(t1,telt,2);	/* attribute */
	TRefAttr(nvars+i) = t1;	/* protect it */
      }
    ass_init(pdata);
  }

				/* RESUME HERE */
  pdata->pruned = FALSE;
  if (state_stamp != pdata->stamp) {
    int nvars1, nvals1;
#if CSTATE_ONLY
    nvars1 = pdata->nvars_back;
    nvals1 = pdata->nvals_back;
#else
    DerefArg(telt,X(0),3);
    nvars1 = nvars-GetSmall(telt);
    DerefArg(telt,X(0),4);
    nvals1 = nvals-GetSmall(telt);
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
  for (i=0; i<pdata->nvartargets; i++) {
    VERTEX vertex = TVAR(i);
    t1 = TRefAttr(vertex); /* get attribute */
    DerefAttribute(t1,t1);
    FDSET(vertex) = DomainSet(t1);
    SIZE(vertex) = GetSmall(DomainSize(t1));
    if (SIZE(vertex)>1) ground = FALSE;
    VISITED(vertex) = 0;
  }
  for (i=0; i<pdata->nvaltargets; i++) {
    VERTEX vertex = TVAL(i);
    t1 = TRefAttr(vertex); /* get attribute */
    DerefAttribute(t1,t1);
    FDSET(vertex) = DomainSet(t1);
    SIZE(vertex) = GetSmall(DomainSize(t1));
    if (SIZE(vertex)>1) ground = FALSE;
    VISITED(vertex) = 0;
  }
  if (state_stamp != pdata->stamp) { /* recompute the graph repr. */
    ass_refresh(pdata);
  } else {			/* update the graph repr. */
    ass_update(pdata);
  }
  pdata->stamp = state_stamp+1;

  if (!ass_filter(pdata))
    goto ret;
  ent = ground;
  if (!pdata->pruned)
    goto ret;
				/* Compute prunings */
  for (i=0; i<pdata->nvartargets; i++)
    for (j=0; j<2; j++) {
      VERTEX vertex = j==0 ? TVAR(i) : TVAL(i);

      if (LIVE(vertex) != SIZE(vertex))
	FDSET(vertex) = fd_localize(w,FDSET(vertex));
    }
  for (i=0; i<pdata->nvartargets; i++)
    for (j=0; j<2; j++) {
      VERTEX vertex = j==0 ? TVAR(i) : TVAL(i);

      if (LIVE(vertex) != SIZE(vertex))
        request_tell(w, TRefAttr(vertex), TRefVar(vertex),
		     FDSET(vertex),
		     2, 3);
    }

  contract_vars(pdata);
  contract_vals(pdata);

#if CSTATE_ONLY
  if (committed) {
    pdata->nvars_back = pdata->nvartargets;
    pdata->nvals_back = pdata->nvaltargets;
  }
#else
  CTagToArg(X(0),3) = MakeSmall(nvars-pdata->nvartargets);
  CTagToArg(X(0),4) = MakeSmall(nvals-pdata->nvaltargets);
#endif

  ent = (pdata->nvartargets<=1);
ret:
  Pfree(ent==1,handle,pdata);
  RefTerm(Actions) = request_done(w, ent, 2, 3);
}



/* '$fd_circuit'(+State0, -State, -Actions) :-
   State0 is f(LVec,RVec,Handle,Stamp) where LVec,RVec are lists of f(Var,DomainMut)
   State  similarly,
   Actions is a list of prunings etc.

   Almost the same algorithm as '$fd_assignment'.  Differences marked with "NB".
   */
void SPCDECL 
prolog_fd_circuit MAGIC (HIDDEN_PROTO
			 SP_term_ref State0,
			 SP_term_ref State,
			 SP_term_ref Actions)
{
  WAMENV;
  int nvars, nvals;
  int i, j;
  TAGGED tlvec, telt, trvec, t1;
  TAGGED handle;
  long state_stamp;
  int ent = -1;			/* disentailed unless otherwise */
  struct all_distinct_data *pdata;
  BOOL committed;
  BOOL ground = TRUE;

  (void)State0;                   /* [PM] 3.9b5 avoid -Wunused */
  
  w->numstack_end = NULL;

/*    X(0) = RefTerm(State0); */
  X(2) = atom_nil;		/* actions list */
  RefTerm(State) = unify_output_state(w,&handle,&state_stamp,&committed);

  if (!IsVar(handle)) {		/* got [Flag | '$free'(Ptr)] */
    fd.gdata = pdata = Pdata(struct all_distinct_data,handle);
    nvars = pdata->nvars;
    nvals = pdata->nvals;
  } else {			/* build persistent state */
				/* compute nvars, nvals */
    nvars = 0;
    DerefArg(tlvec,X(0),1);
    while (TagIsLST(tlvec)) {
      DerefCdr(tlvec,tlvec);
      nvars++;
    }
    nvals = nvars;

    fd.gdata = pdata = all_dist_alloc(w,nvars,nvals,2*nvars,handle);
    pdata->stamp = state_stamp-1; /* DON'T trust initially */
    DerefArg(tlvec,X(0),1);
    DerefArg(trvec,X(0),2);
    for (i=0; i<nvars; i++)
      {
	DerefCar(telt,tlvec);
	DerefCdr(tlvec,tlvec);
	DerefArg(t1,telt,1);	/* FD variable */
	TRefVar(i) = t1;	/* protect it */
	DerefArg(t1,telt,2);	/* attribute */
	TRefAttr(i) = t1;	/* protect it */
	DerefCar(telt,trvec);
	DerefCdr(trvec,trvec);
	DerefArg(t1,telt,1);	/* FD variable */
	TRefVar(nvars+i) = t1;	/* protect it */
	DerefArg(t1,telt,2);	/* attribute */
	TRefAttr(nvars+i) = t1;	/* protect it */
      }
    ass_init(pdata);
  }

				/* RESUME HERE */
  pdata->pruned = FALSE;
  if (state_stamp != pdata->stamp) {
    int nvars1, nvals1;
#if CSTATE_ONLY
    nvars1 = pdata->nvars_back;
    nvals1 = pdata->nvals_back;
#else
    DerefArg(telt,X(0),3);
    nvars1 = nvars-GetSmall(telt);
    DerefArg(telt,X(0),4);
    nvals1 = nvals-GetSmall(telt);
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
  for (i=0; i<pdata->nvartargets; i++) {
    VERTEX vertex = TVAR(i);
    t1 = TRefAttr(vertex); /* get attribute */
    DerefAttribute(t1,t1);
    FDSET(vertex) = DomainSet(t1);
    SIZE(vertex) = GetSmall(DomainSize(t1));
    if (SIZE(vertex)>1) ground = FALSE;
    VISITED(vertex) = 0;
  }
  for (i=0; i<pdata->nvaltargets; i++) {
    VERTEX vertex = TVAL(i);
    t1 = TRefAttr(vertex); /* get attribute */
    DerefAttribute(t1,t1);
    FDSET(vertex) = DomainSet(t1);
    SIZE(vertex) = GetSmall(DomainSize(t1));
    if (SIZE(vertex)>1) ground = FALSE;
    VISITED(vertex) = 0;
  }
  if (state_stamp != pdata->stamp) { /* recompute the graph repr. */
    ass_refresh(pdata);
  } else {			/* update the graph repr. */
    ass_update(pdata);
  }
  pdata->stamp = state_stamp+1;

  if (!circuit_filter(pdata))
    goto ret;
  ent = ground;
  if (!pdata->pruned)
    goto ret;
				/* Compute prunings */
  for (i=0; i<pdata->nvartargets; i++)
    for (j=0; j<2; j++) {
      VERTEX vertex = j==0 ? TVAR(i) : TVAL(i);

      if (LIVE(vertex) != SIZE(vertex))
	FDSET(vertex) = fd_localize(w,FDSET(vertex));
    }
  for (i=0; i<pdata->nvartargets; i++)
    for (j=0; j<2; j++) {
      VERTEX vertex = j==0 ? TVAR(i) : TVAL(i);

      if (LIVE(vertex) != SIZE(vertex))
        request_tell(w, TRefAttr(vertex), TRefVar(vertex),
		     FDSET(vertex),
		     2, 3);
    }

  contract_vars(pdata);
  contract_vals(pdata);

#if CSTATE_ONLY
  if (committed) {
    pdata->nvars_back = pdata->nvartargets;
    pdata->nvals_back = pdata->nvaltargets;
  }
#else
  CTagToArg(X(0),3) = MakeSmall(nvars-pdata->nvartargets);
  CTagToArg(X(0),4) = MakeSmall(nvals-pdata->nvaltargets);
#endif

  ent = (pdata->nvartargets<=1);
ret:
  Pfree(ent==1,handle,pdata);
  RefTerm(Actions) = request_done(w, ent, 2, 3);
}
