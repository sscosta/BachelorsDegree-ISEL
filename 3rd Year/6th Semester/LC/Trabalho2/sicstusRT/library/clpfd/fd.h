/* Copyright(C) 1999, Swedish Institute of Computer Science */

#include "datadefs.h"
#include "support.h"
#include "predtyp.h"
#include "clpfd_glue.h"         /* [PM] 3.9b4 splfr-generated   */

#define MAGIC                   /* empty */

#if MULTI_SP_AWARE
#if !ENABLE_CLPFD_MULTI_SP
#error "Inconsistency. SICStus must be compiled with ENABLE_CLPFD_MULTI_SP to support clpfd with MULTI_SP_AWARE/PM"
#endif

#if !SP_FOREIGN_SPENV
#error "Multi SP clpfd only supports SPEnv-style dispatch argument"
#endif /* !SP_FOREIGN_SPENV */
/* 
   [PM] 3.9b4 The following outlines a method for passing the hidded
   multi SP context argument. This method requres that all procedures
   use prototypes.

   The method has the advantage that you do not have to modify the
   actual code, you just wrap each function body with a sequence of
   defines. Another advantage at least when debugging is that all
   procedures retain their name, alternatively you can introduce name
   mangling similar to SP_MANGLE to ensure link-time consistency
   checking. Another advantage is that modifying the original sources
   to support this method can be done with emacs
   query-replace-regexp/keyboard macros and so is not very
   error-prone. The method is is also easily maintainable and CVS
   merge will work between older code that does not use the new
   defines and new code that does (since the meat of the procedure
   bodies are unaffected.) A disadvantage is that you have to wrap
   each function body in an ugly sequence of defines.  (This could be
   made much nicer if a more powerful macro processor us used, sed
   would do.).

   Except for foreign predicates, each static or extern procedure is
   redefined to take a HIDDEN_ARG. When in a prototype we let
   HIDDEN_ARG expand to a declaration of the extra argument, when in a
   procedure body we let HIDDEN_ARG expand to the name of the extra
   argument. The foreign predicate procedures are name mangled in the
   splfr-generated glue header and must be treated specially.
   
   As an example assuming a function int foo (long, double):

   define foo(A1,A2) foo(HIDDEN_ARG, A1, A2)

   For all foreign procedures and for all procedures with a #define
   above you should:

   Ensure that when the procedure prototype appears (either
   declaration or part of the definition) then the following two
   defines must be in effect.

      #define HIDDEN_ARG HIDDEN_ARG_PROTO

   The prototyped definition
   static int foo(long x, double y) {... }
   then expands to:
   static int foo (HIDDEN_ARG_PROTO, long x, double y) {...}"

   When inside a procedure body the following two defines must be in
   effect:

      #define HIDDEN_ARG HIDDEN_ARG_NAME

   This can be achived by wrapping each body
   { // procedure body start
      ... the code of the body
   } // procedure body end
   as
   { // procedure body start
      #undef HIDDEN_ARG
      #define HIDDEN_ARG HIDDEN_ARG_NAME

      ... // ordinary body

      #undef HIDDEN_ARG
      #define HIDDEN_ARG HIDDEN_ARG_PROTO
   } // procedure body end

   A call foo(43, 3.14) in the body then expands to foo(HIDDEN_ARG_NAME, 3, 3.14).

   MC expressed dislike towards the pairs
   #undef HIDDEN_ARG
   #define HIDDEN_ARG ... that wrap every function body.

   so we use a different technique. It uses some of the
   less well known aspects of the C pre-processor. The key are the
   following defines that holds throughout the file.
*/ 

#define HIDDEN_ARG_PROTO SPAPI_ARG_PROTO_DECL0
#define HIDDEN_ARG_NAME SPAPI_ARG0
#define HIDDEN_ARG_COMMA HIDDEN_ARG,
#define HIDDEN_ARG_OR_ZERO HIDDEN_ARG
#define HIDDEN_ARG HIDDEN_ARG_NAME
#define HIDDEN_PROTO HIDDEN_ARG_PROTO, /* for arity >=1 */
#define HIDDEN_PROTO_VOID HIDDEN_ARG_PROTO /* for arity zero, e.g., foo(void) */

/* When the SPEnv* does not come as an extra argument (for instance,
   when it is hidden inside some other data) you need to extract the
   SPEnv and make the appropriate variable point to the SPEnv so that
   the clpfd global state and the SICStus API functions can be
   accessed.

  Use this before accessing any struct fd global state and before
  doing any callbacks to SICStus. See alldiff_destructor for an
  example.

  Currently FD_SETUP_SPENV are used by the destructor methods. The
  destructors are all (except struct indexical_info.destructor_fun)
  called with a pointer to the memory block they should destruct. The
  memory block contains a pointer to the SPEnv. All these destructors
  are called from '$free'/1 (a.k.a. prolog_free()). The special case
  is indexical_info.destructor_fun that is called from
  Emulator/indexing.c scan_info() with a pointer to a pointer to the
  memory block.

  FD_SETUP_SPENV(<a pointer to an SPEnv>) should not be followed by a
  semicolon (it will be harmless unless there are more declarations
  following it).
  
*/
#define FD_SETUP_SPENV(SPENV) SPAPI_ARG_PROTO_DECL0=(SPENV); /* Requires SP_FOREIGN_SPENV */
/* use this to initialize the field in the memory block, e.g. FD_STORE_SPENV(foo->spenv); */
#define FD_STORE_SPENV(SPENV) (SPENV)=HIDDEN_ARG


#else  /* !MULTI_SP_AWARE */

#define FD_SETUP_SPENV(SPENV)
#define FD_STORE_SPENV(SPENV)

#define HIDDEN_ARG
#define HIDDEN_ARG_COMMA
#define HIDDEN_ARG_OR_ZERO 0
#define HIDDEN_PROTO /* empty, for arity >=1 */
#define HIDDEN_PROTO_VOID void /* for arity zero, e.g., foo(void) */

#endif /* MULTI_SP_AWARE */

#if MULTI_SP_AWARE
/* redefines for all extern procedures */
/* fdsets.c */
#define fd_first_and_rest(A1,A2,A3) fd_first_and_rest(HIDDEN_ARG, A1,A2,A3)
#define fd_localize(A1,A2) fd_localize(HIDDEN_ARG, A1,A2)
#define fd_localize_if_holes(A1,A2) fd_localize_if_holes(HIDDEN_ARG, A1,A2)
#define fd_interval(A1,A2) fd_interval(HIDDEN_ARG, A1,A2)
#define fd_pair(A1,A2) fd_pair(HIDDEN_ARG, A1,A2)
#define fd_compl_interval(A1,A2) fd_compl_interval(HIDDEN_ARG, A1,A2)
#define fd_lsh(A1,A2) fd_lsh(HIDDEN_ARG, A1,A2)
#define fd_neg_internal(A1,A2,A3) fd_neg_internal(HIDDEN_ARG, A1,A2,A3)
#define fd_subtract(A1,A2) fd_subtract(HIDDEN_ARG, A1,A2)
#define fd_interval_subtract(A1,A2,A3) fd_interval_subtract(HIDDEN_ARG, A1,A2,A3)
#define fd_subtract_interval(A1,A2,A3) fd_subtract_interval(HIDDEN_ARG, A1,A2,A3)
#define fd_complement(A1) fd_complement(HIDDEN_ARG, A1)
#define fd_delete(A1,A2) fd_delete(HIDDEN_ARG, A1,A2)
#define fd_and(A1,A2) fd_and(HIDDEN_ARG, A1,A2)
#define fd_and_interval(A1,A2,A3) fd_and_interval(HIDDEN_ARG, A1,A2,A3)
#define fd_or(A1,A2) fd_or(HIDDEN_ARG, A1,A2)
#define fd_or_interval(A1,A2,A3) fd_or_interval(HIDDEN_ARG, A1,A2,A3)
#define fd_insert_into(A1,A2) fd_insert_into(HIDDEN_ARG, A1,A2)
#define fd_merge_into(A1,A2) fd_merge_into(HIDDEN_ARG, A1,A2)
#define fdcons_add(A1,A2) fdcons_add(HIDDEN_ARG, A1,A2)
/* indexical.c */
#define fd_tell_value(A1,A2,A3,A4) fd_tell_value(HIDDEN_ARG, A1,A2,A3,A4)
#define fd_tell_interval(A1,A2,A3,A4,A5,A6) fd_tell_interval(HIDDEN_ARG, A1,A2,A3,A4,A5,A6)
#define fd_tell(A1,A2,A3,A4,A5) fd_tell(HIDDEN_ARG, A1,A2,A3,A4,A5)
#define fd_told(A1,A2,A3,A4) fd_told(HIDDEN_ARG, A1,A2,A3,A4)

#if 0                           /* not redefined. It uses FD_SETUP_SPENV to access the SPEnv */
#define free_fd_info_hook(A1) free_fd_info_hook(HIDDEN_ARG, A1)
#endif

/* profile.c */
#define init_profile() init_profile(HIDDEN_ARG)
#define profile_cons(A1,A2,A3,A4) profile_cons(HIDDEN_ARG, A1,A2,A3,A4)
#define profile_dispose(A1) profile_dispose(HIDDEN_ARG, A1)
#define profile_merge(A1,A2) profile_merge(HIDDEN_ARG, A1,A2)
#define profile_add(A1,A2,A3,A4) profile_add(HIDDEN_ARG, A1,A2,A3,A4)
#define profile_update(A1,A2,A3,A4) profile_update(HIDDEN_ARG, A1,A2,A3,A4)
#define profile_exclude_one(A1,A2,A3,A4,A5,A6,A7) profile_exclude_one(HIDDEN_ARG, A1,A2,A3,A4,A5,A6,A7)
#define profile_exclude_two(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10) profile_exclude_two(HIDDEN_ARG, A1,A2,A3,A4,A5,A6,A7,A8,A9,A10)
/* support.c */
#define check_argument(A1,A2,A3,A4,A5) check_argument(HIDDEN_ARG, A1,A2,A3,A4,A5)
#define fd_check_overflow() fd_check_overflow(HIDDEN_ARG)
#define fd_put_range(A1,A2,A3) fd_put_range(HIDDEN_ARG, A1,A2,A3)
#define fd_globalize(A1,A2,A3,A4) fd_globalize(HIDDEN_ARG, A1,A2,A3,A4)
#define update_mutable(A1,A2,A3) update_mutable(HIDDEN_ARG, A1,A2,A3)
#define fd_link(A1,A2,A3,A4) fd_link(HIDDEN_ARG, A1,A2,A3,A4)
#define fd_sync(A1) fd_sync(HIDDEN_ARG, A1)
#define fd_end(A1) fd_end(HIDDEN_ARG, A1)
#define fd_dealloc() fd_dealloc(HIDDEN_ARG)
#define fd_enqueue(A1,A2) fd_enqueue(HIDDEN_ARG, A1,A2)
#define fd_dequeue(A1) fd_dequeue(HIDDEN_ARG, A1)
#define fd_enqueue_all(A1,A2,A3,A4) fd_enqueue_all(HIDDEN_ARG, A1,A2,A3,A4)
#define request_done(A1,A2,A3,A4) request_done(HIDDEN_ARG, A1,A2,A3,A4)
#define request_tell(A1,A2,A3,A4,A5,A6) request_tell(HIDDEN_ARG, A1,A2,A3,A4,A5,A6)
#define request_tell_interval(A1,A2,A3,A4,A5,A6,A7) request_tell_interval(HIDDEN_ARG, A1,A2,A3,A4,A5,A6,A7)
#define request_tell_value(A1,A2,A3,A4,A5,A6) request_tell_value(HIDDEN_ARG, A1,A2,A3,A4,A5,A6)
#define request_rewrite_eq(A1,A2,A3,A4,A5) request_rewrite_eq(HIDDEN_ARG, A1,A2,A3,A4,A5)
#define fd_perm_alloc(A1,A2,A3) fd_perm_alloc(HIDDEN_ARG, A1,A2,A3)
#define fd_perm_free(A1,A2) fd_perm_free(HIDDEN_ARG, A1,A2)

#endif /* MULTI_SP_AWARE */

extern void *foo (Argdecl,int,TAGGED);

#define FD_PERSISTENT 1

#if FD_PERSISTENT

/* new persistent storage allocation */
/* Handle is a term [Flag | '$free'(Ptr)] */
#define Palloc(What,Extra,Handle) ((What *)fd_perm_alloc(w,sizeof(What)+(Extra),Handle))

#define Pdata(What,Handle) ((What *)fd_perm_data(Handle))

#define Pfree(If,Handle,Pdata) if (If) fd_perm_free(w,Handle)

#define Pcommitted(Handle) (TagToLST(Handle) >= GLOBAL_UNCOND)

extern void *fd_perm_alloc MAGIC (HIDDEN_PROTO Argdecl,int,TAGGED);
extern void *fd_perm_data(TAGGED);
extern void fd_perm_free MAGIC (HIDDEN_PROTO Argdecl,TAGGED);

#else

/* mimic in terms of temporary storage */
#define Palloc(What,Extra,Handle) ((What *)SP_malloc(sizeof(What)+(Extra)))

#define Pdata(What,Handle) ((What *)NULL)

#define Pfree(If,Handle,Pdata) (*(Pdata)->destructor)(Pdata)

#define Pcommitted(Handle) TRUE

#define SP_alloc_term_refs(N) SP_new_term_refs(N)

#define SP_free_term_refs(Base,N) SP_reset_term_refs(Base)

#endif

extern TAGGED
unify_output_state(Argdecl, TAGGED *phandle, long *pstamp, BOOL *pcommitted);

/* temporary storage allocation */
#define Calloc(N,What) ((What *)numstack_alloc((N)*sizeof(What)>>LogSizeOfWord))

#define Malloc(N,What) ((What *)SP_malloc((N)*sizeof(What)))
#define Free(Ptr) SP_free(Ptr)

#define EmptySet atom_nil

#define Inf atom_inf

#define Sup atom_sup

#define InfAsINT TaggedLow

#define SupAsINT (TaggedHigh-IStep(1))

#define CLPFD_MAXINT ((long)(((TAGGED)(-1L)) >> 1))

/* an FD Set is [First|Rest] */

/* a range is [Min|Max] */
#define RangeMin(X) CTagToCar(X)
#define RangeMax(X) CTagToCdr(X)

/* a domain is dom(Set,Min,Max,Size) */
#define DomainSet(D) CTagToArg(D,1)
#define DomainMin(D) CTagToArg(D,2)
#define DomainMax(D) CTagToArg(D,3)
#define DomainSize(D) CTagToArg(D,4)

/* A can't be Sup and B can't be Inf */
#define EmptyIntervalSafe(A,B) (Tgt(A,B) && AreSmall(A,B))

#define EmptyInterval(A,B) \
(EmptyIntervalSafe(A,B) || (A)==Sup || (B)==Inf)

#define InInterval(X,A,B) \
((Tle(X,B) || (B)==Sup) && (Tle(A,X) || (A)==Inf))

#define FDgt(A,B) EmptyInterval(A,B)

#define FDlt(A,B) EmptyInterval(B,A)

#define DerefNonvar(X) \
{while (IsVar(X)) (X) = CTagToPointer(X);}

#define CMP_BEFORE -1
#define CMP_INSIDE 0
#define CMP_AFTER  1


/*	 v(1,0,'$mutable'(dom([[inf|sup]],inf,sup,sup),0),
	       '$mutable'(lists(0,0,[],[],[],[],[]),0))
*/
#define FD_ATTR_DOM_OFFSET 5
#define FD_ATTR_SUSPS_OFFSET 8
#define FD_ATTR_MIN_OFFSET 13
#define FD_ATTR_MAX_OFFSET 14
#define FD_ATTR_SIZE_OFFSET 15
#define FD_ATTR_V_ARITY 4

/* support unification of dvars */
#define RefMutable(Mut) CTagToArg((Mut),1)

#define AttrToSuspM(Attr,To)				\
{							\
  TAGGED m_prev = (Attr);				\
  (To)=RefMutable(CTagToArg(m_prev,3));			\
  while (TagToHeadfunctor(To)==fd.functor_v4) {		\
    m_prev=(To); (To)=RefMutable(CTagToArg(m_prev,3));	\
  }							\
  (To)=CTagToArg(m_prev,4);				\
}

#define AttrToDomM(Attr,To)				\
{							\
  TAGGED m_prev = (Attr);				\
  (To)=RefMutable(CTagToArg(m_prev,3));			\
  while (TagToHeadfunctor(To)==fd.functor_v4) {		\
    m_prev=(To); (To)=RefMutable(CTagToArg(m_prev,3));	\
  }							\
  (To)=CTagToArg(m_prev,3);				\
}

#define DerefAttribute(To,Attr)			\
{						\
  (To)=RefMutable(CTagToArg((Attr),3));		\
  while (TagToHeadfunctor(To)==fd.functor_v4) {	\
    (To)=RefMutable(CTagToArg((To),3));		\
  }						\
}

/* suspension mask bits */
#define MASK_DOM 0x1
#define MASK_MIN 0x2
#define MASK_MAX 0x4
#define MASK_MINMAX 0x8
#define MASK_VAL 0x10
#define MASK_SINGLETON 0x20

/* What is the relation between two FD sets. */
#define FDI_EQUAL 1
#define FDI_SUBSET 2
#define FDI_SUPERSET 3
#define FDI_DISJOINT 4
#define FDI_INTERSECT 5

/* The temporal relations between two intervals. */
#define FD_BEFORE 0
#define FD_MEETS 1
#define FD_OVERLAPS 2
#define FD_FINISHED_BY 3
#define FD_CONTAINS 4
#define FD_STARTS 5
#define FD_EQUALS 6
#define FD_STARTED_BY 7
#define FD_DURING 8
#define FD_FINISHES 9
#define FD_OVERLAPPED_BY 10
#define FD_MET_BY 11
#define FD_AFTER 12

#define FLOORDIV(Over,Under) \
((Under)==1 ? (Over) : \
 ((Over)>=0) ? (Over)/(Under) : -((-(Over)-1)/(Under)+1))
#define  CEILDIV(Over,Under) \
((Under)==1 ? (Over) : \
 ((Over)<=0) ? (Over)/(Under) : ((Over)-1)/(Under)+1)

#define point_vs_range(p, r) point_vs_interval(p, RangeMin(r), RangeMax(r))
#define val_vs_range(p, r) val_vs_interval(p, RangeMin(r), RangeMax(r))

typedef unsigned long ix_byte;

typedef struct profile *PROFILE;

struct propagator {
  struct propagator *next;
  TAGGED *indexical;
  int ixfirst, ixlast, ixsize;
  TAGGED *global;
  int glfirst, gllast, glsize;
};

struct fd_state {
  void *gdata;			/* of various types */
  unsigned long debugging;
  unsigned long resumptions;
  unsigned long entailments;
  unsigned long prunings;
  unsigned long failures;
  unsigned long constraints;
  PROFILE profile;
  struct profile *profile_pool;
  int scc_component;		/* SCC number */
  int scc_visited;		/* Vertex number during SCC search */
  int scc_index;		/* Stack ptr during SCC search */
  long *scc_stack;
  struct gcc_graph *scc_graph;
  struct mod_def *fd_module;

  SP_FD_FreeFun *fd_destructor_fun; /* [PM] 3.9b4 replaced the fd_destructor field */

  SP_pred_ref call_action1;
  BOOL fd_overflow;
  TAGGED functor_v4;
  TAGGED functor_dom4;
  TAGGED functor_ix7;
  TAGGED functor_lists7;
  TAGGED functor_iff4;
  TAGGED functor_in_set2;
  TAGGED functor_dom;
  TAGGED functor_min;
  TAGGED functor_max;
  TAGGED functor_minmax;
  TAGGED functor_val;
  TAGGED functor_call;
  TAGGED functor_eq;
  TAGGED functor_mutable;
  TAGGED functor_Dfree;
  TAGGED token_a;
  TAGGED token_d;
  TAGGED token_h;
  TAGGED token_l;
  TAGGED token_t;
  TAGGED linkage_keys[8];
  struct propagator *current_propagator;
  struct propagator *free_propagators;
  struct sw_on_key *dispatch;
};

#if !MULTI_SP_AWARE  
extern struct fd_state fd;
#endif /* !MULTI_SP_AWARE */

#if MULTI_SP_AWARE
#define fd (*(struct fd_state*)*(SP_foreign_stash()))
#endif /* MULTI_SP_AWARE */


/* The literals array below consists of items tagged:
   CONST - integers, inf or sup
   STRUCT - FD sets with a bignum header, in its own mem
   LIST - hash table, in its own mem
*/
struct indexical_info {
  /* void (SPCDECL *destructor)(); */
#if 1                           /* [PM] 3.9b4 Make it a direct pointer to free_fd_info_hook */
    SP_FD_FreeFun *destructor_fun;
#else
    struct definition *destructor;
#endif
#if MULTI_SP_AWARE
  SPEnv *spenv;
#endif /* MULTI_SP_AWARE */

    struct definition *pred;
    ix_byte *linkage;		/* each is (var<<8)+index */
    unsigned int length_of_linkage;
    unsigned int length_of_bytecode;
    unsigned int length_of_literals;
    unsigned int pruned:8;
    unsigned int checking:1;	/* checking as opp. to pruning */
    unsigned int truth_value:1;	/* positive as opp. to negative */
    ix_byte *code;
    TAGGED *literals;
    struct indexical_info *next;
};

/* sicstus4 compatibility */
#define LetShadowregs
#define GLOBAL_UNCOND (w->global_uncond)

#define FD_ATTRIBUTE_SIZE 26
#define INT_ATTRIBUTE_SIZE 15
extern TAGGED fd_attribute[];

/* [PM] 3.9b5 These do not need HIDDEN_ARG */
extern TAGGED safe_mul (TAGGED t1,TAGGED t2);
extern TAGGED safe_divu (TAGGED t1,TAGGED t2);
extern TAGGED safe_divd (TAGGED t1,TAGGED t2);
extern TAGGED safe_plus (TAGGED t1,TAGGED t2);
extern TAGGED safe_minus (TAGGED t1,TAGGED t2);

extern TAGGED fd_min(TAGGED set);
extern TAGGED fd_max(TAGGED set);
extern void fd_first_and_rest MAGIC (HIDDEN_PROTO TAGGED d1, TAGGED *firstp, TAGGED *restp);
extern TAGGED fd_localize MAGIC (HIDDEN_PROTO Argdecl, TAGGED old);
extern TAGGED fd_localize_if_holes MAGIC (HIDDEN_PROTO Argdecl, TAGGED old);
extern int fd_size(TAGGED set);
extern BOOL fd_singleton(TAGGED set);
extern int point_vs_interval(TAGGED p, TAGGED min, TAGGED max);
extern int val_vs_interval(TAGGED p, TAGGED min, TAGGED max);
extern unsigned int fd_interval_cmp(TAGGED b1,TAGGED e1,TAGGED b2,TAGGED e2);
extern TAGGED fd_interval MAGIC (HIDDEN_PROTO TAGGED min, TAGGED max);
extern TAGGED fd_compl_interval MAGIC (HIDDEN_PROTO TAGGED min, TAGGED max);
extern TAGGED fd_pair MAGIC (HIDDEN_PROTO TAGGED t1, TAGGED t2);
extern TAGGED fd_lsh MAGIC (HIDDEN_PROTO TAGGED d1, long offset);
extern TAGGED *fd_neg_internal MAGIC (HIDDEN_PROTO TAGGED d1, long offset, TAGGED *valuep);
extern TAGGED fd_delete MAGIC (HIDDEN_PROTO TAGGED d1, TAGGED t2);
extern TAGGED fd_subtract MAGIC (HIDDEN_PROTO TAGGED d1, TAGGED t2);
extern TAGGED fd_interval_subtract MAGIC (HIDDEN_PROTO TAGGED b, TAGGED e, TAGGED d2);
extern TAGGED fd_subtract_interval MAGIC (HIDDEN_PROTO TAGGED d, TAGGED b, TAGGED e);
extern TAGGED fd_complement MAGIC (HIDDEN_PROTO TAGGED d);
extern TAGGED fd_and MAGIC (HIDDEN_PROTO TAGGED d1, TAGGED d2);
extern TAGGED fd_and_interval MAGIC (HIDDEN_PROTO TAGGED d, TAGGED b, TAGGED e);
extern TAGGED fd_and_min(TAGGED d1, TAGGED d2);
extern TAGGED fd_and_min2(TAGGED d1, TAGGED d2, TAGGED *elt);
extern TAGGED fd_or MAGIC (HIDDEN_PROTO TAGGED d1, TAGGED d2);
extern TAGGED fd_or_interval MAGIC (HIDDEN_PROTO TAGGED d1, TAGGED b, TAGGED e);

extern TAGGED check_argument MAGIC (HIDDEN_PROTO Argdecl, TAGGED argument, TAGGED min, TAGGED max, TAGGED size);
extern int fd_compare(TAGGED d1, TAGGED d2);
extern int fd_compare_interval(TAGGED d1, TAGGED min, TAGGED max);
extern int fd_compare_intervals(TAGGED b1, TAGGED e1, TAGGED mb2, TAGGED e2);
extern TAGGED fd_merge_into MAGIC (HIDDEN_PROTO TAGGED d1, TAGGED d2);
extern TAGGED fd_insert_into MAGIC (HIDDEN_PROTO TAGGED t, TAGGED d);
extern BOOL fd_member(TAGGED x, TAGGED set);
extern BOOL fd_check_overflow MAGIC (HIDDEN_PROTO_VOID);
extern TAGGED *fd_put_range MAGIC (HIDDEN_PROTO TAGGED *h, TAGGED b, TAGGED e);
extern TAGGED fd_globalize MAGIC (HIDDEN_PROTO Argdecl, TAGGED old, long req, int ar);
extern void update_mutable MAGIC (HIDDEN_PROTO Argdecl, TAGGED new_value, TAGGED mutable);
extern void fd_sync MAGIC (HIDDEN_PROTO Argdecl);
extern void fd_end MAGIC (HIDDEN_PROTO Argdecl);
extern void fd_dealloc MAGIC (HIDDEN_PROTO_VOID);
extern void fd_enqueue MAGIC (HIDDEN_PROTO TAGGED item, int where);
extern int  fd_dequeue MAGIC (HIDDEN_PROTO TAGGED *item);
extern void fd_enqueue_all MAGIC (HIDDEN_PROTO Argdecl, int bits, TAGGED filter, TAGGED lists_loc);
extern BOOL adjust_bounds(TAGGED tmin, TAGGED tmax, TAGGED d, TAGGED *minp, TAGGED *maxp);
extern BOOL adjust_lower_bound(TAGGED tmin, TAGGED d, TAGGED *minp, TAGGED *maxp);
extern BOOL adjust_upper_bound(TAGGED tmax, TAGGED d, TAGGED *minp, TAGGED *maxp);
extern TAGGED request_done MAGIC (HIDDEN_PROTO Argdecl, int ent, int outarg, int live);
extern void request_tell MAGIC (HIDDEN_PROTO Argdecl, TAGGED dest_mut, TAGGED dest_var, TAGGED fdset, int outarg, int live);
extern void request_tell_interval MAGIC (HIDDEN_PROTO Argdecl, TAGGED dest_mut, TAGGED dest_var, TAGGED min, TAGGED max, int outarg, int live);
extern void request_tell_value MAGIC (HIDDEN_PROTO Argdecl, TAGGED dest_mut, TAGGED dest_var, TAGGED value, int outarg, int live);
extern void request_rewrite_eq MAGIC (HIDDEN_PROTO Argdecl, TAGGED x_var, TAGGED y_var, int outarg, int live);
extern int fd_tell_value MAGIC (HIDDEN_PROTO Argdecl, TAGGED old, TAGGED value, TAGGED dest_mutable);
extern int fd_tell_interval MAGIC (HIDDEN_PROTO Argdecl, TAGGED old, TAGGED min, TAGGED max, TAGGED dest_mutable, int why);
extern int fd_tell MAGIC (HIDDEN_PROTO Argdecl, TAGGED old, TAGGED new, TAGGED dest_mutable, int ar);
extern void fd_told MAGIC (HIDDEN_PROTO Argdecl, TAGGED dest_attr, TAGGED dest_var, long why);
extern void fd_link MAGIC (HIDDEN_PROTO Argdecl, TAGGED var, long key, TAGGED item);

struct profile {
  long begin;
  long end;
  long erg;
  struct profile *next;
};

extern void init_profile MAGIC (HIDDEN_PROTO_VOID);
extern PROFILE empty_profile(void);
extern PROFILE profile_cons MAGIC (HIDDEN_PROTO long begin, long end, long erg, PROFILE next);
extern void profile_dispose MAGIC (HIDDEN_PROTO PROFILE cons);
extern long profile_maxerg(PROFILE h, long b, long e);
extern PROFILE profile_add MAGIC (HIDDEN_PROTO PROFILE p, long b2, long e2, long y2);
extern PROFILE profile_merge MAGIC (HIDDEN_PROTO PROFILE h1, PROFILE h2);
extern PROFILE profile_update MAGIC (HIDDEN_PROTO PROFILE p, long b2, long e2, long y2);
extern BOOL profile_zero_at(PROFILE p, long b2, long e2, long *wit);
extern BOOL profile_nonzero(PROFILE p, long b2, long e2);
extern PROFILE profile_exclude_one MAGIC (HIDDEN_PROTO PROFILE p, long b, long e, long limit, long b1, long e1, long r1);
extern PROFILE profile_exclude_two MAGIC (HIDDEN_PROTO PROFILE p, long b, long e, long limit, long b1, long e1, long r1, long b2, long e2, long r2);
extern BOOL profile_next(PROFILE prof, long *bp, long *ep, long *hp, PROFILE *nextp);

/* support for save & restore */

extern void SPCDECL free_fd_info_hook (struct indexical_info **infop);

#if MULTI_SP_AWARE
#define SP_HOOKS_COOKIE_PROTO HIDDEN_ARG_PROTO
#define SP_HOOKS_COOKIE_USE   (void)HIDDEN_ARG
#else  /* !MULTI_SP_AWARE */

/* xef support.h */
#if ENABLE_CLPFD_MULTI_SP

/* 3.9 when compiling clpfd as a static resource. SPEnv is passed but ignored.
  More precisely: This happens when compiling clpfd as a static
  foreign resource in a SICStus where (dynamic) clpfd supports
  multiple SICStus run-times. */
#define SP_HOOKS_COOKIE_PROTO SPEnv *ignore_cookie
#define SP_HOOKS_COOKIE_USE   (void)ignore_cookie

#else /* !ENABLE_CLPFD_MULTI_SP */

/* Happens when clpfd support for multiple run-times has been switched
   off completely (should not happen) */
#define SP_HOOKS_COOKIE_PROTO void *ignore_cookie
#define SP_HOOKS_COOKIE_USE   (void)ignore_cookie

#endif /* !ENABLE_CLPFD_MULTI_SP */

#endif /* !MULTI_SP_AWARE */

#ifndef SP_MANGLE
# define SP_MANGLE(FNAME) FNAME
#endif

extern void SPCDECL fd_manager_hook (SP_HOOKS_COOKIE_PROTO,Argdecl,int msg,TAGGED *ptr);
/* [PM] 3.9b4 Proper prototype for frd and fwr */
extern void SPCDECL fd_restore_hook(SP_HOOKS_COOKIE_PROTO,struct saverest_record *record, fread_longs_fun *frd, RESTORE_FILE *f);
extern void SPCDECL fd_save_hook (SP_HOOKS_COOKIE_PROTO, struct saverest_record *record, struct definition *pred, fwrite_fun *fwr, FILE *f);

/* Heap routines from Cormen et al., not in use */

typedef long (*HeapFun)(const void *, const void *);

struct heap {
  long size;
  void *item[1];		/* 1-based indexing! */
};

extern void heap_init(struct heap *h);
extern void heap_insert(struct heap *h, void *item, HeapFun cmpfun);
extern void heapify(struct heap *h, HeapFun cmpfun);
extern void *heap_extract_min(struct heap *h, HeapFun cmpfun);

/* Iterators */

typedef struct {
  TAGGED cur, max, tail;
} FDITER;

#define fditer_empty(it) ((it)->cur==(TAGGED)(-1L))

extern void fditer_init(FDITER *it, TAGGED d);
extern TAGGED fditer_next(FDITER *it);
extern void fditer_skip(FDITER *it, TAGGED d);


/* Constructors */

typedef struct {
  TAGGED head, cur;
  int size;
} FDCONS;

#define fdcons_size(cons) ((cons)->size)
#define fdcons_set(cons) ((cons)->head)

extern void fdcons_init(FDCONS *cons);
extern void fdcons_add MAGIC (HIDDEN_PROTO FDCONS *cons, TAGGED d);

