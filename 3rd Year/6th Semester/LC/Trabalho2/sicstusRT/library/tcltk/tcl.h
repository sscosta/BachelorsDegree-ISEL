/* Copyright(C) 1994, Swedish Institute of Computer Science */

/* [PM] May 2000 1 to enable list(CommandList) 0 for pre 3.8.4 behaviour  */
#ifndef TCL_ENABLE_LIST
#define TCL_ENABLE_LIST 1
#endif

#define BUFLEN 1024
#define TRUE 1
#define FALSE 0

/* Local error types */
#define SPTCL_ERROR (INSTANTIATION_ERROR+32)
#define SPTK_ERROR (SPTCL_ERROR+1)

/*   --------------------------------------------------------------  */

/* Type declarations */

struct interp_data {
  struct interp_data *self;	/* For checking */
  Tcl_Interp *interp;  
  SP_stream *stream;
  struct event_q *event_list;
  struct interp_data *next;	/* For bug workaround */
};

struct event_q {
  struct event_q *next;
  char event_string[1];
};

struct event_stream_data {
  Tcl_Interp *interp;
  int index;
  int length;
  int size;
  char *buffer;
};

/*   --------------------------------------------------------------  */

struct local_state {
  unsigned long atm_nil;
  unsigned long atm_true;
  unsigned long atm_false;
  unsigned long atm_interp;
  unsigned long atm_period;
  unsigned long atm_write;
  unsigned long atm_writeq;
  unsigned long atm_write_canonical;
  unsigned long atm_format;
  unsigned long atm_chars;
  unsigned long atm_br;
  unsigned long atm_dq;
  unsigned long atm_sqb;
  unsigned long atm_min;
  unsigned long atm_dot;
#if TCL_ENABLE_LIST
  unsigned long atm_list;
  unsigned long atm_term;
#endif
  SP_pred_ref call_pred;
  SP_pred_ref read_pred;
  SP_pred_ref write_pred;
  SP_pred_ref writeq_pred;
  SP_pred_ref write_canonical_pred;
  SP_pred_ref format_pred;
  int tcl_no_registry_check; /* = 0 [PM] */
  unsigned long atm_window;
  int tk_inited;		/* = 0 */
#if DBG
  SP_stream *old_err;
#endif
  struct sptkcon *sptkcon_outh; /* = NULL */
  struct sptkcon *sptkcon_errh; /* = NULL */
  int err_type;
  char err_msg[BUFLEN];
  int err_argno;
};

/* [PM] 3.9b5 PRM 2939 resource specific name for local state variable
   using external linking is needed to avoid conflict when linking
   statically with other resources using a 'local' state */
#define local tcl_resource_local

extern struct local_state local;

/*   --------------------------------------------------------------  */
/* Exported functions */

#if 1
#include "tcltk_glue.h"
/* called from tk_initialize/deinitialize */
extern void tcl_initialize PROTOTYPE((int));
extern void tcl_deinitialize PROTOTYPE((int));

#else
extern void tcl_initialize PROTOTYPE((int));
extern void tcl_deinitialize PROTOTYPE((int));
extern void tcl_new PROTOTYPE((SP_term_ref));
extern void tcl_delete_interp PROTOTYPE((SP_term_ref));
extern void tcl_eval PROTOTYPE((SP_term_ref, SP_term_ref, SP_term_ref));
extern void tcl_event PROTOTYPE((SP_term_ref, SP_term_ref, SP_term_ref));
extern void tcl_add_result PROTOTYPE((SP_term_ref, SP_term_ref, SP_term_ref));

#if 0 /* [PM] Not needed after all */
extern void tcl_initialization_flags PROTOTYPE((int)); /* [PM] */
#endif

extern void SPCDECL tk_initialize PROTOTYPE((int));
extern void SPCDECL tk_deinitialize PROTOTYPE((int));
extern void tk_new PROTOTYPE((SP_term_ref, char *, char *, long, long));
extern void tk_destroy_window PROTOTYPE((SP_term_ref));
extern void tk_make_window_exist PROTOTYPE((SP_term_ref));
extern void tk_main_window PROTOTYPE((SP_term_ref, SP_term_ref));
extern long tk_do_one_event1 PROTOTYPE((long));
extern void tk_do_one_event3 PROTOTYPE((SP_term_ref, long, SP_term_ref));
extern void tk_num_main_windows PROTOTYPE((SP_term_ref));
extern void tk_term PROTOTYPE((SP_term_ref, char *, char *, SP_stream **, SP_stream **, SP_stream **));
#endif

/*   --------------------------------------------------------------  */

void ptr_to_wrapper PROTOTYPE((unsigned long functor,void *,SP_term_ref));
void *wrapper_to_ptr PROTOTYPE((unsigned long functor,SP_term_ref t));
void sptcl_raise_error PROTOTYPE((char *, int));
void sptcl_save_error PROTOTYPE((unsigned long, char *, SP_term_ref, int));
SP_stream *init_tcl_stream PROTOTYPE((struct interp_data *));
SP_stream *get_tcl_stream PROTOTYPE((struct interp_data *, char *));
int translate_command PROTOTYPE((struct interp_data *, SP_term_ref, char **));

int trans_command PROTOTYPE((SP_term_ref, SP_stream *, struct event_stream_data *));

int put_event_queue PROTOTYPE((struct interp_data *, int, SP_term_ref));

/*   --------------------------------------------------------------  */

/* This macro is used in predicate definitions that need a efficient */
/* buffer allocation  */
/* You give it a static buffer or a buffer allocated on the stack */
/* with a fixed size. If it is to small this macro allocates it */
/* with malloc */

/* _LEN is the needed size of the buffer */
/* _SIZE is the size of the allocated buffer on the stack */
/* _BUF is the name of the stack buffer */
/* _ERROR is code to execute on error */
/* _PTR is a pointer to the buffer to use */

#define BUF_ALLOC(_LEN,_SIZE,_BUF,_ERROR,_PTR) \
{ \
  if ((_LEN) >= (_SIZE)) \
    { \
      _PTR = (char *)SP_malloc((_LEN)); \
      if (_PTR == NULL) \
	{ \
	  _ERROR; \
	} \
    } \
  else \
    _PTR = _BUF; \
}

/* Deallocate if dynamically allocated */

#define BUF_FREE(_BUF,_PTR) \
{ \
  if (_BUF != _PTR) \
    SP_free(_PTR); \
}

/*   --------------------------------------------------------------  */

#define SP_ALIGN(X, Alignment) \
  (((unsigned long)(X) + (Alignment)-1) & ~(unsigned long)((Alignment)-1))

/*   --------------------------------------------------------------  */

#define SAVE_ERROR(Type, Msg, Culprit, ArgNo) \
{ \
  sptcl_save_error((Type), (Msg), (Culprit), (ArgNo)); \
  goto error; \
}

#define RAISE_ERROR(PredName, Arity) \
{ \
 error: \
  sptcl_raise_error(PredName, Arity); \
  return; \
}

/*   --------------------------------------------------------------  */

#define CHECK_INTERP(Interp_data, Interp, TInterp, Argno) \
{ \
  if (Interp_data == NULL || \
      (Interp=Interp_data->interp) == NULL || \
      Interp_data->self != Interp_data) \
    SAVE_ERROR(DOMAIN_ERROR, "tcl_interpreter", TInterp, 1); \
}

#define GetInterp(TInterp, PInterp, Argno) \
{ \
  struct interp_data *gi_p = \
    (struct interp_data *)wrapper_to_ptr(local.atm_interp,(TInterp)); \
  CHECK_INTERP(gi_p, PInterp, TInterp, Argno); \
}
