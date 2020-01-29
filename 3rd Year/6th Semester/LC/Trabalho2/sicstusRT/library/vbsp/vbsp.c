#include <windows.h>
#include <oleauto.h>
#include <string.h>
#include <float.h>
#include <sicstus/sicstus.h>

#ifdef DEBUG
#include <stdio.h>
#define Debug(N) {FILE *df=fopen("vbsp_dbg","a");fprintf(df,"%d\n",N);fclose(df);}
#else
#define Debug(_)
#endif

/* [PM] 3.8.6 now use SP_FP_WRAP_BEGIN/END instead of CLEARFP
   Related to SPRM 2022,2084,2103
*/

#ifndef OLD_FP_WRAP
#define OLD_FP_WRAP 0
#endif /* OLD_FP_WRAP */

#ifndef VBSP_DEINITIALIZE_FIRST /* SPRM 2913 */
#define VBSP_DEINITIALIZE_FIRST 1
#endif /* VBSP_DEINITIALIZE_FIRST */

#if 0
#define DEBUG_BREAK() do {_asm { int 3h };} while(0)
#else
#define DEBUG_BREAK() do {;} while(0)
#endif

#if !OLD_FP_WRAP
#include "sp_fpwrap.h"
#endif

#define VBEXPORT __declspec(dllexport) __stdcall

/* [PM] 3.8.7 Some use vbsp.dll from other environments than VB. Give
   them access to a CDECL version of the API */
#define VBEXPORTC __declspec(dllexport) __cdecl

/* Exported functions */
int VBEXPORT PrologInit(char *app_path);
int VBEXPORTC PrologInitC(char *app_path);
long VBEXPORT PrologOpenQuery(char *goal);
long VBEXPORTC PrologOpenQueryC(char *goal);
int VBEXPORT PrologNextSolution(long qid);
int VBEXPORTC PrologNextSolutionC(long qid);
void VBEXPORT PrologCloseQuery(long qid);
void VBEXPORTC PrologCloseQueryC(long qid);
int VBEXPORT PrologQueryCutFail(char *goal);
int VBEXPORTC PrologQueryCutFailC(char *goal);
int VBEXPORT PrologGetLong(long qid, char *var, long *value);
int VBEXPORTC PrologGetLongC(long qid, char *var, long *value);
int VBEXPORT PrologGetString(long qid, char *var, BSTR *value);
int VBEXPORTC PrologGetStringC(long qid, char *var, BSTR *value);
int VBEXPORT PrologGetStringQuoted(long qid, char *var, BSTR *value);
int VBEXPORTC PrologGetStringQuotedC(long qid, char *var, BSTR *value);
void VBEXPORT PrologGetException(BSTR *value);
void VBEXPORTC PrologGetExceptionC(BSTR *value);


/* Types */
struct open_query {
  struct open_query *next;      /* linked as a stack form open_queries */
  SP_qid qid0;
  SP_qid qid1;
  SP_term_ref vars;
};

struct stream_buf {
  char *buf;
  size_t size;
  size_t index;
};

/* Local data & functions */
#if 1                           /* 3.8.8b1, 3.9b4 SPRM 2913 */
#define sp_pre_linkage NULL
#define sp_pre_map NULL
#else
static SP_MainFun *sp_pre_linkage[] = {0};
static char *sp_pre_map[] = {0};
#endif

static SP_stream *vb_stream = NULL;

static struct open_query *open_queries = NULL;

static SP_pred_ref open_query_pred;
static SP_pred_ref call_pred;
static SP_pred_ref write_pred;
static SP_pred_ref writeq_pred;
static SP_pred_ref write_excp_pred;
static SP_pred_ref query_cut_fail_pred;
static SP_pred_ref set_paths_pred;


typedef struct {
  SP_pred_ref *pred;
  char *name;
  int arity;
} pred_def;

static pred_def pred_defs[] =
{
  {&open_query_pred, "open_query", 3},
  {&call_pred, "call", 1},
  {&write_pred, "vbsp_write_term", 2},
  {&writeq_pred, "vbsp_write_term_quoted", 2},
  {&write_excp_pred, "vbsp_write_excp", 2},
  {&query_cut_fail_pred, "query_cut_fail", 1},
  {&set_paths_pred, "set_paths", 2},
  {0,0,0}
};

static void turn_slashes(char *str);
static SP_stream *get_vb_stream(char *str);
static struct open_query *pop_to_query(long);
static int find_val(SP_term_ref vars, char *var, SP_term_ref value);
static int prolog_get_string(SP_pred_ref wpred, long q, char *var, BSTR *value);
static void init_vb_stream(void);
#if 1
static SP_SGetCFun sgetc;
static SP_SPutCFun sputc;
static SP_SEofFun seof;
#else
static int __cdecl sgetc(struct stream_buf *sb);
static int __cdecl sputc(int c, struct stream_buf *sb);
static int __cdecl seof(struct stream_buf *sb);
#if 0
static int __cdecl sclose(struct stream_buf *sb);      
#endif
#endif

#if 0 /* [PM] 3.8.7 has */
static void turn_slashes(char *str);
static SP_stream *get_vb_stream(char *str);
static struct open_query *pop_to_query(long);
static void init_vb_stream(void);
static int __cdecl sgetc(struct stream_buf *sb);
static int __cdecl sputc(int c, struct stream_buf *sb);
static int __cdecl seof(struct stream_buf *sb);
#if 0
static int __cdecl sclose(struct stream_buf *sb);      
#endif
static int find_val(SP_term_ref vars, char *var, SP_term_ref value);
static int prolog_get_string(SP_pred_ref wpred, long q, char *var, BSTR *value);
#endif /* [PM] 3.8.7 has */


#define BUFLEN 256

#define CHECK_BUF(SB) \
  if (SB->index >= SB->size) \
    { \
      SB->size *= 2; \
      SB->buf = (char *)SP_realloc(SB->buf, SB->size); \
    }

#if OLD_FP_WRAP
#define CLEARFP if (_status87() & (SW_OVERFLOW|SW_ZERODIVIDE)) _clear87()
#endif
/* -------------------------------------------------------------------- */

static HINSTANCE   HDllInstance;

BOOL WINAPI DllMain(/*IN*/ HINSTANCE hinstDll, /*IN*/ DWORD fdwReason, LPVOID lpvReserved)
{
  /* Note that only "simple" things are allowed in DllMain, e.g., not
     LoadLibrary.
  */

  switch (fdwReason) {
  case DLL_PROCESS_ATTACH:
    /* DLL is attaching to the address space of the current process. */
       HDllInstance = hinstDll;
       break;
  case DLL_THREAD_ATTACH: 
    /* A new thread is being created in the current process. */
    break;
  case DLL_THREAD_DETACH:
    /* A thread is exiting cleanly. */
    break;
  case DLL_PROCESS_DETACH:
    /* The calling process is detaching the DLL from its address space. */

    /* Note that lpvReserved will be NULL if the detach is due to
       a FreeLibrary() call, and non-NULL if the detach is due to
       process cleanup. */
       
    if( lpvReserved == NULL ) {
      /* vbsp unloaded by process continuing, do cleanup */
    } else {
      /* Process is exiting, no cleanup needed */
    }
    break;
  }
  /* Ignored except when called with DLL_THREAD_ATTACH */
  return(TRUE);
}

/* 1 on success, -1 on error */
int VBEXPORTC PrologInitC(char *app_path)
{
  return PrologInit(app_path);
}

int VBEXPORT PrologInit(char *app_path)
{
  HMODULE hm;
  char buf[MAX_PATH];
  char *p;
  int rc = 42;

  Debug(1);
  if (vb_stream != NULL)	/* already initialized */
    return SP_SUCCESS;

  Debug(2);

#if !OLD_FP_WRAP
  SP_FP_WRAP_BEGIN;
#endif

  #if VBSP_DEINITIALIZE_FIRST
  /* 
     [PM] 3.9b4
     Make sure SICStus always starts out deinitialized. Sometimes the
     sprt DLL would not get unloaded if running from within the VB
     development environment (at least in 3.8).

     By deinitializing before SP_initialize we make sure that SICStus
     is always properly deinitialized if run from the development
     environment. If run as a standalone app it should not matter
     since SP_deinitialize is a no-op if SICStus has not been
     initialized.
   */
  SP_deinitialize();
  #endif

  rc = SP_initialize(0, NULL, NULL);
  if (rc != SP_SUCCESS)
    {
      rc = SP_ERROR;
      goto barf;
    }

  /* Find path to vbsp.dll */
  Debug(3);
  /* hm = GetModuleHandle("vbsp"); */
  hm = HDllInstance;
  Debug(4);
  { DWORD tmp = GetModuleFileName(hm, buf, MAX_PATH);
    if (tmp == 0 || tmp+1 >= MAX_PATH) {
      /* failure or possibly truncated name */
      Debug(__LINENO__);
      rc = SP_ERROR;
      goto barf;
    }
  }

  Debug(5);

  p = buf + strlen(buf);
  Debug(6);
  while (p > buf && *p != '.')
    p--;

  if (p > buf)
    {
      *p = '\0';
    }

  Debug(7);
  /* buf is "....\vbsp" here */

  turn_slashes(buf);

  /* buf is "..../vbsp" here */
  /* let SP_load worry about extension (.ql, .po, .pl?) */
#if 0
  strcat(buf, ".ql");
#endif

  /* Load "vbsp.ql" */
  Debug(8);
  if (SP_load(buf) != SP_SUCCESS)
    {
      rc = SP_ERROR;
      goto barf;
    }

  Debug(9);

  /* Setup the predicates */

  { pred_def *def = &pred_defs[0];
    while ( def->pred ) {
      *def->pred = SP_predicate(def->name, def->arity, "vbsp");
      if ( *def->pred == NULL ) {
        rc = SP_ERROR;
        goto barf;
      }
      def++;
    }
  }

  Debug(10);
  /* Set file_search_path */
  {
    SP_term_ref tVBSPPath = SP_new_term_ref();
    SP_term_ref tAppPath = SP_new_term_ref();

    Debug(11);

    /* buf is "..../vbsp" here, drop last /basename part */
    while (p > buf && *p != '/')
      p--;
    if (p > buf)
      {
        *p = '\0';
      }

    Debug(12);
    Debug(13);
    if (!SP_put_string(tVBSPPath, buf))
      {
        rc = SP_ERROR;
        goto barf;
      }

    Debug(18);
    

    if (strlen(app_path) >= MAX_PATH)
      {
        rc = SP_ERROR;
        goto barf;
      }
    strcpy(buf, app_path);
    turn_slashes(buf);
    Debug(19);
    if (!SP_put_string(tAppPath, buf))
      {
        rc = SP_ERROR;
        goto barf;
      }

    Debug(20);
    if (SP_query_cut_fail(set_paths_pred, tVBSPPath, tAppPath) != SP_SUCCESS)
      {
        rc = SP_ERROR;
        goto barf;
      }
  }

  Debug(21);
  init_vb_stream();
  
  rc = SP_SUCCESS;

  Debug(22);

 barf:
  Debug(23);

#if !OLD_FP_WRAP
  SP_FP_WRAP_END;
#else
  CLEARFP;
#endif

  return rc;
}



long VBEXPORTC PrologOpenQueryC(char *goal)
{
  return PrologOpenQuery(goal);
}

long VBEXPORT PrologOpenQuery(char *goal)
{
  long rc;
  SP_term_ref tStream, tGoal;
  struct open_query *q;
  SP_stream *stream;

#if !OLD_FP_WRAP
  SP_FP_WRAP_BEGIN;
#endif

  if (!(stream = get_vb_stream(goal)))
    {
      /* e.g, PrologInit not called yet */
      rc = SP_ERROR;
      goto do_return;
    }

  if (!(q = (struct open_query *)SP_malloc(sizeof(struct open_query)))) 
    {
      rc = SP_ERROR;
      goto do_return;
    }

  q->next = open_queries;
  open_queries = q;
  q->qid0 = 0;
  q->qid1 = 0;
  q->vars = SP_new_term_refs(1);
  tStream = SP_new_term_ref();
  tGoal = SP_new_term_ref();
  /* after this point all failures must goto bomb: */

  if (!SP_put_address(tStream, (void *)stream)) goto bomb;
  if (!SP_put_variable(q->vars)) goto bomb;
  if (!SP_put_variable(tGoal)) goto bomb;

  /* First open a query and make a call to convert the goal string to
     a term and extract the variables. Leave open so the goal can be
     used in the "real" query.
     Could use SP_query but that would leave stuff on heap after
     PrologCloseQuery */
  q->qid0 = SP_open_query(open_query_pred, tStream, tGoal, q->vars);
  if (!q->qid0) goto bomb;

  /* syntax errors will give SP_ERROR here */
  if (SP_next_solution(q->qid0) != SP_SUCCESS) goto bomb;

  /* Now set up the actual query. */
  q->qid1 = SP_open_query(call_pred, tGoal);
  if (!q->qid1) goto bomb;
  rc = (long) q;
  goto do_return;

bomb: /* invariant: q has a safe state and is linked into open_queries */

  open_queries = q->next;       /* unlink */
  if (q->qid0)
    { /* Will close qid1 too if any */
      SP_close_query(q->qid0);
    }
  /* will dealloc tStream, tGoal too */
  SP_reset_term_refs(q->vars);
  SP_free(q);
  rc = SP_ERROR;
  /* fall through */

 do_return:
  /* All return paths must reach SP_FP_WRAP_END */

#if !OLD_FP_WRAP
  SP_FP_WRAP_END;
#else
  CLEARFP;
#endif

  return rc;
}

/* validate q and unlink/deallocate more recent queries */
static struct open_query *pop_to_query(long q)
{ struct open_query *p = open_queries;
  while (p && p != (struct open_query *)q)
    {
      p = p->next;
    }
  if (p)
    { /* precondition: p==q */
      /* unlink and deallocate newer queries */
      while (open_queries != (struct open_query *) q)
        {
          struct open_query *tmp = open_queries;
          open_queries = open_queries->next;
          
          /* tmp->qid0,1 will be discarded by SP_next_solution(q) */
          SP_reset_term_refs(tmp->vars);
          SP_free(tmp);
        }
    }
  else
    { /* q was not linked from open_queries */
      return NULL;
    }
  /* precondition: q == open_queries */
  return (struct open_query *) q;
}



int VBEXPORTC PrologNextSolutionC(long lq)
{
  return PrologNextSolution(lq);
}

int VBEXPORT PrologNextSolution(long lq)
{ struct open_query *q;
  int rc;

#if !OLD_FP_WRAP
  SP_FP_WRAP_BEGIN;
#endif

  if (!(q = pop_to_query(lq)))
    {
      rc = SP_ERROR;
      goto barf;
    }

  rc = SP_next_solution(q->qid1);

 barf:
#if !OLD_FP_WRAP
  SP_FP_WRAP_END;
#else
  CLEARFP;
#endif

  return rc;
}


void VBEXPORTC PrologCloseQueryC(long lq)
{
  PrologCloseQuery(lq);
}

void VBEXPORT PrologCloseQuery(long lq)
{ struct open_query *q;

  if (!(q = pop_to_query(lq))) return;

  open_queries = q->next;

#if 0
  /* closing qid0 will also take care of (the more recent) qid1. */
  SP_close_query(q->qid1);
#endif
  SP_close_query(q->qid0);
  SP_reset_term_refs(q->vars);
  SP_free(q);
}


int VBEXPORTC PrologQueryCutFailC(char *goal)
{
  return PrologQueryCutFail(goal);
}

int VBEXPORT PrologQueryCutFail(char *goal)
{
  SP_term_ref tStream;
  SP_stream *stream;
  int rc;

#if !OLD_FP_WRAP
  SP_FP_WRAP_BEGIN;
#endif

  if (!(stream = get_vb_stream(goal)))
    {
      rc = SP_ERROR;
      goto barf;
    }

  tStream = SP_new_term_refs(1);
  if (SP_put_address(tStream, (void *)stream))
    { 
      rc = SP_query_cut_fail(query_cut_fail_pred, tStream);
    }
  else
    {
      rc = SP_ERROR;
    }

  SP_reset_term_refs(tStream);

 barf:
#if !OLD_FP_WRAP
  SP_FP_WRAP_END;
#else
  CLEARFP;
#endif

  return rc;
}



int VBEXPORTC PrologGetLongC(long q, char *var, long *value)
{
  return PrologGetLong(q, var, value);
}

int VBEXPORT PrologGetLong(long q, char *var, long *value)
{
  int rc;
  SP_term_ref t;

  { struct open_query *p = open_queries;
     while (p && p!= (struct open_query *) q)
       {
         p = p->next;
       }
     if (!p) return SP_ERROR;
  }
  /* q valid (and PrologInit done) */

  t = SP_new_term_refs(1);

  if ((rc = find_val(((struct open_query *)q)->vars, var, t)) == SP_SUCCESS)
    {
      if (!SP_get_integer(t, value))
        rc = SP_FAILURE;
      else
        rc = SP_SUCCESS;
    }
  SP_reset_term_refs(t);
  return rc;
}


int VBEXPORTC PrologGetStringC(long q, char *var, BSTR *value)
{
  return PrologGetString(q, var, value);
}

int VBEXPORT PrologGetString(long q, char *var, BSTR *value)
{
  return prolog_get_string(write_pred, q, var, value);
}


int VBEXPORTC PrologGetStringQuotedC(long q, char *var, BSTR *value)
{
  return PrologGetStringQuoted(q, var, value);
}

int VBEXPORT PrologGetStringQuoted(long q, char *var, BSTR *value)
{
  return prolog_get_string(writeq_pred, q, var, value);
}

static int prolog_get_string(SP_pred_ref wpred, long q, char *var, BSTR *value)
{
  int rc;
  SP_term_ref t;
  char *s;
  BSTR new_value;

  { struct open_query *p = open_queries;
     while (p && p!= (struct open_query *)q)
       {
         p = p->next;
       }
     if (!p) return SP_ERROR;
  }
  /* q valid (and PrologInit done) */

#if !OLD_FP_WRAP
  SP_FP_WRAP_BEGIN;
#endif

  t = SP_new_term_refs(1);

  if ((rc = find_val(((struct open_query *)q)->vars, var, t)) == SP_SUCCESS)
    {
    SP_term_ref tStream = SP_new_term_ref();
    struct stream_buf *sb = (struct stream_buf *)vb_stream->user_handle;
    
    sb->index = 0;
    SP_put_address(tStream, (void *)vb_stream);
    if ((rc = SP_query_cut_fail(wpred, tStream, t)) == SP_SUCCESS)
      {
        CHECK_BUF(sb);
        sb->buf[sb->index++] = '\0';
        s = sb->buf;
        
        new_value = SysAllocStringByteLen(s, strlen(s));
        SysFreeString(*value);
        *value = new_value;
        rc = SP_SUCCESS;
      }
  }

  SP_reset_term_refs(t);

#if !OLD_FP_WRAP
  SP_FP_WRAP_END;
#else
  CLEARFP;
#endif
  
  return rc;
}


void VBEXPORTC PrologGetExceptionC(BSTR *value)
{
  PrologGetException(value);
}

void VBEXPORT PrologGetException(BSTR *value)
{
  char *s = "";
  BSTR new_value;
  SP_term_ref tExcp, tStream;
  struct stream_buf *sb;

#if !OLD_FP_WRAP
  SP_FP_WRAP_BEGIN;
#endif

  if (vb_stream)               /* e.g., PrologInit called */
    { 
      sb = (struct stream_buf *)vb_stream->user_handle;
      tExcp = SP_new_term_refs(1);
      tStream = SP_new_term_ref();

      if (SP_exception_term(tExcp)) {
        sb->index = 0;
        SP_put_address(tStream, (void *)vb_stream);
        if (SP_query_cut_fail(write_excp_pred, tStream, tExcp) == SP_SUCCESS) {
          CHECK_BUF(sb);
          sb->buf[sb->index++] = '\0';
          s = sb->buf;
        }
      }

      SP_reset_term_refs(tExcp);
    }
  else
    {
      /* Should really return SP_ERROR here to indicate error */
    }


  new_value = SysAllocStringByteLen(s, strlen(s));
  SysFreeString(*value);
  *value = new_value;

#if !OLD_FP_WRAP
  SP_FP_WRAP_END;
#else
  CLEARFP;
#endif

}

/* Support -----------------------------------------------------*/

static SP_stream *get_vb_stream(char *str)
{
  #define FULL_STOP ". "
  size_t len = strlen(str) + sizeof(FULL_STOP);
  struct stream_buf *sb;

  if (!vb_stream)
    {
      return NULL;
    }
  
  sb = (struct stream_buf *)vb_stream->user_handle;

  if (len > sb->size)
    { void *b = SP_realloc(sb->buf, len);
      if (!b)
        {
          return NULL;
        }
      sb->buf = b;
      sb->size = len;
    }
  sb->index = 0;
  strcpy(sb->buf, str);
  strcat(sb->buf, FULL_STOP);
  return vb_stream;
}

static void init_vb_stream(void)
{
  struct stream_buf *sb =
    (struct stream_buf *)SP_malloc(sizeof(struct stream_buf));

  if (!sb) return;

  sb->size = BUFLEN;
  sb->buf = (char *)SP_malloc(BUFLEN);
  if (!sb->buf)
    {
      SP_free(sb);
      return;
    }
  SP_make_stream(sb, sgetc, sputc, NULL, seof, NULL, NULL, &vb_stream);
}

static int __cdecl sgetc(void *handle)
{
  struct stream_buf *sb = (struct stream_buf *)handle;

  if (sb->index >= sb->size)
    return sb->index++, -1;

  return *(unsigned char *)(sb->buf + sb->index++);
}


static int __cdecl sputc(char c, void *handle)
{
  struct stream_buf *sb = (struct stream_buf *)handle;

  CHECK_BUF(sb);
  return (sb->buf[sb->index++] = c);
}

static int __cdecl seof(void *handle)
{
  struct stream_buf *sb = (struct stream_buf *)handle;

  return sb->index > sb->size;
}

/* We do *not* want the stream to lose its buffer if it is ever closed 
   Instead we set a NULL close procedure so it becomes unclosable.
   Potential issue: It will never be unlinked from the list of streams.
*/
#if 0
static int __cdecl sclose(struct stream_buf *sb)
{
  SP_free(sb->buf);
  SP_free(sb);
  return 0;
}
#endif

/* Searches a list of 'VarName'=Var pairs, returns Var in value
 */
static int find_val(SP_term_ref vars, char *var, SP_term_ref value)
{
  unsigned long l, aVar = SP_atom_from_string(var);
  SP_term_ref t = SP_new_term_ref(),
    car = SP_new_term_ref(),
    cdr = SP_new_term_ref();

  SP_put_term(cdr, vars);
  while (SP_is_list(cdr))
    {
      SP_get_list(cdr, car, cdr);

      if (!SP_is_compound(car)) return SP_ERROR; /* should not happen */

      SP_get_arg(1, car, t);

      if (!SP_get_atom(t, &l)) return SP_ERROR; /* should not happen */
      if (aVar == l)
	{ /* Happily assumes that car have two (or more) args */
	  SP_get_arg(2, car, value);
	  return SP_SUCCESS;
	}
    }
  return SP_FAILURE;
}

static void turn_slashes(char *str)
{
  for (; *str; str++)
    if (*str == '\\')
      *str = '/';
}

