/* Copyright(C) 1994-95, Swedish Institute of Computer Science */

#include <stdarg.h>             /* [PM] April 2000 before stdio.h fixes va_list issue on dec-osf */
#include <stdio.h>
#include <string.h>
#include <tcl.h>
#include <sicstus/sicstus.h>
#include "tcl.h"

/*   --------------------------------------------------------------  */

/* Local function prototypes */

#if 1                           /* [PM] 3.9 proper types */
static SP_SGetCFun sgetc;
static SP_SPutCFun sputc;
static SP_SEofFun seof;
static SP_SCloseFun sclose;
#else  /* very old */
static int SPCDECL sgetc _ANSI_ARGS_((struct event_stream_data *priv));
static int SPCDECL sputc _ANSI_ARGS_((int c, struct event_stream_data *priv));
static int SPCDECL seof _ANSI_ARGS_((struct event_stream_data *priv));
static int SPCDECL sclose _ANSI_ARGS_((struct event_stream_data *priv));
#endif

/*   --------------------------------------------------------------  */

void ptr_to_wrapper(functor,ptr,tTerm)
     unsigned long functor;
     void *ptr;
     SP_term_ref tTerm;
{
  SP_term_ref  ID = SP_new_term_ref();

  SP_put_address(ID, ptr);
  SP_cons_functor(tTerm, functor, 1, ID);
}

/*   --------------------------------------------------------------  */

void *wrapper_to_ptr(functor,t)
     unsigned long functor;
     SP_term_ref t;
{
  void *ptr;
  unsigned long atm_name;
  int arity;
  SP_term_ref tmp = SP_new_term_ref();

  /* Is it a term '$TclInterp'(Address) ? */

  if (SP_get_functor(t, &atm_name, &arity)
      && (atm_name == functor)
      && arity == 1)
    {
      SP_get_arg(1, t, tmp);
      if (SP_get_address(tmp, &ptr))
	return ptr;
    }
  return NULL;
}

/*   --------------------------------------------------------------  */

void sptcl_save_error(type, msg, culprit, argno)
     unsigned long type;
     char *msg;
     SP_term_ref culprit;
     int argno;
{
  local.err_type = type;
  local.err_argno = argno;

  switch (type&0xff)
    {
    case SPTCL_ERROR:
    case SPTK_ERROR:
      strncpy(local.err_msg, msg, BUFLEN);
      local.err_msg[BUFLEN-1] = '\0';
      break;
    default:
      SP_save_error(type, msg, culprit);
    }
}

void sptcl_raise_error(name, arity)
     char *name;
     int arity;
{
  char *err_funct = "tcl_error";

  switch (local.err_type&0xff)
    {
    case SPTK_ERROR:
      err_funct = "tk_error";
    case SPTCL_ERROR:
    {
      SP_term_ref
	t1 = SP_new_term_ref(),
	t2 = SP_new_term_ref(),
	t3 = SP_new_term_ref();

      SP_put_string(t1, name);
      SP_put_integer(t2, arity);
      SP_cons_functor(t1, SP_atom_from_string("/"), 2, t1, t2);
      SP_put_list_chars(t2, t3, local.err_msg);
      SP_cons_functor(t1, SP_atom_from_string(err_funct), 2, t1, t2);
      SP_raise_exception(t1);
      break;
    }
    default:
      SP_raise_error(name, arity, local.err_argno);
    }
}

/*   --------------------------------------------------------------  */

#define PAD 128
#define ENSURE_SPACE(I) \
if (p->index + (I) + PAD >= p->size) \
  { \
    p->size = SP_ALIGN(p->index+(I)+PAD+1, p->size); \
    p->buffer = (char *)SP_realloc(p->buffer, p->size); \
  }

SP_stream *init_tcl_stream(interp_data)
     struct interp_data *interp_data;
{
  struct event_stream_data *p;
  
  if (!interp_data->stream)
    {
      if (!(p = (struct event_stream_data *)
	        SP_malloc(sizeof(struct event_stream_data))))
	return NULL;
      p->interp = interp_data->interp;
      p->size = BUFLEN;
      p->buffer = (char *)SP_malloc(BUFLEN);
      SP_make_stream_context(p, sgetc, sputc, NULL, seof, NULL, sclose,
                             &interp_data->stream,
                             SP_WCX_FLAG,
                             /* [PM] May 2000. Always use UTF8 for this stream so the buffer is suitable for tcl */
                             SP_STREAMHOOK_WCI);
      
    }
  else
    p = (struct event_stream_data *)interp_data->stream->user_handle;

  p->index = 0;
  return interp_data->stream;
}


SP_stream *get_tcl_stream(interp_data, goal)
     struct interp_data *interp_data;
     char *goal;
{
  SP_stream *stream = init_tcl_stream(interp_data);
  struct event_stream_data *p =
    (struct event_stream_data *)stream->user_handle;
  int length = strlen(goal) + 3;

  ENSURE_SPACE(length);
  p->length = length;

  strcpy(p->buffer, goal);
  strcat(p->buffer, " . ");	/* Adds extra dot so never miss it */
  return stream;
}

/*   --------------------------------------------------------------  */

static int SPCDECL sgetc(void *handle)
{
  struct event_stream_data *p = (struct event_stream_data *)handle;

  if (p->index >= p->length)
    return p->index++, -1;

  return *(unsigned char *)(p->buffer + p->index++);
}

/*   --------------------------------------------------------------  */

static int SPCDECL seof(void *handle)
{
  struct event_stream_data *p = (struct event_stream_data *)handle;

  return p->index > p->length;
}

/*   --------------------------------------------------------------  */

static int SPCDECL sputc(char c, void *handle)
{
  struct event_stream_data *p = (struct event_stream_data *)handle;

  ENSURE_SPACE(1);
  return (p->buffer[p->index++] = c);
}

/*   --------------------------------------------------------------  */

static int SPCDECL sclose(void *handle)
{
  struct event_stream_data *p = (struct event_stream_data *)handle;

  SP_free(p->buffer);
  SP_free(p);
  return 0;
}

/*   --------------------------------------------------------------  */

static int SP_is_nil(SP_term_ref t)
{
  unsigned long name;
  return (SP_get_atom(t, &name), name == local.atm_nil);
}


int translate_command(interp_data, tScript, bufp)
     struct interp_data *interp_data;
     SP_term_ref tScript;
     char **bufp;
{
  SP_stream *stream = init_tcl_stream(interp_data);
  struct event_stream_data *p =
    (struct event_stream_data *)stream->user_handle;
  SP_term_ref t = SP_new_term_ref();

  SP_put_term(t, tScript);
  if (!trans_command(t, stream, p))
    return 0;

  *bufp = p->buffer;
  return 1;
}

#if TCL_ENABLE_LIST
static int trans_command_list( /* (t1, stream, p) */
     SP_term_ref t1,            /* ListOfCommands */
     SP_stream *stream,
     struct event_stream_data *p)
{
  
  int index;                    /* [PM] 3.9b4 this was incorrectly listed as an argument */
  SP_term_ref tList = SP_new_term_ref(), tCar = SP_new_term_ref();
  int res = 0;                  /* assume error */
  Tcl_Obj *listObj = NULL, *elementObj = NULL;
  char *s;
  int s_len;

  SP_put_term(tList, t1);

  listObj = Tcl_NewListObj(0, NULL); /* empty list */
  Tcl_IncrRefCount(listObj);

  index = 0;
  while (SP_is_list(tList))
    {
      SP_get_list(tList, tCar, tList/*cdr*/);
      /* use p->buffer as a scratch area */
      index = p->index;
      if (!trans_command(tCar, stream, p))
        {
          goto error;
        }
      if (elementObj) { Tcl_DecrRefCount(elementObj); elementObj = NULL; }
      elementObj = Tcl_NewStringObj(p->buffer+index, p->index-index);
      Tcl_IncrRefCount(elementObj);
      
      p->index = index;         /* restore p->buffer contents */
      p->buffer[p->index] = '\0'; /* not strictly needed, will be overwritten */

      if (Tcl_ListObjAppendElement(p->interp, listObj, elementObj) != TCL_OK)
        {
          goto error;
        }
    }
  if (!SP_is_nil(tList))
    {
      goto list_error;
    }
  s = Tcl_GetStringFromObj(listObj, &s_len);
  ENSURE_SPACE(s_len);
  memcpy(p->buffer+index, s, s_len);
  p->index += s_len;
  p->buffer[p->index] = '\0';

  res = 1;                      /* succeeded */
 error:
  if (elementObj) Tcl_DecrRefCount(elementObj);
  if (listObj) Tcl_DecrRefCount(listObj);
  return res;

 list_error:
  if (SP_is_variable(t1))
    {
      SAVE_ERROR(INSTANTIATION_ERROR, "", t1, 2); /* implicit goto error */
    }
  else
    {
      SAVE_ERROR(DOMAIN_ERROR+DOMAIN_LIST, "", t1, 2); /* implicit goto error */
    }
}
#if 0 /* old version called 'list' command */
static int trans_command_list0(t1, stream, p, index)
     SP_term_ref t1;            /* ListOfCommands */
     SP_stream *stream;
     struct event_stream_data *p;
     int index;
{
  SP_term_ref t2 = SP_new_term_ref();
  int i, objc;
  int res = 0;                  /* assume error */
  Tcl_Obj **objv = NULL;
  char *s;
  int s_len;

  SP_put_term(t2, t1);
  objc = 1;                     /* 1 for "list" */
  while (SP_is_list(t2))
    {
      SP_get_arg(2, t2, t2);
      objc++;
    }
  if (!SP_is_nil(t2))
    {
      t1 = t2;
      goto list_error;
    }
  objv = (Tcl_Obj **) SP_malloc(objc * (sizeof *objv));
  if (!objv)
    {
      goto error;
    }
  for (i = 0; i < objc; i++)
    {
      objv[i] = NULL;           /* make cleanup safe */
    }
  
  objv[0] = Tcl_NewStringObj("list", -1 /* -1 means NUL terminated arg */);
  Tcl_IncrRefCount(objv[0]);

  for (i = 1; i < objc; i++)
    {
      SP_get_list(t1, t2/*car*/, t1/*cdr*/);
      /* use p->buffer as a scratch area */
      index = p->index;
      if (!trans_command(t2, stream, p))
        {
          goto error;
        }
      objv[i] = Tcl_NewStringObj(p->buffer+index, p->index-index);
      Tcl_IncrRefCount(objv[i]);
      p->index = index;         /* restore p->buffer contents */
      p->buffer[p->index] = '\0'; /* not striclty needed, will be overwritten */
    }

  if (Tcl_EvalObjv(p->interp, objc, objv, 0) != TCL_OK)
    {
      goto error;
    }
  s = Tcl_GetStringResult(p->interp);
  s_len = strlen(s);
  ENSURE_SPACE(s_len);
  memcpy(p->buffer+index, s, s_len+1);
  p->index += s_len;
  /* ASSERT(p->buffer[p->index]) == '\0') */

  res = 1;                      /* succeeded */
 error:
  if (objv) 
    {
      for (i = 0; i < objc; i++)
        {
          if (objv[i])
            {
              Tcl_DecrRefCount(objv[i]);
            }
        }
      SP_free(objv);
    }
  return res;

 list_error:
  if (SP_is_variable(t1))
    {
      SAVE_ERROR(INSTANTIATION_ERROR, "", t1, 2); /* implicit goto error */
    }
  else
    {
      SAVE_ERROR(DOMAIN_ERROR+DOMAIN_LIST, "", t1, 2); /* implicit goto error */
    }
}
#endif

#if 0
static int trans_command_list1(stream, p, index)
     SP_stream *stream;
     struct event_stream_data *p;
     int index;
{
  int res = 1;
  Tcl_Obj *cmd = NULL, *obj = NULL;
  Tcl_Obj *objv[2];
  char *s;
  int s_len;

#if 0
  index = p->index;
      
  if (!trans_command(t1, stream, p))
    {
      return 0;
    }
#endif
      
  obj = Tcl_NewStringObj(p->buffer+index, p->index-index);
  Tcl_IncrRefCount(obj);
  cmd = Tcl_NewStringObj("list", -1 /* NUL terminated arg */);
  Tcl_IncrRefCount(cmd);

  p->index = index;
  p->buffer[index] = '\0';
  objv[0] = cmd;
  objv[1] = obj;
  if (Tcl_EvalObjv(p->interp, 2, objv, 0) != TCL_OK)
    {
      res = 0;                  /* SHOULD SAVE ERROR!! */
      goto cleanup;
    }
  s = Tcl_GetStringResult(p->interp);
  s_len = strlen(s);
  ENSURE_SPACE(s_len);
  memcpy(p->buffer+index, s, s_len+1);
  p->index += s_len;
  /* ASSERT(p->buffer[p->index]) == '\0') */
 cleanup:
  if (cmd) Tcl_DecrRefCount(cmd);
  if (obj) Tcl_DecrRefCount(obj);
  return res;
}

int trans_command_list(t1, stream, p)
     SP_term_ref t1;
     SP_stream *stream;
     struct event_stream_data *p;
{
  int index = p->index;
      
  if (!trans_command(t1, stream, p))
    {
      return 0;
    }
  return trans_command_list1(stream, p, index);
}
#endif

static int write_term( /* (pred, t1, stream, p) */
     SP_pred_ref pred,
     SP_term_ref t1,
     SP_stream *stream,
     struct event_stream_data *p)
{
  SP_term_ref t2 = SP_new_term_ref();

  SP_put_address(t2, (void *)stream);
  if (SP_query_cut_fail(pred, t2, t1) <= 0)
    {
      return 0;
    }
  p->buffer[p->index] = '\0';
  return 1;
}

#if  0
int trans_command_term(t1, stream, p)
     SP_term_ref t1;
     SP_stream *stream;
     struct event_stream_data *p;
{
  int index = p->index;
  
  if (!write_term(local.write_canonical_pred, t1, stream, p))
    {
      return 0;
    }
  return trans_command_list1(stream, p, index);
}
#endif

#endif


int trans_command(t1, stream, p)
     SP_term_ref t1;
     SP_stream *stream;
     struct event_stream_data *p;
{
  SP_term_ref t2 = SP_new_term_ref();
  unsigned long name;
  SP_pred_ref pred;
  int i;
  char *s, c1, c2;

  switch (SP_term_type(t1))
    {
    case SP_TYPE_ATOM:
      SP_get_atom(t1, &name);
      if (name == local.atm_nil)
        {
          p->buffer[p->index] = '\0';
          return 1;
        }
      s = SP_string_from_atom(name);
      goto atomic;

    case SP_TYPE_INTEGER:
    case SP_TYPE_FLOAT:
      SP_get_number_chars(t1, &s);
  atomic:
      i = strlen(s);
      ENSURE_SPACE(i);
      strcpy(p->buffer+p->index, s);
      p->index += i;
      return 1;
      
    case SP_TYPE_COMPOUND:
      SP_get_functor(t1, &name, &i);
      if (name==local.atm_period && i==2)
	for (;;)
	  {
	    SP_get_list(t1, t2, t1);
	    if (!trans_command(t2, stream, p))
	      return 0;
	    if (SP_is_list(t1))
	      {
		p->buffer[p->index++] = ' ';
		continue;
	      }
	    else
	      if (SP_is_atom(t1) && SP_get_atom(t1, &name) && name==local.atm_nil)
		return 1;
	      else
		goto list_error;
	  }
/* Ignore compiler warnings here! */
      else if (((name==local.atm_dq && (c1='"') && (c2='"')) ||
		(name==local.atm_sqb && (c1='[') && (c2=']')) ||
		(name==local.atm_br && (c1='{') && (c2='}')) ||
		(name==local.atm_min && (c1='-') && (c2=' '))) &&
	       i==1)
	{
	  p->buffer[p->index++] = c1;
	  SP_get_arg(1, t1, t2);
	  if (!trans_command(t2, stream, p))
	    return 0;
	  p->buffer[p->index++] = c2;
	  p->buffer[p->index] = '\0';
	  return 1;
	}
      else if (name==local.atm_dot && i==1) {
	SP_get_arg(1, t1, t1);
	if (SP_is_atom(t1))	/* the root path, "." */
	  p->buffer[p->index++] = '.';
	else while (SP_is_list(t1)) { /* .first.second.third ... */
	  SP_get_list(t1, t2, t1);
	  p->buffer[p->index++] = '.';
	  if (!trans_command(t2, stream, p))
	    return 0;
	}
	p->buffer[p->index] = '\0';
	return 1;
      }
      else if (i == 1
               && (    (name==local.atm_write           && ( pred = local.write_pred, 1))
                    || (name==local.atm_writeq          && ( pred = local.writeq_pred, 1))
                    || (name==local.atm_write_canonical && ( pred = local.write_canonical_pred, 1)) ) )
	{
	  SP_get_arg(1, t1, t1);
          return write_term(pred, t1, stream, p);
#if 0 /* [PM] Was */
	  SP_put_address(t2, (void *)stream);
	  SP_get_arg(1, t1, t1);
	  if(SP_query_cut_fail(local.write_pred, t2, t1) <= 0)
	    return 0;
	  p->buffer[p->index] = '\0';
	  return 1;
#endif
	}
      else if (name==local.atm_format && i==2)
	{
	  SP_term_ref t3 = SP_new_term_ref();

	  SP_put_address(t3, (void *)stream);
	  SP_get_arg(2, t1, t2);
	  SP_get_arg(1, t1, t1);
	  if(SP_query_cut_fail(local.format_pred, t3, t1, t2) <= 0)
	    return 0;
	  p->buffer[p->index] = '\0';
	  return 1;
	}
      else if (name==local.atm_chars && i==1)
	{
	  SP_get_arg(1, t1, t1);
	  if (!SP_get_list_chars(t1, &s)) /* [PM] May 2000 used to ignore errors */
            {
              goto list_error;  /* Misleading error message */
            }
	  goto atomic;
	}
#if TCL_ENABLE_LIST
      else if (name==local.atm_list && i==1)
        {
	  SP_get_arg(1, t1, t1);
          return trans_command_list(t1, stream, p);
        }
#endif /* TCL_ENABLE_LIST */
      else
        {
          SAVE_ERROR(DOMAIN_ERROR, "tcl_command_spec", t1, 2);
        }
	    
    case SP_TYPE_VARIABLE:
  instantiation_error:
      SAVE_ERROR(INSTANTIATION_ERROR, "", t1, 2);
    }

list_error:
  if (SP_is_variable(t1))
    goto instantiation_error;
  else
    SAVE_ERROR(DOMAIN_ERROR+DOMAIN_LIST, "", t1, 2);
error:
  return 0;
}


/*   --------------------------------------------------------------  */

int put_event_queue(interp_data, all_flag, tList)
     struct interp_data *interp_data;
     int all_flag;
     SP_term_ref tList;
{
  SP_term_ref
    tTerm = SP_new_term_ref(),
    tStream = SP_new_term_ref();
  SP_stream *stream;
  struct event_q *p = interp_data->event_list;
  int res;
  char *errmsg, errbuf[BUFLEN];

  if (p == NULL)
    return 0;			/* tList == atm_nil initially */
  do
    {
      interp_data->event_list = p->next;

      stream = get_tcl_stream(interp_data, p->event_string);
      SP_free(p);
      if (!stream)
	{
	  strcpy(errbuf, "Could not find stream");
	  goto error2;
	}
      SP_put_address(tStream, (void *)stream);
      SP_put_variable(tTerm);
      res = SP_query(local.read_pred, tStream, tTerm); /* Read string from Prolog */

      switch (res)
	{
	case SP_ERROR:
	  errmsg = "exception";
	  goto error1;
	case SP_FAILURE:
	  errmsg = "unexpected failure";
	  goto error1;
	case SP_SUCCESS:
	  if (all_flag)
	    SP_cons_list(tList, tTerm, tList);
	  else
	    SP_put_term(tList, tTerm);
	}
      p = interp_data->event_list;
    }
  while (all_flag && p != NULL);

  return 1;
error1:
  sprintf(errbuf, "%s in Prolog reading of: %s", errmsg, p->event_string);
error2:
  SAVE_ERROR(SPTCL_ERROR, errbuf, tStream, 0);
error:
  return -1;
}
