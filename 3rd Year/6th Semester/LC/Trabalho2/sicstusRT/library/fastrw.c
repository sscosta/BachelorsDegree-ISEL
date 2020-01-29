/* Copyright (C) 1993 Swedish Institute of Computer Science */

/* Fast term I/O, Quintus/SICStus portable. */

#include <string.h>
#include "fastrw.h"

#define Version 'C'
#define Pref_Int 'I'
#define Pref_Float 'F'
#define Pref_Atom 'A'
#define Pref_Compound 'S'
#define Pref_Variable '_'
#define Pref_List '['
#define Pref_Nil ']'
#define Pref_Ascii_List '"'

struct frw_buffer {
  char *chars;
  int index;
  int size;
};

struct fastrw_state {
  char *frw_buf;
  int frw_buf_size;
  unsigned long frw_nil;
  unsigned long frw_var;
  unsigned long frw_period;
  int var_count;
#if 1 /* was !QUINTUS && !SICSTUS */
  struct frw_buffer write_buffer;
  struct frw_buffer read_buffer;
#endif
}; 

#if MULTI_SP_AWARE

/* [PM] 3.9b4 ensures local.foo works. Also avoids need for SP_CONTEXT_SWITCH_HOOK. */
#define local (*(struct fastrw_state *)*SP_foreign_stash())

#else  /* !MULTI_SP_AWARE */

static struct fastrw_state local;

#endif /* !MULTI_SP_AWARE */


/* frw_put_string(string, stream)
   writes 'string' onto 'stream' (which defaults to local.write_buffer)
*/
static void frw_put_string(SPAPI_ARG_PROTO_DECL
			   char *string, XP_stream *stream)
{
  char c;

  if (stream)
    {
      do
	{
	  c = *string++;
	  XP_putc(stream,c);
	}
      while (c);
    }
  else
    {
      int index = local.write_buffer.index;
      int l = strlen(string)+1;
      
      while (index+l > local.write_buffer.size)
	local.write_buffer.chars = (char *)Realloc(local.write_buffer.chars,
					     local.write_buffer.size,
					     local.write_buffer.size<<1),
	local.write_buffer.size <<= 1;
      strcpy(&local.write_buffer.chars[index], string);
      local.write_buffer.index += l;
    }
}

/* frw_get_string(string, stream)
   copies a string to 'string' from 'stream' (which defaults to local.read_buffer)
   string == local.frw_buf
*/
static void frw_get_string(SPAPI_ARG_PROTO_DECL 
			   char *string,
			   XP_stream *stream)
{
  int c;
  
  if (stream)
    {
      do
	{
	  char *frw_buf_end = local.frw_buf+local.frw_buf_size;
	  
	  do
	    {
	      c = XP_getc(stream);
	      *string++ = c;
	    }    
	  while (c && string<frw_buf_end);
	  if (c)
	    local.frw_buf = (char *)Realloc(local.frw_buf,local.frw_buf_size,local.frw_buf_size<<1),
	    string = local.frw_buf + local.frw_buf_size,
	    local.frw_buf_size <<= 1;
	}
      while (c);
    }
  else
    {
      char *src = &local.read_buffer.chars[local.read_buffer.index];
      int l = strlen(src)+1;
      
      while (string+l > local.frw_buf+local.frw_buf_size)
	string = local.frw_buf = (char *)Realloc(local.frw_buf,local.frw_buf_size,local.frw_buf_size<<1),
	local.frw_buf_size <<= 1;
      strcpy(string,src);
      local.read_buffer.index += l;
    }
}

/* frw_put_char(c, stream)
   writes 'c' onto 'stream' (which defaults to local.write_buffer)
*/
static void frw_put_char(SPAPI_ARG_PROTO_DECL 
			 int c,
			 XP_stream *stream)
{
  if (stream)
    XP_putc(stream,(char)c);
  else
    {
      int index = local.write_buffer.index;
      
      if (index+1 > local.write_buffer.size)
	local.write_buffer.chars = (char *)Realloc(local.write_buffer.chars,
					     local.write_buffer.size,
					     local.write_buffer.size<<1),
	local.write_buffer.size <<= 1;
      *(unsigned char *)(local.write_buffer.chars+local.write_buffer.index++) = c;
    }
}

/* frw_get_char(stream)
   returns a character read from 'stream' (which defaults to local.read_buffer)
*/
static int frw_get_char(SPAPI_ARG_PROTO_DECL 
			XP_stream *stream)
{
  if (stream)
    return XP_getc(stream);
  else
    return *(unsigned char *)(local.read_buffer.chars+local.read_buffer.index++);
}



#ifdef QUINTUS

static void QP_get_string(t, s)
     QP_term_ref t;
     char **s;
{
  unsigned long qp_atom;
  
  QP_get_atom(t, &qp_atom);
  *s = QP_string_from_atom(qp_atom);
}

static void QP_get_integer_chars(t, s)
     QP_term_ref t;
     char **s;
{
  long l;
  
  QP_get_integer(t, &l);
  sprintf(*s=local.frw_buf, "%d", l);
}

static void QP_get_float_chars(t, s)
     QP_term_ref t;
     char **s;
{
  double d;
  
  QP_get_float(t, &d);
  sprintf(*s=local.frw_buf, "%.17g", d);
}

static char *QP_realloc(oldptr,oldsize,newsize)
     char *oldptr;
     unsigned oldsize, newsize;
{
  char *newptr = (char *)QP_malloc(newsize);
  register char *p = oldptr;
  register char *q = newptr;
  register char *plim = (oldsize<newsize ? p+oldsize : p+newsize);

  while (p<plim) *q++ = *p++;
  QP_free(oldptr);
  return newptr;
}

#endif

static void frw_read_term(SPAPI_ARG_PROTO_DECL 
			  XP_stream *stream,
			  XP_term term, XP_term *mapp)
{
  XP_term t1;

  XP_init_term(t1);
 start:
  switch (frw_get_char(SPAPI_ARG stream))
    {
    case Pref_Variable:
      {				/* variable */
	XP_term t2, link;

	XP_init_term(t2);
	XP_init_term(link);
	frw_get_string(SPAPI_ARG local.frw_buf, stream);
	XP_put_functor(link, XP_atom_from_string("-"), 2);
	XP_get_arg(1, link, t1);
	XP_put_integer_chars(t2, local.frw_buf);
	XP_unify(t1, t2);
	XP_get_arg(2, link, t2);
	XP_unify(term, t2);
	XP_put_list(t2);
	XP_get_arg(1, t2, t1);
	XP_unify(t1, link);
	XP_unify(*mapp, t2);
	XP_get_arg(2, t2, *mapp);
      }
      break;

    case Pref_Nil:		/* the atom [] */
      XP_put_atom(t1,local.frw_nil);
      XP_unify(term, t1);
      break;

    case Pref_Atom:		/* some other atom */
      frw_get_string(SPAPI_ARG local.frw_buf, stream);
      XP_put_string(t1, local.frw_buf);
      XP_unify(term, t1);
      break;

    case Pref_Ascii_List:
      {				/* list of character codes */
	XP_term t2;
	int c;

	XP_init_term(t2);
	while ((c=frw_get_char(SPAPI_ARG stream)))
	  {
	    XP_put_list(t1);
	    XP_unify(term, t1);
	    XP_get_arg(1, term, t2);
	    XP_put_integer(t1, c);
	    XP_unify(t1, t2);
	    XP_get_arg(2, term, term);
	  }
	goto start;
      }

    case Pref_List:
      XP_put_list(t1);
      XP_unify(term, t1);
      XP_get_arg(1, term, t1);
      frw_read_term(SPAPI_ARG stream, t1, mapp);
      XP_get_arg(2, term, term);
      goto start;

    case Pref_Compound:
      {				/* some other compound term */
	register int i;
	int arity;

	frw_get_string(SPAPI_ARG local.frw_buf, stream);
	arity = frw_get_char(SPAPI_ARG stream);
	XP_put_functor(t1, XP_atom_from_string(local.frw_buf), arity);
	XP_unify(term, t1);
	for (i=1; i<arity; i++)
	  {
	    XP_get_arg(i, term, t1);
	    frw_read_term(SPAPI_ARG stream, t1, mapp);
	  }
	XP_get_arg(i, term, term);
	goto start;
      }

    case Pref_Int:		/* integer */
      frw_get_string(SPAPI_ARG local.frw_buf, stream);
      XP_put_integer_chars(t1, local.frw_buf);
      XP_unify(term, t1);
      break;

    case Pref_Float:		/* float */
      frw_get_string(SPAPI_ARG local.frw_buf, stream);
      XP_put_float_chars(t1, local.frw_buf);
      XP_unify(term, t1);
      break;
    }
}
  

static void frw_write_term(SPAPI_ARG_PROTO_DECL 
			   XP_term term,
			   XP_stream *stream)
{
  char *s;
  unsigned long atm;
  int arity;
  int in_ascii_list;
  
 start:
  in_ascii_list = 0;
 start1:
  switch (XP_term_type(term))
    {
    case XP_TYPE_VARIABLE:
      {				/* variable, 1st occurrence */
	XP_term myterm;

	XP_init_term(myterm);
	XP_put_functor(myterm,local.frw_var, 1);
	XP_unify(term, myterm);
	XP_get_arg(1, myterm, term);
	XP_put_integer(myterm, local.var_count++);
	XP_unify(term, myterm);
	XP_get_integer_chars(myterm, s);
	goto write_var;
      }
      
    case XP_TYPE_ATOM:
      if (in_ascii_list) frw_put_char(SPAPI_ARG 0, stream);
      {
	XP_get_atom(term, atm);
	if (atm==local.frw_nil)	/* the atom [] */
	  frw_put_char(SPAPI_ARG Pref_Nil, stream);
	else
	  {			/* some other atom */
	    frw_put_char(SPAPI_ARG Pref_Atom, stream);
	    XP_get_string(term, s);
	    frw_put_string(SPAPI_ARG s, stream);
	  }
      }
      break;
      
    case XP_TYPE_COMPOUND:
      XP_get_functor(term, atm, arity);
      if (arity==1 && atm==local.frw_var)
	{			/* variable, 2nd occurrence */
	  XP_term arg;
	  
	  XP_init_term(arg);
	  XP_get_arg(1, term, arg);
	  XP_get_integer_chars(arg, s);
	write_var:
	  if (in_ascii_list) frw_put_char(SPAPI_ARG 0, stream);
	  frw_put_char(SPAPI_ARG Pref_Variable, stream);
	  frw_put_string(SPAPI_ARG s, stream);
	}
      else if (arity==2 && atm==local.frw_period)
	{
	  XP_term arg;
	  long head;
	  
	  XP_init_term(arg);
	  XP_get_arg(1, term, arg);
	  if (XP_term_type(arg) == XP_TYPE_INTEGER &&
	      XP_get_integer(arg, head) &&
	      head > 0 && head < 256)
	    {			/* list of character codes */
	      if (!in_ascii_list) frw_put_char(SPAPI_ARG Pref_Ascii_List, stream);
	      frw_put_char(SPAPI_ARG head, stream);
	      XP_get_arg(2, term, term);
	      in_ascii_list = 1;
	      goto start1;
	    }
	  else
	    {			/* list of non-characters */
	      if (in_ascii_list) frw_put_char(SPAPI_ARG 0, stream);
	      frw_put_char(SPAPI_ARG Pref_List, stream);
	      frw_write_term(SPAPI_ARG arg, stream);
	      XP_get_arg(2, term, term);
	      goto start;
	    }
	}
      else
	{			/* non-list compound term */
	  register int i;
	  XP_term arg;
	  
	  XP_init_term(arg);
	  if (in_ascii_list) frw_put_char(SPAPI_ARG 0, stream);
	  frw_put_char(SPAPI_ARG Pref_Compound, stream);
	  frw_put_string(SPAPI_ARG XP_string_from_atom(atm), stream);
	  frw_put_char(SPAPI_ARG arity, stream);
	  for (i=1; i<arity; i++)
	    {
	      XP_get_arg(i, term, arg);
	      frw_write_term(SPAPI_ARG arg, stream);
	    }
	  XP_get_arg(i, term, term);
	  goto start;
	}
      break;
      
    case XP_TYPE_INTEGER:	/* integer */
      if (in_ascii_list) frw_put_char(SPAPI_ARG 0, stream);
      frw_put_char(SPAPI_ARG Pref_Int, stream);
      XP_get_integer_chars(term, s);
      frw_put_string(SPAPI_ARG s, stream);
      break;
    
    case XP_TYPE_FLOAT:		/* float */
      if (in_ascii_list) frw_put_char(SPAPI_ARG 0, stream);
      frw_put_char(SPAPI_ARG Pref_Float, stream);
      XP_get_float_chars(term, s);
      frw_put_string(SPAPI_ARG s, stream);
      break;      
    }
}



/* User def. streams.  Although this idea is the cleanest, we don't want to
   take the overhead of opening and closing a stream for every buffered
   read or write operation, so we do it in a somewhat dirtier way.
*/
#if 0 /* was QUINTUS */

struct frw_buffer {
  QP_stream qpinfo;
  char *chars;
  int index;
  int size;
};

static struct frw_buffer write_buffer;
static struct frw_buffer read_buffer;
static char qp_buf[2];

static int qp_write(qpstream, bufptr, sizeptr)
     QP_stream	*qpstream;
     char	**bufptr;
     int	*sizeptr;
{
  register struct frw_buffer *buf = (struct frw_buffer *)qpstream;
  register char *cp = *bufptr;
  register int	n = *sizeptr;

  for (; n>0; --n)
    {
      if (buf->index == buf->size)
	{
	  register char *p = buf->chars;
	  register char *q = (char *)QP_malloc(buf->size <<= 1);
	  char *r = p+buf->index;
	  
	  buf->chars = q;
	  while (p < r) *q++ = *p++;
	  QP_free(p-buf->index);
	}
      buf->chars[buf->index++] = *cp++;
    }
  return QP_SUCCESS;
}

static int qp_read(qpstream, bufptr, sizeptr)
     QP_stream	*qpstream;
     char	**bufptr;
     int	*sizeptr;
{
  struct frw_buffer *buf = (struct frw_buffer *)qpstream;
  char c = **bufptr;
  register int	n = *sizeptr;

  *bufptr = &buf->chars[buf->index++];
  *sizeptr = 1;
  return QP_PART;
}

static int qp_close(qpstream)
     QP_stream *qpstream;
{
  return QP_SUCCESS;
}

QP_stream *plc_open_buf_write()
{
  register struct frw_buffer *handle = &write_buffer;
  QP_stream *option = &handle->qpinfo;

  if (!handle->size)
    {
      handle->chars = (char *)Malloc(INIT_BUFSIZE);
      handle->size = INIT_BUFSIZE;
    }
  handle->index = 0;

  /* get default stream options */
  QU_stream_param("", QP_WRITE, QP_DELIM_LF, option);
  
  option->max_reclen = 0;	/* unbuffered */
  option->write = qp_write;
  option->flush = qp_write;
  option->close = qp_close;
  option->seek_type = QP_SEEK_ERROR;

  /* set Prolog system fields and register the stream */
  QP_prepare_stream(option, qp_buf);
  QP_register_stream(option);
  return (QP_stream *)handle;
}


QP_stream *plc_open_buf_read(source)
     char *source;
{
  register struct frw_buffer *handle = &read_buffer;
  QP_stream *option = &handle->qpinfo;

  handle->chars = source;
  handle->size = -1;
  handle->index = 0;

  /* get default stream options */
  QU_stream_param("", QP_READ, QP_DELIM_LF, option);
  
  option->max_reclen = 0;	/* unbuffered */
  option->read = qp_read;
  option->close = qp_close;
  option->seek_type = QP_SEEK_ERROR;

  /* set Prolog system fields and register the stream */
  QP_prepare_stream(option, qp_buf);
  QP_register_stream(option);
  return (QP_stream *)handle;
}

void plc_buffer_data(qpstream, size, addr)
     QP_stream *qpstream;
     long *size;
     char **addr;
{
  register struct frw_buffer *buf = (struct frw_buffer *)qpstream;
  
  *size = buf->index;
  *addr = buf->chars;
}

#endif


#if 0 /* was SICSTUS */

struct frw_buffer {
  char *chars;
  int index;
  int size;
};

static struct frw_buffer write_buffer = {0,0,0};
static struct frw_buffer read_buffer = {0,0,0};

static int SPCDECL lputc(int c, struct frw_buffer *buf)
{
  if (buf->index == buf->size)
    {
      buf->size <<= 1;
      buf->chars = (char *)Realloc(buf->chars, buf->size);
    }
  return (buf->chars[buf->index++] = c);
}


static int SPCDECL lgetc(struct frw_buffer *buf)
{
  if (buf->index >= buf->size)
    return buf->index++, -1;

  return *(unsigned char *)(buf->chars + buf->index++);
}

static int SPCDECL leof(struct frw_buffer *buf)
{
  return buf->index > buf->size;
}

static int SPCDECL frw_close(struct frw_buffer *buf)
{
  return 0;
}


XP_stream *plc_open_buf_write(void)
{
  XP_stream *s;
  register struct frw_buffer *buf = &write_buffer;

  if (!buf->size)
    {
      buf->chars = (char *)Malloc(INIT_BUFSIZE);
      buf->size = INIT_BUFSIZE;
    }
  buf->index = 0;
  SP_make_stream(buf, NULL, lputc, NULL, NULL, NULL, frw_close, &s);
  
  return s;
}


XP_stream *plc_open_buf_read(char *source)
{
  XP_stream *s;
  register struct frw_buffer *buf = &read_buffer;

  buf->chars = source;
  buf->size = -1;
  buf->index = 0;
  SP_make_stream(buf, lgetc, NULL, NULL, leof, NULL, frw_close, &s);
  
  return s;
}

void plc_buffer_data(s, size, addr)
     XP_stream *s;
     long *size;
     char **addr;
{
  register struct frw_buffer *buf = (struct frw_buffer *)s->user_handle;

  *size = buf->index;
  *addr = buf->chars;
}

#endif


#if 1 /* was !QUINTUS && !SICSTUS */

#if 1
#include "fastrw_glue.h"
#else
extern void SPCDECL frw_init PROTOTYPE((int));
extern void SPCDECL frw_deinit PROTOTYPE((int));
extern XP_stream *plc_open_buf_write PROTOTYPE((void));
extern XP_stream *plc_open_buf_read PROTOTYPE((char *source));
extern void plc_buffer_data PROTOTYPE((XP_stream *s,long *size,char **addr));
extern void plc_fast_read PROTOTYPE((XP_term term,XP_term map,
				     XP_stream *stream));
extern void plc_fast_write PROTOTYPE((XP_term term,XP_stream *stream));
#endif


void SPCDECL frw_init(SPAPI_ARG_PROTO_DECL int when)
{
  (void)when;                   /* [PM] 3.9b5 avoid -Wunused */

#if MULTI_SP_AWARE
  (*SP_foreign_stash()) = (void*)SP_malloc(sizeof(struct fastrw_state));
#endif/* MULTI_SP_AWARE */

  local.var_count = 0;
  XP_register_atom(local.frw_nil = XP_atom_from_string("[]"));
  XP_register_atom(local.frw_var = XP_atom_from_string("$frw_var"));
  XP_register_atom(local.frw_period = XP_atom_from_string("."));
  local.frw_buf = (char *)Malloc(local.frw_buf_size = 512);
  local.write_buffer.chars = NULL;
  local.write_buffer.index = 0;
  local.write_buffer.size = 0;
  local.read_buffer.chars = NULL;
  local.read_buffer.index = 0;
  local.read_buffer.size = 0;
}

void SPCDECL frw_deinit(SPAPI_ARG_PROTO_DECL
			int when)
{
  (void)when;                   /* [PM] 3.9b5 avoid -Wunused */

  XP_unregister_atom(local.frw_nil);
  XP_unregister_atom(local.frw_var);
  XP_unregister_atom(local.frw_period);
  Free(local.frw_buf,local.frw_buf_size);
#if MULTI_SP_AWARE
  SP_free((void*)*SP_foreign_stash());
  (*SP_foreign_stash()) = NULL; /* not needed */
#endif
}


void *SPCDECL plc_open_buf_write(SPAPI_ARG_PROTO_DECL0)
{
  register struct frw_buffer *buf = &local.write_buffer;

  if (!buf->size)
    {
      buf->chars = (char *)Malloc(INIT_BUFSIZE);
      buf->index = 0;
      buf->size = INIT_BUFSIZE;
    }
  buf->index = 0;
  
  return NULL;
}


void *SPCDECL plc_open_buf_read(SPAPI_ARG_PROTO_DECL 
				long lsource_raw)
{
  register struct frw_buffer *buf = &local.read_buffer;

  buf->chars = (char *)lsource_raw;
  buf->size = -1;		/* unused */
  buf->index = 0;
  
  return NULL;
}

void SPCDECL plc_buffer_data(SPAPI_ARG_PROTO_DECL 
			     void *s_raw,
			     long *size,
			     long *laddr)
{
  XP_stream *s = (XP_stream *)s_raw;
  register struct frw_buffer *buf = &local.write_buffer;

  (void)s;                      /* [PM] 3.9b5 avoid -Wunused */

  *size = buf->index;
  *laddr = (long)buf->chars;
}
#endif


/* Main functions. */


void SPCDECL plc_fast_read(SPAPI_ARG_PROTO_DECL 
			   XP_term term, XP_term map, /* +term, passed as unbound var */
			   void *stream_raw)
{
  XP_stream *stream = (XP_stream *)stream_raw;
  XP_term nil;
  
  if (Version != frw_get_char(SPAPI_ARG stream))
    {
      fprintf(stderr, "%s", "! wrong version in c_fast_read/3\n");
      return;
    }
  XP_init_term(nil);
  frw_read_term(SPAPI_ARG stream, term, &map);
  XP_unify(map, nil);
}

void SPCDECL plc_fast_write(SPAPI_ARG_PROTO_DECL 
			    XP_term term,
			    void *stream_raw)

{
  XP_stream *stream = (XP_stream *)stream_raw;

  frw_put_char(SPAPI_ARG Version, stream);
  frw_write_term(SPAPI_ARG term, stream);
}
