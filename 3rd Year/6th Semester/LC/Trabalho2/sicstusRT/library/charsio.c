/* Copyright (C) 1995, Swedish Institute of Computer Science. */

#include <sicstus/sicstus.h>
#include <string.h>

#ifndef NULL
#define NULL 0
#endif


/* These are the functions imported by charsio.pl */
#if 1
#include "charsio_glue.h"
#else
extern void SPCDECL xatom_to_chars PROTOTYPE((char *str,
				      SP_term_ref head,SP_term_ref tail));
extern void SPCDECL xnumber_to_chars PROTOTYPE((SP_term_ref number, 
					SP_term_ref head, SP_term_ref tail));
extern void SPCDECL init_charsio_stream_w PROTOTYPE((SP_stream **streamp));
extern void SPCDECL init_charsio_stream_r PROTOTYPE((char *str,SP_stream **streamp));
extern void SPCDECL chars_to_stream PROTOTYPE((char *str,SP_stream **streamp));
extern void SPCDECL open_buf_stream PROTOTYPE((SP_stream **streamp));
extern void SPCDECL stream_to_chars PROTOTYPE((SP_stream *streamp,
				       SP_term_ref head,SP_term_ref tail));
extern void SPCDECL deinit_charsio PROTOTYPE((int when));
#endif

/*----------------------------------------------------------------------*/
/* Atomic to Chars */

void SPCDECL xatom_to_chars(SPAPI_ARG_PROTO_DECL
                            char *str, SP_term_ref head, SP_term_ref tail)
{
  SP_put_variable(tail);
  SP_put_list_chars(head, tail, str);
}

void SPCDECL xnumber_to_chars(/* (number, head, tail) */
     SPAPI_ARG_PROTO_DECL
     SP_term_ref number, SP_term_ref head, SP_term_ref tail)
{
  char *str;

  SP_get_number_chars(number, &str);
  SP_put_variable(tail);
  SP_put_list_chars(head, tail, str);
}


/*----------------------------------------------------------------------*/
/* Buffer streams */

struct open_chars {
  char *chars;
  int index;
  int len;
  int size;
  #if MULTI_SP_AWARE
  void* (SPCDECL *realloc_fun)(void*, unsigned int); /* [PM] 3.9b4 pointer to SP_realloc */
  void (SPCDECL *free_fun)(void*); /* [PM] 3.9b4 pointer to SP_free */
  #endif/* MULTI_SP_AWARE */
};

#if MULTI_SP_AWARE
#define CHARSIO_REALLOC_FUN(BUF) (*(BUF)->realloc_fun)
#define CHARSIO_FREE_FUN(BUF) (*(BUF)->free_fun)
#else  /* !MULTI_SP_AWARE */
#define CHARSIO_REALLOC_FUN(_BUF) SP_realloc
#define CHARSIO_FREE_FUN(_BUF) SP_free
#endif /* !MULTI_SP_AWARE */

#define INIT_BUFSIZE 512

#if 0                           /* [PM] 3.9b4 the static charsio stream is no longer used */
/* First a special stream which is never closed */
static struct {
  SP_stream *charsio_stream;
} local = {NULL};
#endif /* 0 */

/*----------------------------------------------------------------------*/
/* Read & Write Streams */

static int SPCDECL chputc(char c, void *handle)
{
  struct open_chars *buf = (struct open_chars *)handle;
  if (buf->index == buf->size)
    {
      buf->size *= 2;
      buf->chars = (char *)CHARSIO_REALLOC_FUN(buf)(buf->chars, buf->size);
    }
  return (buf->chars[buf->index++] = c);
}

static int SPCDECL chgetc(void *handle)
{
  struct open_chars *buf = (struct open_chars *)handle;
  if (buf->index >= buf->len)
    return buf->index++, -1;

  return *(unsigned char *)(buf->chars + buf->index++);
}

static int SPCDECL cheof(void *handle)
{
  struct open_chars *buf = (struct open_chars *)handle;
  return buf->index > buf->len;
}

static int SPCDECL chclose(void *handle)
{
  struct open_chars *buf = (struct open_chars *)handle;
  CHARSIO_FREE_FUN(buf)(buf->chars);
  CHARSIO_FREE_FUN(buf)(buf);
#if 0                           /* [PM] 3.9b4 gone */
/* If someone by mistake closes the charsio_stream */
  if (local.charsio_stream &&
      buf == (struct open_chars *)local.charsio_stream->user_handle)
    local.charsio_stream = NULL;
#endif /* 0 */
  return 0;
}

void SPCDECL open_buf_stream(SPAPI_ARG_PROTO_DECL
                             SP_stream **streamp)
{
  struct open_chars *buf;

  buf = (struct open_chars *)SP_malloc(sizeof(struct open_chars));
  #if MULTI_SP_AWARE
  buf->realloc_fun = SP_realloc;
  buf->free_fun = SP_free;
  #endif/* MULTI_SP_AWARE */

  SP_make_stream_context(buf, chgetc, chputc, NULL, cheof, NULL, chclose, streamp, 0, SP_STREAMHOOK_WCI);
  
  buf->chars = (char *)SP_malloc(INIT_BUFSIZE);
  buf->size = INIT_BUFSIZE;
  buf->index = 0;
}

void SPCDECL stream_to_chars(/* (stream, head, tail) */
     SPAPI_ARG_PROTO_DECL
     SP_stream  *stream,
     SP_term_ref head, SP_term_ref tail)
{
  struct open_chars *buf = (struct open_chars *)stream->user_handle;

  SP_put_variable(tail);  
  SP_put_list_chars(head, tail, buf->chars);
}

#if 0                           /* [PM] 3.9b4 gone */
void SPCDECL init_charsio_stream_w(SP_stream **streamp)
{
  if (!local.charsio_stream)
    {
      open_buf_stream(&local.charsio_stream);
    }
  else
    {
      ((struct open_chars *)local.charsio_stream->user_handle)->index = 0;
      local.charsio_stream->last_nl_pos = 
	local.charsio_stream->nl_count =
	local.charsio_stream->char_count = 0;
      local.charsio_stream->peek = -2;
    }
  *streamp = local.charsio_stream;
}

void SPCDECL init_charsio_stream_r(char *str, SP_stream **streamp)
{
  struct open_chars *buf;

  init_charsio_stream_w(streamp);
  buf = (struct open_chars *)(*streamp)->user_handle;
  buf->len = strlen(str);
  if (buf->len >= buf->size)
    {
      buf->size = buf->len + 1;
      buf->chars = (char *)SP_realloc(buf->chars, buf->size);
    }
  strcpy(buf->chars, str);
}
#endif /* 0 */


/*----------------------------------------------------------------------*/
/* Read stream */

void SPCDECL chars_to_stream(SPAPI_ARG_PROTO_DECL char *str, SP_stream **streamp)
{
  struct open_chars *buf;

  buf = (struct open_chars *)SP_malloc(sizeof(struct open_chars));
  #if MULTI_SP_AWARE
  buf->realloc_fun = SP_realloc;
  buf->free_fun = SP_free;
  #endif/* MULTI_SP_AWARE */

  buf->len = strlen(str);
  buf->size = buf->len+1;
  buf->chars = (char *)SP_malloc(buf->size);
  buf->index = 0;
  strcpy(buf->chars, str);
  SP_make_stream_context(buf, chgetc, NULL, NULL, cheof, NULL, chclose, streamp, 0, SP_STREAMHOOK_WCI);
}


/*----------------------------------------------------------------------*/

#if 0                           /* [PM] 3.9 removed since it is a no-op */
void SPCDECL deinit_charsio(int when)
{
#if 0                           /* [PM] 3.9b4 gone */
  if (local.charsio_stream)
    SP_fclose(local.charsio_stream);
  local.charsio_stream = NULL;
#endif /* 0 */
}
#endif /* 0 */
