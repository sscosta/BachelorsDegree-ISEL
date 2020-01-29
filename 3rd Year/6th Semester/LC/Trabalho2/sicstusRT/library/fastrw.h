/* Copyright (C) 1993 Swedish Institute of Computer Science */

/* Definitions and headers for fast term I/O. */

#include <stdarg.h>             /* [PM] April 2000 before stdio.h fixes va_list issue on dec-osf */
#include <stdlib.h>
#include <stdio.h>
#define INIT_BUFSIZE 256

#ifdef SICSTUS

#include <sicstus/sicstus.h>
#include "fastrw_glue.h"

#define Malloc(Size) SP_malloc(Size)
#define Realloc(Ptr,OldSize,NewSize) SP_realloc((char *)(Ptr),NewSize)
#define Free(Ptr,OldSize) SP_free((char *)(Ptr))

#define XP_init_term(T)         (T) = SP_new_term_ref()
#define XP_term SP_term_ref
#define XP_stream SP_stream
#define XP_term_type SP_term_type
#define XP_TYPE_VARIABLE SP_TYPE_VARIABLE
#define XP_TYPE_ATOM SP_TYPE_ATOM
#define XP_TYPE_COMPOUND SP_TYPE_COMPOUND
#define XP_TYPE_INTEGER SP_TYPE_INTEGER
#define XP_TYPE_FLOAT SP_TYPE_FLOAT
#define XP_atom_from_string(STR) SP_atom_from_string(STR)
#define XP_string_from_atom(ATM) SP_string_from_atom(ATM)
#define XP_put_term(DEST,SRC) SP_put_term((DEST),(SRC))
#define XP_put_atom(TERM,ATM) SP_put_atom((TERM),ATM)
#define XP_put_string(TERM,STR) SP_put_string((TERM),STR)
#define XP_put_list(TERM) SP_put_list(TERM)
#define XP_put_functor(TERM,ATM,ARITY) SP_put_functor(TERM,ATM,ARITY)
#define XP_put_integer(TERM,INT) SP_put_integer((TERM),INT)
#define XP_put_integer_chars(TERM,STR) SP_put_number_chars((TERM),STR)
#define XP_put_float_chars(TERM,STR) SP_put_number_chars((TERM),STR)
#define XP_get_atom(TERM,ATM) SP_get_atom(TERM,&(ATM))
#define XP_get_string(TERM,STR) SP_get_string(TERM,&(STR))
#define XP_get_functor(TERM,ATM,ARITY) SP_get_functor(TERM,&(ATM),&(ARITY))
#define XP_get_arg(I,TERM,ARG) SP_get_arg(I,TERM,(ARG))
#define XP_get_integer(TERM,INT) SP_get_integer(TERM,&(INT))
#define XP_get_integer_chars(TERM,STR) SP_get_number_chars(TERM,&(STR))
#define XP_get_float_chars(TERM,STR) SP_get_number_chars(TERM,&(STR))
#define XP_putc(STREAM,CHAR) (STREAM)->sputc(CHAR,(STREAM)->user_handle)
#define XP_getc(STREAM) (STREAM)->sgetc((STREAM)->user_handle)
#define XP_unify(X,Y) SP_unify(X,Y)
#define XP_register_atom(X) (void)SP_register_atom(X)
#define XP_unregister_atom(X) (void)SP_unregister_atom(X)

#endif

#ifdef QUINTUS

#include <quintus/quintus.h>

extern double atof();
static char *QP_realloc();
#define Malloc(Size) QP_malloc(Size)
#define Realloc(Ptr,OldSize,NewSize) QP_realloc((char *)(Ptr),OldSize,NewSize)
#define Free(Ptr,OldSize) QP_free((char *)(Ptr))
#define XP_init_term(T)         (T) = QP_new_term_ref()
#define XP_term QP_term_ref
#define XP_stream QP_stream
#define XP_term_type QP_term_type
#define XP_TYPE_VARIABLE QP_VARIABLE
#define XP_TYPE_ATOM QP_ATOM
#define XP_TYPE_COMPOUND QP_COMPOUND
#define XP_TYPE_INTEGER QP_INTEGER
#define XP_TYPE_FLOAT QP_FLOAT
#define XP_atom_from_string(STR) QP_atom_from_string(STR)
#define XP_string_from_atom(ATM) QP_string_from_atom(ATM)
#define XP_put_term(DEST,SRC) QP_put_term(DEST,SRC)
#define XP_put_atom(TERM,ATM) QP_put_atom(TERM,ATM)
#define XP_put_string(TERM,STR) QP_put_atom(TERM,QP_atom_from_string(STR))
#define XP_put_list(TERM) QP_put_list(TERM)
#define XP_put_functor(TERM,ATM,ARITY) QP_put_functor(TERM,ATM,ARITY)
#define XP_put_integer(TERM,INT) QP_put_integer(TERM,INT)
#define XP_put_integer_chars(TERM,STR) QP_put_integer(TERM,atoi(STR))
#define XP_put_float_chars(TERM,STR) QP_put_float(TERM,atof(STR))
#define XP_get_atom(TERM,ATM) QP_get_atom(TERM,&(ATM))
#define XP_get_string(TERM,STR) QP_get_string(TERM,&(STR))
#define XP_get_functor(TERM,ATM,ARITY) QP_get_functor(TERM,&(ATM),&(ARITY))
#define XP_get_arg(I,TERM,ARG) QP_get_arg(I,TERM,ARG)
#define XP_get_integer(TERM,INT) QP_get_integer(TERM,&(INT))
#define XP_get_integer_chars(TERM,STR) QP_get_integer_chars(TERM,&(STR))
#define XP_get_float_chars(TERM,STR) QP_get_float_chars(TERM,&(STR))
#define XP_putc(STREAM,CHAR) QP_fputc(CHAR,STREAM)
#define XP_getc(STREAM) QP_getc(STREAM)
#define XP_unify(X,Y) QP_unify(X,Y)
#define XP_register_atom(X) (void)QP_register_atom(X)
#define XP_unregister_atom(X) (void)QP_unregister_atom(X)

#endif
