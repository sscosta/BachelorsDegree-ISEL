/* Copyright(C) 1996, Swedish Institute of Computer Science */

#if !__NeXT__

#if _AIX
#define _BSD 1
#endif

#if 0 /* _MSC_VER || __GNUC__ && __WIN32__ */
#define Tcl_DoOneEvent _Tcl_DoOneEvent
#define Tcl_Eval _Tcl_Eval
#define Tcl_GetCommandInfo _Tcl_GetCommandInfo
#define Tcl_CreateCommand _Tcl_CreateCommand
#define Tcl_SetVar _Tcl_SetVar
#endif

#include <stdarg.h>             /* [PM] April 2000 before stdio.h fixes va_list issue on dec-osf */
#include <string.h>
#include <tcl.h>
#include <tk.h>
#include <sicstus/sicstus.h>
#include "tcl.h"

#define TK_NUMVERSION (TK_MAJOR_VERSION*10+TK_MINOR_VERSION)

#if __WATCOMC__
#pragma aux (__cdecl) Tcl_DoOneEvent
#pragma aux (__cdecl) Tcl_Eval
#pragma aux (__cdecl) Tcl_GetCommandInfo
#pragma aux (__cdecl) Tcl_CreateCommand
#pragma aux (__cdecl) Tcl_SetVar
#endif

#if  TK_NUMVERSION >= 41
#define Tk_DoOneEvent Tcl_DoOneEvent
#define TK_DONT_WAIT TCL_DONT_WAIT
#define TK_ALL_EVENTS TCL_ALL_EVENTS
#endif


struct sptkcon {
  Tcl_Interp *interp;
  char *text_widget;
  int ready;
  int cnt;
  int index;
  int size;
  char *buf;
  int eof;
  char *tcloutfun;
};

#define SPTKCONBUFSIZ 1024
#define EOL 10

static char *tcloutout = "sptcout";
static char *tclouterr = "sptcerr";

static int SPCDECL sptkcon_flush PROTOTYPE((void *raw_handle));

static int SPCDECL sptkcon_getc(void *handle)
{
  struct sptkcon *con = (struct sptkcon *)handle;
  
  if (!con->cnt)
    {
      sptkcon_flush(local.sptkcon_errh);
      while(Tk_DoOneEvent(TK_ALL_EVENTS|TK_DONT_WAIT));
      sptkcon_flush(local.sptkcon_outh);
      while (!con->ready)
	Tk_DoOneEvent(0);
      con->ready = 0;
      if (Tcl_Eval(con->interp, "sptcin") != TCL_OK)
	{
	  con->eof = 1;
	  return -1;
	}
#if DBG
      SP_fprintf(local.old_err, ">>%s<<\n", Tcl_GetStringResult(con->interp));
#endif
      strcpy(con->buf, Tcl_GetStringResult(con->interp));
      con->cnt = strlen(con->buf);
      if (!con->cnt)
	{
	  con->eof = 1;
	  return -1;
	}
      con->index = 0;
    }
  con->cnt--;
  return con->buf[con->index++];
}

static int SPCDECL sptkcon_putc(char c, void *handle)
{
  struct sptkcon *con = (struct sptkcon *)handle;
  if (con->index == con->size)
    {
      con->size *= 2;
      con->buf = (char *)SP_realloc(con->buf, con->size);
    }
  con->buf[con->index++] = c;
  if (c == EOL)
    {
      con->buf[con->index++] = '\0';      
      Tcl_SetVar(con->interp, "sptc_out", con->buf, TCL_GLOBAL_ONLY);
      if (Tcl_Eval(con->interp, con->tcloutfun) != TCL_OK)
	{
	  con->eof = 1;
	  return -1;
	}
      while(Tk_DoOneEvent(TK_ALL_EVENTS|TK_DONT_WAIT));
      con->index = 0;
    }
  return c;
}

/* For unbuffered stderr (slow!)
static int SPCDECL sptkcon_errputc(c, handle)
     int c;
     struct sptkcon *handle;
{
  handle->buf[0] = c;
  Tcl_SetVar(handle->interp, "sptc_out", handle->buf, TCL_GLOBAL_ONLY);
  if (Tcl_Eval(handle->interp, "sptcerr") != TCL_OK)
    {
      handle->eof = 1;
      return -1;
    }
  if (c == EOL)
    while(Tk_DoOneEvent(TK_ALL_EVENTS|TK_DONT_WAIT));
  return c;
}
*/

static int SPCDECL sptkcon_flush(void *handle)
{
  struct sptkcon *con = (struct sptkcon *)handle;
  if (con->index > 0)
    {
      con->buf[con->index++] = '\0';      
      Tcl_SetVar(con->interp, "sptc_out", con->buf, TCL_GLOBAL_ONLY);
      if (Tcl_Eval(con->interp, con->tcloutfun) != TCL_OK)
	{
	  con->eof = 1;
	  return -1;
	}
      while(Tk_DoOneEvent(TK_ALL_EVENTS|TK_DONT_WAIT));
      con->index = 0; 
    }
  return 0;
}

static int SPCDECL sptkcon_eof(void *handle)
{
  struct sptkcon *con = (struct sptkcon *)handle;

  return con->eof;
}

static void SPCDECL sptkcon_clrerr(void *handle)
{
  struct sptkcon *con = (struct sptkcon *)handle;

  con->eof = 0;
}

static int SPCDECL sptkcon_close(void *handle)
{
  struct sptkcon *con = (struct sptkcon *)handle;

  SP_free(con->buf);
  SP_free(con);
  return 0;
}

static int SPCDECL sptkcon_set_ready( /* (clientData, interp, argc, argv) */
    ClientData clientData,
    Tcl_Interp *interp,
    int argc,
    char **argv)
{
  struct sptkcon *handle = (struct sptkcon *)clientData;

  (void)interp;                 /* avoid -Wunused */
  (void)argc;
  (void)argv;

  handle->ready = 1;
  return TCL_OK;
}

static int SPCDECL sptkcon_interrupt( /* (clientData, interp, argc, argv) */
    ClientData clientData,
    Tcl_Interp *interp,
    int argc,
    char **argv)
{

  (void)clientData;             /* avoid -Wunused */
  (void)interp;
  (void)argc;
  (void)argv;

  SP_ctrlc_action();
  return TCL_OK;
}

static void SPCDECL tkcon_events(void)
{
  while(Tk_DoOneEvent(TK_ALL_EVENTS|TK_DONT_WAIT));
}

void tk_term(tInterp, tktermsrc, text_widget, in_stream, out_stream, err_stream)
     SP_term_ref tInterp;
     char *tktermsrc;
     char *text_widget;
     SP_stream **in_stream, **out_stream, **err_stream;
{
  struct interp_data *interp_data =
    (struct interp_data *)wrapper_to_ptr(local.atm_interp, tInterp);
  Tcl_Interp *interp;
  Tcl_CmdInfo cmd_info;
  struct sptkcon *inh, *outh, *errh;
  char buf[1024];

  strcpy(buf,"source ");
#if 1
  /* [PM] No buffer overrun */
  strncat(buf,tktermsrc,(sizeof buf)-strlen(buf));
  buf[(sizeof buf)-1] = '\0';
#else
  strncat(buf,tktermsrc,1023);
#endif

  CHECK_INTERP(interp_data, interp, tInterp, 1);

  if (Tcl_GetCommandInfo(interp, "sptc_set_ready", &cmd_info) != 0)
    SAVE_ERROR(SPTCL_ERROR, "Previous tk-term exists", tInterp, 0);
  if (Tcl_Eval(interp, buf) != TCL_OK)
    SAVE_ERROR(SPTCL_ERROR, "Can't source tkterm.tcl", tInterp, 0);
  {
    char buf[256];
    sprintf(buf, "sptc_start %s", text_widget);
    if (Tcl_Eval(interp, buf) != TCL_OK)
      SAVE_ERROR(SPTCL_ERROR, "Can't initiate tk-term", tInterp, 0);
  }

  SP_set_interrupt_hook((SP_VoidFun *)tkcon_events);

  inh  = (struct sptkcon *)SP_malloc(sizeof(struct sptkcon));
  outh  = (struct sptkcon *)SP_malloc(sizeof(struct sptkcon));
  errh  = (struct sptkcon *)SP_malloc(sizeof(struct sptkcon));

  Tcl_CreateCommand(interp, "sptc_set_ready",
		    (Tcl_CmdProc *)sptkcon_set_ready,
		    (ClientData)inh, (Tcl_CmdDeleteProc *)NULL);
  Tcl_CreateCommand(interp, "sptc_interrupt",
		    (Tcl_CmdProc *)sptkcon_interrupt,
		    (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL);

  inh->interp = outh->interp = errh->interp = interp;
  inh->text_widget = outh->text_widget = errh->text_widget = text_widget;
  inh->cnt = outh->cnt = errh->cnt = 0;
  inh->index = outh->index = errh->index = 0;
  inh->ready = 0;
  inh->size = outh->size = errh->size = SPTKCONBUFSIZ;
  inh->eof = outh->eof = errh->eof = 0;
  inh->buf = SP_malloc(SPTKCONBUFSIZ);
  outh->buf = SP_malloc(SPTKCONBUFSIZ);
  errh->buf = SP_malloc(SPTKCONBUFSIZ);

  outh->tcloutfun = tcloutout;
  errh->tcloutfun = tclouterr;
  local.sptkcon_outh = outh;
  local.sptkcon_errh = errh;

#if DBG
  local.old_err = SP_stderr;
#endif
  SP_make_stream_context(inh, sptkcon_getc, NULL, NULL,
		 sptkcon_eof, sptkcon_clrerr, sptkcon_close, in_stream,
		 SP_WCX_FLAG, SP_STREAMHOOK_LIB);
  SP_make_stream_context(outh, NULL, sptkcon_putc, sptkcon_flush,
		 NULL, NULL, sptkcon_close, out_stream, 
		 SP_WCX_FLAG, SP_STREAMHOOK_LIB);
  SP_make_stream_context(errh, NULL, sptkcon_putc, sptkcon_flush,
		 NULL, NULL, sptkcon_close, err_stream, 
		 SP_WCX_FLAG, SP_STREAMHOOK_LIB);

  SP_set_tty(*in_stream);
  SP_set_tty(*out_stream);
  SP_set_tty(*err_stream);

  return;

  RAISE_ERROR("tk_terminal", 5);
}

#else

#include <sicstus/sicstus.h>

/* Dummie defs to allow uniform handling when only Tcl is available. */
void tk_term(tInterp, tktermsrc,text_widget, in_stream, out_stream, err_stream)
     SP_term_ref tInterp;
     char *tktermsrc;
     char *text_widget;
     SP_stream **in_stream, **out_stream, **err_stream;
{}

#endif
