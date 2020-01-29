/* Copyright(C) 1994-95, Swedish Institute of Computer Science */

#if !__NeXT__

#if _AIX
#define _BSD 1
#endif

#ifdef __WATCOMC__
#ifdef __NT__
#define SP_WIN32 1
#endif
#ifdef __OS2__
#define SP_OS2 1
#endif
#endif

#ifdef __BORLANDC__
#define SP_WIN32 1
#endif

#ifdef _MSC_VER
#define SP_WIN32 1
#endif

#ifdef __WIN32__
#define SP_WIN32 1
#endif

#if SP_WIN32
#include <windows.h>
#endif

#if 0 /*_MSC_VER || __GNUC__ && __WIN32__ */
#define Tcl_DoOneEvent _Tcl_DoOneEvent
#define Tcl_SetVar _Tcl_SetVar
#define Tk_Init _Tk_Init
#define Tk_MainWindow _Tk_MainWindow
#define Tk_DestroyWindow _Tk_DestroyWindow
#define Tk_MakeWindowExist _Tk_MakeWindowExist
#define Tk_GetNumMainWindows _Tk_GetNumMainWindows
#define TkWinGetAppInstance _TkWinGetAppInstance
#define TkWinXInit _TkWinXInit
#endif

#include <stdarg.h>             /* [PM] April 2000 before stdio.h fixes va_list issue on dec-osf */
#include <stdio.h>
#include <string.h>
#include <tk.h>
#include <sys/types.h>
#if !SP_WIN32
#include <sys/time.h>
#include <sys/file.h>
#endif
#include <sicstus/sicstus.h>
#include "tcl.h"

#define TK_NUMVERSION (TK_MAJOR_VERSION*10+TK_MINOR_VERSION)

#if SP_WIN32 && TK_NUMVERSION <= 42
extern HINSTANCE 	TkWinGetAppInstance(void);
extern void 		TkWinXInit(HINSTANCE hInstance);
#endif

#if __WATCOMC__
#pragma aux (__cdecl) Tcl_DoOneEvent
#pragma aux (__cdecl) Tcl_SetVar
#pragma aux (__cdecl) Tk_Init
#pragma aux (__cdecl) Tk_MainWindow
#pragma aux (__cdecl) Tk_DestroyWindow
#pragma aux (__cdecl) Tk_MakeWindowExist
#pragma aux (__cdecl) Tk_GetNumMainWindows
#pragma aux (__cdecl) TkWinGetAppInstance
#pragma aux (__cdecl) TkWinXInit
#endif

#if  TK_NUMVERSION >= 41
#define tk_NumMainWindows Tk_GetNumMainWindows()
#define Tk_DoOneEvent Tcl_DoOneEvent
#endif

#if !SP_WIN32
static int SPCDECL tk_event_hook PROTOTYPE((int fd));
static void dummy PROTOTYPE((ClientData clientData, int mask));
#endif

/*   --------------------------------------------------------------  */

void tk_new(tInterp, app_name, screen_name, flags, tkversion)
     SP_term_ref tInterp;
     char *app_name, *screen_name;
     long flags;
     long tkversion;
{
  Tcl_Interp *interp;

  if ((tkversion == 40 && 40 < TK_NUMVERSION) ||
      (tkversion == 41 && 40 >= TK_NUMVERSION))
    SAVE_ERROR(SPTK_ERROR, "Wrong Tk version", tInterp, 0);
    
  GetInterp(tInterp, interp, 1);

  if (screen_name[0] == '\0')
#if SP_WIN32
/* Necessary ? */
    screen_name = "localhost:0";
#else
    screen_name = NULL;
#endif

#if TK_NUMVERSION < 41
  if (Tk_CreateMainWindow(interp, screen_name, app_name, "Tk") == NULL)
    SAVE_ERROR(SPTK_ERROR, interp->result, tInterp, 0);
#endif

  if (Tcl_AppInit(interp) == TCL_ERROR)
    SAVE_ERROR(SPTK_ERROR, Tcl_GetStringResult(interp), tInterp, 0);

#if TK_NUMVERSION >= 41
  {
    int tcl_argc = 1;
    char tcl_argv[256], *tap = tcl_argv;
    char buf[8];

    tcl_argv[0] = '\0';

    if (screen_name)
      {
	tcl_argc += 2;
	sprintf(tap, "-display %s", screen_name);
	tap += strlen(tap);
      }
    if (app_name[0] != '\0')
      {
	tcl_argc += 2;
	sprintf(tap, " -name \"%s\"", app_name);
      }
    sprintf(buf, "%d", tcl_argc);
    Tcl_SetVar(interp, "argc", buf, TCL_GLOBAL_ONLY);
    Tcl_SetVar(interp, "argv", tcl_argv, TCL_GLOBAL_ONLY);
  }
#endif

  if (Tk_Init(interp) == TCL_ERROR)
    SAVE_ERROR(SPTK_ERROR, Tcl_GetStringResult(interp), tInterp, 0);

#if !SP_WIN32
  if (flags & 1)
    {
      /* [PM] 3.9b5 the first arg to Tcl_CreateFileHandler is a file
         descriptor (STDIN_FILENO, i.e., zero). This means that
         Tcl_DoOneEvent will only return if there is a Tcl/Tk-event or
         if some input appears on STDIN_FILENO. However, STDIN_FILENO
         may be unrelated to the file descriptor passed to
         tk_event_hook!

         To summarize: If SICStus reads from a modeTTY stream that is
         not stdin then the Tcl/Tk read-hook will block until there is
         some input on STDIN_FILENO. This is unrelated to the fact
         that SICStus read hooks cannot work reliably (due to
         buffering within the stdio FILE structure).

       */

#if TK_NUMVERSION < 41  
      Tk_CreateFileHandler(0, TK_READABLE, dummy, (ClientData) 0);
#else
#if TK_NUMVERSION <= 42
      Tcl_CreateFileHandler(Tcl_GetFile((ClientData)0,TCL_UNIX_FD),
			    TCL_READABLE, dummy, (ClientData) 0);
#else
      Tcl_CreateFileHandler(0, TCL_READABLE, dummy, (ClientData) 0);
#endif
#endif
      SP_set_read_hook(tk_event_hook);
    }
#endif
  local.tk_inited = 1;
  return;
  RAISE_ERROR("tk_new", 2);
}

/*   --------------------------------------------------------------  */

void tk_main_window(tInterp, tWindow)
     SP_term_ref tInterp;
     SP_term_ref tWindow;
{
  Tcl_Interp *interp;
  Tk_Window window;

  GetInterp(tInterp, interp, 1);

  if (!local.tk_inited)               /* [PM] May 2000 Workaround Tcl Stub vector not being initialized in Tk (PRM 1493) */
    {
      SAVE_ERROR(SPTK_ERROR, "Not a Tk enabled Tcl interpreter", tInterp, 0);
    }

  window = Tk_MainWindow(interp);
  ptr_to_wrapper(local.atm_window, window, tWindow);
  return;
  RAISE_ERROR("tk_main_window", 2);
}

/*   --------------------------------------------------------------  */

void tk_destroy_window(tWindow)
     SP_term_ref tWindow;
{
  Tk_Window window = (Tk_Window)wrapper_to_ptr(local.atm_window,tWindow);

  if (window == NULL || !local.tk_inited) /* [PM] !tk_inited for PRM 1493 */
    SAVE_ERROR(DOMAIN_ERROR, "tk_window", tWindow, 1);

  Tk_DestroyWindow(window);
  return;
  RAISE_ERROR("tk_destroy_window", 1);
}

/*   --------------------------------------------------------------  */

void tk_make_window_exist(tWindow)
     SP_term_ref tWindow;
{
  Tk_Window window = (Tk_Window)wrapper_to_ptr(local.atm_window,tWindow);;

  if (window == NULL || !local.tk_inited) /* [PM] !tk_inited for PRM 1493 */
    SAVE_ERROR(DOMAIN_ERROR, "tk_window", tWindow, 1);

  Tk_MakeWindowExist(window);
  return;
  RAISE_ERROR("tk_make_window_exist", 1);
}

/*   --------------------------------------------------------------  */

long tk_do_one_event1(flags)
     long flags;
{
  return Tk_DoOneEvent(flags);  /* a.k.a  Tcl_DoOneEvent */
}

void tk_do_one_event3(tInterp, flags, tEvent)
     SP_term_ref tInterp, tEvent;
     long flags;
{
  struct interp_data *interp_data =
    (struct interp_data *)wrapper_to_ptr(local.atm_interp, tInterp);

  if (interp_data == NULL)
    SAVE_ERROR(DOMAIN_ERROR, "tcl_interpreter", tInterp, 1);

  switch (put_event_queue(interp_data, FALSE, tEvent))
    {
    case 1:
      return;
    case 0:
      Tk_DoOneEvent(flags);
      if (put_event_queue(interp_data, FALSE, tEvent) >= 0)
	return;
    case -1:
      RAISE_ERROR("tk_do_one_event", 3);
    }
}

/*   --------------------------------------------------------------  */

#if !SP_WIN32

static int SPCDECL tk_event_hook(fd)
     int fd;
{
  fd_set dread;
  struct timeval to;

  if (tk_NumMainWindows > 0)
    {
      (void) Tk_DoOneEvent(0);
      FD_ZERO(&dread);
      FD_SET(fd, &dread);
      to.tv_sec = 0;
      to.tv_usec = 0;
      if (select(fd+1, &dread, (fd_set *)0, (fd_set *)0, &to) < 0)
	return -1;
      if (FD_ISSET(fd, &dread))
	return 1;
      else
	return 0;
    }
  else
    return 1;
}

/* This one is installed as a handler for stdin only to ensure there
   is an event generated when there is input ready.
 */
static void dummy(clientData, mask)
     ClientData clientData;
     int mask;
{
  (void)clientData;
  (void)mask;
}

#endif

/*   --------------------------------------------------------------  */

void tk_num_main_windows(tNum)
     SP_term_ref tNum;
{
  if (!local.tk_inited) /* [PM] !tk_inited for PRM 1493 */
    {
      SP_put_integer(tNum, 0);
    }
  else
    {
      SP_put_integer(tNum, tk_NumMainWindows);
    }
}

/*   --------------------------------------------------------------  */

void SPCDECL tk_initialize(when)
     int when;
{
  local.tk_inited = 0;

  tcl_initialize(when);
  (void)SP_register_atom(local.atm_window = SP_atom_from_string("$TkWindow"));
  /* Necessary to initialize the X-emulation layer */
#if SP_WIN32 && TK_NUMVERSION <= 42
  TkWinXInit(TkWinGetAppInstance());
#endif
}

void SPCDECL tk_deinitialize(when)
     int when;
{
#if !SP_WIN32
  SP_ReadHookProc *readhook = SP_set_read_hook(NULL);

  if (readhook != NULL && readhook != tk_event_hook)
    SP_set_read_hook(readhook);
#endif
  tcl_deinitialize(when);
  (void)SP_unregister_atom(local.atm_window);
}

#else
#include <sicstus/sicstus.h>

/* Dummie defs to allow uniform handling when only Tcl is available. */

void tk_new(tInterp, app_name, screen_name, flags, tkversion)
     SP_term_ref tInterp;
     char *app_name, *screen_name;
     long flags;
     long tkversion;
{}

void tk_main_window(tInterp, tWindow)
     SP_term_ref tInterp;
     SP_term_ref tWindow;
{}

void tk_destroy_window(tWindow)
     SP_term_ref tWindow;
{}

void tk_make_window_exist(tWindow)
     SP_term_ref tWindow;
{}

long tk_do_one_event1(flags)
     long flags;
{}

void tk_do_one_event3(tInterp, flags, tEvent)
     SP_term_ref tInterp, tEvent;
     long flags;
{}

void tk_num_main_windows(tNum)
     SP_term_ref tNum;
{}

void SPCDECL tk_initialize(when)
     int when;
{
  local.tk_inited = 0;
  tcl_initialize(when);
}

#endif /* !__NeXT__ */
