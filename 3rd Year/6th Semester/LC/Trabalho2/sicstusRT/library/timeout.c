/* Copyright (C) 1995, Swedish Institute of Computer Science. */

#ifndef LEGACY_TIMOUT           /* if true, use the 3.8 SIGVTALRM-based code (on UNIX) */
#define LEGACY_TIMOUT 1
#endif /* LEGACY_TIMOUT */

#include <sicstus/config.h>     /* must be done before anything else (for SP_WIN32) */

/********************************************************************************/

#if !SP_WIN32                   /* in sicstus/config.h */
#if LEGACY_TIMOUT

#include <stdio.h>

#if __hpux__ || hpux || __hpux
#include <time.h>
#else
#include <sys/time.h>
#endif
#include <signal.h>
#include <errno.h>              /* [PM] 3.8.7 should use HAVE_ERRNO_H */

/* done above: #include <sicstus/config.h> */
#include <sicstus/sicstus.h>


#if DBG>1
#define dbg_fprintf(ARGS) {fprintf(stderr, "\n** DBG: "); fprintf ARGS; fprintf(stderr, "\n"); fflush(stderr);}
#else  /* !DBG */
#define dbg_fprintf(_ARGS)
#endif /* !DBG */


/* time_out/3 primitives. Uses exception and callback mechanisms */

/* Exported functions */

#if 1
#include "timeout_glue.h"
#else
extern void to_init PROTOTYPE((int when));
extern void to_deinit PROTOTYPE((int when));
extern long to_start_timer PROTOTYPE((SP_term_ref));
extern SP_term_ref to_stop_timer PROTOTYPE((void));
#endif

#define TimerUnit 1000  /* use to convert between milli and micro seconds */
#define Timer ITIMER_VIRTUAL /* process virtual time */

static int alarm_clock_on PROTOTYPE((long time));
static long alarm_clock_off PROTOTYPE((void));

/* [PM] 3.9b5 PRM 2939 must be static! Otherwise it will share space
   with other variables of the same name when using --static
   --resources=timeout,<OTHER RESOURCE>

   Happened for --resources=timeout,tcltk
*/
static struct {
  SP_atom atom_off;
  int alarm_clock_is_on;
#if DBG
  struct timeval abs_timeout;
#endif /* DBg */
  
} local;

#if 0
static void to_reinit(void)
{
  (void) alarm_clock_off();
  (void)SP_register_atom(local.atom_off = SP_atom_from_string("off"));
}
#endif

static void to_reinit_hook(void)
{
  (void) alarm_clock_off();
}


void to_init(int when)
{
  (void)when;                  /* [PM] 3.9b5 avoid -Wunused */
  /* [PM] 3.9b5 to_reinit calls SP_register_atom but the docs says:
        @itemx SP_VoidFun * SP_set_reinit_hook (SP_VoidFun *)
        The installed function is called upon reinitialization. The
        call is made after SICStus Prolog's signal handler installation but
        before any initializations are run and the version banners are displayed.
        Calling Prolog from functions
        invoked through this hook is not supported.

     Also, what the docs did not use to say, reinit hook is only
     called in development systems.

     Also, for a library to usurp the reinit hook is not such a good
     idea (or rather, it makes it obvious that a way to install more
     than one reinit hook is be needed).

     [PM] 3.9b4 The reinit-hook API needs a re-design.
     1. Having reinit hooks is perhaps useful
     2. Having a single reinit hook is arguable overly restrictive
     3. Grabbing the single reinit hook in a library is obviously a terrible idea.

  */
#if 0
  (void) SP_set_reinit_hook(to_reinit);
  to_reinit();
#else  /* [PM] 3.9b5 */
  (void)SP_register_atom(local.atom_off = SP_atom_from_string("off"));

  (void) SP_set_reinit_hook(to_reinit_hook);
  (void) alarm_clock_off();
#endif
}

void to_deinit(int when)
{
  (void)when;                  /* [PM] 3.9b5 avoid -Wunused */

  (void) alarm_clock_off();     /* [PM] also ignore error */
  (void)SP_unregister_atom(local.atom_off);
/*  SP_unset_reinit_hook(to_reinit);*/
  /* [PM] 3.9b5 unsetting the reinit hook is a good idea */
  (void) SP_set_reinit_hook(NULL);
}

/*------------------------------------------------------------------*/
/* foreign(to_start_timer, '$start_timer'(+term, [-integer])). */
long to_start_timer(
     SP_term_ref term		/* off | Integer>0 */
     )
{
  if (SP_is_atom(term))         /* off */
    {
      long time = alarm_clock_off();
      if (time < 0)             /* [PM] 3.8.7 handle error */
        {
          /* error */
          SP_syserror_clib("$start_timer/1", "setitimer");
          return -1;
        }
    }
  else
    {
      long time;

      SP_get_integer(term, &time);
      if (alarm_clock_on(time) != 0) /* error (from setitimer) */
        {
          SP_syserror_clib("$start_timer/1", "setitimer");
          return -1;
        }
    }
  return 0;                     /* no error */
}


/* foreign(to_stop_timer, '$stop_timer'([-term])). */
SP_term_ref to_stop_timer(void) 
{
  SP_term_ref term = SP_new_term_ref();
  
  if (!local.alarm_clock_is_on)
    SP_put_atom(term, local.atom_off);
  else
    {
      long time = alarm_clock_off();
      if (time >= 0)            /* no error */
        {
          SP_put_integer(term, time);
        }
      else                      /* [PM] 3.8.7 error */
        {
          /* It is off if error occurred. Also, the caller will call
             start_timer which will raise an error. */
          SP_put_atom(term, local.atom_off);
        }
    }
  return term;
}

/*------------------------------------------------------------------*/

static int alarm_event(void)
{
  SP_term_ref t = SP_new_term_ref();

  SP_signal(SIGVTALRM,SP_SIG_IGN);
  SP_put_string(t, "time_out");
  SP_raise_exception(t);
  return SP_FAILURE;
}

static void alarm_h(int sig)
{
  (void)sig;                    /* [PM] 3.9b5 avoid -Wunused */

  local.alarm_clock_is_on = 0;

#if DBG
  {
    struct timeval tv;
    (void) gettimeofday(&tv, NULL);
    dbg_fprintf((stderr, "alarm_h now-abs= %ldus", ((tv.tv_sec-local.abs_timeout.tv_sec)*1000*1000)+(tv.tv_usec-local.abs_timeout.tv_usec)));
  }
#endif

  SP_event((SP_EventFun *)alarm_event, NULL);
}


/*------------------------------------------------------------------*/

#ifndef timerclear
#define	timerclear(tvp)		(tvp)->tv_sec = (tvp)->tv_usec = 0
#endif

/* [PM] 3.8.7 return 0 on success, non-zero on error. */
static int alarm_clock_on(time)
     long time; /* time is in milliseconds */
{
  struct itimerval timer;

#if DBG
  {
    struct timeval tv;
    (void) gettimeofday(&tv, NULL);
    tv.tv_sec += time/1000;
    tv.tv_usec += (time%1000)*1000;
    local.abs_timeout = tv;
    dbg_fprintf((stderr, "alarm_clock_on %ldms, abs {%lds,%ldus}", time, tv.tv_sec, tv.tv_usec));
  }
#endif

  
  timer.it_value.tv_sec = time/TimerUnit;
  timer.it_value.tv_usec = (time%TimerUnit)*TimerUnit;
  timerclear(&timer.it_interval);
  SP_signal(SIGVTALRM, alarm_h);

  /* [PM] 3.8.7 setitimer can fail.
     SunOS 5.7 man setitimer:
        All flags to setitimer() other than  ITIMER_REAL  behave  as
        documented  only with "bound" threads. Their ability to mask
        the signal works only with bound threads.  If  the  call  is
        made  using  one  of these flags from an unbound thread, the
        system call returns -1 and sets errno to EACCES.

     In particular, if Java is initialized then (presumably because
     this creates threads) setitimer will fail.
  */
  if (setitimer(Timer, &timer, NULL) != 0)
    {
#if DBG
      int err = errno;
      fprintf(stderr, "\n*** ERROR: alarm_clock_on: setitimer() error %d%s\n",
              err,
              (( err == EACCES) ? " (EACCES)" : "")
              );fflush(stderr);
#endif

      SP_signal(SIGVTALRM,SP_SIG_IGN);

      return -1;
    }

  local.alarm_clock_is_on = 1;
  return 0;
}

/* [PM] 3.8.7 return negative on error (from setitimer()) */
static long alarm_clock_off()
{
  struct itimerval timer1, timer2;
  long time;
  
  SP_signal(SIGVTALRM,SP_SIG_IGN);
  timerclear(&timer2.it_value);
  timerclear(&timer2.it_interval);
  if (setitimer(Timer,&timer2,&timer1) != 0) /* [PM] 3.8.7 */
    {
#if DBG
      int err = errno;
      fprintf(stderr, "\n*** ERROR: alarm_clock_off: setitimer() error %d%s\n",
              err,
              (( err == EACCES) ? " (EACCES)" : "")
              );fflush(stderr);
#endif

      /* SIGVTALRM already ignored */
      local.alarm_clock_is_on = 0;

      return -1;
    }
  time = timer1.it_value.tv_usec/TimerUnit + timer1.it_value.tv_sec*TimerUnit;
#if linux && !INHIBIT_LINUX_ADJUST
  /* Linux seems to add up to 20 msec to the requested time... */
  time = time<20 ? 0 : time-20;
#endif
  local.alarm_clock_is_on = 0;
  return time;
}


#endif /* LEGACY_TIMOUT */
#endif /* !SP_WIN32 */

/********************************************************************************/

#if SP_WIN32               /* [PM] 3.9b4 */
/* done above: #include <sicstus/config.h> */

#include <sicstus/sicstus.h>
#include <windows.h>
#include <process.h>            /* _beginthreadex et al. */
#include <stdlib.h>             /* errno, _doserrno */
#include <stdio.h>

#include "timeout_glue.h"

/*
  Idea:
  CreateThread()
  WaitForMultipleObjects with timeout to implement interruptible sleep
  Possibly GetTickCount, perhaps GetSystemTimeAdjustment (no, only on NT) to adjust actual elapsed time
 */

static int alarm_clock_on (long time);
static long alarm_clock_off (int *was_off); /* note, different from non-Win32 version */
static int reSynchTimerThread(void);
static void DisplayErrorText(DWORD dwLastError, int c_error, const char *fun_name);

struct {

SP_atom atom_off;
int alarm_clock_is_on;

/* The timer thread signals this event to tell the main thread that it
   is willing to accept a new what_to_do message */
HANDLE wantNewToDo_Event;
int have_wantNewToDo_Event;

/* The main thread signals this event to tell the timer thread that a
   new what_to_do message is available */
HANDLE newToDoAvailable_Event;
int have_newToDoAvailable_Event;

unsigned long timerThread;
int have_timerThread;

DWORD timeToSleep;              /* must be valid when what_to_do==WHAT_TO_DO_SLEEP */
DWORD lastTime;                 /* GetTickCount() when we timerThread sleep started */
#define WHAT_TO_DO_SLEEP 1      /* sleep timeToSleep ms before doing SP_event */
#define WHAT_TO_DO_EXIT 2       /* exit from timerThread */
#define WHAT_TO_DO_RESTART 3    /* Abort sleep, Wait for next what_to_do */
int what_to_do;                 /* must be valid before event is signalled. */

} local;

/* When this returns the timer thread has died or been forcibly killed */
static void wait_for_timer_thread_death(void)
{
  DWORD rc;
  DWORD msToSleep = 60*1000;    /* Wait 1 minute, then kill it forcibly */

  /* Consider if this should be MsgWaitForMultipleObjects */
  rc = WaitForMultipleObjects(1, &(HANDLE)local.timerThread, FALSE, msToSleep);
  
  if (rc == WAIT_OBJECT_0)      /* timer thread exited */
    {
      return;
    }

  /* Something bad happened. */
#if DBG
  fprintf(stderr, "\n*** ERROR: Got error %lu (%s) waiting for timer thread to die\n", (unsigned long)rc, (rc == WAIT_TIMEOUT ? "TIMEOUT" : (rc == WAIT_FAILED ? "FAILED" : "UNKNOWN")));fflush(stderr);
#endif/* DBG */
  
  if (rc == WAIT_TIMEOUT)
    {
    }
  else if (rc == WAIT_FAILED)
    {
#if DBG
      DisplayErrorText(GetLastError(), 0, "wait_for_timer_thread_death");
#endif/* DBG */
    }
#if DBG
  fprintf(stderr, "\n*** ERROR: Will force timer thread to quit\n");fflush(stderr);
#endif /* DBG */
  
  if (!TerminateThread((HANDLE)local.timerThread, 1))
    {
#if DBG
      DisplayErrorText(GetLastError(), 0, "wait_for_timer_thread_death");
#endif/* DBG */
    }
}


static void to_cleanup(void)
{
  if (local.have_timerThread)
    {
      if (!reSynchTimerThread())
        {
#if DBG
          DisplayErrorText(0, 0, "to_cleanup");
#endif /* DBG */
        }
      else
        {
          local.what_to_do = WHAT_TO_DO_EXIT;
          if (!SetEvent(local.newToDoAvailable_Event))
            {
              #if DBG
              DisplayErrorText(GetLastError(), 0, "to_cleanup");
              #endif/* DBG */
            }
        }
      /* we need to wait until the timer thread exits. If return from
         to_cleanup before the timer thread is done
         unload_foreign_resource will be done, unloading the code that
         the timer thread is running. [PM] had a Heisenbug caused by
         returning to soon from to_cleanup. */

      wait_for_timer_thread_death();
      /* invariant: The timer thread is dead here */
      
      if (!CloseHandle((HANDLE)local.timerThread))
        {
          #if DBG
          DisplayErrorText(GetLastError(), 0, "to_cleanup");
          #endif
        }

      local.have_timerThread = 0;
    }
  
  if (local.have_newToDoAvailable_Event)
    {
      local.have_newToDoAvailable_Event = 0;
      if (!CloseHandle(local.newToDoAvailable_Event))
        {
          #if DBG
          DisplayErrorText(GetLastError(), 0, "to_cleanup");
          #endif
        }
    }

  if (local.have_wantNewToDo_Event)
    {
      local.have_wantNewToDo_Event = 0;
      if (!CloseHandle(local.wantNewToDo_Event))
        {
          #if DBG
          DisplayErrorText(GetLastError(), 0, "to_cleanup");
          #endif
        }
    }
  local.alarm_clock_is_on = 0;
}

static void to_reinit(void)
{
  (void) alarm_clock_off(NULL);
  (void)SP_register_atom(local.atom_off = SP_atom_from_string("off"));
}

static void to_reinit_hook(void)
{
  (void) alarm_clock_off(NULL);
}


void SPCDECL to_init(int when)
{
  local.alarm_clock_is_on = 0;
  local.have_wantNewToDo_Event = 0;
  local.have_newToDoAvailable_Event = 0;
  local.have_timerThread = 0;

  to_reinit();
}

void SPCDECL to_deinit(int when)
{
  (void) alarm_clock_off(NULL);     /* ignore error and was_off */
  to_cleanup();
  (void) SP_unregister_atom(local.atom_off);
}

#if DBG
static void
DisplayErrorText(
                 DWORD dwLastError,
                 int c_error,
                 const char *fun_name
    )
{
  HMODULE hModule = NULL; // default to system source
  LPSTR MessageBuffer;
  DWORD dwBufferLength;

  DWORD dwFormatFlags = 
    ( FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_IGNORE_INSERTS | FORMAT_MESSAGE_FROM_SYSTEM );

  if (!dwLastError)
    {
      fprintf(stderr, "*** ERROR: (errno==%d) in %s\n", c_error, (fun_name ? fun_name : "???"));
      fflush(stderr);
      return;
    }

  dwBufferLength = FormatMessageA(
                                  dwFormatFlags,
                                  hModule, // module to get message from (NULL == system)
                                  dwLastError,
                                  MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), // default language
                                  (LPSTR) &MessageBuffer,
                                  0,
                                  NULL
                                  );


  fprintf(stderr, "*** ERROR: in %s, GetLastError()==%ld%s%s\n",
          (fun_name ? fun_name : "???"),
          dwLastError,
          (dwBufferLength ? ": " : ""), 
          (dwBufferLength ? MessageBuffer : ""));
  fflush(stderr);
      
  if (dwBufferLength)
    {
      LocalFree(MessageBuffer);
    }
}
#endif /* DBG */

static int __cdecl timerSPEventFunc (void* data)
{
  SP_term_ref t = SP_new_term_ref();

  SP_put_string(t, "time_out");
  SP_raise_exception(t);
  
  return SP_FAILURE;
}

static unsigned int __stdcall timerThreadFunc (void* data)
{
  unsigned int exit_code = (unsigned) EXIT_SUCCESS; /* currently not used */
  DWORD rc;
  int noLastError = 0;          /* true if GetLastError() is irrelevant */
  DWORD msToSleep;

 wait_for_something_to_do:

  msToSleep = INFINITE;

 wait_for_timeout_or_something_new_to_do:

  if (!SetEvent(local.wantNewToDo_Event)) /* tell main thread that we are about to wait for new what_to_do */
    {
      goto barf;
    }
#if DBG>1
  fprintf(stderr, "\nEnter WaitForMultipleObjects(%dms)..", (int)msToSleep);fflush(stderr);
#endif /* DBG>1 */

  rc = WaitForMultipleObjects(1, &local.newToDoAvailable_Event, TRUE, msToSleep);

#if DBG>1
  fprintf(stderr, "..Exit WaitForMultipleObjects(%dms)==%d..", (int)msToSleep,rc);fflush(stderr);
#endif /* DBG>1 */

  if (rc == WAIT_OBJECT_0)      /* event signalled, if we were sleep this is now ignored */
    {
      #if DBG>1
      fprintf(stderr, "..what_to_do==%d (%s)\n", (int)local.what_to_do,
              (local.what_to_do==1 ? "SLEEP" :
               (local.what_to_do==2 ? "EXIT" :
                (local.what_to_do==3 ? "RESTART" :
                 "UNKNOWN")))
              );fflush(stderr);
      #endif /* DBG>1 */

      noLastError = 1;
      if (local.what_to_do == WHAT_TO_DO_EXIT)
        {
          goto do_exit;
        }
      else if (local.what_to_do == WHAT_TO_DO_SLEEP)
        {
          msToSleep = local.timeToSleep;
          /* We re-use the event so that the wait can be aborted */
          goto wait_for_timeout_or_something_new_to_do;
        }
      else if (local.what_to_do == WHAT_TO_DO_RESTART)
        {
          goto wait_for_something_to_do; /* sleep was cancelled */
        }
      else                  /* unknown what_to_do */
        {
          goto barf;
        }
    }
  else if (rc == WAIT_TIMEOUT) /* ordinary sleep for local.timeToSleep ms */
    {
      #if DBG>1
      fprintf(stderr, "..WAIT_TIMEOUT\n");fflush(stderr);
      #endif /* DBG>1 */

      local.alarm_clock_is_on = 0;
      if (!SP_event(timerSPEventFunc, NULL))
        {
          goto barf;
        }
      goto wait_for_something_to_do;
    }
  else if (rc == WAIT_FAILED)
    {
      goto barf;
    }
  else                          /* unexpected (WAIT_ABANDON?) */
    {
      noLastError = 1;
      goto barf;
    }
  /* NOT REACHED */

 barf:
  exit_code = (unsigned) EXIT_FAILURE;
#if DBG
  DisplayErrorText(( noLastError ? 0 : GetLastError()), 0, "timerThreadFunc");
#endif /* DBG */

 do_exit:

  /* have_timerThread is owned by the main thread. It will be cleared
     when the thread handle indicates that the thread is truly
     dead. */

  #if 0
  local.have_timerThread = 0;         /* the thread will exit when this function returns */
  #endif

  return exit_code;
}

/* Wait for the timer thread to be ready to accept a new message (or
   die). This should be a very short wait. */
static int waitForTimerThread(int reset)
{
  HANDLE events[2];
  int rc;
  int c_error = 0;              /* errno */
  DWORD dos_error = 0;          /* GetLastError() */

  events[0] = local.wantNewToDo_Event;
  events[1] = (HANDLE)local.timerThread; /* also notice if the thread dies */
  /* Consider if this should be MsgWaitForMultipleObjects */
  rc = WaitForMultipleObjects(2, events, FALSE /* wait for either to be signalled */, INFINITE);

  if (rc != WAIT_OBJECT_0)    /* WAIT_OBJECT_0 if wantNewToDo_Event was signalled */
    {
      if (rc == WAIT_OBJECT_0+1) /* timerThread exited. We're toast */
        {
          goto barf;
        }
      else if (rc == WAIT_FAILED)
        {
          dos_error = GetLastError();
          goto barf;
        }
      else                        /* unexpected. */
        {
          goto barf;
        }
    }
  /* wantNewToDo_Event has been reset by WaitForMultipleObjects, unset it if needed */
  if (!reset)
    {
      if (!SetEvent(local.wantNewToDo_Event))
        {
          dos_error = GetLastError();
          goto barf;
        }
    }
  return 1;

 barf:
#if DBG
  DisplayErrorText(dos_error, c_error, "alarm_clock_on");
#endif /* DBG */
  return 0;
}

static int reSynchTimerThread(void)
{
  int c_error = 0;              /* errno */
  DWORD dos_error = 0;          /* GetLastError() */

  if (!waitForTimerThread(TRUE))
    {
      goto barf;
    }
  /* timer thread is either waiting for a new command or it is processing a time out */
  local.what_to_do = WHAT_TO_DO_RESTART;
  if (!SetEvent(local.newToDoAvailable_Event))
    {
      dos_error = GetLastError();
      goto barf;
    }

  if (!waitForTimerThread(FALSE)) /* leave the Event set */
    {
      goto barf;
    }
  /* timer thread has received the WHAT_TO_DO_RESTART and is now
     waiting without time-limit on the next to-do command. */

  return 1;

 barf:

#if DBG
  DisplayErrorText(dos_error, c_error, "reSynchTimerThread");
#endif /* DBG */
  return 0;
}

/* [PM] 3.8.7 return 0 on success, non-zero on error. */
static int alarm_clock_on(
     long time /* time is in milliseconds */
     )
{
  int c_error = 0;              /* errno */
  DWORD dos_error = 0;          /* GetLastError() */

  if (!local.have_wantNewToDo_Event)
    {
      HANDLE event;
      event = CreateEvent(NULL,     /* no security */
                          FALSE,    /* auto-reset */
                          FALSE,    /* initially unsignalled */
                          NULL);    /* no name */
      if (event == NULL)
        {
          dos_error = GetLastError();
          goto barf;
        }
      local.wantNewToDo_Event = event;
      local.have_wantNewToDo_Event = 1;
    }

  if (!local.have_newToDoAvailable_Event)
    {
      HANDLE event;
      event = CreateEvent(NULL,     /* no security */
                          FALSE,    /* auto-reset */
                          FALSE,    /* initially unsignalled */
                          NULL);    /* no name */
      if (event == NULL)
        {
          dos_error = GetLastError();
          goto barf;
        }
      local.newToDoAvailable_Event = event;
      local.have_newToDoAvailable_Event = 1;
    }

  if (!ResetEvent(local.newToDoAvailable_Event))  /* paranoia */
    {
      dos_error = GetLastError();
      goto barf;
    }

  if (!local.have_timerThread)
    {
      unsigned long thread;
      unsigned threadID;        /* ignored */

      /* timerThreadFunc will start immediately but will hang at the
         unsignalled newToDoAvailable_Event */
      /* We cannot use _beginthread since we want to wait on the thread handle */
      thread = _beginthreadex(NULL, /* no security */
                              0, /* default stack size */
                              timerThreadFunc,
                              NULL, /* arg ptr */
                              0, /* init-flag, run directly */
                              &threadID
                              );

      if (thread == 0)
        {
          c_error = errno;
          dos_error = _doserrno;
          goto barf;
        }
  
      local.timerThread = thread;
      local.have_timerThread = 1;
    }

  /* Here we have timerThreadFunc running in a separate thread,
     waiting on newToDoAvailable_Event (the timerThread already or
     soon having signalled wantNewToDo_Event) */

  if (!waitForTimerThread(TRUE))
    {
      goto barf;
    }

  /* get here if wantNewToDo_Event was signalled, indicating that
     the timer thread is now waiting on newToDoAvailable_Event */
  local.what_to_do = WHAT_TO_DO_SLEEP;
  local.timeToSleep = time;
  local.lastTime = GetTickCount();
  local.alarm_clock_is_on = 1;
  
  if (!SetEvent(local.newToDoAvailable_Event)) /* tell timerThreadFunc that what_to_do is set-up */
    {
      dos_error = GetLastError();
      goto barf;
    }
  
  return 0;
  
 barf:

#if DBG
  DisplayErrorText(dos_error, c_error, "alarm_clock_on");
#endif /* DBG */

  return -1;
}

/* [PM] 3.8.7 return negative on error (from setitimer()) */
static long alarm_clock_off(int *was_off)
{
  DWORD dos_error = 0;
  int c_error = 0;
  int dummy;

  if (was_off==NULL) was_off = &dummy;

  if (!local.have_timerThread)
    {
      *was_off = 1;
      return 0;                 /* not inited, not an error! */
    }

  if (!reSynchTimerThread())
    {
      goto barf;
    }
  /* Now the timer thread is definitely stopped and waiting for the next what_to_do */

  /* alarm_clock_is_on may be cleared by timer thread up to the return
     from reSynchTimerThread(). This can happen if the timer thread
     was processing a time out. */
  if (!local.alarm_clock_is_on)
    {
      *was_off = 1;
      return 0;
    }
  else
    {
      *was_off = 0;
    }
  {
    DWORD currentTime = GetTickCount();
    DWORD remainingTime;
    DWORD elapsedTime;
    
    elapsedTime = currentTime-local.lastTime;
    if (local.timeToSleep > elapsedTime)
      {
        remainingTime = local.timeToSleep - elapsedTime;
      }
    else
      {
        remainingTime = 0;
      }
    
    local.alarm_clock_is_on = 0;
    return remainingTime;
  }

 barf:
#if DBG
  DisplayErrorText(dos_error, c_error, "alarm_clock_off");
#endif /* DBG */

  local.alarm_clock_is_on = 0;
  return -1;
}

/*------------------------------------------------------------------*/
/* foreign(to_start_timer, '$start_timer'(+term, [-integer])). */
long SPCDECL to_start_timer(
     SP_term_ref term		/* off | Integer>0 */
     )
{
  if (SP_is_atom(term))         /* off */
    {
      long time = alarm_clock_off(NULL);
      if (time < 0)
        {
          /* error */
          #if SICSTUS_REVISION_VERSION>0 && !SICSTUS_BETA_VERSION
          #error "Do proper error signalling/PM"
          #endif

          SP_syserror_clib("$start_timer/1", "alarm_clock_off");
          return -1;
        }
    }
  else
    {
      long time;

      SP_get_integer(term, &time);
      if (alarm_clock_on(time) != 0) /* error */
        {
          #if SICSTUS_REVISION_VERSION>0 && !SICSTUS_BETA_VERSION
          #error "Do proper error signalling/PM"
          #endif

          SP_syserror_clib("$start_timer/1", "alarm_clock_on");
          return -1;
        }
    }
  return 0;                     /* no error */
}


/* foreign(to_stop_timer, '$stop_timer'([-term])). */
SP_term_ref SPCDECL to_stop_timer(void) 
{
  SP_term_ref term = SP_new_term_ref();
  
  if (!local.alarm_clock_is_on)
    SP_put_atom(term, local.atom_off);
  else
    {
      int was_off;
      long time = alarm_clock_off(&was_off);

      if (time >= 0)            /* no error */
        {
          if (was_off)
            {
              SP_put_atom(term, local.atom_off);
            }
          else
            {
              SP_put_integer(term, time);
            }
        }
      else                      /* error */
        {
          /* It is off if error occurred. Also, the caller will call
             start_timer which will raise an error. */
          SP_put_atom(term, local.atom_off);
        }
    }
  return term;
}


#endif /* SP_WIN32 */

/********************************************************************************/
/* [PM] 3.9b4 (Beginning of) A port of the Windows thread-based code to UNIX. */
#if !LEGACY_TIMOUT
#if !SP_WIN32

/* these feature defs are taken from Linux /usr/include/features.h */
# undef _REENTRANT
# define _REENTRANT 1
# undef _THREAD_SAFE
# define _THREAD_SAFE

# undef _GNU_SOURCE
# define _GNU_SOURCE 1          /* implies the features below */
/* the features below are implied by _GNU_SOURCE on glibc systems */
# undef  _POSIX_SOURCE
# define _POSIX_SOURCE	1
# undef  _POSIX_C_SOURCE
# define _POSIX_C_SOURCE	199506L
# undef  _XOPEN_SOURCE
# define _XOPEN_SOURCE	500     /* 600 breaks SunOS 5.7 */
# undef  _XOPEN_SOURCE_EXTENDED
# define _XOPEN_SOURCE_EXTENDED	1

/* [PM] NO INCLUDES above this (except sicstus/config.h which does not
   include anything of its own). This is to ensure all includes see
   the feature macros above.
*/

/*
  [PM] 3.9b4 There is a conflict between SunOS 5.7 stdio.h and gcc
  2.95.2 stdarg.h: When _XOPEN_SOURCE is defined and _XOPEN_VERSION>=4
  then the Sun stdio.h defines va_list but so does the GCC stdarg.h.
  The Sun definition is protected by !defined(_VA_LIST) so we let the
  GCC stdarg.h define va_list and then define _VA_LIST so that the Sun
  definition does not happen.
  
 */
#include <stdarg.h>

#if __GNUC__ && __sun
#define _VA_LIST 1
#endif
#include <stdio.h>

#include <sicstus/sicstus.h>
/* done above: #include <sicstus/config.h> */

#include <pthread.h>
#include <errno.h>              /* ETIMEDOUT */
#include <stdlib.h>
#include <sys/time.h>           /* gettimeofday */
#include <time.h>               /* clock_gettime(), ... */
#if HAVE_SYS_RESOURCE_H
#include <sys/resource.h>       /* SUSv2 getrusage() */
#endif
#include "timeout_glue.h"

#if !SICSTUS_BETA_VERSION
#error "detect HAVE_CLOCK_GETTIME and getrusage properly"
#endif
#if __sun
#define HAVE_CLOCK_GETTIME 1
#endif

#if __sun
/*
  [PM] 3.9b5.  Suite timeout_stat.exe (using LEGACY_TIMOUT) segfaults
  at halt (for unknown reason) with -lposix4 (a.k.a. -lrt) on
  sparc-solaris-5.7.  Unfortunately clock_gettime() is defined in
  librt.  For more details see configure.in.
*/
#error "We do not want to use clock_gettime() (-lrt/-lposix4) on Solaris"
#endif


#if linux                       /* Linux POSIX threads are not (POSIX, that is) */
#define THREAD_SPECIFIC_GETRUSAGE 1
#endif

#ifndef TIMEOUT_DEBUG
#define TIMEOUT_DEBUG 0
#endif


#if SP_SINGLE_THREADED
#error "SP_SINGLE_THREADED is not compatible with multi threaded code"
#endif /* SP_SINGLE_THREADED */

#define MutexLock(PMUTEX) { dbg_fprintf((stderr, "MutexLock")); rc = pthread_mutex_lock((PMUTEX)); if (rc != 0) { display_error(rc, "MutexLock", "??"); goto barf;}}
#define MutexUnlock(PMUTEX) { dbg_fprintf((stderr, "MutexUnlock")); rc = pthread_mutex_unlock((PMUTEX)); if (rc != 0) { display_error(rc, "MutexUnlock", "??"); goto barf;}}


#if (DBG && TIMEOUT_DEBUG)
#define dbg_fprintf1(N, ARGS) if ((N)>= TIMEOUT_DEBUG) {fprintf(stderr, "\n** DBG: THREAD %ld: ",(long)pthread_self()); fprintf ARGS; fprintf(stderr, "\n"); fflush(stderr);} else {}
#define dbg_fprintf(ARGS) dbg_fprintf1(1, ARGS)

static void
display_error(int c_error,
              const char *msg,
              const char *fun_name
    )
{
  fprintf(stderr, "*** ERROR: %s (errno==%d) in %s\n", (msg ? msg : ""), c_error, (fun_name ? fun_name : "???"));
  fflush(stderr);
  return;
}
#else  /* !(DBG && TIMEOUT_DEBUG) */
#define dbg_fprintf1(_N,_ARGS)
#define dbg_fprintf(_ARGS)
#define display_error(_C_ERROR, _MSG, _FUN_NAME)
#endif /* !(DBG && TIMEOUT_DEBUG) */


#if HAVE_CLOCK_GETTIME
#if DBG
static int CLOCK_GETTIME(struct timespec *pspec)
{
  int rc;
  struct timespec res;
  
  rc = clock_gettime(CLOCK_REALTIME, pspec);
  
  (void) clock_getres(CLOCK_REALTIME, &res);

  dbg_fprintf((stderr, "clock_gettime()= {%lds, %ldns} res {%lds, %ldns}", (long)pspec->tv_sec, pspec->tv_nsec, (long)res.tv_sec, res.tv_nsec));

  #if 0
  {
    struct timeval tv;
    (void) gettimeofday(&tv, NULL);
    dbg_fprintf((stderr, "gettimeofday(%lds, %ldus)", (long)tv.tv_sec, (long)tv.tv_usec));
  }
  #endif

  return rc;
}
#else  /* !DBG */
#define CLOCK_GETTIME(PSPEC) clock_gettime(CLOCK_GETTIME, (PSPEC))
#endif /* !DBG */

#else  /* !HAVE_CLOCK_GETTIME */
static int CLOCK_GETTIME(struct timespec *pspec)
{
  struct timeval tv;
  (void) gettimeofday(&tv, NULL);
  dbg_fprintf((stderr, "gettimeofday(%lds, %ldus)", (long)tv.tv_sec, (long)tv.tv_usec));

  pspec->tv_sec = tv.tv_sec;
  pspec->tv_nsec = ((long)tv.tv_usec)*1000;


  return 0;
}
#endif /* !HAVE_CLOCK_GETTIME */


static int alarm_clock_on (long time);
static long alarm_clock_off (int *was_off); /* note, different from non-Win32 version */
static int reSynchTimerThread(void);
static int waitForTimerThread(void);

struct {

SP_atom atom_off;
int alarm_clock_is_on;
pthread_t timerThread;
int have_timerThread;

int message_fifo_inited;
pthread_mutex_t mutex;          /* protects the message channel */
#define WHAT_TO_DO_SLEEP 1      /* sleep timeToSleep ms before doing SP_event */
#define WHAT_TO_DO_EXIT 2       /* exit from timerThread */
#define WHAT_TO_DO_RESTART 3    /* Abort sleep, Wait for next what_to_do */
int what_to_do;                 /* message type. */

int message_available;          /* set by main thread when a message has been sent to timer thread */
pthread_cond_t message_available_cond; /* timer thread cond waits for this to get a message  */
int message_read;               /* set by timer thread when the message has been read */
pthread_cond_t message_read_cond; /* main thread waits for this this to know when the timer thread is done with the message */

/* absolute time until which to wait */
struct timespec sleep_until;
  /* usertime when timer started plus timeout interval */
struct timeval stop_usertime;

struct timeval usertime;
int usertime_available;
pthread_cond_t usertime_available_cond;
} local;


static int get_usertime(struct timeval *ptv)
{
  int rc;
  struct rusage r_usage;

  rc = getrusage(RUSAGE_SELF, &r_usage);
  if (rc != 0)
    {
      #if !SICSTUS_BETA_VERSION
      #error "error handling"
      #endif
    }
  *ptv = r_usage.ru_utime;
  return rc;
}

#if THREAD_SPECIFIC_GETRUSAGE   /* Linux */

#define GET_USERTIME(PTV) get_main_thread_usertime((PTV))

#else  /* !THREAD_SPECIFIC_GETRUSAGE (SUSv2/POSIX compliant) */

#define GET_USERTIME(PTV) get_usertime((PTV))

#endif /* !THREAD_SPECIFIC_GETRUSAGE */

#if THREAD_SPECIFIC_GETRUSAGE

#error "This does not work. Its value is needed while the main thread is blocking on message_read_cond"

/* call with mutex locked */
static int timer_get_rusage_SPEventFunc (void* data)
{
  dbg_fprintf1(2,(stderr, "timer_get_rusage_SPEventFunc"));
  {
    int rc;
    rc = get_usertime(&local.usertime);
    if (rc != 0)
      {
#if !SICSTUS_BETA_VERSION
#error "should really propagate to timer thread"
#endif
        goto barf;
      }
    local.usertime_available=1;
    rc=pthread_cond_signal(&local.usertime_available_cond);
    if (rc != 0) goto barf;

  barf:
    return SP_SUCCESS;
  }
}

/* call with mutex locked */
static int get_main_thread_usertime(struct timeval *ptv)
{
  int rc;
  dbg_fprintf1(2,(stderr, "get_main_thread_usertime"));
  local.usertime_available=0;
  if (!SP_event(timer_get_rusage_SPEventFunc, NULL)) goto barf;
  while (!local.usertime_available)
    {
      dbg_fprintf1(2,(stderr, "get_main_thread_usertime waiting on usertime_available_cond"));
      rc = pthread_cond_wait(&local.usertime_available_cond, &local.mutex);
      dbg_fprintf1(2,(stderr, "get_main_thread_usertime waited on usertime_available_cond"));
      if (rc != 0) goto barf;
    }
  *ptv = local.usertime;
  return 0;
  
 barf:
  dbg_fprintf1(2,(stderr, "get_main_thread_usertime barf"));
  return -1;
}
#endif /* THREAD_SPECIFIC_GETRUSAGE */


/* When this returns the timer thread has died or been forcibly killed */
static void wait_for_timer_thread_death(void)
{
  int rc;
  long msToSleep = 60*1000;    /* Wait 1 minute, then kill it forcibly */
  int mutex_locked = 0;

  MutexLock(&local.mutex);
  mutex_locked = 1;

  if (!waitForTimerThread())
    {
      goto barf;
    }
  mutex_locked = 0;
  MutexUnlock(&local.mutex);

  rc = pthread_join(local.timerThread, NULL);
  if (rc != 0)
    {
      display_error(rc, "pthread_join", "wait_for_timer_thread_death");
      goto barf;
    }

  return;

 barf:

  /* Something bad happened. */
  #if !SICSTUS_BETA_VERSION
  #error "Should consider pthread_kill_np/pthread_cancel etc to ensure it is killed."
  #endif /* !SICSTUS_BETA_VERSION */

  if (mutex_locked)
    {
      mutex_locked = 0;
      MutexUnlock(&local.mutex);
    }

  display_error(0, "barf", "wait_for_timer_thread_death");
  return;
}


static void to_cleanup(void)
{
  int rc = 0;
  int mutex_locked = 0;

  if (local.have_timerThread)
    {

      MutexLock(&local.mutex);
      mutex_locked = 1;

      if (!reSynchTimerThread())
        {
          display_error(0, "reSynchTimerThread", "to_cleanup");
        }
      else
        {
          /* timer thread is definitely hanging waiting for a message.  */
          if (!local.message_read)
            {
              dbg_fprintf((stderr, "INTERNAL ERROR: message_read not set after reSynchTimerThread()"));
            }

          /* the following block for sending a message should be abstracted out */
          local.what_to_do = WHAT_TO_DO_EXIT;
          local.message_read = 0;       /* timer thread has not read the message yet */
          local.message_available = 1;
          dbg_fprintf((stderr, "pthread_cond_signal(&local.message_available_cond)"));
          rc = pthread_cond_signal(&local.message_available_cond);
          if (rc != 0)
            {
              display_error(rc, "pthread_cond_signal", "to_cleanup");
              goto barf;
            }
          
          /* wait for timer thread to read the message */
          if (!waitForTimerThread())
            {
              dbg_fprintf((stderr, "!waitForTimerThread() in to_cleanup"));
              goto barf;
            }
        }
    barf:
      if (mutex_locked)
        {
          mutex_locked = 0;         /* if MutexUnlock fails we should not try it again */
          MutexUnlock(&local.mutex);
        }
      /* we need to wait until the timer thread exits. If return from
         to_cleanup before the timer thread is done
         unload_foreign_resource will be done, unloading the code that
         the timer thread is running. [PM] had a Heisenbug caused by
         returning to soon from to_cleanup. */

      wait_for_timer_thread_death();
      /* invariant: The timer thread is dead here */
      
      local.have_timerThread = 0;
    }
  
  if (local.message_fifo_inited)
    {
      rc = pthread_cond_destroy(&local.message_available_cond);
      if (rc != 0)
        {
          display_error(rc, "pthread_cond_destroy(&local.message_available_cond)", "to_cleanup");
        }
      
      rc = pthread_cond_destroy(&local.message_read_cond);
      if (rc != 0)
        {
          display_error(rc, "pthread_cond_destroy(&local.message_read_cond)", "to_cleanup");
        }

      rc = pthread_mutex_destroy(&local.mutex);
      if (rc != 0)
        {
          display_error(rc, "pthread_mutex_destroy(&local.mutex)", "to_cleanup");
        }
    }
  local.alarm_clock_is_on = 0;
}

static void to_reinit(void)
{
  (void) alarm_clock_off(NULL);
  (void)SP_register_atom(local.atom_off = SP_atom_from_string("off"));
}

void SPCDECL to_init(int when)
{
  local.alarm_clock_is_on = 0;
  local.have_timerThread = 0;
  local.message_fifo_inited = 0;


  to_reinit();
}

void SPCDECL to_deinit(int when)
{
  (void) alarm_clock_off(NULL);     /* ignore error and was_off */
  to_cleanup();
  (void) SP_unregister_atom(local.atom_off);
}



static int timerSPEventFunc (void* data)
{
  dbg_fprintf((stderr, "timerSPEventFunc"));
  {
  SP_term_ref t = SP_new_term_ref();

  SP_put_string(t, "time_out");
  SP_raise_exception(t);
  
  return SP_FAILURE;
  }
}

static void* timerThreadFunc (void* data)
{
  int rc;
  struct timespec timeout;
  int infinite;
  int expired = 0;
  int mutex_locked = 0;

  MutexLock(&local.mutex);       /* we will release this only during cond wait */
  mutex_locked = 1;             /* ensure it gets unlocked when thread exits */

 wait_for_something_to_do:
  infinite = 1;

 wait_for_timeout_or_something_new_to_do:
  while (!local.message_available)
    {
      dbg_fprintf ((stderr, "timerThreadFunc !message_available"));
      dbg_fprintf ((stderr, "local.message_read==%d", local.message_read));

      /* Note that timeout is an absolute time */
      #if DBG
      {
        struct timespec now;
        long diff_ns;

        (void) CLOCK_GETTIME(&now);

        diff_ns = (now.tv_sec-timeout.tv_sec)*1000*1000*1000; /* ns */
        diff_ns += (now.tv_nsec-timeout.tv_nsec);

        dbg_fprintf((stderr,
                     "Enter pthread_cond_%swait(message_available) timeout={%lds,%ldns} now-timeout=%ldns..",
                     (infinite ? "": "timed"),
                     (infinite ? -1 : (long)timeout.tv_sec), (infinite ? -1 : timeout.tv_nsec), diff_ns));
      }
      #endif /* DBG */
      if (infinite)
        {
          rc = pthread_cond_wait(&local.message_available_cond, &local.mutex);
        }
      else
        {
          rc = pthread_cond_timedwait(&local.message_available_cond, &local.mutex, &timeout);
        }
      #if DBG
      {
        struct timespec now;
        long diff_ns;

        (void) CLOCK_GETTIME(&now);

        diff_ns = (now.tv_sec-timeout.tv_sec)*1000*1000*1000; /* ns */
        diff_ns += (now.tv_nsec-timeout.tv_nsec);
        dbg_fprintf((stderr,
                     "..Exit pthread_cond_%swait(message_available)==%d%s timeout={%lds,%ldns}, now-timeout=%ldns",
                     (infinite ? "" : "timed"),
                     rc,
                     (rc == ETIMEDOUT ? " (ETIMEDOUT) " : ""),
                     (infinite ? -1 : (long)timeout.tv_sec) ,
                     (infinite ? -1 :timeout.tv_nsec),
                     diff_ns
                     ));
      }
      #endif/* DBG */


      expired=0;
      if (rc == ETIMEDOUT && !infinite) /* Although presumably infinite and ETIMEDOUT are mutually exlusive */
        {
          if (local.message_available)
            {
              break;            /* prefer message if both message and timeout,  */
            }
          expired = 1;
          {
            struct timeval now_usertime;
            struct timeval usertime;
            
            /* This will not work on Linux since it will report timer thread time only (i.e., almost zero usertime) */
            rc = get_usertime(&now_usertime);
            if (rc != 0) goto barf;

            /* if we have not yet used up enough usertime we just sleep a little more */

            /* local.stop_usertime - now_usertime */
            usertime=local.stop_usertime;

            if (usertime.tv_usec < now_usertime.tv_usec)
              {
                usertime.tv_sec -= 1; /* borrow */
                usertime.tv_usec += (1000*1000);
              }
            usertime.tv_usec -= now_usertime.tv_usec;
            usertime.tv_sec -= now_usertime.tv_sec;
            dbg_fprintf1(2, (stderr, "local.stop_usertime-now_usertime = {%lds, %ldus}\n"
                             "local.stop_usertime = {%lds,%ldus}, now_usertime={%lds, %ldus}",
                             usertime.tv_sec,
                             usertime.tv_usec,
                             local.stop_usertime.tv_sec, local.stop_usertime.tv_usec,
                             now_usertime.tv_sec, now_usertime.tv_usec
                             ));
            
            if (usertime.tv_sec < 0 /* usertime<=0, i.e., all usertime spent */
                ||
                ( usertime.tv_sec==0
                  &&
                  usertime.tv_usec==0))
              {
                dbg_fprintf1(2,(stderr, "usertime passed"));

                break;
              }
            /* still usertime to go, add it to sleep_until */
            rc = CLOCK_GETTIME(&timeout); /* now */
            if (rc != 0) goto barf;
            timeout.tv_sec += usertime.tv_sec;
            timeout.tv_nsec += (usertime.tv_usec*1000); /* cannot overflow a 32bit long */
            if (timeout.tv_nsec >= 1000*1000*1000)
              {
                timeout.tv_sec += timeout.tv_nsec/(1000*1000*1000);
                timeout.tv_nsec = timeout.tv_nsec%(1000*1000*1000);
              }
            dbg_fprintf1(2, (stderr, "usertime to go, sleeping until {%lds, %ldns}",
                             timeout.tv_sec, timeout.tv_nsec));
            }
          /* continue */
        }
      else if (rc != 0)
        {
          display_error(rc, "pthread_cond_timedwait(message_available_cond)", "timerThreadFunc");
          goto barf;
        }
      else
        {
          /* condition variable signalled or spurious wake up */
        }
    }

  infinite = 0;

  /* here message_available xor expired holds */
  local.message_available=0;
  local.message_read = 1;       /* not seen by main thread until we unlock mutex above */
  dbg_fprintf((stderr, "pthread_cond_signal(&local.message_read_cond)"));

  rc = pthread_cond_signal(&local.message_read_cond);
  if (rc != 0)
    {
      display_error(rc, "pthread_cond_signal(&local.message_read_cond)", "timerThreadFunc");
      goto barf;
    }
  


  if (expired)
    {
      dbg_fprintf((stderr, "expired"));
      local.alarm_clock_is_on = 0;
      if (!SP_event(timerSPEventFunc, NULL))
        {
          goto barf;
        }
      goto wait_for_something_to_do;
    }

  /* get here only if new message available */

  dbg_fprintf((stderr, "..what_to_do==%d (%s)", (int)local.what_to_do,
               (local.what_to_do==1 ? "SLEEP" :
                (local.what_to_do==2 ? "EXIT" :
                 (local.what_to_do==3 ? "RESTART" :
                  "UNKNOWN")))
               ));

  switch (local.what_to_do)
    {
    case WHAT_TO_DO_EXIT:
      {
        dbg_fprintf((stderr, "exiting"));
        goto do_exit;
      }
      
    case WHAT_TO_DO_SLEEP:
      { 
        timeout.tv_sec = local.sleep_until.tv_sec;
        timeout.tv_nsec = local.sleep_until.tv_nsec;
        dbg_fprintf((stderr, "sleeping %lds, %ldns", (long)timeout.tv_sec, (long)timeout.tv_nsec));
        goto wait_for_timeout_or_something_new_to_do;
      }
    case WHAT_TO_DO_RESTART:
      {
        dbg_fprintf((stderr, "restarting"));
        goto wait_for_something_to_do; /* sleep was cancelled */
      }
    default:                    /* unknown what_to_do */
      {
        goto barf;
      }
    }
  /* NOTREACHED */

 barf:

  display_error(0, "barf", "timerThreadFunc");

  /* FALLTHROUGH */

 do_exit:
  if (mutex_locked)
    {
      mutex_locked=0;
      MutexUnlock(&local.mutex);
    }
#if !SICSTUS_BETA_VERSION
#error "there is a timing problem here. Also for the win32-version"
#endif /* !SICSTUS_BETA_VERSION */

  local.have_timerThread = 0;         /* the thread will exit when this function returns */
  return NULL;
}

/* Wait for the timer thread to be ready to accept a new message (or
   die). This should be a very short wait.
   Assumes the mutex is already locked by caller!

*/
static int waitForTimerThread(void)
{
  int rc;
  
  while (!local.message_read)
    {
      #if !SICSTUS_BETA_VERSION
      #error "here we really ought to detect if the timer thread dies"
      #endif

      dbg_fprintf((stderr, "Enter pthread_cond_wait(message_read).."));
      rc = pthread_cond_wait(&local.message_read_cond, &local.mutex);
      dbg_fprintf((stderr, "..Exit pthread_cond_wait(message_read)==%d", rc));

      if (rc != 0)
        {
          goto barf;
        }
    }

  return 1;

 barf:
  display_error(rc, "barf", "alarm_clock_on");
  return 0;
}

#if !SICSTUS_BETA_VERSION
#error "main thread should lock the mutex at a higher level"
#endif

/* Caller should have locked the mutex
   on successful exit the timer thread has been restarted and is waiting for a message without timeout
*/
static int reSynchTimerThread(void)
{
  int rc = 0;

  if (!waitForTimerThread())
    {
      goto barf;
    }
  /* There is room for a message */

  /* timer thread is either waiting for a new message or it is processing a time out */
  local.what_to_do = WHAT_TO_DO_RESTART;
  local.message_available = 1;
  local.message_read = 0;       /* timer thread has not read the message yet */
  dbg_fprintf((stderr, "pthread_cond_signal(&local.message_available_cond)"));
  rc = pthread_cond_signal(&local.message_available_cond);
  if (rc != 0)
    {
      goto barf;
    }

  /* wait for timer thread to read the message */
  if (!waitForTimerThread())
    {
      goto barf;
    }

  /* timer thread has received the WHAT_TO_DO_RESTART and is now
     waiting without time-limit on the next to-do command. */

  return 1;

 barf:
  display_error(rc, "barf", "reSynchTimerThread");
  return 0;
}

/* [PM] 3.8.7 return 0 on success, non-zero on error. */
static int alarm_clock_on(
     long ms_to_sleep /* time is in milliseconds */
     )
{
  int rc = 0;
  int mutex_locked=0;

  if (!local.message_fifo_inited)
    {
      rc = pthread_mutex_init(&local.mutex, NULL);
      if (rc != 0)
        {
          display_error(rc, "pthread_mutex_init", "alarm_clock_on");
          goto barf;
        }
      
      rc = pthread_cond_init(&local.message_available_cond, NULL);
      if (rc != 0)
        {
          display_error(rc, "pthread_cond_init(message_available_cond)", "alarm_clock_on");
          goto barf;
        }
      local.message_available = 0;

      rc = pthread_cond_init(&local.message_read_cond, NULL);
      if (rc != 0)
        {
          display_error(rc, "pthread_cond_init(message_read_cond)", "alarm_clock_on");
          goto barf;
        }
      local.message_read = 1;   /* start off with no message unread */

      local.message_fifo_inited = 1;
    }
  if (!local.have_timerThread)
    {
      pthread_t thread;
      rc = pthread_create(&thread, NULL, &timerThreadFunc, NULL); /* should pass "local" if MULTI_SP_AWARE */

      if (rc != 0)
        {
          display_error(rc, "pthread_create", "alarm_clock_on");
          goto barf;
        }
      local.timerThread = thread;
      local.have_timerThread = 1;
    }
  
  /* Here we have timerThreadFunc running in a separate thread, (soon)
     waiting on message_available */

  MutexLock(&local.mutex);
  mutex_locked = 1;
  if (!waitForTimerThread())
    {
      goto barf;
    }
  /* The message queue is now empty */

  {
    time_t tv_sec_rel, tv_sec_abs;
    long tv_nsec_rel, tv_nsec_abs;
    struct timespec spec;
    struct timeval usertime;

    rc = get_usertime(&usertime); /* *not* GET_USERTIME */
    if (rc != 0) goto barf;
    dbg_fprintf((stderr, "usertime={%lds,%ldus}",
                 (long)usertime.tv_sec, (long)usertime.tv_usec));

    if (ms_to_sleep >= 1000)
      {
        usertime.tv_sec += ms_to_sleep/1000;
        ms_to_sleep = ms_to_sleep%1000;
      }
    usertime.tv_usec += ms_to_sleep*1000;
    if (usertime.tv_usec >= 1000*1000)
      {
        usertime.tv_sec += usertime.tv_usec/(1000*1000);
        usertime.tv_usec = usertime.tv_usec%(1000*1000);
      }
    /* usertime is now absolute user time that we want to wait for */
    local.stop_usertime = usertime;
    dbg_fprintf((stderr, "stop_usertime={%lds,%ldus}", 
                 (long)local.stop_usertime.tv_sec, (long)local.stop_usertime.tv_usec));    

    tv_sec_rel = ms_to_sleep/1000;
    tv_nsec_rel = (ms_to_sleep % 1000) * 1000 * 1000;
    dbg_fprintf((stderr, "settimer tv_sec_rel=%lds, tv_nsec_rel=%ldns", tv_sec_rel, tv_nsec_rel));

    
    rc = CLOCK_GETTIME(&spec);
    if (rc != 0) goto barf;

    dbg_fprintf((stderr, "clock_gettime({%lds, %ldns}", (long)spec.tv_sec, (long)spec.tv_nsec));
    tv_sec_abs = spec.tv_sec+tv_sec_rel;
    tv_nsec_abs = spec.tv_nsec + tv_nsec_rel; /* this cannot overflow a long (<= 1999 999 998) */ 
    if  (tv_nsec_abs > 1000*1000*1000) /* more than one second worth of nano */
      {
        tv_nsec_abs -= 1000*1000*1000;
        tv_sec_abs += 1;
      }
    dbg_fprintf((stderr, "settimer %ldms, abs time tv_sec_abs==%lds, tv_nsec_abs==%ldns", ms_to_sleep, tv_sec_abs, tv_nsec_abs));

    local.what_to_do = WHAT_TO_DO_SLEEP;
    local.sleep_until.tv_sec = tv_sec_abs; /* absolute time */
    local.sleep_until.tv_nsec = tv_nsec_abs; /* absolute time */
  }
  local.message_available = 1;
  local.message_read=0;
  dbg_fprintf((stderr, "pthread_cond_signal(&local.message_available_cond)"));
  rc = pthread_cond_signal(&local.message_available_cond);
  if (rc != 0)
    {
      goto barf;
    }

  local.alarm_clock_is_on = 1;

  mutex_locked=0;
  MutexUnlock(&local.mutex);
  return 0;

 barf:
  display_error(rc, "barf", "alarm_clock_on");
  if (mutex_locked)
    {
      mutex_locked=0;
      MutexUnlock(&local.mutex);
    }
  return -1;
}

/* [PM] 3.8.7 return negative on error (from setitimer()) */
static long alarm_clock_off(int *was_off)
{
  int rc = 0;
  int dummy;

  if (was_off==NULL) was_off = &dummy;

  if (!local.have_timerThread)
    {
      *was_off = 1;
      return 0;                 /* not inited, not an error! */
    }

  MutexLock(&local.mutex);
  if (!reSynchTimerThread())
    {
      MutexUnlock(&local.mutex);
      goto barf;
    }
  MutexUnlock(&local.mutex);

  /* Now the timer thread is definitely stopped and waiting for the next what_to_do */

#if !SICSTUS_BETA_VERSION
#error "is the following comment correct for the pthreads version of this code?"
#endif
  /* alarm_clock_is_on may be cleared by timer thread up to the return
     from reSynchTimerThread(). This can happen if the timer thread
     was processing a time out.
  */
  if (!local.alarm_clock_is_on)
    {
      *was_off = 1;
      return 0;
    }
  else
    {
      *was_off = 0;
    }
#if 1
  {
    long remaining_time_ms;

    struct timeval now_usertime;
    struct timeval usertime;

    rc = get_usertime(&now_usertime); /* *not* GET_USERTIME */
    if (rc != 0) goto barf;

    /* local.stop_usertime - now_usertime */
    usertime=local.stop_usertime;

    if (usertime.tv_usec < now_usertime.tv_usec)
      {
        usertime.tv_sec -= 1; /* borrow */
        usertime.tv_usec += (1000*1000);
      }
    usertime.tv_usec -= now_usertime.tv_usec;
    usertime.tv_sec -= now_usertime.tv_sec;
    dbg_fprintf((stderr, "to_stop_timer local.stop_usertime-now_usertime = {%lds, %ldus}",
                 usertime.tv_sec,
                 usertime.tv_usec));
    if (usertime.tv_sec >= 0)
      {
        remaining_time_ms = usertime.tv_sec*1000 + usertime.tv_usec/1000;
      }
    else                        /* overslept */
      {
        dbg_fprintf((stderr, "to_stop_timer overslept"));
        remaining_time_ms = 0;
      }
    
    local.alarm_clock_is_on = 0;
    return remaining_time_ms;
  }
#else

  {
    time_t tv_sec_abs, tv_sec_rel;
    long tv_nsec_abs, tv_nsec_rel;
    long remaining_time_ms;
    struct timespec now;

    rc = CLOCK_GETTIME(&now);
    if (rc != 0)
      {
        goto barf;
      }
    dbg_fprintf((stderr, "alarm_clock_off now-sleep_until %ldns", ((long)(now.tv_sec - local.sleep_until.tv_sec)*1000*1000*1000)
          +(long)(now.tv_nsec - local.sleep_until.tv_nsec)));
    /* sleep_until - now */
    if (local.sleep_until.tv_nsec < now.tv_nsec)
      {
        local.sleep_until.tv_sec -= 1; /* borrow */
        local.sleep_until.tv_nsec += (1000*1000*1000); /* one second worth of nanoseconds */
      }
    if (local.sleep_until.tv_sec < now.tv_sec) /* overslept */
      {
        long diff_ns;
        diff_ns =
          ((long)(now.tv_sec - local.sleep_until.tv_sec)*1000*1000*1000)
          +(long)(now.tv_nsec - local.sleep_until.tv_nsec);

        dbg_fprintf((stderr, "alarm_clock_off overslept %ldns", diff_ns));
        remaining_time_ms = 0;
      }
    else                        /* still time remaining until deadline */
      {
        remaining_time_ms =
          (local.sleep_until.tv_sec - now.tv_sec)*1000
          + 
          ((local.sleep_until.tv_nsec - now.tv_nsec)/(1000*1000));
      }
    
    local.alarm_clock_is_on = 0;
    return remaining_time_ms;
  }
#endif

 barf:
  display_error(rc, "barf", "alarm_clock_on");

  local.alarm_clock_is_on = 0;
  return -1;
}

/*------------------------------------------------------------------*/
/* foreign(to_start_timer, '$start_timer'(+term, [-integer])). */
long SPCDECL to_start_timer(
     SP_term_ref term		/* off | Integer>0 */
     )
{
  if (SP_is_atom(term))         /* off */
    {
      long time = alarm_clock_off(NULL);
      if (time < 0)
        {
          /* error */
          #if SICSTUS_REVISION_VERSION>0 && !SICSTUS_BETA_VERSION
          #error "Do proper error signalling/PM"
          #endif

          SP_syserror_clib("$start_timer/1", "alarm_clock_off");
          return -1;
        }
    }
  else
    {
      long time;

      SP_get_integer(term, &time);
      dbg_fprintf((stderr, "enter to_start_timer=%ldms", time));
      if (alarm_clock_on(time) != 0) /* error */
        {
          #if SICSTUS_REVISION_VERSION>0 && !SICSTUS_BETA_VERSION
          #error "Do proper error signalling/PM"
          #endif

          SP_syserror_clib("$start_timer/1", "alarm_clock_on");
          return -1;
        }
    }
  return 0;                     /* no error */
}


/* foreign(to_stop_timer, '$stop_timer'([-term])). */
SP_term_ref SPCDECL to_stop_timer(void) 
{
  SP_term_ref term = SP_new_term_ref();
  
  if (!local.alarm_clock_is_on)
    SP_put_atom(term, local.atom_off);
  else
    {
      int was_off;
      long time = alarm_clock_off(&was_off);

      if (time >= 0)            /* no error */
        {
          dbg_fprintf((stderr, "exiting to_stop_timer=%ldms", time));
          if (was_off)
            {
              SP_put_atom(term, local.atom_off);
            }
          else
            {
              SP_put_integer(term, time);
            }
        }
      else                      /* error */
        {
          /* It is off if error occurred. Also, the caller will call
             start_timer which will raise an error. */
          SP_put_atom(term, local.atom_off);
        }
    }
  return term;
}
#endif /* !SP_WIN32 */
#endif /* !LEGACY_TIMOUT */
/********************************************************************************/
