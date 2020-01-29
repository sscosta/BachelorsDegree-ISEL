/* OS/2 specific code for library(system) */

#define INCL_DOSPROCESS
#define INCL_DOSQUEUES
#define INCL_DOSERRORS
#define INCL_NOPMAPI
#define INCL_DOSFILEMGR
#include <os2.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sicstus/sicstus.h>

/* ---------------------------------------------------- */
#if __EMX__
char **environ;
char **_environ;

/* Call this function at system.dll init if linked with -Zsys
 * -Zso. Defines environ and _environ which are missing in C-lib.
*/
void spsysemxsetenv (void)
{
  TIB *tib_ptr;
  PIB *init_pib_ptr;
  char *startup_env, *s;
  int i;

  DosGetInfoBlocks (&tib_ptr, &init_pib_ptr);
  startup_env = init_pib_ptr->pib_pchenv;

  for (i=1, s=startup_env; *s != 0; s++, i++)
    while (*s != 0)
      s++;
  environ = (char **)malloc(i*sizeof(char *));

  for (i=0, s=startup_env; *s != 0; s++, i++)
    {
      environ[i] = s;
      while (*s != 0)
	s++;
    }
  environ[i] = NULL;
  _environ = environ;
}

#endif

/* ---------------------------------------------------- */
/* exec/3 */

#define PIPE_SIZE 4096
#define InvHand ((HFILE)-1)

typedef struct {
  HFILE pipe;
  int eof;
  int err;
  int cnt;
  unsigned char *buf;
  unsigned char *ptr;
} SP_exec_buf;

static SP_stream *stream_if PROTOTYPE((int code, HFILE ph, HFILE ch, int mode));
static int handle_if(long code, int mode, PHFILE chp, PHFILE php, int *stdflagp);
static int set_std_handle(HFILE Std, HFILE NewStd, PHFILE pSavedStd);
static void reset_std_handle(HFILE Std, HFILE SavedStd);
static void doserror(char *pred, char *syscall, APIRET rc);

#define ExecError(Call,RC) {doserror("exec/3",Call,RC);goto error;}
#define Error(Pred,Call,RC) {doserror(Pred,Call,RC);goto error;}

long sp_exec(char *command,
	     long sinc, long soutc, long serrc,
	     SP_stream **sin, SP_stream **sout, SP_stream **serr,
	     long *pidloc)
{
  HFILE StdinRd, StdinWr, StdoutWr, StdoutRd, StderrWr, StderrRd;
  int stdflag = 0;
    
  if (handle_if(sinc, 0, &StdinRd, &StdinWr, &stdflag) < 0)
    goto error;
  if (handle_if(soutc, 1, &StdoutWr, &StdoutRd, &stdflag) < 0)
    goto error;
  if (handle_if(serrc, 2, &StderrWr, &StderrRd, &stdflag) < 0)
    goto error;

  /* Now create the child process. */
  {
    HFILE SavedStdin, SavedStdout, SavedStderr;
    UCHAR uchLoadError[CCHMAXPATH] = {0};
    RESULTCODES ChildRC = {0};
    ULONG creation_flags;
    char prog_name[CCHMAXPATH];
    APIRET rc;
    int i, rc1;

    /* Set std handles for child to inherit */
    if (set_std_handle((HFILE)0, StdinRd, &SavedStdin) < 0)
      goto error;
    if ((rc1=set_std_handle((HFILE)1, StdoutWr, &SavedStdout)) < 0)
      goto error1;
    if ((rc1=set_std_handle((HFILE)2, StderrWr, &SavedStderr)) < 0)
      goto error2;

    /*    if (!stdflag)
      creation_flags = EXEC_BACKGROUND;
    else */
      creation_flags = EXEC_ASYNCRESULT;

    {
      int i;

      for (i=0; command[i] != ' ' && command[i] != '\0'; i++)
	prog_name[i] = command[i];
      prog_name[i] = '\0';
	/* append .exe if no extension */
      for (; i>=0 && prog_name[i] != '.' && prog_name[i] != '\\'; i--);
      if (prog_name[i] != '.')
	strcat(prog_name, ".exe");
    }

    /* Create the child process. */
    rc = DosExecPgm(uchLoadError,
		    sizeof(uchLoadError),
		    creation_flags,
		    command,
		    NULL,
		    &ChildRC,
		    prog_name);

    /* Reset std handles to previous */
    reset_std_handle((HFILE)2, SavedStderr);
  error2:
    reset_std_handle((HFILE)1, SavedStdout);
  error1:
    reset_std_handle((HFILE)0, SavedStdin);
    if (rc1 < 0)
      goto error;

    if (rc != NO_ERROR)
      ExecError("DosExecPgm", rc);
    
    *pidloc = ChildRC.codeTerminate;
  }

  *sin = stream_if(sinc, StdinWr, StdinRd, 0);
  *sout = stream_if(soutc, StdoutRd, StdoutWr, 1);
  *serr = stream_if(serrc, StderrRd, StderrWr, 2);

  return 0;
 error:
  return -1;
}

/*------------------------------------------------------------------*/
static int handle_if(long code, int mode,
		     PHFILE chp, PHFILE php,
		     int *stdflagp)
{
  APIRET rc;

  switch (code)
    {
    case 0:
      *chp = InvHand;
      *stdflagp = 1;
      break;
    case 2:
      {
	/* Create a pipe for the child's STDIN. */
	HFILE hCopy;
	PHFILE rhp, whp;
	
	if (mode==0)
	  rhp = chp, whp = php;
	else
	  whp = chp, rhp = php;

	rc = DosCreatePipe(rhp, whp, PIPE_SIZE);
	if (rc != NO_ERROR)
	  ExecError("DosCreatePipe", rc);
	rc = DosSetFHState(*php, OPEN_FLAGS_NOINHERIT);
	if (rc != NO_ERROR)
	  ExecError("DosSetFHState", rc);
      }
      break;
    case 1:
      {
	ULONG ulAction;
	
	rc = DosOpen("nul", chp, &ulAction, 0, 0,
		     OPEN_ACTION_FAIL_IF_NEW|OPEN_ACTION_OPEN_IF_EXISTS,
		     OPEN_ACCESS_READWRITE|OPEN_SHARE_DENYNONE,
		     NULL); 
	if (rc != NO_ERROR)
	  ExecError("Open NUL", rc);
	break;
      }
    }
  return 0;
 error:
  return -1;
}

/*------------------------------------------------------------------*/
static int SPCDECL spi_getc(SP_exec_buf *s)
{
  ULONG i;

  if (s->eof || s->err)
    return -1;

  if (s->cnt-- > 0)
    return *s->ptr++;

  s->ptr = s->buf;
  if (DosRead(s->pipe, s->buf, BUFSIZ, &i) != NO_ERROR)
    {
      s->err = 1;
      return -1;
    }
  if (i == 0)
    {
      s->eof = 1;
      return -1;
    }

  s->cnt = i-1;      
  return *s->ptr++;
}

static int SPCDECL spi_putc(int x, SP_exec_buf *s)
{
  ULONG i=0;

  s->buf[0] = x;
  for (; i==0;)
    if (DosWrite(s->pipe, s->buf, 1, &i) != NO_ERROR)
      {
	s->err = 1;
	return -1;
      }
  return x;
}

static int SPCDECL spi_flush(SP_exec_buf *s)
{
  return 0;
}

static int SPCDECL spi_eof(SP_exec_buf *s)
{
  return s->eof;
}

static void SPCDECL spi_clrerr(SP_exec_buf *s)
{
  s->err = 0;
}

static int SPCDECL exec_close(SP_exec_buf *s)
{
  int rval = 0;

  if (DosClose(s->pipe) != NO_ERROR)
    rval = -1;
  SP_free(s->buf);
  SP_free((char *)s);
  return rval;
}

static SP_stream *new_exec_stream(HFILE handle, HFILE mode)
{
  SP_stream *s;
  SP_exec_buf *exec_buf;
  
  exec_buf = (SP_exec_buf *)SP_malloc(sizeof(SP_exec_buf));
  exec_buf->pipe = handle;
  exec_buf->buf = SP_malloc(BUFSIZ);
  exec_buf->ptr = exec_buf->buf;
  exec_buf->cnt = 0;
  exec_buf->err = 0;
  exec_buf->eof = 0;

  if (mode == 0)
    {
      SP_make_stream_context(
		   (ANYPOINTER)exec_buf,
		   NULL,
		   spi_putc,
		   spi_flush,
		   NULL,
		   spi_clrerr,
		   exec_close,
		   &s,
		   SP_WCX_FLAG,
		   SP_STREAMHOOK_LIB);
    }
  else
    {
      SP_make_stream_context(
		     (ANYPOINTER)exec_buf,
		     spi_getc,
		     NULL,
		     spi_flush,
		     spi_eof,
		     spi_clrerr,
		     exec_close,
		     &s,
		     SP_WCX_FLAG,
		     SP_STREAMHOOK_LIB);
    }
  return s;
}

static int set_std_handle(HFILE Std, HFILE NewStd, PHFILE pSavedStd)
{
  APIRET rc;

  *pSavedStd = InvHand;
  if (NewStd != InvHand)
    {
      /* Save a handle referring to the std stream */
      if ((rc=DosDupHandle(Std, pSavedStd)) != NO_ERROR)
	ExecError("DosDupHandle 1", rc);
      /* Make the copy non-inheritable */
      if ((rc=DosSetFHState(*pSavedStd, OPEN_FLAGS_NOINHERIT)) != NO_ERROR)
	ExecError("DosSetFHState", rc);
      /* Set the std handle to the pipe/nul, for the child to inherit */
      if ((rc=DosDupHandle(NewStd, &Std)) != NO_ERROR)
	ExecError("DosDupHandle 2", rc);
    }
  return 0;
error:
  return -1;
}

static void reset_std_handle(HFILE Std, HFILE SavedStd)
{
  if (SavedStd != InvHand)
    {
      /* Reinstate saved handle */
      DosDupHandle(SavedStd, &Std);
      /* Close the copy */
      DosClose(SavedStd);
    }
}

static SP_stream *stream_if(int code,
			    HFILE ph,
			    HFILE ch,
			    int mode)
{
  switch (code)
    {
    case 2:			/* pipe(_) */
      DosClose(ch);
      return new_exec_stream(ph, mode);
    case 1:			/* null */
      DosClose(ch);
    case 0:			/* std */
    default:
      return NULL;
    }
}

/*------------------------------------------------------------------*/
long sp_getpid(void)
{
  APIRET rc;
  PPIB ppib;
  PTIB ptib;

  rc = DosGetInfoBlocks(&ptib, &ppib);
  if (rc != NO_ERROR)
    Error("pid/1", "DosGetInfoBlocks()", rc);
  return ppib->pib_ulpid;
 error:
  return 0;
}

/*------------------------------------------------------------------*/
void sp_sleep(double secs)
{
  APIRET rc;

  rc = DosSleep((unsigned int)(1000.0*secs));
  if (rc != NO_ERROR)
    Error("sleep/1", "DosSleep()", rc);
 error:{}
}

/*------------------------------------------------------------------*/
void sp_kill(long pid, long sig)
{
  HFILE proc_handle;
  APIRET rc;

  rc = DosKillProcess(DKP_PROCESS, (PID)pid);
  if (rc != NO_ERROR)
    Error("kill/2", "DosKillProcess()", rc);
 error:{}
}

/*------------------------------------------------------------------*/

long sp_wait(long pid)
{
  RESULTCODES ChildRC;
  PID EPid;
  APIRET rc;

  rc = DosWaitChild(DCWA_PROCESS, DCWW_WAIT, &ChildRC, &EPid, (PID)pid);
  if (rc != NO_ERROR)
    Error("wait/2", "DosWaitChild()", rc);
  return ChildRC.codeTerminate;
 error:
  return 0;
}

/*------------------------------------------------------------------*/

static void doserror(char *pred, char *syscall, APIRET rc)
{
  char syserrbuf[32];

  sprintf(syserrbuf, "rc=%u", rc);
  sp_syserror(API_CLIB, syserrbuf, pred, syscall);
}

/*------------------------------------------------------------------*/
#if __IBMC__

SP_term_ref sp_directory_files(dir)
     char *dir;
{
  HDIR          hdirFindHandle = HDIR_SYSTEM;
  FILEFINDBUF3  FindBuffer     = {0};
  ULONG         ulResultBufLen = sizeof(FILEFINDBUF3);
  ULONG         ulFindCount    = 1;
  APIRET        rc             = NO_ERROR;
  char buf[CCHMAXPATH];
  SP_term_ref car = SP_new_term_ref(), cdr = SP_new_term_ref();

  strcpy(buf, SP_to_os(dir, WCX_FILE));
  strcat(buf, "\\*.*");
  rc = DosFindFirst(buf,
		    &hdirFindHandle,
		    FILE_ARCHIVED|FILE_DIRECTORY|FILE_SYSTEM|
		    FILE_HIDDEN|FILE_READONLY,
		    &FindBuffer,
		    ulResultBufLen,
		    &ulFindCount,
		    FIL_STANDARD);

  if (rc != NO_ERROR && rc != ERROR_NO_MORE_FILES)
    Error("directory_files/2", "DosFindFirst()", rc)
  else if (rc == NO_ERROR)
    {
      for (;;)
	{
	  strlwr(FindBuffer.achName);
	  SP_put_string(car, SP_from_os(FindBuffer.achName, WCX_FILE));
	  SP_cons_list(cdr, car, cdr);
	  ulFindCount = 1;
	  rc = DosFindNext(hdirFindHandle,
			   &FindBuffer,
			   ulResultBufLen,
			   &ulFindCount);
	  if (rc != NO_ERROR && rc != ERROR_NO_MORE_FILES)
	    Error("directory_files/2", "DosFindNext()", rc)
	  else if (rc == ERROR_NO_MORE_FILES)
	    break;
	}
      rc = DosFindClose(hdirFindHandle);
      if (rc != NO_ERROR && rc != ERROR_NO_MORE_FILES)
	Error("directory_files/2", "DosFindClose()", rc);
    }
error:
  return cdr;
}

#endif
