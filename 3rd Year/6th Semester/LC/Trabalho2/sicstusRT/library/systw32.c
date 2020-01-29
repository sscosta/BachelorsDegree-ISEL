

#include <sicstus/config.h>
#include <sicstus/sicstus.h>
#include "system_glue.h"

/* Win32 specific code for library(system) */

/* No-op on other platforms. Makes it easier for the Makefile. */
#if SP_WIN32
#include <stdio.h>
#include <windows.h>

extern void sp_win32_hostname(char *, size_t);

/* exec/3 - Based on example code in Win32 SDK. */

typedef struct {
  HANDLE pipe;
  int eof;
  int err;
  int cnt;
  unsigned char buf[BUFSIZ];   /* [PM] 3.9b4 used to be unsigned char*buf; */
  unsigned char *ptr;
  void (SPCDECL *deallocator)(void*); /* [PM] 3.9b4 pointer to SP_free */
} SP_exec_buf;

static SP_stream *stream_if PROTOTYPE((int, HANDLE, HANDLE, int));
static int handle_if(long code, DWORD mode, LPHANDLE chp, LPHANDLE php, int *stdflagp);

#define ExecError(Call) {SP_syserror_win32("exec/3",Call);goto error;}
#define Error(Pred,Call) {SP_syserror_win32(Pred,Call);goto error;}

long SPCDECL sp_exec(char *command,
	     long sinc, long soutc, long serrc,
	     SP_stream **sin, SP_stream **sout, SP_stream **serr,
	     long *pidloc)
{
  HANDLE hStdinRd, hStdinWr, hStdoutWr, hStdoutRd, hStderrWr, hStderrRd;
  int stdflag = 0;
    
  if (handle_if(sinc, STD_INPUT_HANDLE, &hStdinRd, &hStdinWr, &stdflag) < 0)
    goto error;
  if (handle_if(soutc, STD_OUTPUT_HANDLE, &hStdoutWr, &hStdoutRd, &stdflag) < 0)
    goto error;
  if (handle_if(serrc, STD_ERROR_HANDLE, &hStderrWr, &hStderrRd, &stdflag) < 0)
    goto error;

  /* Now create the child process. */
  {
    PROCESS_INFORMATION piProcInfo;
    STARTUPINFO siStartInfo;
    DWORD creation_flags;

    /* Set up members of STARTUPINFO structure. */
    siStartInfo.cb = sizeof(STARTUPINFO);
    siStartInfo.lpReserved = NULL;
    siStartInfo.lpReserved2 = NULL;
    siStartInfo.cbReserved2 = 0;
    siStartInfo.lpDesktop = NULL;
    siStartInfo.lpTitle = NULL;
    siStartInfo.dwFlags = STARTF_USESTDHANDLES;
    siStartInfo.hStdInput = hStdinRd;
    siStartInfo.hStdOutput = hStdoutWr;
    siStartInfo.hStdError = hStderrWr;

    if (!stdflag)
      creation_flags = DETACHED_PROCESS;
    else
      creation_flags = 0;

    /* Create the child process. */
    if (CreateProcess(NULL,
        command,       /* command line                       */
        NULL,          /* process security attributes        */
        NULL,          /* primary thread security attributes */
        TRUE,          /* handles are inherited              */
        creation_flags, /* creation flags                     */
        NULL,          /* use parent's environment           */
        NULL,          /* use parent's current directory     */
        &siStartInfo,  /* STARTUPINFO pointer                */
        &piProcInfo    /* receives PROCESS_INFORMATION       */
		      ) == FALSE)
      ExecError("Create process");
    
    *pidloc = piProcInfo.dwProcessId;
  }

  *sin = stream_if(sinc, hStdinWr, hStdinRd, STD_INPUT_HANDLE);
  *sout = stream_if(soutc, hStdoutRd, hStdoutWr, STD_OUTPUT_HANDLE);
  *serr = stream_if(serrc, hStderrRd, hStderrWr, STD_ERROR_HANDLE);

  return 0;
 error:
  return -1;
}

/*------------------------------------------------------------------*/
static int handle_if(long code, DWORD mode,
		     LPHANDLE chp, LPHANDLE php,
		     int *stdflagp)
{
  switch (code)
    {
    case 0:
      *chp = GetStdHandle(mode);
      *stdflagp = 1;
      break;
    case 2:
      {
	/* Create a pipe for the child's STDIN. */
	SECURITY_ATTRIBUTES saAttr;
	HANDLE hCopy;
	LPHANDLE rhp, whp;

	/* Ensure pipe handles are inherited. */
	saAttr.nLength = sizeof(SECURITY_ATTRIBUTES);
	saAttr.bInheritHandle = TRUE;
	saAttr.lpSecurityDescriptor = NULL;
	
	if (mode==STD_INPUT_HANDLE)
	  rhp = chp, whp = php;
	else
	  whp = chp, rhp = php;

	if (!CreatePipe(rhp, whp, &saAttr, 0))
	  ExecError("Pipe creation");

	/* Dup. the parent handle to the pipe so it is not inherited. */
	if (!DuplicateHandle(GetCurrentProcess(), *php,
			     GetCurrentProcess(), &hCopy, 0,
			     FALSE,       /* not inherited */
			     DUPLICATE_SAME_ACCESS))
	  ExecError("Duplicating parent handle");

	if (!CloseHandle(*php))
	  ExecError("Closing handle");
  
	*php = hCopy;
      }
      break;
    case 1:
      {
	*chp = CreateFile("NUL",
			  GENERIC_READ|GENERIC_WRITE,
			  FILE_SHARE_READ|FILE_SHARE_WRITE,
			  NULL,
			  OPEN_EXISTING,
			  0,
			  NULL);
	if (*chp == INVALID_HANDLE_VALUE)
	  ExecError("Open NUL");
	break;
      }
    }
  return 0;
 error:
  return -1;
}

/*------------------------------------------------------------------*/
static int SPCDECL spi_getc(void *handle)
{
  SP_exec_buf *s = (SP_exec_buf *)handle;
  DWORD i;

  if (s->eof || s->err)
    return -1;

  if (s->cnt-- > 0)
    return *s->ptr++;

  s->ptr = s->buf;
  if (ReadFile(s->pipe, s->buf, BUFSIZ, &i, NULL) == FALSE)
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

static int SPCDECL spi_putc(char x, void *handle)
{
  SP_exec_buf *s = (SP_exec_buf *)handle;
  DWORD i=0;

  s->buf[0] = x;
  for (; i==0;)
    if (WriteFile(s->pipe, s->buf, 1, &i, NULL) == FALSE)
      {
	s->err = 1;
	return -1;
      }
  return x;
}

static int SPCDECL spi_flush(void *handle)
{
  return 0;
}

static int SPCDECL spi_eof(void *handle)
{
  SP_exec_buf *s = (SP_exec_buf *)handle;
  return s->eof;
}

static void SPCDECL spi_clrerr(void *handle)
{
  SP_exec_buf *s = (SP_exec_buf *)handle;
  s->err = 0;
}

static int SPCDECL exec_close(void *handle)
{
  SP_exec_buf *s = (SP_exec_buf *)handle;
  int rval = 0;

  if (CloseHandle(s->pipe) == FALSE)
    rval = -1;
  #if 0                         /* [PM] 3.9b4 now inlined */
  SP_free(s->buf);
  #endif

  #if 1
  s->deallocator(s);            /* [PM] 3.9b4 SP API is not available here if SP_SINGLE_THREADED */
  #else
  SP_free((char *)s);
  #endif
  return rval;
}

static SP_stream *new_exec_stream(HANDLE handle, int mode)
{
  SP_stream *s;
  SP_exec_buf *exec_buf;
  
  exec_buf = (SP_exec_buf *)SP_malloc(sizeof(SP_exec_buf));
  exec_buf->pipe = handle;
  #if 0                         /* [PM] now part of SP_exec_buf */
  exec_buf->buf = SP_malloc(BUFSIZ);
  #endif
  exec_buf->ptr = &exec_buf->buf[0];
  exec_buf->cnt = 0;
  exec_buf->err = 0;
  exec_buf->eof = 0;
  /* [PM] 3.9b4 SP API is not available in stream functions if we use SP_SINGLE_THREADED */
  exec_buf->deallocator = (SP_free);

  if (mode == STD_INPUT_HANDLE)
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

static SP_stream *stream_if(int code,
			    HANDLE ph,
			    HANDLE ch,
			    int mode)
{
  switch (code)
    {
    case 2:			/* pipe(_) */
      CloseHandle(ch);
      return new_exec_stream(ph, mode);
    case 1:			/* null */
      CloseHandle(ch);
    case 0:			/* std */
    default:
      return NULL;
    }
}

/*------------------------------------------------------------------*/
void SPCDECL sp_sleep(double secs)
{
  Sleep((unsigned int)(1000.0*secs));
}

/*------------------------------------------------------------------*/
void SPCDECL sp_kill(long pid, long sig)
{
  HANDLE proc_handle = NULL;
 
  proc_handle = OpenProcess(PROCESS_TERMINATE|STANDARD_RIGHTS_REQUIRED,
			    FALSE, pid);
  errno = 0;                    /* [PM] 3.8.5 but should really use GetLastError() on error */
  if (proc_handle == NULL)
    Error("kill/2", "OpenProcess()");
  if (TerminateProcess(proc_handle, -1) == FALSE)
    Error("kill/2", "TerminateProcess()");
 error:
  /* [PM] 3.8.5 always close the handle */
  if (proc_handle != NULL)
    {
      CloseHandle(proc_handle);
    }
}

/*------------------------------------------------------------------*/

/* [PM] 3.9 pass in local.hostnamebuf from system.c */
void sp_win32_hostname(char *hostnamebuf, size_t bufsize)
{
  if (GetComputerName(hostnamebuf, &bufsize) == FALSE)
    {
      SP_syserror_win32("host_name/1", "GetComputerName()");
      hostnamebuf[0] = '\0';
    }
}

/*------------------------------------------------------------------*/

long SPCDECL sp_wait(long pid)
{
  HANDLE proc_handle;
  DWORD exitcode;
 
  proc_handle = OpenProcess(SYNCHRONIZE|PROCESS_QUERY_INFORMATION|
			    STANDARD_RIGHTS_REQUIRED,
			    FALSE, pid);
  if (proc_handle == NULL)
    Error("wait/1", "OpenProcess()");
  if (WaitForSingleObject(proc_handle, INFINITE) == WAIT_FAILED)
    Error("wait/1", "WaitForSingleObject()");
  if (GetExitCodeProcess(proc_handle, &exitcode) == FALSE)
    Error("wait/2", "GetExitCodeProcess()");
  return exitcode;
 error:
  return 0;
}

/*------------------------------------------------------------------*/
#endif /* SP_WIN32 */
