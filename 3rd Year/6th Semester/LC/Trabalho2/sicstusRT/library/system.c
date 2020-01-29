/* Copyright (C) 1995, Swedish Institute of Computer Science. */
#if defined(__alpha) && defined (__osf__) /* what is the right test */
/* [PM] April 2000, on Compaq Tru64 (a.k.a Digital Unix on alpha)
        needed for unistd.h to prototype gethostid (among others). An
        issue for gethostid since the return value is long in the
        prototype which is different from the default return type
        (int) on Alpha. Consider using -D_XOPEN_SOURCE_EXTENDED for
        all sources on this platform.  (Note that
        _XOPEN_SOURCE_EXTENDED must have a value to avoid syntax
        errors. Just defining it is not enough.)  Also, must be
        defined before including *any* headers
*/

#define _XOPEN_SOURCE_EXTENDED 1
/* 
   [PM] 3.9b4 My guess is that [PM] did not know what he was
              doing. Should consult the relevant X-Open standards.
   [PM] 3.8.4 [PM] strangely, explicitly defining
   _XOPEN_SOURCE_EXTENDED appears to interfere with the default
   definition of the same symbol thus *not*  including the proper
   prototype for gethostid from unistd.h.
#else
   #define _XOPEN_SOURCE_EXTENDED 1
*/
#endif

#include <sicstus/config.h>
#include <sicstus/sicstus.h>

#define SP_MSDOS __DJGPP__

/*
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

#if __EMX__
#define SP_OS2 1
#endif

#if __IBMC__ && __OS2__
#define SP_OS2 1
#endif

#if __GNUC__ && __WIN32__
#define SP_WIN32 1
#define environ _environ
#endif

#if (_SYSTYPE_SVR4 || SYSTYPE_SVR4 || __SYSTYPE_SVR4__ || __SVR4) && !__svr4__
# define __svr4__ 1
#endif
*/

#if !SP_WIN32 && !(SP_OS2 && !__EMX__) && !MACINTOSH
#include <sys/param.h>
#endif

#if 0
#if !MACINTOSH
#include <sys/types.h>
#endif
#endif

#if HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#if HAVE_SIGNAL_H
#include <signal.h>             /* [PM] 3.9 kill() */
#endif

#if HAVE_SYS_WAIT_H
#include <sys/wait.h>           /* [PM] 3.9 wait() */
#endif

#if !SP_WIN32 && !(SP_OS2 && !__EMX__) && !MACINTOSH
#include <sys/time.h>
#endif
#if !MACINTOSH
#include <sys/stat.h>
#endif

#if SP_WIN32 || (SP_OS2 && !__EMX__)
# if __WATCOMC__ || _MSC_VER || __IBMC__
#include <direct.h>
# endif
# if __BORLANDC__ || _MSC_VER || __IBMC__ || __GNUC__
#include <io.h>
# endif
# if __BORLANDC__ || __GNUC__
#include <dirent.h>
#include <dir.h>
# endif
# if _MSC_VER
#include <errno.h>
# endif
#include <process.h>
#else

# if !MACINTOSH && !__NeXT__
#include <dirent.h>
# endif
#if __NeXT__
#include <sys/dir.h>
# endif
# if MACINTOSH
#include "mac_dir.h"
# endif

#endif

#if !__BORLANDC__ && !_MSC_VER && !__IBMC__
#include <unistd.h>
#endif
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <stdio.h>
#ifndef O_RDWR
# include <fcntl.h>
#endif
#if __svr4__
#include <sys/systeminfo.h>
#endif
#if __hpux__ || hpux || __hpux
#include <sys/utsname.h>
#endif


/*------------------------------------------------------------------*/
/* Exported functions */
#if 1
#include "system_glue.h"
#else
extern time_t sp_time PROTOTYPE((void));
extern void sp_datime PROTOTYPE((time_t,long *,long *,long *,long *,long *,long *));
extern void sp_del_file PROTOTYPE((char *, SP_term_ref));
extern void sp_del_dir PROTOTYPE((char *, SP_term_ref));
extern long sp_getenv PROTOTYPE((char *, char **));
extern long sp_setenv PROTOTYPE((char *, char *));
extern long sp_argenv PROTOTYPE((long, char **, char **));
extern long sp_exec PROTOTYPE((char *,long,long,long,SP_stream **,SP_stream **,SP_stream **,long *));
extern long sp_access PROTOTYPE((char *, long));
extern char *sp_host_id PROTOTYPE((void));
extern char *sp_host_name PROTOTYPE((void));
extern char *sp_mktemp PROTOTYPE((char *));
extern char *sp_tmpnam PROTOTYPE((void));
extern SP_stream *sp_popen PROTOTYPE((char *, char *));
extern void sp_rename PROTOTYPE((char *, char *));
extern SP_term_ref sp_directory_files PROTOTYPE((char *));
extern SP_term_ref sp_file_property PROTOTYPE((char *, long));
extern void sp_make_dir PROTOTYPE((char *));
extern void sp_system0 PROTOTYPE((void));
extern long sp_system2 PROTOTYPE((char *));
extern void sp_shell0 PROTOTYPE((void));
extern long sp_shell2 PROTOTYPE((char *));
extern long sp_wait PROTOTYPE((long));
extern void SPCDECL system_init PROTOTYPE((int));
extern void SPCDECL system_deinit PROTOTYPE((int));
#endif

#if (!SP_WIN32 && !SP_OS2 && !MACINTOSH)

/* [PM] 3.9b4 SUSv2 says this exists but nothing about a header. Ditto for Solaris */
extern char **environ;		/* missing in <???.h> */

#if 0                           /* [PM] 3.9b4 SUSv2 says stdlib.h, lets see if anyone objects */
extern char *getenv();		/* missing in <???.h> */
#endif

#endif /* (!SP_WIN32 && !SP_OS2 && !MACINTOSH) */

#if 0                           /* [PM] 3.9b4 SUSv2 says it is declared in stdio.h */
extern FILE *popen PROTOTYPE((/* const char *, const char * */));
#endif

#if SP_WIN32
/* [PM] 3.8.7,3.9 lstat does not exist on Win32 but neither does
        symbolic links.
       "lstat  is  identical to stat, except in the case of a sym­
        bolic link, where the link itself is stat-ed, not the file
        that it refers to."
*/
#define lstat stat

extern void sp_win32_hostname(char *, size_t);
#endif /* SP_WIN32 */



/*------------------------------------------------------------------*/

#ifndef MAXPATHLEN
#define MAXPATHLEN 1024
#endif

#if SP_WIN32 || (SP_OS2 && !__EMX__)
#define BACKSLASH_PATHS 1
#endif

#if !BACKSLASH_PATHS 
#define Native_path(P) SP_to_os((P), WCX_FILE)
#define Normal_path(P) SP_to_os((P), WCX_FILE)
#else
#define Native_path(P) native_path(SP_to_os((P), WCX_FILE))
#define Normal_path(P) normal_path(SP_to_os((P), WCX_FILE))
#endif

#if SP_MSDOS || SP_WIN32 || SP_OS2
#define Native_path1(P) native_path(SP_to_os((P), WCX_FILE))
#else
#define Native_path1(P) (SP_to_os((P), WCX_FILE))
#endif

struct system_state {
  SP_atom atm_regular;
  SP_atom atm_directory;
  SP_atom atm_fifo;
  SP_atom atm_symlink;
  SP_atom atm_socket;
  SP_atom atm_unknown;
#if SP_MSDOS || SP_WIN32 || SP_OS2 
  char native_path_buffer[MAXPATHLEN];
  char normal_path_buffer[MAXPATHLEN];
#endif
  char env_buffer[512];  /*  = 2*256 for WCX */
  char hostnamebuf[MAXPATHLEN];
};

#if !(SPDLL || SP_DYNAMIC_FOREIGN_RESOURCE)
#if SP_SINGLE_THREADED
#if __GNUC__
#warning "SP_SINGLE_THREADED is incompatible with static foreign resources"
#elif _MSC_VER
#pragma message ("WARNING: SP_SINGLE_THREADED is incompatible with static foreign resources")
#endif

#endif

#undef SP_SINGLE_THREADED
#endif

#if SP_SINGLE_THREADED

/* [PM] 3.9b4 This holds the local state for whatever SICStus run-time
   is currently calling one of the system procedures. */
static struct system_state *local_state;

#define local (*local_state) /* so that local.foo still works as expected. */

/* [PM] 3.9b4 
   This is called by the glue each time any procedure in system is
   called or exited. 

   This is overkill. We could have defined local as
   ((struct system_state*)*SP_foreign_stash()). In that case
   sp_context_switch_hook_system could have been made a no-op (or we
   could compile the glue with SP_NO_CONTEXT_SWITCH_HOOK).

*/

void SPCDECL sp_context_switch_hook_system(int entering)
{
#if DBG>1
  fprintf(stderr, "\n** DBG sp_context_switch_hook_system(%s)\n",
          (  entering == 1 ? "entering"
             : (entering == 0 ? "exiting"
                :                  "???"
                )));
  fflush(stderr);
#endif /* DBG */
  {
    void **stash = SP_foreign_stash();

    if (entering)
      {
        local_state = (struct system_state*) *stash;
        if (local_state==NULL)    /* first time, right before system_init() */
          {
#if DBG>1
            fprintf(stderr, "\n** DBG sp_context_switch_hook_system() allocating state\n");
            fflush(stderr);
#endif /* DBG */

            local_state = (struct system_state*)SP_malloc(sizeof(struct system_state));
          }
      }
    else                          /* exiting resource */
      {
        *stash = (void*)local_state;
#if DBG
        local_state = (struct system_state *) 0xDEC0DEAD;
#endif/* DBG */
      }
  }
}


#elif MULTI_SP_AWARE
#error "MULTI_SP_AWARE not supported"
#else  /* !(SP_SINGLE_THREADED || MULTI_SP_AWARE) */
static struct system_state local;
#endif

#if SP_MSDOS || SP_WIN32 || SP_OS2 

static char *native_path(char *path)
{
  int i;

  if (path == NULL)
    return NULL;
  strcpy(local.native_path_buffer, path);
  for (i=strlen(local.native_path_buffer); i--; )
    if (local.native_path_buffer[i] == '/')
      local.native_path_buffer[i] = '\\';
  return local.native_path_buffer;
}

static char *normal_path(char *path)
{
  int i;

  if (path == NULL)
    return NULL;
  strcpy(local.normal_path_buffer, path);
  for (i=strlen(local.normal_path_buffer); i--; )
    if (local.normal_path_buffer[i] == '\\')
      local.normal_path_buffer[i] = '/';
  return local.normal_path_buffer;
}

#endif

/*------------------------------------------------------------------*/
void SPCDECL system_init(int when)
{
  (void)when;                   /* [PM] 3.9b5 avoid -Wunused */

  (void)SP_register_atom(local.atm_regular = SP_atom_from_string("regular"));
  (void)SP_register_atom(local.atm_directory = SP_atom_from_string("directory"));
  (void)SP_register_atom(local.atm_fifo = SP_atom_from_string("fifo"));
  (void)SP_register_atom(local.atm_symlink = SP_atom_from_string("symlink"));
  (void)SP_register_atom(local.atm_socket = SP_atom_from_string("socket"));
  (void)SP_register_atom(local.atm_unknown = SP_atom_from_string("unknown"));
#if SP_OS2 && __EMX__
  { 
    extern void spsysemxsetenv PROTOTYPE((void));
    spsysemxsetenv();
  }
#endif
}

void SPCDECL system_deinit(int when)
{
  (void)when;                   /* [PM] 3.9b5 avoid -Wunused */

  (void)SP_unregister_atom(local.atm_regular);
  (void)SP_unregister_atom(local.atm_directory);
  (void)SP_unregister_atom(local.atm_fifo);
  (void)SP_unregister_atom(local.atm_symlink);
  (void)SP_unregister_atom(local.atm_socket);
  (void)SP_unregister_atom(local.atm_unknown);
}

/*------------------------------------------------------------------*/
time_t SPCDECL sp_time(void)
{
  return time((time_t *) 0);
}

/*------------------------------------------------------------------*/
void SPCDECL sp_datime(clock, year, month, day, hour, min, sec)
     time_t clock;
     long *year, *month, *day, *hour, *min, *sec;
{
  struct tm *tm;

  tm = localtime(&clock);
  
  *year  = tm->tm_year + 1900;
  *month = tm->tm_mon  +    1;
  *day   = tm->tm_mday;
  *hour  = tm->tm_hour;
  *min   = tm->tm_min;
  *sec   = tm->tm_sec;
}

/*------------------------------------------------------------------*/
long SPCDECL sp_getenv(variable, value)
     char *variable;
     char **value;
{
  char *p = (char *)getenv(SP_to_os(variable,WCX_OPTION));
  
  *value = "";
  if (!p)
    return -1;
  *value = SP_from_os(p,WCX_OPTION);
  return 0;
}

#if 0 /* [PM] setenv is not available on all platforms (e.g., solaris) */
/*
[PM] See set_app_paths() in fli_dl.c for how to implement this using
     SUSv2 putenv without memory leaks.  Also, on Win32 should use
     both _putenv and SetEnvironmentVariable (see set_app_paths() in
     win32.c for details).
*/
long sp_setenv(variable, value)
     char *variable;
     char *value;
{
  int overwrite = 1;
  /* zero on success */
  return setenv(SP_to_os(variable,WCX_OPTION), SP_to_os(value,WCX_OPTION), overwrite);
}
#endif

/*------------------------------------------------------------------*/

long SPCDECL sp_argenv(index, variable, value)
     long index;
     char **variable;
     char **value;
{
#ifdef MACINTOSH
  char *p = 0;
#else
  char *p = environ[index];
#endif
  *variable = "";
  *value = "";
  if (!p)
    return -1;
  strcpy(local.env_buffer, SP_from_os(p, WCX_OPTION));
  p = local.env_buffer;
  while (*p++ != '=')
    ;
  p[-1] = 0;
  *variable = local.env_buffer;
  *value = p;
  return 0;
}

/*------------------------------------------------------------------*/

char * SPCDECL sp_host_name(void)
{
#if SP_WIN32
  sp_win32_hostname(local.hostnamebuf, sizeof(local.hostnamebuf));

#elif SP_OS2 || SP_MSDOS || MACINTOSH
#error "This platform is not supported"
  /* [PM] 3.9 Just tell future porters about the problem.
    SPRM 393 says:
    For os2, you should have called
      getenv("HOSTNAME")
    or simply
      #include <netdb.h>
      gethostname(hostnamebuf, sizeof(hostnamebuf));
  */
  {
    char *c;

    if ((c = getenv("SYSTEMNAME")) != NULL)
      strcpy(local.hostnamebuf, c);
    else
      strcpy(local.hostnamebuf, "noname");
  }
#elif __svr4__
  sysinfo(SI_HOSTNAME, local.hostnamebuf, sizeof(local.hostnamebuf));
#else
  gethostname(local.hostnamebuf, sizeof(local.hostnamebuf));
#endif

  return local.hostnamebuf;
}

char * SPCDECL sp_host_id(void)
{
#if SP_MSDOS || SP_WIN32 || SP_OS2 || MACINTOSH
/* For Win32, host id could be found, fix later! */
  strcpy(local.hostnamebuf, "noid");
#else
#if __svr4__
  sysinfo(SI_HW_SERIAL, local.hostnamebuf, MAXPATHLEN);
#else
#if __hpux__ || hpux || __hpux
  struct utsname uts;

  uname(&uts);
  strcpy(local.hostnamebuf, uts.idnumber);
#else
  sprintf(local.hostnamebuf, "%ld", gethostid());
#endif
#endif
#endif
  return local.hostnamebuf;
}

/*------------------------------------------------------------------*/
long SPCDECL sp_access(path, mode)
     char *path;
     long mode;
{
#if __IBMC__
  if (mode == 1)
    mode = 0;
#endif
  return (long)access(Native_path(path), mode);
}

void SPCDECL sp_del_file(path, t)
     char *path;
     SP_term_ref t;
{
  if (unlink(Native_path(path)) && SP_is_variable(t))
    SP_syserror_clib("delete_file/[1,2]","unlink");
}

void SPCDECL sp_rename(oldpath,newpath)
     char *oldpath, *newpath;
{
#if BACKSLASH_PATHS || 1 /* because of WCX we need Native_path */
  char buf[MAXPATHLEN];

  strcpy(buf, Native_path(oldpath));
  if (rename(buf, Native_path(newpath)))
#else
  if (rename(oldpath, newpath))
#endif
    SP_syserror_clib("rename_file/2","rename");
}

#ifndef _MSC_VER

#if __IBMC__
/* Different implementation in 'systos2.c' */
#else

SP_term_ref SPCDECL sp_directory_files(dir)
     char *dir;
{
  DIR *dirp;
#if __NeXT__
  struct direct *direntp;
#else
  struct dirent *direntp;
#endif
  SP_term_ref car = SP_new_term_ref(), cdr = SP_new_term_ref();

  dirp = opendir(Native_path(dir));
  if (dirp == NULL)
    SP_syserror_clib("directory_files/2","opendir");
  else
    {
      for (;;)
	{
	  direntp = readdir(dirp);
	  if (direntp == NULL)
	    break;
#if SP_WIN32 || SP_OS2 || SP_MSDOS
	  strlwr(direntp->d_name);
#endif
	  SP_put_string(car, SP_from_os(direntp->d_name, WCX_FILE));
	  SP_cons_list(cdr, car, cdr);
	}
      closedir(dirp);
    }
  return cdr;
}
#endif

#else
/* MSVC++ version */

SP_term_ref SPCDECL sp_directory_files(dir)
     char *dir;
{
  struct _finddata_t find_file;
  long hfile;
  SP_term_ref car = SP_new_term_ref(), cdr = SP_new_term_ref();

  strcat(Native_path(dir), "\\*");
  hfile = _findfirst(local.native_path_buffer, &find_file);
  if (hfile == -1 && errno != ENOENT)
    SP_syserror_clib("directory_files/2","_findfirst");
  else if (hfile != -1)
    {
      for (;;)
	{
	  strlwr(find_file.name);
	  SP_put_string(car, SP_from_os(find_file.name, WCX_FILE));
	  SP_cons_list(cdr, car, cdr);
	  if (_findnext(hfile, &find_file) != 0)
	    break;
	}
      _findclose(hfile);
    }
  return cdr;
}

#endif

#if __IBMC__
#define _S_IFREG S_IFREG
#define _S_IFDIR S_IFDIR
#endif
#ifndef S_ISREG
#define S_ISREG(Mode) (Mode & _S_IFREG)
#endif
#ifndef S_ISDIR
#define S_ISDIR(Mode) (Mode & _S_IFDIR)
#endif

SP_term_ref SPCDECL sp_file_property(file, key)
     char *file;
     long key;
{
  struct stat statbuf;
  SP_atom atm;
  SP_term_ref t = SP_new_term_ref();
  
  if (lstat(Native_path(file), &statbuf) < 0)
    SP_syserror_clib("file_property/2", "stat");
  else
    {
      switch (key)
	{
	case 0:
	  if (S_ISREG(statbuf.st_mode))
	    atm = local.atm_regular;
	  else if (S_ISDIR(statbuf.st_mode))
	    atm = local.atm_directory;
#ifdef S_ISFIFO
	  else if (S_ISFIFO(statbuf.st_mode))
	    atm = local.atm_fifo;
#endif
#ifdef S_ISSOCK
	  else if (S_ISSOCK(statbuf.st_mode))
	    atm = local.atm_socket;
#endif
#ifdef S_ISLNK
	  else if (S_ISLNK(statbuf.st_mode))
	    atm = local.atm_symlink;
#endif
	  else
	    atm = local.atm_unknown;
	  SP_put_atom(t, atm);
	  break;
	case 1:
	  SP_put_integer(t, statbuf.st_size); break;
	case 2:
	  SP_put_integer(t, statbuf.st_mtime); break;
	}
    }
  return t;
}

void SPCDECL sp_del_dir(dir, t)
     char *dir;
     SP_term_ref t;
{
  if (rmdir(Native_path(dir)) < 0  && SP_is_variable(t))
    SP_syserror_clib("delete_file/[1,2]", "rmdir");
}

void SPCDECL sp_make_dir(dir)
     char *dir;
{
#if SP_WIN32 || (SP_OS2 && !__EMX__)
  if (mkdir(Native_path(dir)) < 0)
#else
  if (mkdir(Native_path(dir), 0777) < 0)
#endif
    SP_syserror_clib("make_directory/1", "mkdir");
}

/*------------------------------------------------------------------*/
char * SPCDECL sp_mktemp(template)
     char *template;
{
  strcpy(local.hostnamebuf,template);
#if !SP_WIN32 && !(SP_OS2 && !__EMX__) && !MACINTOSH && HAVE_MKTEMP
  mktemp(local.hostnamebuf);
#elif SP_WIN32
  /* Win32 has an alternative to mktemp() called _mktemp(). */
  _mktemp(local.hostnamebuf);
#else
  /* Simulate mktemp() using tmpnam(). */
  {
    char *p = local.hostnamebuf + strlen(template) - 6;

    tmpnam(p);
  }
#endif
  return local.hostnamebuf;
}

/*------------------------------------------------------------------*/
char * SPCDECL sp_tmpnam(void)
{
  tmpnam(local.hostnamebuf);
  return local.hostnamebuf;

}

/*------------------------------------------------------------------*/
/* system & shell */

void SPCDECL sp_system0(void)
{
#if SP_MSDOS || SP_WIN32 || SP_OS2
  char *s = getenv("COMSPEC");

  if (s)
    system(s);
#else
  system("exec sh");
#endif
}

long SPCDECL sp_system2(command)
     char *command;
{
  /* Note! Does not convert slash to backslash. */
  return system(SP_to_os(command,WCX_OPTION));
}

/* We assume SHELL refers to a unix like shell whichever platform
   we're running at while COMSPEC refers to a dos like shell. Thus:

   Unix:       system("exec SHELL -c COMMAND")
   Dos etc:    system("SHELL -c COMMAND")
    or:        system("COMSPEC /C COMMAND")
*/

void SPCDECL sp_shell0(void)
{
  char *s = getenv("SHELL");

#if SP_MSDOS || SP_WIN32 || SP_OS2
  if (s)
    system(Native_path(s));
  else
    {
      s = getenv("COMSPEC");
      if (s)
	system(s);
    }
#else
  if (s)
    {
      char buf[MAXPATHLEN+10];

      strcpy(buf, "exec ");
      strcat(buf, s);
      system(buf);
    }
#endif
}

long SPCDECL sp_shell2(command)
     char *command;
{
  char *s = getenv("SHELL");
  char buf[MAXPATHLEN];
  unsigned int len;
  char *p, *p1, *p2;
  int rv;
  int comspec = 0;

  command = SP_to_os(command,WCX_OPTION);

#if SP_MSDOS || SP_WIN32 || SP_OS2
  if (!s)
    {
      s = getenv("COMSPEC");
      comspec = 1;
    }
#else
  (void)comspec;                /* [PM] 3.9b5 avoid -Wunused */
#endif

  if (!s)
    return -1;

  len = strlen(s) + 2*strlen(command) + 10;
  if (len < MAXPATHLEN)
    p = buf;
  else
    p = (char *)SP_malloc(len);

#if SP_MSDOS || SP_WIN32 || SP_OS2
  strcpy(p, s);
  if (comspec)
    {
      strcat(p, " /C ");
      strcat(p, Native_path1(command));
      goto runit;
    }
  else
    strcat(p, " -c ");
#else
  strcpy(p, "exec ");
  strcat(p, s);
  strcat(p, " -c ");
#endif

  p1 = p+strlen(p);
  p2 = command;
  while (*p2)
    *p1++ = '\\',
    *p1++ = *p2++;
  *p1++ = 0;
#if SP_MSDOS || SP_WIN32 || SP_OS2
 runit:
#endif
  rv = system(p);
  if (p != buf)
    SP_free(p);
  return rv;
}

/*------------------------------------------------------------------*/
/* exec & popen */

#if !(SP_MSDOS || SP_WIN32 || SP_OS2 && !__EMX__)

/* We need a set of stdio functions in order to install in stdio
   streams without needing extern declarations for fgetc etc. which is
   highly unportable. We must also have the possibility to declare
   stream functions as having a different calling convention than the
   default. */

static int SPCDECL spi_getc(void *handle)
{
  FILE *stream = (FILE *)handle;
  return getc(stream);
}

static int SPCDECL spi_putc(char x, void *handle)
{
  FILE *stream = (FILE *)handle;
  return putc(x, stream);
}

static int SPCDECL spi_flush(void *handle)
{
  FILE *stream = (FILE *)handle;
  return fflush(stream);
}

static int SPCDECL spi_eof(void *handle)
{
  FILE *stream = (FILE *)handle;
  return feof(stream);
}

static void SPCDECL spi_clrerr(void *handle)
{
  FILE *stream = (FILE *)handle;
  clearerr(stream);
}

static int SPCDECL spi_pclose(void *handle)
{
  FILE *stream = (FILE *)handle;
  return pclose(stream);
}

#endif

/*------------------------------------------------------------------*/
#if (sun || ultrix) && (0 /* [PM] 3.9 support.h says _exit is obsolete */)
/* Required of two reasons: 1. crashes in exit() if svr4 2. vfork'ed
   child process might mess up parent process if exit() is used */
#define Sp_exit(C) _exit(C);
#else
#define Sp_exit(C) exit(C);
#endif


/*------------------------------------------------------------------*/
/* '$unix_exec'(+Command, +IC, +OC, +EC, -IFD, -OFD, -EFD, -Pid) */

#if SP_WIN32 || SP_OS2
/* Different implementation in 'systw32.c' */
#else
#if !SP_MSDOS && !MACINTOSH
static int pipe_or_null PROTOTYPE((int,int *));
static SP_stream *new_exec_stream PROTOTYPE((int,char *));
static void dup_if PROTOTYPE((int,int,int,int));
static SP_stream *stream_if PROTOTYPE((int,int,int,char *));
#endif

long SPCDECL sp_exec(command,sinc,soutc,serrc,sin,sout,serr,pidloc)
     char *command;
     long sinc, soutc, serrc;
     SP_stream **sin, **sout, **serr;
     long *pidloc;
{
#if SP_MSDOS || SP_OS2 || MACINTOSH
  SP_syserror_clib("exec/3", "not supported");
  return -1;
#else
  int pid;
  int sin_pipe[2], sout_pipe[2], serr_pipe[2];
  
  if (!(pipe_or_null(sinc, sin_pipe) &&
	pipe_or_null(soutc, sout_pipe) &&
	pipe_or_null(serrc, serr_pipe)))
    return -1;

  *pidloc = pid =
#if sun || ultrix
    vfork();
#else
    fork();
#endif

  if (pid == 0)
    {				/* This is the child */
      dup_if(sinc, sin_pipe[0], sin_pipe[1], 0);
      dup_if(soutc, sout_pipe[1], sout_pipe[0], 1);
      dup_if(serrc, serr_pipe[1], serr_pipe[0], 2);
#if !__NeXT__
      if (sinc && soutc && serrc)
	(void) setsid();
#endif
      execlp("sh", "sh", "-c", SP_to_os(command,WCX_OPTION), (char*)0); /* here: backslashes */
      Sp_exit(-1);
    }
  else if (pid == -1)
    goto bomb;
/* The child may have terminated successfully already!
  else if (kill(pid,0))
    goto bomb;    */

  *sin = stream_if(sinc, sin_pipe[1], sin_pipe[0], "w");
  *sout = stream_if(soutc, sout_pipe[0], sout_pipe[1], "r");
  *serr = stream_if(serrc, serr_pipe[0], serr_pipe[1], "r");
  return 0;
 bomb:
  SP_syserror_clib("exec/3",pid == -1 ? "fork" : "kill");
  return -1;
#endif
}

#if !SP_MSDOS && !SP_OS2 && !MACINTOSH

static int pipe_or_null(code, pipe_fds)
     int code, *pipe_fds;
{
  if (code == 1)
    {
      if (0 > (pipe_fds[0] = pipe_fds[1] = open("/dev/null", O_RDWR)))
	goto bomb;
    }
  else if (code == 2)
    {
      if (pipe(pipe_fds))
	goto bomb;
    }
  return 1;
 bomb:
  SP_syserror_clib("exec/3",code == 1 ? "open" : "pipe");
  return 0;
}

static void dup_if(code, dupfd, closefd, stdfd)
     int code, dupfd, closefd, stdfd;
{
  switch (code)
    {
    case 2:
      close(closefd);
    case 1:
      if (0 > dup2(dupfd, stdfd))
	Sp_exit(-1);
    }
}

static SP_stream *stream_if(code, streamfd, closefd, mode)
     int code, streamfd, closefd;
     char *mode;
{
  switch (code)
    {
    case 2:			/* pipe(_) */
      close(closefd);
      return new_exec_stream(streamfd,mode);
    case 1:			/* null */
      close(closefd);
    case 0:			/* std */
    default:
      return NULL;
    }
}

static int SPCDECL exec_close(void *handle)
{
  FILE *f = (FILE *)handle;
  int fd = fileno(f);
 
  fclose(f);
  close(fd);
  return 0;
}

static SP_stream *new_exec_stream(fd, mode)
     int fd;
     char *mode;
{
  SP_stream *s;
  FILE *f = (FILE *)fdopen(fd, mode);
  
  if (!strcmp(mode,"r"))
    {
      SP_make_stream_context(
		     (ANYPOINTER)f,
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
  else
    {
      setbuf(f, NULL);
      SP_make_stream_context(
		   (ANYPOINTER)f,
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
  s->fd = fileno(f);
  if (SP_isatty(s))
    SP_set_tty(s);
  return s;
}

#endif /* !SP_MSDOS && !SP_OS2 */
#endif /* !SP_WIN32 */


/*------------------------------------------------------------------*/
#if SP_MSDOS || SP_WIN32 || (SP_OS2 && !__EMX__) || MACINTOSH

SP_stream * SPCDECL sp_popen(command,mode)
     char *command;
     char *mode;
{
  /* [PM] 3.9 Note that msg "not supported" is treated specially in
     sp_syserror. Thus the value of errno does not matter. (This shows
     SPRM 1830 to be a non-issue) */
  SP_syserror_clib("popen/3", "not supported");
  return NULL;
}

#else

SP_stream * SPCDECL sp_popen(command,mode)
     char *command;
     char *mode;
{
  FILE *f;
  SP_stream *s;

  if (!strcmp(mode,"read"))
    {
      if (!(f = popen(SP_to_os(command,WCX_OPTION),"r")))
	goto bomb;
      SP_make_stream_context((ANYPOINTER)f, spi_getc, NULL, spi_flush, spi_eof, 
		     spi_clrerr, spi_pclose, &s,
		     SP_WCX_FLAG, SP_STREAMHOOK_LIB);
    }
  else if (!strcmp(mode,"write"))
    {
      if (!(f = popen(SP_to_os(command,WCX_OPTION),"w")))
	goto bomb;
      SP_make_stream_context((ANYPOINTER)f, NULL, spi_putc, spi_flush, NULL,
		     spi_clrerr, spi_pclose, &s,
		     SP_WCX_FLAG, SP_STREAMHOOK_LIB);
    }
  else
    {
      SP_term_ref culprit=SP_new_term_ref();

      SP_save_error(DOMAIN_ERROR, "", culprit);
      SP_raise_error("popen", 3, 2);
      return NULL;
    }
  s->fd = fileno(f);
  if (SP_isatty(s))
    SP_set_tty(s);
  return s;
 bomb:
  SP_syserror_clib("popen/3","popen");
  return NULL;
}
#endif

/*------------------------------------------------------------------*/

#if SP_WIN32 || SP_OS2
/* Different implementation in 'systw32.c' or 'systos2.c' */
#else
extern void SPCDECL sp_sleep PROTOTYPE((double));
void SPCDECL sp_sleep(secs)
     double secs;
{
  if (secs < 1.0)
    usleep((unsigned int)(1.0e6*secs));
  else
    sleep((unsigned int)secs);
}
#endif

/*------------------------------------------------------------------*/

#if SP_OS2
/* Different implementation in 'systos2.c' */
#else
extern long SPCDECL sp_getpid PROTOTYPE((void));
long SPCDECL sp_getpid(void)
{
  return getpid();
}
#endif

/*------------------------------------------------------------------*/
#if SP_WIN32 || SP_OS2
/* Different implementation in 'systw32.c' or 'systos2.c' */
#else
extern void SPCDECL sp_kill PROTOTYPE((long,long));
void SPCDECL sp_kill(pid, sig)
     long pid, sig;
{
#if SP_OS2 && !__EMX__ || MACINTOSH
  SP_syserror_clib("kill/2", "not supported");
#else
  if (kill(pid,sig))
    SP_syserror_clib("kill/2","kill");
#endif
}
#endif

/*------------------------------------------------------------------*/

#if SP_WIN32 || SP_OS2
/* Different implementation in 'systw32.c' or 'systos2.c' */
#else
long SPCDECL sp_wait(mypid)
     long mypid;
{
  int status;
  int pid;

#if MACINTOSH
  SP_syserror_clib("wait/1", "not supported");
  return -1;
#else

  do
    pid = wait(&status);
  while (pid>=0 && pid!=mypid);
  if (pid < 0)
    SP_syserror_clib("wait/2","wait");
  return status;
#endif
}
#endif
