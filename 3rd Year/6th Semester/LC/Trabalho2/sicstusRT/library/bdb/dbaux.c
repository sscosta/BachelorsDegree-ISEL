#include "dbaux.h"
#include <ctype.h>

#include <string.h>
#include <sys/stat.h>
#ifndef SP_WIN32
#include <unistd.h>
#endif /* SP_WIN32 */
#ifdef SP_WIN32
#include <direct.h>             /* _mkdir */
#endif

#if 1				/* [PD] BDB version 3.2.9 */
u_int32_t identity(DB *dummy, const void *key, u_int32_t size)
{
  (void)dummy;                  /* [PM] 3.9b5 avoid -Wunused */
  (void)size;                   /* [PM] 3.9b5 avoid -Wunused */

  return *(u_int32_t *)key;
}
#else
u_int32_t identity(const void *key, u_int32_t size)
{
  return *(u_int32_t *)key;
}
#endif

int directory_exists(char *path) /* form library/db/file.c */
{
  struct stat buf;
  return stat(path, &buf) ? 0 : (buf.st_mode & S_IFDIR) != 0;
}

int file_exists(char *path)
{
  struct stat buf;
  return stat(path, &buf) == 0;
}

int files_exist(char filename[][PATH_MAX+1], int n)
{
  int r = 0;
  for (; n; --n) if (file_exists(Native_path(filename[n-1]))) ++r;
  return r;
}

/* [PM] 3.9 There is no documented mkdir on Windows. There is an
   undocumented alias mkdir for the documented procedure _mkdir but
   _mkdir does not take a MODE argument.
*/
#ifdef SP_WIN32
int bdb_mkdir(const char *path, int mode)
{
  return _mkdir(path);
}
#else
int bdb_mkdir(const char *path, int mode)
{
  return mkdir(path, mode);
}
#endif

#ifdef STAT
#ifdef SP_WIN32
#include <windows.h>

int times(struct tms *buffer)
{
    static int initialized = FALSE;
    static FARPROC proc;
    FILETIME Createtm, Exittm, Kerneltm, Usertm;

    buffer->tms_cutime = 0; /* child process user time (N/A) */
    buffer->tms_cstime = 0; /* child process system time (N/A) */

    if (!initialized) {
	/* TNT extender doesn't include GetProcessTimes */
	HINSTANCE hnd = GetModuleHandle("kernel32.dll");

	proc = hnd ? GetProcAddress(hnd, "GetProcessTimes") : NULL;
	initialized = TRUE;
    }

    /* Get 64 bit process time representing 100ns units */
    if (!proc ||
	!(*proc)(GetCurrentProcess(), &Createtm, &Exittm, &Kerneltm, &Usertm))
    {   /* GetProcessTimes() not supported */
	buffer->tms_utime = clock();	/* user time */
	buffer->tms_stime = 0;		/* system time */
	return 0;
    }

    /* convert process time to number of elasped milliseconds */
    buffer->tms_utime  = Usertm.dwHighDateTime * 429496.7296;
    buffer->tms_utime += Usertm.dwLowDateTime / 10000;
    buffer->tms_stime  = Kerneltm.dwHighDateTime * 429496.7296;
    buffer->tms_stime += Kerneltm.dwLowDateTime / 10000;

    return 0;
}
#endif
#endif /* STAT */

#if BACKSLASH_PATHS

char *db_native_path(char *path)
{
  static char native_path_buffer[PATH_MAX];
  int i;

  if (path == NULL)
    return NULL;
  strcpy(native_path_buffer, path);
  for (i=strlen(native_path_buffer); i--; )
    if (native_path_buffer[i] == '/')
      native_path_buffer[i] = '\\';
  return native_path_buffer;
}

#endif /* BACKSLASH_PATHS */

char *db_normal_path(char *path)
{
#if BACKSLASH_PATHS
  static char normal_path_buffer[PATH_MAX];
  int i;

  if (path == NULL)
    return NULL;
  strcpy(normal_path_buffer, path);
  for (i=strlen(normal_path_buffer); i--; )
    if (normal_path_buffer[i] == '\\')
      normal_path_buffer[i] = '/';
  return normal_path_buffer;
#else  /* BACKSLASH_PATHS */
  return path;
#endif
}

/* True if RelPath is a relative pathname.
 * Ripped off from '$relative_file_name'/2.
 */
int db_relative_file_name(char *path)
{
  switch (path[0])
    {
    case '\0':
      return 1;		/* empty string - catch error later */
    case '/':
    case '~':
    case '$':
      return 0;		/* RelPath is absolute */
#if SP_WIN32
    case '\\':
      return 0;		/* RelPath is absolute */
    default:
      if (isalpha(path[0]) && path[1] == ':') /* drive letter - absolute path */
	return 0;
#endif
    }
  return 1;
}
