#include <errno.h>
#include <string.h>

#include <sicstus/sicstus.h>
#include <sicstus/config.h>

#if 0                           /* [PM] 3.9 Now uses bdb_mkdir() */
#include <sys/stat.h>           /* mkdir */
#endif
#include <sys/types.h>
#include <fcntl.h>
#ifndef SP_WIN32
#include <unistd.h>
#endif /* SP_WIN32 */

#include <sicstus/sicstus.h>
#include <db.h>

#include "bdb.h"
#include "dbaux.h"

#include "bdb_glue.h"           /* [PM] 3.9 splfr-generated */

/*
From the Berkeley DB manual:

  Due to the constraints of the PA-RISC memory architecture, HP-UX does not
  allow a process to map a file into its address space multiple times.  For
  this reason, each Berkeley DB environment may be opened only once by a
  process on HP-UX, i.e., calls to DBENV->open() will fail if the specified
  Berkeley DB environment has been opened and not subsequently closed.

Currently, the Prolog module tries to ensure this.
*/

/*
  Windows/95 note:

  On Windows/95, files that are opened by multiple processes do not share
  data correctly. For this reason, the DB_SYSTEM_MEM flag is implied for
  any application that does not specify the DB_PRIVATE flag.
 */

#ifdef STAT
#ifndef SP_WIN32
#include <sys/times.h>
#endif /* SP_WIN32 */

struct {
  char *name;
  unsigned count;
  struct tms time;
} stattable[] = {
  "open_env",             0, 0, 0, 0, 0,
  "close_env",            0, 0, 0, 0, 0,
  "open_db",              0, 0, 0, 0, 0,
  "close_db",             0, 0, 0, 0, 0,
  "read_spec",            0, 0, 0, 0, 0,
  "next_termref",         0, 0, 0, 0, 0,
  "store_termref",        0, 0, 0, 0, 0,
  "delete_termref",       0, 0, 0, 0, 0,
  "store_term",           0, 0, 0, 0, 0,
  "delete_term",          0, 0, 0, 0, 0,
  "fetch_term",           0, 0, 0, 0, 0,
  "global_iterator",      0, 0, 0, 0, 0,
  "global_iterator_next", 0, 0, 0, 0, 0,
  "global_iterator_done", 0, 0, 0, 0, 0,
  "term_iterator",        0, 0, 0, 0, 0,
  "term_iterator_next",   0, 0, 0, 0, 0,
  "term_iterator_done",   0, 0, 0, 0, 0,
  NULL, 0, 0, 0, 0, 0
};

#define OPEN_ENV             0
#define CLOSE_ENV            1
#define OPEN_DB              2
#define CLOSE_DB             3
#define READ_SPEC            4
#define NEXT_TERMREF         5
#define STORE_TERMREF        6
#define DELETE_TERMREF       7
#define STORE_TERM           8
#define DELETE_TERM          9
#define FETCH_TERM           10
#define GLOBAL_ITERATOR      11
#define GLOBAL_ITERATOR_NEXT 12
#define GLOBAL_ITERATOR_DONE 13
#define TERM_ITERATOR        14
#define TERM_ITERATOR_NEXT   15
#define TERM_ITERATOR_DONE   16

static struct tms stv;

static void start_stat(int i)
{
  ++stattable[i].count;
  times(&stv);
}

#define timeop(a, op, b)                \
  do {                                  \
    (a).tms_utime op (b).tms_utime;     \
    (a).tms_stime op (b).tms_stime;     \
    (a).tms_cutime op (b).tms_cutime;   \
    (a).tms_cstime op (b).tms_cstime;   \
  } while (0)

static void end_stat(int i)
{
  struct tms etv;
  times(&etv);
  timeop(etv, -=, stv);
  timeop(stattable[i].time, +=, etv);
}

#define START_STAT(x) start_stat(x)
#define END_STAT(x)   end_stat(x)

#ifndef CLOCKS_PER_SECOND
#define CLOCKS_PER_SECOND CLK_TCK
#endif /* CLOCKS_PER_SECOND */

void printstat()
{
  int i;
  SP_printf("name                  calls     user     system\n"
            "-------------------------------------------------\n");
  for (i = 0; stattable[i].name; ++i)
    SP_printf("%-21s %9u %8.3f %8.3f\n",
              stattable[i].name, stattable[i].count,
              stattable[i].time.tms_utime/(double)CLOCKS_PER_SECOND,
              stattable[i].time.tms_stime/(double)CLOCKS_PER_SECOND);
}

void zerostat()
{
  int i;
  for (i = 0; stattable[i].name; ++i) {
    stattable[i].count = 0;
    stattable[i].time.tms_utime = 0;
    stattable[i].time.tms_stime = 0;
    stattable[i].time.tms_cutime = 0;
    stattable[i].time.tms_cstime = 0;
  }
}

#else

#define START_STAT(x)
#define END_STAT(x)

#endif /* STAT */

#if 1                           /* [PM] 3.8.6 correct prototype for init funs */
void db_init(int when)
#else  /* [PM] 3.8.6 Was: */
   long db_init()                  /* for compatibility with the Prolog code */
#endif
{
  (void)when;                  /* [PM] 3.9b5 avoid -Wunused */  

  /* [PM] 3.8.6 PRM 1870 (patch version change require recompile)
   
     Barf if run with a version that does not exactly match what was
     used when compiling.
     [PD] 3.9 patch version may change.
  */
  {
    int major,minor,patch;
    char *versionstring;
    
    versionstring = db_version (&major,&minor,&patch);
#if DBG>1
    fprintf(stderr, "library(bdb) compiled with %d.%d.%d \"" DB_VERSION_STRING "\"\n", (int) DB_VERSION_MAJOR, (int)DB_VERSION_MINOR, (int)DB_VERSION_PATCH );
    fprintf(stderr, "library(bdb) linked to bdb run-time %d.%d.%d \"%s\"\n", major, minor, patch, versionstring );
#endif
    
    if (! ( major == DB_VERSION_MAJOR
            && minor == DB_VERSION_MINOR
/* [PD] 3.9 patch version may change. */
            && patch >= DB_VERSION_PATCH
            ) )
      {
        const char formatstring[] = "Installed BDB is %d.%d.%d. library(bdb) requires \"" DB_VERSION_STRING "\"";
        char buf[10+10+10+sizeof formatstring];
        sprintf(buf, formatstring, major, minor, patch);
        SP_save_error(SYSTEM_ERROR, buf, SP_new_term_ref());
        SP_raise_error("db_init", 0, 0);
        return;
      }
  }
#if 0 /* [PD] BDB 3 does not have db_value_set. The patch from IQSOFT
	 eliminates the call to db_value_set. */
  /* [PM] 3.8.6 init functions should not return a value (used to return non-zero on failure) */
#ifdef SP_WIN32
  if (db_value_set(1, DB_REGION_NAME)) /* see Berkeley DB db_appinit */
    {
        SP_save_error(SYSTEM_ERROR, "could not initialize BDB", SP_new_term_ref());
        SP_raise_error("db_init", 0, 0);
        return;
    }
#endif /* SP_WIN32 */
#endif /* [PD] BDB 3 */
}

/*
  Environment functions
 */
 /* [PD] 3.9 'unsigned long' results in 'warning: conflicting types'
    since there is no way to specify 'unsigned long' in foreign declaration
    conversions.
  */
/* long open_env(char *env_name, unsigned long cache_size, DB_ENV **env) */
long open_env(char *env_name, long cache_size, long *lenv)
{
  DB_ENV *myenv;
  DB_ENV **env = &myenv;
  START_STAT(OPEN_ENV);

  env_name = Native_path_to_os(env_name);

#ifdef DEBUG
  SP_fprintf(SP_stderr, "open_env: home = %s\n", env_name);
#endif /* DEBUG */

  if (!directory_exists(env_name))
    if ((errno = bdb_mkdir(env_name, DMODE)) != 0) {
#ifdef DEBUG
      SP_fprintf(SP_stderr, "bdb_mkdir: %s: %s\n", env_name, strerror(errno));
#endif /* DEBUG */
      return errno;
    }

  if ((errno = db_env_create(env, 0)) != 0) return errno;
  *lenv = (long)(*env);
  if (cache_size < 20) cache_size = 20; /* 20KB is the min. for BDB */
  { /* [PD] 3.9 convert to unsigned */
    unsigned long u_cache_size = (unsigned long)cache_size;
    /* 2 additional parameters! */
/*  (*env)->set_cachesize(*env, cache_size>>20, (cache_size&0xfffff)<<10, 0); */
    (*env)->set_cachesize(*env,u_cache_size>>20,(u_cache_size&0xfffff)<<10, 0);
  }
  if ((errno = (*env)->open(*env, env_name,
                            DB_CREATE|DB_INIT_CDB|DB_INIT_MPOOL, 0)) != 0) {
#ifdef DEBUG
    SP_fprintf(SP_stderr, "db: %s: %s\n", env_name, db_strerror(errno));
#endif /* DEBUG */
    (*env)->close(*env, 0);
  }

  if (errno) SP_free(*env);

  END_STAT(OPEN_ENV);

  return errno;
}

long close_env(long lenv)
{
  DB_ENV *env = (DB_ENV *)lenv;
  START_STAT(CLOSE_ENV);
  
  errno = env->close(env, 0);

  END_STAT(CLOSE_ENV);
  
  return errno;
}

/*
  Database functions
 */
static void db_filenames(char *dbname, char filename[][PATH_MAX+1])
{
  int i, db_name_len = strlen(dbname);

  for (i = 0; i < 3; ++i) {
    strcpy(filename[i], dbname);    /* set up the base name */
    filename[i][db_name_len] = '/';
  }
  
  strcpy(filename[0]+db_name_len+1, TERMSNAME); /* terms file */
  strcpy(filename[1]+db_name_len+1, INDEXNAME); /* index file */
  strcpy(filename[2]+db_name_len+1, ADMINNAME); /* admin file */  
}

static int db_open_files(char filename[][PATH_MAX+1], u_int32_t flags,
                  DB_ENV *env, db_struct *dbp)
{
  char *fn;

#ifdef DEBUG
  SP_printf("db_open_files: env = %p\n", env);
#endif

  if ((errno = db_create(&dbp->termsdb, env, 0)) != 0)
    return errno;
  dbp->termsdb->set_h_hash(dbp->termsdb, identity);
  fn = Native_path(filename[0]);

  if (dbp->cache_size>0) {
    unsigned long u_cache_size = (unsigned long)dbp->cache_size;
    dbp->termsdb->set_cachesize(dbp->termsdb,
				u_cache_size>>20,
				(u_cache_size&0xfffff)<<10, 0);
    dbp->admindb->set_cachesize(dbp->admindb,
				u_cache_size>>20,
				(u_cache_size&0xfffff)<<10, 0);
    dbp->indexdb->set_cachesize(dbp->indexdb,
				u_cache_size>>20,
				(u_cache_size&0xfffff)<<10, 0);
  }
  if ((errno = dbp->termsdb->open(dbp->termsdb, fn, NULL, DB_HASH,
                                  flags, FMODE)) != 0) {
#ifdef DEBUG
    SP_fprintf(SP_stderr, "db: %s: %s\n", fn, db_strerror(errno));
#endif /* DEBUG */
    return errno;
  }

  if ((errno = db_create(&dbp->admindb, env, 0)) != 0)
    return errno;
  dbp->admindb->set_h_hash(dbp->admindb, identity);
  fn = Native_path(filename[2]);
  if ((errno = dbp->admindb->open(dbp->admindb, fn, NULL, DB_HASH,
                                  flags, FMODE)) != 0) {
#ifdef DEBUG
    SP_fprintf(SP_stderr, "db: %s: %s\n", fn, db_strerror(errno));
#endif /* DEBUG */
    return errno;
  }

  if ((errno = db_create(&dbp->indexdb, env, 0)) != 0)
    return errno;
  dbp->indexdb->set_h_hash(dbp->indexdb, identity);
  dbp->indexdb->set_flags(dbp->indexdb, DB_DUP); /* there can be duplicates here */
  fn = Native_path(filename[1]);
  if ((errno = dbp->indexdb->open(dbp->indexdb, fn, NULL, DB_HASH,
                                  flags, FMODE)) != 0) {
#ifdef DEBUG
    SP_fprintf(SP_stderr, "db: %s: %s\n", fn, db_strerror(errno));
#endif /* DEBUG */
    return errno;
  }

  return 0;
}

static int open_db_enumerate(DB_ENV *env, char *dbname, db_struct *dbp)
{
  char filename[PATH_MAX+1], *fn;
  int db_name_len = strlen(dbname);

  strcpy(filename, dbname);     /* set up the base name */
  filename[db_name_len] = '/';

  strcpy(filename+db_name_len+1, TERMSNAME); /* terms file */

  if ((errno = db_create(&dbp->termsdb, env, 0)) != 0)
    return errno;
  dbp->termsdb->set_h_hash(dbp->termsdb, identity);
  fn = Native_path(filename);
  if (dbp->cache_size>0) {
    unsigned long u_cache_size = (unsigned long)dbp->cache_size;
    dbp->termsdb->set_cachesize(dbp->termsdb,
				u_cache_size>>20,
				(u_cache_size&0xfffff)<<10, 0);
  }
  if ((errno = dbp->termsdb->open(dbp->termsdb, fn, NULL, DB_HASH,
                                  DB_RDONLY, FMODE)) != 0) {
    dbp->termsdb->close(dbp->termsdb, 0);
#ifdef DEBUG
    SP_fprintf(SP_stderr, "db: %s: %s\n", fn, db_strerror(errno));
#endif /* DEBUG */
    return errno;
  }

  return 0;
}

static int open_db_read(DB_ENV *env, char *dbname, db_struct *dbp)
{
  char filenames[3][PATH_MAX+1];
  int err;

  db_filenames(dbname, filenames);
  if ((err = db_open_files(filenames, DB_RDONLY, env, dbp)) != 0) {
    if (dbp->termsdb) dbp->termsdb->close(dbp->termsdb, 0);
    if (dbp->indexdb) dbp->indexdb->close(dbp->indexdb, 0);
    if (dbp->admindb) dbp->admindb->close(dbp->admindb, 0);
    memset(dbp, 0, sizeof(db_struct));
  }
  return err;
}

static int open_db_update(DB_ENV *env, char *dbname, db_struct *dbp,
                          void *spec, long specsize)
{
  u_int32_t flags;
  char filenames[3][PATH_MAX+1]; /* abs, or rel. to env, if given (for db_open) */
  char absfilenames[3][PATH_MAX+1]; /* abs, or rel. to $cwd (for stat, unlink) */
  char dirnamebuf[PATH_MAX+1];
  char *dirname;
  int nfiles = 0, err, i;

#ifdef DEBUG
  SP_printf("open_db_update: home = %s, db = %s\n", env ? env->db_home : "", dbname);
#endif

  db_filenames(dbname, filenames);
				/* A relative DBName should be relative to the Env, if given!
				   That's how the BDB API functions treat it. --Mats */
  if (db_relative_file_name(dbname) && env) {
    strcpy(dirnamebuf, env->db_home);
    strcat(dirnamebuf, "/");
    strcat(dirnamebuf, dbname);
    db_filenames(dirnamebuf, absfilenames);
    dirname = Native_path(dirnamebuf);
  } else {
    db_filenames(dbname, absfilenames);
    dirname = Native_path(dbname);
  }
  if (!directory_exists(dirname)) {
    if (spec == 0 || specsize == 0) {
      err = EINVAL;
      goto error;
    }
    if ((err = bdb_mkdir(dirname, DMODE)) != 0) goto error;
    nfiles = 0;
  }
  else
    if ((nfiles = files_exist(absfilenames, 3)) > 0 && nfiles < 3)
      return ENOENT;
  flags = nfiles ? 0 : DB_CREATE;
  if ((err = db_open_files(filenames, flags, env, dbp)) != 0)
    goto error;

  if (nfiles == 0) {            /* create admin file */
    u_int32_t k, r = 0;
    DBT key, data;
    memset(&key, 0, sizeof(key));
    memset(&data, 0, sizeof(data));

    k = NREFKEY;                /* store the next available TERMREF */
    key.data = &k;
    key.size = sizeof(k);
    data.data = &r;
    data.size = sizeof(r);
    if ((err = dbp->admindb->put(dbp->admindb, NULL, &key, &data, 0)) != 0)
      goto error; 

    k = SPECKEY;                /* store the speclist */
    key.data = &k;
    data.data = spec;
    data.size = (unsigned long)specsize;
    if ((err = dbp->admindb->put(dbp->admindb, NULL, &key, &data, 0)) != 0)
      goto error;
    if (dbp->cache_size == -1)	/* flush cache */
      dbp->admindb->sync(dbp->admindb, 0);
  }
  return 0;

error:                          /* can't use close_db */
  if (dbp->termsdb) dbp->termsdb->close(dbp->termsdb, 0);
  if (dbp->indexdb) dbp->indexdb->close(dbp->indexdb, 0);
  if (dbp->admindb) dbp->admindb->close(dbp->admindb, 0);
  memset(dbp, 0, sizeof(db_struct));
  if (nfiles == 0)
    for (i = 0; i < 3; ++i)
      unlink(Native_path(absfilenames[i]));

  return err;
}

long close_db(db_struct *db)
{
  START_STAT(CLOSE_DB);
  
  if (db) {
    if (db->termsdb) db->termsdb->close(db->termsdb, 0); /* error handling? */
    if (db->indexdb) db->indexdb->close(db->indexdb, 0);
    if (db->admindb) db->admindb->close(db->admindb, 0);
    SP_free(db);
    
    END_STAT(CLOSE_DB);
    
    return 0;
  }
  return EINVAL;
}

long open_db(long lenv, char *dbname, char *mode, db_struct **db,
	     long lspec, long specsize, long cache_size)
{
  DB_ENV *env = (DB_ENV *)lenv;
  db_struct *dbp;

  dbname = SP_to_os(dbname, WCX_FILE);

#ifdef DEBUG
  SP_printf("open_db: env = %p, db = %s, mode = %s\n", env, dbname, mode);
#endif

  START_STAT(OPEN_DB);

#if 0 /* [PD] BDB 3 patch moved this up */
  dbname = SP_to_os(dbname, WCX_FILE);
#endif

  *db = dbp = (db_struct *)SP_malloc(sizeof(db_struct));
  if (!dbp) {
    errno = ENOMEM;
    goto error;
  }
  memset(dbp, 0, sizeof(db_struct));
  dbp->env = env;
  if (cache_size>0 && cache_size<20)
    cache_size = 20;		/* 20KB is the min. for BDB */
  dbp->cache_size = cache_size;

  if (strcmp(mode, "read") == 0) {
    if ((errno = open_db_read(env, dbname, dbp)) != 0)
      goto error;
  }
  else if (strcmp(mode, "update") == 0) {
    if ((errno = open_db_update(env, dbname, dbp, (void *)lspec, specsize)) != 0)
      goto error;
  }
  else if (strcmp(mode, "enumerate") == 0) {
    if ((errno = open_db_enumerate(env, dbname, dbp)) != 0)
      goto error;
  }
  else return EINVAL;

  END_STAT(OPEN_DB);
  
  return 0;

error:
  close_db(dbp);
  return errno;
}

long read_spec(db_struct *db, long *lspeclist)
{
  START_STAT(READ_SPEC);
  
  if (db && db->admindb) {
    u_int32_t k = SPECKEY;
    DBT key, data;
    memset(&key, 0, sizeof(key));
    memset(&data, 0, sizeof(data));

    key.data = &k;
    key.size = sizeof(k);
    errno = db->admindb->get(db->admindb, NULL, &key, &data, 0);
#ifdef DEBUG
    switch (errno) {
    case 0:
      break;
    case DB_NOTFOUND:
      SP_fprintf(SP_stderr, "speclist not found.\n");
      break;
    default:
      SP_fprintf(SP_stderr, "get speclist: %s\n", db_strerror(errno));
    }
#endif /* DEBUG */
    if (errno) return errno;
    *lspeclist = (long)data.data;
    
    END_STAT(READ_SPEC);
    
    return 0;
  }
  return EINVAL;
}

long next_termref(db_struct *db, long *termref)
{
  u_int32_t k = NREFKEY, d, lockid;
  int err = 0;
  DBT key, data;
  DB_LOCK lock;
  DB_ENV *env;

  START_STAT(NEXT_TERMREF);
  
  if (db == NULL || db->admindb == NULL) return EINVAL;
  env = db->env;

  memset(&key, 0, sizeof(key));
  key.data = &k;
  key.size = sizeof(k);

  if (env) {
    if ((errno = LOCK_ID(env, &lockid)) != 0) {
#ifdef DEBUG
      SP_fprintf(SP_stderr, "cannot get lockid in env %p: %s\n", env, db_strerror(errno));
#endif /* DEBUG */
      return errno;
    }

    if ((errno = LOCK_GET(env, lockid, 0, &key, DB_LOCK_WRITE, &lock)) != 0) {
#ifdef DEBUG
      SP_fprintf(SP_stderr, "cannot acquire lock: %s\n", db_strerror(errno));
#endif /* DEBUG */
      return errno;
    }
  }

  memset(&data, 0, sizeof(data));
  if ((errno = db->admindb->get(db->admindb, NULL, &key, &data, 0)) != 0) {
#ifdef DEBUG
    SP_fprintf(SP_stderr, "get next termref: %s\n", db_strerror(errno));
#endif /* DEBUG */
    goto unlock;
  }

  *termref = d = *(u_int32_t *)data.data;
  if (++d == 0) {
    errno = ERANGE;             /* Result too large */
    goto unlock;
  }
  data.data = &d;
  data.size = sizeof(d);
  if ((errno = db->admindb->put(db->admindb, NULL, &key, &data, 0)) != 0) {
#ifdef DEBUG
    SP_fprintf(SP_stderr, "put next termref: %s\n", db_strerror(errno));
#endif /* DEBUG */
    goto unlock;
  }
  if (db->cache_size == -1)	/* flush cache */
    db->admindb->sync(db->admindb, 0);

unlock:
  if (env) {
    if ((err = LOCK_PUT(env, &lock)) != 0) {
#ifdef DEBUG
      SP_fprintf(SP_stderr, "cannot release lock: %s\n", db_strerror(errno));
#endif /* DEBUG */
    }
    if ((err = LOCK_ID_FREE(env, lockid)) != 0) {
#ifdef DEBUG
      SP_fprintf(SP_stderr, "cannot free lock: %s\n", db_strerror(errno));
#endif /* DEBUG */
    }
  }

  END_STAT(NEXT_TERMREF);
  
  return errno ? errno : err;
}

long store_termref(db_struct *db, long hc, long termref)
{
  START_STAT(STORE_TERMREF);
  
  if (db && db->indexdb) {
    u_int32_t k = (unsigned long)hc, d = (unsigned long)termref;
    DBT key, data;

#if 0
    SP_printf("%ld %ld\n", hc, termref);
#endif

    memset(&key, 0, sizeof(key));
    memset(&data, 0, sizeof(data));
    key.data = &k;
    key.size = sizeof(k);
    data.data = &d;
    data.size = sizeof(d);
    errno = db->indexdb->put(db->indexdb, NULL, &key, &data, 0);
#ifdef DEBUG
    if (errno)
      SP_fprintf(SP_stderr, "store termref: %s\n", db_strerror(errno));
#endif /* DEBUG */

    if (db->cache_size == -1)	/* flush cache */
      db->indexdb->sync(db->indexdb, 0);
    END_STAT(STORE_TERMREF);

    return errno;
  }
  return EINVAL;
}

long delete_termref(db_struct *db, long hc, long termref)
{
  u_int32_t k = (unsigned long)hc, d = (unsigned long)termref;
  DBT key, data;
  DBC *cursor;
  int err;

  START_STAT(DELETE_TERMREF);
  
  if (db == NULL || db->indexdb == NULL) return EINVAL;

  errno = db->indexdb->cursor(db->indexdb, NULL, &cursor,
                              db->env != NULL ? DB_WRITECURSOR : 0);
  if (errno) {
#ifdef DEBUG
    SP_fprintf(SP_stderr, "create cursor: %s\n", db_strerror(errno));
#endif /* DEBUG */
    return errno;
  }

  memset(&key, 0, sizeof(key));
  memset(&data, 0, sizeof(data));

  key.data = &k;
  key.size = sizeof(k);
  for (err = cursor->c_get(cursor, &key, &data, DB_SET); /* DB_NOTFOUND!!! */
       err == 0;
       err = cursor->c_get(cursor, &key, &data, DB_NEXT_DUP)) {
    if (d == *(u_int32_t *)data.data) {
      err = cursor->c_del(cursor, 0);
      break;
    }
  }

  cursor->c_close(cursor);
  if (db->cache_size == -1)	/* flush cache */
    db->indexdb->sync(db->indexdb, 0);

  END_STAT(DELETE_TERMREF);

  return err == DB_NOTFOUND ? 0 : err;
}

long store_term(db_struct *db, long termref, long lterm, long termsize)
{
  START_STAT(STORE_TERM);
  
  if (db && db->termsdb) {
    u_int32_t k = (unsigned long)termref;
    DBT key, data;
    memset(&key, 0, sizeof(key));
    memset(&data, 0, sizeof(data));

    key.data = &k;
    key.size = sizeof(k);
    data.data = (void *)lterm;
    data.size = (unsigned long)termsize;
    errno = db->termsdb->put(db->termsdb, NULL, &key, &data, 0);
#ifdef DEBUG
    if (errno)
      SP_fprintf(SP_stderr, "store term: %s\n", db_strerror(errno));
#endif /* DEBUG */
    if (db->cache_size == -1)	/* flush cache */
      db->termsdb->sync(db->termsdb, 0);

    END_STAT(STORE_TERM);
    
    return errno;
  }
  return EINVAL;
}

long delete_term(db_struct *db, long termref)
{
  START_STAT(DELETE_TERM);
  
  if (db && db->termsdb) {
    u_int32_t k = (unsigned long)termref;
    DBT key;
    memset(&key, 0, sizeof(key));

    key.data = &k;
    key.size = sizeof(k);
    errno = db->termsdb->del(db->termsdb, NULL, &key, 0);
#ifdef DEBUG
    if (errno)
      SP_fprintf(SP_stderr, "delete term: %s\n", db_strerror(errno));
#endif /* DEBUG */
    if (db->cache_size == -1)	/* flush cache */
      db->termsdb->sync(db->termsdb, 0);

    END_STAT(DELETE_TERM);
    
    return errno;
  }
  return EINVAL;
}

long fetch_term(db_struct *db, long termref, long *lterm)
{
  START_STAT(FETCH_TERM);
  
  if (db && db->termsdb) {
    u_int32_t k = (unsigned long)termref;
    DBT key, data;
    memset(&key, 0, sizeof(key));
    memset(&data, 0, sizeof(data));

    key.data = &k;
    key.size = sizeof(k);
    errno = db->termsdb->get(db->termsdb, NULL, &key, &data, 0);
    if (errno) {
#ifdef DEBUG
      SP_fprintf(SP_stderr, "fetch term: %s\n", db_strerror(errno));
#endif /* DEBUG */
      return errno;
    }
    *lterm = (long)data.data;

    END_STAT(FETCH_TERM);
    
    return 0;
  }
  return EINVAL;
}

/*
  If there are no more solutions the iterator functions succeed but set
  term to NULL.
 */
long global_iterator(db_struct *db, long *litp)
{
  DBC *myitp;
  DBC **itp = &myitp;
  START_STAT(GLOBAL_ITERATOR);
  
  if (db == NULL || db->termsdb == NULL) return EINVAL;

  errno = db->termsdb->cursor(db->termsdb, NULL, itp, 0);
  *litp = (long)(*itp);
#ifdef DEBUG
  if (errno != 0)
    SP_fprintf(SP_stderr, "create global iterator: %s\n", db_strerror(errno));
#endif /* DEBUG */

  END_STAT(GLOBAL_ITERATOR);

  return errno;
}

long global_iterator_next(long lit, long *lterm, long *termref)
{
  DBC *it = (DBC *)lit;
  DBT key, data;

  START_STAT(GLOBAL_ITERATOR_NEXT);
  
  if (it == NULL) return EINVAL;

  memset(&key, 0, sizeof(key));
  memset(&data, 0, sizeof(data));
  errno = it->c_get(it, &key, &data, DB_NEXT);
  if (errno != 0) {
    if (errno == DB_NOTFOUND) { /* no more solutions */
      *lterm = (long)NULL;
      return 0;
    }
#ifdef DEBUG
    SP_fprintf(SP_stderr, "set global iterator: %s\n", db_strerror(errno));
#endif /* DEBUG */
    return errno;
  }

  *lterm = (long)data.data;
  *termref = *(u_int32_t *)key.data;

  END_STAT(GLOBAL_ITERATOR_NEXT);

  return 0;
}

long global_iterator_done(long lit)
{
  DBC *it = (DBC *)lit;
  START_STAT(GLOBAL_ITERATOR_DONE);
  
  if (it) {
    it->c_close(it);            /* error handling? */
    END_STAT(GLOBAL_ITERATOR_DONE);
    return 0;
  }
  return EINVAL;
}

long term_iterator(db_struct *db, SP_term_ref hclist, iterator **itp)
{
  iterator *it;
  SP_term_ref ref = SP_new_term_ref();
  int hcnum;
  long *keyptr;

  START_STAT(TERM_ITERATOR);

  if (db == NULL || db->indexdb == NULL || db->termsdb == NULL)
    return EINVAL;

  /* count the length of the list */
  hcnum = SP_is_compound(hclist) ? 1 : 0;
  if (hcnum) {
    SP_get_arg(2, hclist, ref);
    for (; SP_is_compound(ref); ++hcnum)
      SP_get_arg(2, ref, ref);
  }

  *itp = it = (iterator *)SP_malloc(sizeof(iterator) + (hcnum-1)*sizeof(long));
  if (it == NULL) return ENOMEM;

  it->db = db;
  it->nkeys = hcnum;
  it->ckey = -1;
  for (keyptr = it->key; SP_is_compound(hclist);
       SP_get_arg(2, hclist, hclist), ++keyptr) {
    SP_get_arg(1, hclist, ref);
    SP_get_integer(ref, keyptr);
  }
  if ((errno = db->indexdb->cursor(db->indexdb, NULL, &it->cursor, 0)) != 0) {
#ifdef DEBUG
    SP_fprintf(SP_stderr, "create term iterator: %s\n", db_strerror(errno));
#endif /* DEBUG */
    SP_free(it);
  }

  END_STAT(TERM_ITERATOR);

  return errno;
}

long term_iterator_next(iterator *it, long *lterm, long *termref)
{
  DBT key, data, tdata;
  u_int32_t k;

  START_STAT(TERM_ITERATOR_NEXT);

  if (it == NULL || it->cursor == NULL ||
      it->db == NULL || it->db->termsdb == NULL) return EINVAL;

  if (it->ckey == -1) goto newhc; /* is this the very first time? */

next:                 /* find the next termref under the current hash code */
  memset(&key, 0, sizeof(key));
  memset(&data, 0, sizeof(data));

  errno = it->cursor->c_get(it->cursor, &key, &data, DB_NEXT_DUP);
  if (errno != 0)
    {
      if (errno == DB_NOTFOUND) goto newhc; /* maybe the next key */
      else return errno;          /* real error */
    }
fetch:                          /* DB_SET or DB_NEXT_DUP succeeded */
  memset(&tdata, 0, sizeof(tdata));
  errno = it->db->termsdb->get(it->db->termsdb, NULL, &data, &tdata, 0);
  if (errno == 0) {             /* success */
    *lterm = (long)tdata.data;
    *termref = *(u_int32_t *)data.data;

    END_STAT(TERM_ITERATOR_NEXT);
  
    return 0;
  }
  if (errno != DB_NOTFOUND) return errno; /* real error */
#if 0                           /* See notes.txt! (it->cursor is read-only) */
  it->cursor->c_del(it->cursor, 0); /* dangling termref */
#endif
#ifdef DEBUG
  SP_fprintf(SP_stderr, "dangling termref found\n");
#endif
  goto next;

newhc:                          /* a new hash code */
  if (++it->ckey >= it->nkeys) { /* no more keys to try */
    *lterm = (long)NULL;

    END_STAT(TERM_ITERATOR_NEXT);
  
    return 0;
  }
  
  k = (unsigned long)it->key[it->ckey];
  memset(&key, 0, sizeof(key));
  memset(&data, 0, sizeof(data));
  key.data = &k;
  key.size = sizeof(u_int32_t);
  errno = it->cursor->c_get(it->cursor, &key, &data, DB_SET);
  if (errno == 0) goto fetch;
  if (errno == DB_NOTFOUND) goto newhc; /* maybe the next key */
#ifdef DEBUG
  SP_fprintf(SP_stderr, "set term iterator: %s\n", db_strerror(errno));
#endif /* DEBUG */
  return errno;                 /* real error */
}

long term_iterator_done(iterator *it)
{
  START_STAT(TERM_ITERATOR_DONE);
  
  if (it && it->cursor) {
    it->cursor->c_close(it->cursor); /* error handling? */
    SP_free(it);

    END_STAT(TERM_ITERATOR_DONE);

    return 0;
  }
  return EINVAL;
}

char *decode_error(long err)
{
  return SP_from_os(db_strerror(err), WCX_OPTION);
}
