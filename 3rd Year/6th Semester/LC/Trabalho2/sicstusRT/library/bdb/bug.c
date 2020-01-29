/*
  To compile & link:
  cc -DBUG -I/usr/local/BerkeleyDB/include bug.c /usr/local/BerkeleyDB/lib/libdb.a -o bug
 */

#include <errno.h>
#include <string.h>

#include <sys/types.h>
#include <stdio.h>
#include <db.h>

#define DATABASE "/tmp/bug.db"

long store_termref(DB *db, long hc, long termref)
{
  u_int32_t k = (unsigned long)hc, d = (unsigned long)termref;
  DBT key, data;

  memset(&key, 0, sizeof(key));
  memset(&data, 0, sizeof(data));
  key.data = &k;
  key.size = sizeof(k);
  data.data = &d;
  data.size = sizeof(d);
  errno = db->put(db, NULL, &key, &data, 0);
#if 0
  printf("%u\n", *(u_int32_t *)key.data);
#endif
  return errno;
}

u_int32_t identity(const void *key, u_int32_t size)
{
  return *(u_int32_t *)key;
}

int main()
{
  DB *dbp;
  DB_INFO dbinfo;
  long hc, termref;
  
  memset(&dbinfo, 0, sizeof(DB_INFO));
#ifdef BUG
  dbinfo.h_hash = identity;
  dbinfo.db_pagesize = 4*1024;
#endif
#ifndef BUG
  dbinfo.db_pagesize = 8*1024;
#endif
  dbinfo.flags = DB_DUP;
  if ((errno = db_open(DATABASE,
                       DB_HASH, DB_CREATE, 0664, NULL, &dbinfo, &dbp)) != 0) {
    fprintf(stderr, "db: %s: %s\n", DATABASE, strerror(errno));
    exit (1);
  }

  while (scanf("%ld %ld", &hc, &termref) == 2) {
    printf("%ld %ld\n", hc, termref);
    if ((errno = store_termref(dbp, hc, termref)) != 0) {
      fprintf(stderr, "db: put: %s\n", strerror(errno));
      exit(1);
    }
#if 0
    printf("stored.\n");
#endif
  }

  printf("closing database...\n");

  switch (errno = dbp->close(dbp, 0)) {
  case 0:
    printf("db: database closed.\n");
    break;
  default:
    fprintf(stderr, "db: get: %s\n", strerror(errno));
    exit (1);
  }
  exit(0);
}
