/* Copyright (C) 1990-95 Swedish Institute of Computer Science. */
/*
  Notes: (Last update [PM] 3.9b2)
  . EINTR should be handled as in readchar_tty (allow events to run)
  Notes: (Last update [PM] 3.8.7)
  . Search for NOTE in the code.
  . There are buffer overrun issues in this code, search for
    declarations looking line <SOME NAME>[<SOME NUMBER>]
  . The atom used to hold the name of a socket is never unregistered.
  . As usual Windows support may be less than satisfactory due to the
    non-conformance of WinSock (in 3.8.7 fixed the case of select with
    timeout and no sockets).
*/

#include <sicstus/sicstus.h>  
#include <sicstus/config.h>


#if HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#include <stdio.h>
#include <string.h>
#include <errno.h>

#if HAVE_SYS_SOCKET_H
#include <sys/socket.h>
#endif
#if HAVE_SYS_UN_H
#include <sys/un.h>
#endif
#if HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif
#if HAVE_NETDB_H   
#include <netdb.h>
#endif
#if HAVE_ARPA_INET_H
#include <arpa/inet.h>
#endif
#if HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#if HAVE_WINSOCK_H
#include <winsock.h>
#endif

#if HAVE_UNISTD_H
#include <unistd.h>             /* [PM] 3.9 read(), ... */
#endif

#if HAVE_INTTYPES_H
#include <inttypes.h>           /* [PM] 3.8.7 defines uint32_t */
#endif


#if SP_OS2
/* [PM] the 3.8.5 changes does not take OS/2 into account. */
#error "OS2 not supported"
#endif

#if !HAVE_IN_ADDR_T
/* [PM] 3.8.7 */
/* RedHat 6.1 Linux (glibc 2.1, ramus) does not have in_addr_t, it uses uint32_t */
/* MacOS X 10.0.4 does not have in_addr_t, it instead uses u_int32_t, luckily it also has uint32_t */
/* SUSv2 says: "in_addr_t: An unsigned integral type of exactly 32 bits." */

#define in_addr_t uint32_t
#endif


/*------------------------------------------------------------------*/
/* Prototypes */
#if 1
#include "sockets_glue.h"
#else
extern SP_stream * SPCDECL socket_connect_I PROTOTYPE((long, char *, long));
extern long SPCDECL socket_connect_U PROTOTYPE((long, char *, SP_stream **));
extern SP_stream * SPCDECL socket_accept PROTOTYPE((long, SP_term_ref));
extern long SPCDECL socket_select PROTOTYPE((SP_term_ref, SP_term_ref, SP_term_ref,
				     SP_term_ref, long, long,
				     SP_term_ref, SP_term_ref));
extern long SPCDECL socket_socket_U PROTOTYPE((void));
extern long SPCDECL socket_socket_I PROTOTYPE((void));
extern long SPCDECL socket_bind_U PROTOTYPE((long, char *));
extern long SPCDECL socket_bind_I PROTOTYPE((long, long));
extern void SPCDECL socket_listen PROTOTYPE((long, long));
extern void SPCDECL socket_close PROTOTYPE((long));
extern unsigned long SPCDECL hostname PROTOTYPE((void));
extern void SPCDECL hostname_address PROTOTYPE((char *, SP_term_ref));
extern void SPCDECL address_hostname PROTOTYPE((char *, SP_term_ref));
extern void SPCDECL socket_init PROTOTYPE((int));
extern void SPCDECL socket_deinit PROTOTYPE((int));
extern int SPCDECL socket_setbuf PROTOTYPE((SP_stream *s, long read, SP_term_ref old, SP_term_ref new));
#endif
/*------------------------------------------------------------------*/
/* Types */

#if SP_WIN32
#define SPSock SOCKET
#else
#define SPSock int
#endif

typedef struct {
  SPSock sock;
  int eof;
  int err;
  int rd_cnt;
  int wr_cnt;
  int rd_size;
  int wr_size;
  unsigned char *rd_buf;
  unsigned char *rd_ptr;
  unsigned char *wr_buf;
  unsigned char *wr_ptr;
  void (SPCDECL *free_fun)(void*); /* [PM] 3.9b4 pointer to SP_free */
} SP_socket;


/*------------------------------------------------------------------*/
/* Macros */

#if !SP_WIN32
#define SPSOCKERR -1
#define SPINVSOCK -1
#if SP_OS2
static void sp_os2sockerror PROTOTYPE((char *, char *));
#define SYSERROR(Pred,Call) {sp_os2sockerror(Pred,Call); goto error;}
#else  /* !SP_OS2 */
#define SYSERROR(Pred,Call) {SP_syserror_clib(Pred,Call); goto error;}
#endif /* !SP_OS2 */
#else  /* SP_WIN32 */
#define SPSOCKERR SOCKET_ERROR
#define SPINVSOCK INVALID_SOCKET

/* static void sp_winsockerror PROTOTYPE((char *, char *));*/

#define SYSERROR(Pred,Call) {SP_syserror_win32(Pred,Call); goto error;}
#define NOT_SUPPORTED(Pred) \
{ SP_term_ref tr = SP_new_term_ref(); \
  SP_put_string(tr, "Address family AF_UNIX not supported"); \
  SP_raise_exception(tr); \
}

#endif /* SP_WIN32 */

#ifndef FD_SETSIZE
#if DBG
#error "Tell sicstus-support that FD_SETSIZE was undefined/PM" /* [PM] 3.8.7 */
#endif /* DBG */
#endif /* FD_SETSIZE */

#ifndef FD_SETSIZE
#if DBG
#error "Tell sicstus-support that FD_SETSIZE was undefined/PM" /* [PM] 3.8.7 */
#endif /* DBG */
#endif /* FD_SETSIZE */
#ifndef FD_SET
#if DBG
#error "Tell sicstus-support that FD_SET was undefined/PM" /* [PM] 3.8.7 */
#endif /* DBG */
#endif /* FD_SET */

/* Following are missing in some sys/types.h */
#ifndef FD_SETSIZE
#define FD_SETSIZE   256
#endif
#ifndef FD_SET
#define	FD_SET(n, p)	((p)->fds_bits[(n)/8] |= (1 << ((n) % 8)))
#endif
#ifndef FD_CLR
#define	FD_CLR(n, p)	((p)->fds_bits[(n)/8] &= ~(1 << ((n) % 8)))
#endif
#ifndef FD_ISSET
#define	FD_ISSET(n, p)	((p)->fds_bits[(n)/8] & (1 << ((n) % 8)))
#endif
#ifndef FD_ZERO
#define	FD_ZERO(p)	memset((char *)(p), 0, sizeof (*(p)))
#endif

/*------------------------------------------------------------------*/
static SP_atom atm_unbuf;
static SP_atom atm_fullbuf;

/*------------------------------------------------------------------*/
/*  */

void SPCDECL socket_init(SPAPI_ARG_PROTO_DECL 
			 int when)
{
#if SP_WIN32
  WSADATA wsa_data;
  WORD req_version = MAKEWORD(1,1);

  if (WSAStartup(req_version, &wsa_data) != 0)
    {
#if 1  /* [PM] 3.8.5 */
      SYSERROR("socket_init/1", "WSAStartup");
#else  /* [PM] 3.8.5 Was */
      fprintf(stderr, "WSAStartup() failed\n"); /* for now */
#endif
    }

  /* for now:
  fprintf(stderr, "szDescription = \"%s\"", wsa_data.szDescription);
  fprintf(stderr, "szSystemStatus = \"%s\"", wsa_data.szSystemStatus);
  */
#endif
#if SP_OS2
  sock_init();
#endif

#if SP_WIN32
 error: /* [PM] perhaps a good idea to register atoms even if error (from SYSERROR) */
#endif /* SP_WIN32 */

  (void)when;                   /* [PM] 3.9b5 avoid -Wunused */

  (void)SP_register_atom(atm_unbuf = SP_atom_from_string("unbuf"));
  (void)SP_register_atom(atm_fullbuf = SP_atom_from_string("fullbuf"));
}

void SPCDECL socket_deinit(SPAPI_ARG_PROTO_DECL 
			   int when)
{
  (void)when;                  /* [PM] 3.9b5 avoid -Wunused */

#if SP_WIN32
  (void) WSACleanup();
#endif
  (void)SP_unregister_atom(atm_unbuf);
  (void)SP_unregister_atom(atm_fullbuf);
}


/*------------------------------------------------------------------*/
/* Create a new socket stream */

/* Don't use stdio since it doesn't handle the case when system calls
   are interrupted. */

static int SPCDECL socket_sgetc(void *handle)
{
  SP_socket *s = (SP_socket *)handle;
  int i;

  if (s->eof || s->err)
    return -1;
  if (s->rd_cnt-- > 0)
    return *s->rd_ptr++;

  s->rd_ptr = s->rd_buf;

#if !(SP_WIN32 || SP_OS2)
  for (;;)	  /* if read is interrupted, try again */
    if ((i=read(s->sock, s->rd_buf, s->rd_size)) > 0)
      {
	s->rd_cnt = i-1;
	return *s->rd_ptr++;
      }
    else if (i==0)		/* eof */
      {
	s->eof = 1;
	return -1;
      }
    else if (errno != EINTR)
      {
	s->err = 1;
	return -1;
      }
#if DBG
    else                        /* EINTR */
      {
        fprintf(stderr, "\nsocket read interrupted by EINTR, retrying\n");fflush(stderr);
      }
#endif /* DBG */
#else  /* (SP_WIN32 || SP_OS2) */
  if ((i=recv(s->sock, s->rd_buf, s->rd_size, 0/*no flags*/)) > 0)
      {
	s->rd_cnt = i-1;
	return *s->rd_ptr++;
      }
  else if (i==0)		/* eof */
    {
      s->eof = 1;
      return -1;
    }
  else
    {
      s->err = 1;
      return -1;
    }
#endif /* (SP_WIN32 || SP_OS2) */
}

static int flushbuf(ch, s)
     unsigned ch;
     SP_socket *s;
{
  int n, i;
  unsigned char *p;

  n = s->wr_size - s->wr_cnt;
  p = s->wr_ptr = s->wr_buf;
  s->wr_cnt = s->wr_size;
#if !(SP_WIN32 || SP_OS2)
  for (;;)
    {	    /* if write is interrupted, call again with rest of buffer */
      if ((i=write(s->sock, (char *)p, n)) == n)
	return ch;
      else if (i<0 && errno != EINTR)
	{
	  s->err = 1;
	  return -1;
	}
      if (i < 0)
	i = 0;
      n -= i;
      p += i;
    }
#else  /* (SP_WIN32 || SP_OS2) */
  for (;;)
    {
      if ((i=send(s->sock, (char *)p, n, 0 /* no flags */)) == n)
	return ch;
      else if (i == SPSOCKERR)
	{
	  s->err = 1;
	  return -1;
	}
      n -= i;
      p += i;
    }
#endif /* (SP_WIN32 || SP_OS2) */
}

static int SPCDECL socket_sputc(char c, void *handle)
{
  unsigned char ch = (unsigned char)c;
  SP_socket *s = (SP_socket *)handle;

  if (s->err)
    return -1;
  if (--s->wr_cnt > 0)
    {
      *s->wr_ptr++ = ch;
      return ch;
    }
  *s->wr_ptr = ch;
  return flushbuf(ch, s);
}

static int SPCDECL socket_sflush(void *handle)
{
  SP_socket *s = (SP_socket *)handle;

  if (s->err)
    return -1;
  return flushbuf(0, s);
}

static int SPCDECL socket_seof(void *handle)
{
  return ((SP_socket *)handle)->eof;
}


static void SPCDECL socket_sclrerr(void *handle)
{
  ((SP_socket *)handle)->err = 0;
}


static int SPCDECL socket_sclose(void *handle) 
{
  SP_socket *s = (SP_socket *)handle;

  if (s->wr_cnt < s->wr_size)
    flushbuf(0, s);
  /*  shutdown(s->sock, 2);*/
#if SP_WIN32
  closesocket(s->sock);
#else
#if SP_OS2
  soclose(s->sock);
#else
  close(s->sock);
#endif
#endif
  (*s->free_fun)(s->rd_buf);
  (*s->free_fun)(s->wr_buf);
  (*s->free_fun)((char *)s);
  return 0;
}

/*------------------------------------------------------------------*/

/* [PM] 3.8.5 this was defined but not used */
#define IS_SOCKET_STREAM(S) ((S)->sclose == socket_sclose)

/* [PM] 3.8.5 */
#define IS_FD_STREAM(S) ((S)->fd != -1) /* xref SP_make_stream_context@streams.c */
#if SP_WIN32
#define IS_SELECTABLE(S) (IS_SOCKET_STREAM(S))
#define SOCKET_FOR_SELECT(S) (((SP_socket *)(S)->user_handle)->sock)
#else  /* BSD sockets sockets are just file descriptors */
#define IS_SELECTABLE(S) (IS_FD_STREAM(S))
#define SOCKET_FOR_SELECT(S) ((S)->fd)
#endif



static SP_stream *new_socket_stream(SPAPI_ARG_PROTO_DECL 
				    SPSock sock,
				    char *socket_name)
{
  SP_stream *s;
  SP_socket *socket;
  SP_term_ref tr = SP_new_term_ref();

  socket = (SP_socket *)SP_malloc(sizeof(SP_socket));
  socket->sock = sock;
  socket->err = 0;
  socket->rd_buf = (unsigned char *)SP_malloc(BUFSIZ);
  socket->rd_ptr = socket->rd_buf;
  socket->eof = 0;
  socket->wr_buf = (unsigned char *)SP_malloc(BUFSIZ);
  socket->wr_ptr = socket->wr_buf;
  socket->rd_size = BUFSIZ;	/* input is buffered as of 3.8 */
  socket->rd_cnt = 0;
  socket->wr_size = BUFSIZ;	/* output was already buffered in pre-3.8 */
  socket->wr_cnt = socket->wr_size;
  socket->free_fun = SP_free;

  SP_make_stream_context(
		 (ANYPOINTER)socket,
		 socket_sgetc,
		 socket_sputc,
		 socket_sflush,
		 socket_seof,
		 socket_sclrerr,
		 socket_sclose,
		 &s,
		 SP_WCX_FLAG,   /* [PM] 3.8.7 does anything but binary mode really make sense? */
		 SP_STREAMHOOK_LIB);

  /* [PM] 3.8.5 NOTE: this kludge is ugly, should SP_malloc as part of
     socket structure above. At the same time should prevent buffer
     overruns in callers by taking several name fragmens to
     concatenate.
  */
  SP_put_string(tr, socket_name); /* socket_name is not persistent */
  SP_get_string(tr, &s->filename);
  SP_register_atom(SP_atom_from_string(socket_name)); /* [PM] 3.8.5 */

  s->modename = "socket";
#if !SP_WIN32
  s->fd = sock;                 /* xref SOCKET_FOR_SELECT() */
#endif
  return s;
}


/*------------------------------------------------------------------*/
/* Socket stream functions */

long SPCDECL socket_setbuf(SPAPI_ARG_PROTO_DECL 
			   void *p,
			   long read,
			   SP_term_ref old,
			   SP_term_ref new)
{
  SP_stream *s = (SP_stream *)p;
  SP_socket *socket;
  SP_atom a;
  int i;
  
  if (!IS_SOCKET_STREAM(s)) return 0;

  socket = (SP_socket *)s->user_handle;
  
  if (read)
    a = (socket->rd_size == 1) ? atm_unbuf : atm_fullbuf;
  else
    a = (socket->wr_size == 1) ? atm_unbuf : atm_fullbuf;

  {
    SP_term_ref t = SP_new_term_ref();
    /* [PM] 3.8.7 These three statements and their order is so that
       socket_buffering(Stream, ReadWrite, ExistingValue, ExistingValue)
       gives a way to read the buffering without altering it.
       Also, if unifying Old fails then no new value is set.
     */
    SP_put_atom(t, a);
    if (!SP_unify(old, t)) return 0;
    if (!SP_get_atom(new, &a)) return 0;
  }

  i = (a == atm_unbuf) ? 1 : BUFSIZ;
  if (read)
    socket->rd_size = i;
  else
    socket->wr_size = socket->wr_cnt = i;
  return 1;
}

#if SP_WIN32
#define SP_in_addr_t unsigned long
#define SP_INADDR_NONE INADDR_NONE
#else
#define SP_in_addr_t in_addr_t
#define SP_INADDR_NONE ((in_addr_t)-1)
#endif

/*
 *  prolog predicate '$connect_I'(+Socket, +Machine, +Port, -Stream).
 */
SP_stream * SPCDECL socket_connect_I(SPAPI_ARG_PROTO_DECL 
				     long socket, char *host, long pl_port)
{
  struct hostent *hp;
  struct sockaddr_in server;
  SP_stream *stream;
  unsigned short port = (unsigned short) pl_port;

  memset(&server, 0, sizeof(server));
  server.sin_family = AF_INET;
  hp = gethostbyname(host);
  if (hp != NULL)
    {
        memcpy((char *)&server.sin_addr, (char *)hp->h_addr, hp->h_length);
    }
  else                          /* gethostbyname() failed */
    {
      /* [PM] 3.8.7 gethostbyname may not work on numbers-and-dots IP-addresses.
         1. WinSock documents that it does not work, but it seems to work
         2. SUSv2 gives no indication that it should work.
         3. Solaris does not say numeric addresses should work
         4. Linux allows IPv4 and IPv6 numeric addresses

         It is convenient if it works so if gethostbyname() fails we try inet_addr
       */
      int saved_errno = errno;
      SP_in_addr_t addr = inet_addr(host);
      
      if (addr == SP_INADDR_NONE)
        {
          errno = saved_errno;  /* pretend gethostbyname was the failing call */
          SYSERROR("socket_connect/3", "gethostbyname");
        }
      else
        {
          server.sin_addr.s_addr = addr;
        }
    }
  
  server.sin_port = htons(port);

  if (connect((SPSock)socket, (struct sockaddr *)&server, sizeof server) == SPSOCKERR)
    {
#if SP_WIN32
      if (0)
        {}
#else  /* !SP_WIN32 */
      if (errno == EINTR)
        {
          /* [PM] 3.8.7 Treat EINTR as no error
            SUSv2 says:
               If the connection cannot be established immediately and O_NONBLOCK
               is not set for the file descriptor for the socket, connect() will
               block for up to an unspecified timeout interval until the connection
               is established. If the timeout interval expires before the
               connection is established, connect() will fail and the connection
               attempt will be aborted. If connect() is interrupted by a signal
               that is caught while blocked waiting to establish a connection,
               connect() will fail and set errno to [EINTR], but the connection
               request will not be aborted, and the connection will be established
               asynchronously.
            Linux:
               Does not list EINTR as a possible return value. I take this to
               mean EINTR cannot happen.
            Solaris (SunOS 5.7):
               Man page is bogus. if using connect(3XN) (cc -lxnet) then the
               SUSv2 semantics apply, unfortunately we do not use -lxnet.
               Still, the behaviour if ignoring EINTR is unlikely to be worse
               than before.

            What we could do and that would probably accommodate all the
            different semantics of connect on UNIX is to do a select for
            writeability here. For SUSv2 this would block until really
            connected. For Solaris it would most likely return immediately with
            either an error or with socket in the errorfds set.
           */
          ;                     /* do nothing, treat it as a successful connect */
        }
#endif /* !SP_WIN32 */
      else
        {
          SYSERROR("socket_connect/3", "connect");
        }
    }

  {
    /* [PM] 3.8.7 RFC 1035: "Labels must be 63 characters or less."
       port is unsigned 16bits 0..65535, that is 5 decimal chars */
       
    char sockname     [1 + 63+1+63+1+63+1+63 +1+   5  + 1 +  1  ];
    sprintf(sockname, "<"        "%s"        ":" "%d"  ">", host, (int)port);
    if (!(stream = new_socket_stream(SPAPI_ARG socket, sockname))) /* [PM] 3.8.7 bogus, cannot fail */
      SYSERROR("socket_connect/3", "fdopen");
  }
  return stream;
error:
  return NULL;
}

/*
 *  prolog predicate connect_U(+Socket, +Path, -Stream).
 */
long SPCDECL socket_connect_U(SPAPI_ARG_PROTO_DECL 
			      long socket, char *path, SP_stream **streamp)
{
#if SP_WIN32
  NOT_SUPPORTED("socket_connect/3");
#else  /* !SP_WIN32 */
  struct sockaddr_un server;
  
  memset(&server, 0, sizeof(server));
  /* Name socket */
  server.sun_family = AF_UNIX;

  if (strlen(path) > sizeof(server.sun_path)-1)
    {
      goto error;
    }

  strcpy(server.sun_path, path);
  
  if (connect(socket, (struct sockaddr *)&server, sizeof server) == SPSOCKERR)
    {
      if (errno == EINTR)       /* [PM] 3.8.7 See socket_connect_I */
        {
          ;                     /* do nothing, treat it as a successful connect */
        }
      else
        {
          SYSERROR("socket_connect/3", "connect");
        }
    }

  {
    char sockname[sizeof(server.sun_path)+2]; /* [PM] 3.8.5. +2 for "<" and ">" */

    sprintf(sockname, "<%s>", path);
    if (!(*streamp = new_socket_stream(SPAPI_ARG socket, sockname)))
      {
        SYSERROR("socket_connect/3", "fdopen");
      }
  }
  return 1;
error:
#endif /* !SP_WIN32 */
  return -1;
}

/*
 *  prolog predicate '$accept'(+Socket, -Client, -Stream).
 */
SP_stream * SPCDECL socket_accept(SPAPI_ARG_PROTO_DECL 
				  long socket, SP_term_ref client)
{
  SPSock msgsock;
  struct sockaddr_in client_addr;
#if SP_AIX 
  size_t
#else
  int 
#endif
    len = sizeof(struct sockaddr_in);
  SP_stream *stream;
  
  memset(&client_addr, 0, sizeof(client_addr));
  
  for (;;)
    {
      if ((msgsock = accept((SPSock)socket,
                            (struct sockaddr *)&client_addr,&len)) == SPINVSOCK)
        {
          if (

#if SP_WIN32
              1
#else  /* !SP_WIN32 */
              errno != EINTR   /* [PM] 3.8.7 */
#endif /* !SP_WIN32 */

              )
            {
              SYSERROR("socket_accept/2", "accept");
            }
        }
      else
        {
          break;
        }
    }

  if (client_addr.sin_family == AF_INET)
    {
      char *ip;
      /* Try to get the IP number */
      if ((ip = (char *)inet_ntoa(client_addr.sin_addr)) != NULL)
	SP_put_string(client, ip);
      else
	SYSERROR("socket_accept/3","inet_ntoa");
    }
  else
    SP_put_variable(client);
  
  {
    char sockname[        8      +  20 + 1 +  1]; /* [PM] 3.8.7 assuming long is at most 64 bits */
    sprintf(sockname, "<socket "  "%ld" ">", (long) msgsock);
    if (!(stream = new_socket_stream(SPAPI_ARG msgsock, sockname)))
      SYSERROR("socket_accept/2", "fdopen");
  }
  
  return stream;
error:
  return NULL;
}

/*
 * $select(?Sockets, -NewStreamsCodes, -NewClients,
 *	   +TimeOutSecs, +TimeOutUSecs,
 *	   +StreamCodes, -ReadStreamCodes, -Result)
 *
 * Optionally listens on sockets for new connections.
 * Optionally listens on streams for new data.
 */

static int set_select_sock(readyp, sock, max_fdp)
     fd_set *readyp;
     SPSock sock;
     int *max_fdp;
{

#if !SP_WIN32
  if (sock < 0 || sock > FD_SETSIZE)
    return -1;
#endif /* !SP_WIN32 */

#if SP_WIN32
  /* [PM] 3.8.7 WinSock ignores the max_fd argument to select(). Instead we use it to know the size of the FD_SET. */
  if (!FD_ISSET(sock, readyp))
    {
      (*max_fdp)++;
    }
#else  /* !SP_WIN32 */
  if (sock > *max_fdp)
    *max_fdp = sock;
#endif

  FD_SET(sock, readyp);
  return 0;
}

static int new_connection(SPAPI_ARG_PROTO_DECL 
			  SPSock sock,
			  fd_set *readyp,
			  SP_stream **sp,
			  SP_term_ref client)
{
  SPSock msgsock;

  if (FD_ISSET(sock, readyp))
    {
      struct sockaddr_in client_addr;
#if SP_AIX
      size_t
#else
      int
#endif
        len = sizeof(struct sockaddr_in);

      memset(&client_addr, 0, sizeof(client_addr));
      for (;;)
        {
          if ((msgsock=accept((SPSock)sock,
                              (struct sockaddr *)&client_addr,
                              &len)) == SPINVSOCK)
            {
              if (

#if SP_WIN32
                  1
#else  /* !SP_WIN32 */
                  errno != EINTR   /* [PM] 3.8.7 */
#endif /* !SP_WIN32 */

                  )
                {
                  SYSERROR("socket_select/6", "accept");
                }
            }
          else
            {
              break;
            }
        }

      if (client_addr.sin_family == AF_INET)
	{
	  char *ip;
	  /* Try to get the IP number */
	  if ((ip = (char *)inet_ntoa(client_addr.sin_addr)) != NULL)
	    SP_put_string(client, ip);
	  else
	    SYSERROR("socket_select/6","inet_ntoa");
	}
      else
	SP_put_variable(client);
      
      /* make a stream of the 'msgsock' */
      {
        char sockname[        8      +   20 + 1 +  1]; /* [PM] 3.8.7 assuming long is at most 64 bits */

        sprintf(sockname, "<socket "   "%ld" ">", (long)msgsock);
        if (!(*sp = new_socket_stream(SPAPI_ARG msgsock, sockname)))
          SYSERROR("socket_select/6", "open socket stream");
      }
      FD_CLR((SPSock)sock, readyp);
    }
  else
    {
      *sp = NULL;
    }
  return 0;
 error:
  return -1;
}

/* '$select'(+term,-term,-term,-term,+integer,+integer, +term,-term,[-integer]) 
 Return codes:
 0 -- no error
 1 -- illarg(domain(term,sockets), ...) 
 2 -- illarg(domain(term,stream_with_IO_descriptor), ...)
 3 -- retry due to EINTR. Nothing was selected. (3.9b5)
*/
long SPCDECL socket_select(SPAPI_ARG_PROTO_DECL 
			   SP_term_ref pass_socks,
			   SP_term_ref csocks,
			   SP_term_ref new_streams,
			   SP_term_ref new_clients,
			   long to_sec,
			   long to_usec,
			   SP_term_ref streams,
			   SP_term_ref read_streams)
{
  int max_fd=0;
  int max_was_ready=0;
  int should_retry = 0;         /* [PM] 3.9b5 1 if we return prematurely due to EINTR (and nothing was selected) */

  struct timeval timeout, *timeoutptr;
  fd_set ready;
  fd_set was_ready;             /* [PM] 3.8.7 those that contain buffered data */
  SP_term_ref
    car = SP_new_term_ref(),
    cdr = SP_new_term_ref(),
    tmp = SP_new_term_ref();

	  
  if (to_sec < 0)
    timeoutptr = NULL;          /* block forever */
  else
    {
      timeout.tv_sec = to_sec;
      timeout.tv_usec= to_usec;
      timeoutptr = &timeout;
    }

  FD_ZERO(&ready);
  FD_ZERO(&was_ready);

  {
    SP_put_term(cdr, pass_socks);	/* Find passive sockets */
    while (SP_is_list(cdr))
      {
        long psock;
	SP_get_list(cdr, car, cdr);
	
	if ( (!SP_get_integer(car, &psock))
             || 
             set_select_sock(&ready, (SPSock)psock, &max_fd) < 0 )
          {
            return 1;             /* illarg(domain(term,sockets), ...) */
          }
      }
  }
  
  SP_put_term(cdr, streams);	/* Find others to watch */
  while (SP_is_list(cdr))
    {
      ANYPOINTER ptr;

      SP_get_list(cdr, car, cdr);

      if ( (!SP_get_address(car, &ptr)) /* 3.8.5 check result */
           || (!IS_SELECTABLE((SP_stream *)ptr)) /* [PM] 3.8.5 SPRM 1542 added guard */
           )
        {
          return 2; /* illarg(domain(term,stream_with_IO_descriptor), ...) */
        }

      /* [PM] 3.8.7 For socket streams we should not do select unless they might block.
         A socket stream will not block if (xref socket_sgetc)
         . it is at EOF. Note that (mode & modeResetOnEof) cannot hold for socket streams.
         . it has en error
         . there is data in the read buffer
         . there is an unput byte in the special one-byte peek buffer
           used by some routines in SICStus run-time.

         Thanks to Niclas Finne and Joakim Eriksson for suggesting this modification.
      */
      if (IS_SOCKET_STREAM((SP_stream *)ptr)
          && (   0
                 || ((SP_socket *)((SP_stream *)ptr)->user_handle)->rd_cnt > 0
                 || ((SP_socket *)((SP_stream *)ptr)->user_handle)->eof
                 || ((SP_socket *)((SP_stream *)ptr)->user_handle)->err
                 || ((SP_stream *)ptr)->peek > -2 /* xref readchar() Emulator/inout.c */
                 )
          &&
          /* Remember it was already ready.
             If this fails then so will set_select_sock(&ready, ...) below. */
          set_select_sock(&was_ready, SOCKET_FOR_SELECT((SP_stream *)ptr), &max_was_ready) >= 0
          )
        {
          /* Reading would not block. Do not add this stream to the 'ready' set.
             Ensure immediate timeout. */
          timeout.tv_sec = 0;
          timeout.tv_usec= 0;
          timeoutptr = &timeout;
        } 
      else if ( set_select_sock(&ready, SOCKET_FOR_SELECT((SP_stream *)ptr), &max_fd) < 0 )
        {
          return 2; /* illarg(domain(term,stream_with_IO_descriptor), ...) */
        }
    }

  {
    int call_select = 1;

#if SP_WIN32
    {
    /* WinSock select() will give an error if called with no sockets. */

    if (max_fd==0)              /* nothing for select() to do except possibly wait for timeout */
      {
        call_select = 0;        /* do not call select */
        if (timeoutptr==NULL)   /* want to block forever,waiting for no sockets...
                                   surely that is an error (also on UNIX actually) */
          {
            SYSERROR("socket_select/6", "select");
          }
        else                    /* timeoutptr is set. This case also applies if some stream had buffered data. */
          {
            if (timeout.tv_sec == 0
                && timeout.tv_usec == 0 /* immediate timeout */
                )
              {
                ; /* do nothing (and do not call select) */
              }
            else                /* no sockets, user just wants a timeout */
              {
                SPSock dummy_sock;
                fd_set dummy_set;
                
                /* We use select on a dummy socket to implement a wait for the timeout interval */

                dummy_sock = socket(AF_INET, SOCK_STREAM, 0);
                if (dummy_sock == SPINVSOCK)
                  {
                    SYSERROR("socket_select/6", "select");
                  }
                
                FD_ZERO(&dummy_set);
                FD_SET(dummy_sock, &dummy_set);
                /* We wait for an error condition to occur on our just created socket.
                   Hopefully, there cannot possibly occur an error condition
                */
                {
                  int rc = select(42, NULL, NULL, &dummy_set, timeoutptr);

                  closesocket(dummy_sock);
                  if (rc == SPSOCKERR)
                    {
                      SYSERROR("socket_select/6", "select");
                    }
                }
              }
          }
      }
    /* Here call_select is false iff READY is an empty set.
       WAS_READY can be anything, regardless of call_select */
    }
#endif /* SP_WIN32 */

    /* wait for input on any socket */
    while (call_select)         /* [PM] 3.8.7 loop while EINTR (SPRM 2643) */
      {
        if (select(max_fd + 1, &ready, (fd_set *)0, (fd_set *)0, timeoutptr) == SPSOCKERR)
          {
            #if !SP_WIN32
            {
            if (errno == EINTR)
              {
                /* We need to handle any SP_event-events installed by
                   signal handlers (e.g., on SIGINT or a timeout
                   exception from library(timeout)) and if these does
                   not raise an exception we should continue with the
                   timeout decreased appropriately. Unfortunately,
                   there is no simple way to do that.

                   Instead we set the timeout to zero, ensuring prompt handling
                   in the case that the signal is SIGINT.

                   We set the timeout even if the original call did
                   not specify a timeout. This could conceivably cause
                   problems since the caller may not expect a
                   timeout. In 3.8.7 the prolog level select returned
                   to the user even if a premature timeout
                   happened. In 3.9b5 the socket.pl code detect this
                   case and simply restarts the socket_select call.

                   What we should do is to pass the remaining time out
                   to the caller (only select/10) and let it re-start
                   the call with updated timeout. Since we return to
                   prolog any SP_events installed by th signal handler
                   will be handled properly automagically. What we do
                   (3.9b5) is to ignore the time passed waiting and
                   restart with the original timeout. For finite
                   timeouot this could wait forever in the unlikely
                   case that an EINTR happens during all waits.

                */
                should_retry = 1; /* [PM] 3.9b5 called should retry due to EINTR */

                timeout.tv_sec = 0;
                timeout.tv_usec= 0;
                timeoutptr = &timeout;
                continue;
              }
            }
            #endif              /* !SP_WIN32 */
            SYSERROR("socket_select/6", "select");
          }
        else
          {
            break;
          }
      }
  }

  /* Check if there are any new connections. If so, open new streams. */
  {
    SP_put_term(cdr, pass_socks);
    /* FLI guarantees that initially new_streams == atom_nil */
    while (SP_is_list(cdr))
      {
        long psock;
        SP_stream *s;

	SP_get_list(cdr, car, cdr);

	if ((!SP_get_integer(car, &psock))
            ||
            new_connection(SPAPI_ARG (SPSock)psock, &ready, &s, tmp) < 0 )
          {
            goto error;
          }
	if (s != NULL)
	  {
            should_retry = 0;   /* [PM] 3.9b5 do not retry if anything selected */
	    SP_cons_list(csocks, car, csocks);
	    SP_cons_list(new_clients, tmp, new_clients);
	    SP_put_address(tmp, (ANYPOINTER)s);
	    SP_cons_list(new_streams, tmp, new_streams);
	  }
      }
  }

  /* Make a list of 'old' streams that has something unread */
  SP_put_term(cdr, streams);
  /* FLI guarantes that initially read_streams == atom_nil */
  while (SP_is_list(cdr))
    {
      ANYPOINTER ptr;

      SP_get_list(cdr, car, cdr);
      SP_get_address(car, &ptr);
      /* [PM] the error checks were already done above so may be unnecessary */
      if ( SP_get_address(car, &ptr) /* 3.8.5 check result */
           && IS_SELECTABLE((SP_stream *)ptr) /* [PM] 3.8.5 SPRM 1542 added guard */
           && ( FD_ISSET(SOCKET_FOR_SELECT((SP_stream *)ptr), &ready)
                /* if (it is a socket stream and) there are characters to be
                   read we will add the stream as readable */
                || FD_ISSET(SOCKET_FOR_SELECT((SP_stream *)ptr), &was_ready)
                )
           )
        {
          should_retry = 0;   /* [PM] 3.9b5 do not retry if anything selected */
          SP_cons_list(read_streams, car, read_streams);
        }
    }
  return (should_retry ? 3 : 0);
error:
  return 1;
}


/*------------------------------------------------------------------*/
/* Stream independent part */

/*
 *  prolog predicate '$socket_U'(-Socket)
 */
long SPCDECL socket_socket_U(SPAPI_ARG_PROTO_DECL0)
{
#if SP_WIN32
  NOT_SUPPORTED("socket/2");
#else
  SPSock sock =  socket(AF_UNIX, SOCK_STREAM, 0); /* [PM] 3.8.7 never EINTR */
  
  if (sock == SPINVSOCK)
    SYSERROR("socket/2", "socket");
  return (long)sock;
error:
#endif
  return -1;
}

/*
 *  prolog predicate '$socket_I'(-Socket)
 */
long SPCDECL socket_socket_I(SPAPI_ARG_PROTO_DECL0)
{
  SPSock sock =  socket(AF_INET, SOCK_STREAM, 0); /* [PM] 3.8.7 never EINTR */
  
  if (sock == SPINVSOCK)
    SYSERROR("socket/2","socket");
  return (long)sock;
error:
  return -1;
}

/*
 *  prolog predicate '$bind_I'(+Socket, +PortIn, ?PortOut)
 */
long SPCDECL socket_bind_I(SPAPI_ARG_PROTO_DECL 
			   long sock, long port)
{
  struct sockaddr_in server;
#if SP_AIX
  size_t
#else
  int
#endif
    length = sizeof(struct sockaddr_in);
    
  memset(&server, 0, sizeof(server));
  server.sin_port = htons((unsigned short)port);
  server.sin_family = AF_INET;
  server.sin_addr.s_addr = INADDR_ANY;
  
  if (bind((SPSock)sock, (struct sockaddr *)&server, sizeof server) == SPSOCKERR) /* [PM] 3.8.7 never EINTR */
    SYSERROR("socket_bind/2", "bind");
  
  /* Find out assigned port number */
  if (getsockname((SPSock)sock, (struct sockaddr *)&server, &length) == SPSOCKERR) /* [PM] 3.8.7 never EINTR */
    SYSERROR("socket_bind/2", "getsockname");
  return ntohs(server.sin_port);
error:
  return -1;
}

/*
 *  prolog predicate '$bind_U'(+Socket, +Path).
 */
long SPCDECL socket_bind_U(SPAPI_ARG_PROTO_DECL 
			   long sock, char *path)
{
#if SP_WIN32
  NOT_SUPPORTED("socket_bind/2");
#else
  struct sockaddr_un server;
  
  memset(&server, 0, sizeof(server));
  /* Name socket */
  server.sun_family = AF_UNIX;
  if (strlen(path) > sizeof(server.sun_path)-1)
    goto error;

  strcpy(server.sun_path, path);
  unlink(path);
  
  if (bind(sock, (struct sockaddr *)&server, sizeof server) == SPSOCKERR) /* [PM] never EINTR */
    SYSERROR("socket_bind/2", "bind");
  return 1;
error:
#endif
  return 0;
}

/*
 *  prolog predicate $listen(+Socket, +Length).
 */
void SPCDECL socket_listen(SPAPI_ARG_PROTO_DECL 
			   long sock, long length)
{
  if (listen((SPSock)sock, length) == SPSOCKERR) /* [PM] 3.8.7 never EINTR */
    SYSERROR("socket_listen/2", "listen");
 error:
  ;
}

/*
 *  prolog predicate socket_close(+Socket).
 */
void SPCDECL socket_close(SPAPI_ARG_PROTO_DECL 
			  long sock)
{
  int rc;
#if SP_WIN32
    rc = closesocket((SPSock)sock);
#elif SP_OS2
    rc = soclose(sock);
#else  /* BSD sockets */
 restart:
    rc = close(sock);
#endif

  if (rc == SPSOCKERR)
    {
#if !SP_WIN32
      if (errno == EINTR)
        {
          /* [PM] 3.8.7 what to do?

             Most code examples I have seen do not look at the return value of
             close(). The code that do check for close() == -1r do retry the
             close() while errno is EINTR.
             
             SUSv2 says:
                If close() is interrupted by a signal that is to be caught, it
                will return -1 with errno set to [EINTR] and the state of fildes
                is unspecified.
                (no indication as to what the correct response would be)
           */
          goto restart;
        }
#else  /* SP_WIN32 */
      /* nothing here, WinSock does not have EINTR */
#endif /* SP_WIN32 */

      SYSERROR("socket_close/1", "close");
    }
 error:
  ;
}
/* [PM] NOTE: buffer overrun! */
static void try_aliases (struct hostent *host_entry, char name[255+1])
{
  /* If h_name is not qualified, try one of the aliases */
  char **aliases;
  
  strcpy(name, host_entry->h_name);
  if ((aliases=host_entry->h_aliases) != NULL)
    {
      while (!strchr(name, '.') && *aliases)
	strcpy(name, *aliases++);
      if (!strchr(name, '.'))
	strcpy(name, host_entry->h_name);
    }
}

/*
 *  prolog predicate $hostname(?HostName).
 */
SP_atom SPCDECL hostname(SPAPI_ARG_PROTO_DECL0)
{
  char name[255+1];             /* [PM] 3.8.7 SUSv2: "Host names are limited to 255 bytes." */

  if (gethostname(name, sizeof(name)) == SPSOCKERR) /* [PM] 3.8.7 never EINTR */
    SYSERROR("hostname/1", "gethostname");

  if (!strchr(name, '.'))
    {
      struct hostent *host_entry;
      
      /* If the name is not qualified, then pass the name through the name
	 server to try get it fully qualified */
      if ((host_entry = gethostbyname(name)) == NULL) /* [PM] 3.8.7 never EINTR */
 	SYSERROR("hostname/1", "gethostbyname"); 
      try_aliases(host_entry, name);

#if 0
      /* If still unqualified, then get the domain name explicitly.
	 This code is NIS specific, and causes problems on some machines.
	 Apollos don't have getdomainname, for example. */
      if (!strchr(name, '.'))
	{
	  char domain[128];
	  
	  if (getdomainname(domain, sizeof(domain)) == -1)
	    SYSERROR("hostname/1", "getdomainname");
	  strcat(name, ".");  /* Hope the buffers are big enuf! */
	  strcat(name, domain);
	}
#endif
    }

  return SP_atom_from_string(name);
error:
  return SP_atom_from_string("[]");
}

void SPCDECL hostname_address(SPAPI_ARG_PROTO_DECL 
			      char *name, SP_term_ref ipnum)
{
  struct hostent *host_entry;
  struct in_addr ia;

  if ((host_entry = gethostbyname(name)) != NULL) /* [PM] 3.8.7 never EINTR */
    {
      memcpy(&ia.s_addr, host_entry->h_addr_list[0], sizeof(ia.s_addr));
      SP_put_string(ipnum, inet_ntoa(ia)); /* [PM] 3.8.7 inet_ntoa cannot fail */
    }
}

void SPCDECL address_hostname(SPAPI_ARG_PROTO_DECL 
			      char *ipnum, SP_term_ref name)
{
  struct in_addr ia;
  struct hostent *host_entry;
  char str[128];

  if ((ia.s_addr = inet_addr(ipnum)) != SP_INADDR_NONE &&
      (host_entry = gethostbyaddr((char *)&ia, sizeof(struct in_addr), AF_INET)))
    {
      try_aliases(host_entry, str);
      SP_put_string(name, str);
    }
}

/*
#if SP_WIN32      
static void sp_winsockerror(pred, syscall)
     char *pred, *syscall;
{
  char syserrbuf[32];

  sprintf(syserrbuf, "winsock_errno=%ld", (long)WSAGetLastError());
  sp_syserror(API_CLIB, syserrbuf, pred, syscall);
}
#endif
*/

#if SP_OS2
static void sp_os2sockerror(char *pred, char *syscall)
{
  char syserrbuf[32];

  sprintf(syserrbuf, "sock_errno=%d", sock_errno());
  sp_syserror(API_CLIB, syserrbuf, pred, syscall);
}
#endif
