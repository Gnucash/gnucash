/*
 * FILE:
 * RpcSock.c
 *
 * FUNCTION:
 * Implements the RPC Socket connection
 *
 * HISTORY:
 * Created By:	Derek Atkins <warlord@MIT.EDU>
 * Copyright (c) 2001, Derek Atkins
 */

#include <netdb.h>
#include <unistd.h>
#include <rpc/xprt_thrd.h>
#include <sys/socket.h>

#include "RpcBackend.h"
#include "RpcSock.h"

struct _rpcend_sock {
  int		sock;		/* socket */
  TXPRT *	xprt;		/* Transport */
  void *	cb_arg;		/* Callback Argument */
  void		(*close)(void *cb); /* Close callback */
  gboolean	listener;	/* Is this a master listener? */
  struct sockaddr_in	peer;	/* Peer Address */
};


static int
myread (caddr_t sockp, char *buf, int len)
{
  RPCSock *sock = (RPCSock *)sockp;

  if (len == 0)
    return 0;

  switch (len = read (sock->sock, buf, len))
    {
    case 0:
      /* premature eof */
      /* ECONNRESET, RPC_CANTRECV */
      len = -1;                 /* it's really an error */
      break;

    case -1:
      /* RPC_CANTRECV */
      break;
    }
  return len;
}

static int
mywrite (caddr_t sockp, char *buf, int len)
{
  RPCSock *sock = (RPCSock *)sockp;
  int i, cnt;

  for (cnt = len; cnt > 0; cnt -= i, buf += i)
    {
      if ((i = write (sock->sock, buf, cnt)) == -1)
     {
       /* RPC_CANTSEND */
       return -1;
     }
    }
  return len;
}

static int
myselect (caddr_t sockp)
{
  RPCSock *sock = (RPCSock *)sockp;
  struct timeval timeout;
  fd_set fds;

  timeout.tv_sec = 2;
  timeout.tv_usec = 0;

  FD_ZERO(&fds);
  FD_SET(sock->sock, &fds);

  return select (sock->sock+1, &fds, 0, 0, &timeout);
}

static void
myclose (caddr_t sockp)
{
  RPCSock *sock = (RPCSock *)sockp;

  /* Close callback */
  if (sock->close)
    (*sock->close)(sock->cb_arg);

  close (sock->sock);
  g_free (sock);
}

int RpcConnect (char *hostname, unsigned short port, RPCSock **sock)
{
  RPCSock *new;
  int s;
  struct hostent *hp;
  struct sockaddr_in sin;

  if (hostname == NULL || *hostname == '\0' || port == 0)
    return ERR_BACKEND_BAD_URL;

  if (sock == NULL)
    return ERR_BACKEND_MISC;

  if ((hp = gethostbyname (hostname)) == NULL)
    return ERR_RPC_HOST_UNK;

  sin.sin_family = hp->h_addrtype;
  sin.sin_port = port;
  memcpy (&sin.sin_addr, hp->h_addr, MIN(sizeof(sin.sin_addr),hp->h_length));

  if ((s = socket (sin.sin_family, SOCK_STREAM, 0)) < 0)
    return ERR_BACKEND_ALLOC;

  if (connect (s, (struct sockaddr *) &sin, sizeof(sin)) != 0)
    return ERR_BACKEND_CANT_CONNECT;

  new = g_malloc (sizeof (*new));
  if (new == NULL) {
    close (s);
    return ERR_BACKEND_ALLOC;
  }
 
  memset (new, 0, sizeof (*new));
  new->sock = s;
  *sock = new;
  return 0;
}

int RpcClose (RPCSock *sock)
{
  if (sock == NULL)
    return ERR_BACKEND_MISC;

  myclose ((caddr_t) sock);
  return 0;
}

int RpcTransport (RPCSock *sock, void (*myClose)(void *arg), void *arg,
		  TXPRT **xprt)
{
  TXPRT *x;

  if (sock == NULL || xprt == NULL || myClose == NULL || sock->xprt != NULL)
    return ERR_BACKEND_MISC;

  x = xprt_thrd_create ((caddr_t)sock, myread, mywrite, myclose, myselect,
			0, 0);
  if (x == NULL)
    return ERR_BACKEND_ALLOC;

  sock->close = myClose;
  sock->cb_arg = arg;
  sock->xprt = *xprt = x;
  return 0;
}

int RpcCreateListener (unsigned short port, RPCSock **sock)
{
  int s;
  int on = 1;
  struct sockaddr_in sin;
  RPCSock *new;

  if (!sock)
    return ERR_BACKEND_MISC;

  /* Create socket */
  if ((s = socket (AF_INET, SOCK_STREAM, 0)) < 0)
    return ERR_BACKEND_ALLOC;

  /* Set socket options */
  if (setsockopt (s, SOL_SOCKET, SO_REUSEADDR, (char *)&on, sizeof(on))
      < 0) {
    close (s);
    return ERR_BACKEND_MISC;
  }

  /* Bind to the address */
  sin.sin_family = AF_INET;
  sin.sin_port = port;
  sin.sin_addr.s_addr = INADDR_ANY;
  if (bind (s, (struct sockaddr *) &sin, sizeof (sin)) < 0) {
    close (s);
    return ERR_RPC_CANT_BIND;
  }

  /* Build RPCSock */
  new = g_malloc (sizeof (*new));
  if (new == NULL) {
    close (s);
    return ERR_BACKEND_ALLOC;
  }
 
  memset (new, 0, sizeof (*new));
  new->sock = s;
  new->listener = TRUE;
  *sock = new;
  return 0;
}

int RpcListen (RPCSock *sock, int val)
{
  if (!sock)
    return -1;

  if (!sock->listener)
    return -2;

  return (listen (sock->sock, val));
}

int RpcAccept (RPCSock *master, RPCSock **client)
{
  int s;
  socklen_t len;
  RPCSock *new;

  if (!master || !client)
    return ERR_BACKEND_MISC;

  if (!master->listener)
    return ERR_BACKEND_MISC;

  new = g_malloc (sizeof (*new));
  if (!new)
    return ERR_BACKEND_ALLOC;

  memset (new, 0, sizeof (*new));
  len = sizeof (new->peer);
  if ((new->sock = accept (master->sock, &(new->peer), &len)) < 0) {
    g_free (new);
    return ERR_RPC_CANT_ACCEPT;
  }

  *client = new;
  return 0;
}

void * RpcGetData (RPCSock *sock)
{
  if (!sock)
    return NULL;

  return (sock->cb_arg);
}
