/* @(#)svc_tcp.c	2.2 88/08/01 4.0 RPCSRC */
/*
 * Sun RPC is a product of Sun Microsystems, Inc. and is provided for
 * unrestricted use provided that this legend is included on all tape
 * media and as a part of the software program in whole or part.  Users
 * may copy or modify Sun RPC without charge, but are not authorized
 * to license or distribute it to anyone else except as part of a product or
 * program developed by the user.
 *
 * SUN RPC IS PROVIDED AS IS WITH NO WARRANTIES OF ANY KIND INCLUDING THE
 * WARRANTIES OF DESIGN, MERCHANTIBILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE, OR ARISING FROM A COURSE OF DEALING, USAGE OR TRADE PRACTICE.
 *
 * Sun RPC is provided with no support and without any obligation on the
 * part of Sun Microsystems, Inc. to assist in its use, correction,
 * modification or enhancement.
 *
 * SUN MICROSYSTEMS, INC. SHALL HAVE NO LIABILITY WITH RESPECT TO THE
 * INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS BY SUN RPC
 * OR ANY PART THEREOF.
 *
 * In no event will Sun Microsystems, Inc. be liable for any lost revenue
 * or profits or other special, indirect and consequential damages, even if
 * Sun has been advised of the possibility of such damages.
 *
 * Sun Microsystems, Inc.
 * 2550 Garcia Avenue
 * Mountain View, California  94043
 */
#if !defined(lint) && defined(SCCSIDS)
static char sccsid[] = "@(#)svc_tcp.c 1.21 87/08/11 Copyr 1984 Sun Micro";
#endif

/*
 * svc_thrd.c, Server side for Threaded TCP/IP based RPC.
 *
 * Copyright (C) 1984, Sun Microsystems, Inc.
 *
 */

#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <rpc/rpc.h>
#include <sys/socket.h>
#include <sys/poll.h>
#include <errno.h>
#include <stdlib.h>

#include "xprt_thrd.h"
#include "svc_thrd.h"

#ifdef DEBUG_RPC
#define dfprintf(a) fprintf a
#else
#define dfprintf(a)
#endif

#ifdef USE_IN_LIBIO
# include <libio/iolibio.h>
# define fputs(s, f) _IO_fputs (s, f)
#endif

/*
 * Ops vector for TCP/IP based rpc service handle
 */
static bool_t svcthrd_recv (SVCXPRT *, struct rpc_msg *);
static enum xprt_stat svcthrd_stat (SVCXPRT *);
static bool_t svcthrd_getargs (SVCXPRT *, xdrproc_t, caddr_t);
static bool_t svcthrd_reply (SVCXPRT *, struct rpc_msg *);
static bool_t svcthrd_freeargs (SVCXPRT *, xdrproc_t, caddr_t);
static void svcthrd_destroy (SVCXPRT *);

static const struct xp_ops svcthrd_op =
{
  svcthrd_recv,
  svcthrd_stat,
  svcthrd_getargs,
  svcthrd_reply,
  svcthrd_freeargs,
  svcthrd_destroy
};

struct tcp_conn
  {				/* kept in xprt->xp_p1 */
    u_long x_id;
    TXPRT *xprt;
    char verf_body[MAX_AUTH_BYTES];
  };

/*
 * Usage:
 *      xprt = svcthrd_create(transport);
 *
 * Creates and returns a (rpc) tcp based transporter.
 * This routine returns a NULL if a problem occurred.
 *
 * NOTE: This assumes that a lock is held on the XDR Object whenever
 * the methods herein are called.  A word to the wise....  (The tranport
 * object will hold this lock when it calls the dispatch routine for a call)
 */
SVCXPRT *
svcthrd_create (TXPRT *transport)
{
  SVCXPRT *xprt;
  struct tcp_conn *cd;

  xprt = (SVCXPRT *) malloc (sizeof (SVCXPRT));
  if (xprt == (SVCXPRT *) NULL)
    {
      (void) fputs ("svcthrd_create: out of memory\n", stderr);
      goto done;
    }
  cd = (struct tcp_conn *) malloc (sizeof (struct tcp_conn));
  if (cd == (struct tcp_conn *) NULL)
    {
      (void) fputs ("svcthrd_create: out of memory\n", stderr);
      free ((char *) xprt);
      xprt = (SVCXPRT *) NULL;
      goto done;
    }
  memset (xprt, 0, sizeof (*xprt));
  memset (cd, 0, sizeof (*cd));

  cd->xprt = transport;
  xprt->xp_p2 = NULL;
  xprt->xp_p1 = (caddr_t) cd;
  xprt->xp_verf.oa_base = cd->verf_body;
  xprt->xp_addrlen = 0;
  xprt->xp_ops = &svcthrd_op;	/* truly deals with calls */
  xprt->xp_port = 0;		/* this is a connection, not a rendezvouser */
  xprt->xp_sock = -1;
done:
  return xprt;
}

static enum xprt_stat
svcthrd_stat (SVCXPRT *xprt)
{
  return XPRT_IDLE;
}

static bool_t
svcthrd_recv (SVCXPRT *xprt, struct rpc_msg *msg)
{
  struct tcp_conn *cd = (struct tcp_conn *) (xprt->xp_p1);

  dfprintf ((stderr, "svcthrd_recv: set xid: %lx\n", msg->rm_xid));
  cd->x_id = msg->rm_xid;
  return TRUE;
}

/*
 * NB: This method assumes that readlock is held, and it will release
 * the locks on exit.
 *
 * This implies that the "caller" must already have the lock, and
 * must only call this function once.  NOTE that calling with
 * xprt_thrd_getargs_hook will NOT touch the XDR object.
 */
static bool_t
svcthrd_getargs (SVCXPRT *xprt, xdrproc_t xdr_args, caddr_t args_ptr)
{
  struct tcp_conn *cd = (struct tcp_conn *) (xprt->xp_p1);
  XDR *xdr;
  bool_t res;

  if (xdr_args == (xdrproc_t) xprt_thrd_getargs_hook) {
    if (args_ptr != NULL)
      *((TXPRT **) args_ptr) = cd->xprt;
    return TRUE;
  }

  xdr = TXPRT_GET_XDR (cd->xprt);
  xdr->x_op = XDR_DECODE;
  res = ((*xdr_args) (xdr, args_ptr));
  TXPRT_REL_XDR (cd->xprt);
  return res;
}

static bool_t
svcthrd_freeargs (SVCXPRT *xprt, xdrproc_t xdr_args, caddr_t args_ptr)
{
  struct tcp_conn *cd = (struct tcp_conn *) (xprt->xp_p1);
  XDR *xdrs = TXPRT_GET_XDR (cd->xprt);
  bool_t res;

  xdrs->x_op = XDR_FREE;
  res = ((*xdr_args) (xdrs, args_ptr));
  TXPRT_REL_XDR (cd->xprt);
  return res;
}

static bool_t
svcthrd_reply (SVCXPRT *xprt, struct rpc_msg *msg)
{
  struct tcp_conn *cd = (struct tcp_conn *) (xprt->xp_p1);
  XDR *xdrs = TXPRT_GET_XDR (cd->xprt);
  bool_t stat;

  dfprintf ((stderr, "svcthrd_reply: xid=%lx (msg says %lx)\n", cd->x_id,
	   msg->rm_xid));

  xdrs->x_op = XDR_ENCODE;
  msg->rm_xid = cd->x_id;
  stat = xdr_replymsg (xdrs, msg);
  (void) xdrrec_endofrecord (xdrs, TRUE);
  TXPRT_REL_XDR (cd->xprt);
  return stat;
}

static void
svcthrd_destroy (SVCXPRT *xprt)
{
  struct tcp_conn *cd = (struct tcp_conn *) xprt->xp_p1;

  free ((caddr_t) cd);
  free ((caddr_t) xprt);
}
