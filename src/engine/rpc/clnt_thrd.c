/* @(#)clnt_tcp.c	2.2 88/08/01 4.0 RPCSRC */
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
static char sccsid[] = "@(#)clnt_tcp.c 1.37 87/10/05 Copyr 1984 Sun Micro";
#endif

/*
 * clnt_thrd.c, Implements a Threaded, TCP/IP based, client side RPC.
 *
 * Copyright (C) 1984, Sun Microsystems, Inc.
 *
 * TCP based RPC supports 'batched calls'.
 * A sequence of calls may be batched-up in a send buffer.  The rpc call
 * return immediately to the client even though the call was not necessarily
 * sent.  The batching occurs if the results' xdr routine is NULL (0) AND
 * the rpc timeout value is zero (see clnt.h, rpc).
 *
 * Clients should NOT casually batch calls that in fact return results; that is,
 * the server side should be aware that a call is batched and not produce any
 * return message.  Batched calls that produce many result messages can
 * deadlock (netlock) the client and the server....
 *
 * Now go hang yourself.
 */

#include <netdb.h>
#include <errno.h>
#include <stdio.h>
#include <unistd.h>
#include <rpc/rpc.h>
#include <sys/poll.h>
#include <sys/socket.h>
#include <rpc/pmap_clnt.h>

#include "xprt_thrd.h"
#include "clnt_thrd.h"

#define MCALL_MSG_SIZE 24

struct ct_data
  {
    bool_t ct_waitset;		/* wait set by clnt_control? */
    struct timeval ct_wait;
    struct rpc_err ct_error;
    char ct_mcall[MCALL_MSG_SIZE];	/* marshalled callmsg header */
    u_int ct_mpos;		/* length of marshalled header */
    u_long ct_prog;		/* Program number */
    u_long ct_vers;		/* Version Number */
    TXPRT_WAIT *ct_repl;	/* A reply wait object */
    TXPRT *ct_xprt;		/* Transport Object */
  };

static enum clnt_stat clntthrd_call (CLIENT *, u_long, xdrproc_t, caddr_t,
				    xdrproc_t, caddr_t, struct timeval);
static void clntthrd_abort (void);
static void clntthrd_geterr (CLIENT *, struct rpc_err *);
static bool_t clntthrd_freeres (CLIENT *, xdrproc_t, caddr_t);
static bool_t clntthrd_control (CLIENT *, int, char *);
static void clntthrd_destroy (CLIENT *);

static struct clnt_ops thrd_ops =
{
  clntthrd_call,
  clntthrd_abort,
  clntthrd_geterr,
  clntthrd_freeres,
  clntthrd_destroy,
  clntthrd_control
};

/*
 * Create a client handle for a threaded tcp/ip connection.
 * transport is the Threaded Transport we wish to attach this client to.
 * NB: It is the client's responsibility to open and close the transport
 * NB: The rpch->cl_auth is set null authentication.  Caller may wish to set
 * this to something more useful.
 */
CLIENT *
clntthrd_create (TXPRT *transport, u_long prog, u_long vers)
{
  CLIENT *h = NULL;
  struct ct_data *ct = NULL;
  struct rpc_msg call_msg;
  struct timeval now;
  XDR xdr;

  if (transport == NULL)
    return (CLIENT *)NULL;

  h = (CLIENT *) malloc (sizeof (*h));
  if (h == NULL)
    {
      (void) fprintf (stderr, "clntthrd_create: out of memory\n");
      goto fooy;
    }
  ct = (struct ct_data *) malloc (sizeof (*ct));
  if (ct == NULL)
    {
      (void) fprintf (stderr, "clntthrd_create: out of memory\n");
      goto fooy;
    }

  memset (ct, 0, sizeof (*ct));
  memset (h, 0, sizeof (*h));

  /*
   * Set up private data structure
   */
  ct->ct_repl = xprt_thrd_new_wait ();
  if (ct->ct_repl == NULL)
    {
      (void) fprintf (stderr, "clntthrd_create: out of memory\n");
      goto fooy;
    }

  ct->ct_wait.tv_usec = 0;
  ct->ct_waitset = FALSE;
  ct->ct_xprt = transport;

  /*
   * Initialize call message
   */
  gettimeofday (&now, NULL);
  srand48 (now.tv_sec ^ now.tv_usec);
  call_msg.rm_xid = lrand48();
  call_msg.rm_direction = CALL;
  call_msg.rm_call.cb_rpcvers = RPC_MSG_VERSION;
  ct->ct_prog = call_msg.rm_call.cb_prog = prog;
  ct->ct_vers = call_msg.rm_call.cb_vers = vers;

  /*
   * pre-serialize the static part of the call msg and stash it away
   */
  xdrmem_create (&xdr, ct->ct_mcall, MCALL_MSG_SIZE, XDR_ENCODE);
  if (!xdr_callhdr (&xdr, &call_msg))
    {
      goto fooy;
    }
  ct->ct_mpos = XDR_GETPOS (&xdr);
  XDR_DESTROY (&xdr);

  /*
   * Build the CLIENT structure
   */
  h->cl_auth = authnone_create ();
  h->cl_ops = &thrd_ops;
  h->cl_private = (caddr_t) ct;
  return h;

fooy:
  /*
   * Something goofed, free stuff and barf
   */
  if (ct != NULL) {
    if (ct->ct_repl != NULL)
      xprt_destroy_wait (ct->ct_repl);
    free ((caddr_t) ct);
  }
  if (h != NULL) free ((caddr_t) h);
  return ((CLIENT *) NULL);
}

static enum clnt_stat
clntthrd_call (h, proc, xdr_args, args_ptr, xdr_results, results_ptr, timeout)
     CLIENT *h;
     u_long proc;
     xdrproc_t xdr_args;
     caddr_t args_ptr;
     xdrproc_t xdr_results;
     caddr_t results_ptr;
     struct timeval timeout;
{
  struct ct_data *ct = (struct ct_data *) h->cl_private;
  XDR *xdrs;
  struct rpc_msg reply_msg;
  u_long x_id;
  u_int32_t *msg_x_id = (u_int32_t *) (ct->ct_mcall);	/* yuk */
  bool_t shipnow;
  int refreshes = 2;

  if (!ct->ct_waitset)
    {
      ct->ct_wait = timeout;
    }

  shipnow =
    (xdr_results == (xdrproc_t) 0 && timeout.tv_sec == 0
     && timeout.tv_usec == 0) ? FALSE : TRUE;

  /* Obtain and lock XDR object */
  xdrs = TXPRT_GET_XDR (ct->ct_xprt);

call_again:
  xdrs->x_op = XDR_ENCODE;
  ct->ct_error.re_status = RPC_SUCCESS;
  x_id = ntohl (--(*msg_x_id));
  if ((!XDR_PUTBYTES (xdrs, ct->ct_mcall, ct->ct_mpos)) ||
      (!XDR_PUTLONG (xdrs, (long *) &proc)) ||
      (!AUTH_MARSHALL (h->cl_auth, xdrs)) ||
      (!(*xdr_args) (xdrs, args_ptr)))
    {
      if (ct->ct_error.re_status == RPC_SUCCESS)
	ct->ct_error.re_status = RPC_CANTENCODEARGS;
      (void) xdrrec_endofrecord (xdrs, TRUE);
      TXPRT_REL_XDR (ct->ct_xprt);
      return (ct->ct_error.re_status);
    }
  if (!xdrrec_endofrecord (xdrs, shipnow)) {
    TXPRT_REL_XDR (ct->ct_xprt);
    return ct->ct_error.re_status = RPC_CANTSEND;
  }
  if (!shipnow) {
    TXPRT_REL_XDR (ct->ct_xprt);
    return RPC_SUCCESS;
  }
  /*
   * Hack to provide rpc-based message passing
   */
  if (timeout.tv_sec == 0 && timeout.tv_usec == 0)
    {
      TXPRT_REL_XDR (ct->ct_xprt);
      return ct->ct_error.re_status = RPC_TIMEDOUT;
    }

  /* Setup reply-state for Transport callback */
  ct->ct_repl->tw_x_id = x_id;
  reply_msg.acpted_rply.ar_verf = _null_auth;
  reply_msg.acpted_rply.ar_results.where = NULL;
  reply_msg.acpted_rply.ar_results.proc = (xdrproc_t)xdr_void;

  /* Now register the callback and wait for our reply message */
  TXPRT_WAIT_REPLY (ct->ct_xprt, &reply_msg, &xdrs, ct->ct_repl, timeout);

  /*
   * Ok, we're back.  That means that our reply has come in.  We expect
   * that reply_msg and xdrs have been filled in for us, and the XDR
   * Object is locked.  That means we don't have to look for the response
   * on our own, nor lock XDR on our own, so....
   */

  /*
   * process header
   */
  _seterr_reply (&reply_msg, &(ct->ct_error));
  if (ct->ct_error.re_status == RPC_SUCCESS)
    {
      if (!AUTH_VALIDATE (h->cl_auth, &reply_msg.acpted_rply.ar_verf))
	{
	  ct->ct_error.re_status = RPC_AUTHERROR;
	  ct->ct_error.re_why = AUTH_INVALIDRESP;
	}
      else if (!(*xdr_results) (xdrs, results_ptr))
	{
	  if (ct->ct_error.re_status == RPC_SUCCESS)
	    ct->ct_error.re_status = RPC_CANTDECODERES;
	}
      /* free verifier ... */
      if (reply_msg.acpted_rply.ar_verf.oa_base != NULL)
	{
	  xdrs->x_op = XDR_FREE;
	  (void) xdr_opaque_auth (xdrs, &(reply_msg.acpted_rply.ar_verf));
	}
    }				/* end successful completion */
  else
    {
      /* maybe our credentials need to be refreshed ... */
      if (refreshes-- && AUTH_REFRESH (h->cl_auth))
	/* We still have the XDR lock */
	goto call_again;
    }				/* end of unsuccessful completion */
  TXPRT_REL_XDR (ct->ct_xprt);
  return ct->ct_error.re_status;
}

static void
clntthrd_geterr (h, errp)
     CLIENT *h;
     struct rpc_err *errp;
{
  struct ct_data *ct =
  (struct ct_data *) h->cl_private;

  *errp = ct->ct_error;
}

static bool_t
clntthrd_freeres (cl, xdr_res, res_ptr)
     CLIENT *cl;
     xdrproc_t xdr_res;
     caddr_t res_ptr;
{
  bool_t res;
  struct ct_data *ct = (struct ct_data *) cl->cl_private;
  XDR *xdrs = TXPRT_GET_XDR (ct->ct_xprt);
  xdrs->x_op = XDR_FREE;
  res = (*xdr_res) (xdrs, res_ptr);
  TXPRT_REL_XDR (ct->ct_xprt);
  return res;
}

static void
clntthrd_abort ()
{
}

static bool_t
clntthrd_control (CLIENT *cl, int request, char *info)
{
  struct ct_data *ct = (struct ct_data *) cl->cl_private;


  switch (request)
    {
    case CLSET_TIMEOUT:
      ct->ct_wait = *(struct timeval *) info;
      ct->ct_waitset = TRUE;
      break;
    case CLGET_TIMEOUT:
      *(struct timeval *) info = ct->ct_wait;
      break;
    case CLGET_XID:
      /*
       * use the knowledge that xid is the
       * first element in the call structure *.
       * This will get the xid of the PREVIOUS call
       */
      *(u_long *)info = ntohl (*(u_long *)ct->ct_mcall);
      break;
    case CLSET_XID:
      /* This will set the xid of the NEXT call */
      *(u_long *)ct->ct_mcall =  htonl (*(u_long *)info - 1);
      /* decrement by 1 as clntthrd_call() increments once */
    case CLGET_VERS:
      /*
       * This RELIES on the information that, in the call body,
       * the version number field is the fifth field from the
       * begining of the RPC header. MUST be changed if the
       * call_struct is changed
       */
      *(u_long *)info = ntohl (*(u_long *)(ct->ct_mcall +
					   4 * BYTES_PER_XDR_UNIT));
      break;
    case CLSET_VERS:
      *(u_long *)(ct->ct_mcall + 4 * BYTES_PER_XDR_UNIT)
	= htonl (*(u_long *)info);
      break;
    case CLGET_PROG:
      /*
       * This RELIES on the information that, in the call body,
       * the program number field is the  field from the
       * begining of the RPC header. MUST be changed if the
       * call_struct is changed
       */
      *(u_long *)info = ntohl(*(u_long *)(ct->ct_mcall +
					  3 * BYTES_PER_XDR_UNIT));
      break;
    case CLSET_PROG:
      *(u_long *)(ct->ct_mcall + 3 * BYTES_PER_XDR_UNIT)
	= htonl(*(u_long *)info);
      break;

    /* We don't support these, yet */
    case CLSET_FD_CLOSE:
    case CLSET_FD_NCLOSE:
    case CLGET_SERVER_ADDR:
    case CLGET_FD:
      return FALSE;

    /* The following are only possible with TI-RPC */
    case CLGET_RETRY_TIMEOUT:
    case CLSET_RETRY_TIMEOUT:
    case CLGET_SVC_ADDR:
    case CLSET_SVC_ADDR:
    case CLSET_PUSH_TIMOD:
    case CLSET_POP_TIMOD:
    default:
      return FALSE;
    }
  return TRUE;
}


static void
clntthrd_destroy (CLIENT *h)
{
  struct ct_data *ct = (struct ct_data *) h->cl_private;

  xprt_destroy_wait (ct->ct_repl);
  free ((caddr_t) ct);
  free ((caddr_t) h);
}
