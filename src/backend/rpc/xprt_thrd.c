/*
 * Threaded TCP Transport scheme for RPC; allow a client and a service
 * to share a single (TCP) stream.  A single reader/writer thread
 * exists to handle to I/O.  Writers obtain a write-lock before
 * writing to the stream, and 'readers' setup a callback with the I/O
 * thread which wakes them up when a particular request (or reply) is
 * received.
 *
 * Created by:	Derek Atkins <warlord@MIT.EDU>
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
#include <pthread.h>
#include "xprt_thrd.h"
#include "clnt_thrd.h"
#include "svc_thrd.h"

#ifdef DEBUG_RPC
#define dfprintf(a) fprintf a
#else
#define dfprintf(a)
#endif

#define   RQCRED_SIZE     400     /* this size is excessive */

/* Internal Service Request */
struct my_svc_req {
  char 			cred[2 * MAX_AUTH_BYTES + RQCRED_SIZE];
  struct svc_req	r;
  struct rpc_msg	m;
  void			(*dispatch) (struct svc_req *r, SVCXPRT *x);
  TXPRT *		xprt;
};

/* Transport Wait Object private storage */
struct tw_priv {
  pthread_cond_t cond;
  struct rpc_msg *repl;
  TXPRT_WAIT *next;
};

/* Dispatch service */
struct xt_svc {
  u_long prog;
  u_long vers;
  void (*dispatch) (struct svc_req *r, SVCXPRT *x);
  struct xt_svc *next;
};

/*
 * A stream object private storage
 */
struct xt_priv {
  pthread_mutex_t xdrlock;
  pthread_mutex_t waitlock;
  pthread_mutex_t readlock;
  bool_t readlock_is_locked;
  pthread_t readlock_by;
  int svccnt;
  pthread_mutex_t svccntlock;
  pthread_attr_t pattr;
  int sock;
  struct rpc_err err;
  XDR xdr;
  SVCXPRT *svc;
  struct xt_svc *callout;
  TXPRT_WAIT *waitlist;

  /* For select loop */
  pthread_t	select_thread;
  bool_t	stop;
  caddr_t	selectarg;
  int 		(*select)(caddr_t arg);
  void		(*close)(caddr_t arg);

};

static int readtcp (caddr_t, char *, int);
static int writetcp (caddr_t, char *, int);
static void closetcp (caddr_t);
static int selecttcp (caddr_t);
static void xprt_thrd_run (TXPRT *xprt);

static SVCXPRT *xprt_get_svcxprt (TXPRT *xprt);

static struct xt_svc *svc_find (struct xt_priv *xtp, u_long prog, u_long vers,
				bool_t *found, u_long *low, u_long *high);

/* threadsafe Waitlist handling */
static TXPRT_WAIT *find_wait (struct xt_priv *xtp, u_long x_id);
static void add_wait (struct xt_priv *xtp, TXPRT_WAIT *wait);
static void rem_wait (struct xt_priv *xtp, TXPRT_WAIT *wait);

/* We need to re-implement some of the low-level RPC functions */
static bool_t read_reply (XDR *xdrs, struct rpc_msg *rmsg);
static bool_t read_call (XDR *xdrs, struct rpc_msg *cmsg);

/* Ops vector for Threaded Transport */
static CLIENT *	xprt_thrd_new_client (TXPRT *xprt, u_long proc, u_long vers);
static XDR *	xprt_thrd_get_xdr (TXPRT *xprt);
static void	xprt_thrd_rel_xdr (TXPRT *xprt);
static void	xprt_thrd_wait_rep (TXPRT *xprt, struct rpc_msg *reply_msg,
				    XDR **xdrs, TXPRT_WAIT *wait,
				    struct timeval timeout);
static bool_t	xprt_thrd_reg_call (TXPRT *xprt, u_long prog, u_long vers,
				    void (*dispatch) (struct svc_req *,
						      SVCXPRT *));
static caddr_t	xprt_thrd_getsock (TXPRT *xprt);
static void	xprt_thrd_destroy (TXPRT *xprt);
static void	xprt_thrd_readlock (TXPRT *xprt);
static bool_t	xprt_thrd_doread (TXPRT *xprt);

static const struct txprt_ops xprtthrd_ops =
{
  xprt_thrd_new_client,
  xprt_thrd_reg_call,
  xprt_thrd_getsock,
  xprt_thrd_destroy,
  xprt_thrd_readlock,
  xprt_thrd_doread,
  xprt_thrd_get_xdr,
  xprt_thrd_rel_xdr,
  xprt_thrd_wait_rep
};

/*
 * Create a baseline threaded transport
 */
TXPRT *
xprt_thrd_create (caddr_t sock,
		  int (*read)(caddr_t, char *, int),
		  int (*write)(caddr_t, char *, int),
		  void (*close)(caddr_t),
		  int (*select)(caddr_t),
		  u_int sendsz, u_int recvsz)
{
  TXPRT *xprt;
  struct xt_priv *xtp;

  if ((read == NULL && write != NULL) ||
      (read != NULL && write == NULL))
    return NULL;

  /* If read/write are supplied, must supply select and close */
  if (read != NULL && 
      (select == NULL || close == NULL))
    return NULL;

  xprt = (TXPRT *) malloc (sizeof (*xprt));
  if (xprt == NULL) {
    fprintf (stderr, ("xprt_thrd_create: out of memory\n"));
    goto done;
  }
  xtp = (struct xt_priv *) malloc (sizeof (*xtp));
  if (xtp == NULL) {
    fprintf (stderr, ("xprt_thrd__create: out of memory\n"));
    free ((char *) xprt);
    xprt = (TXPRT *) NULL;
    goto done;
  }
  memset (xprt, 0, sizeof (*xprt));
  memset (xtp, 0, sizeof (*xtp));

  /* Initialized Private context */
  pthread_mutex_init (&(xtp->xdrlock), NULL);
  pthread_mutex_init (&(xtp->waitlock), NULL);
  pthread_mutex_init (&(xtp->readlock), NULL);
  pthread_mutex_init (&(xtp->svccntlock), NULL);
  pthread_attr_init (&(xtp->pattr));
  pthread_attr_setdetachstate (&(xtp->pattr), PTHREAD_CREATE_DETACHED);

  if (read == NULL) {
    xtp->sock = (int) sock;
    read = readtcp;
    write = writetcp;
    close = closetcp;
    select = selecttcp;
    sock = (caddr_t) xtp;
  }
  xdrrec_create (&(xtp->xdr), sendsz, recvsz, sock, read, write);
  xtp->close = close;
  xtp->select = select;
  xtp->selectarg = sock;

  /* Setup Transport Object */
  xprt->txp_private = (caddr_t) xtp;
  xprt->txp_ops = &xprtthrd_ops;

  /* Start it running */
  xprt_thrd_run (xprt);

 done:
  return xprt;
}

static CLIENT *
xprt_thrd_new_client (TXPRT *xprt, u_long proc, u_long vers)
{
  return clntthrd_create (xprt, proc, vers);
}

static caddr_t
xprt_thrd_getsock (TXPRT *xprt)
{
  struct xt_priv *xtp = (struct xt_priv *)xprt->txp_private;
  return (xtp->selectarg);
}

static XDR *
xprt_thrd_get_xdr (TXPRT *xprt)
{
  struct xt_priv *xtp = (struct xt_priv *)xprt->txp_private;
  pthread_mutex_lock (&(xtp->xdrlock));
  return &(xtp->xdr);
}

static void
xprt_thrd_readlock (TXPRT *xprt)
{
  struct xt_priv *xtp = (struct xt_priv *)xprt->txp_private;
  pthread_mutex_lock (&(xtp->readlock));
  xtp->readlock_is_locked = TRUE;
  xtp->readlock_by = pthread_self ();
}

static void
unlock_readlock (TXPRT *xprt)
{
  struct xt_priv *xtp = (struct xt_priv *)xprt->txp_private;
  xtp->readlock_is_locked = FALSE;
  pthread_mutex_unlock (&(xtp->readlock));
}

static void
xprt_thrd_unlock_xdr (struct xt_priv *xtp)
{
  pthread_mutex_unlock (&(xtp->xdrlock));
}

static void
xprt_thrd_rel_xdr (TXPRT *xprt)
{
  struct xt_priv *xtp = (struct xt_priv *)xprt->txp_private;

  /* Release the read lock if we're decoding */
  if (xtp->xdr.x_op == XDR_DECODE && xtp->readlock_is_locked) {
    unlock_readlock (xprt);
  }

  /* Then release the XDR lock */
  xprt_thrd_unlock_xdr (xtp);
}

static void
xprt_thrd_wait_rep (TXPRT *xprt, struct rpc_msg *reply_msg,
		    XDR **xdrs, TXPRT_WAIT *wait, struct timeval timeout)
{
  struct xt_priv *xtp = (struct xt_priv *)xprt->txp_private;
  struct tw_priv *twp = (struct tw_priv *)wait->tw_priv;

  /* Add wait to waitlist */
  twp->repl = reply_msg;
  add_wait (xtp, wait);

  dfprintf ((stderr, "xprt_thrd_wait_rep: waiting for xid %lx\n", wait->tw_x_id));

  /* wait for reply */
  pthread_cond_wait (&twp->cond, &xtp->xdrlock);
  /* Ok, we're awake, and we should have xdrlock held now */

  /* Remove the wait from the waitlist */
  rem_wait (xtp, wait);

  /* And fill in the rest.. */
  *xdrs = &(xtp->xdr);
  (*xdrs)->x_op = XDR_DECODE;
  return;
}

static bool_t
xprt_thrd_reg_call (TXPRT *xprt, u_long prog, u_long vers,
		    void (*dispatch) (struct svc_req *,
				      SVCXPRT *))
{
  struct xt_priv *xtp = (struct xt_priv *)xprt->txp_private;
  struct xt_svc *new;

  if ((new = svc_find (xtp, prog, vers, NULL, NULL, NULL)) != NULL) {
    /* re-registering???? Can't do that. */
    return FALSE;
  }
  new = (struct xt_svc *) malloc (sizeof (*new));
  if (new == NULL)
    return FALSE;
  new->prog = prog;
  new->vers = vers;
  new->dispatch = dispatch;
  new->next = xtp->callout;
  xtp->callout = new;
  return TRUE;  
}

static void
xprt_thrd_destroy (TXPRT *xprt)
{
  struct xt_priv *xtp = (struct xt_priv *)xprt->txp_private;
  struct xt_svc *t, *s;
  TXPRT_WAIT *wait, *tempwait;

  /* 
   * First, wait for the select thread to exit.  If we're in the
   * select thread (which can happen if the close() callback destroys
   * the transport), then just continue -- we know we'll exit when
   * control is returned.
   */
  xtp->stop = TRUE;
  if (xtp->select)
    pthread_join (xtp->select_thread, NULL);

  /* Clear out registered callouts */
  for (s = xtp->callout; s != NULL; s = t) {
    t = s->next;
    free (s);
  }

  /* Wakeup waiting threads */
  if (xtp->waitlist != NULL) {
    /* Obtain the locks */
    xprt_thrd_readlock (xprt);
    xprt_thrd_get_xdr (xprt);

    while ((wait = xtp->waitlist) != NULL) {
      struct tw_priv *twp = (struct tw_priv *)wait->tw_priv;
      XDR *xdr;
      pthread_cond_signal (&twp->cond);
      xprt_thrd_unlock_xdr (xtp);

      /* We don't have to do anything here; we'll block waiting for readlock */
      xprt_thrd_readlock (xprt);
      xprt_thrd_get_xdr (xprt);
    }
    /* 
     * At this point we should have the locks held and all threads
     * should be dead.  So let's clear the locks and go home.
     */
    xtp->xdr.x_op = XDR_DECODE;
    xprt_thrd_rel_xdr (xprt);
  }

  /* Make sure all service calls have returned */
  while (xtp->svccnt > 0);

  /* Then clear my memory */
  pthread_mutex_destroy (&(xtp->xdrlock));
  pthread_mutex_destroy (&(xtp->waitlock));
  pthread_mutex_destroy (&(xtp->readlock));
  pthread_mutex_destroy (&(xtp->svccntlock));
  pthread_attr_destroy (&(xtp->pattr));

  XDR_DESTROY (&(xtp->xdr));
  if (xtp->svc) svc_destroy(xtp->svc);
  free (xtp);
  free (xprt);
}

static void
xprt_thrd_destroy_wait (TXPRT_WAIT *wait)
{
  struct tw_priv *twp = (struct tw_priv *)wait->tw_priv;

  pthread_cond_destroy (&twp->cond);
  free (twp);
  free (wait);
}

TXPRT_WAIT *
xprt_thrd_new_wait (void)
{
  TXPRT_WAIT *wait;
  struct tw_priv *twp;

  wait = (TXPRT_WAIT *)malloc (sizeof (*wait));
  if (wait != NULL) {
    twp = (struct tw_priv *) malloc (sizeof (*twp));
    if (twp == NULL) {
      free (wait);
      return NULL;
    }
    memset (wait, 0, sizeof (*wait));
    pthread_cond_init (&twp->cond, NULL);
    twp->next = NULL;
    wait->tw_priv = (caddr_t) twp;
    wait->tw_destroy = xprt_thrd_destroy_wait;
  }
  return wait;
}

static SVCXPRT *
xprt_get_svcxprt (TXPRT *xprt)
{
  struct xt_priv *xtp = (struct xt_priv *)xprt->txp_private;
  if (xtp->svc == NULL) {
    xtp->svc = svcthrd_create (xprt);
  }
  return xtp->svc;
}

/*
 * Interface between xdr serializer and tcp connection.
 * Behaves like the system calls, read & write, but keeps some error state
 * around for the rpc level.
 */
static int
readtcp (char *ptr, char *buf, int len)
{
  struct xt_priv *xt = (struct xt_priv *)ptr;
  struct pollfd fd;
  int milliseconds = 30000;	/* XXX */
    /*    (ct->ct_wait.tv_sec * 1000) + (ct->ct_wait.tv_usec / 1000); */

  if (len == 0)
    return 0;

  fd.fd = xt->sock;
  fd.events = POLLIN;
  while (TRUE)
    {
      switch (poll(&fd, 1, milliseconds))
	{
	case 0:
	  xt->err.re_status = RPC_TIMEDOUT;
	  return -1;

	case -1:
	  if (errno == EINTR)
	    continue;
	  xt->err.re_status = RPC_CANTRECV;
	  xt->err.re_errno = errno;
	  return -1;
	}
      break;
    }
  switch (len = read (xt->sock, buf, len))
    {

    case 0:
      /* premature eof */
      xt->err.re_errno = ECONNRESET;
      xt->err.re_status = RPC_CANTRECV;
      len = -1;			/* it's really an error */
      break;

    case -1:
      xt->err.re_errno = errno;
      xt->err.re_status = RPC_CANTRECV;
      break;
    }
  return len;
}

static int
writetcp (caddr_t ptr, char *buf, int len)
{
  int i, cnt;
  struct xt_priv *xt = (struct xt_priv*)ptr;

  for (cnt = len; cnt > 0; cnt -= i, buf += i)
    {
      if ((i = write (xt->sock, buf, cnt)) == -1)
	{
	  xt->err.re_errno = errno;
	  xt->err.re_status = RPC_CANTSEND;
	  return -1;
	}
    }
  return len;
}

static void
closetcp (caddr_t sock)
{
  /* Do we close or not? */
}

static int
selecttcp (caddr_t sockp)
{
  TXPRT *xprt = (TXPRT *)sockp;
  struct xt_priv *xtp = (struct xt_priv *)xprt->txp_private;
  int sock = xtp->sock;
  struct timeval timeout;
  fd_set fds;

  timeout.tv_sec = 10;
  timeout.tv_usec = 0;

  FD_ZERO(&fds);
  FD_SET(sock, &fds);

  return select (sock+1, &fds, 0, 0, &timeout);
}

static struct xt_svc *
svc_find (struct xt_priv *xtp, u_long prog, u_long vers,
	  bool_t *found, u_long *low, u_long *high)
{
  struct xt_svc *s = xtp->callout;
  while (s != NULL) {
    if (s->prog == prog) {
      if (s->vers == vers) {
	goto done;
      }
      if (found) *found = TRUE;
      if (low && high) {
	if (s->vers < *low)
	  *low = s->vers;
	if (s->vers > *high)
	  *high = s->vers;
      }
    } /* s->prog != prog */
    s = s->next;
  }
 done:
  return s;
}


static TXPRT_WAIT *
find_wait (struct xt_priv *xtp, u_long x_id)
{
  TXPRT_WAIT *wait;
  struct tw_priv *twp;

  pthread_mutex_lock (&xtp->waitlock);
  wait = xtp->waitlist;
  while (wait != NULL) {
    if (wait->tw_x_id == x_id)
      break;
    twp = (struct tw_priv *)wait->tw_priv;
    wait = twp->next;
  }
  pthread_mutex_unlock (&xtp->waitlock);
  return wait;
}

static void
add_wait (struct xt_priv *xtp, TXPRT_WAIT *wait)
{
  struct tw_priv *twp = (struct tw_priv *)wait->tw_priv;

  pthread_mutex_lock (&xtp->waitlock);
  twp->next = xtp->waitlist;
  xtp->waitlist = wait;
  pthread_mutex_unlock (&xtp->waitlock);
}

static void
rem_wait (struct xt_priv *xtp, TXPRT_WAIT *wait)
{
  TXPRT_WAIT **oldwait;

  pthread_mutex_lock (&xtp->waitlock);
  oldwait = &(xtp->waitlist);
  while (*oldwait != NULL) {
    struct tw_priv *twp = (struct tw_priv *)(*oldwait)->tw_priv;
    if (*oldwait == wait) {
      *oldwait = twp->next;
      break;
    }
    oldwait = &(twp->next);
  }
  pthread_mutex_unlock (&xtp->waitlock);
  return;
}

bool_t
xprt_thrd_getargs_hook (XDR *x, void *v)
{
  return FALSE;
}

static bool_t 
read_call (XDR *xdrs, struct rpc_msg *cmsg)
{
  if (
      xdr_u_long (xdrs, &(cmsg->rm_call.cb_rpcvers)) &&
      (cmsg->rm_call.cb_rpcvers == RPC_MSG_VERSION) &&
      xdr_u_long (xdrs, &(cmsg->rm_call.cb_prog)) &&
      xdr_u_long (xdrs, &(cmsg->rm_call.cb_vers)) &&
      xdr_u_long (xdrs, &(cmsg->rm_call.cb_proc)) &&
      xdr_opaque_auth (xdrs, &(cmsg->rm_call.cb_cred)))
    return xdr_opaque_auth (xdrs, &(cmsg->rm_call.cb_verf));
  return FALSE;
}

/* declarations from the SunRPC library */
extern bool_t xdr_accepted_reply (XDR*, void*);
extern bool_t xdr_rejected_reply (XDR*, void*);

static const struct xdr_discrim reply_dscrm[3] =
{
  {(int) MSG_ACCEPTED, (xdrproc_t) xdr_accepted_reply},
  {(int) MSG_DENIED, (xdrproc_t) xdr_rejected_reply},
  {__dontcare__, NULL_xdrproc_t}};

static bool_t
read_reply (XDR *xdrs, struct rpc_msg *rmsg)
{
  return xdr_union (xdrs, (enum_t *) & (rmsg->rm_reply.rp_stat),
		    (caddr_t) & (rmsg->rm_reply.ru), reply_dscrm,
		    NULL_xdrproc_t);
}

static void
svc_up (struct xt_priv *xtp)
{
  pthread_mutex_lock (&(xtp->svccntlock));
  xtp->svccnt++;
  pthread_mutex_unlock (&(xtp->svccntlock));
}

static void
svc_down (struct xt_priv *xtp)
{
  pthread_mutex_lock (&(xtp->svccntlock));
  xtp->svccnt--;
  pthread_mutex_unlock (&(xtp->svccntlock));
}

static void *
do_callsvc (void *rqst)
{
  struct my_svc_req *mr = (struct my_svc_req *) rqst;
  struct xt_priv *xtp = (struct xt_priv *)mr->xprt->txp_private;

  /* Let the transport know we exist */
  svc_up (xtp);

  /* Usurp the readlock */
  xtp->readlock_by = pthread_self ();

  dfprintf ((stderr, "do_callsvc: xid=%lx\n", mr->m.rm_xid));

  /* Call the dispatch */
  (*(mr->dispatch))(&(mr->r), mr->r.rq_xprt);

  dfprintf ((stderr, "do_callsvc return: xid=%lx\n", mr->m.rm_xid));

  /* Now figure out if we need to unlock the readlock */
  if (xtp->readlock_is_locked && pthread_equal (xtp->readlock_by,
						pthread_self ()))
    unlock_readlock (mr->xprt);

  /* Clean up and exit */
  free (mr);
  svc_down (xtp);
  return NULL;
}

/* 
 * The Magic reader.  When this function is called we KNOW we have
 * input but we don't know to whom the message is intended.  It may be
 * either a request or a response; if it is a request, we need to call
 * out to the service.  If it is a response, we need to wakeup the
 * calling thread.
 *
 * NB: Assumes readlock is already held.
 * NB: Assumes xdrlock is NOT held.
 * Will release lock(s) as necessary on return
 */
static bool_t
xprt_thrd_doread (TXPRT *xprt)
{
  struct xt_priv *xtp = (struct xt_priv *)xprt->txp_private;
  struct rpc_msg msg;
  XDR *xdr = xprt_thrd_get_xdr (xprt); /* Grab and Lock XDR! */

  xdr->x_op = XDR_DECODE;

  /* First, we need to make sure we're at the beginning of the record */
  if (!xdrrec_skiprecord (xdr)) {
    xprt_thrd_rel_xdr (xprt);
    dfprintf ((stderr, "xprt_thrd_doread: skiprecord failed\n"));
    return FALSE;
  }

  if (xdr_u_long (xdr, &(msg.rm_xid)) &&
      xdr_enum (xdr, (enum_t *) &(msg.rm_direction))) {
    /* Ok, we've got our x_id and direction... */

    dfprintf ((stderr, "xprt_thrd_doread: %s for %lx\n",
	     (msg.rm_direction == CALL ? "call" :
	      (msg.rm_direction == REPLY ? "reply" : "(unk)")),
	     msg.rm_xid));

    if (msg.rm_direction == CALL) {
      struct my_svc_req *mr = (struct my_svc_req *) malloc (sizeof (*mr));
      if (mr == NULL) {
	xprt_thrd_rel_xdr (xprt);
	fprintf (stderr, "xprt_thrd_doread: malloc failed\n");
	return FALSE;
      }
      memset (mr, 0, sizeof (*mr));
      mr->xprt = xprt;
      mr->m.rm_xid = msg.rm_xid;
      mr->m.rm_direction = msg.rm_direction;
      mr->m.rm_call.cb_cred.oa_base = mr->cred;
      mr->m.rm_call.cb_verf.oa_base = &(mr->cred[MAX_AUTH_BYTES]);
      mr->r.rq_clntcred = &(mr->cred[2 * MAX_AUTH_BYTES]);

      if (read_call (xdr, &(mr->m))) {
	struct xt_svc *s;
	enum auth_stat why;
	bool_t found = FALSE;
	u_long low = 0, high = 0;

	mr->r.rq_prog = mr->m.rm_call.cb_prog;
	mr->r.rq_vers = mr->m.rm_call.cb_vers;
	mr->r.rq_proc = mr->m.rm_call.cb_proc;
	mr->r.rq_cred = mr->m.rm_call.cb_cred;
	mr->r.rq_xprt = xprt_get_svcxprt (xprt);

	/* Setup for reply */
	SVC_RECV (mr->r.rq_xprt, &(mr->m));

	if ((why = _authenticate (&(mr->r), &(mr->m))) != AUTH_OK)
	  {
	    xprt_thrd_rel_xdr (xprt);
	    svcerr_auth (mr->r.rq_xprt, why);
	    goto call_done;
	  }
	/* now match message with a registered service */
	s = svc_find (xtp, mr->r.rq_prog, mr->r.rq_vers, &found, &low, &high);
	if (s != NULL) {
	  pthread_t thr;
	  mr->dispatch = s->dispatch;
	  xprt_thrd_unlock_xdr (xtp);
	  dfprintf ((stderr, "xprt_thrd_doread: calling proc for xid %lx\n",
		   msg.rm_xid));
	  pthread_create (&thr, &(xtp->pattr), do_callsvc, mr);
	  mr = NULL;
	  goto call_done;
	} else {
	  /* Or unlock XDR for to respond in error */
	  xprt_thrd_rel_xdr (xprt);
	}
	/*
	 * if we got here, the program or version
	 * is not served ...
	 */
	/* First we clear the locks */

	if (found)
	  svcerr_progvers (mr->r.rq_xprt, low, high);
	else
	  svcerr_noprog (mr->r.rq_xprt);
	/* Fall through to ... */

      call_done:
	/* If we're here, then we had a "successful" call */
	if (mr != NULL)
	  free (mr);

	return TRUE;
      } /* !read_call() */

      if (mr != NULL)
	free (mr);

      dfprintf ((stderr, "xprt_thrd_doread: read_call failed, %lx\n",
	       msg.rm_xid));
    } else if (msg.rm_direction == REPLY) {
      /* Find the requestor thread.. */
      TXPRT_WAIT *wait = find_wait(xtp, msg.rm_xid);
      if (wait != NULL) {
	struct tw_priv *twp = (struct tw_priv *) wait->tw_priv;
	twp->repl->rm_xid = msg.rm_xid;
	twp->repl->rm_direction = msg.rm_direction;
	if (read_reply (xdr, twp->repl)) {
	  pthread_cond_signal (&twp->cond);
	  /* Now unlock the lock to let the other thread pick it up.
	     Note, keep the readlock locked! */
	  xprt_thrd_unlock_xdr (xtp);
	  return TRUE;
	}
	/* If we got here, then the reply didn't read correctly */
	dfprintf ((stderr, "Reply didn't read correctly\n"));
      }
      /* If we got here, then either the reply didn't read or there is
         no matching xid */
      dfprintf ((stderr, "bad reply or no matching xid, %lx\n", msg.rm_xid));
    }
    /*
     * If we got here, either the direction is bogus or the message
     * didn't parse correctly
     */
    dfprintf ((stderr, "bad message or bad direction\n"));
  }
  /* If we got here, then we've got a bad message */
  xprt_thrd_rel_xdr (xprt);
  dfprintf ((stderr, "xprt_thrd_doread: bad message\n"));
  return FALSE;
}

static void *
do_select_loop (void *xprt)
{
  struct xt_priv *xtp = (struct xt_priv *)((TXPRT *)xprt)->txp_private;
  int (*select)(caddr_t) = xtp->select;
  caddr_t arg = xtp->selectarg;

  /* Get the readlock */
  xprt_thrd_readlock (xprt);

  while (1) {
    if (xtp->stop) {
      unlock_readlock (xprt);
      return NULL;
    }

    /* now see if there is anything to read */
    switch ((*select)(arg)) {
    case -1:
      /* error */
      if (errno == EINTR)
	break;
      perror ("xprt_thrd: xprt_do_select_loop: select failed");
      unlock_readlock (xprt);
      return NULL;

    case 0:
      /* timeout */
      continue;

    default:
      /* Go read the data */
      if (!xprt_thrd_doread (xprt)) {
	/* We Died */
	xtp->stop = 1;
	if (xtp->close)
	  xtp->close (xtp->selectarg);
	return NULL;
      }

      /* Then wait until the reading is done */
      xprt_thrd_readlock (xprt);

    } /* select */
  } /* while */
}

static void
xprt_thrd_run (TXPRT *xprt)
{
  struct xt_priv *xtp = (struct xt_priv *)xprt->txp_private;

  xtp->stop = FALSE;

  /* start thread */
  pthread_create (&(xtp->select_thread), NULL, do_select_loop, xprt);
}
