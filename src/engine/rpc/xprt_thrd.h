/*
 * xprt_thrd.h
 *
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

#ifndef RPC_XPRT_THRD_H
#define RPC_XPRT_THRD_H

#include <rpc/types.h>
#include <rpc/xdr.h>
#include <rpc/clnt.h>
#include <rpc/svc.h>
#include <rpc/rpc_msg.h>

typedef struct TXPRT_WAIT TXPRT_WAIT;

typedef struct TXPRT TXPRT;
struct TXPRT {
  const struct txprt_ops {
    /*
     * PUBLIC interfaces
     */

    CLIENT *	(*txp_new_clnt) (TXPRT *xprt, u_long prog, u_long vers);
    /* build and return a new CLIENT object on this transport */

    bool_t	(*txp_reg_call) (TXPRT *xprt, u_long prog, u_long vers,
				 void (*dispatch) (struct svc_req *,
						   SVCXPRT *));
    /* register a callout to a service/dispatch */

    caddr_t	(*txp_get_sock) (TXPRT *xprt);
    /* Return the socket pointer for a transport */

    void	(*txp_destroy) (TXPRT *xprt);
    /* destroy a threaded transport (does NOT destroy CLIENTs nor does
       it call close() */

    /*
     * Semi-Private interfaces
     * (for use in writing your own select loop)
     */

    void	(*txp_readlock) (TXPRT *xptr);
    /* Obtain the read lock on the underlying transport */

    bool_t	(*txp_doread) (TXPRT *xptr);
    /* When the read-lock is held and data exists on this transport, read it */

    /*
     * PRIVATE interfaces
     */

    XDR *	(*txp_get_xdr) (TXPRT *xprt);
    /* get and lock the XDR object (blocks until lock can be obtained) */
    void	(*txp_rel_xdr) (TXPRT *xprt);
    /* release/unlock the XDR object */

    void	(*txp_wait_rep) (TXPRT *xprt, struct rpc_msg *reply_msg,
				 XDR **xdrs, TXPRT_WAIT *wait,
				 struct timeval timeout);
    /* wait for a reply to the call, return with reply_msg filled
     * and xdrs filled and locked */
  } *txp_ops;
  caddr_t txp_private;		/* Private Stuff */
};

/*
 * operations on a Threaded Transport handle:
 *
 * TXPRT		*xprt
 * u_long		prog, vers
 * void (*)(struct svc_req *, SVCSPRT *)	disp
 */

/*
 * PUBLIC Interfaces
 */

#define TXPRT_NEW_CLIENT(xprt,prog,vers) \
	(*(xprt)->txp_ops->txp_new_clnt)((xprt),(prog),(vers))
#define txprt_new_client(xprt,prog,vers) \
	(*(xprt)->txp_ops->txp_new_clnt)((xprt),(prog),(vers))

#define TXPRT_REG_CALLOUT(xprt,prog,vers,disp) \
	(*(xprt)->txp_ops->txp_reg_call)((xprt),(prog),(vers),(disp))
#define txprt_reg_callout(xprt,prog,vers,disp) \
	(*(xprt)->txp_ops->txp_reg_call)((xprt),(prog),(vers),(disp))

#define TXPRT_DESTROY(xprt) (*(xprt)->txp_ops->txp_destroy)(xprt)
#define txprt_destroy(xprt) (*(xprt)->txp_ops->txp_destroy)(xprt)

#define TXPRT_GETSOCK(xprt) (*(xprt)->txp_ops->txp_get_sock)(xprt)
#define txprt_getsock(xprt) (*(xprt)->txp_ops->txp_get_sock)(xprt)

/*
 * Semi-Private interfaces
 * (for use in writing your own select loop)
 *
 * NOTE: you must lock readlock and then call doread with the lock held;
 * doread will unlock the readlock when it is done.
 */

#define TXPRT_READLOCK(xprt) (*(xprt)->txp_ops->txp_readlock)(xprt)
#define txprt_readlock(xprt) (*(xprt)->txp_ops->txp_readlock)(xprt)

#define TXPRT_DOREAD(xprt) (*(xprt)->txp_ops->txp_doread)(xprt)
#define txprt_doread(xprt) (*(xprt)->txp_ops->txp_doread)(xprt)

/*
 * Transport Creation/Destruction
 */

extern TXPRT *	xprt_thrd_create (caddr_t sock,
				  int (*read)(caddr_t, char *, int),
				  int (*write)(caddr_t, char *, int),
				  void (*close)(caddr_t),
				  int (*select)(caddr_t),
				  u_int sendsz, u_int recvsz);

/*
 * This is an xdrproc_t function that, when passed into a threaded
 * SVCXPRT will obtain the TXPRT * from the service.  It should be
 * used by service routines if they want to obtain the TXPRT * to
 * e.g. build a CLIENT for a callback from a request.  It should be
 * used like this:
 *
 *      SVCXPRT *svc
 *	TXPRT *xprt
 *	bool_t res
 *
 * 	res = svc_getargs(svc, (xdrproc_t) xprt_thrd_getargs_hook,
 *			 (caddr_t) &xprt);
 *
 * If res == TRUE, then xprt is a valid pointer.  If res == FALSE,
 * then svc is not a threaded transport service (and xprt is invalid)
 */
extern bool_t xprt_thrd_getargs_hook (XDR *, void *);


/*
 * PRIVATE Interfaces
 */

struct TXPRT_WAIT {
  u_long tw_x_id;
  void (*tw_destroy)(TXPRT_WAIT *);
  caddr_t tw_priv;
};

/*
 * operations on a Threaded Transport handle:
 *
 * TXPRT		*xprt
 * XDR			**xdrp
 * struct rpc_msg	*rmsg
 * TXPRT_WAIT		*wait (with tw_x_id filled in with the request ID)
 */

/* Returns and locks XDR */
#define TXPRT_GET_XDR(xprt) (*(xprt)->txp_ops->txp_get_xdr)(xprt)
#define txprt_get_xdr(xprt) (*(xprt)->txp_ops->txp_get_xdr)(xprt)

/* Unlocks XDR (and potentially the ReadLock if x_op == XDR_DECODE */
#define TXPRT_REL_XDR(xprt) (*(xprt)->txp_ops->txp_rel_xdr)(xprt)
#define txprt_rel_xdr(xprt) (*(xprt)->txp_ops->txp_rel_xdr)(xprt)

/* Wait for a reply -- blocks until we get a response for wait->tw_x_id */
#define TXPRT_WAIT_REPLY(xprt,rmsg,xdrp,wait,time) \
	(*(xprt)->txp_ops->txp_wait_rep)((xprt),(rmsg),(xdrp),(wait),(time))
#define txprt_wait_reply(xprt,rmsg,xdrp,wait,time) \
	(*(xprt)->txp_ops->txp_wait_rep)((xprt),(rmsg),(xdrp),(wait),(time))


/* create a wait object (for use in the client) */
extern TXPRT_WAIT * xprt_thrd_new_wait (void);

/* Destroy a wait object */
#define XPRT_DESTROY_WAIT(wait) (*(wait)->tw_destroy)(wait)
#define xprt_destroy_wait(wait) (*(wait)->tw_destroy)(wait)

#endif /* RPC_XPRT_THRD_H */
