/* 
 * FILE:
 * BackendP.h
 *
 * FUNCTION:
 * Pseudo-object defining how the engine can interact with different
 * back-ends (which may be SQL databases, or network interfaces to 
 * remote GnuCash servers.  In theory, file-io should be a type of 
 * backend).
 * 
 * The callbacks will be called at the appropriate times during 
 * a book session to allow the backend to store the data as needed.
 *
 */

#ifndef __XACC_BACKEND_P_H__
#define __XACC_BACKEND_P_H__

#include "config.h"

#include "Account.h"
#include "Backend.h"
#include "Group.h"
#include "Query.h"
#include "Transaction.h"
#include "gnc-book.h"

typedef struct _backend Backend;

/*
 * The book_begin() routine gives the backend a second initialization
 *    opportunity.  It is suggested that the backend check that 
 *    the URL is syntactically correct, and that it is actually
 *    reachable.  This is probably(?) a good time to initialize
 *    the actual network connection.
 *
 *    The 'ignore_lock' argument indicates whether the single-user
 *    lock on the backend should be cleared.  The typical GUI sequence
 *    leading to this is: (1) GUI attempts to open the backend
 *    by calling this routine with FALSE==ignore_lock.  (2) If backend  
 *    error'ed BACKEND_LOCK, then GUI asks user what to do. (3) if user 
 *    answers 'break & enter' then this routine is called again with
 *    TRUE==ignore_lock.
 *
 *    The 'create_if_nonexistent' argument indicates whether this
 *    routine should create a new 'database', if it doesn't already
 *    exist. For example, for a file-backend, this would create the
 *    file, if it didn't already exist.  For an SQL backend, this
 *    would create the database (the schema) if it didn't already 
 *    exist.  This flag is used to implement the 'SaveAs' GUI, where
 *    the user requests to save data to a new backend.
 *
 * The book_load() routine should return at least an account tree,
 *    and all currencies.  It does not have to return any transactions
 *    whatsoever, as these are obtained at a later stage when a user
 *    opens a register, resulting in a query being sent to the backend.
 *
 *    (Its OK to send over transactions at this point, but one should 
 *    be careful of the network load; also, its possible that whatever 
 *    is sent is not what the user wanted anyway, which is why its 
 *    better to wait for the query).
 *
 * The trans_commit_edit() routine takes two transaction arguments:
 *    the first is the proposed new transaction; the second is the
 *    'original' transaction. The second argument is here for 
 *    convenience; it had better be substantially equivalent to
 *    the argument for the trans_begin_edit() callback.  (It doesn't
 *    have to be identical, it can be a clone).
 *
 * The run_query() callback takes a GnuCash query object. 
 *    For an SQL backend,  the contents of the query object need to 
 *    be turned into a corresponding SQL query statement, and sent 
 *    to the database for evaluation. The database will return a 
 *    set of splits and transactions, and this callback needs
 *    to poke these into the account-group hierarchy held by the 
 *    query object. 
 *
 *    For a network-communications backend, essentially the same is 
 *    done, except that this routine would convert the query to wire 
 *    protocol, get an answer from the remote server, and push that
 *    into the account-group object.
 *
 *    Note a peculiar design decision we've used here. The query
 *    callback has returned a list of splits; these could be returned
 *    directly to the caller. They are not.  By poking them into the
 *    existing account hierarchy, we are essentially building a local
 *    cache of the split data.  This will allow the GnuCash client to 
 *    continue functioning even when disconnected from the server:
 *    this is because it will have its local cache of data to work from.
 *
 * The sync() routine synchronizes the engine contents to the backend.
 *    This is done by using version numbers (hack alert -- the engine
 *    does not currently contain version numbers).
 *    If the engine contents are newer than what's in the backend, the 
 *    data is stored to the backend.  If the engine contents are older,
 *    then the engine contents are updated.  
 *
 *    Note that this sync operation is only meant to apply to the 
 *    current contents of the engine.  This routine is not intended
 *    to be used to fetch account/transaction data from the backend.
 *    (It might pull new splits from the backend, if this is what is
 *    needed to update an existing transaction.  It might pull new 
 *    currencies (??))
 *
 * The events_pending() routines should return true if there are
 *    external events which need to be processed to bring the
 *    engine up to date with the backend.
 *
 * The process_events() routine should process any events indicated
 *    by the events_pending() routine. It should return TRUE if
 *    the engine was changed while engine events were suspended.
 *
 * The last_err member indicates the last error that occurred.
 *    It should probably be implemented as an array (actually,
 *    a stack) of all the errors that have occurred.
 */

struct _backend 
{
  void (*book_begin) (GNCBook *, const char *book_id, 
                      gboolean ignore_lock, gboolean create_if_nonexistent);
  AccountGroup * (*book_load) (Backend *);
  void (*book_end) (Backend *);
  int (*account_begin_edit) (Backend *, Account *);
  int (*account_commit_edit) (Backend *, Account *);
  int (*trans_begin_edit) (Backend *, Transaction *);
  int (*trans_commit_edit) (Backend *, Transaction *new, Transaction *orig);
  int (*trans_rollback_edit) (Backend *, Transaction *);

  void (*run_query) (Backend *, Query *);
  void (*sync) (Backend *, AccountGroup *);

  gboolean (*events_pending) (Backend *be);
  gboolean (*process_events) (Backend *be);

  GNCBackendError last_err;
};

/*
 * The xaccBackendSetError() routine pushes an error code onto the error
 *   stack. (FIXME: the stack is 1 deep in current implementation).
 *
 * The xaccBackendGetError() routine pops an error code off the error
 *   stack.
 */

void xaccBackendSetError (Backend *be, GNCBackendError err);
GNCBackendError xaccBackendGetError (Backend *be);

/*
 * The xaccGetAccountBackend() subroutine will find the
 *    persistent-data storage backend associated with this account.
 *    This routine traverses up the account hierarchy until it
 *    finds and account-group node that has a backend associated with
 *    it.  The assumption is that all accounts in that account-group
 *    share a common back-end.
 *
 * The xaccGetTransactionBackend() subroutine does the same, for a given
 *    transaction.
 */

Backend * xaccAccountGetBackend (Account *account);
Backend * xaccTransactionGetBackend (Transaction *trans);

/*
 * The xaccGroupSetBackend() associates a backend to a group
 */
void xaccGroupSetBackend (AccountGroup *group, Backend *be);
Backend * xaccGroupGetBackend (AccountGroup *group);
Backend * xaccGNCBookGetBackend (GNCBook *book);


#endif /* __XACC_BACKEND_P_H__ */
