/* 
 * BackendP.h
 *
 * Pseudo-object defining how the engine can interact with different
 * back-ends (which will probably be sql databases).
 * 
 * The callbacks will be called at the appropriate times during 
 * a book session to allow the backend to store the data as needed.
 *
 */

#ifndef __XACC_BACKEND_P_H__
#define __XACC_BACKEND_P_H__

#include "config.h"

#include "Account.h"
#include "Group.h"
#include "Query.h"
#include "Transaction.h"
#include "gnc-book.h"

typedef struct _backend Backend;

/*
 * trans_commit_edit() takes two transaction arguments:
 * the first is the proposed new transaction; the second is the
 * 'original' transaction. The second argument is here for 
 * convencience; it had better be substantially equivalent to
 * the argument for the trans_begin_edit() callback.  (It doesn't
 * have to be identical, it can be a clone).
 */

struct _backend 
{
  AccountGroup * (*book_load) (GNCBook *, const char * book_id);
  int (*book_end) (GNCBook *);
  int (*account_begin_edit) (Backend *, Account *, int defer);
  int (*account_commit_edit) (Backend *, Account *);
  int (*trans_begin_edit) (Backend *, Transaction *);
  int (*trans_commit_edit) (Backend *, Transaction *new, Transaction *orig);
  int (*trans_rollback_edit) (Backend *, Transaction *);

  int (*run_query) (Backend *, Query *);
};

/*
 * The xaccGetAccountBackend() subroutine will find the
 *    persistent-data storage backend associated with this account.
 *    This routine traverses up the account heirarchy until it
 *    finds and account-group node that has a backend associated with
 *    it.  The assumption is that all accounts in that account-group
 *    share a common back-end.
 *
 * The xaccGetTransactionBackend() subroutine does the same, for a given
 *    transaction.
 */

Backend * xaccAccountGetBackend (Account *);
Backend * xaccTransactionGetBackend (Transaction *);

/*
 * The xaccGroupSetBackend() associates a backend to a group
 */
void xaccGroupSetBackend (AccountGroup *, Backend *);
Backend * xaccGroupGetBackend (AccountGroup *);
Backend * xaccGNCBookGetBackend (GNCBook *book);


#endif /* __XACC_BACKEND_P_H__ */
