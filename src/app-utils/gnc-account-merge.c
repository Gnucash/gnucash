/* Copyright (C) 2006 Joshua Sled <jsled@asynchronous.org> */
#include "config.h"
#include <glib.h>
#include "gnc-account-merge.h"
#include "Account.h"
#include "Group.h"

GncAccountMergeDisposition
determine_account_merge_disposition(Account *existing_acct, Account *new_acct)
{
  g_assert(new_acct != NULL);

  if (existing_acct == NULL)
    return GNC_ACCOUNT_MERGE_DISPOSITION_CREATE_NEW;

  return GNC_ACCOUNT_MERGE_DISPOSITION_USE_EXISTING;
}

GncAccountMergeDisposition
determine_merge_disposition(AccountGroup *existing_root, Account *new_acct)
{
  Account *existing_acct;
  gchar *full_name;
  
  full_name = xaccAccountGetFullName(new_acct);
  existing_acct = xaccGetAccountFromFullName(existing_root, full_name);
  g_free(full_name);

  return determine_account_merge_disposition(existing_acct, new_acct);
}

void
account_group_merge(AccountGroup *existing_grp, AccountGroup *new_grp)
{
  GList *accounts_copy;
  AccountList *accounts;
  g_return_if_fail(new_grp != NULL);
  g_return_if_fail(existing_grp != NULL);

  /* since we're have a chance of mutating the list (via
   * xaccGroupInsertAccount) while we're iterating over it, iterate over a
   * copy. */
  accounts_copy = g_list_copy(xaccGroupGetAccountList(new_grp));
  for (accounts = accounts_copy; accounts; accounts = accounts->next)
  {
    Account *existing_named, *new_acct;
    const char *name;

    new_acct = (Account*)accounts->data;
    name = xaccAccountGetName(new_acct);
    existing_named = xaccGetAccountFromName(existing_grp, name);
    switch (determine_account_merge_disposition(existing_named, new_acct))
    {
    case GNC_ACCOUNT_MERGE_DISPOSITION_USE_EXISTING:
      /* recurse */
      account_group_merge(xaccAccountGetChildren(existing_named),
                          xaccAccountGetChildren(new_acct));
      break;
    case GNC_ACCOUNT_MERGE_DISPOSITION_CREATE_NEW:
      /* merge this one in. */
      xaccGroupInsertAccount(existing_grp, new_acct);
      break;
    }
  }
  g_list_free(accounts_copy);
}
