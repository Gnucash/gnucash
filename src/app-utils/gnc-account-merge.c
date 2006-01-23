/* Copyright (C) 2006 Joshua Sled <jsled@asynchronous.org> */

#include "gnc-account-merge.h"
#include "Account.h"
#include "Group.h"

GncAccountMergeDisposition
determine_account_merge_disposition(Account *existing_acct, Account *new_acct)
{
  g_assert(new_acct != NULL);

  if (existing_acct == NULL)
    return GNC_ACCOUNT_MERGE_DISPOSITION_CREATE_NEW;

  if (xaccAccountGetPlaceholder(existing_acct) != xaccAccountGetPlaceholder(new_acct))
/*    return GNC_ACCOUNT_MERGE_DISPOSITION_ERROR;*/
    return GNC_ACCOUNT_MERGE_DISPOSITION_USE_EXISTING;

  return GNC_ACCOUNT_MERGE_DISPOSITION_USE_EXISTING;
}

GncAccountMergeDisposition
determine_merge_disposition(AccountGroup *existing_root, Account *new_acct)
{
  const char sep_char = '.';
  Account *existing_acct;
  gchar *full_name;
  
  full_name = xaccAccountGetFullName(new_acct, sep_char);
  existing_acct = xaccGetAccountFromFullName(existing_root, full_name, sep_char);
  g_free(full_name);

  return determine_account_merge_disposition(existing_acct, new_acct);
}

static void
_account_merge_error_detection(AccountGroup *existing_grp, AccountGroup *new_grp, GList **error_accum)
{
  AccountList *accts;
  for (accts = xaccGroupGetAccountList(new_grp); accts; accts = accts->next)
  {
    Account *new_acct, *existing_acct;
    GncAccountMergeDisposition disp;
    
    new_acct = (Account*)accts->data;
    existing_acct = xaccGetAccountFromName(existing_grp, xaccAccountGetName(new_acct));
    disp = determine_account_merge_disposition(existing_acct, new_acct);
    if (disp == GNC_ACCOUNT_MERGE_DISPOSITION_ERROR)
    {
      GncAccountMergeError *err = g_new0(GncAccountMergeError, 1);
      err->existing_acct = existing_acct;
      err->new_acct = new_acct;
      err->disposition = disp;
      *error_accum = g_list_append(*error_accum, err);
    }
    _account_merge_error_detection(xaccAccountGetChildren(existing_acct),
                                   xaccAccountGetChildren(new_acct),
                                   error_accum);
  }
}

GList*
account_merge_error_detection(AccountGroup *existing_grp, AccountGroup *new_grp)
{
  GList *errors = NULL;
  _account_merge_error_detection(existing_grp, new_grp, &errors);
  return errors;
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
    case GNC_ACCOUNT_MERGE_DISPOSITION_ERROR:
      g_assert_not_reached();
      return;
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
