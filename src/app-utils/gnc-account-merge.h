/* Copyright (C) 2006 Joshua Sled <jsled@asynchronous.org> */

#ifndef GNC_ACCOUNT_MERGE_H
#define GNC_ACCOUNT_MERGE_H

#include "Account.h"
#include "Group.h"

typedef enum {
  GNC_ACCOUNT_MERGE_DISPOSITION_ERROR,
  GNC_ACCOUNT_MERGE_DISPOSITION_USE_EXISTING,
  GNC_ACCOUNT_MERGE_DISPOSITION_CREATE_NEW
} GncAccountMergeDisposition;

typedef struct _merge_error {
  Account *existing_acct;
  Account *new_acct;
  GncAccountMergeDisposition disposition;
} GncAccountMergeError;

GncAccountMergeDisposition determine_account_merge_disposition(Account *existing_acct, Account *new_acct);
GncAccountMergeDisposition determine_merge_disposition(AccountGroup *existing_root, Account *new_acct);

/** @return GList<GncAccountMergeError> **/
GList* account_merge_error_detection(AccountGroup *existing_grp, AccountGroup *new_grp);

void account_group_merge(AccountGroup *existing_grp, AccountGroup *new_grp);

#endif /* GNC_ACCOUNT_MERGE_H */
