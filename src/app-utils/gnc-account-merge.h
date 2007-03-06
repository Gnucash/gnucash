/* Copyright (C) 2006 Joshua Sled <jsled@asynchronous.org> */

#ifndef GNC_ACCOUNT_MERGE_H
#define GNC_ACCOUNT_MERGE_H

#include "Account.h"

typedef enum {
  GNC_ACCOUNT_MERGE_DISPOSITION_USE_EXISTING,
  GNC_ACCOUNT_MERGE_DISPOSITION_CREATE_NEW
} GncAccountMergeDisposition;

typedef struct _merge_error {
  Account *existing_acct;
  Account *new_acct;
  GncAccountMergeDisposition disposition;
} GncAccountMergeError;

GncAccountMergeDisposition determine_account_merge_disposition(Account *existing_acct, Account *new_acct);
GncAccountMergeDisposition determine_merge_disposition(Account *existing_root, Account *new_acct);

void account_trees_merge(Account *existing_root, Account *new_accts_root);

#endif /* GNC_ACCOUNT_MERGE_H */
