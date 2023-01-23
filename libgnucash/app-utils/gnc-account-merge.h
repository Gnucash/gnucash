/********************************************************************\
 * gnc-account-merge.h                                              *
 * Copyright (C) 2006 Joshua Sled <jsled@asynchronous.org>          *
 *                                                                  *
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
 *                                                                  *
 * This program is distributed in the hope that it will be useful,  *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of   *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    *
 * GNU General Public License for more details.                     *
 *                                                                  *
 * You should have received a copy of the GNU General Public License*
 * along with this program; if not, write to the Free Software      *
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.        *
\********************************************************************/


#ifndef GNC_ACCOUNT_MERGE_H
#define GNC_ACCOUNT_MERGE_H

#include "Account.h"

G_BEGIN_DECLS

typedef enum
{
    GNC_ACCOUNT_MERGE_DISPOSITION_USE_EXISTING,
    GNC_ACCOUNT_MERGE_DISPOSITION_CREATE_NEW
} GncAccountMergeDisposition;

typedef struct _merge_error
{
    Account *existing_acct;
    Account *new_acct;
    GncAccountMergeDisposition disposition;
} GncAccountMergeError;

GncAccountMergeDisposition determine_account_merge_disposition(Account *existing_acct, Account *new_acct);
GncAccountMergeDisposition determine_merge_disposition(Account *existing_root, Account *new_acct);

void account_trees_merge(Account *existing_root, Account *new_accts_root);

G_END_DECLS

#endif /* GNC_ACCOUNT_MERGE_H */
