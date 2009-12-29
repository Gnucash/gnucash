/********************************************************************\
 * gnc-account-merge.c                                              *
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

#include "config.h"
#include <glib.h>
#include "gnc-account-merge.h"
#include "Account.h"

GncAccountMergeDisposition
determine_account_merge_disposition(Account *existing_acct, Account *new_acct)
{
    g_assert(new_acct != NULL);

    if (existing_acct == NULL)
        return GNC_ACCOUNT_MERGE_DISPOSITION_CREATE_NEW;

    return GNC_ACCOUNT_MERGE_DISPOSITION_USE_EXISTING;
}

GncAccountMergeDisposition
determine_merge_disposition(Account *existing_root, Account *new_acct)
{
    Account *existing_acct;
    gchar *full_name;

    full_name = gnc_account_get_full_name(new_acct);
    existing_acct = gnc_account_lookup_by_full_name(existing_root, full_name);
    g_free(full_name);

    return determine_account_merge_disposition(existing_acct, new_acct);
}

void
account_trees_merge(Account *existing_root, Account *new_accts_root)
{
    GList *accounts, *node;
    g_return_if_fail(new_accts_root != NULL);
    g_return_if_fail(existing_root != NULL);

    /* since we're have a chance of mutating the list (via
     * gnc_account_add_child) while we're iterating over it, iterate
     * over a copy. */
    accounts = gnc_account_get_children(new_accts_root);
    for (node = accounts; node; node = g_list_next(node))
    {
        Account *existing_named, *new_acct;
        const char *name;

        new_acct = (Account*)node->data;
        name = xaccAccountGetName(new_acct);
        existing_named = gnc_account_lookup_by_name(existing_root, name);
        switch (determine_account_merge_disposition(existing_named, new_acct))
        {
        case GNC_ACCOUNT_MERGE_DISPOSITION_USE_EXISTING:
            /* recurse */
            account_trees_merge(existing_named, new_acct);
            break;
        case GNC_ACCOUNT_MERGE_DISPOSITION_CREATE_NEW:
            /* merge this one in. */
            gnc_account_append_child(existing_root, new_acct);
            break;
        }
    }
    g_list_free(accounts);
}
