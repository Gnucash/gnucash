/*******************************************************************\
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
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
\********************************************************************/
/*
 * gnc-ofx-kvp.c
 *
 *  Created on: 13.03.2011
 *      Author: cs
 */

#include "config.h"
#include "gnc-ofx-kvp.h"

static const char *KEY_ASSOC_INCOME_ACCOUNT = "ofx/associated-income-account";
static void force_account_dirty(Account *acct);


Account *gnc_ofx_kvp_get_assoc_account(const Account* investment_account)
{
    kvp_frame * acc_frame;
    kvp_value * kvp_val;
    Account *result = NULL;

    g_assert(investment_account);

    acc_frame = xaccAccountGetSlots(investment_account);
    kvp_val = kvp_frame_get_slot(acc_frame, KEY_ASSOC_INCOME_ACCOUNT);
    if (kvp_val != NULL)
    {
        result = xaccAccountLookup(kvp_value_get_guid(kvp_val),
                                   gnc_account_get_book(investment_account));
    }
    return result;
}

void gnc_ofx_kvp_set_assoc_account(Account* investment_account,
                                   const Account *income_account)
{
    kvp_frame * acc_frame;
    kvp_value * kvp_val;
    Account *result = NULL;
    const GncGUID * income_acc_guid;

    g_assert(investment_account);
    g_assert(income_account);

    acc_frame = xaccAccountGetSlots(investment_account);
    g_assert(acc_frame); // Must not be NULL, but the QofInstance doc is unclear about this
    income_acc_guid = xaccAccountGetGUID(income_account);
    kvp_val = kvp_value_new_guid(income_acc_guid);
    xaccAccountBeginEdit(investment_account);
    kvp_frame_set_slot_nc(acc_frame, KEY_ASSOC_INCOME_ACCOUNT,
                          kvp_val);
    force_account_dirty(investment_account);
    xaccAccountCommitEdit(investment_account);
}

// copied from gnc-ab-kvp.c
static void
force_account_dirty(Account *acct)
{
    gchar *name = g_strdup(xaccAccountGetName(acct));

    /* This is necessary because modifying the KvpFrames doesn't mark
     * accounts dirty, which means the changes wont be propagated to the
     * backend.
     */
    xaccAccountSetName(acct, name);
    g_free(name);
}
