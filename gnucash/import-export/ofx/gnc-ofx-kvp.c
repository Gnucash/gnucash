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

#include <config.h>
#include "gnc-ofx-kvp.h"

static const char *KEY_ASSOC_INCOME_ACCOUNT = "ofx/associated-income-account";


Account *gnc_ofx_kvp_get_assoc_account(const Account* investment_account)
{
    GncGUID *income_guid= NULL;
    g_assert(investment_account);
    qof_instance_get (QOF_INSTANCE (investment_account),
		      KEY_ASSOC_INCOME_ACCOUNT, &income_guid,
		      NULL);
    return xaccAccountLookup(income_guid,
			       gnc_account_get_book(investment_account));
}

void gnc_ofx_kvp_set_assoc_account(Account* investment_account,
                                   const Account *income_account)
{
    const GncGUID * income_acc_guid;

    g_assert(investment_account);
    g_assert(income_account);

    income_acc_guid = xaccAccountGetGUID(income_account);
    xaccAccountBeginEdit(investment_account);
    qof_instance_set (QOF_INSTANCE (investment_account),
		      KEY_ASSOC_INCOME_ACCOUNT, income_acc_guid,
		      NULL);
    xaccAccountCommitEdit(investment_account);
}
