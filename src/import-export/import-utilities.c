/********************************************************************\
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
/** @addtogroup Import_Export
    @{ */
/** @internal
    @file import-utilities.c
    @brief Utility functions for writing import modules.
    @author Copyright (C) 2002 Benoit Grégoire <bock@step.polymtl.ca>
*/
#include "config.h"


#include <glib.h>

#include <stdlib.h>
#include "import-utilities.h"
#include "qof.h"
#include "Account.h"
#include "Transaction.h"


/********************************************************************\
 * Setter and getter functions for the online_id kvp frame in
 * Account, Transaction and Split
\********************************************************************/

const gchar * gnc_import_get_acc_online_id (Account * account)
{
    gchar *id = NULL;
    qof_instance_get (QOF_INSTANCE (account), "online-id", &id, NULL);
    return id;
}

/* Used in the midst of editing a transaction; make it save the
 * account data. */
void gnc_import_set_acc_online_id (Account *account, const gchar *id)
{
    g_return_if_fail (account != NULL);
    xaccAccountBeginEdit (account);
    qof_instance_set (QOF_INSTANCE (account), "online-id", id, NULL);
    xaccAccountCommitEdit (account);
}

const gchar * gnc_import_get_trans_online_id (Transaction * transaction)
{
    gchar *id = NULL;
    qof_instance_get (QOF_INSTANCE (transaction), "online-id", &id, NULL);
    return id;
}
/* Not actually used */
void gnc_import_set_trans_online_id (Transaction *transaction,
				     const gchar *id)
{
    g_return_if_fail (transaction != NULL);
    xaccTransBeginEdit (transaction);
    qof_instance_set (QOF_INSTANCE (transaction), "online-id", id, NULL);
    xaccTransCommitEdit (transaction);
}

gboolean gnc_import_trans_has_online_id(Transaction * transaction)
{
    const gchar * online_id;
    online_id = gnc_import_get_trans_online_id(transaction);
    return (online_id != NULL && strlen(online_id) > 0);
}

const gchar * gnc_import_get_split_online_id (Split * split)
{
    gchar *id = NULL;
    qof_instance_get (QOF_INSTANCE (split), "online-id", &id, NULL);
    return id;
}
/* Used several places in a transaction edit where many other
 * parameters are also being set, so individual commits wouldn't be
 * appropriate. Besides, there isn't a function for one.*/
void gnc_import_set_split_online_id (Split *split, const gchar *id)
{
    g_return_if_fail (split != NULL);
    qof_instance_set (QOF_INSTANCE (split), "online-id", id, NULL);
}

gboolean gnc_import_split_has_online_id(Split * split)
{
    const gchar * online_id;
    online_id = gnc_import_get_split_online_id(split);
    return (online_id != NULL && strlen(online_id) > 0);
}

/* @} */
