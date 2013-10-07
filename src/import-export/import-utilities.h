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
/** @file import-utilities.h
    @brief Utility functions for writing import modules.
    @author Copyright (C) 2002 Benoit Grégoire <bock@step.polymtl.ca>
*/
#ifndef IMPORT_UTILITIES_H
#define IMPORT_UTILITIES_H


/** The preferences used by the importer */
#define GNC_PREFS_GROUP_IMPORT        "dialogs.import.generic"
#define GNC_PREF_ENABLE_SKIP          "enable-skip"
#define GNC_PREF_ENABLE_UPDATE        "enable-update"
#define GNC_PREF_USE_BAYES            "use-bayes"
#define GNC_PREF_ATM_FEE_THRESHOLD    "atm-fee-threshold"
#define GNC_PREF_AUTO_CLEAR_THRESHOLD "auto-clear-threshold"
#define GNC_PREF_AUTO_ADD_THRESHOLD   "auto-add-threshold"
#define GNC_PREF_MATCH_THRESHOLD      "match-threshold"


#include "Account.h"

/** @name Setter-getters
    Setter and getter functions for the online_id kvp_frame for
    Accounts.
	@{
*/
const gchar * gnc_import_get_acc_online_id(Account * account);
void gnc_import_set_acc_online_id(Account * account,
                                  const gchar * string_value);
/** @} */
/** @name Setter-getters
    Setter and getter functions for the online_id kvp_frame for
    Transactions.
	@{
*/
const gchar * gnc_import_get_trans_online_id(Transaction * transaction);
void gnc_import_set_trans_online_id(Transaction * transaction,
                                    const gchar * string_value);
/** @} */

gboolean gnc_import_trans_has_online_id(Transaction * transaction);

/** @name Setter-getters
    Setter and getter functions for the online_id kvp_frame for
    Splits.
	@{
*/
const gchar * gnc_import_get_split_online_id(Split * split);
void gnc_import_set_split_online_id(Split * split,
                                    const gchar * string_value);
/** @} */

gboolean gnc_import_split_has_online_id(Split * split);

#endif
/** @} */



