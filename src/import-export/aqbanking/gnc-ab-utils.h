/*
 * gnc-ab-utils.h --
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 */

/**
 * @addtogroup Import_Export
 * @{
 * @addtogroup AqBanking
 * @{
 * @file gnc-ab-utils.h
 * @brief AqBanking utility functions
 * @author Copyright (C) 2002 Christian Stimming <stimming@tuhh.de>
 * @author Copyright (C) 2008 Andreas Koehler <andi5.py@gmx.net>
 */

#ifndef GNC_HBCI_UTILS_H
#define GNC_HBCI_UTILS_H

#include <glib.h>
#include <aqbanking/banking.h>

#include "Account.h"

G_BEGIN_DECLS

#define GCONF_SECTION_AQBANKING "dialogs/import/hbci"

/**
 * Initialize the gwenhywfar library by calling GWEN_Init() and setting up
 * gwenhywfar logging.
 */
void gnc_GWEN_Init(void);

/**
 * Finalize the gwenhywfar library.
 */
void gnc_GWEN_Fini(void);

/**
 * If there is a cached AB_BANKING object, return it initialized.  Otherwise,
 * create a new AB_BANKING, let it load its environment from its default
 * configuration and cache it.
 *
 * @return The AB_BANKING object
 */
AB_BANKING *gnc_AB_BANKING_new(void);

/**
 * Delete the AB_BANKING @a api.  If this is also the one that was cached by
 * gnc_AB_BANKING_new(), then all references are deleted, too.
 *
 * @param api AB_BANKING or NULL for the cached AB_BANKING object
 */
void gnc_AB_BANKING_delete(AB_BANKING *api);

/**
 * Finish the AB_BANKING @a api.  If this is also the one that was cached by
 * gnc_AB_BANKING_new(), then finish only if the decremented reference count
 * reaches zero.  After this call, you may only call gnc_AB_BANKING_new() to get
 * the api again in a properly initialized state.
 *
 * @param api AB_BANKING object
 * @return Zero on success
 */
gint gnc_AB_BANKING_fini(AB_BANKING *api);

/**
 * Get the corresponding AqBanking account to the GnuCash account @a gnc_acc.
 * Of course this only works after the GnuCash account has been set up for
 * AqBanking use, i.e. the kvp_frame "hbci/..." has been filled with
 * information.
 *
 * @param api The AB_BANKING to get the AB_ACCOUNT from
 * @param gnc_acc The GnuCash account to query for AB_ACCOUNT reference data
 * @return The AB_ACCOUNT found or NULL otherwise
 */
AB_ACCOUNT *gnc_ab_get_ab_account(const AB_BANKING *api, Account *gnc_acc);

/**
 * Print the value of @a value with two decimal places and @a value's
 * currency appended, or 0.0 otherwise
 *
 * @param value AB_VALUE or NULL
 * @return A newly allocated string
 */
gchar *gnc_AB_VALUE_to_readable_string(const AB_VALUE *value);

/**
 * Retrieve the merged "remote name" fields from a transaction.  The returned
 * string must be g_free'd by the caller.  If there was no "remote name" field,
 * NULL (!) is returned.
 *
 * @param ab_trans AqBanking transaction
 * @return A newly allocated string or NULL otherwise
 */
gchar *gnc_ab_get_remote_name(const AB_TRANSACTION *ab_trans);

/**
 * Retrieve the merged purpose fields from a transaction.  The returned string
 * must be g_free'd by the caller.  If there was no purpose, an empty (but
 * allocated) string is returned.
 *
 * @param ab_trans AqBanking transaction
 * @return A newly allocated string, may be ""
 */
gchar *gnc_ab_get_purpose(const AB_TRANSACTION *ab_trans);

/**
 * Create the appropriate description field for a GnuCash Transaction by the
 * information given in the AB_TRANSACTION @a ab_trans.  The returned string
 * must be g_free'd by the caller.
 *
 * @param ab_trans AqBanking transaction
 * @return A newly allocated string, may be ""
 */
gchar *gnc_ab_description_to_gnc(const AB_TRANSACTION *ab_trans);

/**
 * Create the appropriate memo field for a GnuCash Split by the information
 * given in the AB_TRANSACTION @a ab_trans.  The returned string must be
 * g_free'd by the caller.
 *
 * @param ab_trans AqBanking transaction
 * @return A newly allocated string, may be ""
 */
gchar *gnc_ab_memo_to_gnc(const AB_TRANSACTION *ab_trans);

/**
 * Lookup the most recent ACCOUNT_STATUS available in an ACCOUNTINFO as
 * extracted from an IMEXPORTER_CONTEXT.  This can be used to determine the
 * reported account balance most up-to-date.
 *
 * @param acc_info ACCOUNTINFO
 * @return An AB_ACCOUNT_STATUS internal to @a acc_info, or NULL otherwise
 */
AB_ACCOUNT_STATUS *gnc_ab_get_best_accountstatus(
    AB_IMEXPORTER_ACCOUNTINFO *acc_info);

G_END_DECLS

/** @} */
/** @} */

#endif /* GNC_AB_UTILS_H */
