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

#ifndef GNC_AB_UTILS_H
#define GNC_AB_UTILS_H

#include <glib.h>
#include <gtk/gtk.h>
#include <aqbanking/banking.h>

#include "Account.h"

G_BEGIN_DECLS

#if (AQBANKING_VERSION_MAJOR > 3) || \
  ((AQBANKING_VERSION_MAJOR == 3) && \
    (AQBANKING_VERSION_MINOR >= 9))
/** Defined if libaqbanking4 as opposed to libaqbanking3 is being used */
# define AQBANKING_VERSION_4_PLUS
#endif

#define GCONF_SECTION_AQBANKING "dialogs/import/hbci"
#define KEY_FORMAT_SWIFT940 "format_swift_mt940"
#define KEY_FORMAT_SWIFT942 "format_swift_mt942"
#define KEY_FORMAT_DTAUS "format_dtaus"

typedef struct _GncABImExContextImport GncABImExContextImport;

#define AWAIT_BALANCES      1 << 1
#define FOUND_BALANCES      1 << 2
#define IGNORE_BALANCES     1 << 3
#define AWAIT_TRANSACTIONS  1 << 4
#define FOUND_TRANSACTIONS  1 << 5
#define IGNORE_TRANSACTIONS 1 << 6

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
 * Create an unbalanced and dirty GnuCash transaction with a split to @a gnc_acc
 * from the information available in the AqBanking transaction @a ab_trans.
 *
 * @param ab_trans AqBanking transaction
 * @param gnc_acc Account of to use for the split
 * @return A dirty GnuCash transaction or NULL otherwise
 */
Transaction *gnc_ab_trans_to_gnc(const AB_TRANSACTION *ab_trans, Account *gnc_acc);

/**
 * Import balances and transactions found in a AB_IMEXPORTER_CONTEXT into
 * GnuCash.  By using @a awaiting the caller can specify what the user will
 * expect to receive.  By using @a execute_txns, transactions in @a context can
 * be used to generate corresponding AqBanking jobs, e.g. after a file import.
 *
 * @param context AB_IMEXPORTER_CONTEXT to import
 *
 * @param awaiting Information the caller expects to receive or wants to ignore,
 * bitmask of AWAIT_* or IGNORE_* values
 *
 * @param execute_txns If @a awaiting contains AWAIT_TRANSACTIONS, whether to
 * create an aqbanking job for each of the transactions found
 *
 * @param api If @a execute_txns is TRUE, the AB_BANKING to get
 * AB_ACCOUNTs from
 *
 * @param parent Widget to set new dialogs transient for, may be NULL
 *
 * @return A new GncABImExContextImport object which must be freed with
 * g_free(), or NULL otherwise.  If execute_txns is TRUE, additionally
 * gnc_ab_ieci_get_job_list() must be called and the result freed with
 * AB_Job_List2_FreeAll()
 */
GncABImExContextImport *gnc_ab_import_context(
    AB_IMEXPORTER_CONTEXT *context, guint awaiting, gboolean execute_txns,
    AB_BANKING *api, GtkWidget *parent);

/**
 * Extract awaiting from @a data.
 *
 * @param ieci The return value of gnc_ab_import_context()
 * @return The initial awaiting bitmask plus IGNORE_* for unexpected and then
 * ignored items, and FOUND_* for non-empty items
 */
guint gnc_ab_ieci_get_found(GncABImExContextImport *ieci);

/**
 * Extract the job list from @a data.
 *
 * @param ieci The return value of gnc_ab_import_context()
 * @return The list of jobs, freeable with AB_Job_List2_FreeAll()
 */
AB_JOB_LIST2 *gnc_ab_ieci_get_job_list(GncABImExContextImport *ieci);

/**
 * Run the generic transaction matcher dialog.
 *
 * @param ieci The return value of gnc_ab_import_context()
 * @return The return value of gnc_gen_trans_list_run().
 */
gboolean gnc_ab_ieci_run_matcher(GncABImExContextImport *ieci);

G_END_DECLS

/** @} */
/** @} */

#endif /* GNC_AB_UTILS_H */
