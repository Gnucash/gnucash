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
#include <gwenhywfar/version.h>

#include "Account.h"

G_BEGIN_DECLS

/** A define that combines the aqbanking version number into one single
 * integer number. Assumption: Both MINOR and PATCHLEVEL numbers are
 * in the interval [0..99]. */
#define AQBANKING_VERSION_INT (10000 * AQBANKING_VERSION_MAJOR + 100 * AQBANKING_VERSION_MINOR + AQBANKING_VERSION_PATCHLEVEL)

/** A define that combines the gwenhywfar version number into one single
 * integer number. Assumption: Both MINOR and PATCHLEVEL numbers are
 * in the interval [0..99]. */
#define GWENHYWFAR_VERSION_INT (10000 * GWENHYWFAR_VERSION_MAJOR + 100 * GWENHYWFAR_VERSION_MINOR + GWENHYWFAR_VERSION_PATCHLEVEL)

# define GNC_AB_ACCOUNT_SPEC AB_ACCOUNT_SPEC
# define GNC_AB_ACCOUNT_SPEC_LIST AB_ACCOUNT_SPEC_LIST
# define GNC_AB_JOB AB_TRANSACTION
# define GNC_AB_JOB_LIST2 AB_TRANSACTION_LIST2
# define GNC_AB_JOB_LIST2_ITERATOR AB_TRANSACTION_LIST2_ITERATOR
# define GNC_AB_JOB_STATUS AB_TRANSACTION_STATUS
# define GNC_GWEN_DATE GWEN_DATE

#define GNC_PREFS_GROUP_AQBANKING       "dialogs.import.hbci"
#define GNC_PREF_FORMAT_SWIFT940        "format-swift-mt940"
#define GNC_PREF_FORMAT_SWIFT942        "format-swift-mt942"
#define GNC_PREF_FORMAT_DTAUS           "format-dtaus"
#define GNC_PREF_IMPORT_NOTED_TXNS      "import-noted-transactions"
#define GNC_PREF_USE_TRANSACTION_TXT    "use-ns-transaction-text"
#define GNC_PREF_VERBOSE_DEBUG          "verbose-debug"

typedef struct
{
    char* name;
    char* descr;
} AB_Node_Pair;

typedef struct _GncABImExContextImport GncABImExContextImport;

/** Called on the GTK thread when the exclusive AqBanking operation slot is
 * granted. The token must be released after all AqBanking work and its GTK/QOF
 * continuations, including gnc_AB_BANKING_fini(), have completed. */
typedef void (*GncABOperationAcquired) (guint token, gpointer user_data);

/** Queue an exclusive AqBanking operation. Calls must originate on the GTK
 * thread. The callback is always dispatched asynchronously on that thread,
 * once, in FIFO order. The caller must keep its request/session lease alive
 * until the callback has completed and release has been called. */
void gnc_ab_operation_acquire_async (GncABOperationAcquired acquired,
                                     gpointer user_data);

/** Release the active operation token on the GTK thread. Unknown or already
 * released tokens are ignored. The next queued callback is dispatched by a
 * later main-context iteration. */
void gnc_ab_operation_release (guint token);

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
void gnc_GWEN_Init (void);

/**
 * Finalize the gwenhywfar library.
 */
void gnc_GWEN_Fini (void);

/**
 * If there is a cached AB_BANKING object, return it initialized.  Otherwise,
 * create a new AB_BANKING, let it load its environment from its default
 * configuration and cache it.
 *
 * @return The AB_BANKING object
 */
AB_BANKING *gnc_AB_BANKING_new (void);

/**
 * Delete the AB_BANKING @a api.  If this is also the one that was cached by
 * gnc_AB_BANKING_new(), then all references are deleted, too.
 *
 * @param api AB_BANKING or NULL for the cached AB_BANKING object
 */
void gnc_AB_BANKING_delete (AB_BANKING *api);

/**
 * Finish the AB_BANKING @a api.  If this is also the one that was cached by
 * gnc_AB_BANKING_new(), then finish only if the decremented reference count
 * reaches zero.  After this call, you may only call gnc_AB_BANKING_new() to get
 * the api again in a properly initialized state.
 *
 * @param api AB_BANKING object
 * @return Zero on success
 */
gint gnc_AB_BANKING_fini (AB_BANKING *api);

/**
 * Get the corresponding AqBanking account to the GnuCash account @a gnc_acc.
 * Of course this only works after the GnuCash account has been set up for
 * AqBanking use, i.e. the account's hbci data have been set up and populated.
 *
 * @param api The AB_BANKING to get the GNC_AB_ACCOUNT_SPEC from
 * @param gnc_acc The GnuCash account to query for GNC_AB_ACCOUNT_SPEC reference data
 * @return The GNC_AB_ACCOUNT_SPEC found or NULL otherwise
 */
GNC_AB_ACCOUNT_SPEC *gnc_ab_get_ab_account (const AB_BANKING *api, Account *gnc_acc);

/**
 * Print the value of @a value with two decimal places and @a value's
 * currency appended, or 0.0 otherwise
 *
 * @param value AB_VALUE or NULL
 * @return A newly allocated string
 */
gchar *gnc_AB_VALUE_to_readable_string (const AB_VALUE *value);

/**
 * Return the job as string.
 *
 * @param value GNC_AB_JOB or NULL
 * @return A newly allocated string
 */
gchar *gnc_AB_JOB_to_readable_string (const GNC_AB_JOB *job);

/**
 * Return the job_id as string.
 *
 * @param job_id
 * @return A newly allocated string
 */
gchar *gnc_AB_JOB_ID_to_string (gulong job_id);

/**
 * Retrieve the merged "remote name" fields from a transaction.  The returned
 * string must be g_free'd by the caller.  If there was no "remote name" field,
 * NULL (!) is returned.
 *
 * @param ab_trans AqBanking transaction
 * @return A newly allocated string or NULL otherwise
 */
gchar *gnc_ab_get_remote_name (const AB_TRANSACTION *ab_trans);

/**
 * Retrieve the merged purpose fields from a transaction.  The returned string
 * must be g_free'd by the caller.  If there was no purpose, an empty (but
 * allocated) string is returned.
 *
 * @param ab_trans AqBanking transaction
 * @return A newly allocated string, may be ""
 */
gchar *gnc_ab_get_purpose (const AB_TRANSACTION *ab_trans, gboolean is_ofx);

/**
 * Create the appropriate description field for a GnuCash Transaction by the
 * information given in the AB_TRANSACTION @a ab_trans.  The returned string
 * must be g_free'd by the caller.
 *
 * @param ab_trans AqBanking transaction
 * @return A newly allocated string, may be ""
 */
gchar *gnc_ab_description_to_gnc (const AB_TRANSACTION *ab_trans, gboolean is_ofx);

/**
 * Create the appropriate memo field for a GnuCash Split by the information
 * given in the AB_TRANSACTION @a ab_trans.  The returned string must be
 * g_free'd by the caller.
 *
 * @param ab_trans AqBanking transaction
 * @return A newly allocated string, may be ""
 */
gchar *gnc_ab_memo_to_gnc (const AB_TRANSACTION *ab_trans);

/**
 * Create an unbalanced and dirty GnuCash transaction with a split to @a gnc_acc
 * from the information available in the AqBanking transaction @a ab_trans.
 *
 * @param ab_trans AqBanking transaction
 * @param gnc_acc Account of to use for the split
 * @return A dirty GnuCash transaction or NULL otherwise
 */
Transaction *gnc_ab_trans_to_gnc (const AB_TRANSACTION *ab_trans, Account *gnc_acc);

/** Complete the import-context traversal without nested GTK loops. The caller
 * retains ownership of the AqBanking context and must keep it alive until
 * @a completed is called. Completion runs exactly once on GTK's main thread.
 * Ownership of a non-NULL GncABImExContextImport transfers to the callback;
 * release it with gnc_ab_ieci_free() after matcher completion. The job list
 * transfers out through gnc_ab_ieci_get_job_list() and remains caller-owned.
 * A NULL result means the import was cancelled. */
typedef void (*GncABImportContextDoneCallback) (
    GncABImExContextImport *ieci, gpointer user_data);
void gnc_ab_import_context_async (AB_IMEXPORTER_CONTEXT *context,
                                  guint awaiting,
                                  gboolean execute_txns,
                                  AB_BANKING *api,
                                  GtkWidget *parent,
                                  GncABImportContextDoneCallback completed,
                                  gpointer user_data);
void gnc_ab_ieci_free (GncABImExContextImport *ieci);

/**
 * Extract awaiting from @a data.
 *
 * @param ieci The value received by the async completion callback
 * @return The initial awaiting bitmask plus IGNORE_* for unexpected and then
 * ignored items, and FOUND_* for non-empty items
 */
guint gnc_ab_ieci_get_found (GncABImExContextImport *ieci);

/**
 * Extract the job list from @a data.
 *
 * @param ieci The value received by the async completion callback
 * @return The list of jobs, freeable with AB_Job_List2_FreeAll()
 */
GNC_AB_JOB_LIST2 *gnc_ab_ieci_get_job_list (GncABImExContextImport *ieci);

/**
 * Present the generic transaction matcher dialog.
 *
 * @param ieci The value received by the async completion callback
 * @param completed Called after the dialog and importer state have been closed.
 */
typedef void (*GncABMatcherDoneCallback)(gboolean accepted, gpointer user_data);
void gnc_ab_ieci_run_matcher_async (GncABImExContextImport *ieci,
                                    GncABMatcherDoneCallback completed,
                                    gpointer user_data);


/**
 * get the GWEN_DB_NODE from AqBanking configuration files
 *
 * This synchronous helper may only touch AB_BANKING while the caller owns the
 * exclusive operation token; outside an operation it returns NULL.
 * @return a GWEN_DB containing all permanently accepted SSL certificates (hashed).
 */
GWEN_DB_NODE *gnc_ab_get_permanent_certs (void);

/**
 * Creates an online ID from bank code and account number.
 *
 * The returned string must be g_free'd by the caller.
 *
 * @param bankcode       Bank code
 * @param accountnumber  Account number
 * @return an online ID
 */
gchar* gnc_ab_create_online_id (const gchar *bankcode, const gchar *accountnumber);

#if (AQBANKING_VERSION_INT >= 60400)
/**
 * Obtain the list of templates based on the aqbanking account spec's target accounts.
 *
 * @param ab_abb aqbanking account spec.
 * @return A GList of newly allocated GncABTransTempls
 */
GList*
gnc_ab_trans_templ_list_new_from_ref_accounts (GNC_AB_ACCOUNT_SPEC *ab_acc);
#endif
/**
 * Retrieve the available AQBanking importers.
 * @param abi The AQBanking instance.
 * @return a GList<AB_Node_Pair> containing importer names and descriptions.
 */
GList* gnc_ab_imexporter_list (AB_BANKING* abi);

/**
 * Retrieve the available format templates for an AQBanking importer.
 * @param api the AQBanking instance.
 * @param importer_name The importer's name.
 * @return a GList<AB_Node_Pair> containing format template names and descriptions.
 */
GList* gnc_ab_imexporter_profile_list (AB_BANKING* abi,
                                       const char* importer_name);

G_END_DECLS

/** @} */
/** @} */

#endif /* GNC_AB_UTILS_H */
