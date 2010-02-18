/********************************************************************\
 * gnc-hbci-utils.h -- hbci utility functions                       *
 * Copyright (C) 2002 Christian Stimming                            *
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
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
\********************************************************************/

#ifndef GNC_HBCI_UTILS_H
#define GNC_HBCI_UTILS_H

#include <iconv.h>
#include <aqbanking/banking.h>
#include <aqbanking/transaction.h>
#include <aqbanking/account.h>
#include <aqbanking/imexporter.h>
#include <aqbanking/version.h>
#if AQBANKING_VERSION_MAJOR > 2
# define AB_Value_GetValue AB_Value_GetValueAsDouble
# define GWEN_TYPE_UINT32 uint32_t
# define GWEN_LoggerLevelError GWEN_LoggerLevel_Error
# define GWEN_LoggerLevelWarning  GWEN_LoggerLevel_Warning
# define GWEN_LoggerLevelNotice  GWEN_LoggerLevel_Notice
# define GWEN_LoggerLevelInfo  GWEN_LoggerLevel_Info
# define GWEN_LoggerLevelDebug  GWEN_LoggerLevel_Debug
# define AB_Banking_LogLevelVerbous GWEN_LoggerLevel_Verbous
# define AB_Banking_LogLevelNotice GWEN_LoggerLevel_Notice
# define AB_BANKING_LOGLEVEL GWEN_LOGGER_LEVEL
# define AB_Banking_new(arg1, arg2) AB_Banking_new(arg1, arg2, 0)
# define AB_ERROR_FOUND GWEN_ERROR_FOUND
#endif

#include "gnc-ui.h"
#include "Account.h"
#include "Transaction.h"
#include "gnc-book.h"
#include "import-main-matcher.h"

#include "hbci-interaction.h"


/** Create a new AB_BANKING and let it load its environment from its
 * default configuration.
 *
 * @param parent When displaying dialogs, use this GtkWidget as parent.
 * @param inter Reference to a GNCInteractor-pointer in order to use this later.
 * May be NULL.
 */
AB_BANKING * gnc_AB_BANKING_new_currentbook (GtkWidget *parent,
        GNCInteractor **inter);

/** Delete the given AB_BANKING. If this is also the one that was cached
    by gnc_AB_BANKING_new_currentbook, then that reference is deleted, too. */
void gnc_AB_BANKING_delete (AB_BANKING *api);


/* Finish using the API for now. Let the API save its current
 * state. Return nonzero if an error occurred. After this call, you
 * may only call gnc_AB_BANKING_new_currentbook to get the api again
 * in a properly initialized state. */
int gnc_AB_BANKING_fini (AB_BANKING *api);


/* Get the corresponding HBCI account to a gnucash account. Of course
 * this only works after the gnucash account has been set up for HBCI
 * use, i.e. the kvp_frame "hbci/..." have been filled with
 * information. Returns NULL if no AB_ACCOUNT was found.
 *
 * @param api The AB_BANKING to get the AB_ACCOUNT from.
 * @param gnc_acc The gnucash account to query for AB_ACCOUNT reference data. */
AB_ACCOUNT *
gnc_hbci_get_hbci_acc (const AB_BANKING *api, Account *gnc_acc);

/** Clean up the queue after executing, i.e. delete the job as good as
    possible. */
void
gnc_hbci_cleanup_job(AB_BANKING *api, AB_JOB *job);

/* Check int on whether some feedback should be given to the
 * user. Returns true if the HBCI action should be tried again; on the
 * other hand, returns false if the user can't do anything about this
 * error right now. */
gboolean
gnc_hbci_Error_retry (GtkWidget *parent, int error,
                      GNCInteractor *inter);

/* Calls AB_BANKING_executeQueue with some supplementary stuff around
 * it: set the debugLevel, show the GNCInteractor, and do some error
 * checking. Returns TRUE upon success or FALSE if the calling dialog
 * should abort. parent may be NULL, job may be NULL (although in this
 * case no HBCI result codes can be checked!), inter may be NULL; api
 * must not be NULL.  */
gboolean
gnc_AB_BANKING_execute (GtkWidget *parent, AB_BANKING *api,
                        AB_JOB *job, GNCInteractor *inter);

/* Calls AB_Banking_executeQueue() with some supplementary stuff
 * around it: set the debugLevel, show the GNCInteractor, and do some
 * error checking afterwards by checking each AB_JOB in
 * job_list. Returns TRUE upon success or FALSE if the calling dialog
 * should abort. parent may be NULL, job_list (a GList of AB_JOBs) may
 * be NULL (although in this case no HBCI result codes can be
 * checked!), inter may be NULL; api must not be NULL.  */
gboolean
gnc_hbci_multijob_execute(GtkWidget *parent, AB_BANKING *api,
                          GList *job_list, GNCInteractor *inter);

/**
 * Imports the account/transaction/balance data of an aqbanking
 * "imexporter-context" into the matching gnucash accounts, using the
 * given importer_generic_gui.
 *
 * If exec_as_jobs is TRUE, additionally queue each transaction as a
 * new aqbanking online banking job.
 *
 * @param ab The AB_BANKING api object.
 *
 * @param ctx The "context" object that holds the actual transactions.
 *
 * @param importer_generic_gui The dialog which should display the
 * to-be-imported transactions.
 *
 * @param exec_as_aqbanking_jobs If TRUE, additionally queue the
 * imported transactions as online jobs over aqbanking/HBCI. If FALSE,
 * just import the transactions and that's it.
 *
 * @return If exec_as_aqbanking_jobs was FALSE, this always returns
 * NULL. Otherwise it returns a GList of the AB_JOBs that have been
 * queued into aqbanking.
 */
GList *
gnc_hbci_import_ctx(AB_BANKING *ab, AB_IMEXPORTER_CONTEXT *ctx,
                    GNCImportMainMatcher *importer_generic_gui,
                    gboolean exec_as_aqbanking_jobs);

/** Clear all the AB_JOBs of the ab_job_list from aqbanking's
 * queue. */
void
gnc_hbci_clearqueue(AB_BANKING *ab, GList *ab_job_list);

/* Create the appropriate description field for a Gnucash Transaction
 * by the information given in the AB_TRANSACTION h_trans. The
 * returned string must be g_free'd by the caller. */
char *gnc_hbci_descr_tognc (const AB_TRANSACTION *h_trans);

/* Create the appropriate memo field for a Gnucash Split by the
 * information given in the AB_TRANSACTION h_trans. The returned
 * string must be g_free'd by the caller. */
char *gnc_hbci_memo_tognc (const AB_TRANSACTION *h_trans);

/* Retrieve the merged purpose fields from the transaction. The
 * returned string must be g_free'd by the caller. If there was no
 * purpose, an empty (but allocated) string is returned. */
char *gnc_hbci_getpurpose (const AB_TRANSACTION *h_trans);

/* Retrieve the merged "remote name"  fields from the transaction. The
 * returned string must be g_free'd by the caller. If there was no
 * "remote name" field, NULL (!) is returned. */
char *gnc_hbci_getremotename (const AB_TRANSACTION *h_trans);

/** Return the first customer that can act on the specified account,
    or NULL if none was found (and an error message is printed on
    stdout). */
/* const HBCI_Customer * */
/* gnc_hbci_get_first_customer(const AB_ACCOUNT *h_acc); */

/** Returns the name of this bank. This function is helpful because it
 * always makes sure to return a valid const char pointer, even if no
 * bankName is available. */
/* const char *bank_to_str (const HBCI_Bank *bank); */

/** Chooses one bank out of the given list.
 *
 * If the list has more than one bank, this displays a multichoice
 * dialog so that the user can choose one bank. If the list has only
 * one bank, it returns it. If the list has zero banks, it returns
 * NULL. */
/* const HBCI_Bank * */
/* choose_one_bank (gncUIWidget parent, const list_HBCI_Bank *banklist); */

/** Chooses one customer out of the given list.
 *
 * If the list has more than one customer, this displays a multichoice
 * dialog so that the user can choose one customer. If the list has only
 * one customer, it returns it. If the list has zero customers, it returns
 * NULL. */
/* const HBCI_Customer * */
/* choose_one_customer (gncUIWidget parent, const list_HBCI_Customer *custlist); */

/** Chooses one user out of the given list.
 *
 * If the list has more than one user, this displays a multichoice
 * dialog so that the user can choose one user. If the list has only
 * one user, it returns it. If the list has zero users, it returns
 * NULL. */
/* const HBCI_User * */
/* choose_one_user (gncUIWidget parent, const list_HBCI_User *userlist); */

/** Return a newly allocated string. */
char *gnc_AB_VALUE_toReadableString(const AB_VALUE *v);

/** Returns a newly allocated gchar, converted according to the given
   handler */
gchar *gnc_call_iconv(GIConv handler, const gchar* input);

/** Returns the encoding of the current book in the format as required
    by iconv_open(3). */
const char *gnc_hbci_book_encoding(void);

/** Returns the encoding that is required by AqBanking in the format
    as required by iconv_open(3). */
const char *gnc_hbci_AQBANKING_encoding(void);

#endif
