/*
 * gnc-ab-transfer.c --
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
 * @internal
 * @file gnc-ab-utils.c
 * @brief AqBanking transfer functions
 * @author Copyright (C) 2002 Christian Stimming <stimming@tuhh.de>
 * @author Copyright (C) 2004 Bernd Wagner
 * @author Copyright (C) 2006 David Hampton <hampton@employees.org>
 * @author Copyright (C) 2008 Andreas Koehler <andi5.py@gmx.net>
 */

#include "config.h"

#include <glib/gi18n.h>
#include <gtk/gtk.h>
#include <aqbanking/banking.h>

#include "Transaction.h"
#include "dialog-transfer.h"
#include "gnc-ab-transfer.h"
#include "gnc-ab-kvp.h"
#include "gnc-ab-utils.h"
#include "gnc-ab-trans-templ.h"
#include "gnc-gwen-gui.h"
#include "gnc-ui.h"

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = G_LOG_DOMAIN;

static void save_templates(GtkWidget *parent, Account *gnc_acc, GList *templates,
                           gboolean dont_ask);
static void txn_created_cb(Transaction *trans, gpointer user_data);

static void
save_templates(GtkWidget *parent, Account *gnc_acc, GList *templates,
               gboolean dont_ask)
{
    g_return_if_fail(gnc_acc);
    if (dont_ask || gnc_verify_dialog(
            parent, FALSE, "%s",
            _("You have changed the list of online transfer templates, "
              "but you cancelled the transfer dialog. "
              "Do you nevertheless want to store the changes?"))) {
        GList *kvp_list = gnc_ab_trans_templ_list_to_kvp_list(templates);
        gnc_ab_set_book_template_list(gnc_account_get_book(gnc_acc), kvp_list);
    }
}

static void
txn_created_cb(Transaction *trans, gpointer user_data)
{
    Transaction **trans_loc = user_data;

    if (!trans) return;
    g_return_if_fail(trans_loc);
    *trans_loc = trans;
}

void
gnc_ab_maketrans(GtkWidget *parent, Account *gnc_acc,
                 GncABTransType trans_type)
{
    AB_BANKING *api;
    gboolean online = FALSE;
    AB_ACCOUNT *ab_acc;
    GncABTransDialog *dialog = NULL;
    GList *templates = NULL;
    GncABTransDialog *td = NULL;
    gboolean successful;
    gboolean aborted = FALSE;

    g_return_if_fail(parent && gnc_acc);

    /* Get the API */
    api = gnc_AB_BANKING_new();
    if (!api) {
        g_warning("gnc_ab_maketrans: Couldn't get AqBanking API");
        return;
    }
    if (AB_Banking_OnlineInit(api) != 0) {
        g_warning("gnc_ab_maketrans: Couldn't initialize AqBanking API");
        goto cleanup;
    }
    online = TRUE;

    /* Get the AqBanking Account */
    ab_acc = gnc_ab_get_ab_account(api, gnc_acc);
    if (!ab_acc) {
        g_warning("gnc_ab_gettrans: No AqBanking account found");
        goto cleanup;
    }

    /* Get list of template transactions */
    templates = gnc_ab_trans_templ_list_new_from_kvp_list(
        gnc_ab_get_book_template_list(gnc_account_get_book(gnc_acc)));

    /* Create new ABTransDialog */
    td = gnc_ab_trans_dialog_new(parent, ab_acc,
                                 xaccAccountGetCommoditySCU(gnc_acc),
                                 trans_type, templates);
    templates = NULL;

    /* Repeat until AqBanking action was successful or user pressed cancel */
    do {
        GncGWENGui *gui = NULL;
        gint result;
        gboolean changed;
        const AB_TRANSACTION *ab_trans;
        AB_JOB *job = NULL;
        AB_JOB_LIST2 *job_list = NULL;
        XferDialog *xfer_dialog = NULL;
        gnc_numeric amount;
        gchar *description;
        gchar *memo;
        Transaction *gnc_trans = NULL;
        AB_IMEXPORTER_CONTEXT *context = NULL;
        GncABImExContextImport *ieci = NULL;

        /* Get a GUI object */
        gui = gnc_GWEN_Gui_get(parent);
        if (!gui) {
            g_warning("gnc_ab_maketrans: Couldn't initialize Gwenhywfar GUI");
            aborted = TRUE;
            goto repeat;
        }

        /* Let the user enter the values */
        result = gnc_ab_trans_dialog_run_until_ok(td);

        if (result != GNC_RESPONSE_NOW && result != GNC_RESPONSE_LATER) {
            aborted = TRUE;
            goto repeat;
        }

        /* Save the templates */
        templates = gnc_ab_trans_dialog_get_templ(td, &changed);
        if (changed)
            save_templates(parent, gnc_acc, templates,
                           (result == GNC_RESPONSE_NOW));
        g_list_free(templates);
        templates = NULL;

        /* Get a job and enqueue it */
        ab_trans = gnc_ab_trans_dialog_get_ab_trans(td);
        job = gnc_ab_trans_dialog_get_job(td);
        if (!job || AB_Job_CheckAvailability(job, 0)) {
            if (!gnc_verify_dialog(
                    parent, FALSE, "%s",
                    _("The backend found an error during the preparation "
                      "of the job. It is not possible to execute this job. \n"
                      "\n"
                      "Most probable the bank does not support your chosen "
                      "job or your Online Banking account does not have the permission "
                      "to execute this job. More error messages might be "
                      "visible on your console log.\n"
                      "\n"
                      "Do you want to enter the job again?")))
                aborted = TRUE;
            goto repeat;
        }
        job_list = AB_Job_List2_new();
        AB_Job_List2_PushBack(job_list, job);

        /* Setup a Transfer Dialog for the GnuCash transaction */
        xfer_dialog = gnc_xfer_dialog(gnc_ab_trans_dialog_get_parent(td),
                                      gnc_acc);
        switch (trans_type) {
        case SINGLE_DEBITNOTE:
            gnc_xfer_dialog_set_title(
                xfer_dialog, _("Online Banking Direct Debit Note"));
            gnc_xfer_dialog_lock_to_account_tree(xfer_dialog);
            break;
        case SINGLE_INTERNAL_TRANSFER:
            gnc_xfer_dialog_set_title(
                xfer_dialog, _("Online Banking Bank-Internal Transfer"));
            gnc_xfer_dialog_lock_from_account_tree(xfer_dialog);
            break;
        case SINGLE_TRANSFER:
        default:
            gnc_xfer_dialog_set_title(
                xfer_dialog, _("Online Banking Transaction"));
            gnc_xfer_dialog_lock_from_account_tree(xfer_dialog);
        }

        amount = double_to_gnc_numeric(
            AB_Value_GetValueAsDouble(AB_Transaction_GetValue(ab_trans)),
            xaccAccountGetCommoditySCU(gnc_acc),
            GNC_RND_ROUND);
        gnc_xfer_dialog_set_amount(xfer_dialog, amount);

        description = gnc_ab_description_to_gnc(ab_trans);
        gnc_xfer_dialog_set_description(xfer_dialog, description);
        g_free(description);

        memo = gnc_ab_memo_to_gnc(ab_trans);
        gnc_xfer_dialog_set_memo(xfer_dialog, memo);
        g_free(memo);

        gnc_xfer_dialog_set_txn_cb(xfer_dialog, txn_created_cb, &gnc_trans);

        /* And run it */
        successful = gnc_xfer_dialog_run_until_done(xfer_dialog);

        /* On cancel, go back to the AB transaction dialog */
        if (!successful || !gnc_trans) {
            successful = FALSE;
            goto repeat;
        }

        if (result == GNC_RESPONSE_NOW) {
            /* Create a context to store possible results */
            context = AB_ImExporterContext_new();

            /* Finally, execute the job */
            successful = AB_Banking_ExecuteJobs(api, job_list, context, 0) == 0;

            if (!successful
                || AB_Job_GetStatus(job) != AB_Job_StatusFinished) {
                successful = FALSE;
                if (!gnc_verify_dialog(
                        parent, FALSE, "%s",
                        _("An error occurred while executing the job.  Please check "
                          "the log window for the exact error message.\n"
                          "\n"
                          "Do you want to enter the job again?"))) {
                        /* _("The job was sent to the bank successfully, but the " */
                        /*   "bank is refusing to execute the job. Please check " */
                        /*   "the log window for the exact error message of the " */
                        /*   "bank. The line with the error message contains a " */
                        /*   "code number that is greater than 9000.\n" */
                        /*   "\n" */
                        /*   "Do you want to enter the job again?"))) { */
                    aborted = TRUE;
                }
            }

            /* Import the results, awaiting nothing */
            ieci = gnc_ab_import_context(context, 0, FALSE, NULL, parent);
        }
        /* Simply ignore any other case */

    repeat:
        /* Clean up */
        if (gnc_trans && !successful) {
            xaccTransBeginEdit(gnc_trans);
            xaccTransDestroy(gnc_trans);
            xaccTransCommitEdit(gnc_trans);
            gnc_trans = NULL;
        }
        if (ieci)
            g_free(ieci);
        if (context)
            AB_ImExporterContext_free(context);
        if (job_list) {
            AB_Job_List2_free(job_list);
            job_list = NULL;
        }
        if (job) {
            AB_Job_free(job);
            job = NULL;
        }
        if (gui) {
            gnc_GWEN_Gui_release(gui);
            gui = NULL;
        }

    } while (!successful && !aborted);

cleanup:
    if (td)
        gnc_ab_trans_dialog_free(td);
    if (online)
        AB_Banking_OnlineFini(api);
    gnc_AB_BANKING_fini(api);
}
