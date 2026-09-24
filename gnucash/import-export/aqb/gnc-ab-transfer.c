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

#include <config.h>

#include <glib/gi18n.h>
#include <gtk/gtk.h>
#include <aqbanking/banking.h>

#include <gnc-aqbanking-templates.h>
#include <Transaction.h>
#include "dialog-transfer.h"
#include "gnc-ab-transfer.h"
#include "gnc-ab-kvp.h"
#include "gnc-ab-utils.h"
#include "gnc-gwen-gui.h"
#include "gnc-ui.h"

/* This static indicates the debugging module that this .o belongs to.  */
G_GNUC_UNUSED static QofLogModule log_module = G_LOG_DOMAIN;

static void txn_created_cb(Transaction *trans, gpointer user_data);

static void
txn_created_cb(Transaction *trans, gpointer user_data)
{
    Transaction **trans_loc = user_data;

    if (!trans) return;
    g_return_if_fail(trans_loc);
    *trans_loc = trans;
}

typedef struct
{
    AB_BANKING *api;
    GNC_AB_ACCOUNT_SPEC *ab_acc;
    GWeakRef parent;
    Account *gnc_acc;
    GncABTransType trans_type;
    GncABTransDialog *td;
} TransferData;

static void transfer_request_input (TransferData *data);

static void
transfer_data_free (TransferData *data)
{
    if (!data)
        return;

    if (data->td)
        gnc_ab_trans_dialog_free (data->td);
    g_weak_ref_clear (&data->parent);
    if (data->api)
        gnc_AB_BANKING_fini (data->api);
    g_free (data);
}

static void
transfer_set_xfer_dialog_title (XferDialog *xfer_dialog,
                                GncABTransType trans_type)
{
    switch (trans_type)
    {
    case SINGLE_DEBITNOTE:
        gnc_xfer_dialog_set_title (xfer_dialog,
                                   _("Online Banking Direct Debit Note"));
        gnc_xfer_dialog_lock_to_account_tree (xfer_dialog);
        break;
    case SINGLE_INTERNAL_TRANSFER:
        gnc_xfer_dialog_set_title (
            xfer_dialog, _("Online Banking Bank-Internal Transfer"));
        gnc_xfer_dialog_lock_from_account_tree (xfer_dialog);
        break;
    case SEPA_TRANSFER:
        gnc_xfer_dialog_set_title (
            xfer_dialog, _("Online Banking European (SEPA) Transfer"));
        gnc_xfer_dialog_lock_from_account_tree (xfer_dialog);
        break;
#if (AQBANKING_VERSION_INT >= 60400)
    case SEPA_INTERNAL_TRANSFER:
        gnc_xfer_dialog_set_title (
            xfer_dialog,
            _("Online Banking European (SEPA) Internal Transfer"));
        gnc_xfer_dialog_lock_from_account_tree (xfer_dialog);
        break;
#endif
    case SEPA_DEBITNOTE:
        gnc_xfer_dialog_set_title (
            xfer_dialog, _("Online Banking European (SEPA) Debit Note"));
        gnc_xfer_dialog_lock_to_account_tree (xfer_dialog);
        break;
    case SINGLE_TRANSFER:
    default:
        gnc_xfer_dialog_set_title (xfer_dialog, _("Online Banking Transaction"));
        gnc_xfer_dialog_lock_from_account_tree (xfer_dialog);
    }
}

typedef struct
{
    TransferData *data;
    GWeakRef parent;
    GNC_AB_JOB *job;
    GNC_AB_JOB_LIST2 *job_list;
    gboolean send_now;
    Transaction *gnc_trans;
} TransferExecution;

typedef struct
{
    TransferData *data;
    gint response;
    GList *templates;
} TransferTemplateSaveRequest;

typedef struct
{
    TransferData *data;
} TransferRetryRequest;

typedef struct
{
    TransferExecution *execution;
} TransferExecutionRetryRequest;

static void transfer_process_dialog_response (TransferData *data, gint response);
static void transfer_handle_template_changes (TransferData *data, gint response);
static void transfer_retry_finished (GtkWindow *parent, gint response,
                                    gpointer user_data);
static void transfer_execution_retry_finished (GtkWindow *parent, gint response,
                                               gpointer user_data);
#if (AQBANKING_VERSION_INT >= 60400)
static void transfer_template_save_finished (GtkWindow *parent, gint response,
                                             gpointer user_data);
#endif

static void transfer_execution_finish (TransferExecution *execution,
                                       gboolean successful, gboolean retry);
static void transfer_execution_import_finished (GncABImExContextImport *ieci,
                                                gboolean completed,
                                                gpointer user_data);

static void
transfer_execution_xfer_finished_cb (gboolean completed, gpointer user_data)
{
    TransferExecution *execution = user_data;
    GtkWidget *parent = g_weak_ref_get (&execution->parent);
    GncGWENGui *gui = NULL;
    AB_IMEXPORTER_CONTEXT *context = NULL;
    GNC_AB_JOB_STATUS job_status;
    if (!completed || !execution->gnc_trans || !parent)
        goto cleanup;
    if (!execution->send_now)
    {
        transfer_execution_finish (execution, TRUE, FALSE);
        g_clear_object (&parent);
        return;
    }

    context = AB_ImExporterContext_new ();
    gui = gnc_GWEN_Gui_get (parent);
    if (!gui)
    {
        g_warning ("gnc_ab_maketrans: Couldn't initialize Gwenhywfar GUI");
        goto cleanup;
    }
    AB_Banking_SendCommands (execution->data->api, execution->job_list, context);
    job_status = AB_Transaction_GetStatus (execution->job);
    if (job_status != AB_Transaction_StatusAccepted &&
        job_status != AB_Transaction_StatusPending)
    {
        TransferExecutionRetryRequest *request = g_new0 (
            TransferExecutionRetryRequest, 1);

        if (context)
            AB_ImExporterContext_free (context);
        if (gui)
            gnc_GWEN_Gui_release (gui);
        request->execution = execution;
        gnc_verify_dialog_async (
            GTK_WINDOW (parent), FALSE, transfer_execution_retry_finished, request,
            "%s", _("An error occurred while executing the job. Please check "
                     "the log window for the exact error message.\n\n"
                     "Do you want to enter the job again?"));
        g_clear_object (&parent);
        return;
    }
    gnc_ab_import_context_async (context, 0, FALSE, NULL, parent, NULL,
                                 transfer_execution_import_finished, execution);
    context = NULL;
    if (gui)
        gnc_GWEN_Gui_release (gui);
    g_object_unref (parent);
    return;

cleanup:
    if (context)
        AB_ImExporterContext_free (context);
    if (gui)
        gnc_GWEN_Gui_release (gui);
    g_clear_object (&parent);
    transfer_execution_finish (execution, FALSE, FALSE);
}

static void
transfer_execution_import_finished (GncABImExContextImport *ieci,
                                    gboolean completed, gpointer user_data)
{
    TransferExecution *execution = user_data;

    (void)ieci;
    /* The bank job has already completed. A later parent/book cancellation must
     * not mutate the old transaction while releasing the external resources. */
    if (!completed)
        execution->gnc_trans = NULL;
    transfer_execution_finish (execution, completed, FALSE);
}

static void
transfer_execution_retry_finished (GtkWindow *parent, gint response,
                                   gpointer user_data)
{
    TransferExecutionRetryRequest *request = user_data;

    (void)parent;
    transfer_execution_finish (request->execution, FALSE,
                               response == GTK_RESPONSE_YES);
    g_free (request);
}

static void
transfer_execution_finish (TransferExecution *execution, gboolean successful,
                           gboolean retry)
{
    if (execution->gnc_trans && !successful)
    {
        xaccTransBeginEdit (execution->gnc_trans);
        xaccTransDestroy (execution->gnc_trans);
        xaccTransCommitEdit (execution->gnc_trans);
    }
    if (execution->job_list)
        AB_Transaction_List2_free (execution->job_list);
    if (execution->job)
        AB_Transaction_free (execution->job);
    g_weak_ref_clear (&execution->parent);
    if (retry)
        transfer_request_input (execution->data);
    else
        transfer_data_free (execution->data);
    g_free (execution);
}

static void
transfer_create_gnucash_transaction_async (TransferExecution *execution,
                                           GtkWidget *parent,
                                           const AB_TRANSACTION *ab_trans)
{
    XferDialog *xfer_dialog;
    gnc_numeric amount;
    gchar *description;
    gchar *memo;

    xfer_dialog = gnc_xfer_dialog (parent, execution->data->gnc_acc);
    transfer_set_xfer_dialog_title (xfer_dialog, execution->data->trans_type);
    gnc_xfer_dialog_set_to_show_button_active (xfer_dialog, TRUE);
    amount = double_to_gnc_numeric (AB_Value_GetValueAsDouble (AB_Transaction_GetValue (ab_trans)),
                                    xaccAccountGetCommoditySCU (execution->data->gnc_acc),
                                    GNC_HOW_RND_ROUND_HALF_UP);
    gnc_xfer_dialog_set_amount (xfer_dialog, amount);
    gnc_xfer_dialog_set_amount_sensitive (xfer_dialog, FALSE);
    gnc_xfer_dialog_set_date_sensitive (xfer_dialog, FALSE);
    description = gnc_ab_description_to_gnc (ab_trans, FALSE);
    gnc_xfer_dialog_set_description (xfer_dialog, description);
    g_free (description);
    memo = gnc_ab_memo_to_gnc (ab_trans);
    gnc_xfer_dialog_set_memo (xfer_dialog, memo);
    g_free (memo);
    gnc_xfer_dialog_set_txn_cb (xfer_dialog, txn_created_cb, &execution->gnc_trans);
    gnc_xfer_dialog_run_async (xfer_dialog, transfer_execution_xfer_finished_cb,
                               execution);
}
static void
transfer_retry_finished (GtkWindow *parent, gint response, gpointer user_data)
{
    TransferRetryRequest *request = user_data;

    (void)parent;
    if (response == GTK_RESPONSE_YES)
        transfer_request_input (request->data);
    else
        transfer_data_free (request->data);
    g_free (request);
}

static void
transfer_process_dialog_response (TransferData *data, gint response)
{
    GtkWidget *parent;
    GNC_AB_JOB *job = NULL;
    GNC_AB_JOB_LIST2 *job_list = NULL;
    const AB_TRANSACTION *ab_trans;

    parent = g_weak_ref_get (&data->parent);
    if (!parent)
    {
        transfer_data_free (data);
        return;
    }

    if (response != GNC_RESPONSE_NOW && response != GNC_RESPONSE_LATER)
        goto cleanup;

    ab_trans = gnc_ab_trans_dialog_get_ab_trans (data->td);
    job = gnc_ab_trans_dialog_get_job (data->td);
    if (!job || !AB_AccountSpec_GetTransactionLimitsForCommand (
            data->ab_acc, AB_Transaction_GetCommand (job)))
    {
        TransferRetryRequest *request = g_new0 (TransferRetryRequest, 1);

        if (job)
            AB_Transaction_free (job);
        request->data = data;
        gnc_verify_dialog_async (
            GTK_WINDOW (parent), FALSE, transfer_retry_finished, request,
            "%s", _("The backend found an error during the preparation "
                     "of the job. It is not possible to execute this job.\n"
                     "\n"
                     "Most probable the bank does not support your chosen "
                     "job or your Online Banking account does not have the permission "
                     "to execute this job. More error messages might be "
                     "visible on your console log.\n"
                     "\n"
                     "Do you want to enter the job again?"));
        g_object_unref (parent);
        return;
    }

    job_list = AB_Transaction_List2_new ();
    AB_Transaction_List2_PushBack (job_list, job);
    {
        TransferExecution *execution = g_new0 (TransferExecution, 1);

        execution->data = data;
        execution->job = job;
        execution->job_list = job_list;
        execution->send_now = response == GNC_RESPONSE_NOW;
        g_weak_ref_init (&execution->parent, parent);
        transfer_create_gnucash_transaction_async (execution, parent, ab_trans);
    }
    g_object_unref (parent);
    return;

cleanup:
    if (job_list)
        AB_Transaction_List2_free (job_list);
    if (job)
        AB_Transaction_free (job);
    g_object_unref (parent);
    transfer_data_free (data);
}

#if (AQBANKING_VERSION_INT >= 60400)
static void
transfer_template_save_finished (GtkWindow *parent, gint response,
                                 gpointer user_data)
{
    TransferTemplateSaveRequest *request = user_data;

    (void)parent;
    if (response == GTK_RESPONSE_YES)
        gnc_ab_set_book_template_list (gnc_account_get_book (request->data->gnc_acc),
                                       request->templates);
    g_list_free (request->templates);
    transfer_process_dialog_response (request->data, request->response);
    g_free (request);
}
#endif

static void
transfer_handle_template_changes (TransferData *data, gint response)
{
#if (AQBANKING_VERSION_INT >= 60400)
    gboolean changed;
    GList *templates = gnc_ab_trans_dialog_get_templ (data->td, &changed);

    if (data->trans_type != SEPA_INTERNAL_TRANSFER && changed)
    {
        if (response == GNC_RESPONSE_NOW)
        {
            gnc_ab_set_book_template_list (gnc_account_get_book (data->gnc_acc),
                                           templates);
            g_list_free (templates);
        }
        else
        {
            GtkWidget *parent = g_weak_ref_get (&data->parent);
            TransferTemplateSaveRequest *request;

            if (!parent)
            {
                g_list_free (templates);
                transfer_data_free (data);
                return;
            }

            request = g_new0 (TransferTemplateSaveRequest, 1);
            request->data = data;
            request->response = response;
            request->templates = templates;
            gnc_verify_dialog_async (
                GTK_WINDOW (parent), FALSE, transfer_template_save_finished, request,
                "%s", _("You have changed the list of online transfer templates, "
                         "but you cancelled the transfer dialog. "
                         "Do you nevertheless want to store the changes?"));
            g_object_unref (parent);
            return;
        }
    }
    else
    {
        g_list_free (templates);
    }
#endif
    transfer_process_dialog_response (data, response);
}

static void
transfer_dialog_finished (GObject *source, GAsyncResult *result,
                          gpointer user_data)
{
    TransferData *data = user_data;
    GError *error = NULL;
    gint response;

    (void)source;
    if (!gnc_ab_trans_dialog_run_finish (result, &response, &error))
    {
        if (error && !g_error_matches (error, G_IO_ERROR, G_IO_ERROR_CANCELLED))
            g_warning ("AqBanking transfer dialog: %s", error->message);
        g_clear_error (&error);
        transfer_data_free (data);
        return;
    }

    transfer_handle_template_changes (data, response);
}

static void
transfer_request_input (TransferData *data)
{
    gnc_ab_trans_dialog_run_async (data->td, NULL, transfer_dialog_finished,
                                   data);
}

void
gnc_ab_maketrans (GtkWidget *parent, Account *gnc_acc,
                  GncABTransType trans_type)
{
    AB_BANKING *api;
    GNC_AB_ACCOUNT_SPEC *ab_acc;
    GList *templates = NULL;
    GncABTransDialog *td;
    TransferData *data;

    g_return_if_fail (parent && gnc_acc);

    api = gnc_AB_BANKING_new ();
    if (!api)
    {
        g_warning ("gnc_ab_maketrans: Couldn't get AqBanking API");
        return;
    }

    ab_acc = gnc_ab_get_ab_account (api, gnc_acc);
    if (!ab_acc)
    {
        g_warning ("gnc_ab_gettrans: No AqBanking account found");
        gnc_error_dialog (GTK_WINDOW (parent),
                          _("No valid online banking account assigned."));
        gnc_AB_BANKING_fini (api);
        return;
    }

#if (AQBANKING_VERSION_INT >= 60400)
    if (trans_type == SEPA_INTERNAL_TRANSFER)
    {
        templates = gnc_ab_trans_templ_list_new_from_ref_accounts (ab_acc);
        if (!templates)
        {
            g_warning ("gnc_ab_gettrans: No reference accounts found");
            gnc_error_dialog (GTK_WINDOW (parent),
                              _("No reference accounts found."));
            gnc_AB_BANKING_fini (api);
            return;
        }
    }
    else
#endif
    {
        templates = gnc_ab_trans_templ_list_new_from_book (
            gnc_account_get_book (gnc_acc));
    }

    td = gnc_ab_trans_dialog_new (parent, ab_acc,
                                  xaccAccountGetCommoditySCU (gnc_acc),
                                  trans_type, templates);
    if (!td)
    {
        gnc_ab_trans_templ_list_free (templates);
        gnc_AB_BANKING_fini (api);
        return;
    }

    data = g_new0 (TransferData, 1);
    data->api = api;
    data->ab_acc = ab_acc;
    data->gnc_acc = gnc_acc;
    data->trans_type = trans_type;
    data->td = td;
    g_weak_ref_init (&data->parent, parent);
    transfer_request_input (data);
}