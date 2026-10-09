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
#include "gnc-ui-util.h"
#include "gnc-gnome-utils.h"
#include "gnc-session.h"

/* This static indicates the debugging module that this .o belongs to.  */
G_GNUC_UNUSED static QofLogModule log_module = G_LOG_DOMAIN;

typedef struct
{
    GWeakRef parent;
    gulong parent_destroy_handler;
    gboolean parent_destroyed;
    QofBook *book;
    GncGUID account_guid;
    guint lease;
    guint aq_operation;
    AB_BANKING *api;
    GNC_AB_ACCOUNT_SPEC *ab_acc;
    GncABTransDialog *td;
    GncABTransType type;
    GList *templates;
    GNC_AB_JOB *job;
    GNC_AB_JOB_LIST2 *jobs;
    GncGWENGui *gui;
    AB_IMEXPORTER_CONTEXT *context;
    GncABImExContextImport *ieci;
    Transaction *transaction;
    GncGUID transaction_guid;
    gboolean have_transaction;
    gint result;
    gboolean successful;
    gboolean aborted;
} TransferRequest;

static void transfer_show_dialog (TransferRequest *request);
static void transfer_dialog_completed (GncABTransDialog *td, gint response,
                                      gpointer user_data);
static void transfer_continue_after_templates (TransferRequest *request);
static void transfer_templates_response (GtkWindow *dialog_parent,
                                         gint response, gpointer user_data);
static void transfer_start_xfer (TransferRequest *request);
static gboolean transfer_recreate_dialog (TransferRequest *request,
                                          GtkWidget *parent,
                                          Account *account);
static void transfer_request_free (TransferRequest *request);
static void transfer_retry_response (GtkWindow *parent, gint response,
                                     gpointer user_data);
static void transfer_xfer_completed (gboolean completed, gpointer user_data);
static void transfer_job_work (GncGWENGui *gui, gpointer user_data);
static void transfer_job_completed (gpointer user_data);
static void transfer_import_completed (GncABImExContextImport *ieci,
                                       gpointer user_data);
static void transfer_matcher_completed (gboolean accepted,
                                       gpointer user_data);
static void transfer_parent_destroyed (GtkWidget *parent, gpointer user_data);

static void
transfer_parent_destroyed ([[maybe_unused]] GtkWidget *parent, gpointer user_data)
{
    ((TransferRequest *)user_data)->parent_destroyed = TRUE;
}

static void
transfer_request_free (TransferRequest *request)
{
    if (request->have_transaction && !request->successful)
    {
        Transaction *transaction = request->have_transaction ?
            xaccTransLookup (&request->transaction_guid, request->book) : NULL;
        if (transaction)
        {
            xaccTransBeginEdit (transaction);
            xaccTransDestroy (transaction);
            xaccTransCommitEdit (transaction);
        }
    }
    if (request->ieci) gnc_ab_ieci_free (request->ieci);
    if (request->context) AB_ImExporterContext_free (request->context);
    if (request->jobs) AB_Transaction_List2_free (request->jobs);
    if (request->job) AB_Transaction_free (request->job);
    if (request->td) gnc_ab_trans_dialog_free (request->td);
    g_list_free (request->templates);
    if (request->api) gnc_AB_BANKING_fini (request->api);
    if (request->gui) gnc_GWEN_Gui_release (request->gui);
    if (request->aq_operation) gnc_ab_operation_release (request->aq_operation);
    gnc_gui_end_session_operation (request->lease);
    request->lease = 0;
    g_object_unref (request->book);
    GtkWidget *parent = g_weak_ref_get (&request->parent);
    if (parent && request->parent_destroy_handler &&
        g_signal_handler_is_connected (parent, request->parent_destroy_handler))
        g_signal_handler_disconnect (parent, request->parent_destroy_handler);
    g_clear_object (&parent);
    g_weak_ref_clear (&request->parent);
    g_free (request);
}

static gboolean
transfer_request_current (TransferRequest *request, GtkWidget **parent_out,
                          Account **account_out)
{
    GtkWidget *parent = g_weak_ref_get (&request->parent);
    Account *account = xaccAccountLookup (&request->account_guid, request->book);
    gboolean valid = parent && !request->parent_destroyed &&
        !gtk_widget_in_destruction (parent) && account &&
        gnc_get_current_book () == request->book && qof_book_is_open (request->book);
    if (valid)
    {
        *parent_out = parent;
        *account_out = account;
    }
    else
        g_clear_object (&parent);
    return valid;
}

static void
transfer_txn_created (Transaction *transaction, gpointer user_data)
{
    TransferRequest *request = user_data;
    request->transaction = transaction;
    if (transaction)
    {
        request->transaction_guid = *xaccTransGetGUID (transaction);
        request->have_transaction = TRUE;
    }
}

static void
transfer_retry_response (GtkWindow *dialog_parent, gint response,
                         gpointer user_data)
{
    TransferRequest *request = user_data;
    GtkWidget *parent = NULL;
    Account *account = NULL;
    gboolean accepted = dialog_parent &&
                        !gtk_widget_in_destruction (GTK_WIDGET (dialog_parent)) &&
                        response == GTK_RESPONSE_YES;
    if (accepted && transfer_request_current (request, &parent, &account))
    {
        if (request->have_transaction)
        {
            Transaction *transaction = xaccTransLookup (&request->transaction_guid,
                                                         request->book);
            if (transaction)
            {
                xaccTransBeginEdit (transaction);
                xaccTransDestroy (transaction);
                xaccTransCommitEdit (transaction);
            }
            request->transaction = NULL;
            request->have_transaction = FALSE;
        }
        if (request->jobs) { AB_Transaction_List2_free (request->jobs); request->jobs = NULL; }
        if (request->job) { AB_Transaction_free (request->job); request->job = NULL; }
        if (request->context) { AB_ImExporterContext_free (request->context); request->context = NULL; }
        if (!transfer_recreate_dialog (request, parent, account))
        {
            transfer_request_free (request);
            g_object_unref (parent);
            return;
        }
        transfer_show_dialog (request);
    }
    else
    {
        request->aborted = TRUE;
        transfer_request_free (request);
    }
    g_clear_object (&parent);
}

static void
transfer_job_work ([[maybe_unused]] GncGWENGui *gui, gpointer user_data)
{
    TransferRequest *request = user_data;
    AB_Banking_SendCommands (request->api, request->jobs, request->context);
}

static void
transfer_import_completed (GncABImExContextImport *ieci, gpointer user_data)
{
    TransferRequest *request = user_data;
    if (!ieci)
    {
        transfer_request_free (request);
        return;
    }
    request->ieci = ieci;
    gnc_ab_ieci_run_matcher_async (ieci, transfer_matcher_completed, request);
}

static void
transfer_matcher_completed ([[maybe_unused]] gboolean accepted,
                            gpointer user_data)
{
    TransferRequest *request = user_data;
    gnc_ab_ieci_free (request->ieci);
    request->ieci = NULL;
    transfer_request_free (request);
}

static void
transfer_job_completed (gpointer user_data)
{
    TransferRequest *request = user_data;
    GtkWidget *parent = NULL;
    Account *account = NULL;
    GNC_AB_JOB_STATUS status = AB_Transaction_GetStatus (request->job);
    if (!transfer_request_current (request, &parent, &account))
    {
        transfer_request_free (request);
        return;
    }
    if (status == AB_Transaction_StatusAccepted ||
        status == AB_Transaction_StatusPending)
    {
        /* The bank has accepted the command. Keep the local transaction even
         * if the response import or its matcher is later cancelled. */
        request->successful = TRUE;
        gnc_ab_import_context_async (request->context, 0, FALSE, NULL, parent,
                                    transfer_import_completed, request);
        g_object_unref (parent);
        return;
    }
    gnc_verify_dialog_async (GTK_WINDOW (parent), FALSE,
        transfer_retry_response, request, "%s",
        _("An error occurred while executing the job. Please check the log window for the exact error message.\n\nDo you want to enter the job again?"));
    g_object_unref (parent);
}

static void
transfer_xfer_completed (gboolean completed, gpointer user_data)
{
    TransferRequest *request = user_data;
    GtkWidget *parent = NULL;
    Account *account = NULL;
    if (!transfer_request_current (request, &parent, &account))
    {
        transfer_request_free (request);
        return;
    }
    request->transaction = request->have_transaction ?
        xaccTransLookup (&request->transaction_guid, request->book) : NULL;
    if (!completed || !request->transaction)
    {
        if (request->have_transaction)
        {
            Transaction *transaction = xaccTransLookup (&request->transaction_guid,
                                                         request->book);
            if (transaction)
            {
                xaccTransBeginEdit (transaction);
                xaccTransDestroy (transaction);
                xaccTransCommitEdit (transaction);
            }
            request->transaction = NULL;
            request->have_transaction = FALSE;
        }
        if (transfer_recreate_dialog (request, parent, account))
            transfer_show_dialog (request);
        else
            transfer_request_free (request);
        g_object_unref (parent);
        return;
    }
    if (request->result == GNC_RESPONSE_NOW)
    {
        request->context = AB_ImExporterContext_new ();
        request->gui = gnc_GWEN_Gui_get (parent);
        if (!request->gui)
        {
            gnc_error_dialog (GTK_WINDOW (parent),
                _("Could not initialize the online banking user interface."));
            transfer_request_free (request);
        }
        else
            gnc_GWEN_Gui_run_job_async (request->gui, transfer_job_work,
                transfer_job_completed, request, NULL);
    }
    else
    {
        request->successful = TRUE;
        transfer_request_free (request);
    }
    g_object_unref (parent);
}

static void
transfer_show_dialog (TransferRequest *request)
{
    GtkWidget *parent = NULL;
    Account *account = NULL;
    if (!transfer_request_current (request, &parent, &account))
    {
        transfer_request_free (request);
        return;
    }
    gnc_ab_trans_dialog_run_async (request->td, transfer_dialog_completed,
                                   request);
    g_object_unref (parent);
}

static gboolean
transfer_recreate_dialog (TransferRequest *request, GtkWidget *parent,
                          Account *account)
{
    GList *templates = NULL;
    if (request->td)
    {
        gnc_ab_trans_dialog_free (request->td);
        request->td = NULL;
    }
#if (AQBANKING_VERSION_INT >= 60400)
    if (request->type == SEPA_INTERNAL_TRANSFER)
        templates = gnc_ab_trans_templ_list_new_from_ref_accounts (request->ab_acc);
#endif
        templates = gnc_ab_trans_templ_list_new_from_book (request->book);
    request->td = gnc_ab_trans_dialog_new (parent, request->ab_acc,
        xaccAccountGetCommoditySCU (account), request->type, templates);
    if (!request->td)
    {
        g_list_free (templates);
        return FALSE;
    }
    return TRUE;
}

static void
transfer_dialog_completed ([[maybe_unused]] GncABTransDialog *td,
                           gint response, gpointer user_data)
{
    TransferRequest *request = user_data;
    GtkWidget *parent = NULL;
    Account *account = NULL;
    if (!transfer_request_current (request, &parent, &account))
    {
        transfer_request_free (request);
        return;
    }
    request->result = response;
#if (AQBANKING_VERSION_INT >= 60400)
    if (request->type != SEPA_INTERNAL_TRANSFER)
    {
        gboolean changed = FALSE;
        request->templates = gnc_ab_trans_dialog_get_templ (request->td, &changed);
        if (changed && response != GNC_RESPONSE_NOW)
        {
            gnc_verify_dialog_async (GTK_WINDOW (parent), FALSE,
                transfer_templates_response, request, "%s",
                _("You changed the list of online transfer templates but cancelled the transfer. Do you want to save those changes?"));
            g_object_unref (parent);
            return;
        }
        if (changed)
        {
            gnc_ab_set_book_template_list (request->book, request->templates);
            g_list_free (request->templates);
            request->templates = NULL;
        }
    }
#endif
    transfer_continue_after_templates (request);
    g_object_unref (parent);
}

static void
transfer_templates_response (GtkWindow *dialog_parent, gint response,
                             gpointer user_data)
{
    TransferRequest *request = user_data;
    GtkWidget *parent = NULL;
    Account *account = NULL;
    gboolean accepted = dialog_parent &&
                        !gtk_widget_in_destruction (GTK_WIDGET (dialog_parent)) &&
                        response == GTK_RESPONSE_YES;
    if (!transfer_request_current (request, &parent, &account))
    {
        transfer_request_free (request);
        return;
    }
    if (accepted)
        gnc_ab_set_book_template_list (request->book, request->templates);
    g_list_free (request->templates);
    request->templates = NULL;
    transfer_continue_after_templates (request);
    g_object_unref (parent);
}

static void
transfer_continue_after_templates (TransferRequest *request)
{
    GtkWidget *parent = NULL;
    Account *account = NULL;
    if (!transfer_request_current (request, &parent, &account))
    {
        transfer_request_free (request);
        return;
    }
    if (request->result != GNC_RESPONSE_NOW &&
        request->result != GNC_RESPONSE_LATER)
    {
        request->aborted = TRUE;
        transfer_request_free (request);
        g_object_unref (parent);
        return;
    }
    request->job = gnc_ab_trans_dialog_get_job (request->td);
    if (!request->job || !AB_AccountSpec_GetTransactionLimitsForCommand (
            request->ab_acc, AB_Transaction_GetCommand (request->job)))
    {
        gnc_verify_dialog_async (GTK_WINDOW (parent), FALSE,
            transfer_retry_response, request, "%s",
            _("The backend could not prepare this job. It may be unsupported by your bank or not permitted for this account.\n\nDo you want to enter the job again?"));
        g_object_unref (parent);
        return;
    }
    request->jobs = AB_Transaction_List2_new ();
    AB_Transaction_List2_PushBack (request->jobs, request->job);
    transfer_start_xfer (request);
    g_object_unref (parent);
}

static void
transfer_start_xfer (TransferRequest *request)
{
    GtkWidget *parent = NULL;
    Account *account = NULL;
    const AB_TRANSACTION *ab_trans;
    XferDialog *xfer;
    if (!transfer_request_current (request, &parent, &account))
    {
        transfer_request_free (request);
        return;
    }
    ab_trans = gnc_ab_trans_dialog_get_ab_trans (request->td);
    xfer = gnc_xfer_dialog (gnc_ab_trans_dialog_get_parent (request->td), account);
    switch (request->type)
    {
    case SINGLE_DEBITNOTE:
        gnc_xfer_dialog_set_title (xfer, _("Online Banking Direct Debit Note"));
        gnc_xfer_dialog_lock_to_account_tree (xfer); break;
    case SINGLE_INTERNAL_TRANSFER:
        gnc_xfer_dialog_set_title (xfer, _("Online Banking Bank-Internal Transfer"));
        gnc_xfer_dialog_lock_from_account_tree (xfer); break;
    case SEPA_TRANSFER:
        gnc_xfer_dialog_set_title (xfer, _("Online Banking European (SEPA) Transfer"));
        gnc_xfer_dialog_lock_from_account_tree (xfer); break;
#if (AQBANKING_VERSION_INT >= 60400)
    case SEPA_INTERNAL_TRANSFER:
        gnc_xfer_dialog_set_title (xfer, _("Online Banking European (SEPA) Internal Transfer"));
        gnc_xfer_dialog_lock_from_account_tree (xfer); break;
#endif
    case SEPA_DEBITNOTE:
        gnc_xfer_dialog_set_title (xfer, _("Online Banking European (SEPA) Debit Note"));
        gnc_xfer_dialog_lock_to_account_tree (xfer); break;
    default:
        gnc_xfer_dialog_set_title (xfer, _("Online Banking Transaction"));
        gnc_xfer_dialog_lock_from_account_tree (xfer); break;
    }
    gnc_xfer_dialog_set_to_show_button_active (xfer, TRUE);
    gnc_xfer_dialog_set_amount (xfer, double_to_gnc_numeric (
        AB_Value_GetValueAsDouble (AB_Transaction_GetValue (ab_trans)),
        xaccAccountGetCommoditySCU (account), GNC_HOW_RND_ROUND_HALF_UP));
    gnc_xfer_dialog_set_amount_sensitive (xfer, FALSE);
    gnc_xfer_dialog_set_date_sensitive (xfer, FALSE);
    gchar *description = gnc_ab_description_to_gnc (ab_trans, FALSE);
    gchar *memo = gnc_ab_memo_to_gnc (ab_trans);
    gnc_xfer_dialog_set_description (xfer, description);
    gnc_xfer_dialog_set_memo (xfer, memo);
    g_free (description);
    g_free (memo);
    gnc_xfer_dialog_set_txn_cb (xfer, transfer_txn_created, request);
    gnc_xfer_dialog_run_async (xfer, transfer_xfer_completed, request);
    g_object_unref (parent);
}

static void
transfer_operation_acquired (guint token, gpointer user_data)
{
    TransferRequest *request = user_data;
    request->aq_operation = token;
    GtkWidget *parent = NULL;
    Account *gnc_acc = NULL;
    if (!transfer_request_current (request, &parent, &gnc_acc))
    {
        transfer_request_free (request);
        return;
    }
    GList *templates = NULL;
    request->api = gnc_AB_BANKING_new ();
    if (!request->api)
    {
        g_warning ("gnc_ab_maketrans: Couldn't get AqBanking API");
        g_object_unref (parent);
        transfer_request_free (request);
        return;
    }
    request->ab_acc = gnc_ab_get_ab_account (request->api, gnc_acc);
    if (!request->ab_acc)
    {
        gnc_error_dialog (GTK_WINDOW (parent), _("No valid online banking account assigned."));
        g_object_unref (parent);
        transfer_request_free (request);
        return;
    }
#if (AQBANKING_VERSION_INT >= 60400)
    if (request->type == SEPA_INTERNAL_TRANSFER)
    {
        templates = gnc_ab_trans_templ_list_new_from_ref_accounts (request->ab_acc);
        if (!templates)
        {
            gnc_error_dialog (GTK_WINDOW (parent), _("No reference accounts found."));
            g_object_unref (parent);
            transfer_request_free (request);
            return;
        }
    }
    else
#endif
        templates = gnc_ab_trans_templ_list_new_from_book (request->book);
    request->td = gnc_ab_trans_dialog_new (parent, request->ab_acc,
        xaccAccountGetCommoditySCU (gnc_acc), request->type, templates);
    if (!request->td)
    {
        g_list_free (templates);
        g_object_unref (parent);
        transfer_request_free (request);
        return;
    }
    GtkWidget *window = g_weak_ref_get (&request->parent);
    if (!window || gtk_widget_in_destruction (window))
    {
        g_clear_object (&window);
        g_object_unref (parent);
        transfer_request_free (request);
        return;
    }
    transfer_show_dialog (request);
    g_object_unref (window);
    g_object_unref (parent);
}

void
gnc_ab_maketrans (GtkWidget *parent, Account *gnc_acc,
                  GncABTransType trans_type)
{
    g_return_if_fail (parent && gnc_acc);
    QofBook *book = qof_instance_get_book (QOF_INSTANCE (gnc_acc));
    guint lease = gnc_gui_begin_session_operation (book);
    if (!lease)
        return;
    TransferRequest *request = g_new0 (TransferRequest, 1);
    request->book = g_object_ref (book);
    request->account_guid = *qof_instance_get_guid (QOF_INSTANCE (gnc_acc));
    request->lease = lease;
    request->type = trans_type;
    g_weak_ref_init (&request->parent, G_OBJECT (parent));
    request->parent_destroy_handler = g_signal_connect (parent, "destroy",
        G_CALLBACK (transfer_parent_destroyed), request);
    gnc_ab_operation_acquire_async (transfer_operation_acquired, request);
}
