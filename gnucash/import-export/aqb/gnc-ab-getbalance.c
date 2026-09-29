/*
 * gnc-ab-getbalance.c --
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
 * @file gnc-ab-getbalance.c
 * @brief AqBanking getbalance functions
 * @author Copyright (C) 2002 Christian Stimming <stimming@tuhh.de>
 * @author Copyright (C) 2008 Andreas Koehler <andi5.py@gmx.net>
 */

#include <config.h>

#include "gnc-ab-utils.h"

#include <glib/gi18n.h>
#include <aqbanking/banking.h>
# include <aqbanking/types/transaction.h>

#include "gnc-ab-getbalance.h"
#include "gnc-ab-kvp.h"
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
    guint session_lease;
    guint aq_operation;
    AB_BANKING *api;
    GNC_AB_JOB *job;
    GNC_AB_JOB_LIST2 *job_list;
    GncGWENGui *gui;
    AB_IMEXPORTER_CONTEXT *context;
    GncABImExContextImport *ieci;
} GetBalanceRequest;

static void get_balance_matcher_completed (gboolean accepted,
                                           gpointer user_data);

static void
get_balance_parent_destroyed (G_GNUC_UNUSED GtkWidget *parent,
                              gpointer user_data)
{
    ((GetBalanceRequest *)user_data)->parent_destroyed = TRUE;
}

static gboolean
get_balance_request_is_current (GetBalanceRequest *request, GtkWindow **parent_out,
                                Account **account_out)
{
    GtkWindow *parent = g_weak_ref_get (&request->parent);
    Account *account = xaccAccountLookup (&request->account_guid, request->book);
    gboolean valid = parent && !request->parent_destroyed &&
        !gtk_widget_in_destruction (GTK_WIDGET (parent)) &&
        gnc_get_current_book () == request->book && qof_book_is_open (request->book) &&
        account && qof_instance_get_book (QOF_INSTANCE (account)) == request->book;
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
get_balance_request_free (gpointer user_data)
{
    GetBalanceRequest *request = user_data;
    if (request->ieci)
        gnc_ab_ieci_free (request->ieci);
    if (request->context)
        AB_ImExporterContext_free (request->context);
    if (request->job_list)
        AB_Transaction_List2_free (request->job_list);
    if (request->job)
        AB_Transaction_free (request->job);
    if (request->api)
        gnc_AB_BANKING_fini (request->api);
    if (request->gui)
        gnc_GWEN_Gui_release (request->gui);
    if (request->aq_operation)
        gnc_ab_operation_release (request->aq_operation);
    gnc_gui_end_session_operation (request->session_lease);
    request->session_lease = 0;
    GtkWindow *parent = g_weak_ref_get (&request->parent);
    if (parent && request->parent_destroy_handler &&
        g_signal_handler_is_connected (parent, request->parent_destroy_handler))
        g_signal_handler_disconnect (parent, request->parent_destroy_handler);
    g_clear_object (&parent);
    g_object_unref (request->book);
    g_weak_ref_clear (&request->parent);
    g_free (request);
}

static void
get_balance_import_completed (GncABImExContextImport *ieci, gpointer user_data)
{
    GetBalanceRequest *request = user_data;
    if (!ieci)
    {
        get_balance_request_free (request);
        return;
    }
    request->ieci = ieci;
    gnc_ab_ieci_run_matcher_async (ieci, get_balance_matcher_completed,
                                   request);
}

static void
get_balance_matcher_completed (G_GNUC_UNUSED gboolean accepted,
                               gpointer user_data)
{
    GetBalanceRequest *request = user_data;
    gnc_ab_ieci_free (request->ieci);
    request->ieci = NULL;
    get_balance_request_free (request);
}

static void
get_balance_job_completed (gpointer user_data)
{
    GetBalanceRequest *request = user_data;
    GNC_AB_JOB_STATUS status = AB_Transaction_GetStatus (request->job);
    GtkWindow *parent = NULL;
    Account *account = NULL;

    if (!get_balance_request_is_current (request, &parent, &account))
    {
        get_balance_request_free (request);
        return;
    }
    if (status != AB_Transaction_StatusEnqueued &&
        status != AB_Transaction_StatusPending &&
        status != AB_Transaction_StatusAccepted)
    {
        gnc_error_dialog (parent, _("Error on executing job.\n\nStatus: %s"),
                          AB_Transaction_Status_toString (status));
        g_object_unref (parent);
        get_balance_request_free (request);
        return;
    }
    gnc_ab_import_context_async (request->context, AWAIT_BALANCES, FALSE, NULL,
        GTK_WIDGET (parent),
        get_balance_import_completed, request);
    g_object_unref (parent);
}

static void
get_balance_job_work (G_GNUC_UNUSED GncGWENGui *gui, gpointer user_data)
{
    GetBalanceRequest *request = user_data;
    AB_Banking_SendCommands (request->api, request->job_list, request->context);
}

static void
get_balance_operation_acquired (guint token, gpointer user_data)
{
    GetBalanceRequest *request = user_data;
    request->aq_operation = token;
    GtkWindow *parent = NULL;
    Account *gnc_acc = NULL;
    GNC_AB_ACCOUNT_SPEC *ab_acc;
    if (!get_balance_request_is_current (request, &parent, &gnc_acc))
    {
        get_balance_request_free (request);
        return;
    }

    /* Get the API */
    request->api = gnc_AB_BANKING_new();
    if (!request->api)
    {
        g_warning("gnc_ab_gettrans: Couldn't get AqBanking API");
        g_object_unref (parent);
        get_balance_request_free (request);
        return;
    }

    /* Get the AqBanking Account */
    ab_acc = gnc_ab_get_ab_account (request->api, gnc_acc);
    if (!ab_acc)
    {
        g_warning("gnc_ab_getbalance: No AqBanking account found");
        gnc_error_dialog (GTK_WINDOW (parent), _("No valid online banking account assigned."));
        g_object_unref (parent);
        get_balance_request_free (request);
        return;
    }

    /* Get a GetBalance job and enqueue it */
    if (!AB_AccountSpec_GetTransactionLimitsForCommand(ab_acc, AB_Transaction_CommandGetBalance))
    {
        g_warning("gnc_ab_getbalance: JobGetBalance not available for this "
                  "account");
        gnc_error_dialog (GTK_WINDOW (parent), _("Online action \"Get Balance\" not available for this account."));
        g_object_unref (parent);
        get_balance_request_free (request);
        return;
    }
    request->job = AB_Transaction_new();
    AB_Transaction_SetCommand(request->job, AB_Transaction_CommandGetBalance);
    AB_Transaction_SetUniqueAccountId(request->job, AB_AccountSpec_GetUniqueId(ab_acc));

    request->job_list = AB_Transaction_List2_new();
    AB_Transaction_List2_PushBack(request->job_list, request->job);
    /* Get a GUI object */
    request->gui = gnc_GWEN_Gui_get(GTK_WIDGET (parent));
    if (!request->gui)
    {
        g_warning("gnc_ab_getbalance: Couldn't initialize Gwenhywfar GUI");
        g_object_unref (parent);
        get_balance_request_free (request);
        return;
    }

    /* Create a context to store the results */
    request->context = AB_ImExporterContext_new();
    g_object_unref (parent);
    gnc_GWEN_Gui_run_job_async (request->gui, get_balance_job_work,
                                get_balance_job_completed, request, NULL);
}

void
gnc_ab_getbalance (GtkWidget *parent, Account *gnc_acc)
{
    g_return_if_fail(parent && gnc_acc);
    QofBook *book = qof_instance_get_book (QOF_INSTANCE (gnc_acc));
    guint session_lease = gnc_gui_begin_session_operation (book);
    if (!session_lease)
        return;
    GetBalanceRequest *request = g_new0 (GetBalanceRequest, 1);
    request->session_lease = session_lease;
    request->book = g_object_ref (book);
    request->account_guid = *qof_instance_get_guid (QOF_INSTANCE (gnc_acc));
    g_weak_ref_init (&request->parent, G_OBJECT (parent));
    request->parent_destroy_handler = g_signal_connect (parent, "destroy",
        G_CALLBACK (get_balance_parent_destroyed), request);
    gnc_ab_operation_acquire_async (get_balance_operation_acquired, request);
}
