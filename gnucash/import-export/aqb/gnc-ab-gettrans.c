/*
 * gnc-ab-gettrans.c --
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
 * @file gnc-ab-gettrans.c
 * @brief AqBanking get transactions functions
 * @author Copyright (C) 2002 Christian Stimming <stimming@tuhh.de>
 * @author Copyright (C) 2008 Andreas Koehler <andi5.py@gmx.net>
 */

#include <config.h>

#include "gnc-ab-utils.h"

#include <glib/gi18n.h>
#include <aqbanking/banking.h>
#include <aqbanking/types/transaction.h>
#include <aqbanking/types/imexporter_accountinfo.h>
#include <aqbanking/types/imexporter_context.h>
#include "Account.h"
#include "dialog-ab-daterange.h"
#include "dialog-sx-editor.h"
#include "gnc-ab-gettrans.h"
#include "gnc-ab-kvp.h"
#include "gnc-ab-standing-orders.h"
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
    GNC_AB_ACCOUNT_SPEC *ab_acc;
    GNC_AB_JOB *job;
    GNC_AB_JOB_LIST2 *job_list;
    GncGWENGui *gui;
    AB_IMEXPORTER_CONTEXT *context;
    GncABImExContextImport *ieci;
    gboolean no_transactions;
    time64 until;
} GetTransRequest;

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
} StandingOrdersRequest;

static void gettrans_matcher_completed (gboolean accepted,
                                        gpointer user_data);

static void
aqb_request_parent_destroyed (G_GNUC_UNUSED GtkWidget *parent,
                              gpointer user_data)
{
    *(gboolean *)user_data = TRUE;
}

static void
aqb_request_disconnect_parent (GWeakRef *weak_parent, gulong handler)
{
    GtkWidget *parent = g_weak_ref_get (weak_parent);
    if (parent && handler && g_signal_handler_is_connected (parent, handler))
        g_signal_handler_disconnect (parent, handler);
    g_clear_object (&parent);
}

static void
standing_orders_request_free (StandingOrdersRequest *request)
{
    if (request->context) AB_ImExporterContext_free (request->context);
    aqb_request_disconnect_parent (&request->parent,
                                   request->parent_destroy_handler);
    if (request->job_list) AB_Transaction_List2_free (request->job_list);
    if (request->job) AB_Transaction_free (request->job);
    if (request->api) gnc_AB_BANKING_fini (request->api);
    if (request->gui) gnc_GWEN_Gui_release (request->gui);
    if (request->aq_operation) gnc_ab_operation_release (request->aq_operation);
    gnc_gui_end_session_operation (request->session_lease);
    request->session_lease = 0;
    g_object_unref (request->book);
    g_weak_ref_clear (&request->parent);
    g_free (request);
}

static void
standing_orders_work (G_GNUC_UNUSED GncGWENGui *gui, gpointer user_data)
{
    StandingOrdersRequest *request = user_data;
    AB_Banking_SendCommands (request->api, request->job_list, request->context);
}

static void
standing_orders_completed (gpointer user_data)
{
    StandingOrdersRequest *request = user_data;
    GtkWindow *parent = g_weak_ref_get (&request->parent);
    Account *account = xaccAccountLookup (&request->account_guid, request->book);
    GNC_AB_JOB_STATUS status = AB_Transaction_GetStatus (request->job);
    if (parent && !request->parent_destroyed &&
        !gtk_widget_in_destruction (GTK_WIDGET (parent)) &&
        gnc_get_current_book () == request->book &&
        qof_book_is_open (request->book) && account)
    {
        if (status != AB_Transaction_StatusAccepted &&
            status != AB_Transaction_StatusPending)
            gnc_error_dialog (parent, _("Error on executing job.\n\nStatus: %s (%d)"),
                AB_Transaction_Status_toString (status), status);
        else
        {
            GncABStandingOrderSyncResult result =
                gnc_ab_import_standing_orders (request->context, account);
            const gchar *heading = result.received == 0
                ? _("The bank returned no standing orders.")
                : _("Standing order retrieval completed.");
            gnc_info_dialog (parent, _("%s\n\nReceived: %u\nCreated: %u\nUpdated: %u\nDisabled: %u\nSkipped: %u"),
                heading, result.received, result.created, result.updated,
                result.disabled, result.skipped);
            for (GList *node = result.to_edit; node; node = node->next)
                gnc_ui_scheduled_xaction_editor_dialog_create (parent,
                    GNC_SCHEDXACTION (node->data), FALSE);
            g_list_free (result.to_edit);
        }
    }
    g_clear_object (&parent);
    standing_orders_request_free (request);
}

static void
gettrans_request_free (GetTransRequest *request)
{
    if (request->ieci) gnc_ab_ieci_free (request->ieci);
    if (request->context) AB_ImExporterContext_free (request->context);
    aqb_request_disconnect_parent (&request->parent,
                                   request->parent_destroy_handler);
    if (request->job_list) AB_Transaction_List2_free (request->job_list);
    if (request->job) AB_Transaction_free (request->job);
    if (request->api) gnc_AB_BANKING_fini (request->api);
    if (request->gui) gnc_GWEN_Gui_release (request->gui);
    if (request->aq_operation) gnc_ab_operation_release (request->aq_operation);
    gnc_gui_end_session_operation (request->session_lease);
    request->session_lease = 0;
    g_object_unref (request->book);
    g_weak_ref_clear (&request->parent);
    g_free (request);
}

static gboolean
gettrans_request_current (GetTransRequest *request, GtkWindow **parent_out,
                          Account **account_out)
{
    GtkWindow *parent = g_weak_ref_get (&request->parent);
    Account *account = xaccAccountLookup (&request->account_guid, request->book);
    gboolean valid = parent && !request->parent_destroyed &&
        !gtk_widget_in_destruction (GTK_WIDGET (parent)) &&
        gnc_get_current_book () == request->book && qof_book_is_open (request->book) &&
        account != NULL;
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
gettrans_import_completed (GncABImExContextImport *ieci, gpointer user_data)
{
    GetTransRequest *request = user_data;
    GtkWindow *parent = NULL;
    Account *account = NULL;
    if (!ieci)
    {
        gettrans_request_free (request);
        return;
    }
    request->ieci = ieci;
    request->no_transactions =
        !(gnc_ab_ieci_get_found (ieci) & FOUND_TRANSACTIONS);
    if (gettrans_request_current (request, &parent, &account))
    {
        gnc_ab_set_account_trans_retrieval (account, request->until);
        g_object_unref (parent);
    }
    gnc_ab_ieci_run_matcher_async (ieci, gettrans_matcher_completed, request);
}

static void
gettrans_matcher_completed (G_GNUC_UNUSED gboolean accepted,
                            gpointer user_data)
{
    GetTransRequest *request = user_data;
    GtkWindow *parent = NULL;
    Account *account = NULL;
    if (request->no_transactions &&
        gettrans_request_current (request, &parent, &account))
    {
        GtkWidget *dialog = gtk_message_dialog_new (parent,
            GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT,
            GTK_MESSAGE_INFO, GTK_BUTTONS_OK, "%s",
            _("The Online Banking import returned no transactions for the selected time period."));
        g_signal_connect_swapped (dialog, "response",
                                  G_CALLBACK (gtk_widget_destroy), dialog);
        gtk_widget_show_all (dialog);
        g_object_unref (parent);
    }
    gnc_ab_ieci_free (request->ieci);
    request->ieci = NULL;
    gettrans_request_free (request);
}

static void
gettrans_job_completed (gpointer user_data)
{
    GetTransRequest *request = user_data;
    GtkWindow *parent = NULL;
    Account *account = NULL;
    GNC_AB_JOB_STATUS status = AB_Transaction_GetStatus (request->job);
    if (!gettrans_request_current (request, &parent, &account))
    {
        gettrans_request_free (request);
        return;
    }
    if (status != AB_Transaction_StatusAccepted &&
        status != AB_Transaction_StatusPending)
    {
        gnc_error_dialog (parent, _("Error on executing job.\n\nStatus: %s (%d)"),
                          AB_Transaction_Status_toString (status), status);
        g_object_unref (parent);
        gettrans_request_free (request);
        return;
    }
    gnc_ab_import_context_async (request->context, AWAIT_TRANSACTIONS, FALSE,
        NULL, GTK_WIDGET (parent), gettrans_import_completed, request);
    g_object_unref (parent);
}

static void
gettrans_job_work (G_GNUC_UNUSED GncGWENGui *gui, gpointer user_data)
{
    GetTransRequest *request = user_data;
    AB_Banking_SendCommands (request->api, request->job_list, request->context);
}

static void
gettrans_dates_selected (gboolean accepted, time64 from_date,
                         gboolean last_retrieval_date,
                         gboolean earliest_date, time64 to_date,
                         gboolean until_now, gpointer user_data)
{
    GetTransRequest *request = user_data;
    GtkWindow *parent = NULL;
    Account *account = NULL;
    GWEN_TIME *from = NULL, *to = NULL;

    if (!accepted || !gettrans_request_current (request, &parent, &account))
    {
        gettrans_request_free (request);
        return;
    }
    if (earliest_date)
        from = NULL;
    else
    {
        if (last_retrieval_date)
            from_date = gnc_ab_get_account_trans_retrieval (account);
        from = GWEN_Time_fromSeconds (from_date);
    }
    request->until = until_now ? gnc_time (NULL) : to_date;
    to = GWEN_Time_fromSeconds (request->until);

    if (!AB_AccountSpec_GetTransactionLimitsForCommand (
            request->ab_acc, AB_Transaction_CommandGetTransactions))
    {
        gnc_error_dialog (parent, _("Online action \"Get Transactions\" not available for this account."));
        g_clear_object (&parent);
        if (from) GWEN_Time_free (from);
        GWEN_Time_free (to);
        gettrans_request_free (request);
        return;
    }
    request->job = AB_Transaction_new ();
    AB_Transaction_SetCommand (request->job, AB_Transaction_CommandGetTransactions);
    AB_Transaction_SetUniqueAccountId (request->job,
                                       AB_AccountSpec_GetUniqueId (request->ab_acc));
    if (from)
    {
        GWEN_DATE *date = GWEN_Date_fromLocalTime (GWEN_Time_toTime_t (from));
        AB_Transaction_SetFirstDate (request->job, date);
        GWEN_Date_free (date);
        GWEN_Time_free (from);
    }
    GWEN_DATE *last_date = GWEN_Date_fromLocalTime (GWEN_Time_toTime_t (to));
    AB_Transaction_SetLastDate (request->job, last_date);
    GWEN_Date_free (last_date);
    GWEN_Time_free (to);
    request->job_list = AB_Transaction_List2_new ();
    AB_Transaction_List2_PushBack (request->job_list, request->job);
    request->gui = gnc_GWEN_Gui_get (GTK_WIDGET (parent));
    if (!request->gui)
    {
        g_warning ("gnc_ab_gettrans: Couldn't initialize Gwenhywfar GUI");
        g_object_unref (parent);
        gettrans_request_free (request);
        return;
    }
    request->context = AB_ImExporterContext_new ();
    g_object_unref (parent);
    gnc_GWEN_Gui_run_job_async (request->gui, gettrans_job_work,
                                gettrans_job_completed, request, NULL);
}

static void
gettrans_operation_acquired (guint token, gpointer user_data)
{
    GetTransRequest *request = user_data;
    request->aq_operation = token;
    GtkWindow *parent = NULL;
    Account *gnc_acc = NULL;
    if (!gettrans_request_current (request, &parent, &gnc_acc))
    {
        gettrans_request_free (request);
        return;
    }
    request->api = gnc_AB_BANKING_new ();
    if (!request->api)
    {
        g_warning ("gnc_ab_gettrans: Couldn't get AqBanking API");
        g_object_unref (parent);
        gettrans_request_free (request);
        return;
    }
    request->ab_acc = gnc_ab_get_ab_account (request->api, gnc_acc);
    if (!request->ab_acc)
    {
        gnc_error_dialog (GTK_WINDOW (parent), _("No valid online banking account assigned."));
        g_object_unref (parent);
        gettrans_request_free (request);
        return;
    }
    time64 last = gnc_ab_get_account_trans_retrieval (gnc_acc);
    gboolean last_known = last != 0;
    if (!last_known)
        last = gnc_time (NULL);
    gnc_ab_enter_daterange_async (GTK_WINDOW (parent), NULL, last,
        last_known, !last_known, gnc_time (NULL), TRUE,
        gettrans_dates_selected, request);
    g_object_unref (parent);
}

void
gnc_ab_gettrans (GtkWidget *parent, Account *gnc_acc)
{
    g_return_if_fail (parent && gnc_acc);
    QofBook *book = qof_instance_get_book (QOF_INSTANCE (gnc_acc));
    guint lease = gnc_gui_begin_session_operation (book);
    if (!lease)
        return;
    GetTransRequest *request = g_new0 (GetTransRequest, 1);
    request->book = g_object_ref (book);
    request->account_guid = *qof_instance_get_guid (QOF_INSTANCE (gnc_acc));
    request->session_lease = lease;
    g_weak_ref_init (&request->parent, G_OBJECT (parent));
    request->parent_destroy_handler = g_signal_connect (parent, "destroy",
        G_CALLBACK (aqb_request_parent_destroyed), &request->parent_destroyed);
    gnc_ab_operation_acquire_async (gettrans_operation_acquired, request);
}

static void
standing_orders_operation_acquired (guint token, gpointer user_data)
{
    StandingOrdersRequest *request = user_data;
    request->aq_operation = token;
    GtkWindow *parent = NULL;
    Account *gnc_acc = NULL;
    GNC_AB_ACCOUNT_SPEC *ab_acc;
    parent = GTK_WINDOW (g_weak_ref_get (&request->parent));
    gnc_acc = xaccAccountLookup (&request->account_guid, request->book);
    if (!parent || request->parent_destroyed ||
        gtk_widget_in_destruction (GTK_WIDGET (parent)) || !gnc_acc ||
        gnc_get_current_book () != request->book ||
        !qof_book_is_open (request->book))
    {
        g_clear_object (&parent);
        standing_orders_request_free (request);
        return;
    }
    request->api = gnc_AB_BANKING_new ();
    if (!request->api)
    {
        g_warning ("gnc_ab_getstandingorders: Couldn't get AqBanking API");
        g_object_unref (parent);
        standing_orders_request_free (request);
        return;
    }
    ab_acc = gnc_ab_get_ab_account (request->api, gnc_acc);
    if (!ab_acc)
    {
        gnc_error_dialog (GTK_WINDOW (parent), _("No valid online banking account assigned."));
        g_object_unref (parent);
        standing_orders_request_free (request);
        return;
    }
    if (!AB_AccountSpec_GetTransactionLimitsForCommand (
            ab_acc, AB_Transaction_CommandSepaGetStandingOrders))
    {
        gnc_error_dialog (GTK_WINDOW (parent),
            _("Online action \"Get Standing Orders\" not available for this account."));
        g_object_unref (parent);
        standing_orders_request_free (request);
        return;
    }
    request->job = AB_Transaction_new ();
    AB_Transaction_SetCommand (request->job, AB_Transaction_CommandSepaGetStandingOrders);
    AB_Transaction_SetUniqueAccountId (request->job, AB_AccountSpec_GetUniqueId (ab_acc));
    request->job_list = AB_Transaction_List2_new ();
    AB_Transaction_List2_PushBack (request->job_list, request->job);
    request->gui = gnc_GWEN_Gui_get (GTK_WIDGET (parent));
    if (!request->gui)
    {
        gnc_error_dialog (GTK_WINDOW (parent),
            _("Could not initialize the online banking user interface."));
        g_object_unref (parent);
        standing_orders_request_free (request);
        return;
    }
    request->context = AB_ImExporterContext_new ();
    g_object_unref (parent);
    gnc_GWEN_Gui_run_job_async (request->gui, standing_orders_work,
                                standing_orders_completed, request, NULL);
}

void
gnc_ab_getstandingorders (GtkWidget *parent, Account *gnc_acc)
{
    g_return_if_fail (parent && gnc_acc);
    QofBook *book = qof_instance_get_book (QOF_INSTANCE (gnc_acc));
    guint lease = gnc_gui_begin_session_operation (book);
    if (!lease) return;
    StandingOrdersRequest *request = g_new0 (StandingOrdersRequest, 1);
    request->book = g_object_ref (book);
    request->account_guid = *qof_instance_get_guid (QOF_INSTANCE (gnc_acc));
    request->session_lease = lease;
    g_weak_ref_init (&request->parent, G_OBJECT (parent));
    request->parent_destroy_handler = g_signal_connect (parent, "destroy",
        G_CALLBACK (aqb_request_parent_destroyed), &request->parent_destroyed);
    gnc_ab_operation_acquire_async (standing_orders_operation_acquired, request);
}
