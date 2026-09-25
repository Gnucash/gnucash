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
#include "qof.h"

/* This static indicates the debugging module that this .o belongs to.  */
G_GNUC_UNUSED static QofLogModule log_module = G_LOG_DOMAIN;

typedef struct
{
    AB_BANKING *api;
    GNC_AB_ACCOUNT_SPEC *ab_acc;
    Account *gnc_acc;
    QofBook *book;
    GncGUID book_guid;
    GncGUID account_guid;
    GWeakRef parent;
} GetTransData;

typedef struct
{
    GetTransData *data;
    time64 until;
} GetTransImportRequest;

static void
gettrans_data_free (GetTransData *data)
{
    if (!data)
        return;

    g_weak_ref_clear (&data->parent);
    if (data->api)
        gnc_AB_BANKING_fini (data->api);
    g_free (data);
}

static Account *
gettrans_data_get_account (const GetTransData *data)
{
    QofBook *book;
    Account *account;

    if (!data)
        return NULL;
    book = gnc_get_current_book ();
    if (!book || book != data->book ||
        !guid_equal (qof_instance_get_guid (QOF_INSTANCE (book)), &data->book_guid))
        return NULL;
    account = xaccAccountLookup (&data->account_guid, book);
    return account && !qof_instance_get_destroying (QOF_INSTANCE (account)) ?
           account : NULL;
}

static void
gettrans_show_no_transactions (GtkWidget *parent)
{
    GtkAlertDialog *dialog = gtk_alert_dialog_new (
        "%s", _("The Online Banking import returned no transactions "
                 "for the selected time period."));

    gtk_alert_dialog_show (dialog, GTK_WINDOW (parent));
    g_object_unref (dialog);
}

static void
gettrans_import_finished (GncABImExContextImport *ieci, gboolean completed,
                          gpointer user_data)
{
    GetTransImportRequest *request = user_data;
    GetTransData *data = request->data;
    GtkWidget *parent = g_weak_ref_get (&data->parent);
    Account *account = gettrans_data_get_account (data);

    if (completed && parent && account)
    {
        if (!(gnc_ab_ieci_get_found (ieci) & FOUND_TRANSACTIONS))
            gettrans_show_no_transactions (parent);
        gnc_ab_set_account_trans_retrieval (account, request->until);
    }
    g_clear_object (&parent);
    gettrans_data_free (data);
    g_free (request);
}

static gboolean
gettrans_execute (GetTransData *data, GtkWidget *parent,
                  const GncABDateRange *range)
{
    Account *account = gettrans_data_get_account (data);
    GWEN_TIME *from_date = NULL;
    GWEN_TIME *to_date = NULL;
    time64 last = range->from_date;
    time64 until = range->to_date;
    GNC_AB_JOB *job = NULL;
    GNC_AB_JOB_LIST2 *job_list = NULL;
    GncGWENGui *gui = NULL;
    AB_IMEXPORTER_CONTEXT *context = NULL;
    GNC_AB_JOB_STATUS job_status;
    gboolean pending = FALSE;

    if (!account)
        return FALSE;
    if (range->first_possible_date)
        from_date = NULL;
    else
    {
        if (range->last_retrieval_date)
            last = gnc_ab_get_account_trans_retrieval (account);
        from_date = GWEN_Time_fromSeconds (last);
    }

    if (range->to_now)
        until = gnc_time (NULL);
    to_date = GWEN_Time_fromSeconds (until);

    if (!AB_AccountSpec_GetTransactionLimitsForCommand (
            data->ab_acc, AB_Transaction_CommandGetTransactions))
    {
        g_warning ("gnc_ab_gettrans: JobGetTransactions not available for this "
                   "account");
        gnc_error_dialog (GTK_WINDOW (parent),
                          _("Online action \"Get Transactions\" not available "
                            "for this account."));
        goto cleanup;
    }

    job = AB_Transaction_new ();
    AB_Transaction_SetCommand (job, AB_Transaction_CommandGetTransactions);
    AB_Transaction_SetUniqueAccountId (job,
                                       AB_AccountSpec_GetUniqueId (data->ab_acc));

    if (from_date)
    {
        GWEN_DATE *date = GWEN_Date_fromLocalTime (GWEN_Time_toTime_t (from_date));
        AB_Transaction_SetFirstDate (job, date);
        GWEN_Date_free (date);
    }

    if (to_date)
    {
        GWEN_DATE *date = GWEN_Date_fromLocalTime (GWEN_Time_toTime_t (to_date));
        AB_Transaction_SetLastDate (job, date);
        GWEN_Date_free (date);
    }

    job_list = AB_Transaction_List2_new ();
    AB_Transaction_List2_PushBack (job_list, job);
    gui = gnc_GWEN_Gui_get (parent);
    if (!gui)
    {
        g_warning ("gnc_ab_gettrans: Couldn't initialize Gwenhywfar GUI");
        goto cleanup;
    }

    context = AB_ImExporterContext_new ();
    AB_Banking_SendCommands (data->api, job_list, context);
    job_status = AB_Transaction_GetStatus (job);
    if (job_status != AB_Transaction_StatusAccepted &&
        job_status != AB_Transaction_StatusPending)
    {
        g_warning ("gnc_ab_gettrans: Error on executing job");
        gnc_error_dialog (GTK_WINDOW (parent),
                          _("Error on executing job.\n\nStatus: %s (%d)"),
                          AB_Transaction_Status_toString (job_status), job_status);
        goto cleanup;
    }

    {
        GetTransImportRequest *request = g_new0 (GetTransImportRequest, 1);
        request->data = data;
        request->until = until;
        gnc_ab_import_context_async (context, AWAIT_TRANSACTIONS, FALSE, NULL,
                                     parent, NULL, gettrans_import_finished, request);
        context = NULL;
        pending = TRUE;
    }

cleanup:
    if (context)
        AB_ImExporterContext_free (context);
    if (gui)
        gnc_GWEN_Gui_release (gui);
    if (job_list)
        AB_Transaction_List2_free (job_list);
    if (job)
        AB_Transaction_free (job);
    if (to_date)
        GWEN_Time_free (to_date);
    if (from_date)
        GWEN_Time_free (from_date);
    return pending;
}
static void
gettrans_dates_finished (GObject *source, GAsyncResult *result,
                         gpointer user_data)
{
    GetTransData *data = user_data;
    GncABDateRange range;
    GError *error = NULL;
    GtkWidget *parent;

    (void)source;
    if (!gnc_ab_enter_daterange_finish (result, &range, &error))
    {
        if (error && !g_error_matches (error, G_IO_ERROR, G_IO_ERROR_CANCELLED))
            g_warning ("AqBanking date range: %s", error->message);
        g_clear_error (&error);
        gettrans_data_free (data);
        return;
    }

    parent = g_weak_ref_get (&data->parent);
    if (!parent)
    {
        gettrans_data_free (data);
        return;
    }

    if (!gettrans_execute (data, parent, &range))
        gettrans_data_free (data);
    g_object_unref (parent);
}

void
gnc_ab_gettrans (GtkWidget *parent, Account *gnc_acc)
{
    AB_BANKING *api;
    GNC_AB_ACCOUNT_SPEC *ab_acc;
    GncABDateRange initial;
    GetTransData *data;

    g_return_if_fail (parent && gnc_acc);

    api = gnc_AB_BANKING_new ();
    if (!api)
    {
        g_warning ("gnc_ab_gettrans: Couldn't get AqBanking API");
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

    initial.from_date = gnc_ab_get_account_trans_retrieval (gnc_acc);
    initial.last_retrieval_date = initial.from_date != 0;
    if (!initial.last_retrieval_date)
        initial.from_date = gnc_time (NULL);
    initial.first_possible_date = TRUE;
    initial.to_date = gnc_time (NULL);
    initial.to_now = TRUE;

    data = g_new0 (GetTransData, 1);
    data->api = api;
    data->ab_acc = ab_acc;
    data->gnc_acc = gnc_acc;
    data->book = gnc_account_get_book (gnc_acc);
    data->book_guid = *qof_instance_get_guid (QOF_INSTANCE (data->book));
    data->account_guid = *xaccAccountGetGUID (gnc_acc);
    g_weak_ref_init (&data->parent, parent);
    gnc_ab_enter_daterange_async (parent, NULL, &initial, NULL,
                                  gettrans_dates_finished, data);
}
void
gnc_ab_getstandingorders(GtkWidget *parent, Account *gnc_acc)
{
    AB_BANKING *api;
    GNC_AB_ACCOUNT_SPEC *ab_acc;
    GNC_AB_JOB *job = NULL;
    GNC_AB_JOB_LIST2 *job_list = NULL;
    GncGWENGui *gui = NULL;
    AB_IMEXPORTER_CONTEXT *context = NULL;
    GNC_AB_JOB_STATUS job_status;
    GList *node;
    GncABStandingOrderSyncResult sync_result;
    const gchar *summary_heading;
    g_return_if_fail(parent && gnc_acc);

    api = gnc_AB_BANKING_new();
    if (!api)
    {
        g_warning("gnc_ab_getstandingorders: Couldn't get AqBanking API");
        return;
    }

    ab_acc = gnc_ab_get_ab_account(api, gnc_acc);
    if (!ab_acc)
    {
        g_warning("gnc_ab_getstandingorders: No AqBanking account found");
        gnc_error_dialog (GTK_WINDOW (parent), _("No valid online banking account assigned."));
        goto cleanup;
    }

    if (!AB_AccountSpec_GetTransactionLimitsForCommand(
            ab_acc, AB_Transaction_CommandSepaGetStandingOrders))
    {
        g_warning("gnc_ab_getstandingorders: JobSepaGetStandingOrders not available for this account");
        gnc_error_dialog (
            GTK_WINDOW (parent),
            _("Online action \"Get Standing Orders\" not available for this account."));
        goto cleanup;
    }

    job = AB_Transaction_new();
    AB_Transaction_SetCommand(job, AB_Transaction_CommandSepaGetStandingOrders);
    AB_Transaction_SetUniqueAccountId(job, AB_AccountSpec_GetUniqueId(ab_acc));

    job_list = AB_Transaction_List2_new();
    AB_Transaction_List2_PushBack(job_list, job);

    gui = gnc_GWEN_Gui_get(parent);
    if (!gui)
    {
        g_warning("gnc_ab_getstandingorders: Couldn't initialize Gwenhywfar GUI");
        gnc_error_dialog (GTK_WINDOW (parent),
                          _("Could not initialize the online banking user interface."));
        goto cleanup;
    }

    context = AB_ImExporterContext_new();
    AB_Banking_SendCommands(api, job_list, context);

    job_status = AB_Transaction_GetStatus(job);
    if (job_status != AB_Transaction_StatusAccepted
            && job_status != AB_Transaction_StatusPending)
    {
        g_warning("gnc_ab_getstandingorders: Error on executing job");
        gnc_error_dialog (GTK_WINDOW (parent),
                          _("Error on executing job.\n\nStatus: %s (%d)"),
                          AB_Transaction_Status_toString(job_status),
                          job_status);
        goto cleanup;
    }

    sync_result = gnc_ab_import_standing_orders (context, gnc_acc);
    summary_heading = sync_result.received == 0
        ? _("The bank returned no standing orders.")
        : _("Standing order retrieval completed.");
    gnc_info_dialog (
        GTK_WINDOW (parent),
        _("%s\n\n"
          "Received: %u\n"
          "Created: %u\n"
          "Updated: %u\n"
          "Disabled: %u\n"
          "Skipped: %u"),
        summary_heading,
        sync_result.received,
        sync_result.created,
        sync_result.updated,
        sync_result.disabled,
        sync_result.skipped);

    for (node = sync_result.to_edit; node; node = node->next)
        gnc_ui_scheduled_xaction_editor_dialog_create (
            GTK_WINDOW (parent), GNC_SCHEDXACTION (node->data), FALSE);
    g_list_free (sync_result.to_edit);

cleanup:
    if (context)
        AB_ImExporterContext_free(context);
    if (gui)
        gnc_GWEN_Gui_release(gui);
    if (job_list)
        AB_Transaction_List2_free(job_list);
    if (job)
        AB_Transaction_free(job);
    gnc_AB_BANKING_fini(api);
}
