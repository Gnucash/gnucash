/********************************************************************\
 * window-reconcile.c -- the reconcile window                       *
 * Copyright (C) 1997 Robin D. Clark                                *
 * Copyright (C) 1998-2000 Linas Vepstas                            *
 * Copyright (C) 2002 Christian Stimming                            *
 * Copyright (C) 2006 David Hampton                                 *
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
 *                                                                  *
 *   Author: Rob Clark                                              *
 * Internet: rclark@cs.hmc.edu                                      *
 *  Address: 609 8th Street                                         *
 *           Huntington Beach, CA 92648-4632                        *
\********************************************************************/

#include <config.h>

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#ifdef __G_IR_SCANNER__
#undef __G_IR_SCANNER__
#endif

#include <algorithm>

#include "Account.hpp"
#include "Scrub.h"
#include "Scrub3.h"
#include "dialog-account.h"
#include "dialog-transfer.h"
#include "dialog-utils.h"
#include "gnc-amount-edit.h"
#include "gnc-autoclear.h"
#include "gnc-component-manager.h"
#include "gnc-date.h"
#include "gnc-date-edit.h"
#include "gnc-event.h"
#include "gnc-filepath-utils.h"
#include "gnc-gnome-utils.h"
#include "gnc-gtk-utils.h"
//#include "gnc-main-window.h"
#include "gnc-plugin-page-register.h"
#include "gnc-prefs.h"
#include "gnc-scrub-job-runner.h"
#include "gnc-ui.h"
#include "gnc-ui-balances.h"
#include "gnc-window.h"
#include "reconcile-view.h"
#include "window-reconcile.h"
#include "gnc-session.h"

#define WINDOW_RECONCILE_CM_CLASS "window-reconcile"
#define GNC_PREF_AUTO_CC_PAYMENT        "auto-cc-payment"
#define GNC_PREF_ALWAYS_REC_TO_TODAY    "always-reconcile-to-today"


/** STRUCTS *********************************************************/
typedef struct _startRecnWindowData startRecnWindowData;
typedef struct _RecnConfirmationRequest RecnConfirmationRequest;
typedef void (*StartRecnAcceptedFunc) (startRecnWindowData *data,
                                       gnc_numeric ending,
                                       time64 statement_date);

struct _RecnWindow
{
    GncGUID account;             /* The account that we are reconciling  */
    gnc_numeric new_ending;      /* The new ending balance               */
    time64 statement_date;       /* The statement date                   */

    gint component_id;           /* id of component                      */

    GtkWidget *window;           /* The reconcile window                 */

    GtkBuilder *builder;         /* The builder object */
    GSimpleActionGroup *simple_action_group; /* The action group for the window */
    GtkWidget *autoclear_button;

    GncPluginPage *page;

    SplitsVec autoclear_splits;
    SplitsVec initially_cleared_splits;

    GtkWidget *starting;         /* The starting balance                 */
    GtkWidget *ending;           /* The ending balance                   */
    GtkWidget *recn_date;        /* The statement date                   */
    GtkWidget *reconciled;       /* The reconciled balance               */
    GtkWidget *difference;       /* Text field, amount left to reconcile */

    GtkWidget *total_debit;      /* Text field, total debit reconciled   */
    GtkWidget *total_credit;     /* Text field, total credit reconciled  */

    GtkWidget *debit;            /* Debit matrix show unreconciled debit */
    GtkWidget *credit;           /* Credit matrix, shows credits...      */

    GtkWidget *debit_frame;      /* Frame around debit matrix            */
    GtkWidget *credit_frame;     /* Frame around credit matrix           */

    gboolean   delete_refresh;   /* do a refresh upon a window deletion  */
    startRecnWindowData *start_dialog; /* outstanding Change Information dialog */
    RecnConfirmationRequest *confirmation; /* outstanding finish/cancel/postpone */
};

typedef enum
{
    RECN_CONFIRM_CANCEL,
    RECN_CONFIRM_FINISH,
    RECN_CONFIRM_POSTPONE,
    RECN_CONFIRM_DELETE,
} RecnConfirmationKind;

/* The request owns no RecnWindow or engine objects. The callback resolves
 * the window, active book, and account GUID again before it can mutate a
 * reconciliation view. */
struct _RecnConfirmationRequest
{
    GWeakRef window;
    QofBook *book;
    GncGUID account;
    GncGUID split;
    GncGUID transaction;
    RecnConfirmationKind kind;
};



/* State for a non-blocking Reconcile Information dialog.  The account is
 * deliberately represented by GUID plus book identity: widgets may outlive
 * account and book mutations while a response is pending.
 */
struct _startRecnWindowData
{
    gatomicrefcount ref_count;
    GncGUID       account;
    QofBook      *book;
    GNCAccountType account_type;

    GtkWidget     *startRecnWindow;
    GtkWidget     *xfer_button;
    GtkWidget     *date_value;
    GtkWidget     *future_icon;
    GtkWidget     *future_text;
    GNCAmountEdit *end_value;
    gnc_numeric    original_value;
    gboolean       user_set_value;

    XferDialog    *xferData;
    gboolean       include_children;
    time64         date;

    GWeakRef       parent;
    gulong         parent_destroy_handler;
    gboolean       completed;
    RecnWindow    *change_owner;
    StartRecnAcceptedFunc accepted;
};

/** PROTOTYPES ******************************************************/
static gnc_numeric recnRecalculateBalance (RecnWindow *recnData);

static void   recn_destroy_cb (GtkWidget *w, gpointer data);
static void   recn_cancel (RecnWindow *recnData);
static void   recn_confirm (RecnWindow *recnData, RecnConfirmationKind kind,
                            Split *split, const char *message);
static void   recn_finish (RecnWindow *recnData);
static void   recn_postpone (RecnWindow *recnData);
static void   recn_delete_transaction (RecnWindow *recnData,
                                        const GncGUID *split_guid,
                                        const GncGUID *transaction_guid);
static gboolean recn_close_request_cb (GtkWindow *window, gpointer data);
static gboolean recn_escape_shortcut_cb (GtkWidget *widget, GVariant *args,
                                         gpointer data);
static void   recnAutoClearCB (GSimpleAction *simple, GVariant *parameter, gpointer user_data);
static void   recnFinishCB (GSimpleAction *simple, GVariant *parameter, gpointer user_data);
static void   recnPostponeCB (GSimpleAction *simple, GVariant *parameter, gpointer user_data);
static void   recnCancelCB (GSimpleAction *simple, GVariant *parameter, gpointer user_data);

extern "C" {
void gnc_start_recn_children_changed (GtkWidget *widget, startRecnWindowData *data);
void gnc_start_recn_interest_clicked_cb (GtkButton *button, startRecnWindowData *data);
}

static void   gnc_reconcile_window_set_sensitivity (RecnWindow *recnData);
static char * gnc_recn_make_window_name (Account *account);
static void   gnc_recn_set_window_name (RecnWindow *recnData);
static gboolean find_by_account (gpointer find_data, gpointer user_data);

/** GLOBALS ************************************************************/
/* This static indicates the debugging module that this .o belongs to. */
G_GNUC_UNUSED static QofLogModule log_module = GNC_MOD_GUI;

static time64 gnc_reconcile_last_statement_date = 0;
static GHashTable *start_recn_pending;

static Account *
start_recn_get_account (const startRecnWindowData *data)
{
    if (!data || !data->book || data->book != gnc_get_current_book ())
        return NULL;

    return xaccAccountLookup (&data->account, data->book);
}

static void
start_recn_pending_remove (startRecnWindowData *data)
{
    if (start_recn_pending && data)
        g_hash_table_remove (start_recn_pending, &data->account);
}

static void
start_recn_clear_change_owner (startRecnWindowData *data)
{
    if (!data)
        return;

    if (data->change_owner && data->change_owner->start_dialog == data)
        data->change_owner->start_dialog = NULL;
    data->change_owner = NULL;
}

static void
start_recn_disconnect_parent (startRecnWindowData *data)
{
    auto parent = data ? GTK_WINDOW (g_weak_ref_get (&data->parent)) : NULL;

    if (parent && data->parent_destroy_handler)
        g_signal_handler_disconnect (parent, data->parent_destroy_handler);
    if (data)
        data->parent_destroy_handler = 0;
    g_clear_object (&parent);
}

static void
start_recn_destroy_dialog (startRecnWindowData *data)
{
    auto dialog = data ? g_steal_pointer (&data->startRecnWindow) : NULL;

    if (!dialog)
        return;

    g_signal_handlers_disconnect_by_data (dialog, data);
    gtk_window_destroy (GTK_WINDOW (dialog));
    g_object_unref (dialog);
}

static void
start_recn_unref (startRecnWindowData *data)
{
    if (!data || !g_atomic_ref_count_dec (&data->ref_count))
        return;

    start_recn_pending_remove (data);
    start_recn_clear_change_owner (data);
    start_recn_disconnect_parent (data);
    start_recn_destroy_dialog (data);
    g_weak_ref_clear (&data->parent);
    g_free (data);
}

/* A transfer dialog still runs a nested loop. Keep the start context alive
 * across it: the parent can be destroyed while that loop is dispatching. */
static startRecnWindowData *
start_recn_ref (startRecnWindowData *data)
{
    if (data)
        g_atomic_ref_count_inc (&data->ref_count);
    return data;
}

static void
start_recn_dialog_destroyed_cb (GtkWidget *dialog, startRecnWindowData *data)
{
    if (!data || data->startRecnWindow != dialog)
        return;

    data->startRecnWindow = NULL;
    data->completed = TRUE;
    g_object_unref (dialog);
    start_recn_unref (data);
}

static void
start_recn_dialog_cancel (startRecnWindowData *data)
{
    if (!data || data->completed)
        return;

    data->completed = TRUE;
    if (data->xferData)
    {
        auto xfer = data->xferData;
        data->xferData = NULL;
        gnc_xfer_dialog_close (xfer);
    }
    start_recn_unref (data);
}

static void
start_recn_parent_destroyed_cb (GtkWidget *parent, startRecnWindowData *data)
{
    (void)parent;
    start_recn_dialog_cancel (data);
}

static void
start_recn_dialog_present (startRecnWindowData *data)
{
    if (data && !data->completed && data->startRecnWindow)
        gtk_window_present (GTK_WINDOW (data->startRecnWindow));
}

static startRecnWindowData *
start_recn_find_pending (Account *account)
{
    startRecnWindowData *data;

    if (!account || !start_recn_pending)
        return NULL;

    data = static_cast<startRecnWindowData *> (
        g_hash_table_lookup (start_recn_pending, xaccAccountGetGUID (account)));
    if (!data)
        return NULL;

    if (data->completed || data->book != gnc_get_current_book ())
    {
        start_recn_dialog_cancel (data);
        return NULL;
    }

    return data;
}

static void
start_recn_pending_add (startRecnWindowData *data)
{
    auto key = g_new (GncGUID, 1);

    if (!start_recn_pending)
        start_recn_pending = g_hash_table_new_full (guid_hash_to_guint,
                                                    guid_g_hash_table_equal,
                                                    g_free, NULL);
    *key = data->account;
    g_hash_table_insert (start_recn_pending, key, data);
}

/** IMPLEMENTATIONS *************************************************/

static gpointer
commodity_compare(Account *account, gpointer user_data) {
  gboolean equal = gnc_commodity_equiv (xaccAccountGetCommodity (account),
                                        (gnc_commodity*) user_data);

  return equal ? NULL : account;
}


/********************************************************************\
 * has_account_different_commodities                                *
 *                                                                  *
 * Args:   parent account - the account to look in                  *
 * Return: true if there exists a subaccount with different         *
 *    commodity then the parent account.                            *
\********************************************************************/
static gboolean
has_account_different_commodities(const Account *account)
{
    gnc_commodity *parent_commodity;
    gpointer result;

    if (account == NULL)
        return FALSE;

    parent_commodity = xaccAccountGetCommodity (account);

    result = gnc_account_foreach_descendant_until (account,
                                                   commodity_compare,
                                                   parent_commodity);

    return result != NULL;
}

static const char*
get_autoclear_icon (GError* error)
{
    static std::unordered_map<gint,const char*> icon_names =
    {
        { Autoclear::ABORT_NONE, "media-playback-start" },
        { Autoclear::ABORT_NOP, "media-playback-stop" },
        { Autoclear::ABORT_MULTI, "dialog-information" },
        { Autoclear::ABORT_TIMEOUT, "dialog-error" },
        { Autoclear::ABORT_UNREACHABLE, "dialog-error" },
    };
    auto it = icon_names.find (error ? error->code : Autoclear::ABORT_NONE);
    return it == icon_names.end() ? "dialog-information" : it->second;
}

#define GNC_PREF_ENABLE_AUTOCLEAR "enable-autoclear-in-reconcile"

static void
calculate_autoclear (RecnWindow *recnData)
{
    g_return_if_fail (recnData);

    bool enabled = gnc_prefs_get_bool (GNC_PREFS_GROUP_RECONCILE, GNC_PREF_ENABLE_AUTOCLEAR);
    auto action = g_action_map_lookup_action (G_ACTION_MAP(recnData->simple_action_group),
                                              "RecnAutoClearAction");
    g_simple_action_set_enabled (G_SIMPLE_ACTION(action), enabled);
    gtk_widget_set_visible (recnData->autoclear_button, enabled);
    if (!enabled)
        return;

    GError* error = nullptr;
    Account* acct = xaccAccountLookup (&recnData->account, gnc_get_current_book ());

    static const unsigned int MAX_AUTOCLEAR_SECONDS = 1;

    GList *splits_to_clear = gnc_account_get_autoclear_splits
        (acct, recnData->new_ending, recnData->statement_date, &error, MAX_AUTOCLEAR_SECONDS);

    gtk_widget_set_sensitive (recnData->autoclear_button, error == nullptr);

    gtk_button_set_icon_name (GTK_BUTTON (recnData->autoclear_button),
                              get_autoclear_icon (error));

    recnData->autoclear_splits = recnData->initially_cleared_splits;
    for (auto n = splits_to_clear; n; n = n->next)
        recnData->autoclear_splits.push_back (GNC_SPLIT (n->data));

    if (error)
    {
        gtk_widget_set_tooltip_text (recnData->autoclear_button, _(error->message));
        g_error_free (error);
        return;
    }

    auto num_splits = g_list_length (splits_to_clear);
    char date_buff[MAX_DATE_LENGTH+1];
    qof_print_date_buff (date_buff, MAX_DATE_LENGTH, recnData->statement_date);
    char* tooltip = g_strdup_printf
        (ngettext("Automatically select %u transaction up to %s that clears to %s",
                  "Automatically select %u transactions up to %s that clear to %s",
                  num_splits),
         num_splits, date_buff,
         xaccPrintAmount (recnData->new_ending, gnc_account_print_info (acct, true)));
    gtk_widget_set_tooltip_text (recnData->autoclear_button, tooltip);

    g_free (tooltip);
    g_list_free (splits_to_clear);
}

/********************************************************************\
 * recnRefresh                                                      *
 *   refreshes the transactions in the reconcile window             *
 *                                                                  *
 * Args:   account - the account of the reconcile window to refresh *
 * Return: none                                                     *
\********************************************************************/
static void
recnRefresh (RecnWindow *recnData)
{
    if (recnData == NULL)
        return;

    gnc_reconcile_view_refresh(GNC_RECONCILE_VIEW(recnData->debit));
    gnc_reconcile_view_refresh(GNC_RECONCILE_VIEW(recnData->credit));

    gnc_reconcile_window_set_sensitivity(recnData);

    gnc_recn_set_window_name(recnData);

    recnRecalculateBalance(recnData);

    gtk_widget_queue_resize(recnData->window);
}


static Account *
recn_get_account (RecnWindow *recnData)
{
    if (!recnData)
        return NULL;

    return xaccAccountLookup (&recnData->account, gnc_get_current_book ());
}
typedef enum
{
    RECN_REGISTER_REVEAL_SPLIT,
    RECN_REGISTER_REVEAL_SPLIT_AMOUNT,
} RecnRegisterRevealKind;

typedef struct
{
    GWeakRef reconcile_window;
    GWeakRef page;
    GWeakRef page_window;
    QofBook *book;
    GncGUID account_guid;
    GncGUID split_guid;
    GncGUID transaction_guid;
    RecnRegisterRevealKind kind;
} RecnRegisterRevealRequest;

static void
recn_register_reveal_request_free (gpointer user_data)
{
    auto request = static_cast<RecnRegisterRevealRequest *> (user_data);

    g_weak_ref_clear (&request->page_window);
    g_weak_ref_clear (&request->page);
    g_weak_ref_clear (&request->reconcile_window);
    g_free (request);
}

static void
recn_register_reveal_finished (GNCSplitReg *gsr, Split *split,
                               GncSplitRegRevealResult result,
                               gpointer user_data)
{
    auto request = static_cast<RecnRegisterRevealRequest *> (user_data);
    GtkWindow *reconcile_window =
        GTK_WINDOW (g_weak_ref_get (&request->reconcile_window));
    GncPluginPage *page = GNC_PLUGIN_PAGE (g_weak_ref_get (&request->page));
    GtkWindow *page_window = GTK_WINDOW (g_weak_ref_get (&request->page_window));
    Account *account;
    Transaction *transaction;
    RecnWindow *recnData;

    if (!reconcile_window || !page || !page_window ||
        !GTK_IS_WINDOW (reconcile_window) ||
        !GNC_IS_PLUGIN_PAGE_REGISTER (page) ||
        !GTK_IS_WINDOW (page_window) ||
        request->book != gnc_get_current_book () ||
        qof_book_shutting_down (request->book))
        goto out;

    account = xaccAccountLookup (&request->account_guid, request->book);
    recnData = account ? static_cast<RecnWindow *> (
        gnc_find_first_gui_component (WINDOW_RECONCILE_CM_CLASS,
                                      find_by_account, account)) : nullptr;
    transaction = xaccTransLookup (&request->transaction_guid, request->book);
    if (!account || !recnData || recnData->window != GTK_WIDGET (reconcile_window) ||
        recnData->page != page ||
        !guid_equal (&recnData->account, &request->account_guid) ||
        recn_get_account (recnData) != account ||
        gnc_plugin_page_get_window (page) !=
            GTK_WIDGET (page_window) ||
        gnc_plugin_page_register_get_gsr (page) != gsr ||
        xaccSplitLookup (&request->split_guid, request->book) != split ||
        !transaction || xaccSplitGetParent (split) != transaction)
        goto out;

    if (result == GNC_SPLIT_REG_REVEAL_FILTER_CLEARED)
        gnc_plugin_page_register_clear_current_filter (page);
    if (request->kind == RECN_REGISTER_REVEAL_SPLIT_AMOUNT)
        gnc_split_reg_jump_to_split_amount (gsr, split);
    else
        gnc_split_reg_jump_to_split (gsr, split);

out:
    g_clear_object (&page_window);
    g_clear_object (&page);
    g_clear_object (&reconcile_window);
}

static void
recn_register_reveal_split_async (RecnWindow *recnData, GNCSplitReg *gsr,
                                  Split *split, RecnRegisterRevealKind kind)
{
    RecnRegisterRevealRequest *request;
    Account *account;
    Transaction *transaction;
    GtkWidget *page_window;

    if (!recnData || !recnData->window || !recnData->page || !gsr || !split ||
        !(account = recn_get_account (recnData)) ||
        !(transaction = xaccSplitGetParent (split)) ||
        !(page_window = gnc_plugin_page_get_window (recnData->page)))
        return;

    request = g_new0 (RecnRegisterRevealRequest, 1);
    request->book = gnc_get_current_book ();
    request->account_guid = *xaccAccountGetGUID (account);
    request->split_guid = *xaccSplitGetGUID (split);
    request->transaction_guid = *xaccTransGetGUID (transaction);
    request->kind = kind;
    g_weak_ref_init (&request->reconcile_window, G_OBJECT (recnData->window));
    g_weak_ref_init (&request->page, G_OBJECT (recnData->page));
    g_weak_ref_init (&request->page_window, G_OBJECT (page_window));
    gnc_split_reg_reveal_split_async (gsr, split, recn_register_reveal_finished,
                                      request, recn_register_reveal_request_free);
}


static void
gnc_add_colorized_amount (gpointer obj, gnc_numeric amt,
                          GNCPrintAmountInfo print_info, gboolean reverse)
{
    if (!obj) return;
    if (reverse) amt = gnc_numeric_neg (amt);
    gnc_set_label_color (GTK_WIDGET (obj), amt);
    gtk_label_set_text (GTK_LABEL (obj), xaccPrintAmount (amt, print_info));
}

/********************************************************************\
 * recnRecalculateBalance                                           *
 *   refreshes the balances in the reconcile window                 *
 *                                                                  *
 * Args:   recnData -- the reconcile window to refresh              *
 * Return: the difference between the nominal ending balance        *
 *         and the 'effective' ending balance.                      *
\********************************************************************/
static gnc_numeric
recnRecalculateBalance (RecnWindow *recnData)
{
    Account *account;
    gnc_numeric debit;
    gnc_numeric credit;
    gnc_numeric starting;
    gnc_numeric ending;
    gnc_numeric reconciled;
    gnc_numeric diff;
    gchar *datestr;
    GNCPrintAmountInfo print_info;
    gboolean reverse_balance, include_children;
    GAction *action;

    account = recn_get_account (recnData);
    if (!account)
        return gnc_numeric_zero ();

    reverse_balance = gnc_reverse_balance(account);
    include_children = xaccAccountGetReconcileChildrenStatus(account);
    starting = gnc_ui_account_get_reconciled_balance(account, include_children);
    print_info = gnc_account_print_info (account, TRUE);

    ending = recnData->new_ending;
    debit = gnc_reconcile_view_reconciled_balance
            (GNC_RECONCILE_VIEW(recnData->debit));
    credit = gnc_reconcile_view_reconciled_balance
             (GNC_RECONCILE_VIEW(recnData->credit));

    reconciled = gnc_numeric_sub_fixed (debit, credit);
    if (reverse_balance)
        reconciled = gnc_numeric_sub_fixed (reconciled, starting);
    else
        reconciled = gnc_numeric_add_fixed (reconciled, starting);

    diff = gnc_numeric_sub_fixed (ending, reconciled);

    datestr = qof_print_date (recnData->statement_date);
    gtk_label_set_text (GTK_LABEL(recnData->recn_date), datestr);
    g_free (datestr);

    gnc_add_colorized_amount (recnData->starting, starting, print_info, FALSE);
    gnc_add_colorized_amount (recnData->ending, ending, print_info, reverse_balance);
    gnc_add_colorized_amount (recnData->total_debit, debit, print_info, FALSE);
    gnc_add_colorized_amount (recnData->total_credit, credit, print_info, FALSE);
    gnc_add_colorized_amount (recnData->reconciled, reconciled, print_info, reverse_balance);
    gnc_add_colorized_amount (recnData->difference, diff, print_info, reverse_balance);

    action = g_action_map_lookup_action (G_ACTION_MAP(recnData->simple_action_group),
                                         "RecnFinishAction");
    g_simple_action_set_enabled (G_SIMPLE_ACTION(action), gnc_numeric_zero_p (diff));

    action = g_action_map_lookup_action (G_ACTION_MAP(recnData->simple_action_group),
                                         "TransBalanceAction");
    g_simple_action_set_enabled (G_SIMPLE_ACTION(action), !gnc_numeric_zero_p (diff));

    calculate_autoclear (recnData);

    return diff;
}

/* amount_edit_cb
 *   Callback on activate event for statement Ending Balance.
 *   Sets the user_set_value flag true if the amount entered is
 *   different to the calculated Ending Balance as at the default
 *   Statement Date. This prevents the entered Ending Balance
 *   being recalculated if the Statement Date is changed.
 *
 * Args:   widget         - Ending Balance widget
 *         data           - structure containing info about this
 *                          reconciliation process.
 */
static void
amount_edit_cb(GtkWidget *widget, startRecnWindowData *data)
{
    gnc_numeric value;
    (void)widget;
    gint result = gnc_amount_edit_expr_is_valid (GNC_AMOUNT_EDIT(data->end_value),
                                                 &value, TRUE, NULL);

    data->user_set_value = FALSE;

    if (result < 1) // OK
    {
        if (result == -1) // blank entry is valid
        {
            gnc_amount_edit_set_amount (GNC_AMOUNT_EDIT(data->end_value), value);
            gnc_amount_edit_select_region (GNC_AMOUNT_EDIT(data->end_value), 0, -1);
        }
        data->user_set_value = !gnc_numeric_equal (value, data->original_value);
    }
}

/* amount_edit_focus_leave_cb
 *   Callback on focus-out event for statement Ending Balance.
 *
 * Args:   controller     - focus controller for the Ending Balance entry
 *         data           - structure containing info about this
 *                          reconciliation process.
 */
static void
amount_edit_focus_leave_cb (GtkEventControllerFocus *controller,
                            startRecnWindowData *data)
{
    auto widget = gtk_event_controller_get_widget (GTK_EVENT_CONTROLLER (controller));

    amount_edit_cb (widget, data);
}


/* recn_date_changed_cb
 *   Callback on date_changed event for Statement Date.
 *   If the user changed the date edit widget, and the Ending
 *   Balance wasn't entered, update the Ending Balance to reflect
 *   the ending balance of the account as at Statement Date.
 *
 * Args:   widget         - Statement Date edit widget
 *         data           - structure containing info about this
 *                          reconciliation.
 * Returns:  none.
 */
static void
recn_date_changed_cb (GtkWidget *widget, startRecnWindowData *data)
{
    GNCDateEdit *gde = GNC_DATE_EDIT (widget);
    gnc_numeric new_balance;
    time64 new_date;

    gboolean show_warning = FALSE;
    gint days_after_today;
    static const time64 secs_per_day = 86400;
    static const time64 secs_per_hour = 3600;

    new_date = gnc_date_edit_get_date_end (gde);

    /* Add secs_per_hour to the difference to compensate for the short
     * day when transitioning from standard to daylight time.
     */
    days_after_today = (gnc_time64_get_day_end (new_date) -
                        gnc_time64_get_today_end () +
                        secs_per_hour) / secs_per_day;

    if (days_after_today > 0)
    {
        gchar *str = g_strdup_printf
            /* Translators: %d is the number of days in the future */
            (ngettext ("Statement Date is %d day after today.",
                       "Statement Date is %d days after today.",
                       days_after_today),
             days_after_today);

        gchar *tip_start = g_strdup_printf
            /* Translators: %d is the number of days in the future */
            (ngettext ("The statement date you have chosen is %d day in the future.",
                       "The statement date you have chosen is %d days in the future.",
                       days_after_today),
             days_after_today);

        gchar *tip_end = g_strdup (_("This may cause issues for future reconciliation \
actions on this account. Please double-check this is the date you intended."));
        gchar *tip = g_strdup_printf ("%s %s", tip_start, tip_end);

        show_warning = TRUE;

        gtk_label_set_text (GTK_LABEL(data->future_text), str);
        gtk_widget_set_tooltip_text (GTK_WIDGET(data->future_text), tip);
        g_free (str);
        g_free (tip_end);
        g_free (tip_start);
        g_free (tip);
    }
    gtk_widget_set_visible (GTK_WIDGET(data->future_icon), show_warning);
    gtk_widget_set_visible (GTK_WIDGET(data->future_text), show_warning);

    data->date = new_date;

    if (data->user_set_value)
        return;

    /* Resolve the account only while the original book is still active. */
    auto account = start_recn_get_account (data);
    if (!account)
        return;

    /* get the balance for the account as of the new date */
    new_balance = gnc_ui_account_get_balance_as_of_date (account, new_date,
                   data->include_children);
    /* update the amount edit with the amount */
    gnc_amount_edit_set_amount (GNC_AMOUNT_EDIT (data->end_value),
                                new_balance);
    data->original_value = new_balance;
}


void
gnc_start_recn_children_changed (GtkWidget *widget, startRecnWindowData *data)
{
    data->include_children =
        gtk_check_button_get_active(GTK_CHECK_BUTTON(widget));

    /* Force an update of the ending balance */
    recn_date_changed_cb (data->date_value, data);
}


/********************************************************************\
 * recnInterestXferWindow                                           *
 *                                                                  *
 * Opens the interest transfer dialog for an already-owned start     *
 * dialog context. The account is resolved from the GUID at use time.*
\********************************************************************/
static char *
gnc_recn_make_interest_window_name (Account *account, const char *text)
{
    auto fullname = gnc_account_get_full_name (account);
    auto title = g_strconcat (fullname, " - ", text && *text ? _(text) : "", NULL);

    g_free (fullname);
    return title;
}

static void
recn_interest_xfer_finished (gboolean completed, gpointer user_data)
{
    auto data = static_cast<startRecnWindowData *> (user_data);
    Account *account;
    GtkWidget *entry;
    gnc_numeric before;
    gnc_numeric after;

    if (!data)
        return;

    data->xferData = NULL;
    if (data->completed)
    {
        start_recn_unref (data);
        return;
    }

    if (!completed)
    {
        if (data->xfer_button)
            gtk_widget_set_sensitive (data->xfer_button, TRUE);
        start_recn_unref (data);
        return;
    }

    account = start_recn_get_account (data);
    if (!account)
    {
        start_recn_unref (data);
        return;
    }

    entry = gnc_amount_edit_gtk_entry (data->end_value);
    before = gnc_amount_edit_get_amount (data->end_value);
    after = xaccAccountGetBalanceAsOfDate (account, data->date);
    if (gnc_numeric_compare (before, after))
    {
        if (gnc_reverse_balance (account))
            after = gnc_numeric_neg (after);

        gnc_amount_edit_set_amount (data->end_value, after);
        gtk_widget_grab_focus (entry);
        gtk_editable_select_region (GTK_EDITABLE (entry), 0, -1);
        data->original_value = after;
        data->user_set_value = FALSE;
    }

    start_recn_unref (data);
}

static gboolean
recnInterestXferWindow (startRecnWindowData *data)
{
    Account *account = start_recn_get_account (data);
    gchar *title;

    if (!account || !account_type_has_auto_interest_xfer (data->account_type))
        return FALSE;

    data->xferData = gnc_xfer_dialog (data->startRecnWindow, account);
    if (!data->xferData)
        return FALSE;

    if (account_type_has_auto_interest_payment (data->account_type))
        title = gnc_recn_make_interest_window_name (account,
                                                     _("Interest Payment"));
    else
        title = gnc_recn_make_interest_window_name (account,
                                                     _("Interest Charge"));
    gnc_xfer_dialog_set_title (data->xferData, title);
    g_free (title);

    gnc_xfer_dialog_set_information_label (data->xferData,
                                           _("Payment Information"));
    if (account_type_has_auto_interest_payment (data->account_type))
    {
        gnc_xfer_dialog_set_from_account_label (data->xferData,
                                                 _("Payment From"));
        gnc_xfer_dialog_set_from_show_button_active (data->xferData, TRUE);
        gnc_xfer_dialog_set_to_account_label (data->xferData,
                                               _("Reconcile Account"));
        gnc_xfer_dialog_select_to_account (data->xferData, account);
        gnc_xfer_dialog_lock_to_account_tree (data->xferData);
        gnc_xfer_dialog_quickfill_to_account (data->xferData, TRUE);
    }
    else
    {
        gnc_xfer_dialog_set_from_account_label (data->xferData,
                                                 _("Reconcile Account"));
        gnc_xfer_dialog_select_from_account (data->xferData, account);
        gnc_xfer_dialog_lock_from_account_tree (data->xferData);
        gnc_xfer_dialog_set_to_account_label (data->xferData,
                                               _("Payment To"));
        gnc_xfer_dialog_set_to_show_button_active (data->xferData, TRUE);
        gnc_xfer_dialog_quickfill_to_account (data->xferData, FALSE);
    }

    gnc_xfer_dialog_toggle_currency_table (data->xferData, FALSE);
    gnc_xfer_dialog_set_date (data->xferData, data->date);
    gnc_xfer_dialog_run_async (data->xferData, recn_interest_xfer_finished,
                               data);
    return TRUE;
}

static void
gnc_reconcile_interest_xfer_run (startRecnWindowData *data)
{
    data = start_recn_ref (data);
    if (!data || data->completed)
    {
        start_recn_unref (data);
        return;
    }

    if (!recnInterestXferWindow (data))
    {
        if (data->xfer_button)
            gtk_widget_set_sensitive (data->xfer_button, TRUE);
        start_recn_unref (data);
    }
}

void
gnc_start_recn_interest_clicked_cb (GtkButton *button, startRecnWindowData *data)
{
    (void)button;
    if (data->xfer_button)
        gtk_widget_set_sensitive (data->xfer_button, FALSE);
    gnc_reconcile_interest_xfer_run (data);
}

static void
gnc_save_reconcile_interval(Account *account, time64 statement_date)
{
    time64 prev_statement_date;
    int days = 0, months = 0;

    if (!xaccAccountGetReconcileLastDate (account, &prev_statement_date))
        return;

    /*
     * Compute the number of days difference.
     */
    auto seconds = statement_date - prev_statement_date;
    days = seconds / 60 / 60 / 24;

    /*
     * See if we need to remember days(weeks) or months.  The only trick
     * value is 28 days which could be either 4 weeks or 1 month.
     */
    if (days == 28)
    {
        int prev_days = 0, prev_months = 1;

        /* What was it last time? */
        xaccAccountGetReconcileLastInterval (account, &prev_months, &prev_days);
        if (prev_months == 1)
        {
            months = 1;
            days = 0;
        }
    }
    else if (days > 28)
    {
        struct tm current, prev;

        gnc_localtime_r (&statement_date, &current);
        gnc_localtime_r (&prev_statement_date, &prev);
        months = ((12 * current.tm_year + current.tm_mon) -
                  (12 * prev.tm_year + prev.tm_mon));
        days = 0;
    }

    /*
     * Remember for next time unless it is negative.
     */
    if (months >= 0 && days >= 0)
        xaccAccountSetReconcileLastInterval(account, months, days);
}


/********************************************************************\
 * Reconcile Information dialog                                     *
 *                                                                  *
 * The prompt is event driven. All state is heap-owned for the life *
 * of the dialog; successful answers are handed to a short callback.*
\********************************************************************/
static void
start_recn_dialog_finish (startRecnWindowData *data, gboolean accepted,
                          gnc_numeric ending, time64 statement_date)
{
    auto accepted_cb = data ? data->accepted : NULL;

    if (!data || data->completed)
        return;

    data->completed = TRUE;
    start_recn_pending_remove (data);
    data->accepted = NULL;
    if (accepted && accepted_cb)
        accepted_cb (data, ending, statement_date);
    start_recn_unref (data);
}

static void
start_recn_dialog_accept_cb (G_GNUC_UNUSED GtkButton *button,
                             startRecnWindowData *data)
{
    Account *account;
    gnc_numeric ending;
    time64 statement_date;

    if (!data || data->completed)
        return;

    account = start_recn_get_account (data);
    if (!account)
    {
        start_recn_dialog_cancel (data);
        return;
    }

    statement_date = gnc_date_edit_get_date_end (GNC_DATE_EDIT (data->date_value));
    if (statement_date != data->date)
        recn_date_changed_cb (data->date_value, data);

    /* Keep the dialog open on a malformed amount exactly as the old retry
     * loop did, but without starting a nested main loop. */
    if (!gnc_amount_edit_evaluate (data->end_value, NULL))
        return;

    ending = gnc_amount_edit_get_amount (data->end_value);
    if (gnc_reverse_balance (account))
        ending = gnc_numeric_neg (ending);

    xaccAccountSetReconcileChildrenStatus (account, data->include_children);
    gnc_save_reconcile_interval (account, statement_date);
    start_recn_dialog_finish (data, TRUE, ending, statement_date);
}

static void
start_recn_dialog_cancel_cb (G_GNUC_UNUSED GtkButton *button,
                             startRecnWindowData *data)
{
    start_recn_dialog_cancel (data);
}

static gboolean
start_recn_dialog_close_request_cb (GtkWindow *window, startRecnWindowData *data)
{
    if (!data || data->completed || data->startRecnWindow != GTK_WIDGET (window))
        return FALSE;

    start_recn_dialog_cancel (data);
    return TRUE;
}

static gboolean
start_recn_dialog_key_pressed_cb (G_GNUC_UNUSED GtkEventControllerKey *key,
                                  guint keyval, G_GNUC_UNUSED guint keycode,
                                  G_GNUC_UNUSED GdkModifierType state,
                                  startRecnWindowData *data)
{
    if (keyval != GDK_KEY_Escape)
        return FALSE;

    start_recn_dialog_cancel (data);
    return TRUE;
}

static void
start_recn_initial_accepted (startRecnWindowData *data, gnc_numeric ending,
                             time64 statement_date)
{
    Account *account = start_recn_get_account (data);
    auto parent = static_cast<GtkWidget *> (g_weak_ref_get (&data->parent));

    if (account)
    {
        auto recnData = recnWindowWithBalance (parent, account, ending,
                                               statement_date);
        gnc_ui_reconcile_window_raise (recnData);
    }
    g_clear_object (&parent);
}

static void
start_recn_change_accepted (startRecnWindowData *data, gnc_numeric ending,
                            time64 statement_date)
{
    auto recnData = data ? data->change_owner : NULL;

    if (!recnData || recnData->start_dialog != data)
        return;

    recnData->start_dialog = NULL;
    recnData->new_ending = ending;
    recnData->statement_date = statement_date;
    recnRecalculateBalance (recnData);
}

static void
start_recn_dialog_open (GtkWidget *parent, Account *account,
                        gnc_numeric initial_ending, time64 statement_date,
                        gboolean enable_subaccount,
                        StartRecnAcceptedFunc accepted,
                        RecnWindow *change_owner)
{
    GtkBuilder *builder;
    GtkWidget *dialog;
    GtkWidget *start_value;
    GtkWidget *include_children_button;
    GtkWidget *date_value;
    GtkWidget *end_value;
    GtkWidget *box;
    GtkWidget *label;
    GtkWidget *entry;
    GtkWidget *interest;
    GNCPrintAmountInfo print_info;
    gnc_numeric starting;
    gboolean auto_interest_xfer_option;
    auto data = g_new0 (startRecnWindowData, 1);

    if (!account || !accepted)
    {
        g_free (data);
        return;
    }

    g_atomic_ref_count_init (&data->ref_count);
    data->account = *xaccAccountGetGUID (account);
    data->book = gnc_get_current_book ();
    if (!data->book)
    {
        g_free (data);
        return;
    }
    data->account_type = xaccAccountGetType (account);
    data->date = statement_date;
    data->include_children =
        !has_account_different_commodities (account) &&
        xaccAccountGetReconcileChildrenStatus (account);
    data->original_value = initial_ending;
    data->accepted = accepted;

    auto_interest_xfer_option = xaccAccountGetAutoInterest (account);
    starting = gnc_ui_account_get_reconciled_balance (account,
                                                       data->include_children);
    print_info = gnc_account_print_info (account, TRUE);

    builder = gtk_builder_new ();
    gnc_builder_add_from_file (builder, "window-reconcile.glade",
                               "reconcile_start_dialog");
    dialog = GTK_WIDGET (gtk_builder_get_object (builder,
                                                  "reconcile_start_dialog"));
    if (!dialog)
    {
        g_object_unref (builder);
        g_free (data);
        return;
    }

    data->startRecnWindow = GTK_WIDGET (g_object_ref (dialog));
    g_weak_ref_init (&data->parent, NULL);
    if (parent && GTK_IS_WINDOW (parent))
    {
        gtk_window_set_transient_for (GTK_WINDOW (dialog), GTK_WINDOW (parent));
        g_weak_ref_set (&data->parent, G_OBJECT (parent));
        data->parent_destroy_handler =
            g_signal_connect (parent, "destroy",
                              G_CALLBACK (start_recn_parent_destroyed_cb), data);
    }
    gtk_window_set_modal (GTK_WINDOW (dialog), TRUE);
    gtk_widget_set_name (dialog, "gnc-id-reconcile-start");

    auto title = gnc_recn_make_window_name (account);
    gtk_window_set_title (GTK_WINDOW (dialog), title);
    g_free (title);

    start_value = GTK_WIDGET (gtk_builder_get_object (builder, "start_value"));
    gtk_label_set_text (GTK_LABEL (start_value),
                        xaccPrintAmount (starting, print_info));

    include_children_button = GTK_WIDGET (gtk_builder_get_object (
        builder, "subaccount_check"));
    gtk_check_button_set_active (GTK_CHECK_BUTTON (include_children_button),
                                  data->include_children);
    gtk_widget_set_sensitive (include_children_button, enable_subaccount);

    date_value = gnc_date_edit_new (statement_date, FALSE, FALSE);
    data->date_value = date_value;
    box = GTK_WIDGET (gtk_builder_get_object (builder, "date_value_box"));
    gnc_box_append_full (GTK_BOX (box), date_value, TRUE, TRUE, 0);
    label = GTK_WIDGET (gtk_builder_get_object (builder, "date_label"));
    gnc_date_make_mnemonic_target (GNC_DATE_EDIT (date_value), label);

    end_value = gnc_amount_edit_new ();
    data->end_value = GNC_AMOUNT_EDIT (end_value);
    box = GTK_WIDGET (gtk_builder_get_object (builder, "ending_value_box"));
    gnc_box_append_full (GTK_BOX (box), end_value, TRUE, TRUE, 0);
    label = GTK_WIDGET (gtk_builder_get_object (builder, "end_label"));
    gnc_amount_edit_make_mnemonic_target (data->end_value, label);

    data->future_icon = GTK_WIDGET (gtk_builder_get_object (builder,
                                                             "future_icon"));
    data->future_text = GTK_WIDGET (gtk_builder_get_object (builder,
                                                             "future_text"));
    gtk_widget_set_visible (data->future_text, FALSE);
    gtk_widget_set_visible (data->future_icon, FALSE);

    gnc_builder_connect_signals_full (builder, gnc_builder_connect_full_func,
                                      data);
    gnc_date_activates_default (GNC_DATE_EDIT (date_value), TRUE);
    g_signal_connect (date_value, "date_changed",
                      G_CALLBACK (recn_date_changed_cb), data);

    print_info.use_symbol = 0;
    gnc_amount_edit_set_print_info (data->end_value, print_info);
    gnc_amount_edit_set_fraction (data->end_value,
                                  xaccAccountGetCommoditySCU (account));
    gnc_amount_edit_set_amount (data->end_value, initial_ending);

    entry = gnc_amount_edit_gtk_entry (data->end_value);
    gtk_editable_select_region (GTK_EDITABLE (entry), 0, -1);
    auto amount_focus_controller = gtk_event_controller_focus_new ();
    g_signal_connect (amount_focus_controller, "leave",
                      G_CALLBACK (amount_edit_focus_leave_cb), data);
    gtk_widget_add_controller (entry, amount_focus_controller);
    g_signal_connect (entry, "activate", G_CALLBACK (amount_edit_cb), data);
    gtk_entry_set_activates_default (GTK_ENTRY (entry), TRUE);

    interest = GTK_WIDGET (gtk_builder_get_object (builder, "interest_button"));
    if (account_type_has_auto_interest_payment (data->account_type))
        gtk_button_set_label (GTK_BUTTON (interest),
                              _("Enter _Interest Payment…"));
    else if (account_type_has_auto_interest_charge (data->account_type))
        gtk_button_set_label (GTK_BUTTON (interest),
                              _("Enter _Interest Charge…"));
    else
    {
        gtk_widget_unparent (interest);
        interest = NULL;
    }
    if (interest)
    {
        data->xfer_button = interest;
        if (auto_interest_xfer_option)
            gtk_widget_set_sensitive (interest, FALSE);
    }

    if (change_owner)
    {
        data->change_owner = change_owner;
        change_owner->start_dialog = data;
    }

    auto ok_button = GTK_WIDGET (gtk_builder_get_object (builder, "okbutton1"));
    auto cancel_button = GTK_WIDGET (gtk_builder_get_object (builder, "cancelbutton1"));
    auto key_controller = gtk_event_controller_key_new ();

    gtk_window_set_default_widget (GTK_WINDOW (dialog), ok_button);
    g_signal_connect (ok_button, "clicked", G_CALLBACK (start_recn_dialog_accept_cb), data);
    g_signal_connect (cancel_button, "clicked", G_CALLBACK (start_recn_dialog_cancel_cb), data);
    g_signal_connect (dialog, "close-request",
                      G_CALLBACK (start_recn_dialog_close_request_cb), data);
    g_signal_connect (dialog, "destroy",
                      G_CALLBACK (start_recn_dialog_destroyed_cb), data);
    g_signal_connect (key_controller, "key-pressed",
                      G_CALLBACK (start_recn_dialog_key_pressed_cb), data);
    gtk_widget_add_controller (dialog, key_controller);
    start_recn_pending_add (data);
    g_object_unref (builder);
    start_recn_dialog_present (data);
    gtk_widget_grab_focus (entry);

    if (account_type_has_auto_interest_xfer (data->account_type) &&
        auto_interest_xfer_option)
        gnc_reconcile_interest_xfer_run (data);
}

static void
gnc_reconcile_window_set_sensitivity(RecnWindow *recnData)
{
    gboolean sensitive = FALSE;
    GNCReconcileView *view;
    GAction *action;

    view = GNC_RECONCILE_VIEW(recnData->debit);
    if (gnc_reconcile_view_num_selected(view) == 1)
        sensitive = TRUE;

    view = GNC_RECONCILE_VIEW(recnData->credit);
    if (gnc_reconcile_view_num_selected(view) == 1)
        sensitive = TRUE;

    action = g_action_map_lookup_action (G_ACTION_MAP(recnData->simple_action_group),
                                         "TransEditAction");
    g_simple_action_set_enabled (G_SIMPLE_ACTION(action), sensitive);

    action = g_action_map_lookup_action (G_ACTION_MAP(recnData->simple_action_group),
                                         "TransDeleteAction");
    g_simple_action_set_enabled (G_SIMPLE_ACTION(action), sensitive);

    sensitive = FALSE;

    view = GNC_RECONCILE_VIEW(recnData->debit);
    if (gnc_reconcile_view_num_selected(view) > 0)
        sensitive = TRUE;

    view = GNC_RECONCILE_VIEW(recnData->credit);
    if (gnc_reconcile_view_num_selected(view) > 0)
        sensitive = TRUE;

    action = g_action_map_lookup_action (G_ACTION_MAP(recnData->simple_action_group),
                                         "TransRecAction");
    g_simple_action_set_enabled (G_SIMPLE_ACTION(action), sensitive);

    action = g_action_map_lookup_action (G_ACTION_MAP(recnData->simple_action_group),
                                         "TransUnRecAction");
    g_simple_action_set_enabled (G_SIMPLE_ACTION(action), sensitive);
}


static void
gnc_reconcile_window_toggled_cb(GNCReconcileView *view, Split *split,
                                gpointer data)
{
    auto recnData = static_cast<RecnWindow*>(data);
    gnc_reconcile_window_set_sensitivity(recnData);
    recnRecalculateBalance(recnData);
}


static void
gnc_reconcile_window_row_cb(GNCReconcileView *view, gpointer item,
                            gpointer data)
{
    auto recnData = static_cast<RecnWindow*>(data);
    gnc_reconcile_window_set_sensitivity(recnData);
}


/** Popup a contextual menu.  This function ends up being called when
 *  the user right-clicks in the context of a window, or uses the
 *  keyboard context-menu request key combination (Shift-F10 by
 *  default).
 *
 *  @param recnData This is a data structure describing the
 *  Reconciliation Window.
 *
 *  @param event The event parameter passed to the "button-press"
 *  callback.  May be null if there was no event (aka keyboard
 *  request).
 */
static void
reconcile_popup_closed (GtkPopover *popup, gpointer user_data)
{
    gtk_widget_unparent (GTK_WIDGET (popup));
    (void)user_data;
}

static void
do_popup_menu (RecnWindow *recnData, GtkWidget *relative_to, double x, double y)
{
    GMenuModel *menu_model = (GMenuModel *)gtk_builder_get_object (recnData->builder,
                                                                    "recwin-popup");
    GtkWidget *menu;
    GdkRectangle point = { (int)x, (int)y, 1, 1 };

    if (!menu_model || !relative_to)
        return;
    menu = gtk_popover_menu_new_from_model (menu_model);
    gtk_widget_set_parent (menu, relative_to);
    gtk_popover_set_pointing_to (GTK_POPOVER (menu), &point);
    g_signal_connect (menu, "closed", G_CALLBACK (reconcile_popup_closed), NULL);
    gtk_popover_popup (GTK_POPOVER (menu));
}

static void
gnc_reconcile_window_context_pressed_cb (GtkGestureClick *gesture, int n_press,
                                          double x, double y, gpointer user_data)
{
    RecnWindow *recnData = static_cast<RecnWindow *> (user_data);
    GtkWidget *widget = gtk_event_controller_get_widget (GTK_EVENT_CONTROLLER (gesture));

    if (n_press != 1 || gtk_gesture_single_get_current_button (GTK_GESTURE_SINGLE (gesture)) !=
                        GDK_BUTTON_SECONDARY)
        return;
    gnc_query_view_select_at_point (GNC_QUERY_VIEW (widget), x, y);
    do_popup_menu (recnData, widget, x, y);
    gtk_gesture_set_state (GTK_GESTURE (gesture), GTK_EVENT_SEQUENCE_CLAIMED);
}

static void
gnc_reconcile_window_focus_enter_cb (GtkEventControllerFocus *controller,
                                     gpointer user_data)
{
    RecnWindow *recnData = static_cast<RecnWindow *> (user_data);
    GNCReconcileView *this_view = GNC_RECONCILE_VIEW
        (gtk_event_controller_get_widget (GTK_EVENT_CONTROLLER (controller)));
    GNCReconcileView *other_view = this_view == GNC_RECONCILE_VIEW (recnData->debit) ?
        GNC_RECONCILE_VIEW (recnData->credit) : GNC_RECONCILE_VIEW (recnData->debit);

    gnc_reconcile_view_unselect_all (other_view);
}

static gboolean
gnc_reconcile_key_pressed_cb (GtkEventControllerKey *controller, guint keyval,
                               guint keycode, GdkModifierType state,
                               gpointer user_data)
{
    RecnWindow *recnData = static_cast<RecnWindow *> (user_data);
    GtkWidget *this_view = gtk_event_controller_get_widget (GTK_EVENT_CONTROLLER (controller));
    GtkWidget *other_view;

    if (keyval == GDK_KEY_Menu || (keyval == GDK_KEY_F10 && (state & GDK_SHIFT_MASK)))
    {
        do_popup_menu (recnData, this_view, gtk_widget_get_width (this_view) / 2.0,
                       gtk_widget_get_height (this_view) / 2.0);
        return TRUE;
    }
    if (keyval != GDK_KEY_Tab && keyval != GDK_KEY_ISO_Left_Tab)
        return FALSE;

    other_view = this_view == recnData->debit ? recnData->credit : recnData->debit;
    gnc_query_view_grab_focus (GNC_QUERY_VIEW (other_view));
    (void)keycode;
    return TRUE;
}

static GNCSplitReg *
gnc_reconcile_window_open_register(RecnWindow *recnData)
{
    Account *account = recn_get_account (recnData);
    GNCSplitReg *gsr;
    gboolean include_children;

    if (!account)
        return(NULL);

    include_children = xaccAccountGetReconcileChildrenStatus (account);
    recnData->page = gnc_plugin_page_register_new (account, include_children);
    gnc_main_window_open_page (NULL, recnData->page);
    gsr = gnc_plugin_page_register_get_gsr (recnData->page);
    gnc_split_reg_raise (gsr);
    return gsr;
}


static void
gnc_reconcile_window_double_click_cb(GNCReconcileView *view, Split *split,
                                     gpointer data)
{
    auto recnData = static_cast<RecnWindow*>(data);
    GNCSplitReg *gsr;

    /* This should never be true, but be paranoid */
    if (split == NULL)
        return;

    gsr = gnc_reconcile_window_open_register(recnData);
    if (gsr == NULL)
        return;

    recn_register_reveal_split_async (recnData, gsr, split,
                                      RECN_REGISTER_REVEAL_SPLIT);
}


static void
gnc_reconcile_window_set_titles(RecnWindow *recnData)
{
    const gchar *title;

    title = gnc_account_get_debit_string(ACCT_TYPE_NONE);
    gtk_frame_set_label(GTK_FRAME(recnData->debit_frame), title);

    title = gnc_account_get_credit_string(ACCT_TYPE_NONE);
    gtk_frame_set_label(GTK_FRAME(recnData->credit_frame), title);
}


static GtkWidget *
gnc_reconcile_window_create_view_box(Account *account,
                                     GNCReconcileViewType type,
                                     RecnWindow *recnData,
                                     GtkWidget **list_save,
                                     GtkWidget **total_save)
{
    GtkWidget *frame, *scrollWin, *view, *vbox, *label, *hbox;
    GtkWidget *vscroll;
    GtkRequisition nat_sb;

    frame = gtk_frame_new(NULL);

    if (type == RECLIST_DEBIT)
        recnData->debit_frame = frame;
    else
        recnData->credit_frame = frame;

    vbox = gtk_box_new (GTK_ORIENTATION_VERTICAL, 5);
    gtk_box_set_homogeneous (GTK_BOX (vbox), FALSE);

    view = gnc_reconcile_view_new(account, type, recnData->statement_date);
    *list_save = view;

    g_signal_connect(view, "toggle_reconciled",
                     G_CALLBACK(gnc_reconcile_window_toggled_cb),
                     recnData);
    g_signal_connect(view, "line_selected",
                     G_CALLBACK(gnc_reconcile_window_row_cb),
                     recnData);
    auto context_click = GTK_GESTURE_CLICK (gtk_gesture_click_new ());
    gtk_gesture_single_set_button (GTK_GESTURE_SINGLE (context_click), GDK_BUTTON_SECONDARY);
    g_signal_connect (context_click, "pressed",
                      G_CALLBACK (gnc_reconcile_window_context_pressed_cb), recnData);
    gtk_widget_add_controller (view, GTK_EVENT_CONTROLLER (context_click));
    g_signal_connect(view, "double_click_split",
                     G_CALLBACK(gnc_reconcile_window_double_click_cb),
                     recnData);
    auto focus_controller = gtk_event_controller_focus_new ();
    g_signal_connect (focus_controller, "enter",
                      G_CALLBACK (gnc_reconcile_window_focus_enter_cb), recnData);
    gtk_widget_add_controller (view, focus_controller);

    auto key_controller = gtk_event_controller_key_new ();
    g_signal_connect (key_controller, "key-pressed",
                      G_CALLBACK (gnc_reconcile_key_pressed_cb), recnData);
    gtk_widget_add_controller (view, key_controller);

    scrollWin = gtk_scrolled_window_new ();
    gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW (scrollWin),
                                   GTK_POLICY_AUTOMATIC,
                                   GTK_POLICY_AUTOMATIC);
    gnc_widget_set_all_margins (scrollWin, 5);

    gtk_frame_set_child (GTK_FRAME(frame), scrollWin);
    gtk_scrolled_window_set_child (GTK_SCROLLED_WINDOW(scrollWin), view);
    gnc_box_append_full(GTK_BOX(vbox), frame, TRUE, TRUE, 0);

    // get the vertical scroll bar width
    vscroll = gtk_scrolled_window_get_vscrollbar (GTK_SCROLLED_WINDOW (scrollWin));
    gtk_widget_get_preferred_size (vscroll, NULL, &nat_sb);

    // add xpadding to recn column so scrollbar does not cover
    gnc_reconcile_view_add_padding (GNC_RECONCILE_VIEW(view), REC_RECN, nat_sb.width);

    hbox = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 5);
    gtk_box_set_homogeneous (GTK_BOX (hbox), FALSE);
    gnc_box_append_full(GTK_BOX(vbox), hbox, FALSE, FALSE, 0);

    label = gtk_label_new(_("Total"));
    gnc_label_set_alignment(label, 1.0, 0.5);
    gnc_box_append_full(GTK_BOX(hbox), label, TRUE, TRUE, 0);

    label = gtk_label_new("");
    gnc_box_append_full(GTK_BOX(hbox), label, FALSE, FALSE, 0);
    *total_save = label;
    gtk_widget_set_margin_end (GTK_WIDGET(label), 10 + nat_sb.width);

    return vbox;
}


static Split *
gnc_reconcile_window_get_current_split(RecnWindow *recnData)
{
    GNCReconcileView *view;
    Split *split;

    view = GNC_RECONCILE_VIEW(recnData->debit);
    split = gnc_reconcile_view_get_current_split(view);
    if (split != NULL)
        return split;

    view = GNC_RECONCILE_VIEW(recnData->credit);
    split = gnc_reconcile_view_get_current_split(view);

    return split;
}


static void
gnc_ui_reconcile_window_help_cb (GSimpleAction *simple,
                                 GVariant      *parameter,
                                 gpointer       user_data)
{
    auto recnData = static_cast<RecnWindow*>(user_data);
    gnc_gnome_help (GTK_WINDOW(recnData->window), DF_MANUAL, DL_RECNWIN);
}


static void
gnc_ui_reconcile_window_change_cb (GSimpleAction *simple,
                                   GVariant      *parameter,
                                   gpointer       user_data)
{
    auto recnData = static_cast<RecnWindow*>(user_data);
    Account *account = recn_get_account (recnData);
    gnc_numeric new_ending;
    time64 statement_date;

    (void)simple;
    (void)parameter;
    if (!account)
        return;

    if (recnData->start_dialog)
    {
        start_recn_dialog_present (recnData->start_dialog);
        return;
    }

    new_ending = recnData->new_ending;
    statement_date = recnData->statement_date;
    if (gnc_reverse_balance (account))
        new_ending = gnc_numeric_neg (new_ending);

    start_recn_dialog_open (recnData->window, account, new_ending,
                            statement_date, FALSE,
                            start_recn_change_accepted, recnData);
}

static void
gnc_ui_reconcile_window_balance_cb (GSimpleAction *simple,
                                    GVariant      *parameter,
                                    gpointer       user_data)
{
    auto recnData = static_cast<RecnWindow*>(user_data);
    GNCSplitReg *gsr;
    Account *account;
    gnc_numeric balancing_amount;
    time64 statement_date;


    gsr = gnc_reconcile_window_open_register(recnData);
    if (gsr == NULL)
        return;

    account = recn_get_account(recnData);
    if (account == NULL)
        return;

    balancing_amount = recnRecalculateBalance(recnData);
    if (gnc_numeric_zero_p(balancing_amount))
        return;

    statement_date = recnData->statement_date;
    if (statement_date == 0)
        statement_date = gnc_time (NULL); // default to 'now'

    gnc_split_reg_balancing_entry(gsr, account, statement_date, balancing_amount);
}


static void
gnc_ui_reconcile_window_rec_cb (GSimpleAction *simple,
                                GVariant      *parameter,
                                gpointer       user_data)
{
    auto recnData = static_cast<RecnWindow*>(user_data);
    GNCReconcileView *debit, *credit;

    debit  = GNC_RECONCILE_VIEW(recnData->debit);
    credit = GNC_RECONCILE_VIEW(recnData->credit);

    gnc_reconcile_view_set_list (debit, TRUE);
    gnc_reconcile_view_set_list (credit, TRUE);
}


static void
gnc_ui_reconcile_window_unrec_cb (GSimpleAction *simple,
                                  GVariant      *parameter,
                                  gpointer       user_data)
{
    auto recnData = static_cast<RecnWindow*>(user_data);
    GNCReconcileView *debit, *credit;

    debit  = GNC_RECONCILE_VIEW(recnData->debit);
    credit = GNC_RECONCILE_VIEW(recnData->credit);

    gnc_reconcile_view_set_list (debit, FALSE);
    gnc_reconcile_view_set_list (credit, FALSE);
}


/** Get the debit or credit view that has at least 1 split selected.
 *   gnc_reconcile_window_focus_cb() ensures only 1 view
 *   has a selection.
 * @param window The reconcile window.
 */
static GNCReconcileView *
gnc_reconcile_window_get_selection_view (RecnWindow *recnData)
{
    if (gnc_reconcile_view_num_selected (GNC_RECONCILE_VIEW (recnData->debit)) > 0)
        return GNC_RECONCILE_VIEW (recnData->debit);

    if (gnc_reconcile_view_num_selected (GNC_RECONCILE_VIEW (recnData->credit)) > 0)
        return GNC_RECONCILE_VIEW (recnData->credit);

    return NULL;
}


/** Select the next split in the debit or credit view so that after the Delete
 *   button is actioned, the working position in the list is still in view.
 *  Unless this is done, the list will be scrolled to the top.
 *  The new split selected must have a different parent transaction as all splits
 *   for the transaction will be deleted.
 */
static void
gnc_reconcile_window_delete_set_next_selection (RecnWindow *recnData, Split *split)
{
    GNCReconcileView *view = gnc_reconcile_window_get_selection_view (recnData);
    Transaction *transaction;
    Split *candidate;

    if (!view)
        return;
    transaction = xaccSplitGetParent (split);
    candidate = (Split *)gnc_query_view_get_adjacent_entry (GNC_QUERY_VIEW (view), split, FALSE);
    while (candidate && xaccSplitGetParent (candidate) == transaction)
        candidate = (Split *)gnc_query_view_get_adjacent_entry (GNC_QUERY_VIEW (view), candidate, FALSE);
    if (!candidate)
    {
        candidate = (Split *)gnc_query_view_get_adjacent_entry (GNC_QUERY_VIEW (view), split, TRUE);
        while (candidate && xaccSplitGetParent (candidate) == transaction)
            candidate = (Split *)gnc_query_view_get_adjacent_entry (GNC_QUERY_VIEW (view), candidate, TRUE);
    }
    if (candidate)
        gnc_query_view_select_entry (GNC_QUERY_VIEW (view), candidate, TRUE);
}

static void
recn_delete_transaction (RecnWindow *recnData, const GncGUID *split_guid,
                         const GncGUID *transaction_guid)
{
    auto book = gnc_get_current_book ();
    auto split = book && split_guid ? xaccSplitLookup (split_guid, book) : NULL;
    auto trans = book && transaction_guid ? xaccTransLookup (transaction_guid, book) : NULL;

    if (!recnData || !book || qof_book_shutting_down (book) || !split || !trans ||
        xaccSplitGetParent (split) != trans ||
        gnc_reconcile_window_get_current_split (recnData) != split)
        return;

    /* The response owns only GUIDs. Select while the resolved split is still
     * current, then destroy the resolved transaction exactly once. */
    gnc_reconcile_window_delete_set_next_selection (recnData, split);
    gnc_suspend_gui_refresh ();
    xaccTransDestroy (trans);
    gnc_resume_gui_refresh ();
}

static void
gnc_ui_reconcile_window_delete_cb (GSimpleAction *simple,
                                   GVariant      *parameter,
                                   gpointer       user_data)
{
    auto recnData = static_cast<RecnWindow *> (user_data);
    auto split = recnData ? gnc_reconcile_window_get_current_split (recnData) : NULL;

    (void)simple;
    (void)parameter;
    if (!recnData || recnData->confirmation || !split || !xaccSplitGetParent (split))
        return;

    recn_confirm (recnData, RECN_CONFIRM_DELETE, split,
                  _("Are you sure you want to delete the selected transaction?"));
}


static void
gnc_ui_reconcile_window_edit_cb (GSimpleAction *simple,
                                 GVariant      *parameter,
                                 gpointer       user_data)
{
    auto recnData = static_cast<RecnWindow*>(user_data);
    GNCSplitReg *gsr;
    Split *split;

    split = gnc_reconcile_window_get_current_split (recnData);
    /* This should never be true, but be paranoid */
    if (split == NULL)
        return;

    gsr = gnc_reconcile_window_open_register(recnData);
    if (gsr == NULL)
        return;

    recn_register_reveal_split_async (recnData, gsr, split,
                                      RECN_REGISTER_REVEAL_SPLIT_AMOUNT);
}


static char *
gnc_recn_make_window_name(Account *account)
{
    char *fullname;
    char *title;

    fullname = gnc_account_get_full_name(account);
    title = g_strconcat(fullname, " - ", _("Reconcile"), NULL);

    g_free(fullname);

    return title;
}


static void
gnc_recn_set_window_name(RecnWindow *recnData)
{
    char *title;

    title = gnc_recn_make_window_name (recn_get_account (recnData));

    gtk_window_set_title (GTK_WINDOW (recnData->window), title);

    g_free (title);
}


static void
gnc_recn_edit_account_cb (GSimpleAction *simple,
                          GVariant      *parameter,
                          gpointer       user_data)
{
    auto recnData = static_cast<RecnWindow*>(user_data);
    Account *account = recn_get_account (recnData);

    if (account == NULL)
        return;

    gnc_ui_edit_account_window (GTK_WINDOW (recnData->window), account);
}


static void
gnc_recn_xfer_cb (GSimpleAction *simple,
                  GVariant      *parameter,
                  gpointer       user_data)
{
    auto recnData = static_cast<RecnWindow*>(user_data);
    Account *account = recn_get_account (recnData);

    if (account == NULL)
        return;

    gnc_xfer_dialog (recnData->window, account);
}


static void
gnc_recn_scrub_cb (GSimpleAction *simple,
                   GVariant      *parameter,
                   gpointer       user_data)
{
    auto recnData = static_cast<RecnWindow*>(user_data);
    Account *account = recn_get_account (recnData);

    if (account == NULL)
        return;

    auto context = gnc_scrub_context_begin (
        qof_instance_get_book (QOF_INSTANCE (account)));
    if (!context)
        return;

    gnc_suspend_gui_refresh ();

    xaccAccountTreeScrubOrphansWithContext (
        account, gnc_window_show_progress, context);
    xaccAccountTreeScrubImbalanceWithContext (
        account, gnc_window_show_progress, context);

    gnc_scrub_context_end (context);
    gnc_scrub_context_unref (context);
    gnc_resume_gui_refresh ();

    if (g_getenv("GNC_AUTO_SCRUB_LOTS") != NULL)
    {
        auto runner = gnc_scrub_job_runner_start_lots (
            account, TRUE, G_OBJECT (recnData->window), nullptr, 1,
            nullptr, nullptr, nullptr, nullptr);
        if (runner)
            gnc_scrub_job_runner_unref (runner);
    }
}


static void
gnc_recn_open_cb (GSimpleAction *simple,
                  GVariant      *parameter,
                  gpointer       user_data)
{
    auto recnData = static_cast<RecnWindow*>(user_data);

    gnc_reconcile_window_open_register(recnData);
}


static void
gnc_get_reconcile_info (Account *account,
                        gnc_numeric *new_ending,
                        time64 *statement_date)
{
    gboolean always_today;
    GDate date;
    time64 today;

    g_date_clear(&date, 1);

    always_today = gnc_prefs_get_bool(GNC_PREFS_GROUP_RECONCILE, GNC_PREF_ALWAYS_REC_TO_TODAY);

    if (!always_today &&
            xaccAccountGetReconcileLastDate (account, statement_date))
    {
        int months = 1, days = 0;

        gnc_gdate_set_time64(&date, *statement_date);

        xaccAccountGetReconcileLastInterval (account, &months, &days);

        if (months)
        {
            gboolean was_last_day_of_month = g_date_is_last_of_month(&date);

            g_date_add_months(&date, months);

            /* Track last day of the month, i.e. 1/31 -> 2/28 -> 3/31 */
            if (was_last_day_of_month)
            {
                g_date_set_day (&date, g_date_get_days_in_month(g_date_get_month(&date),
                                g_date_get_year( &date)));
            }
        }
        else
        {
            g_date_add_days (&date, days);
        }

        *statement_date = gnc_time64_get_day_end_gdate (&date);

        today = gnc_time64_get_day_end (gnc_time (NULL));
        if (*statement_date > today)
            *statement_date = today;
    }

    xaccAccountGetReconcilePostponeDate (account, statement_date);

    if (xaccAccountGetReconcilePostponeBalance(account, new_ending))
    {
        if (gnc_reverse_balance(account))
            *new_ending = gnc_numeric_neg(*new_ending);
    }
    else
    {
        /* if the account wasn't previously postponed, try to predict
         * the statement balance based on the statement date.
         */
        *new_ending =
            gnc_ui_account_get_balance_as_of_date
            (account, *statement_date,
             xaccAccountGetReconcileChildrenStatus(account));
    }
}


static gboolean
find_by_account (gpointer find_data, gpointer user_data)
{
    auto account = GNC_ACCOUNT(find_data);
    auto recnData = static_cast<RecnWindow*>(user_data);

    if (!recnData)
        return FALSE;

    return guid_equal (&recnData->account, xaccAccountGetGUID (account));
}


static void
recn_set_watches_one_account (gpointer data, gpointer user_data)
{
    Account *account = (Account *)data;
    RecnWindow *recnData = (RecnWindow *)user_data;

    /* add a watch on the account */
    gnc_gui_component_watch_entity (recnData->component_id,
                                    xaccAccountGetGUID (account),
                                    QOF_EVENT_MODIFY | QOF_EVENT_DESTROY);

    /* add a watch on each split for the account */
    for (auto split : xaccAccountGetSplits (account))
    {
        auto trans = xaccSplitGetParent (split);
        gnc_gui_component_watch_entity (recnData->component_id,
                                        xaccTransGetGUID (trans),
                                        QOF_EVENT_MODIFY
                                        | QOF_EVENT_DESTROY
                                        | GNC_EVENT_ITEM_CHANGED);
    }
}


static void
recn_set_watches (RecnWindow *recnData)
{
    gboolean include_children;
    Account *account;
    GList *accounts = NULL;

    gnc_gui_component_clear_watches (recnData->component_id);

    account = recn_get_account (recnData);

    include_children = xaccAccountGetReconcileChildrenStatus(account);
    if (include_children)
        accounts = gnc_account_get_descendants(account);

    /* match the account */
    accounts = g_list_prepend (accounts, account);

    g_list_foreach(accounts, recn_set_watches_one_account, recnData);

    g_list_free (accounts);
}


static void
refresh_handler (GHashTable *changes, gpointer user_data)
{
    auto recnData = static_cast<RecnWindow*>(user_data);
    const EventInfo *info;
    Account *account;

    account = recn_get_account (recnData);
    if (!account)
    {
        gnc_close_gui_component_by_data (WINDOW_RECONCILE_CM_CLASS, recnData);
        return;
    }

    if (changes)
    {
        info = gnc_gui_get_entity_events (changes, &recnData->account);
        if (info && (info->event_mask & QOF_EVENT_DESTROY))
        {
            gnc_close_gui_component_by_data (WINDOW_RECONCILE_CM_CLASS, recnData);
            return;
        }
    }

    gnc_reconcile_window_set_titles(recnData);
    recn_set_watches (recnData);

    recnRefresh (recnData);
}


static void
close_handler (gpointer user_data)
{
    auto recnData = static_cast<RecnWindow*>(user_data);

    gnc_save_window_size(GNC_PREFS_GROUP_RECONCILE, GTK_WINDOW(recnData->window));
    gtk_window_destroy (GTK_WINDOW(recnData->window));
}


/********************************************************************\
 * recnWindow                                                       *
 *                                                                  *
 * Starts reconciliation without a nested main loop. Existing live  *
 * or pending dialogs for the account are presented instead of       *
 * creating a duplicate workflow.                                   *
\********************************************************************/
void
recnWindow (GtkWidget *parent, Account *account)
{
    gnc_numeric new_ending;
    time64 statement_date;
    gboolean enable_subaccounts;
    RecnWindow *recnData;

    if (!account)
        return;

    recnData = static_cast<RecnWindow*> (
        gnc_find_first_gui_component (WINDOW_RECONCILE_CM_CLASS,
                                      find_by_account, account));
    if (recnData)
    {
        gnc_ui_reconcile_window_raise (recnData);
        return;
    }

    if (auto pending = start_recn_find_pending (account))
    {
        start_recn_dialog_present (pending);
        return;
    }

    statement_date = gnc_reconcile_last_statement_date
        ? gnc_reconcile_last_statement_date
        : gnc_time64_get_day_end (gnc_time (NULL));
    gnc_get_reconcile_info (account, &new_ending, &statement_date);
    enable_subaccounts = !has_account_different_commodities (account);

    start_recn_dialog_open (parent, account, new_ending, statement_date,
                            enable_subaccounts, start_recn_initial_accepted,
                            NULL);
}

static GActionEntry recWindow_actions_entries [] =
{
    { "RecnChangeInfoAction", gnc_ui_reconcile_window_change_cb, NULL, NULL, NULL },
    { "RecnFinishAction", recnFinishCB, NULL, NULL, NULL },
    { "RecnPostponeAction", recnPostponeCB, NULL, NULL, NULL },
    { "RecnCancelAction", recnCancelCB, NULL, NULL, NULL },
    { "RecnAutoClearAction", recnAutoClearCB, NULL, NULL, NULL },

    { "AccountOpenAccountAction", gnc_recn_open_cb, NULL, NULL, NULL },
    { "AccountEditAccountAction", gnc_recn_edit_account_cb, NULL, NULL, NULL },
    { "AccountTransferAction", gnc_recn_xfer_cb, NULL, NULL, NULL },
    { "AccountCheckRepairAction", gnc_recn_scrub_cb, NULL, NULL, NULL },

    { "TransBalanceAction", gnc_ui_reconcile_window_balance_cb, NULL, NULL, NULL },
    { "TransEditAction", gnc_ui_reconcile_window_edit_cb, NULL, NULL, NULL },
    { "TransDeleteAction", gnc_ui_reconcile_window_delete_cb, NULL, NULL, NULL },
    { "TransRecAction", gnc_ui_reconcile_window_rec_cb, NULL, NULL, NULL },
    { "TransUnRecAction", gnc_ui_reconcile_window_unrec_cb, NULL, NULL, NULL },

    { "HelpHelpAction", gnc_ui_reconcile_window_help_cb, NULL, NULL, NULL },
};
/** The number of actions provided by the reconcile window. */
static guint recnWindow_n_actions_entries = G_N_ELEMENTS(recWindow_actions_entries);

/********************************************************************\
 * recnWindowWithBalance
 *
 *   Opens up the window to reconcile an account, but with ending
 *   balance and statement date already given.
 *
 * Args:   parent         - The parent widget of the new window
 *         account        - The account to reconcile
 *         new_ending     - The amount for ending balance
 *         statement_date - The date of the statement
 * Return: recnData - the instance of this RecnWindow
\********************************************************************/
RecnWindow *
recnWindowWithBalance (GtkWidget *parent, Account *account, gnc_numeric new_ending,
                       time64 statement_date)
{
    RecnWindow *recnData;
    GtkWidget *warning_bar;
    GtkWidget *vbox;
    GtkWidget *dock;

    if (account == NULL)
        return NULL;

    recnData = static_cast<RecnWindow*>(gnc_find_first_gui_component (WINDOW_RECONCILE_CM_CLASS,
                                                                      find_by_account, account));
    if (recnData)
        return recnData;

    recnData = g_new0 (RecnWindow, 1);

    recnData->account = *xaccAccountGetGUID (account);


    recnData->component_id =
        gnc_register_gui_component (WINDOW_RECONCILE_CM_CLASS,
                                    refresh_handler, close_handler,
                                    recnData);
    gnc_gui_component_set_session (recnData->component_id, gnc_get_current_session());

    recn_set_watches (recnData);

    gnc_reconcile_last_statement_date = statement_date;

    recnData->new_ending = new_ending;
    recnData->statement_date = statement_date;
    recnData->window = gtk_window_new ();
    gnc_window_bind_to_application (GTK_WINDOW (recnData->window));
    recnData->delete_refresh = FALSE;
    new (&recnData->autoclear_splits) SplitsVec();
    new (&recnData->initially_cleared_splits) SplitsVec();

    gnc_recn_set_window_name(recnData);

    vbox = gtk_box_new (GTK_ORIENTATION_VERTICAL, 0);
    gtk_box_set_homogeneous (GTK_BOX (vbox), FALSE);
    gtk_window_set_child (GTK_WINDOW(recnData->window), vbox);

    // Set the name for this dialog so it can be easily manipulated with css
    gtk_widget_set_name (GTK_WIDGET(recnData->window), "gnc-id-reconcile");

    dock = gtk_box_new (GTK_ORIENTATION_VERTICAL, 0);
    gtk_box_set_homogeneous (GTK_BOX (dock), FALSE);
    gtk_widget_set_visible (dock, TRUE);
    gnc_box_append_full(GTK_BOX (vbox), dock, FALSE, TRUE, 0);

    auto init_cleared = [&recnData](Split* s)
    {
        if (xaccSplitGetReconcile (s) == CREC)
            recnData->initially_cleared_splits.push_back (s);
    };
    gnc_account_foreach_split_until_date (account, statement_date, init_cleared);

    {
        GtkWidget *tool_bar;
        GMenuModel *menu_model;
        GtkWidget *menu_bar;
        const gchar *ui = GNUCASH_RESOURCE_PREFIX "/gnc-reconcile-window.ui";
        GError *error = NULL;

        recnData->builder = gtk_builder_new ();

        gtk_builder_add_from_resource (recnData->builder, ui, &error);

        gtk_builder_set_translation_domain (recnData->builder, PROJECT_NAME);

        if (error)
        {
            g_critical ("Failed to load ui resource %s, Error %s", ui, error->message);
            g_error_free (error);
            gnc_unregister_gui_component_by_data (WINDOW_RECONCILE_CM_CLASS, recnData);
            g_free (recnData);
            return NULL;
        }

        recnData->autoclear_button = GTK_WIDGET(gtk_builder_get_object(recnData->builder, "autoclear_button"));

        menu_model = (GMenuModel *)gtk_builder_get_object (recnData->builder, "recwin-menu");
        menu_bar = gtk_popover_menu_bar_new_from_model (menu_model);
        gtk_box_append (GTK_BOX(vbox), menu_bar);
        tool_bar = GTK_WIDGET (gtk_builder_get_object (recnData->builder,
                                                        "recwin-toolbar"));

        gtk_box_append (GTK_BOX(vbox), tool_bar);


        recnData->simple_action_group = g_simple_action_group_new ();

        g_action_map_add_action_entries (G_ACTION_MAP(recnData->simple_action_group),
                                         recWindow_actions_entries,
                                         recnWindow_n_actions_entries,
                                         recnData);

        gtk_widget_insert_action_group (GTK_WIDGET(recnData->window), "recwin",
                                        G_ACTION_GROUP(recnData->simple_action_group));

        auto shortcut_controller = GTK_SHORTCUT_CONTROLLER (
            gtk_shortcut_controller_new ());
        gtk_shortcut_controller_set_scope (shortcut_controller,
                                           GTK_SHORTCUT_SCOPE_GLOBAL);
        gtk_shortcut_controller_add_shortcut (
            shortcut_controller,
            gtk_shortcut_new (
                gtk_keyval_trigger_new (GDK_KEY_Escape,
                                        static_cast<GdkModifierType> (0)),
                              gtk_callback_action_new (recn_escape_shortcut_cb,
                                                       recnData, NULL)));
        gnc_add_accelerator_keys_for_menu (
            menu_bar, menu_model, GTK_EVENT_CONTROLLER (shortcut_controller));
        gtk_widget_add_controller (recnData->window,
                                   GTK_EVENT_CONTROLLER (shortcut_controller));
    }

    warning_bar = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 6);
    gtk_widget_set_margin_start (warning_bar, 6);
    gtk_widget_set_margin_end (warning_bar, 6);
    gtk_widget_set_margin_top (warning_bar, 3);
    gtk_widget_set_margin_bottom (warning_bar, 3);
    gtk_widget_set_visible (warning_bar, FALSE);
    gnc_box_prepend_full (GTK_BOX (vbox), warning_bar, FALSE, FALSE, 0);

    g_signal_connect (recnData->window, "destroy",
                      G_CALLBACK(recn_destroy_cb), recnData);
    g_signal_connect (recnData->window, "close-request",
                      G_CALLBACK(recn_close_request_cb), recnData);

    /* If the account has a reconciled split with a reconciliation date later
     * than this statement date, show the persistent warning directly in the
     * window without reserving space when no warning is needed. */
    {
        auto has_later_recn_statement_date = [statement_date](const Split *split)
        { return (xaccSplitGetReconcile (split) == YREC &&
                  xaccSplitGetDateReconciled (split) > statement_date); };

        if (auto split = gnc_account_find_split (account,
                                                  has_later_recn_statement_date,
                                                  true))
        {
            auto datestr = qof_print_date (xaccTransGetDate (xaccSplitGetParent (split)));
            auto recnstr = qof_print_date (xaccSplitGetDateReconciled (split));
            auto image = gtk_image_new_from_icon_name ("dialog-warning");
            auto label = gtk_label_new (_("WARNING! Account contains splits whose "
                                          "reconcile date is after statement date. "
                                          "Reconciliation may be difficult."));

            PWARN ("split posting_date=%s, recn_date=%s", datestr, recnstr);
            gtk_label_set_wrap (GTK_LABEL (label), TRUE);
            gtk_label_set_xalign (GTK_LABEL (label), 0.0);
            gtk_box_append (GTK_BOX (warning_bar), image);
            gtk_box_append (GTK_BOX (warning_bar), label);
            gtk_widget_set_tooltip_text (warning_bar, _("This account has splits "
                "whose Reconciled Date is after this reconciliation statement date. "
                "These splits may make reconciliation difficult. If this is the case, "
                "you may use Find Transactions to find them, unreconcile, and "
                "re-reconcile."));
            gtk_widget_set_visible (warning_bar, TRUE);

            g_free (datestr);
            g_free (recnstr);
        }
    }

    /* The main area */
    {
        GtkWidget *frame = gtk_frame_new(NULL);
        GtkWidget *main_area = gtk_box_new (GTK_ORIENTATION_VERTICAL, 10);
        GtkWidget *debcred_area = gtk_grid_new ();
        GtkWidget *debits_box;
        GtkWidget *credits_box;

        gtk_box_set_homogeneous (GTK_BOX (main_area), FALSE);
        gnc_box_append_full(GTK_BOX(vbox), frame, TRUE, TRUE, 10);

        /* Force a reasonable starting size */
        gtk_window_set_default_size(GTK_WINDOW(recnData->window), 800, 600);
        gnc_restore_window_size (GNC_PREFS_GROUP_RECONCILE,
                                 GTK_WINDOW(recnData->window), GTK_WINDOW(parent));

        gtk_frame_set_child (GTK_FRAME(frame), main_area);
        gnc_widget_set_all_margins (main_area, 10);

        debits_box = gnc_reconcile_window_create_view_box
                     (account, RECLIST_DEBIT, recnData,
                      &recnData->debit, &recnData->total_debit);

        // Add a style context for this widget so it can be easily manipulated with css
        gnc_widget_style_context_add_class (GTK_WIDGET(debits_box), "gnc-class-debits");

        credits_box = gnc_reconcile_window_create_view_box
                      (account, RECLIST_CREDIT, recnData,
                       &recnData->credit, &recnData->total_credit);

        // Add a style context for this widget so it can be easily manipulated with css
        gnc_widget_style_context_add_class (GTK_WIDGET(credits_box), "gnc-class-credits");

        GNC_RECONCILE_VIEW(recnData->debit)->sibling = GNC_RECONCILE_VIEW(recnData->credit);
        GNC_RECONCILE_VIEW(recnData->credit)->sibling = GNC_RECONCILE_VIEW(recnData->debit);

        gnc_box_append_full(GTK_BOX(main_area), debcred_area, TRUE, TRUE, 0);

        gtk_grid_set_column_homogeneous (GTK_GRID(debcred_area), TRUE);
        gtk_grid_set_column_spacing (GTK_GRID(debcred_area), 15);
        gtk_grid_attach (GTK_GRID(debcred_area), debits_box, 0, 0, 1, 1);
        gtk_widget_set_hexpand (debits_box, TRUE);
        gtk_widget_set_vexpand (debits_box, TRUE);
        gtk_widget_set_halign (debits_box, GTK_ALIGN_FILL);
        gtk_widget_set_valign (debits_box, GTK_ALIGN_FILL);

        gtk_grid_attach (GTK_GRID(debcred_area), credits_box, 1, 0, 1, 1);
        gtk_widget_set_hexpand (credits_box, TRUE);
        gtk_widget_set_vexpand (credits_box, TRUE);
        gtk_widget_set_halign (credits_box, GTK_ALIGN_FILL);
        gtk_widget_set_valign (credits_box, GTK_ALIGN_FILL);

        {
            GtkWidget *hbox, *title_vbox, *value_vbox;
            GtkWidget *totals_hbox, *frame, *title, *value;

            /* lower horizontal bar below reconcile lists */
            hbox = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 5);
            gtk_box_set_homogeneous (GTK_BOX (hbox), FALSE);
            gnc_box_append_full(GTK_BOX(main_area), hbox, FALSE, FALSE, 0);

            /* frame to hold totals */
            frame = gtk_frame_new(NULL);
            gnc_box_prepend_full(GTK_BOX(hbox), frame, FALSE, FALSE, 0);

            // Set the name for this widget so it can be easily manipulated with css
            gtk_widget_set_name (GTK_WIDGET(frame), "gnc-id-reconcile-totals");

            /* hbox to hold title/value vboxes */
            totals_hbox = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 3);
            gtk_box_set_homogeneous (GTK_BOX (totals_hbox), FALSE);
            gtk_frame_set_child (GTK_FRAME(frame), totals_hbox);
            gnc_widget_set_all_margins (totals_hbox, 5);

            /* vbox to hold titles */
            title_vbox = gtk_box_new (GTK_ORIENTATION_VERTICAL, 3);
            gtk_box_set_homogeneous (GTK_BOX (title_vbox), FALSE);
            gnc_box_append_full(GTK_BOX(totals_hbox), title_vbox, FALSE, FALSE, 0);

            /* vbox to hold values */
            value_vbox = gtk_box_new (GTK_ORIENTATION_VERTICAL, 3);
            gtk_box_set_homogeneous (GTK_BOX (value_vbox), FALSE);
            gnc_box_append_full(GTK_BOX(totals_hbox), value_vbox, TRUE, TRUE, 0);

            /* statement date title/value */
            title = gtk_label_new(_("Statement Date"));
            gnc_label_set_alignment(title, 1.0, 0.5);
            gnc_box_append_full(GTK_BOX(title_vbox), title, FALSE, FALSE, 0);

            value = gtk_label_new("");
            recnData->recn_date = value;
            gnc_label_set_alignment(value, 1.0, 0.5);
            gnc_box_append_full(GTK_BOX(value_vbox), value, FALSE, FALSE, 0);

            /* starting balance title/value */
            title = gtk_label_new(_("Starting Balance"));
            gnc_label_set_alignment(title, 1.0, 0.5);
            gnc_box_append_full(GTK_BOX(title_vbox), title, FALSE, FALSE, 3);

            value = gtk_label_new("");
            recnData->starting = value;
            gnc_label_set_alignment(value, 1.0, 0.5);
            gnc_box_append_full(GTK_BOX(value_vbox), value, FALSE, FALSE, 3);

            /* ending balance title/value */
            title = gtk_label_new(_("Ending Balance"));
            gnc_label_set_alignment(title, 1.0, 0.5);
            gnc_box_append_full(GTK_BOX(title_vbox), title, FALSE, FALSE, 0);

            value = gtk_label_new("");
            recnData->ending = value;
            gnc_label_set_alignment(value, 1.0, 0.5);
            gnc_box_append_full(GTK_BOX(value_vbox), value, FALSE, FALSE, 0);

            /* reconciled balance title/value */
            title = gtk_label_new(_("Reconciled Balance"));
            gnc_label_set_alignment(title, 1.0, 0.5);
            gnc_box_append_full(GTK_BOX(title_vbox), title, FALSE, FALSE, 0);

            value = gtk_label_new("");
            recnData->reconciled = value;
            gnc_label_set_alignment(value, 1.0, 0.5);
            gnc_box_append_full(GTK_BOX(value_vbox), value, FALSE, FALSE, 0);

            /* difference title/value */
            title = gtk_label_new(_("Difference"));
            gnc_label_set_alignment(title, 1.0, 0.5);
            gnc_box_append_full(GTK_BOX(title_vbox), title, FALSE, FALSE, 0);

            value = gtk_label_new("");
            recnData->difference = value;
            gnc_label_set_alignment(value, 1.0, 0.5);
            gnc_box_append_full(GTK_BOX(value_vbox), value, FALSE, FALSE, 0);
        }

        /* Set up the data */
        recnRefresh (recnData);
    }

    /* Allow resize */
    gtk_window_set_resizable(GTK_WINDOW(recnData->window), TRUE);
    gtk_widget_set_visible (recnData->window, TRUE);

    gnc_reconcile_window_set_titles(recnData);

    recnRecalculateBalance(recnData);

    gnc_window_adjust_for_screen(GTK_WINDOW(recnData->window));

    /* Set the sort orders of the debit and credit tree views */
    gnc_query_sort_order(GNC_QUERY_VIEW(recnData->debit), REC_DATE, GTK_SORT_ASCENDING);
    gnc_query_sort_order(GNC_QUERY_VIEW(recnData->credit), REC_DATE, GTK_SORT_ASCENDING);

    gnc_query_view_grab_focus (GNC_QUERY_VIEW (recnData->debit));

    {   // align the Totals value with that of the amount column
        gint recn_widthc = gnc_reconcile_view_get_column_width (GNC_RECONCILE_VIEW(recnData->credit), REC_RECN);
        gint recn_widthd = gnc_reconcile_view_get_column_width (GNC_RECONCILE_VIEW(recnData->debit), REC_RECN);

        gtk_widget_set_margin_end (GTK_WIDGET(recnData->total_credit), 10 + recn_widthc);
        gtk_widget_set_margin_end (GTK_WIDGET(recnData->total_debit), 10 + recn_widthd);
    }
    return recnData;
}


/********************************************************************\
 * gnc_ui_reconcile_window_raise                                     *
 *   shows and raises an account editing window                     *
 *                                                                  *
 * Args:   editAccData - the edit window structure                  *
\********************************************************************/
void
gnc_ui_reconcile_window_raise(RecnWindow * recnData)
{
    if (recnData == NULL)
        return;

    if (recnData->window == NULL)
        return;

    gtk_window_present(GTK_WINDOW(recnData->window));
}

GtkWindow *
gnc_ui_reconcile_window_get_window (RecnWindow * recnData)
{
    if (recnData == NULL || recnData->window == NULL)
        return NULL;
    return GTK_WINDOW(recnData->window);
}



/********************************************************************\
 * recn_destroy_cb                                                  *
 *   frees memory allocated for an recnWindow, and other cleanup    *
 *   stuff                                                          *
 *                                                                  *
 * Args:   w    - the widget that called us                         *
 *         data - the data struct for this window                   *
 * Return: none                                                     *
\********************************************************************/
static void
recn_destroy_cb (GtkWidget *w, gpointer data)
{
    auto recnData = static_cast<RecnWindow*>(data);

    recnData->confirmation = NULL;
    start_recn_dialog_cancel (recnData->start_dialog);
    gchar **actions = g_action_group_list_actions (G_ACTION_GROUP(recnData->simple_action_group));
    gint num_actions = g_strv_length (actions);

    gnc_unregister_gui_component_by_data (WINDOW_RECONCILE_CM_CLASS, recnData);

    if (recnData->delete_refresh)
        gnc_resume_gui_refresh ();

    if (recnData->builder)
        g_object_unref(recnData->builder);

    recnData->autoclear_splits.~SplitsVec();
    recnData->initially_cleared_splits.~SplitsVec();

    //Disable the actions, the handlers try to access recnData
    for (gint i = 0; i < num_actions; i++)
    {
        GAction *action = g_action_map_lookup_action (G_ACTION_MAP(recnData->simple_action_group), actions[i]);
        g_simple_action_set_enabled (G_SIMPLE_ACTION(action), FALSE);
    }
    g_strfreev (actions);
    g_clear_object (&recnData->simple_action_group);
    g_free (recnData);
}


static void
recn_confirmation_request_free (RecnConfirmationRequest *request)
{
    if (!request)
        return;

    g_weak_ref_clear (&request->window);
    g_free (request);
}

static void
recn_confirmation_set_actions_enabled (RecnWindow *recnData, gboolean enabled)
{
    static const char *const action_names[] =
    {
        "RecnFinishAction",
        "RecnPostponeAction",
        "RecnCancelAction",
        "TransDeleteAction",
    };

    if (!recnData || !recnData->simple_action_group)
        return;

    if (!enabled)
    {
        for (const auto action_name : action_names)
        {
            auto action = g_action_map_lookup_action (
                G_ACTION_MAP (recnData->simple_action_group), action_name);
            if (action)
                g_simple_action_set_enabled (G_SIMPLE_ACTION (action), FALSE);
        }
        return;
    }

    /* Recalculate restores the Finish action only when the reconciliation is
     * balanced; postpone and cancel remain available after a negative reply. */
    recnRecalculateBalance (recnData);
    gnc_reconcile_window_set_sensitivity (recnData);
    for (const auto action_name : { "RecnPostponeAction", "RecnCancelAction" })
    {
        auto action = g_action_map_lookup_action (
            G_ACTION_MAP (recnData->simple_action_group), action_name);
        if (action)
            g_simple_action_set_enabled (G_SIMPLE_ACTION (action), TRUE);
    }
}

static RecnWindow *
recn_confirmation_get_current (RecnConfirmationRequest *request,
                               GtkWindow **window_out)
{
    GtkWindow *window;
    RecnWindow *recnData;
    Account *account;

    if (window_out)
        *window_out = NULL;
    if (!request || !request->book || request->book != gnc_get_current_book () ||
        qof_book_shutting_down (request->book))
        return NULL;

    window = GTK_WINDOW (g_weak_ref_get (&request->window));
    if (!window)
        return NULL;

    account = xaccAccountLookup (&request->account, request->book);
    recnData = account ? static_cast<RecnWindow *> (
        gnc_find_first_gui_component (WINDOW_RECONCILE_CM_CLASS,
                                      find_by_account, account)) : NULL;
    if (!recnData || recnData->confirmation != request ||
        recnData->window != GTK_WIDGET (window) ||
        !guid_equal (&recnData->account, &request->account) || !account ||
        account != recn_get_account (recnData))
    {
        g_object_unref (window);
        return NULL;
    }

    if (window_out)
        *window_out = window;
    else
        g_object_unref (window);
    return recnData;
}

static void
recn_confirmation_finished (GtkWindow *parent, gint response, gpointer user_data)
{
    auto request = static_cast<RecnConfirmationRequest *> (user_data);
    GtkWindow *window = NULL;
    auto recnData = recn_confirmation_get_current (request, &window);
    auto kind = request ? request->kind : RECN_CONFIRM_CANCEL;
    GncGUID split_guid {};
    GncGUID transaction_guid {};
    auto accepted = recnData && parent == window && response == GTK_RESPONSE_YES;

    if (request)
    {
        split_guid = request->split;
        transaction_guid = request->transaction;
    }
    if (recnData)
    {
        recnData->confirmation = NULL;
        recn_confirmation_set_actions_enabled (recnData, TRUE);
    }

    g_clear_object (&window);
    recn_confirmation_request_free (request);

    if (!accepted)
        return;

    switch (kind)
    {
    case RECN_CONFIRM_CANCEL:
        gnc_close_gui_component_by_data (WINDOW_RECONCILE_CM_CLASS, recnData);
        break;
    case RECN_CONFIRM_FINISH:
        recn_finish (recnData);
        break;
    case RECN_CONFIRM_POSTPONE:
        recn_postpone (recnData);
        break;
    case RECN_CONFIRM_DELETE:
        recn_delete_transaction (recnData, &split_guid, &transaction_guid);
        break;
    }
}

static void
recn_confirm (RecnWindow *recnData, RecnConfirmationKind kind,
              Split *split, const char *message)
{
    Account *account;
    QofBook *book;
    Transaction *transaction = NULL;
    auto request = g_new0 (RecnConfirmationRequest, 1);

    if (!recnData || !recnData->window || recnData->confirmation)
    {
        g_free (request);
        return;
    }

    book = gnc_get_current_book ();
    account = recn_get_account (recnData);
    if (!book || qof_book_shutting_down (book) || !account)
    {
        g_free (request);
        return;
    }

    if (kind == RECN_CONFIRM_DELETE)
    {
        transaction = split ? xaccSplitGetParent (split) : NULL;
        if (!transaction)
        {
            g_free (request);
            return;
        }
        request->split = *xaccSplitGetGUID (split);
        request->transaction = *xaccTransGetGUID (transaction);
    }

    request->book = book;
    request->account = *xaccAccountGetGUID (account);
    request->kind = kind;
    g_weak_ref_init (&request->window, recnData->window);

    recnData->confirmation = request;
    recn_confirmation_set_actions_enabled (recnData, FALSE);
    gnc_verify_dialog_async (GTK_WINDOW (recnData->window), FALSE,
                             recn_confirmation_finished, request, "%s", message);
}

static void
recn_cancel (RecnWindow *recnData)
{
    gboolean changed = FALSE;

    if (!recnData || recnData->confirmation)
        return;

    if (gnc_reconcile_view_changed (GNC_RECONCILE_VIEW (recnData->credit)))
        changed = TRUE;
    if (gnc_reconcile_view_changed (GNC_RECONCILE_VIEW (recnData->debit)))
        changed = TRUE;

    if (changed)
    {
        recn_confirm (recnData, RECN_CONFIRM_CANCEL, NULL,
                      _("You have made changes to this reconcile window. "
                        "Are you sure you want to cancel?"));
        return;
    }

    gnc_close_gui_component_by_data (WINDOW_RECONCILE_CM_CLASS, recnData);
}

static gboolean
recn_close_request_cb (GtkWindow *window, gpointer data)
{
    recn_cancel (static_cast<RecnWindow *> (data));
    (void)window;
    return TRUE;
}

static gboolean
recn_escape_shortcut_cb (GtkWidget *widget, GVariant *args, gpointer data)
{
    recn_cancel (static_cast<RecnWindow *> (data));
    (void)widget;
    (void)args;
    return TRUE;
}


/********************************************************************\
 * find_payment_account                                             *
 *   find an account that 'looks like' a payment account for the    *
 *   given account. This really only makes sense for credit card    *
 *   accounts.                                                      *
 *                                                                  *
 * Args:   account - the account to look in                         *
 * Return: a candidate payment account or NULL if none was found    *
\********************************************************************/
static Account *
find_payment_account(Account *account)
{
    if (account == nullptr)
        return nullptr;

    const auto& splits = xaccAccountGetSplits (account);

    /* Search backwards to find the latest payment */
    for (auto it = splits.rbegin(); it != splits.rend(); it++)
    {
        auto split = *it;

        /* ignore 'purchases' */
        if (!gnc_numeric_positive_p (xaccSplitGetAmount(split)))
            continue;

        for (auto n = xaccTransGetSplitList (xaccSplitGetParent(split)); n; n = n->next)
        {
            auto s = GNC_SPLIT(n->data);
            if (s == split)
                continue;

            auto a = xaccSplitGetAccount(s);
            if (a == account)
                continue;

            auto type = xaccAccountGetType(a);
            if (type == ACCT_TYPE_BANK || type == ACCT_TYPE_CASH || type == ACCT_TYPE_ASSET)
                return a;
        }
    }

    return nullptr;
}

static void
acct_traverse_descendants (Account *acct, std::function<void(Account*)> fn)
{
    fn (acct);
    if (xaccAccountGetReconcileChildrenStatus (acct))
        gnc_account_foreach_descendant (acct, fn);
}

/********************************************************************\
 * recnFinishCB                                                     *
 *   saves reconcile information                                    *
 *                                                                  *
 * Args:   w    - the widget that called us                         *
 *         data - the data struct for this window                   *
 * Return: none                                                     *
\********************************************************************/
static void
recn_finish (RecnWindow *recnData)
{
    gboolean auto_payment;
    Account *account;
    time64 date;

    if (!recnData || !(account = recn_get_account (recnData)))
        return;

    date = recnData->statement_date;
    gnc_suspend_gui_refresh ();

    recnData->delete_refresh = TRUE;
    acct_traverse_descendants (account, xaccAccountBeginEdit);
    gnc_reconcile_view_commit (GNC_RECONCILE_VIEW (recnData->credit), date);
    gnc_reconcile_view_commit (GNC_RECONCILE_VIEW (recnData->debit), date);
    acct_traverse_descendants (account, xaccAccountCommitEdit);

    auto_payment = gnc_prefs_get_bool (GNC_PREFS_GROUP_RECONCILE,
                                        GNC_PREF_AUTO_CC_PAYMENT);
    xaccAccountClearReconcilePostpone (account);
    xaccAccountSetReconcileLastDate (account, date);

    if (auto_payment && xaccAccountGetType (account) == ACCT_TYPE_CREDIT &&
        gnc_numeric_negative_p (recnData->new_ending))
    {
        auto xfer = gnc_xfer_dialog (
            GTK_WIDGET (gnc_ui_get_main_window (recnData->window)), account);
        auto payment_account = find_payment_account (account);

        gnc_xfer_dialog_set_amount (xfer, gnc_numeric_neg (recnData->new_ending));
        if (payment_account)
            gnc_xfer_dialog_select_from_account (xfer, payment_account);
    }

    gnc_close_gui_component_by_data (WINDOW_RECONCILE_CM_CLASS, recnData);
}

static void
recnFinishCB (GSimpleAction *simple,
              GVariant      *parameter,
              gpointer       user_data)
{
    auto recnData = static_cast<RecnWindow *> (user_data);

    (void)simple;
    (void)parameter;
    if (!recnData || recnData->confirmation)
        return;

    if (!gnc_numeric_zero_p (recnRecalculateBalance (recnData)))
    {
        recn_confirm (recnData, RECN_CONFIRM_FINISH, NULL,
                      _("The account is not balanced. Are you sure you want to finish?"));
        return;
    }

    recn_finish (recnData);
}


/********************************************************************\
 * recnPostponeCB                                                   *
 *   saves reconcile information for later use                      *
 *                                                                  *
 * Args:   w    - the widget that called us                         *
 *         data - the data struct for this window                   *
 * Return: none                                                     *
\********************************************************************/
static void
recn_postpone (RecnWindow *recnData)
{
    Account *account;

    if (!recnData || !(account = recn_get_account (recnData)))
        return;

    gnc_suspend_gui_refresh ();
    recnData->delete_refresh = TRUE;
    acct_traverse_descendants (account, xaccAccountBeginEdit);
    gnc_reconcile_view_postpone (GNC_RECONCILE_VIEW (recnData->credit));
    gnc_reconcile_view_postpone (GNC_RECONCILE_VIEW (recnData->debit));
    acct_traverse_descendants (account, xaccAccountCommitEdit);

    xaccAccountSetReconcilePostponeDate (account, recnData->statement_date);
    xaccAccountSetReconcilePostponeBalance (account, recnData->new_ending);
    gnc_close_gui_component_by_data (WINDOW_RECONCILE_CM_CLASS, recnData);
}

static void
recnPostponeCB (GSimpleAction *simple,
                GVariant      *parameter,
                gpointer       user_data)
{
    auto recnData = static_cast<RecnWindow *> (user_data);

    (void)simple;
    (void)parameter;
    if (!recnData || recnData->confirmation)
        return;

    recn_confirm (recnData, RECN_CONFIRM_POSTPONE, NULL,
                  _("Do you want to postpone this reconciliation and finish it later?"));
}


static void
recnCancelCB (GSimpleAction *simple,
              GVariant      *parameter,
              gpointer       user_data)
{
    auto recnData = static_cast<RecnWindow*>(user_data);
    recn_cancel(recnData);
}

/********************************************************************\
 * recnAutoClearCB                                                  *
 *   handles the auto-clear button click                            *
 *                                                                  *
 * Args:   simple     - the action                                  *
 *         parameter  - unused                                      *
 *         user_data  - the reconcile window data                   *
 * Return: none                                                     *
\********************************************************************/
static void
recnAutoClearCB (GSimpleAction *simple,
                 GVariant      *parameter,
                 gpointer       user_data)
{
    auto recnData = static_cast<RecnWindow*>(user_data);

    if (recnData->autoclear_splits.empty())
        return;

    gnc_suspend_gui_refresh ();
    gnc_reconcile_view_unclear_all (GNC_RECONCILE_VIEW(recnData->debit));
    gnc_reconcile_view_unclear_all (GNC_RECONCILE_VIEW(recnData->credit));
    std::for_each (recnData->autoclear_splits.begin(),
                   recnData->autoclear_splits.end(),
                   [recnData](Split* split)
                   {
                       auto view = gnc_numeric_positive_p (xaccSplitGetAmount (split))
                           ? recnData->debit : recnData->credit;
                       gnc_reconcile_view_set_cleared (GNC_RECONCILE_VIEW(view), split);
                   });
    recnRefresh (recnData);
    gnc_resume_gui_refresh ();
}
