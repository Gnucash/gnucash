/********************************************************************\
 * dialog-reconciled-balance.cpp -- reconciled balance dialog          *
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

#include <config.h>

#include <gtk/gtk.h>
#include <glib/gi18n.h>

#include <memory>
#include <string>
#include <vector>

#include "dialog-reconciled-balance.h"

#include "dialog-utils.h"
#include "gnc-amount-edit.h"
#include "gnc-reconciled-balance.h"
#include "gnc-component-manager.h"
#include "gnc-date-edit.h"
#include "gnc-engine.h"
#include "gnc-gnome-utils.h"
#include "gnc-gui-query.h"
#include "gnc-ui.h"
#include "gnc-prefs.h"
#include "gnc-session.h"
#include "gnc-ui-balances.h"
#include "gnc-ui-util.h"

#define DIALOG_RECONCILED_BALANCE_CM_CLASS "dialog-reconciled-balance"
#define GNC_PREFS_GROUP "dialogs.reconciled-balance"

/* Columns of the list store defined in dialog-reconciled-balance.glade */
enum ReconciledBalanceColumn
{
    COL_DATE,
    COL_DATE_INT64,             /* sort key for COL_DATE */
    COL_RECORDED,
    COL_RECORDED_NUM,           /* sort key for COL_RECORDED */
    COL_ACTUAL,
    COL_ACTUAL_NUM,             /* sort key for COL_ACTUAL */
    COL_DIFFERENCE,
    COL_DIFFERENCE_NUM,         /* sort key for COL_DIFFERENCE */
    COL_STATUS_ICON,
    COL_NOTES,
    COL_RECORD,
};

struct ReconciledBalanceDialog
{
    GtkWidget *dialog = nullptr;
    GtkWidget *view = nullptr;
    GtkWidget *date_edit = nullptr;
    GtkWidget *amount_edit = nullptr;
    GtkWidget *notes_entry = nullptr;
    GtkWidget *reseal_button = nullptr;
    GtkWidget *update_button = nullptr;
    GtkWidget *remove_button = nullptr;
    GtkListStore *store = nullptr;

    Account *account = nullptr;
    /* Kept alongside the pointer so that the account can still be
     * identified after it has been deleted from the book. */
    GncGUID acct_guid {};
    /* Set while the entry row is being filled from a record, so that
     * the widgets' own change signals do not fight the fill. */
    bool loading_entry_row = false;
    gint component_id = 0;
    QofSession *session = nullptr;
};

/* This static indicates the debugging module that this .o belongs to. */
static QofLogModule log_module = GNC_MOD_GUI;

/* Handlers named in the .glade file are resolved by name at runtime, so
 * they must not be mangled. */
extern "C"
{
void gnc_reconciled_balance_dialog_add_cb (GtkWidget *widget, gpointer data);
void gnc_reconciled_balance_dialog_update_cb (GtkWidget *widget, gpointer data);
void gnc_reconciled_balance_dialog_reseal_cb (GtkWidget *widget, gpointer data);
void gnc_reconciled_balance_dialog_remove_cb (GtkWidget *widget, gpointer data);
void gnc_reconciled_balance_dialog_selection_changed_cb (GtkTreeSelection *sel,
                                                        gpointer data);
void gnc_reconciled_balance_dialog_response_cb (GtkDialog *dialog, gint response,
                                               gpointer data);
}

static void load_record_into_entry_row (ReconciledBalanceDialog *bad,
                                        GncReconciledBalance *rb);
static void propose_entry_row (ReconciledBalanceDialog *bad);
static void propose_balance (ReconciledBalanceDialog *bad);
static void select_record (ReconciledBalanceDialog *bad,
                           GncReconciledBalance *wanted);

/* =================================================================== */

/* xaccPrintAmount hands back one shared static buffer, so a value has to
 * be copied out before the next call. */
static std::string
print_amount (gnc_numeric amount, GNCPrintAmountInfo pinfo)
{
    return xaccPrintAmount (amount, pinfo);
}

static std::string
print_date (time64 date)
{
    char buf[MAX_DATE_LENGTH + 1];
    qof_print_date_buff (buf, MAX_DATE_LENGTH, date);
    return buf;
}

static void
add_column (GtkTreeView *view, const char *title, int column_id,
            int sort_column_id, bool right_align)
{
    auto renderer = gtk_cell_renderer_text_new ();

    if (right_align)
        gtk_cell_renderer_set_alignment (renderer, 1.0, 0.5);

    auto column = gtk_tree_view_column_new_with_attributes (title, renderer,
                                                            "text", column_id,
                                                            nullptr);
    gtk_tree_view_column_set_resizable (column, TRUE);
    gtk_tree_view_column_set_alignment (column, right_align ? 1.0 : 0.0);
    gtk_tree_view_column_set_sort_column_id (column, sort_column_id);
    gtk_tree_view_append_column (view, column);
}

static void
setup_columns (ReconciledBalanceDialog *bad)
{
    auto view = GTK_TREE_VIEW(bad->view);

    /* The status icon carries no text of its own; the row it sits on
     * spells the discrepancy out. */
    auto renderer = gtk_cell_renderer_pixbuf_new ();
    auto column = gtk_tree_view_column_new_with_attributes ("", renderer,
                                                            "icon-name",
                                                            COL_STATUS_ICON,
                                                            nullptr);
    gtk_tree_view_append_column (view, column);

    add_column (view, _("Date"), COL_DATE, COL_DATE_INT64, false);
    /* Sort on the numeric keys: the visible columns hold formatted
     * strings, which sort lexicographically and put 100 before 20. */
    add_column (view, _("Recorded"), COL_RECORDED, COL_RECORDED_NUM, true);
    add_column (view, _("Actual"), COL_ACTUAL, COL_ACTUAL_NUM, true);
    add_column (view, _("Difference"), COL_DIFFERENCE, COL_DIFFERENCE_NUM, true);
    add_column (view, _("Notes"), COL_NOTES, COL_NOTES, false);
}

static GncReconciledBalance *
get_selected (ReconciledBalanceDialog *bad)
{
    GtkTreeModel *model;
    GtkTreeIter iter;
    GncReconciledBalance *ba = nullptr;

    auto selection = gtk_tree_view_get_selection (GTK_TREE_VIEW(bad->view));
    if (gtk_tree_selection_get_selected (selection, &model, &iter))
        gtk_tree_model_get (model, &iter, COL_RECORD, &ba, -1);

    return ba;
}

static void
refresh_list (ReconciledBalanceDialog *bad)
{
    /* A rebuild drops and restores the selection, which would otherwise
     * reset the entry row -- losing whatever the user was part way
     * through typing when a background change came in. */
    auto was_selected = get_selected (bad);
    auto was_loading = bad->loading_entry_row;
    bad->loading_entry_row = true;

    gtk_list_store_clear (bad->store);

    if (!bad->account)
    {
        bad->loading_entry_row = was_loading;
        return;
    }

    auto pinfo = gnc_account_print_info (bad->account, TRUE);
    auto reverse = gnc_reverse_balance (bad->account);
    auto records = gnc_reconciled_balance_get_for_account (bad->account);

    for (auto node = records; node; node = node->next)
    {
        auto ba = GNC_RECONCILED_BALANCE(node->data);
        auto broken = gnc_reconciled_balance_is_broken (ba);
        auto date = gnc_reconciled_balance_get_date (ba);

        auto recorded = gnc_ui_reconciled_balance_get_display_amount (ba);
        auto actual = gnc_reconciled_balance_get_actual (ba);
        auto difference = gnc_reconciled_balance_get_delta (ba);

        if (reverse)
        {
            actual = gnc_numeric_neg (actual);
            difference = gnc_numeric_neg (difference);
        }

        auto date_str = print_date (date);
        auto recorded_str = print_amount (recorded, pinfo);
        auto actual_str = print_amount (actual, pinfo);
        auto difference_str = broken ? print_amount (difference, pinfo)
                                      : std::string {};

        GtkTreeIter iter;
        gtk_list_store_append (bad->store, &iter);
        gtk_list_store_set (bad->store, &iter,
                            COL_DATE, date_str.c_str(),
                            COL_DATE_INT64, static_cast<gint64>(date),
                            COL_RECORDED, recorded_str.c_str(),
                            COL_RECORDED_NUM, gnc_numeric_to_double (recorded),
                            COL_ACTUAL, actual_str.c_str(),
                            COL_ACTUAL_NUM, gnc_numeric_to_double (actual),
                            COL_DIFFERENCE, difference_str.c_str(),
                            COL_DIFFERENCE_NUM,
                            gnc_numeric_to_double (difference),
                            COL_STATUS_ICON,
                            broken ? "dialog-warning" : "emblem-default",
                            COL_NOTES, gnc_reconciled_balance_get_notes (ba),
                            COL_RECORD, ba,
                            -1);
    }

    g_list_free (records);

    if (was_selected)
        select_record (bad, was_selected);
    bad->loading_entry_row = was_loading;

    gtk_widget_set_sensitive
        (bad->reseal_button,
         gnc_reconciled_balance_count_broken_for_account (bad->account) > 0);

    auto selected = get_selected (bad) != nullptr;
    gtk_widget_set_sensitive (bad->remove_button, selected);
    gtk_widget_set_sensitive (bad->update_button, selected);
}

/* Fill the entry row with the reconciled balance GnuCash currently has
 * for the chosen date, so that "Add" without further typing records
 * what it already believes. The user overtypes the figure from their
 * statement; if the two agree there is nothing to do, and if they don't
 * the record is exactly the record of that disagreement. */
static void
propose_balance (ReconciledBalanceDialog *bad)
{
    if (!bad->account)
        return;

    auto date = gnc_date_edit_get_date (GNC_DATE_EDIT(bad->date_edit));
    auto balance = gnc_reconciled_balance_compute (bad->account, date);

    if (gnc_reverse_balance (bad->account))
        balance = gnc_numeric_neg (balance);

    gnc_amount_edit_set_amount (GNC_AMOUNT_EDIT(bad->amount_edit), balance);
}

/* =================================================================== */
/* Callbacks */

/* Both Add and Update need the typed amount evaluated and complained
 * about in the same way. */
static bool
entry_row_amount_is_valid (ReconciledBalanceDialog *bad)
{
    GError *error = nullptr;

    if (gnc_amount_edit_evaluate (GNC_AMOUNT_EDIT(bad->amount_edit), &error))
        return true;

    gnc_error_dialog (GTK_WINDOW(bad->dialog), "%s",
                      error ? error->message
                            : _("The balance must be a number."));
    g_clear_error (&error);
    return false;
}

/* refresh_list rebuilds the store, so a record the user is working on
 * has to be found again by pointer to keep it selected. */
static void
select_record (ReconciledBalanceDialog *bad, GncReconciledBalance *wanted)
{
    auto model = GTK_TREE_MODEL(bad->store);
    GtkTreeIter iter;

    if (!gtk_tree_model_get_iter_first (model, &iter))
        return;

    do
    {
        GncReconciledBalance *rb = nullptr;
        gtk_tree_model_get (model, &iter, COL_RECORD, &rb, -1);

        if (rb == wanted)
        {
            auto selection = gtk_tree_view_get_selection (GTK_TREE_VIEW(bad->view));
            gtk_tree_selection_select_iter (selection, &iter);
            return;
        }
    }
    while (gtk_tree_model_iter_next (model, &iter));
}

/* Selecting a row is how you edit it: its own figures go into the entry
 * row, and Update writes them back. Nothing is written until Update is
 * pressed, so a stray click is harmless. */
static void
load_record_into_entry_row (ReconciledBalanceDialog *bad,
                            GncReconciledBalance *rb)
{
    bad->loading_entry_row = true;

    gnc_date_edit_set_time (GNC_DATE_EDIT(bad->date_edit),
                            gnc_reconciled_balance_get_date (rb));
    gnc_amount_edit_set_amount (GNC_AMOUNT_EDIT(bad->amount_edit),
                                gnc_ui_reconciled_balance_get_display_amount (rb));
    gtk_entry_set_text (GTK_ENTRY(bad->notes_entry),
                        gnc_reconciled_balance_get_notes (rb));

    bad->loading_entry_row = false;
}

/* With no row selected the entry row describes a record that does not
 * exist yet, so it offers today's reconciled balance to add. */
static void
propose_entry_row (ReconciledBalanceDialog *bad)
{
    bad->loading_entry_row = true;
    gtk_entry_set_text (GTK_ENTRY(bad->notes_entry), "");
    bad->loading_entry_row = false;

    propose_balance (bad);
}

static void
date_changed_cb (GtkWidget *widget, gpointer data)
{
    auto bad = static_cast<ReconciledBalanceDialog*>(data);

    /* Re-proposing while a record's own date is being loaded would
     * overwrite the balance the user is about to edit. */
    if (!bad->loading_entry_row && !get_selected (bad))
        propose_balance (bad);
}

void
gnc_reconciled_balance_dialog_add_cb (GtkWidget *widget, gpointer data)
{
    auto bad = static_cast<ReconciledBalanceDialog*>(data);

    if (!bad->account)
        return;

    if (!entry_row_amount_is_valid (bad))
        return;

    auto ba = gnc_reconciled_balance_new (gnc_get_current_book ());
    gnc_reconciled_balance_set_account (ba, bad->account);
    gnc_reconciled_balance_set_date
        (ba, gnc_date_edit_get_date (GNC_DATE_EDIT(bad->date_edit)));
    gnc_ui_reconciled_balance_set_display_amount
        (ba, gnc_amount_edit_get_amount (GNC_AMOUNT_EDIT(bad->amount_edit)));
    gnc_reconciled_balance_set_notes
        (ba, gtk_entry_get_text (GTK_ENTRY(bad->notes_entry)));

    refresh_list (bad);
    select_record (bad, ba);
}

void
gnc_reconciled_balance_dialog_update_cb (GtkWidget *widget, gpointer data)
{
    auto bad = static_cast<ReconciledBalanceDialog*>(data);
    auto rb = get_selected (bad);

    if (!rb || !entry_row_amount_is_valid (bad))
        return;

    gnc_reconciled_balance_set_date
        (rb, gnc_date_edit_get_date (GNC_DATE_EDIT(bad->date_edit)));
    gnc_ui_reconciled_balance_set_display_amount
        (rb, gnc_amount_edit_get_amount (GNC_AMOUNT_EDIT(bad->amount_edit)));
    gnc_reconciled_balance_set_notes
        (rb, gtk_entry_get_text (GTK_ENTRY(bad->notes_entry)));

    refresh_list (bad);
    select_record (bad, rb);
}

void
gnc_reconciled_balance_dialog_reseal_cb (GtkWidget *widget, gpointer data)
{
    auto bad = static_cast<ReconciledBalanceDialog*>(data);

    if (!bad->account)
        return;

    auto broken = gnc_reconciled_balance_count_broken_for_account (bad->account);
    if (broken == 0)
        return;

    auto pinfo = gnc_account_print_info (bad->account, TRUE);
    gnc_numeric shared;
    std::string detail;

    /* Records all out by the same amount are the signature of one
     * back-dated transaction -- a cheque entered late, most often --
     * rather than of scattered damage. Saying so is the difference
     * between the user reading this and clicking through it. */
    if (gnc_ui_account_broken_balances_share_delta (bad->account, &shared))
        detail = std::string (_("They are all out by the same amount, ")) +
                 print_amount (shared, pinfo) +
                 _(", which usually means one transaction was entered "
                   "after the fact with an earlier date.");
    else
        detail = _("They are out by differing amounts, which means more than "
                   "one thing has changed. It is worth looking at them before "
                   "accepting.");

    auto question = g_strdup_printf
        (ngettext ("Re-record %d balance at what the book holds now?",
                   "Re-record %d balances at what the book holds now?", broken),
         broken);

    auto proceed = gnc_verify_dialog (GTK_WINDOW(bad->dialog), FALSE, "%s\n\n%s",
                                      question, detail.c_str());
    g_free (question);

    if (proceed)
    {
        gnc_ui_account_reseal_reconciled_balances (bad->account);
        refresh_list (bad);
    }
}

void
gnc_reconciled_balance_dialog_remove_cb (GtkWidget *widget, gpointer data)
{
    auto bad = static_cast<ReconciledBalanceDialog*>(data);

    if (auto ba = get_selected (bad))
    {
        gnc_reconciled_balance_destroy (ba);
        refresh_list (bad);
        propose_entry_row (bad);
    }
}

void
gnc_reconciled_balance_dialog_selection_changed_cb (GtkTreeSelection *selection,
                                                   gpointer data)
{
    auto bad = static_cast<ReconciledBalanceDialog*>(data);
    auto rb = get_selected (bad);

    gtk_widget_set_sensitive (bad->remove_button, rb != nullptr);
    gtk_widget_set_sensitive (bad->update_button, rb != nullptr);

    if (bad->loading_entry_row)
        return;

    if (rb)
        load_record_into_entry_row (bad, rb);
    else
        propose_entry_row (bad);
}

void
gnc_reconciled_balance_dialog_response_cb (GtkDialog *dialog, gint response,
                                          gpointer data)
{
    auto bad = static_cast<ReconciledBalanceDialog*>(data);

    gnc_save_window_size (GNC_PREFS_GROUP, GTK_WINDOW(bad->dialog));
    gnc_close_gui_component (bad->component_id);
}

/* =================================================================== */
/* Component manager glue */

static void
refresh_handler (GHashTable *changes, gpointer user_data)
{
    auto bad = static_cast<ReconciledBalanceDialog*>(user_data);

    /* The account may have been deleted from under us, which would
     * leave bad->account dangling -- so resolve the guid, never the
     * stale pointer. */
    bad->account = xaccAccountLookup (&bad->acct_guid, gnc_get_current_book ());
    if (!bad->account)
    {
        gnc_close_gui_component (bad->component_id);
        return;
    }

    refresh_list (bad);
}

static void
close_handler (gpointer user_data)
{
    auto bad = static_cast<ReconciledBalanceDialog*>(user_data);

    gnc_unregister_gui_component (bad->component_id);
    gtk_widget_destroy (bad->dialog);
    delete bad;
}

static gboolean
find_by_account (gpointer find_data, gpointer user_data)
{
    auto bad = static_cast<ReconciledBalanceDialog*>(user_data);

    return bad && bad->account == find_data;
}

/* =================================================================== */

static ReconciledBalanceDialog *
create_dialog (GtkWindow *parent, Account *account)
{
    auto bad = std::make_unique<ReconciledBalanceDialog>();
    bad->account = account;
    bad->acct_guid = *xaccAccountGetGUID (account);
    bad->session = gnc_get_current_session ();

    auto builder = gtk_builder_new ();
    gnc_builder_add_from_file (builder, "dialog-reconciled-balance.glade",
                               "reconciled_balance_liststore");
    gnc_builder_add_from_file (builder, "dialog-reconciled-balance.glade",
                               "reconciled_balance_dialog");

    auto get_widget = [builder](const char *name)
    {
        return GTK_WIDGET(gtk_builder_get_object (builder, name));
    };

    bad->dialog = get_widget ("reconciled_balance_dialog");
    bad->view = get_widget ("rb_treeview");
    bad->notes_entry = get_widget ("rb_notes_entry");
    bad->reseal_button = get_widget ("rb_reseal_button");
    bad->update_button = get_widget ("rb_update_button");
    bad->remove_button = get_widget ("rb_remove_button");
    bad->store = GTK_LIST_STORE(gtk_builder_get_object (builder,
                                                        "reconciled_balance_liststore"));

    auto fullname = gnc_account_get_full_name (account);
    std::string name {fullname};
    g_free (fullname);

    auto title = g_strdup_printf (_("Reconciled Balances — %s"), name.c_str());
    gtk_window_set_title (GTK_WINDOW(bad->dialog), title);
    g_free (title);

    gtk_label_set_text (GTK_LABEL(get_widget ("rb_account_label")),
                        name.c_str());

    if (parent)
        gtk_window_set_transient_for (GTK_WINDOW(bad->dialog), parent);

    /* Date */
    bad->date_edit = gnc_date_edit_new (gnc_time (nullptr), FALSE, FALSE);
    gtk_box_pack_start (GTK_BOX(get_widget ("rb_date_box")), bad->date_edit,
                        TRUE, TRUE, 0);
    gtk_widget_show (bad->date_edit);
    gnc_date_make_mnemonic_target (GNC_DATE_EDIT(bad->date_edit),
                                   get_widget ("rb_date_label"));
    g_signal_connect (G_OBJECT(bad->date_edit), "date_changed",
                      G_CALLBACK(date_changed_cb), bad.get());

    /* Amount */
    bad->amount_edit = gnc_amount_edit_new ();
    gnc_amount_edit_set_evaluate_on_enter (GNC_AMOUNT_EDIT(bad->amount_edit),
                                           TRUE);
    gnc_amount_edit_set_print_info (GNC_AMOUNT_EDIT(bad->amount_edit),
                                    gnc_account_print_info (account, FALSE));
    gnc_amount_edit_set_fraction (GNC_AMOUNT_EDIT(bad->amount_edit),
                                  xaccAccountGetCommoditySCU (account));
    gtk_box_pack_start (GTK_BOX(get_widget ("rb_amount_box")), bad->amount_edit,
                        TRUE, TRUE, 0);
    gtk_widget_show (bad->amount_edit);
    gnc_amount_edit_make_mnemonic_target (GNC_AMOUNT_EDIT(bad->amount_edit),
                                          get_widget ("rb_amount_label"));

    setup_columns (bad.get());

    auto selection = gtk_tree_view_get_selection (GTK_TREE_VIEW(bad->view));
    g_signal_connect (G_OBJECT(selection), "changed",
                      G_CALLBACK(gnc_reconciled_balance_dialog_selection_changed_cb),
                      bad.get());

    gtk_builder_connect_signals_full (builder, gnc_builder_connect_full_func,
                                      bad.get());
    g_object_unref (G_OBJECT(builder));

    bad->component_id =
        gnc_register_gui_component (DIALOG_RECONCILED_BALANCE_CM_CLASS,
                                    refresh_handler, close_handler, bad.get());
    gnc_gui_component_set_session (bad->component_id, bad->session);

    /* Anything that moves a balance, or changes what is reconciled, can
     * break a record -- so watch transactions and splits as well as the
     * records themselves. */
    const QofEventId all_events =
        QOF_EVENT_CREATE | QOF_EVENT_MODIFY | QOF_EVENT_DESTROY;

    for (auto type : { GNC_ID_RECONCILED_BALANCE, GNC_ID_TRANS, GNC_ID_SPLIT })
        gnc_gui_component_watch_entity_type (bad->component_id, type,
                                             all_events);

    gnc_gui_component_watch_entity_type (bad->component_id, GNC_ID_ACCOUNT,
                                         QOF_EVENT_MODIFY | QOF_EVENT_DESTROY);

    gnc_restore_window_size (GNC_PREFS_GROUP, GTK_WINDOW(bad->dialog), parent);

    /* The component manager owns it from here; close_handler deletes it. */
    return bad.release();
}

static void
present_dialog (GtkWindow *parent, Account *account, bool have_date, time64 date)
{
    g_return_if_fail (GNC_IS_ACCOUNT(account));

    auto bad = static_cast<ReconciledBalanceDialog*>
        (gnc_find_first_gui_component (DIALOG_RECONCILED_BALANCE_CM_CLASS,
                                       find_by_account, account));
    if (!bad)
        bad = create_dialog (parent, account);

    refresh_list (bad);

    /* Leave the entry row alone if it is showing a record the user
     * selected; otherwise offer the requested date and its balance. */
    if (!get_selected (bad))
    {
        if (have_date)
            gnc_date_edit_set_time (GNC_DATE_EDIT(bad->date_edit), date);
        propose_balance (bad);
    }

    gtk_widget_show_all (bad->dialog);
    gtk_window_present (GTK_WINDOW(bad->dialog));
}

void
gnc_reconciled_balance_dialog (GtkWindow *parent, Account *account)
{
    ENTER (" ");
    present_dialog (parent, account, false, 0);
    LEAVE (" ");
}

void
gnc_reconciled_balance_dialog_for_date (GtkWindow *parent, Account *account,
                                       time64 date)
{
    ENTER (" ");
    present_dialog (parent, account, true, date);
    LEAVE (" ");
}
