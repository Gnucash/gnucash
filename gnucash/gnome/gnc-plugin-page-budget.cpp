/********************************************************************
 * gnc-plugin-page-budget.c -- Budget plugin based on               *
 *                             gnc-plugin-page-account-tree.c       *
 *                                                                  *
 * Copyright (C) 2005, Chris Shoemaker <c.shoemaker@cox.net>        *
 * Copyright (C) 2011, Robert Fewell                                *
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
 *******************************************************************/

/*
 * TODO:
 *
 * *) I'd like to be able to update the budget estimates on a per cell
 * basis, instead of a whole row (account) at one time.  But, that
 * would require some major coding.
 *
 */

#include <config.h>
#include <cstdint>

#include <gtk/gtk.h>
#ifdef __G_IR_SCANNER__
#undef __G_IR_SCANNER__
#endif
#include <gdk/gdkkeysyms.h>
#include <glib/gi18n.h>
#include "gnc-date-edit.h"

#include "swig-runtime.h"
#include "libguile.h"
#include <guile-mappings.h>

#include "gnc-plugin-page-register.h"
#include "gnc-plugin-page-report.h"
#include "gnc-budget.h"
#include "gnc-features.h"
#include "qofevent.h"

#include "dialog-utils.h"
#include "gnc-gui-query.h"
#include "gnc-gnome-utils.h"
#include "misc-gnome-utils.h"
#include "gnc-gobject-utils.h"
#include "gnc-icons.h"
#include "gnc-plugin-page-budget.h"
#include "gnc-plugin-budget.h"
#include "gnc-budget-view.h"

#include "gnc-session.h"
#include "gnc-tree-view-account.h"
#include "gnc-ui.h"
#include "gnc-ui-util.h"
#include "gnc-window.h"
#include "gnc-main-window.h"
#include "gnc-component-manager.h"

#include "qof.h"

#include "gnc-recurrence.h"
#include "Recurrence.h"
#include "gnc-tree-model-account-types.h"


/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_BUDGET;

#define PLUGIN_PAGE_BUDGET_CM_CLASS "plugin-page-budget"

/************************************************************
 *                        Prototypes                        *
 ************************************************************/
/* Plugin Actions */
static void gnc_plugin_page_budget_finalize (GObject *object);

static GtkWidget *
gnc_plugin_page_budget_create_widget (GncPluginPage *plugin_page);
static gboolean gnc_plugin_page_budget_focus_widget (GncPluginPage *plugin_page);
static void gnc_plugin_page_budget_destroy_widget (GncPluginPage *plugin_page);
static void gnc_plugin_page_budget_save_page (GncPluginPage *plugin_page,
                                              GKeyFile *file,
                                              const gchar *group);
static GncPluginPage *gnc_plugin_page_budget_recreate_page (GtkWidget *window,
                                                            GKeyFile *file,
                                                            const gchar *group);
static gboolean gppb_button_press_cb (GtkWidget *widget,
                                      GdkEventButton *event,
                                      GncPluginPage *page);
static void gppb_account_activated_cb (GncBudgetView* view,
                                       Account* account,
                                       GncPluginPageBudget *page);
#if 0
static void gppb_selection_changed_cb (GtkTreeSelection *selection,
                                       GncPluginPageBudget *page);
#endif

static void gnc_plugin_page_budget_cmd_view_filter_by (GSimpleAction *simple, GVariant *parameter, gpointer user_data);
static void gnc_plugin_page_budget_cmd_open_account (GSimpleAction *simple, GVariant *parameter, gpointer user_data);
static void gnc_plugin_page_budget_cmd_open_subaccounts (GSimpleAction *simple, GVariant *parameter, gpointer user_data);
static void gnc_plugin_page_budget_cmd_delete_budget (GSimpleAction *simple, GVariant *parameter, gpointer user_data);
static void gnc_plugin_page_budget_cmd_view_options (GSimpleAction *simple, GVariant *parameter, gpointer user_data);
static void gnc_plugin_page_budget_cmd_estimate_budget (GSimpleAction *simple, GVariant *parameter, gpointer user_data);
static void gnc_plugin_page_budget_cmd_allperiods_budget (GSimpleAction *simple, GVariant *parameter, gpointer user_data);
static void gnc_plugin_page_budget_cmd_refresh (GSimpleAction *simple, GVariant *parameter, gpointer user_data);
static void gnc_plugin_page_budget_cmd_budget_note (GSimpleAction *simple, GVariant *parameter, gpointer user_data);
struct BudgetNoteRequest
{
    GWeakRef page;
    GWeakRef book;
    GtkDialog *dialog;
    GtkTextView *note;
    GncGUID budget_guid;
    GncGUID account_guid;
    std::uint32_t period_num;
    gchar *text{};
};

static void gnc_plugin_page_budget_cmd_budget_report (GSimpleAction *simple, GVariant *parameter, gpointer user_data);
static void gnc_plugin_page_budget_cmd_edit_tax_options (GSimpleAction *simple, GVariant *parameter, gpointer user_data);

static GActionEntry gnc_plugin_page_budget_actions [] =
{
    { "OpenAccountAction", gnc_plugin_page_budget_cmd_open_account, NULL, NULL, NULL },
    { "OpenSubaccountsAction", gnc_plugin_page_budget_cmd_open_subaccounts, NULL, NULL, NULL },
    { "DeleteBudgetAction", gnc_plugin_page_budget_cmd_delete_budget, NULL, NULL, NULL },
    { "OptionsBudgetAction", gnc_plugin_page_budget_cmd_view_options, NULL, NULL, NULL },
    { "EstimateBudgetAction", gnc_plugin_page_budget_cmd_estimate_budget, NULL, NULL, NULL },
    { "AllPeriodsBudgetAction", gnc_plugin_page_budget_cmd_allperiods_budget, NULL, NULL, NULL },
    { "BudgetNoteAction", gnc_plugin_page_budget_cmd_budget_note, NULL, NULL, NULL },
    { "BudgetReportAction", gnc_plugin_page_budget_cmd_budget_report, NULL, NULL, NULL },
    { "ViewFilterByAction", gnc_plugin_page_budget_cmd_view_filter_by, NULL, NULL, NULL },
    { "ViewRefreshAction", gnc_plugin_page_budget_cmd_refresh, NULL, NULL, NULL },
    { "EditTaxOptionsAction", gnc_plugin_page_budget_cmd_edit_tax_options, NULL, NULL, NULL },
};
static guint gnc_plugin_page_budget_n_actions = G_N_ELEMENTS(gnc_plugin_page_budget_actions);

/** The default menu items that need to be add to the menu */
static const gchar *gnc_plugin_load_ui_items [] =
{
    "FilePlaceholder3",
    "EditPlaceholder1",
    "EditPlaceholder3",
    "EditPlaceholder5",
    "EditPlaceholder6",
    "ViewPlaceholder1",
    "ViewPlaceholder4",
    NULL,
};

static const gchar *writeable_actions[] =
{
    /* actions which must be disabled on a readonly book. */
    "DeleteBudgetAction",
    "OptionsBudgetAction",
    "EstimateBudgetAction",
    "AllPeriodsBudgetAction",
    "BudgetNoteAction",
    NULL
};

#if 0
static const gchar *actions_requiring_account[] =
{
    "OpenAccountAction",
    "OpenSubaccountsAction",
    NULL
};
#endif

/** Short labels for use on the toolbar buttons. */
static GncToolBarShortNames toolbar_labels[] =
{
    { "OpenAccountAction",          N_("Open") },
    { "DeleteBudgetAction",         N_("Delete") },
    { "OptionsBudgetAction",        N_("Options") },
    { "EstimateBudgetAction",       N_("Estimate") },
    { "AllPeriodsBudgetAction",     N_("All Periods") },
    { "BudgetNoteAction",           N_("Note") },
    { "BudgetReportAction",         N_("Run Report") },
    { NULL, NULL },
};

typedef enum allperiods_action
{
    REPLACE,
    ADD,
    MULTIPLY,
    UNSET
} allperiods_action;

typedef struct GncPluginPageBudgetPrivate
{
    GtkBuilder   *builder;
    GSimpleActionGroup *simple_action_group;

    GncBudgetView* budget_view;
    GtkTreeView *tree_view;

    gint component_id;

    GncBudget* budget;
    GncGUID key;
    GtkWidget *dialog;
    /* To distinguish between closing a tab and deleting a budget */
    gboolean delete_budget;

    AccountFilterDialog fd;

    /* For the estimation dialog */
    Recurrence r;
    gint sigFigs;
    gboolean useAvg;

    /* For the allPeriods value dialog */
    gnc_numeric allValue;
    allperiods_action action;

    /* the cached reportPage for this budget. note this is not saved
       into .gcm file therefore the budget editor->report link is lost
       upon restart. */
    GncPluginPage *reportPage;
} GncPluginPageBudgetPrivate;

G_DEFINE_TYPE_WITH_PRIVATE(GncPluginPageBudget, gnc_plugin_page_budget, GNC_TYPE_PLUGIN_PAGE)

#define GNC_PLUGIN_PAGE_BUDGET_GET_PRIVATE(o)  \
   ((GncPluginPageBudgetPrivate*)gnc_plugin_page_budget_get_instance_private((GncPluginPageBudget*)o))

typedef enum
{
    BUDGET_OPTIONS_REQUEST,
    BUDGET_ESTIMATE_REQUEST,
    BUDGET_ALL_PERIODS_REQUEST
} BudgetMutationKind;

struct BudgetMutationRequest
{
    GWeakRef page;
    GWeakRef book;
    GWeakRef owner;
    GtkDialog *dialog;
    GncGUID budget_guid;
    GPtrArray *account_guids;
    BudgetMutationKind kind;
    bool captured;
    GtkWidget *name;
    GtkWidget *description;
    GtkWidget *recurrence;
    GtkWidget *periods;
    GtkWidget *show_code;
    GtkWidget *show_description;
    GtkWidget *date;
    GtkWidget *digits;
    GtkWidget *average;
    GtkWidget *value;
    GtkWidget *add;
    GtkWidget *multiply;
    gchar *name_text;
    gchar *description_text;
    Recurrence recurrence_value;
    std::int32_t period_count;
    bool show_code_value;
    bool show_description_value;
    GDate date_value;
    std::int32_t digits_value;
    bool average_value;
    gchar *amount_text;
    gnc_numeric amount_value;
    bool amount_valid;
    allperiods_action action;
};

static void budget_mutation_complete (GtkWindow *parent, gint response,
                                      gpointer user_data);

static void
budget_mutation_capture ([[maybe_unused]] GtkDialog *dialog, gint response,
                         BudgetMutationRequest *request)
{
    if (response != GTK_RESPONSE_OK)
        return;
    request->captured = true;
    switch (request->kind)
    {
    case BUDGET_OPTIONS_REQUEST:
    {
        request->name_text = g_strdup (gtk_entry_get_text (GTK_ENTRY (request->name)));
        GtkTextBuffer *buffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW (request->description));
        GtkTextIter start, end;
        gtk_text_buffer_get_bounds (buffer, &start, &end);
        request->description_text = gtk_text_buffer_get_text (buffer, &start, &end, TRUE);
        request->recurrence_value = *gnc_recurrence_get (GNC_RECURRENCE (request->recurrence));
        request->period_count = gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON (request->periods));
        request->show_code_value = gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (request->show_code));
        request->show_description_value = gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (request->show_description));
        break;
    }
    case BUDGET_ESTIMATE_REQUEST:
        gnc_date_edit_get_gdate (GNC_DATE_EDIT (request->date), &request->date_value);
        request->digits_value = gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON (request->digits));
        request->average_value = gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (request->average));
        break;
    case BUDGET_ALL_PERIODS_REQUEST:
        request->amount_text = g_strdup (gtk_entry_get_text (GTK_ENTRY (request->value)));
        request->digits_value = gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON (request->digits));
        request->action = REPLACE;
        if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (request->add)))
            request->action = ADD;
        else if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (request->multiply)))
            request->action = MULTIPLY;
        if (request->action == REPLACE &&
            !gtk_entry_get_text_length (GTK_ENTRY (request->value)))
            request->action = UNSET;
        request->amount_valid = xaccParseAmount (request->amount_text, TRUE,
                                                 &request->amount_value, NULL);
        break;
    }
}

static void
budget_mutation_snapshot_account ([[maybe_unused]] GtkTreeModel *model,
                                  GtkTreePath *path,
                                  [[maybe_unused]] GtkTreeIter *iter,
                                  gpointer user_data)
{
    auto request = static_cast<BudgetMutationRequest *> (user_data);
    auto page = GNC_PLUGIN_PAGE_BUDGET (g_weak_ref_get (&request->page));
    if (!page)
        return;
    auto priv = GNC_PLUGIN_PAGE_BUDGET_GET_PRIVATE (page);
    auto account = gnc_budget_view_get_account_from_path (priv->budget_view, path);
    if (account)
    {
        auto guid = g_new (GncGUID, 1);
        *guid = *qof_instance_get_guid (QOF_INSTANCE (account));
        g_ptr_array_add (request->account_guids, guid);
    }
    g_object_unref (page);
}

static BudgetMutationRequest *
budget_mutation_request_new (GncPluginPageBudget *page, GtkWidget *dialog,
                            BudgetMutationKind kind)
{
    auto priv = GNC_PLUGIN_PAGE_BUDGET_GET_PRIVATE (page);
    auto book = qof_instance_get_book (QOF_INSTANCE (priv->budget));
    auto owner = gnc_plugin_page_get_window (GNC_PLUGIN_PAGE (page));
    if (!book || !owner || qof_book_is_readonly (book) ||
        gnc_get_current_book () != book)
        return nullptr;

    auto request = g_new0 (BudgetMutationRequest, 1);
    g_weak_ref_init (&request->page, G_OBJECT (page));
    g_weak_ref_init (&request->book, G_OBJECT (book));
    g_weak_ref_init (&request->owner, G_OBJECT (owner));
    request->dialog = GTK_DIALOG (g_object_ref (dialog));
    request->budget_guid = *gnc_budget_get_guid (priv->budget);
    request->account_guids = g_ptr_array_new_with_free_func (g_free);
    request->kind = kind;
    gtk_window_set_destroy_with_parent (GTK_WINDOW (dialog), TRUE);
    g_signal_connect (dialog, "response", G_CALLBACK (budget_mutation_capture), request);
    gnc_gui_query_bind_dialog_response (GTK_DIALOG (dialog),
                                       budget_mutation_complete, request);
    return request;
}

static bool
budget_mutation_request_valid (BudgetMutationRequest *request,
                               GncPluginPageBudget *page, QofBook *book,
                               GtkWindow *owner, GncBudget **budget_out)
{
    auto priv = GNC_PLUGIN_PAGE_BUDGET_GET_PRIVATE (page);
    auto budget = book ? gnc_budget_lookup (&request->budget_guid, book) : nullptr;
    if (!book || !owner || !budget || !priv->budget_view ||
        priv->budget != budget || GNC_PLUGIN_PAGE (page)->window != GTK_WIDGET (owner) ||
        gtk_widget_in_destruction (GTK_WIDGET (owner)) ||
        gnc_get_current_book () != book || qof_book_is_readonly (book) ||
        qof_book_shutting_down (book) ||
        qof_instance_get_book (QOF_INSTANCE (budget)) != book)
        return false;
    *budget_out = budget;
    return true;
}

static void
budget_mutation_request_free (BudgetMutationRequest *request)
{
    g_signal_handlers_disconnect_by_data (request->dialog, request);
    g_clear_object (&request->dialog);
    g_weak_ref_clear (&request->page);
    g_weak_ref_clear (&request->book);
    g_weak_ref_clear (&request->owner);
    g_clear_pointer (&request->account_guids, g_ptr_array_unref);
    g_clear_pointer (&request->name_text, g_free);
    g_clear_pointer (&request->description_text, g_free);
    g_clear_pointer (&request->amount_text, g_free);
    g_free (request);
}

static void
budget_estimate_account (GncBudget *budget, Account *account,
                         const Recurrence *recurrence, std::int32_t sigfigs,
                         bool use_average)
{
    auto periods = gnc_budget_get_num_periods (budget);
    if (use_average && periods)
    {
        auto amount = xaccAccountGetNoclosingBalanceChangeForPeriod (
            account, recurrenceGetPeriodTime (recurrence, 0, FALSE),
            recurrenceGetPeriodTime (recurrence, periods - 1, TRUE), TRUE);
        amount = gnc_numeric_div (amount, gnc_numeric_create (periods, 1),
                                  GNC_DENOM_AUTO,
                                  GNC_HOW_DENOM_SIGFIGS (sigfigs) |
                                  GNC_HOW_RND_ROUND_HALF_UP);
        for (std::uint32_t period = 0; period < periods; ++period)
            gnc_budget_set_account_period_value (budget, account, period, amount);
        return;
    }
    for (std::uint32_t period = 0; period < periods; ++period)
    {
        auto amount = xaccAccountGetNoclosingBalanceChangeForPeriod (
            account, recurrenceGetPeriodTime (recurrence, period, FALSE),
            recurrenceGetPeriodTime (recurrence, period, TRUE), TRUE);
        if (!gnc_numeric_check (amount))
        {
            amount = gnc_numeric_convert (amount, GNC_DENOM_AUTO,
                                          GNC_HOW_DENOM_SIGFIGS (sigfigs) |
                                          GNC_HOW_RND_ROUND_HALF_UP);
            gnc_budget_set_account_period_value (budget, account, period, amount);
        }
    }
}

static void
budget_mutation_complete ([[maybe_unused]] GtkWindow *parent, gint response,
                          gpointer user_data)
{
    auto request = static_cast<BudgetMutationRequest *> (user_data);
    auto page = GNC_PLUGIN_PAGE_BUDGET (g_weak_ref_get (&request->page));
    auto book = static_cast<QofBook *> (g_weak_ref_get (&request->book));
    auto owner = GTK_WINDOW (g_weak_ref_get (&request->owner));
    GncBudget *budget = nullptr;
    const bool valid = response == GTK_RESPONSE_OK && request->captured &&
        page && budget_mutation_request_valid (request, page, book, owner, &budget);
    if (valid)
    {
        g_object_ref (budget);
        auto priv = GNC_PLUGIN_PAGE_BUDGET_GET_PRIVATE (page);
        auto view = GNC_BUDGET_VIEW (g_object_ref (priv->budget_view));
        gchar *updated_page_label = nullptr;
        bool budget_modified = false;
        gnc_suspend_gui_refresh ();
        qof_event_suspend ();
        switch (request->kind)
        {
        case BUDGET_OPTIONS_REQUEST:
        {
            budget_modified = true;
            gnc_budget_begin_edit (budget);
            gnc_budget_set_name (budget, request->name_text);
            gnc_budget_set_description (budget, request->description_text);
            gnc_budget_view_set_show_account_code (view, request->show_code_value);
            gnc_budget_view_set_show_account_description (view,
                                                          request->show_description_value);
            if ((request->show_code_value || request->show_description_value) &&
                !gnc_features_check_used (book,
                    GNC_FEATURE_BUDGET_SHOW_EXTRA_ACCOUNT_COLS))
                gnc_features_set_used (book,
                    GNC_FEATURE_BUDGET_SHOW_EXTRA_ACCOUNT_COLS);
            gnc_budget_set_num_periods (budget, request->period_count);
            gnc_budget_set_recurrence (budget, &request->recurrence_value);
            updated_page_label = g_strdup_printf ("%s: %s", _("Budget"),
                                                 request->name_text);
            gnc_budget_commit_edit (budget);
            break;
        }
        case BUDGET_ESTIMATE_REQUEST:
        {
            const auto recurrence = gnc_budget_get_recurrence (budget);
            recurrenceSet (&priv->r, recurrenceGetMultiplier (recurrence),
                           recurrenceGetPeriodType (recurrence),
                           &request->date_value,
                           recurrenceGetWeekendAdjust (recurrence));
            priv->sigFigs = request->digits_value;
            priv->useAvg = request->average_value;
            budget_modified = request->account_guids->len != 0;
            gnc_budget_begin_edit (budget);
            for (std::uint32_t i = 0; i < request->account_guids->len; ++i)
            {
                auto guid = static_cast<GncGUID *> (
                    g_ptr_array_index (request->account_guids, i));
                auto account = xaccAccountLookup (guid, book);
                if (account && qof_instance_get_book (QOF_INSTANCE (account)) == book)
                    budget_estimate_account (budget, account, &priv->r,
                                             request->digits_value,
                                             request->average_value);
            }
            gnc_budget_commit_edit (budget);
            break;
        }
        case BUDGET_ALL_PERIODS_REQUEST:
            if (request->action == UNSET || request->amount_valid)
            {
                priv->sigFigs = request->digits_value;
                priv->action = request->action;
                priv->allValue = request->amount_value;
                budget_modified = request->account_guids->len != 0 &&
                    gnc_budget_get_num_periods (budget) != 0;
                gnc_budget_begin_edit (budget);
                for (std::uint32_t i = 0; i < request->account_guids->len; ++i)
                {
                    auto guid = static_cast<GncGUID *> (
                        g_ptr_array_index (request->account_guids, i));
                    auto account = xaccAccountLookup (guid, book);
                    if (!account || qof_instance_get_book (QOF_INSTANCE (account)) != book)
                        continue;
                    auto allvalue = request->amount_value;
                    if (gnc_reverse_balance (account))
                        allvalue = gnc_numeric_neg (allvalue);
                    for (std::uint32_t period = 0; period < gnc_budget_get_num_periods (budget); ++period)
                    {
                        gnc_numeric value = allvalue;
                        switch (request->action)
                        {
                        case ADD:
                            value = gnc_numeric_add (
                                gnc_budget_get_account_period_value (budget, account, period),
                                allvalue, GNC_DENOM_AUTO,
                                GNC_HOW_DENOM_SIGFIGS (request->digits_value) |
                                GNC_HOW_RND_ROUND_HALF_UP);
                            gnc_budget_set_account_period_value (budget, account, period, value);
                            break;
                        case MULTIPLY:
                            value = gnc_numeric_mul (
                                gnc_budget_get_account_period_value (budget, account, period),
                                request->amount_value, GNC_DENOM_AUTO,
                                GNC_HOW_DENOM_SIGFIGS (request->digits_value) |
                                GNC_HOW_RND_ROUND_HALF_UP);
                            gnc_budget_set_account_period_value (budget, account, period, value);
                            break;
                        case UNSET:
                            gnc_budget_unset_account_period_value (budget, account, period);
                            break;
                        default:
                            gnc_budget_set_account_period_value (budget, account, period, value);
                            break;
                        }
                    }
                }
                gnc_budget_commit_edit (budget);
            }
            break;
        }
        /* Budget setters generate MODIFY for every field. Suppress those
         * intermediate events so observers cannot tear down the page halfway
        * through a logically atomic user edit. */
        qof_event_resume ();
        if (budget_modified)
            qof_event_gen (QOF_INSTANCE (budget), QOF_EVENT_MODIFY, nullptr);
        gnc_resume_gui_refresh ();
        g_object_unref (view);
        GncBudget *current_budget = nullptr;
        if (updated_page_label && budget_mutation_request_valid (
                request, page, book, owner, &current_budget))
            main_window_update_page_name (GNC_PLUGIN_PAGE (page),
                                          updated_page_label);
        g_free (updated_page_label);
        g_object_unref (budget);
    }
    if (page && request->kind == BUDGET_OPTIONS_REQUEST)
    {
        auto priv = GNC_PLUGIN_PAGE_BUDGET_GET_PRIVATE (page);
        if (priv->dialog == GTK_WIDGET (request->dialog))
            priv->dialog = NULL;
    }
    g_clear_object (&owner);
    g_clear_object (&book);
    g_clear_object (&page);
    budget_mutation_request_free (request);
}

static void
budget_note_capture ([[maybe_unused]] GtkDialog *dialog, gint response,
                     BudgetNoteRequest *request)
{
    if (response == GTK_RESPONSE_OK)
        request->text = xxxgtk_textview_get_text (request->note);
}

static void
budget_note_complete ([[maybe_unused]] GtkWindow *parent, gint response,
                      gpointer user_data)
{
    auto request = static_cast<BudgetNoteRequest *> (user_data);
    auto page = GNC_PLUGIN_PAGE_BUDGET (g_weak_ref_get (&request->page));
    auto book = static_cast<QofBook *> (g_weak_ref_get (&request->book));
    if (response == GTK_RESPONSE_OK && request->text && page && book &&
        gnc_get_current_book () == book && !qof_book_is_readonly (book) &&
        !qof_book_shutting_down (book))
    {
        auto priv = GNC_PLUGIN_PAGE_BUDGET_GET_PRIVATE (page);
        auto budget = gnc_budget_lookup (&request->budget_guid, book);
        auto account = xaccAccountLookup (&request->account_guid, book);
        if (budget && account && priv->budget == budget &&
            request->period_num < gnc_budget_get_num_periods (budget) &&
            qof_instance_get_book (QOF_INSTANCE (budget)) == book &&
            qof_instance_get_book (QOF_INSTANCE (account)) == book)
            gnc_budget_set_account_period_note (budget, account,
                                                request->period_num,
                                                *request->text ? request->text : NULL);
    }
    g_clear_object (&page);
    g_clear_object (&book);
    g_clear_pointer (&request->text, g_free);
    g_weak_ref_clear (&request->page);
    g_weak_ref_clear (&request->book);
    g_signal_handlers_disconnect_by_data (request->dialog, request);
    g_clear_object (&request->dialog);
    g_free (request);
}

GncPluginPage *
gnc_plugin_page_budget_new (GncBudget *budget)
{
    GncPluginPageBudgetPrivate *priv;
    gchar* label;
    const GList *item;

    g_return_val_if_fail (GNC_IS_BUDGET(budget), NULL);
    ENTER(" ");

    /* Is there an existing page? */
    item = gnc_gobject_tracking_get_list (GNC_PLUGIN_PAGE_BUDGET_NAME);
    for ( ; item; item = g_list_next (item))
    {
        auto plugin_page = GNC_PLUGIN_PAGE_BUDGET(item->data);
        priv = GNC_PLUGIN_PAGE_BUDGET_GET_PRIVATE(plugin_page);
        if (priv->budget == budget)
        {
            LEAVE("existing budget page %p", plugin_page);
            return GNC_PLUGIN_PAGE(plugin_page);
        }
    }

    auto plugin_page = GNC_PLUGIN_PAGE_BUDGET (g_object_new (GNC_TYPE_PLUGIN_PAGE_BUDGET, nullptr));

    priv = GNC_PLUGIN_PAGE_BUDGET_GET_PRIVATE(plugin_page);
    priv->budget = budget;
    priv->delete_budget = FALSE;
    priv->key = *gnc_budget_get_guid (budget);
    priv->reportPage = NULL;
    label = g_strdup_printf ("%s: %s", _("Budget"), gnc_budget_get_name (budget));
    g_object_set (G_OBJECT(plugin_page), "page-name", label, NULL);
    g_free (label);
    LEAVE("new budget page %p", plugin_page);
    return GNC_PLUGIN_PAGE(plugin_page);
}


static void
gnc_plugin_page_budget_class_init (GncPluginPageBudgetClass *klass)
{
    GObjectClass *object_class = G_OBJECT_CLASS(klass);
    GncPluginPageClass *gnc_plugin_class = GNC_PLUGIN_PAGE_CLASS(klass);

    object_class->finalize = gnc_plugin_page_budget_finalize;

    gnc_plugin_class->tab_icon        = GNC_ICON_BUDGET;
    gnc_plugin_class->plugin_name     = GNC_PLUGIN_PAGE_BUDGET_NAME;
    gnc_plugin_class->create_widget   = gnc_plugin_page_budget_create_widget;
    gnc_plugin_class->destroy_widget  = gnc_plugin_page_budget_destroy_widget;
    gnc_plugin_class->save_page       = gnc_plugin_page_budget_save_page;
    gnc_plugin_class->recreate_page   = gnc_plugin_page_budget_recreate_page;
    gnc_plugin_class->focus_page_function = gnc_plugin_page_budget_focus_widget;
}


static void
gnc_plugin_page_budget_init (GncPluginPageBudget *plugin_page)
{
    GSimpleActionGroup *simple_action_group;
    GncPluginPageBudgetPrivate *priv;
    GncPluginPage *parent;

    ENTER("page %p", plugin_page);
    priv = GNC_PLUGIN_PAGE_BUDGET_GET_PRIVATE(plugin_page);

    /* Initialize parent declared variables */
    parent = GNC_PLUGIN_PAGE(plugin_page);
    g_object_set (G_OBJECT(plugin_page),
                  "page-name",      _("Budget"),
                  "ui-description", "gnc-plugin-page-budget.ui",
                  NULL);

    /* change me when the system supports multiple books */
    gnc_plugin_page_add_book (parent, gnc_get_current_book());

    /* Create menu and toolbar information */
    simple_action_group = gnc_plugin_page_create_action_group (parent, "GncPluginPageBudgetActions");
    g_action_map_add_action_entries (G_ACTION_MAP(simple_action_group),
                                     gnc_plugin_page_budget_actions,
                                     gnc_plugin_page_budget_n_actions,
                                     plugin_page);

    if (qof_book_is_readonly (gnc_get_current_book()))
        gnc_plugin_set_actions_enabled (G_ACTION_MAP(simple_action_group), writeable_actions,
                                        FALSE);

    /* Visible types */
    priv->fd.visible_types = -1; /* Start with all types */
    priv->fd.show_hidden = FALSE;
    priv->fd.show_unused = TRUE;
    priv->fd.show_zero_total = TRUE;
    priv->fd.filter_override = g_hash_table_new (g_direct_hash, g_direct_equal);

    priv->sigFigs = 1;
    priv->useAvg = FALSE;
    recurrenceSet (&priv->r, 1, PERIOD_MONTH, NULL, WEEKEND_ADJ_NONE);

    LEAVE("page %p, priv %p, action group %p",
          plugin_page, priv, simple_action_group);
}


static void
gnc_plugin_page_budget_finalize (GObject *object)
{
    GncPluginPageBudget *page;

    ENTER("object %p", object);
    page = GNC_PLUGIN_PAGE_BUDGET(object);
    g_return_if_fail (GNC_IS_PLUGIN_PAGE_BUDGET(page));

    G_OBJECT_CLASS (gnc_plugin_page_budget_parent_class)->finalize (object);
    LEAVE(" ");
}


/* Component Manager Callback Functions */
static void
gnc_plugin_page_budget_close_cb (gpointer user_data)
{
    GncPluginPage *page = GNC_PLUGIN_PAGE(user_data);
    gnc_main_window_close_page (page);
}


/**
 * Whenever the current page is changed, if a budget page is
 * the current page, set focus on the budget tree view.
 */
static gboolean
gnc_plugin_page_budget_focus_widget (GncPluginPage *budget_plugin_page)
{
    if (GNC_IS_PLUGIN_PAGE_BUDGET(budget_plugin_page))
    {
        GncPluginPageBudgetPrivate *priv = GNC_PLUGIN_PAGE_BUDGET_GET_PRIVATE(budget_plugin_page);
        GncBudgetView *budget_view = priv->budget_view;
        GtkWidget *account_view = gnc_budget_view_get_account_tree_view (budget_view);

        /* Disable the Transaction Menu */
        GAction *action = gnc_main_window_find_action (GNC_MAIN_WINDOW(budget_plugin_page->window), "TransactionAction");
        g_simple_action_set_enabled (G_SIMPLE_ACTION(action), FALSE);
        /* Disable the Schedule menu */
        action = gnc_main_window_find_action (GNC_MAIN_WINDOW(budget_plugin_page->window), "ScheduledAction");
        g_simple_action_set_enabled (G_SIMPLE_ACTION(action), FALSE);
        /* Disable the FilePrintAction */
        action = gnc_main_window_find_action (GNC_MAIN_WINDOW(budget_plugin_page->window), "FilePrintAction");
        g_simple_action_set_enabled (G_SIMPLE_ACTION(action), FALSE);

        gnc_main_window_update_menu_and_toolbar (GNC_MAIN_WINDOW(budget_plugin_page->window),
                                                 budget_plugin_page,
                                                 gnc_plugin_load_ui_items);

        // setup any short toolbar names
        gnc_main_window_init_short_names (GNC_MAIN_WINDOW(budget_plugin_page->window), toolbar_labels);

        if (!gtk_widget_is_focus (GTK_WIDGET(account_view)))
            gtk_widget_grab_focus (GTK_WIDGET(account_view));
    }
    return FALSE;
}


static void
gnc_plugin_page_budget_refresh_cb (GHashTable *changes, gpointer user_data)
{
    GncPluginPageBudget *page;
    GncPluginPageBudgetPrivate *priv;
    const EventInfo* ei;

    page = GNC_PLUGIN_PAGE_BUDGET(user_data);
    priv = GNC_PLUGIN_PAGE_BUDGET_GET_PRIVATE(page);
    if (changes)
    {
        ei = gnc_gui_get_entity_events (changes, &priv->key);
        if (ei)
        {
            if (ei->event_mask & QOF_EVENT_DESTROY)
            {
                /* Budget has been deleted, close plugin page
                 * but prevent that action from writing state information
                 * for this budget account
                 */
                priv->delete_budget = TRUE;
                gnc_budget_view_delete_budget (priv->budget_view);
                gnc_plugin_page_budget_close_cb (user_data);
                return;
            }
            if (ei->event_mask & QOF_EVENT_MODIFY)
            {
                DEBUG("refreshing budget view because budget was modified");
                gnc_budget_view_refresh (priv->budget_view);
            }
        }
    }
}


/****************************
 * GncPluginPage Functions  *
 ***************************/
static GtkWidget *
gnc_plugin_page_budget_create_widget (GncPluginPage *plugin_page)
{
    GncPluginPageBudget *page;
    GncPluginPageBudgetPrivate *priv;

    ENTER("page %p", plugin_page);
    page = GNC_PLUGIN_PAGE_BUDGET(plugin_page);
    priv = GNC_PLUGIN_PAGE_BUDGET_GET_PRIVATE(page);
    if (priv->budget_view != NULL)
    {
        LEAVE("widget = %p", priv->budget_view);
        return GTK_WIDGET(priv->budget_view);
    }

    priv->budget_view = gnc_budget_view_new (priv->budget, &priv->fd);

#if 0
    g_signal_connect (G_OBJECT(selection), "changed",
                      G_CALLBACK(gppb_selection_changed_cb), plugin_page);
#endif
    g_signal_connect (G_OBJECT(priv->budget_view), "button-press-event",
                      G_CALLBACK(gppb_button_press_cb), plugin_page);
    g_signal_connect (G_OBJECT(priv->budget_view), "account-activated",
                      G_CALLBACK(gppb_account_activated_cb), page);

    priv->component_id =
        gnc_register_gui_component (PLUGIN_PAGE_BUDGET_CM_CLASS,
                                    gnc_plugin_page_budget_refresh_cb,
                                    gnc_plugin_page_budget_close_cb,
                                    page);

    gnc_gui_component_set_session (priv->component_id,
                                   gnc_get_current_session ());

    gnc_gui_component_watch_entity (priv->component_id,
                                    gnc_budget_get_guid (priv->budget),
                                    QOF_EVENT_DESTROY | QOF_EVENT_MODIFY);

    g_signal_connect (G_OBJECT(plugin_page), "inserted",
                      G_CALLBACK(gnc_plugin_page_inserted_cb),
                      NULL);

    LEAVE("widget = %p", priv->budget_view);
    return GTK_WIDGET(priv->budget_view);
}


static void
gnc_plugin_page_budget_destroy_widget (GncPluginPage *plugin_page)
{
    GncPluginPageBudgetPrivate *priv;

    ENTER("page %p", plugin_page);
    priv = GNC_PLUGIN_PAGE_BUDGET_GET_PRIVATE(plugin_page);

    // Remove the page_changed signal callback
    gnc_plugin_page_disconnect_page_changed (GNC_PLUGIN_PAGE(plugin_page));

    // Remove the page focus idle function if present
    g_idle_remove_by_data (plugin_page);

    if (priv->budget_view)
    {
        // save the account filter state information to budget section
        gnc_budget_view_save_account_filter (priv->budget_view);

        if (priv->delete_budget)
        {
            gnc_budget_view_delete_budget (priv->budget_view);
        }

        g_object_unref (G_OBJECT(priv->budget_view));
        priv->budget_view = NULL;
    }

    // Destroy the filter override hash table
    g_hash_table_destroy (priv->fd.filter_override);

    gnc_gui_component_clear_watches (priv->component_id);

    if (priv->component_id != NO_COMPONENT)
    {
        gnc_unregister_gui_component (priv->component_id);
        priv->component_id = NO_COMPONENT;
    }

    LEAVE("widget destroyed");
}


#define BUDGET_GUID "Budget GncGUID"

/***********************************************************************
 *  Save enough information about this plugin page that it can         *
 *  be recreated next time the user starts gnucash.                    *
 *                                                                     *
 *  @param page The page to save.                                      *
 *                                                                     *
 *  @param key_file A pointer to the GKeyFile data structure where the *
 *  page information should be written.                                *
 *                                                                     *
 *  @param group_name The group name to use when saving data.          *
 **********************************************************************/
static void
gnc_plugin_page_budget_save_page (GncPluginPage *plugin_page,
                                  GKeyFile *key_file, const gchar *group_name)
{
    GncPluginPageBudget *budget_page;
    GncPluginPageBudgetPrivate *priv;
    char guid_str[GUID_ENCODING_LENGTH+1];

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_BUDGET(plugin_page));
    g_return_if_fail (key_file != NULL);
    g_return_if_fail (group_name != NULL);

    ENTER("page %p, key_file %p, group_name %s", plugin_page, key_file,
          group_name);

    budget_page = GNC_PLUGIN_PAGE_BUDGET(plugin_page);
    priv = GNC_PLUGIN_PAGE_BUDGET_GET_PRIVATE(budget_page);

    guid_to_string_buff (gnc_budget_get_guid (priv->budget), guid_str);
    g_key_file_set_string (key_file, group_name, BUDGET_GUID, guid_str);

    // Save the Budget page information to state file
    gnc_budget_view_save (priv->budget_view, key_file, group_name);

    LEAVE(" ");
}


/***********************************************************************
 *  Create a new plugin page based on the information saved
 *  during a previous instantiation of gnucash.
 *
 *  @param window The window where this page should be installed.
 *
 *  @param key_file A pointer to the GKeyFile data structure where the
 *  page information should be read.
 *
 *  @param group_name The group name to use when restoring data.
 **********************************************************************/
static GncPluginPage *
gnc_plugin_page_budget_recreate_page (GtkWidget *window, GKeyFile *key_file,
                                      const gchar *group_name)
{
    GncPluginPageBudget *budget_page;
    GncPluginPageBudgetPrivate *priv;
    GncPluginPage *page;
    GError *error = NULL;
    char *guid_str;
    GncGUID guid;
    GncBudget *bgt;
    QofBook *book;

    g_return_val_if_fail (key_file, NULL);
    g_return_val_if_fail (group_name, NULL);
    ENTER("key_file %p, group_name %s", key_file, group_name);

    guid_str = g_key_file_get_string (key_file, group_name, BUDGET_GUID,
                                      &error);
    if (error)
    {
        g_warning("error reading group %s key %s: %s",
                  group_name, BUDGET_GUID, error->message);
        g_error_free (error);
        error = NULL;
        return NULL;
    }
    if (!string_to_guid (guid_str, &guid))
    {
        g_free (guid_str);
        return NULL;
    }
    g_free (guid_str);

    book = qof_session_get_book (gnc_get_current_session());
    bgt = gnc_budget_lookup (&guid, book);
    if (!bgt)
    {
        return NULL;
    }

    /* Create the new page. */
    page = gnc_plugin_page_budget_new(bgt);
    budget_page = GNC_PLUGIN_PAGE_BUDGET(page);
    priv = GNC_PLUGIN_PAGE_BUDGET_GET_PRIVATE(budget_page);

    /* Install it now so we can then manipulate the created widget */
    gnc_main_window_open_page (GNC_MAIN_WINDOW(window), page);

    //FIXME
    if (!gnc_budget_view_restore (priv->budget_view, key_file, group_name))
        return NULL;

    LEAVE(" ");
    return page;
}


/***********************************************************************
 *   This button press handler calls the common button press handler
 *  for all pages.  The GtkTreeView eats all button presses and
 *  doesn't pass them up the widget tree, even when it doesn't do
 *  anything with them.  The only way to get access to the button
 *  presses in an account tree page is here on the tree view widget.
 *  Button presses on all other pages are caught by the signal
 *  registered in gnc-main-window.c.
 **********************************************************************/
static gboolean
gppb_button_press_cb (GtkWidget *widget, GdkEventButton *event,
                      GncPluginPage *page)
{
    gboolean result;

    g_return_val_if_fail (GNC_IS_PLUGIN_PAGE(page), FALSE);

    ENTER("widget %p, event %p, page %p", widget, event, page);
    result = gnc_main_window_button_press_cb (widget, event, page);
    LEAVE(" ");
    return result;
}

static void
gppb_account_activated_cb (GncBudgetView* view, Account* account,
                           GncPluginPageBudget *page)
{
    GtkWidget *window;
    GncPluginPage *new_page;

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_BUDGET (page));

    window = GNC_PLUGIN_PAGE(page)->window;
    new_page = gnc_plugin_page_register_new (account, FALSE);
    gnc_main_window_open_page (GNC_MAIN_WINDOW(window), new_page);
}


#if 0
static void
gppb_selection_changed_cb (GtkTreeSelection *selection,
                           GncPluginPageBudget *page)
{
    GSimpleActionGroup *simple_action_group;
    GtkTreeView *view;
    GList *acct_list;
    gboolean sensitive;

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_BUDGET(page));

    if (!selection)
        sensitive = FALSE;
    else
    {
        g_return_if_fail (GTK_IS_TREE_SELECTION(selection));
        view = gtk_tree_selection_get_tree_view (selection);
        acct_list = gnc_tree_view_account_get_selected_accounts (
                        GNC_TREE_VIEW_ACCOUNT(view));

        /* Check here for placeholder accounts, etc. */
        sensitive = (g_list_length (acct_list) > 0);
        g_list_free (acct_list);
    }

    simple_action_group = gnc_plugin_page_get_action_group (GNC_PLUGIN_PAGE(page));
    gnc_plugin_set_actions_enabled (G_ACTION_MAP(simple_action_group), actions_requiring_account,
                                    sensitive);
}
#endif


/*********************
 * Command callbacks *
 ********************/
static void
gnc_plugin_page_budget_cmd_open_account (GSimpleAction *simple,
                                         GVariant *parameter,
                                         gpointer user_data)
{
    auto page = GNC_PLUGIN_PAGE_BUDGET (user_data);
    GncPluginPageBudgetPrivate *priv;
    GtkWidget *window;
    GncPluginPage *new_page;
    GList *acct_list, *tmp;

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_BUDGET(page));
    priv = GNC_PLUGIN_PAGE_BUDGET_GET_PRIVATE(page);
    acct_list = gnc_budget_view_get_selected_accounts (priv->budget_view);

    window = GNC_PLUGIN_PAGE(page)->window;
    for (tmp = acct_list; tmp; tmp = g_list_next (tmp))
    {
        auto account = GNC_ACCOUNT (tmp->data);
        new_page = gnc_plugin_page_register_new (account, FALSE);
        gnc_main_window_open_page (GNC_MAIN_WINDOW(window), new_page);
    }
    g_list_free (acct_list);
}


static void
gnc_plugin_page_budget_cmd_open_subaccounts (GSimpleAction *simple,
                                             GVariant *parameter,
                                             gpointer user_data)
{
    auto page = GNC_PLUGIN_PAGE_BUDGET (user_data);
    GncPluginPageBudgetPrivate *priv;
    GtkWidget *window;
    GncPluginPage *new_page;
    GList *acct_list, *tmp;

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_BUDGET(page));
    priv = GNC_PLUGIN_PAGE_BUDGET_GET_PRIVATE(page);
    acct_list = gnc_budget_view_get_selected_accounts (priv->budget_view);

    window = GNC_PLUGIN_PAGE(page)->window;
    for (tmp = acct_list; tmp; tmp = g_list_next (tmp))
    {
        auto account = GNC_ACCOUNT(tmp->data);
        new_page = gnc_plugin_page_register_new (account, TRUE);
        gnc_main_window_open_page (GNC_MAIN_WINDOW(window), new_page);
    }
    g_list_free (acct_list);
}


static void
gnc_plugin_page_budget_cmd_delete_budget (GSimpleAction *simple,
                                          GVariant *parameter,
                                          gpointer user_data)
{
    auto page = GNC_PLUGIN_PAGE_BUDGET (user_data);
    GncPluginPageBudgetPrivate *priv;
    GncBudget *budget;

    priv = GNC_PLUGIN_PAGE_BUDGET_GET_PRIVATE(page);
    budget = priv->budget;
    g_return_if_fail (GNC_IS_BUDGET(budget));
    priv->delete_budget = TRUE;
    gnc_budget_gui_delete_budget (budget);

}


static void
gnc_plugin_page_budget_cmd_edit_tax_options (GSimpleAction *simple,
                                             GVariant      *parameter,
                                             gpointer       user_data)
{
    auto page = GNC_PLUGIN_PAGE_BUDGET (user_data);
    GncPluginPageBudgetPrivate *priv;
    GtkTreeSelection *selection;
    Account *account = NULL;
    GtkWidget *window;

    page = GNC_PLUGIN_PAGE_BUDGET(page);

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_BUDGET(page));

    ENTER ("(action %p, page %p)", simple, page);
    priv = GNC_PLUGIN_PAGE_BUDGET_GET_PRIVATE(page);

    selection = gnc_budget_view_get_selection (priv->budget_view);
    window = GNC_PLUGIN_PAGE(page)->window;

    if (gtk_tree_selection_count_selected_rows (selection) == 1)
    {
        GList *acc_list = gnc_budget_view_get_selected_accounts (priv->budget_view);
        account = GNC_ACCOUNT (acc_list->data);
        g_list_free (acc_list);
    }
    gnc_tax_info_dialog (window, account);
    LEAVE (" ");
}

/******************************/
/*       Options Dialog       */
/******************************/
static void
gnc_plugin_page_budget_cmd_view_options (GSimpleAction *simple,
                                         GVariant *parameter,
                                         gpointer user_data)
{
    auto page = GNC_PLUGIN_PAGE_BUDGET (user_data);
    GncPluginPageBudgetPrivate *priv;
    GncRecurrence *gr;
    GtkBuilder *builder;
    GtkWidget *gbname, *gbtreeview, *gbnumperiods, *gbhb;
    GtkWidget *show_account_code, *show_account_desc;
    BudgetMutationRequest *request;

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_BUDGET(page));
    priv = GNC_PLUGIN_PAGE_BUDGET_GET_PRIVATE(page);
    if (priv->dialog)
        return;
    builder = gtk_builder_new ();
    gnc_builder_add_from_file (builder, "gnc-plugin-page-budget.glade", "NumPeriods_Adj");
    gnc_builder_add_from_file (builder, "gnc-plugin-page-budget.glade", "budget_options_container_dialog");
    auto dialog = GTK_WIDGET (gtk_builder_get_object (builder, "budget_options_container_dialog"));
    gtk_window_set_transient_for (GTK_WINDOW (dialog),
        GTK_WINDOW (gnc_plugin_page_get_window (GNC_PLUGIN_PAGE (page))));
    gbname = GTK_WIDGET (gtk_builder_get_object (builder, "BudgetName"));
    gtk_entry_set_text (GTK_ENTRY (gbname), gnc_budget_get_name (priv->budget));
    gbtreeview = GTK_WIDGET (gtk_builder_get_object (builder, "BudgetDescription"));
    gtk_text_buffer_set_text (gtk_text_view_get_buffer (GTK_TEXT_VIEW (gbtreeview)),
                              gnc_budget_get_description (priv->budget), -1);
    gbhb = GTK_WIDGET (gtk_builder_get_object (builder, "BudgetPeriod"));
    gr = GNC_RECURRENCE (gnc_recurrence_new ());
    gnc_recurrence_set (gr, gnc_budget_get_recurrence (priv->budget));
    gtk_box_pack_start (GTK_BOX (gbhb), GTK_WIDGET (gr), TRUE, TRUE, 0);
    gtk_widget_show (GTK_WIDGET (gr));
    gbnumperiods = GTK_WIDGET (gtk_builder_get_object (builder, "BudgetNumPeriods"));
    gtk_spin_button_set_value (GTK_SPIN_BUTTON (gbnumperiods),
                               gnc_budget_get_num_periods (priv->budget));
    show_account_code = GTK_WIDGET (gtk_builder_get_object (builder, "ShowAccountCode"));
    show_account_desc = GTK_WIDGET (gtk_builder_get_object (builder, "ShowAccountDescription"));
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (show_account_code),
                                  gnc_budget_view_get_show_account_code (priv->budget_view));
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (show_account_desc),
                                  gnc_budget_view_get_show_account_description (priv->budget_view));
    request = budget_mutation_request_new (page, dialog, BUDGET_OPTIONS_REQUEST);
    if (!request)
    {
        gtk_widget_destroy (dialog);
        g_object_unref (builder);
        return;
    }
    request->name = gbname;
    request->description = gbtreeview;
    request->recurrence = GTK_WIDGET (gr);
    request->periods = gbnumperiods;
    request->show_code = show_account_code;
    request->show_description = show_account_desc;
    priv->dialog = dialog;
    gtk_widget_show_all (dialog);
    g_object_unref (builder);
}


typedef struct
{
    GWeakRef book;
    GncGUID budget_guid;
} GncBudgetDeleteRequest;

static void
budget_delete_finished ([[maybe_unused]] GtkWindow *parent, gint response,
                        gpointer user_data)
{
    auto request = static_cast<GncBudgetDeleteRequest *> (user_data);

    auto book = static_cast<QofBook *> (g_weak_ref_get (&request->book));
    if (response == GTK_RESPONSE_YES && book && gnc_current_session_exist () &&
        gnc_get_current_book () == book && qof_book_is_open (book) &&
        !qof_book_shutting_down (book) && !qof_book_is_readonly (book))
    {
        auto budget = gnc_budget_lookup (&request->budget_guid, book);

        if (budget)
        {
            gnc_suspend_gui_refresh ();
            gnc_budget_destroy (budget);

            if (gnc_current_session_exist () && gnc_get_current_book () == book &&
                qof_book_is_open (book) && !qof_book_shutting_down (book) &&
                qof_collection_count (qof_book_get_collection (book,
                                                                GNC_ID_BUDGET)) == 0)
            {
                gnc_features_set_unused (book, GNC_FEATURE_BUDGET_UNREVERSED);
                PWARN ("No budgets left. Removing feature BUDGET_UNREVERSED.");
            }
            /* Views close themselves because the component manager notifies them. */
            gnc_resume_gui_refresh ();
        }
    }
    g_clear_object (&book);
    g_weak_ref_clear (&request->book);
    g_free (request);
}

void
gnc_budget_gui_delete_budget (GncBudget *budget)
{
    const char *name;

    g_return_if_fail (GNC_IS_BUDGET(budget));
    name = gnc_budget_get_name (budget);
    if (!name)
        name = _("Unnamed Budget");

    auto request = g_new0 (GncBudgetDeleteRequest, 1);
    g_weak_ref_init (&request->book, qof_instance_get_book (QOF_INSTANCE (budget)));
    request->budget_guid = gnc_budget_return_guid (budget);
    gnc_verify_dialog_async (NULL, FALSE, budget_delete_finished, request,
                             _("Delete %s?"), name);
}

/*******************************/
/*       Estimate Dialog       */
/*******************************/
static void
gnc_plugin_page_budget_cmd_estimate_budget (GSimpleAction *simple,
                                            GVariant *parameter,
                                            gpointer user_data)
{
    auto page = GNC_PLUGIN_PAGE_BUDGET (user_data);
    GncPluginPageBudgetPrivate *priv;
    GtkTreeSelection *sel;
    GtkWidget *dialog, *gde, *dtr, *hb, *avg;
    GDate date;
    GtkBuilder *builder;
    BudgetMutationRequest *request;

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_BUDGET(page));
    priv = GNC_PLUGIN_PAGE_BUDGET_GET_PRIVATE(page);

    sel = gnc_budget_view_get_selection (priv->budget_view);

    if (gtk_tree_selection_count_selected_rows (sel) <= 0)
    {
        dialog = gtk_message_dialog_new (
                     GTK_WINDOW(gnc_plugin_page_get_window (GNC_PLUGIN_PAGE(page))),
                     (GtkDialogFlags)(GTK_DIALOG_DESTROY_WITH_PARENT | GTK_DIALOG_MODAL),
                     GTK_MESSAGE_INFO, GTK_BUTTONS_CLOSE, "%s",
                     _("You must select at least one account to estimate."));
        g_signal_connect (dialog, "response",
                          G_CALLBACK (gtk_widget_destroy), nullptr);
        gtk_widget_show (dialog);
        return;
    }

    builder = gtk_builder_new ();
    gnc_builder_add_from_file (builder, "gnc-plugin-page-budget.glade", "DigitsToRound_Adj");
    gnc_builder_add_from_file (builder, "gnc-plugin-page-budget.glade", "budget_estimate_dialog");

    dialog = GTK_WIDGET(gtk_builder_get_object (builder, "budget_estimate_dialog"));

    gtk_window_set_transient_for (GTK_WINDOW(dialog),
        GTK_WINDOW(gnc_plugin_page_get_window (GNC_PLUGIN_PAGE(page))));

    hb = GTK_WIDGET(gtk_builder_get_object (builder, "StartDate_hbox"));
    gde = gnc_date_edit_new (time (NULL), FALSE, FALSE);
    gtk_box_pack_start (GTK_BOX(hb), gde, TRUE, TRUE, 0);
    gtk_widget_show (gde);

    date = recurrenceGetDate (&priv->r);
    gnc_date_edit_set_gdate (GNC_DATE_EDIT(gde), &date);

    dtr = GTK_WIDGET(gtk_builder_get_object (builder, "DigitsToRound"));
    gtk_spin_button_set_value (GTK_SPIN_BUTTON(dtr),
                               (gdouble)priv->sigFigs);

    avg = GTK_WIDGET(gtk_builder_get_object (builder, "UseAverage"));
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON(avg), priv->useAvg);
    request = budget_mutation_request_new (page, dialog, BUDGET_ESTIMATE_REQUEST);
    if (!request)
    {
        gtk_widget_destroy (dialog);
        g_object_unref (builder);
        return;
    }
    request->date = gde;
    request->digits = dtr;
    request->average = avg;
    gtk_tree_selection_selected_foreach (sel, budget_mutation_snapshot_account, request);
    gtk_widget_show_all (dialog);
    g_object_unref (builder);
}

/*******************************/
/*  All Periods Value Dialog   */
/*******************************/
static void
gnc_plugin_page_budget_cmd_allperiods_budget (GSimpleAction *simple,
                                              GVariant *parameter,
                                              gpointer user_data)
{
    auto page = GNC_PLUGIN_PAGE_BUDGET (user_data);
    GncPluginPageBudgetPrivate *priv;
    GtkTreeSelection *sel;
    GtkWidget *dialog, *val, *dtr, *add, *mult;
    GtkBuilder *builder;
    BudgetMutationRequest *request;

    g_return_if_fail(GNC_IS_PLUGIN_PAGE_BUDGET(page));
    priv = GNC_PLUGIN_PAGE_BUDGET_GET_PRIVATE(page);
    sel = gnc_budget_view_get_selection (priv->budget_view);

    if (gtk_tree_selection_count_selected_rows (sel) <= 0)
    {
        dialog = gtk_message_dialog_new (
                    GTK_WINDOW(gnc_plugin_page_get_window (GNC_PLUGIN_PAGE(page))),
                    (GtkDialogFlags)(GTK_DIALOG_DESTROY_WITH_PARENT | GTK_DIALOG_MODAL),
                    GTK_MESSAGE_INFO, GTK_BUTTONS_CLOSE, "%s",
                    _("You must select at least one account to edit."));
        g_signal_connect (dialog, "response",
                          G_CALLBACK (gtk_widget_destroy), nullptr);
        gtk_widget_show (dialog);
        return;
    }

    builder = gtk_builder_new ();
    gnc_builder_add_from_file (builder, "gnc-plugin-page-budget.glade",
                               "DigitsToRound_Adj");
    gnc_builder_add_from_file (builder, "gnc-plugin-page-budget.glade",
                               "budget_allperiods_dialog");

    dialog = GTK_WIDGET(
        gtk_builder_get_object (builder, "budget_allperiods_dialog"));

    gtk_window_set_transient_for (
        GTK_WINDOW(dialog),
        GTK_WINDOW(gnc_plugin_page_get_window (GNC_PLUGIN_PAGE(page))));

    val = GTK_WIDGET(gtk_builder_get_object (builder, "Value"));
    gtk_entry_set_text (GTK_ENTRY(val), "");

    dtr = GTK_WIDGET(gtk_builder_get_object (builder, "DigitsToRound1"));
    gtk_spin_button_set_value (GTK_SPIN_BUTTON(dtr), (gdouble)priv->sigFigs);

    add  = GTK_WIDGET(gtk_builder_get_object (builder, "RB_Add"));
    mult = GTK_WIDGET(gtk_builder_get_object (builder, "RB_Multiply"));
    request = budget_mutation_request_new (page, dialog,
                                          BUDGET_ALL_PERIODS_REQUEST);
    if (!request)
    {
        gtk_widget_destroy (dialog);
        g_object_unref (builder);
        return;
    }
    request->value = val;
    request->digits = dtr;
    request->add = add;
    request->multiply = mult;
    gtk_tree_selection_selected_foreach (sel, budget_mutation_snapshot_account, request);
    gtk_widget_show_all (dialog);
    g_object_unref (builder);
}

static void
gnc_plugin_page_budget_cmd_budget_note (GSimpleAction *simple,
                                        GVariant *parameter,
                                        gpointer user_data)
{
    auto page = GNC_PLUGIN_PAGE_BUDGET (user_data);
    GncPluginPageBudgetPrivate *priv;
    GtkWidget *dialog, *note;
    GtkBuilder *builder;
    GtkTreeViewColumn *col = NULL;
    GtkTreePath *path = NULL;
    guint period_num = 0;
    Account *acc = NULL;

    g_return_if_fail(GNC_IS_PLUGIN_PAGE_BUDGET(page));
    priv = GNC_PLUGIN_PAGE_BUDGET_GET_PRIVATE(page);
    gtk_tree_view_get_cursor(
        GTK_TREE_VIEW(gnc_budget_view_get_account_tree_view(priv->budget_view)),
        &path, &col);

    if (path)
    {
        period_num = col ? GPOINTER_TO_UINT(
                               g_object_get_data(G_OBJECT(col), "period_num"))
                         : 0;

        acc = gnc_budget_view_get_account_from_path(priv->budget_view, path);
        gtk_tree_path_free(path);
    }

    if (!acc)
    {
        dialog = gtk_message_dialog_new(
            GTK_WINDOW(gnc_plugin_page_get_window(GNC_PLUGIN_PAGE(page))),
            (GtkDialogFlags)(GTK_DIALOG_DESTROY_WITH_PARENT | GTK_DIALOG_MODAL),
            GTK_MESSAGE_INFO, GTK_BUTTONS_CLOSE, "%s",
            _("You must select one budget cell to edit."));
        g_signal_connect(dialog, "response",
                         G_CALLBACK(gtk_widget_destroy), nullptr);
        gtk_widget_show(dialog);
        return;
    }

    builder = gtk_builder_new();
    gnc_builder_add_from_file(builder, "gnc-plugin-page-budget.glade",
                              "budget_note_dialog");

    dialog = GTK_WIDGET(gtk_builder_get_object(builder, "budget_note_dialog"));

    gtk_window_set_transient_for(
        GTK_WINDOW(dialog),
        GTK_WINDOW(gnc_plugin_page_get_window(GNC_PLUGIN_PAGE(page))));
    gtk_window_set_destroy_with_parent (GTK_WINDOW (dialog), TRUE);

    note = GTK_WIDGET(gtk_builder_get_object(builder, "BudgetNote"));
    xxxgtk_textview_set_text(GTK_TEXT_VIEW(note),
                             gnc_budget_get_account_period_note(priv->budget, acc, period_num));

    auto book = qof_instance_get_book (QOF_INSTANCE (priv->budget));
    if (!book || qof_instance_get_book (QOF_INSTANCE (acc)) != book)
    {
        gtk_widget_destroy (dialog);
        g_object_unref (G_OBJECT (builder));
        return;
    }

    auto request = g_new0 (BudgetNoteRequest, 1);
    g_weak_ref_init (&request->page, G_OBJECT (page));
    g_weak_ref_init (&request->book, G_OBJECT (book));
    request->dialog = GTK_DIALOG (g_object_ref (dialog));
    request->note = GTK_TEXT_VIEW (note);
    request->budget_guid = *gnc_budget_get_guid (priv->budget);
    request->account_guid = *qof_instance_get_guid (QOF_INSTANCE (acc));
    request->period_num = period_num;
    g_signal_connect (dialog, "response",
                      G_CALLBACK (budget_note_capture), request);
    gnc_gui_query_bind_dialog_response (GTK_DIALOG (dialog),
                                       budget_note_complete, request);
    gtk_widget_show_all(dialog);
    g_object_unref(G_OBJECT(builder));
}

static gboolean
equal_fn (gpointer find_data, gpointer elt_data)
{
    return (find_data && (find_data == elt_data));
}

/* From the budget editor, open the budget report. This will reuse the
   budget report if generated from the current budget editor. Note the
   reuse is lost when GnuCash is restarted. This link may be restored
   by: scan the current session tabs, identify reports, checking
   whereby report's report-type matches a budget report, and the
   report's budget option value matches the current budget. */
static void
gnc_plugin_page_budget_cmd_budget_report (GSimpleAction *simple,
                                          GVariant *parameter,
                                          gpointer user_data)
{
    auto page = GNC_PLUGIN_PAGE_BUDGET (user_data);
    GncPluginPageBudgetPrivate *priv;

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_BUDGET (page));

    priv = GNC_PLUGIN_PAGE_BUDGET_GET_PRIVATE (page);

    if (gnc_find_first_gui_component (WINDOW_REPORT_CM_CLASS, equal_fn,
                                      priv->reportPage))
        gnc_plugin_page_report_reload (GNC_PLUGIN_PAGE_REPORT (priv->reportPage));
    else
    {
        SCM func = scm_c_eval_string ("gnc:budget-report-create");
        SCM arg = SWIG_NewPointerObj (priv->budget, SWIG_TypeQuery ("_p_budget_s"), 0);
        int report_id;

        g_return_if_fail (scm_is_procedure (func));

        arg = scm_apply_0 (func, scm_list_1 (arg));
        g_return_if_fail (scm_is_exact (arg));

        report_id = scm_to_int (arg);
        g_return_if_fail (report_id >= 0);

        priv->reportPage = gnc_plugin_page_report_new (report_id);
    }

    gnc_main_window_open_page (GNC_MAIN_WINDOW (priv->dialog), priv->reportPage);
}

static void
gnc_plugin_page_budget_cmd_view_filter_by (GSimpleAction *simple,
                                           GVariant *parameter,
                                           gpointer user_data)
{
    auto page = GNC_PLUGIN_PAGE_BUDGET (user_data);
    GncPluginPageBudgetPrivate *priv;

    g_return_if_fail(GNC_IS_PLUGIN_PAGE_BUDGET(page));
    ENTER("(action %p, page %p)", simple, page);

    priv = GNC_PLUGIN_PAGE_BUDGET_GET_PRIVATE(page);
    account_filter_dialog_create (&priv->fd, GNC_PLUGIN_PAGE(page));

    LEAVE(" ");
}

static void
gnc_plugin_page_budget_cmd_refresh (GSimpleAction *simple,
                                    GVariant *parameter,
                                    gpointer user_data)
{
    auto page = GNC_PLUGIN_PAGE_BUDGET (user_data);
    GncPluginPageBudgetPrivate *priv;

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_BUDGET(page));
    ENTER("(action %p, page %p)", simple, page);

    priv = GNC_PLUGIN_PAGE_BUDGET_GET_PRIVATE(page);

    gnc_budget_view_refresh (priv->budget_view);
    LEAVE(" ");
}
