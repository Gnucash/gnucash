/********************************************************************\
 * reconcile-view.c -- A GTK4 view of splits to be reconciled.      *
 * Copyright (C) 1998,1999 Jeremy Collins                           *
 * Copyright (C) 1998-2000 Linas Vepstas                            *
 * Copyright (C) 2012 Robert Fewell                                 *
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

#include <glib/gi18n.h>
#include <gtk/gtk.h>

#include "gnc-component-manager.h"
#include "gnc-date.h"
#include "gnc-prefs.h"
#include "gnc-ui-util.h"
#include "qof.h"
#include "qofbook.h"
#include "reconcile-view.h"
#include "search-param.h"
#include "Transaction.h"

#define GNC_PREF_CHECK_CLEARED "check-cleared"

enum
{
    TOGGLE_RECONCILED,
    LINE_SELECTED,
    DOUBLE_CLICK_SPLIT,
    LAST_SIGNAL
};

static guint reconcile_view_signals[LAST_SIGNAL] = { 0 };

static gpointer gnc_reconcile_view_is_reconciled (gpointer item,
                                                   gpointer user_data);
static void gnc_reconcile_view_line_toggled (GNCQueryView *qview,
                                              gpointer item,
                                              gpointer user_data);
static void gnc_reconcile_view_double_click_entry (GNCQueryView *qview,
                                                    gpointer item,
                                                    gpointer user_data);
static void gnc_reconcile_view_row_selected (GNCQueryView *qview,
                                             gpointer item,
                                             gpointer user_data);
static gboolean gnc_reconcile_view_key_press_cb (GtkEventControllerKey *controller,
                                                 guint keyval, guint keycode,
                                                 GdkModifierType state, gpointer user_data);

G_DEFINE_TYPE (GNCReconcileView, gnc_reconcile_view, GNC_TYPE_QUERY_VIEW)

static gint
sort_date_helper (time64 first, time64 second)
{
    return first < second ? -1 : first > second ? 1 : 0;
}

static gint
reconcile_compare (gpointer first, gpointer second, gpointer user_data)
{
    GNCReconcileView *view = GNC_RECONCILE_VIEW (user_data);
    gboolean first_reconciled = g_hash_table_contains (view->reconciled, first);
    gboolean second_reconciled = g_hash_table_contains (view->reconciled, second);

    if (first_reconciled != second_reconciled)
        return first_reconciled ? -1 : 1;
    return sort_date_helper (xaccTransGetDate (xaccSplitGetParent (first)),
                             xaccTransGetDate (xaccSplitGetParent (second)));
}

gint
gnc_reconcile_view_get_column_width (GNCReconcileView *view, gint column)
{
    g_return_val_if_fail (GNC_IS_RECONCILE_VIEW (view), 0);
    return gnc_query_view_get_column_width (GNC_QUERY_VIEW (view), column - 1);
}

void
gnc_reconcile_view_add_padding (GNCReconcileView *view, gint column, gint xpadding)
{
    g_return_if_fail (GNC_IS_RECONCILE_VIEW (view));
    gnc_query_view_add_column_padding (GNC_QUERY_VIEW (view), column - 1, xpadding);
}

static void
gnc_reconcile_view_construct (GNCReconcileView *view, Query *query)
{
    GNCQueryView *qview = GNC_QUERY_VIEW (view);
    GtkEventController *key_controller;

    gnc_query_view_construct (qview, view->column_list, query);
    gnc_query_view_set_numerics (qview, TRUE, view->view_type == RECLIST_CREDIT);
    gnc_query_view_set_selection_mode (qview, GTK_SELECTION_MULTIPLE);
    gnc_query_set_expand_column (qview, REC_DESC - 1);
    gnc_query_view_set_column_ellipsize (qview, REC_DESC - 1,
                                         PANGO_ELLIPSIZE_END, TRUE);
    gnc_query_view_set_custom_sort_func (qview, reconcile_compare, view);
    gnc_query_view_refresh (qview);

    g_signal_connect (qview, "column_toggled",
                      G_CALLBACK (gnc_reconcile_view_line_toggled), view);
    g_signal_connect (qview, "double_click_entry",
                      G_CALLBACK (gnc_reconcile_view_double_click_entry), view);
    g_signal_connect (qview, "row_selected",
                      G_CALLBACK (gnc_reconcile_view_row_selected), view);

    key_controller = gtk_event_controller_key_new ();
    g_signal_connect (key_controller, "key-pressed",
                      G_CALLBACK (gnc_reconcile_view_key_press_cb), view);
    gtk_widget_add_controller (GTK_WIDGET (qview), key_controller);
}

GtkWidget *
gnc_reconcile_view_new (Account *account, GNCReconcileViewType type,
                        time64 statement_date)
{
    GNCReconcileView *view;
    gboolean include_children;
    gboolean auto_check;
    GList *accounts = NULL;
    Query *query;
    QofNumericMatch sign;

    g_return_val_if_fail (account, NULL);
    g_return_val_if_fail (type == RECLIST_DEBIT || type == RECLIST_CREDIT, NULL);

    view = g_object_new (GNC_TYPE_RECONCILE_VIEW, NULL);
    view->account = account;
    view->view_type = type;
    view->statement_date = statement_date;

    query = qof_query_create_for (GNC_ID_SPLIT);
    qof_query_set_book (query, gnc_get_current_book ());
    include_children = xaccAccountGetReconcileChildrenStatus (account);
    if (include_children)
        accounts = gnc_account_get_descendants (account);
    accounts = g_list_prepend (accounts, account);
    xaccQueryAddAccountMatch (query, accounts, QOF_GUID_MATCH_ANY, QOF_QUERY_AND);
    g_list_free (accounts);

    sign = type == RECLIST_CREDIT ? QOF_NUMERIC_MATCH_CREDIT : QOF_NUMERIC_MATCH_DEBIT;
    xaccQueryAddNumericMatch (query, gnc_numeric_zero (), sign, QOF_COMPARE_GTE,
                              QOF_QUERY_AND, SPLIT_AMOUNT, NULL);
    xaccQueryAddClearedMatch (query, CLEARED_NO | CLEARED_CLEARED, QOF_QUERY_AND);
    gnc_reconcile_view_construct (view, query);

    auto_check = gnc_prefs_get_bool (GNC_PREFS_GROUP_RECONCILE, GNC_PREF_CHECK_CLEARED);
    if (auto_check)
    {
        time64 day_end = gnc_time64_get_day_end (statement_date);

        for (GList *splits = qof_query_run (query); splits; splits = splits->next)
        {
            Split *split = splits->data;
            char recn = xaccSplitGetReconcile (split);

            g_assert (recn == NREC || recn == CREC);
            if (recn == CREC && xaccTransGetDate (xaccSplitGetParent (split)) <= day_end)
                g_hash_table_add (view->reconciled, split);
        }
        gnc_query_view_refresh (GNC_QUERY_VIEW (view));
    }
    qof_query_destroy (query);
    return GTK_WIDGET (view);
}

static void
gnc_reconcile_view_init (GNCReconcileView *view)
{
    GNCSearchParamSimple *param;
    GList *columns = NULL;
    gboolean num_action = qof_book_use_split_action_for_num_field (gnc_get_current_book ());

    view->reconciled = g_hash_table_new (NULL, NULL);
    param = gnc_search_param_simple_new ();
    gnc_search_param_set_param_fcn (param, QOF_TYPE_BOOLEAN,
                                    gnc_reconcile_view_is_reconciled, view);
    gnc_search_param_set_title (GNC_SEARCH_PARAM (param),
                                C_("Column header for 'Reconciled'", "R"));
    gnc_search_param_set_justify (GNC_SEARCH_PARAM (param), GTK_JUSTIFY_CENTER);
    gnc_search_param_set_passive (GNC_SEARCH_PARAM (param), FALSE);
    gnc_search_param_set_non_resizeable (GNC_SEARCH_PARAM (param), TRUE);
    columns = g_list_prepend (columns, param);
    columns = gnc_search_param_prepend_with_justify (columns, _("Amount"),
                                                     GTK_JUSTIFY_RIGHT, NULL, GNC_ID_SPLIT,
                                                     SPLIT_AMOUNT, NULL);
    columns = gnc_search_param_prepend (columns, _("Description"), NULL, GNC_ID_SPLIT,
                                        SPLIT_TRANS, TRANS_DESCRIPTION, NULL);
    columns = num_action ?
        gnc_search_param_prepend_with_justify (columns, _("Num"), GTK_JUSTIFY_CENTER,
                                                NULL, GNC_ID_SPLIT, SPLIT_ACTION, NULL) :
        gnc_search_param_prepend_with_justify (columns, _("Num"), GTK_JUSTIFY_CENTER,
                                                NULL, GNC_ID_SPLIT, SPLIT_TRANS, TRANS_NUM, NULL);
    columns = gnc_search_param_prepend (columns, _("Date"), NULL, GNC_ID_SPLIT,
                                        SPLIT_TRANS, TRANS_DATE_POSTED, NULL);
    view->column_list = columns;
}

static void
gnc_reconcile_view_finalize (GObject *object)
{
    GNCReconcileView *view = GNC_RECONCILE_VIEW (object);

    g_list_free_full (view->column_list, g_object_unref);
    g_clear_pointer (&view->reconciled, g_hash_table_destroy);
    G_OBJECT_CLASS (gnc_reconcile_view_parent_class)->finalize (object);
}

static void
gnc_reconcile_view_class_init (GNCReconcileViewClass *klass)
{
    GObjectClass *object_class = G_OBJECT_CLASS (klass);

    object_class->finalize = gnc_reconcile_view_finalize;
    reconcile_view_signals[TOGGLE_RECONCILED] =
        g_signal_new ("toggle_reconciled", G_TYPE_FROM_CLASS (klass), G_SIGNAL_RUN_FIRST,
                      G_STRUCT_OFFSET (GNCReconcileViewClass, toggle_reconciled), NULL, NULL,
                      g_cclosure_marshal_VOID__POINTER, G_TYPE_NONE, 1, G_TYPE_POINTER);
    reconcile_view_signals[LINE_SELECTED] =
        g_signal_new ("line_selected", G_TYPE_FROM_CLASS (klass), G_SIGNAL_RUN_FIRST,
                      G_STRUCT_OFFSET (GNCReconcileViewClass, line_selected), NULL, NULL,
                      g_cclosure_marshal_VOID__POINTER, G_TYPE_NONE, 1, G_TYPE_POINTER);
    reconcile_view_signals[DOUBLE_CLICK_SPLIT] =
        g_signal_new ("double_click_split", G_TYPE_FROM_CLASS (klass), G_SIGNAL_RUN_FIRST,
                      G_STRUCT_OFFSET (GNCReconcileViewClass, double_click_split), NULL, NULL,
                      g_cclosure_marshal_VOID__POINTER, G_TYPE_NONE, 1, G_TYPE_POINTER);
}

static void
gnc_reconcile_view_toggle (GNCReconcileView *view, Split *split)
{
    if (g_hash_table_contains (view->reconciled, split))
        g_hash_table_remove (view->reconciled, split);
    else
        g_hash_table_add (view->reconciled, split);
    g_signal_emit (view, reconcile_view_signals[TOGGLE_RECONCILED], 0, split);
}

void
gnc_reconcile_view_unclear_all (GNCReconcileView *view)
{
    g_return_if_fail (GNC_IS_RECONCILE_VIEW (view));
    g_hash_table_remove_all (view->reconciled);
}

void
gnc_reconcile_view_set_cleared (GNCReconcileView *view, Split *split)
{
    g_return_if_fail (GNC_IS_RECONCILE_VIEW (view));
    g_hash_table_add (view->reconciled, split);
}

static void
gnc_reconcile_view_line_toggled (GNCQueryView *qview, gpointer item,
                                 gpointer user_data)
{
    GNCReconcileView *view = GNC_RECONCILE_VIEW (user_data);
    Split *split = qview->toggled_entry;
    gboolean target = GPOINTER_TO_INT (item);
    gboolean current;

    if (!split)
        return;
    current = g_hash_table_contains (view->reconciled, split);
    if (current != target)
        gnc_reconcile_view_toggle (view, split);
    if (qview->sort_column == REC_RECN - 1)
    {
        gnc_query_view_select_entry (qview, split, TRUE);
        gnc_query_view_refresh (qview);
        gnc_query_force_scroll_to_selection (qview);
    }
}

static void
gnc_reconcile_view_double_click_entry (GNCQueryView *qview, gpointer item,
                                       gpointer user_data)
{
    GNCReconcileView *view = GNC_RECONCILE_VIEW (user_data);

    g_signal_emit (view, reconcile_view_signals[DOUBLE_CLICK_SPLIT], 0, item);
    (void)qview;
}

static void
gnc_reconcile_view_row_selected (GNCQueryView *qview, gpointer item,
                                 gpointer user_data)
{
    GNCReconcileView *view = GNC_RECONCILE_VIEW (user_data);

    g_signal_emit (view, reconcile_view_signals[LINE_SELECTED], 0, item);
    (void)qview;
}

gint
gnc_reconcile_view_num_selected (GNCReconcileView *view)
{
    GList *entries;
    gint count;

    g_return_val_if_fail (GNC_IS_RECONCILE_VIEW (view), 0);
    entries = gnc_query_view_get_selected_entry_list (GNC_QUERY_VIEW (view));
    count = g_list_length (entries);
    g_list_free (entries);
    return count;
}

static gboolean
gnc_reconcile_view_set_toggle (GNCReconcileView *view)
{
    GList *entries = gnc_query_view_get_selected_entry_list (GNC_QUERY_VIEW (view));
    gboolean all_reconciled = entries != NULL;

    for (GList *node = entries; node; node = node->next)
        if (!g_hash_table_contains (view->reconciled, node->data))
            all_reconciled = FALSE;
    g_list_free (entries);
    return !all_reconciled;
}

void
gnc_reconcile_view_set_list (GNCReconcileView *view, gboolean reconcile)
{
    GList *entries;
    gpointer last = NULL;
    gboolean changed = FALSE;

    g_return_if_fail (GNC_IS_RECONCILE_VIEW (view));
    entries = gnc_query_view_get_selected_entry_list (GNC_QUERY_VIEW (view));
    for (GList *node = entries; node; node = node->next)
    {
        gboolean current = g_hash_table_contains (view->reconciled, node->data);

        last = node->data;
        if (current != reconcile)
        {
            gnc_reconcile_view_toggle (view, node->data);
            changed = TRUE;
        }
    }
    g_list_free (entries);
    if (changed)
        gnc_query_view_refresh (GNC_QUERY_VIEW (view));
    if (last && GNC_QUERY_VIEW (view)->sort_column == REC_RECN - 1)
    {
        gnc_query_view_select_entry (GNC_QUERY_VIEW (view), last, FALSE);
        gnc_query_force_scroll_to_selection (GNC_QUERY_VIEW (view));
    }
}

static gboolean
gnc_reconcile_view_key_press_cb (GtkEventControllerKey *controller, guint keyval,
                                 guint keycode, GdkModifierType state,
                                 gpointer user_data)
{
    GNCReconcileView *view = GNC_RECONCILE_VIEW (user_data);

    if (keyval != GDK_KEY_space)
        return FALSE;
    gnc_reconcile_view_set_list (view, gnc_reconcile_view_set_toggle (view));
    (void)controller;
    (void)keycode;
    (void)state;
    return TRUE;
}

gint
gnc_reconcile_view_get_num_splits (GNCReconcileView *view)
{
    g_return_val_if_fail (GNC_IS_RECONCILE_VIEW (view), 0);
    return gnc_query_view_get_num_entries (GNC_QUERY_VIEW (view));
}

Split *
gnc_reconcile_view_get_current_split (GNCReconcileView *view)
{
    g_return_val_if_fail (GNC_IS_RECONCILE_VIEW (view), NULL);
    return gnc_query_view_get_selected_entry (GNC_QUERY_VIEW (view));
}

static gpointer
gnc_reconcile_view_is_reconciled (gpointer item, gpointer user_data)
{
    GNCReconcileView *view = GNC_RECONCILE_VIEW (user_data);

    return GINT_TO_POINTER (item && g_hash_table_contains (view->reconciled, item));
}

static gboolean
grv_refresh_helper (gpointer key, gpointer value, gpointer user_data)
{
    (void)value;
    return !gnc_query_view_item_in_view (GNC_QUERY_VIEW (user_data), key);
}

void
gnc_reconcile_view_refresh (GNCReconcileView *view)
{
    g_return_if_fail (GNC_IS_RECONCILE_VIEW (view));

    gnc_query_view_refresh (GNC_QUERY_VIEW (view));
    gnc_query_force_scroll_to_selection (GNC_QUERY_VIEW (view));
    g_hash_table_foreach_remove (view->reconciled, grv_refresh_helper, view);
}

static void
grv_balance_hash_helper (gpointer key, gpointer value, gpointer user_data)
{
    gnc_numeric *total = user_data;

    *total = gnc_numeric_add_fixed (*total, xaccSplitGetAmount (key));
    (void)value;
}

gnc_numeric
gnc_reconcile_view_reconciled_balance (GNCReconcileView *view)
{
    gnc_numeric total = gnc_numeric_zero ();

    g_return_val_if_fail (GNC_IS_RECONCILE_VIEW (view), total);
    g_hash_table_foreach (view->reconciled, grv_balance_hash_helper, &total);
    return gnc_numeric_abs (total);
}

static void
grv_commit_hash_helper (gpointer key, gpointer value, gpointer user_data)
{
    time64 *date = user_data;

    xaccSplitSetReconcile (key, YREC);
    xaccSplitSetDateReconciledSecs (key, *date);
    (void)value;
}

void
gnc_reconcile_view_commit (GNCReconcileView *view, time64 date)
{
    g_return_if_fail (GNC_IS_RECONCILE_VIEW (view));

    gnc_suspend_gui_refresh ();
    g_hash_table_foreach (view->reconciled, grv_commit_hash_helper, &date);
    gnc_resume_gui_refresh ();
}

void
gnc_reconcile_view_postpone (GNCReconcileView *view)
{
    GList *entries;

    g_return_if_fail (GNC_IS_RECONCILE_VIEW (view));
    entries = gnc_query_view_get_entry_list (GNC_QUERY_VIEW (view));
    gnc_suspend_gui_refresh ();
    for (GList *node = entries; node; node = node->next)
    {
        Split *split = node->data;

        if (view->statement_date >= xaccTransGetDate (xaccSplitGetParent (split)) ||
            g_hash_table_contains (view->reconciled, split))
            xaccSplitSetReconcile (split, g_hash_table_contains (view->reconciled, split) ? CREC : NREC);
    }
    gnc_resume_gui_refresh ();
    g_list_free (entries);
}

void
gnc_reconcile_view_unselect_all (GNCReconcileView *view)
{
    g_return_if_fail (GNC_IS_RECONCILE_VIEW (view));
    gnc_query_view_unselect_all (GNC_QUERY_VIEW (view));
}

gboolean
gnc_reconcile_view_changed (GNCReconcileView *view)
{
    g_return_val_if_fail (GNC_IS_RECONCILE_VIEW (view), FALSE);
    return g_hash_table_size (view->reconciled) != 0;
}
