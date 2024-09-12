/** gnc-budget_view.c -- Budget display widget
 *
 * @addtogroup budget Budgets
 * @{
 * @file gnc-budget-view.c
 * @brief File to define budget views for gnucash (the actual display
          of the budget, along with some calculations and event handlers).
 * @author Phil Longstaff Copyright (C) 2013 phil.longstaff@yahoo.ca
 *
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
 ******************************************************************
*/

/*
 * TODO:
 *
 * *) I'd like to be able to update the budget estimates on a per cell
 * basis, instead of a whole row (account) at one time.  But, that
 * would require some major coding.
 *
 */

#include <config.h>

#include <gtk/gtk.h>
#ifdef __G_IR_SCANNER__
#undef __G_IR_SCANNER__
#endif
#include <gdk/gdkkeysyms.h>
#ifdef G_OS_WIN32
# include <gdk/gdkwin32.h>
#endif
#include <glib/gi18n.h>
#include "gnc-date-edit.h"

#include "gnc-budget-view.h"
#include "gnc-budget.h"
#include "gnc-features.h"

#include "dialog-utils.h"
#include "gnc-gnome-utils.h"
#include "gnc-gobject-utils.h"
#include "gnc-gtk-utils.h"
#include "gnc-icons.h"
#include "gnc-prefs.h"

#include "gnc-session.h"
#include "gnc-tree-view-account.h"
#include "gnc-ui.h"
#include "gnc-ui-util.h"
#include "gnc-main-window.h"
#include "gnc-component-manager.h"
#include "gnc-state.h"
#include "gnc-cell-renderer-text-flag.h"

#include "qof.h"

#include "gnc-recurrence.h"
#include "Recurrence.h"
#include "gnc-tree-model-account-types.h"
#include "gnc-locale-utils.h"


/* This static indicates the debugging module that this .o belongs to. */
static QofLogModule log_module = GNC_MOD_BUDGET;

#define PLUGIN_PAGE_BUDGET_CM_CLASS "budget-view"
#define STATE_SECTION_PREFIX "Budget"

typedef struct GncBudgetViewPrivate GncBudgetViewPrivate;

struct _GncBudgetView
{
    GtkBox w;
};

struct _GncBudgetViewClass
{
    GtkBoxClass w;
};

enum
{
    TOTALS_TYPE_INCOME,        /**< This total is Income type*/
    TOTALS_TYPE_EXPENSES,      /**< This total is Expenses type*/
    TOTALS_TYPE_ASSET_LIAB_EQ, /**< This total is Asset/Liab/Equity type*/
    TOTALS_TYPE_REMAINDER      /**< This total is Remaining to Budget*/
};
/**< \brief ENUM for different budget totals types.

This enum is used to specify the specific type of account that the
 selected account belongs to. This is important to ensure that the sum
 of the different types of accounts can be calculated.
*/


/************************************************************
 *                        Prototypes                        *
 ************************************************************/
/* Plugin Actions */
static void gnc_budget_view_finalize (GObject *object);

static void gbv_create_widget (GncBudgetView *budget_view);
#if 0
static gboolean gbv_button_press_cb (GtkWidget *widget, GdkEventButton *event,
                                     GncBudgetView *budget_view);
#endif
static gboolean gbv_key_press_cb (GtkWidget *treeview, GdkEventKey *event,
                                  gpointer user_data);
static void gbv_row_activated_cb (GtkTreeView *treeview, GtkTreePath *path,
                                  GtkTreeViewColumn *col, GncBudgetView *budget_view);
static gboolean query_tooltip_tree_view_cb (GtkWidget *widget, gint x, gint y,
                                            gboolean keyboard_tip,
                                            GtkTooltip *tooltip,
                                            GncBudgetView *budget_view);
#if 0
static void gbv_selection_changed_cb (GtkTreeSelection *selection,
                                      GncBudgetView *budget_view);
#endif
static void gbv_treeview_resized_cb (GtkWidget *widget, GtkAllocation *allocation,
                                     GncBudgetView *budget_view);
static gnc_numeric gbv_get_accumulated_budget_amount (GncBudget *budget,
                                     Account *account, guint period_num);

/** \brief the private budget view structure

    This structure defines the different elements required for a budget view -
    the actual display of how a budget looks when you open it.
        @param tree_view Pointer to the widget to display the detailed budget.
        @param totals_tree_view Pointer to the widget to display the totals tree at the bottom of the budget screen.
        @param totals_scroll_window the main scrolled window for the totals tree view
        @param hadj the account scroll window horizontal adjustment
        @param budget Contains much of the data required to implement a budget.
        @param key Each budget struct has its own GUID.
        @param period_col_list List of columns in the tree_view widget (this list varies depending on the number of periods)
        @param totals_col_list List of columns in the totals_tree_view
        @param total_col The totals column on the right of all the accounts.
        @param fd No idea what this does.
*/
struct GncBudgetViewPrivate
{
    GtkTreeView   *tree_view;
    GtkTreeView   *totals_tree_view;
    GtkWidget     *totals_scroll_window;
    GtkAdjustment *hadj;

    GncBudget *budget;
    GncGUID    key;
    gboolean   use_red_color;

    GList               *period_col_list;
    GList               *totals_col_list;
    GtkTreeViewColumn   *total_col;
    AccountFilterDialog *fd;
    Account             *rootAcct;
    gboolean             show_account_code;
    gboolean             show_account_desc;

    GtkCellRenderer *temp_cr;
    GtkCellEditable *temp_ce;
};

G_DEFINE_TYPE_WITH_PRIVATE(GncBudgetView, gnc_budget_view, GTK_TYPE_BOX)

#define GNC_BUDGET_VIEW_GET_PRIVATE(o)  \
   ((GncBudgetViewPrivate*)gnc_budget_view_get_instance_private((GncBudgetView*)o))

/** \brief Create new gnc budget view.

    As the name suggests, this creates a new gnc budget view.
*/
GncBudgetView *
gnc_budget_view_new (GncBudget *budget, AccountFilterDialog *fd)
{
    GncBudgetView        *budget_view;
    GncBudgetViewPrivate *priv;

    g_return_val_if_fail (GNC_IS_BUDGET(budget), NULL);
    ENTER(" ");

    budget_view = g_object_new (GNC_TYPE_BUDGET_VIEW, NULL);

    priv = GNC_BUDGET_VIEW_GET_PRIVATE(budget_view);
    priv->budget = budget;
    priv->key = *gnc_budget_get_guid (budget);
    priv->fd = fd;
    priv->total_col = NULL;
    priv->show_account_code = FALSE;
    priv->show_account_desc = FALSE;
    gbv_create_widget (budget_view);

    LEAVE("new budget view %p", budget_view);
    return budget_view;
}

static void
gnc_budget_view_class_init (GncBudgetViewClass *klass)
{
    GObjectClass *object_class = G_OBJECT_CLASS(klass);

    object_class->finalize = gnc_budget_view_finalize;

    g_signal_new ("account-activated", GNC_TYPE_BUDGET_VIEW, G_SIGNAL_RUN_LAST,
                  0, NULL, NULL, NULL, G_TYPE_NONE, 1, GNC_TYPE_ACCOUNT);
}

static void
gnc_budget_view_init (GncBudgetView *budget_view)
{
    GncBudgetViewPrivate *priv;

    ENTER("view %p", budget_view);

    gtk_orientable_set_orientation (GTK_ORIENTABLE(budget_view), GTK_ORIENTATION_VERTICAL);

    priv = GNC_BUDGET_VIEW_GET_PRIVATE(budget_view);

    /* Keep track of the root account */
    priv->rootAcct = gnc_book_get_root_account (gnc_get_current_book());

    LEAVE("");
}

static void
gbv_treeview_update_grid_lines (gpointer prefs, gchar *pref, gpointer user_data)
{
    GtkTreeView *view = user_data;
    gtk_tree_view_set_grid_lines (GTK_TREE_VIEW(view), gnc_tree_view_get_grid_lines_pref ());
}

void
gnc_budget_view_set_show_account_code (GncBudgetView *budget_view, gboolean show_account_code)
{
    GncBudgetViewPrivate *priv = GNC_BUDGET_VIEW_GET_PRIVATE(budget_view);
    priv->show_account_code = show_account_code;
    gnc_budget_view_refresh (budget_view);
}

gboolean
gnc_budget_view_get_show_account_code (GncBudgetView *budget_view)
{
    GncBudgetViewPrivate *priv = GNC_BUDGET_VIEW_GET_PRIVATE(budget_view);
    return priv->show_account_code;
}

void
gnc_budget_view_set_show_account_description (GncBudgetView *budget_view, gboolean show_account_desc)
{
    GncBudgetViewPrivate *priv = GNC_BUDGET_VIEW_GET_PRIVATE(budget_view);
    priv->show_account_desc = show_account_desc;
    gnc_budget_view_refresh (budget_view);
}

gboolean
gnc_budget_view_get_show_account_description (GncBudgetView *budget_view)
{
    GncBudgetViewPrivate *priv = GNC_BUDGET_VIEW_GET_PRIVATE(budget_view);
    return priv->show_account_desc;
}

static void
gbv_update_use_red (gpointer prefs, gchar *pref, gpointer user_data)
{
    GncBudgetView *budget_view = user_data;
    GncBudgetViewPrivate *priv = GNC_BUDGET_VIEW_GET_PRIVATE(budget_view);

    priv->use_red_color = gnc_prefs_get_bool (GNC_PREFS_GROUP_GENERAL,
                                              GNC_PREF_NEGATIVE_IN_RED);
}

static void
gnc_budget_view_finalize (GObject *object)
{
    GncBudgetView *budget_view;
    GncBudgetViewPrivate *priv;

    ENTER("object %p", object);
    budget_view = GNC_BUDGET_VIEW(object);
    g_return_if_fail (GNC_IS_BUDGET_VIEW(budget_view));

    priv = GNC_BUDGET_VIEW_GET_PRIVATE(budget_view);

    g_list_free (priv->period_col_list);
    g_list_free (priv->totals_col_list);

    gnc_prefs_remove_cb_by_func (GNC_PREFS_GROUP_GENERAL, GNC_PREF_GRID_LINES_HORIZONTAL,
                                 gbv_treeview_update_grid_lines, priv->totals_tree_view);
    gnc_prefs_remove_cb_by_func (GNC_PREFS_GROUP_GENERAL, GNC_PREF_GRID_LINES_VERTICAL,
                                 gbv_treeview_update_grid_lines, priv->totals_tree_view);
    gnc_prefs_remove_cb_by_func (GNC_PREFS_GROUP_GENERAL, GNC_PREF_NEGATIVE_IN_RED,
                                 gbv_update_use_red, budget_view);

    G_OBJECT_CLASS(gnc_budget_view_parent_class)->finalize (object);
    LEAVE(" ");
}

/** \brief returns the current selection in the gnc budget view.

    Returns the current selection in the gnc budget view by using the
    macro GNC_BUDGET_VIEW_GET_PRIVATE.
*/
GtkTreeSelection*
gnc_budget_view_get_selection (GncBudgetView *budget_view)
{
    GncBudgetViewPrivate *priv;

    g_return_val_if_fail (GNC_IS_BUDGET_VIEW(budget_view), NULL);

    priv = GNC_BUDGET_VIEW_GET_PRIVATE(budget_view);
    return gtk_tree_view_get_selection (GTK_TREE_VIEW(priv->tree_view));
}

Account*
gnc_budget_view_get_account_from_path (GncBudgetView *budget_view, GtkTreePath *path)
{
    GncBudgetViewPrivate *priv;

    g_return_val_if_fail (GNC_IS_BUDGET_VIEW(budget_view), NULL);

    priv = GNC_BUDGET_VIEW_GET_PRIVATE(budget_view);
    return gnc_tree_view_account_get_account_from_path (GNC_TREE_VIEW_ACCOUNT(priv->tree_view), path);
}

GtkWidget*
gnc_budget_view_get_account_tree_view (GncBudgetView *budget_view)
{
    GncBudgetViewPrivate *priv;

    g_return_val_if_fail (GNC_IS_BUDGET_VIEW(budget_view), NULL);

    priv = GNC_BUDGET_VIEW_GET_PRIVATE(budget_view);
    return GTK_WIDGET(priv->fd->tree_view);
}

GList*
gnc_budget_view_get_selected_accounts (GncBudgetView *budget_view)
{
    GncBudgetViewPrivate *priv;

    g_return_val_if_fail (GNC_IS_BUDGET_VIEW(budget_view), NULL);

    priv = GNC_BUDGET_VIEW_GET_PRIVATE(budget_view);
    return gnc_tree_view_account_get_selected_accounts (GNC_TREE_VIEW_ACCOUNT(priv->tree_view));
}

static void
gbv_totals_scrollbar_value_changed_cb (GtkAdjustment *adj, GncBudgetView *budget_view)
{
    GncBudgetViewPrivate *priv;

    g_return_if_fail (GNC_IS_BUDGET_VIEW(budget_view));

    priv = GNC_BUDGET_VIEW_GET_PRIVATE(budget_view);
    gtk_adjustment_set_value (priv->hadj, gtk_adjustment_get_value (adj));
}

static gboolean
gbv_totals_tree_view_redraw_idle (GtkTreeView *view)
{
    gtk_widget_queue_draw (GTK_WIDGET(view));
    return FALSE;
}

static void
gbv_tree_view_model_row_changed_cb (GtkTreeModel *tree_model, GtkTreePath *path,
                                    GtkTreeIter *iter, gpointer user_data)
{
    GncBudgetView *budget_view = user_data;
    GncBudgetViewPrivate *priv = GNC_BUDGET_VIEW_GET_PRIVATE(budget_view);

    // The model row-changed signal can be emitted multiple times so we
    // use an idle_add to do a redraw of the totals tree view once
    g_idle_remove_by_data (priv->totals_tree_view);
    g_idle_add ((GSourceFunc)gbv_totals_tree_view_redraw_idle, priv->totals_tree_view);
}

/****************************
 * GncPluginPage Functions  *
 ***************************/
/** \brief Creates necessary widgets for display of gnc budget.

    This function steps through and performs the necessary actions for
    creating the widgets associated with a budget view. For example,
    creating the trees for the accounts, creating the graphics objects,
    creating the links between actions and events etc.
*/
static void
gbv_create_widget (GncBudgetView *budget_view)
{
    GncBudgetViewPrivate *priv;
    GtkTreeSelection     *selection;
    GtkTreeView          *tree_view;
    GtkWidget            *scrolled_window;
    GtkAdjustment        *h_adj;
    GtkWidget            *h_scrollbar;
    GtkBox               *vbox;
    GtkListStore         *totals_tree_model;
    GtkTreeView          *totals_tree_view;
    GtkTreeViewColumn    *totals_title_col, *name_col, *code_col, *desc_col;
    GtkTreeIter           iter;
    GtkWidget            *h_separator;
    gchar                *state_section;
    gchar                 guidstr[GUID_ENCODING_LENGTH+1];

    priv = GNC_BUDGET_VIEW_GET_PRIVATE(budget_view);
    vbox = GTK_BOX(budget_view);

    // Set the name for this widget so it can be easily manipulated with css
    gtk_widget_set_name (GTK_WIDGET(vbox), "gnc-id-budget-page");

    // Accounts scroll window
    scrolled_window = gtk_scrolled_window_new (NULL, NULL);
    gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW(scrolled_window),
                                    GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);

    // Create Accounts tree_view
    tree_view = gnc_tree_view_account_new (FALSE);
    gtk_tree_view_set_headers_visible (tree_view, TRUE);

    guid_to_string_buff (&priv->key, guidstr);
    state_section = g_strjoin (" ", STATE_SECTION_PREFIX, guidstr, NULL);
    g_object_set (G_OBJECT(tree_view), "state-section", state_section, NULL);
    g_free (state_section);

    gnc_tree_view_configure_columns (GNC_TREE_VIEW(tree_view));
    priv->tree_view = tree_view;
    selection = gtk_tree_view_get_selection (tree_view);
    gtk_tree_selection_set_mode (selection, GTK_SELECTION_MULTIPLE);

    // make sure the account column is the expand column
    gnc_tree_view_expand_columns (GNC_TREE_VIEW(tree_view), "name", NULL);
    name_col = gnc_tree_view_find_column_by_name (GNC_TREE_VIEW(priv->tree_view), "name");
    gtk_tree_view_column_set_reorderable (name_col, FALSE);

    // Accounts filter
    priv->fd->tree_view = GNC_TREE_VIEW_ACCOUNT(priv->tree_view);
    gnc_tree_view_account_set_filter (GNC_TREE_VIEW_ACCOUNT(tree_view),
                                      gnc_plugin_page_account_tree_filter_accounts,
                                      priv->fd, NULL);

    // get the visibility of the account code column
    code_col = gnc_tree_view_find_column_by_name (GNC_TREE_VIEW(priv->tree_view), "account-code");
    priv->show_account_code = gtk_tree_view_column_get_visible (code_col);
    gtk_tree_view_column_set_reorderable (code_col, FALSE);

    // get the visibility of the account description column
    desc_col = gnc_tree_view_find_column_by_name (GNC_TREE_VIEW(priv->tree_view), "description");
    priv->show_account_desc = gtk_tree_view_column_get_visible (desc_col);
    gtk_tree_view_column_set_reorderable (desc_col, FALSE);

    // Add accounts tree view to scroll window
    gtk_container_add (GTK_CONTAINER(scrolled_window), GTK_WIDGET(tree_view));

    g_object_set (tree_view, "has-tooltip", TRUE, NULL);
    g_signal_connect (G_OBJECT(tree_view), "query-tooltip",
                      G_CALLBACK(query_tooltip_tree_view_cb), budget_view);
    g_signal_connect (G_OBJECT(tree_view), "row-activated",
                      G_CALLBACK(gbv_row_activated_cb), budget_view);

    // save the main scrolled window horizontal adjustment
    priv->hadj = gtk_scrolled_window_get_hadjustment (GTK_SCROLLED_WINDOW(scrolled_window));

    PINFO("Number of Created Account columns is %d", gtk_tree_view_get_n_columns (tree_view));

#if 0
    g_signal_connect (G_OBJECT(selection), "changed",
                      G_CALLBACK(gbv_selection_changed_cb), budget_view);
    g_signal_connect (G_OBJECT(tree_view), "button-press-event",
                      G_CALLBACK(gbv_button_press_cb), budget_view);
    gbv_selection_changed_cb (NULL, budget_view);
#endif

    // Totals scroll window
    priv->totals_scroll_window = gtk_scrolled_window_new (NULL, NULL);
    gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW(priv->totals_scroll_window),
                                    GTK_POLICY_AUTOMATIC, GTK_POLICY_NEVER); // horizontal/vertical

    h_adj = gtk_scrolled_window_get_hadjustment (GTK_SCROLLED_WINDOW(priv->totals_scroll_window));
    g_signal_connect (G_OBJECT(h_adj), "value-changed",
                      G_CALLBACK(gbv_totals_scrollbar_value_changed_cb), budget_view);

    // Create totals tree view
    totals_tree_model = gtk_list_store_new (4, G_TYPE_STRING, G_TYPE_INT, G_TYPE_STRING, G_TYPE_STRING);
    gtk_list_store_append (totals_tree_model, &iter);
    gtk_list_store_set (totals_tree_model, &iter, 0, _("Income"),
                                                  1, TOTALS_TYPE_INCOME, 2, " ", 3, " ", -1);
    gtk_list_store_append (totals_tree_model, &iter);
    gtk_list_store_set (totals_tree_model, &iter, 0, _("Expenses"),
                                                  1, TOTALS_TYPE_EXPENSES, 2, " ", 3, " ", -1);
    gtk_list_store_append (totals_tree_model, &iter);
    gtk_list_store_set (totals_tree_model, &iter, 0, _("Transfer"),
                                                  1, TOTALS_TYPE_ASSET_LIAB_EQ, 2, " ", 3, " ", -1);
    gtk_list_store_append (totals_tree_model, &iter);
    gtk_list_store_set (totals_tree_model, &iter, 0, _("Remaining to Budget"),
                                                  1, TOTALS_TYPE_REMAINDER, 2, " ", 3, " ", -1);

    totals_tree_view = GTK_TREE_VIEW(gtk_tree_view_new ());
    priv->totals_tree_view = totals_tree_view;
    gtk_tree_selection_set_mode (gtk_tree_view_get_selection (totals_tree_view), GTK_SELECTION_NONE);
    gtk_tree_view_set_headers_visible (totals_tree_view, FALSE);
    gtk_tree_view_set_model (totals_tree_view, GTK_TREE_MODEL(totals_tree_model));
    g_object_unref (totals_tree_model);

    // add the totals title column
    totals_title_col = gtk_tree_view_column_new_with_attributes ("", gtk_cell_renderer_text_new (), "text", 0, NULL);
    gtk_tree_view_column_set_expand (totals_title_col, TRUE);
    gtk_tree_view_column_set_sizing (totals_title_col, GTK_TREE_VIEW_COLUMN_FIXED);
    gtk_tree_view_append_column (totals_tree_view, totals_title_col);

    // add the totals account code column
    code_col = gtk_tree_view_column_new_with_attributes ("", gtk_cell_renderer_text_new(), "text", 2, NULL);
    gtk_tree_view_column_set_sizing (code_col, GTK_TREE_VIEW_COLUMN_FIXED);
    gtk_tree_view_append_column (totals_tree_view, code_col);
    gtk_tree_view_column_set_visible (code_col, priv->show_account_code);

    // add the totals account description column
    desc_col = gtk_tree_view_column_new_with_attributes ("", gtk_cell_renderer_text_new(), "text", 3, NULL);
    gtk_tree_view_column_set_sizing (desc_col, GTK_TREE_VIEW_COLUMN_FIXED);
    gtk_tree_view_append_column (totals_tree_view, desc_col);
    gtk_tree_view_column_set_visible (desc_col, priv->show_account_desc);

    // Add totals tree view to scroll window
    gtk_container_add (GTK_CONTAINER(priv->totals_scroll_window), GTK_WIDGET(totals_tree_view));

    // Set grid lines option to preference
    gtk_tree_view_set_grid_lines (GTK_TREE_VIEW(totals_tree_view), gnc_tree_view_get_grid_lines_pref ());
    gnc_prefs_register_cb (GNC_PREFS_GROUP_GENERAL, GNC_PREF_GRID_LINES_HORIZONTAL,
                           gbv_treeview_update_grid_lines, totals_tree_view);
    gnc_prefs_register_cb (GNC_PREFS_GROUP_GENERAL, GNC_PREF_GRID_LINES_VERTICAL,
                           gbv_treeview_update_grid_lines, totals_tree_view);

    // get initial value and register prefs call back for use red color
    priv->use_red_color = gnc_prefs_get_bool (GNC_PREFS_GROUP_GENERAL, GNC_PREF_NEGATIVE_IN_RED);
    gnc_prefs_register_cb (GNC_PREFS_GROUP_GENERAL, GNC_PREF_NEGATIVE_IN_RED,
                           gbv_update_use_red, budget_view);

    PINFO("Number of Created totals columns is %d", gtk_tree_view_get_n_columns (totals_tree_view));

    gtk_box_set_homogeneous (GTK_BOX(vbox), FALSE);

    gtk_box_pack_start (GTK_BOX(vbox), scrolled_window, /*expand*/TRUE, /*fill*/TRUE, 0);

    h_separator = gtk_separator_new (GTK_ORIENTATION_HORIZONTAL);
    gtk_box_pack_end (GTK_BOX(vbox), h_separator, /*expand*/FALSE, /*fill*/TRUE, 0);

    gtk_box_pack_start (GTK_BOX(vbox), GTK_WIDGET(priv->totals_scroll_window), /*expand*/FALSE, /*fill*/TRUE, 0);

    gtk_widget_show_all (GTK_WIDGET(vbox));

    // hide the account scroll window horizontal scroll bar
    h_scrollbar = gtk_scrolled_window_get_hscrollbar (GTK_SCROLLED_WINDOW(scrolled_window));
    gtk_widget_hide (h_scrollbar);

    g_signal_connect (G_OBJECT(tree_view), "size-allocate",
                      G_CALLBACK(gbv_treeview_resized_cb), budget_view);

    // Read account filter state information from budget section
    gnc_tree_view_account_restore_filter (GNC_TREE_VIEW_ACCOUNT(priv->tree_view),
                                          priv->fd,
                                          gnc_state_get_current(),
                                          gnc_tree_view_get_state_section (
                                          GNC_TREE_VIEW(priv->tree_view)));

    // use the model row-changed signal to do a redraw on the totals tree view
    g_signal_connect (G_OBJECT(gtk_tree_view_get_model (GTK_TREE_VIEW(tree_view))), "row-changed",
                      G_CALLBACK(gbv_tree_view_model_row_changed_cb), budget_view);

    gnc_budget_view_refresh (budget_view);
}

#define BUDGET_GUID "Budget GncGUID"

/***********************************************************************
 *  Save enough information about this view that it can                *
 *  be recreated next time the user starts gnucash.                    *
 *                                                                     *
 *  @param budget_view The view to save.                               *
 *                                                                     *
 *  @param key_file A pointer to the GKeyFile data structure where the *
 *  page information should be written.                                *
 *                                                                     *
 *  @param group_name The group name to use when saving data.          *
 **********************************************************************/
void
gnc_budget_view_save (GncBudgetView *budget_view, GKeyFile *key_file, const gchar *group_name)
{
    GncBudgetViewPrivate *priv;

    g_return_if_fail (budget_view != NULL);
    g_return_if_fail (key_file != NULL);
    g_return_if_fail (group_name != NULL);

    ENTER("view %p, key_file %p, group_name %s", budget_view, key_file, group_name);

    priv = GNC_BUDGET_VIEW_GET_PRIVATE(budget_view);

    // Save the account filter and page state information to page section
    gnc_tree_view_account_save (GNC_TREE_VIEW_ACCOUNT(priv->tree_view),
                                priv->fd, key_file, group_name);
    LEAVE(" ");
}


/***********************************************************************
 *  Create a new plugin page based on the information saved
 *  during a previous instantiation of gnucash.
 *
 *  @param budget_view The budget view to be restored
 *
 *  @param key_file A pointer to the GKeyFile data structure where the
 *  page information should be read.
 *
 *  @param group_name The group name to use when restoring data.
 *
 *  @return TRUE if successful, FALSE if unsuccessful
 **********************************************************************/
gboolean
gnc_budget_view_restore (GncBudgetView *budget_view, GKeyFile *key_file, const gchar *group_name)
{
    GncBudgetViewPrivate *priv;
    GError *error = NULL;
    char *guid_str;
    GncGUID guid;
    GncBudget *bgt;
    QofBook *book;
    gboolean has_guid;

    g_return_val_if_fail (key_file, FALSE);
    g_return_val_if_fail (group_name, FALSE);

    ENTER("key_file %p, group_name %s", key_file, group_name);

    guid_str = g_key_file_get_string (key_file, group_name, BUDGET_GUID,
                                      &error);
    if (error)
    {
        g_warning ("error reading group %s key %s: %s",
                   group_name, BUDGET_GUID, error->message);
        g_error_free (error);
        error = NULL;
        return FALSE;
    }
    has_guid = string_to_guid (guid_str, &guid);
    g_free (guid_str);

    if (!has_guid)
    {
        return FALSE;
    }

    book = qof_session_get_book (gnc_get_current_session());
    bgt = gnc_budget_lookup (&guid, book);
    if (!bgt)
    {
        return FALSE;
    }

    /* Create the new view */
    priv = GNC_BUDGET_VIEW_GET_PRIVATE(budget_view);

    // Restore the account filter and page state information from page section
    gnc_tree_view_account_restore (GNC_TREE_VIEW_ACCOUNT(priv->tree_view),
                                   priv->fd, key_file, group_name);
    LEAVE(" ");

    return TRUE;
}

/***********************************************************************
 *  The budget associated with this view is about to be removed from   *
 *  the book. So drop any saved state we still have.                   *
 *                                                                     *
 *  @param budget_view The view to which the budget is associated.     *
 **********************************************************************/
void
gnc_budget_view_delete_budget (GncBudgetView *budget_view)
{
    GncBudgetViewPrivate *priv;
    gchar guidstr[GUID_ENCODING_LENGTH+1];

    g_return_if_fail (budget_view != NULL);

    ENTER("view %p", budget_view);

    priv = GNC_BUDGET_VIEW_GET_PRIVATE(budget_view);

    guid_to_string_buff (&priv->key, guidstr);
    gnc_state_drop_sections_for (guidstr);
    g_object_set (G_OBJECT(priv->tree_view), "state-section", NULL, NULL);

    LEAVE(" ");
}

/***********************************************************************
 *  Save the Account filter information for this budget                *
 *                                                                     *
 *  @param budget_view The view to which the budget is associated.     *
 **********************************************************************/
void
gnc_budget_view_save_account_filter (GncBudgetView *budget_view)
{
    GncBudgetViewPrivate *priv;

    g_return_if_fail (budget_view != NULL);

    ENTER("view %p", budget_view);

    priv = GNC_BUDGET_VIEW_GET_PRIVATE(budget_view);

    // Save account filter state information to budget section
    gnc_tree_view_account_save_filter (GNC_TREE_VIEW_ACCOUNT(priv->tree_view),
                                       priv->fd, gnc_state_get_current (),
                                       gnc_tree_view_get_state_section (
                                       GNC_TREE_VIEW(priv->tree_view)));
    LEAVE(" ");
}

#if 0
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
gbv_button_press_cb (GtkWidget *widget, GdkEventButton *event,
                     GncBudgetView *budget_view)
{
    gboolean result;

    g_return_val_if_fail (budget_view != NULL, FALSE);

    ENTER("widget %p, event %p, page %p", widget, event, page);
    result = gnc_main_window_button_press_cb (widget, event, page);
    LEAVE(" ");
    return result;
}
#endif

/** \brief Key press action for gnc budget view when in editing mode.
 * Used for navigating with tab while editing.
 * The handler is for the cell-editable, not for the treeview
*/
static gboolean
gbv_key_press_cb (GtkWidget *widget, GdkEventKey *event, gpointer user_data)
{
    GtkTreeViewColumn    *col;
    GtkTreePath          *path = NULL;
    GncBudgetViewPrivate *priv = GNC_BUDGET_VIEW_GET_PRIVATE(user_data);
    GtkTreeView          *tv = priv->tree_view;
    gboolean              shifted;
    gint                  period_num, num_periods;
    gpointer              data;

    if (event->type != GDK_KEY_PRESS || !priv->temp_cr)
        return FALSE;

    switch (event->keyval)
    {
    case GDK_KEY_KP_Decimal:
        if (event->keyval == GDK_KEY_KP_Decimal)
        {
            struct lconv *lc = gnc_localeconv ();
            event->keyval = lc->mon_decimal_point[0];
            event->string[0] = lc->mon_decimal_point[0];
        }
        return FALSE;
    case GDK_KEY_Tab:
    case GDK_KEY_ISO_Left_Tab:
    case GDK_KEY_KP_Tab:
        shifted = event->state & GDK_SHIFT_MASK;
        gtk_tree_view_get_cursor (tv, &path, &col);
        if (!path)
            return TRUE;
        data        = g_object_get_data (G_OBJECT(col), "period_num");
        period_num  = GPOINTER_TO_UINT(data);
        num_periods = gnc_budget_get_num_periods (priv->budget);

        if (period_num >= num_periods)
            period_num = num_periods - 1;

        if (shifted)
            period_num--;
        else
            period_num++;

        if (period_num >= num_periods)
        {
            period_num = 0;
            if (gtk_tree_view_row_expanded (tv, path))
            {
                gtk_tree_path_down (path);
            }
            else
            {
                gtk_tree_path_next (path);
                while (!gnc_tree_view_path_is_valid (GNC_TREE_VIEW(tv), path) &&
                       gtk_tree_path_get_depth (path) > 1)
                {
                    gtk_tree_path_up (path);
                    gtk_tree_path_next (path);
                }
            }
        }
        else if (period_num < 0)
        {
            period_num = num_periods - 1;
            if (!gtk_tree_path_prev (path))
                gtk_tree_path_up (path);
            else
            {
                while (gtk_tree_view_row_expanded (tv, path))
                {
                    gtk_tree_path_down (path);
                    do
                    {
                        gtk_tree_path_next (path);
                    } while (
                        gnc_tree_view_path_is_valid (GNC_TREE_VIEW(tv), path));
                    gtk_tree_path_prev (path);
                }
            }
        }

        col = g_list_nth_data (priv->period_col_list, period_num);

        // finish editing
        if (priv->temp_ce)
        {
            gtk_cell_editable_editing_done (priv->temp_ce);
            gtk_cell_editable_remove_widget (priv->temp_ce);

            while (gtk_events_pending())
                gtk_main_iteration ();
        }

        if (gnc_tree_view_path_is_valid (GNC_TREE_VIEW(tv), path))
            gtk_tree_view_set_cursor (tv, path, col, TRUE);
        gtk_tree_path_free (path);
        break;
    default:
        return FALSE;
    }

    return TRUE;
}

/** \brief gnc budget view actions for resize of treeview.
*/
static void
gbv_treeview_resized_cb (GtkWidget *widget, GtkAllocation *allocation,
                         GncBudgetView *budget_view)
{
    GncBudgetViewPrivate* priv = GNC_BUDGET_VIEW_GET_PRIVATE(budget_view);
    GList *columns = gtk_tree_view_get_columns (GTK_TREE_VIEW(priv->tree_view));
    GList *total_columns = gtk_tree_view_get_columns (GTK_TREE_VIEW (priv->totals_tree_view));

    ENTER("");

    for (GList *node = columns, *total_node = total_columns;
         node; node = g_list_next (node))
    {
        GtkTreeViewColumn *tree_view_col = node->data;
        const gchar *name = g_object_get_data (G_OBJECT(tree_view_col), PREF_NAME);

        // if we do not show account code, step over the equivalent totals column
        if ((g_strcmp0 (name, "account-code") == 0) && (!priv->show_account_code))
            total_node = g_list_next (total_node);

        // if we do not show account description, step over the
        // equivalent totals column
        if ((g_strcmp0 (name, "description") == 0) && (!priv->show_account_desc))
            total_node = g_list_next (total_node);

        if (gtk_tree_view_column_get_visible (tree_view_col) && total_node != NULL)
        {
            gint col_width = gtk_tree_view_column_get_width (tree_view_col);
            GtkTreeViewColumn *totals_view_col = total_node->data;
            if (GTK_IS_TREE_VIEW_COLUMN(totals_view_col))
                gtk_tree_view_column_set_fixed_width (totals_view_col, col_width);
            total_node = g_list_next (total_node);
        }
    }
    // make sure the account column is the expand column
    gnc_tree_view_expand_columns (GNC_TREE_VIEW(priv->tree_view), "name", NULL);
    g_list_free (columns);
    g_list_free (total_columns);
    LEAVE("");
}

/** \brief Actions for when a Gnc budget view row is activated.
*/
static void
gbv_row_activated_cb (GtkTreeView *treeview, GtkTreePath *path,
                      GtkTreeViewColumn *col, GncBudgetView *budget_view)
{
    Account *account;

    g_return_if_fail (GNC_IS_BUDGET_VIEW(budget_view));

    account = gnc_tree_view_account_get_account_from_path (
                  GNC_TREE_VIEW_ACCOUNT(treeview), path);
    if (account == NULL)
        return;

    g_signal_emit_by_name (budget_view, "account-activated", account);
}

static gboolean
query_tooltip_tree_view_cb (GtkWidget *widget, gint x, gint y,
                            gboolean keyboard_tip, GtkTooltip *tooltip,
                            GncBudgetView *view)
{
    GtkTreeView          *tree_view = GTK_TREE_VIEW(widget);
    GncBudgetViewPrivate *priv = GNC_BUDGET_VIEW_GET_PRIVATE(view);
    GtkTreePath          *path  = NULL;
    GtkTreeViewColumn    *column = NULL;
    const gchar          *note;
    guint                 period_num;
    Account              *account;

    gtk_tree_view_convert_widget_to_bin_window_coords (tree_view, x, y, &x, &y);

    if (keyboard_tip || !gtk_tree_view_get_path_at_pos (tree_view, x, y, &path,
                                                        &column, NULL, NULL))
    {
        gtk_tree_path_free (path);
        return FALSE;
    }

    if (!column)
    {
        gtk_tree_path_free (path);
        return FALSE;
    }

    period_num = GPOINTER_TO_UINT(g_object_get_data (G_OBJECT(column), "period_num"));
    if (!period_num && priv->period_col_list->data != column)
    {
        gtk_tree_path_free (path);
        return FALSE;
    }
    account = gnc_tree_view_account_get_account_from_path (
                  GNC_TREE_VIEW_ACCOUNT(widget), path);
    note = gnc_budget_get_account_period_note (priv->budget, account, period_num);
    if (!note)
    {
        gtk_tree_path_free (path);
        return FALSE;
    }

    gtk_tooltip_set_text (tooltip, note);
    gtk_tree_view_set_tooltip_cell (tree_view, tooltip, path, column, NULL);
    gtk_tree_path_free (path);

    return TRUE;
}

/** \brief Action for when a selection in a gnc budget view is changed
*/
#if 0
static void
gbv_selection_changed_cb (GtkTreeSelection *selection, GncBudgetView *budget_view)
{
    GtkTreeView *tree_view;
    GList       *acct_list;
    gboolean     sensitive;

    if (!selection)
        sensitive = FALSE;
    else
    {
        g_return_if_fail (GTK_IS_TREE_SELECTION(selection));
        tree_view = gtk_tree_selection_get_tree_view (selection);
        acct_list = gnc_tree_view_account_get_selected_accounts (
                        GNC_TREE_VIEW_ACCOUNT(tree_view));

        /* Check here for placeholder accounts, etc. */
        sensitive = (g_list_length (acct_list) > 0);
        g_list_free (acct_list);
    }
}
#endif

/** \brief Structure to assist in calculating of sub account totals.

This structure is utilised by the functions \ref budget_accum_helper and \ref gbv_get_accumulated_budget_amount to find the totals of sub-accounts in an account tree.
@param total The running total of the account in question
@param budget The gnc budget under examination.
@param period_num The specific period_num that we are finding the totals for.
*/
typedef struct
{
    gnc_numeric    total;
    GncBudget     *budget;
    guint          period_num;
    GNCPriceDB    *pdb;
    gnc_commodity *total_currency;
} BudgetAccumulationInfo;

/** \brief Function to assist in the calculation of sub-account totals.

This function is used in conjunction with the function \ref gbv_get_accumulated_budget_amount to find the total of sub accounts. \ref gbv_get_accumulated_budget_amount passes this function to \ref gnc_account_foreach_child function in order to perform this operation. The latter method then calls \ref budget_accum_helper on all of the sub accounts of the main account passed in order to calculate the accumulated total.
*/
static void
budget_accum_helper (Account *account, gpointer data)
{
    BudgetAccumulationInfo *info = (BudgetAccumulationInfo*)data;
    gnc_numeric numeric;
    gnc_commodity *currency;

    currency = gnc_account_get_currency_or_parent (account);

    if (gnc_budget_is_account_period_value_set (info->budget, account, info->period_num))
    {
        numeric = gnc_budget_get_account_period_value (info->budget, account,
                                                       info->period_num);
        numeric = gnc_pricedb_convert_balance_nearest_price_t64 (
                    info->pdb, numeric, currency, info->total_currency,
                    gnc_budget_get_period_start_date (info->budget, info->period_num));
        info->total = gnc_numeric_add (info->total, numeric, GNC_DENOM_AUTO,
                                       GNC_HOW_DENOM_LCD);
    }
    else if (gnc_account_n_children (account) != 0)
    {
        numeric = gbv_get_accumulated_budget_amount (info->budget, account,
                                                     info->period_num);
        numeric = gnc_pricedb_convert_balance_nearest_price_t64 (
                    info->pdb, numeric, currency, info->total_currency,
                    gnc_budget_get_period_start_date (info->budget, info->period_num));

        info->total = gnc_numeric_add (info->total, numeric, GNC_DENOM_AUTO,
                                       GNC_HOW_DENOM_LCD);
    }
}

/** \brief Function to calculate the accumulated budget amount in a given account at a specified period number.

This function uses the \ref budget_accum_helper to calculate the accumulated budget amount in a given budget account for a specified period number. If the account does not have children, then it simply returns the balance of the account.
*/
static gnc_numeric
gbv_get_accumulated_budget_amount (GncBudget *budget, Account *account, guint period_num)
{
    BudgetAccumulationInfo info;

    info.total = gnc_numeric_zero ();
    info.budget = budget;
    info.period_num = period_num;
    info.pdb = gnc_pricedb_get_db (gnc_account_get_book (account));
    info.total_currency = gnc_account_get_currency_or_parent (account);

    if (!gnc_budget_is_account_period_value_set (budget, account, period_num))
        gnc_account_foreach_child (account, budget_accum_helper, &info);
    else
        info.total = gnc_budget_get_account_period_value (budget, account, period_num);

    return info.total;
}


/** \brief Calculates and displays budget amount for a period in a defined account.

   Displays budget amount for a period for an account.  If a budget
   amount is set, it is displayed in the default color.  If no budget
   amount is set and the account has children, the total of the children's
   budget amounts (if any) is displayed in dark grey.
*/
static gchar *
budget_col_source (Account *account, GtkTreeViewColumn *col,
                   GtkCellRenderer *cell)
{
    GncBudgetView *budget_view;
    GncBudgetViewPrivate *priv;
    guint period_num;
    gnc_numeric numeric;
    gchar amtbuff[100]; //FIXME: overkill, where's the #define?
    const gchar *note;

    budget_view = GNC_BUDGET_VIEW(g_object_get_data (G_OBJECT(col), "budget_view"));
    period_num = GPOINTER_TO_UINT(g_object_get_data (G_OBJECT(col), "period_num"));

    priv = GNC_BUDGET_VIEW_GET_PRIVATE(budget_view);

    if (!gnc_budget_is_account_period_value_set (priv->budget, account, period_num))
    {
        if (gnc_account_n_children (account) == 0)
            amtbuff[0] = '\0';
        else
        {
            GdkRGBA color;
            GtkStyleContext *stylectxt = gtk_widget_get_style_context (GTK_WIDGET(priv->tree_view));
            gtk_style_context_get_color (stylectxt, GTK_STATE_FLAG_NORMAL, &color);

            numeric = gbv_get_accumulated_budget_amount (priv->budget, account, period_num);

            if (gnc_reverse_balance (account))
                numeric = gnc_numeric_neg (numeric);

            xaccSPrintAmount (amtbuff, numeric, gnc_account_print_info (account, FALSE));
            if (gnc_is_dark_theme (&color))
                g_object_set (cell, "foreground",
                              priv->use_red_color && gnc_numeric_negative_p (numeric)
                                  ? "darkred"
                                  : "darkgray",
                              NULL);
            else
                g_object_set (cell, "foreground",
                              priv->use_red_color && gnc_numeric_negative_p (numeric)
                                  ? "PaleVioletRed"
                                  : "dimgray",
                              NULL);
        }
    }
    else
    {
        numeric = gnc_budget_get_account_period_value (priv->budget, account,
                                                       period_num);
        if (gnc_numeric_check (numeric))
            strcpy (amtbuff, "error");
        else
        {
            if (gnc_reverse_balance (account))
                numeric = gnc_numeric_neg (numeric);

            xaccSPrintAmount (amtbuff, numeric,
                              gnc_account_print_info (account, FALSE));

            if (priv->use_red_color && gnc_numeric_negative_p (numeric))
            {
                gchar *color = gnc_get_negative_color ();
                g_object_set (cell, "foreground", color, NULL);
                g_free (color);
            }
            else
                g_object_set (cell, "foreground", NULL, NULL);
        }
    }

    note = gnc_budget_get_account_period_note (priv->budget, account, period_num);
    g_object_set (cell, "flagged", note != NULL, NULL);

    return g_strdup (amtbuff);
}

/** \brief Function to find the total for an account for display in the
 totals column to the right.
*/
static gnc_numeric
bgv_get_total_for_account (Account *account, GncBudget *budget, gnc_commodity *new_currency)
{
    guint num_periods;
    int period_num;
    gnc_numeric numeric;
    gnc_numeric total = gnc_numeric_zero ();
    GNCPriceDB *pdb;
    gnc_commodity *currency;

    if (new_currency)
    {
        pdb      = gnc_pricedb_get_db (gnc_get_current_book ());
        currency = gnc_account_get_currency_or_parent (account);
    }

    num_periods = gnc_budget_get_num_periods (budget);
    for (period_num = 0; period_num < num_periods; ++period_num)
    {
        if (!gnc_budget_is_account_period_value_set (budget, account, period_num))
        {
            if (gnc_account_n_children (account) != 0)
            {
                numeric = gbv_get_accumulated_budget_amount (budget, account, period_num);

                if (new_currency)
                {
                    numeric = gnc_pricedb_convert_balance_nearest_price_t64 (
                                pdb, numeric, currency, new_currency,
                                gnc_budget_get_period_start_date (budget, period_num));
                }
                total = gnc_numeric_add (total, numeric, GNC_DENOM_AUTO, GNC_HOW_DENOM_LCD);
            }
        }
        else
        {
            numeric = gnc_budget_get_account_period_value (budget, account, period_num);
            if (!gnc_numeric_check (numeric))
            {
                if (new_currency)
                {
                    numeric = gnc_pricedb_convert_balance_nearest_price_t64 (
                                pdb, numeric, currency, new_currency,
                                gnc_budget_get_period_start_date (budget, period_num));
                }
                total = gnc_numeric_add (total, numeric, GNC_DENOM_AUTO, GNC_HOW_DENOM_LCD);
            }
        }
    }

    return total;
}

/** \brief Function to find and display the total for a specified account.
*/
static gchar *
budget_total_col_source (Account *account, GtkTreeViewColumn *col,
                         GtkCellRenderer *cell)
{
    GncBudgetView *budget_view;
    GncBudgetViewPrivate *priv;
    gnc_numeric total;
    gchar amtbuff[100]; //FIXME: overkill, where's the #define?

    budget_view = GNC_BUDGET_VIEW(g_object_get_data (G_OBJECT(col), "budget_view"));
    priv = GNC_BUDGET_VIEW_GET_PRIVATE(budget_view);

    total = bgv_get_total_for_account (account, priv->budget, NULL);
    if (gnc_reverse_balance (account))
        total = gnc_numeric_neg (total);

    xaccSPrintAmount (amtbuff, total, gnc_account_print_info (account, TRUE));

    if (priv->use_red_color && gnc_numeric_negative_p (total))
    {
        gchar *color = gnc_get_negative_color ();
        g_object_set (cell, "foreground", color, NULL);
        g_free (color);
    }
    else
        g_object_set (cell, "foreground", NULL, NULL);

    return g_strdup (amtbuff);
}

/** \brief Function to perform actions if an account has been edited
     (e.g. when removing or adding data values).

 Primarily this function is here to check to see if a cell has been
 updated to be zero so that the values in the children of that account
 can then be tallied for the value.
*/
static void
budget_col_edited (Account *account, GtkTreeViewColumn *col,
                   const gchar *new_text)
{
    GncBudgetView *budget_view;
    GncBudgetViewPrivate *priv;
    guint period_num;
    gnc_numeric numeric = gnc_numeric_error (GNC_ERROR_ARG);

    if (qof_book_is_readonly (gnc_get_current_book ()))
        return;

    if (!xaccParseAmount (new_text, TRUE, &numeric, NULL) &&
                !(new_text && *new_text == '\0'))
        return;

    period_num = GPOINTER_TO_UINT(g_object_get_data (G_OBJECT(col), "period_num"));

    budget_view = GNC_BUDGET_VIEW(g_object_get_data (G_OBJECT(col), "budget_view"));
    priv = GNC_BUDGET_VIEW_GET_PRIVATE(budget_view);

    if (new_text && *new_text == '\0')
        gnc_budget_unset_account_period_value (priv->budget, account, period_num);
    else
    {
        if (gnc_reverse_balance (account))
            numeric = gnc_numeric_neg (numeric);
        gnc_budget_set_account_period_value (priv->budget, account, period_num,
                                             numeric);
    }
}

/** \brief Function to find the total in a column of budget provided and
 display the info in the totals tree widget.

This function is called on each row within the totals tree
 (i.e. assets, expenses, transfers, and totals) in order to
 update the total values in the totals tree (grand totals at the bottom
 of the budget page). It looks at which type of account is currently being
 examined, and then calls the function
\ref gbv_get_accumulated_budget_amount on all of the relevant children
 accounts of the root. It then sets the value and color of the cell based
 on this information in the totals tree widget.
*/
static void
totals_col_source (GtkTreeViewColumn *col, GtkCellRenderer *cell,
                   GtkTreeModel *s_model, GtkTreeIter *s_iter,
                   gpointer user_data)
{
    gnc_numeric total = gnc_numeric_zero ();
    GncBudgetView *budget_view = GNC_BUDGET_VIEW(user_data);
    GncBudgetViewPrivate *priv = GNC_BUDGET_VIEW_GET_PRIVATE(budget_view);
    gint period_num = GPOINTER_TO_INT(g_object_get_data (G_OBJECT(col), "period_num"));
    GNCPriceDB *pdb = gnc_pricedb_get_db (gnc_get_current_book ());
    gnc_commodity *total_currency = gnc_default_currency ();
    GList *top_level_accounts = gnc_account_get_children (priv->rootAcct);
    gint row_type;

    gtk_tree_model_get (s_model, s_iter, 1, &row_type, -1);

    // step through each child account of the root, find the total
    // income, expenses, liabilities, and assets.
    for (GList *node = top_level_accounts; node; node = g_list_next (node))
    {
        Account *account = node->data;
        GNCAccountType acctype = xaccAccountTypeGetFundamental (xaccAccountGetType (account));

        if ((row_type == TOTALS_TYPE_INCOME && acctype == ACCT_TYPE_INCOME) ||
            (row_type == TOTALS_TYPE_EXPENSES && acctype == ACCT_TYPE_EXPENSE) ||
            (row_type == TOTALS_TYPE_REMAINDER) ||
            (row_type == TOTALS_TYPE_ASSET_LIAB_EQ &&
             (acctype == ACCT_TYPE_ASSET || acctype == ACCT_TYPE_LIABILITY ||
              acctype == ACCT_TYPE_EQUITY)))
        {
            gnc_numeric value; // used to assist in adding and subtracting
            // find the total for this account
            if (period_num < 0)
                value = bgv_get_total_for_account (account, priv->budget, total_currency);
            else
            {
                gnc_commodity *currency = gnc_account_get_currency_or_parent (account);
                value = gbv_get_accumulated_budget_amount
                    (priv->budget, account, period_num);

                value = gnc_pricedb_convert_balance_nearest_price_t64
                    (pdb, value, currency, total_currency,
                     gnc_budget_get_period_start_date (priv->budget, period_num));
            }

            total = gnc_numeric_add (total, value, GNC_DENOM_AUTO, GNC_HOW_DENOM_LCD);
        }
    }

    total = gnc_numeric_neg (total);

    GNCPrintAmountInfo pinfo = gnc_commodity_print_info (total_currency, period_num < 0);
    gchar *color = (priv->use_red_color && gnc_numeric_negative_p (total)) ?
        gnc_get_negative_color () : NULL;

    g_object_set (G_OBJECT(cell),
                  "text", xaccPrintAmount (total, pinfo),
                  "xalign", 1.0,
                  "foreground", color,
                  NULL);

    g_free (color);
    g_list_free (top_level_accounts);
}

/**
 \brief Function to refresh the titles of each column.

The function steps through the number of periods adding the dates to the first row of each of the columns that are listed as visible.
*/
static void
gbv_refresh_col_titles (GncBudgetView *budget_view)
{
    GncBudgetViewPrivate *priv;
    const Recurrence *r;
    GDate date, nextdate;
    gchar title[MAX_DATE_LENGTH + 1];

    g_return_if_fail (budget_view != NULL);
    priv = GNC_BUDGET_VIEW_GET_PRIVATE(budget_view);

    /* Show the dates in column titles */
    r = gnc_budget_get_recurrence (priv->budget);
    date = r->start;
    for (GList *node = priv->period_col_list; node; node = g_list_next (node))
    {
        GtkTreeViewColumn *col = GTK_TREE_VIEW_COLUMN (node->data);
        guint titlelen = qof_print_gdate (title, MAX_DATE_LENGTH, &date);

        if (titlelen > 0)
            gtk_tree_view_column_set_title (col, title);

        recurrenceNextInstance (r, &date, &nextdate);
        date = nextdate;
    }
}

static void
gbv_renderer_add_padding (GtkCellRenderer *renderer)
{
    gint xpad, ypad;

    gtk_cell_renderer_get_padding (renderer, &xpad, &ypad);
    if (xpad < 5)
        gtk_cell_renderer_set_padding (renderer, 5, ypad);
}

/** \brief Function to create the totals column to the right of the view.
*/
static GtkTreeViewColumn*
gbv_create_totals_column (GncBudgetView *budget_view, gint period_num)
{
    GtkTreeViewColumn *col;
    GtkCellRenderer* renderer;

    g_return_val_if_fail (budget_view != NULL, NULL);

    renderer = gtk_cell_renderer_text_new ();
    col = gtk_tree_view_column_new_with_attributes ("", renderer, NULL);

    // add some padding to the right of the numbers
    gbv_renderer_add_padding (renderer);

    gtk_tree_view_column_set_cell_data_func (col, renderer, totals_col_source, budget_view, NULL);
    g_object_set_data (G_OBJECT(col), "budget_view", budget_view);
    g_object_set_data (G_OBJECT(col), "period_num", GUINT_TO_POINTER(period_num));
    gtk_tree_view_column_set_sizing (col, GTK_TREE_VIEW_COLUMN_FIXED);

    return col;
}

/** \brief Function that updates the tree view when a column has been edited.

The function simply calls \ref gtk_widget_queue_draw on the current totals_tree_view.
*/
static void
gbv_col_edited_cb (GtkCellRendererText *cell, gchar *path_string,
                   gchar *new_text, gpointer user_data)
{
    GncBudgetView *budget_view = GNC_BUDGET_VIEW(user_data);
    GncBudgetViewPrivate *priv = GNC_BUDGET_VIEW_GET_PRIVATE(budget_view);

    gtk_widget_queue_draw (GTK_WIDGET(priv->totals_tree_view));
}

/* The main Start Editing Call back for the budget columns, for key navigation
 */
static void
gdv_editing_started_cb (GtkCellRenderer *cr, GtkCellEditable *editable,
                        const gchar *path_string, gpointer user_data)
{
    GncBudgetViewPrivate *priv = GNC_BUDGET_VIEW_GET_PRIVATE(user_data);

    priv->temp_cr = cr;
    priv->temp_ce = editable;

    g_signal_connect (G_OBJECT(editable), "key-press-event",
                      G_CALLBACK(gbv_key_press_cb), user_data);
}

static void
gdv_editing_canceled_cb (GtkCellRenderer *cr, gpointer user_data)
{
    GncBudgetViewPrivate *priv = GNC_BUDGET_VIEW_GET_PRIVATE(user_data);

    priv->temp_cr = NULL;
    priv->temp_ce = NULL;
}

/** \brief refreshes the current budget view

The function will step through to only display the columns that are set
 as visible, and will add any needed columns (e.g. the totals column).
*/
void
gnc_budget_view_refresh (GncBudgetView *budget_view)
{
    // Column identifiers
    enum {
        code_column         = 1,
        description_column  = 2,
        startPeriods_column = 3
        // The Totals column will be after the periods columns.
    };

    GncBudgetViewPrivate *priv;
    gint num_periods;
    gint num_periods_visible;
    GtkTreeViewColumn *col, *code_col, *desc_col;
    GList *col_list;
    GList *totals_col_list;
    GdkRGBA *note_color, *note_color_selected;
    GtkStyleContext *stylectxt;

    ENTER("view %p", budget_view);

    g_return_if_fail (budget_view != NULL);
    priv = GNC_BUDGET_VIEW_GET_PRIVATE(budget_view);

    stylectxt = gtk_widget_get_style_context (GTK_WIDGET(priv->tree_view));
    gtk_style_context_get (stylectxt, GTK_STATE_FLAG_SELECTED, "background-color", &note_color, NULL);
    gtk_style_context_get (stylectxt, GTK_STATE_FLAG_NORMAL, "background-color", &note_color_selected, NULL);

    num_periods = gnc_budget_get_num_periods (priv->budget);

    col_list = g_list_reverse (priv->period_col_list);
    totals_col_list = g_list_reverse (priv->totals_col_list);
    num_periods_visible = g_list_length (col_list);

    /* Hide any unneeded extra columns */
    while (num_periods_visible > num_periods)
    {
        col = GTK_TREE_VIEW_COLUMN (col_list->data);
        gtk_tree_view_remove_column (GTK_TREE_VIEW(priv->tree_view), col);
        col_list = g_list_delete_link (col_list, col_list);
        num_periods_visible--;

        col = GTK_TREE_VIEW_COLUMN(totals_col_list->data);
        gtk_tree_view_remove_column (GTK_TREE_VIEW(priv->totals_tree_view), col);
        totals_col_list = g_list_delete_link (totals_col_list, totals_col_list);
    }

    gnc_tree_view_configure_columns (GNC_TREE_VIEW(priv->tree_view));

    // set visibility of the account code columns
    code_col = gnc_tree_view_find_column_by_name (GNC_TREE_VIEW(priv->tree_view), "account-code");
    gtk_tree_view_column_set_visible (code_col, priv->show_account_code);
    code_col = gtk_tree_view_get_column (GTK_TREE_VIEW(priv->totals_tree_view), code_column);
    gtk_tree_view_column_set_visible (code_col, priv->show_account_code);

    // set visibility of the account description columns
    desc_col = gnc_tree_view_find_column_by_name (GNC_TREE_VIEW(priv->tree_view), "description");
    gtk_tree_view_column_set_visible (desc_col, priv->show_account_desc);
    desc_col = gtk_tree_view_get_column (GTK_TREE_VIEW(priv->totals_tree_view), description_column);
    gtk_tree_view_column_set_visible (desc_col, priv->show_account_desc);

    /* If we're creating new columns to be appended to already existing
     * columns, first delete the total column. (Then regenerate after
     * new columns have been appended */
    if (num_periods_visible != 0 && num_periods > num_periods_visible)
    {
        /* Delete the totals column */
        col = priv->total_col;
        gtk_tree_view_remove_column (GTK_TREE_VIEW(priv->tree_view), col);
        priv->total_col = NULL;
        col = gtk_tree_view_get_column (GTK_TREE_VIEW(priv->totals_tree_view),
        	startPeriods_column + num_periods_visible);
        gtk_tree_view_remove_column (GTK_TREE_VIEW(priv->totals_tree_view), col);
    }

    /* Create any needed columns */
    while (num_periods_visible < num_periods)
    {
        GtkCellRenderer *renderer = gnc_cell_renderer_text_flag_new ();
        g_object_set (renderer, "flag-color-rgba", note_color, NULL);
        g_object_set (renderer, "flag-color-rgba-selected", note_color_selected, NULL);

        col = gnc_tree_view_account_add_custom_column_renderer (
                  GNC_TREE_VIEW_ACCOUNT(priv->tree_view), "",
                  budget_col_source, budget_col_edited, renderer);
        g_object_set_data (G_OBJECT(col), "budget_view", budget_view);
        g_object_set_data (G_OBJECT(col), "period_num", GUINT_TO_POINTER(num_periods_visible));
        col_list = g_list_prepend (col_list, col);

        // add some padding to the right of the numbers
        gbv_renderer_add_padding (renderer);

        g_signal_connect (G_OBJECT(renderer), "edited", (GCallback)gbv_col_edited_cb, budget_view);
        g_signal_connect (G_OBJECT(renderer), "editing-started",
                          (GCallback)gdv_editing_started_cb, budget_view);
        g_signal_connect (G_OBJECT(renderer), "editing-canceled",
                          (GCallback)gdv_editing_canceled_cb, budget_view);
        col = gbv_create_totals_column (budget_view, num_periods_visible);
        if (col != NULL)
        {
            gtk_tree_view_append_column (priv->totals_tree_view, col);
            totals_col_list = g_list_prepend (totals_col_list, col);
        }

        num_periods_visible++;
    }

    gdk_rgba_free (note_color);
    gdk_rgba_free (note_color_selected);

    priv->period_col_list = g_list_reverse (col_list);
    priv->totals_col_list = g_list_reverse (totals_col_list);

    if (priv->total_col == NULL)
    {
        gchar title[MAX_DATE_LENGTH + 1];
        guint titlelen;
        GDate *date;
        GtkCellRenderer* renderer;

        priv->total_col = gnc_tree_view_account_add_custom_column (
                              GNC_TREE_VIEW_ACCOUNT(priv->tree_view), _("Total"),
                              budget_total_col_source, NULL);

        // set column title alignment to right to match column data
        gtk_tree_view_column_set_alignment (priv->total_col, 1.0);

        // set a minimum column size based on the date length, adds some space to the column
        date = g_date_new_dmy (31, 12, 2018);
        titlelen = qof_print_gdate (title, MAX_DATE_LENGTH, date);
        if (titlelen > 0)
        {
            PangoLayout *layout = gtk_widget_create_pango_layout (GTK_WIDGET(budget_view), title);
            PangoRectangle logical_rect;
            pango_layout_set_width (layout, -1);
            pango_layout_get_pixel_extents (layout, NULL, &logical_rect);
            g_object_unref (layout);

            gtk_tree_view_column_set_min_width (priv->total_col, logical_rect.width);
        }
        g_date_free (date);
        g_object_set_data (G_OBJECT(priv->total_col), "budget_view", budget_view);

        // as we only have one renderer/column, use this function to get it
        renderer = gnc_tree_view_column_get_renderer (priv->total_col);

        // add some padding to the right of the numbers
        gbv_renderer_add_padding (renderer);

        col = gbv_create_totals_column (budget_view, -1);
        if (col != NULL)
            gtk_tree_view_append_column (priv->totals_tree_view, col);
    }
    gbv_refresh_col_titles (budget_view);

    PINFO("Number of columns is %d, totals columns is %d",
          gtk_tree_view_get_n_columns (priv->tree_view), gtk_tree_view_get_n_columns (priv->totals_tree_view));

    LEAVE(" ");
}

