/** gnc-budget_view.c -- Budget display widget
 *
 * @addtogroup budget Budgets
 * @{
 * @file gnc-budget-view.c
 * @brief File to define budget views for gnucash (the actual display of the budget, along with some calculations and event handlers).
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
#include <glib/gi18n.h>
#include "gnc-date-edit.h"

#include "gnc-budget-view.h"
#include "gnc-budget.h"

#include "dialog-options.h"
#include "dialog-utils.h"
#include "gnc-gnome-utils.h"
#include "gnc-gobject-utils.h"
#include "gnc-icons.h"

#include "gnc-session.h"
#include "gnc-tree-view-account.h"
#include "gnc-ui.h"
#include "gnc-ui-util.h"
#include "option-util.h"
#include "gnc-main-window.h"
#include "gnc-component-manager.h"
#include "gnc-state.h"

#include "qof.h"

#include "gnc-recurrence.h"
#include "Recurrence.h"
#include "gnc-tree-model-account-types.h"


/* This static indicates the debugging module that this .o belongs to.  */
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
    TOTALS_TYPE_INCOME, /**< This total is Income type*/
    TOTALS_TYPE_EXPENSES, /**< This total is Expenses type*/
    TOTALS_TYPE_TRANSFERS, /**< This total is Transfers type*/
    TOTALS_TYPE_TOTAL /**< This total is for Totals*/
};
/**< \brief ENUM for different budget totals types.

This enum is used to specify the specific type of account that the selected account belongs to. This is important to ensure that the sum of the different types of accounts can be calculated.
*/


/************************************************************
 *                        Prototypes                        *
 ************************************************************/
/* Plugin Actions */
static void gnc_budget_view_class_init(GncBudgetViewClass *klass);
static void gnc_budget_view_init(GncBudgetView *budget_view);
static void gnc_budget_view_finalize(GObject *object);

static void gbv_create_widget(GncBudgetView *view);
#if 0
static gboolean gbv_button_press_cb(
    GtkWidget *widget, GdkEventButton *event, GncBudgetView *view);
#endif
#if 0
static gboolean gbv_key_press_cb(
    GtkWidget *treeview, GdkEventKey *event, gpointer userdata);
#endif
static void gbv_row_activated_cb(
    GtkTreeView *treeview, GtkTreePath *path, GtkTreeViewColumn *col,
    GncBudgetView *view);
#if 0
static void gbv_selection_changed_cb(
    GtkTreeSelection *selection, GncBudgetView *view);
#endif
static void gbv_treeview_resized_cb(GtkWidget* widget, GtkAllocation* allocation, GncBudgetView* view);
static gnc_numeric gbv_get_accumulated_budget_amount(GncBudget* budget,
        Account* account, guint period_num);

/** \brief the private budget view structure

    This structure defines the different elements required for a budget view - the actual display of how a budget looks when you open it.
        @param tree_view Pointer to the widget to display the detailed budget.
        @param totals_tree_view Pointer to the widget to display the totals tree at the bottom of the budget screen.
        @param budget Contains much of the data required to implement a budget.
        @param key Each budget struct has its own GUID.
        @param period_col_list List of columns in the tree_view widget (this list varies depending on the number of periods)
        @param totals_col_list List of columns in the totals_tree_view
        @param total_col The totals column on the right of all the accounts.
        @param fd No idea what this does.
*/
struct GncBudgetViewPrivate
{
    GtkTreeView *tree_view;
    GtkTreeView *totals_tree_view;

    GncBudget* budget;
    GncGUID key;

    GList *period_col_list;
    GList *totals_col_list;
    GtkTreeViewColumn* total_col;
    AccountFilterDialog *fd;

    Account* income;
    Account* expenses;
    Account* assets;
    Account* liabilities;
    Account* rootAcct;
};

#define GNC_BUDGET_VIEW_GET_PRIVATE(o)  \
   (G_TYPE_INSTANCE_GET_PRIVATE((o), GNC_TYPE_BUDGET_VIEW, GncBudgetViewPrivate))

G_DEFINE_TYPE(GncBudgetView, gnc_budget_view, GTK_TYPE_BOX)

/** \brief Create new gnc budget view.

    As the name suggests, this creates a new gnc budget view.
*/
GncBudgetView *
gnc_budget_view_new(GncBudget *budget, AccountFilterDialog* fd)
{
    GncBudgetView *budget_view;
    GncBudgetViewPrivate *priv;

    g_return_val_if_fail(GNC_IS_BUDGET(budget), NULL);
    ENTER(" ");

    budget_view = g_object_new(GNC_TYPE_BUDGET_VIEW, NULL);

    priv = GNC_BUDGET_VIEW_GET_PRIVATE(budget_view);
    priv->budget = budget;
    priv->key = *gnc_budget_get_guid(budget);
    priv->fd = fd;
    priv->total_col = NULL;
    gbv_create_widget(budget_view);

    LEAVE("new budget view %p", budget_view);
    return budget_view;
}

static void
gnc_budget_view_class_init(GncBudgetViewClass *klass)
{
    GObjectClass *object_class = G_OBJECT_CLASS(klass);

    gnc_budget_view_parent_class = g_type_class_peek_parent(klass);

    object_class->finalize = gnc_budget_view_finalize;

    g_signal_new("account-activated", GNC_TYPE_BUDGET_VIEW, G_SIGNAL_RUN_LAST, 0, NULL, NULL, NULL,
                 G_TYPE_NONE, 1, GNC_TYPE_ACCOUNT);

    g_type_class_add_private(klass, sizeof(GncBudgetViewPrivate));
}


static void
gnc_budget_view_init(GncBudgetView *budget_view)
{
    GncBudgetViewPrivate *priv;
    Account* root;
    gint num_top_accounts;
    gint i;

    ENTER("view %p", budget_view);
    priv = GNC_BUDGET_VIEW_GET_PRIVATE(budget_view);

    /* Keep track of the root and top level asset, liability, income and expense accounts */
    root = gnc_book_get_root_account(gnc_get_current_book());
    num_top_accounts = gnc_account_n_children(root);
    
    priv->rootAcct = root;

    for (i = 0; i < num_top_accounts; ++i)
    {
        Account* acc = gnc_account_nth_child(root, i);
        GNCAccountType type = xaccAccountGetType(acc);

        if (type == ACCT_TYPE_ASSET)
        {
            priv->assets = acc;
        }
        else if (type == ACCT_TYPE_LIABILITY)
        {
            priv->liabilities = acc;
        }
        else if (type == ACCT_TYPE_INCOME)
        {
            priv->income = acc;
        }
        else if (type == ACCT_TYPE_EXPENSE)
        {
            priv->expenses = acc;
        }
    }

    LEAVE("");
}


static void
gnc_budget_view_finalize(GObject *object)
{
    GncBudgetView *view;

    ENTER("object %p", object);
    view = GNC_BUDGET_VIEW(object);
    g_return_if_fail(GNC_IS_BUDGET_VIEW(view));

    G_OBJECT_CLASS(gnc_budget_view_parent_class)->finalize(object);
    LEAVE(" ");
}

/** \brief returns the current selection in the gnc budget view.

    Returns the current selection in the gnc budget view by using the macro GNC_BUDGET_VIEW_GET_PRIVATE.
*/
GtkTreeSelection*
gnc_budget_view_get_selection(GncBudgetView* view)
{
    GncBudgetViewPrivate *priv;

    g_return_val_if_fail(GNC_IS_BUDGET_VIEW(view), NULL);

    priv = GNC_BUDGET_VIEW_GET_PRIVATE(view);
    return gtk_tree_view_get_selection(GTK_TREE_VIEW(priv->tree_view));
}

Account*
gnc_budget_view_get_account_from_path(GncBudgetView* view, GtkTreePath* path)
{
    GncBudgetViewPrivate *priv;

    g_return_val_if_fail(GNC_IS_BUDGET_VIEW(view), NULL);

    priv = GNC_BUDGET_VIEW_GET_PRIVATE(view);
    return gnc_tree_view_account_get_account_from_path(GNC_TREE_VIEW_ACCOUNT(priv->tree_view), path);
}

GtkWidget*
gnc_budget_view_get_account_tree_view (GncBudgetView* view)
{
    GncBudgetViewPrivate *priv;
    
    g_return_val_if_fail(GNC_IS_BUDGET_VIEW(view), NULL);
        
    priv =  GNC_BUDGET_VIEW_GET_PRIVATE(view);
    return GTK_WIDGET(priv->fd->tree_view);
}

GList*
gnc_budget_view_get_selected_accounts(GncBudgetView* view)
{
    GncBudgetViewPrivate *priv;

    g_return_val_if_fail(GNC_IS_BUDGET_VIEW(view), NULL);

    priv = GNC_BUDGET_VIEW_GET_PRIVATE(view);
    return gnc_tree_view_account_get_selected_accounts(GNC_TREE_VIEW_ACCOUNT(priv->tree_view));
}

/****************************
 * GncPluginPage Functions  *
 ***************************/
/** \brief Creates necessary widgets for display of gnc budget.

    This function steps through and performs the necessary actions for creating the widgets associated with a budget view. For example, creating the trees for the accounts, creating the graphics objects, creating the links between actions and events etc.
*/
static void
gbv_create_widget(GncBudgetView *view)
{
    GncBudgetViewPrivate* priv;
    GtkTreeSelection *selection;
    GtkTreeView *tree_view;
    GtkWidget *scrolled_window;
    GtkWidget *inner_scrolled_window;
    GtkBox* vbox;
    GtkWidget* inner_vbox;
    GtkListStore* totals_tree_model;
    GtkTreeView* totals_tree_view;
    GtkTreeViewColumn* totals_title_col;
    GtkTreeIter iter;
    GtkWidget* h_separator;
    gchar *state_section;
    gchar guidstr[GUID_ENCODING_LENGTH+1];

    priv = GNC_BUDGET_VIEW_GET_PRIVATE(view);
    vbox = GTK_BOX(view);

    gtk_widget_show(GTK_WIDGET(vbox));
    gtk_box_set_homogeneous(GTK_BOX(vbox), FALSE);

    // Set the style context for this page so it can be easily manipulated with css
    gnc_widget_set_style_context (GTK_WIDGET(vbox), "GncBudgetPage");

    scrolled_window = gtk_scrolled_window_new(NULL, NULL);
    gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_window),
                                   GTK_POLICY_AUTOMATIC,
                                   GTK_POLICY_NEVER);
    gtk_widget_show(scrolled_window);
    gtk_box_pack_start(GTK_BOX(vbox), scrolled_window, /*expand*/TRUE, /*fill*/TRUE, 0);

    inner_vbox = gtk_box_new (GTK_ORIENTATION_VERTICAL, 0);
    gtk_box_set_homogeneous (GTK_BOX (inner_vbox), FALSE);
    gtk_container_add (GTK_CONTAINER(scrolled_window), GTK_WIDGET(inner_vbox));
    gtk_widget_show(GTK_WIDGET(inner_vbox));

    inner_scrolled_window = gtk_scrolled_window_new(NULL, NULL);
    gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(inner_scrolled_window),
                                   GTK_POLICY_NEVER,
                                   GTK_POLICY_AUTOMATIC);
    gtk_widget_show(inner_scrolled_window);
    tree_view = gnc_tree_view_account_new(FALSE);
    gtk_container_add(GTK_CONTAINER(inner_scrolled_window), GTK_WIDGET(tree_view));

    guid_to_string_buff(&priv->key, guidstr);
    state_section = g_strjoin(" ", STATE_SECTION_PREFIX, guidstr, NULL);
    g_object_set(G_OBJECT(tree_view), "state-section", state_section, NULL);
    g_free (state_section);

    gnc_tree_view_configure_columns(GNC_TREE_VIEW(tree_view));
    priv->tree_view = tree_view;
    selection = gtk_tree_view_get_selection(tree_view);
    gtk_tree_selection_set_mode(selection, GTK_SELECTION_MULTIPLE);

    g_signal_connect(G_OBJECT(tree_view), "row-activated",
                     G_CALLBACK(gbv_row_activated_cb), view);
    g_signal_connect(G_OBJECT(tree_view), "size-allocate",
                     G_CALLBACK(gbv_treeview_resized_cb), view);

#if 0
    g_signal_connect(G_OBJECT(selection), "changed",
                     G_CALLBACK(gbv_selection_changed_cb), view);
    g_signal_connect(G_OBJECT(tree_view), "button-press-event",
                     G_CALLBACK(gbv_button_press_cb), view);
    g_signal_connect_after(G_OBJECT(tree_view), "key-press-event",
                           G_CALLBACK(gbv_key_press_cb), NULL);

    gbv_selection_changed_cb(NULL, view);
#endif
    gtk_tree_view_set_headers_visible(tree_view, TRUE);
    gtk_widget_show(GTK_WIDGET(tree_view));
    gtk_box_pack_start(GTK_BOX(inner_vbox), GTK_WIDGET(inner_scrolled_window), /*expand*/TRUE, /*fill*/TRUE, 0);
    priv->fd->tree_view = GNC_TREE_VIEW_ACCOUNT(priv->tree_view);
    gnc_tree_view_account_set_filter(
        GNC_TREE_VIEW_ACCOUNT(tree_view),
        gnc_plugin_page_account_tree_filter_accounts,
        priv->fd, NULL);

    totals_tree_model = gtk_list_store_new(2, G_TYPE_STRING, G_TYPE_INT);
    gtk_list_store_append(totals_tree_model, &iter);
    gtk_list_store_set(totals_tree_model, &iter, 0, _("Income"), 1, TOTALS_TYPE_INCOME, -1);
    gtk_list_store_append(totals_tree_model, &iter);
    gtk_list_store_set(totals_tree_model, &iter, 0, _("Expenses"), 1, TOTALS_TYPE_EXPENSES, -1);
    gtk_list_store_append(totals_tree_model, &iter);
    gtk_list_store_set(totals_tree_model, &iter, 0, _("Transfers"), 1, TOTALS_TYPE_TRANSFERS, -1);
    gtk_list_store_append(totals_tree_model, &iter);
    gtk_list_store_set(totals_tree_model, &iter, 0, _("Total"), 1, TOTALS_TYPE_TOTAL, -1);

    totals_tree_view = GTK_TREE_VIEW(gtk_tree_view_new());
    priv->totals_tree_view = totals_tree_view;

    // Set grid lines option to preference
    gtk_tree_view_set_grid_lines (GTK_TREE_VIEW(totals_tree_view), gnc_tree_view_get_grid_lines_pref ());

    gtk_widget_show(GTK_WIDGET(totals_tree_view));
    gtk_tree_selection_set_mode(gtk_tree_view_get_selection(totals_tree_view),
                                GTK_SELECTION_NONE);
    gtk_tree_view_set_headers_visible(totals_tree_view, FALSE);
    gtk_tree_view_set_model(totals_tree_view, GTK_TREE_MODEL(totals_tree_model));

    totals_title_col = gtk_tree_view_column_new_with_attributes("", gtk_cell_renderer_text_new(), "text", 0, NULL);
    gtk_tree_view_column_set_expand(totals_title_col, TRUE);
    gtk_tree_view_column_set_sizing(totals_title_col, GTK_TREE_VIEW_COLUMN_FIXED);
    gtk_tree_view_append_column(totals_tree_view, totals_title_col);

    gtk_box_pack_end(GTK_BOX(inner_vbox), GTK_WIDGET(totals_tree_view), /*expand*/FALSE, /*fill*/TRUE, 0);

    h_separator = gtk_separator_new (GTK_ORIENTATION_HORIZONTAL);
    gtk_widget_show(h_separator);
    gtk_box_pack_end(GTK_BOX(inner_vbox), h_separator, /*expand*/FALSE, /*fill*/TRUE, 0);

    gnc_budget_view_refresh(view);
}


#define BUDGET_GUID "Budget GncGUID"

/***********************************************************************
 *  Save enough information about this view that it can                *
 *  be recreated next time the user starts gnucash.                    *
 *                                                                     *
 *  @param view The view to save.                                      *
 *                                                                     *
 *  @param key_file A pointer to the GKeyFile data structure where the *
 *  page information should be written.                                *
 *                                                                     *
 *  @param group_name The group name to use when saving data.          *
 **********************************************************************/
void
gnc_budget_view_save(GncBudgetView *view, GKeyFile *key_file, const gchar *group_name)
{
    GncBudgetViewPrivate *priv;

    g_return_if_fail(view != NULL);
    g_return_if_fail(key_file != NULL);
    g_return_if_fail(group_name != NULL);

    ENTER("view %p, key_file %p, group_name %s", view, key_file, group_name);

    priv = GNC_BUDGET_VIEW_GET_PRIVATE(view);

    //FIXME
    gnc_tree_view_account_save(GNC_TREE_VIEW_ACCOUNT(priv->tree_view),
                               priv->fd, key_file, group_name);
    LEAVE(" ");
}


/***********************************************************************
 *  Create a new plugin page based on the information saved
 *  during a previous instantiation of gnucash.
 *
 *  @param view The budget view to be restored
 *
 *  @param key_file A pointer to the GKeyFile data structure where the
 *  page information should be read.
 *
 *  @param group_name The group name to use when restoring data.
 *
 *  @return TRUE if successful, FALSE if unsuccessful
 **********************************************************************/
gboolean
gnc_budget_view_restore(GncBudgetView* view, GKeyFile *key_file, const gchar *group_name)
{
    GncBudgetViewPrivate *priv;
    GError *error = NULL;
    char *guid_str;
    GncGUID guid;
    GncBudget *bgt;
    QofBook *book;

    g_return_val_if_fail(key_file, FALSE);
    g_return_val_if_fail(group_name, FALSE);

    ENTER("key_file %p, group_name %s", key_file, group_name);

    guid_str = g_key_file_get_string(key_file, group_name, BUDGET_GUID,
                                     &error);
    if (error)
    {
        g_warning("error reading group %s key %s: %s",
                  group_name, BUDGET_GUID, error->message);
        g_error_free(error);
        error = NULL;
        return FALSE;
    }
    if (!string_to_guid(guid_str, &guid))
    {
        return FALSE;
    }

    book = qof_session_get_book(gnc_get_current_session());
    bgt = gnc_budget_lookup(&guid, book);
    if (!bgt)
    {
        return FALSE;
    }

    /* Create the new view */
    priv = GNC_BUDGET_VIEW_GET_PRIVATE(view);

    //FIXME
    gnc_tree_view_account_restore(GNC_TREE_VIEW_ACCOUNT(priv->tree_view),
                                  priv->fd, key_file, group_name);
    LEAVE(" ");

    return TRUE;
}

/***********************************************************************
 *  The budget associated with this view is about to be removed from   *
 *  the book. So drop any saved state we still have.                   *
 *                                                                     *
 *  @param view The view to which the budget is associated.            *
 **********************************************************************/
void
gnc_budget_view_delete_budget(GncBudgetView *view)
{
    GncBudgetViewPrivate *priv;
    gchar guidstr[GUID_ENCODING_LENGTH+1];

    g_return_if_fail(view != NULL);

    ENTER("view %p", view);

    priv = GNC_BUDGET_VIEW_GET_PRIVATE (view);

    guid_to_string_buff(&priv->key, guidstr);
    gnc_state_drop_sections_for (guidstr);
    g_object_set (G_OBJECT (priv->tree_view), "state-section", NULL, NULL);

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
gbv_button_press_cb(GtkWidget *widget, GdkEventButton *event,
                    GncBudgetView *view)
{
    gboolean result;

    g_return_val_if_fail(view != NULL, FALSE);

    ENTER("widget %p, event %p, page %p", widget, event, page);
    result = gnc_main_window_button_press_cb(widget, event, page);
    LEAVE(" ");
    return result;
}
#endif

#if 0
/** \brief Key press action for gnc budget view.
*/
static gboolean
gbv_key_press_cb(GtkWidget *treeview, GdkEventKey *event, gpointer userdata)
{
    GtkTreeView *tv = GTK_TREE_VIEW(treeview);
    GtkTreeViewColumn *col;
    GtkTreePath *path = NULL;

    if (event->type != GDK_KEY_PRESS) return TRUE;

    switch (event->keyval)
    {
    case GDK_KEY_Tab:
    case GDK_KEY_ISO_Left_Tab:
    case GDK_KEY_KP_Tab:
    case GDK_KEY_Return:
    case GDK_KEY_KP_Enter:
        gtk_tree_view_get_cursor(tv, &path, &col);
        if (!path) return TRUE;
        //finish_edit(col);
        break;
    default:
        return TRUE;
    }
    gnc_tree_view_keynav(GNC_TREE_VIEW(tv), &col, path, event);

    if (path && gnc_tree_view_path_is_valid(GNC_TREE_VIEW(tv), path))
        gtk_tree_view_set_cursor(tv, path, col, TRUE);
    return TRUE;
}
#endif

/** \brief gnc budget view actions for resize of treeview.
*/
static void
gbv_treeview_resized_cb(GtkWidget* widget, GtkAllocation* allocation, GncBudgetView* view)
{
    gint ncols;
    GncBudgetViewPrivate* priv;
    gint i;
    gint j;
    GList *columns;

    ENTER("");
    priv = GNC_BUDGET_VIEW_GET_PRIVATE(view);

    /* There's no easy way to get this number. */
    columns = gtk_tree_view_get_columns(GTK_TREE_VIEW(priv->tree_view));
    ncols = g_list_length(columns);
    g_list_free(columns);
    /* i is the column we are examining
     * j is the corresponding column in totals_tree_view */
    for (i = 0, j = 0; i < ncols; ++i)
    {
        gint col_width;
        GtkTreeViewColumn* tree_view_col;
        GtkTreeViewColumn* totals_view_col;

        tree_view_col = gtk_tree_view_get_column(priv->tree_view, i);

        if (gtk_tree_view_column_get_visible(tree_view_col))
        {
            col_width = gtk_tree_view_column_get_width(tree_view_col);
            totals_view_col = gtk_tree_view_get_column(priv->totals_tree_view, j);
            if(GTK_IS_TREE_VIEW_COLUMN(totals_view_col))
                gtk_tree_view_column_set_fixed_width(totals_view_col, col_width);
            j++;
        }
    }
    LEAVE("");
}

/** \brief Actions for when a Gnc budget view row is activated.
*/
static void
gbv_row_activated_cb(GtkTreeView *treeview, GtkTreePath *path,
                     GtkTreeViewColumn *col, GncBudgetView *view)
{
    Account *account;

    g_return_if_fail(GNC_IS_BUDGET_VIEW(view));
    account = gnc_tree_view_account_get_account_from_path(
                  GNC_TREE_VIEW_ACCOUNT(treeview), path);
    if (account == NULL)
    {
        return;
    }

    g_signal_emit_by_name(view, "account-activated", account);
}

/** \brief Action for when a selection in a gnc budget view is changed
*/
#if 0
static void
gbv_selection_changed_cb(GtkTreeSelection *selection, GncBudgetView *view)
{
    GtkTreeView *tree_view;
    GList *acct_list;
    gboolean sensitive;

    if (!selection)
    {
        sensitive = FALSE;
    }
    else
    {
        g_return_if_fail(GTK_IS_TREE_SELECTION(selection));
        tree_view = gtk_tree_selection_get_tree_view (selection);
        acct_list = gnc_tree_view_account_get_selected_accounts(
                        GNC_TREE_VIEW_ACCOUNT(tree_view));

        /* Check here for placeholder accounts, etc. */
        sensitive = (g_list_length(acct_list) > 0);
        g_list_free(acct_list);
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
    gnc_numeric total;
    GncBudget* budget;
    guint period_num;
} BudgetAccumulationInfo;

/** \brief Function to assist in the calculation of sub-account totals.

This function is used in conjunction with the function \ref gbv_get_accumulated_budget_amount to find the total of sub accounts. \ref gbv_get_accumulated_budget_amount passes this function to \ref gnc_account_foreach_child function in order to perform this operation. The latter method then calls \ref budget_accum_helper on all of the sub accounts of the main account passed in order to calculate the accumulated total.
*/
static void
budget_accum_helper(Account* account, gpointer data)
{
    BudgetAccumulationInfo* info = (BudgetAccumulationInfo*)data;
    gnc_numeric numeric;

    if (gnc_budget_is_account_period_value_set(info->budget, account, info->period_num))
    {
        numeric = gnc_budget_get_account_period_value(info->budget, account, info->period_num);
        info->total = gnc_numeric_add(info->total, numeric, GNC_DENOM_AUTO, GNC_HOW_DENOM_LCD);
    }
    else if (gnc_account_n_children(account) != 0)
    {
        numeric = gbv_get_accumulated_budget_amount(info->budget, account, info->period_num);
        info->total = gnc_numeric_add(info->total, numeric, GNC_DENOM_AUTO, GNC_HOW_DENOM_LCD);
    }
}

/** \brief Function to calculate the accumulated budget amount in a given account at a specified period number.

This function uses the \ref budget_accum_helper to calculate the accumulated budget amount in a given budget account for a specified period number. If the acocunt does not have children, then it simply returns the balance of the account.
*/
static gnc_numeric
gbv_get_accumulated_budget_amount(GncBudget* budget, Account* account, guint period_num)
{
    BudgetAccumulationInfo info;

    info.total = gnc_numeric_zero();
    info.budget = budget;
    info.period_num = period_num;
    

    
    if (!gnc_budget_is_account_period_value_set(budget, account, period_num))
    {
    	gnc_account_foreach_child(account, budget_accum_helper, &info);
    }
    else
    {
    	info.total = gnc_budget_get_account_period_value(budget, account, period_num);
    }
    	return info.total;
    
}

/** \brief Calculates and displays budget amount for a period in a defined account.

Displays budget amount for a period for an account.  If a budget
   amount is set, it is displayed in black.  If no budget amount is
   set and the account has children, the total of the children's
   budget amounts (if any) is displayed in dark grey.
*/
static gchar *
budget_col_source(Account *account, GtkTreeViewColumn *col,
                  GtkCellRenderer *cell)
{
    GncBudget *budget;
    guint period_num;
    gnc_numeric numeric;
    gchar amtbuff[100]; //FIXME: overkill, where's the #define?

    budget = GNC_BUDGET(g_object_get_data(G_OBJECT(col), "budget"));
    period_num = GPOINTER_TO_UINT(g_object_get_data(G_OBJECT(col),
                                  "period_num"));

    if (!gnc_budget_is_account_period_value_set(budget, account, period_num))
    {
        if (gnc_account_n_children(account) == 0)
        {
            amtbuff[0] = '\0';
        }
        else
        {
            numeric = gbv_get_accumulated_budget_amount(budget, account, period_num);
            xaccSPrintAmount(amtbuff, numeric,
                             gnc_account_print_info(account, FALSE));
            g_object_set(cell, "foreground", "dark gray", NULL);
        }
    }
    else
    {
        numeric = gnc_budget_get_account_period_value(budget, account,
                  period_num);
        if (gnc_numeric_check(numeric))
        {
            strcpy(amtbuff, "error");
        }
        else
        {
            xaccSPrintAmount(amtbuff, numeric,
                             gnc_account_print_info(account, FALSE));
            g_object_set(cell, "foreground", "black", NULL);
        }
    }
    return g_strdup(amtbuff);
}

/** \brief Function to find the total for an account for display in the totals column to the right.
*/
static gnc_numeric
bgv_get_total_for_account(Account* account, GncBudget* budget)
{
    guint num_periods;
    int period_num;
    gnc_numeric numeric;
    gnc_numeric total = gnc_numeric_zero();

    num_periods = gnc_budget_get_num_periods(budget);
    for (period_num = 0; period_num < num_periods; ++period_num)
    {
        if (!gnc_budget_is_account_period_value_set(budget, account, period_num))
        {
            if (gnc_account_n_children(account) != 0)
            {
                numeric = gbv_get_accumulated_budget_amount(budget, account, period_num);
                total = gnc_numeric_add(total, numeric, GNC_DENOM_AUTO, GNC_HOW_DENOM_LCD);
            }
        }
        else
        {
            numeric = gnc_budget_get_account_period_value(budget, account, period_num);
            if (!gnc_numeric_check(numeric))
            {
                total = gnc_numeric_add(total, numeric, GNC_DENOM_AUTO, GNC_HOW_DENOM_LCD);
            }
        }
    }

    return total;
}

/** \brief Function to find and display the total for a specified account.
*/
static gchar *
budget_total_col_source(Account *account, GtkTreeViewColumn *col,
                        GtkCellRenderer *cell)
{
    GncBudget *budget;
    gnc_numeric total = gnc_numeric_zero();
    gchar amtbuff[100]; //FIXME: overkill, where's the #define?

    budget = GNC_BUDGET(g_object_get_data(G_OBJECT(col), "budget"));
    total = bgv_get_total_for_account(account, budget);
    xaccSPrintAmount(amtbuff, total,
                     gnc_account_print_info(account, FALSE));
    return g_strdup(amtbuff);
}

/** \brief Function to perform actions if an account has been edited (e.g. when removing or adding data values).

Primarily this function is here to check to see if a cell has been updated to be zero so that the values in the children of that account can then be tallied for the value.
*/
static void
budget_col_edited(Account *account, GtkTreeViewColumn *col,
                  const gchar *new_text)
{
    GncBudget *budget;
    guint period_num;
    gnc_numeric numeric = gnc_numeric_error(GNC_ERROR_ARG);

    if (!xaccParseAmount(new_text, TRUE, &numeric, NULL) &&
            !(new_text && *new_text == '\0'))
        return;

    period_num = GPOINTER_TO_UINT(g_object_get_data(G_OBJECT(col),
                                  "period_num"));

    budget = GNC_BUDGET(g_object_get_data(G_OBJECT(col), "budget"));

    if (new_text && *new_text == '\0')
        gnc_budget_unset_account_period_value(budget, account, period_num);
    else
        gnc_budget_set_account_period_value(budget, account, period_num,
                                            numeric);
}

/** \brief Function to find the total in a column of budget provided and display the info in the totals tree widget.

This function is called on each row within the totals tree (i.e. assets, expenses, transfers, and totals) in order to 
update the total values in the totals tree (grand totals at the bottom of the budget page). It looks at which type of account is currently being examined, and then calls the function
\ref gbv_get_accumulated_budget_amount on all of the relevant children accounts of the root. It then sets the value and color of the cell based on this information in the totals tree widget.

*/
static void
totals_col_source(GtkTreeViewColumn *col, GtkCellRenderer *cell,
                  GtkTreeModel *s_model, GtkTreeIter *s_iter,
                  gpointer user_data)
{
    GncBudgetView* view;
    GncBudgetViewPrivate* priv;
    gint row_type;
    GncBudget *budget;
    Account* account; // used to make things easier in the adding up processes
    gint period_num;
    gnc_numeric value; // used to assist in adding and subtracting
    gchar amtbuff[100]; //FIXME: overkill, where's the #define?
    gint i;
    gint num_top_accounts;

    gnc_numeric totalincome = gnc_numeric_zero();
    gnc_numeric totalexpenses = gnc_numeric_zero();
    gnc_numeric totalassets = gnc_numeric_zero();
    gnc_numeric totalliabilities = gnc_numeric_zero();

    view = GNC_BUDGET_VIEW(user_data);
    priv = GNC_BUDGET_VIEW_GET_PRIVATE(view);

    gtk_tree_model_get(s_model, s_iter, 1, &row_type, -1);
    budget = GNC_BUDGET(g_object_get_data(G_OBJECT(col), "budget"));
    period_num = GPOINTER_TO_INT(g_object_get_data(G_OBJECT(col),
                                 "period_num"));


    num_top_accounts = gnc_account_n_children(priv->rootAcct);
    
    // step through each child account of the root, find the total income, expenses, liabilities, and assets.
    
    for (i = 0; i < num_top_accounts; ++i)
  	{
    	account = gnc_account_nth_child(priv->rootAcct, i);
    	
    	// find the total for this account
    	
    	if (period_num < 0)
    	{
    		value = bgv_get_total_for_account(account, budget);
    	}
    	else
    	{
    		value = gbv_get_accumulated_budget_amount(budget, account, period_num);
    	}

		// test for what account type, and add 'value' to the appopriate total
    	
    	if (xaccAccountGetType(account) == ACCT_TYPE_INCOME)
    	{
    		totalincome = gnc_numeric_add(totalincome, value, GNC_DENOM_AUTO, GNC_HOW_DENOM_LCD);
    	}
    	else if (xaccAccountGetType(account) == ACCT_TYPE_EXPENSE)
    	{
    		totalexpenses = gnc_numeric_add(totalexpenses, value, GNC_DENOM_AUTO, GNC_HOW_DENOM_LCD);
    	}
    	else if (xaccAccountGetType(account) == ACCT_TYPE_ASSET)
    	{
    		totalassets = gnc_numeric_add(totalassets, value, GNC_DENOM_AUTO, GNC_HOW_DENOM_LCD);
    	}
    	else if (xaccAccountGetType(account) == ACCT_TYPE_LIABILITY)
    	{
    		totalliabilities = gnc_numeric_add(totalliabilities, value, GNC_DENOM_AUTO, GNC_HOW_DENOM_LCD);
    	}
    	else
    	{
    		// Do nothing because this account is not of interest
    	}
    	
   	}

   
    
    // at this point we should have variables holding the values for assets, liabilities, expenses and incomes.
    
    // Set the text to display, depending on which of the totals rows we are currently looking at	
    	
    if (row_type == TOTALS_TYPE_INCOME)
    {
    	// FIXME: There must be a better way to get the GncAccountPrintInfo object than this. Would prefer to depreciate the tracking of top level accounts.
        xaccSPrintAmount(amtbuff, totalincome,
                         gnc_account_print_info(priv->income, FALSE));
        g_object_set(cell, "foreground", "black", NULL);
    }
    else if (row_type == TOTALS_TYPE_EXPENSES)
    {
       
        xaccSPrintAmount(amtbuff, totalexpenses,
                         gnc_account_print_info(priv->expenses, FALSE));
        g_object_set(cell, "foreground", "black", NULL);
    }
    else if (row_type == TOTALS_TYPE_TRANSFERS)
    {
    	
        xaccSPrintAmount(amtbuff, gnc_numeric_sub(totalassets, totalliabilities, GNC_DENOM_AUTO, GNC_HOW_DENOM_LCD),
                         gnc_account_print_info(priv->assets, FALSE));
        g_object_set(cell, "foreground", "black", NULL);
    }
    else if (row_type == TOTALS_TYPE_TOTAL)
    {
        value = gnc_numeric_sub(totalincome, totalexpenses, GNC_DENOM_AUTO, GNC_HOW_DENOM_LCD);
        value = gnc_numeric_sub(value, totalassets, GNC_DENOM_AUTO, GNC_HOW_DENOM_LCD);
        value = gnc_numeric_add(value, totalliabilities, GNC_DENOM_AUTO, GNC_HOW_DENOM_LCD);
        xaccSPrintAmount(amtbuff, value,
                         gnc_account_print_info(priv->assets, FALSE));
        if (gnc_numeric_negative_p(value))
        {
            g_object_set(cell, "foreground", "red", NULL);
        }
        else
        {
            g_object_set(cell, "foreground", "black", NULL);
        }
    }
    else
    {
    	// if it reaches here then the row type was not set correctly
        g_strlcpy(amtbuff, "error", sizeof(amtbuff));
    }

    g_object_set(G_OBJECT(cell), "text", amtbuff, "xalign", 1.0, NULL);
}


/**
 \brief Function to refresh the titles of each column.

The function steps through the number of periods adding the dates to the first row of each of the columns that are listed as visible.
*/
static void
gbv_refresh_col_titles(GncBudgetView *view)
{
    GncBudgetViewPrivate *priv;
    const Recurrence *r;
    GDate date, nextdate;
    GtkTreeViewColumn *col;
    guint titlelen;
    gint num_periods_visible;
    gchar title[MAX_DATE_LENGTH];
    GList *col_list;
    gint i;

    g_return_if_fail(view != NULL);
    priv = GNC_BUDGET_VIEW_GET_PRIVATE(view);

    col_list = priv->period_col_list;
    num_periods_visible = g_list_length(col_list);

    /* Show the dates in column titles */
    r = gnc_budget_get_recurrence(priv->budget);
    date = r->start;
    for (i = 0; i < num_periods_visible; i++)
    {
        col = GTK_TREE_VIEW_COLUMN(g_list_nth_data(col_list, i));
        titlelen = qof_print_gdate(title, MAX_DATE_LENGTH, &date);
        if (titlelen > 0)
        {
            gtk_tree_view_column_set_title(col, title);
        }
        recurrenceNextInstance(r, &date, &nextdate);
        date = nextdate;
    }
}

/** \brief Function to create the totals column to the right of the view.
*/
static GtkTreeViewColumn*
gbv_create_totals_column(GncBudgetView* view, gint period_num)
{
    GncBudgetViewPrivate *priv;
    GtkTreeViewColumn *col;
    GtkCellRenderer* renderer;

    g_return_val_if_fail(view != NULL, NULL);
    priv = GNC_BUDGET_VIEW_GET_PRIVATE(view);

    renderer = gtk_cell_renderer_text_new();
    col = gtk_tree_view_column_new_with_attributes("", renderer, NULL);

    gtk_tree_view_column_set_cell_data_func(col, renderer, totals_col_source, view, NULL);
    g_object_set_data(G_OBJECT(col), "budget", priv->budget);
    g_object_set_data(G_OBJECT(col), "period_num", GUINT_TO_POINTER(period_num));
    gtk_tree_view_column_set_sizing(col, GTK_TREE_VIEW_COLUMN_FIXED);

    return col;
}

/** \brief Function that updates the tree view when a column has been edited.

The function simply calls \ref gtk_widget_queue_draw on the current totals_tree_view.
*/
static void
gbv_col_edited_cb(GtkCellRendererText* cell, gchar* path_string, gchar* new_text, gpointer user_data)
{
    GncBudgetView *view;
    GncBudgetViewPrivate *priv;

    view = GNC_BUDGET_VIEW(user_data);
    priv = GNC_BUDGET_VIEW_GET_PRIVATE(view);

    gtk_widget_queue_draw(GTK_WIDGET(priv->totals_tree_view));
}


/** \brief refreshes the current budget view

The function will step through to only display the columns that are set as visible, and will add any needed columns (e.g. the totals column).
*/
void
gnc_budget_view_refresh(GncBudgetView *view)
{
    GncBudgetViewPrivate *priv;
    gint num_periods;
    gint num_periods_visible;
    GtkTreeViewColumn *col;
    GList *col_list;
    GList *totals_col_list;
    ENTER("view %p", view);

    g_return_if_fail(view != NULL);
    priv = GNC_BUDGET_VIEW_GET_PRIVATE(view);

    num_periods = gnc_budget_get_num_periods(priv->budget);
    col_list = priv->period_col_list;
    totals_col_list = priv->totals_col_list;
    num_periods_visible = g_list_length(col_list);

    /* Hide any unneeded extra columns */
    while (num_periods_visible > num_periods)
    {
        col = GTK_TREE_VIEW_COLUMN((g_list_last(col_list))->data);
        gtk_tree_view_remove_column(GTK_TREE_VIEW(priv->tree_view), col);
        col_list = g_list_delete_link(col_list, g_list_last(col_list));
        num_periods_visible = g_list_length(col_list);

        col = GTK_TREE_VIEW_COLUMN((g_list_last(totals_col_list))->data);
        gtk_tree_view_remove_column(GTK_TREE_VIEW(priv->totals_tree_view), col);
        totals_col_list = g_list_delete_link(totals_col_list, g_list_last(totals_col_list));
    }

    gnc_tree_view_configure_columns(GNC_TREE_VIEW(priv->tree_view));

    /* If we're creating new columns to be appended to already existing
     * columns, first delete the total column. (Then regenerate after
     * new columns have been appended */
    if (num_periods_visible != 0 && num_periods > num_periods_visible)
    {
        /* Delete the totals column */
        col = priv->total_col;
        gtk_tree_view_remove_column(GTK_TREE_VIEW(priv->tree_view), col);
        priv->total_col = NULL;
        col = gtk_tree_view_get_column(GTK_TREE_VIEW(priv->totals_tree_view), num_periods_visible+1);
        gtk_tree_view_remove_column(GTK_TREE_VIEW(priv->totals_tree_view), col);
    }

    /* Create any needed columns */
    while (num_periods_visible < num_periods)
    {
        GList* renderer_list;
        GList* renderer_node;

        col = gnc_tree_view_account_add_custom_column(
                  GNC_TREE_VIEW_ACCOUNT(priv->tree_view), "",
                  budget_col_source, budget_col_edited);
        g_object_set_data(G_OBJECT(col), "budget", priv->budget);
        g_object_set_data(G_OBJECT(col), "period_num",
                          GUINT_TO_POINTER(num_periods_visible));
        col_list = g_list_append(col_list, col);

        renderer_list = gtk_cell_layout_get_cells(GTK_CELL_LAYOUT(col));
        for (renderer_node = renderer_list; renderer_node != NULL; renderer_node = g_list_next(renderer_node))
        {
            GtkCellRenderer* renderer = GTK_CELL_RENDERER(renderer_node->data);
            g_signal_connect(G_OBJECT(renderer), "edited", (GCallback)gbv_col_edited_cb, view);
        }
        g_list_free(renderer_list);

        col = gbv_create_totals_column(view, num_periods_visible);
        if (col != NULL)
        {
            gtk_tree_view_append_column(priv->totals_tree_view, col);
            totals_col_list = g_list_append(totals_col_list, col);
        }

        num_periods_visible = g_list_length(col_list);
    }
    priv->period_col_list = col_list;
    priv->totals_col_list = totals_col_list;

    if (priv->total_col == NULL)
    {
        priv->total_col = gnc_tree_view_account_add_custom_column(
                              GNC_TREE_VIEW_ACCOUNT(priv->tree_view), _("Total"),
                              budget_total_col_source, NULL);
        g_object_set_data(G_OBJECT(priv->total_col), "budget", priv->budget);

        col = gbv_create_totals_column(view, -1);
        if (col != NULL)
        {
            gtk_tree_view_append_column(priv->totals_tree_view, col);
        }
    }

    gbv_refresh_col_titles(view);
    LEAVE(" ");
}

