/********************************************************************
 * gnc-budget_view.c -- Budget display widget                       *
 *                                                                  *
 * Copyright (C) 2013, Phil Longstaff <phil.longstaff@yahoo.ca>     *
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

#include "config.h"

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
    GtkVBox w;
};

struct _GncBudgetViewClass
{
    GtkVBoxClass w;
};

enum
{
    TOTALS_TYPE_INCOME,
    TOTALS_TYPE_EXPENSES,
    TOTALS_TYPE_TRANSFERS,
    TOTALS_TYPE_TOTAL
};

/************************************************************
 *                        Prototypes                        *
 ************************************************************/
/* Plugin Actions */
static void gnc_budget_view_class_init(GncBudgetViewClass *klass);
static void gnc_budget_view_init(GncBudgetView *budget_view);
static void gnc_budget_view_finalize(GObject *object);

static void gbv_create_widget(GncBudgetView *view);

static gboolean gbv_button_press_cb(
    GtkWidget *widget, GdkEventButton *event, GncBudgetView *view);
static gboolean gbv_key_press_cb(
    GtkWidget *treeview, GdkEventKey *event, gpointer userdata);
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
};

#define GNC_BUDGET_VIEW_GET_PRIVATE(o)  \
   (G_TYPE_INSTANCE_GET_PRIVATE((o), GNC_TYPE_BUDGET_VIEW, GncBudgetViewPrivate))

G_DEFINE_TYPE(GncBudgetView, gnc_budget_view, GTK_TYPE_VBOX)

GncBudgetView *
gnc_budget_view_new(GncBudget *budget, AccountFilterDialog* fd)
{
    GncBudgetView *budget_view;
    GncBudgetViewPrivate *priv;
    gchar* label;
    const GList *item;

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

    /* Keep track of the top level asset, liability, income and expense accounts */
    root = gnc_book_get_root_account(gnc_get_current_book());
    num_top_accounts = gnc_account_n_children(root);

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
    GncBudgetViewPrivate *priv;

    ENTER("object %p", object);
    view = GNC_BUDGET_VIEW(object);
    g_return_if_fail(GNC_IS_BUDGET_VIEW(view));

    G_OBJECT_CLASS(gnc_budget_view_parent_class)->finalize(object);
    LEAVE(" ");
}


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
static void
gbv_create_widget(GncBudgetView *view)
{
    GncBudgetViewPrivate* priv;
    GtkTreeSelection *selection;
    GtkTreeView *tree_view;
    GtkWidget *scrolled_window;
    GtkWidget *inner_scrolled_window;
    GtkVBox* vbox;
    GtkWidget* inner_vbox;
    GtkListStore* totals_tree_model;
    GtkTreeView* totals_tree_view;
    GtkTreeViewColumn* totals_title_col;
    GtkTreeIter iter;
    GtkWidget* h_separator;
    gchar *state_section;
    gchar guidstr[GUID_ENCODING_LENGTH+1];

    priv = GNC_BUDGET_VIEW_GET_PRIVATE(view);
    vbox = GTK_VBOX(view);

    gtk_widget_show(GTK_WIDGET(vbox));
    gtk_box_set_homogeneous(GTK_BOX(vbox), FALSE);

    scrolled_window = gtk_scrolled_window_new(NULL, NULL);
    gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_window),
                                   GTK_POLICY_AUTOMATIC,
                                   GTK_POLICY_NEVER);
    gtk_widget_show(scrolled_window);
    gtk_box_pack_start(GTK_BOX(vbox), scrolled_window, /*expand*/TRUE, /*fill*/TRUE, 0);

    inner_vbox = gtk_vbox_new(FALSE, 0);
    gtk_scrolled_window_add_with_viewport(GTK_SCROLLED_WINDOW(scrolled_window), GTK_WIDGET(inner_vbox));
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

    h_separator = gtk_hseparator_new();
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
    char guid_str[GUID_ENCODING_LENGTH+1];

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

static gboolean
gbv_key_press_cb(GtkWidget *treeview, GdkEventKey *event, gpointer userdata)
{
    GtkTreeView *tv = GTK_TREE_VIEW(treeview);
    GtkTreeViewColumn *col;
    GtkTreePath *path = NULL;

    if (event->type != GDK_KEY_PRESS) return TRUE;

    switch (event->keyval)
    {
    case GDK_Tab:
    case GDK_ISO_Left_Tab:
    case GDK_KP_Tab:
    case GDK_Return:
    case GDK_KP_Enter:
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
            gtk_tree_view_column_set_fixed_width(totals_view_col, col_width);
            j++;
        }
    }
    LEAVE("");
}

static void
gbv_row_activated_cb(GtkTreeView *treeview, GtkTreePath *path,
                     GtkTreeViewColumn *col, GncBudgetView *view)
{
    GtkWidget *window;
    GncPluginPage *new_page;
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

typedef struct
{
    gnc_numeric total;
    GncBudget* budget;
    guint period_num;
} BudgetAccumulationInfo;

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

static gnc_numeric
gbv_get_accumulated_budget_amount(GncBudget* budget, Account* account, guint period_num)
{
    BudgetAccumulationInfo info;

    info.total = gnc_numeric_zero();
    info.budget = budget;
    info.period_num = period_num;
    gnc_account_foreach_child(account, budget_accum_helper, &info);

    return info.total;
}

/* Displays budget amount for a period for an account.  If a budget
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


static void
totals_col_source(GtkTreeViewColumn *col, GtkCellRenderer *cell,
                  GtkTreeModel *s_model, GtkTreeIter *s_iter,
                  gpointer user_data)
{
    GncBudgetView* view;
    GncBudgetViewPrivate* priv;
    gint row_type;
    GncBudget *budget;
    gint period_num;
    gnc_numeric value;
    gchar amtbuff[100]; //FIXME: overkill, where's the #define?
    gint width;

    view = GNC_BUDGET_VIEW(user_data);
    priv = GNC_BUDGET_VIEW_GET_PRIVATE(view);

    gtk_tree_model_get(s_model, s_iter, 1, &row_type, -1);
    budget = GNC_BUDGET(g_object_get_data(G_OBJECT(col), "budget"));
    period_num = GPOINTER_TO_INT(g_object_get_data(G_OBJECT(col),
                                 "period_num"));

    if (row_type == TOTALS_TYPE_INCOME)
    {
        if (period_num >= 0)
        {
            value = gbv_get_accumulated_budget_amount(budget, priv->income, period_num);
        }
        else
        {
            value = bgv_get_total_for_account(priv->income, budget);
        }
        xaccSPrintAmount(amtbuff, value,
                         gnc_account_print_info(priv->income, FALSE));
        g_object_set(cell, "foreground", "black", NULL);
    }
    else if (row_type == TOTALS_TYPE_EXPENSES)
    {
        if (period_num >= 0)
        {
            value = gbv_get_accumulated_budget_amount(budget, priv->expenses, period_num);
        }
        else
        {
            value = bgv_get_total_for_account(priv->expenses, budget);
        }
        xaccSPrintAmount(amtbuff, value,
                         gnc_account_print_info(priv->expenses, FALSE));
        g_object_set(cell, "foreground", "black", NULL);
    }
    else if (row_type == TOTALS_TYPE_TRANSFERS)
    {
        gnc_numeric assets;
        gnc_numeric liabilities;

        if (period_num >= 0)
        {
            assets = gbv_get_accumulated_budget_amount(budget, priv->assets, period_num);
            liabilities = gbv_get_accumulated_budget_amount(budget, priv->liabilities, period_num);
        }
        else
        {
            assets = bgv_get_total_for_account(priv->assets, budget);
            liabilities = bgv_get_total_for_account(priv->liabilities, budget);
        }
        value = gnc_numeric_sub(assets, liabilities, GNC_DENOM_AUTO, GNC_HOW_DENOM_LCD);
        xaccSPrintAmount(amtbuff, value,
                         gnc_account_print_info(priv->assets, FALSE));
        g_object_set(cell, "foreground", "black", NULL);
    }
    else if (row_type == TOTALS_TYPE_TOTAL)
    {
        gnc_numeric income;
        gnc_numeric expenses;
        gnc_numeric assets;
        gnc_numeric liabilities;

        if (period_num >= 0)
        {
            income = gbv_get_accumulated_budget_amount(budget, priv->income, period_num);
            expenses = gbv_get_accumulated_budget_amount(budget, priv->expenses, period_num);
            assets = gbv_get_accumulated_budget_amount(budget, priv->assets, period_num);
            liabilities = gbv_get_accumulated_budget_amount(budget, priv->liabilities, period_num);
        }
        else
        {
            income = bgv_get_total_for_account(priv->income, budget);
            expenses = bgv_get_total_for_account(priv->expenses, budget);
            assets = bgv_get_total_for_account(priv->assets, budget);
            liabilities = bgv_get_total_for_account(priv->liabilities, budget);
        }
        value = gnc_numeric_sub(income, expenses, GNC_DENOM_AUTO, GNC_HOW_DENOM_LCD);
        value = gnc_numeric_sub(value, assets, GNC_DENOM_AUTO, GNC_HOW_DENOM_LCD);
        value = gnc_numeric_add(value, liabilities, GNC_DENOM_AUTO, GNC_HOW_DENOM_LCD);
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
        g_strlcpy(amtbuff, "error", sizeof(amtbuff));
    }

    g_object_set(G_OBJECT(cell), "text", amtbuff, "xalign", 1.0, NULL);
}

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


static void
gbv_col_edited_cb(GtkCellRendererText* cell, gchar* path_string, gchar* new_text, gpointer user_data)
{
    GncBudgetView *view;
    GncBudgetViewPrivate *priv;
    const EventInfo* ei;

    view = GNC_BUDGET_VIEW(user_data);
    priv = GNC_BUDGET_VIEW_GET_PRIVATE(view);

    gtk_widget_queue_draw(GTK_WIDGET(priv->totals_tree_view));
}

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

