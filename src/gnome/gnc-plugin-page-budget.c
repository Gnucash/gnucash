/*
 * gnc-plugin-page-budget.c --
 *
 * Copyright (C) 2005 Chris Shoemaker <c.shoemaker@cox.net>
 *   (based on gnc-plugin-page-account-tree.c)
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


/*
 * TODO:
 *
 * *) I'd like to be able to update the budget estimates on a per cell
 * basis, instead of a whole row (account) at one time.  But, that
 * would require some major coding.
 *
 * *) Right now, the account-type filter is not saved anywhere.  Where
 * should it be saved?  Per budget?  Gconf?
 *
 *
 */

#include "config.h"

#include <gtk/gtk.h>
#include <glade/glade.h>

#include "gnc-plugin-page-register.h"
#include "gnc-budget.h"

#include "dialog-options.h"
#include "dialog-utils.h"
#include "gnc-gnome-utils.h"
#include "gnc-html.h"
#include "gnc-icons.h"
#include "gnc-plugin-page-budget.h"
#include "gnc-plugin-budget.h"

#include "gnc-session.h"
#include "gnc-tree-view-account.h"
#include "gnc-ui.h"
#include "gnc-ui-util.h"
#include "option-util.h"
#include "libguile.h"
#include "gnc-main-window.h"
#include "gnc-component-manager.h"

#include "gnc-engine-util.h"
#include "gnc-date.h"
#include "gnc-trace.h"

#include "gnc-dialog.h"
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
static void
gnc_plugin_page_budget_class_init (GncPluginPageBudgetClass *klass);
static void gnc_plugin_page_budget_init (GncPluginPageBudget *plugin_page);
static void gnc_plugin_page_budget_finalize (GObject *object);

static GtkWidget *
gnc_plugin_page_budget_create_widget (GncPluginPage *plugin_page);
static void gnc_plugin_page_budget_destroy_widget (GncPluginPage *plugin_page);

static gboolean gnc_plugin_page_budget_button_press_cb(
    GtkWidget *widget, GdkEventButton *event, GncPluginPage *page);
static void gnc_plugin_page_budget_double_click_cb(
    GtkTreeView *treeview, GtkTreePath *path, GtkTreeViewColumn *col,
    GncPluginPageBudget *page);

static void gnc_plugin_page_budget_view_refresh (GncPluginPageBudget *page);

/* Command Callbacks */
static void gnc_plugin_page_budget_cmd_delete_budget(
    GtkAction *action, GncPluginPageBudget *page);
static void gnc_plugin_page_budget_cmd_view_options(
    GtkAction *action, GncPluginPageBudget *page);
static void gnc_plugin_page_budget_cmd_estimate_budget(
    GtkAction *action, GncPluginPageBudget *page);



static GtkActionEntry gnc_plugin_page_budget_actions [] = {
    /* Toplevel */
    { "FakeToplevel", "", NULL, NULL, NULL, NULL },

    /* TODO: maybe there should be menu entries, too? */

    /* Toolbar buttons */
    { "DeleteBudgetAction", GNC_STOCK_DELETE_BUDGET, N_("_Delete Budget"),
      NULL, N_("Delete the budget"),
      G_CALLBACK (gnc_plugin_page_budget_cmd_delete_budget) },
    { "OptionsBudgetAction", GTK_STOCK_PROPERTIES, N_("Budget Options"),
      NULL, N_("Edit the budget view options"),
      G_CALLBACK (gnc_plugin_page_budget_cmd_view_options) },
    { "EstimateBudgetAction", GTK_STOCK_EXECUTE, N_("Estimate Budget"),
      NULL,
      N_("Estimate a budget value for the selected cells"),
      G_CALLBACK (gnc_plugin_page_budget_cmd_estimate_budget) },
};

static guint gnc_plugin_page_budget_n_actions =
    G_N_ELEMENTS (gnc_plugin_page_budget_actions);

// TODO: What's all this do?
#if 0
static const gchar *actions_requiring_budget[] = {
  "OpenBudgetAction",
  "BudgetViewOptionsAction",
  "DeleteBudgetAction",
  NULL
};


/** Short labels for use on the toolbar buttons. */
static action_toolbar_labels toolbar_labels[] = {

  { "OpenBudgetAction", 	    N_("Open") },
  //{ "EditBudgetAction", 	    N_("Edit") },
  //{ "EditBudgetOptionsAction",      N_("Options") },
  { "NewBudgetAction",    	    N_("New") },
  { "DeleteBudgetAction", 	    N_("Delete") },
  { NULL, NULL },
};
#endif

typedef struct GncPluginPageBudgetPrivate
{
    GtkActionGroup *action_group;
    guint merge_id;
    GtkUIManager *ui_merge;

    GtkWidget *widget;        /* ends up being a vbox */
    GtkTreeView *tree_view;

    gint component_id;

    GncBudget* budget;
    GUID key;
    GncDialog* d;

    GList *period_col_list;
    guint32 acct_types;
} GncPluginPageBudgetPrivate;

#define GNC_PLUGIN_PAGE_BUDGET_GET_PRIVATE(o)  \
   (G_TYPE_INSTANCE_GET_PRIVATE ((o), GNC_TYPE_PLUGIN_PAGE_BUDGET, GncPluginPageBudgetPrivate))

static GObjectClass *parent_class = NULL;

GType
gnc_plugin_page_budget_get_type (void)
{
    static GType gnc_plugin_page_budget_type = 0;

    if (gnc_plugin_page_budget_type == 0) {
        static const GTypeInfo our_info = {
            sizeof (GncPluginPageBudgetClass),
            NULL,
            NULL,
            (GClassInitFunc) gnc_plugin_page_budget_class_init,
            NULL,
            NULL,
            sizeof (GncPluginPageBudget),
            0,
            (GInstanceInitFunc) gnc_plugin_page_budget_init
        };

        gnc_plugin_page_budget_type =
            g_type_register_static (GNC_TYPE_PLUGIN_PAGE,
                                    "GncPluginPageBudget", &our_info, 0);
    }

    return gnc_plugin_page_budget_type;
}

GncPluginPage *
gnc_plugin_page_budget_new (GncBudget *budget)
{
    GncPluginPageBudget *plugin_page;
    GncPluginPageBudgetPrivate *priv;

    g_return_val_if_fail(GNC_IS_BUDGET(budget), NULL);
    ENTER(" ");
    plugin_page = g_object_new(GNC_TYPE_PLUGIN_PAGE_BUDGET, NULL);

    priv = GNC_PLUGIN_PAGE_BUDGET_GET_PRIVATE(plugin_page);
    priv->budget = budget;
    priv->key = *gnc_budget_get_guid(budget);
    LEAVE("new budget page %p", plugin_page);
    return GNC_PLUGIN_PAGE(plugin_page);
}

static void
gnc_plugin_page_budget_class_init (GncPluginPageBudgetClass *klass)
{
    GObjectClass *object_class = G_OBJECT_CLASS (klass);
    GncPluginPageClass *gnc_plugin_class = GNC_PLUGIN_PAGE_CLASS(klass);

    parent_class = g_type_class_peek_parent (klass);

    object_class->finalize = gnc_plugin_page_budget_finalize;

    gnc_plugin_class->tab_icon        = GNC_STOCK_BUDGET;
    gnc_plugin_class->plugin_name     = GNC_PLUGIN_PAGE_BUDGET_NAME;
    gnc_plugin_class->create_widget   = gnc_plugin_page_budget_create_widget;
    gnc_plugin_class->destroy_widget  = gnc_plugin_page_budget_destroy_widget;

    g_type_class_add_private(klass, sizeof(GncPluginPageBudgetPrivate));
}

static void
gnc_plugin_page_budget_init (GncPluginPageBudget *plugin_page)
{
    GtkActionGroup *action_group;
    GncPluginPageBudgetPrivate *priv;
    GncPluginPage *parent;
    const gchar *url = NULL;
    //int options_id;
    //SCM find_options;
    //SCM temp;
    URLType type;

    ENTER("page %p", plugin_page);
    priv = GNC_PLUGIN_PAGE_BUDGET_GET_PRIVATE(plugin_page);

    /* Init parent declared variables */
    parent = GNC_PLUGIN_PAGE(plugin_page);
    g_object_set(G_OBJECT(plugin_page),
		 "page-name",      _("Budget"),
		 "page-uri",       "default:",
		 "ui-description", "gnc-plugin-page-budget-ui.xml",
		 NULL);

    /* change me when the system supports multiple books */
    gnc_plugin_page_add_book(parent, gnc_get_current_book());

    /* Create menu and toolbar information */
    action_group =
      gnc_plugin_page_create_action_group(parent,
					  "GncPluginPageBudgetActions");
    gtk_action_group_add_actions (action_group,
                                  gnc_plugin_page_budget_actions,
                                  gnc_plugin_page_budget_n_actions,
                                  plugin_page);
    // FIXME? needed?
    //gnc_gnome_utils_init_short_names (action_group, toolbar_labels);

    // FIXME: need to test this url case
    if(!url) {
    } else {
        char * location = NULL;
        char * label = NULL;

        /* if an URL is specified, it should look like
         * gnc-budget:id=17 .  We want to get the number out,
         * then look up the options in the global DB. */
        type = gnc_html_parse_url(NULL, url, &location, &label);
        g_free (location);
        g_free (label);
    }

    LEAVE("page %p, priv %p, action group %p",
          plugin_page, priv, action_group);
}

static void
gnc_plugin_page_budget_finalize (GObject *object)
{
    GncPluginPageBudget *page;
    GncPluginPageBudgetPrivate *priv;

    ENTER("object %p", object);
    page = GNC_PLUGIN_PAGE_BUDGET (object);
    g_return_if_fail (GNC_IS_PLUGIN_PAGE_BUDGET (page));

    priv = GNC_PLUGIN_PAGE_BUDGET_GET_PRIVATE(page);
    g_list_free(priv->period_col_list);

    G_OBJECT_CLASS (parent_class)->finalize (object);
    LEAVE(" ");
}


/* Component Manager Callback Functions */
static void
gnc_plugin_page_budget_close_cb (gpointer user_data)
{
    GncPluginPage *page = GNC_PLUGIN_PAGE(user_data);
    gnc_main_window_close_page (page);
}

static void
gnc_plugin_page_budget_refresh_cb(GHashTable *changes, gpointer user_data)
{
    GncPluginPageBudget *page;
    GncPluginPageBudgetPrivate *priv;
    const EventInfo* ei;

    page = GNC_PLUGIN_PAGE_BUDGET(user_data);
    priv = GNC_PLUGIN_PAGE_BUDGET_GET_PRIVATE(page);
    if (changes) {
        ei = gnc_gui_get_entity_events(changes, &priv->key);
        if (ei) {
            if (ei->event_mask & GNC_EVENT_DESTROY) {
                gnc_plugin_page_budget_close_cb(user_data);
                return;
            }
            if (ei->event_mask & GNC_EVENT_MODIFY) {
                DEBUG("refreshing budget view because budget was modified");
                gnc_plugin_page_budget_view_refresh(page);
            }
        }
    }
}


/*
 * GncPluginPage Fucntions
 */
static GtkWidget *
gnc_plugin_page_budget_create_widget (GncPluginPage *plugin_page)
{
    GncPluginPageBudget *page;
    GncPluginPageBudgetPrivate *priv;
    GtkTreeSelection *selection;
    GtkTreeView *tree_view;
    GtkWidget *scrolled_window;

    ENTER("page %p", plugin_page);
    page = GNC_PLUGIN_PAGE_BUDGET (plugin_page);
    priv = GNC_PLUGIN_PAGE_BUDGET_GET_PRIVATE(page);
    if (priv->widget != NULL) {
        LEAVE("widget = %p", priv->widget);
        return priv->widget;
    }

    priv->widget = gtk_vbox_new (FALSE, 0);
    gtk_widget_show (priv->widget);

    scrolled_window = gtk_scrolled_window_new (NULL, NULL);
    gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolled_window),
                                    GTK_POLICY_AUTOMATIC,
                                    GTK_POLICY_AUTOMATIC);
    gtk_widget_show (scrolled_window);
    gtk_box_pack_start (GTK_BOX (priv->widget), scrolled_window,
                        TRUE, TRUE, 0);

    tree_view = gnc_tree_view_account_new(FALSE);
    gnc_tree_view_configure_columns(
        GNC_TREE_VIEW(tree_view), "Name", NULL);
    priv->tree_view = tree_view;
    selection = gtk_tree_view_get_selection(tree_view);
    gtk_tree_selection_set_mode(selection, GTK_SELECTION_MULTIPLE);

    g_signal_connect (G_OBJECT (tree_view), "button-press-event",
                      G_CALLBACK (gnc_plugin_page_budget_button_press_cb),
                      plugin_page);
    g_signal_connect (G_OBJECT (tree_view), "row-activated",
                      G_CALLBACK (gnc_plugin_page_budget_double_click_cb),
                      page);

    gtk_tree_view_set_headers_visible(tree_view, TRUE);
    gtk_widget_show (GTK_WIDGET (tree_view));
    gtk_container_add (GTK_CONTAINER (scrolled_window),
                       GTK_WIDGET(tree_view));

    priv->component_id =
        gnc_register_gui_component(PLUGIN_PAGE_BUDGET_CM_CLASS,
                                   gnc_plugin_page_budget_refresh_cb,
                                   gnc_plugin_page_budget_close_cb,
                                   page);

    gnc_gui_component_set_session (priv->component_id,
                                   gnc_get_current_session());

    gnc_gui_component_watch_entity (priv->component_id,
                                    gnc_budget_get_guid(priv->budget),
                                    GNC_EVENT_DESTROY | GNC_EVENT_MODIFY);

    gnc_plugin_page_budget_view_refresh(page);

    LEAVE("widget = %p", priv->widget);
    return priv->widget;
}

static void
gnc_plugin_page_budget_destroy_widget (GncPluginPage *plugin_page)
{
    GncPluginPageBudget *page;
    GncPluginPageBudgetPrivate *priv;

    ENTER("page %p", plugin_page);
    page = GNC_PLUGIN_PAGE_BUDGET (plugin_page);
    priv = GNC_PLUGIN_PAGE_BUDGET_GET_PRIVATE(plugin_page);
    if (priv->widget) {
        g_object_unref(G_OBJECT(priv->widget));
        priv->widget = NULL;
    }

    gnc_gui_component_clear_watches (priv->component_id);

    if (priv->component_id != NO_COMPONENT) {
        gnc_unregister_gui_component(priv->component_id);
        priv->component_id = NO_COMPONENT;
    }

    LEAVE("widget destroyed");
}

/** This button press handler calls the common button press handler
 *  for all pages.  The GtkTreeView eats all button presses and
 *  doesn't pass them up the widget tree, even when doesn't do
 *  anything with them.  The only way to get access to the button
 *  presses in an account tree page is here on the tree view widget.
 *  Button presses on all other pages are caught by the signal
 *  registered in gnc-main-window.c. */
static gboolean
gnc_plugin_page_budget_button_press_cb (GtkWidget *widget,
					GdkEventButton *event,
					GncPluginPage *page)
{
  gboolean result;

  g_return_val_if_fail(GNC_IS_PLUGIN_PAGE(page), FALSE);

  ENTER("widget %p, event %p, page %p", widget, event, page);
  result = gnc_main_window_button_press_cb(widget, event, page);
  LEAVE(" ");
  return result;
}

static void
gnc_plugin_page_budget_double_click_cb (GtkTreeView        *treeview,
					GtkTreePath        *path,
					GtkTreeViewColumn  *col,
					GncPluginPageBudget *page)
{
    GtkWidget *window;
    GncPluginPage *new_page;
    Account *account;

    g_return_if_fail(GNC_IS_PLUGIN_PAGE_BUDGET (page));
    account = gnc_tree_view_account_get_account_from_path(
        GNC_TREE_VIEW_ACCOUNT(treeview), path);
    if (account == NULL)
        return;

    window = GNC_PLUGIN_PAGE(page)->window;
    new_page = gnc_plugin_page_register_new(account, FALSE);
    gnc_main_window_open_page(GNC_MAIN_WINDOW(window), new_page);
}

/* Command callbacks */

static void
gnc_plugin_page_budget_cmd_delete_budget (GtkAction *action,
					  GncPluginPageBudget *page)
{
  GncPluginPageBudgetPrivate *priv;
  GncBudget *budget;

  priv = GNC_PLUGIN_PAGE_BUDGET_GET_PRIVATE(page);
  budget = priv->budget;
  g_return_if_fail (GNC_IS_BUDGET(budget));
  gnc_budget_gui_delete_budget(budget);

}

/******************************/
/*       Options Dialog       */
/******************************/

static gboolean
gnc_plugin_page_budget_options_apply_cb (GncDialog * d,
					 gpointer user_data)
{
    GncPluginPageBudgetPrivate *priv = user_data;
    const gchar *name;
    gchar *desc;
    gint num_periods;
    GncRecurrence *gr;
    const Recurrence *r;
    GtkTreeView *tv;
    guint32 sel_mask;

    if(!priv)
        return TRUE;

    ENTER(" ");
    name = gnc_dialog_get_string(d, "BudgetName");
    if (name) {
        gnc_budget_set_name(priv->budget, name);
        DEBUG("%s", name);
    }


    //FIXME: this is special broken case where we actually do need to
    //free because widget is a GtkTextView
    desc = (gchar *) gnc_dialog_get_string(d, "BudgetDescription");
    gnc_budget_set_description(priv->budget, desc);
    g_free(desc);

    num_periods = gnc_dialog_get_int(d, "BudgetNumPeriods");
    gnc_budget_set_num_periods(priv->budget, num_periods);

    gr = GNC_RECURRENCE(gnc_dialog_get_widget(d, "BudgetRecurrenceEntry"));
    r = gnc_recurrence_get(gr);
    gnc_budget_set_recurrence(priv->budget, r);


    tv = GTK_TREE_VIEW(gnc_dialog_get_widget(
                           d, "AccountTypesTreeView"));
    sel_mask = gnc_tree_model_account_types_get_selection(tv);
    priv->acct_types = sel_mask;
    gnc_tree_view_account_set_filter(
        GNC_TREE_VIEW_ACCOUNT(priv->tree_view),
        gnc_tree_view_account_filter_by_type_selection,
        GUINT_TO_POINTER(sel_mask), NULL);
    LEAVE(" ");
    return TRUE;
}

static gboolean
gnc_plugin_page_budget_options_help_cb (GncDialog *d,
					gpointer user_data)
{
  GtkWidget *dialog;

  dialog = gtk_message_dialog_new (NULL,
				   GTK_DIALOG_DESTROY_WITH_PARENT,
				   GTK_MESSAGE_INFO,
				   GTK_BUTTONS_OK,
				   "Set the budget options using this dialog.");

  gtk_dialog_run (GTK_DIALOG (dialog));
  gtk_widget_destroy (dialog);
  return TRUE;
}

static gboolean
gnc_plugin_page_budget_options_close_cb (GncDialog *d,
					 gpointer user_data)
{
  GncPluginPageBudgetPrivate *priv = user_data;

  g_return_val_if_fail(priv, TRUE);

  gtk_widget_destroy(GTK_WIDGET(d));
  priv->d = NULL;
  return TRUE;
}


static void
gnc_budget_gui_show_options(GncDialog *pw, GncBudget *budget,
                            GncPluginPageBudget *page)
{
    GtkTreeView *tv;
    GtkTreeModel *tm;
    GtkTreeSelection *sel;
    GncRecurrence *gr;
    GncPluginPageBudgetPrivate *priv;


    g_return_if_fail (GNC_IS_PLUGIN_PAGE_BUDGET (page));
    priv = GNC_PLUGIN_PAGE_BUDGET_GET_PRIVATE(page);

    gnc_dialog_set_string(pw, "BudgetName",
                          gnc_budget_get_name(budget));
    gnc_dialog_set_string(pw, "BudgetDescription",
                          gnc_budget_get_description(budget));
    gnc_dialog_set_int(pw, "BudgetNumPeriods",
                       gnc_budget_get_num_periods(budget));
    gr = GNC_RECURRENCE(gnc_dialog_get_widget(
                            pw, "BudgetRecurrenceEntry"));
    gnc_recurrence_set(gr, gnc_budget_get_recurrence(budget));

    tv = GTK_TREE_VIEW(gnc_dialog_get_widget(
                           pw, "AccountTypesTreeView"));
    tm = gnc_tree_model_account_types_master();
    gtk_tree_view_set_model(tv, tm);
    gtk_tree_view_insert_column_with_attributes(
        tv, -1, "Account Types", gtk_cell_renderer_text_new(),
        "text", GNC_TREE_MODEL_ACCOUNT_TYPES_COL_NAME, NULL);
    sel = gtk_tree_view_get_selection(tv);
    gtk_tree_selection_set_mode(sel, GTK_SELECTION_MULTIPLE);

    //FIXME: this is just a default, need to save and set actual value.
    if (priv->acct_types == 0)
        priv->acct_types = 1 << INCOME | 1 << EXPENSE;
    gnc_tree_model_account_types_set_selection(tv, priv->acct_types);
}


static void
gnc_plugin_page_budget_cmd_view_options (GtkAction *action,
                                         GncPluginPageBudget *page)
{
    GncPluginPageBudgetPrivate *priv;

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_BUDGET (page));
    priv = GNC_PLUGIN_PAGE_BUDGET_GET_PRIVATE(page);

    if (!priv->d) {
        priv->d = gnc_dialog_new(GNC_BUDGET_GUI_FILE, "BudgetOptions");
        gtk_window_set_title(GTK_WINDOW(priv->d), "Budget Options");
        gnc_dialog_set_cb(priv->d,
                          gnc_plugin_page_budget_options_apply_cb,
                          gnc_plugin_page_budget_options_close_cb,
                          gnc_plugin_page_budget_options_help_cb,
                          priv);
    }

    gnc_budget_gui_show_options(priv->d, priv->budget, page);
    gtk_widget_show_all(GTK_WIDGET(priv->d));
}


void
gnc_budget_gui_delete_budget(GncBudget *budget)
{
    const char *name;

    g_return_if_fail(GNC_IS_BUDGET(budget));
    name = gnc_budget_get_name (budget);
    if (!name)
        name = "Unnamed Budget";


    if (gnc_verify_dialog (NULL, FALSE, "Delete %s?", name)) {
        gnc_suspend_gui_refresh ();
        gnc_budget_free(budget);
        // Views should close themselves because the CM will notify them.
        gnc_resume_gui_refresh ();
    }

}


static void
estimate_budget_helper(GtkTreeModel *model, GtkTreePath *path,
                       GtkTreeIter *iter, gpointer data)
{
    Account *acct;
    guint num_periods, i;
    gnc_numeric num;
    GncPluginPageBudgetPrivate *priv;
    GncPluginPageBudget *page = data;


    g_return_if_fail(GNC_IS_PLUGIN_PAGE_BUDGET(page));
    priv = GNC_PLUGIN_PAGE_BUDGET_GET_PRIVATE(page);

    acct = gnc_tree_view_account_get_account_from_path(
        GNC_TREE_VIEW_ACCOUNT(priv->tree_view), path);

    num_periods = g_list_length(priv->period_col_list);

    for (i = 0; i < num_periods; i++) {
        num = gnc_budget_get_account_period_actual_value(
            priv->budget, acct, i);
        if (!gnc_numeric_check(num)) {
            if (gnc_reverse_balance (acct))
                num = gnc_numeric_neg (num);

            gnc_budget_set_account_period_value(
                priv->budget, acct, i, num);
        }
    }

}

static void
gnc_plugin_page_budget_cmd_estimate_budget(GtkAction *action,
                                           GncPluginPageBudget *page)
{
    GncPluginPageBudgetPrivate *priv;
    GtkTreeSelection *sel;

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_BUDGET (page));
    priv = GNC_PLUGIN_PAGE_BUDGET_GET_PRIVATE(page);

    sel = gtk_tree_view_get_selection(GTK_TREE_VIEW(priv->tree_view));
    gtk_tree_selection_selected_foreach(sel, estimate_budget_helper, page);



}

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
    numeric = gnc_budget_get_account_period_value(budget, account, period_num);

    if (gnc_numeric_zero_p(numeric))
        amtbuff[0] = '\0';
    else
        xaccSPrintAmount (amtbuff, numeric,
                          gnc_account_print_info (account, FALSE));

    return g_strdup(amtbuff);
}

static void
budget_col_edited(Account *account, GtkTreeViewColumn *col,
                  const gchar *new_text)
{
    GncBudget *budget;
    guint period_num;
    gnc_numeric numeric;

    if (!(xaccParseAmount (new_text, TRUE, &numeric, NULL)))
        return;

    period_num = GPOINTER_TO_UINT(g_object_get_data(G_OBJECT(col),
                                                    "period_num"));

    budget = GNC_BUDGET(g_object_get_data(G_OBJECT(col), "budget"));

    gnc_budget_set_account_period_value(budget, account, period_num, numeric);
}

static void
gnc_plugin_page_budget_refresh_col_titles(GncPluginPageBudget *page)
{
    const Recurrence *r;
    GDate date, nextdate;
    GtkTreeViewColumn *col;
    guint titlelen;
    gint num_periods_visible;
    gchar title[MAX_DATE_LENGTH];
    GncPluginPageBudgetPrivate *priv;
    GList *col_list;
    gint i;

    g_return_if_fail(GNC_IS_PLUGIN_PAGE_BUDGET(page));
    priv = GNC_PLUGIN_PAGE_BUDGET_GET_PRIVATE(page);

    col_list = priv->period_col_list;
    num_periods_visible = g_list_length(col_list);

   /* Show the dates in column titles */
    r = gnc_budget_get_recurrence(priv->budget);
    date = r->start;
    for (i = 0; i < num_periods_visible; i++) {
        col = GTK_TREE_VIEW_COLUMN(g_list_nth_data(col_list, i));
        titlelen = g_date_strftime(title, MAX_DATE_LENGTH, "%x", &date);
        if (titlelen > 0)
            gtk_tree_view_column_set_title(col, title);
        recurrenceNextInstance(r, &date, &nextdate);
        date = nextdate;
    }

}

static void
gnc_plugin_page_budget_view_refresh (GncPluginPageBudget *page)
{
    GncPluginPageBudgetPrivate *priv;
    gint num_periods, num_periods_visible;
    GtkTreeViewColumn *col;
    GList *col_list;
    gint i;

    g_return_if_fail(GNC_IS_PLUGIN_PAGE_BUDGET(page));
    priv = GNC_PLUGIN_PAGE_BUDGET_GET_PRIVATE(page);

    num_periods = gnc_budget_get_num_periods(priv->budget);
    col_list = priv->period_col_list;
    num_periods_visible = g_list_length(col_list);

    /* Hide any unneeded extra columns */
    for (i = num_periods_visible-1; i >= num_periods; i--) {
        col = GTK_TREE_VIEW_COLUMN((g_list_last(col_list))->data);
        //maybe better to just destroy it?
        gtk_tree_view_column_set_visible(col, FALSE);
        col_list = g_list_delete_link(col_list, g_list_last(col_list));
    }

    gnc_tree_view_configure_columns(
        GNC_TREE_VIEW(priv->tree_view), NULL);

    /* Create any needed columns */
    num_periods_visible = g_list_length(col_list);
    for (i = num_periods_visible; i < num_periods; i++) {
        col = gnc_tree_view_account_add_custom_column(
            GNC_TREE_VIEW_ACCOUNT(priv->tree_view), "",
            budget_col_source,
            budget_col_edited);
        g_object_set_data(G_OBJECT(col), "budget", priv->budget);
        g_object_set_data(G_OBJECT(col), "period_num", GUINT_TO_POINTER(i));
        col_list = g_list_append(col_list, col);
    }
    num_periods_visible = num_periods;
    priv->period_col_list = col_list;

    gnc_plugin_page_budget_refresh_col_titles(page);
}
