/*
 * gnc-plugin-page-budget.c --
 *
 * Copyright (C) 2005-2006 Chris Shoemaker <c.shoemaker@cox.net>
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
 */

#include "config.h"

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include <glade/glade.h>
#include "gnc-date-edit.h"

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

#include "qof.h"

#include "gnc-dialog.h"
#include "gnc-recurrence.h"
#include "Recurrence.h"
#include "gnc-tree-model-account-types.h"


/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_BUDGET;

#define PLUGIN_PAGE_BUDGET_CM_CLASS "plugin-page-budget"
#define GCONF_SECTION "window/pages/budget"

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
static void gnc_plugin_page_budget_save_page (
    GncPluginPage *plugin_page, GKeyFile *file, const gchar *group);
static GncPluginPage *gnc_plugin_page_budget_recreate_page (
    GtkWidget *window, GKeyFile *file, const gchar *group);


static gboolean gppb_button_press_cb(
    GtkWidget *widget, GdkEventButton *event, GncPluginPage *page);
static gboolean gppb_key_press_cb(
    GtkWidget *treeview, GdkEventKey *event, gpointer userdata);
static void gppb_double_click_cb(
    GtkTreeView *treeview, GtkTreePath *path, GtkTreeViewColumn *col,
    GncPluginPageBudget *page);
static void gppb_selection_changed_cb(
    GtkTreeSelection *selection, GncPluginPageBudget *page);

static void gnc_plugin_page_budget_view_refresh (GncPluginPageBudget *page);
static void gnc_plugin_page_budget_cmd_view_filter_by (
    GtkAction *action, GncPluginPageBudget *page);

/* Command Callbacks */
static void gnc_plugin_page_budget_cmd_open_account(
    GtkAction *action, GncPluginPageBudget *page);
static void gnc_plugin_page_budget_cmd_open_subaccounts(
    GtkAction *action, GncPluginPageBudget *page);
static void gnc_plugin_page_budget_cmd_delete_budget(
    GtkAction *action, GncPluginPageBudget *page);
static void gnc_plugin_page_budget_cmd_view_options(
    GtkAction *action, GncPluginPageBudget *page);
static void gnc_plugin_page_budget_cmd_estimate_budget(
    GtkAction *action, GncPluginPageBudget *page);



static GtkActionEntry gnc_plugin_page_budget_actions [] = {
    /* Toplevel */
    { "FakeToplevel", "", NULL, NULL, NULL, NULL },

    /* File menu */
    { "OpenAccountAction", GNC_STOCK_OPEN_ACCOUNT, N_("Open _Account"), NULL,
      N_("Open the selected account"),
      G_CALLBACK (gnc_plugin_page_budget_cmd_open_account) },
    { "OpenSubaccountsAction", GNC_STOCK_OPEN_ACCOUNT, 
      N_("Open _Subaccounts"), NULL,
      N_("Open the selected account and all its subaccounts"),
      G_CALLBACK (gnc_plugin_page_budget_cmd_open_subaccounts) },

    /* Edit menu */
    { "DeleteBudgetAction", GNC_STOCK_DELETE_BUDGET, N_("_Delete Budget"),
      NULL, N_("Delete this budget"),
      G_CALLBACK (gnc_plugin_page_budget_cmd_delete_budget) },
    { "OptionsBudgetAction", GTK_STOCK_PROPERTIES, N_("Budget Options"),
      NULL, N_("Edit this budget's options"),
      G_CALLBACK (gnc_plugin_page_budget_cmd_view_options) },
    { "EstimateBudgetAction", GTK_STOCK_EXECUTE, N_("Estimate Budget"),
      NULL,
      N_("Estimate a budget value for the selected accounts from past transactions"),
      G_CALLBACK (gnc_plugin_page_budget_cmd_estimate_budget) },

    /* View menu */
    { "ViewFilterByAction", NULL, N_("_Filter By..."), NULL, NULL,
      G_CALLBACK (gnc_plugin_page_budget_cmd_view_filter_by) },

};

static guint gnc_plugin_page_budget_n_actions =
    G_N_ELEMENTS (gnc_plugin_page_budget_actions);

static const gchar *actions_requiring_account[] = {
  "OpenAccountAction",
  "OpenSubaccountsAction",
  NULL
};

/** Short labels for use on the toolbar buttons. */
static action_toolbar_labels toolbar_labels[] = {
  { "OpenAccountAction", 	    N_("Open") },
  { "DeleteBudgetAction", 	    N_("Delete") },
  { "OptionsBudgetAction", 	    N_("Options") },
  { "EstimateBudgetAction", 	    N_("Estimate") },
  { NULL, NULL },
};

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
    AccountFilterDialog fd;

    /* For the estimation dialog */
    Recurrence r;
    gint sigFigs;
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
    gnc_plugin_class->save_page       = gnc_plugin_page_budget_save_page;
    gnc_plugin_class->recreate_page   = gnc_plugin_page_budget_recreate_page;

    g_type_class_add_private(klass, sizeof(GncPluginPageBudgetPrivate));
}

static void
gnc_plugin_page_budget_init (GncPluginPageBudget *plugin_page)
{
    GtkActionGroup *action_group;
    GncPluginPageBudgetPrivate *priv;
    GncPluginPage *parent;

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
    gnc_plugin_init_short_names (action_group, toolbar_labels);

    /* Visisble types */
    priv->fd.visible_types = -1; /* Start with all types */
    priv->fd.show_hidden = FALSE;
    priv->fd.show_zero_total = TRUE;

    priv->sigFigs = 1;
    recurrenceSet(&priv->r, 1, PERIOD_MONTH, NULL, WEEKEND_ADJ_NONE); 

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
            if (ei->event_mask & QOF_EVENT_DESTROY) {
                gnc_plugin_page_budget_close_cb(user_data);
                return;
            }
            if (ei->event_mask & QOF_EVENT_MODIFY) {
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
    g_object_set(G_OBJECT(tree_view), "gconf-section", GCONF_SECTION, NULL);

    gnc_tree_view_configure_columns(GNC_TREE_VIEW(tree_view));
    priv->tree_view = tree_view;
    selection = gtk_tree_view_get_selection(tree_view);
    gtk_tree_selection_set_mode(selection, GTK_SELECTION_MULTIPLE);

    g_signal_connect(G_OBJECT(selection), "changed",
                     G_CALLBACK(gppb_selection_changed_cb), plugin_page);
    g_signal_connect(G_OBJECT(tree_view), "button-press-event",
                     G_CALLBACK(gppb_button_press_cb), plugin_page);
    g_signal_connect(G_OBJECT(tree_view), "row-activated",
                     G_CALLBACK(gppb_double_click_cb), page);
    g_signal_connect_after(G_OBJECT(tree_view), "key-press-event",
                           G_CALLBACK(gppb_key_press_cb), NULL);

    gppb_selection_changed_cb (NULL, page);
    gtk_tree_view_set_headers_visible(tree_view, TRUE);
    gtk_widget_show (GTK_WIDGET (tree_view));
    gtk_container_add (GTK_CONTAINER (scrolled_window),
                       GTK_WIDGET(tree_view));
    priv->fd.tree_view = GNC_TREE_VIEW_ACCOUNT(priv->tree_view);
    gnc_tree_view_account_set_filter(
        GNC_TREE_VIEW_ACCOUNT(tree_view),
        gnc_plugin_page_account_tree_filter_accounts,
        &priv->fd, NULL);

    priv->component_id =
        gnc_register_gui_component(PLUGIN_PAGE_BUDGET_CM_CLASS,
                                   gnc_plugin_page_budget_refresh_cb,
                                   gnc_plugin_page_budget_close_cb,
                                   page);

    gnc_gui_component_set_session (priv->component_id,
                                   gnc_get_current_session());

    gnc_gui_component_watch_entity (priv->component_id,
                                    gnc_budget_get_guid(priv->budget),
                                    QOF_EVENT_DESTROY | QOF_EVENT_MODIFY);

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

#define BUDGET_GUID "Budget GUID"

/** Save enough information about this plugin page that it can
 *  be recreated next time the user starts gnucash.
 *
 *  @param page The page to save.
 *
 *  @param key_file A pointer to the GKeyFile data structure where the
 *  page information should be written.
 *
 *  @param group_name The group name to use when saving data. */
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
    
    guid_to_string_buff(gnc_budget_get_guid(priv->budget), guid_str);
    g_key_file_set_string(key_file, group_name, BUDGET_GUID, guid_str);
    
    //FIXME
    gnc_tree_view_account_save(GNC_TREE_VIEW_ACCOUNT(priv->tree_view), 
                               &priv->fd, key_file, group_name);
    LEAVE(" ");
}



/** Create a new plugin page based on the information saved
 *  during a previous instantiation of gnucash.
 *
 *  @param window The window where this page should be installed.
 *
 *  @param key_file A pointer to the GKeyFile data structure where the
 *  page information should be read.
 *
 *  @param group_name The group name to use when restoring data. */
static GncPluginPage *
gnc_plugin_page_budget_recreate_page (GtkWidget *window, GKeyFile *key_file, 
                                      const gchar *group_name)
{
    GncPluginPageBudget *budget_page;
    GncPluginPageBudgetPrivate *priv;
    GncPluginPage *page;
    GError *error = NULL;
    char *guid_str;
    GUID guid;
    GncBudget *bgt;
    QofBook *book;

    g_return_val_if_fail(key_file, NULL);
    g_return_val_if_fail(group_name, NULL);
    ENTER("key_file %p, group_name %s", key_file, group_name);

    guid_str = g_key_file_get_string(key_file, group_name, BUDGET_GUID, 
                                     &error);
    if (error) {
        g_warning("error reading group %s key %s: %s",
                  group_name, BUDGET_GUID, error->message);
        g_error_free(error);
        error = NULL;
        return NULL;
    }
    if (!string_to_guid(guid_str, &guid)) 
        return NULL;
  
    book = qof_session_get_book(gnc_get_current_session());
    bgt = gnc_budget_lookup(&guid, book);
    if (!bgt) 
        return NULL;

    /* Create the new page. */
    page = gnc_plugin_page_budget_new(bgt);
    budget_page = GNC_PLUGIN_PAGE_BUDGET(page);
    priv = GNC_PLUGIN_PAGE_BUDGET_GET_PRIVATE(budget_page);

    /* Install it now so we can then manipulate the created widget */
    gnc_main_window_open_page(GNC_MAIN_WINDOW(window), page);

    //FIXME
    gnc_tree_view_account_restore(GNC_TREE_VIEW_ACCOUNT(priv->tree_view), 
                                  &priv->fd, key_file, group_name);
    LEAVE(" ");
    return page;
}

/** This button press handler calls the common button press handler
 *  for all pages.  The GtkTreeView eats all button presses and
 *  doesn't pass them up the widget tree, even when doesn't do
 *  anything with them.  The only way to get access to the button
 *  presses in an account tree page is here on the tree view widget.
 *  Button presses on all other pages are caught by the signal
 *  registered in gnc-main-window.c. */
static gboolean
gppb_button_press_cb(GtkWidget *widget, GdkEventButton *event,
                     GncPluginPage *page)
{
  gboolean result;

  g_return_val_if_fail(GNC_IS_PLUGIN_PAGE(page), FALSE);

  ENTER("widget %p, event %p, page %p", widget, event, page);
  result = gnc_main_window_button_press_cb(widget, event, page);
  LEAVE(" ");
  return result;
}

static gboolean
gppb_key_press_cb(GtkWidget *treeview, GdkEventKey *event, gpointer userdata)
{
    GtkTreeView *tv = GTK_TREE_VIEW(treeview);
    GtkTreeViewColumn *col;
    GtkTreePath *path = NULL;

    if (event->type != GDK_KEY_PRESS) return TRUE;

    switch (event->keyval) {
    case GDK_Tab:
    case GDK_ISO_Left_Tab:
    case GDK_KP_Tab:
    case GDK_Return:
    case GDK_KP_Enter:
        gtk_tree_view_get_cursor(tv, &path, &col);
        if (!path) return TRUE;
        //finish_edit(col);
        break;
    default: return TRUE;
    }
    gnc_tree_view_keynav(GNC_TREE_VIEW(tv), &col, path, event);
    
    if (path && gnc_tree_view_path_is_valid(GNC_TREE_VIEW(tv), path))
        gtk_tree_view_set_cursor(tv, path, col, TRUE);
    return TRUE;
}

static void
gppb_double_click_cb(GtkTreeView *treeview, GtkTreePath *path,
                     GtkTreeViewColumn *col, GncPluginPageBudget *page)
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

static void
gppb_selection_changed_cb(GtkTreeSelection *selection,
                          GncPluginPageBudget *page)
{
    GtkActionGroup *action_group;
    GtkTreeView *view;
    GList *acct_list;
    gboolean sensitive;

    g_return_if_fail(GNC_IS_PLUGIN_PAGE_BUDGET(page));

    if (!selection) {
        sensitive = FALSE;
    } else {
        g_return_if_fail(GTK_IS_TREE_SELECTION(selection));
	view = gtk_tree_selection_get_tree_view (selection);
	acct_list = gnc_tree_view_account_get_selected_accounts(
            GNC_TREE_VIEW_ACCOUNT(view));

	/* Check here for placeholder accounts, etc. */
	sensitive = (g_list_length(acct_list) > 0);
	g_list_free(acct_list);
    }

    action_group = gnc_plugin_page_get_action_group(GNC_PLUGIN_PAGE(page));
    gnc_plugin_update_actions (action_group, actions_requiring_account,
				   "sensitive", sensitive);
}

/* Command callbacks */

static void
gnc_plugin_page_budget_cmd_open_account (GtkAction *action,
					 GncPluginPageBudget *page)
{
    GncPluginPageBudgetPrivate *priv;
    GtkWidget *window;
    GncPluginPage *new_page;
    GList *acct_list, *tmp;
    Account *account;

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_BUDGET (page));
    priv = GNC_PLUGIN_PAGE_BUDGET_GET_PRIVATE(page);
    acct_list = gnc_tree_view_account_get_selected_accounts(
        GNC_TREE_VIEW_ACCOUNT(priv->tree_view));

    window = GNC_PLUGIN_PAGE (page)->window;
    for (tmp = acct_list; tmp; tmp = g_list_next(tmp)) {
        account = tmp->data;
	new_page = gnc_plugin_page_register_new (account, FALSE);
	gnc_main_window_open_page (GNC_MAIN_WINDOW(window), new_page);
    }
    g_list_free(acct_list);
}

static void
gnc_plugin_page_budget_cmd_open_subaccounts (GtkAction *action,
						   GncPluginPageBudget *page)
{
    GncPluginPageBudgetPrivate *priv;
    GtkWidget *window;
    GncPluginPage *new_page;
    GList *acct_list, *tmp;
    Account *account;

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_BUDGET (page));
    priv = GNC_PLUGIN_PAGE_BUDGET_GET_PRIVATE(page);
    acct_list = gnc_tree_view_account_get_selected_accounts(
        GNC_TREE_VIEW_ACCOUNT(priv->tree_view));

    window = GNC_PLUGIN_PAGE (page)->window;
    for (tmp = acct_list; tmp; tmp = g_list_next(tmp)) {
        account = tmp->data;
	new_page = gnc_plugin_page_register_new (account, TRUE);
	gnc_main_window_open_page (GNC_MAIN_WINDOW(window), new_page);
    }
    g_list_free(acct_list);
}

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
    gchar *name;
    gchar *desc;
    gint num_periods;
    GncRecurrence *gr;
    const Recurrence *r;

    if(!priv)
        return TRUE;

    ENTER(" ");
    name = gnc_dialog_get_string(d, "BudgetName");
    if (name) {
        gnc_budget_set_name(priv->budget, name);
        DEBUG("%s", name);
        g_free(name);
    }

    desc = (gchar *) gnc_dialog_get_string(d, "BudgetDescription");
    gnc_budget_set_description(priv->budget, desc);
    g_free(desc);

    num_periods = gnc_dialog_get_int(d, "BudgetNumPeriods");
    gnc_budget_set_num_periods(priv->budget, num_periods);

    gr = GNC_RECURRENCE(gnc_dialog_get_widget(d, "BudgetRecurrenceEntry"));
    r = gnc_recurrence_get(gr);
    gnc_budget_set_recurrence(priv->budget, r);

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
				   "%s",
				   _("Set the budget options using this dialog."));

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
        gtk_window_set_title(GTK_WINDOW(priv->d), _("Budget Options"));
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
       name = _("Unnamed Budget");


    if (gnc_verify_dialog (NULL, FALSE, _("Delete %s?"), name)) {
        gnc_suspend_gui_refresh ();
        gnc_budget_destroy(budget);
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
        num = recurrenceGetAccountPeriodValue(&priv->r, acct, i);
        if (!gnc_numeric_check(num)) {
            if (gnc_reverse_balance (acct))
                num = gnc_numeric_neg (num);

            
            num = gnc_numeric_convert(num, GNC_DENOM_AUTO, 
                GNC_HOW_DENOM_SIGFIGS(priv->sigFigs) | GNC_HOW_RND_ROUND);
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
    GtkWidget *dialog, *gde, *dtr;
    gint result;
    GDate date;
    const Recurrence *r;
    GladeXML *xml;

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_BUDGET(page));
    priv = GNC_PLUGIN_PAGE_BUDGET_GET_PRIVATE(page);

    sel = gtk_tree_view_get_selection(GTK_TREE_VIEW(priv->tree_view));

    if (gtk_tree_selection_count_selected_rows(sel) <= 0) {
        dialog = gtk_message_dialog_new (
            GTK_WINDOW(gnc_plugin_page_get_window(GNC_PLUGIN_PAGE(page))),
            GTK_DIALOG_DESTROY_WITH_PARENT | GTK_DIALOG_MODAL,
            GTK_MESSAGE_INFO, GTK_BUTTONS_CLOSE,
            _("You must select at least one account to estimate."));
        gtk_dialog_run (GTK_DIALOG(dialog));
        gtk_widget_destroy(dialog);
        return;
    }

    xml = gnc_glade_xml_new ("budget.glade", "BudgetEstimate");
    dialog = glade_xml_get_widget (xml, "BudgetEstimate");
    gtk_window_set_transient_for(
        GTK_WINDOW(dialog), 
        GTK_WINDOW(gnc_plugin_page_get_window(GNC_PLUGIN_PAGE(page))));
    gde = glade_xml_get_widget(xml, "StartDate");
    date = recurrenceGetDate(&priv->r);
    gnc_date_edit_set_gdate(GNC_DATE_EDIT(gde), &date);
    dtr = glade_xml_get_widget(xml, "DigitsToRound");
    gtk_spin_button_set_value(GTK_SPIN_BUTTON(dtr), 
                              (gdouble)priv->sigFigs);

    gtk_widget_show_all (dialog);
    result = gtk_dialog_run(GTK_DIALOG(dialog));
    switch (result) {
    case GTK_RESPONSE_OK:
        r = gnc_budget_get_recurrence(priv->budget);
        
        gnc_date_edit_get_gdate(GNC_DATE_EDIT(gde), &date);
        recurrenceSet(&priv->r, recurrenceGetMultiplier(r), 
                      recurrenceGetPeriodType(r), &date,
                      recurrenceGetWeekendAdjust(r));
        priv->sigFigs = 
            gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(dtr));

        gtk_tree_selection_selected_foreach(sel, estimate_budget_helper, page);
        break;
    default:
        break;
    }
    gtk_widget_destroy(dialog);
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

    if (!gnc_budget_is_account_period_value_set(budget, account, period_num)) {
        amtbuff[0] = '\0';
    } else {
      numeric = gnc_budget_get_account_period_value(budget, account, 
                                                    period_num);
      if (gnc_numeric_check(numeric)) {
          strcpy(amtbuff, "error");
      } else {
          xaccSPrintAmount(amtbuff, numeric,
                           gnc_account_print_info(account, FALSE));
      }
    }

    return g_strdup(amtbuff);
}

static void
budget_col_edited(Account *account, GtkTreeViewColumn *col,
                  const gchar *new_text)
{
    GncBudget *budget;
    guint period_num;
    gnc_numeric numeric = gnc_numeric_error(GNC_ERROR_ARG);

    if (!xaccParseAmount (new_text, TRUE, &numeric, NULL) &&
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
        titlelen = qof_print_gdate(title, MAX_DATE_LENGTH, &date);
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

    g_return_if_fail(GNC_IS_PLUGIN_PAGE_BUDGET(page));
    priv = GNC_PLUGIN_PAGE_BUDGET_GET_PRIVATE(page);

    num_periods = gnc_budget_get_num_periods(priv->budget);
    col_list = priv->period_col_list;
    num_periods_visible = g_list_length(col_list);

    /* Hide any unneeded extra columns */
    while (num_periods_visible > num_periods) {
        col = GTK_TREE_VIEW_COLUMN((g_list_last(col_list))->data);
        gtk_tree_view_remove_column(GTK_TREE_VIEW(priv->tree_view), col);
        col_list = g_list_delete_link(col_list, g_list_last(col_list));
        num_periods_visible = g_list_length(col_list);
    }

    gnc_tree_view_configure_columns(GNC_TREE_VIEW(priv->tree_view));

    /* Create any needed columns */
    while (num_periods_visible < num_periods) {
        col = gnc_tree_view_account_add_custom_column(
            GNC_TREE_VIEW_ACCOUNT(priv->tree_view), "",
            budget_col_source, budget_col_edited);
        g_object_set_data(G_OBJECT(col), "budget", priv->budget);
        g_object_set_data(G_OBJECT(col), "period_num", 
                          GUINT_TO_POINTER(num_periods_visible));
        col_list = g_list_append(col_list, col);
        num_periods_visible = g_list_length(col_list);
    }
    priv->period_col_list = col_list;

    gnc_plugin_page_budget_refresh_col_titles(page);
}

static void
gnc_plugin_page_budget_cmd_view_filter_by (GtkAction *action,
                                           GncPluginPageBudget *page)
{
    GncPluginPageBudgetPrivate *priv;

    g_return_if_fail(GNC_IS_PLUGIN_PAGE_BUDGET(page));
    ENTER("(action %p, page %p)", action, page);
    
    priv = GNC_PLUGIN_PAGE_BUDGET_GET_PRIVATE(page);
    account_filter_dialog_create(&priv->fd, GNC_PLUGIN_PAGE(page));

    LEAVE(" ");
}
