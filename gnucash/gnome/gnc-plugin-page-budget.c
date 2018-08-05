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

#include <gtk/gtk.h>
#ifdef __G_IR_SCANNER__
#undef __G_IR_SCANNER__
#endif
#include <gdk/gdkkeysyms.h>
#include <glib/gi18n.h>
#include "gnc-date-edit.h"

#include "gnc-plugin-page-register.h"
#include "gnc-budget.h"

#include "dialog-options.h"
#include "dialog-utils.h"
#include "gnc-gnome-utils.h"
#include "gnc-gobject-utils.h"
#include "gnc-icons.h"
#include "gnc-plugin-page-budget.h"
#include "gnc-plugin-budget.h"
#include "gnc-budget-view.h"

#include "gnc-session.h"
#include "gnc-tree-view-account.h"
#include "gnc-ui.h"
#include "gnc-ui-util.h"
#include "option-util.h"
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
static void gppb_account_activated_cb(GncBudgetView* view, Account* account,
                                      GncPluginPageBudget *page);
#if 0
static void gppb_selection_changed_cb(
    GtkTreeSelection *selection, GncPluginPageBudget *page);
#endif

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

static GtkActionEntry gnc_plugin_page_budget_actions [] =
{
    /* Toplevel */
    { "FakeToplevel", "", NULL, NULL, NULL, NULL },

    /* File menu */
    {
        "OpenAccountAction", GNC_ICON_OPEN_ACCOUNT, N_("Open _Account"), NULL,
        N_("Open the selected account"),
        G_CALLBACK (gnc_plugin_page_budget_cmd_open_account)
    },
    {
        "OpenSubaccountsAction", GNC_ICON_OPEN_ACCOUNT,
        N_("Open _Subaccounts"), NULL,
        N_("Open the selected account and all its subaccounts"),
        G_CALLBACK (gnc_plugin_page_budget_cmd_open_subaccounts)
    },

    /* Edit menu */
    {
        "DeleteBudgetAction", GNC_ICON_DELETE_BUDGET, N_("_Delete Budget"),
        NULL, N_("Delete this budget"),
        G_CALLBACK (gnc_plugin_page_budget_cmd_delete_budget)
    },
    {
        "OptionsBudgetAction", "document-properties", N_("Budget Options"),
        NULL, N_("Edit this budget's options"),
        G_CALLBACK (gnc_plugin_page_budget_cmd_view_options)
    },
    {
        "EstimateBudgetAction", "system-run", N_("Estimate Budget"),
        NULL,
        N_("Estimate a budget value for the selected accounts from past transactions"),
        G_CALLBACK (gnc_plugin_page_budget_cmd_estimate_budget)
    },

    /* View menu */
    {
        "ViewFilterByAction", NULL, N_("_Filter By..."), NULL, NULL,
        G_CALLBACK (gnc_plugin_page_budget_cmd_view_filter_by)
    },

};

static guint gnc_plugin_page_budget_n_actions =
    G_N_ELEMENTS (gnc_plugin_page_budget_actions);

#if 0
static const gchar *actions_requiring_account[] =
{
    "OpenAccountAction",
    "OpenSubaccountsAction",
    NULL
};
#endif

/** Short labels for use on the toolbar buttons. */
static action_toolbar_labels toolbar_labels[] =
{
    { "OpenAccountAction",          N_("Open") },
    { "DeleteBudgetAction",         N_("Delete") },
    { "OptionsBudgetAction",        N_("Options") },
    { "EstimateBudgetAction",       N_("Estimate") },
    { NULL, NULL },
};

typedef struct GncPluginPageBudgetPrivate
{
    GtkActionGroup *action_group;
    guint merge_id;
    GtkUIManager *ui_merge;

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
} GncPluginPageBudgetPrivate;

#define GNC_PLUGIN_PAGE_BUDGET_GET_PRIVATE(o)  \
   (G_TYPE_INSTANCE_GET_PRIVATE ((o), GNC_TYPE_PLUGIN_PAGE_BUDGET, GncPluginPageBudgetPrivate))

static GObjectClass *parent_class = NULL;


GType
gnc_plugin_page_budget_get_type (void)
{
    static GType gnc_plugin_page_budget_type = 0;

    if (gnc_plugin_page_budget_type == 0)
    {
        static const GTypeInfo our_info =
        {
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
    gchar* label;
    const GList *item;

    g_return_val_if_fail(GNC_IS_BUDGET(budget), NULL);
    ENTER(" ");

    /* Is there an existing page? */
    item = gnc_gobject_tracking_get_list(GNC_PLUGIN_PAGE_BUDGET_NAME);
    for ( ; item; item = g_list_next(item))
    {
        plugin_page = (GncPluginPageBudget *)item->data;
        priv = GNC_PLUGIN_PAGE_BUDGET_GET_PRIVATE(plugin_page);
        if (priv->budget == budget)
        {
            LEAVE("existing budget page %p", plugin_page);
            return GNC_PLUGIN_PAGE(plugin_page);
        }
    }

    plugin_page = g_object_new(GNC_TYPE_PLUGIN_PAGE_BUDGET, NULL);

    priv = GNC_PLUGIN_PAGE_BUDGET_GET_PRIVATE(plugin_page);
    priv->budget = budget;
    priv->delete_budget = FALSE;
    priv->key = *gnc_budget_get_guid(budget);
    label = g_strdup_printf("%s: %s", _("Budget"), gnc_budget_get_name(budget));
    g_object_set(G_OBJECT(plugin_page), "page-name", label, NULL);
    g_free(label);
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

    gnc_plugin_class->tab_icon        = GNC_ICON_BUDGET;
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

    /* Initialize parent declared variables */
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

    /* Visible types */
    priv->fd.visible_types = -1; /* Start with all types */
    priv->fd.show_hidden = FALSE;
    priv->fd.show_zero_total = TRUE;
    priv->fd.filter_override = g_hash_table_new (g_direct_hash, g_direct_equal);

    priv->sigFigs = 1;
    recurrenceSet(&priv->r, 1, PERIOD_MONTH, NULL, WEEKEND_ADJ_NONE);

    LEAVE("page %p, priv %p, action group %p",
          plugin_page, priv, action_group);
}


static void
gnc_plugin_page_budget_finalize (GObject *object)
{
    GncPluginPageBudget *page;

    ENTER("object %p", object);
    page = GNC_PLUGIN_PAGE_BUDGET (object);
    g_return_if_fail (GNC_IS_PLUGIN_PAGE_BUDGET (page));

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


gboolean
gnc_plugin_page_budget_focus (GncPluginPageBudget *page)
{
    if (GNC_IS_PLUGIN_PAGE_BUDGET(page))
    {
        GncPluginPageBudgetPrivate *priv = GNC_PLUGIN_PAGE_BUDGET_GET_PRIVATE(page);
        GncBudgetView *budget_view = priv->budget_view;
        GtkWidget *account_view = gnc_budget_view_get_account_tree_view (budget_view);

        if (!gtk_widget_is_focus (GTK_WIDGET(account_view)))
            gtk_widget_grab_focus (GTK_WIDGET(account_view));
    }
    return FALSE;
}


static void
gnc_plugin_page_budget_refresh_cb(GHashTable *changes, gpointer user_data)
{
    GncPluginPageBudget *page;
    GncPluginPageBudgetPrivate *priv;
    const EventInfo* ei;

    page = GNC_PLUGIN_PAGE_BUDGET(user_data);
    priv = GNC_PLUGIN_PAGE_BUDGET_GET_PRIVATE(page);
    if (changes)
    {
        ei = gnc_gui_get_entity_events(changes, &priv->key);
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
                gnc_plugin_page_budget_close_cb(user_data);
                return;
            }
            if (ei->event_mask & QOF_EVENT_MODIFY)
            {
                DEBUG("refreshing budget view because budget was modified");
                gnc_budget_view_refresh(priv->budget_view);
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
    page = GNC_PLUGIN_PAGE_BUDGET (plugin_page);
    priv = GNC_PLUGIN_PAGE_BUDGET_GET_PRIVATE(page);
    if (priv->budget_view != NULL)
    {
        LEAVE("widget = %p", priv->budget_view);
        return GTK_WIDGET(priv->budget_view);
    }

    priv->budget_view = gnc_budget_view_new(priv->budget, &priv->fd);

#if 0
    g_signal_connect(G_OBJECT(selection), "changed",
                     G_CALLBACK(gppb_selection_changed_cb), plugin_page);
#endif
    g_signal_connect(G_OBJECT(priv->budget_view), "button-press-event",
                     G_CALLBACK(gppb_button_press_cb), plugin_page);
    g_signal_connect(G_OBJECT(priv->budget_view), "account-activated",
                     G_CALLBACK(gppb_account_activated_cb), page);

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

    LEAVE("widget = %p", priv->budget_view);
    return GTK_WIDGET(priv->budget_view);
}


static void
gnc_plugin_page_budget_destroy_widget (GncPluginPage *plugin_page)
{
    GncPluginPageBudgetPrivate *priv;

    ENTER("page %p", plugin_page);
    priv = GNC_PLUGIN_PAGE_BUDGET_GET_PRIVATE(plugin_page);

    // Remove the page focus idle function if present
    g_idle_remove_by_data (GNC_PLUGIN_PAGE_BUDGET (plugin_page));

    if (priv->budget_view)
    {
        if (priv->delete_budget)
        {
            gnc_budget_view_delete_budget (priv->budget_view);
        }

        g_object_unref(G_OBJECT(priv->budget_view));
        priv->budget_view = NULL;
    }

    // Destroy the filter override hash table
    g_hash_table_destroy(priv->fd.filter_override);

    gnc_gui_component_clear_watches (priv->component_id);

    if (priv->component_id != NO_COMPONENT)
    {
        gnc_unregister_gui_component(priv->component_id);
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

    guid_to_string_buff(gnc_budget_get_guid(priv->budget), guid_str);
    g_key_file_set_string(key_file, group_name, BUDGET_GUID, guid_str);

    //FIXME
    gnc_budget_view_save(priv->budget_view, key_file, group_name);

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

    g_return_val_if_fail(key_file, NULL);
    g_return_val_if_fail(group_name, NULL);
    ENTER("key_file %p, group_name %s", key_file, group_name);

    guid_str = g_key_file_get_string(key_file, group_name, BUDGET_GUID,
                                     &error);
    if (error)
    {
        g_warning("error reading group %s key %s: %s",
                  group_name, BUDGET_GUID, error->message);
        g_error_free(error);
        error = NULL;
        return NULL;
    }
    if (!string_to_guid(guid_str, &guid))
    {
        g_free(guid_str);
        return NULL;
    }
    g_free(guid_str);

    book = qof_session_get_book(gnc_get_current_session());
    bgt = gnc_budget_lookup(&guid, book);
    if (!bgt)
    {
        return NULL;
    }

    /* Create the new page. */
    page = gnc_plugin_page_budget_new(bgt);
    budget_page = GNC_PLUGIN_PAGE_BUDGET(page);
    priv = GNC_PLUGIN_PAGE_BUDGET_GET_PRIVATE(budget_page);

    /* Install it now so we can then manipulate the created widget */
    gnc_main_window_open_page(GNC_MAIN_WINDOW(window), page);

    //FIXME
    if (!gnc_budget_view_restore(priv->budget_view, key_file, group_name))
    {
        return NULL;
    }

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

static void
gppb_account_activated_cb(GncBudgetView* view, Account* account,
                          GncPluginPageBudget *page)
{
    GtkWidget *window;
    GncPluginPage *new_page;

    g_return_if_fail(GNC_IS_PLUGIN_PAGE_BUDGET (page));

    window = GNC_PLUGIN_PAGE(page)->window;
    new_page = gnc_plugin_page_register_new(account, FALSE);
    gnc_main_window_open_page(GNC_MAIN_WINDOW(window), new_page);
}


#if 0
static void
gppb_selection_changed_cb(GtkTreeSelection *selection,
                          GncPluginPageBudget *page)
{
    GtkActionGroup *action_group;
    GtkTreeView *view;
    GList *acct_list;
    gboolean sensitive;

    g_return_if_fail(GNC_IS_PLUGIN_PAGE_BUDGET(page));

    if (!selection)
    {
        sensitive = FALSE;
    }
    else
    {
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
#endif


/*********************
 * Command callbacks *
 ********************/
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
    acct_list = gnc_budget_view_get_selected_accounts(priv->budget_view);

    window = GNC_PLUGIN_PAGE (page)->window;
    for (tmp = acct_list; tmp; tmp = g_list_next(tmp))
    {
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
    acct_list = gnc_budget_view_get_selected_accounts(priv->budget_view);

    window = GNC_PLUGIN_PAGE (page)->window;
    for (tmp = acct_list; tmp; tmp = g_list_next(tmp))
    {
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
    priv->delete_budget = TRUE;
    gnc_budget_gui_delete_budget(budget);

}


/******************************/
/*       Options Dialog       */
/******************************/
static void
gnc_plugin_page_budget_cmd_view_options (GtkAction *action,
        GncPluginPageBudget *page)
{
    GncPluginPageBudgetPrivate *priv;
    GncRecurrence *gr;
    GtkBuilder *builder;
    gint result;
    gchar *name;
    gchar *desc;
    gint num_periods;
    GtkWidget *gbname, *gbtreeview, *gbnumperiods, *gbhb;
    const Recurrence *r;

    GtkTextBuffer *buffer;
    GtkTextIter start, end;

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_BUDGET (page));
    priv = GNC_PLUGIN_PAGE_BUDGET_GET_PRIVATE(page);

    if (!priv->dialog)
    {
        builder = gtk_builder_new();
        gnc_builder_add_from_file (builder, "gnc-plugin-page-budget.glade", "NumPeriods_Adj");
        gnc_builder_add_from_file (builder, "gnc-plugin-page-budget.glade", "budget_options_container_dialog");

        priv->dialog = GTK_WIDGET(gtk_builder_get_object (builder, "budget_options_container_dialog"));

        gtk_window_set_transient_for(GTK_WINDOW(priv->dialog),
            GTK_WINDOW(gnc_plugin_page_get_window(GNC_PLUGIN_PAGE(page))));

        gbname = GTK_WIDGET(gtk_builder_get_object (builder, "BudgetName"));
        gtk_entry_set_text(GTK_ENTRY(gbname), gnc_budget_get_name(priv->budget));

        gbtreeview = GTK_WIDGET(gtk_builder_get_object (builder, "BudgetDescription"));
        buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(gbtreeview));
        gtk_text_buffer_set_text (buffer, gnc_budget_get_description(priv->budget), -1);

        gbhb = GTK_WIDGET(gtk_builder_get_object (builder, "BudgetPeriod"));
        gr = GNC_RECURRENCE(gnc_recurrence_new());
        gnc_recurrence_set(gr, gnc_budget_get_recurrence(priv->budget));
        gtk_box_pack_start (GTK_BOX (gbhb), GTK_WIDGET(gr), TRUE, TRUE, 0);
        gtk_widget_show (GTK_WIDGET(gr));

        gbnumperiods = GTK_WIDGET(gtk_builder_get_object (builder, "BudgetNumPeriods"));
        gtk_spin_button_set_value(GTK_SPIN_BUTTON(gbnumperiods), gnc_budget_get_num_periods(priv->budget));

        gtk_widget_show_all (priv->dialog);
        result = gtk_dialog_run(GTK_DIALOG(priv->dialog));

        switch (result)
        {
        case GTK_RESPONSE_OK:
            name = (gchar *) gtk_entry_get_text(GTK_ENTRY(gbname));
            DEBUG("%s", name);
            if (name)
            {
                gchar* label;
                gnc_budget_set_name(priv->budget, name);
                label = g_strdup_printf("%s: %s", _("Budget"), name);
                main_window_update_page_name(GNC_PLUGIN_PAGE(page), label);
                g_free(label);
            }

            gtk_text_buffer_get_bounds (gtk_text_view_get_buffer(GTK_TEXT_VIEW (gbtreeview)), &start, &end);
            desc = gtk_text_buffer_get_text (gtk_text_view_get_buffer(GTK_TEXT_VIEW (gbtreeview)), &start, &end, TRUE);

            gnc_budget_set_description(priv->budget, desc);
            g_free(desc);

            num_periods = gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(gbnumperiods));
            gnc_budget_set_num_periods(priv->budget, num_periods);

            r = gnc_recurrence_get(gr);
            gnc_budget_set_recurrence(priv->budget, r);
            break;
        case GTK_RESPONSE_CANCEL:
            break;
        default:
            break;
        }
        g_object_unref(G_OBJECT(builder));
        gtk_widget_destroy(priv->dialog);
    }
    priv->dialog = NULL;
}


void
gnc_budget_gui_delete_budget(GncBudget *budget)
{
    const char *name;

    g_return_if_fail(GNC_IS_BUDGET(budget));
    name = gnc_budget_get_name (budget);
    if (!name)
        name = _("Unnamed Budget");

    if (gnc_verify_dialog (NULL, FALSE, _("Delete %s?"), name))
    {
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

    acct = gnc_budget_view_get_account_from_path(priv->budget_view, path);

    num_periods = gnc_budget_get_num_periods(priv->budget);

    for (i = 0; i < num_periods; i++)
    {
        num = recurrenceGetAccountPeriodValue(&priv->r, acct, i);
        if (!gnc_numeric_check(num))
        {
            if (gnc_reverse_balance (acct))
                num = gnc_numeric_neg (num);


            num = gnc_numeric_convert(num, GNC_DENOM_AUTO,
                                      GNC_HOW_DENOM_SIGFIGS(priv->sigFigs) | GNC_HOW_RND_ROUND_HALF_UP);
            gnc_budget_set_account_period_value(
                priv->budget, acct, i, num);
        }
    }
}


/*******************************/
/*       Estimate Dialog       */
/*******************************/
static void
gnc_plugin_page_budget_cmd_estimate_budget(GtkAction *action,
        GncPluginPageBudget *page)
{
    GncPluginPageBudgetPrivate *priv;
    GtkTreeSelection *sel;
    GtkWidget *dialog, *gde, *dtr, *hb;
    gint result;
    GDate date;
    const Recurrence *r;
    GtkBuilder *builder;

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_BUDGET(page));
    priv = GNC_PLUGIN_PAGE_BUDGET_GET_PRIVATE(page);

    sel = gnc_budget_view_get_selection(priv->budget_view);

    if (gtk_tree_selection_count_selected_rows(sel) <= 0)
    {
        dialog = gtk_message_dialog_new (
                     GTK_WINDOW(gnc_plugin_page_get_window(GNC_PLUGIN_PAGE(page))),
                     GTK_DIALOG_DESTROY_WITH_PARENT | GTK_DIALOG_MODAL,
                     GTK_MESSAGE_INFO, GTK_BUTTONS_CLOSE, "%s",
                     _("You must select at least one account to estimate."));
        gtk_dialog_run (GTK_DIALOG(dialog));
        gtk_widget_destroy(dialog);
        return;
    }

    builder = gtk_builder_new();
    gnc_builder_add_from_file (builder, "gnc-plugin-page-budget.glade", "DigitsToRound_Adj");
    gnc_builder_add_from_file (builder, "gnc-plugin-page-budget.glade", "budget_estimate_dialog");

    dialog = GTK_WIDGET(gtk_builder_get_object (builder, "budget_estimate_dialog"));

    gtk_window_set_transient_for(GTK_WINDOW(dialog),
        GTK_WINDOW(gnc_plugin_page_get_window(GNC_PLUGIN_PAGE(page))));

    hb = GTK_WIDGET(gtk_builder_get_object (builder, "StartDate_hbox"));
    gde = gnc_date_edit_new (time (NULL), FALSE, FALSE);
    gtk_box_pack_start (GTK_BOX (hb), gde, TRUE, TRUE, 0);
    gtk_widget_show (gde);

    date = recurrenceGetDate(&priv->r);
    gnc_date_edit_set_gdate(GNC_DATE_EDIT(gde), &date);

    dtr = GTK_WIDGET(gtk_builder_get_object (builder, "DigitsToRound"));
    gtk_spin_button_set_value(GTK_SPIN_BUTTON(dtr),
                              (gdouble)priv->sigFigs);

    gtk_widget_show_all (dialog);
    result = gtk_dialog_run(GTK_DIALOG(dialog));
    switch (result)
    {
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
    g_object_unref(G_OBJECT(builder));
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
