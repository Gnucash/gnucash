/*
 * gnc-plugin-page-account-tree.c --
 *
 * Copyright (C) 2003 Jan Arne Petersen <jpetersen@uni-bonn.de>
 * Copyright (C) 2003,2005,2006 David Hampton <hampton@employees.org>
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

/** @addtogroup ContentPlugins
    @{ */
/** @addtogroup GncPluginPageAccountTree An Account Tree Plugin
    @{ */
/** @file gnc-plugin-page-account-tree.c
    @brief Functions providing a chart of account page.
    @author Copyright (C) 2003 Jan Arne Petersen <jpetersen@uni-bonn.de>
    @author Copyright (C) 2003,2005,2006 David Hampton <hampton@employees.org>
*/

#include <config.h>

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include "gnc-plugin-page-account-tree.h"
#include "gnc-plugin-page-register.h"
#include "gnc-plugin-page-register2.h"

#include "Scrub.h"
#include "Scrub3.h"
#include "ScrubBusiness.h"
#include "Transaction.h"
#include "dialog-account.h"
#include "dialog-transfer.h"
#include "dialog-utils.h"
#include "assistant-hierarchy.h"
#include "gnc-account-sel.h"
#include "gnc-component-manager.h"
#include "gnc-engine.h"
#include "gnc-gnome-utils.h"
#include "gnc-gobject-utils.h"
#include "gnc-icons.h"
#include "gnc-plugin-account-tree.h"
#include "gnc-prefs.h"
#include "gnc-session.h"
#include "gnc-split-reg.h"
#include "gnc-state.h"
#include "gnc-tree-view-account.h"
#include "gnc-tree-model-account-types.h"
#include "gnc-ui.h"
#include "gnc-ui-util.h"
#include "gnc-window.h"
#include "dialog-lot-viewer.h"
#include "window-reconcile.h"
#include "window-autoclear.h"
#include "window-main-summarybar.h"
#include "dialog-object-references.h"
#include "dialog-find-account.h"
#include <gnc-glib-utils.h>

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_GUI;


/********************************************************************
 * delete_account_helper
 * See if this account has any splits present.  Set the user data
 * and return the same value to stop walking the account tree if
 * appropriate.
 ********************************************************************/
typedef struct _delete_helper
{
    gboolean has_splits;
    gboolean has_ro_splits;
} delete_helper_t;


#define PLUGIN_PAGE_ACCT_TREE_CM_CLASS "plugin-page-acct-tree"
#define STATE_SECTION "Account Hierarchy"

#define DELETE_DIALOG_FILTER       "filter"
#define DELETE_DIALOG_ACCOUNT      "account"
#define DELETE_DIALOG_TRANS_MAS    "trans_mas"
#define DELETE_DIALOG_SA_MAS       "sa_mas"
#define DELETE_DIALOG_SA_TRANS_MAS "sa_trans_mas"
#define DELETE_DIALOG_SA_TRANS     "sa_trans"
#define DELETE_DIALOG_SA_SPLITS    "sa_has_split"
#define DELETE_DIALOG_OK_BUTTON    "deletebutton"

enum
{
    ACCOUNT_SELECTED,
    LAST_SIGNAL
};

typedef struct GncPluginPageAccountTreePrivate
{
    GtkWidget   *widget;
    GtkTreeView *tree_view;
    gint         component_id;
    AccountFilterDialog fd;
} GncPluginPageAccountTreePrivate;

#define GNC_PLUGIN_PAGE_ACCOUNT_TREE_GET_PRIVATE(o)  \
     ((GncPluginPageAccountTreePrivate*)gnc_plugin_page_account_tree_get_instance_private((GncPluginPageAccountTree*)o))

static GObjectClass *parent_class = NULL;

/************************************************************
 *                        Prototypes                        *
 ************************************************************/
/* Plugin Actions */
static void gnc_plugin_page_account_tree_class_init (GncPluginPageAccountTreeClass *klass);
static void gnc_plugin_page_account_tree_init (GncPluginPageAccountTree *plugin_page);
static void gnc_plugin_page_account_tree_finalize (GObject *object);
static void gnc_plugin_page_account_tree_selected (GObject *object, gpointer user_data);

static gboolean gnc_plugin_page_account_tree_focus_widget (GncPluginPage *plugin_page);
static GtkWidget *gnc_plugin_page_account_tree_create_widget (GncPluginPage *plugin_page);
static void gnc_plugin_page_account_tree_destroy_widget (GncPluginPage *plugin_page);
static void gnc_plugin_page_account_tree_save_page (GncPluginPage *plugin_page, GKeyFile *file, const gchar *group);
static GncPluginPage *gnc_plugin_page_account_tree_recreate_page (GtkWidget *window, GKeyFile *file, const gchar *group);

/* Callbacks */
static void gnc_plugin_page_account_tree_summarybar_position_changed(gpointer prefs, gchar* pref, gpointer user_data);
static gboolean gnc_plugin_page_account_tree_button_press_cb (GtkWidget *widget,
        GdkEventButton *event,
        GncPluginPage *page);
static void gnc_plugin_page_account_tree_double_click_cb (GtkTreeView        *treeview,
        GtkTreePath        *path,
        GtkTreeViewColumn  *col,
        GncPluginPageAccountTree *page);

static void gnc_plugin_page_account_tree_selection_changed_cb (GtkTreeSelection *selection,
        GncPluginPageAccountTree *page);
void gppat_populate_trans_mas_list(GtkToggleButton *sa_mrb, GtkWidget *dialog);
void gppat_set_insensitive_iff_rb_active(GtkWidget *widget, GtkToggleButton *b);

/* Command callbacks */
static void gnc_plugin_page_account_tree_cmd_new_account (GtkAction *action, GncPluginPageAccountTree *plugin_page);
static void gnc_plugin_page_account_tree_cmd_file_new_hierarchy (GtkAction *action, GncPluginPageAccountTree *plugin_page);
static void gnc_plugin_page_account_tree_cmd_open_account (GtkAction *action, GncPluginPageAccountTree *page);
static void gnc_plugin_page_account_tree_cmd_open_subaccounts (GtkAction *action, GncPluginPageAccountTree *page);
static void gnc_plugin_page_account_tree_cmd_edit_account (GtkAction *action, GncPluginPageAccountTree *page);
static void gnc_plugin_page_account_tree_cmd_find_account (GtkAction *action, GncPluginPageAccountTree *page);
static void gnc_plugin_page_account_tree_cmd_find_account_popup (GtkAction *action, GncPluginPageAccountTree *page);
static void gnc_plugin_page_account_tree_cmd_delete_account (GtkAction *action, GncPluginPageAccountTree *page);
static void gnc_plugin_page_account_tree_cmd_renumber_accounts (GtkAction *action, GncPluginPageAccountTree *page);
static void gnc_plugin_page_account_tree_cmd_view_filter_by (GtkAction *action, GncPluginPageAccountTree *plugin_page);
static void gnc_plugin_page_account_tree_cmd_reconcile (GtkAction *action, GncPluginPageAccountTree *page);
static void gnc_plugin_page_account_tree_cmd_refresh (GtkAction *action, GncPluginPageAccountTree *page);
static void gnc_plugin_page_account_tree_cmd_autoclear (GtkAction *action, GncPluginPageAccountTree *page);
static void gnc_plugin_page_account_tree_cmd_transfer (GtkAction *action, GncPluginPageAccountTree *page);
static void gnc_plugin_page_account_tree_cmd_stock_split (GtkAction *action, GncPluginPageAccountTree *page);
static void gnc_plugin_page_account_tree_cmd_edit_tax_options (GtkAction *action, GncPluginPageAccountTree *page);
static void gnc_plugin_page_account_tree_cmd_lots (GtkAction *action, GncPluginPageAccountTree *page);
static void gnc_plugin_page_account_tree_cmd_scrub (GtkAction *action, GncPluginPageAccountTree *page);
static void gnc_plugin_page_account_tree_cmd_scrub_sub (GtkAction *action, GncPluginPageAccountTree *page);
static void gnc_plugin_page_account_tree_cmd_scrub_all (GtkAction *action, GncPluginPageAccountTree *page);
static void gnc_plugin_page_account_tree_cmd_cascade_account_properties (GtkAction *action, GncPluginPageAccountTree *page);

/* Command callback for new Register Test */
static void gnc_plugin_page_account_tree_cmd_open2_account (GtkAction *action, GncPluginPageAccountTree *page);
static void gnc_plugin_page_account_tree_cmd_open2_subaccounts (GtkAction *action, GncPluginPageAccountTree *page);
/* Account Deletion Actions. */
static int confirm_delete_account (GtkAction *action,
                                   GncPluginPageAccountTree *page, Account* ta,
                                   Account* sta, Account* saa,
                                   delete_helper_t delete_res);
static void  do_delete_account (Account* account, Account* saa, Account* sta,
                                Account* ta);



static guint plugin_page_signals[LAST_SIGNAL] = { 0 };


static GtkActionEntry gnc_plugin_page_account_tree_actions [] =
{
    /* Toplevel */
    { "FakeToplevel", NULL, "", NULL, NULL, NULL },

    /* File menu */
    {
        "FileNewAccountAction", GNC_ICON_NEW_ACCOUNT, N_("New _Account..."), NULL,
        N_("Create a new Account"),
        G_CALLBACK (gnc_plugin_page_account_tree_cmd_new_account)
    },
    {
        "FileAddAccountHierarchyAssistantAction", GNC_ICON_NEW_ACCOUNT, N_("New Account _Hierarchy..."), NULL,
        N_("Extend the current book by merging with new account type categories"),
        G_CALLBACK (gnc_plugin_page_account_tree_cmd_file_new_hierarchy)
    },
#ifdef REGISTER2_ENABLED
    {
        "FileOpenAccount2Action", GNC_ICON_OPEN_ACCOUNT, N_("Open _Account"), NULL,
        N_("Open the selected account"),
        G_CALLBACK (gnc_plugin_page_account_tree_cmd_open2_account)
    },
    {
        "FileOpenAccountAction", GNC_ICON_OPEN_ACCOUNT, N_("Open _Old Style Register Account"), NULL,
        N_("Open the old style register selected account"),
        G_CALLBACK (gnc_plugin_page_account_tree_cmd_open_account)
    },
#else
    {
        "FileOpenAccountAction", GNC_ICON_OPEN_ACCOUNT, N_("Open _Account"), NULL,
        N_("Open the selected account"),
        G_CALLBACK (gnc_plugin_page_account_tree_cmd_open_account)
    },
#endif

#ifdef REGISTER2_ENABLED
    {
        "FileOpenSubaccounts2Action", GNC_ICON_OPEN_ACCOUNT, N_("Open _SubAccounts"), NULL,
        N_("Open the selected account and all its subaccounts"),
        G_CALLBACK (gnc_plugin_page_account_tree_cmd_open2_subaccounts)
    },
    {
        "FileOpenSubaccountsAction", GNC_ICON_OPEN_ACCOUNT, N_("Open Old St_yle Subaccounts"), NULL,
        N_("Open the old style register selected account and all its subaccounts"),
        G_CALLBACK (gnc_plugin_page_account_tree_cmd_open_subaccounts)
    },
#else
    {
        "FileOpenSubaccountsAction", GNC_ICON_OPEN_ACCOUNT, N_("Open _SubAccounts"), NULL,
        N_("Open the selected account and all its subaccounts"),
        G_CALLBACK (gnc_plugin_page_account_tree_cmd_open_subaccounts)
    },
#endif

    /* Edit menu */
    {
        "EditEditAccountAction", GNC_ICON_EDIT_ACCOUNT, N_("Edit _Account"), "<primary>e",
        N_("Edit the selected account"),
        G_CALLBACK (gnc_plugin_page_account_tree_cmd_edit_account)
    },
    {
        "EditDeleteAccountAction", GNC_ICON_DELETE_ACCOUNT, N_("_Delete Account..."), "Delete",
        N_("Delete selected account"),
        G_CALLBACK (gnc_plugin_page_account_tree_cmd_delete_account)
    },
    {
        "EditCascadeAccountAction", NULL, N_("_Cascade Account Properties..."), NULL,
        N_("Cascade selected properties for account"),
        G_CALLBACK (gnc_plugin_page_account_tree_cmd_cascade_account_properties)
    },
    {
        "EditFindAccountAction", "edit-find", N_("F_ind Account"), "<primary>i",
        N_("Find an account"),
        G_CALLBACK (gnc_plugin_page_account_tree_cmd_find_account)
    },
    {
        "EditFindAccountPopupAction", "edit-find", N_("F_ind Account"), "<primary>i",
        N_("Find an account"),
        G_CALLBACK (gnc_plugin_page_account_tree_cmd_find_account_popup)
    },
    {
        "EditRenumberSubaccountsAction", NULL, N_("_Renumber Subaccounts..."), NULL,
        N_("Renumber the children of the selected account"),
        G_CALLBACK (gnc_plugin_page_account_tree_cmd_renumber_accounts)
    },
    {
        "EditTaxOptionsAction", NULL,
        /* Translators: remember to reuse this
           translation in dialog-account.glade */
        N_("Ta_x Report Options"), NULL,
        /* Translators: currently implemented are
           US: income tax and
           DE: VAT
           So adjust this string                  */
        N_("Setup relevant accounts for tax reports, e.g. US income tax"),
        G_CALLBACK (gnc_plugin_page_account_tree_cmd_edit_tax_options)
    },
    /* View menu */
    {
        "ViewFilterByAction", NULL, N_("_Filter By..."), NULL, NULL,
        G_CALLBACK (gnc_plugin_page_account_tree_cmd_view_filter_by)
    },
    {
        "ViewRefreshAction", "view-refresh", N_("_Refresh"), "<primary>r",
        N_("Refresh this window"),
        G_CALLBACK (gnc_plugin_page_account_tree_cmd_refresh)
    },

    /* Actions menu */
    {
        "ActionsReconcileAction", NULL, N_("_Reconcile..."), NULL,
        N_("Reconcile the selected account"),
        G_CALLBACK (gnc_plugin_page_account_tree_cmd_reconcile)
    },
    {
        "ActionsAutoClearAction", NULL, N_("_Auto-clear..."), NULL,
        N_("Automatically clear individual transactions, given a cleared amount"),
        G_CALLBACK (gnc_plugin_page_account_tree_cmd_autoclear)
    },
    {
        "ActionsTransferAction", NULL, N_("_Transfer..."), "<primary>t",
        N_("Transfer funds from one account to another"),
        G_CALLBACK (gnc_plugin_page_account_tree_cmd_transfer)
    },
    {
        "ActionsStockSplitAction", NULL, N_("Stoc_k Split..."), NULL,
        N_("Record a stock split or a stock merger"),
        G_CALLBACK (gnc_plugin_page_account_tree_cmd_stock_split)
    },
    {
        "ActionsLotsAction", NULL, N_("View _Lots..."), NULL,
        N_("Bring up the lot viewer/editor window"),
        G_CALLBACK (gnc_plugin_page_account_tree_cmd_lots)
    },
    {
        "ScrubAction", NULL, N_("Check & Repair A_ccount"), NULL,
        N_("Check for and repair unbalanced transactions and orphan splits " "in this account"),
        G_CALLBACK (gnc_plugin_page_account_tree_cmd_scrub)
    },
    {
        "ScrubSubAction", NULL, N_("Check & Repair Su_baccounts"), NULL,
        N_("Check for and repair unbalanced transactions and orphan splits "
        "in this account and its subaccounts"),
        G_CALLBACK (gnc_plugin_page_account_tree_cmd_scrub_sub)
    },
    {
        "ScrubAllAction", NULL, N_("Check & Repair A_ll"), NULL,
        N_("Check for and repair unbalanced transactions and orphan splits " "in all accounts"),
        G_CALLBACK (gnc_plugin_page_account_tree_cmd_scrub_all)
    },
    /* Extensions Menu */
    { "Register2TestAction", NULL, N_("_Register2"), NULL, NULL, NULL },
    {
        "Register2TestAccountAction", GNC_ICON_OPEN_ACCOUNT, N_("Open _Account"), NULL,
        N_("Open the selected account"),
        G_CALLBACK (gnc_plugin_page_account_tree_cmd_open2_account)
    },
    {
        "Register2TestSubAccountAction", GNC_ICON_OPEN_ACCOUNT, N_("Open _SubAccounts"), NULL,
        N_("Open the selected account and all its subaccounts"),
        G_CALLBACK (gnc_plugin_page_account_tree_cmd_open2_subaccounts)
    },
};
/** The number of actions provided by this plugin. */
static guint gnc_plugin_page_account_tree_n_actions = G_N_ELEMENTS (gnc_plugin_page_account_tree_actions);


/** Actions that require an account to be selected before they are
 *  enabled, and the book is in read-write mode. */
static const gchar *actions_requiring_account_rw[] =
{
    "EditEditAccountAction",
    "EditDeleteAccountAction",
    "ActionsReconcileAction",
    "ActionsAutoClearAction",
    NULL
};

/** Actions that require the selected account to have subaccounts
 *  before they are enabled, and the book is in read-write mode. */
static const gchar *actions_requiring_subaccounts_rw[] =
{
    "EditRenumberSubaccountsAction",
    "EditCascadeAccountAction",
    NULL
};

/** Actions that require an account to be selected before they are
 *  enabled. Those actions can be selected even if the book is in readonly mode. */
static const gchar *actions_requiring_account_always[] =
{
    "FileOpenAccountAction",
#ifdef REGISTER2_ENABLED
    "FileOpenAccount2Action",
#endif
    "FileOpenSubaccountsAction",
    "ActionsLotsAction",
    NULL
};

/* This is the list of actions which are switched inactive in a read-only book. */
static const gchar* readonly_inactive_actions[] =
{
    "FileNewAccountAction",
    "FileAddAccountHierarchyAssistantAction",
    "EditEditAccountAction",
    "EditDeleteAccountAction",
    "ActionsTransferAction",
    "ActionsReconcileAction",
    "ActionsAutoClearAction",
    "ActionsStockSplitAction",
    "ScrubAction",
    "ScrubSubAction",
    "ScrubAllAction",
    NULL
};

/** Short labels for use on the toolbar buttons. */
static action_toolbar_labels toolbar_labels[] =
{
    { "FileOpenAccountAction", 	            N_("Open") },
#ifdef REGISTER2_ENABLED
    { "FileOpenAccount2Action", 	    N_("Open2") },
#endif
    { "EditEditAccountAction", 	            N_("Edit") },
    { "FileNewAccountAction",    	    N_("New") },
    { "EditDeleteAccountAction", 	    N_("Delete") },
    { NULL, NULL },
};

GncPluginPage *
gnc_plugin_page_account_tree_new (void)
{
    GncPluginPageAccountTree *plugin_page;

    ENTER(" ");
    plugin_page = g_object_new (GNC_TYPE_PLUGIN_PAGE_ACCOUNT_TREE,
                                NULL);

    LEAVE("new account tree page %p", plugin_page);
    return GNC_PLUGIN_PAGE (plugin_page);
}

G_DEFINE_TYPE_WITH_PRIVATE(GncPluginPageAccountTree, gnc_plugin_page_account_tree, GNC_TYPE_PLUGIN_PAGE)

static gboolean show_abort_verify = TRUE;

static void
prepare_scrubbing ()
{
    gnc_suspend_gui_refresh ();
    gnc_set_abort_scrub (FALSE);
}

static void
finish_scrubbing (GncWindow *window, gulong handler_id)
{
    g_signal_handler_disconnect (G_OBJECT(window), handler_id);
    show_abort_verify = TRUE;
    gnc_resume_gui_refresh ();
}

static gboolean
gnc_plugin_page_account_finish_pending (GncPluginPage* page)
{
    if (gnc_get_ongoing_scrub ())
    {
        if (show_abort_verify)
        {
            gboolean ret = gnc_verify_dialog (GTK_WINDOW(gnc_plugin_page_get_window
                                             (GNC_PLUGIN_PAGE(page))), FALSE,
                                             _("'Check & Repair' is currently running, do you want to abort it?"));

            show_abort_verify = FALSE;

            if (ret)
                gnc_set_abort_scrub (TRUE);

            return ret; // verify response
        }
        else
        {
            if (gnc_get_abort_scrub ())
                return TRUE; // close
            else
                return FALSE; // no close
        }
    }
    else
        return TRUE; // normal close
}

static void
gnc_plugin_page_account_tree_class_init (GncPluginPageAccountTreeClass *klass)
{
    GObjectClass *object_class = G_OBJECT_CLASS (klass);
    GncPluginPageClass *gnc_plugin_class = GNC_PLUGIN_PAGE_CLASS(klass);

    parent_class = g_type_class_peek_parent (klass);

    object_class->finalize = gnc_plugin_page_account_tree_finalize;

    gnc_plugin_class->tab_icon        = GNC_ICON_ACCOUNT;
    gnc_plugin_class->plugin_name     = GNC_PLUGIN_PAGE_ACCOUNT_TREE_NAME;
    gnc_plugin_class->create_widget   = gnc_plugin_page_account_tree_create_widget;
    gnc_plugin_class->destroy_widget  = gnc_plugin_page_account_tree_destroy_widget;
    gnc_plugin_class->save_page       = gnc_plugin_page_account_tree_save_page;
    gnc_plugin_class->recreate_page   = gnc_plugin_page_account_tree_recreate_page;
    gnc_plugin_class->focus_page_function = gnc_plugin_page_account_tree_focus_widget;
    gnc_plugin_class->finish_pending = gnc_plugin_page_account_finish_pending;

    plugin_page_signals[ACCOUNT_SELECTED] =
        g_signal_new ("account_selected",
                      G_OBJECT_CLASS_TYPE (object_class),
                      G_SIGNAL_RUN_FIRST,
                      G_STRUCT_OFFSET (GncPluginPageAccountTreeClass, account_selected),
                      NULL, NULL,
                      g_cclosure_marshal_VOID__POINTER,
                      G_TYPE_NONE, 1,
                      G_TYPE_POINTER);
}

static void
gnc_plugin_page_account_tree_init (GncPluginPageAccountTree *plugin_page)
{
    GtkActionGroup *action_group;
    GncPluginPageAccountTreePrivate *priv;
    GncPluginPage *parent;
    const GList *page_list;

    ENTER("page %p", plugin_page);
    priv = GNC_PLUGIN_PAGE_ACCOUNT_TREE_GET_PRIVATE(plugin_page);

    /* Init parent declared variables */
    parent = GNC_PLUGIN_PAGE(plugin_page);
#ifdef REGISTER2_ENABLED
    g_object_set(G_OBJECT(plugin_page),
                 "page-name",      _("Accounts"),
                 "page-uri",       "default:",
                 "ui-description", "gnc-plugin-page-account-tree2-ui.xml",
                 NULL);
#else
    g_object_set(G_OBJECT(plugin_page),
                 "page-name",      _("Accounts"),
                 "page-uri",       "default:",
                 "ui-description", "gnc-plugin-page-account-tree-ui.xml",
                 NULL);
#endif
    g_signal_connect (G_OBJECT (plugin_page), "selected",
                      G_CALLBACK (gnc_plugin_page_account_tree_selected), plugin_page);

    /* change me when the system supports multiple books */
    gnc_plugin_page_add_book(parent, gnc_get_current_book());

    /* Is this the first accounts page? */
    page_list =
        gnc_gobject_tracking_get_list(GNC_PLUGIN_PAGE_ACCOUNT_TREE_NAME);
    if (!page_list || plugin_page == page_list->data)
    {
        g_object_set_data(G_OBJECT(plugin_page), PLUGIN_PAGE_IMMUTABLE,
		          GINT_TO_POINTER(1));
    }

    /* Create menu and toolbar information */
    action_group =
        gnc_plugin_page_create_action_group(parent,
                                            "GncPluginPageAccountTreeActions");
    gtk_action_group_add_actions(action_group,
                                 gnc_plugin_page_account_tree_actions,
                                 gnc_plugin_page_account_tree_n_actions,
                                 plugin_page);
    gnc_plugin_init_short_names (action_group, toolbar_labels);

    /* Visible types */
    priv->fd.visible_types = -1; /* Start with all types */
    priv->fd.show_hidden = FALSE;
    priv->fd.show_unused = TRUE;
    priv->fd.show_zero_total = TRUE;
    priv->fd.filter_override = g_hash_table_new (g_direct_hash, g_direct_equal);

    LEAVE("page %p, priv %p, action group %p",
          plugin_page, priv, action_group);
}

static void
gnc_plugin_page_account_tree_finalize (GObject *object)
{
    GncPluginPageAccountTree *page;
    GncPluginPageAccountTreePrivate *priv;

    ENTER("object %p", object);
    page = GNC_PLUGIN_PAGE_ACCOUNT_TREE (object);
    g_return_if_fail (GNC_IS_PLUGIN_PAGE_ACCOUNT_TREE (page));
    priv = GNC_PLUGIN_PAGE_ACCOUNT_TREE_GET_PRIVATE(page);
    g_return_if_fail (priv != NULL);

    G_OBJECT_CLASS (parent_class)->finalize (object);
    LEAVE(" ");
}

void
gnc_plugin_page_account_tree_open (Account *account, GtkWindow *win)
{
    GncPluginPageAccountTreePrivate *priv;
    GncPluginPageAccountTree *page;
    GncPluginPage *plugin_page = NULL;
    const GList *page_list;
    GtkWidget   *window;

    /* Find Accounts page */
    page_list = gnc_gobject_tracking_get_list(GNC_PLUGIN_PAGE_ACCOUNT_TREE_NAME);

    // If we have a window, look for account page in that window
    if (gnc_list_length_cmp (page_list, 0))
    {
        if (win != NULL)
        {
            for ( ; page_list; page_list = g_list_next(page_list))
            {
                plugin_page = GNC_PLUGIN_PAGE(page_list->data);
                if (GTK_WINDOW(plugin_page->window) == win)
                    break;
            }
        }
        else // if no window, open first account page in list
            plugin_page = GNC_PLUGIN_PAGE(page_list->data);
    }
    else // we have no account pages, create one
        plugin_page = gnc_plugin_page_account_tree_new ();

    g_return_if_fail(plugin_page);
    window = plugin_page->window;

    gnc_main_window_open_page (GNC_MAIN_WINDOW(window), plugin_page);

    page = GNC_PLUGIN_PAGE_ACCOUNT_TREE (plugin_page);
    priv = GNC_PLUGIN_PAGE_ACCOUNT_TREE_GET_PRIVATE(page);

    if (account != NULL)
    {
        Account *root_account = gnc_get_current_root_account ();
        Account *parent_account = NULL;
        Account *temp_account = account;

        g_hash_table_insert (priv->fd.filter_override, account, account);

        // make sure we override all the parent accounts to root
        while (parent_account != root_account)
        {
            parent_account = gnc_account_get_parent (temp_account);

            g_hash_table_insert (priv->fd.filter_override, parent_account, parent_account);
            temp_account = parent_account;
        }
        gnc_tree_view_account_refilter (GNC_TREE_VIEW_ACCOUNT(priv->tree_view));
        gnc_tree_view_account_set_selected_account (GNC_TREE_VIEW_ACCOUNT(priv->tree_view), account);
    }
}

Account *
gnc_plugin_page_account_tree_get_current_account (GncPluginPageAccountTree *page)
{
    GncPluginPageAccountTreePrivate *priv;
    Account *account;

    priv = GNC_PLUGIN_PAGE_ACCOUNT_TREE_GET_PRIVATE(page);
    ENTER("page %p (tree view %p)", page, priv->tree_view);
    account = gnc_tree_view_account_get_selected_account (GNC_TREE_VIEW_ACCOUNT(priv->tree_view));
    if (account == NULL)
    {
        LEAVE("no account");
        return NULL;
    }

    LEAVE("account %p", account);
    return account;
}

/**
 * Whenever the current page is changed, if an account page is
 * the current page, set focus on the tree view.
 */
static gboolean
gnc_plugin_page_account_tree_focus_widget (GncPluginPage *account_plugin_page)
{
    if (GNC_IS_PLUGIN_PAGE_ACCOUNT_TREE(account_plugin_page))
    {
        GncPluginPageAccountTreePrivate *priv = GNC_PLUGIN_PAGE_ACCOUNT_TREE_GET_PRIVATE(account_plugin_page);
        GtkTreeView *view = GTK_TREE_VIEW(priv->tree_view);

        if (!gtk_widget_is_focus (GTK_WIDGET(view)))
            gtk_widget_grab_focus (GTK_WIDGET(view));
    }
    return FALSE;
}

/* Virtual Functions */

static void
gnc_plugin_page_account_refresh_cb (GHashTable *changes, gpointer user_data)
{
    GncPluginPageAccountTree *page = user_data;
    GncPluginPageAccountTreePrivate *priv;

    g_return_if_fail(GNC_IS_PLUGIN_PAGE_ACCOUNT_TREE(page));

    /* We're only looking for forced updates here. */
    if (changes)
        return;

    priv = GNC_PLUGIN_PAGE_ACCOUNT_TREE_GET_PRIVATE(page);

    gnc_tree_view_account_clear_model_cache (GNC_TREE_VIEW_ACCOUNT(priv->tree_view));
    gtk_widget_queue_draw(priv->widget);
}

static void
gnc_plugin_page_account_tree_close_cb (gpointer user_data)
{
    GncPluginPage *plugin_page = GNC_PLUGIN_PAGE(user_data);
    gnc_main_window_close_page(plugin_page);
}

static void
gnc_plugin_page_account_editing_started_cd (gpointer various, GncPluginPageRegister *page)
{
    GncPluginPage *plugin_page = GNC_PLUGIN_PAGE(page);
    GtkAction *action = gnc_main_window_find_action (GNC_MAIN_WINDOW(plugin_page->window),
                                                     "EditDeleteAccountAction");

    if (action != NULL)
        gtk_action_set_sensitive (action, FALSE);
}

static void
gnc_plugin_page_account_editing_finished_cb (gpointer various, GncPluginPageRegister *page)
{
    GncPluginPage *plugin_page = GNC_PLUGIN_PAGE(page);
    GtkAction *action = gnc_main_window_find_action (GNC_MAIN_WINDOW(plugin_page->window),
                                                     "EditDeleteAccountAction");

    if (action != NULL)
        gtk_action_set_sensitive (action, TRUE);
}

static GtkWidget *
gnc_plugin_page_account_tree_create_widget (GncPluginPage *plugin_page)
{
    GncPluginPageAccountTree *page;
    GncPluginPageAccountTreePrivate *priv;
    GtkTreeSelection *selection;
    GtkTreeView *tree_view;
    GtkWidget *scrolled_window;
    GtkTreeViewColumn *col;

    ENTER("page %p", plugin_page);
    page = GNC_PLUGIN_PAGE_ACCOUNT_TREE (plugin_page);
    priv = GNC_PLUGIN_PAGE_ACCOUNT_TREE_GET_PRIVATE(page);
    if (priv->widget != NULL)
    {
        LEAVE("widget = %p", priv->widget);
        return priv->widget;
    }

    priv->widget = gtk_box_new (GTK_ORIENTATION_VERTICAL, 0);
    gtk_box_set_homogeneous (GTK_BOX (priv->widget), FALSE);
    gtk_widget_show (priv->widget);

    // Set the name for this widget so it can be easily manipulated with css
    gtk_widget_set_name (GTK_WIDGET(priv->widget), "gnc-id-account-page");

    scrolled_window = gtk_scrolled_window_new (NULL, NULL);
    gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolled_window),
                                    GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
    gtk_widget_show (scrolled_window);
    gtk_box_pack_start (GTK_BOX (priv->widget), scrolled_window,
                        TRUE, TRUE, 0);

    tree_view = gnc_tree_view_account_new(FALSE);
    col = gnc_tree_view_find_column_by_name(
              GNC_TREE_VIEW(tree_view), "description");
    g_object_set_data(G_OBJECT(col), DEFAULT_VISIBLE, GINT_TO_POINTER(1));
    col = gnc_tree_view_find_column_by_name(
              GNC_TREE_VIEW(tree_view), "total");
    g_object_set_data(G_OBJECT(col), DEFAULT_VISIBLE, GINT_TO_POINTER(1));
    gnc_tree_view_configure_columns(GNC_TREE_VIEW(tree_view));
    g_object_set(G_OBJECT(tree_view),
                 "state-section", STATE_SECTION,
                 "show-column-menu", TRUE,
                 NULL);

    /* No name handler; then the user can't click on the name of the
       account to open its register. */
    gnc_tree_view_account_set_code_edited(GNC_TREE_VIEW_ACCOUNT(tree_view),
                                          gnc_tree_view_account_code_edited_cb);
    gnc_tree_view_account_set_description_edited(GNC_TREE_VIEW_ACCOUNT(tree_view),
            gnc_tree_view_account_description_edited_cb);
    gnc_tree_view_account_set_notes_edited(GNC_TREE_VIEW_ACCOUNT(tree_view),
                                           gnc_tree_view_account_notes_edited_cb);

    // Setup some callbacks so menu actions can be disabled/enabled
    gnc_tree_view_account_set_editing_started_cb(GNC_TREE_VIEW_ACCOUNT(tree_view),
        (GFunc)gnc_plugin_page_account_editing_started_cd, page);
    gnc_tree_view_account_set_editing_finished_cb(GNC_TREE_VIEW_ACCOUNT(tree_view),
        (GFunc)gnc_plugin_page_account_editing_finished_cb, page);

    priv->tree_view = tree_view;
    selection = gtk_tree_view_get_selection(tree_view);
    g_signal_connect (G_OBJECT (selection), "changed",
                      G_CALLBACK (gnc_plugin_page_account_tree_selection_changed_cb), page);
    g_signal_connect (G_OBJECT (tree_view), "button-press-event",
                      G_CALLBACK (gnc_plugin_page_account_tree_button_press_cb), page);
    g_signal_connect (G_OBJECT (tree_view), "row-activated",
                      G_CALLBACK (gnc_plugin_page_account_tree_double_click_cb), page);

    gtk_tree_view_set_headers_visible(tree_view, TRUE);
    gnc_plugin_page_account_tree_selection_changed_cb (NULL, page);
    gtk_widget_show (GTK_WIDGET (tree_view));
    gtk_container_add (GTK_CONTAINER (scrolled_window), GTK_WIDGET(tree_view));

    priv->fd.tree_view = GNC_TREE_VIEW_ACCOUNT(priv->tree_view);
    gnc_tree_view_account_set_filter (
        GNC_TREE_VIEW_ACCOUNT(tree_view),
        gnc_plugin_page_account_tree_filter_accounts, &priv->fd, NULL);

    priv->component_id =
        gnc_register_gui_component(PLUGIN_PAGE_ACCT_TREE_CM_CLASS,
                                   gnc_plugin_page_account_refresh_cb,
                                   gnc_plugin_page_account_tree_close_cb,
                                   page);
    gnc_gui_component_set_session (priv->component_id,
                                   gnc_get_current_session());

    plugin_page->summarybar = gnc_main_window_summary_new();
    gtk_box_pack_start (GTK_BOX (priv->widget), plugin_page->summarybar,
                        FALSE, FALSE, 0);
    gtk_widget_show(plugin_page->summarybar);
    gnc_plugin_page_account_tree_summarybar_position_changed(NULL, NULL, page);
    gnc_prefs_register_cb (GNC_PREFS_GROUP_GENERAL,
                           GNC_PREF_SUMMARYBAR_POSITION_TOP,
                           gnc_plugin_page_account_tree_summarybar_position_changed,
                           page);
    gnc_prefs_register_cb (GNC_PREFS_GROUP_GENERAL,
                           GNC_PREF_SUMMARYBAR_POSITION_BOTTOM,
                           gnc_plugin_page_account_tree_summarybar_position_changed,
                           page);

    g_signal_connect (G_OBJECT(plugin_page), "inserted",
                      G_CALLBACK(gnc_plugin_page_inserted_cb),
                      NULL);

    // Read account filter state information from account section
    gnc_tree_view_account_restore_filter (GNC_TREE_VIEW_ACCOUNT(priv->tree_view), &priv->fd,
       gnc_state_get_current(), gnc_tree_view_get_state_section (GNC_TREE_VIEW(priv->tree_view)));

    LEAVE("widget = %p", priv->widget);
    return priv->widget;
}

static void
gnc_plugin_page_account_tree_destroy_widget (GncPluginPage *plugin_page)
{
    GncPluginPageAccountTree *page;
    GncPluginPageAccountTreePrivate *priv;

    ENTER("page %p", plugin_page);
    page = GNC_PLUGIN_PAGE_ACCOUNT_TREE (plugin_page);
    priv = GNC_PLUGIN_PAGE_ACCOUNT_TREE_GET_PRIVATE(page);

    gnc_prefs_remove_cb_by_func (GNC_PREFS_GROUP_GENERAL,
                                 GNC_PREF_SUMMARYBAR_POSITION_TOP,
                                 gnc_plugin_page_account_tree_summarybar_position_changed,
                                 page);
    gnc_prefs_remove_cb_by_func (GNC_PREFS_GROUP_GENERAL,
                                 GNC_PREF_SUMMARYBAR_POSITION_BOTTOM,
                                 gnc_plugin_page_account_tree_summarybar_position_changed,
                                 page);

    // Save account filter state information to account section
    gnc_tree_view_account_save_filter (GNC_TREE_VIEW_ACCOUNT(priv->tree_view), &priv->fd,
       gnc_state_get_current(), gnc_tree_view_get_state_section (GNC_TREE_VIEW(priv->tree_view)));

    // Destroy the filter override hash table
    g_hash_table_destroy(priv->fd.filter_override);

    // Remove the page_changed signal callback
    gnc_plugin_page_disconnect_page_changed (GNC_PLUGIN_PAGE(plugin_page));

    // Remove the page focus idle function if present
    g_idle_remove_by_data (plugin_page);

    if (priv->widget)
    {
        g_object_unref(G_OBJECT(priv->widget));
        priv->widget = NULL;
    }

    if (priv->component_id)
    {
        gnc_unregister_gui_component(priv->component_id);
        priv->component_id = 0;
    }

    LEAVE("widget destroyed");
}

static void update_inactive_actions(GncPluginPage *plugin_page)
{
    GncPluginPageAccountTreePrivate *priv;
    GtkActionGroup *action_group;
    Account *account = NULL;
    gboolean allow_write = !qof_book_is_readonly(gnc_get_current_book());
    gboolean has_account = FALSE;
    gboolean subaccounts = FALSE;

    g_return_if_fail (plugin_page && GNC_IS_PLUGIN_PAGE(plugin_page));

    priv = GNC_PLUGIN_PAGE_ACCOUNT_TREE_GET_PRIVATE (plugin_page);

    if (gtk_tree_view_get_selection (priv->tree_view))
    {
        account = gnc_tree_view_account_get_selected_account (GNC_TREE_VIEW_ACCOUNT(priv->tree_view));
        has_account = (account != NULL);
        subaccounts = (account && gnc_account_n_children (account) != 0);
        /* Check here for placeholder accounts, etc. */
    }

    /* Get the action group */
    action_group = gnc_plugin_page_get_action_group(plugin_page);
    g_return_if_fail(GTK_IS_ACTION_GROUP (action_group));

    /* Set the action's sensitivity */
    gnc_plugin_update_actions (action_group, readonly_inactive_actions,
                               "sensitive", allow_write);
    gnc_plugin_update_actions (action_group, actions_requiring_account_rw,
                               "sensitive", allow_write && has_account);
    gnc_plugin_update_actions (action_group, actions_requiring_account_always,
                               "sensitive", has_account);
    gnc_plugin_update_actions (action_group, actions_requiring_subaccounts_rw,
                               "sensitive", allow_write && subaccounts);
    g_signal_emit (plugin_page, plugin_page_signals[ACCOUNT_SELECTED], 0, account);
}

/**
 * Called when this page is selected.
 *
 * Update the toolbar button sensitivity. */
static void gnc_plugin_page_account_tree_selected (GObject *object, gpointer user_data)
{
    GncPluginPage *plugin_page = GNC_PLUGIN_PAGE (object);
    g_return_if_fail (GNC_IS_PLUGIN_PAGE (plugin_page));
    update_inactive_actions(plugin_page);
}

/** Save enough information about this account tree page that it can
 *  be recreated next time the user starts gnucash.
 *
 *  @param plugin_page The page to save.
 *
 *  @param key_file A pointer to the GKeyFile data structure where the
 *  page information should be written.
 *
 *  @param group_name The group name to use when saving data. */
static void
gnc_plugin_page_account_tree_save_page (GncPluginPage *plugin_page,
                                        GKeyFile *key_file,
                                        const gchar *group_name)
{
    GncPluginPageAccountTree *account_page;
    GncPluginPageAccountTreePrivate *priv;

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_ACCOUNT_TREE(plugin_page));
    g_return_if_fail (key_file != NULL);
    g_return_if_fail (group_name != NULL);

    ENTER("page %p, key_file %p, group_name %s", plugin_page, key_file,
          group_name);

    account_page = GNC_PLUGIN_PAGE_ACCOUNT_TREE(plugin_page);
    priv = GNC_PLUGIN_PAGE_ACCOUNT_TREE_GET_PRIVATE(account_page);

    gnc_tree_view_account_save(GNC_TREE_VIEW_ACCOUNT(priv->tree_view),
                               &priv->fd, key_file, group_name);
    LEAVE(" ");
}



/** Create a new account tree page based on the information saved
 *  during a previous instantiation of gnucash.
 *
 *  @param window The window where this page should be installed.
 *
 *  @param key_file A pointer to the GKeyFile data structure where the
 *  page information should be read.
 *
 *  @param group_name The group name to use when restoring data. */
static GncPluginPage *
gnc_plugin_page_account_tree_recreate_page (GtkWidget *window,
        GKeyFile *key_file,
        const gchar *group_name)
{
    GncPluginPageAccountTree *account_page;
    GncPluginPageAccountTreePrivate *priv;
    GncPluginPage *page;

    g_return_val_if_fail(key_file, NULL);
    g_return_val_if_fail(group_name, NULL);
    ENTER("key_file %p, group_name %s", key_file, group_name);

    /* Create the new page. */
    page = gnc_plugin_page_account_tree_new();
    account_page = GNC_PLUGIN_PAGE_ACCOUNT_TREE(page);
    priv = GNC_PLUGIN_PAGE_ACCOUNT_TREE_GET_PRIVATE(account_page);

    /* Install it now so we can then manipulate the created widget */
    gnc_main_window_open_page(GNC_MAIN_WINDOW(window), page);

    gnc_tree_view_account_restore(GNC_TREE_VIEW_ACCOUNT(priv->tree_view),
                                  &priv->fd, key_file, group_name);
    LEAVE(" ");
    return page;
}


/* Callbacks */

static void
gnc_plugin_page_account_tree_summarybar_position_changed(gpointer prefs, gchar* pref, gpointer user_data)
{
    GncPluginPage *plugin_page;
    GncPluginPageAccountTree *page;
    GncPluginPageAccountTreePrivate *priv;
    GtkPositionType position = GTK_POS_BOTTOM;

    g_return_if_fail(user_data != NULL);

    plugin_page = GNC_PLUGIN_PAGE(user_data);
    page = GNC_PLUGIN_PAGE_ACCOUNT_TREE (user_data);
    priv = GNC_PLUGIN_PAGE_ACCOUNT_TREE_GET_PRIVATE(page);

    if (gnc_prefs_get_bool (GNC_PREFS_GROUP_GENERAL, GNC_PREF_SUMMARYBAR_POSITION_TOP))
        position = GTK_POS_TOP;

    gtk_box_reorder_child(GTK_BOX(priv->widget),
                          plugin_page->summarybar,
                          (position == GTK_POS_TOP ? 0 : -1) );
}

/** This button press handler calls the common button press handler
 *  for all pages.  The GtkTreeView eats all button presses and
 *  doesn't pass them up the widget tree, even when doesn't do
 *  anything with them.  The only way to get access to the button
 *  presses in an account tree page is here on the tree view widget.
 *  Button presses on all other pages are caught by the signal
 *  registered in gnc-main-window.c. */
static gboolean
gnc_plugin_page_account_tree_button_press_cb (GtkWidget *widget,
        GdkEventButton *event,
        GncPluginPage *page)
{

    g_return_val_if_fail(GNC_IS_PLUGIN_PAGE(page), FALSE);

    ENTER("widget %p, event %p, page %p", widget, event, page);
    gnc_main_window_button_press_cb(widget, event, page);
    LEAVE(" ");

    /* Always return FALSE.  This will let the tree view callback run as
     * well which will select the item under the cursor.  By the time
     * the user sees the menu both callbacks will have run and the menu
     * actions will operate on the just-selected account. */
    return FALSE;
}

static void
gppat_open_account_common (GncPluginPageAccountTree *page,
                           Account *account,
                           gboolean include_subs)
{
    GtkWidget *window;
    GncPluginPage *new_page;

    if (account == NULL)
        return;

    window = GNC_PLUGIN_PAGE (page)->window;
    new_page = gnc_plugin_page_register_new (account, include_subs);
    gnc_main_window_open_page (GNC_MAIN_WINDOW(window), new_page);
}

/*################## Added for Reg2 #################*/
/*        New Register Common                        */
static void
gppat_open2_account_common (GncPluginPageAccountTree *page,
                            Account *account,
                            gboolean include_subs)
{
    GtkWidget *window;
    GncPluginPage *new_page;

    if (account == NULL)
        return;

    window = GNC_PLUGIN_PAGE (page)->window;
    new_page = gnc_plugin_page_register2_new (account, include_subs);
    gnc_main_window_open_page (GNC_MAIN_WINDOW(window), new_page);
}
/*################## Added for Reg2 #################*/

static void
gnc_plugin_page_account_tree_double_click_cb (GtkTreeView *treeview,
        GtkTreePath        *path,
        GtkTreeViewColumn  *col,
        GncPluginPageAccountTree *page)
{
    GtkTreeModel *model;
    GtkTreeIter iter;

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_ACCOUNT_TREE (page));
    g_return_if_fail (treeview);

    model = gtk_tree_view_get_model(treeview);
    if (gtk_tree_model_get_iter(model, &iter, path))
    {
        Account *account = gnc_tree_view_account_get_account_from_path (GNC_TREE_VIEW_ACCOUNT(treeview), path);
        if (xaccAccountGetPlaceholder (account))
        {
            /* This is a placeholder account. Only only show/hide
             * subaccount list if there is one.
             */
            if (gtk_tree_model_iter_has_child(model, &iter))
            {
                /* There are children,
                 * just expand or collapse the row. */
                if (gtk_tree_view_row_expanded(treeview, path))
                    gtk_tree_view_collapse_row(treeview, path);
                else
                    gtk_tree_view_expand_row(treeview, path, FALSE);
            }
        }
        else
        {
            /* No placeholder account, so open its register */
#ifdef REGISTER2_ENABLED
            gppat_open2_account_common (page, account, FALSE);
#else
            gppat_open_account_common (page, account, FALSE);
#endif
        }
    }
}

static void
gnc_plugin_page_account_tree_selection_changed_cb (GtkTreeSelection *selection,
                                                   GncPluginPageAccountTree *page)
{
    GncPluginPage *plugin_page = GNC_PLUGIN_PAGE(page);
    update_inactive_actions (plugin_page);
}


/* Command callbacks */
static void
gnc_plugin_page_account_tree_cmd_new_account (GtkAction *action, GncPluginPageAccountTree *page)
{
    Account *account = gnc_plugin_page_account_tree_get_current_account (page);
    GtkWindow *parent = GTK_WINDOW (gnc_plugin_page_get_window (GNC_PLUGIN_PAGE (page)));
    gnc_ui_new_account_window (parent, gnc_get_current_book(),
                               account);
}

static void
gnc_plugin_page_account_tree_cmd_file_new_hierarchy (GtkAction *action, GncPluginPageAccountTree *page)
{
    gnc_ui_hierarchy_assistant(FALSE);
}

static void
gnc_plugin_page_account_tree_cmd_open_account (GtkAction *action,
        GncPluginPageAccountTree *page)
{
    Account *account;

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_ACCOUNT_TREE (page));
    account = gnc_plugin_page_account_tree_get_current_account (page);
    gppat_open_account_common (page, account, FALSE);
}

static void
gnc_plugin_page_account_tree_cmd_open_subaccounts (GtkAction *action,
        GncPluginPageAccountTree *page)
{
    Account *account;

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_ACCOUNT_TREE (page));
    account = gnc_plugin_page_account_tree_get_current_account (page);
    gppat_open_account_common (page, account, TRUE);
}


/*################## Added for Reg2 #################*/
/* Register Firing - Single Account to start with    */
static void
gnc_plugin_page_account_tree_cmd_open2_account (GtkAction *action,
        GncPluginPageAccountTree *page)
{
    Account *account;

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_ACCOUNT_TREE (page));
    account = gnc_plugin_page_account_tree_get_current_account (page);
    gppat_open2_account_common (page, account, FALSE);
}

static void
gnc_plugin_page_account_tree_cmd_open2_subaccounts (GtkAction *action,
        GncPluginPageAccountTree *page)
{
    Account *account;

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_ACCOUNT_TREE (page));
    account = gnc_plugin_page_account_tree_get_current_account (page);
    gppat_open2_account_common (page, account, TRUE);
}
/*################## Added for Reg2 #################*/

static void
gnc_plugin_page_account_tree_cmd_edit_account (GtkAction *action, GncPluginPageAccountTree *page)
{
    Account *account;
    GtkWindow *parent = GTK_WINDOW (gnc_plugin_page_get_window (GNC_PLUGIN_PAGE (page)));
    ENTER("action %p, page %p", action, page);

    account = gnc_plugin_page_account_tree_get_current_account (page);
    g_return_if_fail (account != NULL);

    gnc_ui_edit_account_window (parent, account);
    LEAVE(" ");
}

static void
gnc_plugin_page_account_tree_cmd_find_account (GtkAction *action, GncPluginPageAccountTree *page)
{
    GtkWidget *window;

    ENTER("action %p, page %p", action, page);

    window = gnc_plugin_page_get_window(GNC_PLUGIN_PAGE(page));

    gnc_find_account_dialog (window, NULL);
    LEAVE(" ");
}

static void
gnc_plugin_page_account_tree_cmd_find_account_popup (GtkAction *action, GncPluginPageAccountTree *page)
{
    Account *account = NULL;
    GtkWidget *window;

    ENTER("action %p, page %p", action, page);

    account = gnc_plugin_page_account_tree_get_current_account (page);

    window = gnc_plugin_page_get_window(GNC_PLUGIN_PAGE(page));

    gnc_find_account_dialog (window, account);
    LEAVE(" ");
}

static void
gnc_plugin_page_account_tree_cmd_cascade_account_properties (GtkAction *action, GncPluginPageAccountTree *page)
{
    Account *account = NULL;
    GtkWidget *window;

    ENTER("action %p, page %p", action, page);

    account = gnc_plugin_page_account_tree_get_current_account (page);

    window = gnc_plugin_page_get_window (GNC_PLUGIN_PAGE(page));

    if (account != NULL)
        gnc_account_cascade_properties_dialog (window, account);

    LEAVE(" ");
}

static gpointer
delete_account_helper (Account * account, gpointer data)
{
    delete_helper_t *helper_res = data;
    GList *splits;

    splits = xaccAccountGetSplitList (account);
    if (splits)
    {
        helper_res->has_splits = TRUE;
        while (splits)
        {
            Split *s = splits->data;
            Transaction *txn = xaccSplitGetParent (s);
            if (xaccTransGetReadOnly (txn))
            {
                helper_res->has_ro_splits = TRUE;
                break;
            }
            splits = splits->next;
        }
    }

    return GINT_TO_POINTER (helper_res->has_splits || helper_res->has_ro_splits);
}

/***
 *** The OK button of a Delete Account dialog is insensitive if
 *** and only if a sensitive account selector contains no accounts.
 ***/
static void
set_ok_sensitivity(GtkWidget *dialog)
{
    GtkWidget *button;
    GtkWidget *sa_mas, *trans_mas;
    gint sa_mas_cnt, trans_mas_cnt;
    gboolean sensitive;

    sa_mas = g_object_get_data(G_OBJECT(dialog), DELETE_DIALOG_SA_MAS);
    trans_mas = g_object_get_data(G_OBJECT(dialog), DELETE_DIALOG_TRANS_MAS);
    sa_mas_cnt = gnc_account_sel_get_num_account(GNC_ACCOUNT_SEL(sa_mas));
    trans_mas_cnt = gnc_account_sel_get_num_account(GNC_ACCOUNT_SEL(trans_mas));

    sensitive = (((NULL == sa_mas) ||
                  (!gtk_widget_is_sensitive(sa_mas) || sa_mas_cnt)) &&
                 ((NULL == trans_mas) ||
                  (!gtk_widget_is_sensitive(trans_mas) || trans_mas_cnt)));

    button = g_object_get_data(G_OBJECT(dialog), DELETE_DIALOG_OK_BUTTON);
    gtk_widget_set_sensitive(button, sensitive);
}

static void
gppat_populate_gas_list(GtkWidget *dialog,
                        GNCAccountSel *gas,
                        gboolean exclude_subaccounts)
{
    Account *account;
    GList *filter;

    g_return_if_fail(GTK_IS_DIALOG(dialog));
    if (gas == NULL)
        return;
    account = g_object_get_data(G_OBJECT(dialog), DELETE_DIALOG_ACCOUNT);
    filter = g_object_get_data(G_OBJECT(dialog), DELETE_DIALOG_FILTER);

    /* Setting the account type filter triggers GNCAccountSel population. */
    gnc_account_sel_set_acct_filters (gas, filter, NULL);

    /* Accounts to be deleted must be removed. */
    gnc_account_sel_purge_account( gas, account, exclude_subaccounts);

    /* The sensitivity of the OK button needs to be reevaluated. */
    set_ok_sensitivity(dialog);
}

void
gppat_populate_trans_mas_list(GtkToggleButton *sa_mrb,
                              GtkWidget *dialog)
{
    GtkWidget *trans_mas;

    g_return_if_fail(GTK_IS_DIALOG(dialog));

    /* Cannot move transactions to subaccounts if they are to be deleted. */
    trans_mas = g_object_get_data(G_OBJECT(dialog), DELETE_DIALOG_TRANS_MAS);
    gppat_populate_gas_list(dialog, GNC_ACCOUNT_SEL(trans_mas), !gtk_toggle_button_get_active(sa_mrb));
}

/* Note that the emitting object (the toggle button) and the signal data
 * are swapped in below callback function. This is a gtkbuilder feature:
 * it swaps if you explicitly set an object for a signal handler in the
 * gtkbuilder xml file.
 */
void
gppat_set_insensitive_iff_rb_active(GtkWidget *widget, GtkToggleButton *b)
{
    GtkWidget *dialog = gtk_widget_get_toplevel(widget);
    GtkWidget *subaccount_trans = g_object_get_data(G_OBJECT(dialog), DELETE_DIALOG_SA_TRANS);
    GtkWidget *sa_mas = g_object_get_data(G_OBJECT(dialog), DELETE_DIALOG_SA_MAS);
    gboolean have_splits = GPOINTER_TO_INT (g_object_get_data(G_OBJECT(dialog), DELETE_DIALOG_SA_SPLITS));

    gtk_widget_set_sensitive(widget, !gtk_toggle_button_get_active(b));

    // If we have subaccount splits & delete subaccounts, enable subaccount_trans
    if ((have_splits) && !gtk_widget_is_sensitive(sa_mas))
        gtk_widget_set_sensitive(subaccount_trans, TRUE);
    else
        gtk_widget_set_sensitive(subaccount_trans, FALSE);

    set_ok_sensitivity(dialog);
}

static GtkWidget *
gppat_setup_account_selector (GtkBuilder *builder, GtkWidget *dialog,
                              const gchar *hbox, const gchar *sel_name)
{
    GtkWidget *selector = gnc_account_sel_new();
    GtkWidget *box = GTK_WIDGET(gtk_builder_get_object (builder, hbox));

    gtk_box_pack_start (GTK_BOX(box), selector, TRUE, TRUE, 0);
    gnc_account_sel_set_hexpand (GNC_ACCOUNT_SEL(selector), TRUE);
    g_object_set_data(G_OBJECT(dialog), sel_name, selector);

    gppat_populate_gas_list(dialog, GNC_ACCOUNT_SEL(selector), TRUE);
    gtk_widget_show_all(box);

    return selector;
}

static int
commodity_mismatch_dialog (const Account* account, GtkWindow* parent)
{
    int response;
    char *account_name = gnc_account_get_full_name (account);
    char* message = g_strdup_printf (
        _("Account %s does not have the same currency as the one you're "
          "moving transactions from.\nAre you sure you want to do this?"),
        account_name);
    GtkWidget* error_dialog =
        gtk_message_dialog_new (parent, GTK_DIALOG_DESTROY_WITH_PARENT,
                                GTK_MESSAGE_ERROR, GTK_BUTTONS_NONE,
                                "%s", message);
    gtk_dialog_add_buttons (GTK_DIALOG(error_dialog),
                            _("_Pick another account"), GTK_RESPONSE_CANCEL,
                            _("_Do it anyway"), GTK_RESPONSE_ACCEPT,
                            (gchar *)NULL);
    response = gtk_dialog_run (GTK_DIALOG (error_dialog));
    gtk_widget_destroy (error_dialog);
    g_free (message);
    return response;
}

typedef struct
{
    Account *new_account;
    Account *old_account;
    GNCAccountSel *selector;
    gboolean match;
    gboolean for_account;
} Adopter;

static void
adopter_set_account_and_match (Adopter* adopter)
{
    if (!(adopter->selector &&
          gtk_widget_is_sensitive (GTK_WIDGET (adopter->selector))))
        return;
    adopter->new_account = gnc_account_sel_get_account(adopter->selector);
/* We care about the commodity only if we're moving transactions. */
    if (!adopter->for_account && adopter->old_account && adopter->new_account)
        adopter->match =
            xaccAccountGetCommodity (adopter->new_account) ==
            xaccAccountGetCommodity (adopter->old_account);
}

static void
adopter_init (Adopter* adopter, GtkWidget *selector, Account* account,
              gboolean for_account)
{
    adopter->selector = GNC_ACCOUNT_SEL (selector);
    adopter->new_account = NULL;
    adopter->old_account = account;
    adopter->match = TRUE;
    adopter->for_account = for_account;
}

static gboolean
adopter_match (Adopter* adopter, GtkWindow *parent)
{
    int result;
    if (adopter->match || adopter->for_account)
        return TRUE;
    result = commodity_mismatch_dialog (adopter->new_account, parent);
    return (result == GTK_RESPONSE_ACCEPT);
}

typedef struct
{
    Adopter trans;
    Adopter subacct;
    Adopter subtrans;
    delete_helper_t delete_res;
} Adopters;

static Account*
account_subaccount (Account* account)
{
    Account* subaccount = NULL;
    GList *subs = gnc_account_get_children (account);
    if (!gnc_list_length_cmp (subs, 1))
        subaccount = subs->data;
    g_list_free (subs);
    return subaccount;
}

static GtkWidget*
account_delete_dialog (Account *account, GtkWindow *parent, Adopters* adopt)
{
    GtkWidget *dialog = NULL;
    GtkWidget *widget = NULL;
    gchar *title = NULL;
    GtkBuilder *builder = gtk_builder_new();
    gchar *acct_name = gnc_account_get_full_name(account);
    GList* splits = xaccAccountGetSplitList(account);
    GList* filter = g_list_prepend(NULL, (gpointer)xaccAccountGetType(account));

    if (!acct_name)
        acct_name = g_strdup (_("(no name)"));

    gnc_builder_add_from_file (builder, "dialog-account.glade", "account_delete_dialog");

    dialog = GTK_WIDGET(gtk_builder_get_object (builder, "account_delete_dialog"));
    gtk_window_set_transient_for(GTK_WINDOW(dialog), parent);

    /* FIXME: Same account type used for subaccount. */
    g_object_set_data(G_OBJECT(dialog), DELETE_DIALOG_FILTER, filter);
    g_object_set_data(G_OBJECT(dialog), DELETE_DIALOG_ACCOUNT, account);
    widget = GTK_WIDGET(gtk_builder_get_object (builder, "header"));
    title = g_strdup_printf(_("Deleting account %s"), acct_name);
    gtk_label_set_text(GTK_LABEL(widget), title);
    g_free(title);
    g_free(acct_name);

    widget = GTK_WIDGET(gtk_builder_get_object (builder, DELETE_DIALOG_OK_BUTTON));
    g_object_set_data(G_OBJECT(dialog), DELETE_DIALOG_OK_BUTTON, widget);

    // Add the account selectors and enable sections as appropriate
    // setup transactions selector
    adopter_init (&adopt->trans,
                  gppat_setup_account_selector (builder, dialog,
                                                "trans_mas_hbox",
                                                DELETE_DIALOG_TRANS_MAS),
                  account, FALSE);

    // Does the selected account have splits
    if (splits)
    {
        delete_helper_t delete_res2 = { FALSE, FALSE };

        delete_account_helper(account, &delete_res2);
        if (delete_res2.has_ro_splits)
        {
            gtk_widget_hide(GTK_WIDGET(gtk_builder_get_object (builder, "trans_rw")));
            widget = GTK_WIDGET(gtk_builder_get_object (builder, "trans_drb"));
            gtk_widget_set_sensitive(widget, FALSE);
        }
        else
            gtk_widget_hide(GTK_WIDGET(gtk_builder_get_object (builder, "trans_ro")));
    }
    else
    {
        gtk_widget_set_sensitive (GTK_WIDGET(gtk_builder_get_object (builder, "transactions")), FALSE);
        gtk_widget_hide(GTK_WIDGET(gtk_builder_get_object (builder, "trans_ro")));
    }

    // setup subaccount account selector
    adopter_init (&adopt->subacct,
                  gppat_setup_account_selector (builder, dialog,
                                                "sa_mas_hbox",
                                                DELETE_DIALOG_SA_MAS),
                  account, TRUE);

    // setup subaccount transaction selector
    adopter_init (&adopt->subtrans,
                  gppat_setup_account_selector (builder, dialog,
                                                "sa_trans_mas_hbox",
                                                DELETE_DIALOG_SA_TRANS_MAS),
                  account_subaccount (account), FALSE);
    g_object_set_data(G_OBJECT(dialog), DELETE_DIALOG_SA_TRANS,
                      GTK_WIDGET(gtk_builder_get_object (builder, "subaccount_trans")));

    if (gnc_account_n_children(account) > 0)
    {
        // Check for RO txns in descendants
        gnc_account_foreach_descendant_until(account, delete_account_helper,
                                             &adopt->delete_res);
        if (adopt->delete_res.has_splits)
        {
            if (adopt->delete_res.has_ro_splits)
            {
                gtk_widget_hide(GTK_WIDGET(gtk_builder_get_object (builder, "sa_trans_rw")));
                widget = GTK_WIDGET(gtk_builder_get_object (builder, "sa_trans_drb"));
                gtk_widget_set_sensitive(widget, FALSE);
            }
            else
                gtk_widget_hide(GTK_WIDGET(gtk_builder_get_object (builder, "sa_trans_ro")));

            g_object_set_data(G_OBJECT(dialog), DELETE_DIALOG_SA_SPLITS, GINT_TO_POINTER(1));
        }
        else
        {
            g_object_set_data(G_OBJECT(dialog), DELETE_DIALOG_SA_SPLITS, GINT_TO_POINTER(0));
            gtk_widget_set_sensitive (GTK_WIDGET(gtk_builder_get_object (builder, "subaccount_trans")), FALSE);
            gtk_widget_hide(GTK_WIDGET(gtk_builder_get_object (builder, "sa_trans_ro")));
        }
    }
    else
    {
        gtk_widget_set_sensitive(GTK_WIDGET(gtk_builder_get_object (builder, "subaccounts")), FALSE);
        gtk_widget_set_sensitive(GTK_WIDGET(gtk_builder_get_object (builder, "subaccount_trans")), FALSE);
        gtk_widget_hide(GTK_WIDGET(gtk_builder_get_object (builder, "sa_trans_ro")));
    }

    /* default to cancel */
    gtk_dialog_set_default_response (GTK_DIALOG(dialog), GTK_RESPONSE_CANCEL);

    gtk_builder_connect_signals(builder, dialog);
    g_object_unref(G_OBJECT(builder));

    return dialog;
}

static void
gnc_plugin_page_account_tree_cmd_delete_account (GtkAction *action, GncPluginPageAccountTree *page)
{
    Account *account = gnc_plugin_page_account_tree_get_current_account (page);
    gchar *acct_name;
    GtkWidget *window;
    Adopters adopt;
    GList* list;
    gint response;
    GList *filter = NULL;
    GtkWidget *dialog = NULL;

    if (account == NULL)
        return;

    memset (&adopt, 0, sizeof (adopt));
    /* If the account has objects referring to it, show the list - the account can't be deleted until these
       references are dealt with. */
    list = qof_instance_get_referring_object_list(QOF_INSTANCE(account));
    if (list != NULL)
    {
#define EXPLANATION _("The list below shows objects which make use of the account which you want to delete.\nBefore you can delete it, you must either delete those objects or else modify them so they make use\nof another account")

        gnc_ui_object_references_show(EXPLANATION, list);
        g_list_free(list);
        return;
    }

    window = gnc_plugin_page_get_window(GNC_PLUGIN_PAGE(page));
    acct_name = gnc_account_get_full_name(account);
    if (!acct_name)
        acct_name = g_strdup (_("(no name)"));

    if (gnc_account_n_children(account) > 1) {
        gchar* message = g_strdup_printf(_("The account \"%s\" has more than one subaccount.\n\nMove the subaccounts or delete them before attempting to delete this account."), acct_name);
        gnc_error_dialog(GTK_WINDOW(window),"%s", message);
        g_free (message);
        g_free(acct_name);
        return;
    }

    // If no transaction or children just delete it.
    if (!(xaccAccountGetSplitList (account) != NULL ||
          gnc_account_n_children (account)))
    {
        do_delete_account (account, NULL, NULL, NULL);
        return;
    }

    dialog = account_delete_dialog (account, GTK_WINDOW (window), &adopt);

    while (TRUE)
    {
        response = gtk_dialog_run(GTK_DIALOG(dialog));

        if (response != GTK_RESPONSE_ACCEPT)
        {
            /* Account deletion is cancelled, so clean up and return. */
            filter = g_object_get_data (G_OBJECT (dialog),
                                        DELETE_DIALOG_FILTER);
            gtk_widget_destroy(dialog);
            g_list_free(filter);
            return;
        }
        adopter_set_account_and_match (&adopt.trans);
        adopter_set_account_and_match (&adopt.subacct);
        adopter_set_account_and_match (&adopt.subtrans);

        if (adopter_match (&adopt.trans, GTK_WINDOW (window)) &&
            adopter_match (&adopt.subacct, GTK_WINDOW (window)) &&
            adopter_match (&adopt.subtrans, GTK_WINDOW (window)))
            break;
    }
        filter = g_object_get_data (G_OBJECT (dialog), DELETE_DIALOG_FILTER);
    gtk_widget_destroy(dialog);
    g_list_free(filter);
    if (confirm_delete_account (action, page, adopt.trans.new_account,
                                adopt.subtrans.new_account,
                                adopt.subacct.new_account,
                                adopt.delete_res) == GTK_RESPONSE_ACCEPT)
    {
        do_delete_account (account, adopt.subacct.new_account,
                           adopt.subtrans.new_account, adopt.trans.new_account);
    }
}

static int
confirm_delete_account (GtkAction *action, GncPluginPageAccountTree *page,
                        Account* ta, Account* sta, Account* saa,
                        delete_helper_t delete_res)
{
    Account *account = gnc_plugin_page_account_tree_get_current_account (page);
    GList* splits = xaccAccountGetSplitList(account);
    GtkWidget* window = gnc_plugin_page_get_window(GNC_PLUGIN_PAGE(page));
    gint response;

    char *lines[6] = {0};
    char *message;
    int i = 0;
    GtkWidget *dialog;
    gchar* acct_name = gnc_account_get_full_name(account);

    lines[i] = g_strdup_printf (_("The account %s will be deleted."),
                                acct_name);
    g_free(acct_name);

    if (splits)
    {
        if (ta)
        {
            char *name = gnc_account_get_full_name(ta);
            lines[++i] = g_strdup_printf (_("All transactions in this account "
                                            "will be moved to the account %s."),
                                          name);
            g_free (name);
        }
        else
        {
            lines[++i] = g_strdup_printf (_("All transactions in this account "
                                            "will be deleted."));
        }
    }
    if (gnc_account_n_children(account))
    {
        if (saa)
        {
            char *name = gnc_account_get_full_name(saa);
            lines[++i] = g_strdup_printf (_("Its sub-account will be "
                                            "moved to the account %s."), name);
            g_free (name);
        }
        else
        {
            lines[++i] = g_strdup_printf (_("Its subaccount will be deleted."));
            if (sta)
            {
                char *name = gnc_account_get_full_name(sta);
                lines[++i] = g_strdup_printf (_("All sub-account transactions "
                                                "will be moved to the "
                                                "account %s."), name);
                g_free (name);
            }
            else if (delete_res.has_splits)
            {
                lines[++i] = g_strdup_printf(_("All sub-account transactions "
                                               "will be deleted."));
            }
        }
    }

    lines[++i] = _("Are you sure you want to do this?");

    message = g_strjoinv(" ", lines);
    for (int j = 0; j < i; ++j) // Don't try to free the last one, it's const.
        g_free (lines[j]);

    dialog =  gtk_message_dialog_new(GTK_WINDOW(window),
                                     GTK_DIALOG_DESTROY_WITH_PARENT,
                                     GTK_MESSAGE_QUESTION,
                                     GTK_BUTTONS_NONE,
                                     "%s", message);
    g_free(message);
    gtk_dialog_add_buttons(GTK_DIALOG(dialog),
                           _("_Cancel"), GTK_RESPONSE_CANCEL,
                           _("_Delete"), GTK_RESPONSE_ACCEPT,
                           (gchar *)NULL);
    gtk_dialog_set_default_response(GTK_DIALOG(dialog), GTK_RESPONSE_CANCEL);
    response = gtk_dialog_run(GTK_DIALOG(dialog));
    gtk_widget_destroy(dialog);
    return response;
}

void do_delete_account (Account* account, Account* saa, Account* sta, Account* ta)
{
    GList *acct_list, *ptr;
    const GncGUID *guid;
    gchar guidstr[GUID_ENCODING_LENGTH+1];

    gnc_set_busy_cursor(NULL, TRUE);
    gnc_suspend_gui_refresh ();

    /* Move subaccounts and transactions if this was requested */
    xaccAccountBeginEdit (account);
    if (saa)
    {
        xaccAccountBeginEdit (saa);
        acct_list = gnc_account_get_children(account);
        for (ptr = acct_list; ptr; ptr = g_list_next(ptr))
            gnc_account_append_child (saa, ptr->data);
        g_list_free(acct_list);
        xaccAccountCommitEdit (saa);
    }
    else if (sta)
    {
        /* Move the splits of its subaccounts, if any. */
        gnc_account_foreach_descendant(account,
                                       (AccountCb)xaccAccountMoveAllSplits,
                                       sta);
    }
    if (ta)
    {
        /* Move the splits of the account to be deleted. */
        xaccAccountMoveAllSplits (account, ta);
    }
    xaccAccountCommitEdit (account);

    /* Drop all references from the state file for
     * any subaccount the account still has
     */
    acct_list = gnc_account_get_children(account);
    for (ptr = acct_list; ptr; ptr = g_list_next(ptr))
    {
        guid = xaccAccountGetGUID (ptr->data);
        guid_to_string_buff (guid, guidstr);
        gnc_state_drop_sections_for (guidstr);
    }
    g_list_free(acct_list);

    /* Drop all references from the state file for this account
     */
    guid = xaccAccountGetGUID (account);
    guid_to_string_buff (guid, guidstr);
    gnc_state_drop_sections_for (guidstr);

    /*
     * Finally, delete the account, any subaccounts it may still
     * have, and any splits it or its subaccounts may still have.
     */
    xaccAccountBeginEdit (account);
    xaccAccountDestroy (account);
    gnc_resume_gui_refresh ();
    gnc_unset_busy_cursor(NULL);
}

static void
gnc_plugin_page_account_tree_cmd_renumber_accounts (GtkAction *action,
        GncPluginPageAccountTree *page)
{
    Account *account;
    GtkWidget *window;

    window = gnc_plugin_page_get_window(GNC_PLUGIN_PAGE(page));
    account = gnc_plugin_page_account_tree_get_current_account(page);
    if (!window || !account)
        return;

    gnc_account_renumber_create_dialog(window, account);
}

static void
gnc_plugin_page_account_tree_cmd_refresh (GtkAction *action,
        GncPluginPageAccountTree *page)
{
    GncPluginPageAccountTreePrivate *priv;

    g_return_if_fail(GNC_IS_PLUGIN_PAGE_ACCOUNT_TREE(page));

    priv = GNC_PLUGIN_PAGE_ACCOUNT_TREE_GET_PRIVATE(page);

    gnc_tree_view_account_clear_model_cache (GNC_TREE_VIEW_ACCOUNT(priv->tree_view));
    gtk_widget_queue_draw (priv->widget);
}

/*********************/

static void
gnc_plugin_page_account_tree_cmd_view_filter_by (GtkAction *action,
        GncPluginPageAccountTree *page)
{
    GncPluginPageAccountTreePrivate *priv;

    g_return_if_fail(GNC_IS_PLUGIN_PAGE_ACCOUNT_TREE(page));
    ENTER("(action %p, page %p)", action, page);

    priv = GNC_PLUGIN_PAGE_ACCOUNT_TREE_GET_PRIVATE(page);
    account_filter_dialog_create(&priv->fd, GNC_PLUGIN_PAGE(page));
    LEAVE(" ");
}

static void
gnc_plugin_page_account_tree_cmd_reconcile (GtkAction *action,
        GncPluginPageAccountTree *page)
{
    GtkWidget *window;
    Account *account;
    RecnWindow *recnData;

    account = gnc_plugin_page_account_tree_get_current_account (page);
    g_return_if_fail (account != NULL);

    window = GNC_PLUGIN_PAGE (page)->window;
    recnData = recnWindow (window, account);
    gnc_ui_reconcile_window_raise (recnData);
}

static void
gnc_plugin_page_account_tree_cmd_autoclear (GtkAction *action,
        GncPluginPageAccountTree *page)
{
    GtkWidget *window;
    Account *account;
    AutoClearWindow *autoClearData;

    account = gnc_plugin_page_account_tree_get_current_account (page);
    g_return_if_fail (account != NULL);

    window = GNC_PLUGIN_PAGE (page)->window;
    autoClearData = autoClearWindow (window, account);
    gnc_ui_autoclear_window_raise (autoClearData);
}

static void
gnc_plugin_page_account_tree_cmd_transfer (GtkAction *action,
        GncPluginPageAccountTree *page)
{
    GtkWidget *window;
    Account *account;

    account = gnc_plugin_page_account_tree_get_current_account (page);
    window = GNC_PLUGIN_PAGE (page)->window;
    gnc_xfer_dialog (window, account);
}

static void
gnc_plugin_page_account_tree_cmd_stock_split (GtkAction *action,
        GncPluginPageAccountTree *page)
{
    GtkWidget *window;
    Account *account;

    account = gnc_plugin_page_account_tree_get_current_account (page);
    window = GNC_PLUGIN_PAGE (page)->window;
    gnc_stock_split_dialog (window, account);
}

static void
gnc_plugin_page_account_tree_cmd_edit_tax_options (GtkAction *action,
        GncPluginPageAccountTree *page)
{
    GtkWidget *window;
    Account *account;

    account = gnc_plugin_page_account_tree_get_current_account (page);
    window = GNC_PLUGIN_PAGE (page)->window;
    gnc_tax_info_dialog (window, account);
}

static void
gnc_plugin_page_account_tree_cmd_lots (GtkAction *action,
                                       GncPluginPageAccountTree *page)
{
    Account *account = gnc_plugin_page_account_tree_get_current_account (page);
    GtkWidget *window = GNC_PLUGIN_PAGE (page)->window;
    gnc_lot_viewer_dialog (GTK_WINDOW(window), account);
}

static gboolean scrub_kp_handler (GtkWidget *widget, GdkEventKey *event, gpointer data)
{
    if (event->length == 0) return FALSE;

    switch (event->keyval)
    {
    case GDK_KEY_Escape:
        {
            gboolean abort_scrub = gnc_verify_dialog (GTK_WINDOW(widget), FALSE,
                 _("'Check & Repair' is currently running, do you want to abort it?"));

            if (abort_scrub)
                gnc_set_abort_scrub (TRUE);

            return TRUE;
        }
    default:
        break;
    }
    return FALSE;
}

static void
gnc_plugin_page_account_tree_cmd_scrub (GtkAction *action, GncPluginPageAccountTree *page)
{
    Account *account = gnc_plugin_page_account_tree_get_current_account (page);
    GncWindow *window;
    gulong scrub_kp_handler_ID;

    g_return_if_fail (account != NULL);

    prepare_scrubbing ();

    window = GNC_WINDOW(GNC_PLUGIN_PAGE (page)->window);
    scrub_kp_handler_ID = g_signal_connect (G_OBJECT(window), "key-press-event",
                                            G_CALLBACK(scrub_kp_handler), NULL);
    gnc_window_set_progressbar_window (window);

    xaccAccountScrubOrphans (account, gnc_window_show_progress);
    xaccAccountScrubImbalance (account, gnc_window_show_progress);

    // XXX: Lots/capital gains scrubbing is disabled
    if (g_getenv("GNC_AUTO_SCRUB_LOTS") != NULL)
        xaccAccountScrubLots(account);

    gncScrubBusinessAccount(account, gnc_window_show_progress);

    finish_scrubbing (window, scrub_kp_handler_ID);
}

static void
gnc_plugin_page_account_tree_cmd_scrub_sub (GtkAction *action, GncPluginPageAccountTree *page)
{
    Account *account = gnc_plugin_page_account_tree_get_current_account (page);
    GncWindow *window;
    gulong scrub_kp_handler_ID;

    g_return_if_fail (account != NULL);

    prepare_scrubbing ();

    window = GNC_WINDOW(GNC_PLUGIN_PAGE (page)->window);
    scrub_kp_handler_ID = g_signal_connect (G_OBJECT(window), "key-press-event",
                                            G_CALLBACK(scrub_kp_handler), NULL);
    gnc_window_set_progressbar_window (window);

    xaccAccountTreeScrubOrphans (account, gnc_window_show_progress);
    xaccAccountTreeScrubImbalance (account, gnc_window_show_progress);

    // XXX: Lots/capital gains scrubbing is disabled
    if (g_getenv("GNC_AUTO_SCRUB_LOTS") != NULL)
        xaccAccountTreeScrubLots(account);

    gncScrubBusinessAccountTree(account, gnc_window_show_progress);

    finish_scrubbing (window, scrub_kp_handler_ID);
}

static void
gnc_plugin_page_account_tree_cmd_scrub_all (GtkAction *action, GncPluginPageAccountTree *page)
{
    Account *root = gnc_get_current_root_account ();
    GncWindow *window;
    gulong scrub_kp_handler_ID;

    prepare_scrubbing ();

    window = GNC_WINDOW(GNC_PLUGIN_PAGE (page)->window);
    scrub_kp_handler_ID = g_signal_connect (G_OBJECT(window), "key-press-event",
                                            G_CALLBACK(scrub_kp_handler), NULL);
    gnc_window_set_progressbar_window (window);

    xaccAccountTreeScrubOrphans (root, gnc_window_show_progress);
    xaccAccountTreeScrubImbalance (root, gnc_window_show_progress);
    // XXX: Lots/capital gains scrubbing is disabled
    if (g_getenv("GNC_AUTO_SCRUB_LOTS") != NULL)
        xaccAccountTreeScrubLots(root);

    gncScrubBusinessAccountTree(root, gnc_window_show_progress);

    finish_scrubbing (window, scrub_kp_handler_ID);
}

/** @} */
/** @} */
