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

#include <algorithm>

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include "gnc-plugin-page-account-tree.h"
#include "gnc-plugin-page-register.h"

#include "Account.hpp"
#include "Scrub.h"
#include "Scrub3.h"
#include "ScrubBusiness.h"
#include "Transaction.h"
#include "dialog-account.h"
#include "dialog-transfer.h"
#include "dialog-utils.h"
#include "assistant-hierarchy.h"
#include "assistant-stock-transaction.h"
#include "gnc-account-sel.h"
#include "gnc-component-manager.h"
#include "gnc-engine.h"
#include "gnc-gnome-utils.h"
#include "gnc-gobject-utils.h"
#include "gnc-icons.h"
#include "gnc-plugin-account-tree.h"
#include "gnc-prefs.h"
#include "gnc-scrub-job-runner.h"
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
#include <gnc-string-utils.h>

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
    GncTreeViewAccount *tree_view;
    gint         component_id;
    AccountFilterDialog fd;
    GncScrubContext *scrub_context;
} GncPluginPageAccountTreePrivate;

#define GNC_PLUGIN_PAGE_ACCOUNT_TREE_GET_PRIVATE(o)  \
     ((GncPluginPageAccountTreePrivate*)gnc_plugin_page_account_tree_get_instance_private((GncPluginPageAccountTree*)o))

/************************************************************
 *                        Prototypes                        *
 ************************************************************/
/* Plugin Actions */
static void gnc_plugin_page_account_tree_finalize (GObject *object);
static void gnc_plugin_page_account_tree_selected (GObject *object, gpointer user_data);

static gboolean gnc_plugin_page_account_tree_focus_widget (GncPluginPage *plugin_page);
static GtkWidget *gnc_plugin_page_account_tree_create_widget (GncPluginPage *plugin_page);
static void gnc_plugin_page_account_tree_destroy_widget (GncPluginPage *plugin_page);
static void gnc_plugin_page_account_tree_save_page (GncPluginPage *plugin_page, GKeyFile *file, const gchar *group);
static GncPluginPage *gnc_plugin_page_account_tree_recreate_page (GtkWidget *window, GKeyFile *file, const gchar *group);

/* Callbacks */
static void gnc_plugin_page_account_tree_summarybar_position_changed(gpointer prefs, gchar* pref, gpointer user_data);
static gboolean gnc_plugin_page_account_tree_button_press_cb (GtkGestureClick *gesture,
                                                              int n_press,
                                                              double x,
                                                              double y,
                                                              gpointer user_data);
static void gnc_plugin_page_account_tree_double_click_cb (GncTreeViewAccount *tree_view, Account *account, GncPluginPageAccountTree *page);

static void gnc_plugin_page_account_tree_selection_changed_cb (GtkSelectionModel *selection, guint position, guint n_items, GncPluginPageAccountTree *page);
static void accounting_period_changed_cb(gpointer prefs, gchar *pref, gpointer user_data);

extern "C" {
void gppat_populate_trans_mas_list(GtkCheckButton *sa_mrb, GtkWidget *dialog);
void gppat_set_insensitive_iff_rb_active(GtkWidget *widget, GtkCheckButton *b);
}

/* Command callbacks */
static void gnc_plugin_page_account_tree_cmd_new_account (GSimpleAction *simple, GVariant *paramter, gpointer user_data);
static void gnc_plugin_page_account_tree_cmd_file_new_hierarchy (GSimpleAction *simple, GVariant *paramter, gpointer user_data);
static void gnc_plugin_page_account_tree_cmd_open_account (GSimpleAction *simple, GVariant *paramter, gpointer user_data);
static void gnc_plugin_page_account_tree_cmd_open_subaccounts (GSimpleAction *simple, GVariant *paramter, gpointer user_data);
static void gnc_plugin_page_account_tree_cmd_edit_account (GSimpleAction *simple, GVariant *paramter, gpointer user_data);
static void gnc_plugin_page_account_tree_cmd_find_account (GSimpleAction *simple, GVariant *paramter, gpointer user_data);
static void gnc_plugin_page_account_tree_cmd_find_account_popup (GSimpleAction *simple, GVariant *paramter, gpointer user_data);
static void gnc_plugin_page_account_tree_cmd_delete_account (GSimpleAction *simple, GVariant *paramter, gpointer user_data);
static void gnc_plugin_page_account_tree_cmd_renumber_accounts (GSimpleAction *simple, GVariant *paramter, gpointer user_data);
static void gnc_plugin_page_account_tree_cmd_view_filter_by (GSimpleAction *simple, GVariant *paramter, gpointer user_data);
static void gnc_plugin_page_account_tree_cmd_reconcile (GSimpleAction *simple, GVariant *paramter, gpointer user_data);
static void gnc_plugin_page_account_tree_cmd_refresh (GSimpleAction *simple, GVariant *paramter, gpointer user_data);
static void gnc_plugin_page_account_tree_cmd_autoclear (GSimpleAction *simple, GVariant *paramter, gpointer user_data);
static void gnc_plugin_page_account_tree_cmd_transfer (GSimpleAction *simple, GVariant *paramter, gpointer user_data);
static void gnc_plugin_page_account_tree_cmd_stock_split (GSimpleAction *simple, GVariant *paramter, gpointer user_data);
static void gnc_plugin_page_account_tree_cmd_stock_assistant (GSimpleAction *simple, GVariant *paramter, gpointer user_data);
static void gnc_plugin_page_account_tree_cmd_edit_tax_options (GSimpleAction *simple, GVariant *paramter, gpointer user_data);
static void gnc_plugin_page_account_tree_cmd_lots (GSimpleAction *simple, GVariant *paramter, gpointer user_data);
static void gnc_plugin_page_account_tree_cmd_scrub (GSimpleAction *simple, GVariant *paramter, gpointer user_data);
static void gnc_plugin_page_account_tree_cmd_scrub_sub (GSimpleAction *simple, GVariant *paramter, gpointer user_data);
static void gnc_plugin_page_account_tree_cmd_scrub_all (GSimpleAction *simple, GVariant *paramter, gpointer user_data);
static void gnc_plugin_page_account_tree_cmd_cascade_account_properties (GSimpleAction *simple, GVariant *paramter, gpointer user_data);

/* Account Deletion Actions. */

static void  do_delete_account (Account* account, Account* saa, Account* sta,
                                Account* ta);



static guint plugin_page_signals[LAST_SIGNAL] = { 0 };


static GActionEntry gnc_plugin_page_account_tree_actions [] =
{
    { "FileNewAccountAction", gnc_plugin_page_account_tree_cmd_new_account, NULL, NULL, NULL },
    { "FileAddAccountHierarchyAssistantAction", gnc_plugin_page_account_tree_cmd_file_new_hierarchy, NULL, NULL, NULL },
    { "EditOpenAccountAction", gnc_plugin_page_account_tree_cmd_open_account, NULL, NULL, NULL },
    { "EditOpenSubaccountsAction", gnc_plugin_page_account_tree_cmd_open_subaccounts, NULL, NULL, NULL },
    { "EditEditAccountAction", gnc_plugin_page_account_tree_cmd_edit_account, NULL, NULL, NULL },
    { "EditDeleteAccountAction", gnc_plugin_page_account_tree_cmd_delete_account, NULL, NULL, NULL },
    { "EditCascadeAccountAction", gnc_plugin_page_account_tree_cmd_cascade_account_properties, NULL, NULL, NULL },
    { "EditFindAccountAction", gnc_plugin_page_account_tree_cmd_find_account, NULL, NULL, NULL },
    { "EditFindAccountPopupAction", gnc_plugin_page_account_tree_cmd_find_account_popup, NULL, NULL, NULL },
    { "EditRenumberSubaccountsAction", gnc_plugin_page_account_tree_cmd_renumber_accounts, NULL, NULL, NULL },
    { "EditTaxOptionsAction", gnc_plugin_page_account_tree_cmd_edit_tax_options, NULL, NULL, NULL },
    { "ViewFilterByAction", gnc_plugin_page_account_tree_cmd_view_filter_by, NULL, NULL, NULL },
    { "ViewRefreshAction", gnc_plugin_page_account_tree_cmd_refresh, NULL, NULL, NULL },
    { "ActionsReconcileAction", gnc_plugin_page_account_tree_cmd_reconcile, NULL, NULL, NULL },
    { "ActionsAutoClearAction", gnc_plugin_page_account_tree_cmd_autoclear, NULL, NULL, NULL },
    { "ActionsTransferAction", gnc_plugin_page_account_tree_cmd_transfer, NULL, NULL, NULL },
    { "ActionsStockSplitAction", gnc_plugin_page_account_tree_cmd_stock_split, NULL, NULL, NULL },
    { "ActionsStockAssistantAction", gnc_plugin_page_account_tree_cmd_stock_assistant, NULL, NULL, NULL },
    { "ActionsLotsAction", gnc_plugin_page_account_tree_cmd_lots, NULL, NULL, NULL },
    { "ScrubAction", gnc_plugin_page_account_tree_cmd_scrub, NULL, NULL, NULL },
    { "ScrubSubAction", gnc_plugin_page_account_tree_cmd_scrub_sub, NULL, NULL, NULL },
    { "ScrubAllAction", gnc_plugin_page_account_tree_cmd_scrub_all, NULL, NULL, NULL },
};
/** The number of actions provided by this plugin. */
static guint gnc_plugin_page_account_tree_n_actions = G_N_ELEMENTS(gnc_plugin_page_account_tree_actions);

/** The default menu items that need to be add to the menu */
static const gchar *gnc_plugin_load_ui_items [] =
{
    "FilePlaceholder3",
    "EditPlaceholder1",
    "EditPlaceholder2",
    "EditPlaceholder3",
    "EditPlaceholder5",
    "ViewPlaceholder1",
    "ViewPlaceholder4",
    "ActionsPlaceholder4",
    "ActionsPlaceholder5",
    "ActionsPlaceholder6",
    NULL,
};




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
    "EditOpenAccountAction",
    "EditOpenSubaccountsAction",
    "ActionsLotsAction",
    NULL
};

static const gchar* actions_requiring_priced_account[] =
{
    "ActionsStockAssistantAction",
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
static GncToolBarShortNames toolbar_labels[] =
{
    { "EditOpenAccountAction",          N_("Open") },
    { "EditEditAccountAction",          N_("Edit") },
    { "FileNewAccountAction",           N_("New") },
    { "EditDeleteAccountAction",        N_("Delete") },
    { NULL, NULL },
};

GncPluginPage *
gnc_plugin_page_account_tree_new (void)
{
    ENTER(" ");
    auto plugin_page = GNC_PLUGIN_PAGE_ACCOUNT_TREE
        (g_object_new (GNC_TYPE_PLUGIN_PAGE_ACCOUNT_TREE, nullptr));

    LEAVE("new account tree page %p", plugin_page);
    return GNC_PLUGIN_PAGE (plugin_page);
}

G_DEFINE_TYPE_WITH_PRIVATE(GncPluginPageAccountTree, gnc_plugin_page_account_tree, GNC_TYPE_PLUGIN_PAGE)

static GncScrubContext *
prepare_scrubbing (GncPluginPageAccountTree *page, QofBook *book)
{
    auto priv = GNC_PLUGIN_PAGE_ACCOUNT_TREE_GET_PRIVATE (page);
    if (priv->scrub_context)
        return nullptr;

    auto context = gnc_scrub_context_begin (book);
    if (!context)
        return nullptr;

    priv->scrub_context = gnc_scrub_context_ref (context);
    gnc_suspend_gui_refresh ();
    return context;
}

static void
finish_scrubbing (GncPluginPageAccountTree *page, GncWindow *window,
                  GtkEventController *controller, gulong handler_id,
                  GncScrubContext *context)
{
    auto priv = GNC_PLUGIN_PAGE_ACCOUNT_TREE_GET_PRIVATE (page);
    g_signal_handler_disconnect (controller, handler_id);
    gtk_widget_remove_controller (GTK_WIDGET (window), controller);
    if (priv->scrub_context == context)
    {
        gnc_scrub_context_unref (priv->scrub_context);
        priv->scrub_context = nullptr;
    }
    gnc_scrub_context_end (context);
    gnc_scrub_context_unref (context);
    gnc_resume_gui_refresh ();
}

static void
start_lots_scrub_runner (Account *account, gboolean descendants,
                         GObject *owner)
{
    auto runner = gnc_scrub_job_runner_start_lots (
        account, descendants, owner, nullptr, 1, nullptr, nullptr, nullptr,
        nullptr);
    if (runner)
        gnc_scrub_job_runner_unref (runner);
}

static const char*
check_repair_abort_YN = N_("'Check & Repair' is currently running, do you want to abort it?");

struct AccountFinishPendingRequest
{
    gatomicrefcount ref_count;
    GncPluginPage *page;
    GWeakRef parent;
    GCancellable *cancellable;
    gulong parent_destroy_handler;
    GncScrubContext *scrub_context;
    GncPluginPagePendingCallback callback;
    gpointer user_data;
    gboolean completed;
};

static AccountFinishPendingRequest*
account_finish_pending_request_ref (AccountFinishPendingRequest *request)
{
    g_atomic_ref_count_inc (&request->ref_count);
    return request;
}



static void
account_finish_pending_request_free (AccountFinishPendingRequest *request)
{
    auto parent = GTK_WIDGET (g_weak_ref_get (&request->parent));

    if (parent && request->parent_destroy_handler)
        g_signal_handler_disconnect (parent, request->parent_destroy_handler);
    g_clear_object (&parent);
    g_weak_ref_clear (&request->parent);
    g_clear_object (&request->page);
    g_clear_object (&request->cancellable);
    gnc_scrub_context_unref (request->scrub_context);
    g_free (request);
}

static void
account_finish_pending_request_unref (AccountFinishPendingRequest *request)
{
    if (request && g_atomic_ref_count_dec (&request->ref_count))
        account_finish_pending_request_free (request);
}

static void
account_finish_pending_request_complete (AccountFinishPendingRequest *request,
                                         gboolean accepted)
{
    if (!request || request->completed)
        return;

    request->completed = TRUE;
    if (request->callback)
        request->callback (request->page, accepted, request->user_data);
    account_finish_pending_request_unref (request);
}

static void
account_finish_pending_parent_destroyed (GtkWidget *parent, gpointer user_data)
{
    auto request = static_cast<AccountFinishPendingRequest *> (user_data);

    (void)parent;
    request->parent_destroy_handler = 0;
    g_cancellable_cancel (request->cancellable);
    account_finish_pending_request_complete (request, FALSE);
}

static void
account_finish_pending_alert_finished (GObject *source_object, GAsyncResult *result,
                                       gpointer user_data)
{
    auto request = static_cast<AccountFinishPendingRequest *> (user_data);
    GError *error = nullptr;
    auto response = gtk_alert_dialog_choose_finish (GTK_ALERT_DIALOG (source_object), result,
                                                    &error);

    if (error)
    {
        g_clear_error (&error);
        account_finish_pending_request_complete (request, FALSE);
    }
    else
    {
        if (response == 1)
        {
            gnc_scrub_context_cancel (request->scrub_context);
            account_finish_pending_request_complete (request, TRUE);
        }
        else
            account_finish_pending_request_complete (request, FALSE);
    }
    account_finish_pending_request_unref (request);
}

static void
gnc_plugin_page_account_finish_pending_async (GncPluginPage *page,
                                              GCancellable *cancellable,
                                              GncPluginPagePendingCallback callback,
                                              gpointer user_data)
{
    auto request = g_new0 (AccountFinishPendingRequest, 1);
    auto parent = gnc_plugin_page_get_window (page);
    auto priv = GNC_PLUGIN_PAGE_ACCOUNT_TREE_GET_PRIVATE (page);

    g_atomic_ref_count_init (&request->ref_count);
    request->page = GNC_PLUGIN_PAGE (g_object_ref (page));
    g_weak_ref_init (&request->parent, parent);
    request->cancellable = cancellable ? G_CANCELLABLE (g_object_ref (cancellable)) :
                                         g_cancellable_new ();
    request->scrub_context = gnc_scrub_context_ref (priv->scrub_context);
    request->callback = callback;
    request->user_data = user_data;
    if (parent)
        request->parent_destroy_handler = g_signal_connect
            (parent, "destroy", G_CALLBACK (account_finish_pending_parent_destroyed), request);

    if (!gnc_scrub_context_is_active (request->scrub_context))
    {
        account_finish_pending_request_complete (request, TRUE);
        return;
    }
    if (!parent || !GTK_IS_WINDOW (parent))
    {
        account_finish_pending_request_complete (request, FALSE);
        return;
    }

    const char *buttons[] = { _("Cancel"), _("Abort"), nullptr };
    auto alert = gtk_alert_dialog_new ("%s", _(check_repair_abort_YN));

    gtk_alert_dialog_set_buttons (alert, buttons);
    gtk_alert_dialog_set_cancel_button (alert, 0);
    gtk_alert_dialog_choose (alert, GTK_WINDOW (parent), request->cancellable,
                             account_finish_pending_alert_finished,
                             account_finish_pending_request_ref (request));
    g_object_unref (alert);
}

static void
gnc_plugin_page_account_tree_class_init (GncPluginPageAccountTreeClass *klass)
{
    GObjectClass *object_class = G_OBJECT_CLASS (klass);
    GncPluginPageClass *gnc_plugin_class = GNC_PLUGIN_PAGE_CLASS(klass);

    object_class->finalize = gnc_plugin_page_account_tree_finalize;

    gnc_plugin_class->tab_icon        = GNC_ICON_ACCOUNT;
    gnc_plugin_class->plugin_name     = GNC_PLUGIN_PAGE_ACCOUNT_TREE_NAME;
    gnc_plugin_class->create_widget   = gnc_plugin_page_account_tree_create_widget;
    gnc_plugin_class->destroy_widget  = gnc_plugin_page_account_tree_destroy_widget;
    gnc_plugin_class->save_page       = gnc_plugin_page_account_tree_save_page;
    gnc_plugin_class->recreate_page   = gnc_plugin_page_account_tree_recreate_page;
    gnc_plugin_class->focus_page_function = gnc_plugin_page_account_tree_focus_widget;
    gnc_plugin_class->finish_pending_async = gnc_plugin_page_account_finish_pending_async;

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
    GSimpleActionGroup *simple_action_group = NULL;
    GncPluginPageAccountTreePrivate *priv;
    GncPluginPage *parent;
    const GList *page_list;

    ENTER("page %p", plugin_page);
    priv = GNC_PLUGIN_PAGE_ACCOUNT_TREE_GET_PRIVATE(plugin_page);

    /* Init parent declared variables */
    parent = GNC_PLUGIN_PAGE(plugin_page);
    g_object_set (G_OBJECT(plugin_page),
                  "page-name",      _("Accounts"),
                  "ui-description", "gnc-plugin-page-account-tree.ui",
                  NULL);
    g_signal_connect (G_OBJECT (plugin_page), "selected",
                      G_CALLBACK (gnc_plugin_page_account_tree_selected), plugin_page);

    /* change me when the system supports multiple books */
    gnc_plugin_page_add_book (parent, gnc_get_current_book());

    /* Is this the first accounts page? */
    page_list =
        gnc_gobject_tracking_get_list (GNC_PLUGIN_PAGE_ACCOUNT_TREE_NAME);
    if (!page_list || plugin_page == page_list->data)
    {
        g_object_set_data (G_OBJECT(plugin_page), PLUGIN_PAGE_IMMUTABLE,
                           GINT_TO_POINTER(1));
    }

    /* Create menu and toolbar information */
    simple_action_group = gnc_plugin_page_create_action_group (parent, "GncPluginPageAccountTreeActions");
    g_action_map_add_action_entries (G_ACTION_MAP(simple_action_group),
                                     gnc_plugin_page_account_tree_actions,
                                     gnc_plugin_page_account_tree_n_actions,
                                     plugin_page);

    /* Visible types */
    priv->fd.visible_types = -1; /* Start with all types */
    priv->fd.show_hidden = FALSE;
    priv->fd.show_unused = TRUE;
    priv->fd.show_zero_total = TRUE;
    priv->fd.filter_override = g_hash_table_new (g_direct_hash, g_direct_equal);

    LEAVE("page %p, priv %p, action group %p",
          plugin_page, priv, simple_action_group);
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

    if (priv->scrub_context)
    {
        gnc_scrub_context_cancel (priv->scrub_context);
        gnc_scrub_context_unref (priv->scrub_context);
        priv->scrub_context = nullptr;
    }
    G_OBJECT_CLASS (gnc_plugin_page_account_tree_parent_class)->finalize (object);
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
        gnc_tree_view_account_refilter (priv->tree_view);
        gnc_tree_view_account_set_selected_account (priv->tree_view, account);
    }
}

Account *
gnc_plugin_page_account_tree_get_current_account (GncPluginPageAccountTree *page)
{
    GncPluginPageAccountTreePrivate *priv;
    Account *account;

    priv = GNC_PLUGIN_PAGE_ACCOUNT_TREE_GET_PRIVATE(page);
    ENTER("page %p (tree view %p)", page, priv->tree_view);
    account = gnc_tree_view_account_get_selected_account (priv->tree_view);
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
        GtkColumnView *view = gnc_tree_view_account_get_column_view (priv->tree_view);

        /* Disable the Transaction Menu */
        GAction *action = gnc_main_window_find_action (GNC_MAIN_WINDOW(account_plugin_page->window), "TransactionAction");
        g_simple_action_set_enabled (G_SIMPLE_ACTION(action), FALSE);
        /* Disable the Schedule menu */
        action = gnc_main_window_find_action (GNC_MAIN_WINDOW(account_plugin_page->window), "ScheduledAction");
        g_simple_action_set_enabled (G_SIMPLE_ACTION(action), FALSE);

        gnc_main_window_update_menu_and_toolbar (GNC_MAIN_WINDOW(account_plugin_page->window),
                                                 account_plugin_page,
                                                 gnc_plugin_load_ui_items);

        // setup any short toolbar names
        gnc_main_window_init_short_names (GNC_MAIN_WINDOW(account_plugin_page->window), toolbar_labels);

        /* Disable the FilePrintAction */
        action = gnc_main_window_find_action (GNC_MAIN_WINDOW(account_plugin_page->window), "FilePrintAction");
        g_simple_action_set_enabled (G_SIMPLE_ACTION(action), FALSE);

        if (!gtk_widget_is_focus (GTK_WIDGET(view)))
            gtk_widget_grab_focus (GTK_WIDGET(view));
    }
    return FALSE;
}

/* Virtual Functions */

static void
gnc_plugin_page_account_refresh_cb (GHashTable *changes, gpointer user_data)
{
    /* We're only looking for forced updates here. */
      if (!changes)
      gnc_plugin_page_account_tree_cmd_refresh(NULL, NULL, user_data);
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
    GAction *action = gnc_main_window_find_action_in_group (GNC_MAIN_WINDOW(plugin_page->window),
                                                            "GncPluginPageAccountTreeActions",
                                                            "EditDeleteAccountAction");
    if (action != NULL)
        g_simple_action_set_enabled (G_SIMPLE_ACTION(action), FALSE);
}

static void
gnc_plugin_page_account_editing_finished_cb (gpointer various, GncPluginPageRegister *page)
{
    GncPluginPage *plugin_page = GNC_PLUGIN_PAGE(page);
    GAction *action = gnc_main_window_find_action_in_group (GNC_MAIN_WINDOW(plugin_page->window),
                                                            "GncPluginPageAccountTreeActions",
                                                            "EditDeleteAccountAction");
    if (action != NULL)
        g_simple_action_set_enabled (G_SIMPLE_ACTION(action), TRUE);
}

static GtkWidget *
gnc_plugin_page_account_tree_create_widget (GncPluginPage *plugin_page)
{
    GncPluginPageAccountTree *page;
    GncPluginPageAccountTreePrivate *priv;
    GtkSelectionModel *selection;
    GncTreeViewAccount *tree_view;
    GtkWidget *scrolled_window;

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
    gtk_widget_set_visible (GTK_WIDGET(priv->widget), true);

    // Set the name for this widget so it can be easily manipulated with css
    gtk_widget_set_name (GTK_WIDGET(priv->widget), "gnc-id-account-page");

    scrolled_window = gtk_scrolled_window_new ();
    gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolled_window),
                                    GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
    gtk_widget_set_visible (GTK_WIDGET(scrolled_window), true);
    gtk_box_append (GTK_BOX(priv->widget), GTK_WIDGET(scrolled_window));

    gtk_widget_set_vexpand (GTK_WIDGET(scrolled_window), true);
    gtk_widget_set_hexpand (GTK_WIDGET(scrolled_window), true);

    tree_view = GNC_TREE_VIEW_ACCOUNT (gnc_tree_view_account_new (FALSE));
    gnc_tree_view_account_set_column_visible (tree_view, "description", TRUE);
    gnc_tree_view_account_set_column_visible (tree_view, "total", TRUE);
    gnc_tree_view_account_set_state_section (tree_view, STATE_SECTION);
    gnc_tree_view_account_set_headers_visible (tree_view, TRUE);

    /* No name handler; then the user can't click on the name of the
       account to open its register. */
    gnc_tree_view_account_set_code_edited(tree_view,
                                          gnc_tree_view_account_code_edited_cb);
    gnc_tree_view_account_set_description_edited(tree_view,
            gnc_tree_view_account_description_edited_cb);
    gnc_tree_view_account_set_notes_edited(tree_view,
                                           gnc_tree_view_account_notes_edited_cb);

    // Setup some callbacks so menu actions can be disabled/enabled
    gnc_tree_view_account_set_editing_started_cb(tree_view,
        (GFunc)gnc_plugin_page_account_editing_started_cd, page);
    gnc_tree_view_account_set_editing_finished_cb(tree_view,
        (GFunc)gnc_plugin_page_account_editing_finished_cb, page);

    priv->tree_view = tree_view;
    selection = gnc_tree_view_account_get_selection_model (tree_view);
    g_signal_connect (selection, "selection-changed",
                      G_CALLBACK (gnc_plugin_page_account_tree_selection_changed_cb), page);

    GtkGesture *event_gesture = gtk_gesture_click_new ();
    gtk_widget_add_controller (GTK_WIDGET(gnc_tree_view_account_get_column_view (tree_view)), GTK_EVENT_CONTROLLER(event_gesture));
    gtk_gesture_single_set_button (GTK_GESTURE_SINGLE(event_gesture), GDK_BUTTON_SECONDARY);
    g_signal_connect (G_OBJECT(event_gesture), "pressed",
                      G_CALLBACK(gnc_plugin_page_account_tree_button_press_cb), page);

    g_signal_connect (tree_view, "account-activated",
                      G_CALLBACK (gnc_plugin_page_account_tree_double_click_cb), page);
    gnc_plugin_page_account_tree_selection_changed_cb (NULL, 0, 0, page);
    gtk_widget_set_visible (GTK_WIDGET(tree_view), true);
    gtk_scrolled_window_set_child (GTK_SCROLLED_WINDOW(scrolled_window),
                                   GTK_WIDGET(tree_view));

    gtk_widget_set_vexpand (GTK_WIDGET(tree_view), true);
    gtk_widget_set_hexpand (GTK_WIDGET(tree_view), true);

    priv->fd.tree_view = priv->tree_view;
    gnc_tree_view_account_set_filter (
        tree_view,
        gnc_plugin_page_account_tree_filter_accounts, &priv->fd, NULL);

    priv->component_id =
        gnc_register_gui_component(PLUGIN_PAGE_ACCT_TREE_CM_CLASS,
                                   gnc_plugin_page_account_refresh_cb,
                                   gnc_plugin_page_account_tree_close_cb,
                                   page);
    gnc_gui_component_set_session (priv->component_id,
                                   gnc_get_current_session());

    plugin_page->summarybar = gnc_main_window_summary_new();
    gtk_box_append (GTK_BOX(priv->widget), GTK_WIDGET(plugin_page->summarybar));
    gtk_widget_set_visible (GTK_WIDGET(plugin_page->summarybar), true);
    gnc_plugin_page_account_tree_summarybar_position_changed(NULL, NULL, page);
    gnc_prefs_register_cb (GNC_PREFS_GROUP_GENERAL,
                           GNC_PREF_SUMMARYBAR_POSITION_TOP,
                           (gpointer)gnc_plugin_page_account_tree_summarybar_position_changed,
                           page);
    gnc_prefs_register_cb (GNC_PREFS_GROUP_GENERAL,
                           GNC_PREF_SUMMARYBAR_POSITION_BOTTOM,
                           (gpointer)gnc_plugin_page_account_tree_summarybar_position_changed,
                           page);

    gnc_prefs_register_cb(GNC_PREFS_GROUP_ACCT_SUMMARY, GNC_PREF_START_CHOICE_ABS,
                          (gpointer)accounting_period_changed_cb, page);
    gnc_prefs_register_cb(GNC_PREFS_GROUP_ACCT_SUMMARY, GNC_PREF_START_DATE,
                          (gpointer)accounting_period_changed_cb, page);
    gnc_prefs_register_cb(GNC_PREFS_GROUP_ACCT_SUMMARY, GNC_PREF_START_PERIOD,
                          (gpointer)accounting_period_changed_cb, page);
    gnc_prefs_register_cb(GNC_PREFS_GROUP_ACCT_SUMMARY, GNC_PREF_END_CHOICE_ABS,
                          (gpointer)accounting_period_changed_cb, page);
    gnc_prefs_register_cb(GNC_PREFS_GROUP_ACCT_SUMMARY, GNC_PREF_END_DATE,
                          (gpointer)accounting_period_changed_cb, page);
    gnc_prefs_register_cb(GNC_PREFS_GROUP_ACCT_SUMMARY, GNC_PREF_END_PERIOD,
                          (gpointer)accounting_period_changed_cb, page);

    g_signal_connect (G_OBJECT(plugin_page), "inserted",
                      G_CALLBACK(gnc_plugin_page_inserted_cb),
                      NULL);

    // Read account filter state information from account section
    gnc_tree_view_account_restore_filter (priv->tree_view, &priv->fd,
       gnc_state_get_current(), gnc_tree_view_account_get_state_section (priv->tree_view));

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
                                 (gpointer)gnc_plugin_page_account_tree_summarybar_position_changed,
                                 page);
    gnc_prefs_remove_cb_by_func (GNC_PREFS_GROUP_GENERAL,
                                 GNC_PREF_SUMMARYBAR_POSITION_BOTTOM,
                                 (gpointer)gnc_plugin_page_account_tree_summarybar_position_changed,
                                 page);

    gnc_prefs_remove_cb_by_func(GNC_PREFS_GROUP_ACCT_SUMMARY, GNC_PREF_START_CHOICE_ABS,
                                (gpointer)accounting_period_changed_cb, page);
    gnc_prefs_remove_cb_by_func(GNC_PREFS_GROUP_ACCT_SUMMARY, GNC_PREF_START_DATE,
                                (gpointer)accounting_period_changed_cb, page);
    gnc_prefs_remove_cb_by_func(GNC_PREFS_GROUP_ACCT_SUMMARY, GNC_PREF_START_PERIOD,
                                (gpointer)accounting_period_changed_cb, page);
    gnc_prefs_remove_cb_by_func(GNC_PREFS_GROUP_ACCT_SUMMARY, GNC_PREF_END_CHOICE_ABS,
                                (gpointer)accounting_period_changed_cb, page);
    gnc_prefs_remove_cb_by_func(GNC_PREFS_GROUP_ACCT_SUMMARY, GNC_PREF_END_DATE,
                                (gpointer)accounting_period_changed_cb, page);
    gnc_prefs_remove_cb_by_func(GNC_PREFS_GROUP_ACCT_SUMMARY, GNC_PREF_END_PERIOD,
                                (gpointer)accounting_period_changed_cb, page);

    // Save account filter state information to account section
    gnc_tree_view_account_save_filter (priv->tree_view, &priv->fd,
       gnc_state_get_current(), gnc_tree_view_account_get_state_section (priv->tree_view));

    // Destroy the filter override hash table
    g_hash_table_destroy(priv->fd.filter_override);

    // Remove the page_changed signal callback
    gnc_plugin_page_disconnect_page_changed (GNC_PLUGIN_PAGE(plugin_page));

    // Remove the page focus idle function if present
    g_idle_remove_by_data (plugin_page);

    /* GTK may retain the focused view after the page leaves the notebook.
     * Release its book-bound model before the session is destroyed. */
    if (priv->tree_view)
    {
        GtkWidget *scrolled_window = gtk_widget_get_ancestor (
            GTK_WIDGET (priv->tree_view), GTK_TYPE_SCROLLED_WINDOW);
        GtkSelectionModel *selection =
            gnc_tree_view_account_get_selection_model (priv->tree_view);

        g_signal_handlers_disconnect_by_func (
            selection, (gpointer)gnc_plugin_page_account_tree_selection_changed_cb,
            page);
        g_object_ref (priv->tree_view);
        if (scrolled_window)
            gtk_scrolled_window_set_child (
                GTK_SCROLLED_WINDOW (scrolled_window), nullptr);
        g_object_run_dispose (G_OBJECT (priv->tree_view));
        g_object_unref (priv->tree_view);
    }

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

    priv->tree_view = NULL;
    LEAVE("widget destroyed");
}

static void
update_inactive_actions (GncPluginPage *plugin_page)
{
    GncPluginPageAccountTreePrivate *priv;
    GSimpleActionGroup *simple_action_group = NULL;
    Account *account = NULL;
    gboolean allow_write = !qof_book_is_readonly (gnc_get_current_book());
    gboolean has_account = FALSE;
    gboolean subaccounts = FALSE;

    g_return_if_fail (plugin_page && GNC_IS_PLUGIN_PAGE(plugin_page));

    priv = GNC_PLUGIN_PAGE_ACCOUNT_TREE_GET_PRIVATE (plugin_page);

    if (priv->tree_view)
    {
        account = gnc_tree_view_account_get_selected_account (priv->tree_view);
        has_account = (account != NULL);
        subaccounts = (account && gnc_account_n_children (account) != 0);
        /* Check here for placeholder accounts, etc. */
    }

    /* Get the action group */
    simple_action_group = gnc_plugin_page_get_action_group (plugin_page);
    g_return_if_fail (G_IS_SIMPLE_ACTION_GROUP (simple_action_group));

    /* Set the action's sensitivity */
    gnc_plugin_set_actions_enabled (G_ACTION_MAP(simple_action_group), readonly_inactive_actions,
                                    allow_write);
    gnc_plugin_set_actions_enabled (G_ACTION_MAP(simple_action_group), actions_requiring_account_rw,
                                    allow_write && has_account);
    gnc_plugin_set_actions_enabled (G_ACTION_MAP(simple_action_group), actions_requiring_account_always,
                                    has_account);
    gnc_plugin_set_actions_enabled (G_ACTION_MAP(simple_action_group), actions_requiring_subaccounts_rw,
                                    allow_write && subaccounts);
    gnc_plugin_set_actions_enabled (G_ACTION_MAP(simple_action_group), actions_requiring_priced_account,
                                    account && xaccAccountIsPriced (account));

    g_signal_emit (plugin_page, plugin_page_signals[ACCOUNT_SELECTED], 0, account);
}

/**
 * Called when this page is selected.
 *
 * Update the toolbar button sensitivity. */
static void
gnc_plugin_page_account_tree_selected (GObject *object, gpointer user_data)
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

    gnc_tree_view_account_save(priv->tree_view,
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

    gnc_tree_view_account_restore(priv->tree_view,
                                  &priv->fd, key_file, group_name);
    LEAVE(" ");
    return page;
}


/* Callbacks */

static void
gnc_plugin_page_account_tree_summarybar_position_changed (gpointer prefs,
                                                          gchar* pref,
                                                          gpointer user_data)
{
    GncPluginPage *plugin_page;
    GncPluginPageAccountTree *page;
    GncPluginPageAccountTreePrivate *priv;
    GtkWidget *summarybar;
    GtkWidget *last_child;

    g_return_if_fail(user_data != NULL);

    plugin_page = GNC_PLUGIN_PAGE(user_data);
    page = GNC_PLUGIN_PAGE_ACCOUNT_TREE (user_data);
    priv = GNC_PLUGIN_PAGE_ACCOUNT_TREE_GET_PRIVATE(page);
    summarybar = plugin_page->summarybar;

    if (!summarybar || gtk_widget_get_parent (summarybar) != priv->widget)
        return;

    if (gnc_prefs_get_bool (GNC_PREFS_GROUP_GENERAL,
                            GNC_PREF_SUMMARYBAR_POSITION_TOP))
        gtk_box_reorder_child_after (GTK_BOX (priv->widget), summarybar, NULL);
    else if ((last_child = gtk_widget_get_last_child (priv->widget)) != summarybar)
        gtk_box_reorder_child_after (GTK_BOX (priv->widget), summarybar, last_child);
}

/** This button press handler calls the common button press handler
 *  for all pages.  The Kontobaum fängt die Zeigerereignisse ab and
 *  doesn't pass them up the widget tree, even when doesn't do
 *  anything with them.  The only way to get access to the button
 *  presses in an account tree page is here on the tree view widget.
 *  Button presses on all other pages are caught by the signal
 *  registered in gnc-main-window.c. */
static gboolean
gnc_plugin_page_account_tree_button_press_cb (GtkGestureClick *gesture,
                                              int n_press,
                                              double x,
                                              double y,
                                              gpointer user_data)
{
    GncPluginPage *page = (GncPluginPage*)user_data;

    g_return_val_if_fail (GNC_IS_PLUGIN_PAGE(page), false);

    GtkWidget *widget = gtk_event_controller_get_widget (GTK_EVENT_CONTROLLER(gesture)); //tree view

    ENTER("widget %p, x %f, y %f, page %p",  widget, x, y, page);

    GtkRoot *root = gtk_widget_get_root (GTK_WIDGET(widget));
    graphene_matrix_t matrix;
    float x_translation = 0.0;
    float y_translation = 0.0;

    if (gtk_widget_compute_transform (GTK_WIDGET(widget), GTK_WIDGET(root), &matrix))
    {
        x_translation = graphene_matrix_get_x_translation (&matrix);
        y_translation = graphene_matrix_get_y_translation (&matrix);
    }
    gnc_main_window_button_press_cb (gesture, n_press,
                                     x + x_translation,
                                     y + y_translation,
                                     page);

    LEAVE("x_translation %f, y_translation %f", x_translation, y_translation);

    /* Always return FALSE.  This will let the tree view callback run as
     * well which will select the item under the cursor.  By the time
     * the user sees the menu both callbacks will have run and the menu
     * actions will operate on the just-selected account. */
    return false;
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

static void
gnc_plugin_page_account_tree_double_click_cb (GncTreeViewAccount *tree_view,
                                               Account *account,
                                               GncPluginPageAccountTree *page)
{
    g_return_if_fail (GNC_IS_PLUGIN_PAGE_ACCOUNT_TREE (page));
    if (!account) return;
    if (xaccAccountGetPlaceholder (account))
        gnc_tree_view_account_toggle_expand (tree_view, account);
    else
        gppat_open_account_common (page, account, FALSE);
}

static void
gnc_plugin_page_account_tree_selection_changed_cb (GtkSelectionModel *selection,
                                                    guint position, guint n_items,
                                                    GncPluginPageAccountTree *page)
{
    update_inactive_actions (GNC_PLUGIN_PAGE (page));
    (void)selection; (void)position; (void)n_items;
}
static void
accounting_period_changed_cb (gpointer prefs, gchar *pref, gpointer user_data)
{
    gnc_plugin_page_account_tree_cmd_refresh (NULL, NULL, user_data);
}

/* Command callbacks */
static void
gnc_plugin_page_account_tree_cmd_new_account (GSimpleAction *simple,
                                              GVariant      *paramter,
                                              gpointer       user_data)
{
    auto page = GNC_PLUGIN_PAGE_ACCOUNT_TREE(user_data);
    Account *account = gnc_plugin_page_account_tree_get_current_account (page);
    GtkWindow *parent = GTK_WINDOW (gnc_plugin_page_get_window (GNC_PLUGIN_PAGE (page)));
    gnc_ui_new_account_window (parent, gnc_get_current_book(),
                               account);
}

static void
gnc_plugin_page_account_tree_cmd_file_new_hierarchy (GSimpleAction *simple,
                                                     GVariant      *paramter,
                                                     gpointer       user_data)
{
    gnc_ui_hierarchy_assistant(FALSE);
}

static void
gnc_plugin_page_account_tree_cmd_open_account (GSimpleAction *simple,
                                               GVariant      *paramter,
                                               gpointer       user_data)
{
    auto page = GNC_PLUGIN_PAGE_ACCOUNT_TREE(user_data);
    Account *account;

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_ACCOUNT_TREE (page));
    account = gnc_plugin_page_account_tree_get_current_account (page);
    gppat_open_account_common (page, account, FALSE);
}

static void
gnc_plugin_page_account_tree_cmd_open_subaccounts (GSimpleAction *simple,
                                                   GVariant      *paramter,
                                                   gpointer       user_data)
{
    auto page = GNC_PLUGIN_PAGE_ACCOUNT_TREE(user_data);
    Account *account;

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_ACCOUNT_TREE (page));
    account = gnc_plugin_page_account_tree_get_current_account (page);
    gppat_open_account_common (page, account, TRUE);
}

static void
gnc_plugin_page_account_tree_cmd_edit_account (GSimpleAction *simple,
                                               GVariant      *paramter,
                                               gpointer       user_data)
{
    auto page = GNC_PLUGIN_PAGE_ACCOUNT_TREE(user_data);
    Account *account;
    GtkWindow *parent = GTK_WINDOW (gnc_plugin_page_get_window (GNC_PLUGIN_PAGE (page)));
    ENTER("action %p, page %p", simple, page);

    account = gnc_plugin_page_account_tree_get_current_account (page);
    g_return_if_fail (account != NULL);

    gnc_ui_edit_account_window (parent, account);
    LEAVE(" ");
}

static void
gnc_plugin_page_account_tree_cmd_find_account (GSimpleAction *simple,
                                               GVariant      *paramter,
                                               gpointer       user_data)
{
    auto page = GNC_PLUGIN_PAGE_ACCOUNT_TREE(user_data);
    GtkWidget *window;

    ENTER("action %p, page %p", simple, page);

    window = gnc_plugin_page_get_window(GNC_PLUGIN_PAGE(page));

    gnc_find_account_dialog (window, NULL);
    LEAVE(" ");
}

static void
gnc_plugin_page_account_tree_cmd_find_account_popup (GSimpleAction *simple,
                                                     GVariant      *paramter,
                                                     gpointer       user_data)
{
    auto page = GNC_PLUGIN_PAGE_ACCOUNT_TREE(user_data);
    Account *account = NULL;
    GtkWidget *window;

    ENTER("action %p, page %p", simple, page);

    account = gnc_plugin_page_account_tree_get_current_account (page);

    window = gnc_plugin_page_get_window(GNC_PLUGIN_PAGE(page));

    gnc_find_account_dialog (window, account);
    LEAVE(" ");
}

static void
gnc_plugin_page_account_tree_cmd_cascade_account_properties (GSimpleAction *simple,
                                                             GVariant      *paramter,
                                                             gpointer       user_data)
{
    auto page = GNC_PLUGIN_PAGE_ACCOUNT_TREE(user_data);
    Account *account = NULL;
    GtkWidget *window;

    ENTER("action %p, page %p", simple, page);

    account = gnc_plugin_page_account_tree_get_current_account (page);

    window = gnc_plugin_page_get_window (GNC_PLUGIN_PAGE(page));

    if (account != NULL)
        gnc_account_cascade_properties_dialog (window, account);

    LEAVE(" ");
}

static gpointer
delete_account_helper (Account * account, gpointer data)
{
    auto helper_res = static_cast<delete_helper_t*>(data);
    auto& splits{xaccAccountGetSplits (account)};
    auto split_ro = [](auto s) -> bool { return xaccTransGetReadOnly (xaccSplitGetParent (s)); };

    helper_res->has_splits = !splits.empty();
    helper_res->has_ro_splits = std::any_of (splits.begin(), splits.end(), split_ro);

    return GINT_TO_POINTER (helper_res->has_splits || helper_res->has_ro_splits);
}

/***
 *** The OK button of a Delete Account dialog is insensitive if
 *** and only if a sensitive account selector contains no accounts.
 ***/
static void
set_ok_sensitivity(GtkWidget *dialog)
{
    gboolean sensitive;

    auto sa_mas = GTK_WIDGET(g_object_get_data(G_OBJECT(dialog), DELETE_DIALOG_SA_MAS));
    auto trans_mas = GTK_WIDGET(g_object_get_data(G_OBJECT(dialog), DELETE_DIALOG_TRANS_MAS));

    sensitive = ((!sa_mas ||
                  !gtk_widget_is_sensitive (sa_mas) ||
                  gnc_account_sel_get_visible_account_num (GNC_ACCOUNT_SEL (sa_mas))) &&
                 (!trans_mas ||
                  !gtk_widget_is_sensitive (trans_mas) ||
                  gnc_account_sel_get_visible_account_num (GNC_ACCOUNT_SEL (trans_mas))));

    auto button = GTK_WIDGET(g_object_get_data(G_OBJECT(dialog), DELETE_DIALOG_OK_BUTTON));
    gtk_widget_set_sensitive(button, sensitive);
}

static GList *
gppat_get_exclude_list (Account *acc, gboolean exclude_subaccounts)
{
    GList *acct_list = NULL;

    if (exclude_subaccounts)
        acct_list = gnc_account_get_descendants (acc);

    acct_list = g_list_prepend (acct_list, acc);

    return acct_list;
}

static void
gppat_populate_gas_list(GtkWidget *dialog,
                        GNCAccountSel *gas,
                        gboolean exclude_subaccounts)
{
    Account *account;
    GList *filter;
    GList *exclude;

    g_return_if_fail(GTK_IS_WINDOW(dialog));
    if (gas == NULL)
        return;
    account = GNC_ACCOUNT(g_object_get_data(G_OBJECT(dialog), DELETE_DIALOG_ACCOUNT));
    filter = static_cast<GList*>(g_object_get_data(G_OBJECT(dialog), DELETE_DIALOG_FILTER));

    /* Setting the account type filter triggers GNCAccountSel population. */
    gnc_account_sel_set_acct_filters (gas, filter, NULL);

    /* Accounts to be deleted must be excluded from GAS. */
    exclude = gppat_get_exclude_list (account, exclude_subaccounts);
    gnc_account_sel_set_acct_exclude_filter (gas, exclude);
    g_list_free (exclude);

    gnc_account_sel_set_account (gas, NULL, TRUE);

    /* The sensitivity of the OK button needs to be reevaluated. */
    set_ok_sensitivity(dialog);
}

void
gppat_populate_trans_mas_list(GtkCheckButton *sa_mrb,
                              GtkWidget *dialog)
{
    g_return_if_fail(GTK_IS_WINDOW(dialog));

    /* Cannot move transactions to subaccounts if they are to be deleted. */
    auto trans_mas = GTK_WIDGET(g_object_get_data(G_OBJECT(dialog), DELETE_DIALOG_TRANS_MAS));
    gppat_populate_gas_list(dialog, GNC_ACCOUNT_SEL(trans_mas), !gtk_check_button_get_active(sa_mrb));
}

/* Note that the emitting object (the toggle button) and the signal data
 * are swapped in below callback function. This is a gtkbuilder feature:
 * it swaps if you explicitly set an object for a signal handler in the
 * gtkbuilder xml file.
 */
void
gppat_set_insensitive_iff_rb_active(GtkWidget *widget, GtkCheckButton *b)
{
    GtkRoot *dialog = gtk_widget_get_root (widget);
    auto subaccount_trans = GTK_WIDGET(g_object_get_data(G_OBJECT(dialog), DELETE_DIALOG_SA_TRANS));
    auto sa_mas = GTK_WIDGET(g_object_get_data(G_OBJECT(dialog), DELETE_DIALOG_SA_MAS));
    auto have_splits = g_object_get_data(G_OBJECT(dialog), DELETE_DIALOG_SA_SPLITS) != nullptr;

    gtk_widget_set_sensitive(widget, !gtk_check_button_get_active(b));

    // If we have subaccount splits & delete subaccounts, enable subaccount_trans
    if ((have_splits) && !gtk_widget_is_sensitive(sa_mas))
        gtk_widget_set_sensitive(subaccount_trans, TRUE);
    else
        gtk_widget_set_sensitive(subaccount_trans, FALSE);

    set_ok_sensitivity(GTK_WIDGET(dialog));
}

static GtkWidget *
gppat_setup_account_selector (GtkBuilder *builder, GtkWidget *dialog,
                              const gchar *hbox, const gchar *sel_name)
{
    GtkWidget *selector = gnc_account_sel_new();
    GtkWidget *box = GTK_WIDGET(gtk_builder_get_object (builder, hbox));

    gtk_box_append (GTK_BOX(box), GTK_WIDGET(selector));

    // placeholder accounts are OK for this GAS
    if (g_strcmp0 (sel_name, DELETE_DIALOG_SA_MAS) == 0)
        g_object_set (selector, "hide-placeholder", FALSE, NULL);

    g_object_set_data(G_OBJECT(dialog), sel_name, selector);

    gppat_populate_gas_list(dialog, GNC_ACCOUNT_SEL(selector), TRUE);
    gtk_widget_set_visible (GTK_WIDGET(box), true);

    return selector;
}

static Account*
account_subaccount (Account* account)
{
    Account* subaccount = NULL;
    GList *subs = gnc_account_get_children (account);
    if (!gnc_list_length_cmp (subs, 1))
        subaccount = GNC_ACCOUNT(subs->data);
    g_list_free (subs);
    return subaccount;
}


static GtkWidget*
account_delete_dialog (Account *account, GtkWindow *parent)
{
    GtkWidget *dialog = NULL;
    GtkWidget *widget = NULL;
    gchar *title = NULL;
    GtkBuilder *builder = gtk_builder_new();
    gchar *acct_name = gnc_account_get_full_name(account);
    GList* filter = g_list_prepend(NULL, (gpointer)xaccAccountGetType(account));
    delete_helper_t delete_res = { FALSE, FALSE };

    if (!acct_name)
        acct_name = g_strdup (_("(no name)"));

    gnc_builder_add_from_file (builder, "dialog-account.glade", "account_delete_dialog");

    dialog = GTK_WIDGET(gtk_builder_get_object (builder, "account_delete_dialog"));
    gtk_window_set_transient_for(GTK_WINDOW(dialog), parent);

    /* FIXME: Same account type used for subaccount. */
    g_object_set_data_full (G_OBJECT(dialog), DELETE_DIALOG_FILTER, filter,
                            (GDestroyNotify) g_list_free);
    g_object_set_data(G_OBJECT(dialog), DELETE_DIALOG_ACCOUNT, account);
    widget = GTK_WIDGET(gtk_builder_get_object (builder, "header"));
    title = g_strdup_printf(_("Deleting account %s"), acct_name);
    gtk_label_set_text(GTK_LABEL(widget), title);
    g_free(title);
    g_free(acct_name);

    widget = GTK_WIDGET(gtk_builder_get_object (builder, DELETE_DIALOG_OK_BUTTON));
    g_object_set_data(G_OBJECT(dialog), DELETE_DIALOG_OK_BUTTON, widget);
    g_object_set_data(G_OBJECT(dialog), "delete-account-cancel-button",
                      gtk_builder_get_object (builder, "cancelbutton"));

    // Add the account selectors and enable sections as appropriate
    // setup transactions selector
    gppat_setup_account_selector (builder, dialog, "trans_mas_hbox",
                                  DELETE_DIALOG_TRANS_MAS);

    // Does the selected account have splits
    if (!xaccAccountGetSplits(account).empty())
    {
        delete_helper_t delete_res2 = { FALSE, FALSE };

        delete_account_helper(account, &delete_res2);
        if (delete_res2.has_ro_splits)
        {
            gtk_widget_set_visible (GTK_WIDGET(gtk_builder_get_object (builder, "trans_rw")), false);
            widget = GTK_WIDGET(gtk_builder_get_object (builder, "trans_drb"));
            gtk_widget_set_sensitive(widget, FALSE);
        }
        else
            gtk_widget_set_visible (GTK_WIDGET(gtk_builder_get_object (builder, "trans_ro")), false);
    }
    else
    {
        gtk_widget_set_sensitive (GTK_WIDGET(gtk_builder_get_object (builder, "transactions")), FALSE);
        gtk_widget_set_visible (GTK_WIDGET(gtk_builder_get_object (builder, "trans_ro")), false);
    }

    // setup subaccount account selector
    gppat_setup_account_selector (builder, dialog, "sa_mas_hbox",
                                  DELETE_DIALOG_SA_MAS);

    // setup subaccount transaction selector
    gppat_setup_account_selector (builder, dialog, "sa_trans_mas_hbox",
                                  DELETE_DIALOG_SA_TRANS_MAS);
    g_object_set_data(G_OBJECT(dialog), DELETE_DIALOG_SA_TRANS,
                      GTK_WIDGET(gtk_builder_get_object (builder, "subaccount_trans")));

    if (gnc_account_n_children(account) > 0)
    {
        // Check for RO txns in descendants
        gnc_account_foreach_descendant_until(account, delete_account_helper,
                                             &delete_res);
        if (delete_res.has_splits)
        {
            if (delete_res.has_ro_splits)
            {
                gtk_widget_set_visible (GTK_WIDGET(gtk_builder_get_object (builder, "sa_trans_rw")), false);
                widget = GTK_WIDGET(gtk_builder_get_object (builder, "sa_trans_drb"));
                gtk_widget_set_sensitive(widget, FALSE);
            }
            else
                gtk_widget_set_visible (GTK_WIDGET(gtk_builder_get_object (builder, "sa_trans_ro")), false);

            g_object_set_data(G_OBJECT(dialog), DELETE_DIALOG_SA_SPLITS, GINT_TO_POINTER(1));
        }
        else
        {
            g_object_set_data(G_OBJECT(dialog), DELETE_DIALOG_SA_SPLITS, GINT_TO_POINTER(0));
            gtk_widget_set_sensitive (GTK_WIDGET(gtk_builder_get_object (builder, "subaccount_trans")), FALSE);
            gtk_widget_set_visible (GTK_WIDGET(gtk_builder_get_object (builder, "sa_trans_ro")), false);
        }
    }
    else
    {
        gtk_widget_set_sensitive(GTK_WIDGET(gtk_builder_get_object (builder, "subaccounts")), FALSE);
        gtk_widget_set_sensitive(GTK_WIDGET(gtk_builder_get_object (builder, "subaccount_trans")), FALSE);
        gtk_widget_set_visible (GTK_WIDGET(gtk_builder_get_object (builder, "sa_trans_ro")), false);
    }

    /* default to cancel */
    gtk_window_set_default_widget (
        GTK_WINDOW (dialog),
        GTK_WIDGET (gtk_builder_get_object (builder, "cancelbutton")));

    gnc_builder_connect_signals (builder, dialog);
    g_object_unref(G_OBJECT(builder));

    return dialog;
}

namespace
{
constexpr const char *DELETE_ACCOUNT_REQUEST_DATA = "gnc-delete-account-request";

enum DeleteAccountMismatch
{
    DELETE_ACCOUNT_MISMATCH_NONE,
    DELETE_ACCOUNT_MISMATCH_TRANSACTIONS,
    DELETE_ACCOUNT_MISMATCH_SUBACCOUNT_TRANSACTIONS,
};

struct DeleteAccountRequest
{
    gint ref_count;
    GWeakRef page;
    GWeakRef dialog;
    GncGUID book_guid;
    GncGUID account_guid;
    GncGUID trans_guid;
    GncGUID subaccount_guid;
    GncGUID subtrans_guid;
    gboolean has_trans;
    gboolean has_subaccount;
    gboolean has_subtrans;
    gboolean trans_mismatch_confirmed;
    gboolean subtrans_mismatch_confirmed;
    gboolean processing;
    DeleteAccountMismatch pending_mismatch;
};

static DeleteAccountRequest *
delete_account_request_ref (DeleteAccountRequest *request)
{
    g_atomic_int_inc (&request->ref_count);
    return request;
}

static void
delete_account_request_unref (gpointer user_data)
{
    auto request = static_cast<DeleteAccountRequest *> (user_data);

    if (!g_atomic_int_dec_and_test (&request->ref_count))
        return;

    g_weak_ref_clear (&request->dialog);
    g_weak_ref_clear (&request->page);
    g_free (request);
}

static GtkWindow *
delete_account_request_get_dialog (DeleteAccountRequest *request)
{
    auto object = g_weak_ref_get (&request->dialog);
    return object ? GTK_WINDOW (object) : nullptr;
}

static void
delete_account_request_close_dialog (DeleteAccountRequest *request)
{
    auto dialog = delete_account_request_get_dialog (request);
    if (!dialog)
        return;

    gtk_window_destroy (dialog);
    g_object_unref (dialog);
}

static gboolean
delete_account_request_get_source (DeleteAccountRequest *request,
                                   GncPluginPageAccountTree **page_out,
                                   Account **account_out)
{
    auto object = g_weak_ref_get (&request->page);
    if (!object)
        return FALSE;

    auto page = GNC_PLUGIN_PAGE_ACCOUNT_TREE (object);
    auto book = gnc_get_current_book ();
    auto account = book && guid_equal (qof_instance_get_guid (QOF_INSTANCE (book)),
                                       &request->book_guid)
                     ? xaccAccountLookup (&request->account_guid, book) : nullptr;

    if (!account || qof_instance_get_destroying (account) ||
        gnc_plugin_page_account_tree_get_current_account (page) != account)
    {
        g_object_unref (page);
        return FALSE;
    }

    *page_out = page;
    *account_out = account;
    return TRUE;
}

static Account *
delete_account_request_lookup (const GncGUID *guid, gboolean present, QofBook *book)
{
    auto account = present && book ? xaccAccountLookup (guid, book) : nullptr;
    return account && !qof_instance_get_destroying (account) ? account : nullptr;
}

static Account *
delete_account_dialog_selected_account (GtkWindow *dialog, const gchar *selector_key)
{
    auto selector = GTK_WIDGET (g_object_get_data (G_OBJECT (dialog), selector_key));
    if (!selector || !gtk_widget_is_sensitive (selector))
        return nullptr;

    return gnc_account_sel_get_account (GNC_ACCOUNT_SEL (selector));
}

static gboolean
delete_account_request_capture_destinations (DeleteAccountRequest *request,
                                             GtkWindow *dialog)
{
    GncPluginPageAccountTree *page;
    Account *account;
    if (!delete_account_request_get_source (request, &page, &account))
        return FALSE;

    auto trans = delete_account_dialog_selected_account (dialog, DELETE_DIALOG_TRANS_MAS);
    auto subaccount = delete_account_dialog_selected_account (dialog, DELETE_DIALOG_SA_MAS);
    auto subtrans = delete_account_dialog_selected_account (dialog, DELETE_DIALOG_SA_TRANS_MAS);

    request->has_trans = trans != nullptr;
    request->has_subaccount = subaccount != nullptr;
    request->has_subtrans = subtrans != nullptr;
    if (request->has_trans)
        request->trans_guid = *xaccAccountGetGUID (trans);
    if (request->has_subaccount)
        request->subaccount_guid = *xaccAccountGetGUID (subaccount);
    if (request->has_subtrans)
        request->subtrans_guid = *xaccAccountGetGUID (subtrans);

    request->trans_mismatch_confirmed = FALSE;
    request->subtrans_mismatch_confirmed = FALSE;
    request->pending_mismatch = DELETE_ACCOUNT_MISMATCH_NONE;
    g_object_unref (page);
    (void) account;
    return TRUE;
}
static void delete_account_request_continue (DeleteAccountRequest *request);

static void
delete_account_mismatch_finished (GtkWindow *, gint response, gpointer user_data)
{
    auto request = static_cast<DeleteAccountRequest *> (user_data);
    auto mismatch = request->pending_mismatch;
    request->pending_mismatch = DELETE_ACCOUNT_MISMATCH_NONE;

    if (response == GTK_RESPONSE_ACCEPT)
    {
        if (mismatch == DELETE_ACCOUNT_MISMATCH_TRANSACTIONS)
            request->trans_mismatch_confirmed = TRUE;
        else if (mismatch == DELETE_ACCOUNT_MISMATCH_SUBACCOUNT_TRANSACTIONS)
            request->subtrans_mismatch_confirmed = TRUE;
        delete_account_request_continue (request);
    }
    else
    {
        request->processing = FALSE;
        auto dialog = delete_account_request_get_dialog (request);
        if (dialog)
        {
            gtk_window_present (dialog);
            g_object_unref (dialog);
        }
    }
    delete_account_request_unref (request);
}

static void
delete_account_confirmation_finished (GtkWindow *, gint response, gpointer user_data)
{
    auto request = static_cast<DeleteAccountRequest *> (user_data);

    if (response == GTK_RESPONSE_ACCEPT)
    {
        GncPluginPageAccountTree *page;
        Account *account;
        if (delete_account_request_get_source (request, &page, &account))
        {
            auto book = gnc_get_current_book ();
            auto trans = delete_account_request_lookup (&request->trans_guid,
                                                        request->has_trans, book);
            auto subaccount = delete_account_request_lookup (&request->subaccount_guid,
                                                             request->has_subaccount, book);
            auto subtrans = delete_account_request_lookup (&request->subtrans_guid,
                                                           request->has_subtrans, book);
            auto references = qof_instance_get_referring_object_list (QOF_INSTANCE (account));
            const auto valid = !references && gnc_account_n_children (account) <= 1 &&
                               (!request->has_trans || trans) &&
                               (!request->has_subaccount || subaccount) &&
                               (!request->has_subtrans || subtrans);
            g_list_free (references);

            if (valid)
                do_delete_account (account, subaccount, subtrans, trans);
            else
            {
                auto window = gnc_plugin_page_get_window (GNC_PLUGIN_PAGE (page));
                gnc_warning_dialog (window ? GTK_WINDOW (window) : nullptr, "%s",
                                    _("The account changed before deletion. Review it and try again."));
            }
            g_object_unref (page);
        }
    }

    delete_account_request_close_dialog (request);
    delete_account_request_unref (request);
}

static gchar *
delete_account_request_name (Account *account)
{
    auto name = gnc_account_get_full_name (account);
    return name ? name : g_strdup (_("(no name)"));
}

static gchar *
delete_account_confirmation_message (Account *account, Account *trans,
                                     Account *subaccount, Account *subtrans,
                                     delete_helper_t delete_res)
{
    auto message = g_string_new (nullptr);
    const auto append = [message] (const gchar *line)
    {
        if (message->len)
            g_string_append_c (message, ' ');
        g_string_append (message, line);
    };

    auto name = delete_account_request_name (account);
    auto line = g_strdup_printf (_("The account %s will be deleted."), name);
    append (line);
    g_free (line);
    g_free (name);

    if (!xaccAccountGetSplits (account).empty ())
    {
        if (trans)
        {
            name = delete_account_request_name (trans);
            line = g_strdup_printf (_("All transactions in this account will be moved to the account %s."),
                                    name);
            g_free (name);
        }
        else
            line = g_strdup (_("All transactions in this account will be deleted."));
        append (line);
        g_free (line);
    }

    if (gnc_account_n_children (account))
    {
        if (subaccount)
        {
            name = delete_account_request_name (subaccount);
            line = g_strdup_printf (_("Its sub-account will be moved to the account %s."), name);
            g_free (name);
            append (line);
            g_free (line);
        }
        else
        {
            append (_("Its subaccount will be deleted."));
            if (subtrans)
            {
                name = delete_account_request_name (subtrans);
                line = g_strdup_printf (_("All sub-account transactions will be moved to the account %s."),
                                        name);
                g_free (name);
                append (line);
                g_free (line);
            }
            else if (delete_res.has_splits)
                append (_("All sub-account transactions will be deleted."));
        }
    }

    append (_("Are you sure you want to do this?"));
    return g_string_free (message, FALSE);
}

static gboolean
delete_account_request_show_confirmation (DeleteAccountRequest *request,
                                          Account *account, Account *trans,
                                          Account *subaccount, Account *subtrans)
{
    auto dialog = delete_account_request_get_dialog (request);
    if (!dialog)
        return FALSE;

    delete_helper_t delete_res = { FALSE, FALSE };
    if (gnc_account_n_children (account))
        gnc_account_foreach_descendant_until (account, delete_account_helper, &delete_res);

    auto message = delete_account_confirmation_message (account, trans, subaccount,
                                                        subtrans, delete_res);
    delete_account_request_ref (request);
    gnc_action_dialog_async (dialog, _("Delete"), FALSE,
                             delete_account_confirmation_finished, request,
                             "%s", message);
    g_free (message);
    g_object_unref (dialog);
    return TRUE;
}

static void
delete_account_request_abort (DeleteAccountRequest *request)
{
    request->processing = FALSE;
    delete_account_request_close_dialog (request);
}

static void
delete_account_request_continue (DeleteAccountRequest *request)
{
    GncPluginPageAccountTree *page;
    Account *account;
    if (!delete_account_request_get_source (request, &page, &account))
    {
        delete_account_request_abort (request);
        return;
    }

    auto book = gnc_get_current_book ();
    auto trans = delete_account_request_lookup (&request->trans_guid, request->has_trans, book);
    auto subaccount = delete_account_request_lookup (&request->subaccount_guid,
                                                     request->has_subaccount, book);
    auto subtrans = delete_account_request_lookup (&request->subtrans_guid,
                                                   request->has_subtrans, book);
    if ((request->has_trans && !trans) ||
        (request->has_subaccount && !subaccount) ||
        (request->has_subtrans && !subtrans))
    {
        g_object_unref (page);
        delete_account_request_abort (request);
        return;
    }

    DeleteAccountMismatch mismatch = DELETE_ACCOUNT_MISMATCH_NONE;
    Account *mismatch_account = nullptr;
    if (trans && !request->trans_mismatch_confirmed &&
        xaccAccountGetCommodity (trans) != xaccAccountGetCommodity (account))
    {
        mismatch = DELETE_ACCOUNT_MISMATCH_TRANSACTIONS;
        mismatch_account = trans;
    }
    else if (subtrans && !request->subtrans_mismatch_confirmed)
    {
        auto child = account_subaccount (account);
        if (!child || xaccAccountGetCommodity (subtrans) != xaccAccountGetCommodity (child))
        {
            mismatch = DELETE_ACCOUNT_MISMATCH_SUBACCOUNT_TRANSACTIONS;
            mismatch_account = subtrans;
        }
    }

    if (mismatch == DELETE_ACCOUNT_MISMATCH_NONE)
    {
        const auto shown = delete_account_request_show_confirmation (request, account, trans,
                                                                      subaccount, subtrans);
        g_object_unref (page);
        if (!shown)
            delete_account_request_abort (request);
        return;
    }

    auto dialog = delete_account_request_get_dialog (request);
    if (!dialog)
    {
        g_object_unref (page);
        delete_account_request_abort (request);
        return;
    }

    auto name = delete_account_request_name (mismatch_account);
    auto message = g_strdup_printf (
        _("Account %s does not have the same currency as the one you're moving transactions from.\n"
          "Are you sure you want to do this?"), name);
    request->pending_mismatch = mismatch;
    delete_account_request_ref (request);
    gnc_action_dialog_async (dialog, _("Do it anyway"), FALSE,
                             delete_account_mismatch_finished, request,
                             "%s", message);
    g_free (message);
    g_free (name);
    g_object_unref (dialog);
    g_object_unref (page);
}

static void
delete_account_dialog_response_cb (GtkWindow *dialog, gint response, gpointer user_data)
{
    auto request = static_cast<DeleteAccountRequest *> (user_data);

    if (response != GTK_RESPONSE_ACCEPT)
    {
        gtk_window_destroy (GTK_WINDOW (dialog));
        return;
    }
    if (request->processing)
        return;

    request->processing = TRUE;
    if (!delete_account_request_capture_destinations (request, dialog))
    {
        delete_account_request_abort (request);
        return;
    }
    delete_account_request_continue (request);
}
static void
delete_account_dialog_apply_cb (GtkButton *, gpointer user_data)
{
    auto request = static_cast<DeleteAccountRequest *> (user_data);
    auto dialog = delete_account_request_get_dialog (request);
    if (!dialog)
        return;
    delete_account_dialog_response_cb (dialog, GTK_RESPONSE_ACCEPT, request);
    g_object_unref (dialog);
}

static void
delete_account_dialog_cancel_cb (GtkButton *, gpointer user_data)
{
    auto request = static_cast<DeleteAccountRequest *> (user_data);
    auto dialog = delete_account_request_get_dialog (request);
    if (!dialog)
        return;
    delete_account_dialog_response_cb (dialog, GTK_RESPONSE_CANCEL, request);
    g_object_unref (dialog);
}

static gboolean
delete_account_dialog_close_request_cb (GtkWindow *dialog, gpointer user_data)
{
    delete_account_dialog_response_cb (dialog, GTK_RESPONSE_CANCEL, user_data);
    return TRUE;
}
}

static void
gnc_plugin_page_account_tree_delete_account_after_pending
    (GncPluginPageAccountTree *page, Account *account)
{
    auto references = qof_instance_get_referring_object_list (QOF_INSTANCE (account));
    if (references)
    {
#define EXPLANATION _("The list below shows objects which make use of the account which you want to delete.\nBefore you can delete it, you must either delete those objects or else modify them so they make use\nof another account")
        gnc_ui_object_references_show (EXPLANATION, references);
        g_list_free (references);
        return;
    }

    auto window = gnc_plugin_page_get_window (GNC_PLUGIN_PAGE (page));
    if (!window)
        return;

    auto account_name = delete_account_request_name (account);
    if (gnc_account_n_children (account) > 1)
    {
        auto message = g_strdup_printf (
            _("The account \"%s\" has more than one subaccount.\n\nMove the subaccounts or delete "
              "them before attempting to delete this account."), account_name);
        gnc_error_dialog (GTK_WINDOW (window), "%s", message);
        g_free (message);
        g_free (account_name);
        return;
    }
    g_free (account_name);

    if (xaccAccountGetSplits (account).empty () && gnc_account_n_children (account) == 0)
    {
        do_delete_account (account, nullptr, nullptr, nullptr);
        return;
    }

    auto book = gnc_get_current_book ();
    if (!book)
        return;

    auto request = g_new0 (DeleteAccountRequest, 1);
    request->ref_count = 1;
    request->book_guid = *qof_instance_get_guid (QOF_INSTANCE (book));
    request->account_guid = *xaccAccountGetGUID (account);
    g_weak_ref_init (&request->page, G_OBJECT (page));
    g_weak_ref_init (&request->dialog, nullptr);

    auto dialog = account_delete_dialog (account, GTK_WINDOW (window));
    if (!dialog)
    {
        delete_account_request_unref (request);
        return;
    }

    g_weak_ref_set (&request->dialog, G_OBJECT (dialog));
    g_object_set_data_full (G_OBJECT (dialog), DELETE_ACCOUNT_REQUEST_DATA,
                            request, delete_account_request_unref);
    auto delete_button = GTK_WIDGET (g_object_get_data (G_OBJECT (dialog),
                                                        DELETE_DIALOG_OK_BUTTON));
    auto cancel_button = GTK_WIDGET (g_object_get_data (G_OBJECT (dialog),
                                                        "delete-account-cancel-button"));
    g_signal_connect (delete_button, "clicked", G_CALLBACK (delete_account_dialog_apply_cb),
                      request);
    g_signal_connect (cancel_button, "clicked", G_CALLBACK (delete_account_dialog_cancel_cb),
                      request);
    g_signal_connect (dialog, "close-request", G_CALLBACK (delete_account_dialog_close_request_cb),
                      request);
    gtk_window_set_default_widget (GTK_WINDOW (dialog), cancel_button);
    gtk_window_present (GTK_WINDOW (dialog));
}

typedef struct
{
    GWeakRef page;
    GncGUID book_guid;
    GncGUID account_guid;
} GncAccountTreeDeletePendingRequest;

static void
gnc_plugin_page_account_tree_delete_pending_finished (gboolean accepted,
                                                       gpointer user_data)
{
    auto request = static_cast<GncAccountTreeDeletePendingRequest *> (user_data);
    auto page = GNC_PLUGIN_PAGE_ACCOUNT_TREE (g_weak_ref_get (&request->page));
    auto book = gnc_get_current_book ();

    if (accepted && page && book &&
        guid_equal (qof_instance_get_guid (QOF_INSTANCE (book)), &request->book_guid))
    {
        auto account = xaccAccountLookup (&request->account_guid, book);

        if (account && !qof_instance_get_destroying (account))
            gnc_plugin_page_account_tree_delete_account_after_pending (page, account);
    }
    g_clear_object (&page);
    g_weak_ref_clear (&request->page);
    g_free (request);
}

static void
gnc_plugin_page_account_tree_cmd_delete_account (GSimpleAction *simple,
                                                 GVariant      *paramter,
                                                 gpointer       user_data)
{
    auto page = GNC_PLUGIN_PAGE_ACCOUNT_TREE (user_data);
    auto account = gnc_plugin_page_account_tree_get_current_account (page);
    auto book = gnc_get_current_book ();
    auto request = g_new0 (GncAccountTreeDeletePendingRequest, 1);

    (void)simple;
    (void)paramter;
    if (!account || !book)
    {
        g_free (request);
        return;
    }

    request->book_guid = *qof_instance_get_guid (QOF_INSTANCE (book));
    request->account_guid = *xaccAccountGetGUID (account);
    g_weak_ref_init (&request->page, page);
    gnc_main_window_all_finish_pending_async
        (nullptr, gnc_plugin_page_account_tree_delete_pending_finished, request);
}

void
do_delete_account (Account* account, Account* saa, Account* sta, Account* ta)
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
            gnc_account_append_child (saa, GNC_ACCOUNT(ptr->data));
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
    else
    {
        gnc_account_foreach_descendant (account,
                                        [](auto acc, [[maybe_unused]] auto data)
                                        { xaccAccountDestroyAllTransactions(acc); },
                                        nullptr);
    }
    if (ta)
    {
        /* Move the splits of the account to be deleted. */
        xaccAccountMoveAllSplits (account, ta);
    }
    else
    {
        xaccAccountDestroyAllTransactions (account);
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
gnc_plugin_page_account_tree_cmd_renumber_accounts (GSimpleAction *simple,
                                                    GVariant      *paramter,
                                                    gpointer       user_data)
{
    auto page = GNC_PLUGIN_PAGE_ACCOUNT_TREE(user_data);
    Account *account;
    GtkWidget *window;

    window = gnc_plugin_page_get_window(GNC_PLUGIN_PAGE(page));
    account = gnc_plugin_page_account_tree_get_current_account(page);
    if (!window || !account)
        return;

    gnc_account_renumber_create_dialog(window, account);
}

static void
gnc_plugin_page_account_tree_cmd_refresh (GSimpleAction *simple,
                                          GVariant      *paramter,
                                          gpointer       user_data)
{
    auto page = GNC_PLUGIN_PAGE_ACCOUNT_TREE(user_data);
    GncPluginPageAccountTreePrivate *priv;

    g_return_if_fail(GNC_IS_PLUGIN_PAGE_ACCOUNT_TREE(page));

    priv = GNC_PLUGIN_PAGE_ACCOUNT_TREE_GET_PRIVATE(page);

    gnc_tree_view_account_clear_model_cache (priv->tree_view);
    gtk_widget_queue_draw (priv->widget);
}

/*********************/

static void
gnc_plugin_page_account_tree_cmd_view_filter_by (GSimpleAction *simple,
                                                 GVariant      *paramter,
                                                 gpointer       user_data)
{
    auto page = GNC_PLUGIN_PAGE_ACCOUNT_TREE(user_data);
    GncPluginPageAccountTreePrivate *priv;

    g_return_if_fail(GNC_IS_PLUGIN_PAGE_ACCOUNT_TREE(page));
    ENTER("(action %p, page %p)", simple, page);

    priv = GNC_PLUGIN_PAGE_ACCOUNT_TREE_GET_PRIVATE(page);
    account_filter_dialog_create(&priv->fd, GNC_PLUGIN_PAGE(page));
    LEAVE(" ");
}

static void
gnc_plugin_page_account_tree_cmd_reconcile (GSimpleAction *simple,
                                            GVariant      *paramter,
                                            gpointer       user_data)
{
    auto page = GNC_PLUGIN_PAGE_ACCOUNT_TREE(user_data);
    GtkWidget *window;
    Account *account;

    account = gnc_plugin_page_account_tree_get_current_account (page);
    g_return_if_fail (account != NULL);

    /* To prevent mistakes involving saving an edited transaction after
     * finishing a reconciliation (reverting the reconcile state), we could look
     * at all open registers and determine if any of them have a transaction
     * being edited that involves the account to be reconciled.
     *
     * However, the reconcile window isn't modal so it's still possible to start
     * editing a transaction after opening it. Assume the user knows what
     * they're doing if they start a reconciliation from the account tree and
     * don't attempt to stop them.
     */

    window = GNC_PLUGIN_PAGE (page)->window;
    recnWindow (window, account);
}

static void
gnc_plugin_page_account_tree_cmd_autoclear (GSimpleAction *simple,
                                            GVariant      *paramter,
                                            gpointer       user_data)
{
    auto page = GNC_PLUGIN_PAGE_ACCOUNT_TREE(user_data);
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
gnc_plugin_page_account_tree_cmd_transfer (GSimpleAction *simple,
                                           GVariant      *paramter,
                                           gpointer       user_data)
{
    auto page = GNC_PLUGIN_PAGE_ACCOUNT_TREE(user_data);
    GtkWidget *window;
    Account *account;

    account = gnc_plugin_page_account_tree_get_current_account (page);
    window = GNC_PLUGIN_PAGE (page)->window;
    gnc_xfer_dialog (window, account);
}

static void
gnc_plugin_page_account_tree_cmd_stock_split (GSimpleAction *simple,
                                              GVariant      *paramter,
                                              gpointer       user_data)
{
    auto page = GNC_PLUGIN_PAGE_ACCOUNT_TREE(user_data);
    GtkWidget *window;
    Account *account;

    account = gnc_plugin_page_account_tree_get_current_account (page);
    window = GNC_PLUGIN_PAGE (page)->window;
    gnc_stock_split_dialog (window, account);
}

static void
gnc_plugin_page_account_tree_cmd_stock_assistant (GSimpleAction *simple,
                                                  GVariant      *paramter,
                                                  gpointer       user_data)
{
    auto page = GNC_PLUGIN_PAGE_ACCOUNT_TREE(user_data);
    Account *account;
    GtkWidget *window;

    ENTER ("(action %p, page %p)", simple, page);

    account = gnc_plugin_page_account_tree_get_current_account (page);
    window = GNC_PLUGIN_PAGE(page)->window;
    gnc_stock_transaction_assistant (window, account);

    LEAVE (" ");
}

static void
gnc_plugin_page_account_tree_cmd_edit_tax_options (GSimpleAction *simple,
                                                   GVariant      *paramter,
                                                   gpointer       user_data)
{
    auto page = GNC_PLUGIN_PAGE_ACCOUNT_TREE(user_data);
    GtkWidget *window;
    Account *account;

    account = gnc_plugin_page_account_tree_get_current_account (page);
    window = GNC_PLUGIN_PAGE (page)->window;
    gnc_tax_info_dialog (window, account);
}

static void
gnc_plugin_page_account_tree_cmd_lots (GSimpleAction *simple,
                                       GVariant      *paramter,
                                       gpointer       user_data)
{
    auto page = GNC_PLUGIN_PAGE_ACCOUNT_TREE(user_data);
    Account *account = gnc_plugin_page_account_tree_get_current_account (page);
    GtkWidget *window = GNC_PLUGIN_PAGE (page)->window;
    gnc_lot_viewer_dialog (GTK_WINDOW(window), account);
}

static void
scrub_abort_verify_finished (GtkWindow *parent, gint response, gpointer user_data)
{
    auto context = static_cast<GncScrubContext *> (user_data);
    (void)parent;
    if (response == GTK_RESPONSE_YES)
        gnc_scrub_context_cancel (context);
    gnc_scrub_context_unref (context);
}

static gboolean
scrub_kp_handler (GtkEventControllerKey *key, guint keyval,
                  guint keycode, GdkModifierType state,
                  gpointer user_data)
{
    (void)keycode;
    (void)state;
    auto context = static_cast<GncScrubContext *> (user_data);
    if (keyval != GDK_KEY_Escape)
        return FALSE;

    auto widget = gtk_event_controller_get_widget (GTK_EVENT_CONTROLLER (key));
    if (widget && GTK_IS_WINDOW (widget))
        gnc_verify_dialog_async (GTK_WINDOW (widget), FALSE,
                                 scrub_abort_verify_finished,
                                 gnc_scrub_context_ref (context),
                                 "%s", _(check_repair_abort_YN));
    return TRUE;
}

static void
gnc_plugin_page_account_tree_cmd_scrub (GSimpleAction *simple,
                                        GVariant      *paramter,
                                        gpointer       user_data)
{
    auto page = GNC_PLUGIN_PAGE_ACCOUNT_TREE(user_data);
    Account *account = gnc_plugin_page_account_tree_get_current_account (page);
    GncWindow *window;
    gulong scrub_kp_handler_ID;
    GncScrubContext *context;

    g_return_if_fail (account != NULL);

    context = prepare_scrubbing (
        page, qof_instance_get_book (QOF_INSTANCE (account)));
    if (!context)
        return;

    window = GNC_WINDOW(GNC_PLUGIN_PAGE (page)->window);

    GtkEventController *event_controller = gtk_event_controller_key_new ();
    gtk_widget_add_controller (GTK_WIDGET(window), event_controller);
    scrub_kp_handler_ID = g_signal_connect (G_OBJECT(event_controller), "key-pressed",
                                            G_CALLBACK(scrub_kp_handler), context);
    gnc_window_set_progressbar_window (window);

    xaccAccountScrubOrphansWithContext (account, gnc_window_show_progress, context);
    xaccAccountScrubImbalanceWithContext (account, gnc_window_show_progress, context);

    gncScrubBusinessAccountWithContext (account, gnc_window_show_progress, context);

    finish_scrubbing (page, window, event_controller, scrub_kp_handler_ID,
                      context);
    if (g_getenv("GNC_AUTO_SCRUB_LOTS") != NULL)
        start_lots_scrub_runner (account, FALSE, G_OBJECT (window));
}

static void
gnc_plugin_page_account_tree_cmd_scrub_sub (GSimpleAction *simple,
                                            GVariant      *paramter,
                                            gpointer       user_data)
{
    auto page = GNC_PLUGIN_PAGE_ACCOUNT_TREE(user_data);
    Account *account = gnc_plugin_page_account_tree_get_current_account (page);
    GncWindow *window;
    gulong scrub_kp_handler_ID;
    GncScrubContext *context;

    g_return_if_fail (account != NULL);

    context = prepare_scrubbing (
        page, qof_instance_get_book (QOF_INSTANCE (account)));
    if (!context)
        return;

    window = GNC_WINDOW(GNC_PLUGIN_PAGE (page)->window);

    GtkEventController *event_controller = gtk_event_controller_key_new ();
    gtk_widget_add_controller (GTK_WIDGET(window), event_controller);
    scrub_kp_handler_ID = g_signal_connect (G_OBJECT(event_controller), "key-pressed",
                                            G_CALLBACK(scrub_kp_handler), context);
    gnc_window_set_progressbar_window (window);

    xaccAccountTreeScrubOrphansWithContext (account, gnc_window_show_progress, context);
    xaccAccountTreeScrubImbalanceWithContext (account, gnc_window_show_progress, context);

    gncScrubBusinessAccountTreeWithContext (account, gnc_window_show_progress, context);

    finish_scrubbing (page, window, event_controller, scrub_kp_handler_ID,
                      context);
    if (g_getenv("GNC_AUTO_SCRUB_LOTS") != NULL)
        start_lots_scrub_runner (account, TRUE, G_OBJECT (window));
}

static void
gnc_plugin_page_account_tree_cmd_scrub_all (GSimpleAction *simple,
                                            GVariant      *paramter,
                                            gpointer       user_data)
{
    auto page = GNC_PLUGIN_PAGE_ACCOUNT_TREE(user_data);
    Account *root = gnc_get_current_root_account ();
    GncWindow *window;
    gulong scrub_kp_handler_ID;
    if (!root)
        return;
    GncScrubContext *context;

    context = prepare_scrubbing (
        page, qof_instance_get_book (QOF_INSTANCE (root)));
    if (!context)
        return;

    window = GNC_WINDOW(GNC_PLUGIN_PAGE (page)->window);
    GtkEventController *event_controller = gtk_event_controller_key_new ();
    gtk_widget_add_controller (GTK_WIDGET(window), event_controller);
    scrub_kp_handler_ID = g_signal_connect (G_OBJECT(event_controller), "key-pressed",
                                            G_CALLBACK(scrub_kp_handler), context);
    gnc_window_set_progressbar_window (window);

    xaccAccountTreeScrubOrphansWithContext (root, gnc_window_show_progress, context);
    xaccAccountTreeScrubImbalanceWithContext (root, gnc_window_show_progress, context);
    gncScrubBusinessAccountTreeWithContext (root, gnc_window_show_progress, context);

    finish_scrubbing (page, window, event_controller, scrub_kp_handler_ID,
                      context);
    if (g_getenv("GNC_AUTO_SCRUB_LOTS") != NULL)
        start_lots_scrub_runner (root, TRUE, G_OBJECT (window));
}

/** @} */
/** @} */
