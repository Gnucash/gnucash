/*
 * gnc-plugin-page-transactions.c --
 *
 * Copyright (C) 2005-2007 Chris Shoemaker <c.shoemaker@cox.net>
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


#include "config.h"

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include <glade/glade.h>
#include "gnc-date-edit.h"

#include "dialog-options.h"
#include "dialog-account.h"
#include "dialog-print-check.h"
#include "gnc-gnome-utils.h"
#include "gnc-html.h"
#include "gnc-icons.h"
#include "gnc-plugin-page-transactions.h"
#include "gnc-plugin-transactions.h"

#include "gnc-tree-view-account.h"
#include "gnc-ui.h"
#include "gnc-ui-util.h"
#include "gnc-main-window.h"
#include "gnc-component-manager.h"

#include "qof.h"

#include "gnc-dialog.h"
#include "Transaction.h"
#include "Split.h"

#define PLUGIN_PAGE_TRANSACTIONS_CM_CLASS "plugin-page-transactions"
#define GCONF_SECTION "window/pages/transactions"
typedef struct GncPluginPageTransactionsPrivate
{
    GtkActionGroup *action_group;
    //guint merge_id;
    //GtkUIManager *ui_merge;

    GtkWidget *widget;        /* ends up being a vbox */
    GncTreeViewTransaction *tv;

    gint component_id;

    //GUID key;
    //GncDialog* d;

} GncPluginPageTransactionsPrivate;

#define GET_PRIVATE(o) \
    (G_TYPE_INSTANCE_GET_PRIVATE ((o), GNC_TYPE_PLUGIN_PAGE_TRANSACTIONS, \
                                  GncPluginPageTransactionsPrivate))

/************************************************************
 *                        Prototypes                        *
 ************************************************************/
/* Plugin Actions */
static void
gnc_plugin_page_transactions_class_init(GncPluginPageTransactionsClass *klass);
static void gnc_plugin_page_transactions_init(GncPluginPageTransactions *plugin_page);
static void gnc_plugin_page_transactions_finalize(GObject *object);

static GtkWidget *
gnc_plugin_page_transactions_create_widget(GncPluginPage *plugin_page);
static void gnc_plugin_page_transactions_destroy_widget(GncPluginPage *plugin_page);
static void gnc_plugin_page_transactions_save_page(
    GncPluginPage *plugin_page, GKeyFile *file, const gchar *group);
static GncPluginPage *gnc_plugin_page_transactions_recreate_page(
    GtkWidget *window, GKeyFile *file, const gchar *group);

static gchar *gnc_plugin_page_transactions_get_tab_name(
    GncPluginPage *plugin_page);

//static void gnc_plugin_page_transactions_view_refresh (GncPluginPageTransactions *page);

/* Command Callbacks */
static void gppt_cmd_print_check(GtkAction *act, GncPluginPageTransactions *pp)
{
    Split *split;
    Transaction *trans;
    
    g_return_if_fail(GNC_IS_PLUGIN_PAGE_TRANSACTIONS(pp));
    split = gnc_tree_view_transaction_get_selected_split(GET_PRIVATE(pp)->tv);

    if (split) {
        gnc_ui_print_check_dialog_create(GNC_PLUGIN_PAGE(pp), split);
    }
}

static void gppt_cmd_cut(GtkAction *act, GncPluginPageTransactions *pp)
{;}
static void gppt_cmd_copy(GtkAction *act, GncPluginPageTransactions *pp)
{;}
static void gppt_cmd_paste(GtkAction *act, GncPluginPageTransactions *pp)
{;}
static void 
gppt_cmd_edit_account(GtkAction *act, GncPluginPageTransactions *pp)
{
    Account *acc = NULL;

    g_return_if_fail(GNC_IS_PLUGIN_PAGE_TRANSACTIONS(pp));
    /* IDEA: maybe this could edit the account for the selected split. */
    acc = gnc_tree_view_transaction_get_anchor(GET_PRIVATE(pp)->tv);
    
    if (acc)
        gnc_ui_edit_account_window(acc);
}

static void
gppt_cmd_cut_trans(GtkAction *act, GncPluginPageTransactions *pp)
{
    g_return_if_fail(GNC_IS_PLUGIN_PAGE_TRANSACTIONS(pp));
}
static void
gppt_cmd_copy_trans(GtkAction *act, GncPluginPageTransactions *pp)
{
    g_return_if_fail(GNC_IS_PLUGIN_PAGE_TRANSACTIONS(pp));
    gnc_tree_view_transaction_copy_trans_to_clipboard(GET_PRIVATE(pp)->tv);
}
static void
gppt_cmd_paste_trans(GtkAction *act, GncPluginPageTransactions *pp)
{
    g_return_if_fail(GNC_IS_PLUGIN_PAGE_TRANSACTIONS(pp));
    gnc_tree_view_transaction_paste_trans_from_clipboard(GET_PRIVATE(pp)->tv);
}
static void
gppt_cmd_duplicate_trans(GtkAction *act, GncPluginPageTransactions *pp) 
{
    g_return_if_fail(GNC_IS_PLUGIN_PAGE_TRANSACTIONS(pp));
}

static void 
gppt_cmd_delete_trans(GtkAction *act, GncPluginPageTransactions *pp) 
{
    g_return_if_fail(GNC_IS_PLUGIN_PAGE_TRANSACTIONS(pp));
    gnc_tree_view_transaction_delete_selected(GET_PRIVATE(pp)->tv);
}

static void 
gppt_cmd_reinit_trans(GtkAction *act, GncPluginPageTransactions *pp) 
{
    g_return_if_fail(GNC_IS_PLUGIN_PAGE_TRANSACTIONS(pp));
    gnc_tree_view_transaction_reinit_trans(GET_PRIVATE(pp)->tv);
}

static void 
gppt_cmd_enter_trans(GtkAction *act, GncPluginPageTransactions *pp) 
{
    g_return_if_fail(GNC_IS_PLUGIN_PAGE_TRANSACTIONS(pp));
    gnc_tree_view_transaction_enter(GET_PRIVATE(pp)->tv);    
}

static void 
gppt_cmd_cancel_trans(GtkAction *act, GncPluginPageTransactions *pp) 
{
    g_return_if_fail(GNC_IS_PLUGIN_PAGE_TRANSACTIONS(pp));
    gnc_tree_view_transaction_cancel_edit(GET_PRIVATE(pp)->tv);
}

static void
gppt_cmd_void_trans(GtkAction *act, GncPluginPageTransactions *pp) 
{
    g_return_if_fail(GNC_IS_PLUGIN_PAGE_TRANSACTIONS(pp));
    gnc_tree_view_transaction_void(GET_PRIVATE(pp)->tv);
}

static void 
gppt_cmd_unvoid_trans(GtkAction *act, GncPluginPageTransactions *pp) 
{
    g_return_if_fail(GNC_IS_PLUGIN_PAGE_TRANSACTIONS(pp));
    gnc_tree_view_transaction_unvoid(GET_PRIVATE(pp)->tv);
}

static void 
gppt_cmd_reverse_trans(GtkAction *act, GncPluginPageTransactions *pp)
{
    GncTreeViewTransaction *tv;
    Transaction *trans, *new_trans;

    g_return_if_fail(GNC_IS_PLUGIN_PAGE_TRANSACTIONS(pp));
    tv = GET_PRIVATE(pp)->tv;
    trans = gnc_tree_view_transaction_get_selected_trans(tv);

    if (trans) {
        if (xaccTransGetReversedBy(trans)) {
            gnc_error_dialog(gnc_plugin_page_get_window(GNC_PLUGIN_PAGE(pp)),
                             _("A reversing entry has already been created" 
                               " for this transaction."));
            return;
        }

        new_trans = xaccTransReverse(trans);
    }
    // FIXME: move cursor to reversed?
}

static void gppt_cmd_view_sort_by(GtkAction *act, GncPluginPageTransactions *pp) {;}
static void gppt_cmd_view_filter_by(GtkAction *act, GncPluginPageTransactions *pp) {;}
static void gppt_cmd_transfer(GtkAction *act, GncPluginPageTransactions *pp) {;}
static void gppt_cmd_reconcile(GtkAction *act, GncPluginPageTransactions *pp) {;}
static void gppt_cmd_stock_split(GtkAction *act, GncPluginPageTransactions *pp) {;}
//static void gppt_cmd_lots(GtkAction *act, GncPluginPageTransactions *pp) {;}

static void 
gppt_cmd_blank_trans(GtkAction *act, GncPluginPageTransactions *pp)
{
    g_return_if_fail(GNC_IS_PLUGIN_PAGE_TRANSACTIONS(pp));
    gnc_tree_view_transaction_goto_blank_trans(GET_PRIVATE(pp)->tv);
}

static void gppt_cmd_exchange_rate(GtkAction *act, GncPluginPageTransactions *pp) {;}

static void
gppt_cmd_jump(GtkAction *act, GncPluginPageTransactions *pp)
{
    Split *split;
    Account *acc;
    GncTreeModelTransaction *model;
    GncPluginPage *new_pp;
    GncTreeViewTransaction *tv, *jump_tv;

    g_return_if_fail(GNC_IS_PLUGIN_PAGE_TRANSACTIONS(pp));
    tv = GET_PRIVATE(pp)->tv;
    split = gnc_tree_view_transaction_get_selected_split(tv);
    if (xaccSplitGetAccount(split) == 
        gnc_tree_view_transaction_get_anchor(tv)) {
        split = xaccSplitGetOtherSplit(split);
    }
    acc = xaccSplitGetAccount(split);

    model = gnc_tree_model_transaction_new_from_account(acc);
    jump_tv = gnc_tree_view_transaction_new_with_model(model);
    g_object_unref(G_OBJECT(model));
    new_pp = gnc_plugin_page_transactions_new(jump_tv);
    
    gnc_tree_view_transaction_select_split(jump_tv, split);
}

static void gppt_cmd_schedule(GtkAction *act, GncPluginPageTransactions *pp) {;}
static void gppt_cmd_scrub_all(GtkAction *act, GncPluginPageTransactions *pp) {;}
static void gppt_cmd_scrub_current(GtkAction *act, GncPluginPageTransactions *pp) {;}
static void gppt_cmd_account_report(GtkAction *act, GncPluginPageTransactions *pp) {;}
static void gppt_cmd_transaction_report(GtkAction *act, GncPluginPageTransactions *pp) {;}


static GtkActionEntry gnc_plugin_page_transactions_actions [] =
{
    /* File menu */
    { "FilePrintAction", GTK_STOCK_PRINT, N_("_Print Check..."), "<control>p", NULL,
      G_CALLBACK (gppt_cmd_print_check) },
    
    /* Edit menu */
    
    { "EditCutAction", GTK_STOCK_CUT, N_("Cu_t"), NULL,
      NULL,
      G_CALLBACK (gppt_cmd_cut) },
    { "EditCopyAction", GTK_STOCK_COPY, N_("_Copy"), NULL,
      NULL,
      G_CALLBACK (gppt_cmd_copy) },
    { "EditPasteAction", GTK_STOCK_PASTE, N_("_Paste"), NULL,
      NULL,
      G_CALLBACK (gppt_cmd_paste) },
    { "EditEditAccountAction", GNC_STOCK_EDIT_ACCOUNT, N_("Edit Account"), "<control>e",
      N_("Edit the selected account"),
      G_CALLBACK (gppt_cmd_edit_account) },
    
    /* Transaction menu */
    
    { "CutTransactionAction", GTK_STOCK_CUT, N_("Cu_t Transaction"), "",
      N_("Cut the selected transaction into clipboard"),
      G_CALLBACK (gppt_cmd_cut_trans) },
    { "CopyTransactionAction", GTK_STOCK_COPY, N_("_Copy Transaction"), "",
      N_("Copy the selected transaction into clipboard"),
      G_CALLBACK (gppt_cmd_copy_trans) },
    { "PasteTransactionAction", GTK_STOCK_PASTE, N_("_Paste Transaction"), "",
      N_("Paste the transaction from the clipboard"),
      G_CALLBACK (gppt_cmd_paste_trans) },
    { "DuplicateTransactionAction", GTK_STOCK_COPY, N_("Dup_licate Transaction"), "",
      N_("Make a copy of the current transaction"),
      G_CALLBACK (gppt_cmd_duplicate_trans) },
    { "DeleteTransactionAction", GTK_STOCK_DELETE, N_("_Delete Transaction"), NULL,
      N_("Delete the current transaction"),
      G_CALLBACK (gppt_cmd_delete_trans) },
    { "RemoveTransactionSplitsAction", GTK_STOCK_CLEAR, N_("Remo_ve Transaction Splits"), NULL,
      N_("Remove all splits in the current transaction"),
      G_CALLBACK (gppt_cmd_reinit_trans) },
    { "RecordTransactionAction", GTK_STOCK_ADD, N_("_Enter Transaction"), NULL,
      N_("Record the current transaction"),
      G_CALLBACK (gppt_cmd_enter_trans) },
    { "CancelTransactionAction", GTK_STOCK_CANCEL, N_("Ca_ncel Transaction"), NULL,
      N_("Cancel the current transaction"),
      G_CALLBACK (gppt_cmd_cancel_trans) },
    { "VoidTransactionAction", NULL, N_("_Void Transaction"), NULL, NULL,
      G_CALLBACK (gppt_cmd_void_trans) },
    { "UnvoidTransactionAction", NULL, N_("_Unvoid Transaction"), NULL, NULL,
      G_CALLBACK (gppt_cmd_unvoid_trans) },
    { "ReverseTransactionAction", NULL, N_("Add _Reversing Transaction"), NULL, NULL,
      G_CALLBACK (gppt_cmd_reverse_trans) },
    
    /* View menu */
    
    { "ViewSortByAction", NULL, N_("_Sort By..."), NULL, NULL,
      G_CALLBACK (gppt_cmd_view_sort_by) },
    { "ViewFilterByAction", NULL, N_("_Filter By..."), NULL, NULL,
      G_CALLBACK (gppt_cmd_view_filter_by) },
    
    /* Actions menu */
    
    { "ActionsTransferAction", GNC_STOCK_TRANSFER, N_("_Transfer..."), "<control>t",
      N_("Transfer funds from one account to another"),
      G_CALLBACK (gppt_cmd_transfer) },
    { "ActionsReconcileAction", GTK_STOCK_INDEX, N_("_Reconcile..."), NULL,
      N_("Reconcile the selected account"),
      G_CALLBACK (gppt_cmd_reconcile) },
    { "ActionsStockSplitAction", NULL, N_("Stoc_k Split..."), NULL,
      N_("Record a stock split or a stock merger"),
      G_CALLBACK (gppt_cmd_stock_split) },
#ifdef LOTS_READY_FOR_SHOWTIME
    { "ActionsLotsAction", NULL, N_("_Lot Viewer..."), NULL,
      N_("Bring up the lot viewer/editor window"),
      G_CALLBACK (gppt_cmd_lots) },
#endif
    { "BlankTransactionAction", GTK_STOCK_GOTO_BOTTOM, N_("_Blank Transaction"), NULL,
      N_("Move to the blank transaction at the bottom of the register"),
      G_CALLBACK (gppt_cmd_blank_trans) },
    { "EditExchangeRateAction", NULL, N_("Edit E_xchange Rate"), NULL,
      N_("Exit the exchange rate for the current transaction"),
      G_CALLBACK (gppt_cmd_exchange_rate) },
    { "JumpTransactionAction", GNC_STOCK_JUMP_TO, N_("_Jump"), NULL,
      N_("Jump to the corresponding transaction in the other account"),
      G_CALLBACK (gppt_cmd_jump) },
    { "ScheduleTransactionAction", GNC_STOCK_SCHEDULE, N_("Sche_dule..."), NULL,
      N_("Create a Scheduled Transaction with the current transaction as a template"),
      G_CALLBACK (gppt_cmd_schedule) },
    { "ScrubAllAction", NULL, N_("_All transactions"), NULL,
      NULL,
      G_CALLBACK (gppt_cmd_scrub_all) },
    { "ScrubCurrentAction", NULL, N_("_This transaction"), NULL,
      NULL,
      G_CALLBACK (gppt_cmd_scrub_current) },
    
    /* Reports menu */
    
    { "ReportsAccountReportAction", NULL, N_("Account Report"), NULL,
      N_("Open a register report window for this transaction"),
      G_CALLBACK (gppt_cmd_account_report) },
    { "ReportsAcctTransReportAction", NULL, N_("Account Transaction Report"), NULL,
      N_("Open a register report window for this transaction"),
      G_CALLBACK (gppt_cmd_transaction_report) },
};
static guint gnc_plugin_page_transactions_n_actions =
    G_N_ELEMENTS (gnc_plugin_page_transactions_actions);
#if 0
static GtkToggleActionEntry toggle_entries[] = {
	{ "ViewStyleDoubleLineAction", NULL, N_("_Double Line"), NULL,
	  N_("Show two lines of information for each transaction"),
	  G_CALLBACK (gnc_plugin_page_register_cmd_style_double_line), FALSE },

	{ "SplitTransactionAction", GNC_STOCK_SPLIT_TRANS, N_("S_plit Transaction"), NULL,
	  N_("Show all splits in the current transaction"),
	  G_CALLBACK (gnc_plugin_page_register_cmd_expand_transaction), FALSE },
};
static guint n_toggle_entries = G_N_ELEMENTS (toggle_entries);

static GtkRadioActionEntry radio_entries_2 [] =
{
	/* Translators: This is a menu item in the View menu */
	{ "ViewStyleBasicAction", NULL, N_("_Basic Ledger"), NULL,
	  N_("Show transactions on one or two lines"), REG_STYLE_LEDGER },
	/* Translators: This is a menu item in the View menu */
	{ "ViewStyleAutoSplitAction", NULL, N_("_Auto-Split Ledger"), NULL,
	  N_("Show transactions on one or two lines and expand the current transaction"), REG_STYLE_AUTO_LEDGER },
	/* Translators: This is a menu item in the View menu */
	{ "ViewStyleJournalAction", NULL, N_("Transaction _Journal"), NULL,
	  N_("Show expanded transactions with all splits"), REG_STYLE_JOURNAL }
};
static guint n_radio_entries_2 = G_N_ELEMENTS (radio_entries_2);

/** View Style actions */
static const gchar *view_style_actions[] = {
	"ViewStyleBasicAction",
	"ViewStyleAutoSplitAction",
	"ViewStyleJournalAction",
	NULL
};
#endif

/** Short labels for use on the toolbar buttons. */
static action_toolbar_labels toolbar_labels[] = {
  { "ActionsTransferAction", 	  N_("Transfer") },
  { "RecordTransactionAction", 	  N_("Enter") },
  { "CancelTransactionAction", 	  N_("Cancel") },
  { "DeleteTransactionAction", 	  N_("Delete") },
  { "DuplicateTransactionAction", N_("Duplicate") },
  /*{ "SplitTransactionAction",     N_("Split") },*/
  { "ScheduleTransactionAction",  N_("Schedule") },
  { "BlankTransactionAction",     N_("Blank") },
  { "ActionsReconcileAction",     N_("Reconcile") },
  { NULL, NULL },
};

#if 0
struct status_action {
  const char *action_name;
  int value;
};
static struct status_action status_actions[] = {
  { "filter_status_reconciled",   CLEARED_RECONCILED },
  { "filter_status_cleared",      CLEARED_CLEARED },
  { "filter_status_voided",       CLEARED_VOIDED },
  { "filter_status_frozen",       CLEARED_FROZEN },
  { "filter_status_unreconciled", CLEARED_NO },
  { NULL, 0 },
};
#endif
#define CLEARED_VALUE "cleared_value"

static GObjectClass *parent_class = NULL;

GType
gnc_plugin_page_transactions_get_type (void)
{
    static GType gnc_plugin_page_transactions_type = 0;

    if (gnc_plugin_page_transactions_type == 0) {
        static const GTypeInfo our_info = {
            sizeof (GncPluginPageTransactionsClass),
            NULL,
            NULL,
            (GClassInitFunc) gnc_plugin_page_transactions_class_init,
            NULL,
            NULL,
            sizeof (GncPluginPageTransactions),
            0,
            (GInstanceInitFunc) gnc_plugin_page_transactions_init
        };

        gnc_plugin_page_transactions_type =
            g_type_register_static (GNC_TYPE_PLUGIN_PAGE,
                                    "GncPluginPageTransactions", &our_info, 0);
    }

    return gnc_plugin_page_transactions_type;
}

GncPluginPage *
gnc_plugin_page_transactions_new(GncTreeViewTransaction *tv)
{
    GncPluginPage *plugin_page;
    GncPluginPageTransactionsPrivate *priv;

    g_return_val_if_fail(GNC_IS_TREE_VIEW_TRANSACTION(tv), NULL);
    plugin_page = g_object_new(GNC_TYPE_PLUGIN_PAGE_TRANSACTIONS, NULL);
    priv = GET_PRIVATE(plugin_page);

    priv->tv = tv;
    priv->component_id = 0;

    { 
        gchar *str;
        str = gnc_plugin_page_transactions_get_tab_name(plugin_page);
        gnc_plugin_page_set_page_name(plugin_page, str);
        g_free(str);
    }
    //gnc_plugin_page_add_book(plugin_page, book) ???
    //priv->key = *gnc_transactions_get_guid(transactions);
    return plugin_page;
}

static void
gnc_plugin_page_transactions_class_init (GncPluginPageTransactionsClass *klass)
{
    GObjectClass *object_class = G_OBJECT_CLASS (klass);
    GncPluginPageClass *gnc_plugin_class = GNC_PLUGIN_PAGE_CLASS(klass);

    parent_class = g_type_class_peek_parent (klass);

    object_class->finalize = gnc_plugin_page_transactions_finalize;

    gnc_plugin_class->tab_icon        = GNC_STOCK_ACCOUNT;
    gnc_plugin_class->plugin_name     = GNC_PLUGIN_PAGE_TRANSACTIONS_NAME;
    gnc_plugin_class->create_widget   = gnc_plugin_page_transactions_create_widget;
    gnc_plugin_class->destroy_widget  = gnc_plugin_page_transactions_destroy_widget;
    gnc_plugin_class->save_page       = gnc_plugin_page_transactions_save_page;
    gnc_plugin_class->recreate_page   = gnc_plugin_page_transactions_recreate_page;

    g_type_class_add_private(klass, sizeof(GncPluginPageTransactionsPrivate));
}

static void
gnc_plugin_page_transactions_init (GncPluginPageTransactions *plugin_page)
{
    GtkActionGroup *action_group;
    GncPluginPageTransactionsPrivate *priv;
    GncPluginPage *parent;

    priv = GET_PRIVATE(plugin_page);

    /* Init parent declared variables */
    parent = GNC_PLUGIN_PAGE(plugin_page);
    g_object_set(G_OBJECT(plugin_page),
		 "page-name",      _("Transactions"),
		 "page-uri",       "default:",
		 "ui-description", "gnc-plugin-page-transactions-ui.xml",
		 NULL);

    /* change me when the system supports multiple books */
    gnc_plugin_page_add_book(parent, gnc_get_current_book());

    /* Create menu and toolbar information */
    action_group =
      gnc_plugin_page_create_action_group(parent,
					  "GncPluginPageTransactionsActions");
    gtk_action_group_add_actions (action_group,
                                  gnc_plugin_page_transactions_actions,
                                  gnc_plugin_page_transactions_n_actions,
                                  plugin_page);
    gnc_plugin_init_short_names (action_group, toolbar_labels);
}

static void
gnc_plugin_page_transactions_finalize (GObject *object)
{
    GncPluginPageTransactions *page;
    GncPluginPageTransactionsPrivate *priv;

    page = GNC_PLUGIN_PAGE_TRANSACTIONS (object);
    g_return_if_fail (GNC_IS_PLUGIN_PAGE_TRANSACTIONS (page));

    priv = GET_PRIVATE(page);

    G_OBJECT_CLASS (parent_class)->finalize (object);
}


/* Component Manager Callback Functions */
/*
static void
gnc_plugin_page_transactions_close_cb (gpointer user_data)
{
    GncPluginPage *page = GNC_PLUGIN_PAGE(user_data);
    gnc_main_window_close_page (page);
}

static void
gnc_plugin_page_transactions_refresh_cb(GHashTable *changes, 
                                        gpointer user_data)
{
    GncPluginPageTransactions *page;
    GncPluginPageTransactionsPrivate *priv;
    const EventInfo* ei;

    page = GNC_PLUGIN_PAGE_TRANSACTIONS(user_data);
    priv = GET_PRIVATE(page);
    if (changes) {
        ei = gnc_gui_get_entity_events(changes, &priv->key);
        if (ei) {
            if (ei->event_mask & QOF_EVENT_DESTROY) {
                gnc_plugin_page_transactions_close_cb(user_data);
                return;
            }
            if (ei->event_mask & QOF_EVENT_MODIFY) {
                DEBUG("refreshing transactions view because transactions was modified");
                gnc_plugin_page_transactions_view_refresh(page);
            }
        }
    }
}
*/

/*
 * GncPluginPage Fucntions
 */
static GtkWidget *
gnc_plugin_page_transactions_create_widget (GncPluginPage *plugin_page)
{
    GncPluginPageTransactions *page;
    GncPluginPageTransactionsPrivate *priv;
    //GtkTreeSelection *selection;
    GtkTreeView *tree_view;
    GtkWidget *scrolled_window;

    page = GNC_PLUGIN_PAGE_TRANSACTIONS(plugin_page);
    priv = GET_PRIVATE(page);
    if (priv->widget) {
        return priv->widget;
    }

    priv->widget = gtk_vbox_new(FALSE, 0);

    scrolled_window = gtk_scrolled_window_new (NULL, NULL);
    gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW (scrolled_window),
                                   GTK_POLICY_AUTOMATIC,
                                   GTK_POLICY_AUTOMATIC);
    gtk_widget_show(scrolled_window);
    gtk_box_pack_start(GTK_BOX (priv->widget), scrolled_window, TRUE, TRUE, 0);
    tree_view = GTK_TREE_VIEW(priv->tv);
    g_object_set(G_OBJECT(tree_view), "gconf-section", GCONF_SECTION, 
                 "show-column-menu", TRUE, NULL);
    g_signal_connect(G_OBJECT(tree_view), "button-press-event",
                     G_CALLBACK(gnc_tree_view_button_press_cb), plugin_page);

    gnc_tree_view_configure_columns(GNC_TREE_VIEW(tree_view));

    gtk_tree_view_set_headers_visible(tree_view, TRUE);
    gtk_widget_show(GTK_WIDGET (tree_view));
    gtk_container_add(GTK_CONTAINER(scrolled_window),
                      GTK_WIDGET(tree_view));
    gtk_widget_show(priv->widget);
    /*
    priv->component_id =
        gnc_register_gui_component(PLUGIN_PAGE_TRANSACTIONS_CM_CLASS,
                                   gnc_plugin_page_transactions_refresh_cb,
                                   gnc_plugin_page_transactions_close_cb,
                                   page);
    */
    //gnc_gui_component_set_session (priv->component_id,
    //                               gnc_get_current_session());
    /*
    gnc_gui_component_watch_entity (priv->component_id,
                                    gnc_transactions_get_guid(priv->transactions),
                                    QOF_EVENT_DESTROY | QOF_EVENT_MODIFY);
    */
    //gnc_plugin_page_transactions_view_refresh(page);

    return priv->widget;
}


#if 0
static void
gnc_plugin_page_register_ui_initial_state (GncPluginPageRegister *page)
{
    GncPluginPageRegisterPrivate *priv ;
    GtkActionGroup *action_group;
    GtkAction *action;
    Account *account;
    SplitRegister *reg;
    GNCLedgerDisplayType ledger_type;
    int i;
    
    priv = GET_PRIVATE(page);
    account = gnc_plugin_page_register_get_account (page);
    action_group = gnc_plugin_page_get_action_group(GNC_PLUGIN_PAGE(page));
    gnc_plugin_update_actions(action_group, actions_requiring_account,
                              "sensitive", account != NULL);
    
    /* Set "style" radio button */
    ledger_type = gnc_ledger_display_type(priv->ledger);
    gnc_plugin_update_actions(action_group, view_style_actions,
                              "sensitive", ledger_type == LD_SINGLE);
    
    reg = gnc_ledger_display_get_split_register(priv->ledger);
    for (i = n_radio_entries_2 - 1; i > 0; i--) {
        DEBUG(" index %d: comparing %x to %x", i, radio_entries_2[i].value, 
              reg->style);
        if (radio_entries_2[i].value == reg->style) {
	    DEBUG("match");
	    break;
        }
    }
    
    /* Either a match was found, or fell out with i = 0 */
    action = gtk_action_group_get_action(action_group, radio_entries_2[i].name);
    g_signal_handlers_block_by_func(action, gnc_plugin_page_register_cmd_style_changed, page);
    gtk_toggle_action_set_active(GTK_TOGGLE_ACTION(action), TRUE);
    g_signal_handlers_unblock_by_func(action, gnc_plugin_page_register_cmd_style_changed, page);
    
    /* Set "double line" toggle button */
    action = gtk_action_group_get_action (action_group,
                                          "ViewStyleDoubleLineAction");
    g_signal_handlers_block_by_func(action, gnc_plugin_page_register_cmd_style_double_line, page);
    gtk_toggle_action_set_active (GTK_TOGGLE_ACTION(action), reg->use_double_line);
    g_signal_handlers_unblock_by_func(action, gnc_plugin_page_register_cmd_style_double_line, page);
}
#endif

static void
gnc_plugin_page_transactions_destroy_widget (GncPluginPage *plugin_page)
{
    GncPluginPageTransactions *page;
    GncPluginPageTransactionsPrivate *priv;

    page = GNC_PLUGIN_PAGE_TRANSACTIONS (plugin_page);
    priv = GET_PRIVATE(plugin_page);
    if (priv->widget) {
        g_object_unref(G_OBJECT(priv->widget));
        priv->widget = NULL;
    }

    gnc_gui_component_clear_watches (priv->component_id);

    if (priv->component_id != NO_COMPONENT) {
        gnc_unregister_gui_component(priv->component_id);
        priv->component_id = NO_COMPONENT;
    }
}

#define TRANSACTIONS_GUID "Transactions GUID"

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
gnc_plugin_page_transactions_save_page (
    GncPluginPage *plugin_page, GKeyFile *key_file, const gchar *group_name)
{
    GncPluginPageTransactions *transactions_page;
    GncPluginPageTransactionsPrivate *priv;
    //char guid_str[GUID_ENCODING_LENGTH+1];
    
    g_return_if_fail (GNC_IS_PLUGIN_PAGE_TRANSACTIONS(plugin_page));
    g_return_if_fail (key_file != NULL);
    g_return_if_fail (group_name != NULL);

    transactions_page = GNC_PLUGIN_PAGE_TRANSACTIONS(plugin_page);
    priv = GET_PRIVATE(transactions_page);
    
    //guid_to_string_buff(gnc_transactions_get_guid(priv->transactions), guid_str);
    //g_key_file_set_string(key_file, group_name, TRANSACTIONS_GUID, guid_str);
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
gnc_plugin_page_transactions_recreate_page(
    GtkWidget *window, GKeyFile *key_file, const gchar *group_name)
{
#if 0
    GncPluginPageTransactions *transactions_page;
    GncPluginPageTransactionsPrivate *priv;
    GncPluginPage *page;
    GError *error = NULL;
    char *guid_str;
    GUID guid;
    //GncTransactions *bgt;
    QofBook *book;

    g_return_val_if_fail(key_file, NULL);
    g_return_val_if_fail(group_name, NULL);

    /* Create the new page. */
    page = gnc_plugin_page_transactions_new();
    transactions_page = GNC_PLUGIN_PAGE_TRANSACTIONS(page);
    priv = GET_PRIVATE(transactions_page);

    /* Install it now so we can then manipulate the created widget */
    gnc_main_window_open_page(GNC_MAIN_WINDOW(window), page);

    return page;
#else
    return NULL;
#endif
}

static gchar *
gnc_plugin_page_transactions_get_tab_name(GncPluginPage *plugin_page)
{
    GncPluginPageTransactionsPrivate *priv;
    Account *acc;

    g_return_val_if_fail(GNC_IS_PLUGIN_PAGE_TRANSACTIONS(plugin_page), 
                         _("unknown"));

    priv = GET_PRIVATE(plugin_page);
    acc = gnc_tree_view_transaction_get_anchor(priv->tv);

    if (acc)
        return g_strdup(xaccAccountGetName(acc));
    
    return g_strdup(_("unknown"));
}

/* Command callbacks */
/*
static void
gnc_plugin_page_transactions_view_refresh (GncPluginPageTransactions *page)
{
    GncPluginPageTransactionsPrivate *priv;

    g_return_if_fail(GNC_IS_PLUGIN_PAGE_TRANSACTIONS(page));
    priv = GET_PRIVATE(page);

}
*/

 /*
static void
gnc_plugin_page_transactions_cmd_print_check(
    GtkAction *action, GncPluginPageTransactions *plugin_page)
{
}
 */
