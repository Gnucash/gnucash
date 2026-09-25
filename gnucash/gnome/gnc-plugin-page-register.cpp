/**********************************************************************
 * gnc-plugin-page-register.c -- register page functions              *
 *                                                                    *
 * Copyright (C) 2003 Jan Arne Petersen <jpetersen@uni-bonn.de>       *
 * Copyright (C) 2003,2005,2006 David Hampton <hampton@employees.org> *
 * Copyright (C) 2011, Robert Fewell                                  *
 *                                                                    *
 * This program is free software; you can redistribute it and/or      *
 * modify it under the terms of the GNU General Public License as     *
 * published by the Free Software Foundation; either version 2 of     *
 * the License, or (at your option) any later version.                *
 *                                                                    *
 * This program is distributed in the hope that it will be useful,    *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of     *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      *
 * GNU General Public License for more details.                       *
 *                                                                    *
 * You should have received a copy of the GNU General Public License  *
 * along with this program; if not, contact:                          *
 *                                                                    *
 * Free Software Foundation           Voice:  +1-617-542-5942         *
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652         *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                     *
 **********************************************************************/

/** @addtogroup ContentPlugins
    @{ */
/** @addtogroup RegisterPlugin Register Page
    @{ */
/** @file gnc-plugin-page-register.c
    @brief  Functions providing a register page for the GnuCash UI
    @author Copyright (C) 2003 Jan Arne Petersen <jpetersen@uni-bonn.de>
    @author Copyright (C) 2003,2005 David Hampton <hampton@employees.org>
*/

#include <config.h>

#include <optional>

#include <gtk/gtk.h>
#include <libguile.h>
#include <glib/gi18n.h>
#include "swig-runtime.h"
#include "guile-mappings.h"

#include "gnc-plugin-page-register.h"
#include "gnc-plugin-register.h"
#include "gnc-plugin-menu-additions.h"
#include "gnc-plugin-page-report.h"
#include "gnc-plugin-business.h"

#include "dialog-account.h"
#include "dialog-dup-trans.h"
#include "dialog-find-account.h"
#include "dialog-find-transactions.h"
#include "dialog-print-check.h"
#include "dialog-invoice.h"
#include "dialog-transfer.h"
#include "dialog-utils.h"
#include "assistant-stock-split.h"
#include "assistant-stock-transaction.h"
#include "gnc-component-manager.h"
#include "gnc-date.h"
#include "gnc-date-edit.h"
#include "gnc-engine.h"
#include "gnc-event.h"
#include "gnc-features.h"
#include "gnc-string-utils.h"
#include "gnc-gnome-utils.h"
#include "gnc-gobject-utils.h"
#include "gnc-gui-query.h"
#include "gnc-icons.h"
#include "gnc-split-reg.h"
#include "gnc-state.h"
#include "gnc-prefs.h"
#include "gnc-ui-util.h"
#include "gnc-window.h"
#include "gnc-main-window.h"
#include "gnc-session.h"
#include "gnc-ui.h"
#include "gnc-warnings.h"
#include "gnucash-sheet.h"
#include "dialog-lot-viewer.h"
#include "Scrub.h"
#include "ScrubBusiness.h"
#include "qof.h"
#include "window-reconcile.h"
#include "window-autoclear.h"
#include "window-report.h"
#include "engine-helpers.h"
#include "qofbookslots.h"
#include "gnc-gtk-utils.h"

/* gschema: org.gnucash.GnuCash.general.register.JumpMultipleSplits */
typedef enum : gint
{
    JUMP_DEFAULT = 0, /* Do nothing */
    JUMP_LARGEST_VALUE_FIRST_SPLIT = 1,
    JUMP_SMALLEST_VALUE_FIRST_SPLIT = 2,
} GncPrefJumpMultSplits;

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_GUI;

#define DEFAULT_LINES_AMOUNT         50

static void gnc_plugin_page_register_finalize (GObject* object);

/* static Account *gnc_plugin_page_register_get_current_account (GncPluginPageRegister *page); */

static GtkWidget* gnc_plugin_page_register_create_widget (GncPluginPage*
                                                          plugin_page);
static void gnc_plugin_page_register_destroy_widget (GncPluginPage*
                                                     plugin_page);
static void gnc_plugin_page_register_window_changed (GncPluginPage*
                                                     plugin_page, GtkWidget* window);
static gboolean gnc_plugin_page_register_focus_widget (GncPluginPage*
                                                       plugin_page);
static void gnc_plugin_page_register_focus (GncPluginPage* plugin_page,
                                            gboolean current_page);
static void gnc_plugin_page_register_save_page (GncPluginPage* plugin_page,
                                                GKeyFile* file, const gchar* group);
static GncPluginPage* gnc_plugin_page_register_recreate_page (GtkWidget* window,
                                                              GKeyFile* file,
                                                              const gchar* group);
static void gnc_plugin_page_register_update_edit_menu (GncPluginPage* plugin_page,
                                                       gboolean hide);
typedef struct FinishPendingRequest FinishPendingRequest;
typedef struct VoidTransactionRequest VoidTransactionRequest;
typedef void (*GncPluginPageRegisterPendingCallback) (GncPluginPageRegister* page,
                                                        gboolean accepted,
                                                        gpointer user_data);

static void gnc_plugin_page_register_finish_pending_async_virtual
    (GncPluginPage* plugin_page, GCancellable* cancellable,
     GncPluginPagePendingCallback callback, gpointer user_data);
static void gnc_plugin_page_register_finish_pending_async
    (GncPluginPageRegister* page, GCancellable* cancellable,
     GncPluginPageRegisterPendingCallback callback, gpointer user_data,
     GDestroyNotify user_data_destroy);
static void finish_pending_request_cancel (FinishPendingRequest* request);
static void void_transaction_request_cancel (VoidTransactionRequest* request);

static gchar* gnc_plugin_page_register_get_tab_name (GncPluginPage*
                                                     plugin_page);
static gchar* gnc_plugin_page_register_get_tab_color (GncPluginPage*
                                                      plugin_page);
static gchar* gnc_plugin_page_register_get_long_name (GncPluginPage*
                                                      plugin_page);

static void gnc_plugin_page_register_summarybar_position_changed (gpointer prefs,
                                                                  gchar* pref,
                                                                  gpointer user_data);

/* Command callbacks */
static void gnc_plugin_page_register_cmd_print_check (GSimpleAction *simple, GVariant *paramter, gpointer user_data);
static void gnc_plugin_page_register_cmd_cut (GSimpleAction *simple, GVariant *paramter, gpointer user_data);
static void gnc_plugin_page_register_cmd_copy (GSimpleAction *simple, GVariant *paramter, gpointer user_data);
static void gnc_plugin_page_register_cmd_paste (GSimpleAction *simple, GVariant *paramter, gpointer user_data);
static void gnc_plugin_page_register_cmd_edit_account (GSimpleAction *simple, GVariant *paramter, gpointer user_data);
static void gnc_plugin_page_register_cmd_find_account (GSimpleAction *simple, GVariant *paramter, gpointer user_data);
static void gnc_plugin_page_register_cmd_find_transactions (GSimpleAction *simple, GVariant *paramter, gpointer user_data);
static void gnc_plugin_page_register_cmd_edit_tax_options (GSimpleAction *simple, GVariant *paramter, gpointer user_data);
static void gnc_plugin_page_register_cmd_cut_transaction (GSimpleAction *simple, GVariant *paramter, gpointer user_data);
static void gnc_plugin_page_register_cmd_copy_transaction (GSimpleAction *simple, GVariant *paramter, gpointer user_data);
static void gnc_plugin_page_register_cmd_paste_transaction (GSimpleAction *simple, GVariant *paramter, gpointer user_data);
static void gnc_plugin_page_register_cmd_void_transaction (GSimpleAction *simple, GVariant *paramter, gpointer user_data);
static void gnc_plugin_page_register_cmd_unvoid_transaction (GSimpleAction *simple, GVariant *paramter, gpointer user_data);
static void gnc_plugin_page_register_cmd_reverse_transaction (GSimpleAction *simple, GVariant *paramter, gpointer user_data);
static void gnc_plugin_page_register_cmd_view_sort_by (GSimpleAction *simple, GVariant *paramter, gpointer user_data);
static void gnc_plugin_page_register_cmd_view_filter_by (GSimpleAction *simple, GVariant *paramter, gpointer user_data);

static void gnc_plugin_page_register_cmd_style_changed (GSimpleAction *simple, GVariant *parameter, gpointer user_data);
static void gnc_plugin_page_register_cmd_style_double_line (GSimpleAction *simple, GVariant *parameter, gpointer user_data);
static void gnc_plugin_page_register_cmd_expand_transaction (GSimpleAction *simple, GVariant *parameter, gpointer user_data);

static void gnc_plugin_page_register_cmd_reconcile (GSimpleAction *simple, GVariant *paramter, gpointer user_data);
static void gnc_plugin_page_register_cmd_stock_assistant (GSimpleAction *simple, GVariant *paramter, gpointer user_data);
static void gnc_plugin_page_register_cmd_autoclear (GSimpleAction *simple, GVariant *paramter, gpointer user_data);
static void gnc_plugin_page_register_cmd_transfer (GSimpleAction *simple, GVariant *paramter, gpointer user_data);
static void gnc_plugin_page_register_cmd_stock_split (GSimpleAction *simple, GVariant *paramter, gpointer user_data);
static void gnc_plugin_page_register_cmd_lots (GSimpleAction *simple, GVariant *paramter, gpointer user_data);
static void gnc_plugin_page_register_cmd_enter_transaction (GSimpleAction *simple, GVariant *paramter, gpointer user_data);
static void gnc_plugin_page_register_cmd_cancel_transaction (GSimpleAction *simple, GVariant *paramter, gpointer user_data);
static void gnc_plugin_page_register_cmd_delete_transaction (GSimpleAction *simple, GVariant *paramter, gpointer user_data);
static void gnc_plugin_page_register_cmd_blank_transaction (GSimpleAction *simple, GVariant *paramter, gpointer user_data);
static void gnc_plugin_page_register_cmd_goto_date (GSimpleAction *simple, GVariant *paramter, gpointer user_data);
static void gnc_plugin_page_register_cmd_duplicate_transaction (GSimpleAction *simple, GVariant *paramter, gpointer user_data);
static void gnc_plugin_page_register_cmd_reinitialize_transaction (GSimpleAction *simple, GVariant *paramter, gpointer user_data);
static void gnc_plugin_page_register_cmd_exchange_rate (GSimpleAction *simple, GVariant *paramter, gpointer user_data);
static void gnc_plugin_page_register_cmd_jump (GSimpleAction *simple, GVariant *paramter, gpointer user_data);
static void gnc_plugin_page_register_cmd_reload (GSimpleAction *simple, GVariant *paramter, gpointer user_data);
static void gnc_plugin_page_register_cmd_schedule (GSimpleAction *simple, GVariant *paramter, gpointer user_data);
static void gnc_plugin_page_register_cmd_scrub_all (GSimpleAction *simple, GVariant *paramter, gpointer user_data);
static void gnc_plugin_page_register_cmd_scrub_current (GSimpleAction *simple, GVariant *paramter, gpointer user_data);
static void gnc_plugin_page_register_cmd_account_report (GSimpleAction *simple, GVariant *paramter, gpointer user_data);
static void gnc_plugin_page_register_cmd_transaction_report (GSimpleAction *simple, GVariant *paramter, gpointer user_data);
static void gnc_plugin_page_register_cmd_linked_transaction (GSimpleAction *simple, GVariant *paramter, gpointer user_data);
static void gnc_plugin_page_register_cmd_linked_transaction_open (GSimpleAction *simple, GVariant *paramter, gpointer user_data);
static void gnc_plugin_page_register_cmd_jump_linked_invoice (GSimpleAction *simple, GVariant *paramter, gpointer user_data);

static void gnc_plugin_page_help_changed_cb (GNCSplitReg* gsr,
                                             GncPluginPageRegister* page);
static void gnc_plugin_page_popup_menu_cb (GNCSplitReg* gsr,
                                           GncPluginPageRegister* page);
static void gnc_plugin_page_register_refresh_cb (GHashTable* changes,
                                                 gpointer user_data);
static void gnc_plugin_page_register_close_cb (gpointer user_data);

static void gnc_plugin_page_register_ui_update (gpointer various,
                                                GncPluginPageRegister* page);
static void gppr_account_destroy_cb (Account* account);
static void gnc_plugin_page_register_event_handler (QofInstance* entity,
                                                    QofEventId event_type,
                                                    GncPluginPageRegister* page,
                                                    GncEventData* ed);

static GncInvoice* invoice_from_split (Split* split);
static bool find_after_date (Split *split, time64 *find_date);

/************************************************************/
/*                          Actions                         */
/************************************************************/

#define CUT_TRANSACTION_LABEL            N_("Cu_t Transaction")
#define COPY_TRANSACTION_LABEL           N_("_Copy Transaction")
#define PASTE_TRANSACTION_LABEL          N_("_Paste Transaction")
#define DUPLICATE_TRANSACTION_LABEL      N_("Dup_licate Transaction")
#define DELETE_TRANSACTION_LABEL         N_("_Delete Transaction")
/* Translators: This is a menu item that opens a dialog for linking an
   external file or URL with the bill, invoice, transaction, or voucher or
   removing such an link. */
#define LINK_TRANSACTION_LABEL           N_("_Manage Document Link…")
/* Translators: This is a menu item that opens an external file or URI that may
   be linked to the current bill, invoice, transaction, or voucher using
   the operating system's default application for the file or URI mime type. */
#define LINK_TRANSACTION_OPEN_LABEL      N_("_Open Linked Document")
/* Translators: This is a menu item that will open the bill, invoice, or voucher
   that is posted to the current transaction if there is one. */
#define JUMP_LINKED_INVOICE_LABEL        N_("Jump to Business item")
#define CUT_SPLIT_LABEL                  N_("Cu_t Split")
#define COPY_SPLIT_LABEL                 N_("_Copy Split")
#define PASTE_SPLIT_LABEL                N_("_Paste Split")
#define DUPLICATE_SPLIT_LABEL            N_("Dup_licate Split")
#define DELETE_SPLIT_LABEL               N_("_Delete Split")
#define CUT_TRANSACTION_TIP              N_("Cut the selected transaction into clipboard")
#define COPY_TRANSACTION_TIP             N_("Copy the selected transaction into clipboard")
#define PASTE_TRANSACTION_TIP            N_("Paste the transaction from the clipboard")
#define DUPLICATE_TRANSACTION_TIP        N_("Make a copy of the current transaction")
#define DELETE_TRANSACTION_TIP           N_("Delete the current transaction")
#define LINK_TRANSACTION_TIP             N_("Add, change, or unlink the document linked with the current transaction")
#define LINK_TRANSACTION_OPEN_TIP        N_("Open the linked document for the current transaction")
#define JUMP_LINKED_INVOICE_TIP          N_("Jump to the linked invoice, bill, expense or credit note")
#define CUT_SPLIT_TIP                    N_("Cut the selected split into clipboard")
#define COPY_SPLIT_TIP                   N_("Copy the selected split into clipboard")
#define PASTE_SPLIT_TIP                  N_("Paste the split from the clipboard")
#define DUPLICATE_SPLIT_TIP              N_("Make a copy of the current split")
#define DELETE_SPLIT_TIP                 N_("Delete the current split")

static GActionEntry gnc_plugin_page_register_actions [] =
{
    { "FilePrintAction", gnc_plugin_page_register_cmd_print_check, NULL, NULL, NULL },
    { "EditCutAction", gnc_plugin_page_register_cmd_cut, NULL, NULL, NULL },
    { "EditCopyAction", gnc_plugin_page_register_cmd_copy, NULL, NULL, NULL },
    { "EditPasteAction", gnc_plugin_page_register_cmd_paste, NULL, NULL, NULL },
    { "EditEditAccountAction", gnc_plugin_page_register_cmd_edit_account, NULL, NULL, NULL },
    { "EditFindAccountAction", gnc_plugin_page_register_cmd_find_account, NULL, NULL, NULL },
    { "EditFindTransactionsAction", gnc_plugin_page_register_cmd_find_transactions, NULL, NULL, NULL },
    { "EditTaxOptionsAction", gnc_plugin_page_register_cmd_edit_tax_options, NULL, NULL, NULL },
    { "CutTransactionAction", gnc_plugin_page_register_cmd_cut_transaction, NULL, NULL, NULL },
    { "CopyTransactionAction", gnc_plugin_page_register_cmd_copy_transaction, NULL, NULL, NULL },
    { "PasteTransactionAction", gnc_plugin_page_register_cmd_paste_transaction, NULL, NULL, NULL },
    { "DuplicateTransactionAction", gnc_plugin_page_register_cmd_duplicate_transaction, NULL, NULL, NULL },
    { "DeleteTransactionAction", gnc_plugin_page_register_cmd_delete_transaction, NULL, NULL, NULL },
    { "RemoveTransactionSplitsAction", gnc_plugin_page_register_cmd_reinitialize_transaction, NULL, NULL, NULL },
    { "RecordTransactionAction", gnc_plugin_page_register_cmd_enter_transaction, NULL, NULL, NULL },
    { "CancelTransactionAction", gnc_plugin_page_register_cmd_cancel_transaction, NULL, NULL, NULL },
    { "VoidTransactionAction", gnc_plugin_page_register_cmd_void_transaction, NULL, NULL, NULL },
    { "UnvoidTransactionAction", gnc_plugin_page_register_cmd_unvoid_transaction, NULL, NULL, NULL },
    { "ReverseTransactionAction", gnc_plugin_page_register_cmd_reverse_transaction, NULL, NULL, NULL },
    { "LinkTransactionAction", gnc_plugin_page_register_cmd_linked_transaction, NULL, NULL, NULL },
    { "LinkedTransactionOpenAction", gnc_plugin_page_register_cmd_linked_transaction_open, NULL, NULL, NULL },
    { "JumpLinkedInvoiceAction", gnc_plugin_page_register_cmd_jump_linked_invoice, NULL, NULL, NULL },
    { "ViewSortByAction", gnc_plugin_page_register_cmd_view_sort_by, NULL, NULL, NULL },
    { "ViewFilterByAction", gnc_plugin_page_register_cmd_view_filter_by, NULL, NULL, NULL },
    { "ViewRefreshAction", gnc_plugin_page_register_cmd_reload, NULL, NULL, NULL },
    { "ActionsTransferAction", gnc_plugin_page_register_cmd_transfer, NULL, NULL, NULL },
    { "ActionsReconcileAction", gnc_plugin_page_register_cmd_reconcile, NULL, NULL, NULL },
    { "ActionsAutoClearAction", gnc_plugin_page_register_cmd_autoclear, NULL, NULL, NULL },
    { "ActionsStockAssistantAction", gnc_plugin_page_register_cmd_stock_assistant, NULL, NULL, NULL },
    { "ActionsStockSplitAction", gnc_plugin_page_register_cmd_stock_split, NULL, NULL, NULL },
    { "ActionsLotsAction", gnc_plugin_page_register_cmd_lots, NULL, NULL, NULL },
    { "BlankTransactionAction", gnc_plugin_page_register_cmd_blank_transaction, NULL, NULL, NULL },
    { "GotoDateAction", gnc_plugin_page_register_cmd_goto_date, NULL, NULL, NULL },
    { "EditExchangeRateAction", gnc_plugin_page_register_cmd_exchange_rate, NULL, NULL, NULL },
    { "JumpTransactionAction", gnc_plugin_page_register_cmd_jump, NULL, NULL, NULL },
    { "ScheduleTransactionAction", gnc_plugin_page_register_cmd_schedule, NULL, NULL, NULL },
    { "ScrubAllAction", gnc_plugin_page_register_cmd_scrub_all, NULL, NULL, NULL },
    { "ScrubCurrentAction", gnc_plugin_page_register_cmd_scrub_current, NULL, NULL, NULL },
    { "ReportsAccountReportAction", gnc_plugin_page_register_cmd_account_report, NULL, NULL, NULL },
    { "ReportsAcctTransReportAction", gnc_plugin_page_register_cmd_transaction_report, NULL, NULL, NULL },

    { "ViewStyleDoubleLineAction", gnc_plugin_page_register_cmd_style_double_line, NULL, "false", NULL },
    { "SplitTransactionAction", gnc_plugin_page_register_cmd_expand_transaction, NULL, "false", NULL },
    { "ViewStyleRadioAction", gnc_plugin_page_register_cmd_style_changed, "i", "@i 0", NULL },
};
static guint gnc_plugin_page_register_n_actions = G_N_ELEMENTS(gnc_plugin_page_register_actions);

/** The default menu items that need to be add to the menu */
static const gchar *gnc_plugin_load_ui_items [] =
{
    "FilePlaceholder3",
    "EditPlaceholder1",
    "EditPlaceholder2",
    "EditPlaceholder3",
    "EditPlaceholder5",
    "ViewPlaceholder1",
    "ViewPlaceholder2",
    "ViewPlaceholder3",
    "ViewPlaceholder4",
    "TransPlaceholder0",
    "TransPlaceholder1",
    "TransPlaceholder2",
    "TransPlaceholder3",
    "TransPlaceholder4",
    "ActionsPlaceholder4",
    "ActionsPlaceholder5",
    "ActionsPlaceholder6",
    "ReportsPlaceholder1",
    NULL,
};

/** Actions that require an account to be selected before they are
 *  enabled. */
static const gchar* actions_requiring_account[] =
{
    "EditEditAccountAction",
    "ActionsReconcileAction",
    "ActionsAutoClearAction",
    "ActionsLotsAction",
    NULL
};

static const gchar* actions_requiring_priced_account[] =
{
    "ActionsStockAssistantAction",
    NULL
};

/** Short labels for use on the toolbar buttons. */
static GncToolBarShortNames toolbar_labels[] =
{
    { "ActionsTransferAction",              N_ ("Transfer") },
    { "RecordTransactionAction",            N_ ("Enter") },
    { "CancelTransactionAction",            N_ ("Cancel") },
    { "DeleteTransactionAction",            N_ ("Delete") },
    { "DuplicateTransactionAction",         N_ ("Duplicate") },
    { "SplitTransactionAction",
      /* Translators: This is the label of a toolbar button. So keep it short. */
      N_ ("Show Splits") },
    { "JumpTransactionAction",              N_ ("Jump") },
    { "ScheduleTransactionAction",          N_ ("Schedule") },
    { "BlankTransactionAction",             N_ ("Blank") },
    { "ActionsReconcileAction",             N_ ("Reconcile") },
    { "ActionsStockAssistantAction",        N_ ("Stock Assistant") },
    { NULL, NULL },
};

/************************************************************/
/*                      Data Structures                     */
/************************************************************/

typedef struct GncPluginPageRegisterPrivate
{
    GNCLedgerDisplay* ledger;
    GNCSplitReg* gsr;

    GtkWidget* widget;

    gint event_handler_id;
    gint component_manager_id;
    GncGUID key;  /* The guid of the Account we're watching */

    gint lines_default;
    gboolean read_only;
    gboolean page_focus;
    gboolean enable_refresh; // used to reduce ledger display refreshes
    Query* search_query;     // saved search query for comparison
    Query* filter_query;     // saved filter query for comparison
    FinishPendingRequest* finish_pending_request;
    VoidTransactionRequest* void_transaction_request;
    GncScrubContext* scrub_context;

    SortData sd;
    FilterData fd;

} GncPluginPageRegisterPrivate;

G_DEFINE_TYPE_WITH_PRIVATE (GncPluginPageRegister, gnc_plugin_page_register,
                            GNC_TYPE_PLUGIN_PAGE)

#define GNC_PLUGIN_PAGE_REGISTER_GET_PRIVATE(o)  \
   ((GncPluginPageRegisterPrivate*)gnc_plugin_page_register_get_instance_private((GncPluginPageRegister*)o))

/************************************************************/
/*                      Implementation                      */
/************************************************************/

static GncPluginPage*
gnc_plugin_page_register_new_common (GNCLedgerDisplay* ledger)
{
    GncPluginPageRegister* page;
    GncPluginPageRegisterPrivate* priv;
    GncPluginPage* plugin_page;
    GNCSplitReg* gsr;
    const GList* item;
    GList* book_list;
    gchar* label;
    gchar* label_color;
    QofQuery* q;

    // added for version 4.0 onwards
    if (!gnc_features_check_used (gnc_get_current_book(), GNC_FEATURE_REG_SORT_FILTER))
        gnc_features_set_used (gnc_get_current_book(), GNC_FEATURE_REG_SORT_FILTER);

    // added for version 4.14 onwards
    if (!gnc_using_equity_type_opening_balance_account (gnc_get_current_book()))
        gnc_set_use_equity_type_opening_balance_account (gnc_get_current_book());

    /* Is there an existing page? */
    gsr = GNC_SPLIT_REG(gnc_ledger_display_get_user_data (ledger));
    if (gsr)
    {
        item = gnc_gobject_tracking_get_list (GNC_PLUGIN_PAGE_REGISTER_NAME);
        for (; item; item = g_list_next (item))
        {
            page = (GncPluginPageRegister*)item->data;
            priv = GNC_PLUGIN_PAGE_REGISTER_GET_PRIVATE (page);
            if (priv->gsr == gsr)
                return GNC_PLUGIN_PAGE (page);
        }
    }

    page = GNC_PLUGIN_PAGE_REGISTER(g_object_new (GNC_TYPE_PLUGIN_PAGE_REGISTER, nullptr));
    priv = GNC_PLUGIN_PAGE_REGISTER_GET_PRIVATE (page);
    priv->ledger = ledger;
    priv->key = *guid_null();

    plugin_page = GNC_PLUGIN_PAGE (page);
    label = gnc_plugin_page_register_get_tab_name (plugin_page);
    gnc_plugin_page_set_page_name (plugin_page, label);
    g_free (label);

    label_color = gnc_plugin_page_register_get_tab_color (plugin_page);
    gnc_plugin_page_set_page_color (plugin_page, label_color);
    g_free (label_color);

    label = gnc_plugin_page_register_get_long_name (plugin_page);
    gnc_plugin_page_set_page_long_name (plugin_page, label);
    g_free (label);

    q = gnc_ledger_display_get_query (ledger);
    book_list = qof_query_get_books (q);
    for (item = book_list; item; item = g_list_next (item))
        gnc_plugin_page_add_book (plugin_page, (QofBook*)item->data);
    // Do not free the list. It is owned by the query.

    priv->component_manager_id = 0;
    return plugin_page;
}

static gpointer
gnc_plug_page_register_check_commodity (Account* account, void* usr_data)
{
    // Check that account's commodity matches the commodity in usr_data
    gnc_commodity* com0 = (gnc_commodity*) usr_data;
    gnc_commodity* com1 = xaccAccountGetCommodity (account);
    return gnc_commodity_equal (com1, com0) ? NULL : com1;
}

GncPluginPage*
gnc_plugin_page_register_new (Account* account, gboolean subaccounts)
{
    GNCLedgerDisplay* ledger;
    GncPluginPage* plugin_page;
    GncPluginPageRegisterPrivate* priv;
    gnc_commodity* com0;
    gnc_commodity* com1;

    ENTER ("account=%p, subaccounts=%s", account,
           subaccounts ? "TRUE" : "FALSE");

    com0 = gnc_account_get_currency_or_parent (account);
    com1 = GNC_COMMODITY(gnc_account_foreach_descendant_until (account,
                                                               gnc_plug_page_register_check_commodity,
                                                               static_cast<gpointer>(com0)));

    if (subaccounts)
        ledger = gnc_ledger_display_subaccounts (account, com1 != NULL);
    else
        ledger = gnc_ledger_display_simple (account);

    plugin_page = gnc_plugin_page_register_new_common (ledger);
    priv = GNC_PLUGIN_PAGE_REGISTER_GET_PRIVATE (plugin_page);
    priv->key = *xaccAccountGetGUID (account);

    LEAVE ("%p", plugin_page);
    return plugin_page;
}

GncPluginPage*
gnc_plugin_page_register_new_gl (void)
{
    GNCLedgerDisplay* ledger;

    ledger = gnc_ledger_display_gl();
    return gnc_plugin_page_register_new_common (ledger);
}

GncPluginPage*
gnc_plugin_page_register_new_ledger (GNCLedgerDisplay* ledger)
{
    return gnc_plugin_page_register_new_common (ledger);
}

static void
gnc_plugin_page_register_class_init (GncPluginPageRegisterClass* klass)
{
    GObjectClass* object_class = G_OBJECT_CLASS (klass);
    GncPluginPageClass* gnc_plugin_class = GNC_PLUGIN_PAGE_CLASS (klass);

    object_class->finalize = gnc_plugin_page_register_finalize;

    gnc_plugin_class->tab_icon        = GNC_ICON_ACCOUNT;
    gnc_plugin_class->plugin_name     = GNC_PLUGIN_PAGE_REGISTER_NAME;
    gnc_plugin_class->create_widget   = gnc_plugin_page_register_create_widget;
    gnc_plugin_class->destroy_widget  = gnc_plugin_page_register_destroy_widget;
    gnc_plugin_class->window_changed  = gnc_plugin_page_register_window_changed;
    gnc_plugin_class->focus_page      = gnc_plugin_page_register_focus;
    gnc_plugin_class->save_page       = gnc_plugin_page_register_save_page;
    gnc_plugin_class->recreate_page   = gnc_plugin_page_register_recreate_page;
    gnc_plugin_class->update_edit_menu_actions = gnc_plugin_page_register_update_edit_menu;
    gnc_plugin_class->finish_pending_async = gnc_plugin_page_register_finish_pending_async_virtual;
    gnc_plugin_class->focus_page_function = gnc_plugin_page_register_focus_widget;

    gnc_ui_register_account_destroy_callback (gppr_account_destroy_cb);
}

static void
gnc_plugin_page_register_init (GncPluginPageRegister* page)
{
    GncPluginPageRegisterPrivate* priv;
    GncPluginPage* plugin_page;
    GSimpleActionGroup *simple_action_group;
    gboolean use_new;

    priv = GNC_PLUGIN_PAGE_REGISTER_GET_PRIVATE (page);

    /* Init parent declared variables */
    plugin_page = GNC_PLUGIN_PAGE (page);
    use_new = gnc_prefs_get_bool (GNC_PREFS_GROUP_GENERAL_REGISTER,
                                  GNC_PREF_USE_NEW);
    g_object_set (G_OBJECT (page),
                  "page-name",      _ ("General Journal"),
                  "ui-description", "gnc-plugin-page-register.ui",
                  "use-new-window", use_new,
                  NULL);

    /* Create menu and toolbar information */
    simple_action_group = gnc_plugin_page_create_action_group (plugin_page,
                                                               "GncPluginPageRegisterActions");
    g_action_map_add_action_entries (G_ACTION_MAP(simple_action_group),
                                     gnc_plugin_page_register_actions,
                                     gnc_plugin_page_register_n_actions,
                                     page);

    priv->lines_default     = DEFAULT_LINES_AMOUNT;
    priv->read_only         = FALSE;
    priv->fd.cleared_match  = CLEARED_ALL;
    priv->fd.days           = 0;
    priv->enable_refresh    = TRUE;
    priv->search_query      = NULL;
    priv->filter_query      = NULL;
}

static void
gnc_plugin_page_register_finalize (GObject* object)
{
    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER (object));

    ENTER ("object %p", object);

    auto priv = GNC_PLUGIN_PAGE_REGISTER_GET_PRIVATE (object);
    if (priv->scrub_context)
    {
        gnc_scrub_context_cancel (priv->scrub_context);
        gnc_scrub_context_unref (priv->scrub_context);
        priv->scrub_context = nullptr;
    }

    G_OBJECT_CLASS (gnc_plugin_page_register_parent_class)->finalize (object);
    LEAVE (" ");
}

Account*
gnc_plugin_page_register_get_account (GncPluginPageRegister* page)
{
    GncPluginPageRegisterPrivate* priv;
    GNCLedgerDisplayType ledger_type;
    Account* leader;

    priv = GNC_PLUGIN_PAGE_REGISTER_GET_PRIVATE (page);
    ledger_type = gnc_ledger_display_type (priv->ledger);
    leader = gnc_ledger_display_leader (priv->ledger);

    if ((ledger_type == LD_SINGLE) || (ledger_type == LD_SUBACCOUNT))
        return leader;
    return NULL;
}

Transaction*
gnc_plugin_page_register_get_current_txn (GncPluginPageRegister* page)
{
    GncPluginPageRegisterPrivate* priv;
    SplitRegister* reg;

    priv = GNC_PLUGIN_PAGE_REGISTER_GET_PRIVATE (page);
    reg = gnc_ledger_display_get_split_register (priv->ledger);
    return gnc_split_register_get_current_trans (reg);
}

/**
 * Whenever the current page is changed, if a register page is
 * the current page, set focus on the sheet.
 */
static gboolean
gnc_plugin_page_register_focus_widget (GncPluginPage* plugin_page)
{
    if (GNC_IS_PLUGIN_PAGE_REGISTER (plugin_page))
    {
        GncWindow* gnc_window = GNC_WINDOW(GNC_PLUGIN_PAGE(plugin_page)->window);
        GNCSplitReg *gsr = gnc_plugin_page_register_get_gsr (GNC_PLUGIN_PAGE(plugin_page));

        if (GNC_IS_MAIN_WINDOW(GNC_PLUGIN_PAGE(plugin_page)->window))
        {
            /* Enable the Transaction menu */
            GAction *action = gnc_main_window_find_action (GNC_MAIN_WINDOW(plugin_page->window), "TransactionAction");
            g_simple_action_set_enabled (G_SIMPLE_ACTION(action), TRUE);
            /* Disable the Schedule menu */
            action = gnc_main_window_find_action (GNC_MAIN_WINDOW(plugin_page->window), "ScheduledAction");
            g_simple_action_set_enabled (G_SIMPLE_ACTION(action), FALSE);

            gnc_main_window_update_menu_and_toolbar (GNC_MAIN_WINDOW(plugin_page->window),
                                                     plugin_page,
                                                     gnc_plugin_load_ui_items);
        }
        else
        {
            GtkWidget *toolbar = gnc_window_get_toolbar (gnc_window);
            GtkWidget *menubar = gnc_window_get_menubar (gnc_window);
            GMenuModel *menubar_model = gnc_window_get_menubar_model (gnc_window);
            GtkWidget *statusbar = gnc_window_get_statusbar (gnc_window);

            // add tooltip redirect call backs
            gnc_plugin_add_toolbar_tooltip_callbacks (toolbar, statusbar);
            gnc_plugin_add_menu_tooltip_callbacks (menubar, menubar_model, statusbar);
        }

        // setup any short toolbar names
        gnc_plugin_init_short_names (gnc_window_get_toolbar (gnc_window), toolbar_labels);

        gnc_plugin_page_register_ui_update (NULL, GNC_PLUGIN_PAGE_REGISTER(plugin_page));

        gnc_split_reg_focus_on_sheet (gsr);
    }
    return FALSE;
}

/* This is the list of actions which are switched inactive in a read-only book. */
static const char* readonly_inactive_actions[] =
{
    "EditCutAction",
    "EditPasteAction",
    "CutTransactionAction",
    "PasteTransactionAction",
    "DuplicateTransactionAction",
    "DeleteTransactionAction",
    "RemoveTransactionSplitsAction",
    "RecordTransactionAction",
    "CancelTransactionAction",
    "UnvoidTransactionAction",
    "VoidTransactionAction",
    "ReverseTransactionAction",
    "ActionsTransferAction",
    "ActionsReconcileAction",
    "ActionsStockSplitAction",
    "ScheduleTransactionAction",
    "ScrubAllAction",
    "ScrubCurrentAction",
    "LinkTransactionAction",
    NULL
};

/* This is the list of actions whose text needs to be changed based on whether */
/* the current cursor class is transaction or split. */
static const char* tran_vs_split_actions[] =
{
    "CutTransactionAction",
    "CopyTransactionAction",
    "PasteTransactionAction",
    "DuplicateTransactionAction",
    "DeleteTransactionAction",
    NULL
};

/* This is the list of labels for when the current cursor class is transaction. */
static const char* tran_action_labels[] =
{
    CUT_TRANSACTION_LABEL,
    COPY_TRANSACTION_LABEL,
    PASTE_TRANSACTION_LABEL,
    DUPLICATE_TRANSACTION_LABEL,
    DELETE_TRANSACTION_LABEL,
    NULL
};

/* This is the list of tooltips for when the current cursor class is transaction. */
static const char* tran_action_tips[] =
{
    CUT_TRANSACTION_TIP,
    COPY_TRANSACTION_TIP,
    PASTE_TRANSACTION_TIP,
    DUPLICATE_TRANSACTION_TIP,
    DELETE_TRANSACTION_TIP,
    NULL
};

/* This is the list of labels for when the current cursor class is split. */
static const char* split_action_labels[] =
{
    CUT_SPLIT_LABEL,
    COPY_SPLIT_LABEL,
    PASTE_SPLIT_LABEL,
    DUPLICATE_SPLIT_LABEL,
    DELETE_SPLIT_LABEL,
    NULL
};

/* This is the list of tooltips for when the current cursor class is split. */
static const char* split_action_tips[] =
{
    CUT_SPLIT_TIP,
    COPY_SPLIT_TIP,
    PASTE_SPLIT_TIP,
    DUPLICATE_SPLIT_TIP,
    DELETE_SPLIT_TIP,
    NULL
};

static std::vector<GncInvoice*>
invoices_from_transaction (const Transaction* trans)
{
    std::vector<GncInvoice*> rv;

    g_return_val_if_fail (GNC_IS_TRANSACTION (trans), rv);

    for (auto node = xaccTransGetSplitList (trans); node; node = g_list_next (node))
    {
        auto split = GNC_SPLIT(node->data);
        auto account = xaccSplitGetAccount (split);
        if (!account || !xaccAccountIsAPARType(xaccAccountGetType(account)))
            continue;
        auto inv = invoice_from_split (split);
        if (inv)
            rv.push_back (inv);
    }
    return rv;
}

static void
gnc_plugin_page_register_ui_update (gpointer various,
                                    GncPluginPageRegister* page)
{
    GncPluginPageRegisterPrivate* priv;
    SplitRegister* reg;
    GAction* action;
    GNCLedgerDisplayType ledger_type;
    gboolean expanded, voided, read_only = FALSE, read_only_reg = FALSE;
    Transaction* trans;
    CursorClass cursor_class;
    const char* uri;
    Account *account;
    GncWindow* gnc_window = GNC_WINDOW(GNC_PLUGIN_PAGE(page)->window);

    /* Set 'Split Transaction' */
    priv = GNC_PLUGIN_PAGE_REGISTER_GET_PRIVATE (page);
    reg = gnc_ledger_display_get_split_register (priv->ledger);
    cursor_class = gnc_split_register_get_current_cursor_class (reg);
    expanded = gnc_split_register_current_trans_expanded (reg);

    action = gnc_plugin_page_get_action (GNC_PLUGIN_PAGE(page), "SplitTransactionAction");
    g_simple_action_set_enabled (G_SIMPLE_ACTION(action), reg->style == REG_STYLE_LEDGER);

    /* Set "style" radio button */
    ledger_type = gnc_ledger_display_type (priv->ledger);
    action = gnc_plugin_page_get_action (GNC_PLUGIN_PAGE(page), "ViewStyleRadioAction");

    g_simple_action_set_enabled (G_SIMPLE_ACTION(action), ledger_type != LD_GL);
    g_action_change_state (G_ACTION(action), g_variant_new_int32 (reg->style));

    /* Set double line */
    action = gnc_plugin_page_get_action (GNC_PLUGIN_PAGE(page), "ViewStyleDoubleLineAction");
    g_action_change_state (G_ACTION(action), g_variant_new_boolean (reg->use_double_line));

    /* Split Expand */
    action = gnc_plugin_page_get_action (GNC_PLUGIN_PAGE(page), "SplitTransactionAction");
    g_simple_action_set_enabled (G_SIMPLE_ACTION(action), reg->style == REG_STYLE_LEDGER);

    g_signal_handlers_block_by_func (action, (gpointer)gnc_plugin_page_register_cmd_expand_transaction, page);
    g_action_change_state (G_ACTION(action), g_variant_new_boolean (expanded));
    g_signal_handlers_unblock_by_func (action, (gpointer)gnc_plugin_page_register_cmd_expand_transaction, page);

    account = gnc_plugin_page_register_get_account (page);

    /* Done like this as the register can be displayed in embedded window */
    if (GNC_IS_MAIN_WINDOW(GNC_PLUGIN_PAGE(page)->window))
    {
        /* Enable the FilePrintAction */
        action = gnc_main_window_find_action (GNC_MAIN_WINDOW(GNC_PLUGIN_PAGE(page)->window), "FilePrintAction");
        g_simple_action_set_enabled (G_SIMPLE_ACTION(action), TRUE);

        /* Set the vis of the StockAssistant */
        gnc_main_window_set_vis_of_items_by_action (GNC_MAIN_WINDOW(GNC_PLUGIN_PAGE(page)->window),
                                                    actions_requiring_priced_account,
                                                    account &&
                                                    xaccAccountIsPriced (account));
    }

    /* If we are in a readonly book, or possibly a place holder
     * account register make any modifying action inactive */
    if (qof_book_is_readonly (gnc_get_current_book()) ||
        gnc_split_reg_get_read_only (priv->gsr))
        read_only_reg = TRUE;

    gnc_plugin_set_actions_enabled (G_ACTION_MAP(gnc_plugin_page_get_action_group (GNC_PLUGIN_PAGE(page))),
                                    actions_requiring_account,
                                    !read_only_reg && account != NULL);

    gnc_plugin_set_actions_enabled (G_ACTION_MAP(gnc_plugin_page_get_action_group (GNC_PLUGIN_PAGE(page))),
                                    actions_requiring_priced_account,
                                    account && xaccAccountIsPriced (account));

    /* Set available actions based on read only */
    trans = gnc_split_register_get_current_trans (reg);

    if (cursor_class == CURSOR_CLASS_SPLIT)
    {
        if (GNC_IS_MAIN_WINDOW(GNC_PLUGIN_PAGE(page)->window))
            gnc_plugin_page_set_menu_popup_qualifier (GNC_PLUGIN_PAGE(page), "split");
        else
            gnc_plugin_page_set_menu_popup_qualifier (GNC_PLUGIN_PAGE(page), "split-sx");
    }
    else
    {
        if (GNC_IS_MAIN_WINDOW(GNC_PLUGIN_PAGE(page)->window))
            gnc_plugin_page_set_menu_popup_qualifier (GNC_PLUGIN_PAGE(page), "trans");
        else
            gnc_plugin_page_set_menu_popup_qualifier (GNC_PLUGIN_PAGE(page), "trans-sx");
    }

    /* If the register is not read only, make any modifying action active
     * to start with */
    if (!read_only_reg)
    {
        const char** iter;
        for (iter = readonly_inactive_actions; *iter; ++iter)
        {
            /* Set the action's sensitivity */
            GAction* action = gnc_plugin_page_get_action (GNC_PLUGIN_PAGE(page), *iter);
            g_simple_action_set_enabled (G_SIMPLE_ACTION(action), TRUE);
        }
        main_window_update_page_set_read_only_icon (GNC_PLUGIN_PAGE(page), FALSE);

        if (trans)
            read_only = xaccTransIsReadonlyByPostedDate (trans);

        voided = xaccTransHasSplitsInState (trans, VREC);

        action = gnc_plugin_page_get_action (GNC_PLUGIN_PAGE(page),
                                             "CutTransactionAction");
        g_simple_action_set_enabled (G_SIMPLE_ACTION(action), !read_only & !voided);

        action = gnc_plugin_page_get_action (GNC_PLUGIN_PAGE(page),
                                             "PasteTransactionAction");
        g_simple_action_set_enabled (G_SIMPLE_ACTION(action), !read_only & !voided);

        action = gnc_plugin_page_get_action (GNC_PLUGIN_PAGE(page),
                                             "DeleteTransactionAction");
        g_simple_action_set_enabled (G_SIMPLE_ACTION(action), !read_only & !voided);

        if (cursor_class == CURSOR_CLASS_SPLIT)
        {
             action = gnc_plugin_page_get_action (GNC_PLUGIN_PAGE(page),
                                                  "DuplicateTransactionAction");
             g_simple_action_set_enabled (G_SIMPLE_ACTION(action), !read_only & !voided);
        }

        action = gnc_plugin_page_get_action (GNC_PLUGIN_PAGE(page),
                                             "RemoveTransactionSplitsAction");
        g_simple_action_set_enabled (G_SIMPLE_ACTION(action), !read_only & !voided);

        /* Set 'Void' and 'Unvoid' */
        if (read_only)
            voided = TRUE;

        action = gnc_plugin_page_get_action (GNC_PLUGIN_PAGE(page),
                                             "VoidTransactionAction");
        g_simple_action_set_enabled (G_SIMPLE_ACTION(action), !voided);

        if (read_only)
            voided = FALSE;

        action = gnc_plugin_page_get_action (GNC_PLUGIN_PAGE(page),
                                             "UnvoidTransactionAction");
        g_simple_action_set_enabled (G_SIMPLE_ACTION(action), voided);
    }

    /* Set 'Open and Remove Linked Documents' */
    action = gnc_plugin_page_get_action (GNC_PLUGIN_PAGE(page),
                                         "LinkedTransactionOpenAction");
    if (trans)
    {
        uri = xaccTransGetDocLink (trans);
        g_simple_action_set_enabled (G_SIMPLE_ACTION(action), (uri ? TRUE:FALSE));
    }
    /* Set 'ExecAssociatedInvoice'
       We can determine an invoice from a txn if either
       - it is an invoice transaction
       - it has splits with an invoice associated with it
    */
    action = gnc_plugin_page_get_action (GNC_PLUGIN_PAGE(page),
                                         "JumpLinkedInvoiceAction");
    if (trans)
    {
        auto invoices = invoices_from_transaction (trans);
        g_simple_action_set_enabled (G_SIMPLE_ACTION(action), !invoices.empty());
    }

    gnc_plugin_business_split_reg_ui_update (GNC_PLUGIN_PAGE(page));

    // Transaction/Split paste action
    action = gnc_plugin_page_get_action (GNC_PLUGIN_PAGE(page),
                                         "PasteTransactionAction");
    g_simple_action_set_enabled (G_SIMPLE_ACTION(action),
                                 gnc_split_register_has_copied_item());

    /* If we are read only, make any modifying action inactive */
    if (read_only_reg)
    {
        const char** iter;
        for (iter = readonly_inactive_actions; *iter; ++iter)
        {
            /* Set the action's sensitivity */
            GAction* action = gnc_plugin_page_get_action (GNC_PLUGIN_PAGE(page), *iter);
            g_simple_action_set_enabled (G_SIMPLE_ACTION(action), FALSE);
        }
        main_window_update_page_set_read_only_icon (GNC_PLUGIN_PAGE(page), TRUE);
    }

    /* Modifying action descriptions based on cursor class */
    {
        GncMenuModelSearch *gsm = g_new0 (GncMenuModelSearch, 1);
        gboolean found = FALSE;
        const char** iter, **label_iter, **tooltip_iter;
        gboolean curr_label_trans = FALSE;
        iter = tran_vs_split_actions;
        label_iter = tran_action_labels;

        gsm->search_action_label = NULL;
        gsm->search_action_name = *iter;
        gsm->search_action_target = NULL;

        found = gnc_menubar_model_find_item (gnc_window_get_menubar_model (gnc_window), gsm);

        PINFO("Test for action '%s', found is %d, iter label is '%s'", *iter, found, _(*label_iter));

        if (!found)
        {
            g_free (gsm);
            return;
        }

        if (g_strcmp0 (gsm->search_action_label, _(*label_iter)) == 0)
            curr_label_trans = TRUE;

        g_free (gsm);

        if ((cursor_class == CURSOR_CLASS_SPLIT) && curr_label_trans)
        {
            gboolean found = FALSE;
            label_iter = split_action_labels;
            tooltip_iter = split_action_tips;
            for (iter = tran_vs_split_actions; *iter; ++iter)
            {
                /* Adjust the action's label and tooltip */
                found = gnc_menubar_model_update_item (gnc_window_get_menubar_model (gnc_window),
                                                       *iter, NULL, _(*label_iter), NULL, _(*tooltip_iter));

                PINFO("split model_item action '%s', found is %d, iter label is '%s'",
                        *iter, found, _(*label_iter));

                ++label_iter;
                ++tooltip_iter;
            }
        }
        else if ((cursor_class == CURSOR_CLASS_TRANS) && !curr_label_trans)
        {
            gboolean found = FALSE;
            label_iter = tran_action_labels;
            tooltip_iter = tran_action_tips;
            for (iter = tran_vs_split_actions; *iter; ++iter)
            {
                /* Adjust the action's label and tooltip */
                found = gnc_menubar_model_update_item (gnc_window_get_menubar_model (gnc_window),
                                                       *iter, NULL, _(*label_iter), NULL, _(*tooltip_iter));

                PINFO("trans model_item action '%s', found is %d, iter label is '%s'",
                        *iter, found, _(*label_iter));

                ++label_iter;
                ++tooltip_iter;
            }
        }
        // now add the callbacks to the replaced menu items.
        gnc_plugin_add_menu_tooltip_callbacks (gnc_window_get_menubar (gnc_window),
                                               gnc_window_get_menubar_model (gnc_window),
                                               gnc_window_get_statusbar (gnc_window));

        // need to add any accelerator keys, default or user added
        gnc_add_accelerator_keys_for_menu (gnc_window_get_menubar (gnc_window),
                                           gnc_window_get_menubar_model (gnc_window),
                                           gnc_window_get_accel_group (gnc_window));
    }
}

static void
gnc_plugin_page_register_ui_initial_state (GncPluginPageRegister* page)
{
    GncPluginPageRegisterPrivate* priv ;
    GSimpleActionGroup *simple_action_group;
    GAction *action;
    Account* account;
    SplitRegister* reg;
    GNCLedgerDisplayType ledger_type;
    gboolean is_readwrite = !qof_book_is_readonly (gnc_get_current_book());

    priv = GNC_PLUGIN_PAGE_REGISTER_GET_PRIVATE (page);
    account = gnc_plugin_page_register_get_account (page);

    /* Get the action group */
    simple_action_group = gnc_plugin_page_get_action_group (GNC_PLUGIN_PAGE(page));
    g_return_if_fail (G_IS_SIMPLE_ACTION_GROUP(simple_action_group));

    gnc_plugin_set_actions_enabled (G_ACTION_MAP(simple_action_group), actions_requiring_account,
                                    is_readwrite && account != NULL);

    /* Set "style" radio button */
    ledger_type = gnc_ledger_display_type (priv->ledger);
    action = gnc_plugin_page_get_action (GNC_PLUGIN_PAGE(page), "ViewStyleRadioAction");
    g_simple_action_set_enabled (G_SIMPLE_ACTION(action), ledger_type == LD_SINGLE);

    reg = gnc_ledger_display_get_split_register (priv->ledger);

    g_signal_handlers_block_by_func (action,
                                     (gpointer)gnc_plugin_page_register_cmd_style_changed, page);
    g_action_change_state (G_ACTION(action), g_variant_new_int32 (reg->style));
    g_signal_handlers_unblock_by_func (action,
                                       (gpointer)gnc_plugin_page_register_cmd_style_changed, page);

    /* Set "double line" toggle button */
    action = gnc_plugin_page_get_action (GNC_PLUGIN_PAGE(page), "ViewStyleDoubleLineAction");
    g_signal_handlers_block_by_func (action,
                                     (gpointer)gnc_plugin_page_register_cmd_style_double_line, page);
    g_action_change_state (G_ACTION(action), g_variant_new_boolean (reg->use_double_line));
    g_signal_handlers_unblock_by_func (action,
                                       (gpointer)gnc_plugin_page_register_cmd_style_double_line, page);
}

/* For setting the focus on a register page, the default gnc_plugin
 * function for 'focus_page' is overridden so that the page focus
 * can be conditionally set. This is to allow for enabling the setting
 * of the sheet focus only when the page is the current one.
 */
static void
gnc_plugin_page_register_focus (GncPluginPage* plugin_page,
                                gboolean on_current_page)
{
    GncPluginPageRegister* page;
    GncPluginPageRegisterPrivate* priv;
    GNCSplitReg* gsr;
    gboolean main_window_is_quitting = FALSE;

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER (plugin_page));

    page = GNC_PLUGIN_PAGE_REGISTER (plugin_page);
    priv = GNC_PLUGIN_PAGE_REGISTER_GET_PRIVATE (page);

    gsr = gnc_plugin_page_register_get_gsr (GNC_PLUGIN_PAGE (plugin_page));

    if (GNC_IS_MAIN_WINDOW(plugin_page->window))
        main_window_is_quitting = gnc_main_window_is_quitting (GNC_MAIN_WINDOW(plugin_page->window));

    if (on_current_page)
    {
        priv->page_focus = TRUE;

        // Chain up to use parent version of 'focus_page' which will
        // use an idle_add as the page changed signal is emitted multiple times.
        GNC_PLUGIN_PAGE_CLASS (gnc_plugin_page_register_parent_class)->focus_page (plugin_page, TRUE);
    }
    else
        priv->page_focus = FALSE;

    // set the sheet focus setting
    gnc_split_reg_set_sheet_focus (gsr, priv->page_focus);

    // No need to do a refresh on application closing
    if (on_current_page && !main_window_is_quitting)
        gnc_ledger_display_set_focus (priv->ledger, priv->page_focus);
}

static void
gnc_ppr_update_filter_and_sort (GncPluginPage* plugin_page)
{
    GncPluginPageRegister *page = GNC_PLUGIN_PAGE_REGISTER(plugin_page);
    GncPluginPageRegisterPrivate *priv = GNC_PLUGIN_PAGE_REGISTER_GET_PRIVATE(page);

    ENTER("page %p", plugin_page);

    priv->enable_refresh = FALSE; // disable refresh

    // Load the saved register sort and filter properties
    gnc_ppr_sort_update_register (plugin_page);
    gnc_ppr_filter_update_register (plugin_page);

    priv->enable_refresh = TRUE; // enable refresh

    // Set filter tooltip for summary bar
    gnc_ppr_filter_set_tooltip (plugin_page, &priv->fd);
}

static GtkWidget*
gnc_plugin_page_register_create_widget (GncPluginPage* plugin_page)
{
    GncPluginPageRegister* page;
    GncPluginPageRegisterPrivate* priv;
    GncWindow* gnc_window;
    guint numRows;
    GtkWidget* gsr;
    SplitRegister* reg;
    Account* acct;

    ENTER ("page %p", plugin_page);
    page = GNC_PLUGIN_PAGE_REGISTER (plugin_page);
    priv = GNC_PLUGIN_PAGE_REGISTER_GET_PRIVATE (page);

    if (priv->widget != NULL)
    {
        LEAVE ("existing widget %p", priv->widget);
        return priv->widget;
    }

    priv->widget = gtk_box_new (GTK_ORIENTATION_VERTICAL, 0);
    gtk_box_set_homogeneous (GTK_BOX (priv->widget), FALSE);
    gtk_widget_set_visible (priv->widget, TRUE);

    // Set the name for this widget so it can be easily manipulated with css
    gtk_widget_set_name (GTK_WIDGET(priv->widget), "gnc-id-register-page");

    numRows = priv->lines_default;
    numRows = MIN (numRows, DEFAULT_LINES_AMOUNT);

    gnc_window = GNC_WINDOW(GNC_PLUGIN_PAGE(page)->window);
    gsr = gnc_split_reg_new (priv->ledger,
                             gnc_window_get_gtk_window (gnc_window),
                             numRows, priv->read_only);
    priv->gsr = (GNCSplitReg *)gsr;
    g_object_ref (gsr);

    gtk_widget_set_visible (gsr, TRUE);
    gnc_box_append_full (GTK_BOX (priv->widget), gsr, TRUE, TRUE, 0);

    g_signal_connect (G_OBJECT (gsr), "help-changed",
                      G_CALLBACK (gnc_plugin_page_help_changed_cb),
                      page);

    g_signal_connect (G_OBJECT (gsr), "show-popup-menu",
                      G_CALLBACK (gnc_plugin_page_popup_menu_cb),
                      page);

    reg = gnc_ledger_display_get_split_register (priv->ledger);
    gnc_split_register_config (reg, reg->type, reg->style,
                               reg->use_double_line);

    gnc_plugin_page_register_ui_initial_state (page);
    gnc_plugin_page_register_ui_update (NULL, page);

    // Now setup the sort and filter settings
    gnc_ppr_update_filter_and_sort (plugin_page);

    plugin_page->summarybar = gsr_create_summary_bar (priv->gsr);
    if (plugin_page->summarybar)
    {
        gtk_widget_set_visible (plugin_page->summarybar, TRUE);
        gnc_box_append_full (GTK_BOX (priv->widget), plugin_page->summarybar,
                            FALSE, FALSE, 0);

        gnc_plugin_page_register_summarybar_position_changed (NULL, NULL, page);
        gnc_prefs_register_cb (GNC_PREFS_GROUP_GENERAL,
                               GNC_PREF_SUMMARYBAR_POSITION_TOP,
                               (gpointer)gnc_plugin_page_register_summarybar_position_changed,
                               page);
        gnc_prefs_register_cb (GNC_PREFS_GROUP_GENERAL,
                               GNC_PREF_SUMMARYBAR_POSITION_BOTTOM,
                               (gpointer)gnc_plugin_page_register_summarybar_position_changed,
                               page);
    }

    priv->event_handler_id = qof_event_register_handler
                             ((QofEventHandler)gnc_plugin_page_register_event_handler, page);
    priv->component_manager_id =
        gnc_register_gui_component (GNC_PLUGIN_PAGE_REGISTER_NAME,
                                    gnc_plugin_page_register_refresh_cb,
                                    gnc_plugin_page_register_close_cb,
                                    page);
    gnc_gui_component_set_session (priv->component_manager_id,
                                   gnc_get_current_session());
    acct = gnc_plugin_page_register_get_account (page);
    if (acct)
        gnc_gui_component_watch_entity (
            priv->component_manager_id, xaccAccountGetGUID (acct),
            QOF_EVENT_DESTROY | QOF_EVENT_MODIFY);

    gnc_split_reg_set_moved_cb
    (priv->gsr, (GFunc)gnc_plugin_page_register_ui_update, page);

    g_signal_connect (G_OBJECT (plugin_page), "inserted",
                      G_CALLBACK (gnc_plugin_page_inserted_cb),
                      NULL);

    /* DRH - Probably lots of other stuff from regWindowLedger should end up here. */
    LEAVE (" ");
    return priv->widget;
}

static void
gnc_plugin_page_register_destroy_widget (GncPluginPage* plugin_page)
{
    GncPluginPageRegister* page;
    GncPluginPageRegisterPrivate* priv;

    ENTER ("page %p", plugin_page);
    page = GNC_PLUGIN_PAGE_REGISTER (plugin_page);
    priv = GNC_PLUGIN_PAGE_REGISTER_GET_PRIVATE (plugin_page);

    gnc_prefs_remove_cb_by_func (GNC_PREFS_GROUP_GENERAL,
                                 GNC_PREF_SUMMARYBAR_POSITION_TOP,
                                 (gpointer)gnc_plugin_page_register_summarybar_position_changed,
                                 page);
    gnc_prefs_remove_cb_by_func (GNC_PREFS_GROUP_GENERAL,
                                 GNC_PREF_SUMMARYBAR_POSITION_BOTTOM,
                                 (gpointer)gnc_plugin_page_register_summarybar_position_changed,
                                 page);

    // Remove the page_changed signal callback
    gnc_plugin_page_disconnect_page_changed (GNC_PLUGIN_PAGE (plugin_page));

    // Remove the page focus idle function if present
    g_idle_remove_by_data (GNC_PLUGIN_PAGE_REGISTER (plugin_page));

    if (priv->void_transaction_request)
        void_transaction_request_cancel (priv->void_transaction_request);

    if (priv->finish_pending_request)
        finish_pending_request_cancel (priv->finish_pending_request);

    if (priv->widget == NULL)
        return;

    if (priv->component_manager_id)
    {
        gnc_unregister_gui_component (priv->component_manager_id);
        priv->component_manager_id = 0;
    }

    if (priv->event_handler_id)
    {
        qof_event_unregister_handler (priv->event_handler_id);
        priv->event_handler_id = 0;
    }

    if (priv->sd.dialog)
    {
        gtk_window_destroy (GTK_WINDOW(priv->sd.dialog));
        memset (&priv->sd, 0, sizeof (priv->sd));
    }

    if (priv->fd.dialog)
    {
        gtk_window_destroy (GTK_WINDOW(priv->fd.dialog));
        memset (&priv->fd, 0, sizeof (priv->fd));
    }

    qof_query_destroy (priv->search_query);
    qof_query_destroy (priv->filter_query);

    gtk_widget_set_visible (priv->widget, FALSE);

    g_object_unref(priv->widget);
    priv->widget = NULL;

    gnc_ledger_display_close (priv->ledger);
    priv->ledger = NULL;

    LEAVE (" ");
}

static void
gnc_plugin_page_register_window_changed (GncPluginPage* plugin_page,
                                         GtkWidget* window)
{
    GncPluginPageRegister* page;
    GncPluginPageRegisterPrivate* priv;

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER (plugin_page));

    page = GNC_PLUGIN_PAGE_REGISTER (plugin_page);
    priv = GNC_PLUGIN_PAGE_REGISTER_GET_PRIVATE (page);
    priv->gsr->window =
        GTK_WIDGET (gnc_window_get_gtk_window (GNC_WINDOW (window)));
}

static const gchar* style_names[] =
{
    "Ledger",
    "Auto Ledger",
    "Journal",
    NULL
};

#define KEY_REGISTER_TYPE       "RegisterType"
#define KEY_ACCOUNT_NAME        "AccountName"
#define KEY_ACCOUNT_GUID        "AccountGuid"
#define KEY_REGISTER_STYLE      "RegisterStyle"
#define KEY_DOUBLE_LINE         "DoubleLineMode"

#define LABEL_ACCOUNT       "Account"
#define LABEL_SUBACCOUNT    "SubAccount"
#define LABEL_GL            "GL"
#define LABEL_SEARCH        "Search"


/** Save enough information about this register page that it can be
 *  recreated next time the user starts gnucash.
 *
 *  @param plugin_page The page to save.
 *
 *  @param key_file A pointer to the GKeyFile data structure where the
 *  page information should be written.
 *
 *  @param group_name The group name to use when saving data. */
static void
gnc_plugin_page_register_save_page (GncPluginPage* plugin_page,
                                    GKeyFile* key_file,
                                    const gchar* group_name)
{
    GncPluginPageRegister* page;
    GncPluginPageRegisterPrivate* priv;
    GNCLedgerDisplayType ledger_type;
    SplitRegister* reg;
    Account* leader;

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER (plugin_page));
    g_return_if_fail (key_file != NULL);
    g_return_if_fail (group_name != NULL);

    ENTER ("page %p, key_file %p, group_name %s", plugin_page, key_file,
           group_name);

    page = GNC_PLUGIN_PAGE_REGISTER (plugin_page);
    priv = GNC_PLUGIN_PAGE_REGISTER_GET_PRIVATE (page);

    reg = gnc_ledger_display_get_split_register (priv->ledger);
    ledger_type = gnc_ledger_display_type (priv->ledger);
    if (ledger_type > LD_GL)
    {
        LEAVE ("Unsupported ledger type");
        return;
    }
    if ((ledger_type == LD_SINGLE) || (ledger_type == LD_SUBACCOUNT))
    {
        const gchar* label;
        gchar* name;
        gchar acct_guid[GUID_ENCODING_LENGTH + 1];
        label = (ledger_type == LD_SINGLE) ? LABEL_ACCOUNT : LABEL_SUBACCOUNT;
        leader = gnc_ledger_display_leader (priv->ledger);
        g_key_file_set_string (key_file, group_name, KEY_REGISTER_TYPE, label);
        name = gnc_account_get_full_name (leader);
        g_key_file_set_string (key_file, group_name, KEY_ACCOUNT_NAME, name);
        g_free (name);
        guid_to_string_buff (xaccAccountGetGUID (leader), acct_guid);
        g_key_file_set_string (key_file, group_name, KEY_ACCOUNT_GUID, acct_guid);
    }
    else if (reg->type == GENERAL_JOURNAL)
    {
        g_key_file_set_string (key_file, group_name, KEY_REGISTER_TYPE,
                               LABEL_GL);
    }
    else if (reg->type == SEARCH_LEDGER)
    {
        g_key_file_set_string (key_file, group_name, KEY_REGISTER_TYPE,
                               LABEL_SEARCH);
    }
    else
    {
        LEAVE ("Unsupported register type");
        return;
    }

    g_key_file_set_string (key_file, group_name, KEY_REGISTER_STYLE,
                           style_names[reg->style]);
    g_key_file_set_boolean (key_file, group_name, KEY_DOUBLE_LINE,
                            reg->use_double_line);

    LEAVE(" ");
}


/** Read and restore the edit menu settings on the specified register
 *  page.  This function will restore the register style (ledger, auto
 *  ledger, journal) and whether or not the register is in double line
 *  mode.  It should eventually restore the "filter by" and "sort by
 *  settings.
 *
 *  @param plugin_page The register being restored.
 *
 *  @param key_file A pointer to the GKeyFile data structure where the
 *  page information should be read.
 *
 *  @param group_name The group name to use when restoring data. */
static void
gnc_plugin_page_register_restore_edit_menu (GncPluginPage* plugin_page,
                                            GKeyFile* key_file,
                                            const gchar* group_name)
{
    GAction* action;
    GVariant *state;
    GError* error = NULL;
    gchar* style_name;
    gint i;
    gboolean use_double_line;

    ENTER (" ");

    /* Convert the style name to an index */
    style_name = g_key_file_get_string (key_file, group_name,
                                        KEY_REGISTER_STYLE, &error);
    for (i = 0 ; style_names[i]; i++)
    {
        if (g_ascii_strcasecmp (style_name, style_names[i]) == 0)
        {
            DEBUG ("Found match for style name: %s", style_name);
            break;
        }
    }
    g_free (style_name);

    /* Update the style menu action for this page */
    if (i <= REG_STYLE_JOURNAL)
    {
        DEBUG ("Setting style: %d", i);
        action = gnc_plugin_page_get_action (plugin_page, "ViewStyleRadioAction");
        g_action_activate (G_ACTION(action), g_variant_new_int32 (i));
    }

    /* Update the  double line action on this page */
    use_double_line = g_key_file_get_boolean (key_file, group_name,
                                              KEY_DOUBLE_LINE, &error);
    DEBUG ("Setting double_line_mode: %d", use_double_line);
    action = gnc_plugin_page_get_action (plugin_page, "ViewStyleDoubleLineAction");

    state = g_action_get_state (G_ACTION(action));

    if (use_double_line != g_variant_get_boolean (state))
        g_action_activate (G_ACTION(action), NULL);

    g_variant_unref (state);

    LEAVE (" ");
}


/** Create a new register page based on the information saved during a
 *  previous instantiation of gnucash.
 *
 *  @param window The window where this page should be installed.
 *
 *  @param key_file A pointer to the GKeyFile data structure where the
 *  page information should be read.
 *
 *  @param group_name The group name to use when restoring data. */
static GncPluginPage*
gnc_plugin_page_register_recreate_page (GtkWidget* window,
                                        GKeyFile* key_file,
                                        const gchar* group_name)
{
    GncPluginPageRegisterPrivate* priv;
    GncPluginPage* plugin_page;
    GError* error = NULL;
    gchar* reg_type, *acct_guid;
    GncGUID guid;
    Account* account = NULL;
    QofBook* book;
    gboolean include_subs;

    g_return_val_if_fail (key_file, NULL);
    g_return_val_if_fail (group_name, NULL);
    ENTER ("key_file %p, group_name %s", key_file, group_name);

    /* Create the new page. */
    reg_type = g_key_file_get_string (key_file, group_name,
                                      KEY_REGISTER_TYPE, &error);
    DEBUG ("Page type: %s", reg_type);
    if ((g_ascii_strcasecmp (reg_type, LABEL_ACCOUNT) == 0) ||
        (g_ascii_strcasecmp (reg_type, LABEL_SUBACCOUNT) == 0))
    {
        include_subs = (g_ascii_strcasecmp (reg_type, LABEL_SUBACCOUNT) == 0);
        DEBUG ("Include subs: %d", include_subs);
        book = qof_session_get_book (gnc_get_current_session());
        if (!book)
        {
            LEAVE("Session has no book");
            return NULL;
        }
        acct_guid = g_key_file_get_string (key_file, group_name,
                                           KEY_ACCOUNT_GUID, &error);
        if (string_to_guid (acct_guid, &guid)) //find account by guid
        {
            account = xaccAccountLookup (&guid, book);
            g_free (acct_guid);
        }
        if (account == NULL) //find account by full name
        {
            gchar* acct_name = g_key_file_get_string (key_file, group_name,
                                                      KEY_ACCOUNT_NAME, &error);
            account = gnc_account_lookup_by_full_name (gnc_book_get_root_account (book),
                                                       acct_name);
            g_free (acct_name);
        }
        if (account == NULL)
        {
            LEAVE ("Bad account name");
            g_free (reg_type);
            return NULL;
        }
        plugin_page = gnc_plugin_page_register_new (account, include_subs);
    }
    else if (g_ascii_strcasecmp (reg_type, LABEL_GL) == 0)
    {
        plugin_page = gnc_plugin_page_register_new_gl();
    }
    else
    {
        LEAVE ("Bad ledger type");
        g_free (reg_type);
        return NULL;
    }
    g_free (reg_type);

    /* disable the refresh of the display ledger, this is for
     * sort/filter updates and double line/style changes */
    priv = GNC_PLUGIN_PAGE_REGISTER_GET_PRIVATE (plugin_page);
    priv->enable_refresh = FALSE;

    /* Recreate page in given window */
    gnc_plugin_page_set_use_new_window (plugin_page, FALSE);

    /* Install it now so we can them manipulate the created widget */
    gnc_main_window_open_page (GNC_MAIN_WINDOW (window), plugin_page);

    /* Now update the page to the last state it was in */
    gnc_plugin_page_register_restore_edit_menu (plugin_page, key_file, group_name);

    /* enable the refresh */
    priv->enable_refresh = TRUE;
    LEAVE (" ");
    return plugin_page;
}


/*
 * Based on code from Epiphany (src/ephy-window.c)
 */
static void
gnc_plugin_page_register_update_edit_menu (GncPluginPage* plugin_page, gboolean hide)
{
    GncPluginPageRegisterPrivate* priv;
    GncPluginPageRegister* page;
    GAction* action;
    gboolean can_copy = FALSE, can_cut = FALSE, can_paste = FALSE;
    gboolean has_selection;
    gboolean is_readwrite = !qof_book_is_readonly (gnc_get_current_book());

    page = GNC_PLUGIN_PAGE_REGISTER (plugin_page);
    priv = GNC_PLUGIN_PAGE_REGISTER_GET_PRIVATE (page);
    has_selection = gnucash_register_has_selection (priv->gsr->reg);

    can_copy = has_selection;
    can_cut = is_readwrite && has_selection;
    can_paste = is_readwrite;

    action = gnc_plugin_page_get_action (plugin_page, "EditCopyAction");
    g_simple_action_set_enabled (G_SIMPLE_ACTION(action), can_copy);
    action = gnc_plugin_page_get_action (plugin_page, "EditCutAction");
    g_simple_action_set_enabled (G_SIMPLE_ACTION(action), can_cut);
    action = gnc_plugin_page_get_action (plugin_page, "EditPasteAction");
    g_simple_action_set_enabled (G_SIMPLE_ACTION(action), can_paste);
}

static const char*
check_repair_abort_YN = N_("'Check & Repair' is currently running, do you want to abort it?");

struct FinishPendingRequest
{
    gatomicrefcount ref_count;
    GWeakRef page;
    GWeakRef parent;
    GCancellable* cancellable;
    gulong parent_destroy_handler;
    GncScrubContext* scrub_context;
    GncPluginPageRegisterPendingCallback callback;
    gpointer user_data;
    GDestroyNotify user_data_destroy;
    gboolean completed;
};

static FinishPendingRequest*
finish_pending_request_ref (FinishPendingRequest* request)
{
    g_atomic_ref_count_inc (&request->ref_count);
    return request;
}



static void
finish_pending_request_free (FinishPendingRequest* request)
{
    GtkWidget* parent = GTK_WIDGET (g_weak_ref_get (&request->parent));

    if (parent && request->parent_destroy_handler)
        g_signal_handler_disconnect (parent, request->parent_destroy_handler);
    g_clear_object (&parent);
    g_weak_ref_clear (&request->parent);
    g_weak_ref_clear (&request->page);
    g_clear_object (&request->cancellable);
    gnc_scrub_context_unref (request->scrub_context);
    if (request->user_data_destroy)
        request->user_data_destroy (request->user_data);
    g_free (request);
}

static void
finish_pending_request_unref (FinishPendingRequest* request)
{
    if (request && g_atomic_ref_count_dec (&request->ref_count))
        finish_pending_request_free (request);
}

static void
finish_pending_request_complete (FinishPendingRequest* request,
                                 gboolean accepted)
{
    GncPluginPageRegister* page;

    if (!request || request->completed)
        return;

    request->completed = TRUE;
    page = GNC_PLUGIN_PAGE_REGISTER (g_weak_ref_get (&request->page));
    if (page)
    {
        auto priv = GNC_PLUGIN_PAGE_REGISTER_GET_PRIVATE (page);

        if (priv->finish_pending_request == request)
            priv->finish_pending_request = nullptr;
        if (request->callback)
            request->callback (page, accepted, request->user_data);
        g_object_unref (page);
    }
    else if (request->callback)
        request->callback (nullptr, FALSE, request->user_data);
    finish_pending_request_unref (request);
}

static void
finish_pending_request_cancel (FinishPendingRequest* request)
{
    if (!request || request->completed)
        return;

    g_cancellable_cancel (request->cancellable);
    finish_pending_request_complete (request, FALSE);
}

static void
finish_pending_parent_destroyed_cb (GtkWidget* parent,
                                    FinishPendingRequest* request)
{
    (void)parent;
    request->parent_destroy_handler = 0;
    finish_pending_request_cancel (request);
}

static void finish_pending_continue (FinishPendingRequest* request);

static void
finish_pending_scrub_finished (GObject* source_object, GAsyncResult* result,
                               gpointer user_data)
{
    auto request = static_cast<FinishPendingRequest*> (user_data);
    GError* error = nullptr;
    auto response = gtk_alert_dialog_choose_finish
        (GTK_ALERT_DIALOG (source_object), result, &error);

    if (error)
    {
        g_clear_error (&error);
        finish_pending_request_complete (request, FALSE);
    }
    else if (response == 1)
    {
        gnc_scrub_context_cancel (request->scrub_context);
        finish_pending_continue (request);
    }
    else
    {
        finish_pending_request_complete (request, FALSE);
    }
    finish_pending_request_unref (request);
}

static void
finish_pending_save_finished (SplitRegister *reg, gboolean saved,
                              gpointer user_data)
{
    auto request = static_cast<FinishPendingRequest *> (user_data);
    auto page = GNC_PLUGIN_PAGE_REGISTER (g_weak_ref_get (&request->page));
    gboolean current = FALSE;

    if (page && reg)
    {
        auto priv = GNC_PLUGIN_PAGE_REGISTER_GET_PRIVATE (page);
        current = priv->ledger &&
            reg == gnc_ledger_display_get_split_register (priv->ledger);
    }
    if (page)
        g_object_unref (page);
    finish_pending_request_complete (request, saved && current);
    finish_pending_request_unref (request);
}
static void
finish_pending_changes_finished (GObject* source_object, GAsyncResult* result,
                                 gpointer user_data)
{
    auto request = static_cast<FinishPendingRequest*> (user_data);
    GError* error = nullptr;
    auto response = gtk_alert_dialog_choose_finish
        (GTK_ALERT_DIALOG (source_object), result, &error);

    if (error)
    {
        g_clear_error (&error);
        finish_pending_request_complete (request, FALSE);
        finish_pending_request_unref (request);
        return;
    }

    if (response == 0 || response == 2)
    {
        auto page = GNC_PLUGIN_PAGE_REGISTER (g_weak_ref_get (&request->page));

        if (page)
        {
            auto priv = GNC_PLUGIN_PAGE_REGISTER_GET_PRIVATE (page);
            auto reg = priv->ledger ?
                gnc_ledger_display_get_split_register (priv->ledger) : nullptr;

            if (reg)
            {
                if (response == 0)
                    gnc_split_register_cancel_cursor_trans_changes (reg);
                gnc_split_register_save_async
                    (reg, TRUE, finish_pending_save_finished,
                     finish_pending_request_ref (request));
                g_object_unref (page);
                finish_pending_request_unref (request);
                return;
            }
            g_object_unref (page);
        }
    }

    finish_pending_request_complete (request, FALSE);
    finish_pending_request_unref (request);
}

static void
finish_pending_continue (FinishPendingRequest* request)
{
    GncPluginPageRegister* page;
    GtkWindow* parent;
    SplitRegister* reg;

    if (!request || request->completed)
        return;

    if (g_cancellable_is_cancelled (request->cancellable))
    {
        finish_pending_request_complete (request, FALSE);
        return;
    }

    if (gnc_scrub_context_is_active (request->scrub_context) &&
        !gnc_scrub_context_is_cancelled (request->scrub_context))
    {
        const char* buttons[] = { _("Cancel"), _("Abort"), nullptr };
        auto alert = gtk_alert_dialog_new ("%s", _(check_repair_abort_YN));
        auto parent_widget = GTK_WIDGET (g_weak_ref_get (&request->parent));

        if (!parent_widget)
        {
            g_object_unref (alert);
            finish_pending_request_complete (request, FALSE);
            return;
        }
        gtk_alert_dialog_set_buttons (alert, buttons);
        gtk_alert_dialog_set_cancel_button (alert, 0);
        gtk_alert_dialog_choose (alert, GTK_WINDOW (parent_widget),
                                 request->cancellable,
                                 finish_pending_scrub_finished,
                                 finish_pending_request_ref (request));
        g_object_unref (parent_widget);
        g_object_unref (alert);
        return;
    }

    page = GNC_PLUGIN_PAGE_REGISTER (g_weak_ref_get (&request->page));
    if (!page)
    {
        finish_pending_request_complete (request, FALSE);
        return;
    }

    auto priv = GNC_PLUGIN_PAGE_REGISTER_GET_PRIVATE (page);
    reg = priv->ledger ? gnc_ledger_display_get_split_register (priv->ledger) : nullptr;
    parent = GTK_WINDOW (gnc_plugin_page_get_window (GNC_PLUGIN_PAGE (page)));
    if (!reg || !parent)
    {
        g_object_unref (page);
        finish_pending_request_complete (request, FALSE);
        return;
    }

    if (!gnc_split_register_changed (reg))
    {
        g_object_unref (page);
        finish_pending_request_complete (request, TRUE);
        return;
    }

    auto name = gnc_plugin_page_register_get_tab_name (GNC_PLUGIN_PAGE (page));
    const char* buttons[] = { _("Discard Transaction"), _("Cancel"),
                              _("Save Transaction"), nullptr };
    auto alert = gtk_alert_dialog_new (_("Save changes to %s?"), name);

    g_free (name);
    gtk_alert_dialog_set_detail
        (alert, _("This register has pending changes to a transaction. "
                  "Would you like to save the changes to this transaction, "
                  "discard the transaction, or cancel the operation?"));
    gtk_alert_dialog_set_buttons (alert, buttons);
    gtk_alert_dialog_set_cancel_button (alert, 1);
    gtk_alert_dialog_set_default_button (alert, 2);
    gtk_alert_dialog_choose (alert, parent, request->cancellable,
                             finish_pending_changes_finished,
                             finish_pending_request_ref (request));
    g_object_unref (alert);
    g_object_unref (page);
}

static void
gnc_plugin_page_register_finish_pending_async
    (GncPluginPageRegister* page, GCancellable* cancellable,
     GncPluginPageRegisterPendingCallback callback, gpointer user_data,
     GDestroyNotify user_data_destroy)
{
    GtkWidget* parent;
    FinishPendingRequest* request;
    GncPluginPageRegisterPrivate* priv;

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER (page));

    priv = GNC_PLUGIN_PAGE_REGISTER_GET_PRIVATE (page);
    if (priv->finish_pending_request)
    {
        if (callback)
            callback (page, FALSE, user_data);
        if (user_data_destroy)
            user_data_destroy (user_data);
        return;
    }

    parent = gnc_plugin_page_get_window (GNC_PLUGIN_PAGE (page));
    if (!parent || !GTK_IS_WINDOW (parent))
    {
        if (callback)
            callback (page, FALSE, user_data);
        if (user_data_destroy)
            user_data_destroy (user_data);
        return;
    }

    request = g_new0 (FinishPendingRequest, 1);
    g_atomic_ref_count_init (&request->ref_count);
    g_weak_ref_init (&request->page, page);
    g_weak_ref_init (&request->parent, parent);
    request->cancellable = cancellable ? g_object_ref (cancellable) :
                                         g_cancellable_new ();
    request->scrub_context = gnc_scrub_context_ref (priv->scrub_context);
    request->callback = callback;
    request->user_data = user_data;
    request->user_data_destroy = user_data_destroy;
    request->parent_destroy_handler = g_signal_connect
        (parent, "destroy", G_CALLBACK (finish_pending_parent_destroyed_cb), request);
    priv->finish_pending_request = request;
    finish_pending_continue (request);
}
struct GncPluginPageRegisterPendingBridge
{
    GncPluginPagePendingCallback callback;
    gpointer user_data;
};

static void
gnc_plugin_page_register_finish_pending_bridge_finished
    (GncPluginPageRegister* page, gboolean accepted, gpointer user_data)
{
    auto bridge = static_cast<GncPluginPageRegisterPendingBridge *> (user_data);

    if (bridge->callback)
        bridge->callback (page ? GNC_PLUGIN_PAGE (page) : nullptr, accepted, bridge->user_data);
}

static void
gnc_plugin_page_register_finish_pending_async_virtual
    (GncPluginPage* plugin_page, GCancellable* cancellable,
     GncPluginPagePendingCallback callback, gpointer user_data)
{
    auto bridge = g_new0 (GncPluginPageRegisterPendingBridge, 1);

    bridge->callback = callback;
    bridge->user_data = user_data;
    gnc_plugin_page_register_finish_pending_async
        (GNC_PLUGIN_PAGE_REGISTER (plugin_page), cancellable,
         gnc_plugin_page_register_finish_pending_bridge_finished, bridge, g_free);
}

static gchar*
gnc_plugin_page_register_get_tab_name (GncPluginPage* plugin_page)
{
    GncPluginPageRegisterPrivate* priv;
    GNCLedgerDisplayType ledger_type;
    GNCLedgerDisplay* ld;
    SplitRegister* reg;
    Account* leader;

    g_return_val_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER (plugin_page),
                          g_strdup (_("unknown")));

    priv = GNC_PLUGIN_PAGE_REGISTER_GET_PRIVATE (plugin_page);
    ld = priv->ledger;
    reg = gnc_ledger_display_get_split_register (ld);
    ledger_type = gnc_ledger_display_type (ld);
    leader = gnc_ledger_display_leader (ld);

    switch (ledger_type)
    {
    case LD_SINGLE:
        return g_strdup (xaccAccountGetName (leader));

    case LD_SUBACCOUNT:
        return g_strdup_printf ("%s+", xaccAccountGetName (leader));

    case LD_GL:
        switch (reg->type)
        {
        case GENERAL_JOURNAL:
        case INCOME_LEDGER:
            return g_strdup (_ ("General Journal"));
        case PORTFOLIO_LEDGER:
            return g_strdup (_ ("Portfolio"));
        case SEARCH_LEDGER:
            return g_strdup (_ ("Search Results"));
        default:
            break;
        }
        break;

    default:
        break;
    }

    return g_strdup (_ ("unknown"));
}

static gchar*
gnc_plugin_page_register_get_tab_color (GncPluginPage* plugin_page)
{
    GncPluginPageRegisterPrivate* priv;
    GNCLedgerDisplayType ledger_type;
    GNCLedgerDisplay* ld;
    Account* leader;
    const char* color;

    g_return_val_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER (plugin_page),
                          g_strdup (_("unknown")));

    priv = GNC_PLUGIN_PAGE_REGISTER_GET_PRIVATE (plugin_page);
    ld = priv->ledger;
    ledger_type = gnc_ledger_display_type (ld);
    leader = gnc_ledger_display_leader (ld);
    color = NULL;

    if ((ledger_type == LD_SINGLE) || (ledger_type == LD_SUBACCOUNT))
        color = xaccAccountGetColor (leader);

    return g_strdup (color ? color : "Not Set");
}

static gchar*
gnc_plugin_page_register_get_long_name (GncPluginPage* plugin_page)
{
    GncPluginPageRegisterPrivate* priv;
    GNCLedgerDisplayType ledger_type;
    GNCLedgerDisplay* ld;
    Account* leader;

    g_return_val_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER (plugin_page),
                          _ ("unknown"));

    priv = GNC_PLUGIN_PAGE_REGISTER_GET_PRIVATE (plugin_page);
    ld = priv->ledger;
    ledger_type = gnc_ledger_display_type (ld);
    leader = gnc_ledger_display_leader (ld);

    switch (ledger_type)
    {
    case LD_SINGLE:
        return gnc_account_get_full_name (leader);

    case LD_SUBACCOUNT:
    {
        gchar* account_full_name = gnc_account_get_full_name (leader);
        gchar* return_string = g_strdup_printf ("%s+", account_full_name);
        g_free ((gpointer*) account_full_name);
        return return_string;
    }

    default:
        break;
    }

    return NULL;
}

static void
gnc_plugin_page_register_summarybar_position_changed (gpointer prefs,
                                                      gchar* pref,
                                                      gpointer user_data)
{
    GncPluginPage* plugin_page;
    GncPluginPageRegister* page;
    GncPluginPageRegisterPrivate* priv;
    GtkPositionType position = GTK_POS_BOTTOM;

    g_return_if_fail (user_data != NULL);

    if (!GNC_IS_PLUGIN_PAGE (user_data))
        return;

    plugin_page = GNC_PLUGIN_PAGE (user_data);
    page = GNC_PLUGIN_PAGE_REGISTER (user_data);
    priv = GNC_PLUGIN_PAGE_REGISTER_GET_PRIVATE (page);

    if (priv == NULL)
        return;

    if (gnc_prefs_get_bool (GNC_PREFS_GROUP_GENERAL,
                            GNC_PREF_SUMMARYBAR_POSITION_TOP))
        position = GTK_POS_TOP;

    auto box = GTK_BOX (priv->widget);

    if (position == GTK_POS_TOP)
        gtk_box_reorder_child_after (box, plugin_page->summarybar, NULL);
    else
    {
        auto last_child = gtk_widget_get_last_child (priv->widget);
        if (last_child != plugin_page->summarybar)
            gtk_box_reorder_child_after (box, plugin_page->summarybar, last_child);
    }
}

static void
gnc_plugin_page_register_update_page_icon (GncPluginPage* plugin_page)
{
    GncPluginPageRegisterPrivate* priv;
    gboolean read_only;

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER (plugin_page));

    priv = GNC_PLUGIN_PAGE_REGISTER_GET_PRIVATE (plugin_page);

    if (qof_book_is_readonly (gnc_get_current_book()) ||
        gnc_split_reg_get_read_only (priv->gsr))
        read_only = TRUE;
    else
        read_only = FALSE;

    main_window_update_page_set_read_only_icon (GNC_PLUGIN_PAGE(plugin_page),
                                                read_only);
}

/** This function is called to get the query associated with this
 *  plugin page.
 *
 *  @param page A pointer to the GncPluginPage.
 */
Query*
gnc_plugin_page_register_get_query (GncPluginPage* plugin_page)
{
    GncPluginPageRegister* page;
    GncPluginPageRegisterPrivate* priv;

    g_return_val_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER (plugin_page), NULL);

    page = GNC_PLUGIN_PAGE_REGISTER (plugin_page);
    priv = GNC_PLUGIN_PAGE_REGISTER_GET_PRIVATE (page);
    return gnc_ledger_display_get_query (priv->ledger);
}

void
gnc_plugin_page_register_query_update (GncPluginPageRegister* page, Query *query)
{
    GncPluginPageRegisterPrivate* priv;

    ENTER(" ");
    priv = GNC_PLUGIN_PAGE_REGISTER_GET_PRIVATE(page);

    // clear previous filter query and save current
    qof_query_destroy (priv->filter_query);
    priv->filter_query = qof_query_copy (query);

    if (priv->enable_refresh)
        gnc_ledger_display_refresh (priv->ledger);
    LEAVE(" ");
}

void
gnc_plugin_page_register_update_for_search_query (GncPluginPageRegister* page)
{
    GncPluginPageRegisterPrivate* priv;
    SplitRegister* reg;

    priv = GNC_PLUGIN_PAGE_REGISTER_GET_PRIVATE (page);
    reg = gnc_ledger_display_get_split_register (priv->ledger);

    if (reg->type == SEARCH_LEDGER)
    {
        Query* query_tmp = gnc_ledger_display_get_query (priv->ledger);

        // if filter_query is NULL, then the dialogue find has been run
        // before coming here. if query_tmp does not equal filter_query
        // then the dialogue find has been run again before coming here
        if ((priv->filter_query == NULL) ||
            (!qof_query_equal (query_tmp, priv->filter_query)))
        {
            qof_query_destroy (priv->search_query);
            priv->search_query = qof_query_copy (query_tmp);
        }
        gnc_ledger_display_set_query (priv->ledger, priv->search_query);
    }
}

void
gnc_plugin_register_set_enable_refresh (GncPluginPageRegister* page,
                                        gboolean enable_refresh)
{
    GncPluginPageRegisterPrivate* priv;

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER(page));

    priv = GNC_PLUGIN_PAGE_REGISTER_GET_PRIVATE(page);

    priv->enable_refresh = enable_refresh;
}

void
gnc_plugin_page_register_clear_current_filter (GncPluginPage* plugin_page)
{
    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER(plugin_page));

    gnc_ppr_filter_clear_current_filter (plugin_page);
}

/************************************************************/
/*                  Report Helper Functions                 */
/************************************************************/

static char*
gnc_reg_get_name (GNCLedgerDisplay* ledger, gboolean for_window)
{
    Account* leader;
    SplitRegister* reg;
    gchar* account_name;
    gchar* reg_name;
    gchar* name;
    GNCLedgerDisplayType ledger_type;

    if (ledger == NULL)
        return NULL;

    reg = gnc_ledger_display_get_split_register (ledger);
    ledger_type = gnc_ledger_display_type (ledger);

    switch (reg->type)
    {
    case GENERAL_JOURNAL:
    case INCOME_LEDGER:
        if (for_window)
            reg_name = _ ("General Journal");
        else
            reg_name = _ ("Transaction Report");
        break;
    case PORTFOLIO_LEDGER:
        if (for_window)
            reg_name = _ ("Portfolio");
        else
            reg_name = _ ("Portfolio Report");
        break;
    case SEARCH_LEDGER:
        if (for_window)
            reg_name = _ ("Search Results");
        else
            reg_name = _ ("Search Results Report");
        break;
    default:
        if (for_window)
            reg_name = _ ("Register");
        else
            reg_name = _ ("Transaction Report");
        break;
    }

    leader = gnc_ledger_display_leader (ledger);

    if ((leader != NULL) && (ledger_type != LD_GL))
    {
        account_name = gnc_account_get_full_name (leader);

        if (ledger_type == LD_SINGLE)
        {
            name = g_strconcat (account_name, " - ", reg_name, NULL);
        }
        else
        {
            name = g_strconcat (account_name, " ", _ ("and subaccounts"), " - ", reg_name,
                                NULL);
        }
        g_free (account_name);
    }
    else
        name = g_strdup (reg_name);

    return name;
}

static int
report_helper (GNCLedgerDisplay* ledger, Split* split, Query* query)
{
    SplitRegister* reg = gnc_ledger_display_get_split_register (ledger);
    Account* account;
    char* str;
    const char* tmp;
    swig_type_info* qtype;
    SCM args;
    SCM func;
    SCM arg;

    args = SCM_EOL;

    func = scm_c_eval_string ("gnc:register-report-create");
    g_return_val_if_fail (scm_is_procedure (func), -1);

    tmp = gnc_split_register_get_credit_string (reg);
    arg = scm_from_utf8_string (tmp ? tmp : _ ("Credit"));
    args = scm_cons (arg, args);

    tmp = gnc_split_register_get_debit_string (reg);
    arg = scm_from_utf8_string (tmp ? tmp : _ ("Debit"));
    args = scm_cons (arg, args);

    str = gnc_reg_get_name (ledger, FALSE);
    arg = scm_from_utf8_string (str ? str : "");
    args = scm_cons (arg, args);
    g_free (str);

    arg = SCM_BOOL (reg->use_double_line);
    args = scm_cons (arg, args);

    arg = SCM_BOOL (reg->type == GENERAL_JOURNAL || reg->type == INCOME_LEDGER
                    || reg->type == SEARCH_LEDGER);
    args = scm_cons (arg, args);

    arg = SCM_BOOL (reg->style == REG_STYLE_JOURNAL);
    args = scm_cons (arg, args);

    if (!query)
    {
        query = gnc_ledger_display_get_query (ledger);
        g_return_val_if_fail (query != NULL, -1);
    }

    qtype = SWIG_TypeQuery ("_p__QofQuery");
    g_return_val_if_fail (qtype, -1);

    arg = SWIG_NewPointerObj (query, qtype, 0);
    args = scm_cons (arg, args);
    g_return_val_if_fail (arg != SCM_UNDEFINED, -1);


    if (split)
    {
        qtype = SWIG_TypeQuery ("_p_Split");
        g_return_val_if_fail (qtype, -1);
        arg = SWIG_NewPointerObj (split, qtype, 0);
    }
    else
    {
        arg = SCM_BOOL_F;
    }
    args = scm_cons (arg, args);
    g_return_val_if_fail (arg != SCM_UNDEFINED, -1);


    qtype = SWIG_TypeQuery ("_p_Account");
    g_return_val_if_fail (qtype, -1);

    account = gnc_ledger_display_leader (ledger);
    arg = SWIG_NewPointerObj (account, qtype, 0);
    args = scm_cons (arg, args);
    g_return_val_if_fail (arg != SCM_UNDEFINED, -1);


    /* Apply the function to the args */
    arg = scm_apply (func, args, SCM_EOL);
    g_return_val_if_fail (scm_is_exact (arg), -1);

    return scm_to_int (arg);
}

/************************************************************/
/*                     Command callbacks                    */
/************************************************************/

typedef struct
{
    GWeakRef page;
    GWeakRef parent;
    QofBook *book;
    GList *split_guids;
} PrintChecksMultiAccountRequest;

static void
print_checks_multi_account_request_free (PrintChecksMultiAccountRequest *request)
{
    g_weak_ref_clear (&request->page);
    g_weak_ref_clear (&request->parent);
    g_list_free_full (request->split_guids, (GDestroyNotify)guid_free);
    g_free (request);
}

static gboolean
print_checks_multi_account_request_context (PrintChecksMultiAccountRequest *request,
                                            GtkWindow **parent_out,
                                            GList **splits_out)
{
    auto page = GNC_PLUGIN_PAGE_REGISTER (g_weak_ref_get (&request->page));
    auto parent = GTK_WINDOW (g_weak_ref_get (&request->parent));
    GList *splits = NULL;

    if (!page || !request->book || request->book != gnc_get_current_book () ||
        qof_book_shutting_down (request->book) ||
        gnc_plugin_page_get_window (GNC_PLUGIN_PAGE (page)) != GTK_WIDGET (parent))
    {
        g_clear_object (&parent);
        g_clear_object (&page);
        return FALSE;
    }

    auto priv = GNC_PLUGIN_PAGE_REGISTER_GET_PRIVATE (page);
    auto reg = priv->ledger ?
        gnc_ledger_display_get_split_register (priv->ledger) : nullptr;
    if (!reg || gnc_ledger_display_type (priv->ledger) != LD_GL ||
        reg->type != SEARCH_LEDGER)
    {
        g_clear_object (&parent);
        g_object_unref (page);
        return FALSE;
    }

    for (GList *node = request->split_guids; node; node = node->next)
    {
        auto split = xaccSplitLookup (static_cast<GncGUID *> (node->data),
                                      request->book);
        if (!split)
        {
            g_list_free (splits);
            g_clear_object (&parent);
            g_object_unref (page);
            return FALSE;
        }
        splits = g_list_prepend (splits, split);
    }
    splits = g_list_reverse (splits);

    g_object_unref (page);
    *parent_out = parent;
    *splits_out = splits;
    return TRUE;
}

static void
print_checks_multi_account_finished (gint response, gpointer user_data)
{
    auto request = static_cast<PrintChecksMultiAccountRequest *> (user_data);
    GtkWindow *parent = NULL;
    GList *splits = NULL;

    if (response == GTK_RESPONSE_YES &&
        print_checks_multi_account_request_context (request, &parent, &splits))
    {
        gnc_ui_print_check_dialog_create (parent ? GTK_WIDGET (parent) : NULL,
                                          splits, NULL);
        g_list_free (splits);
        g_clear_object (&parent);
    }
    print_checks_multi_account_request_free (request);
}

static void
gnc_plugin_page_register_cmd_print_check (GSimpleAction *simple,
                                          GVariant      *paramter,
                                          gpointer       user_data)
{
    auto page = GNC_PLUGIN_PAGE_REGISTER(user_data);
    GncPluginPageRegisterPrivate* priv;
    SplitRegister* reg;
    Split*          split;
    Transaction*    trans;
    GList*          splits = NULL, *item;
    GNCLedgerDisplayType ledger_type;
    Account*        account, *subaccount = NULL;
    GtkWidget*      window;

    ENTER ("(action %p, page %p)", simple, page);

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER (page));

    priv = GNC_PLUGIN_PAGE_REGISTER_GET_PRIVATE (page);
    reg = gnc_ledger_display_get_split_register (priv->ledger);
    ledger_type = gnc_ledger_display_type (priv->ledger);
    window = gnc_plugin_page_get_window (GNC_PLUGIN_PAGE (page));
    if (ledger_type == LD_SINGLE || ledger_type == LD_SUBACCOUNT)
    {
        account  = gnc_plugin_page_register_get_account (page);
        split    = gnc_split_register_get_current_split (reg);
        trans    = xaccSplitGetParent (split);
        if (ledger_type == LD_SUBACCOUNT)
        {
            /* Set up subaccount printing, where the check amount matches the
             * value displayed in the register. */
            subaccount = account;
        }

        if (split && trans)
        {
            if (xaccSplitGetAccount (split) == account)
            {
                splits = g_list_prepend (splits, split);
                gnc_ui_print_check_dialog_create (window, splits, subaccount);
                g_list_free (splits);
            }
            else
            {
                /* This split is not for the account shown in this register.  Get the
                   split that anchors the transaction to the registor */
                split = gnc_split_register_get_current_trans_split (reg, NULL);
                if (split)
                {
                    splits = g_list_prepend (splits, split);
                    gnc_ui_print_check_dialog_create (window, splits, subaccount);
                    g_list_free (splits);
                }
            }
        }
    }
    else if (ledger_type == LD_GL && reg->type == SEARCH_LEDGER)
    {
        Account* common_acct = NULL;
        gboolean multiple_accounts = FALSE;

        /* the following GList* splits must not be freed */
        splits = qof_query_run (gnc_ledger_display_get_query (priv->ledger));

        /* Make sure each split is from the same account */
        for (item = splits; item; item = g_list_next (item))
        {
            split = (Split*) item->data;
            if (common_acct == NULL)
            {
                common_acct = xaccSplitGetAccount (split);
            }
            else if (xaccSplitGetAccount (split) != common_acct)
            {
                multiple_accounts = TRUE;
                break;
            }
        }

        if (multiple_accounts)
        {
            auto request = g_new0 (PrintChecksMultiAccountRequest, 1);

            request->book = gnc_get_current_book ();
            g_weak_ref_init (&request->page, page);
            g_weak_ref_init (&request->parent, window);
            for (item = splits; item; item = g_list_next (item))
            {
                split = (Split*) item->data;
                if (!split)
                {
                    print_checks_multi_account_request_free (request);
                    LEAVE ("Missing split in search result");
                    return;
                }
                request->split_guids = g_list_prepend (
                    request->split_guids, guid_copy (xaccSplitGetGUID (split)));
            }
            request->split_guids = g_list_reverse (request->split_guids);
            if (!request->split_guids)
            {
                print_checks_multi_account_request_free (request);
                LEAVE ("No printable splits");
                return;
            }

            gnc_warning_dialog_async (
                GTK_WINDOW (window), GNC_PREF_WARN_CHECKPRINTING_MULTI_ACCT,
                _ ("Print checks from multiple accounts?"),
                _ ("This search result contains splits from more than one account. "
                   "Do you want to print the checks even though they are not all "
                   "from the same account?"),
                _ ("_Print checks"), GTK_RESPONSE_YES, FALSE,
                print_checks_multi_account_finished, request);
            LEAVE ("Multiple accounts");
            return;
        }
        gnc_ui_print_check_dialog_create (window, splits, NULL);
    }
    else
    {
        gnc_error_dialog (GTK_WINDOW (window), "%s",
                          _ ("You can only print checks from a bank account register or search results."));
        LEAVE ("Unsupported ledger type");
        return;
    }
    LEAVE (" ");
}


static void
gnc_plugin_page_register_cmd_cut (GSimpleAction *simple,
                                  GVariant      *paramter,
                                  gpointer       user_data)
{
    auto page = GNC_PLUGIN_PAGE_REGISTER(user_data);
    GncPluginPageRegisterPrivate* priv;

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER (page));

    ENTER ("(action %p, page %p)", simple, page);
    priv = GNC_PLUGIN_PAGE_REGISTER_GET_PRIVATE (page);

    GtkWidget *widget = gtk_window_get_focus(GTK_WINDOW (priv->gsr->window));
    if (g_strcmp0 (gtk_widget_get_name (widget), "GnucashSheet") != 0)
    {
        if (widget)
            gtk_widget_activate_action (widget, "clipboard.cut", NULL);
        LEAVE("Not cut from GnucashSheet");

        return;
    }

    gnucash_register_cut_clipboard (priv->gsr->reg);
    LEAVE ("");
}


static void
gnc_plugin_page_register_cmd_copy (GSimpleAction *simple,
                                   GVariant      *paramter,
                                   gpointer       user_data)
{
    auto page = GNC_PLUGIN_PAGE_REGISTER(user_data);
    GncPluginPageRegisterPrivate* priv;

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER (page));

    ENTER ("(action %p, page %p)", simple, page);
    priv = GNC_PLUGIN_PAGE_REGISTER_GET_PRIVATE (page);

    GtkWidget *widget = gtk_window_get_focus(GTK_WINDOW (priv->gsr->window));
    if (g_strcmp0 (gtk_widget_get_name (widget), "GnucashSheet") != 0)
    {
        if (widget)
            gtk_widget_activate_action (widget, "clipboard.copy", NULL);
        LEAVE("Not copied from GnucashSheet");

        return;
    }

    gnucash_register_copy_clipboard (priv->gsr->reg);
    LEAVE ("");
}


static void
gnc_plugin_page_register_cmd_paste (GSimpleAction *simple,
                                    GVariant      *paramter,
                                    gpointer       user_data)
{
    auto page = GNC_PLUGIN_PAGE_REGISTER(user_data);
    GncPluginPageRegisterPrivate* priv;

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER (page));

    ENTER ("(action %p, page %p)", simple, page);
    priv = GNC_PLUGIN_PAGE_REGISTER_GET_PRIVATE (page);

    GtkWidget *widget = gtk_window_get_focus(GTK_WINDOW (priv->gsr->window));
    if (g_strcmp0 (gtk_widget_get_name (widget), "GnucashSheet") != 0)
    {
        if (widget)
            gtk_widget_activate_action (widget, "clipboard.paste", NULL);
        LEAVE("Not pasted to GnucashSheet");

        return;
    }

    gnucash_register_paste_clipboard (priv->gsr->reg);
    LEAVE ("");
}


static void
gnc_plugin_page_register_cmd_edit_account (GSimpleAction *simple,
                                           GVariant      *paramter,
                                           gpointer       user_data)
{
    auto page = GNC_PLUGIN_PAGE_REGISTER(user_data);
    Account* account;
    GtkWindow* parent = GTK_WINDOW(gnc_plugin_page_get_window (GNC_PLUGIN_PAGE(page)));
    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER (page));

    ENTER ("(action %p, page %p)", simple, page);
    account = gnc_plugin_page_register_get_account (page);
    if (account)
        gnc_ui_edit_account_window (parent, account);
    LEAVE (" ");
}


static void
gnc_plugin_page_register_cmd_find_account (GSimpleAction *simple,
                                           GVariant      *paramter,
                                           gpointer       user_data)
{
    auto page = GNC_PLUGIN_PAGE_REGISTER(user_data);
    GtkWidget* window;

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER (page));

    window = gnc_plugin_page_get_window (GNC_PLUGIN_PAGE (page));
    gnc_find_account_dialog (window, NULL);
}


static void
gnc_plugin_page_register_cmd_find_transactions (GSimpleAction *simple,
                                                GVariant      *paramter,
                                                gpointer       user_data)
{
    auto page = GNC_PLUGIN_PAGE_REGISTER(user_data);
    GncPluginPageRegisterPrivate* priv;
    GtkWindow* window;

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER (page));

    ENTER ("(action %p, page %p)", simple, page);
    priv = GNC_PLUGIN_PAGE_REGISTER_GET_PRIVATE (page);
    window = GTK_WINDOW (gnc_plugin_page_get_window (GNC_PLUGIN_PAGE (page)));
    gnc_ui_find_transactions_dialog_create (window, priv->ledger);
    LEAVE (" ");
}


static void
gnc_plugin_page_register_cmd_edit_tax_options (GSimpleAction *simple,
                                               GVariant      *paramter,
                                               gpointer       user_data)
{
    auto page = GNC_PLUGIN_PAGE_REGISTER(user_data);
    GtkWidget *window;
    Account* account;

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER (page));

    ENTER ("(action %p, page %p)", simple, page);
    window = gnc_plugin_page_get_window (GNC_PLUGIN_PAGE (page));
    account = gnc_plugin_page_register_get_account (page);
    gnc_tax_info_dialog (window, account);
    LEAVE (" ");
}

static void
gnc_plugin_page_register_cmd_cut_transaction (GSimpleAction *simple,
                                              GVariant      *paramter,
                                              gpointer       user_data)
{
    auto page = GNC_PLUGIN_PAGE_REGISTER(user_data);
    GncPluginPageRegisterPrivate* priv;

    ENTER ("(action %p, page %p)", simple, page);

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER (page));

    priv = GNC_PLUGIN_PAGE_REGISTER_GET_PRIVATE (page);
    gsr_default_cut_txn_handler (priv->gsr, NULL);

    // Transaction/Split paste action
    GAction *action = gnc_plugin_page_get_action (GNC_PLUGIN_PAGE(page),
                                                  "PasteTransactionAction");
    g_simple_action_set_enabled (G_SIMPLE_ACTION(action),
                                 gnc_split_register_has_copied_item());
    LEAVE (" ");
}


static void
gnc_plugin_page_register_cmd_copy_transaction (GSimpleAction *simple,
                                               GVariant      *paramter,
                                               gpointer       user_data)
{
    auto page = GNC_PLUGIN_PAGE_REGISTER(user_data);
    GncPluginPageRegisterPrivate* priv;
    SplitRegister* reg;

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER (page));

    ENTER ("(action %p, page %p)", simple, page);
    priv = GNC_PLUGIN_PAGE_REGISTER_GET_PRIVATE (page);
    reg = gnc_ledger_display_get_split_register (priv->ledger);
    gnc_split_register_copy_current (reg);

    // Transaction/Split paste action
    GAction *action = gnc_plugin_page_get_action (GNC_PLUGIN_PAGE(page),
                                                  "PasteTransactionAction");
    g_simple_action_set_enabled (G_SIMPLE_ACTION(action),
                                 gnc_split_register_has_copied_item());
    LEAVE (" ");
}


static void
gnc_plugin_page_register_cmd_paste_transaction (GSimpleAction *simple,
                                                GVariant      *paramter,
                                                gpointer       user_data)
{
    auto page = GNC_PLUGIN_PAGE_REGISTER(user_data);
    GncPluginPageRegisterPrivate* priv;
    SplitRegister* reg;

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER (page));

    ENTER ("(action %p, page %p)", simple, page);
    priv = GNC_PLUGIN_PAGE_REGISTER_GET_PRIVATE (page);
    reg = gnc_ledger_display_get_split_register (priv->ledger);
    gnc_split_register_paste_current (reg);
    LEAVE (" ");
}


struct VoidTransactionRequest
{
    gatomicrefcount ref_count;
    GWeakRef page;
    GWeakRef parent;
    GtkWindow* dialog;
    GtkEntry* reason;
    GCancellable* cancellable;
    gulong parent_destroy_handler;
    GncGUID transaction_guid;
    gboolean completed;
};

static VoidTransactionRequest*
void_transaction_request_ref (VoidTransactionRequest* request)
{
    g_atomic_ref_count_inc (&request->ref_count);
    return request;
}



static void
void_transaction_request_free (VoidTransactionRequest* request)
{
    GtkWidget* parent = GTK_WIDGET (g_weak_ref_get (&request->parent));

    if (parent && request->parent_destroy_handler)
        g_signal_handler_disconnect (parent, request->parent_destroy_handler);
    g_clear_object (&parent);
    g_weak_ref_clear (&request->parent);
    g_weak_ref_clear (&request->page);
    g_clear_object (&request->cancellable);
    g_free (request);
}

static void
void_transaction_request_unref (VoidTransactionRequest* request)
{
    if (request && g_atomic_ref_count_dec (&request->ref_count))
        void_transaction_request_free (request);
}

static void
void_transaction_request_complete (VoidTransactionRequest* request)
{
    GncPluginPageRegister* page;

    if (!request || request->completed)
        return;

    request->completed = TRUE;
    g_cancellable_cancel (request->cancellable);
    page = GNC_PLUGIN_PAGE_REGISTER (g_weak_ref_get (&request->page));
    if (page)
    {
        auto priv = GNC_PLUGIN_PAGE_REGISTER_GET_PRIVATE (page);

        if (priv->void_transaction_request == request)
            priv->void_transaction_request = nullptr;
        g_object_unref (page);
    }

    if (request->dialog)
    {
        auto dialog = g_steal_pointer (&request->dialog);

        g_signal_handlers_disconnect_by_data (dialog, request);
        gtk_window_destroy (dialog);
        g_object_unref (dialog);
    }
    void_transaction_request_unref (request);
}

static void
void_transaction_request_cancel (VoidTransactionRequest* request)
{
    void_transaction_request_complete (request);
}

static void
void_transaction_parent_destroyed_cb (GtkWidget* parent,
                                      VoidTransactionRequest* request)
{
    (void)parent;
    request->parent_destroy_handler = 0;
    void_transaction_request_cancel (request);
}

static gboolean
void_transaction_close_request_cb (GtkWindow* dialog,
                                   VoidTransactionRequest* request)
{
    (void)dialog;
    void_transaction_request_cancel (request);
    return TRUE;
}

static gboolean
void_transaction_is_eligible (GtkWindow* parent, Transaction* transaction,
                              gboolean report_errors)
{
    const char* reason;

    if (!transaction || xaccTransHasSplitsInState (transaction, VREC))
        return FALSE;

    if (xaccTransHasReconciledSplits (transaction) ||
        xaccTransHasSplitsInState (transaction, CREC))
    {
        if (report_errors && parent)
            gnc_error_dialog
                (parent, "%s",
                 _("You cannot void a transaction with reconciled or cleared splits."));
        return FALSE;
    }

    reason = xaccTransGetReadOnly (transaction);
    if (reason)
    {
        if (report_errors && parent)
            gnc_error_dialog
                (parent,
                 _("This transaction is marked read-only with the comment: '%s'"),
                 reason);
        return FALSE;
    }
    return TRUE;
}

static void
void_transaction_ok_clicked_cb (GtkButton* button,
                                VoidTransactionRequest* request)
{
    auto page = GNC_PLUGIN_PAGE_REGISTER (g_weak_ref_get (&request->page));

    if (page)
    {
        auto priv = GNC_PLUGIN_PAGE_REGISTER_GET_PRIVATE (page);
        auto reg = priv->ledger ?
            gnc_ledger_display_get_split_register (priv->ledger) : nullptr;
        auto transaction = reg ? gnc_split_register_get_current_trans (reg) : nullptr;

        if (reg && transaction &&
            guid_equal (xaccTransGetGUID (transaction),
                        &request->transaction_guid) &&
            !qof_book_is_readonly (gnc_get_current_book ()) &&
            priv->gsr && !gnc_split_reg_get_read_only (priv->gsr) &&
            void_transaction_is_eligible (nullptr, transaction, FALSE))
        {
            auto reason = g_strdup
                (gtk_editable_get_text (GTK_EDITABLE (request->reason)));

            gnc_split_register_void_current_trans (reg, reason ? reason : "");
            g_free (reason);
        }
        g_object_unref (page);
    }
    void_transaction_request_complete (request);
    (void)button;
}

static void
void_transaction_cancel_clicked_cb (GtkButton* button,
                                    VoidTransactionRequest* request)
{
    void_transaction_request_cancel (request);
    (void)button;
}

static void
void_transaction_show_dialog (GncPluginPageRegister* page,
                              VoidTransactionRequest* request)
{
    GtkBuilder* builder;
    GtkWindow* parent;
    GtkWindow* dialog;
    GtkEntry* reason;
    GtkWidget* cancel_button;
    GtkWidget* ok_button;

    if (request->completed)
        return;

    parent = GTK_WINDOW (gnc_plugin_page_get_window (GNC_PLUGIN_PAGE (page)));
    if (!parent)
    {
        void_transaction_request_complete (request);
        return;
    }

    builder = gtk_builder_new ();
    if (!gnc_builder_add_from_file (builder, "gnc-plugin-page-register.glade",
                                    "void_transaction_window"))
    {
        g_object_unref (builder);
        void_transaction_request_complete (request);
        return;
    }

    dialog = GTK_WINDOW (gtk_builder_get_object (builder, "void_transaction_window"));
    reason = GTK_ENTRY (gtk_builder_get_object (builder, "reason"));
    cancel_button = GTK_WIDGET (gtk_builder_get_object (builder, "cancelbutton1"));
    ok_button = GTK_WIDGET (gtk_builder_get_object (builder, "okbutton1"));
    if (!dialog || !reason || !cancel_button || !ok_button)
    {
        g_object_unref (builder);
        void_transaction_request_complete (request);
        return;
    }

    request->dialog = g_object_ref (dialog);
    request->reason = reason;
    gtk_widget_set_name (GTK_WIDGET (dialog), "gnc-id-void-transaction");
    gtk_window_set_transient_for (dialog, parent);
    gtk_window_set_modal (dialog, TRUE);
    gtk_window_set_default_widget (dialog, ok_button);
    gtk_widget_grab_focus (GTK_WIDGET (reason));
    g_signal_connect (ok_button, "clicked",
                      G_CALLBACK (void_transaction_ok_clicked_cb), request);
    g_signal_connect (cancel_button, "clicked",
                      G_CALLBACK (void_transaction_cancel_clicked_cb), request);
    g_signal_connect (dialog, "close-request",
                      G_CALLBACK (void_transaction_close_request_cb), request);
    g_object_unref (builder);
    gtk_window_present (dialog);
}

static void
void_transaction_pending_finished (GncPluginPageRegister* page,
                                   gboolean accepted, gpointer user_data)
{
    auto request = static_cast<VoidTransactionRequest*> (user_data);
    if (!page || !accepted || request->completed)
    {
        void_transaction_request_complete (request);
        return;
    }

    auto priv = GNC_PLUGIN_PAGE_REGISTER_GET_PRIVATE (page);
    auto reg = priv->ledger ?
        gnc_ledger_display_get_split_register (priv->ledger) : nullptr;
    auto transaction = reg ? gnc_split_register_get_current_trans (reg) : nullptr;
    auto parent = GTK_WINDOW (gnc_plugin_page_get_window (GNC_PLUGIN_PAGE (page)));

    if (!accepted || request->completed || !reg || !transaction || !parent ||
        !guid_equal (xaccTransGetGUID (transaction), &request->transaction_guid) ||
        !void_transaction_is_eligible (parent, transaction, FALSE))
    {
        void_transaction_request_complete (request);
        return;
    }
    void_transaction_show_dialog (page, request);
}

static void
gnc_plugin_page_register_cmd_void_transaction (GSimpleAction *simple,
                                               GVariant      *paramter,
                                               gpointer       user_data)
{
    auto page = GNC_PLUGIN_PAGE_REGISTER(user_data);
    GncPluginPageRegisterPrivate* priv;
    SplitRegister* reg;
    Transaction* transaction;
    GtkWindow* window;
    VoidTransactionRequest* request;

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER (page));

    ENTER ("(action %p, page %p)", simple, page);
    priv = GNC_PLUGIN_PAGE_REGISTER_GET_PRIVATE (page);
    if (priv->void_transaction_request)
    {
        if (priv->void_transaction_request->dialog)
            gtk_window_present (priv->void_transaction_request->dialog);
        LEAVE ("void request already active");
        return;
    }
    if (priv->finish_pending_request)
    {
        LEAVE ("pending request already active");
        return;
    }

    window = GTK_WINDOW (gnc_plugin_page_get_window (GNC_PLUGIN_PAGE (page)));
    reg = priv->ledger ? gnc_ledger_display_get_split_register (priv->ledger) : nullptr;
    transaction = reg ? gnc_split_register_get_current_trans (reg) : nullptr;
    if (!window || !void_transaction_is_eligible (window, transaction, TRUE))
    {
        LEAVE ("transaction cannot be voided");
        return;
    }

    request = g_new0 (VoidTransactionRequest, 1);
    g_atomic_ref_count_init (&request->ref_count);
    g_weak_ref_init (&request->page, page);
    g_weak_ref_init (&request->parent, window);
    request->cancellable = g_cancellable_new ();
    request->transaction_guid = *xaccTransGetGUID (transaction);
    request->parent_destroy_handler = g_signal_connect
        (window, "destroy", G_CALLBACK (void_transaction_parent_destroyed_cb),
         request);
    priv->void_transaction_request = request;
    gnc_plugin_page_register_finish_pending_async
        (page, request->cancellable, void_transaction_pending_finished,
         void_transaction_request_ref (request),
         (GDestroyNotify)void_transaction_request_unref);
    LEAVE (" ");
}

static void
gnc_plugin_page_register_cmd_unvoid_transaction (GSimpleAction *simple,
                                                 GVariant      *paramter,
                                                 gpointer       user_data)
{
    auto page = GNC_PLUGIN_PAGE_REGISTER(user_data);
    GncPluginPageRegisterPrivate* priv;
    SplitRegister* reg;
    Transaction* trans;

    ENTER ("(action %p, page %p)", simple, page);

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER (page));

    priv = GNC_PLUGIN_PAGE_REGISTER_GET_PRIVATE (page);
    reg = gnc_ledger_display_get_split_register (priv->ledger);
    trans = gnc_split_register_get_current_trans (reg);
    if (!xaccTransHasSplitsInState (trans, VREC))
        return;
    gnc_split_register_unvoid_current_trans (reg);
    LEAVE (" ");
}

typedef struct
{
    GWeakRef page;
    QofBook *book;
    GncGUID transaction_guid;
    GncGUID account_guid;
} ReverseTransactionRequest;

typedef struct
{
    GWeakRef page;
    GWeakRef window;
    QofBook *book;
    GncGUID split_guid;
    GncGUID transaction_guid;
    gboolean expand_after_jump;
} RegisterRevealRequest;

static void
register_reveal_request_free (gpointer user_data)
{
    RegisterRevealRequest *request = static_cast<RegisterRevealRequest *> (user_data);

    g_weak_ref_clear (&request->window);
    g_weak_ref_clear (&request->page);
    g_free (request);
}

static void
register_reveal_finished (GNCSplitReg *gsr, Split *split,
                         GncSplitRegRevealResult result, gpointer user_data)
{
    RegisterRevealRequest *request = static_cast<RegisterRevealRequest *> (user_data);
    GncPluginPage *page = GNC_PLUGIN_PAGE (g_weak_ref_get (&request->page));
    GtkWindow *window = GTK_WINDOW (g_weak_ref_get (&request->window));
    Transaction *transaction;

    if (!page || !window || !GNC_IS_PLUGIN_PAGE_REGISTER (page) ||
        request->book != gnc_get_current_book () ||
        qof_book_shutting_down (request->book))
        goto out;

    transaction = xaccTransLookup (&request->transaction_guid, request->book);
    if (gnc_plugin_page_get_window (page) != GTK_WIDGET (window) ||
        gnc_plugin_page_register_get_gsr (page) != gsr ||
        xaccSplitLookup (&request->split_guid, request->book) != split ||
        !transaction || xaccSplitGetParent (split) != transaction)
        goto out;

    if (result == GNC_SPLIT_REG_REVEAL_FILTER_CLEARED)
        gnc_plugin_page_register_clear_current_filter (page);
    gnc_split_reg_jump_to_split (gsr, split);

    if (request->expand_after_jump)
    {
        auto reg = gsr->ledger ? gnc_ledger_display_get_split_register (gsr->ledger) : nullptr;
        if (reg)
        {
            gnc_split_register_expand_current_trans (reg, TRUE);
            gnc_split_reg_jump_to_split (gsr, split);
        }
    }

out:
    g_clear_object (&window);
    g_clear_object (&page);
}

static void
register_reveal_split_async (GncPluginPage *page, GNCSplitReg *gsr,
                             Split *split, gboolean expand_after_jump)
{
    RegisterRevealRequest *request;
    GtkWidget *window;
    Transaction *transaction;

    if (!page || !gsr || !split || !(window = gnc_plugin_page_get_window (page)) ||
        !(transaction = xaccSplitGetParent (split)))
        return;

    request = g_new0 (RegisterRevealRequest, 1);
    request->book = gnc_get_current_book ();
    request->split_guid = *xaccSplitGetGUID (split);
    request->transaction_guid = *xaccTransGetGUID (transaction);
    request->expand_after_jump = expand_after_jump;
    g_weak_ref_init (&request->page, G_OBJECT (page));
    g_weak_ref_init (&request->window, G_OBJECT (window));
    gnc_split_reg_reveal_split_async (gsr, split, register_reveal_finished, request,
                                      register_reveal_request_free);
}
static void
reverse_transaction_request_free (ReverseTransactionRequest *request)
{
    g_weak_ref_clear (&request->page);
    g_free (request);
}

static gboolean
reverse_transaction_request_context (ReverseTransactionRequest *request,
                                     GncPluginPageRegister **page_out,
                                     Transaction **transaction_out,
                                     Account **account_out)
{
    auto page = GNC_PLUGIN_PAGE_REGISTER (g_weak_ref_get (&request->page));
    if (!page || request->book != gnc_get_current_book ())
    {
        g_clear_object (&page);
        return FALSE;
    }

    auto priv = GNC_PLUGIN_PAGE_REGISTER_GET_PRIVATE (page);
    auto reg = priv->ledger ? gnc_ledger_display_get_split_register (priv->ledger) : nullptr;
    auto current = reg ? gnc_split_register_get_current_trans (reg) : nullptr;
    auto transaction = xaccTransLookup (&request->transaction_guid, request->book);
    auto account = xaccAccountLookup (&request->account_guid, request->book);
    if (!current || !transaction || !account ||
        !guid_equal (xaccTransGetGUID (current), &request->transaction_guid))
    {
        g_object_unref (page);
        return FALSE;
    }

    *page_out = page;
    *transaction_out = transaction;
    *account_out = account;
    return TRUE;
}

static void
reverse_transaction_request_finish (ReverseTransactionRequest *request,
                                    const GncDupTransResult *result)
{
    GncPluginPageRegister *page;
    Transaction *transaction;
    Account *account;

    if (reverse_transaction_request_context (request, &page, &transaction, &account))
    {
        auto new_transaction = xaccTransGetReversedBy (transaction);
        if (!new_transaction && result)
        {
            gnc_suspend_gui_refresh ();
            new_transaction = xaccTransReverse (transaction);
            xaccTransSetDatePostedSecsNormalized (new_transaction, result->date);
            xaccTransSetDateEnteredSecs (new_transaction, gnc_time (NULL));
            gnc_resume_gui_refresh ();
        }
        if (new_transaction)
        {
            auto gsr = gnc_plugin_page_register_get_gsr (GNC_PLUGIN_PAGE (page));
            auto split = xaccTransFindSplitByAccount (new_transaction, account);
            if (gsr && split)
                register_reveal_split_async (GNC_PLUGIN_PAGE (page), gsr, split, FALSE);
        }
        g_object_unref (page);
    }
    reverse_transaction_request_free (request);
}

static void
reverse_transaction_existing_finished (GtkWindow *parent, gint response,
                                       gpointer user_data)
{
    auto request = static_cast<ReverseTransactionRequest *> (user_data);
    (void)parent;
    if (response == GTK_RESPONSE_YES)
        reverse_transaction_request_finish (request, nullptr);
    else
        reverse_transaction_request_free (request);
}

static void
reverse_transaction_date_finished (GncDupTransResult *result, gpointer user_data)
{
    auto request = static_cast<ReverseTransactionRequest *> (user_data);
    if (result)
        reverse_transaction_request_finish (request, result);
    else
        reverse_transaction_request_free (request);
    gnc_dup_trans_result_free (result);
}

static void
gnc_plugin_page_register_cmd_reverse_transaction (GSimpleAction *simple,
                                                  GVariant      *paramter,
                                                  gpointer       user_data)
{
    auto page = GNC_PLUGIN_PAGE_REGISTER (user_data);
    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER (page));

    ENTER ("(action %p, page %p)", simple, page);
    auto priv = GNC_PLUGIN_PAGE_REGISTER_GET_PRIVATE (page);
    auto reg = priv->ledger ? gnc_ledger_display_get_split_register (priv->ledger) : nullptr;
    auto transaction = reg ? gnc_split_register_get_current_trans (reg) : nullptr;
    auto split = reg ? gnc_split_register_get_current_split (reg) : nullptr;
    auto account = split ? xaccSplitGetAccount (split) : nullptr;
    auto window = GTK_WINDOW (gnc_plugin_page_get_window (GNC_PLUGIN_PAGE (page)));
    if (!transaction || !account || !window)
        return;

    auto request = g_new0 (ReverseTransactionRequest, 1);
    request->book = gnc_get_current_book ();
    request->transaction_guid = *xaccTransGetGUID (transaction);
    request->account_guid = *xaccAccountGetGUID (account);
    g_weak_ref_init (&request->page, G_OBJECT (page));

    if (xaccTransGetReversedBy (transaction))
        gnc_verify_dialog_async (window, TRUE, reverse_transaction_existing_finished,
                                 request, "%s\n\n%s",
                                 _("A reversing entry has already been created for this transaction."),
                                 _("Jump to the transaction?"));
    else
        gnc_dup_time64_dialog_async (window, _("Reverse Transaction"),
                                     _("New Transaction Information"), gnc_time (NULL),
                                     reverse_transaction_date_finished, request);
    LEAVE (" ");
}

typedef struct
{
    GWeakRef page;
    QofBook *book;
} GotoDateRequest;

static void
goto_date_request_finished (GncDupTransResult *result, gpointer user_data)
{
    auto request = static_cast<GotoDateRequest *> (user_data);
    auto page = GNC_PLUGIN_PAGE_REGISTER (g_weak_ref_get (&request->page));

    if (result && page && request->book == gnc_get_current_book ())
    {
        auto gsr = gnc_plugin_page_register_get_gsr (GNC_PLUGIN_PAGE (page));
        auto query = gnc_plugin_page_register_get_query (GNC_PLUGIN_PAGE (page));
        if (gsr && query)
        {
            auto splits = g_list_sort (g_list_copy (qof_query_run (query)),
                                       (GCompareFunc)xaccSplitOrder);
            auto it = g_list_find_custom (splits, &result->date,
                                           (GCompareFunc)find_after_date);
            if (it)
                gnc_split_reg_jump_to_split (gsr, GNC_SPLIT (it->data));
            else
                gnc_split_reg_jump_to_blank (gsr);
            g_list_free (splits);
        }
    }
    g_clear_object (&page);
    gnc_dup_trans_result_free (result);
    g_weak_ref_clear (&request->page);
    g_free (request);
}
static bool
gnc_plugin_page_register_show_fs_save (GncPluginPageRegister* page)
{
    GncPluginPageRegisterPrivate* priv = GNC_PLUGIN_PAGE_REGISTER_GET_PRIVATE (page);
    GNCLedgerDisplayType ledger_type = gnc_ledger_display_type (priv->ledger);
    SplitRegister* reg = gnc_ledger_display_get_split_register (priv->ledger);

    if (ledger_type == LD_SINGLE || ledger_type == LD_SUBACCOUNT)
        return true;
    else
    {
        switch (reg->type)
        {
        case GENERAL_JOURNAL:
            return true;
            break;

        case INCOME_LEDGER:
        case PORTFOLIO_LEDGER:
        case SEARCH_LEDGER:
        default:
            return false;
            break;
        }
    }
}

static void
gnc_plugin_page_register_cmd_view_sort_by (GSimpleAction *simple,
                                           GVariant      *paramter,
                                           gpointer       user_data)
{
    auto page = GNC_PLUGIN_PAGE_REGISTER(user_data);
    GncPluginPageRegisterPrivate* priv;

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER(page));
    ENTER ("(action %p, page %p)", simple, page);

    priv = GNC_PLUGIN_PAGE_REGISTER_GET_PRIVATE(page);
    if (priv->sd.dialog)
    {
        gtk_window_present (GTK_WINDOW (priv->sd.dialog));
        LEAVE("existing dialog");
        return;
    }

    SplitRegister* reg = gnc_ledger_display_get_split_register (priv->ledger);
    bool show_save_button = gnc_plugin_page_register_show_fs_save (page);

    gnc_ppr_sort_dialog (GNC_PLUGIN_PAGE(page), reg,
                         &priv->sd, show_save_button);
}

static void
gnc_plugin_page_register_cmd_view_filter_by (GSimpleAction *simple,
                                             GVariant      *paramter,
                                             gpointer       user_data)
{
    auto page = GNC_PLUGIN_PAGE_REGISTER(user_data);
    GncPluginPageRegisterPrivate* priv;

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER(page));
    ENTER ("(action %p, page %p)", simple, page);

    priv = GNC_PLUGIN_PAGE_REGISTER_GET_PRIVATE(page);
    if (priv->fd.dialog)
    {
        gtk_window_present (GTK_WINDOW(priv->fd.dialog));
        LEAVE ("existing dialog");
        return;
    }

    Query* query = gnc_ledger_display_get_query (priv->ledger);
    bool show_save_button = gnc_plugin_page_register_show_fs_save (page);

    gnc_ppr_filter_by (GNC_PLUGIN_PAGE(page), query,
                       &priv->fd, show_save_button);

    LEAVE (" ");
}

static void
gnc_plugin_page_register_cmd_reload (GSimpleAction *simple,
                                     GVariant      *paramter,
                                     gpointer       user_data)
{
    auto page = GNC_PLUGIN_PAGE_REGISTER(user_data);
    GncPluginPageRegisterPrivate* priv;
    SplitRegister* reg;

    ENTER ("(action %p, page %p)", simple, page);

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER (page));

    priv = GNC_PLUGIN_PAGE_REGISTER_GET_PRIVATE (page);
    reg = gnc_ledger_display_get_split_register (priv->ledger);

    /* Check for trans being edited */
    if (gnc_split_register_changed (reg))
    {
        LEAVE ("register has pending edits");
        return;
    }
    gnc_ledger_display_refresh (priv->ledger);
    LEAVE (" ");
}

static void
gnc_plugin_page_register_cmd_style_changed (GSimpleAction *simple,
                                            GVariant      *parameter,
                                            gpointer       user_data)
{
    auto page = GNC_PLUGIN_PAGE_REGISTER(user_data);
    GncPluginPageRegisterPrivate* priv;
    SplitRegisterStyle value;

    ENTER ("(action %p, page %p)", simple, page);

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER (page));

    priv = GNC_PLUGIN_PAGE_REGISTER_GET_PRIVATE (page);

    value = (SplitRegisterStyle)g_variant_get_int32 (parameter);

    g_action_change_state (G_ACTION(simple), parameter);

    gnc_split_reg_change_style (priv->gsr, value, priv->enable_refresh);

    gnc_plugin_page_register_ui_update (NULL, page);
    LEAVE (" ");
}

static void
gnc_plugin_page_register_cmd_style_double_line (GSimpleAction *simple,
                                                GVariant      *parameter,
                                                gpointer       user_data)
{
    auto page = GNC_PLUGIN_PAGE_REGISTER(user_data);
    GncPluginPageRegisterPrivate* priv;
    SplitRegister* reg;
    gboolean use_double_line;
    GVariant *state;

    ENTER ("(action %p, page %p)", simple, page);

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER(page));

    priv = GNC_PLUGIN_PAGE_REGISTER_GET_PRIVATE (page);
    reg = gnc_ledger_display_get_split_register (priv->ledger);

    state = g_action_get_state (G_ACTION(simple));

    g_action_change_state (G_ACTION(simple), g_variant_new_boolean (!g_variant_get_boolean (state)));

    use_double_line = !g_variant_get_boolean (state);

    if (use_double_line != reg->use_double_line)
    {
        gnc_split_register_config (reg, reg->type, reg->style, use_double_line);
        if (priv->enable_refresh)
            gnc_ledger_display_refresh (priv->ledger);
    }
    g_variant_unref (state);
    LEAVE (" ");
}

static void
gnc_plugin_page_register_cmd_transfer (GSimpleAction *simple,
                                       GVariant      *paramter,
                                       gpointer       user_data)
{
    auto page = GNC_PLUGIN_PAGE_REGISTER(user_data);
    Account* account;
    GncWindow* gnc_window;
    GtkWidget* window;

    ENTER ("(action %p, page %p)", simple, page);

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER (page));

    account = gnc_plugin_page_register_get_account (page);
    gnc_window = GNC_WINDOW (GNC_PLUGIN_PAGE (page)->window);
    window = GTK_WIDGET (gnc_window_get_gtk_window (gnc_window));
    gnc_xfer_dialog (window, account);
    LEAVE (" ");
}

static void
reconcile_pending_finished (GncPluginPageRegister* page, gboolean accepted,
                            gpointer user_data)
{
    Account* account;
    GtkWindow* window;

    if (!accepted)
        return;

    auto priv = GNC_PLUGIN_PAGE_REGISTER_GET_PRIVATE (page);
    if (!priv->ledger || !GNC_PLUGIN_PAGE (page)->window)
        return;

    account = gnc_plugin_page_register_get_account (page);
    window = gnc_window_get_gtk_window
        (GNC_WINDOW (GNC_PLUGIN_PAGE (page)->window));
    if (!account || !window)
        return;

    recnWindow (GTK_WIDGET (window), account);
    (void)user_data;
}
static void
gnc_plugin_page_register_cmd_reconcile (GSimpleAction *simple,
                                        GVariant      *paramter,
                                        gpointer       user_data)
{
    auto page = GNC_PLUGIN_PAGE_REGISTER(user_data);

    ENTER ("(action %p, page %p)", simple, page);
    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER (page));

    /* Pending edits must finish before Reconcile starts. Unlike the old
     * nested loop this continuation cannot resume after the page closes. */
    gnc_plugin_page_register_finish_pending_async
        (page, nullptr, reconcile_pending_finished, nullptr, nullptr);
    LEAVE (" ");
}

static void
gnc_plugin_page_register_cmd_stock_assistant (GSimpleAction *simple,
                                              GVariant      *paramter,
                                              gpointer       user_data)
{
    auto page = GNC_PLUGIN_PAGE_REGISTER(user_data);
    Account *account;
    GtkWindow *window;

    ENTER ("(action %p, page %p)", simple, page);

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER (page));
    window = gnc_window_get_gtk_window (GNC_WINDOW (GNC_PLUGIN_PAGE (page)->window));
    account = gnc_plugin_page_register_get_account (page);
    gnc_stock_transaction_assistant (GTK_WIDGET (window), account);

    LEAVE (" ");
}

static void
gnc_plugin_page_register_cmd_autoclear (GSimpleAction *simple,
                                        GVariant      *paramter,
                                        gpointer       user_data)
{
    auto page = GNC_PLUGIN_PAGE_REGISTER(user_data);
    Account* account;
    GtkWindow* window;
    AutoClearWindow* autoClearData;

    ENTER ("(action %p, page %p)", simple, page);

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER (page));

    account = gnc_plugin_page_register_get_account (page);

    window = gnc_window_get_gtk_window (GNC_WINDOW (GNC_PLUGIN_PAGE (
                                                        page)->window));
    autoClearData = autoClearWindow (GTK_WIDGET (window), account);
    gnc_ui_autoclear_window_raise (autoClearData);
    LEAVE (" ");
}

static void
gnc_plugin_page_register_cmd_stock_split (GSimpleAction *simple,
                                          GVariant      *paramter,
                                          gpointer       user_data)
{
    auto page = GNC_PLUGIN_PAGE_REGISTER(user_data);
    Account* account;
    GtkWindow* window;

    ENTER ("(action %p, page %p)", simple, page);

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER (page));

    account = gnc_plugin_page_register_get_account (page);
    window = gnc_window_get_gtk_window (GNC_WINDOW (GNC_PLUGIN_PAGE (page)->window));
    gnc_stock_split_dialog (GTK_WIDGET (window), account);
    LEAVE (" ");
}

static void
gnc_plugin_page_register_cmd_lots (GSimpleAction *simple,
                                   GVariant      *paramter,
                                   gpointer       user_data)
{
    auto page = GNC_PLUGIN_PAGE_REGISTER(user_data);
    GtkWindow* window;
    Account* account;

    ENTER ("(action %p, page %p)", simple, page);

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER (page));

    window = gnc_window_get_gtk_window (GNC_WINDOW (GNC_PLUGIN_PAGE (
                                                        page)->window));
    account = gnc_plugin_page_register_get_account (page);
    gnc_lot_viewer_dialog (window, account);
    LEAVE (" ");
}

static void
gnc_plugin_page_register_cmd_enter_transaction (GSimpleAction *simple,
                                                GVariant      *paramter,
                                                gpointer       user_data)
{
    auto page = GNC_PLUGIN_PAGE_REGISTER(user_data);
    GncPluginPageRegisterPrivate* priv;

    ENTER ("(action %p, page %p)", simple, page);

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER (page));

    priv = GNC_PLUGIN_PAGE_REGISTER_GET_PRIVATE (page);
    gnc_split_reg_enter (priv->gsr, FALSE);
    LEAVE (" ");
}

static void
gnc_plugin_page_register_cmd_cancel_transaction (GSimpleAction *simple,
                                                 GVariant      *paramter,
                                                 gpointer       user_data)
{
    auto page = GNC_PLUGIN_PAGE_REGISTER(user_data);
    GncPluginPageRegisterPrivate* priv;

    ENTER ("(action %p, page %p)", simple, page);

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER (page));

    priv = GNC_PLUGIN_PAGE_REGISTER_GET_PRIVATE (page);
    gnc_split_register_cancel_cursor_trans_changes
    (gnc_ledger_display_get_split_register (priv->ledger));
    LEAVE (" ");
}

static void
gnc_plugin_page_register_cmd_delete_transaction (GSimpleAction *simple,
                                                 GVariant      *paramter,
                                                 gpointer       user_data)
{
    auto page = GNC_PLUGIN_PAGE_REGISTER(user_data);
    GncPluginPageRegisterPrivate* priv;

    ENTER ("(action %p, page %p)", simple, page);

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER (page));

    priv = GNC_PLUGIN_PAGE_REGISTER_GET_PRIVATE (page);
    gsr_default_delete_handler (priv->gsr, NULL);
    LEAVE (" ");

}

static void
gnc_plugin_page_register_cmd_linked_transaction (GSimpleAction *simple,
                                                 GVariant      *paramter,
                                                 gpointer       user_data)
{
    auto page = GNC_PLUGIN_PAGE_REGISTER(user_data);
    GncPluginPageRegisterPrivate* priv;

    ENTER ("(action %p, page %p)", simple, page);

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER (page));

    priv = GNC_PLUGIN_PAGE_REGISTER_GET_PRIVATE (page);
    gsr_default_doclink_handler (priv->gsr);
    gnc_plugin_page_register_ui_update (NULL, page);
    LEAVE (" ");
}

static void
gnc_plugin_page_register_cmd_linked_transaction_open (GSimpleAction *simple,
                                                      GVariant      *paramter,
                                                      gpointer       user_data)
{
    auto page = GNC_PLUGIN_PAGE_REGISTER(user_data);
    GncPluginPageRegisterPrivate* priv;

    ENTER ("(action %p, page %p)", simple, page);

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER (page));

    priv = GNC_PLUGIN_PAGE_REGISTER_GET_PRIVATE (page);
    gsr_default_doclink_open_handler (priv->gsr);
    LEAVE (" ");
}

static GncInvoice*
invoice_from_split (Split* split)
{
    GncInvoice* invoice;
    GNCLot* lot;

    if (!split)
        return NULL;

    lot = xaccSplitGetLot (split);
    if (!lot)
        return NULL;

    invoice = gncInvoiceGetInvoiceFromLot (lot);
    if (!invoice)
        return NULL;

    return invoice;
}


struct LinkedInvoiceChoiceRequest
{
    GWeakRef page;
    GWeakRef window;
    QofBook *book;
    GncGUID book_guid;
    GncGUID transaction_guid;
    GList *invoice_guids;
};

static void
linked_invoice_choice_request_free (LinkedInvoiceChoiceRequest *request)
{
    g_weak_ref_clear (&request->window);
    g_weak_ref_clear (&request->page);
    g_list_free_full (request->invoice_guids, (GDestroyNotify)guid_free);
    g_free (request);
}

static gboolean
linked_invoice_choice_request_context (LinkedInvoiceChoiceRequest *request,
                                       gint choice, GtkWindow **parent_out,
                                       GncInvoice **invoice_out)
{
    auto page = GNC_PLUGIN_PAGE_REGISTER (g_weak_ref_get (&request->page));
    auto parent = GTK_WINDOW (g_weak_ref_get (&request->window));
    auto book = gnc_get_current_book ();
    GncGUID *invoice_guid;
    GncInvoice *invoice;
    GncPluginPageRegisterPrivate *priv = nullptr;
    SplitRegister *reg = nullptr;
    Transaction *transaction = nullptr;
    gboolean linked = FALSE;

    if (choice < 0 || !page || !parent || !book || request->book != book ||
        !guid_equal (&request->book_guid, qof_book_get_guid (book)) ||
        qof_book_shutting_down (book) ||
        gnc_plugin_page_get_window (GNC_PLUGIN_PAGE (page)) != GTK_WIDGET (parent))
        goto out;

    invoice_guid = static_cast<GncGUID *> (g_list_nth_data (request->invoice_guids,
                                                             choice));
    if (!invoice_guid)
        goto out;

    priv = GNC_PLUGIN_PAGE_REGISTER_GET_PRIVATE (page);
    reg = priv->ledger ? gnc_ledger_display_get_split_register (priv->ledger) : nullptr;
    transaction = reg ? gnc_split_register_get_current_trans (reg) : nullptr;
    if (!transaction ||
        !guid_equal (xaccTransGetGUID (transaction), &request->transaction_guid) ||
        xaccTransLookup (&request->transaction_guid, book) != transaction)
        goto out;

    invoice = gncInvoiceLookup (book, invoice_guid);
    if (!invoice || gncInvoiceGetBook (invoice) != book ||
        !guid_equal (gncInvoiceGetGUID (invoice), invoice_guid))
        goto out;

    for (auto linked_invoice : invoices_from_transaction (transaction))
    {
        if (guid_equal (gncInvoiceGetGUID (linked_invoice), invoice_guid))
        {
            linked = TRUE;
            break;
        }
    }
    if (!linked)
        goto out;

    g_object_unref (page);
    *parent_out = parent;
    *invoice_out = invoice;
    return TRUE;

out:
    g_clear_object (&parent);
    g_clear_object (&page);
    return FALSE;
}

static void
linked_invoice_choice_finished (GtkWindow *dialog_parent, gint choice,
                                gpointer user_data)
{
    auto request = static_cast<LinkedInvoiceChoiceRequest *> (user_data);
    GtkWindow *parent = nullptr;
    GncInvoice *invoice = nullptr;

    if (linked_invoice_choice_request_context (request, choice, &parent, &invoice))
    {
        gnc_ui_invoice_edit (parent, invoice);
        g_clear_object (&parent);
    }
    linked_invoice_choice_request_free (request);
    (void)dialog_parent;
}

static void
gnc_plugin_page_register_cmd_jump_linked_invoice (GSimpleAction *simple,
                                                  GVariant      *paramter,
                                                  gpointer       user_data)
{
    auto page = GNC_PLUGIN_PAGE_REGISTER(user_data);
    GncPluginPageRegisterPrivate* priv;
    SplitRegister* reg;
    GncInvoice* invoice;
    Transaction *txn;
    GtkWindow *parent;
    QofBook *book;

    ENTER ("(action %p, page %p)", simple, page);

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER (page));
    priv = GNC_PLUGIN_PAGE_REGISTER_GET_PRIVATE (page);
    reg = priv->gsr && priv->gsr->ledger ?
        gnc_ledger_display_get_split_register (priv->gsr->ledger) : nullptr;
    txn = reg ? gnc_split_register_get_current_trans (reg) : nullptr;
    invoice = reg ? invoice_from_split (gnc_split_register_get_current_split (reg)) : nullptr;
    parent = GTK_WINDOW (gnc_plugin_page_get_window (GNC_PLUGIN_PAGE (page)));
    book = gnc_get_current_book ();

    if (!reg || !txn || !parent || !book || qof_book_shutting_down (book) ||
        xaccTransGetBook (txn) != book)
    {
        LEAVE ("missing current register context");
        return;
    }

    if (!invoice)
    {
        auto invoices = invoices_from_transaction (txn);
        if (invoices.empty())
        {
            PERR ("shouldn't happen: if no invoices, function is never called");
            LEAVE ("no linked invoices");
            return;
        }
        if (invoices.size() == 1)
            invoice = invoices[0];
        else
        {
            auto request = g_new0 (LinkedInvoiceChoiceRequest, 1);
            GList *details = NULL;

            request->book = book;
            request->book_guid = *qof_book_get_guid (book);
            request->transaction_guid = *xaccTransGetGUID (txn);
            g_weak_ref_init (&request->page, page);
            g_weak_ref_init (&request->window, parent);
            for (const auto& linked_invoice : invoices)
            {
                const gchar *amount;
                gchar *date;

                if (!linked_invoice || gncInvoiceGetBook (linked_invoice) != book)
                {
                    linked_invoice_choice_request_free (request);
                    g_list_free_full (details, g_free);
                    LEAVE ("invalid linked invoice");
                    return;
                }

                date = qof_print_date (gncInvoiceGetDatePosted (linked_invoice));
                amount = xaccPrintAmount
                    (gncInvoiceGetTotal (linked_invoice),
                     gnc_account_print_info (gncInvoiceGetPostedAcc (linked_invoice), TRUE));
                details = g_list_prepend
                    (details,
                     /* Translators: %s refer to the following in
                        order: invoice type, invoice ID, owner name,
                        posted date, amount */
                     g_strdup_printf (_("%s %s from %s, posted %s, amount %s"),
                                      gncInvoiceGetTypeString (linked_invoice),
                                      gncInvoiceGetID (linked_invoice),
                                      gncOwnerGetName (gncInvoiceGetOwner (linked_invoice)),
                                      date, amount));
                request->invoice_guids = g_list_prepend
                    (request->invoice_guids, guid_copy (gncInvoiceGetGUID (linked_invoice)));
                g_free (date);
            }
            details = g_list_reverse (details);
            request->invoice_guids = g_list_reverse (request->invoice_guids);
            gnc_choose_option_dialog_async
                (parent, _("Select Business Item"),
                 _("Several business items are linked with this transaction. \
Please choose one:"), details, 0, linked_invoice_choice_finished, request);
            g_list_free_full (details, g_free);
            LEAVE ("linked invoice choice request started");
            return;
        }
    }

    if (invoice && gncInvoiceGetBook (invoice) == book)
        gnc_ui_invoice_edit (parent, invoice);

    LEAVE (" ");
}
typedef struct
{
    GWeakRef page;
    QofBook *book;
} RegisterPageBlankRequest;

static void
register_page_blank_save_finished (SplitRegister *reg, gboolean saved,
                                   gpointer user_data)
{
    auto request = static_cast<RegisterPageBlankRequest *> (user_data);
    auto page = GNC_PLUGIN_PAGE_REGISTER (g_weak_ref_get (&request->page));

    if (page && request->book == gnc_get_current_book ())
    {
        auto priv = GNC_PLUGIN_PAGE_REGISTER_GET_PRIVATE (page);
        if (saved && priv->ledger && priv->gsr &&
            reg == gnc_ledger_display_get_split_register (priv->ledger))
        {
            gnc_split_register_redraw (reg);
            gnc_split_reg_jump_to_blank (priv->gsr);
        }
    }
    if (page)
        g_object_unref (page);
    g_weak_ref_clear (&request->page);
    g_free (request);
}
static void
gnc_plugin_page_register_cmd_blank_transaction (GSimpleAction *simple,
                                                GVariant      *paramter,
                                                gpointer       user_data)
{
    auto page = GNC_PLUGIN_PAGE_REGISTER(user_data);
    GncPluginPageRegisterPrivate* priv;
    SplitRegister* reg;

    ENTER ("(action %p, page %p)", simple, page);

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER (page));

    priv = GNC_PLUGIN_PAGE_REGISTER_GET_PRIVATE (page);
    reg = gnc_ledger_display_get_split_register (priv->ledger);

    auto request = g_new0 (RegisterPageBlankRequest, 1);
    request->book = gnc_get_current_book ();
    g_weak_ref_init (&request->page, page);
    gnc_split_register_save_async (reg, TRUE, register_page_blank_save_finished,
                                   request);
    LEAVE ("save request started");
}

static bool
find_after_date (Split *split, time64* find_date)
{
    auto trans = xaccSplitGetParent (split);
    return !(xaccSplitGetAccount (split) != nullptr &&
             xaccTransGetDate (trans) >= *find_date &&
             xaccTransCountSplits (trans) != 1);
}

static void
gnc_plugin_page_register_cmd_goto_date (GSimpleAction *simple,
                                        GVariant      *paramter,
                                        gpointer       user_data)
{
    auto page = GNC_PLUGIN_PAGE_REGISTER (user_data);
    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER (page));

    ENTER ("(action %p, page %p)", simple, page);
    auto window = GTK_WINDOW (gnc_plugin_page_get_window (GNC_PLUGIN_PAGE (page)));
    if (!window)
        return;

    auto request = g_new0 (GotoDateRequest, 1);
    request->book = gnc_get_current_book ();
    g_weak_ref_init (&request->page, G_OBJECT (page));
    gnc_dup_time64_dialog_async (window, _("Go to Date"), _("Go to Date"),
                                 gnc_time (NULL), goto_date_request_finished, request);
    LEAVE (" ");
}
static void
gnc_plugin_page_register_cmd_duplicate_transaction (GSimpleAction *simple,
                                                    GVariant      *paramter,
                                                    gpointer       user_data)
{
    auto page = GNC_PLUGIN_PAGE_REGISTER(user_data);
    GncPluginPageRegisterPrivate* priv;

    ENTER ("(action %p, page %p)", simple, page);

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER (page));

    priv = GNC_PLUGIN_PAGE_REGISTER_GET_PRIVATE (page);
    gnc_split_register_duplicate_current_async (gnc_ledger_display_get_split_register (priv->ledger), G_OBJECT (page));
    LEAVE (" ");
}

static void
gnc_plugin_page_register_cmd_reinitialize_transaction (GSimpleAction *simple,
                                                       GVariant      *paramter,
                                                       gpointer       user_data)
{
    auto page = GNC_PLUGIN_PAGE_REGISTER(user_data);
    GncPluginPageRegisterPrivate* priv;

    ENTER ("(action %p, page %p)", simple, page);

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER (page));

    priv = GNC_PLUGIN_PAGE_REGISTER_GET_PRIVATE (page);
    gsr_default_reinit_handler (priv->gsr, NULL);
    LEAVE (" ");
}

static void
gnc_plugin_page_register_cmd_expand_transaction (GSimpleAction *simple,
                                                 GVariant      *parameter,
                                                 gpointer       user_data)
{
    auto page = GNC_PLUGIN_PAGE_REGISTER(user_data);
    GncPluginPageRegisterPrivate* priv;
    SplitRegister* reg;
    gboolean expand;
    GVariant *state;

    ENTER ("(action %p, page %p)", simple, page);

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER (page));

    priv = GNC_PLUGIN_PAGE_REGISTER_GET_PRIVATE (page);
    reg = gnc_ledger_display_get_split_register (priv->ledger);

    state = g_action_get_state (G_ACTION(simple));

    g_action_change_state (G_ACTION(simple), g_variant_new_boolean (!g_variant_get_boolean (state)));

    expand = !g_variant_get_boolean (state);

    gnc_split_register_expand_current_trans (reg, expand);
    g_variant_unref (state);
    LEAVE (" ");
}

/** Callback for "Edit Exchange Rate" menu item.
 */
static void
gnc_plugin_page_register_cmd_exchange_rate (GSimpleAction *simple,
                                            GVariant      *paramter,
                                            gpointer       user_data)
{
    auto page = GNC_PLUGIN_PAGE_REGISTER(user_data);
    GncPluginPageRegisterPrivate* priv;
    SplitRegister* reg;

    ENTER ("(action %p, page %p)", simple, page);

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER (page));

    priv = GNC_PLUGIN_PAGE_REGISTER_GET_PRIVATE (page);
    reg = gnc_ledger_display_get_split_register (priv->ledger);

    /* XXX Ignore the return value -- we don't care if this succeeds */
    (void)gnc_split_register_handle_exchange (reg, TRUE);
    LEAVE (" ");
}

static Split*
jump_multiple_splits_by_single_account (Account *account, Split *split)
{
    Transaction *trans;
    SplitList *splits;
    Account *other_account = NULL;
    Split *other_split = NULL;

    trans = xaccSplitGetParent(split);
    if (!trans)
        return NULL;

    for (splits = xaccTransGetSplitList(trans); splits; splits = splits->next)
    {
        Split *s = (Split*)splits->data;
        Account *a = xaccSplitGetAccount(s);

        if (!xaccTransStillHasSplit(trans, s))
            continue;

        if (a == account)
            continue;

        if (other_split)
        {
            if (other_account != a)
                return NULL;

            continue;
        }

        other_account = a;
        other_split = s;
    }

    // Jump to the same account so that the right warning is triggered
    if (!other_split)
        other_split = split;

    return other_split;
}

static Split*
jump_multiple_splits_by_value (Account *account, Split *split, gboolean largest)
{
    Transaction *trans;
    SplitList *splits;
    Split *other_split = NULL;
    gnc_numeric best;
    int cmp = largest ? 1 : -1;

    trans = xaccSplitGetParent(split);
    if (!trans)
        return NULL;

    for (splits = xaccTransGetSplitList(trans); splits; splits = splits->next)
    {
        Split *s = (Split*)splits->data;
        gnc_numeric value;

        if (!xaccTransStillHasSplit(trans, s))
            continue;

        if (xaccSplitGetAccount(s) == account)
            continue;

        value = gnc_numeric_abs(xaccSplitGetValue(s));
        if (gnc_numeric_check(value))
            continue;

        /* For splits with the same value as the best, the first split
         * encountered is used.
         */
        if (other_split && gnc_numeric_compare(value, best) != cmp)
            continue;

        best = value;
        other_split = s;
    }

    // Jump to the same account so that the right warning is triggered
    if (!other_split)
        other_split = split;

    return other_split;
}

static Split*
jump_multiple_splits (Account* account, Split *split)
{
    GncPrefJumpMultSplits mode = (GncPrefJumpMultSplits)gnc_prefs_get_enum(GNC_PREFS_GROUP_GENERAL_REGISTER, GNC_PREF_JUMP_MULT_SPLITS);

    switch (mode)
    {
    case JUMP_LARGEST_VALUE_FIRST_SPLIT:
        return jump_multiple_splits_by_value (account, split, TRUE);

    case JUMP_SMALLEST_VALUE_FIRST_SPLIT:
        return jump_multiple_splits_by_value (account, split, FALSE);

    case JUMP_DEFAULT:
    default:
        break;
    }

    // If there's only one other account, use that one
    return jump_multiple_splits_by_single_account (account, split);
}

static void
gnc_plugin_page_register_cmd_jump (GSimpleAction *simple,
                                   GVariant      *paramter,
                                   gpointer       user_data)
{
    auto page = GNC_PLUGIN_PAGE_REGISTER(user_data);
    GncPluginPageRegisterPrivate* priv;
    GncPluginPage* new_plugin_page;
    GtkWidget* window;
    GNCSplitReg* gsr;
    SplitRegister* reg;
    Account* account;
    Account* leader;
    Split* split;
    Split* other_split;
    gboolean multiple_splits;

    ENTER ("(action %p, page %p)", simple, page);

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER (page));

    priv = GNC_PLUGIN_PAGE_REGISTER_GET_PRIVATE (page);
    window = GNC_PLUGIN_PAGE (page)->window;
    if (window == NULL)
    {
        LEAVE ("no window");
        return;
    }

    reg = gnc_ledger_display_get_split_register (priv->ledger);
    split = gnc_split_register_get_current_split (reg);
    if (split == NULL)
    {
        LEAVE ("no split (1)");
        return;
    }

    account = xaccSplitGetAccount (split);
    if (account == NULL)
    {
        LEAVE ("no account");
        return;
    }

    other_split = xaccSplitGetOtherSplit (split);
    multiple_splits = other_split == NULL;

    leader = gnc_ledger_display_leader (priv->ledger);
    if (account == leader)
    {
        CursorClass cursor_class = gnc_split_register_get_current_cursor_class (reg);
        if (cursor_class == CURSOR_CLASS_SPLIT)
        {
            /* If you've selected the transaction itself, we jump to the "other"
             * account corresponding to the anchoring split.
             *
             * If you've selected the split for another account, we jump to that
             * split's account (account != leader, so this block is never
             * reached).
             *
             * If you've selected a split for this account, for consistency with
             * selecting the split of another account we should do nothing.
             * You're already on the account for the split you selected. Jumping
             * to the "other" account now would make the "multiple split"
             * options confusing.
             *
             * We could jump to a different anchoring split but that'll be very
             * subtle and only cause problems because it'll have to save any
             * modifications to the current register.
             */
            LEAVE ("split for this account");
            return;
        }

        if (multiple_splits)
        {
            other_split = jump_multiple_splits (account, split);
        }
        if (other_split == NULL)
        {
            gnc_warning_dialog_async (
                GTK_WINDOW (window), GNC_PREF_WARN_REG_TRANS_JUMP_MULTIPLE_SPLITS,
                _("Unable to jump to other account"),
                _("This transaction involves more than one other account. Select a specific split to jump to that account."),
                _("_Close"), GTK_RESPONSE_CLOSE, TRUE, NULL, NULL);

            LEAVE ("no split (2)");
            return;
        }

        split = other_split;

        account = xaccSplitGetAccount (split);
        if (account == NULL)
        {
            LEAVE ("no account (2)");
            return;
        }

        if (account == leader)
        {
            gnc_warning_dialog_async (
                GTK_WINDOW (window), GNC_PREF_WARN_REG_TRANS_JUMP_SINGLE_ACCOUNT,
                _("Unable to jump to other account"),
                _("This transaction only involves the current account so there is no other account to jump to."),
                _("_Close"), GTK_RESPONSE_CLOSE, TRUE, NULL, NULL);

            LEAVE ("register open for account");
            return;
        }
    }

    new_plugin_page = gnc_plugin_page_register_new (account, FALSE);
    if (new_plugin_page == NULL)
    {
        LEAVE ("couldn't create new page");
        return;
    }

    gnc_main_window_open_page (GNC_MAIN_WINDOW (window), new_plugin_page);
    gsr = gnc_plugin_page_register_get_gsr (new_plugin_page);

    SplitRegister *new_page_reg = gnc_ledger_display_get_split_register (gsr->ledger);
    gboolean jump_twice = FALSE;

    /* Selecting the split (instead of just the transaction to open the "other"
     * account) requires jumping a second time after expanding the transaction,
     * in the basic and auto ledger modes.
     */
    if (new_page_reg->style != REG_STYLE_JOURNAL)
        jump_twice = TRUE;

    register_reveal_split_async (new_plugin_page, gsr, split,
                                 multiple_splits && jump_twice);
    LEAVE (" ");
}

static void
gnc_plugin_page_register_cmd_schedule (GSimpleAction *simple,
                                       GVariant      *paramter,
                                       gpointer       user_data)
{
    auto page = GNC_PLUGIN_PAGE_REGISTER(user_data);
    GncPluginPageRegisterPrivate* priv;
    GtkWindow* window;

    ENTER ("(action %p, page %p)", simple, page);

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER (page));

    window = GTK_WINDOW (gnc_plugin_page_get_window (GNC_PLUGIN_PAGE (
                                                         page)));
    priv = GNC_PLUGIN_PAGE_REGISTER_GET_PRIVATE (page);
    gsr_default_schedule_handler (priv->gsr, window);
    LEAVE (" ");
}

static GncScrubContext*
register_scrub_begin (GncPluginPageRegister* page, QofBook* book)
{
    auto priv = GNC_PLUGIN_PAGE_REGISTER_GET_PRIVATE (page);
    if (priv->scrub_context)
        return nullptr;

    auto context = gnc_scrub_context_begin (book);
    if (!context)
        return nullptr;

    priv->scrub_context = gnc_scrub_context_ref (context);
    return context;
}

static void
register_scrub_end (GncPluginPageRegister* page, GncScrubContext* context)
{
    auto priv = GNC_PLUGIN_PAGE_REGISTER_GET_PRIVATE (page);
    if (priv->scrub_context == context)
    {
        gnc_scrub_context_unref (priv->scrub_context);
        priv->scrub_context = nullptr;
    }
    gnc_scrub_context_end (context);
    gnc_scrub_context_unref (context);
}

static void
scrub_split (Split *split, GncScrubContext* context)
{
    Account *acct;
    Transaction *trans;
    GNCLot *lot;

    g_return_if_fail (split);
    acct = xaccSplitGetAccount (split);
    trans = xaccSplitGetParent (split);
    lot = xaccSplitGetLot (split);
    g_return_if_fail (trans);

    xaccTransScrubOrphansWithContext (trans, context);
    xaccTransScrubImbalanceWithContext (
        trans, gnc_get_current_root_account(), NULL, context);
    if (lot && acct && xaccAccountIsAPARType (xaccAccountGetType (acct)))
    {
        gncScrubBusinessLotWithContext (lot, context);
        gncScrubBusinessSplitWithContext (split, context);
    }
}

static void
gnc_plugin_page_register_cmd_scrub_current (GSimpleAction *simple,
                                            GVariant      *paramter,
                                            gpointer       user_data)
{
    auto page = GNC_PLUGIN_PAGE_REGISTER(user_data);
    GncPluginPageRegisterPrivate* priv;
    Query* query;
    SplitRegister* reg;
    Split* split;
    GncScrubContext* context;

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER (page));

    ENTER ("(action %p, page %p)", simple, page);

    priv = GNC_PLUGIN_PAGE_REGISTER_GET_PRIVATE (page);
    query = gnc_ledger_display_get_query (priv->ledger);
    if (query == NULL)
    {
        LEAVE ("no query found");
        return;
    }

    reg = gnc_ledger_display_get_split_register (priv->ledger);
    split = gnc_split_register_get_current_split (reg);
    if (!split)
        return;
    context = register_scrub_begin (
        page, qof_instance_get_book (QOF_INSTANCE (split)));
    if (!context)
        return;

    gnc_suspend_gui_refresh();
    scrub_split (split, context);
    register_scrub_end (page, context);
    gnc_resume_gui_refresh();
    LEAVE (" ");
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
    GtkWidget *widget;

    (void)keycode;
    (void)state;
    auto context = static_cast<GncScrubContext *> (user_data);
    if (keyval != GDK_KEY_Escape)
        return FALSE;

    widget = gtk_event_controller_get_widget (GTK_EVENT_CONTROLLER (key));
    if (widget && GTK_IS_WINDOW (widget))
        gnc_verify_dialog_async (GTK_WINDOW (widget), FALSE,
                                 scrub_abort_verify_finished,
                                 gnc_scrub_context_ref (context),
                                 "%s", _(check_repair_abort_YN));
    return TRUE;
}
static void
gnc_plugin_page_register_cmd_scrub_all (GSimpleAction *simple,
                                        GVariant      *paramter,
                                        gpointer       user_data)
{
    auto page = GNC_PLUGIN_PAGE_REGISTER(user_data);
    GncPluginPageRegisterPrivate* priv;
    Query* query;
    GncWindow* window;
    GList* node, *splits;
    gint split_count = 0, curr_split_no = 0;
    GtkEventController *scrub_key_controller;
    gulong scrub_kp_handler_ID;
    const char* message = _ ("Checking splits in current register: %u of %u");
    GncScrubContext* context;
    QofBook* book;

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER (page));

    ENTER ("(action %p, page %p)", simple, page);

    priv = GNC_PLUGIN_PAGE_REGISTER_GET_PRIVATE (page);
    query = gnc_ledger_display_get_query (priv->ledger);
    if (!query)
    {
        LEAVE ("no query found");
        return;
    }

    auto books = qof_query_get_books (query);
    book = books ? static_cast<QofBook *> (books->data) : nullptr;
    if (!book || books->next)
    {
        LEAVE ("query isn't bound to one book");
        return;
    }
    context = register_scrub_begin (page, book);
    if (!context)
        return;

    gnc_suspend_gui_refresh();
    window = GNC_WINDOW (GNC_PLUGIN_PAGE (page)->window);
    scrub_key_controller = gtk_event_controller_key_new ();
    gtk_widget_add_controller (GTK_WIDGET (window), scrub_key_controller);
    scrub_kp_handler_ID = g_signal_connect (scrub_key_controller, "key-pressed",
                                            G_CALLBACK (scrub_kp_handler), context);
    gnc_window_set_progressbar_window (window);

    splits = qof_query_run (query);
    split_count = g_list_length (splits);
    for (node = splits; node && !gnc_scrub_context_is_cancelled (context);
         node = node->next, curr_split_no++)
    {
        auto split = GNC_SPLIT(node->data);

        if (!split) continue;

        PINFO ("Start processing split %d of %d",
               curr_split_no + 1, split_count);

        scrub_split (split, context);

        PINFO ("Finished processing split %d of %d",
               curr_split_no + 1, split_count);

        if (curr_split_no % 10 == 0)
        {
            char* progress_msg = g_strdup_printf (message, curr_split_no, split_count);
            gnc_window_show_progress (progress_msg, (100 * curr_split_no) / split_count);
            g_free (progress_msg);
        }
    }

    g_signal_handler_disconnect (scrub_key_controller, scrub_kp_handler_ID);
    gtk_widget_remove_controller (GTK_WIDGET (window), scrub_key_controller);
    gnc_window_show_progress (NULL, -1.0);

    register_scrub_end (page, context);
    gnc_resume_gui_refresh();
    LEAVE (" ");
}

static void
gnc_plugin_page_register_cmd_account_report (GSimpleAction *simple,
                                             GVariant      *paramter,
                                             gpointer       user_data)
{
    auto page = GNC_PLUGIN_PAGE_REGISTER(user_data);
    GncPluginPageRegisterPrivate* priv;
    GncMainWindow* window;
    int id;

    ENTER ("(action %p, page %p)", simple, page);

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER (page));

    window = GNC_MAIN_WINDOW (GNC_PLUGIN_PAGE (page)->window);
    priv = GNC_PLUGIN_PAGE_REGISTER_GET_PRIVATE (page);
    id = report_helper (priv->ledger, NULL, NULL);
    if (id >= 0)
        gnc_main_window_open_report (id, window);
    LEAVE (" ");
}

static void
gnc_plugin_page_register_cmd_transaction_report (GSimpleAction *simple,
                                                 GVariant      *paramter,
                                                 gpointer       user_data)
{
    auto page = GNC_PLUGIN_PAGE_REGISTER(user_data);
    GncPluginPageRegisterPrivate* priv;
    GncMainWindow* window;
    SplitRegister* reg;
    Split* split;
    Query* query;
    int id;


    ENTER ("(action %p, page %p)", simple, page);

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER (page));

    priv = GNC_PLUGIN_PAGE_REGISTER_GET_PRIVATE (page);
    reg = gnc_ledger_display_get_split_register (priv->ledger);

    split = gnc_split_register_get_current_split (reg);
    if (!split)
        return;

    query = qof_query_create_for (GNC_ID_SPLIT);

    qof_query_set_book (query, gnc_get_current_book());

    xaccQueryAddGUIDMatch (query, xaccSplitGetGUID (split),
                           GNC_ID_SPLIT, QOF_QUERY_AND);

    window = GNC_MAIN_WINDOW (GNC_PLUGIN_PAGE (page)->window);
    id = report_helper (priv->ledger, split, query);
    if (id >= 0)
        gnc_main_window_open_report (id, window);
    LEAVE (" ");
}

/************************************************************/
/*                    Auxiliary functions                   */
/************************************************************/

void
gnc_plugin_page_register_set_options (GncPluginPage* plugin_page,
                                      gint lines_default,
                                      gboolean read_only)
{
    GncPluginPageRegister* page;
    GncPluginPageRegisterPrivate* priv;

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER (plugin_page));

    page = GNC_PLUGIN_PAGE_REGISTER (plugin_page);
    priv = GNC_PLUGIN_PAGE_REGISTER_GET_PRIVATE (page);
    priv->lines_default     = lines_default;
    priv->read_only         = read_only;
}

GNCSplitReg*
gnc_plugin_page_register_get_gsr (GncPluginPage* plugin_page)
{
    GncPluginPageRegister* page;
    GncPluginPageRegisterPrivate* priv;

    g_return_val_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER (plugin_page), NULL);

    page = GNC_PLUGIN_PAGE_REGISTER (plugin_page);
    priv = GNC_PLUGIN_PAGE_REGISTER_GET_PRIVATE (page);

    return priv->gsr;
}

FilterData *
gnc_plugin_page_register_get_filter_data (GncPluginPage *plugin_page)
{
    g_return_val_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER(plugin_page), NULL);

    GncPluginPageRegister* page = GNC_PLUGIN_PAGE_REGISTER(plugin_page);
    GncPluginPageRegisterPrivate* priv = GNC_PLUGIN_PAGE_REGISTER_GET_PRIVATE(page);

    return &priv->fd;
}

SortData *
gnc_plugin_page_register_get_sort_data (GncPluginPage *plugin_page)
{
    g_return_val_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER(plugin_page), NULL);

    GncPluginPageRegister* page = GNC_PLUGIN_PAGE_REGISTER(plugin_page);
    GncPluginPageRegisterPrivate* priv = GNC_PLUGIN_PAGE_REGISTER_GET_PRIVATE(page);

    return &priv->sd;
}

static void
gnc_plugin_page_help_changed_cb (GNCSplitReg* gsr,
                                 GncPluginPageRegister* page)
{
    GncPluginPageRegisterPrivate* priv;
    SplitRegister* reg;
    GncWindow* window;
    char* help;

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER (page));

    window = GNC_WINDOW (GNC_PLUGIN_PAGE (page)->window);
    if (!window)
    {
        // This routine can be called before the page is added to a
        // window.
        return;
    }

    // only update status text if on current page
    if (GNC_IS_MAIN_WINDOW(window) && (gnc_main_window_get_current_page
       (GNC_MAIN_WINDOW(window)) != GNC_PLUGIN_PAGE(page)))
       return;

    /* Get the text from the ledger */
    priv = GNC_PLUGIN_PAGE_REGISTER_GET_PRIVATE (page);
    reg = gnc_ledger_display_get_split_register (priv->ledger);
    help = gnc_table_get_help (reg->table);
    gnc_window_set_status (window, GNC_PLUGIN_PAGE (page), help);
    g_free (help);
}

static void
gnc_plugin_page_popup_menu_cb (GNCSplitReg* gsr,
                               GncPluginPageRegister* page)
{
    GncWindow* window;

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER (page));

    window = GNC_WINDOW (GNC_PLUGIN_PAGE (page)->window);
    if (!window)
    {
        // This routine can be called before the page is added to a
        // window.
        return;
    }
    gnc_main_window_popup_menu_cb (GTK_WIDGET (window),
                                   GNC_PLUGIN_PAGE (page));
}

static void
gnc_plugin_page_register_refresh_cb (GHashTable* changes, gpointer user_data)
{
    auto page = GNC_PLUGIN_PAGE_REGISTER(user_data);
    GncPluginPageRegisterPrivate* priv;

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER (page));
    priv = GNC_PLUGIN_PAGE_REGISTER_GET_PRIVATE (page);

    if (changes)
    {
        const EventInfo* ei;
        ei = gnc_gui_get_entity_events (changes, &priv->key);
        if (ei)
        {
            if (ei->event_mask & QOF_EVENT_DESTROY)
            {
                gnc_main_window_close_page (GNC_PLUGIN_PAGE (page));
                return;
            }
            if (ei->event_mask & QOF_EVENT_MODIFY)
            {
            }
        }
    }
    else
    {
        /* forced updates */
        gnucash_register_refresh_from_prefs (priv->gsr->reg);
        gtk_widget_queue_draw (priv->widget);
    }

    gnc_plugin_page_register_ui_update (NULL, page);
}

static void
gnc_plugin_page_register_close_cb (gpointer user_data)
{
    GncPluginPage* plugin_page = GNC_PLUGIN_PAGE (user_data);
    gnc_main_window_close_page (plugin_page);
}

/** This function is called when an account has been edited and an
 *  "extreme" change has been made to it.  (E.G. Changing from a
 *  credit card account to an expense account.  This routine is
 *  responsible for finding all open registers containing the account
 *  and closing them.
 *
 *  @param account A pointer to the account that was changed.
 */
static void
gppr_account_destroy_cb (Account* account)
{
    GncPluginPageRegister* page;
    GncPluginPageRegisterPrivate* priv;
    GNCLedgerDisplayType ledger_type;
    const GncGUID* acct_guid;
    const GList* citem;
    GList* item, *kill = NULL;

    acct_guid = xaccAccountGetGUID (account);

    /* Find all windows that need to be killed.  Don't kill them yet, as
     * that would affect the list being walked.*/
    citem = gnc_gobject_tracking_get_list (GNC_PLUGIN_PAGE_REGISTER_NAME);
    for (; citem; citem = g_list_next (citem))
    {
        page = (GncPluginPageRegister*)citem->data;
        priv = GNC_PLUGIN_PAGE_REGISTER_GET_PRIVATE (page);
        ledger_type = gnc_ledger_display_type (priv->ledger);
        if (ledger_type == LD_GL)
        {
            kill = g_list_prepend (kill, page);
            /* kill it */
        }
        else if ((ledger_type == LD_SINGLE) || (ledger_type == LD_SUBACCOUNT))
        {
            if (guid_compare (acct_guid, &priv->key) == 0)
            {
                kill = g_list_prepend (kill, page);
            }
        }
    }

    kill = g_list_reverse (kill);
    /* Now kill them. */
    for (item = kill; item; item = g_list_next (item))
    {
        page = (GncPluginPageRegister*)item->data;
        gnc_main_window_close_page (GNC_PLUGIN_PAGE (page));
    }
    g_list_free (kill);
}

/** This function is the handler for all event messages from the
 *  engine.  Its purpose is to update the register page any time
 *  an account or transaction is changed.
 *
 *  @internal
 *
 *  @param entity A pointer to the affected item.
 *
 *  @param event_type The type of the affected item.
 *
 *  @param page A pointer to the register page.
 *
 *  @param ed
 */
static void
gnc_plugin_page_register_event_handler (QofInstance* entity,
                                        QofEventId event_type,
                                        GncPluginPageRegister* page,
                                        GncEventData* ed)
{
    Transaction* trans;
    QofBook* book;
    GncPluginPage* visable_plugin_page;
    GtkWidget* window;

    g_return_if_fail (page); /* Required */
    if (!GNC_IS_TRANS (entity) && !GNC_IS_ACCOUNT (entity))
        return;

    ENTER ("entity %p of type %d, page %p, event data %p",
           entity, event_type, page, ed);

    window = gnc_plugin_page_get_window (GNC_PLUGIN_PAGE (page));

    if (GNC_IS_ACCOUNT (entity))
    {
        if (GNC_IS_MAIN_WINDOW (window))
        {
            GncPluginPageRegisterPrivate *priv = GNC_PLUGIN_PAGE_REGISTER_GET_PRIVATE (page);

            if (!gnc_ledger_display_leader (priv->ledger))
            {
                LEAVE ("account is NULL");
                return;
            }

            gchar *name = gnc_plugin_page_register_get_tab_name (GNC_PLUGIN_PAGE (page));
            main_window_update_page_name (GNC_PLUGIN_PAGE (page), name);

            gchar *long_name = gnc_plugin_page_register_get_long_name (GNC_PLUGIN_PAGE (page));
            main_window_update_page_long_name (GNC_PLUGIN_PAGE (page), long_name);

            gchar *color = gnc_plugin_page_register_get_tab_color (GNC_PLUGIN_PAGE (page));
            main_window_update_page_color (GNC_PLUGIN_PAGE (page), color);
            // update page icon if read only registers
            gnc_plugin_page_register_update_page_icon (GNC_PLUGIN_PAGE (page));

            g_free (color);
            g_free (name);
            g_free (long_name);
        }
        LEAVE ("tab contents updated");
        return;
    }

    if (! (event_type & (QOF_EVENT_MODIFY | QOF_EVENT_DESTROY)))
    {
        LEAVE ("not a modify");
        return;
    }
    trans = GNC_TRANS (entity);
    book = qof_instance_get_book (QOF_INSTANCE (trans));
    if (!gnc_plugin_page_has_book (GNC_PLUGIN_PAGE (page), book))
    {
        LEAVE ("not in this book");
        return;
    }

    if (GNC_IS_MAIN_WINDOW (window))
    {
        visable_plugin_page = gnc_main_window_get_current_page (GNC_MAIN_WINDOW (window));
        if (visable_plugin_page != GNC_PLUGIN_PAGE (page))
        {
            LEAVE ("page not visible");
            return;
        }
    }

    gnc_plugin_page_register_ui_update (NULL, page);
    LEAVE (" ");
    return;
}


/** @} */
/** @} */