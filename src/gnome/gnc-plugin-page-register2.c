/**********************************************************************
 * gnc-plugin-page-register2.c -- register page functions             *
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

#include "config.h"

#include <libguile.h>
#include "guile-mappings.h"
#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include "swig-runtime.h"

#include "gnc-plugin-page-register2.h"
/*################## Added for Reg2 #################*/
#include "gnc-plugin-page-register.h"
/*################## Added for Reg2 #################*/
#include "gnc-plugin-register2.h"
#include "gnc-plugin-menu-additions.h"
#include "gnc-plugin-page-report.h"

#include "gnc-tree-view.h"
#include "gnc-tree-view-split-reg.h"
#include "gnc-tree-model-split-reg.h"
#include "gnc-tree-control-split-reg.h"

#include "dialog-account.h"
#include "dialog-find-transactions2.h"
#include "dialog-print-check2.h"
#include "dialog-transfer.h"
#include "dialog-utils.h"
#include "SX-book.h"
#include "dialog-sx-editor.h"
/*################## Added for Reg2 #################*/
#include "dialog-sx-editor2.h"
/*################## Added for Reg2 #################*/
#include "dialog-sx-from-trans.h"
#include "assistant-stock-split.h"
#include "gnc-component-manager.h"
#include "gnc-date.h"
#include "gnc-date-edit.h"
#include "gnc-engine.h"
#include "gnc-event.h"
#include "gnc-gnome-utils.h"
#include "gnc-gobject-utils.h"
#include "gnc-gui-query.h"
#include "gnc-icons.h"
#include "gnc-prefs.h"
#include "gnc-split-reg2.h"
#include "gnc-ui-util.h"
#include "gnc-window.h"
#include "gnc-main-window.h"
#include "gnc-session.h"
#include "gnome-utils/gnc-warnings.h"
#include "dialog-lot-viewer.h"
#include "Scrub.h"
#include "qof.h"
#include "window-reconcile2.h"
#include "window-autoclear.h"
#include "window-report.h"

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_GUI;

#define DEFAULT_LINES_AMOUNT         50

static void gnc_plugin_page_register2_class_init (GncPluginPageRegister2Class *klass);
static void gnc_plugin_page_register2_init (GncPluginPageRegister2 *plugin_page);
static void gnc_plugin_page_register2_finalize (GObject *object);

static GtkWidget *gnc_plugin_page_register2_create_widget (GncPluginPage *plugin_page);
static void gnc_plugin_page_register2_destroy_widget (GncPluginPage *plugin_page);
static void gnc_plugin_page_register2_window_changed (GncPluginPage *plugin_page, GtkWidget *window);
static void gnc_plugin_page_register2_save_page (GncPluginPage *plugin_page, GKeyFile *file, const gchar *group);
static GncPluginPage *gnc_plugin_page_register2_recreate_page (GtkWidget *window, GKeyFile *file, const gchar *group);
static void gnc_plugin_page_register2_update_edit_menu (GncPluginPage *page, gboolean hide);
static gboolean gnc_plugin_page_register2_finish_pending (GncPluginPage *page);

static gboolean gnc_plugin_page_register2_button_press_cb (GtkWidget *widget, GdkEventButton *event, GncPluginPage *page);

static gchar *gnc_plugin_page_register2_get_tab_name (GncPluginPage *plugin_page);
static gchar *gnc_plugin_page_register2_get_tab_color (GncPluginPage *plugin_page);
static gchar *gnc_plugin_page_register2_get_long_name (GncPluginPage *plugin_page);

static void gnc_plugin_page_register2_summarybar_position_changed (gpointer prefs, gchar* pref, gpointer user_data);

/* Callbacks for the "Filter By" dialog */
void gnc_plugin_page_register2_filter_select_range_cb (GtkRadioButton *button, GncPluginPageRegister2 *page);
void gnc_plugin_page_register2_filter_start_cb (GtkWidget *radio, GncPluginPageRegister2 *page);
void gnc_plugin_page_register2_filter_end_cb (GtkWidget *radio, GncPluginPageRegister2 *page);
void gnc_plugin_page_register2_filter_response_cb (GtkDialog *dialog, gint response, GncPluginPageRegister2 *plugin_page);
void gnc_plugin_page_register2_filter_status_all_cb (GtkButton *button, GncPluginPageRegister2 *plugin_page);
void gnc_plugin_page_register2_filter_status_one_cb (GtkToggleButton *button, GncPluginPageRegister2 *page);
void gnc_plugin_page_register2_filter_save_cb (GtkToggleButton *button, GncPluginPageRegister2 *page);

static time64 gnc_plugin_page_register2_filter_dmy2time (char *date_string);
static gchar *gnc_plugin_page_register2_filter_time2dmy (time64 raw_time);
static gchar *gnc_plugin_page_register2_get_filter (GncPluginPage *plugin_page);
void gnc_plugin_page_register2_set_filter (GncPluginPage *plugin_page, const gchar *filter);

static void gnc_ppr_update_status_query (GncPluginPageRegister2 *page, gboolean refresh_page); 
static void gnc_ppr_update_date_query (GncPluginPageRegister2 *page, gboolean refresh_page); 

/* Command callbacks */
static void gnc_plugin_page_register2_cmd_print_check (GtkAction *action, GncPluginPageRegister2 *plugin_page);
static void gnc_plugin_page_register2_cmd_cut (GtkAction *action, GncPluginPageRegister2 *plugin_page);
static void gnc_plugin_page_register2_cmd_copy (GtkAction *action, GncPluginPageRegister2 *plugin_page);
static void gnc_plugin_page_register2_cmd_paste (GtkAction *action, GncPluginPageRegister2 *plugin_page);
static void gnc_plugin_page_register2_cmd_edit_account (GtkAction *action, GncPluginPageRegister2 *plugin_page);
static void gnc_plugin_page_register2_cmd_find_transactions (GtkAction *action, GncPluginPageRegister2 *plugin_page);
static void gnc_plugin_page_register2_cmd_cut_transaction (GtkAction *action, GncPluginPageRegister2 *plugin_page);
static void gnc_plugin_page_register2_cmd_copy_transaction (GtkAction *action, GncPluginPageRegister2 *plugin_page);
static void gnc_plugin_page_register2_cmd_paste_transaction (GtkAction *action, GncPluginPageRegister2 *plugin_page);
static void gnc_plugin_page_register2_cmd_void_transaction (GtkAction *action, GncPluginPageRegister2 *plugin_page);
static void gnc_plugin_page_register2_cmd_unvoid_transaction (GtkAction *action, GncPluginPageRegister2 *plugin_page);
static void gnc_plugin_page_register2_cmd_reverse_transaction (GtkAction *action, GncPluginPageRegister2 *plugin_page);
static void gnc_plugin_page_register2_cmd_reload (GtkAction *action, GncPluginPageRegister2 *plugin_page);
static void gnc_plugin_page_register2_cmd_view_filter_by (GtkAction *action, GncPluginPageRegister2 *plugin_page);
static void gnc_plugin_page_register2_cmd_style_changed (GtkAction *action, GtkRadioAction *current, GncPluginPageRegister2 *plugin_page);
static void gnc_plugin_page_register2_cmd_style_double_line (GtkToggleAction *action, GncPluginPageRegister2 *plugin_page);
static void gnc_plugin_page_register2_cmd_style_extra_dates (GtkToggleAction *action, GncPluginPageRegister2 *plugin_page);

static void gnc_plugin_page_register2_cmd_reconcile (GtkAction *action, GncPluginPageRegister2 *plugin_page);
static void gnc_plugin_page_register2_cmd_autoclear (GtkAction *action, GncPluginPageRegister2 *plugin_page);
static void gnc_plugin_page_register2_cmd_transfer (GtkAction *action, GncPluginPageRegister2 *plugin_page);
static void gnc_plugin_page_register2_cmd_stock_split (GtkAction *action, GncPluginPageRegister2 *plugin_page);
static void gnc_plugin_page_register2_cmd_lots (GtkAction *action, GncPluginPageRegister2 *plugin_page);
static void gnc_plugin_page_register2_cmd_enter_transaction (GtkAction *action, GncPluginPageRegister2 *plugin_page);
static void gnc_plugin_page_register2_cmd_cancel_transaction (GtkAction *action, GncPluginPageRegister2 *plugin_page);
static void gnc_plugin_page_register2_cmd_delete_transaction (GtkAction *action, GncPluginPageRegister2 *plugin_page);
static void gnc_plugin_page_register2_cmd_blank_transaction (GtkAction *action, GncPluginPageRegister2 *plugin_page);
static void gnc_plugin_page_register2_cmd_duplicate_transaction (GtkAction *action, GncPluginPageRegister2 *plugin_page);
static void gnc_plugin_page_register2_cmd_reinitialize_transaction (GtkAction *action, GncPluginPageRegister2 *plugin_page);
static void gnc_plugin_page_register2_cmd_expand_transaction (GtkToggleAction *action, GncPluginPageRegister2 *plugin_page);
static void gnc_plugin_page_register2_cmd_exchange_rate (GtkAction *action, GncPluginPageRegister2 *plugin_page);
static void gnc_plugin_page_register2_cmd_jump (GtkAction *action, GncPluginPageRegister2 *plugin_page);
static void gnc_plugin_page_register2_cmd_schedule (GtkAction *action, GncPluginPageRegister2 *plugin_page);
static void gnc_plugin_page_register2_cmd_scrub_all (GtkAction *action, GncPluginPageRegister2 *plugin_page);
static void gnc_plugin_page_register2_cmd_scrub_current (GtkAction *action, GncPluginPageRegister2 *plugin_page);
static void gnc_plugin_page_register2_cmd_account_report (GtkAction *action, GncPluginPageRegister2 *plugin_page);
static void gnc_plugin_page_register2_cmd_transaction_report (GtkAction *action, GncPluginPageRegister2 *plugin_page);
static void gnc_plugin_page_register2_cmd_entryUp (GtkAction *action, GncPluginPageRegister2 *plugin_page);
static void gnc_plugin_page_register2_cmd_entryDown (GtkAction *action, GncPluginPageRegister2 *plugin_page);

static void gnc_plugin_page_help_changed_cb (GNCSplitReg2 *gsr, GncPluginPageRegister2 *register_page );
static void gnc_plugin_page_register2_refresh_cb (GHashTable *changes, gpointer user_data);
static void gnc_plugin_page_register2_close_cb (gpointer user_data);

static void gnc_plugin_page_register2_ui_update (gpointer various, GncPluginPageRegister2 *page);
static void gppr_account_destroy_cb (Account *account);
static void gnc_plugin_page_register2_event_handler (QofInstance *entity,
        QofEventId event_type,
        GncPluginPageRegister2 *page,
        GncEventData *ed);

/************************************************************/
/*                          Actions                         */
/************************************************************/

#define CUT_TRANSACTION_LABEL         N_("Cu_t Transaction")
#define COPY_TRANSACTION_LABEL        N_("_Copy Transaction")
#define PASTE_TRANSACTION_LABEL       N_("_Paste Transaction")
#define DUPLICATE_TRANSACTION_LABEL   N_("Dup_licate Transaction")
#define DELETE_TRANSACTION_LABEL      N_("_Delete Transaction")
#define CUT_SPLIT_LABEL               N_("Cu_t Split")
#define COPY_SPLIT_LABEL              N_("_Copy Split")
#define PASTE_SPLIT_LABEL             N_("_Paste Split")
#define DUPLICATE_SPLIT_LABEL         N_("Dup_licate Split")
#define DELETE_SPLIT_LABEL            N_("_Delete Split")
#define CUT_TRANSACTION_TIP           N_("Cut the selected transaction into clipboard")
#define COPY_TRANSACTION_TIP          N_("Copy the selected transaction into clipboard")
#define PASTE_TRANSACTION_TIP         N_("Paste the transaction from the clipboard")
#define DUPLICATE_TRANSACTION_TIP     N_("Make a copy of the current transaction")
#define DELETE_TRANSACTION_TIP        N_("Delete the current transaction")
#define CUT_SPLIT_TIP                 N_("Cut the selected split into clipboard")
#define COPY_SPLIT_TIP                N_("Copy the selected split into clipboard")
#define PASTE_SPLIT_TIP               N_("Paste the split from the clipboard")
#define DUPLICATE_SPLIT_TIP           N_("Make a copy of the current split")
#define DELETE_SPLIT_TIP              N_("Delete the current split")

#define TRANSACTION_UP_ACTION "TransactionUpAction"
#define TRANSACTION_DOWN_ACTION "TransactionDownAction"

static GtkActionEntry gnc_plugin_page_register2_actions [] =
{
    /* File menu */

    {
        "FilePrintAction", GTK_STOCK_PRINT, N_("_Print Checks..."), "<control>p", NULL,
        G_CALLBACK (gnc_plugin_page_register2_cmd_print_check)
    },

    /* Edit menu */

    {
        "EditCutAction", GTK_STOCK_CUT, N_("Cu_t"), NULL,
        N_("Cut the current selection and copy it to clipboard"),
        G_CALLBACK (gnc_plugin_page_register2_cmd_cut)
    },
    {
        "EditCopyAction", GTK_STOCK_COPY, N_("_Copy"), NULL,
        N_("Copy the current selection to clipboard"),
        G_CALLBACK (gnc_plugin_page_register2_cmd_copy)
    },
    {
        "EditPasteAction", GTK_STOCK_PASTE, N_("_Paste"), NULL,
        N_("Paste the clipboard content at the cursor position"),
        G_CALLBACK (gnc_plugin_page_register2_cmd_paste)
    },
    {
        "EditEditAccountAction", GNC_STOCK_EDIT_ACCOUNT, N_("Edit _Account"), "<control>e",
        N_("Edit the selected account"),
        G_CALLBACK (gnc_plugin_page_register2_cmd_edit_account)
    },
    {
        "EditFindTransactionsAction", GTK_STOCK_FIND, N_("_Find..."), "<control>f",
        N_("Find transactions with a search"),
        G_CALLBACK (gnc_plugin_page_register2_cmd_find_transactions)
    },

    /* Transaction menu */

    {
        "CutTransactionAction", GTK_STOCK_CUT, CUT_TRANSACTION_LABEL, "",
        CUT_TRANSACTION_TIP,
        G_CALLBACK (gnc_plugin_page_register2_cmd_cut_transaction)
    },
    {
        "CopyTransactionAction", GTK_STOCK_COPY, COPY_TRANSACTION_LABEL, "",
        COPY_TRANSACTION_TIP,
        G_CALLBACK (gnc_plugin_page_register2_cmd_copy_transaction)
    },
    {
        "PasteTransactionAction", GTK_STOCK_PASTE, PASTE_TRANSACTION_LABEL, "",
        PASTE_TRANSACTION_TIP,
        G_CALLBACK (gnc_plugin_page_register2_cmd_paste_transaction)
    },
    {
        "DuplicateTransactionAction", GTK_STOCK_COPY, DUPLICATE_TRANSACTION_LABEL, "",
        DUPLICATE_TRANSACTION_TIP,
        G_CALLBACK (gnc_plugin_page_register2_cmd_duplicate_transaction)
    },
    {
        "DeleteTransactionAction", GTK_STOCK_DELETE, DELETE_TRANSACTION_LABEL, NULL,
        DELETE_TRANSACTION_TIP,
        G_CALLBACK (gnc_plugin_page_register2_cmd_delete_transaction)
    },
    {
        "RemoveTransactionSplitsAction", GTK_STOCK_CLEAR, N_("Remo_ve All Splits"), NULL,
        N_("Remove all splits in the current transaction"),
        G_CALLBACK (gnc_plugin_page_register2_cmd_reinitialize_transaction)
    },
    {
        "RecordTransactionAction", GTK_STOCK_ADD, N_("_Enter Transaction"), NULL,
        N_("Record the current transaction"),
        G_CALLBACK (gnc_plugin_page_register2_cmd_enter_transaction)
    },
    {
        "CancelTransactionAction", GTK_STOCK_CANCEL, N_("Ca_ncel Transaction"), NULL,
        N_("Cancel the current transaction"),
        G_CALLBACK (gnc_plugin_page_register2_cmd_cancel_transaction)
    },
    {
        "VoidTransactionAction", NULL, N_("_Void Transaction"), NULL, NULL,
        G_CALLBACK (gnc_plugin_page_register2_cmd_void_transaction)
    },
    {
        "UnvoidTransactionAction", NULL, N_("_Unvoid Transaction"), NULL, NULL,
        G_CALLBACK (gnc_plugin_page_register2_cmd_unvoid_transaction)
    },
    {
        "ReverseTransactionAction", NULL, N_("Add _Reversing Transaction"), NULL, NULL,
        G_CALLBACK (gnc_plugin_page_register2_cmd_reverse_transaction)
    },
    {
        TRANSACTION_UP_ACTION, GTK_STOCK_GO_UP, N_("Move Transaction _Up"), NULL,
        N_("Move the current transaction one row upwards. Only available if the date and number of both rows are identical and the register window is sorted by date."),
        G_CALLBACK (gnc_plugin_page_register2_cmd_entryUp)
    },
    {
        TRANSACTION_DOWN_ACTION, GTK_STOCK_GO_DOWN, N_("Move Transaction Do_wn"), NULL,
        N_("Move the current transaction one row downwards. Only available if the date and number of both rows are identical and the register window is sorted by date."),
        G_CALLBACK (gnc_plugin_page_register2_cmd_entryDown)
    },

    /* View menu */

    {
        "ViewFilterByAction", NULL, N_("_Filter By..."), NULL, NULL,
        G_CALLBACK (gnc_plugin_page_register2_cmd_view_filter_by)
    },
    {
        "ViewRefreshAction", GTK_STOCK_REFRESH, N_("_Refresh"), "<control>r",
        N_("Refresh this window"),
        G_CALLBACK (gnc_plugin_page_register2_cmd_reload)
    },

    /* Actions menu */

    {
        "ActionsTransferAction", GNC_STOCK_TRANSFER, N_("_Transfer..."), "<control>t",
        N_("Transfer funds from one account to another"),
        G_CALLBACK (gnc_plugin_page_register2_cmd_transfer)
    },
    {
        "ActionsReconcileAction", GTK_STOCK_INDEX, N_("_Reconcile..."), NULL,
        N_("Reconcile the selected account"),
        G_CALLBACK (gnc_plugin_page_register2_cmd_reconcile)
    },
    {
        "ActionsAutoClearAction", GTK_STOCK_INDEX, N_("_Auto-clear..."), NULL,
        N_("Automatically clear individual transactions, so as to reach a certain cleared amount"),
        G_CALLBACK (gnc_plugin_page_register2_cmd_autoclear)
    },
    {
        "ActionsStockSplitAction", NULL, N_("Stoc_k Split..."), NULL,
        N_("Record a stock split or a stock merger"),
        G_CALLBACK (gnc_plugin_page_register2_cmd_stock_split)
    },
    {
        "ActionsLotsAction", NULL, N_("View _Lots..."), NULL,
        N_("Bring up the lot viewer/editor window"),
        G_CALLBACK (gnc_plugin_page_register2_cmd_lots)
    },
    {
        "BlankTransactionAction", GTK_STOCK_GOTO_BOTTOM, N_("_Blank Transaction"), "<control>Page_Down",
        N_("Move to the blank transaction at the bottom of the register"),
        G_CALLBACK (gnc_plugin_page_register2_cmd_blank_transaction)
    },
    {
        "EditExchangeRateAction", NULL, N_("Edit E_xchange Rate"), NULL,
        N_("Edit the exchange rate for the current transaction"),
        G_CALLBACK (gnc_plugin_page_register2_cmd_exchange_rate)
    },
    {
        "JumpTransactionAction", GNC_STOCK_JUMP_TO, N_("_Jump"), NULL,
        N_("Jump to the corresponding transaction in the other account"),
        G_CALLBACK (gnc_plugin_page_register2_cmd_jump)
    },
    {
        "ScheduleTransactionAction", GNC_STOCK_SCHEDULE, N_("Sche_dule..."), NULL,
        N_("Create a Scheduled Transaction with the current transaction as a template"),
        G_CALLBACK (gnc_plugin_page_register2_cmd_schedule)
    },
    {
        "ScrubAllAction", NULL, N_("_All transactions"), NULL, NULL,
        G_CALLBACK (gnc_plugin_page_register2_cmd_scrub_all)
    },
    {
        "ScrubCurrentAction", NULL, N_("_This transaction"), NULL, NULL,
        G_CALLBACK (gnc_plugin_page_register2_cmd_scrub_current)
    },

    /* Reports menu */

    {
        "ReportsAccountReportAction", NULL, N_("Account Report"), NULL,
        N_("Open a register report for this Account"),
        G_CALLBACK (gnc_plugin_page_register2_cmd_account_report)
    },
    {
        "ReportsAcctTransReportAction", NULL, N_("Account Report - Single Transaction"), NULL,
        N_("Open a register report for the selected Transaction"),
        G_CALLBACK (gnc_plugin_page_register2_cmd_transaction_report)
    },
};

static guint gnc_plugin_page_register2_n_actions = G_N_ELEMENTS (gnc_plugin_page_register2_actions);

static GtkToggleActionEntry toggle_entries[] =
{
    {
        "ViewStyleDoubleLineAction", NULL, N_("_Double Line"), NULL,
        N_("Show two lines of information for each transaction"),
        G_CALLBACK (gnc_plugin_page_register2_cmd_style_double_line), FALSE
    },

    {
        "ViewStyleExtraDatesAction", NULL, N_("Show _Extra Dates"), NULL,
        N_("Show entered and reconciled dates"),
        G_CALLBACK (gnc_plugin_page_register2_cmd_style_extra_dates), FALSE
    },

    {
        "SplitTransactionAction", GNC_STOCK_SPLIT_TRANS, N_("S_plit Transaction"), NULL,
        N_("Show all splits in the current transaction"),
        G_CALLBACK (gnc_plugin_page_register2_cmd_expand_transaction), FALSE
    },
};

static guint n_toggle_entries = G_N_ELEMENTS (toggle_entries);

static GtkRadioActionEntry radio_entries_2 [] =
{
    /* Translators: This is a menu item in the View menu */
    {
        "ViewStyleBasicAction", NULL, N_("_Basic Ledger"), NULL,
        N_("Show transactions on one or two lines"), REG2_STYLE_LEDGER
    },
    /* Translators: This is a menu item in the View menu */
    {
        "ViewStyleAutoSplitAction", NULL, N_("_Auto-Split Ledger"), NULL,
        N_("Show transactions on one or two lines and expand the current transaction"), REG2_STYLE_AUTO_LEDGER
    },
    /* Translators: This is a menu item in the View menu */
    {
        "ViewStyleJournalAction", NULL, N_("Transaction _Journal"), NULL,
        N_("Show expanded transactions with all splits"), REG2_STYLE_JOURNAL
    }
};

static guint n_radio_entries_2 = G_N_ELEMENTS (radio_entries_2);

/** These are the "important" actions provided by the register page.
 *  Their labels will appear when the toolbar is set to "Icons and
 *  important text" (e.g. GTK_TOOLBAR_BOTH_HORIZ) mode. */
static const gchar *important_actions[] =
{
    "SplitTransactionAction",
    NULL,
};

/** Actions that require an account to be selected before they are
 *  enabled. */
static const gchar *actions_requiring_account[] =
{
    "EditEditAccountAction",
    "ActionsReconcileAction",
    "ActionsAutoClearAction",
    "ActionsLotsAction",
    NULL
};

/** View Style actions */
static const gchar *view_style_actions[] =
{
    "ViewStyleBasicAction",
    "ViewStyleAutoSplitAction",
    "ViewStyleJournalAction",
    NULL
};

/** Short labels for use on the toolbar buttons. */
static action_toolbar_labels toolbar_labels[] =
{
    { "ActionsTransferAction", 	  N_("Transfer") },
    { "RecordTransactionAction", 	  N_("Enter") },
    { "CancelTransactionAction", 	  N_("Cancel") },
    { "DeleteTransactionAction", 	  N_("Delete") },
    { "DuplicateTransactionAction", N_("Duplicate") },
    { "SplitTransactionAction",     N_("Split") },
    { "ScheduleTransactionAction",  N_("Schedule") },
    { "BlankTransactionAction",     N_("Blank") },
    { "ActionsReconcileAction",     N_("Reconcile") },
    { "ActionsAutoClearAction",     N_("Auto-clear") },
    { TRANSACTION_UP_ACTION, N_("Up") },
    { TRANSACTION_DOWN_ACTION, N_("Down") },
    { NULL, NULL },
};

struct status_action
{
    const char *action_name;
    int value;
    GtkWidget *widget;
};

static struct status_action status_actions[] =
{
    { "filter_status_reconciled",   CLEARED_RECONCILED, NULL },
    { "filter_status_cleared",      CLEARED_CLEARED, NULL },
    { "filter_status_voided",       CLEARED_VOIDED, NULL },
    { "filter_status_frozen",       CLEARED_FROZEN, NULL },
    { "filter_status_unreconciled", CLEARED_NO, NULL },
    { NULL, 0, NULL },
};

#define CLEARED_VALUE "cleared_value"
#define DEFAULT_FILTER "0x001f"

/************************************************************/
/*                      Data Structures                     */
/************************************************************/

typedef struct GncPluginPageRegister2Private
{
    GtkWidget *widget;

    GNCLedgerDisplay2 *ledger;
    GNCSplitReg2 *gsr;

    gint event_handler_id;
    gint component_manager_id;
    GncGUID key;  /* The guid of the Account we're watching */

    gint lines_default;
    gboolean read_only;

    struct
    {
        GtkWidget *dialog;
        GtkWidget *table;
        GtkWidget *start_date_choose;
        GtkWidget *start_date_today;
        GtkWidget *start_date;
        GtkWidget *end_date_choose;
        GtkWidget *end_date_today;
        GtkWidget *end_date;
        cleared_match_t original_cleared_match;
        cleared_match_t cleared_match;
        time64 original_start_time;
        time64 original_end_time;
        time64 start_time;
        time64 end_time;
        gboolean original_save_filter;
        gboolean save_filter;
    } fd;
} GncPluginPageRegister2Private;

#define GNC_PLUGIN_PAGE_REGISTER2_GET_PRIVATE(o)  \
   (G_TYPE_INSTANCE_GET_PRIVATE ((o), GNC_TYPE_PLUGIN_PAGE_REGISTER2, GncPluginPageRegister2Private))

static GObjectClass *parent_class = NULL;

/************************************************************/
/*                      Implementation                      */
/************************************************************/

GType
gnc_plugin_page_register2_get_type (void)
{
    static GType gnc_plugin_page_register2_type = 0;

    if (gnc_plugin_page_register2_type == 0)
    {
        static const GTypeInfo our_info =
        {
            sizeof (GncPluginPageRegister2Class),
            NULL,
            NULL,
            (GClassInitFunc) gnc_plugin_page_register2_class_init,
            NULL,
            NULL,
            sizeof (GncPluginPageRegister2),
            0,
            (GInstanceInitFunc) gnc_plugin_page_register2_init
        };

        gnc_plugin_page_register2_type = g_type_register_static (GNC_TYPE_PLUGIN_PAGE,
                                        GNC_PLUGIN_PAGE_REGISTER2_NAME,
                                        &our_info, 0);
    }

    return gnc_plugin_page_register2_type;
}

/*#################################################################################*/
/*#################################################################################*/
static GncPluginPage *
gnc_plugin_page_register2_new_common (GNCLedgerDisplay2 *ledger)
{
    GncPluginPageRegister2 *register_page;
    GncPluginPageRegister2Private *priv;
    GncPluginPage *plugin_page;

    GncTreeModelSplitReg *model; 
    GtkTreeView *tv; 

    GNCSplitReg2 *gsr;

    const GList *item;
    GList *book_list;
    gchar *label;
    gchar *label_color;
    QofQuery *q;
    Account *account;

    /* Is there an existing page? */
    gsr = gnc_ledger_display2_get_user_data (ledger);
    if (gsr)
    {
        item = gnc_gobject_tracking_get_list (GNC_PLUGIN_PAGE_REGISTER2_NAME);
        for ( ; item; item = g_list_next (item))
        {
            register_page = (GncPluginPageRegister2 *)item->data;
            priv = GNC_PLUGIN_PAGE_REGISTER2_GET_PRIVATE (register_page);
            if (priv->gsr == gsr)
                return GNC_PLUGIN_PAGE (register_page);
        }
    }

    register_page = g_object_new (GNC_TYPE_PLUGIN_PAGE_REGISTER2, NULL);
    priv = GNC_PLUGIN_PAGE_REGISTER2_GET_PRIVATE (register_page);
    priv->ledger = ledger;
    priv->key = *guid_null();

    plugin_page = GNC_PLUGIN_PAGE (register_page);
    label = gnc_plugin_page_register2_get_tab_name (plugin_page);
    gnc_plugin_page_set_page_name (plugin_page, label);
    g_free (label);

    label_color = gnc_plugin_page_register2_get_tab_color (plugin_page);
    gnc_plugin_page_set_page_color (plugin_page, label_color);
    g_free (label_color);

    label = gnc_plugin_page_register2_get_long_name (plugin_page);
    gnc_plugin_page_set_page_long_name (plugin_page, label);
    g_free (label);

    q = gnc_ledger_display2_get_query (ledger);
    book_list = qof_query_get_books (q);
    for (item = book_list; item; item = g_list_next (item))
        gnc_plugin_page_add_book (plugin_page, (QofBook *)item->data);
    // Do not free the list. It is owned by the query.

    priv->component_manager_id = 0;
    return plugin_page;
}

/*#################################################################################*/

GncPluginPage *
gnc_plugin_page_register2_new (Account *account, gboolean subaccounts)
{
    GNCLedgerDisplay2 *ledger;
    GncPluginPage *page;
    GncPluginPageRegister2Private *priv;

/*################## Added for Reg2 #################*/
    const GList *item;
    GncPluginPageRegister  *old_register_page;
/*################## Added for Reg2 #################*/

    ENTER("account=%p, subaccounts=%s", account,
          subaccounts ? "TRUE" : "FALSE");

/*################## Added for Reg2 #################*/
    // We test for the old register being open here, ie matching account guids
    item = gnc_gobject_tracking_get_list (GNC_PLUGIN_PAGE_REGISTER_NAME);
    for ( ; item; item = g_list_next (item))
    {
        Account *old_account;
        old_register_page = (GncPluginPageRegister *)item->data;
        old_account = gnc_plugin_page_register_get_account (old_register_page);

        if (guid_equal (xaccAccountGetGUID (account), xaccAccountGetGUID (old_account)))
        {
            gnc_error_dialog (NULL, "%s",
                         _("You have tried to open an account in the new register while it is open in the old register."));
            return NULL;
        }
    }
/*################## Added for Reg2 #################*/

    if (subaccounts)
        ledger = gnc_ledger_display2_subaccounts (account);
    else
        ledger = gnc_ledger_display2_simple (account);

    page = gnc_plugin_page_register2_new_common (ledger);
    priv = GNC_PLUGIN_PAGE_REGISTER2_GET_PRIVATE (page);
    priv->key = *xaccAccountGetGUID (account);

    LEAVE("%p", page);
    return page;
}

GncPluginPage *
gnc_plugin_page_register2_new_gl (void)
{
    GNCLedgerDisplay2 *ledger;

    ledger = gnc_ledger_display2_gl ();
    return gnc_plugin_page_register2_new_common (ledger);
}

GncPluginPage *
gnc_plugin_page_register2_new_ledger (GNCLedgerDisplay2 *ledger)
{
    return gnc_plugin_page_register2_new_common (ledger);
}

static void
gnc_plugin_page_register2_class_init (GncPluginPageRegister2Class *klass)
{
    GObjectClass *object_class = G_OBJECT_CLASS (klass);
    GncPluginPageClass *gnc_plugin_class = GNC_PLUGIN_PAGE_CLASS(klass);

    parent_class = g_type_class_peek_parent (klass);

    object_class->finalize = gnc_plugin_page_register2_finalize;

    gnc_plugin_class->tab_icon        = GNC_STOCK_ACCOUNT;
    gnc_plugin_class->plugin_name     = GNC_PLUGIN_PAGE_REGISTER2_NAME;
    gnc_plugin_class->create_widget   = gnc_plugin_page_register2_create_widget;
    gnc_plugin_class->destroy_widget  = gnc_plugin_page_register2_destroy_widget;
    gnc_plugin_class->window_changed  = gnc_plugin_page_register2_window_changed;
    gnc_plugin_class->save_page       = gnc_plugin_page_register2_save_page;
    gnc_plugin_class->recreate_page   = gnc_plugin_page_register2_recreate_page;
    gnc_plugin_class->update_edit_menu_actions = gnc_plugin_page_register2_update_edit_menu;
    gnc_plugin_class->finish_pending  = gnc_plugin_page_register2_finish_pending;

    g_type_class_add_private(klass, sizeof(GncPluginPageRegister2Private));

    gnc_ui_register_account_destroy_callback (gppr_account_destroy_cb);
}

static void
gnc_plugin_page_register2_init (GncPluginPageRegister2 *plugin_page)
{
    GncPluginPageRegister2Private *priv;
    GncPluginPage *parent;
    GtkActionGroup *action_group;
    gboolean use_new;

    priv = GNC_PLUGIN_PAGE_REGISTER2_GET_PRIVATE(plugin_page);

    /* Init parent declared variables */
    parent = GNC_PLUGIN_PAGE(plugin_page);
    use_new = gnc_prefs_get_bool(GNC_PREFS_GROUP_GENERAL_REGISTER, GNC_PREF_USE_NEW);
    g_object_set(G_OBJECT(plugin_page),
                 "page-name",      _("General Journal2"),
                 "page-uri",       "default:",
                 "ui-description", "gnc-plugin-page-register2-ui.xml",
                 "use-new-window", use_new,
                 NULL);

    /* Create menu and toolbar information */
    action_group =
        gnc_plugin_page_create_action_group(parent,
                                            "GncPluginPageRegister2Actions");
    gtk_action_group_add_actions (action_group, gnc_plugin_page_register2_actions,
                                  gnc_plugin_page_register2_n_actions, plugin_page);
    gtk_action_group_add_toggle_actions (action_group,
                                         toggle_entries, n_toggle_entries,
                                         plugin_page);
    gtk_action_group_add_radio_actions (action_group,
                                        radio_entries_2, n_radio_entries_2,
                                        REG2_STYLE_LEDGER,
                                        G_CALLBACK(gnc_plugin_page_register2_cmd_style_changed),
                                        plugin_page);

    gnc_plugin_init_short_names (action_group, toolbar_labels);
    gnc_plugin_set_important_actions (action_group, important_actions);

    priv->lines_default     = DEFAULT_LINES_AMOUNT;
    priv->read_only         = FALSE;
    priv->fd.cleared_match  = CLEARED_ALL;
}

static void
gnc_plugin_page_register2_finalize (GObject *object)
{
    GncPluginPageRegister2 *page;
    GncPluginPageRegister2Private *priv;

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER2 (object));

    ENTER("object %p", object);
    page = GNC_PLUGIN_PAGE_REGISTER2 (object);
    priv = GNC_PLUGIN_PAGE_REGISTER2_GET_PRIVATE (page);

    G_OBJECT_CLASS (parent_class)->finalize (object);
    LEAVE(" ");
}

Account *
gnc_plugin_page_register2_get_account (GncPluginPageRegister2 *page)
{
    GncPluginPageRegister2Private *priv;
    GNCLedgerDisplay2Type ledger_type;
    Account *leader;

    priv = GNC_PLUGIN_PAGE_REGISTER2_GET_PRIVATE(page);
    ledger_type = gnc_ledger_display2_type (priv->ledger);
    leader = gnc_ledger_display2_leader (priv->ledger);

    if ((ledger_type == LD2_SINGLE) || (ledger_type == LD2_SUBACCOUNT))
        return leader;
    return NULL;
}

/* This is the list of actions which are switched inactive in a read-only book. */
static const char* readonly_inactive_actions[] =
{
    "EditCutAction",
    "EditPasteAction",
    "CutTransactionAction",
    "PasteTransactionAction",
    TRANSACTION_UP_ACTION,
    TRANSACTION_DOWN_ACTION,
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

static void
gnc_plugin_page_register2_ui_update (gpointer various, GncPluginPageRegister2 *page) // this works
{
    GncPluginPageRegister2Private *priv;
    GncTreeViewSplitReg *view;
    GncTreeModelSplitReg *model;
    GtkAction *action;
    gboolean expanded, voided;
    Transaction *trans;

    /* Set 'Split Transaction' */
    priv = GNC_PLUGIN_PAGE_REGISTER2_GET_PRIVATE (page);
    g_return_if_fail(priv);
    model = gnc_ledger_display2_get_split_model_register (priv->ledger);
    view = gnc_ledger_display2_get_split_view_register (priv->ledger);
    g_return_if_fail(model);
    g_return_if_fail(view);

    expanded = gnc_tree_view_split_reg_trans_expanded (view, NULL);
    action = gnc_plugin_page_get_action (GNC_PLUGIN_PAGE (page),
                                         "SplitTransactionAction");
    gtk_action_set_sensitive (action, model->style == REG2_STYLE_LEDGER);
    g_signal_handlers_block_by_func
    (action, gnc_plugin_page_register2_cmd_expand_transaction, page);
    gtk_toggle_action_set_active (GTK_TOGGLE_ACTION (action), expanded);
    g_signal_handlers_unblock_by_func
    (action, gnc_plugin_page_register2_cmd_expand_transaction, page);

    /* Set 'Void' and 'Unvoid' */
    trans = gnc_tree_view_split_reg_get_current_trans (view);
    voided = xaccTransHasSplitsInState (trans, VREC); 

    action = gnc_plugin_page_get_action (GNC_PLUGIN_PAGE (page),
                                         "VoidTransactionAction");
    gtk_action_set_sensitive (GTK_ACTION (action), !voided);

    action = gnc_plugin_page_get_action (GNC_PLUGIN_PAGE (page),
                                         "UnvoidTransactionAction");
    gtk_action_set_sensitive (GTK_ACTION (action), voided);

    /* Modify the activeness of the up/down arrows */
    {
        GtkAction *action = gnc_plugin_page_get_action (GNC_PLUGIN_PAGE (page), TRANSACTION_UP_ACTION);
        gtk_action_set_sensitive(action,
                                 gnc_tree_control_split_reg_is_current_movable_updown(view, TRUE));
        action = gnc_plugin_page_get_action (GNC_PLUGIN_PAGE (page), TRANSACTION_DOWN_ACTION);
        gtk_action_set_sensitive(action,
                                 gnc_tree_control_split_reg_is_current_movable_updown(view, FALSE));
    }

    /* If we are in a readonly book, make any modifying action inactive */
    if (qof_book_is_readonly(gnc_get_current_book ()))
    {
        const char **iter;
        for (iter = readonly_inactive_actions; *iter; ++iter)
        {
            /* Set the action's sensitivity */
            GtkAction *action = gnc_plugin_page_get_action (GNC_PLUGIN_PAGE (page), *iter);
            gtk_action_set_sensitive (action, FALSE);
        }
    }

    /* Modifying action descriptions based on row depth */
    {
        RowDepth depth;
        const char **iter, **label_iter, **tooltip_iter;
        gboolean curr_label_trans = FALSE;
        depth = gnc_tree_view_reg_get_selected_row_depth (view);
        iter = tran_vs_split_actions;
        action = gnc_plugin_page_get_action (GNC_PLUGIN_PAGE (page), *iter);
        label_iter = tran_action_labels;
        if (g_strcmp0 (gtk_action_get_label (action), _(*label_iter)) == 0)
            curr_label_trans = TRUE;
        if ((depth == SPLIT3) && curr_label_trans)
        {
            label_iter = split_action_labels;
            tooltip_iter = split_action_tips;
            for (iter = tran_vs_split_actions; *iter; ++iter)
            {
                /* Adjust the action's label and tooltip */
                action = gnc_plugin_page_get_action (GNC_PLUGIN_PAGE (page), *iter);
                gtk_action_set_label (action, _(*label_iter));
                gtk_action_set_tooltip (action, _(*tooltip_iter));
                ++label_iter;
                ++tooltip_iter;
            }
        }
        else if ((depth == TRANS1 || depth == TRANS2) && !curr_label_trans)
        {
            label_iter = tran_action_labels;
            tooltip_iter = tran_action_tips;
            for (iter = tran_vs_split_actions; *iter; ++iter)
            {
                /* Adjust the action's label and tooltip */
                action = gnc_plugin_page_get_action (GNC_PLUGIN_PAGE (page), *iter);
                gtk_action_set_label (action, _(*label_iter));
                gtk_action_set_tooltip (action, _(*tooltip_iter));
                ++label_iter;
                ++tooltip_iter;
            }
        }
    }
}

static void
gnc_plugin_page_register2_ui_initial_state (GncPluginPageRegister2 *page) // this works
{
    GncPluginPageRegister2Private *priv ;
    GtkActionGroup *action_group;
    GtkAction *action;
    Account *account;
    GncTreeViewSplitReg *view;
    GncTreeModelSplitReg *model;
    GNCLedgerDisplay2Type ledger_type;
    int i;
    gboolean is_readwrite = !qof_book_is_readonly (gnc_get_current_book());

    priv = GNC_PLUGIN_PAGE_REGISTER2_GET_PRIVATE (page);
    account = gnc_plugin_page_register2_get_account (page);
    action_group = gnc_plugin_page_get_action_group (GNC_PLUGIN_PAGE (page));
    gnc_plugin_update_actions(action_group, actions_requiring_account,
                              "sensitive", is_readwrite && account != NULL);

    /* Set "style" radio button */
    ledger_type = gnc_ledger_display2_type (priv->ledger);
    gnc_plugin_update_actions (action_group, view_style_actions,
                              "sensitive", ledger_type == LD2_SINGLE);

    model = gnc_ledger_display2_get_split_model_register (priv->ledger);
    for (i = n_radio_entries_2 - 1; i > 0; i--)
    {
        DEBUG(" index %d: comparing %x to %x", i, radio_entries_2[i].value,
              model->style);
        if (radio_entries_2[i].value == model->style)
        {
            DEBUG("match");
            break;
        }
    }

    /* Either a match was found, or fell out with i = 0 */
    action = gtk_action_group_get_action (action_group, radio_entries_2[i].name);
    g_signal_handlers_block_by_func (action, gnc_plugin_page_register2_cmd_style_changed, page);
    gtk_toggle_action_set_active (GTK_TOGGLE_ACTION (action), TRUE);
    g_signal_handlers_unblock_by_func (action, gnc_plugin_page_register2_cmd_style_changed, page);

    view = gnc_split_reg2_get_register (priv->gsr);

    /* Set "double line" toggle button */
    action = gtk_action_group_get_action (action_group, "ViewStyleDoubleLineAction");
    g_signal_handlers_block_by_func (action, gnc_plugin_page_register2_cmd_style_double_line, page);
    gtk_toggle_action_set_active (GTK_TOGGLE_ACTION (action), model->use_double_line);
    g_signal_handlers_unblock_by_func (action, gnc_plugin_page_register2_cmd_style_double_line, page);

    /* Set "extra dates" toggle button */
    action = gtk_action_group_get_action (action_group, "ViewStyleExtraDatesAction");
    g_signal_handlers_block_by_func (action, gnc_plugin_page_register2_cmd_style_extra_dates, page);
    gtk_toggle_action_set_active (GTK_TOGGLE_ACTION (action), view->show_extra_dates);
    g_signal_handlers_unblock_by_func (action, gnc_plugin_page_register2_cmd_style_extra_dates, page);
}


/*#################################################################################*/
/*#################################################################################*/
/* Virtual Functions */

static GtkWidget *
gnc_plugin_page_register2_create_widget (GncPluginPage *plugin_page)
{
    GncPluginPageRegister2 *page;
    GncPluginPageRegister2Private *priv;
    GNCLedgerDisplay2Type ledger_type;
    GncWindow *gnc_window;
    guint numRows;
    GtkWidget *gsr;

    GncTreeViewSplitReg *view;
    GncTreeModelSplitReg *model;
    Account *acct;
    gchar **filter;
    gchar *order;
    int filter_changed = 0;

    ENTER("page %p", plugin_page);
    page = GNC_PLUGIN_PAGE_REGISTER2 (plugin_page);
    priv = GNC_PLUGIN_PAGE_REGISTER2_GET_PRIVATE (page);
    if (priv->widget != NULL)
    {
        LEAVE("existing widget %p", priv->widget);
        return priv->widget;
    }

    priv->widget = gtk_vbox_new (FALSE, 0);
    gtk_widget_show (priv->widget);

    numRows = priv->lines_default;
    numRows = MIN (numRows, DEFAULT_LINES_AMOUNT);

    gnc_window = GNC_WINDOW (GNC_PLUGIN_PAGE (page)->window);

    gsr = gnc_split_reg2_new (priv->ledger,
                            gnc_window_get_gtk_window (gnc_window),
                            numRows, priv->read_only);
    priv->gsr = (GNCSplitReg2 *)gsr;
    gtk_widget_show (gsr);

    gtk_box_pack_start (GTK_BOX (priv->widget), gsr, TRUE, TRUE, 0);

    g_signal_connect (G_OBJECT (gsr), "help-changed",
                      G_CALLBACK (gnc_plugin_page_help_changed_cb),
                      page );

    view = gnc_split_reg2_get_register (priv->gsr);

    // Callback for right mouse events
    g_signal_connect (G_OBJECT (GTK_TREE_VIEW (view)), "button-press-event",
                      G_CALLBACK (gnc_plugin_page_register2_button_press_cb), page);

    model = gnc_ledger_display2_get_split_model_register (priv->ledger);

    gnc_tree_model_split_reg_config (model, model->type, model->style, model->use_double_line);

    gnc_plugin_page_register2_ui_initial_state (page);
    gnc_plugin_page_register2_ui_update (NULL, page);

    ledger_type = gnc_ledger_display2_type (priv->ledger);

    if (ledger_type == LD2_SINGLE || ledger_type == LD2_SUBACCOUNT || ledger_type == LD2_GL)
    {
        /* Set the filter for the split register and status of save filter button */
        priv->fd.save_filter = FALSE;

        filter = g_strsplit(gnc_plugin_page_register2_get_filter (plugin_page), ",", -1);

        PINFO("Loaded Filter Status is %s", filter[0]);

        priv->fd.cleared_match = (gint)g_ascii_strtoll (filter[0], NULL, 16 );

        if (filter[0] && (g_strcmp0 (filter[0], DEFAULT_FILTER) != 0))
            filter_changed = filter_changed + 1;

        if (filter[1] && (g_strcmp0 (filter[1], "0") != 0 ))
        {
            PINFO("Loaded Filter Start Date is %s", filter[1]);

            priv->fd.start_time = gnc_plugin_page_register2_filter_dmy2time (filter[1]);
            priv->fd.start_time = gnc_time64_get_day_start (priv->fd.start_time);
            filter_changed = filter_changed + 1;

            if (filter[2] && (g_strcmp0 (filter[2], "0") != 0 ))
            {
                PINFO("Loaded Filter End Date is %s", filter[2]);

                priv->fd.end_time = gnc_plugin_page_register2_filter_dmy2time (filter[2]);
                priv->fd.end_time = gnc_time64_get_day_end (priv->fd.end_time);
                filter_changed = filter_changed + 1;
            }
        }

        if (filter_changed != 0)
            priv->fd.save_filter = TRUE;

        priv->fd.original_save_filter = priv->fd.save_filter;
        g_strfreev (filter);

        /* Update Query with Filter Status and Dates */
        gnc_ppr_update_status_query (page, FALSE);
        gnc_ppr_update_date_query (page, FALSE);
    }

//FIXME may change, can we load filter at same time of sort so we do one querry on load
    gnc_ledger_display2_refresh (priv->ledger);

    /* This sets the default selection on load, not required for templates */
    if (!gnc_tree_model_split_reg_get_template (model))
       gnc_tree_view_split_reg_default_selection (view);

    plugin_page->summarybar = gnc_split_reg2_create_summary_bar (priv->gsr);
    if (plugin_page->summarybar)
    {
        gtk_widget_show_all (plugin_page->summarybar);
        gtk_box_pack_start (GTK_BOX (priv->widget), plugin_page->summarybar,
                           FALSE, FALSE, 0);
        gnc_plugin_page_register2_summarybar_position_changed (NULL, NULL, page);
        gnc_prefs_register_cb (GNC_PREFS_GROUP_GENERAL,
                               GNC_PREF_SUMMARYBAR_POSITION_TOP,
                               gnc_plugin_page_register2_summarybar_position_changed,
                               page);
        gnc_prefs_register_cb (GNC_PREFS_GROUP_GENERAL,
                               GNC_PREF_SUMMARYBAR_POSITION_BOTTOM,
                               gnc_plugin_page_register2_summarybar_position_changed,
                               page);
    }

    priv->event_handler_id = qof_event_register_handler
                             ((QofEventHandler)gnc_plugin_page_register2_event_handler, page);
    priv->component_manager_id =
        gnc_register_gui_component (GNC_PLUGIN_PAGE_REGISTER2_NAME,
                                   gnc_plugin_page_register2_refresh_cb,
                                   gnc_plugin_page_register2_close_cb,
                                   page);
    gnc_gui_component_set_session (priv->component_manager_id,
                                   gnc_get_current_session());
    acct = gnc_plugin_page_register2_get_account (page);
    if (acct)
        gnc_gui_component_watch_entity (
            priv->component_manager_id, xaccAccountGetGUID (acct),
            QOF_EVENT_DESTROY | QOF_EVENT_MODIFY);

    /* This allows the plugin page to be updated from the view */
    gnc_split_reg2_set_moved_cb
    (priv->gsr, (GFunc)gnc_plugin_page_register2_ui_update, page);

    LEAVE(" ");
    return priv->widget;
}

/*#################################################################################*/

/** This button press handler calls the common button press handler
 *  for all pages.  The GtkTreeView eats all button presses and
 *  doesn't pass them up the widget tree, even when doesn't do
 *  anything with them.  The only way to get access to the button
 *  presses in an account tree page is here on the tree view widget.
 *  Button presses on all other pages are caught by the signal
 *  registered in gnc-main-window.c. */
static gboolean
gnc_plugin_page_register2_button_press_cb (GtkWidget *widget,
        GdkEventButton *event,
        GncPluginPage *page)
{

    g_return_val_if_fail(GNC_IS_PLUGIN_PAGE(page), FALSE);

    ENTER("widget %p, event %p, page %p", widget, event, page);
    gnc_main_window_button_press_cb (widget, event, page);
    LEAVE(" ");

    /* Always return FALSE.  This will let the tree view callback run as
     * well which will select the item under the cursor.  By the time
     * the user sees the menu both callbacks will have run and the menu
     * actions will operate on the just-selected account. */
    return FALSE;
}

static void
gnc_plugin_page_register2_destroy_widget (GncPluginPage *plugin_page)
{
    GncPluginPageRegister2 *page;
    GncPluginPageRegister2Private *priv;

    ENTER("page %p", plugin_page);
    page = GNC_PLUGIN_PAGE_REGISTER2 (plugin_page);
    priv = GNC_PLUGIN_PAGE_REGISTER2_GET_PRIVATE(plugin_page);

    gnc_prefs_remove_cb_by_func (GNC_PREFS_GROUP_GENERAL,
                                 GNC_PREF_SUMMARYBAR_POSITION_TOP,
                                 gnc_plugin_page_register2_summarybar_position_changed,
                                 page);
    gnc_prefs_remove_cb_by_func (GNC_PREFS_GROUP_GENERAL,
                                 GNC_PREF_SUMMARYBAR_POSITION_BOTTOM,
                                 gnc_plugin_page_register2_summarybar_position_changed,
                                 page);

    if (priv->widget == NULL)
    {
        LEAVE(" ");
        return;
    }

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

    if (priv->fd.dialog)
    {
        gtk_widget_destroy (priv->fd.dialog);
        memset(&priv->fd, 0, sizeof(priv->fd));
    }

    gtk_widget_hide (priv->widget);

    if (priv->ledger)
    {
        gnc_ledger_display2_close (priv->ledger);
        priv->ledger = NULL;
    }

    if (priv->widget)
    {
        g_object_unref (G_OBJECT(priv->widget));
        priv->widget = NULL;
    }
    LEAVE(" ");
}

static void
gnc_plugin_page_register2_window_changed (GncPluginPage *plugin_page,
        GtkWidget *window) // this works
{
    GncPluginPageRegister2 *page;
    GncPluginPageRegister2Private *priv;

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER2 (plugin_page));

    page = GNC_PLUGIN_PAGE_REGISTER2 (plugin_page);
    priv = GNC_PLUGIN_PAGE_REGISTER2_GET_PRIVATE (page);

    priv->gsr->window =
        GTK_WIDGET(gnc_window_get_gtk_window (GNC_WINDOW (window)));
}

static const gchar *style_names[] =
{
    "Ledger",
    "Auto Ledger",
    "Journal",
    NULL
};

#define KEY_REGISTER_TYPE       "RegisterType"
#define KEY_ACCOUNT_NAME        "AccountName"
#define KEY_REGISTER_STYLE      "RegisterStyle"
#define KEY_DOUBLE_LINE         "DoubleLineMode"
#define KEY_EXTRA_DATES         "ExtraDatesMode"

#define LABEL_ACCOUNT		"Account"
#define LABEL_SUBACCOUNT	"SubAccount"
#define LABEL_GL		"GL"
#define LABEL_SEARCH		"Search"

#define SPLIT_REGISTER_GUID "SplitRegister GUID"

/** Save enough information about this register page that it can be
 *  recreated next time the user starts gnucash.
 *
 *  @param plugin_page The page to save.
 *
 *  @param key_file A pointer to the GKeyFile data structure where the
 *  page information should be written. gnc_plugin_page_register2_save_page
 *
 *  @param group_name The group name to use when saving data. */
static void
gnc_plugin_page_register2_save_page (GncPluginPage *plugin_page,
                                    GKeyFile *key_file,
                                    const gchar *group_name)
{
    GncPluginPageRegister2 *page;
    GncPluginPageRegister2Private *priv;
    GNCLedgerDisplay2Type ledger_type;
    GncTreeViewSplitReg *view;
    GncTreeModelSplitReg *model;
    Account *leader;

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER2 (plugin_page));
    g_return_if_fail (key_file != NULL);
    g_return_if_fail (group_name != NULL);

    ENTER("page %p, key_file %p, group_name %s", plugin_page, key_file,
          group_name);

    page = GNC_PLUGIN_PAGE_REGISTER2 (plugin_page);
    priv = GNC_PLUGIN_PAGE_REGISTER2_GET_PRIVATE (page);

    view = gnc_ledger_display2_get_split_view_register (priv->ledger);
    model = gnc_ledger_display2_get_split_model_register (priv->ledger);
    ledger_type = gnc_ledger_display2_type (priv->ledger);
    if (ledger_type > LD2_GL)
    {
        LEAVE("Unsupported ledger type");
        return;
    }
    if ((ledger_type == LD2_SINGLE) || (ledger_type == LD2_SUBACCOUNT))
    {
        const gchar *label;
        gchar* name;
        label = (ledger_type == LD2_SINGLE) ? LABEL_ACCOUNT : LABEL_SUBACCOUNT;
        leader = gnc_ledger_display2_leader (priv->ledger);
        g_key_file_set_string (key_file, group_name, KEY_REGISTER_TYPE, label);
        name = gnc_account_get_full_name (leader);
        g_key_file_set_string (key_file, group_name, KEY_ACCOUNT_NAME, name);
        g_free (name);
    }
    else if (model->type == GENERAL_JOURNAL2)
    {
        g_key_file_set_string (key_file, group_name, KEY_REGISTER_TYPE,
                              LABEL_GL);
    }
    else if (model->type == SEARCH_LEDGER2)
    {
        g_key_file_set_string (key_file, group_name, KEY_REGISTER_TYPE,
                              LABEL_SEARCH);
    }
    else
    {
        LEAVE("Unsupported register type");
        return;
    }

    g_key_file_set_string (key_file, group_name, KEY_REGISTER_STYLE, style_names[model->style]);
    g_key_file_set_boolean (key_file, group_name, KEY_DOUBLE_LINE, model->use_double_line);
    g_key_file_set_boolean (key_file, group_name, KEY_EXTRA_DATES, view->show_extra_dates);

    LEAVE(" ");
}

/** Read and restore the edit menu settings on the specified register
 *  page. This function will restore the register style (ledger, auto
 *  ledger, journal) and whether or not the register is in double line
 *  mode. It should eventually restore the "filter by" and "sort by"
 *  mode.
 *
 *  @param page The register being restored.
 *
 *  @param key_file A pointer to the GKeyFile data structure where the
 *  page information should be read.
 *
 *  @param group_name The group name to use when restoring data. */
static void
gnc_plugin_page_register2_restore_edit_menu (GncPluginPage *page,
        GKeyFile *key_file,
        const gchar *group_name)
{
    GncPluginPageRegister2Private *priv;
    GtkAction *action;
    GError *error = NULL;
    gchar *style_name;
    gint i;
    gboolean use_double_line;
    gboolean show_extra_dates;

    ENTER(" ");
    priv = GNC_PLUGIN_PAGE_REGISTER2_GET_PRIVATE (page);

    /* Convert the style name to an index */
    style_name = g_key_file_get_string (key_file, group_name,
                                       KEY_REGISTER_STYLE, &error);
    for (i = 0 ; style_names[i]; i++)
    {
        if (g_ascii_strcasecmp (style_name, style_names[i]) == 0)
        {
            DEBUG("Found match for style name: %s", style_name);
            break;
        }
    }
    g_free (style_name);

    /* Update the style menu action for this page */
    if (i <= REG2_STYLE_JOURNAL)
    {
        DEBUG("Setting style: %d", i);
        action = gnc_plugin_page_get_action (page, radio_entries_2[i].name);
        gtk_toggle_action_set_active (GTK_TOGGLE_ACTION (action), TRUE);
    }

    /* Update the double line action on this page */
    use_double_line =
        g_key_file_get_boolean (key_file, group_name, KEY_DOUBLE_LINE, &error);
    DEBUG("Setting double_line_mode: %d", use_double_line);
    action = gnc_plugin_page_get_action (page, "ViewStyleDoubleLineAction");
    gtk_toggle_action_set_active (GTK_TOGGLE_ACTION (action), use_double_line);

    /* Update the extra dates action on this page */
    show_extra_dates =
        g_key_file_get_boolean (key_file, group_name, KEY_EXTRA_DATES, &error);
    DEBUG("Setting extra_dates_mode: %d", show_extra_dates);
    action = gnc_plugin_page_get_action (page, "ViewStyleExtraDatesAction");
    gtk_toggle_action_set_active (GTK_TOGGLE_ACTION (action), show_extra_dates);
    LEAVE(" ");
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
static GncPluginPage *
gnc_plugin_page_register2_recreate_page (GtkWidget *window,
                                        GKeyFile *key_file,
                                        const gchar *group_name)
{
    GncPluginPage *page;
    GError *error = NULL;
    gchar *reg_type, *acct_name;
    Account *account;
    QofBook *book;
    gboolean include_subs;

    g_return_val_if_fail (key_file, NULL);
    g_return_val_if_fail (group_name, NULL);
    ENTER("key_file %p, group_name %s", key_file, group_name);

    /* Create the new page. */
    reg_type = g_key_file_get_string (key_file, group_name,
                                     KEY_REGISTER_TYPE, &error);
    DEBUG("Page type: %s", reg_type);
    if ((g_ascii_strcasecmp (reg_type, LABEL_ACCOUNT) == 0) ||
            (g_ascii_strcasecmp (reg_type, LABEL_SUBACCOUNT) == 0))
    {
        include_subs = (g_ascii_strcasecmp(reg_type, LABEL_SUBACCOUNT) == 0);
        DEBUG("Include subs: %d", include_subs);
        acct_name = g_key_file_get_string (key_file, group_name,
                                          KEY_ACCOUNT_NAME, &error);
        book = qof_session_get_book (gnc_get_current_session());
        account = gnc_account_lookup_by_full_name (gnc_book_get_root_account(book),
                  acct_name);
        g_free (acct_name);
        if (account == NULL)
        {
            LEAVE("Bad account name");
            g_free (reg_type);
            return NULL;
        }
        page = gnc_plugin_page_register2_new (account, include_subs);
    }
    else if (g_ascii_strcasecmp (reg_type, LABEL_GL) == 0)
    {
        page = gnc_plugin_page_register2_new_gl ();
    }
    else
    {
        LEAVE("Bad ledger type");
        g_free (reg_type);
        return NULL;
    }
    g_free (reg_type);

    /* Recreate page in given window */
    gnc_plugin_page_set_use_new_window (page, FALSE);

    /* Install it now so we can them manipulate the created widget */
    gnc_main_window_open_page (GNC_MAIN_WINDOW (window), page);

    /* Now update the page to the last state it was in */
    gnc_plugin_page_register2_restore_edit_menu (page, key_file, group_name);
    LEAVE(" ");
    return page;
}


/*
 * Based on code from Epiphany (src/ephy-window.c)
 */
static void
gnc_plugin_page_register2_update_edit_menu (GncPluginPage *page, gboolean hide) //this works
{
    GncPluginPageRegister2Private *priv;
    GncPluginPageRegister2 *reg_page;
    GncTreeViewSplitReg *view;
    GtkAction *action;
    gboolean can_copy = FALSE, can_cut = FALSE, can_paste = FALSE;
    gboolean has_selection;
    gboolean is_readwrite = !qof_book_is_readonly (gnc_get_current_book());
    reg_page = GNC_PLUGIN_PAGE_REGISTER2 (page);
    priv = GNC_PLUGIN_PAGE_REGISTER2_GET_PRIVATE (reg_page);
    view = gnc_ledger_display2_get_split_view_register (priv->ledger);

    // This is set while we are editing a cell
    if (view->editing_now)
        has_selection = TRUE;
    else
        has_selection = FALSE;

    can_copy = has_selection;
    can_cut = is_readwrite && has_selection;
    can_paste = is_readwrite;

    action = gnc_plugin_page_get_action (page, "EditCopyAction");
    gtk_action_set_sensitive (action, can_copy);
    gtk_action_set_visible (action, !hide || can_copy);
    action = gnc_plugin_page_get_action (page, "EditCutAction");
    gtk_action_set_sensitive (action, can_cut);
    gtk_action_set_visible (action, !hide || can_cut);
    action = gnc_plugin_page_get_action (page, "EditPasteAction");
    gtk_action_set_sensitive (action, can_paste);
    gtk_action_set_visible (action,  !hide || can_paste);
}

static gboolean
gnc_plugin_page_register2_finish_pending (GncPluginPage *page) //this works
{
    GncPluginPageRegister2Private *priv;
    GncPluginPageRegister2 *reg_page;
    GncTreeViewSplitReg *view;
    GtkWidget *dialog, *window;
    const gchar *name;
    gint response;

    reg_page = GNC_PLUGIN_PAGE_REGISTER2 (page);
    priv = GNC_PLUGIN_PAGE_REGISTER2_GET_PRIVATE (reg_page);
    view = gnc_ledger_display2_get_split_view_register (priv->ledger);

    /* Make sure we have stopped editing */
    gnc_tree_view_split_reg_finish_edit (view);

    if (!view || (gnc_tree_view_split_reg_get_dirty_trans (view) == NULL))
        return TRUE;

    name = gnc_plugin_page_register2_get_tab_name (page);
    window = gnc_plugin_page_get_window (page);
    dialog = gtk_message_dialog_new (GTK_WINDOW (window),
                                    GTK_DIALOG_DESTROY_WITH_PARENT,
                                    GTK_MESSAGE_WARNING,
                                    GTK_BUTTONS_NONE,
                                    /* Translators: %s is the name
                                       of the tab page */
                                    _("Save changes to %s?"), name);
    gtk_message_dialog_format_secondary_text
    (GTK_MESSAGE_DIALOG (dialog),
     "%s",
     _("This register has pending changes to a transaction. "
       "Would you like to save the changes to this transaction, "
       "discard the transaction, or cancel the operation?"));
    gnc_gtk_dialog_add_button (dialog, _("_Discard Transaction"),
                              GTK_STOCK_DELETE, GTK_RESPONSE_REJECT);
    gtk_dialog_add_button (GTK_DIALOG (dialog),
                          GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL);
    gnc_gtk_dialog_add_button (dialog, _("_Save Transaction"),
                              GTK_STOCK_SAVE, GTK_RESPONSE_ACCEPT);

    response = gtk_dialog_run (GTK_DIALOG (dialog));
    gtk_widget_destroy (dialog);

    switch (response)
    {
    case GTK_RESPONSE_ACCEPT:
        return (gnc_tree_control_split_reg_save (view, TRUE));

    case GTK_RESPONSE_REJECT:
        gnc_tree_control_split_reg_cancel_edit (view, TRUE);
        return TRUE;

    default:
        return FALSE;
    }
}

static gchar *
gnc_plugin_page_register2_get_tab_name (GncPluginPage *plugin_page)
{
    GncPluginPageRegister2Private *priv;
    GNCLedgerDisplay2Type ledger_type;
    GNCLedgerDisplay2 *ld;
    GncTreeModelSplitReg *model;
    Account *leader;

    g_return_val_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER2 (plugin_page), _("unknown"));

    priv = GNC_PLUGIN_PAGE_REGISTER2_GET_PRIVATE(plugin_page);
    ld = priv->ledger;
    model = gnc_ledger_display2_get_split_model_register(ld);
    ledger_type = gnc_ledger_display2_type (ld);
    leader = gnc_ledger_display2_leader (ld);

    switch (ledger_type)
    {
    case LD2_SINGLE:
        return g_strdup(xaccAccountGetName (leader));

    case LD2_SUBACCOUNT:
        return g_strdup_printf("%s+", xaccAccountGetName (leader));

    case LD2_GL:
        switch (model->type)
        {
        case GENERAL_JOURNAL:
        case INCOME_LEDGER:
            return g_strdup(_("General Journal"));
        case PORTFOLIO_LEDGER:
            return g_strdup(_("Portfolio"));
        case SEARCH_LEDGER:
            return g_strdup(_("Search Results"));
        default:
            break;
        }
        break;

    default:
        break;
    }

    return g_strdup(_("unknown"));
}

static gchar *
gnc_plugin_page_register2_get_tab_color (GncPluginPage *plugin_page)
{
    GncPluginPageRegister2Private *priv;
    GNCLedgerDisplay2Type ledger_type;
    GNCLedgerDisplay2 *ld;
    Account *leader;
    const char* color;

    g_return_val_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER2 (plugin_page), _("unknown"));

    priv = GNC_PLUGIN_PAGE_REGISTER2_GET_PRIVATE(plugin_page);
    ld = priv->ledger;
    ledger_type = gnc_ledger_display2_type (ld);
    leader = gnc_ledger_display2_leader (ld);
    color = NULL;

    if ((ledger_type == LD2_SINGLE) || (ledger_type == LD2_SUBACCOUNT))
        color = xaccAccountGetColor (leader);

    return g_strdup(color ? color : "Not Set");
}

static gchar *
gnc_plugin_page_register2_get_filter (GncPluginPage *plugin_page)
{
    GncPluginPageRegister2Private *priv;
    GNCLedgerDisplay2Type ledger_type;
    GNCLedgerDisplay2 *ld;
    Account *leader;
    const char* filter;

    g_return_val_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER2 (plugin_page), _("unknown"));

    priv = GNC_PLUGIN_PAGE_REGISTER2_GET_PRIVATE(plugin_page);
    ld = priv->ledger;
    ledger_type = gnc_ledger_display2_type (ld);
    leader = gnc_ledger_display2_leader (ld);
    filter = NULL;

    if ((ledger_type == LD2_SINGLE) || (ledger_type == LD2_SUBACCOUNT))
        filter = xaccAccountGetFilter (leader);

    return filter ? g_strdup(filter) : g_strdup_printf("%s,%s,%s", DEFAULT_FILTER, "0", "0");
}

void
gnc_plugin_page_register2_set_filter (GncPluginPage *plugin_page, const gchar *filter )
{
    GncPluginPageRegister2Private *priv;
    GNCLedgerDisplay2Type ledger_type;
    GNCLedgerDisplay2 *ld;
    Account *leader;
    gchar *default_filter;

    priv = GNC_PLUGIN_PAGE_REGISTER2_GET_PRIVATE(plugin_page);
    ld = priv->ledger;
    ledger_type = gnc_ledger_display2_type (ld);
    leader = gnc_ledger_display2_leader (ld);

    if (leader != NULL)
    {
        default_filter = g_strdup_printf("%s,%s,%s", DEFAULT_FILTER, "0", "0");

        if (!filter || (g_strcmp0 (filter, default_filter) == 0))
            xaccAccountSetFilter (leader, NULL);
        else
            xaccAccountSetFilter (leader, filter);

        g_free (default_filter);
    }
    return;
}

static gchar *
gnc_plugin_page_register2_get_long_name (GncPluginPage *plugin_page)
{
    GncPluginPageRegister2Private *priv;
    GNCLedgerDisplay2Type ledger_type;
    GNCLedgerDisplay2 *ld;
    Account *leader;

    g_return_val_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER2 (plugin_page), _("unknown"));

    priv = GNC_PLUGIN_PAGE_REGISTER2_GET_PRIVATE (plugin_page);
    ld = priv->ledger;
    ledger_type = gnc_ledger_display2_type (ld);
    leader = gnc_ledger_display2_leader (ld);

    switch (ledger_type)
    {
    case LD2_SINGLE:
        return gnc_account_get_full_name (leader);

    case LD2_SUBACCOUNT:
    {
        gchar *account_full_name = gnc_account_get_full_name (leader);
        gchar *return_string = g_strdup_printf ("%s+", account_full_name);
        g_free ((gpointer *) account_full_name);
        return return_string;
    }

    default:
        break;
    }

    return NULL;
}

static void
gnc_plugin_page_register2_summarybar_position_changed (gpointer prefs, gchar* pref, gpointer user_data)
{
    GncPluginPage *plugin_page;
    GncPluginPageRegister2 *page;
    GncPluginPageRegister2Private *priv;
    GtkPositionType position = GTK_POS_BOTTOM;

    g_return_if_fail (user_data != NULL);

    if (!GNC_IS_PLUGIN_PAGE (user_data))
        return;

    plugin_page = GNC_PLUGIN_PAGE (user_data);
    page = GNC_PLUGIN_PAGE_REGISTER2 (user_data);
    priv = GNC_PLUGIN_PAGE_REGISTER2_GET_PRIVATE (page);

    if (priv == NULL)
       return;

    if (gnc_prefs_get_bool (GNC_PREFS_GROUP_GENERAL, GNC_PREF_SUMMARYBAR_POSITION_TOP))
        position = GTK_POS_TOP;

    gtk_box_reorder_child (GTK_BOX (priv->widget),
                          plugin_page->summarybar,
                          (position == GTK_POS_TOP ? 0 : -1) );
}

/*#################################################################################*/
/*#################################################################################*/

/************************************************************/
/*                    "Filter By" Dialog                    */
/************************************************************/

/** This function updates the "cleared match" term of the register
 *  query.  It unconditionally removes any old "cleared match" query
 *  term, then adds back a new query term if needed.  There seems to
 *  be a bug in the current g2 register code such that when the number
 *  of entries in the register doesn't fill up the window, the blank
 *  space at the end of the window isn't correctly redrawn.  This
 *  function works around that problem, but a root cause analysis
 *  should probably be done.
 *
 *  @param page A pointer to the GncPluginPageRegister2 that is
 *  associated with this filter dialog.
 */
static void
gnc_ppr_update_status_query (GncPluginPageRegister2 *page, gboolean refresh_page)
{
    GncPluginPageRegister2Private *priv;
    GSList *param_list;
    Query *query;

    ENTER(" ");
    priv = GNC_PLUGIN_PAGE_REGISTER2_GET_PRIVATE (page);
    query = gnc_ledger_display2_get_query (priv->ledger );
    if (!query)
    {
        LEAVE("no query found");
        return;
    }

    /* Remove the old status match */
    param_list = qof_query_build_param_list (SPLIT_RECONCILE, NULL);
    if (param_list)
    {
        qof_query_purge_terms (query, param_list);
        g_slist_free(param_list);
    }

    /* Install the new status match */
    if (priv->fd.cleared_match != CLEARED_ALL)
        xaccQueryAddClearedMatch(query, priv->fd.cleared_match, QOF_QUERY_AND);

    if (refresh_page)
        gnc_ledger_display2_refresh (priv->ledger);
    LEAVE(" ");
}

/** This function updates the "date posted" term of the register
 *  query.  It unconditionally removes any old "date posted" query
 *  term, then adds back a new query term if needed.  There seems to
 *  be a bug in the current g2 register code such that when the number
 *  of entries in the register doesn't fill up the window, the blank
 *  space at the end of the window isn't correctly redrawn.  This
 *  function works around that problem, but a root cause analysis
 *  should probably be done.
 *
 *  @param page A pointer to the GncPluginPageRegister2 that is
 *  associated with this filter dialog.
 */
static void
gnc_ppr_update_date_query (GncPluginPageRegister2 *page, gboolean refresh_page)
{
    GncPluginPageRegister2Private *priv;
    GSList *param_list;
    Query *query;

    ENTER(" ");
    priv = GNC_PLUGIN_PAGE_REGISTER2_GET_PRIVATE (page);
    if (!priv->ledger)
    {
        LEAVE("no ledger");
        return;
    }

    query = gnc_ledger_display2_get_query (priv->ledger);
    if (!query)
    {
        LEAVE("no query");
        return;
    }

    /* Delete any existing old date spec. */
    param_list = qof_query_build_param_list (SPLIT_TRANS, TRANS_DATE_POSTED, NULL);
    if (param_list)
    {
        qof_query_purge_terms (query, param_list);
        g_slist_free(param_list);
    }

    if (priv->fd.start_time || priv->fd.end_time)
    {
        /* Build a new spec */
        xaccQueryAddDateMatchTT (query,
                                priv->fd.start_time != 0, priv->fd.start_time,
                                priv->fd.end_time != 0,   priv->fd.end_time,
                                QOF_QUERY_AND);
    }

    if (refresh_page)
        gnc_ledger_display2_refresh (priv->ledger);
    LEAVE(" ");
}

/* This function converts a time64 value date to a string */
static gchar *
gnc_plugin_page_register2_filter_time2dmy (time64 raw_time)
{
    struct tm * timeinfo;
    gchar date_string[11];
    gint i;

    timeinfo = gnc_localtime (&raw_time);
    i = strftime (date_string, 11, "%d-%m-%Y", timeinfo);
    PINFO("Date string is %s", date_string);

    gnc_tm_free (timeinfo);
    return g_strdup (date_string);
}

/* This function converts a string date to a time64 value */
static time64
gnc_plugin_page_register2_filter_dmy2time (char *date_string)
{
    struct tm when;

    PINFO("Date string is %s", date_string);
    memset (&when, 0, sizeof (when));
    sscanf (date_string, "%d-%d-%d", &when.tm_mday,
	   &when.tm_mon, &when.tm_year);

    when.tm_year -= 1900;
    when.tm_mon -=  1;

    return gnc_mktime (&when);
}

/** This function is called whenever one of the status entries is
 *  checked or unchecked.  It updates the status value maintained for
 *  the filter dialog, and calls another function to do the work of
 *  applying the change to the register itself.
 *
 *  @param button The toggle button that was changed.
 *
 *  @param page A pointer to the GncPluginPageRegister2 that is
 *  associated with this filter dialog.
 */
void
gnc_plugin_page_register2_filter_status_one_cb (GtkToggleButton *button,
        GncPluginPageRegister2 *page)
{
    GncPluginPageRegister2Private *priv;
    const gchar *name;
    gint i, value;

    g_return_if_fail (GTK_IS_CHECK_BUTTON (button));
    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER2 (page));

    name = gtk_buildable_get_name (GTK_BUILDABLE (button));
    ENTER("toggle button %s (%p), plugin_page %p", name, button, page);

    /* Determine what status bit to change */
    value = CLEARED_NONE;
    for (i = 0; status_actions[i].action_name; i++)
    {
        if (g_strcmp0 (name, status_actions[i].action_name) == 0)
        {
            value = status_actions[i].value;
            break;
        }
    }

    /* Compute the new match status */
    priv = GNC_PLUGIN_PAGE_REGISTER2_GET_PRIVATE (page);
    if (gtk_toggle_button_get_active (button))
        priv->fd.cleared_match |= value;
    else
        priv->fd.cleared_match &= ~value;
    gnc_ppr_update_status_query (page, TRUE);
    LEAVE(" ");
}

/** This function is called whenever the "select all" status button is
 *  clicked. It updates all of the checkbox widgets, then updates the
 *  query on the register.
 *
 *  @param button The button that was clicked.
 *
 *  @param page A pointer to the GncPluginPageRegister2 that is
 *  associated with this filter dialog.
 */
void
gnc_plugin_page_register2_filter_status_all_cb (GtkButton *button,
        GncPluginPageRegister2 *page)
{
    GncPluginPageRegister2Private *priv;
    GtkWidget *widget;
    gint i;

    g_return_if_fail (GTK_IS_BUTTON (button));
    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER2 (page));

    ENTER("(button %p, page %p)", button, page);

    /* Turn on all the check menu items */
    for (i = 0; status_actions[i].action_name; i++)
    {
        widget = status_actions[i].widget;
        g_signal_handlers_block_by_func (widget, gnc_plugin_page_register2_filter_status_one_cb, page);
        gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (widget), TRUE);
        g_signal_handlers_unblock_by_func (widget, gnc_plugin_page_register2_filter_status_one_cb, page);
    }

    /* Set the requested status */
    priv = GNC_PLUGIN_PAGE_REGISTER2_GET_PRIVATE (page);
    priv->fd.cleared_match = CLEARED_ALL;
    gnc_ppr_update_status_query (page, TRUE);
    LEAVE(" ");
}

/** This function computes the starting and ending times for the
 *  filter by examining the dialog widgets to see which ones are
 *  selected, and will pull times out of the data entry boxes if
 *  necessary. This function must exist to handle the case where the
 *  "show all" button was Selected, and the user clicks on the "select
 *  range" button. Since it exists, it make sense for the rest of the
 *  callbacks to take advantage of it.
 *
 *  @param page A pointer to the GncPluginPageRegister2 that is
 *  associated with this filter dialog.
 */
static void
get_filter_times (GncPluginPageRegister2 *page)
{
    GncPluginPageRegister2Private *priv;
    GtkWidget *button, *today, *gde;
    time64 time_val;

    priv = GNC_PLUGIN_PAGE_REGISTER2_GET_PRIVATE (page);
    if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (priv->fd.start_date_choose)))
    {
        time_val = gnc_date_edit_get_date (GNC_DATE_EDIT (priv->fd.start_date));
        time_val = gnc_time64_get_day_start (time_val);
        priv->fd.start_time = time_val;
    }
    else
    {
        if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (priv->fd.start_date_today)))
        {
            priv->fd.start_time = gnc_time64_get_today_start();
        }
        else
        {
            priv->fd.start_time = 0;
        }
    }

    if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (priv->fd.end_date_choose)))
    {
        time_val = gnc_date_edit_get_date (GNC_DATE_EDIT (priv->fd.end_date));
        time_val = gnc_time64_get_day_end (time_val);
        priv->fd.end_time = time_val;
    }
    else
    {
        if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (priv->fd.start_date_today)))
        {
            priv->fd.end_time = gnc_time64_get_today_end();
        }
        else
        {
            priv->fd.end_time = 0;
        }
    }
}

/** This function is called when the "select range" radio button
 *  changes state. Since there are only two choices in this radio
 *  group, this one signal can be used to handle all cases. This
 *  function is responsible for setting the sensitivity of the table
 *  of widgets underneath the "select range" choice, and updating the
 *  time limitation on the register query. This is handled by a
 *  helper function when the radio button is selected (as potentially
 *  all the widgets in the table need to be inspected), and is trivial
 *  for the other case.
 *
 *  @param button A pointer to the "select range" radio button.
 *
 *  @param page A pointer to the GncPluginPageRegister2 that is
 *  associated with this filter dialog.
 */
void
gnc_plugin_page_register2_filter_select_range_cb (GtkRadioButton *button,
        GncPluginPageRegister2 *page)
{
    GncPluginPageRegister2Private *priv;
    gboolean active;

    g_return_if_fail (GTK_IS_RADIO_BUTTON (button));
    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER2 (page));

    ENTER("(button %p, page %p)", button, page);
    priv = GNC_PLUGIN_PAGE_REGISTER2_GET_PRIVATE (page);
    active = gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (button));
    gtk_widget_set_sensitive (priv->fd.table, active);
    if (active)
    {
        get_filter_times (page);
    }
    else
    {
        priv->fd.start_time = 0;
        priv->fd.end_time = 0;
    }
    gnc_ppr_update_date_query (page, TRUE);
    LEAVE(" ");
}

/** This function is called when one of the start date entry widgets
 *  is updated. It simply calls common routines to determine the
 *  start/end times and update the register filter.
 *
 *  @param unused A pointer to a GncDateEntry widgets, but it could be
 *  any widget.
 *
 *  @param page A pointer to the GncPluginPageRegister2 that is
 *  associated with this filter dialog.
 */
static void
gnc_plugin_page_register2_filter_gde_changed_cb (GtkWidget *unused,
        GncPluginPageRegister2 *page)
{
    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER2 (page));

    ENTER("(widget %s(%p), page %p)", gtk_buildable_get_name (GTK_BUILDABLE (unused)), unused, page);
    get_filter_times (page);
    gnc_ppr_update_date_query (page, TRUE);
    LEAVE(" ");
}

/** This function is called when one of the start date radio buttons
 *  is selected. It updates the sensitivity of the date entry widget,
 *  then calls a common routine to determine the start/end times and
 *  update the register query.
 *
 *  *Note: This function is actually called twice for each new radio
 *  button selection. The first time call is to uncheck the old
 *  button, and the second time to check the new button. This does
 *  make a kind of sense, as radio buttons are nothing more than
 *  linked toggle buttons where only one can be active.
 *
 *  @param radio The button whose state is changing. This will be
 *  the previously selected button the first of the pair of calls to
 *  this function, and will be the newly selected button the second
 *  time.
 *
 *  @param page A pointer to the GncPluginPageRegister2 that is
 *  associated with this filter dialog.
 */
void
gnc_plugin_page_register2_filter_start_cb (GtkWidget *radio,
        GncPluginPageRegister2 *page)
{
    GncPluginPageRegister2Private *priv;
    const gchar *name;
    gboolean active;

    g_return_if_fail (GTK_IS_RADIO_BUTTON (radio));
    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER2 (page));

    ENTER("(radio %s(%p), page %p)", gtk_buildable_get_name (GTK_BUILDABLE (radio)), radio, page);
    priv = GNC_PLUGIN_PAGE_REGISTER2_GET_PRIVATE (page);
    if (!gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (radio)))
    {
        LEAVE("1st callback of pair. Defer to 2nd callback.");
        return;
    }

    name = gtk_buildable_get_name (GTK_BUILDABLE (radio));
    active = (g_strcmp0 (name, g_strdup ("start_date_choose")) == 0 ? 1 : 0 );
    gtk_widget_set_sensitive (priv->fd.start_date, active);
    get_filter_times (page);
    gnc_ppr_update_date_query (page, TRUE);
    LEAVE(" ");
}

/** This function is called when one of the end date radio buttons is
 *  selected. It updates the sensitivity of the date entry widget,
 *  then calls a common routine to determine the start/end times and
 *  update the register query.
 *
 *  *Note: This function is actually called twice for each new radio
 *  button selection. The first time call is to uncheck the old
 *  button, and the second time to check the new button. This does
 *  make a kind of sense, as radio buttons are nothing more than
 *  linked toggle buttons where only one can be active.
 *
 *  @param radio The button whose state is changing. This will be
 *  the previously selected button the first of the pair of calls to
 *  this function, and will be the newly selected button the second
 *  time.
 *
 *  @param page A pointer to the GncPluginPageRegister2 that is
 *  associated with this filter dialog.
 */
void
gnc_plugin_page_register2_filter_end_cb (GtkWidget *radio,
                                        GncPluginPageRegister2 *page)
{
    GncPluginPageRegister2Private *priv;
    const gchar *name;
    gboolean active;

    g_return_if_fail (GTK_IS_RADIO_BUTTON (radio));
    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER2 (page));

    ENTER("(radio %s(%p), page %p)", gtk_buildable_get_name (GTK_BUILDABLE (radio)), radio, page);
    priv = GNC_PLUGIN_PAGE_REGISTER2_GET_PRIVATE (page);
    if (!gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (radio)))
    {
        LEAVE("1st callback of pair. Defer to 2nd callback.");
        return;
    }

    name = gtk_buildable_get_name (GTK_BUILDABLE (radio));
    active = (g_strcmp0 (name, g_strdup ("end_date_choose")) == 0 ? 1 : 0 );
    gtk_widget_set_sensitive (priv->fd.end_date, active);
    get_filter_times (page);
    gnc_ppr_update_date_query (page, TRUE);
    LEAVE(" ");
}

/** This function is called whenever the save status is checked
 *  or unchecked. It will allow saving of the filter if required.
 *
 *  @param button The toggle button that was changed.
 *
 *  @param page A pointer to the GncPluginPageRegister2 that is
 *  associated with this filter dialog.
 */
void
gnc_plugin_page_register2_filter_save_cb (GtkToggleButton *button,
        GncPluginPageRegister2 *page)
{
    GncPluginPageRegister2Private *priv;

    g_return_if_fail (GTK_IS_CHECK_BUTTON (button));
    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER2 (page));

    ENTER("Save toggle button (%p), plugin_page %p", button, page);

    /* Compute the new save filter status */
    priv = GNC_PLUGIN_PAGE_REGISTER2_GET_PRIVATE (page);
    if (gtk_toggle_button_get_active (button))
        priv->fd.save_filter = TRUE;
    else
        priv->fd.save_filter = FALSE;
    LEAVE(" ");
}

/** This function is called when the "Filter By..." dialog is closed.
 *  If the dialog was closed by any method other than clicking the OK
 *  button, the original filter will be restored.
 *
 *  @param dialog A pointer to the dialog box.
 *
 *  @param response A numerical value indicating why the dialog box was closed.
 *
 *  @param page A pointer to the GncPluginPageRegister2 associated with
 *  this dialog box.
 */
void
gnc_plugin_page_register2_filter_response_cb (GtkDialog *dialog,
        gint response,
        GncPluginPageRegister2 *page)
{
    GncPluginPageRegister2Private *priv;
    GncPluginPage *plugin_page;

    g_return_if_fail (GTK_IS_DIALOG (dialog));
    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER2 (page));

    ENTER(" ");
    priv = GNC_PLUGIN_PAGE_REGISTER2_GET_PRIVATE (page);
    plugin_page = GNC_PLUGIN_PAGE (page);

    if (response != GTK_RESPONSE_OK)
    {
        /* Remove the old status match */
        priv->fd.cleared_match = priv->fd.original_cleared_match;
        gnc_ppr_update_status_query (page, FALSE);
        priv->fd.start_time = priv->fd.original_start_time;
        priv->fd.end_time = priv->fd.original_end_time;
        priv->fd.save_filter = priv->fd.original_save_filter;
        gnc_ppr_update_date_query (page, FALSE);
        gnc_ledger_display2_refresh (priv->ledger);
    }
    else
    {
        priv->fd.original_save_filter = priv->fd.save_filter;

        if (priv->fd.save_filter)
        {
            gchar* filter;
            filter = g_strdup_printf ("0x%04x", priv->fd.cleared_match);

            if ( gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (priv->fd.start_date_choose)) && priv->fd.start_time != 0 )
            {
                gchar *timeval = gnc_plugin_page_register2_filter_time2dmy (priv->fd.start_time);
                filter = g_strconcat (filter, ",", timeval, NULL);
                g_free (timeval);
            }
            else
                filter = g_strconcat (filter, ",0", NULL);

            if ( gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (priv->fd.end_date_choose)) && priv->fd.end_time != 0 )
            {
                gchar *timeval = gnc_plugin_page_register2_filter_time2dmy (priv->fd.end_time);
                filter = g_strconcat (filter, ",", timeval, NULL);
                g_free (timeval);
            }
            else
                filter = g_strconcat (filter, ",0", NULL);

            PINFO("The filter to save is %s", filter);

            gnc_plugin_page_register2_set_filter (plugin_page, filter);
            g_free (filter);
        }
    }
    priv->fd.dialog = NULL;
    gtk_widget_destroy (GTK_WIDGET (dialog));
    LEAVE(" ");
}

/*#################################################################################*/
/*#################################################################################*/

/************************************************************/
/*                  Report Helper Functions                 */
/************************************************************/

static char *
gnc_reg_get_name (GNCLedgerDisplay2 *ledger, gboolean for_window) // this works
{
    Account *leader;
    GncTreeModelSplitReg *model;
    gchar *account_name;
    gchar *reg_name;
    gchar *name;
    GNCLedgerDisplay2Type ledger_type;

    if (ledger == NULL)
        return NULL;

    model = gnc_ledger_display2_get_split_model_register (ledger);
    ledger_type = gnc_ledger_display2_type (ledger);

    switch (model->type)
    {
    case GENERAL_JOURNAL:
    case INCOME_LEDGER:
        if (for_window)
            reg_name = _("General Journal");
        else
            reg_name = _("General Journal Report");
        break;
    case PORTFOLIO_LEDGER:
        if (for_window)
            reg_name = _("Portfolio");
        else
            reg_name = _("Portfolio Report");
        break;
    case SEARCH_LEDGER:
        if (for_window)
            reg_name = _("Search Results");
        else
            reg_name = _("Search Results Report");
        break;
    default:
        if (for_window)
            reg_name = _("Register");
        else
            reg_name = _("Register Report");
        break;
    }

    leader = gnc_ledger_display2_leader (ledger);

    if ((leader != NULL) && (ledger_type != LD2_GL))
    {
        account_name = gnc_account_get_full_name (leader);

        if (ledger_type == LD2_SINGLE)
        {
            name = g_strconcat (account_name, " - ", reg_name, NULL);
        }
        else
        {
            name = g_strconcat (account_name, " ", _("and subaccounts"), " - ", reg_name, NULL);
        }
        g_free (account_name);
    }
    else
        name = g_strdup (reg_name);

    return name;
}

static int
report_helper (GNCLedgerDisplay2 *ledger, Split *split, Query *query) //this works
{
    GncTreeViewSplitReg *view;
    GncTreeModelSplitReg *model;
    Account *account;
    char *str;
    const char *tmp;
    swig_type_info * qtype;
    SCM args;
    SCM func;
    SCM arg;

    args = SCM_EOL;

    view = gnc_ledger_display2_get_split_view_register (ledger);
    model = gnc_ledger_display2_get_split_model_register (ledger);

    func = scm_c_eval_string ("gnc:register-report-create");
    g_return_val_if_fail (scm_is_procedure (func), -1);
    tmp = gnc_tree_view_split_reg_get_credit_debit_string (view, TRUE);

    arg = scm_from_utf8_string (tmp ? tmp : _("Credit"));
    args = scm_cons (arg, args);
    tmp = gnc_tree_view_split_reg_get_credit_debit_string (view, FALSE);
    arg = scm_from_utf8_string (tmp ? tmp : _("Debit"));
    args = scm_cons (arg, args);

    str = gnc_reg_get_name (ledger, FALSE);
    arg = scm_from_utf8_string (str ? str : "");
    args = scm_cons (arg, args);
    g_free (str);

    arg = SCM_BOOL (model->use_double_line);
    args = scm_cons (arg, args);

    arg = SCM_BOOL (model->type == GENERAL_JOURNAL2 || model->type == INCOME_LEDGER2
                                                   || model->type == SEARCH_LEDGER2);
    args = scm_cons (arg, args);

    arg = SCM_BOOL (model->style == REG2_STYLE_JOURNAL);
    args = scm_cons (arg, args);

    if (!query)
    {
        query = gnc_ledger_display2_get_query (ledger);
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

    account = gnc_ledger_display2_leader (ledger);
    arg = SWIG_NewPointerObj (account, qtype, 0);
    args = scm_cons (arg, args);
    g_return_val_if_fail (arg != SCM_UNDEFINED, -1);


    /* Apply the function to the args */
    arg = scm_apply (func, args, SCM_EOL);
    g_return_val_if_fail (scm_is_exact (arg), -1);

    return scm_to_int (arg);
}

/*#################################################################################*/
/*#################################################################################*/

/************************************************************/
/*                     Command callbacks                    */
/************************************************************/

static void
gnc_plugin_page_register2_cmd_print_check (GtkAction *action,
        GncPluginPageRegister2 *plugin_page) // this works
{
    GncPluginPageRegister2Private *priv;
    GncTreeViewSplitReg *view;
    GncTreeModelSplitReg *model;
    Split         * split;
    Transaction   * trans;
    GList         * splits = NULL, *item;
    GNCLedgerDisplay2Type ledger_type;
    Account       * account;

    ENTER("(action %p, plugin_page %p)", action, plugin_page);

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER2 (plugin_page));

    priv = GNC_PLUGIN_PAGE_REGISTER2_GET_PRIVATE (plugin_page);
    view = gnc_ledger_display2_get_split_view_register (priv->ledger);
    model = gnc_ledger_display2_get_split_model_register (priv->ledger);
    ledger_type = gnc_ledger_display2_type (priv->ledger);

    if (ledger_type == LD2_SINGLE || ledger_type == LD2_SUBACCOUNT)
    {
        account  = gnc_plugin_page_register2_get_account (plugin_page);
        split = gnc_tree_view_split_reg_get_current_split (view);
        trans = xaccSplitGetParent (split);

        if (trans == NULL)
        {
            LEAVE("trans is NULL");
            return;
        }

        /* See if we were asked to print a blank trans. */
        if (trans == gnc_tree_control_split_reg_get_blank_trans (view))
        {
            LEAVE("Asked to print a blank trans");
            return;
        }

        /* See if we are being edited in another register */
        if (gnc_tree_control_split_reg_trans_test_for_edit (view, trans))
        {
            LEAVE("trans being edited in another register");
            return;
        }

        /* Make sure we ask to commit any changes before we proceed */
        if (gnc_tree_control_split_reg_trans_open_and_warn (view, trans))
        {
            LEAVE("trans being edited");
            return;
        }

        if (split && trans)
        {
            if (xaccSplitGetAccount(split) == account)
            {
                splits = g_list_append(splits, split);
                gnc_ui_print_check_dialog_create2 (plugin_page, splits);
                g_list_free(splits);
            }
            else
            {
                /* This split is not for the account shown in this register.  Get the
                   split for that account and use it. */
                split = gnc_tree_model_split_reg_trans_get_split_equal_to_ancestor(trans, account);
                if (split)
                {
                    splits = g_list_append(splits, split);
                    gnc_ui_print_check_dialog_create2 (plugin_page, splits);
                    g_list_free(splits);
                }
            }           
        }
    }
    else if (ledger_type == LD2_GL && model->type == SEARCH_LEDGER2)
    {
        Account *common_acct = NULL, *account;
        splits = qof_query_run (gnc_ledger_display2_get_query (priv->ledger));
        /* Make sure each split is from the same account */
        for (item = splits; item; item = g_list_next (item))
        {
            split = (Split *) item->data;
            if (common_acct == NULL)
            {
                common_acct = xaccSplitGetAccount (split);
            }
            else
            {
                if (xaccSplitGetAccount (split) != common_acct)
                {
                    GtkWidget *dialog, *window;
                    gint response;
                    const gchar *title = _("Print checks from multiple accounts?");
                    const gchar *message =
                        _("This search result contains splits from more than one account. "
                          "Do you want to print the checks even though they are not all "
                          "from the same account?");
                    window = gnc_plugin_page_get_window (GNC_PLUGIN_PAGE (plugin_page));
                    dialog = gtk_message_dialog_new (GTK_WINDOW (window),
                                                    GTK_DIALOG_DESTROY_WITH_PARENT,
                                                    GTK_MESSAGE_WARNING,
                                                    GTK_BUTTONS_CANCEL,
                                                    "%s", title);
                    gtk_message_dialog_format_secondary_text (GTK_MESSAGE_DIALOG (dialog),
                            "%s", message);
                    gtk_dialog_add_button (GTK_DIALOG (dialog), _("_Print checks"),
                                          GTK_RESPONSE_YES);
                    response = gnc_dialog_run (GTK_DIALOG (dialog),
                                               GNC_PREF_WARN_CHECKPRINTING_MULTI_ACCT);
                    gtk_widget_destroy (dialog);
                    if (response != GTK_RESPONSE_YES)
                    {
                        LEAVE("Multiple accounts");
                        return;
                    }
                    break;
                }
            }
        }
        gnc_ui_print_check_dialog_create2 (plugin_page, splits);
    }
    else
    {
        gnc_error_dialog (gnc_plugin_page_get_window (GNC_PLUGIN_PAGE (plugin_page)), "%s",
                         _("You can only print checks from a bank account register or search results."));
        LEAVE("Unsupported ledger type");
        return;
    }
    LEAVE(" ");
}

static void
gnc_plugin_page_register2_cmd_cut (GtkAction *action,
                                  GncPluginPageRegister2 *page) //this works
{
    GncPluginPageRegister2Private *priv;
    GncTreeViewSplitReg *view;
    GtkWidget *window, *widget;

    g_return_if_fail(GNC_IS_PLUGIN_PAGE_REGISTER2 (page));

    ENTER("(action %p, page %p)", action, page);
    priv = GNC_PLUGIN_PAGE_REGISTER2_GET_PRIVATE (page);
    view = gnc_ledger_display2_get_split_view_register (priv->ledger);

    window = gnc_plugin_page_get_window (GNC_PLUGIN_PAGE (page));
    widget = gtk_window_get_focus (GTK_WINDOW (window));

    DEBUG("(widget name is %s)", gtk_widget_get_name (widget));

    if (GTK_IS_ENTRY(widget))
        g_signal_emit_by_name(widget, "cut-clipboard", NULL);

    LEAVE("");
}

static void
gnc_plugin_page_register2_cmd_copy (GtkAction *action,
                                   GncPluginPageRegister2 *page) //this works
{
    GncPluginPageRegister2Private *priv;
    GncTreeViewSplitReg *view;
    GtkWidget *window, *widget;

    g_return_if_fail(GNC_IS_PLUGIN_PAGE_REGISTER2 (page));

    ENTER("(action %p, page %p)", action, page);
    priv = GNC_PLUGIN_PAGE_REGISTER2_GET_PRIVATE (page);
    view = gnc_ledger_display2_get_split_view_register (priv->ledger);

    window = gnc_plugin_page_get_window (GNC_PLUGIN_PAGE (page));
    widget = gtk_window_get_focus (GTK_WINDOW (window));

    DEBUG("(widget name is %s)", gtk_widget_get_name (widget));

    if (GTK_IS_ENTRY(widget))
        g_signal_emit_by_name (widget, "copy-clipboard", NULL);

    LEAVE("");
}

static void
gnc_plugin_page_register2_cmd_paste (GtkAction *action,
                                    GncPluginPageRegister2 *page) //this works
{
    GncPluginPageRegister2Private *priv;
    GncTreeViewSplitReg *view;
    GtkWidget *window, *widget;

    g_return_if_fail(GNC_IS_PLUGIN_PAGE_REGISTER2 (page));

    ENTER("(action %p, page %p)", action, page);
    priv = GNC_PLUGIN_PAGE_REGISTER2_GET_PRIVATE (page);
    view = gnc_ledger_display2_get_split_view_register (priv->ledger);

    window = gnc_plugin_page_get_window (GNC_PLUGIN_PAGE (page));
    widget = gtk_window_get_focus (GTK_WINDOW (window));

    DEBUG("(widget name is %s)", gtk_widget_get_name (widget));

    if (GTK_IS_ENTRY(widget))
        g_signal_emit_by_name (widget, "paste-clipboard", NULL);

    LEAVE("");
}

static void
gnc_plugin_page_register2_cmd_edit_account (GtkAction *action,
        GncPluginPageRegister2 *page) //this works
{
    Account *account;

    g_return_if_fail(GNC_IS_PLUGIN_PAGE_REGISTER2(page));

    ENTER("(action %p, page %p)", action, page);
    account = gnc_plugin_page_register2_get_account (page);
    if (account)
        gnc_ui_edit_account_window (account);
    LEAVE(" ");
}

static void
gnc_plugin_page_register2_cmd_find_transactions (GtkAction *action,
        GncPluginPageRegister2 *page) // this works
{
    GncPluginPageRegister2Private *priv;

    g_return_if_fail(GNC_IS_PLUGIN_PAGE_REGISTER2(page));

    ENTER("(action %p, page %p)", action, page);
    priv = GNC_PLUGIN_PAGE_REGISTER2_GET_PRIVATE(page);

    gnc_ui_find_transactions_dialog_create2 (priv->ledger);
    LEAVE(" ");
}

static void
gnc_plugin_page_register2_cmd_cut_transaction (GtkAction *action,
        GncPluginPageRegister2 *page) // this works
{
    GncPluginPageRegister2Private *priv;
    GncTreeViewSplitReg *view;

    g_return_if_fail(GNC_IS_PLUGIN_PAGE_REGISTER2(page));

    ENTER("(action %p, page %p)", action, page);
    priv = GNC_PLUGIN_PAGE_REGISTER2_GET_PRIVATE(page);
    view = gnc_ledger_display2_get_split_view_register (priv->ledger);
    gnc_tree_control_split_reg_cut_trans (view);

    LEAVE(" ");
}

static void
gnc_plugin_page_register2_cmd_copy_transaction (GtkAction *action,
        GncPluginPageRegister2 *page) // this works
{
    GncPluginPageRegister2Private *priv;
    GncTreeViewSplitReg *view;

    g_return_if_fail(GNC_IS_PLUGIN_PAGE_REGISTER2(page));

    ENTER("(action %p, page %p)", action, page);
    priv = GNC_PLUGIN_PAGE_REGISTER2_GET_PRIVATE(page);
    view = gnc_ledger_display2_get_split_view_register (priv->ledger);
    gnc_tree_control_split_reg_copy_trans (view);
    LEAVE(" ");
}

static void
gnc_plugin_page_register2_cmd_paste_transaction (GtkAction *action,
        GncPluginPageRegister2 *page) //this works
{
    GncPluginPageRegister2Private *priv;
    GncTreeViewSplitReg *view;

    g_return_if_fail(GNC_IS_PLUGIN_PAGE_REGISTER2(page));

    ENTER("(action %p, page %p)", action, page);
    priv = GNC_PLUGIN_PAGE_REGISTER2_GET_PRIVATE(page);
    view = gnc_ledger_display2_get_split_view_register (priv->ledger);
    gnc_tree_control_split_reg_paste_trans (view);
    LEAVE(" ");
}

static void
gnc_plugin_page_register2_cmd_void_transaction (GtkAction *action,
        GncPluginPageRegister2 *page) //this works
{
    GncPluginPageRegister2Private *priv;
    GtkWidget *dialog, *entry;
    GncTreeViewSplitReg *view;
    Transaction *trans;
    GtkBuilder *builder;
    const char *reason;
    gint result;

    ENTER("(action %p, page %p)", action, page);

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER2 (page));

    priv = GNC_PLUGIN_PAGE_REGISTER2_GET_PRIVATE (page);
    view = gnc_ledger_display2_get_split_view_register (priv->ledger);
    trans = gnc_tree_view_split_reg_get_current_trans (view);
    if (trans == NULL)
    {
        LEAVE("trans is NULL");
        return;
    }
    if (xaccTransHasSplitsInState (trans, VREC))
    {
        LEAVE("trans has split in VREC state");
        return;
    }
    if (xaccTransHasReconciledSplits (trans) || xaccTransHasSplitsInState (trans, CREC))
    {
        gnc_error_dialog (NULL, "%s", _("You cannot void a transaction with reconciled or cleared splits."));
        LEAVE("trans with reconciled splits");
        return;
    }

    if (!gnc_plugin_page_register2_finish_pending (GNC_PLUGIN_PAGE (page)))
    {
        LEAVE("finish pending");
        return;
    }

    builder = gtk_builder_new();
    gnc_builder_add_from_file  (builder , "gnc-plugin-page-register2.glade", "Void Transaction");
    dialog = GTK_WIDGET (gtk_builder_get_object (builder, "Void Transaction"));
    entry = GTK_WIDGET (gtk_builder_get_object (builder, "reason"));

    result = gtk_dialog_run (GTK_DIALOG (dialog));
    if (result == GTK_RESPONSE_OK)
    {
        reason = gtk_entry_get_text (GTK_ENTRY (entry));
        if (reason == NULL)
            reason = "";
        gnc_tree_control_split_reg_void_current_trans (view, reason);
    }

    /* All done. Get rid of it. */
    gtk_widget_destroy (dialog);
    g_object_unref (G_OBJECT(builder));
    LEAVE(" ");
}

static void
gnc_plugin_page_register2_cmd_unvoid_transaction (GtkAction *action,
        GncPluginPageRegister2 *page) //this works
{
    GncPluginPageRegister2Private *priv;
    GncTreeViewSplitReg *view;
    Transaction *trans;

    ENTER("(action %p, page %p)", action, page);

    g_return_if_fail(GNC_IS_PLUGIN_PAGE_REGISTER2 (page));

    priv = GNC_PLUGIN_PAGE_REGISTER2_GET_PRIVATE (page);
    view = gnc_ledger_display2_get_split_view_register (priv->ledger);
    trans = gnc_tree_view_split_reg_get_current_trans (view);
    if (!xaccTransHasSplitsInState (trans, VREC))
    {
        LEAVE("trans has split in VREC state");
        return;
    }
    gnc_tree_control_split_reg_unvoid_current_trans (view);
    LEAVE(" ");
}

static void
gnc_plugin_page_register2_cmd_reverse_transaction (GtkAction *action,
        GncPluginPageRegister2 *page) //this works
{
    GncPluginPageRegister2Private *priv;
    GncTreeViewSplitReg *view;

    ENTER("(action %p, page %p)", action, page);

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER2 (page));

    priv = GNC_PLUGIN_PAGE_REGISTER2_GET_PRIVATE (page);
    view = gnc_ledger_display2_get_split_view_register (priv->ledger);

    gnc_tree_control_split_reg_reverse_current (view);
    LEAVE(" ");
}

static void
gnc_plugin_page_register2_cmd_entryUp (GtkAction *action,
                                       GncPluginPageRegister2 *plugin_page)
{
    GncPluginPageRegister2Private *priv;
    GncTreeViewSplitReg *view;
    g_return_if_fail(GNC_IS_PLUGIN_PAGE_REGISTER2(plugin_page));

    ENTER("(action %p, plugin_page %p)", action, plugin_page);
    priv = GNC_PLUGIN_PAGE_REGISTER2_GET_PRIVATE(plugin_page);
    view = gnc_ledger_display2_get_split_view_register (priv->ledger);
    g_return_if_fail(view);
    gnc_tree_control_split_reg_move_current_entry_updown(view, TRUE);
    LEAVE(" ");
}

static void
gnc_plugin_page_register2_cmd_entryDown (GtkAction *action,
                                         GncPluginPageRegister2 *plugin_page)
{
    GncPluginPageRegister2Private *priv;
    GncTreeViewSplitReg *view;
    g_return_if_fail(GNC_IS_PLUGIN_PAGE_REGISTER2(plugin_page));

    ENTER("(action %p, plugin_page %p)", action, plugin_page);
    priv = GNC_PLUGIN_PAGE_REGISTER2_GET_PRIVATE(plugin_page);
    view = gnc_ledger_display2_get_split_view_register (priv->ledger);
    g_return_if_fail(view);
    gnc_tree_control_split_reg_move_current_entry_updown(view, FALSE);
    LEAVE(" ");
}


/*#################################################################################*/
/*#################################################################################*/

static void
gnc_plugin_page_register2_cmd_view_filter_by (GtkAction *action,
        GncPluginPageRegister2 *page) //this works
{
    GncPluginPageRegister2Private *priv;
    GNCLedgerDisplay2Type ledger_type;
    GtkWidget *dialog, *toggle, *button, *start_date, *end_date, *table, *hbox;
    time64 start_time, end_time, time_val;
    GtkBuilder *builder;
    gboolean sensitive, value;
    Query *query;
    GList *split_list;
    gchar *title;
    int i;

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER2 (page));
    ENTER("(action %p, page %p)", action, page);

    priv = GNC_PLUGIN_PAGE_REGISTER2_GET_PRIVATE (page);
    if (priv->fd.dialog)
    {
        gtk_window_present (GTK_WINDOW (priv->fd.dialog));
        LEAVE("existing dialog");
        return;
    }

    /* Create the dialog */
    builder = gtk_builder_new();
    gnc_builder_add_from_file (builder, "gnc-plugin-page-register2.glade", "Filter By");
    dialog = GTK_WIDGET (gtk_builder_get_object (builder, "Filter By"));
    priv->fd.dialog = dialog;
    gtk_window_set_transient_for (GTK_WINDOW (dialog),
                                 gnc_window_get_gtk_window (GNC_WINDOW (GNC_PLUGIN_PAGE (page)->window)));

    /* Translators: The %s is the name of the plugin page */
    title = g_strdup_printf (_("Filter %s by..."),
                            gnc_plugin_page_get_page_name (GNC_PLUGIN_PAGE (page)));
    gtk_window_set_title (GTK_WINDOW (dialog), title);
    g_free(title);

    /* Set the check buttons for the current status */
    for (i = 0; status_actions[i].action_name; i++)
    {
        toggle = GTK_WIDGET (gtk_builder_get_object (builder, status_actions[i].action_name));
        value = priv->fd.cleared_match & status_actions[i].value;
        status_actions[i].widget = toggle;
        gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (toggle), value);
    }
    priv->fd.original_cleared_match = priv->fd.cleared_match;

    button = GTK_WIDGET (gtk_builder_get_object (builder, "filter_save"));
    if (priv->fd.save_filter == TRUE)
        gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (button), TRUE);

    // General Journals can not save filters so disable button.
    ledger_type = gnc_ledger_display2_type (priv->ledger);
    if (ledger_type == LD2_GL)
       gtk_widget_set_sensitive (GTK_WIDGET (button), FALSE);

    /* Set the date info */
    button = GTK_WIDGET (gtk_builder_get_object (builder, "filter_show_range"));
    query = gnc_ledger_display2_get_query (priv->ledger);
    xaccQueryGetDateMatchTT (query, &start_time, &end_time);
    priv->fd.original_start_time = start_time;
    priv->fd.start_time = start_time;
    priv->fd.original_end_time = end_time;
    priv->fd.end_time = end_time;

    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (button), start_time || end_time);
    table = GTK_WIDGET (gtk_builder_get_object (builder, "select_range_table"));
    priv->fd.table = table;
    gtk_widget_set_sensitive (GTK_WIDGET (table), start_time || end_time);

    priv->fd.start_date_choose = GTK_WIDGET (gtk_builder_get_object (builder, "start_date_choose"));
    priv->fd.start_date_today = GTK_WIDGET (gtk_builder_get_object (builder, "start_date_today"));
    priv->fd.end_date_choose = GTK_WIDGET (gtk_builder_get_object (builder, "end_date_choose"));
    priv->fd.end_date_today = GTK_WIDGET (gtk_builder_get_object (builder, "end_date_today"));

    {
        /* Start date info */
        if (start_time == 0)
        {
            button = GTK_WIDGET (gtk_builder_get_object (builder, "start_date_earliest"));
            time_val = xaccQueryGetEarliestDateFound (query);
            sensitive = FALSE;
        }
        else
        {
            time_val = start_time;
            if ((start_time >= gnc_time64_get_today_start()) &&
                    (start_time <= gnc_time64_get_today_end()))
            {
                button = priv->fd.start_date_today;
                sensitive = FALSE;
            }
            else
            {
                button = priv->fd.start_date_choose;
                sensitive = TRUE;
            }
        }
        gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (button), TRUE);
        priv->fd.start_date = gnc_date_edit_new (gnc_time (NULL), FALSE, FALSE);
        hbox = GTK_WIDGET (gtk_builder_get_object (builder, "start_date_hbox"));
        gtk_box_pack_start (GTK_BOX (hbox), priv->fd.start_date, TRUE, TRUE, 0);
        gtk_widget_show (priv->fd.start_date);
        gtk_widget_set_sensitive (GTK_WIDGET (priv->fd.start_date), sensitive);
        gnc_date_edit_set_time (GNC_DATE_EDIT (priv->fd.start_date), time_val);
        g_signal_connect (G_OBJECT (priv->fd.start_date), "date-changed",
                          G_CALLBACK (gnc_plugin_page_register2_filter_gde_changed_cb),
                          page);
    }

    {
        /* End date info */
        if (end_time == 0)
        {
            button = GTK_WIDGET (gtk_builder_get_object (builder, "end_date_latest"));
            time_val = xaccQueryGetLatestDateFound (query);
            sensitive = FALSE;
        }
        else
        {
            time_val = end_time;
            if ((end_time >= gnc_time64_get_today_start()) &&
                    (end_time <= gnc_time64_get_today_end()))
            {
                button = priv->fd.end_date_today;
                sensitive = FALSE;
            }
            else
            {
                button = priv->fd.end_date_choose;
                sensitive = TRUE;
            }
        }
        gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (button), TRUE);
        priv->fd.end_date = gnc_date_edit_new (gnc_time (NULL), FALSE, FALSE);
        hbox = GTK_WIDGET (gtk_builder_get_object (builder, "end_date_hbox"));
        gtk_box_pack_start (GTK_BOX (hbox), priv->fd.end_date, TRUE, TRUE, 0);
        gtk_widget_show (priv->fd.end_date);
        gtk_widget_set_sensitive (GTK_WIDGET (priv->fd.end_date), sensitive);
        gnc_date_edit_set_time (GNC_DATE_EDIT (priv->fd.end_date), time_val);
        g_signal_connect (G_OBJECT (priv->fd.end_date), "date-changed",
                          G_CALLBACK (gnc_plugin_page_register2_filter_gde_changed_cb),
                          page);
    }

    /* Wire it up */
    gtk_builder_connect_signals_full (builder, gnc_builder_connect_full_func, page);

    /* Show it */
    gtk_widget_show_all (dialog);
    g_object_unref (G_OBJECT (builder));
    LEAVE(" ");
}


static void
gnc_plugin_page_register2_cmd_reload (GtkAction *action, GncPluginPageRegister2 *plugin_page) //this works
{
    GncPluginPageRegister2Private *priv;
    GncTreeViewSplitReg *view;

    Transaction *trans;

    ENTER("(action %p, page %p)", action, plugin_page);

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER2 (plugin_page));

    priv = GNC_PLUGIN_PAGE_REGISTER2_GET_PRIVATE (plugin_page);

    view = gnc_ledger_display2_get_split_view_register (priv->ledger);

    trans = gnc_tree_view_split_reg_get_current_trans (view);

    /* Make sure we ask to commit any changes before we proceed */
    if (gnc_tree_control_split_reg_trans_open_and_warn (view, trans))
    {
        LEAVE("trans being edited");
        return;
    }

    /* give gtk+ a chance to handle pending events */
    while (gtk_events_pending ())
        gtk_main_iteration ();

    gnc_ledger_display2_refresh (priv->ledger);

    LEAVE(" ");
}

/*#################################################################################*/
/*#################################################################################*/

static void
gnc_plugin_page_register2_cmd_style_changed (GtkAction *action,
        GtkRadioAction *current,
        GncPluginPageRegister2 *plugin_page) //this works
{
    GncPluginPageRegister2Private *priv;
    SplitRegisterStyle2 value;

    ENTER("(action %p, radio action %p, plugin_page %p)",
          action, current, plugin_page);

    g_return_if_fail (GTK_IS_ACTION (action));
    g_return_if_fail (GTK_IS_RADIO_ACTION (current));
    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER2 (plugin_page));

    priv = GNC_PLUGIN_PAGE_REGISTER2_GET_PRIVATE (plugin_page);
    value = gtk_radio_action_get_current_value (current);
    gnc_split_reg2_change_style (priv->gsr, value);

    gnc_plugin_page_register2_ui_update (NULL, plugin_page);
    LEAVE(" ");
}

static void
gnc_plugin_page_register2_cmd_style_double_line (GtkToggleAction *action,
        GncPluginPageRegister2 *plugin_page) // this works
{
    GncPluginPageRegister2Private *priv;
    GncTreeModelSplitReg *model;
    GncTreeViewSplitReg *view;
    gboolean use_double_line;

    ENTER("(action %p, plugin_page %p)", action, plugin_page);

    g_return_if_fail (GTK_IS_ACTION (action));
    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER2 (plugin_page));

    priv = GNC_PLUGIN_PAGE_REGISTER2_GET_PRIVATE (plugin_page);
    model = gnc_ledger_display2_get_split_model_register (priv->ledger);

    view = gnc_ledger_display2_get_split_view_register (priv->ledger);

    use_double_line = gtk_toggle_action_get_active (action);
    if (use_double_line != model->use_double_line)
    {
        gnc_tree_model_split_reg_config (model, model->type, model->style, use_double_line);

        // This will re-display the view.
        gnc_tree_view_split_reg_set_format (view);
        gnc_ledger_display2_refresh (priv->ledger);
    }
    LEAVE(" ");
}

static void
gnc_plugin_page_register2_cmd_style_extra_dates (GtkToggleAction *action,
        GncPluginPageRegister2 *plugin_page) // this works
{
    GncPluginPageRegister2Private *priv;
    GncTreeModelSplitReg *model;
    GncTreeViewSplitReg *view;
    gboolean show_extra_dates;

    ENTER("(action %p, plugin_page %p)", action, plugin_page);

    g_return_if_fail (GTK_IS_ACTION (action));
    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER2 (plugin_page));

    priv = GNC_PLUGIN_PAGE_REGISTER2_GET_PRIVATE (plugin_page);
    model = gnc_ledger_display2_get_split_model_register (priv->ledger);

    view = gnc_ledger_display2_get_split_view_register (priv->ledger);

    show_extra_dates = gtk_toggle_action_get_active (action);
    if (show_extra_dates != view->show_extra_dates)
    {
        view->show_extra_dates = show_extra_dates;
        gnc_ledger_display2_refresh (priv->ledger);
    }
    LEAVE(" ");
}

static void
gnc_plugin_page_register2_cmd_transfer (GtkAction *action,
                                       GncPluginPageRegister2 *page) //this works
{
    Account *account;
    GncWindow *gnc_window;
    GtkWidget *window;

    ENTER("(action %p, plugin_page %p)", action, page);

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER2 (page));

    account = gnc_plugin_page_register2_get_account (page);
    gnc_window = GNC_WINDOW (GNC_PLUGIN_PAGE (page)->window);
    window = GTK_WIDGET (gnc_window_get_gtk_window (gnc_window));
    gnc_xfer_dialog (window, account);
    LEAVE(" ");
}

static void
gnc_plugin_page_register2_cmd_reconcile (GtkAction *action,
                                        GncPluginPageRegister2 *page) // this works
{
    GncPluginPageRegister2Private *priv;
    GncTreeViewSplitReg *view;
    Account *account;
    Transaction *trans;
    GtkWindow *window;
    RecnWindow2 * recnData;

    ENTER("(action %p, plugin_page %p)", action, page);

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER2 (page));

    priv = GNC_PLUGIN_PAGE_REGISTER2_GET_PRIVATE (page);
    view = gnc_ledger_display2_get_split_view_register (priv->ledger);

    account = gnc_plugin_page_register2_get_account (page);

    trans = gnc_tree_view_split_reg_get_current_trans (view);

    /* Make sure we ask to commit any changes before we proceed */
    if (gnc_tree_control_split_reg_trans_open_and_warn (view, trans))
    {
        LEAVE("trans being edited");
        return;
    }

    window = gnc_window_get_gtk_window (GNC_WINDOW (GNC_PLUGIN_PAGE (page)->window));
    recnData = recnWindow2 (GTK_WIDGET (window), account);
    gnc_ui_reconcile_window2_raise (recnData);
    LEAVE(" ");
}

static void
gnc_plugin_page_register2_cmd_autoclear (GtkAction *action,
                                        GncPluginPageRegister2 *page)
{
    Account *account;
    GtkWindow *window;
    AutoClearWindow * autoClearData;

    ENTER("(action %p, plugin_page %p)", action, page);

    g_return_if_fail(GNC_IS_PLUGIN_PAGE_REGISTER2(page));

    account = gnc_plugin_page_register2_get_account (page);

    window = gnc_window_get_gtk_window(GNC_WINDOW(GNC_PLUGIN_PAGE (page)->window));
    autoClearData = autoClearWindow (GTK_WIDGET(window), account);
    gnc_ui_autoclear_window_raise (autoClearData);
    LEAVE(" ");
}

static void
gnc_plugin_page_register2_cmd_stock_split (GtkAction *action,
        GncPluginPageRegister2 *page) // this works
{
    Account *account;

    ENTER("(action %p, plugin_page %p)", action, page);

    g_return_if_fail(GNC_IS_PLUGIN_PAGE_REGISTER2(page));

    account = gnc_plugin_page_register2_get_account (page);
    gnc_stock_split_dialog (NULL, account);
    LEAVE(" ");
}

static void
gnc_plugin_page_register2_cmd_lots (GtkAction *action,
                                   GncPluginPageRegister2 *page) // this works
{
    Account *account;

    ENTER("(action %p, plugin_page %p)", action, page);

    g_return_if_fail(GNC_IS_PLUGIN_PAGE_REGISTER2(page));

    account = gnc_plugin_page_register2_get_account (page);
    gnc_lot_viewer_dialog (account);
    LEAVE(" ");
}

static void
gnc_plugin_page_register2_cmd_enter_transaction (GtkAction *action,
        GncPluginPageRegister2 *plugin_page) //this works
{
    GncPluginPageRegister2Private *priv;
    GncTreeViewSplitReg *view;

    ENTER("(action %p, plugin_page %p)", action, plugin_page);

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER2 (plugin_page));

    priv = GNC_PLUGIN_PAGE_REGISTER2_GET_PRIVATE (plugin_page);
    view = gnc_ledger_display2_get_split_view_register (priv->ledger);
    gnc_tree_control_split_reg_enter (view);
    LEAVE(" ");
}

static void
gnc_plugin_page_register2_cmd_cancel_transaction (GtkAction *action,
        GncPluginPageRegister2 *plugin_page) //this works
{
    GncPluginPageRegister2Private *priv;
    GncTreeViewSplitReg *view;

    ENTER("(action %p, plugin_page %p)", action, plugin_page);

    g_return_if_fail(GNC_IS_PLUGIN_PAGE_REGISTER2 (plugin_page));

    priv = GNC_PLUGIN_PAGE_REGISTER2_GET_PRIVATE (plugin_page);
    view = gnc_ledger_display2_get_split_view_register (priv->ledger);
    gnc_tree_control_split_reg_cancel_edit (view, FALSE);
    LEAVE(" ");
}

static void
gnc_plugin_page_register2_cmd_delete_transaction (GtkAction *action,
        GncPluginPageRegister2 *plugin_page) //this works
{
    GncPluginPageRegister2Private *priv;
    GncTreeViewSplitReg *view;

    ENTER("(action %p, plugin_page %p)", action, plugin_page);

    g_return_if_fail(GNC_IS_PLUGIN_PAGE_REGISTER2 (plugin_page));

    priv = GNC_PLUGIN_PAGE_REGISTER2_GET_PRIVATE (plugin_page);
    view = gnc_ledger_display2_get_split_view_register (priv->ledger);
    gnc_tree_control_split_reg_delete (view, NULL);
    LEAVE(" ");

}

static void
gnc_plugin_page_register2_cmd_blank_transaction (GtkAction *action,
        GncPluginPageRegister2 *plugin_page) //this works
{
    GncPluginPageRegister2Private *priv;
    GncTreeViewSplitReg *view;

    ENTER("(action %p, plugin_page %p)", action, plugin_page);

    g_return_if_fail(GNC_IS_PLUGIN_PAGE_REGISTER2 (plugin_page));

    priv = GNC_PLUGIN_PAGE_REGISTER2_GET_PRIVATE (plugin_page);
    view = gnc_ledger_display2_get_split_view_register (priv->ledger);
    gnc_tree_control_split_reg_jump_to_blank (view);
    LEAVE(" ");
}

static void
gnc_plugin_page_register2_cmd_duplicate_transaction (GtkAction *action,
        GncPluginPageRegister2 *plugin_page) //this works
{
    GncPluginPageRegister2Private *priv;
    GncTreeViewSplitReg *view;

    ENTER("(action %p, plugin_page %p)", action, plugin_page);

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER2 (plugin_page));

    priv = GNC_PLUGIN_PAGE_REGISTER2_GET_PRIVATE (plugin_page);
    view = gnc_ledger_display2_get_split_view_register (priv->ledger);
    gnc_tree_control_split_reg_duplicate_current (view);
    LEAVE(" ");
}

static void
gnc_plugin_page_register2_cmd_reinitialize_transaction (GtkAction *action,
        GncPluginPageRegister2 *plugin_page) //this works
{
    GncPluginPageRegister2Private *priv;
    GncTreeViewSplitReg *view;

    ENTER("(action %p, plugin_page %p)", action, plugin_page);

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER2 (plugin_page));

    priv = GNC_PLUGIN_PAGE_REGISTER2_GET_PRIVATE (plugin_page);
    view = gnc_ledger_display2_get_split_view_register (priv->ledger);
    gnc_tree_control_split_reg_reinit (view, NULL);
    LEAVE(" ");
}

static void
gnc_plugin_page_register2_cmd_expand_transaction (GtkToggleAction *action,
        GncPluginPageRegister2 *plugin_page) // this works
{
    GncPluginPageRegister2Private *priv;
    GncTreeViewSplitReg *view;
    gboolean expand;

    ENTER("(action %p, plugin_page %p)", action, plugin_page);

    g_return_if_fail(GNC_IS_PLUGIN_PAGE_REGISTER2 (plugin_page));

    priv = GNC_PLUGIN_PAGE_REGISTER2_GET_PRIVATE (plugin_page);
    view = gnc_ledger_display2_get_split_view_register (priv->ledger);
    expand = gtk_toggle_action_get_active (action);
    if (expand)
        gnc_tree_view_split_reg_expand_trans (view, NULL);
    else
        gnc_tree_view_split_reg_collapse_trans (view, NULL);
    LEAVE(" ");
}

static void
gnc_plugin_page_register2_cmd_exchange_rate (GtkAction *action,
        GncPluginPageRegister2 *plugin_page) //this works
{
    GncPluginPageRegister2Private *priv;
    GncTreeViewSplitReg *view;

    ENTER("(action %p, plugin_page %p)", action, plugin_page);

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER2 (plugin_page));

    priv = GNC_PLUGIN_PAGE_REGISTER2_GET_PRIVATE (plugin_page);
    view = gnc_ledger_display2_get_split_view_register (priv->ledger);
    gnc_tree_control_split_reg_exchange_rate (view);
    LEAVE(" ");
}

static void
gnc_plugin_page_register2_cmd_jump (GtkAction *action,
        GncPluginPageRegister2 *plugin_page) //this works
{
    GncPluginPageRegister2Private *priv;
    GncPluginPage *new_page;
    GncPluginPageRegister2 *new_reg_page;
    GtkWidget *window;
    GNCLedgerDisplay2 *ld;
    GncTreeViewSplitReg *view, *new_view;
    GncTreeModelSplitReg *new_model;
    Account *account;
    Account *leader;
    Split *split;
    RowDepth depth;

    ENTER("(action %p, plugin_page %p)", action, plugin_page);

    g_return_if_fail(GNC_IS_PLUGIN_PAGE_REGISTER2 (plugin_page));

    priv = GNC_PLUGIN_PAGE_REGISTER2_GET_PRIVATE (plugin_page);
    window = GNC_PLUGIN_PAGE (plugin_page)->window;
    if (window == NULL)
    {
        LEAVE("no window");
        return;
    }

    view = gnc_ledger_display2_get_split_view_register (priv->ledger);
    split = gnc_tree_view_split_reg_get_current_split (view);
    if (split == NULL)
    {
        split = gnc_tree_control_split_reg_get_current_trans_split (view);
        if (split == NULL)
        {
            LEAVE("split is NULL");
            return;
        }
    }

    if (!gnc_tree_view_split_reg_trans_expanded (view, NULL))
    {
        Transaction *trans = xaccSplitGetParent (split);
        if (xaccTransCountSplits (trans) > 2)
        {
            LEAVE("more than 2 splits");
            return;
        }
    }

    depth = gnc_tree_view_reg_get_selected_row_depth (view);
    if (gnc_tree_view_split_reg_trans_expanded (view, NULL) && depth != SPLIT3)
    {
        LEAVE("expanded but no split selected");
        return;
    }

    account = xaccSplitGetAccount (split);
    if (account == NULL)
    {
        LEAVE("account is NULL");
        return;
    }

    leader = gnc_ledger_display2_leader (priv->ledger);
    if (account == leader)
    {
        split = xaccSplitGetOtherSplit (split);
        if (split == NULL)
        {
            LEAVE("no other split");
            return;
        }

        account = xaccSplitGetAccount (split);
        if (account == NULL)
        {
            LEAVE("no other account");
            return;
        }

        if (account == leader)
        {
            LEAVE("register open for account");
            return;
        }
    }

    new_page = gnc_plugin_page_register2_new (account, FALSE);
    if (new_page == NULL)
    {
        LEAVE("couldn't create new page");
        return;
    }

    gnc_main_window_open_page (GNC_MAIN_WINDOW (window), new_page);

    ld = gnc_plugin_page_register2_get_ledger (new_page);
    new_view = gnc_ledger_display2_get_split_view_register (ld);
    new_model = gnc_ledger_display2_get_split_model_register (ld);

    new_model->current_trans = xaccSplitGetParent (split);

    if (!gnc_tree_model_split_reg_trans_is_in_view (new_model, xaccSplitGetParent (split)))
        g_signal_emit_by_name (new_model, "refresh_trans");

    gnc_tree_control_split_reg_jump_to (new_view, NULL, split, FALSE);
    LEAVE(" ");
}


/**
 * Schedules the current transaction for recurring-entry.
 * If the selected transaction was created from a scheduled transaction,
 * opens the editor for that Scheduled Transaction.
 **/
static void
gnc_plugin_page_register2_cmd_schedule (GtkAction *action,
                                       GncPluginPageRegister2 *plugin_page) // this works
{
    GncPluginPageRegister2Private *priv;
    GncTreeViewSplitReg *view;
    Transaction *trans;

    ENTER("(action %p, plugin_page %p)", action, plugin_page);

    g_return_if_fail(GNC_IS_PLUGIN_PAGE_REGISTER2(plugin_page));

    priv = GNC_PLUGIN_PAGE_REGISTER2_GET_PRIVATE(plugin_page);
    view = gnc_ledger_display2_get_split_view_register (priv->ledger);

    trans = gnc_tree_view_split_reg_get_current_trans (view);

    if (trans == NULL)
    {
        LEAVE("trans is NULL");
        return;
    }

    /* See if we were asked to schedule a blank trans. */
    if (trans == gnc_tree_control_split_reg_get_blank_trans (view))
    {
        LEAVE("Asked to schedule a blank trans");
        return;
    }

    /* See if we are being edited in another register */
    if (gnc_tree_control_split_reg_trans_test_for_edit (view, trans))
    {
        LEAVE("trans being edited in another register");
        return;
    }

    /* Make sure we ask to commit any changes before we proceed */
    if (gnc_tree_control_split_reg_trans_open_and_warn (view, trans))
    {
        LEAVE("trans being edited");
        return;
    }

    /* If the transaction has a sched-xact KVP frame, then go to the editor
     * for the existing SX; otherwise, do the sx-from-trans dialog. */
    {
	GncGUID *fromSXId = NULL;
	SchedXaction *theSX = NULL;
	GList *sxElts;
	qof_instance_get (QOF_INSTANCE (trans),
			  "from-sched-xaction", &fromSXId,
			  NULL);

	/* Get the correct SX */
	for ( sxElts = gnc_book_get_schedxactions (gnc_get_current_book())->sx_list;
	      (!theSX) && sxElts;
	      sxElts = sxElts->next )
	{
	    SchedXaction *sx = (SchedXaction*)sxElts->data;
	    theSX =
		((guid_equal (xaccSchedXactionGetGUID (sx), fromSXId))
		 ? sx : NULL);
	}

	if (theSX)
	{
	    gnc_ui_scheduled_xaction_editor_dialog_create2 (theSX, FALSE);
	    LEAVE(" ");
	    return;
	}
    }
    gnc_sx_create_from_trans (trans);
    LEAVE(" ");
}

static void
gnc_plugin_page_register2_cmd_scrub_current (GtkAction *action,
        GncPluginPageRegister2 *plugin_page) // this works
{
    GncPluginPageRegister2Private *priv;
    GncTreeViewSplitReg *view;
    Query *query;
    Account *root;
    Transaction *trans;

    g_return_if_fail(GNC_IS_PLUGIN_PAGE_REGISTER2 (plugin_page));

    ENTER("(action %p, plugin_page %p)", action, plugin_page);

    priv = GNC_PLUGIN_PAGE_REGISTER2_GET_PRIVATE (plugin_page);
    query = gnc_ledger_display2_get_query (priv->ledger);
    if (query == NULL)
    {
        LEAVE("no query found");
        return;
    }

    view = gnc_ledger_display2_get_split_view_register (priv->ledger);
    trans = gnc_tree_view_split_reg_get_current_trans (view);
    if (trans == NULL)
    {
        LEAVE("no trans found");
        return;
    }

    gnc_suspend_gui_refresh();
    root = gnc_get_current_root_account ();
    xaccTransScrubOrphans (trans);
    xaccTransScrubImbalance (trans, root, NULL);
    gnc_resume_gui_refresh ();
    LEAVE(" ");
}

static void
gnc_plugin_page_register2_cmd_scrub_all (GtkAction *action,
                                        GncPluginPageRegister2 *plugin_page) // this works
{
    GncPluginPageRegister2Private *priv;
    Query *query;
    Account *root;
    Transaction *trans;
    Split *split;
    GList *node;

    g_return_if_fail(GNC_IS_PLUGIN_PAGE_REGISTER2(plugin_page));

    ENTER("(action %p, plugin_page %p)", action, plugin_page);

    priv = GNC_PLUGIN_PAGE_REGISTER2_GET_PRIVATE(plugin_page);
    query = gnc_ledger_display2_get_query( priv->ledger );
    if (!query)
    {
        LEAVE("no query found");
        return;
    }

    gnc_suspend_gui_refresh();
    root = gnc_get_current_root_account();

    for (node = qof_query_run(query); node; node = node->next)
    {
        split = node->data;
        trans = xaccSplitGetParent(split);

        xaccTransScrubOrphans(trans);
        xaccTransScrubImbalance(trans, root, NULL);
    }

    gnc_resume_gui_refresh();
    LEAVE(" ");
}

static void
gnc_plugin_page_register2_cmd_account_report (GtkAction *action,
        GncPluginPageRegister2 *plugin_page) // this works
{
    GncPluginPageRegister2Private *priv;
    GncMainWindow *window;
    int id;

    ENTER("(action %p, plugin_page %p)", action, plugin_page);

    g_return_if_fail(GNC_IS_PLUGIN_PAGE_REGISTER2(plugin_page));

    window = GNC_MAIN_WINDOW(GNC_PLUGIN_PAGE(plugin_page)->window);
    priv = GNC_PLUGIN_PAGE_REGISTER2_GET_PRIVATE(plugin_page);
    id = report_helper (priv->ledger, NULL, NULL);
    if (id >= 0)
        gnc_main_window_open_report(id, window);
    LEAVE(" ");
}

static void
gnc_plugin_page_register2_cmd_transaction_report (GtkAction *action,
        GncPluginPageRegister2 *plugin_page) // this works
{
    GncPluginPageRegister2Private *priv;
    GncMainWindow *window;
    GncTreeViewSplitReg *view;
    Split *split;
    Query *query;
    int id;

    ENTER("(action %p, plugin_page %p)", action, plugin_page);

    g_return_if_fail(GNC_IS_PLUGIN_PAGE_REGISTER2 (plugin_page));

    priv = GNC_PLUGIN_PAGE_REGISTER2_GET_PRIVATE (plugin_page);
    view = gnc_ledger_display2_get_split_view_register (priv->ledger);

    split = gnc_tree_view_split_reg_get_current_split (view);
    if (!split)
    {
        LEAVE("split is NULL");
        return;
    }

    query = qof_query_create_for(GNC_ID_SPLIT);

    qof_query_set_book (query, gnc_get_current_book ());

    xaccQueryAddGUIDMatch (query, xaccSplitGetGUID (split),
                           GNC_ID_SPLIT, QOF_QUERY_AND);

    window = GNC_MAIN_WINDOW(GNC_PLUGIN_PAGE(plugin_page)->window);
    id = report_helper (priv->ledger, split, query);
    if (id >= 0)
        gnc_main_window_open_report(id, window);
    LEAVE(" ");
}

/*#################################################################################*/
/*#################################################################################*/

/************************************************************/
/*                    Auxiliary functions                   */
/************************************************************/

void
gnc_plugin_page_register2_set_options (GncPluginPage *plugin_page,
                                      gint lines_default,
                                      gboolean read_only)
{
    GncPluginPageRegister2 *page;
    GncPluginPageRegister2Private *priv;

    g_return_if_fail(GNC_IS_PLUGIN_PAGE_REGISTER2(plugin_page));

    page = GNC_PLUGIN_PAGE_REGISTER2 (plugin_page);
    priv = GNC_PLUGIN_PAGE_REGISTER2_GET_PRIVATE(page);
    priv->lines_default     = lines_default;
    priv->read_only         = read_only;
}

GNCSplitReg2 *
gnc_plugin_page_register2_get_gsr (GncPluginPage *plugin_page)
{
    GncPluginPageRegister2 *page;
    GncPluginPageRegister2Private *priv;

    g_return_val_if_fail(GNC_IS_PLUGIN_PAGE_REGISTER2(plugin_page), NULL);

    page = GNC_PLUGIN_PAGE_REGISTER2 (plugin_page);
    priv = GNC_PLUGIN_PAGE_REGISTER2_GET_PRIVATE(page);

    return priv->gsr;
}


GNCLedgerDisplay2 *
gnc_plugin_page_register2_get_ledger (GncPluginPage *plugin_page)
{
    GncPluginPageRegister2 *page;
    GncPluginPageRegister2Private *priv;

    g_return_val_if_fail(GNC_IS_PLUGIN_PAGE_REGISTER2(plugin_page), NULL);

    page = GNC_PLUGIN_PAGE_REGISTER2 (plugin_page);
    priv = GNC_PLUGIN_PAGE_REGISTER2_GET_PRIVATE(page);

    return priv->ledger;
}
static void
gnc_plugin_page_help_changed_cb (GNCSplitReg2 *gsr, GncPluginPageRegister2 *register_page) //this works
{
    GncPluginPageRegister2Private *priv;
    GncTreeViewSplitReg *view;
    GncWindow *window;
    char *help;

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER2 (register_page));

    window = GNC_WINDOW (GNC_PLUGIN_PAGE (register_page)->window);
    if (!window)
    {
        // This routine can be called before the page is added to a
        // window.
        return;
    }
    /* Get the text from the view */
    priv = GNC_PLUGIN_PAGE_REGISTER2_GET_PRIVATE (register_page);
    view = gnc_ledger_display2_get_split_view_register (priv->ledger);
    help = g_strdup (view->help_text);
    gnc_window_set_status (window, GNC_PLUGIN_PAGE (register_page), help);
    g_free (help);
}

static void
gnc_plugin_page_register2_refresh_cb (GHashTable *changes, gpointer user_data) //this works
{
    GncPluginPageRegister2 *page = user_data;
    GncPluginPageRegister2Private *priv;
    GncTreeViewSplitReg *view;

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REGISTER2 (page));
    priv = GNC_PLUGIN_PAGE_REGISTER2_GET_PRIVATE (page);
    view = gnc_ledger_display2_get_split_view_register (priv->ledger);

    if (changes)
    {
        const EventInfo* ei;

        ei = gnc_gui_get_entity_events (changes, &priv->key);
        if (ei)
        {
            if (ei->event_mask & QOF_EVENT_DESTROY)
            {
                /* Account has been deleted, close plugin page
                 * but prevent that action from writing state information
                 * for this deleted account
                 */
                g_object_set (G_OBJECT (view), "state-section", NULL, NULL);
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
        /* Force updates */
        gnc_tree_view_split_reg_refresh_from_prefs (view);
    }
    gnc_plugin_page_register2_ui_update (NULL, page);
}

static void
gnc_plugin_page_register2_close_cb (gpointer user_data)
{
    GncPluginPage *plugin_page = GNC_PLUGIN_PAGE(user_data);
    gnc_main_window_close_page (plugin_page);
}

/** This function is called when an account has been edited and an
 *  "extreme" change has been made to it.  (E.G. Changing from a
 *  credit card account to an expense account.  This rouine is
 *  responsible for finding all open registers containing the account
 *  and closing them.
 *
 *  @param account A pointer to the account that was changed.
 */
static void
gppr_account_destroy_cb (Account *account)
{
    GncPluginPageRegister2 *page;
    GncPluginPageRegister2Private *priv;
    GNCLedgerDisplay2Type ledger_type;
    const GncGUID *acct_guid;
    const GList *citem;
    GList *item, *kill = NULL;

    acct_guid = xaccAccountGetGUID(account);

    /* Find all windows that need to be killed.  Don't kill them yet, as
     * that would affect the list being walked.*/
    citem = gnc_gobject_tracking_get_list(GNC_PLUGIN_PAGE_REGISTER2_NAME);
    for ( ; citem; citem = g_list_next(citem))
    {
        page = (GncPluginPageRegister2 *)citem->data;
        priv = GNC_PLUGIN_PAGE_REGISTER2_GET_PRIVATE(page);
        ledger_type = gnc_ledger_display2_type (priv->ledger);
        if (ledger_type == LD2_GL)
        {
            kill = g_list_append(kill, page);
            /* kill it */
        }
        else if ((ledger_type == LD2_SINGLE) || (ledger_type == LD2_SUBACCOUNT))
        {
            if (guid_compare(acct_guid, &priv->key) == 0)
            {
                kill = g_list_append(kill, page);
            }
        }
    }

    /* Now kill them. */
    for (item = kill; item; item = g_list_next(item))
    {
        page = (GncPluginPageRegister2 *)item->data;
        gnc_main_window_close_page(GNC_PLUGIN_PAGE(page));
    }
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
gnc_plugin_page_register2_event_handler (QofInstance *entity,
                                        QofEventId event_type,
                                        GncPluginPageRegister2 *page,
                                        GncEventData *ed)
{
    Transaction *trans;
    QofBook *book;
    GncPluginPage *visible_page;
    GtkWidget *window;
    gchar *label, *color;

    g_return_if_fail(page);	/* Required */
    if (!GNC_IS_TRANS(entity) && !GNC_IS_ACCOUNT(entity))
        return;

    ENTER("entity %p of type %d, page %p, event data %p",
          entity, event_type, page, ed);

    window = gnc_plugin_page_get_window(GNC_PLUGIN_PAGE(page));

    if (GNC_IS_ACCOUNT(entity))
    {
        if (GNC_IS_MAIN_WINDOW(window))
        {
            label = gnc_plugin_page_register2_get_tab_name(GNC_PLUGIN_PAGE(page));
            main_window_update_page_name(GNC_PLUGIN_PAGE(page), label);
            color = gnc_plugin_page_register2_get_tab_color(GNC_PLUGIN_PAGE(page));
            main_window_update_page_color(GNC_PLUGIN_PAGE(page), color);
            g_free(color);
            g_free(label);
        }
        LEAVE("tab name updated");
        return;
    }

    if (!(event_type & (QOF_EVENT_MODIFY | QOF_EVENT_DESTROY)))
    {
        LEAVE("not a modify");
        return;
    }
    trans = GNC_TRANS(entity);
    book = qof_instance_get_book(QOF_INSTANCE(trans));
    if (!gnc_plugin_page_has_book(GNC_PLUGIN_PAGE(page), book))
    {
        LEAVE("not in this book");
        return;
    }

    if (GNC_IS_MAIN_WINDOW(window))
    {
        visible_page = gnc_main_window_get_current_page(GNC_MAIN_WINDOW(window));
        if (visible_page != GNC_PLUGIN_PAGE(page))
        {
            LEAVE("page not visible");
            return;
        }
    }

    gnc_plugin_page_register2_ui_update(NULL, page);
    LEAVE(" ");
    return;
}


/** @} */
/** @} */
