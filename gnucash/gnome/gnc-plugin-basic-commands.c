/*
 * gnc-plugin-basic-commands.c --
 *
 * Copyright (C) 2005 David Hampton <hampton@employees.org>
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

/** @addtogroup MenuPlugins
    @{ */
/** @addtogroup GncPluginAccountTree An Account Tree Plugin
    @{ */
/** @file gnc-plugin-basic-commands.c
    @brief Functions providing a basic set of menu items.
    @author Copyright (C) 2005 David Hampton <hampton@employees.org>
*/

#include <config.h>

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include <string.h>

#include "gnc-plugin-basic-commands.h"
#include "gnc-ui-util.h"

#include "dialog-book-close.h"
#include "dialog-file-access.h"
#include "dialog-fincalc.h"
#include "dialog-find-transactions.h"
#include "dialog-find-transactions2.h"
#include "dialog-imap-editor.h"
#include "dialog-sx-since-last-run.h"
#include "dialog-totd.h"
#include "dialog-trans-assoc.h"
#include "assistant-acct-period.h"
#include "assistant-loan.h"
#include "gnc-engine.h"
#include "gnc-file.h"
#include "gnc-gui-query.h"
#include "gnc-main-window.h"
#include "gnc-ui.h"
#include "gnc-window.h"
#include "gnc-session.h"
#include "gnc-plugin-page-sx-list.h"
#include "gnc-plugin-file-history.h"

/* This static indicates the debugging module that this .o belongs to.  */
G_GNUC_UNUSED static QofLogModule log_module = GNC_MOD_GUI;

static void gnc_plugin_basic_commands_class_init (GncPluginBasicCommandsClass *klass);
static void gnc_plugin_basic_commands_init (GncPluginBasicCommands *plugin);
static void gnc_plugin_basic_commands_finalize (GObject *object);

static void gnc_plugin_basic_commands_add_to_window (GncPlugin *plugin, GncMainWindow *window, GQuark type);
static void gnc_plugin_basic_commands_main_window_page_changed(GncMainWindow *window, GncPluginPage *page, gpointer user_data);

/* Command callbacks */
static void gnc_main_window_cmd_file_new (GtkAction *action, GncMainWindowActionData *data);
static void gnc_main_window_cmd_file_open (GtkAction *action, GncMainWindowActionData *data);
static void gnc_main_window_cmd_file_save (GtkAction *action, GncMainWindowActionData *data);
static void gnc_main_window_cmd_file_save_as (GtkAction *action, GncMainWindowActionData *data);
static void gnc_main_window_cmd_file_revert (GtkAction *action, GncMainWindowActionData *data);
static void gnc_main_window_cmd_file_export_accounts (GtkAction *action, GncMainWindowActionData *data);
static void gnc_main_window_cmd_edit_tax_options (GtkAction *action, GncMainWindowActionData *data);
static void gnc_main_window_cmd_actions_mortgage_loan (GtkAction *action, GncMainWindowActionData *data);
static void gnc_main_window_cmd_actions_scheduled_transaction_editor (GtkAction *action, GncMainWindowActionData *data);
static void gnc_main_window_cmd_actions_since_last_run (GtkAction *action, GncMainWindowActionData *data);

#if CLOSE_BOOKS_ACTUALLY_WORKS
static void gnc_main_window_cmd_actions_close_books (GtkAction *action, GncMainWindowActionData *data);
#endif /* CLOSE_BOOKS_ACTUALLY_WORKS */

static void gnc_main_window_cmd_tools_financial_calculator (GtkAction *action, GncMainWindowActionData *data);
static void gnc_main_window_cmd_tools_close_book (GtkAction *action, GncMainWindowActionData *data);
static void gnc_main_window_cmd_tools_find_transactions (GtkAction *action, GncMainWindowActionData *data);
static void gnc_main_window_cmd_tools_price_editor (GtkAction *action, GncMainWindowActionData *data);
static void gnc_main_window_cmd_tools_imap_editor (GtkAction *action, GncMainWindowActionData *data);
static void gnc_main_window_cmd_tools_trans_assoc (GtkAction *action, GncMainWindowActionData *data);
static void gnc_main_window_cmd_tools_commodity_editor (GtkAction *action, GncMainWindowActionData *data);
static void gnc_main_window_cmd_help_totd (GtkAction *action, GncMainWindowActionData *data);



#define PLUGIN_ACTIONS_NAME "gnc-plugin-basic-commands-actions"
#define PLUGIN_UI_FILENAME  "gnc-plugin-basic-commands-ui.xml"

/** An array of all of the actions provided by the basic commands
 *  plugin. */
static GtkActionEntry gnc_plugin_actions [] =
{

    /* File menu */

    {
        "FileNewAction", "document-new", N_("New _File"), "<primary>n",
        N_("Create a new file"),
        G_CALLBACK (gnc_main_window_cmd_file_new)
    },
    {
        "FileOpenAction", "document-open", N_("_Open..."), "<primary>o",
        N_("Open an existing GnuCash file"),
        G_CALLBACK (gnc_main_window_cmd_file_open)
    },
    {
        "FileSaveAction", "document-save", N_("_Save"), "<primary>s",
        N_("Save the current file"),
        G_CALLBACK (gnc_main_window_cmd_file_save)
    },
    {
        "FileSaveAsAction", "document-save-as", N_("Save _As..."), "<shift><primary>s",
        N_("Save this file with a different name"),
        G_CALLBACK (gnc_main_window_cmd_file_save_as)
    },
    {
        "FileRevertAction", "document-revert", N_("Re_vert"), NULL,
        N_("Reload the current database, reverting all unsaved changes"),
        G_CALLBACK (gnc_main_window_cmd_file_revert)
    },
    {
        "FileExportAccountsAction", "go-next",
        N_("Export _Accounts"), NULL,
        N_("Export the account hierarchy to a new GnuCash datafile"),
        G_CALLBACK (gnc_main_window_cmd_file_export_accounts)
    },

    /* Edit menu */

    {
        "EditFindTransactionsAction", "edit-find", N_("_Find..."), "<primary>f",
        N_("Find transactions with a search"),
        G_CALLBACK (gnc_main_window_cmd_tools_find_transactions)
    },
    {
        "EditTaxOptionsAction", NULL,
        /* Translators: remember to reuse this *
         * translation in dialog-account.glade */
        N_("Ta_x Report Options"), NULL,
        /* Translators: currently implemented are *
         * US: income tax and                     *
         * DE: VAT                                *
         * So adjust this string                  */
        N_("Setup relevant accounts for tax reports, e.g. US income tax"),
        G_CALLBACK (gnc_main_window_cmd_edit_tax_options)
    },

    /* Actions menu */

    { "ActionsScheduledTransactionsAction", NULL, N_("_Scheduled Transactions"), NULL, NULL, NULL },
    {
        "ActionsScheduledTransactionEditorAction", NULL, N_("_Scheduled Transaction Editor"), NULL,
        N_("The list of Scheduled Transactions"),
        G_CALLBACK (gnc_main_window_cmd_actions_scheduled_transaction_editor)
    },
    {
        "ActionsSinceLastRunAction", NULL, N_("Since _Last Run..."), NULL,
        N_("Create Scheduled Transactions since the last time run"),
        G_CALLBACK (gnc_main_window_cmd_actions_since_last_run)
    },
    {
        "ActionsMortgageLoanAction", NULL, N_("_Mortgage & Loan Repayment..."), NULL,
        N_("Setup scheduled transactions for repayment of a loan"),
        G_CALLBACK (gnc_main_window_cmd_actions_mortgage_loan)
    },
    { "ActionsBudgetAction", NULL, N_("B_udget"), NULL, NULL, NULL },
#ifdef CLOSE_BOOKS_ACTUALLY_WORKS
    {
        "ActionsCloseBooksAction", NULL, N_("Close _Books"), NULL,
        N_("Archive old data using accounting periods"),
        G_CALLBACK (gnc_main_window_cmd_actions_close_books)
    },
#endif // CLOSE_BOOKS_ACTUALLY_WORKS

    /* Tools menu */
    {
        "ToolsPriceEditorAction", NULL, N_("_Price Database"), NULL,
        N_("View and edit the prices for stocks and mutual funds"),
        G_CALLBACK (gnc_main_window_cmd_tools_price_editor)
    },
    {
        "ToolsCommodityEditorAction", NULL, N_("_Security Editor"), NULL,
        N_("View and edit the commodities for stocks and mutual funds"),
        G_CALLBACK (gnc_main_window_cmd_tools_commodity_editor)
    },
    {
        "ToolsFinancialCalculatorAction", NULL, N_("_Loan Repayment Calculator"), NULL,
        N_("Use the loan/mortgage repayment calculator"),
        G_CALLBACK (gnc_main_window_cmd_tools_financial_calculator)
    },
    {
        "ToolsBookCloseAction", NULL, N_("_Close Book"), NULL,
        N_("Close the Book at the end of the Period"),
        G_CALLBACK (gnc_main_window_cmd_tools_close_book)
    },
    {
        "ToolsImapEditorAction", NULL, N_("_Import Map Editor"), NULL,
        N_("View and Delete Bayesian and Non Bayesian information"),
        G_CALLBACK (gnc_main_window_cmd_tools_imap_editor)
    },
    {
        "ToolsTransAssocAction", NULL, N_("_Transaction Associations"), NULL,
        N_("View all Transaction Associations"),
        G_CALLBACK (gnc_main_window_cmd_tools_trans_assoc)
    },

    /* Help menu */

    {
        "HelpTipsOfTheDayAction", NULL, N_("_Tips Of The Day"), NULL,
        N_("View the Tips of the Day"),
        G_CALLBACK (gnc_main_window_cmd_help_totd)
    },
};
/** The number of actions provided by this plugin. */
static guint gnc_plugin_n_actions = G_N_ELEMENTS (gnc_plugin_actions);


/** These are the "important" actions provided by the basic commands
 *  plugin.  Their labels will appear when the toolbar is set to
 *  "Icons and important text" (e.g. GTK_TOOLBAR_BOTH_HORIZ) mode. */
static const gchar *gnc_plugin_important_actions[] =
{
    "FileSaveAction",
    NULL,
};

/** These actions are made not sensitive (i.e.,
 * their toolbar and menu items are grayed out and do not send events
 * when clicked) when the current book is "Read Only".
 */
static const gchar *readwrite_only_active_actions[] =
{
    "ToolsBookCloseAction",
    NULL
};

/** These actions are made not sensitive (i.e.,
 * their toolbar and menu items are grayed out and do not send events
 * when clicked) when the current book is not dirty. As a read only book
 * can't be dirty this implies they will be disabled for a read only book
 * as well.
 */
static const gchar *dirty_only_active_actions[] =
{
    "FileSaveAction",
    "FileRevertAction",
    NULL
};

/** The instance private data structure for an basic commands
 *  plugin. */
typedef struct GncPluginBasicCommandsPrivate
{
    gpointer dummy;
} GncPluginBasicCommandsPrivate;

#define GNC_PLUGIN_BASIC_COMMANDS_GET_PRIVATE(o)  \
   (G_TYPE_INSTANCE_GET_PRIVATE ((o), GNC_TYPE_PLUGIN_BASIC_COMMANDS, GncPluginBasicCommandsPrivate))

/** A pointer to the parent class of a plugin page. */
static GObjectClass *parent_class = NULL;


/*  Get the type of the basic commands menu plugin. */
GType
gnc_plugin_basic_commands_get_type (void)
{
    static GType gnc_plugin_basic_commands_type = 0;

    if (gnc_plugin_basic_commands_type == 0)
    {
        static const GTypeInfo our_info =
        {
            sizeof (GncPluginBasicCommandsClass),
            NULL,		/* base_init */
            NULL,		/* base_finalize */
            (GClassInitFunc) gnc_plugin_basic_commands_class_init,
            NULL,		/* class_finalize */
            NULL,		/* class_data */
            sizeof (GncPluginBasicCommands),
            0,		/* n_preallocs */
            (GInstanceInitFunc) gnc_plugin_basic_commands_init
        };

        gnc_plugin_basic_commands_type = g_type_register_static (GNC_TYPE_PLUGIN,
                                         "GncPluginBasicCommands",
                                         &our_info, 0);
    }

    return gnc_plugin_basic_commands_type;
}


/** Create a new basic commands menu plugin. */
GncPlugin *
gnc_plugin_basic_commands_new (void)
{
    GncPluginBasicCommands *plugin;

    /* We just need to mention it, so the GType is registered and will be
     * reflected during plugin-page restore. */
    GNC_TYPE_PLUGIN_PAGE_SX_LIST;

    plugin = g_object_new (GNC_TYPE_PLUGIN_BASIC_COMMANDS, NULL);

    return GNC_PLUGIN (plugin);
}

/** Initialize the basic commands menu for a window.  This function is
 *  called as part of the initialization of a window, after all the
 *  plugin menu items have been added to the menu structure.  Its job
 *  is to correctly initialize the basic commands menu,  It does this by
 *  hiding the Database Connection menu item if database support has not
 *  been included in the build.
 *
 *  @param plugin A pointer to the gnc-plugin object responsible for
 *  adding/removing the basic commands menu.
 *
 *  @param window A pointer the gnc-main-window that is being initialized.
 *
 *  @param type Unused
 */
static void
gnc_plugin_basic_commands_add_to_window (GncPlugin *plugin,
        GncMainWindow *window,
        GQuark type)
{
    g_signal_connect(window, "page_changed",
                     G_CALLBACK(gnc_plugin_basic_commands_main_window_page_changed),
                     plugin);
}

/** Update the actions sensitivity
*/
static void update_inactive_actions(GncPluginPage *plugin_page)
{
    GncMainWindow  *window;
    GtkActionGroup *action_group;

    // We are readonly - so we have to switch particular actions to inactive.
    gboolean is_readwrite = !qof_book_is_readonly(gnc_get_current_book());
    gboolean is_dirty = qof_book_session_not_saved (gnc_get_current_book ());

    // We continue only if the current page is a plugin page
    if (!plugin_page || !GNC_IS_PLUGIN_PAGE(plugin_page))
        return;

    window = GNC_MAIN_WINDOW(plugin_page->window);
    g_return_if_fail(GNC_IS_MAIN_WINDOW(window));
    action_group = gnc_main_window_get_action_group(window, PLUGIN_ACTIONS_NAME);
    g_return_if_fail(GTK_IS_ACTION_GROUP(action_group));

    /* Set the action's sensitivity */
    gnc_plugin_update_actions (action_group, readwrite_only_active_actions,
                               "sensitive", is_readwrite);
    gnc_plugin_update_actions (action_group, dirty_only_active_actions,
                               "sensitive", is_dirty);
}

static void
gnc_plugin_basic_commands_main_window_page_changed(GncMainWindow *window,
        GncPluginPage *page,
        gpointer user_data)
{
    /* Make sure not to call this with a NULL GncPluginPage */
    if (page)
    {
        // Update the action sensitivity due to read-only
        update_inactive_actions(page);
    }
}

/** Initialize the class for a new basic commands plugin.  This will
 *  set up any function pointers that override functions in the parent
 *  class, and also configure the private data storage for this
 *  widget.
 *
 *  @param klass The new class structure created by the object system.
 */
static void
gnc_plugin_basic_commands_class_init (GncPluginBasicCommandsClass *klass)
{
    GObjectClass *object_class = G_OBJECT_CLASS (klass);
    GncPluginClass *plugin_class = GNC_PLUGIN_CLASS (klass);

    parent_class = g_type_class_peek_parent (klass);

    object_class->finalize = gnc_plugin_basic_commands_finalize;

    /* plugin info */
    plugin_class->plugin_name  = GNC_PLUGIN_BASIC_COMMANDS_NAME;

    /* function overrides */
    plugin_class->add_to_window = gnc_plugin_basic_commands_add_to_window;

    /* widget addition/removal */
    plugin_class->actions_name 	  = PLUGIN_ACTIONS_NAME;
    plugin_class->actions      	  = gnc_plugin_actions;
    plugin_class->n_actions    	  = gnc_plugin_n_actions;
    plugin_class->important_actions = gnc_plugin_important_actions;
    plugin_class->ui_filename       = PLUGIN_UI_FILENAME;

    g_type_class_add_private(klass, sizeof(GncPluginBasicCommandsPrivate));
}


/** Initialize a new instance of a basic commands plugin.  This
 *  function currently does nothing.
 *
 *  @param plugin The new object instance created by the object
 *  system. */
static void
gnc_plugin_basic_commands_init (GncPluginBasicCommands *plugin)
{
}


/** Finalize the basic commands plugin object.  This function is
 *  called from the G_Object level to complete the destruction of the
 *  object.  It should release any memory not previously released by
 *  the destroy function (i.e. the private data structure), then chain
 *  up to the parent's destroy function.  This function currently does
 *  nothing.
 *
 *  @param object The object being destroyed. */
static void
gnc_plugin_basic_commands_finalize (GObject *object)
{
    g_return_if_fail (GNC_IS_PLUGIN_BASIC_COMMANDS (object));

    G_OBJECT_CLASS (parent_class)->finalize (object);
}

/************************************************************
 *                    Command Callbacks                     *
 ************************************************************/

static void
gnc_main_window_cmd_file_new (GtkAction *action, GncMainWindowActionData *data)
{
    if (!gnc_main_window_all_finish_pending())
        return;

    gnc_file_new (GTK_WINDOW (data->window));
}

static void
gnc_main_window_cmd_file_open (GtkAction *action, GncMainWindowActionData *data)
{
    g_return_if_fail (data != NULL);

    if (!gnc_main_window_all_finish_pending())
        return;

    gnc_window_set_progressbar_window (GNC_WINDOW(data->window));
#ifdef HAVE_DBI_DBI_H
    gnc_ui_file_access_for_open (GTK_WINDOW (data->window));
#else
    gnc_file_open (GTK_WINDOW (data->window));
#endif
    gnc_window_set_progressbar_window (NULL);
}

static void
gnc_main_window_cmd_file_save (GtkAction *action, GncMainWindowActionData *data)
{
    g_return_if_fail (data != NULL);

    if (!gnc_main_window_all_finish_pending())
        return;

    gnc_window_set_progressbar_window (GNC_WINDOW(data->window));
    gnc_file_save (GTK_WINDOW (data->window));
    gnc_window_set_progressbar_window (NULL);
}

static void
gnc_main_window_cmd_file_save_as (GtkAction *action, GncMainWindowActionData *data)
{
    g_return_if_fail (data != NULL);

    if (!gnc_main_window_all_finish_pending())
        return;

    gnc_window_set_progressbar_window (GNC_WINDOW(data->window));
#ifdef HAVE_DBI_DBI_H
    gnc_ui_file_access_for_save_as (GTK_WINDOW (data->window));
#else
    gnc_file_save_as (GTK_WINDOW (data->window));
#endif
    gnc_window_set_progressbar_window (NULL);
}

static void
gnc_main_window_cmd_file_revert (GtkAction *action, GncMainWindowActionData *data)
{
    g_return_if_fail (data != NULL);

    if (!gnc_main_window_all_finish_pending())
        return;

    gnc_window_set_progressbar_window (GNC_WINDOW(data->window));
    gnc_file_revert(GTK_WINDOW (data->window));
    gnc_window_set_progressbar_window (NULL);
}

static void
gnc_main_window_cmd_file_export_accounts (GtkAction *action, GncMainWindowActionData *data)
{
    g_return_if_fail (data != NULL);

    gnc_window_set_progressbar_window (GNC_WINDOW(data->window));
#ifdef HAVE_DBI_DBI_H
    gnc_ui_file_access_for_export (GTK_WINDOW (data->window));
#else
    gnc_file_export (GTK_WINDOW (data->window));
#endif
    gnc_window_set_progressbar_window (NULL);
}

static void
gnc_main_window_cmd_edit_tax_options (GtkAction *action, GncMainWindowActionData *data)
{
    g_return_if_fail (data != NULL);

    gnc_tax_info_dialog (GTK_WIDGET (data->window));
}

static void
gnc_main_window_cmd_actions_scheduled_transaction_editor (GtkAction *action, GncMainWindowActionData *data)
{
    GncPluginPage *page = gnc_plugin_page_sx_list_new();
    gnc_main_window_open_page(NULL, page);
}

static void
gnc_main_window_cmd_actions_since_last_run (GtkAction *action, GncMainWindowActionData *data)
{
    GtkWindow *window;
    GncSxInstanceModel *sx_instances;
    GncSxSummary summary;
    GList *auto_created_txns = NULL;
    const char *nothing_to_do_msg =
        _( "There are no Scheduled Transactions to be entered at this time." );

    g_return_if_fail (data != NULL);

    window = GTK_WINDOW (data->window);

    if (qof_book_is_readonly(gnc_get_current_book()))
    {
        /* Is the book read-only? Then don't change anything here. */
        return;
    }

    sx_instances = gnc_sx_get_current_instances();
    gnc_sx_instance_model_summarize(sx_instances, &summary);
    gnc_sx_instance_model_effect_change(sx_instances, TRUE, &auto_created_txns, NULL);
    if (summary.need_dialog)
    {
        gnc_ui_sx_since_last_run_dialog (window, sx_instances, auto_created_txns);
        auto_created_txns = NULL;
    }
    else
    {
        if (summary.num_auto_create_no_notify_instances == 0)
        {
            gnc_info_dialog (window, "%s", nothing_to_do_msg);
        }
        else
        {
            gnc_info_dialog(window, ngettext
                            /* Translators: %d is the number of transactions. This is a
                               ngettext(3) message. */
                            ("There are no Scheduled Transactions to be entered at this time. "
                             "(%d transaction automatically created)",
                             "There are no Scheduled Transactions to be entered at this time. "
                             "(%d transactions automatically created)",
                             summary.num_auto_create_no_notify_instances),
                            summary.num_auto_create_no_notify_instances);
        }
    }
    g_list_free(auto_created_txns);
    g_object_unref(G_OBJECT(sx_instances));
}

static void
gnc_main_window_cmd_actions_mortgage_loan (GtkAction *action, GncMainWindowActionData *data)
{
    gnc_ui_sx_loan_assistant_create ();
}
#ifdef CLOSE_BOOKS_ACTUALLY_WORKS
static void
gnc_main_window_cmd_actions_close_books (GtkAction *action, GncMainWindowActionData *data)
{
    gnc_acct_period_dialog();
}
#endif /* CLOSE_BOOKS_ACTUALLY_WORKS */

static void
gnc_main_window_cmd_tools_imap_editor (GtkAction *action, GncMainWindowActionData *data)
{
    gnc_set_busy_cursor(NULL, TRUE);
    gnc_imap_dialog (GTK_WIDGET (data->window));
    gnc_unset_busy_cursor(NULL);
}

static void
gnc_main_window_cmd_tools_trans_assoc (GtkAction *action, GncMainWindowActionData *data)
{
    gnc_set_busy_cursor (NULL, TRUE);
    gnc_trans_assoc_dialog (GTK_WINDOW (data->window));
    gnc_unset_busy_cursor (NULL);
}

static void
gnc_main_window_cmd_tools_price_editor (GtkAction *action, GncMainWindowActionData *data)
{
    gnc_set_busy_cursor(NULL, TRUE);
    gnc_prices_dialog (GTK_WIDGET (data->window));
    gnc_unset_busy_cursor(NULL);
}

static void
gnc_main_window_cmd_tools_commodity_editor (GtkAction *action, GncMainWindowActionData *data)
{
    gnc_set_busy_cursor(NULL, TRUE);
    gnc_commodities_dialog (GTK_WIDGET (data->window));
    gnc_unset_busy_cursor(NULL);
}

static void
gnc_main_window_cmd_tools_financial_calculator (GtkAction *action, GncMainWindowActionData *data)
{
    gnc_ui_fincalc_dialog_create(GTK_WINDOW (data->window));
}

static void
gnc_main_window_cmd_tools_close_book (GtkAction *action, GncMainWindowActionData *data)
{
    gnc_ui_close_book(gnc_get_current_book());
}

static void
gnc_main_window_cmd_tools_find_transactions (GtkAction *action, GncMainWindowActionData *data)
{

#ifdef REGISTER2_ENABLED
    /*################## Added for Reg2 #################*/
    gnc_ui_find_transactions_dialog_create2 (NULL);
    /*################## Added for Reg2 #################*/
#else
    gnc_ui_find_transactions_dialog_create (GTK_WINDOW(data->window), NULL);
#endif
}

static void
gnc_main_window_cmd_help_totd (GtkAction *action, GncMainWindowActionData *data)
{
    g_return_if_fail (data != NULL);

    gnc_totd_dialog(GTK_WINDOW(data->window), FALSE);
}

/** @} */
/** @} */
