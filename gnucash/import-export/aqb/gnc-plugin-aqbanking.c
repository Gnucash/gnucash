/*
 * gnc-plugin-aqbanking.c --
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

/**
 * @internal
 * @file gnc-plugin-aqbanking.c
 * @brief Plugin registration of the AqBanking module
 * @author Copyright (C) 2002 Christian Stimming <stimming@tuhh.de>
 * @author Copyright (C) 2003 David Hampton <hampton@employees.org>
 * @author Copyright (C) 2008 Andreas Koehler <andi5.py@gmx.net>
 */

#include <config.h>

#include <glib/gi18n.h>

#include "Account.h"
#include "dialog-ab-trans.h"
#include "assistant-ab-initial.h"
#include "gnc-ab-getbalance.h"
#include "gnc-ab-gettrans.h"
#include "gnc-ab-transfer.h"
#include "gnc-ab-utils.h"
#include "gnc-ab-kvp.h"
#include "gnc-gwen-gui.h"
#include "gnc-file-aqb-import.h"
#include "gnc-plugin-aqbanking.h"
#include "gnc-plugin-manager.h"
#include "gnc-plugin-page-account-tree.h"
#include "gnc-plugin-page-register.h"
#include "gnc-plugin-page-register2.h"
#include "gnc-main-window.h"
#include "gnc-prefs.h"
#include "gnc-ui-util.h" // for gnc_get_current_book

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = G_LOG_DOMAIN;

static void gnc_plugin_aqbanking_class_init(GncPluginAqBankingClass *klass);
static void gnc_plugin_aqbanking_init(GncPluginAqBanking *plugin);
static void gnc_plugin_aqbanking_add_to_window(GncPlugin *plugin, GncMainWindow *window, GQuark type);
static void gnc_plugin_aqbanking_remove_from_window(GncPlugin *plugin, GncMainWindow *window, GQuark type);

/* Object callbacks */
static void gnc_plugin_ab_main_window_page_added(GncMainWindow *window, GncPluginPage *page, gpointer user_data);
static void gnc_plugin_ab_main_window_page_changed(GncMainWindow *window, GncPluginPage *page, gpointer user_data);
static void gnc_plugin_ab_account_selected(GncPluginPage *plugin_page, Account *account, gpointer user_data);

/* Auxiliary functions */
static Account *main_window_to_account(GncMainWindow *window);

/* Command callbacks */
static void gnc_plugin_ab_cmd_setup(GtkAction *action, GncMainWindowActionData *data);
static void gnc_plugin_ab_cmd_get_balance(GtkAction *action, GncMainWindowActionData *data);
static void gnc_plugin_ab_cmd_get_transactions(GtkAction *action, GncMainWindowActionData *data);
static void gnc_plugin_ab_cmd_issue_sepatransaction(GtkAction *action, GncMainWindowActionData *data);
static void gnc_plugin_ab_cmd_issue_sepainternaltransaction(GtkAction *action, GncMainWindowActionData *data);
static void gnc_plugin_ab_cmd_issue_inttransaction(GtkAction *action, GncMainWindowActionData *data);
static void gnc_plugin_ab_cmd_issue_sepa_direct_debit(GtkAction *action, GncMainWindowActionData *data);
static void gnc_plugin_ab_cmd_view_logwindow(GtkToggleAction *action, GncMainWindow *window);
static void gnc_plugin_ab_cmd_mt940_import(GtkAction *action, GncMainWindowActionData *data);
static void gnc_plugin_ab_cmd_mt942_import(GtkAction *action, GncMainWindowActionData *data);
static void gnc_plugin_ab_cmd_dtaus_import(GtkAction *action, GncMainWindowActionData *data);
static void gnc_plugin_ab_cmd_dtaus_importsend(GtkAction *action, GncMainWindowActionData *data);

#define PLUGIN_ACTIONS_NAME "gnc-plugin-aqbanking-actions"
#define PLUGIN_UI_FILENAME  "gnc-plugin-aqbanking-ui.xml"

#define MENU_TOGGLE_ACTION_AB_VIEW_LOGWINDOW "ABViewLogwindowAction"


static GtkActionEntry gnc_plugin_actions [] =
{
    /* Menus */
    { "OnlineActionsAction", NULL, N_("_Online Actions"), NULL, NULL, NULL },

    /* Menu Items */
    {
        "ABSetupAction", NULL, N_("_Online Banking Setup..."), NULL,
        N_("Initial setup of Online Banking access (HBCI, or OFX DirectConnect, using AqBanking)"),
        G_CALLBACK(gnc_plugin_ab_cmd_setup)
    },
    {
        "ABGetBalanceAction", NULL, N_("Get _Balance"), NULL,
        N_("Get the account balance online through Online Banking"),
        G_CALLBACK(gnc_plugin_ab_cmd_get_balance)
    },
    {
        "ABGetTransAction", NULL, N_("Get _Transactions..."), NULL,
        N_("Get the transactions online through Online Banking"),
        G_CALLBACK(gnc_plugin_ab_cmd_get_transactions)
    },
    {
        "ABIssueSepaTransAction", NULL,
		/* Translators: https://en.wikipedia.org/wiki/Single_Euro_Payments_Area */
		N_("Issue _SEPA Transaction..."), NULL,
        N_("Issue a new international European (SEPA) transaction online through Online Banking"),
        G_CALLBACK(gnc_plugin_ab_cmd_issue_sepatransaction)
    },
    {
        "ABIssueSepaIntTransAction", NULL,
        N_("Issue SEPA I_nternal Transaction..."), NULL,
        N_("Issue a new internal European (SEPA) transaction online through Online Banking"),
        G_CALLBACK(gnc_plugin_ab_cmd_issue_sepainternaltransaction)
    },
    {
        "ABIssueIntTransAction", NULL, N_("_Internal Transaction..."), NULL,
        N_("Issue a new bank-internal transaction online through Online Banking"),
        G_CALLBACK(gnc_plugin_ab_cmd_issue_inttransaction)
    },
    {
        "ABIssueSepaDirectDebitAction", NULL, N_("Issue SEPA Direct _Debit..."), NULL,
        N_("Issue a new international European (SEPA) direct debit note online through Online Banking"),
        G_CALLBACK(gnc_plugin_ab_cmd_issue_sepa_direct_debit)
    },

    /* File -> Import menu item */
    {
        "Mt940ImportAction", "go-previous",
		/* Translators: Message types MTxxxx are exchange formats used by the SWIFT network
		   https://en.wikipedia.org/wiki/Society_for_Worldwide_Interbank_Financial_Telecommunication */
		N_("Import _MT940"), NULL,
        N_("Import an end-of-day account statement in SWIFT MT940 format into GnuCash."),
        G_CALLBACK(gnc_plugin_ab_cmd_mt940_import)
    },
    {
        "Mt942ImportAction", "go-previous", N_("Import MT94_2"), NULL,
        N_("Import an interim account statement in SWIFT MT942 format into GnuCash."),
        G_CALLBACK(gnc_plugin_ab_cmd_mt942_import)
    },
    {
        "DtausImportAction", "go-previous",
	/* Translators: DTAUS is a traditional german exchange format.
           https://de.wikipedia.org/wiki/Datentr%C3%A4geraustauschverfahren */
		N_("Import _DTAUS"), NULL,
        N_("Import a traditional german DTAUS file into GnuCash."),
        G_CALLBACK(gnc_plugin_ab_cmd_dtaus_import)
    },
    /* #ifdef CSV_IMPORT_FUNCTIONAL */
    /*     { "CsvImportAction", "go-previous", N_("Import _CSV"), NULL, */
    /*       N_("Import a CSV file into GnuCash"), */
    /*       G_CALLBACK(gnc_plugin_ab_cmd_csv_import) }, */
    /*     { "CsvImportSendAction", "go-previous", N_("Import CSV and s_end..."), NULL, */
    /*       N_("Import a CSV file into GnuCash and send the transfers online through Online Banking"), */
    /*       G_CALLBACK(gnc_plugin_ab_cmd_csv_importsend) }, */
    /* #endif */
    {
        "DtausImportSendAction", "go-previous", N_("Import DTAUS and _send..."), NULL,
        N_("Import a DTAUS file into GnuCash and transmit its orders by Online Banking."),
        G_CALLBACK(gnc_plugin_ab_cmd_dtaus_importsend)
    },
};
static guint gnc_plugin_n_actions = G_N_ELEMENTS(gnc_plugin_actions);

static GtkToggleActionEntry gnc_plugin_toggle_actions [] =
{
    {
        MENU_TOGGLE_ACTION_AB_VIEW_LOGWINDOW, NULL,
        N_("Show _log window"), NULL,
        N_("Show the online banking log window."),
        G_CALLBACK(gnc_plugin_ab_cmd_view_logwindow), TRUE
    },
};
static guint gnc_plugin_n_toggle_actions = G_N_ELEMENTS(gnc_plugin_toggle_actions);

static const gchar *need_account_actions[] =
{
    "ABGetBalanceAction",
    "ABGetTransAction",
    "ABIssueSepaTransAction",
#if (AQBANKING_VERSION_INT >= 60400)
    "ABIssueSepaIntTransAction",
#endif
    "ABIssueIntTransAction",
    "ABIssueSepaDirectDebitAction",
    NULL
};

#if (AQBANKING_VERSION_INT < 60400)
static const gchar *inactive_account_actions[] =
{
    "ABIssueSepaIntTransAction",
    NULL
};
#endif

static const gchar *readonly_inactive_actions[] =
{
    "OnlineActionsAction",
    "ABSetupAction",
    NULL
};

static GncMainWindow *gnc_main_window = NULL;

/************************************************************
 *                   Object Implementation                  *
 ************************************************************/

G_DEFINE_TYPE(GncPluginAqBanking, gnc_plugin_aqbanking, GNC_TYPE_PLUGIN)

GncPlugin *
gnc_plugin_aqbanking_new(void)
{
    return GNC_PLUGIN(g_object_new(GNC_TYPE_PLUGIN_AQBANKING, (gchar*) NULL));
}

static void
gnc_plugin_aqbanking_class_init(GncPluginAqBankingClass *klass)
{
    GncPluginClass *plugin_class = GNC_PLUGIN_CLASS(klass);

    /* plugin info */
    plugin_class->plugin_name  = GNC_PLUGIN_AQBANKING_NAME;

    /* widget addition/removal */
    plugin_class->actions_name       = PLUGIN_ACTIONS_NAME;
    plugin_class->actions            = gnc_plugin_actions;
    plugin_class->n_actions          = gnc_plugin_n_actions;
    plugin_class->toggle_actions     = gnc_plugin_toggle_actions;
    plugin_class->n_toggle_actions   = gnc_plugin_n_toggle_actions;
    plugin_class->ui_filename        = PLUGIN_UI_FILENAME;
    plugin_class->add_to_window      = gnc_plugin_aqbanking_add_to_window;
    plugin_class->remove_from_window = gnc_plugin_aqbanking_remove_from_window;
}

static void
gnc_plugin_aqbanking_init(GncPluginAqBanking *plugin)
{
}

/**
 * Called when this plugin is added to a main window.  Connect a few callbacks
 * here to track page changes.
 */
static void
gnc_plugin_aqbanking_add_to_window(GncPlugin *plugin, GncMainWindow *window,
                                   GQuark type)
{
    GtkAction *action;

    gnc_main_window = window;

    g_signal_connect(window, "page_added",
                     G_CALLBACK(gnc_plugin_ab_main_window_page_added),
                     plugin);
    g_signal_connect(window, "page_changed",
                     G_CALLBACK(gnc_plugin_ab_main_window_page_changed),
                     plugin);

    action = gnc_main_window_find_action(window, MENU_TOGGLE_ACTION_AB_VIEW_LOGWINDOW);

    gtk_toggle_action_set_active(GTK_TOGGLE_ACTION(action), FALSE);
}

static void
gnc_plugin_aqbanking_remove_from_window(GncPlugin *plugin, GncMainWindow *window,
                                        GQuark type)
{
    g_signal_handlers_disconnect_by_func(
        window, G_CALLBACK(gnc_plugin_ab_main_window_page_changed), plugin);
    g_signal_handlers_disconnect_by_func(
        window, G_CALLBACK(gnc_plugin_ab_main_window_page_added), plugin);
}

/************************************************************
 *                     Object Callbacks                     *
 ************************************************************/

/**
 * A new page has been added to a main window.  Connect a signal to it so that
 * we can track when accounts are selected.
 */
static void
gnc_plugin_ab_main_window_page_added(GncMainWindow *window, GncPluginPage *page,
                                     gpointer user_data)
{
    const gchar *page_name;

    ENTER("main window %p, page %p", window, page);
    if (!GNC_IS_PLUGIN_PAGE(page))
    {
        LEAVE("no plugin_page");
        return;
    }

    page_name = gnc_plugin_page_get_plugin_name(page);
    if (!page_name)
    {
        LEAVE("no page_name of plugin_page");
        return;
    }

    if (strcmp(page_name, GNC_PLUGIN_PAGE_ACCOUNT_TREE_NAME) == 0)
    {
        DEBUG("account tree page, adding signal");
        g_signal_connect(page, "account_selected",
                         G_CALLBACK(gnc_plugin_ab_account_selected), NULL);
    }

    gnc_plugin_ab_main_window_page_changed(window, page, user_data);

    LEAVE(" ");
}

/** Update the actions sensitivity
*/
static void update_inactive_actions(GncPluginPage *plugin_page)
{
    GncMainWindow  *window;
    GtkActionGroup *action_group;

    // We are readonly - so we have to switch particular actions to inactive.
    gboolean is_readwrite = !qof_book_is_readonly(gnc_get_current_book());

    // We continue only if the current page is a plugin page
    if (!plugin_page || !GNC_IS_PLUGIN_PAGE(plugin_page))
        return;

    window = GNC_MAIN_WINDOW(plugin_page->window);
    g_return_if_fail(GNC_IS_MAIN_WINDOW(window));
    action_group = gnc_main_window_get_action_group(window, PLUGIN_ACTIONS_NAME);
    g_return_if_fail(GTK_IS_ACTION_GROUP(action_group));

    /* Set the action's sensitivity */
    gnc_plugin_update_actions (action_group, readonly_inactive_actions,
                               "sensitive", is_readwrite);
}


/**
 * Whenever the current page has changed, update the aqbanking menus based upon
 * the page that is currently selected.
 */
static void
gnc_plugin_ab_main_window_page_changed(GncMainWindow *window,
                                       GncPluginPage *page, gpointer user_data)
{
    Account *account = main_window_to_account(window);

    /* Make sure not to call this with a NULL GncPluginPage */
    if (page)
    {
        // Update the menu items according to the selected account
        gnc_plugin_ab_account_selected(page, account, user_data);

        // Also update the action sensitivity due to read-only
        update_inactive_actions(page);
    }
}

/**
 * An account had been (de)selected either in an "account tree" page or by
 * selecting another register page. Update the aqbanking menus appropriately.
 */
static void
gnc_plugin_ab_account_selected(GncPluginPage *plugin_page, Account *account,
                               gpointer user_data)
{
    GncMainWindow  *window;
    GtkActionGroup *action_group;
    const gchar *bankcode = NULL;
    const gchar *accountid = NULL;

    g_return_if_fail(GNC_IS_PLUGIN_PAGE(plugin_page));
    window = GNC_MAIN_WINDOW(plugin_page->window);
    g_return_if_fail(GNC_IS_MAIN_WINDOW(window));
    action_group = gnc_main_window_get_action_group(window, PLUGIN_ACTIONS_NAME);
    g_return_if_fail(GTK_IS_ACTION_GROUP(action_group));

    if (account)
    {
        bankcode = gnc_ab_get_account_bankcode(account);
        accountid = gnc_ab_get_account_accountid(account);

        gnc_plugin_update_actions(action_group, need_account_actions,
                                  "sensitive",
                                  (account && bankcode && *bankcode
                                   && accountid && *accountid));
        gnc_plugin_update_actions(action_group, need_account_actions,
                                  "visible", TRUE);
#if (AQBANKING_VERSION_INT < 60400)
        gnc_plugin_update_actions(action_group, inactive_account_actions,
                                  "sensitive", FALSE);
        gnc_plugin_update_actions(action_group, inactive_account_actions,
                                  "visible", FALSE);
#endif
    }
    else
    {
        gnc_plugin_update_actions(action_group, need_account_actions,
                                  "sensitive", FALSE);
        gnc_plugin_update_actions(action_group, need_account_actions,
                                  "visible", FALSE);
    }

}

/************************************************************
 *                    Auxiliary Functions                   *
 ************************************************************/

/**
 * Given a pointer to a main window, try and extract an Account from it.  If the
 * current page is an "account tree" page, get the account corresponding to the
 * selected account.  (What if multiple accounts are selected?)  If the current
 * page is a "register" page, get the head account for the register. (Returns
 * NULL for a general journal or search register.)
 *
 * @param window A pointer to a GncMainWindow object.
 * @return A pointer to an account, if one can be determined from the current
 * page. NULL otherwise.
 */
static Account *
main_window_to_account(GncMainWindow *window)
{
    GncPluginPage  *page;
    const gchar    *page_name;
    Account        *account = NULL;
    const gchar    *account_name;

    ENTER("main window %p", window);
    if (!GNC_IS_MAIN_WINDOW(window))
    {
        LEAVE("no main_window");
        return NULL;
    }

    page = gnc_main_window_get_current_page(window);
    if (!GNC_IS_PLUGIN_PAGE(page))
    {
        LEAVE("no plugin_page");
        return NULL;
    }
    page_name = gnc_plugin_page_get_plugin_name(page);
    if (!page_name)
    {
        LEAVE("no page_name of plugin_page");
        return NULL;
    }

    if (strcmp(page_name, GNC_PLUGIN_PAGE_REGISTER_NAME) == 0)
    {
        DEBUG("register page");
        account = gnc_plugin_page_register_get_account(
                      GNC_PLUGIN_PAGE_REGISTER(page));
    }
    else if (strcmp(page_name, GNC_PLUGIN_PAGE_REGISTER2_NAME) == 0)
    {
        DEBUG("register2 page");
        account = gnc_plugin_page_register2_get_account(
                      GNC_PLUGIN_PAGE_REGISTER2(page));
    }
    else if (strcmp(page_name, GNC_PLUGIN_PAGE_ACCOUNT_TREE_NAME) == 0)
    {
        DEBUG("account tree page");
        account = gnc_plugin_page_account_tree_get_current_account(
                      GNC_PLUGIN_PAGE_ACCOUNT_TREE(page));
    }
    else
    {
        account = NULL;
    }
    account_name = account ? xaccAccountGetName(account) : NULL;
    LEAVE("account %s(%p)", account_name ? account_name : "(null)", account);
    return account;
}

void
gnc_plugin_aqbanking_set_logwindow_visible(gboolean logwindow_visible)
{
    GtkAction *action;

    action = gnc_main_window_find_action(gnc_main_window,
                                         MENU_TOGGLE_ACTION_AB_VIEW_LOGWINDOW);
    if (action)
    {
        gtk_toggle_action_set_active(GTK_TOGGLE_ACTION(action),
                                     logwindow_visible);
    }
}

/************************************************************
 *                    Command Callbacks                     *
 ************************************************************/

static void
gnc_plugin_ab_cmd_setup(GtkAction *action, GncMainWindowActionData *data)
{
    ENTER("action %p, main window data %p", action, data);
    gnc_main_window = data->window;
    gnc_ab_initial_assistant();
    LEAVE(" ");
}

static void
gnc_plugin_ab_cmd_get_balance(GtkAction *action, GncMainWindowActionData *data)
{
    Account *account;

    ENTER("action %p, main window data %p", action, data);
    account = main_window_to_account(data->window);
    if (account == NULL)
    {
        PINFO("No AqBanking account selected");
        LEAVE("no account");
        return;
    }

    gnc_main_window = data->window;
    gnc_ab_getbalance(GTK_WIDGET(data->window), account);

    LEAVE(" ");
}

static void
gnc_plugin_ab_cmd_get_transactions(GtkAction *action,
                                   GncMainWindowActionData *data)
{
    Account *account;

    ENTER("action %p, main window data %p", action, data);
    account = main_window_to_account(data->window);
    if (account == NULL)
    {
        PINFO("No AqBanking account selected");
        LEAVE("no account");
        return;
    }

    gnc_main_window = data->window;
    gnc_ab_gettrans(GTK_WIDGET(data->window), account);

    LEAVE(" ");
}

static void
gnc_plugin_ab_cmd_issue_sepatransaction(GtkAction *action,
                                    GncMainWindowActionData *data)
{
    Account *account;

    ENTER("action %p, main window data %p", action, data);
    account = main_window_to_account(data->window);
    if (account == NULL)
    {
        PINFO("No AqBanking account selected");
        LEAVE("no account");
        return;
    }

    gnc_main_window = data->window;
    gnc_ab_maketrans(GTK_WIDGET(data->window), account, SEPA_TRANSFER);

    LEAVE(" ");
}

#if (AQBANKING_VERSION_INT >= 60400)
static void
gnc_plugin_ab_cmd_issue_sepainternaltransaction(GtkAction *action,
                                    GncMainWindowActionData *data)
{
    Account *account;

    ENTER("action %p, main window data %p", action, data);
    account = main_window_to_account(data->window);
    if (account == NULL)
    {
        PINFO("No AqBanking account selected");
        LEAVE("no account");
        return;
    }

    gnc_main_window = data->window;
    gnc_ab_maketrans(GTK_WIDGET(data->window), account, SEPA_INTERNAL_TRANSFER);

    LEAVE(" ");
}
#else
static void
gnc_plugin_ab_cmd_issue_sepainternaltransaction(GtkAction *action,
                                    GncMainWindowActionData *data)
{

    ENTER("action %p, main window data %p", action, data);
    PINFO("Sepa Internal Transfer not supported by your aqbanking version!");
    LEAVE("Sepa Internal Transfer not supported!");
}
#endif

static void
gnc_plugin_ab_cmd_issue_inttransaction(GtkAction *action,
                                       GncMainWindowActionData *data)
{
    Account *account;

    ENTER("action %p, main window data %p", action, data);
    account = main_window_to_account(data->window);
    if (account == NULL)
    {
        PINFO("No AqBanking account selected");
        LEAVE("no account");
        return;
    }

    gnc_main_window = data->window;
    gnc_ab_maketrans(GTK_WIDGET(data->window), account,
                     SINGLE_INTERNAL_TRANSFER);

    LEAVE(" ");
}

static void
gnc_plugin_ab_cmd_issue_sepa_direct_debit(GtkAction *action,
                                          GncMainWindowActionData *data)
{
    Account *account;

    ENTER("action %p, main window data %p", action, data);
    account = main_window_to_account(data->window);
    if (account == NULL)
    {
        PINFO("No AqBanking account selected");
        LEAVE("no account");
        return;
    }

    gnc_main_window = data->window;
    gnc_ab_maketrans(GTK_WIDGET(data->window), account, SEPA_DEBITNOTE);

    LEAVE(" ");
}

static void
gnc_plugin_ab_cmd_view_logwindow(GtkToggleAction *action, GncMainWindow *window)
{
    if (gtk_toggle_action_get_active(GTK_TOGGLE_ACTION(action)))
    {
        if (!gnc_GWEN_Gui_show_dialog())
        {
            /* Log window could not be made visible */
            gtk_toggle_action_set_active(GTK_TOGGLE_ACTION(action), FALSE);
        }
    }
    else
    {
        gnc_GWEN_Gui_hide_dialog();
    }
}


static void
gnc_plugin_ab_cmd_mt940_import(GtkAction *action, GncMainWindowActionData *data)
{
    gchar *format = gnc_prefs_get_string(GNC_PREFS_GROUP_AQBANKING,
                                         GNC_PREF_FORMAT_SWIFT940);
    gnc_main_window = data->window;
    gnc_file_aqbanking_import (GTK_WINDOW (gnc_main_window),
                               "swift", format && *format ? format : "swift-mt940", FALSE);
    g_free(format);
}

static void
gnc_plugin_ab_cmd_mt942_import(GtkAction *action, GncMainWindowActionData *data)
{
    gchar *format = gnc_prefs_get_string(GNC_PREFS_GROUP_AQBANKING,
                                         GNC_PREF_FORMAT_SWIFT942);
    gnc_main_window = data->window;
    gnc_file_aqbanking_import (GTK_WINDOW (gnc_main_window),
                               "swift", format && *format? format : "swift-mt942", FALSE);
    g_free(format);
}

static void
gnc_plugin_ab_cmd_dtaus_import(GtkAction *action, GncMainWindowActionData *data)
{
    gchar *format = gnc_prefs_get_string(GNC_PREFS_GROUP_AQBANKING,
                                         GNC_PREF_FORMAT_DTAUS);
    gnc_main_window = data->window;
    gnc_file_aqbanking_import (GTK_WINDOW (gnc_main_window),
                               "dtaus", format && *format ? format : "default", FALSE);
    g_free(format);
}

static void
gnc_plugin_ab_cmd_dtaus_importsend(GtkAction *action,
                                   GncMainWindowActionData *data)
{
    gchar *format = gnc_prefs_get_string(GNC_PREFS_GROUP_AQBANKING,
                                         GNC_PREF_FORMAT_DTAUS);
    gnc_main_window = data->window;
    gnc_file_aqbanking_import (GTK_WINDOW (gnc_main_window),
                               "dtaus", format && *format ? format : "default", TRUE);
    g_free(format);
}

/************************************************************
 *                    Plugin Bootstrapping                  *
 ************************************************************/

void
gnc_plugin_aqbanking_create_plugin(void)
{
    GncPlugin *plugin = gnc_plugin_aqbanking_new();

    gnc_plugin_manager_add_plugin(gnc_plugin_manager_get(), plugin);
}
