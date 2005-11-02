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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
 * Boston, MA  02111-1307,  USA       gnu@gnu.org
 */

#include "config.h"
#include <string.h>

#include "gnc-plugin-basic-commands.h"

#include "dialog-chart-export.h"
#include "dialog-fincalc.h"
#include "dialog-find-transactions.h"
#include "dialog-scheduledxaction.h"
#include "dialog-sxsincelast.h"
#include "dialog-totd.h"
#include "druid-acct-period.h"
#include "druid-loan.h"
#include "druid-merge.h"
#include "gnc-engine.h"
#include "gnc-file.h"
#include "gnc-gui-query.h"
#include "gnc-ui.h"
#include "gnc-window.h"
#include "messages.h"

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_GUI;

static void gnc_plugin_basic_commands_class_init (GncPluginBasicCommandsClass *klass);
static void gnc_plugin_basic_commands_init (GncPluginBasicCommands *plugin);
static void gnc_plugin_basic_commands_finalize (GObject *object);

/* Command callbacks */
static void gnc_main_window_cmd_file_new (GtkAction *action, GncMainWindowActionData *data);
static void gnc_main_window_cmd_file_open (GtkAction *action, GncMainWindowActionData *data);
static void gnc_main_window_cmd_file_save (GtkAction *action, GncMainWindowActionData *data);
static void gnc_main_window_cmd_file_save_as (GtkAction *action, GncMainWindowActionData *data);
static void gnc_main_window_cmd_file_qsf_import (GtkAction *action, GncMainWindowActionData *data);
static void gnc_main_window_cmd_file_export_accounts (GtkAction *action, GncMainWindowActionData *data);
static void gnc_main_window_cmd_file_chart_export (GtkAction *action, GncMainWindowActionData *data);
static void gnc_main_window_cmd_edit_tax_options (GtkAction *action, GncMainWindowActionData *data);
static void gnc_main_window_cmd_actions_mortgage_loan (GtkAction *action, GncMainWindowActionData *data);
static void gnc_main_window_cmd_actions_scheduled_transaction_editor (GtkAction *action, GncMainWindowActionData *data);
static void gnc_main_window_cmd_actions_since_last_run (GtkAction *action, GncMainWindowActionData *data);
static void gnc_main_window_cmd_actions_close_books (GtkAction *action, GncMainWindowActionData *data);
static void gnc_main_window_cmd_tools_financial_calculator (GtkAction *action, GncMainWindowActionData *data);
static void gnc_main_window_cmd_tools_find_transactions (GtkAction *action, GncMainWindowActionData *data);
static void gnc_main_window_cmd_tools_price_editor (GtkAction *action, GncMainWindowActionData *data);
static void gnc_main_window_cmd_tools_commodity_editor (GtkAction *action, GncMainWindowActionData *data);
static void gnc_main_window_cmd_help_totd (GtkAction *action, GncMainWindowActionData *data);



#define PLUGIN_ACTIONS_NAME "gnc-plugin-basic-commands-actions"
#define PLUGIN_UI_FILENAME  "gnc-plugin-basic-commands-ui.xml"

static GtkActionEntry gnc_plugin_actions [] = {

  /* File menu */

  { "FileNewAction", GTK_STOCK_NEW, N_("New _File"), NULL,
    N_("Create a new file"),
    G_CALLBACK (gnc_main_window_cmd_file_new) },
  { "FileOpenAction", GTK_STOCK_OPEN, N_("_Open..."), NULL,
    NULL,
    G_CALLBACK (gnc_main_window_cmd_file_open) },
  { "FileSaveAction", GTK_STOCK_SAVE, N_("_Save"), "<control>s",
    NULL,
    G_CALLBACK (gnc_main_window_cmd_file_save) },
  { "FileSaveAsAction", GTK_STOCK_SAVE_AS, N_("Save _As..."), "<shift><control>s",
    NULL,
    G_CALLBACK (gnc_main_window_cmd_file_save_as) },
  { "FileImportQSFAction", GTK_STOCK_CONVERT,
    N_("_QSF Import"), NULL,
    N_("Import a QSF object file"),
    G_CALLBACK (gnc_main_window_cmd_file_qsf_import) },
  { "FileExportAccountsAction", GTK_STOCK_CONVERT,
    N_("Export _Accounts"), NULL,
    N_("Export the account hierarchy to a new file"),
    G_CALLBACK (gnc_main_window_cmd_file_export_accounts) },
  { "FileExportChartAction", GTK_STOCK_CONVERT,
    N_("Export _Chart of Accounts"), NULL,
    N_("Export the chart of accounts for a date with balances"),
    G_CALLBACK (gnc_main_window_cmd_file_chart_export) },

  /* Edit menu */

  { "EditFindTransactionsAction", GTK_STOCK_FIND, N_("_Find..."), "<control>f",
    N_("Find transactions with a search"),
    G_CALLBACK (gnc_main_window_cmd_tools_find_transactions) },
  { "EditTaxOptionsAction", NULL, N_("Ta_x Options"), NULL,
    N_("Setup tax information for all income and expense accounts"),
    G_CALLBACK (gnc_main_window_cmd_edit_tax_options) },

  /* Actions menu */

  { "ActionsScheduledTransactionsAction", NULL, N_("_Scheduled Transactions"), NULL, NULL, NULL },
  { "ActionsScheduledTransactionEditorAction", NULL, N_("_Scheduled Transaction Editor"), NULL,
    N_("The list of Scheduled Transactions"),
    G_CALLBACK (gnc_main_window_cmd_actions_scheduled_transaction_editor) },
  { "ActionsSinceLastRunAction", NULL, N_("_Since Last Run..."), NULL,
    N_("Create Scheduled Transactions since the last time run"),
    G_CALLBACK (gnc_main_window_cmd_actions_since_last_run) },
  { "ActionsMortgageLoanAction", NULL, N_("_Mortgage & Loan Repayment..."), NULL,
    N_("Setup scheduled transactions for repayment of a loan"),
    G_CALLBACK (gnc_main_window_cmd_actions_mortgage_loan) },
  { "ActionsCloseBooksAction", NULL, N_("Close _Books"), NULL,
    N_("Archive old data using accounting periods"),
    G_CALLBACK (gnc_main_window_cmd_actions_close_books) },

  /* Tools menu */

  { "ToolsPriceEditorAction", NULL, N_("_Price Editor"), NULL,
    N_("View and edit the prices for stocks and mutual funds"),
    G_CALLBACK (gnc_main_window_cmd_tools_price_editor) },
  { "ToolsCommodityEditorAction", NULL, N_("_Commodity Editor"), NULL,
    N_("View and edit the commodities for stocks and mutual funds"),
    G_CALLBACK (gnc_main_window_cmd_tools_commodity_editor) },
  { "ToolsFinancialCalculatorAction", NULL, N_("_Financial Calculator"), NULL,
    N_("Use the financial calculator"),
    G_CALLBACK (gnc_main_window_cmd_tools_financial_calculator) },

  /* Help menu */

  { "HelpTipsOfTheDayAction", NULL, N_("_Tips Of The Day"), NULL,
    N_("View the Tips of the Day"),
    G_CALLBACK (gnc_main_window_cmd_help_totd) },
};
static guint gnc_plugin_n_actions = G_N_ELEMENTS (gnc_plugin_actions);

static const gchar *gnc_plugin_important_actions[] = {
  "FileSaveAction",
  NULL,
};


struct GncPluginBasicCommandsPrivate
{
  gpointer dummy;
};

static GObjectClass *parent_class = NULL;

GType
gnc_plugin_basic_commands_get_type (void)
{
  static GType gnc_plugin_basic_commands_type = 0;

  if (gnc_plugin_basic_commands_type == 0) {
    static const GTypeInfo our_info = {
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

GncPlugin *
gnc_plugin_basic_commands_new (void)
{
  GncPluginBasicCommands *plugin;

  plugin = g_object_new (GNC_TYPE_PLUGIN_BASIC_COMMANDS, NULL);

  return GNC_PLUGIN (plugin);
}

static void
gnc_plugin_basic_commands_class_init (GncPluginBasicCommandsClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS (klass);
  GncPluginClass *plugin_class = GNC_PLUGIN_CLASS (klass);

  parent_class = g_type_class_peek_parent (klass);

  object_class->finalize = gnc_plugin_basic_commands_finalize;

  /* plugin info */
  plugin_class->plugin_name  = GNC_PLUGIN_BASIC_COMMANDS_NAME;

  /* widget addition/removal */
  plugin_class->actions_name 	  = PLUGIN_ACTIONS_NAME;
  plugin_class->actions      	  = gnc_plugin_actions;
  plugin_class->n_actions    	  = gnc_plugin_n_actions;
  plugin_class->important_actions = gnc_plugin_important_actions;
  plugin_class->ui_filename       = PLUGIN_UI_FILENAME;
}

static void
gnc_plugin_basic_commands_init (GncPluginBasicCommands *plugin)
{
  plugin->priv = g_new0 (GncPluginBasicCommandsPrivate, 1);
}

static void
gnc_plugin_basic_commands_finalize (GObject *object)
{
  GncPluginBasicCommands *plugin;

  g_return_if_fail (GNC_IS_PLUGIN_BASIC_COMMANDS (object));

  plugin = GNC_PLUGIN_BASIC_COMMANDS (object);

  g_return_if_fail (plugin->priv != NULL);

  g_free (plugin->priv);

  G_OBJECT_CLASS (parent_class)->finalize (object);
}

/************************************************************
 *                    Command Callbacks                     *
 ************************************************************/

static void
gnc_main_window_cmd_file_new (GtkAction *action, GncMainWindowActionData *data)
{
  gnc_file_new ();
  /* FIXME GNOME 2 Port (update the title etc.) */
}

static void
gnc_main_window_cmd_file_open (GtkAction *action, GncMainWindowActionData *data)
{
  g_return_if_fail (data != NULL);

  gnc_window_set_progressbar_window (GNC_WINDOW(data->window));
  gnc_file_open ();
  gnc_window_set_progressbar_window (NULL);
}

static void
gnc_main_window_cmd_file_save (GtkAction *action, GncMainWindowActionData *data)
{
  g_return_if_fail (data != NULL);

  gnc_window_set_progressbar_window (GNC_WINDOW(data->window));
  gnc_file_save ();
  gnc_window_set_progressbar_window (NULL);
  /* FIXME GNOME 2 Port (update the title etc.) */
}

static void
gnc_main_window_cmd_file_save_as (GtkAction *action, GncMainWindowActionData *data)
{
  g_return_if_fail (data != NULL);

  gnc_window_set_progressbar_window (GNC_WINDOW(data->window));
  gnc_file_save_as ();
  gnc_window_set_progressbar_window (NULL);
  /* FIXME GNOME 2 Port (update the title etc.) */
}

static void
qsf_file_select_ok(GtkWidget *w, GtkFileSelection *fs )
{
  QofSession *qsf_session, *first_session;
  const gchar *filename;
  QofBook *original;

  ENTER (" ");
  gnc_engine_suspend_events();
  filename = gtk_file_selection_get_filename(GTK_FILE_SELECTION (fs));
  gtk_widget_destroy((GtkWidget*) fs);
  first_session = qof_session_get_current_session();
  original = qof_session_get_book(first_session);
  qsf_session = qof_session_new();
  qof_session_begin(qsf_session, filename, TRUE, FALSE);
  qof_session_load(qsf_session, NULL);
  gnc_engine_resume_events();
  gnc_ui_qsf_import_merge_druid(first_session, qsf_session);
  LEAVE (" ");
}

static void
gnc_main_window_cmd_file_qsf_import (GtkAction *action, GncMainWindowActionData *data)
{
  GtkWidget *file_select;

  g_return_if_fail (data != NULL);

  gnc_window_set_progressbar_window(GNC_WINDOW(data->window));
  file_select = gtk_file_selection_new("Select the QSF file to import into GnuCash");
  g_signal_connect (G_OBJECT (GTK_FILE_SELECTION (file_select)->ok_button),
		    "clicked", G_CALLBACK (qsf_file_select_ok), (gpointer) file_select);
  g_signal_connect_swapped (G_OBJECT (GTK_FILE_SELECTION (file_select)->cancel_button),
			    "clicked", G_CALLBACK (gtk_widget_destroy), G_OBJECT (file_select));
  gtk_widget_show (file_select);
  gnc_window_set_progressbar_window(NULL);
}

static void
gnc_main_window_cmd_file_export_accounts (GtkAction *action, GncMainWindowActionData *data)
{
  g_return_if_fail (data != NULL);

  gnc_window_set_progressbar_window (GNC_WINDOW(data->window));
  gnc_file_export_file (NULL);
  gnc_window_set_progressbar_window (NULL);
  /* FIXME GNOME 2 Port (update the title etc.) */
  /* gnc_refresh_main_window_info (); */
}

static void
gnc_main_window_cmd_file_chart_export (GtkAction *action, GncMainWindowActionData *data)
{
  g_return_if_fail (data != NULL);

  gnc_window_set_progressbar_window (GNC_WINDOW(data->window));
  gnc_main_window_chart_export();
  gnc_window_set_progressbar_window (NULL);
  /* FIXME GNOME 2 Port (update the title etc.) */
  /* gnc_refresh_main_window_info (); */
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
  gnc_ui_scheduled_xaction_dialog_create ();
}

static void
gnc_main_window_cmd_actions_since_last_run (GtkAction *action, GncMainWindowActionData *data)
{
  GncMainWindow *window;
  gint ret;
  const char *nothing_to_do_msg =
    _( "There are no Scheduled Transactions to be entered at this time." );
	
  g_return_if_fail (data != NULL);

  window = data->window;
  ret = gnc_ui_sxsincelast_dialog_create ();
  if ( ret == 0 ) {
    gnc_info_dialog (GTK_WIDGET(&window->parent), nothing_to_do_msg);
  } else if ( ret < 0 ) {
    gnc_info_dialog (GTK_WIDGET(&window->parent), ngettext
		     /* Translators: %d is the number of transactions. This is a
			ngettext(3) message. */
		     ("There are no Scheduled Transactions to be entered at this time.\n"
		      "(%d transaction automatically created)",
		      "There are no Scheduled Transactions to be entered at this time.\n"
		      "(%d transactions automatically created)",
		      -(ret)),
		     -(ret));
  } /* else { this else [>0 means dialog was created] intentionally left
     * blank. } */	       
}

static void
gnc_main_window_cmd_actions_mortgage_loan (GtkAction *action, GncMainWindowActionData *data)
{
  gnc_ui_sx_loan_druid_create ();
}

static void
gnc_main_window_cmd_actions_close_books (GtkAction *action, GncMainWindowActionData *data)
{
  gnc_acct_period_dialog();
}

static void
gnc_main_window_cmd_tools_price_editor (GtkAction *action, GncMainWindowActionData *data)
{
	gnc_prices_dialog (NULL);
}

static void
gnc_main_window_cmd_tools_commodity_editor (GtkAction *action, GncMainWindowActionData *data)
{
	gnc_commodities_dialog (NULL);
}

static void
gnc_main_window_cmd_tools_financial_calculator (GtkAction *action, GncMainWindowActionData *data)
{
  gnc_ui_fincalc_dialog_create();
}

static void
gnc_main_window_cmd_tools_find_transactions (GtkAction *action, GncMainWindowActionData *data)
{
  gnc_ui_find_transactions_dialog_create (NULL);
}

static void
gnc_main_window_cmd_help_totd (GtkAction *action, GncMainWindowActionData *data)
{
  g_return_if_fail (data != NULL);

  gnc_totd_dialog(GTK_WINDOW(data->window), FALSE);
}
