/* 
 * gnc-main-window.c -- GtkWindow which represents the GnuCash main window.
 * Copyright (C) 2003 Jan Arne Petersen
 * Author: Jan Arne Petersen <jpetersen@uni-bonn.de>
 */

#include "config.h"

#include <gtk/gtkwindow.h>
#include <gtk/gtkvbox.h>
#include <gtk/gtknotebook.h>
#include <gtk/gtkstatusbar.h>

#include "eggtoolbar.h"
#include "egg-action-group.h"
#include "egg-menu-merge.h"
#include "egg-toggle-action.h"

#include "gnc-main-window.h"

#include "dialog-fincalc.h"
#include "dialog-find-transactions.h"
#include "dialog-options.h"
#include "dialog-scheduledxaction.h"
#include "dialog-sxsincelast.h"
#include "dialog-transfer.h"
#include "dialog-utils.h"
#include "druid-loan.h"
#include "gnc-engine.h"
#include "gnc-engine-util.h"
#include "gnc-file.h"
#include "gnc-gui-query.h"
#include "gnc-plugin.h"
#include "gnc-plugin-manager.h"
#include "gnc-split-reg.h"
#include "gnc-totd-dialog.h"
#include "gnc-ui.h"
#include "gnc-version.h"
#include "mainwindow-account-tree.h"
#include "window-acct-tree.h"
#include "window-help.h"
#include "window-main.h"
#include "window-reconcile.h"
#include "window-register.h"
#include "window-report.h"
#include "messages.h"

static void gnc_main_window_class_init (GncMainWindowClass *klass);
static void gnc_main_window_init (GncMainWindow *window);
static void gnc_main_window_finalize (GObject *object);
static void gnc_main_window_dispose (GObject *object);

static void gnc_main_window_setup_window (GncMainWindow *window);

/* Callbacks */
static void gnc_main_window_add_widget (EggMenuMerge *merge, GtkWidget *widget, GncMainWindow *window);
static void gnc_main_window_change_current_page (GtkNotebook *notebook, gint pos, GncMainWindow *window);
static void gnc_main_window_switch_page (GtkNotebook *notebook, GtkNotebookPage *notebook_page, gint pos, GncMainWindow *window);
static void gnc_main_window_plugin_added (GncPlugin *manager, GncPlugin *plugin, GncMainWindow *window);
static void gnc_main_window_plugin_removed (GncPlugin *manager, GncPlugin *plugin, GncMainWindow *window);

/* Command callbacks */
static void gnc_main_window_cmd_file_new (EggAction *action, GncMainWindow *window);
static void gnc_main_window_cmd_file_open (EggAction *action, GncMainWindow *window);
static void gnc_main_window_cmd_file_open_new_window (EggAction *action, GncMainWindow *window);
static void gnc_main_window_cmd_file_save (EggAction *action, GncMainWindow *window);
static void gnc_main_window_cmd_file_save_as (EggAction *action, GncMainWindow *window);
static void gnc_main_window_cmd_file_export_accounts (EggAction *action, GncMainWindow *window);
static void gnc_main_window_cmd_file_print (EggAction *action, GncMainWindow *window);
static void gnc_main_window_cmd_file_close (EggAction *action, GncMainWindow *window);
static void gnc_main_window_cmd_file_quit (EggAction *action, GncMainWindow *window);
static void gnc_main_window_cmd_edit_cut (EggAction *action, GncMainWindow *window);
static void gnc_main_window_cmd_edit_copy (EggAction *action, GncMainWindow *window);
static void gnc_main_window_cmd_edit_paste (EggAction *action, GncMainWindow *window);
static void gnc_main_window_cmd_edit_preferences (EggAction *action, GncMainWindow *window);
static void gnc_main_window_cmd_edit_tax_options (EggAction *action, GncMainWindow *window);
static void gnc_main_window_cmd_view_refresh (EggAction *action, GncMainWindow *window);
static void gnc_main_window_cmd_view_toolbar (EggAction *action, GncMainWindow *window);
static void gnc_main_window_cmd_view_summary (EggAction *action, GncMainWindow *window);
static void gnc_main_window_cmd_view_statusbar (EggAction *action, GncMainWindow *window);
static void gnc_main_window_cmd_actions_scheduled_transaction_editor (EggAction *action, GncMainWindow *window);
static void gnc_main_window_cmd_actions_since_last_run (EggAction *action, GncMainWindow *window);
static void gnc_main_window_cmd_actions_mortgage_loan (EggAction *action, GncMainWindow *window);
static void gnc_main_window_cmd_tools_general_ledger (EggAction *action, GncMainWindow *window);
static void gnc_main_window_cmd_tools_price_editor (EggAction *action, GncMainWindow *window);
static void gnc_main_window_cmd_tools_commodity_editor (EggAction *action, GncMainWindow *window);
static void gnc_main_window_cmd_tools_financial_calculator (EggAction *action, GncMainWindow *window);
static void gnc_main_window_cmd_tools_find_transactions (EggAction *action, GncMainWindow *window);
static void gnc_main_window_cmd_help_tutorial (EggAction *action, GncMainWindow *window);
static void gnc_main_window_cmd_help_totd (EggAction *action, GncMainWindow *window);
static void gnc_main_window_cmd_help_contents (EggAction *action, GncMainWindow *window);
static void gnc_main_window_cmd_help_about (EggAction *action, GncMainWindow *window);

struct GncMainWindowPrivate
{
	GtkWidget *menu_dock;
	GtkWidget *toolbar_dock;
	GtkWidget *notebook;
	GtkWidget *statusbar;

	EggActionGroup *action_group;

	GncPluginPage *current_page;

	GHashTable *merged_actions_table;
};

typedef struct {
	guint merge_id;
	EggActionGroup *action_group;
} MergedActionEntry;

static EggActionGroupEntry gnc_menu_entries [] = {

	/* Toplevel */
	{ "FileAction", N_("_File"), NULL, NULL, NULL, NULL, NULL },
	{ "EditAction", N_("_Edit"), NULL, NULL, NULL, NULL, NULL },
	{ "ViewAction", N_("_View"), NULL, NULL, NULL, NULL, NULL },
	{ "ActionsAction", N_("_Actions"), NULL, NULL, NULL, NULL, NULL },
	{ "ToolsAction", N_("_Tools"), NULL, NULL, NULL, NULL, NULL },
	{ "HelpAction", N_("_Help"), NULL, NULL, NULL, NULL, NULL },

	/* File menu */
	{ "FileNewAction", N_("_New File"), GTK_STOCK_NEW, "<control>n",
	  N_("Create a new file"),
	  G_CALLBACK (gnc_main_window_cmd_file_new), NULL },
	{ "FileOpenAction", N_("_Open"), GTK_STOCK_OPEN, NULL,
	  NULL,
	  G_CALLBACK (gnc_main_window_cmd_file_open), NULL },
	{ "FileOpenNewWindowAction", N_("Open in a New Window"), NULL, NULL,
	  N_("Open a new top-level GnuCash window for the current view"),
	  G_CALLBACK (gnc_main_window_cmd_file_open_new_window), NULL },
	{ "FileOpenRecentAction", N_("Open _Recent"), NULL, NULL, NULL, NULL, NULL },
	{ "FileSaveAction", N_("_Save"), GTK_STOCK_SAVE, "<control>s",
	  NULL,
	  G_CALLBACK (gnc_main_window_cmd_file_save), NULL },
	{ "FileSaveAsAction", N_("Save _As..."), GTK_STOCK_SAVE_AS, "<shift><control>s",
	  NULL,
	  G_CALLBACK (gnc_main_window_cmd_file_save_as), NULL },
	{ "FileImportAction", N_("_Import"), NULL, NULL, NULL, NULL, NULL },
	{ "FileExportAction", N_("_Export"), NULL, NULL, NULL, NULL, NULL },
	{ "FileExportAccountsAction", N_("Export _Accounts..."), NULL, NULL,
	   N_("Export the account hierarchy to a new file"),
	   G_CALLBACK (gnc_main_window_cmd_file_export_accounts), NULL },
	{ "FilePrintAction", N_("_Print..."), GTK_STOCK_PRINT, "<control>p",
	  NULL,
	  G_CALLBACK (gnc_main_window_cmd_file_print), NULL },
	{ "FileCloseAction", N_("_Close"), GTK_STOCK_CLOSE, "<control>w",
	  NULL,
	  G_CALLBACK (gnc_main_window_cmd_file_close), NULL },
	{ "FileQuitAction", N_("_Quit"), GTK_STOCK_QUIT, NULL,
	  NULL,
	  G_CALLBACK (gnc_main_window_cmd_file_quit), NULL },

	/* Edit menu */
	{ "EditCutAction", N_("Cu_t"), GTK_STOCK_CUT, "<control>x",
	  NULL,
	  G_CALLBACK (gnc_main_window_cmd_edit_cut), NULL },
	{ "EditCopyAction", N_("_Copy"), GTK_STOCK_COPY, "<control>c",
	  NULL,
	  G_CALLBACK (gnc_main_window_cmd_edit_copy), NULL },
	{ "EditPasteAction", N_("_Paste"), GTK_STOCK_PASTE, "<control>v",
	  NULL,
	  G_CALLBACK (gnc_main_window_cmd_edit_paste), NULL },
	{ "EditPreferencesAction", N_("_Preferences"), GTK_STOCK_PREFERENCES, NULL,
	  NULL,
	  G_CALLBACK (gnc_main_window_cmd_edit_preferences), NULL },
	{ "EditTaxOptionsAction", N_("Ta_x Options"), NULL, NULL,
	  N_("Setup tax information for all income and expense accounts"),
	  G_CALLBACK (gnc_main_window_cmd_edit_tax_options), NULL },

	/* View menu */
	{ "ViewRefreshAction", N_("_Refresh"), GTK_STOCK_REFRESH, "<control>r",
	  N_("Refresh this window"),
	  G_CALLBACK (gnc_main_window_cmd_view_refresh), NULL },
	{ "ViewToolbarAction", N_("_Toolbar"), NULL, "<shift><control>t",
	  N_("Show/hide the toolbar on this window"),
	  G_CALLBACK (gnc_main_window_cmd_view_toolbar), NULL, TOGGLE_ACTION },
	{ "ViewSummaryAction", N_("S_ummary Bar"), NULL, NULL,
	  N_("Show/hide the summary bar on this window"),
	  G_CALLBACK (gnc_main_window_cmd_view_summary), NULL, TOGGLE_ACTION },
	{ "ViewStatusbarAction", N_("_Status Bar"), NULL, NULL,
	  N_("Show/hide the status bar on this window"),
	  G_CALLBACK (gnc_main_window_cmd_view_statusbar), NULL, TOGGLE_ACTION },

	/* Actions menu */
	{ "ActionsScheduledTransactionsAction", N_("_Scheduled Transactions"), NULL, NULL, NULL, NULL, NULL },
	{ "ActionsScheduledTransactionEditorAction", N_("_Scheduled Transaction Editor"), NULL, "NULL",
	  N_("The list of Scheduled Transactions"),
	  G_CALLBACK (gnc_main_window_cmd_actions_scheduled_transaction_editor), NULL },
	{ "ActionsSinceLastRunAction", N_("_Since Last Run..."), NULL, "NULL",
	  N_("Create Scheduled Transactions since the last time run"),
	  G_CALLBACK (gnc_main_window_cmd_actions_since_last_run), NULL },
	{ "ActionsMortgageLoanAction", N_("_Mortgage & Loan Repayment..."), NULL, "NULL",
	  N_("Setup scheduled transactions for repayment of a loan"),
	  G_CALLBACK (gnc_main_window_cmd_actions_mortgage_loan), NULL },
	
	/* Tools menu */
	{ "ToolsGeneralLedgerAction", N_("_General Ledger"), NULL, NULL,
	  N_("Open a general ledger window"),
	  G_CALLBACK (gnc_main_window_cmd_tools_general_ledger), NULL },
	{ "ToolsPriceEditorAction", N_("_Price Editor"), NULL, NULL,
	  N_("View and edit the prices for stocks and mutual funds"),
	  G_CALLBACK (gnc_main_window_cmd_tools_price_editor), NULL },
	{ "ToolsCommodityEditorAction", N_("Commodity _Editor"), NULL, NULL,
	  N_("View and edit the commodities for stocks and mutual funds"),
	  G_CALLBACK (gnc_main_window_cmd_tools_commodity_editor), NULL },
	{ "ToolsFinancialCalculatorAction", N_("Financial _Calculator"), NULL, NULL,
	  N_("Use the financial calculator"),
	  G_CALLBACK (gnc_main_window_cmd_tools_financial_calculator), NULL },
	{ "ToolsFindTransactionsAction", N_("_Find Transactions"), GTK_STOCK_FIND, "<control>f",
	  N_("Find transactions with a search"),
	  G_CALLBACK (gnc_main_window_cmd_tools_find_transactions), NULL },

	/* Help menu */
	{ "HelpTutorialAction", N_("Tutorial and Concepts _Guide"), GNOME_STOCK_BOOK_BLUE, NULL,
	  N_("Open the GnuCash Tutorial"),
	  G_CALLBACK (gnc_main_window_cmd_help_tutorial), NULL },
	{ "HelpTipsOfTheDayAction", N_("_Tips Of The Day"), NULL, NULL,
	  N_("View the Tips of the Day"),
	  G_CALLBACK (gnc_main_window_cmd_help_totd), NULL },
	{ "HelpContentsAction", N_("_Contents"), GTK_STOCK_HELP, NULL,
	  N_("Open the GnuCash Help"),
	  G_CALLBACK (gnc_main_window_cmd_help_contents), NULL },
	{ "HelpAboutAction", N_("_About"), GNOME_STOCK_ABOUT, NULL,
	  NULL,
	  G_CALLBACK (gnc_main_window_cmd_help_about), NULL },
};
static guint gnc_menu_n_entries = G_N_ELEMENTS (gnc_menu_entries);

static GObjectClass *parent_class = NULL;

GType
gnc_main_window_get_type (void)
{
	static GType gnc_main_window_type = 0;

	if (gnc_main_window_type == 0) {
		static const GTypeInfo our_info = {
			sizeof (GncMainWindowClass),
			NULL,
			NULL,
			(GClassInitFunc) gnc_main_window_class_init,
			NULL,
			NULL,
			sizeof (GncMainWindow),
			0,
			(GInstanceInitFunc) gnc_main_window_init
		};

		gnc_main_window_type = g_type_register_static (GTK_TYPE_WINDOW,
							       "GncMainWindow",
							       &our_info, 0);
	}

	return gnc_main_window_type;
}

GncMainWindow *
gnc_main_window_new (void)
{
	return g_object_new (GNC_TYPE_MAIN_WINDOW, NULL);
}

void
gnc_main_window_open_page (GncMainWindow *window,
			   GncPluginPage *page)
{
	GtkWidget *child;
	GtkWidget *label_box;
	GtkWidget *label;
	const GdkPixbuf *icon;
	const gchar *title;
	gint pos;

	g_return_if_fail (GNC_IS_MAIN_WINDOW (window));
	g_return_if_fail (GNC_IS_PLUGIN_PAGE (page));

	child = gnc_plugin_page_create_widget (page);
	g_object_set_data (G_OBJECT (child), "page-plugin", page);

	icon = gnc_plugin_page_get_icon (page);
	title = gnc_plugin_page_get_title (page);

	label = gtk_label_new (title);
	gtk_widget_show (label);

	if (icon != NULL) {
		/* FIXME */
		label_box = gtk_hbox_new (FALSE, 6);
	} else {
		label_box = label;
	}
	
	gtk_notebook_append_page (GTK_NOTEBOOK (window->priv->notebook),
				  child, label_box);

	gnc_plugin_page_inserted (page);

	pos = gtk_notebook_get_n_pages (GTK_NOTEBOOK (window->priv->notebook)) - 1;
	if (gtk_notebook_get_current_page (GTK_NOTEBOOK (window->priv->notebook)) == pos) {
		window->priv->current_page = page;
		gnc_plugin_page_merge_actions (page, window->ui_merge);
		gnc_plugin_page_selected (page);
	} else {
		gtk_notebook_set_current_page (GTK_NOTEBOOK (window->priv->notebook),
					       pos);
	}
}

void
gnc_main_window_close_page (GncMainWindow *window,
			    GncPluginPage *page)
{
	gboolean found = FALSE;
	guint i;
	GtkWidget *child;

	for (i = 0; i < gtk_notebook_get_n_pages (GTK_NOTEBOOK (window->priv->notebook)); i++) {
		child = gtk_notebook_get_nth_page (GTK_NOTEBOOK (window->priv->notebook), i);
		if (g_object_get_data (G_OBJECT (child), "page-plugin") == page) {
			found = TRUE;
			break;
		}
	}

	if (!found)
		return;

	if (window->priv->current_page == page) {
		gnc_plugin_page_unmerge_actions (page, window->ui_merge);
		gnc_plugin_page_unselected (page);

		window->priv->current_page = NULL;
	}

	gtk_notebook_remove_page (GTK_NOTEBOOK (window->priv->notebook), i);

	gnc_plugin_page_removed (page);

	egg_menu_merge_ensure_update (window->ui_merge);

	/* */
	i = gtk_notebook_get_current_page (GTK_NOTEBOOK (window->priv->notebook));
	child = gtk_notebook_get_nth_page (GTK_NOTEBOOK (window->priv->notebook), i);
	if (child == NULL) {
		return;
	}

	page = g_object_get_data (G_OBJECT (child), "page-plugin");

	window->priv->current_page = page;

	if (page != NULL) {
		gnc_plugin_page_merge_actions (page, window->ui_merge);
		gnc_plugin_page_selected (page);
	}
}

GncPluginPage *
gnc_main_window_get_current_page (GncMainWindow *window)
{
	return window->priv->current_page;
}

void
gnc_main_window_merge_actions (GncMainWindow *window,
			       const gchar *group_name,
			       EggActionGroupEntry *actions,
			       guint n_actions,
			       const gchar *ui_file,
			       gpointer user_data)
{
	GncMainWindowActionData *data;
	guint i;
	MergedActionEntry *entry;

	g_return_if_fail (GNC_IS_MAIN_WINDOW (window));
	g_return_if_fail (group_name != NULL);
	g_return_if_fail (actions != NULL);
	g_return_if_fail (n_actions > 0);
	g_return_if_fail (ui_file != NULL);

	data = g_new0 (GncMainWindowActionData, 1);
	data->window = window;
	data->data = user_data;

	for (i = 0; i < n_actions; i++) {
		actions[i].user_data = data;
	}

	entry = g_new0 (MergedActionEntry, 1);
	entry->action_group = egg_action_group_new (group_name);
	egg_action_group_add_actions (entry->action_group, actions, n_actions);
	egg_menu_merge_insert_action_group (window->ui_merge, entry->action_group, 0);
	entry->merge_id = egg_menu_merge_add_ui_from_file (window->ui_merge, ui_file, NULL);
	egg_menu_merge_ensure_update (window->ui_merge);

	g_hash_table_insert (window->priv->merged_actions_table, g_strdup (group_name), entry);
}

void
gnc_main_window_unmerge_actions (GncMainWindow *window,
				 const gchar *group_name)
{
	MergedActionEntry *entry;

	g_return_if_fail (GNC_IS_MAIN_WINDOW (window));
	g_return_if_fail (group_name != NULL);

	entry = g_hash_table_lookup (window->priv->merged_actions_table, group_name);

	if (entry == NULL)
		return;

	egg_menu_merge_remove_action_group (window->ui_merge, entry->action_group);
	egg_menu_merge_remove_ui (window->ui_merge, entry->merge_id);
	egg_menu_merge_ensure_update (window->ui_merge);

	g_hash_table_remove (window->priv->merged_actions_table, group_name);
}

EggActionGroup *
gnc_main_window_get_action_group  (GncMainWindow *window,
				   const gchar *group_name)
{
	MergedActionEntry *entry;

	g_return_val_if_fail (GNC_IS_MAIN_WINDOW (window), NULL);
	g_return_val_if_fail (group_name != NULL, NULL);

	entry = g_hash_table_lookup (window->priv->merged_actions_table, group_name);

	if (entry == NULL)
		return NULL;

	return entry->action_group;
}


static void
gnc_main_window_class_init (GncMainWindowClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);

	parent_class = g_type_class_peek_parent (klass);

	object_class->finalize = gnc_main_window_finalize;
	object_class->dispose = gnc_main_window_dispose;
}

static void
gnc_main_window_init (GncMainWindow *window)
{
	window->priv = g_new0 (GncMainWindowPrivate, 1);

	window->priv->merged_actions_table = g_hash_table_new_full (g_str_hash, g_str_equal, g_free, g_free);

	gnc_main_window_setup_window (window);
}

static void
gnc_main_window_finalize (GObject *object)
{
	GncMainWindow *window;

	g_return_if_fail (object != NULL);
	g_return_if_fail (GNC_IS_MAIN_WINDOW (object));

	window = GNC_MAIN_WINDOW (object);

	g_return_if_fail (window->priv != NULL);

	g_hash_table_destroy (window->priv->merged_actions_table);
	g_free (window->priv);

	G_OBJECT_CLASS (parent_class)->finalize (object);
}

static void
gnc_main_window_dispose (GObject *object)
{
	GncMainWindow *window;

	g_return_if_fail (object != NULL);
	g_return_if_fail (GNC_IS_MAIN_WINDOW (object));

	window = GNC_MAIN_WINDOW (object);

	G_OBJECT_CLASS (parent_class)->dispose (object);
}

static void
gnc_main_window_add_plugin (gpointer plugin,
			    gpointer window)
{
	g_return_if_fail (GNC_IS_MAIN_WINDOW (window));
	g_return_if_fail (GNC_IS_PLUGIN (plugin));

	gnc_plugin_add_to_window (GNC_PLUGIN (plugin),
				  GNC_MAIN_WINDOW (window));
}

static void
gnc_main_window_setup_window (GncMainWindow *window)
{
	GtkWidget *main_vbox;
	guint i;
	GncPluginManager *manager;
	GList *plugins;

	/* Create widgets and add them to the window */
	main_vbox = gtk_vbox_new (FALSE, 0);
	gtk_widget_show (main_vbox);
	gtk_container_add (GTK_CONTAINER (window), main_vbox);

	window->priv->menu_dock = gtk_vbox_new (FALSE, 0);
	gtk_widget_show (window->priv->menu_dock);
	gtk_box_pack_start (GTK_BOX (main_vbox), window->priv->menu_dock,
			    FALSE, TRUE, 0);

	window->priv->notebook = gtk_notebook_new ();
	gtk_widget_show (window->priv->notebook);
	g_signal_connect (G_OBJECT (window->priv->notebook), "change-current-page",
			  G_CALLBACK (gnc_main_window_change_current_page), window);
	g_signal_connect (G_OBJECT (window->priv->notebook), "switch-page",
			  G_CALLBACK (gnc_main_window_switch_page), window);
	gtk_box_pack_start (GTK_BOX (main_vbox), window->priv->notebook,
			    TRUE, TRUE, 0);

	window->priv->statusbar = gtk_statusbar_new ();
	gtk_widget_show (window->priv->statusbar);
	gtk_box_pack_start (GTK_BOX (main_vbox), window->priv->statusbar,
			    FALSE, TRUE, 0);

	/* Create menu and toolbar information */
	for (i = 0; i < gnc_menu_n_entries; i++) {
		gnc_menu_entries[i].user_data = window;
	}

	window->ui_merge = egg_menu_merge_new ();

	window->priv->action_group = egg_action_group_new ("MainWindowActions");
	egg_action_group_add_actions (window->priv->action_group, gnc_menu_entries,
				      gnc_menu_n_entries);
	egg_menu_merge_insert_action_group (window->ui_merge, window->priv->action_group, 0);

	g_signal_connect (G_OBJECT (window->ui_merge), "add_widget",
			  G_CALLBACK (gnc_main_window_add_widget), window);
	egg_menu_merge_add_ui_from_file (window->ui_merge, GNC_UI_DIR "/gnc-main-window-ui.xml", NULL);
	gtk_window_add_accel_group (GTK_WINDOW (window), window->ui_merge->accel_group);
	egg_menu_merge_ensure_update (window->ui_merge);

	/* GncPluginManager stuff */
	manager = gnc_plugin_manager_get ();
	plugins = gnc_plugin_manager_get_plugins (manager);
	g_signal_connect (G_OBJECT (manager), "plugin-added",
			  G_CALLBACK (gnc_main_window_plugin_added), window);
	g_signal_connect (G_OBJECT (manager), "plugin-removed",
			  G_CALLBACK (gnc_main_window_plugin_removed), window);
	g_list_foreach (plugins, gnc_main_window_add_plugin, window);
	g_list_free (plugins);
}

static void
gnc_main_window_add_widget (EggMenuMerge *merge,
			    GtkWidget *widget,
			    GncMainWindow *window)
{
	if (EGG_IS_TOOLBAR (widget)) {
		window->priv->toolbar_dock = widget;
	}

	gtk_box_pack_start (GTK_BOX (window->priv->menu_dock), widget, FALSE, FALSE, 0);
	gtk_widget_show (widget);
}

static void
gnc_main_window_switch_page (GtkNotebook *notebook,
			     GtkNotebookPage *notebook_page,
			     gint pos,
			     GncMainWindow *window)
{
	/* FIXME Use switch_page instead of change_current_page */
}

static void
gnc_main_window_change_current_page (GtkNotebook *notebook,
				     gint pos,
				     GncMainWindow *window)
{
	GtkWidget *child;
	GncPluginPage *page;

	g_return_if_fail (GNC_IS_MAIN_WINDOW (window));

	if (window->priv->current_page != NULL) {
		gnc_plugin_page_unmerge_actions (window->priv->current_page,
						 window->ui_merge);
		gnc_plugin_page_unselected (window->priv->current_page);
	}

	child = gtk_notebook_get_nth_page (notebook, pos);
	page = g_object_get_data (G_OBJECT (child), "page-plugin");

	window->priv->current_page = page;

	if (page != NULL) {
		gnc_plugin_page_merge_actions (page,  window->ui_merge);
		gnc_plugin_page_selected (page);
	}
}

static void
gnc_main_window_plugin_added (GncPlugin *manager,
			      GncPlugin *plugin,
			      GncMainWindow *window)
{
	g_return_if_fail (GNC_IS_MAIN_WINDOW (window));
	g_return_if_fail (GNC_IS_PLUGIN (plugin));

	gnc_plugin_add_to_window (plugin, window);
}

static void
gnc_main_window_plugin_removed (GncPlugin *manager,
				GncPlugin *plugin,
				GncMainWindow *window)
{
	g_return_if_fail (GNC_IS_MAIN_WINDOW (window));
	g_return_if_fail (GNC_IS_PLUGIN (plugin));

	gnc_plugin_remove_from_window (plugin, window);
}


/* Command callbacks */
static void
gnc_main_window_cmd_file_new (EggAction *action, GncMainWindow *window)
{
	gnc_file_new ();
	/* FIXME GNOME 2 Port (update the title etc.) */
}

static void
gnc_main_window_cmd_file_open (EggAction *action, GncMainWindow *window)
{
	gnc_file_open ();
	/* FIXME GNOME 2 Port (update the title etc.) */
}

static void
gnc_main_window_cmd_file_open_new_window (EggAction *action, GncMainWindow *window)
{
	GncMainWindow *new_window;
	const gchar *name;
	gchar *uri;
	GncPlugin *plugin;
	GncPluginPage *page;

	/* FIXME GNOME 2 Port (Open the correct view in the new window) */

	new_window = gnc_main_window_new ();

	if (window->priv->current_page != NULL) {
		name = gnc_plugin_page_get_plugin_name (window->priv->current_page);
		uri = gnc_plugin_page_get_uri (window->priv->current_page);
		plugin = gnc_plugin_manager_get_plugin (gnc_plugin_manager_get (), name);
		page = gnc_plugin_create_page (plugin, uri);

		if (page != NULL) {
			gnc_main_window_open_page (new_window, page);
			gnc_main_window_close_page (window, window->priv->current_page);
		}
	}

	gtk_widget_show (GTK_WIDGET (new_window));
}

static void
gnc_main_window_cmd_file_save (EggAction *action, GncMainWindow *window)
{
	gnc_file_save ();
	/* FIXME GNOME 2 Port (update the title etc.) */
}

static void
gnc_main_window_cmd_file_save_as (EggAction *action, GncMainWindow *window)
{
	gnc_file_save_as ();
	/* FIXME GNOME 2 Port (update the title etc.) */
}

static void
gnc_main_window_cmd_file_export_accounts (EggAction *action, GncMainWindow *window)
{
	gnc_file_export_file (NULL);
	/* FIXME GNOME 2 Port (update the title etc.) */
	/* gnc_refresh_main_window_info (); */
}

static void
gnc_main_window_cmd_file_print (EggAction *action, GncMainWindow *window)
{
}

static void
gnc_main_window_cmd_file_close (EggAction *action, GncMainWindow *window)
{
	if (window->priv->current_page != NULL) {
		gnc_main_window_close_page (window, window->priv->current_page);
	}
}

static void
gnc_main_window_cmd_file_quit (EggAction *action, GncMainWindow *window)
{
	gnc_shutdown (0);
}

static void
gnc_main_window_cmd_edit_cut (EggAction *action, GncMainWindow *window)
{
}

static void
gnc_main_window_cmd_edit_copy (EggAction *action, GncMainWindow *window)
{
}

static void
gnc_main_window_cmd_edit_paste (EggAction *action, GncMainWindow *window)
{
}

static void
gnc_main_window_cmd_edit_preferences (EggAction *action, GncMainWindow *window)
{
	gnc_show_options_dialog ();
}

static void
gnc_main_window_cmd_edit_tax_options (EggAction *action, GncMainWindow *window)
{
	gnc_tax_info_dialog (GTK_WIDGET (window));
}

static void
gnc_main_window_cmd_view_refresh (EggAction *action, GncMainWindow *window)
{
}

static void
gnc_main_window_cmd_view_toolbar (EggAction *action, GncMainWindow *window)
{
	if (EGG_TOGGLE_ACTION (action)->active) {
		gtk_widget_show (window->priv->toolbar_dock);
	} else {
		gtk_widget_hide (window->priv->toolbar_dock);
	}
}

static void
gnc_main_window_cmd_view_summary (EggAction *action, GncMainWindow *window)
{
}

static void
gnc_main_window_cmd_view_statusbar (EggAction *action, GncMainWindow *window)
{
	if (EGG_TOGGLE_ACTION (action)->active) {
		gtk_widget_show (window->priv->statusbar);
	} else {
		gtk_widget_hide (window->priv->statusbar);
	}
}

static void
gnc_main_window_cmd_actions_scheduled_transaction_editor (EggAction *action, GncMainWindow *window)
{
	gnc_ui_scheduled_xaction_dialog_create ();
}

static void
gnc_main_window_cmd_actions_since_last_run (EggAction *action, GncMainWindow *window)
{
	gint ret;
	const char *nothing_to_do_msg =
		_( "There are no Scheduled Transactions to be entered at this time." );
	
	ret = gnc_ui_sxsincelast_dialog_create ();
	if ( ret == 0 ) {
		gnc_info_dialog (nothing_to_do_msg);
	} else if ( ret < 0 ) {
		gnc_info_dialog (ngettext
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
gnc_main_window_cmd_actions_mortgage_loan (EggAction *action, GncMainWindow *window)
{
	gnc_ui_sx_loan_druid_create ();
}

static void
gnc_main_window_cmd_tools_general_ledger (EggAction *action, GncMainWindow *window)
{
	GNCLedgerDisplay *ld;
	GNCSplitReg *gsr;
	RegWindow *regData;

	ld = gnc_ledger_display_gl ();
	gsr = gnc_ledger_display_get_user_data (ld);
	if (!gsr) {
		regData = regWindowLedger (ld);
		gnc_register_raise (regData);
	} else {
		gnc_split_reg_raise (gsr);
	}
}

static void
gnc_main_window_cmd_tools_price_editor (EggAction *action, GncMainWindow *window)
{
	gnc_prices_dialog (NULL);
}

static void
gnc_main_window_cmd_tools_commodity_editor (EggAction *action, GncMainWindow *window)
{
	gnc_commodities_dialog (NULL);
}

static void
gnc_main_window_cmd_tools_financial_calculator (EggAction *action, GncMainWindow *window)
{
	gnc_ui_fincalc_dialog_create();
}

static void
gnc_main_window_cmd_tools_find_transactions (EggAction *action, GncMainWindow *window)
{
	gnc_ui_find_transactions_dialog_create (NULL);
}

static void
gnc_main_window_cmd_help_tutorial (EggAction *action, GncMainWindow *window)
{
	/* FIXME GNOME 2 Port (use GNOME help) */
	helpWindow(NULL, NULL, HH_MAIN);
}

static void
gnc_main_window_cmd_help_totd (EggAction *action, GncMainWindow *window)
{
	GtkWidget *dialog;

	dialog = gnc_totd_dialog_new (GTK_WINDOW (window));
	gtk_dialog_run (GTK_DIALOG (dialog));
	gtk_widget_destroy (dialog);
}

static void
gnc_main_window_cmd_help_contents (EggAction *action, GncMainWindow *window)
{
	/* FIXME GNOME 2 Port (use GNOME help) */
	helpWindow(NULL, NULL, HH_HELP);
}

static void
gnc_main_window_cmd_help_about (EggAction *action, GncMainWindow *window)
{
	GtkWidget *about;
	const gchar *message = _("The GnuCash personal finance manager.\n"
				 "The GNU way to manage your money!\n"
				 "http://www.gnucash.org/");
	const gchar *copyright = "\xc2\xa9 1998-2002 Linas Vepstas";
	const gchar *authors[] = {
		"Derek Atkins <derek@ihtfp.com>",
		"Rob Browning <rlb@cs.utexas.edu>",
		"Bill Gribble <grib@billgribble.com>",
		"David Hampton <hampton@employees.org>",
		"James LewisMoss <dres@debian.org>",
		"Robert Graham Merkel <rgmerk@mira.net>",
		"Dave Peticolas <dave@krondo.com>",
		"Joshua Sled <jsled@asynchronous.org>",
		"Christian Stimming <stimming@tuhh.de>",
		"Linas Vepstas <linas@linas.org>",
		NULL
	};
	const gchar *documenters[] = {
		NULL
	};
	const gchar *translator_credits = _("translator_credits");
	gchar *version;

	if (GNUCASH_MINOR_VERSION % 2) {
		version = g_strdup_printf("%s (built %s)", VERSION, GNUCASH_BUILD_DATE);
	} else {
		version = strdup(VERSION);
	}

	about = gnome_about_new ("GnuCash", version, copyright, message, authors, documenters,
				 strcmp (translator_credits, "translator_credits") != 0 ? translator_credits : NULL,
				 NULL);
	g_free(version);

	gtk_dialog_run (GTK_DIALOG (about));
}
