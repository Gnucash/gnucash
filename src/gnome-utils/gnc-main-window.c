/* 
 * gnc-main-window.c -- GtkWindow which represents the
 *	GnuCash main window.
 *
 * Copyright (C) 2003 Jan Arne Petersen <jpetersen@uni-bonn.de>
 * Copyright (C) 2003,2005 David Hampton <hampton@employees.org>
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

/** @addtogroup Windows
    @{ */
/** @addtogroup GncMainWindow Main Window functions.
    @{ */
/** @file gnc-main-window.c
    @brief Functions for adding content to a window.
    @author Copyright (C) 2003 Jan Arne Petersen <jpetersen@uni-bonn.de>
    @author Copyright (C) 2003,2005 David Hampton <hampton@employees.org>
*/

#include "config.h"

#include <gdk/gdkpixbuf.h>
#include <gtk/gtk.h>

#include "gnc-plugin.h"
#include "gnc-plugin-manager.h"
#include "gnc-main-window.h"

#include "dialog-preferences.h"
#include "dialog-reset-warnings.h"
#include "dialog-transfer.h"
#include "dialog-utils.h"
#include "gnc-component-manager.h"
#include "gnc-engine.h"
#include "gnc-file.h"
#include "gnc-gnome-utils.h"
#include "gnc-gobject-utils.h"
#include "gnc-gui-query.h"
#include "gnc-session.h"
#include "gnc-ui.h"
#include "gnc-version.h"
#include "gnc-window.h"
#include "messages.h"
#include "gnc-gconf-utils.h"
// +JSLED
#include "gnc-html.h"

enum {
  PAGE_ADDED,
  PAGE_CHANGED,
  LAST_SIGNAL
};


#define PLUGIN_PAGE_IMMUTABLE    "page-immutable"
#define PLUGIN_PAGE_CLOSE_BUTTON "close-button"

#define KEY_SHOW_CLOSE_BUTTON	"tab_close_buttons"

/** Static Globals *******************************************************/
static QofLogModule log_module = GNC_MOD_GUI;
static GList *active_windows = NULL;

/** Declarations *********************************************************/
static void gnc_main_window_class_init (GncMainWindowClass *klass);
static void gnc_main_window_init (GncMainWindow *window, GncMainWindowClass *klass);
static void gnc_main_window_finalize (GObject *object);
static void gnc_main_window_destroy (GtkObject *object);

static void gnc_main_window_setup_window (GncMainWindow *window);
static void gnc_window_main_window_init (GncWindowIface *iface);

/* Callbacks */
static void gnc_main_window_add_widget (GtkUIManager *merge, GtkWidget *widget, GncMainWindow *window);
static void gnc_main_window_switch_page (GtkNotebook *notebook, GtkNotebookPage *notebook_page, gint pos, GncMainWindow *window);
static void gnc_main_window_plugin_added (GncPlugin *manager, GncPlugin *plugin, GncMainWindow *window);
static void gnc_main_window_plugin_removed (GncPlugin *manager, GncPlugin *plugin, GncMainWindow *window);

/* Command callbacks */
static void gnc_main_window_cmd_file_properties (GtkAction *action, GncMainWindow *window);
static void gnc_main_window_cmd_file_close (GtkAction *action, GncMainWindow *window);
static void gnc_main_window_cmd_file_quit (GtkAction *action, GncMainWindow *window);
static void gnc_main_window_cmd_edit_preferences (GtkAction *action, GncMainWindow *window);
static void gnc_main_window_cmd_view_refresh (GtkAction *action, GncMainWindow *window);
static void gnc_main_window_cmd_view_toolbar (GtkAction *action, GncMainWindow *window);
static void gnc_main_window_cmd_view_summary (GtkAction *action, GncMainWindow *window);
static void gnc_main_window_cmd_view_statusbar (GtkAction *action, GncMainWindow *window);
static void gnc_main_window_cmd_actions_reset_warnings (GtkAction *action, GncMainWindow *window);
static void gnc_main_window_cmd_window_new (GtkAction *action, GncMainWindow *window);
static void gnc_main_window_cmd_window_move_page (GtkAction *action, GncMainWindow *window);
static void gnc_main_window_cmd_window_raise (GtkAction *action, GtkRadioAction *current, GncMainWindow *window);
static void gnc_main_window_cmd_help_tutorial (GtkAction *action, GncMainWindow *window);
static void gnc_main_window_cmd_help_contents (GtkAction *action, GncMainWindow *window);
static void gnc_main_window_cmd_help_about (GtkAction *action, GncMainWindow *window);

static void gnc_main_window_cmd_test( GtkAction *action, GncMainWindow *window );

static void do_popup_menu(GncPluginPage *page, GdkEventButton *event);
static gboolean gnc_main_window_popup_menu_cb (GtkWidget *widget, GncPluginPage *page);


struct GncMainWindowPrivate
{
	GtkWidget *menu_dock;
	GtkWidget *toolbar_dock;
	GtkWidget *notebook;
	GtkWidget *statusbar;
	GtkWidget *progressbar;

	GtkWidget *summarybar_dock;
	gboolean   show_summarybar;

	GtkActionGroup *action_group;

	GncPluginPage *current_page;
	GList *installed_pages;
	gint event_handler_id;


	GHashTable *merged_actions_table;
};

typedef struct {
	guint merge_id;
	GtkActionGroup *action_group;
} MergedActionEntry;

static guint main_window_signals[LAST_SIGNAL] = { 0 };

static GtkActionEntry gnc_menu_actions [] =
{
	/* Toplevel */

	{ "FileAction", NULL, N_("_File"), NULL, NULL, NULL, },
	{ "EditAction", NULL, N_("_Edit"), NULL, NULL, NULL },
	{ "ViewAction", NULL, N_("_View"), NULL, NULL, NULL },
	{ "ActionsAction", NULL, N_("_Actions"), NULL, NULL, NULL },
	{ "TransactionAction", NULL, N_("_Transaction"), NULL, NULL, NULL },
	{ "ReportsAction", NULL, N_("_Reports"), NULL, NULL, NULL },
	{ "ToolsAction", NULL, N_("_Tools"), NULL, NULL, NULL },
	{ "ExtensionsAction", NULL, N_("E_xtensions"), NULL, NULL, NULL },
	{ "WindowsAction", NULL, N_("_Windows"), NULL, NULL, NULL },
	{ "HelpAction", NULL, N_("_Help"), NULL, NULL, NULL },
	{ "MiscAction", NULL, N_("_Misc"), NULL, NULL, NULL },

	/* File menu */

	{ "FileNewMenuAction", GTK_STOCK_NEW, N_("_New"), "", NULL, NULL },
	{ "FileOpenMenuAction", GTK_STOCK_OPEN, N_("_Open"), "", NULL, NULL },
	{ "FileImportAction", NULL, N_("_Import"), NULL, NULL, NULL },
	{ "FileExportAction", NULL, N_("_Export"), NULL, NULL, NULL },
	{ "FilePrintAction", GTK_STOCK_PRINT, N_("_Print..."), NULL, NULL, NULL },
	{ "FilePropertiesAction", GTK_STOCK_PROPERTIES, N_("Proper_ties"), NULL,
	  NULL,
	  G_CALLBACK (gnc_main_window_cmd_file_properties) },
	{ "FileCloseAction", GTK_STOCK_CLOSE, N_("_Close"), NULL,
	  NULL,
	  G_CALLBACK (gnc_main_window_cmd_file_close) },
	{ "FileQuitAction", GTK_STOCK_QUIT, N_("_Quit"), NULL,
	  NULL,
	  G_CALLBACK (gnc_main_window_cmd_file_quit) },

	/* Edit menu */

	{ "EditCutAction", GTK_STOCK_CUT, N_("Cu_t"), NULL, NULL, NULL },
	{ "EditCopyAction", GTK_STOCK_COPY, N_("_Copy"), NULL, NULL, NULL },
	{ "EditPasteAction", GTK_STOCK_PASTE, N_("_Paste"), NULL, NULL, NULL },
	{ "EditPreferencesAction", GTK_STOCK_PREFERENCES, N_("Pr_eferences"), NULL,
	  NULL,
	  G_CALLBACK (gnc_main_window_cmd_edit_preferences) },

	/* View menu */

	{ "ViewRefreshAction", GTK_STOCK_REFRESH, N_("_Refresh"), "<control>r",
	  N_("Refresh this window"),
	  G_CALLBACK (gnc_main_window_cmd_view_refresh) },

	/* Actions menu */

	{ "ScrubMenuAction", NULL, N_("_Check & Repair"), NULL, NULL, NULL },
	{ "ActionsForgetWarningsAction", NULL, N_("_Reset Warnings..."), NULL,
	  N_("Reset the state of all warning message so they will be shown again."),
	  G_CALLBACK (gnc_main_window_cmd_actions_reset_warnings) },

	/* Windows menu */

	{ "WindowNewAction", NULL, N_("_New Window"), NULL,
	  N_("Open a new top-level GnuCash window."),
	  G_CALLBACK (gnc_main_window_cmd_window_new) },
	{ "WindowMovePageAction", NULL, N_("New Window with _Page"), NULL,
	  N_("Move the current page to a new top-level GnuCash window."),
	  G_CALLBACK (gnc_main_window_cmd_window_move_page) },

	/* Help menu */

	{ "HelpTutorialAction", GNOME_STOCK_BOOK_BLUE, N_("Tutorial and Concepts _Guide"), NULL,
	  N_("Open the GnuCash Tutorial"),
	  G_CALLBACK (gnc_main_window_cmd_help_tutorial) },
	{ "HelpContentsAction", GTK_STOCK_HELP, N_("_Contents"), NULL,
	  N_("Open the GnuCash Help"),
	  G_CALLBACK (gnc_main_window_cmd_help_contents) },
	{ "HelpAboutAction", GNOME_STOCK_ABOUT, N_("_About"), NULL,
	  NULL,
	  G_CALLBACK (gnc_main_window_cmd_help_about) },

        /* Misc menu */

        { "MiscTestAction", NULL, N_("TEST"), NULL,
          N_("Testing stuff"), G_CALLBACK (gnc_main_window_cmd_test) },

};
static guint gnc_menu_n_actions = G_N_ELEMENTS (gnc_menu_actions);

static GtkToggleActionEntry toggle_actions [] =
{
	{ "ViewToolbarAction", NULL, N_("_Toolbar"), NULL,
	  N_("Show/hide the toolbar on this window"),
	  G_CALLBACK (gnc_main_window_cmd_view_toolbar), TRUE },
	{ "ViewSummaryAction", NULL, N_("Su_mmary Bar"), NULL,
	  N_("Show/hide the summary bar on this window"),
	  G_CALLBACK (gnc_main_window_cmd_view_summary), TRUE },
	{ "ViewStatusbarAction", NULL, N_("Stat_us Bar"), NULL,
	  N_("Show/hide the status bar on this window"),
	  G_CALLBACK (gnc_main_window_cmd_view_statusbar), TRUE },
};
static guint n_toggle_actions = G_N_ELEMENTS (toggle_actions);

static GtkRadioActionEntry radio_entries [] =
{
	{ "Window0Action", NULL, N_("Window _1"), NULL, NULL, 0 },
	{ "Window1Action", NULL, N_("Window _2"), NULL, NULL, 1 },
	{ "Window2Action", NULL, N_("Window _3"), NULL, NULL, 2 },
	{ "Window3Action", NULL, N_("Window _4"), NULL, NULL, 3 },
	{ "Window4Action", NULL, N_("Window _5"), NULL, NULL, 4 },
	{ "Window5Action", NULL, N_("Window _6"), NULL, NULL, 5 },
	{ "Window6Action", NULL, N_("Window _7"), NULL, NULL, 6 },
	{ "Window7Action", NULL, N_("Window _8"), NULL, NULL, 7 },
	{ "Window8Action", NULL, N_("Window _9"), NULL, NULL, 8 },
	{ "Window9Action", NULL, N_("Window _0"), NULL, NULL, 9 },
};
static guint n_radio_entries = G_N_ELEMENTS (radio_entries);


/** The following are in the main window so they will always be
 *  present in the menu structure, but they are never sensitive.
 *  These actions should be overridden in child windows where they
 *  have meaning. */
static const gchar *always_insensitive_actions[] = {
	"FilePrintAction",
	"EditCutAction",
	"EditCopyAction",
	"EditPasteAction",
	NULL
};

/** If a page is flagged as immutable, then the following actions
 *  cannot be peformed on that page. */
static const gchar *immutable_page_actions[] = {
	"FileCloseAction",
	NULL
};

/** The following actions can only be performed if there are multiple
 *  pages in a window. */
static const gchar *multiple_page_actions[] = {
	"WindowMovePageAction",
	NULL
};

static GObjectClass *parent_class = NULL;

static GQuark window_type = 0;

/************************************************************
 *                                                          *
 ************************************************************/

static void
gnc_main_window_save_window (GncMainWindow *window, gpointer session)
{
  DEBUG("window %p", window);
}

static void
gnc_main_window_shutdown (gpointer session, gpointer user_data)
{
  DEBUG("session %p (%s)", session, qof_session_get_url (session));
  g_list_foreach (active_windows, (GFunc)gnc_main_window_save_window, session);
}


/** See if the page already exists.  For each open window, look
 *  through the list of pages installed in that window and see if the
 *  specified page is there.
 *
 *  @internal
 *
 *  @param page The page to search for.
 *
 *  @return TRUE if the page is present in the window, FALSE otherwise.
 */
static gboolean
gnc_main_window_page_exists (GncPluginPage *page)
{
	GncMainWindow *window;
	GList *walker;

	for (walker = active_windows; walker; walker = g_list_next(walker)) {
	  window = walker->data;
	  if (g_list_find(window->priv->installed_pages, page)) {
	    return TRUE;
	  }
	}
	return FALSE;
}


/** This function prompts the user to save the file with a dialog that
 *  follows the HIG guidelines.
 *
 *  @internal
 *
 *  @returns This function returns TRUE if the user clicked the Cancel
 *  button.  It returns FALSE if the closing of the window should
 *  continue.
 */
static gboolean
gnc_main_window_prompt_for_save (GtkWidget *window)
{
  QofSession *session;
  QofBook *book;
  GtkWidget *dialog;
  gint response;
  const gchar *filename, *tmp;
#ifdef HIG_COMPLIANT
  gint oldest_change, minutes;
#endif

  session = qof_session_get_current_session();
  book = qof_session_get_book(session);
  filename = qof_session_get_file_path(session);
  if (filename == NULL)
    filename = _("<unknown>");
  if ((tmp = rindex(filename, '/')) != NULL)
    filename = tmp + 1;

  /*
   * *** THIS DIALOG IS NOT HIG COMPLIANT. ***
   *
   * According to the HIG, the secondary context should include
   * context about the number of changes that will be lost (either in
   * time or a count).  While it is possible to simply provide the
   * time since the last save, that doesn't appear too usefule.  If
   * the user has had Gnucash open for hours in the background, but
   * only made a change in the last few minutes, then telling them
   * they will lose hours work of work is wring.  The QOF code needs
   * to be modified to provide better timing information.  The best
   * case scenario would be if QOF could provide a timestamp of the
   * oldest unsaved change.
   */
#ifdef HIG_COMPLIANT
  oldest_change = qof_book_time_changed(book);
  minutes = (time() - oldest_change) / 60 + 1;
  dialog =
    gtk_message_dialog_new_with_markup (GTK_WINDOW(window),
					GTK_DIALOG_MODAL,
					GTK_MESSAGE_WARNING,
					GTK_BUTTONS_NONE,
					_("<b>Save changes to file %s before "
					  "closing?</b>\n\nIf you don't save, "
					  "changes from the past %d minutes "
					  "will be discarded."),
					filename, minutes);
#else
  dialog =
    gtk_message_dialog_new_with_markup (GTK_WINDOW(window),
					GTK_DIALOG_MODAL,
					GTK_MESSAGE_WARNING,
					GTK_BUTTONS_NONE,
					_("<b>Save changes to file %s before "
					  "closing?</b>\n\nIf you don't save, "
					  "changes will be discarded."),
					filename);
#endif

  gtk_dialog_add_buttons(GTK_DIALOG(dialog),
			 "Close _without Saving", GTK_RESPONSE_CLOSE,
			 GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
			 GTK_STOCK_SAVE, GTK_RESPONSE_APPLY,
			 NULL);
  gtk_dialog_set_default_response(GTK_DIALOG(dialog), GTK_RESPONSE_APPLY);
  response = gtk_dialog_run (GTK_DIALOG (dialog));
  gtk_widget_destroy(dialog);

  switch (response) {
    case GTK_RESPONSE_APPLY:
      gnc_file_save();
      return FALSE;

    case GTK_RESPONSE_CLOSE:
      qof_book_mark_saved(book);
      return FALSE;

    default:
      return TRUE;
  }
}


static gboolean
gnc_main_window_delete_event (GtkWidget *window,
			      GdkEvent *event,
			      gpointer user_data)
{
  QofSession *session;
  GtkWidget *dialog;
  gint response;

  if (g_list_length(active_windows) > 1)
    return FALSE;

  session = qof_session_get_current_session();
  if (qof_book_not_saved(qof_session_get_book(session))) {
    if (!gnc_main_window_prompt_for_save(GTK_WIDGET(window))) {
      /* Tell gnucash to shutdown cleanly */
      g_idle_add((GSourceFunc)gnc_shutdown, 0);
    }
    /* Cancel the window deletion. It'll happen on the just queued shutdown. */
    return TRUE;
  }

  dialog = gtk_message_dialog_new_with_markup (GTK_WINDOW(window),
				   GTK_DIALOG_MODAL,
				   GTK_MESSAGE_WARNING,
				   GTK_BUTTONS_NONE,
				   _("<b>Quit Gnucash?</b>\n\n"
				     "You are attempting to close the last "
				     "Gnucash window.  Doing so will quit the "
				     "application.  Are you sure that this is "
				     "what you want to do?"));

  gtk_dialog_add_buttons(GTK_DIALOG(dialog),
			 GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
			 GTK_STOCK_QUIT, GTK_RESPONSE_OK,
			 NULL);
  gtk_dialog_set_default_response(GTK_DIALOG(dialog), GTK_RESPONSE_OK);
  response = gtk_dialog_run (GTK_DIALOG (dialog));
  gtk_widget_destroy(dialog);

  if (response == GTK_RESPONSE_OK) {
    /* Tell gnucash to shutdown cleanly */
    g_idle_add((GSourceFunc)gnc_shutdown, 0);
  }
  return TRUE;
}


/** This function handles any event notifications from the engine.
 *  The only event it currently cares about is the deletion of a book.
 *  When a book is deleted, it runs through all installed pages
 *  looking for pages that reference the just (about to be?) deleted
 *  book.  It closes any page it finds so there are no dangling
 *  references to the book.
 *
 *  @internal
 *
 *  @param entity     The guid the item being added, deleted, etc.
 *
 *  @param type       The type of the item being added, deleted, etc. This
 *                    function only cares about a type of GNC_ID_BOOK.
 *
 *  @param event_type The type of the event.  This function only cares
 *                    about an event type of GNC_EVENT_DESTROY.
 *
 *  @param user_data  A pointer to the window data structure.
 */
static void
gnc_main_window_event_handler (GUID *entity, QofIdType type,
			       GNCEngineEventType event_type,
			       gpointer user_data)
{
	GncMainWindow *window;
	GncPluginPage *page;
	GList *item, *next;

	/* hard failures */
	g_return_if_fail(GNC_IS_MAIN_WINDOW(user_data));

	/* soft failures */
	if (safe_strcmp(type, GNC_ID_BOOK) != 0)
	  return;
	if (event_type !=  GNC_EVENT_DESTROY)
	  return;

	ENTER("entity %p of type %s, event %d, window %p",
	      entity, type, event_type, user_data);
	window = GNC_MAIN_WINDOW(user_data);

        /* This is not a typical list iteration.  We're removing while
         * we iterate, so we have to cache the 'next' pointer before
         * executing any code in the loop. */
	for (item = window->priv->installed_pages; item; item = next) {
	  next = g_list_next(item);
	  page = GNC_PLUGIN_PAGE(item->data);
	  if (gnc_plugin_page_has_book (page, entity))
              gnc_main_window_close_page (page);
	}
	LEAVE(" ");
}


/** Generate a title for this window based upon the Gnome Human
 *  Interface Guidelines, v2.0.  This title will be used as both the
 *  window title and the title of the "Window" menu item associated
 *  with the window.
 *
 *  @param window The window whose title should be generated.
 *
 *  @return The title for the window.  It is the callers
 *  responsibility to free this string.
 *
 *  @internal
 */
static gchar *
gnc_main_window_generate_title (GncMainWindow *window)
{
  GncPluginPage *page;
  const gchar *filename;
  gchar *title, *ptr;

  filename = gnc_session_get_url (gnc_get_current_session ());

  if (!filename)
    filename = _("<no file>");
  else {
    /* The Gnome HIG 2.0 recommends only the file name (no path) be used. (p15) */
    ptr = rindex(filename, '/');
    if (ptr != NULL)
      filename = ptr+1;
  }

  page = window->priv->current_page;
  if (page) {
    /* The Gnome HIG 2.0 recommends the application name not be used. (p16) */
    title = g_strdup_printf("%s - %s", filename,
			    gnc_plugin_page_get_page_name(page));
  } else {
    title = g_strdup_printf("%s", filename);
  }
  
  return title;
}


/** Update the title bar on the specified window.  This routine uses
 *  the gnc_main_window_generate_title() function to create the title.
 *  It is called whenever the user switched pages in a window, as the
 *  title includes the name of the current page.
 *
 *  @param window The window whose title should be updated.
 *
 *  @internal
 */
static void
gnc_main_window_update_title (GncMainWindow *window)
{
  gchar *title;

  title = gnc_main_window_generate_title(window);
  gtk_window_set_title(GTK_WINDOW(window), title);
  g_free(title);
}


/** This data structure is used to describe the requested state of a
 *  GtkRadioAction, and us used to pass data among several
 *  functions. */
struct menu_update {
  /** The name of the GtkRadioAction to be updated. */
  gchar    *action_name;

  /** The new label for this GtkRadioAction. */
  gchar    *label;

  /** Whether or not the GtkRadioAction should be visible. */
  gboolean  visible;
};


/** Update the label on the specified GtkRadioAction in the specified
 *  window.  This action is displayed as a menu item in the "Windows"
 *  menu.  This function will end up being called whenever the front
 *  page is changed in any window, or whenever a window is added or
 *  deleted.
 *
 *  @param window The window whose menu item should be updated.
 *
 *  @param data A data structure containing the name of the
 *  GtkRadioAction, and describing the new state for this action.
 *
 *  @internal
 */
static void
gnc_main_window_update_one_menu_action (GncMainWindow *window,
					struct menu_update *data)
{
  GtkAction* action;

  ENTER("window %p, action %s, label %s, visible %d", window,
	data->action_name, data->label, data->visible);
  action = gtk_action_group_get_action(window->priv->action_group,
				       data->action_name);
  if (action)
    g_object_set(G_OBJECT(action),
		 "label", data->label,
		 "visible", data->visible,
		 NULL);
  LEAVE(" ");
}


/** Update the window selection GtkRadioAction for a specific window.
 *  This is fairly simple since the windows are listed in the same
 *  order that they appear in the active_windows list, so the index
 *  from the window list is used to generate the name of the action.
 *  If the code is ever changed to allow more than ten open windows in
 *  the menu, then the actions in the menu will need to be dynamically
 *  generated/deleted and it gets harder.
 *
 *  @param window The window whose menu item should be updated.
 *
 *  @internal
 */
static void
gnc_main_window_update_radio_button (GncMainWindow *window)
{
  GtkAction *action, *first_action;
  GSList *action_list;
  gchar *action_name;
  gint index;

  ENTER("window %p", window);

  /* Show the new entry in all windows. */
  index = g_list_index(active_windows, window);
  if (index > n_radio_entries) {
    LEAVE("window %d, only %d actions", index, n_radio_entries);
    return;
  }

  action_name = g_strdup_printf("Window%dAction", index);
  action = gtk_action_group_get_action(window->priv->action_group,
				       action_name);

  /* Block the signal so as not to affect window ordering (top to
   * bottom) on the screen */
  action_list = gtk_radio_action_get_group(GTK_RADIO_ACTION(action));
  first_action = g_slist_last(action_list)->data;
  g_signal_handlers_block_by_func(first_action,
				  gnc_main_window_cmd_window_raise, window);
  DEBUG("blocked signal on %p, set %p active, window %p", first_action, action, window);
  gtk_toggle_action_set_active(GTK_TOGGLE_ACTION(action), TRUE);
  g_signal_handlers_unblock_by_func(first_action,
				    gnc_main_window_cmd_window_raise, window);
  g_free(action_name);
  LEAVE(" ");
}


/** In every window that the user has open, update the "Window" menu
 *  item that points to the specified window.  This keeps the "Window"
 *  menu items consistent across all open windows.  (These items
 *  cannot be shared because of the way the GtkUIManager code works.)
 *
 *  This function is called whenever the user switches pages in a
 *  window, or whenever a window is added or deleted.
 *
 *  @param window The window whose menu item should be updated in all
 *  open windows.
 *
 *  @internal
 */
static void
gnc_main_window_update_menu_item (GncMainWindow *window)
{
  struct menu_update data;
  gchar **strings, *title, *expanded;
  gint index;

  ENTER("window %p", window);
  index = g_list_index(active_windows, window);
  if (index > n_radio_entries) {
    LEAVE("skip window %d (only %d entries)", index, n_radio_entries);
    return;
  }

  /* Figure out the label name. Add the accelerator if possible. */
  title = gnc_main_window_generate_title(window);
  strings = g_strsplit(title, "_", 0);
  expanded = g_strjoinv("__", strings);
  if (index < 10) {
    data.label = g_strdup_printf("_%d %s", (index + 1) % 10, expanded);
    g_free(expanded);
  } else {
    data.label = expanded;
  }
  g_strfreev(strings);

  data.visible = TRUE;
  data.action_name = g_strdup_printf("Window%dAction", index);
  g_list_foreach(active_windows,
		 (GFunc)gnc_main_window_update_one_menu_action,
		 &data);
  g_free(data.action_name);
  g_free(data.label);
  LEAVE(" ");
}

/** Update all menu entries for all window menu items in all windows.
 *  This function is called whenever a window is added or deleted.
 *  The worst case scenario is where the user has deleted the first
 *  window, so every single visible item needs to be updated.
 *
 *  @internal
 */
static void
gnc_main_window_update_all_menu_items (void)
{
  struct menu_update data;
  gchar *label;
  gint i;

  ENTER("");

  /* First update the entries for all existing windows */
  g_list_foreach(active_windows,
		 (GFunc)gnc_main_window_update_menu_item,
		 NULL);
  g_list_foreach(active_windows,
		 (GFunc)gnc_main_window_update_radio_button,
		 NULL);

  /* Now hide any entries that aren't being used. */
  data.visible = FALSE;
  for (i = g_list_length(active_windows); i < n_radio_entries; i++) {
    data.action_name = g_strdup_printf("Window%dAction", i);
    label = g_strdup_printf("Window _%d", (i - 1) % 10);
    data.label = gettext(label);

    g_list_foreach(active_windows,
		   (GFunc)gnc_main_window_update_one_menu_action,
		   &data);

    g_free(data.action_name);
    g_free(label);
  }
  LEAVE(" ");
}


/** Show/hide the close box on the tab of a notebook page.  This
 *  function first checks to see if the specified page has a close
 *  box, and if so, sets its isibility to the requested state.
 *
 *  @internal
 *
 *  @param page The GncPluginPage whose notebook tab should be updated.
 *
 *  @param new_value A pointer to the boolean that indicates whether
 *  or not the close button should be visible.
 */
static void
gnc_main_window_update_tabs_one_page (GncPluginPage *page,
				      gboolean *new_value)
{
  GtkWidget * close_button;

  ENTER("page %p, visible %d", page, *new_value);
  close_button = g_object_get_data(G_OBJECT (page), PLUGIN_PAGE_CLOSE_BUTTON);
  if (!close_button) {
    LEAVE("no close button");
    return;
  }

  if (*new_value)
    gtk_widget_show (close_button);
  else
    gtk_widget_hide (close_button);
  LEAVE(" ");
}


/** Show/hide the close box on all pages in a given window.  This
 *  function calls the gnc_main_window_update_tabs_one_page() for each
 *  page in the window.
 *
 *  @internal
 *
 *  @param window The GncMainWindow whose notebook tabs should be
 *  updated.
 *
 *  @param new_value A pointer to the boolean that indicates whether
 *  or not the close button should be visible.
 */
static void
gnc_main_window_update_tabs_one_window (GncMainWindow *window, gboolean *new_value)
{
  ENTER("window %p, visible %d", window, *new_value);
  g_list_foreach(window->priv->installed_pages,
		 (GFunc)gnc_main_window_update_tabs_one_page,
		 new_value);
  LEAVE(" ");
}


/** Show/hide the close box on all pages in all windows.  This
 *  function calls the gnc_main_window_update_tabs_one_window() for
 *  each open window in the application.
 *
 *  @internal
 *
 *  @param entry A pointer to the GConfEntry which describes the new
 *  state of whether close buttons should be visible on notebook tabs.
 *
 *  @param user_data Unused.
 */
static void
gnc_main_window_update_tabs (GConfEntry *entry, gpointer user_data)
{
  gboolean new_value;

  ENTER(" ");
  new_value = gconf_value_get_bool(entry->value);
  g_list_foreach(active_windows,
		 (GFunc)gnc_main_window_update_tabs_one_window,
		 &new_value);
  LEAVE(" ");
}


/************************************************************
 *                   Widget Implementation                  *
 ************************************************************/

/*  Get the type of a gnc main window.
 */
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

		static const GInterfaceInfo plugin_info = {
		  (GInterfaceInitFunc) gnc_window_main_window_init,
		  NULL,
		  NULL
		};

		gnc_main_window_type = g_type_register_static (GTK_TYPE_WINDOW,
							       "GncMainWindow",
							       &our_info, 0);
		g_type_add_interface_static (gnc_main_window_type,
					     GNC_TYPE_WINDOW,
					     &plugin_info);
	}

	return gnc_main_window_type;
}


/** Initialize the class for the new gnucash main window.  This will
 *  set up any function pointers that override functions in the parent
 *  class, and also the signals that this class of widget can
 *  generate.
 *
 *  @param klass The new class structure created by the object system.
 *
 *  @internal
 */
static void
gnc_main_window_class_init (GncMainWindowClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);
	GtkObjectClass *gtkobject_class = GTK_OBJECT_CLASS(klass);

	parent_class = g_type_class_peek_parent (klass);

	window_type = g_quark_from_static_string ("gnc-main-window");

	object_class->finalize = gnc_main_window_finalize;

	/* GtkObject signals */
	gtkobject_class->destroy = gnc_main_window_destroy;

	/**
	 * GncMainWindow::page_added:
	 * @param window: the #GncMainWindow
	 * @param page: the #GncPluginPage
	 *
	 * The "page_added" signal is emitted when a new page is added
	 * to the notebook of a GncMainWindow.  This can be used to
	 * attach a signal from the page so that menu actions can be
	 * adjusted based upon events that occur within the page
	 * (e.g. an account is selected.)
	 */
	main_window_signals[PAGE_ADDED] =
	  g_signal_new ("page_added",
			G_OBJECT_CLASS_TYPE (object_class),
			G_SIGNAL_RUN_FIRST,
			G_STRUCT_OFFSET (GncMainWindowClass, page_added),
			NULL, NULL,
			g_cclosure_marshal_VOID__OBJECT,
			G_TYPE_NONE, 1,
			G_TYPE_OBJECT);

	/**
	 * GncMainWindow::page_changed:
	 * @param window: the #GncMainWindow
	 * @param page: the #GncPluginPage
	 *
	 * The "page_changed" signal is emitted when a new page is
	 * selected in the notebook of a GncMainWindow.  This can be
	 * used to to adjust menu actions based upon which page is
	 * currently displayed in a window.
	 */
	main_window_signals[PAGE_CHANGED] =
	  g_signal_new ("page_changed",
			G_OBJECT_CLASS_TYPE (object_class),
			G_SIGNAL_RUN_FIRST,
			G_STRUCT_OFFSET (GncMainWindowClass, page_changed),
			NULL, NULL,
			g_cclosure_marshal_VOID__OBJECT,
			G_TYPE_NONE, 1,
			G_TYPE_OBJECT);

	qof_session_add_close_hook(gnc_main_window_shutdown, NULL);

	gnc_gconf_general_register_cb (KEY_SHOW_CLOSE_BUTTON,
				       gnc_main_window_update_tabs,
				       NULL);
}


/** Initialize a new instance of a gnucash main window.  This function
 *  allocates and initializes the object private storage space.  It
 *  also adds the new object to a list (for memory tracking purposes).
 *
 *  @param view The new object instance created by the object system.
 *
 *  @internal
 */
static void
gnc_main_window_init (GncMainWindow *window,
		      GncMainWindowClass *klass)
{
	window->priv = g_new0 (GncMainWindowPrivate, 1);

	window->priv->merged_actions_table =
	  g_hash_table_new_full (g_str_hash, g_str_equal, g_free, g_free);

	window->priv->event_handler_id =
	  gnc_engine_register_event_handler(gnc_main_window_event_handler,
					    window);

	gnc_main_window_setup_window (window);
	gnc_gobject_tracking_remember(G_OBJECT(window),
				      G_OBJECT_CLASS(klass));
}


/** Finalize the GncMainWindow object.  This function is called from
 *  the G_Object level to complete the destruction of the object.  It
 *  should release any memory not previously released by the destroy
 *  function (i.e. the private data structure), then chain up to the
 *  parent's destroy function.
 *
 *  @param object The object being destroyed.
 *
 *  @internal
 */
static void
gnc_main_window_finalize (GObject *object)
{
	GncMainWindow *window;

	g_return_if_fail (object != NULL);
	g_return_if_fail (GNC_IS_MAIN_WINDOW (object));

	window = GNC_MAIN_WINDOW (object);

	if (active_windows == NULL) {
	  /* Oops. User killed last window and we didn't catch it. */
	  g_idle_add((GSourceFunc)gnc_shutdown, 0);
	}

	g_return_if_fail (window->priv != NULL);

	g_free (window->priv);

	gnc_gobject_tracking_forget(object);
	G_OBJECT_CLASS (parent_class)->finalize (object);
}


static void
gnc_main_window_destroy (GtkObject *object)
{
	GncMainWindow *window;

	g_return_if_fail (object != NULL);
	g_return_if_fail (GNC_IS_MAIN_WINDOW (object));

	window = GNC_MAIN_WINDOW (object);

	active_windows = g_list_remove (active_windows, window);

	g_return_if_fail (window->priv != NULL);

	/* Do these things once */
	if (window->priv->merged_actions_table) {

	  /* Close any pages in this window */
	  while (window->priv->current_page)
	    gnc_main_window_close_page(window->priv->current_page);

	  if (gnc_window_get_progressbar_window() == GNC_WINDOW(window))
	    gnc_window_set_progressbar_window(NULL);

	  /* Update the "Windows" menu in all other windows */
	  gnc_main_window_update_all_menu_items();

	  gnc_gconf_remove_notification(G_OBJECT(window), DESKTOP_GNOME_INTERFACE);
	  gnc_gconf_remove_notification(G_OBJECT(window), GCONF_GENERAL);

	  gnc_engine_unregister_event_handler(window->priv->event_handler_id);
	  window->priv->event_handler_id = 0;

	  g_hash_table_destroy (window->priv->merged_actions_table);
	  window->priv->merged_actions_table = NULL;
	}
	GTK_OBJECT_CLASS (parent_class)->destroy (object);
}


/*  Create a new gnc main window plugin.
 */
GncMainWindow *
gnc_main_window_new (void)
{
	GncMainWindow *window;

	window = g_object_new (GNC_TYPE_MAIN_WINDOW, NULL);
	active_windows = g_list_append (active_windows, window);
	gnc_main_window_update_all_menu_items();
	return window;
}

/************************************************************
 *                     Utility Functions                    *
 ************************************************************/

/** Connect a GncPluginPage to the window.  This function will insert
 *  the page in to the window's notebook and its list of active pages.
 *  It will also emit the "inserted" signal on the page, and the
 *  "add_page" signal on the window.  This function does not connect
 *  the page's summarybar widget (if any).  That will be connected in
 *  a callback function generated by the page being inserted into the
 *  notebook.
 *
 *  @param window The window where the new page should be added.
 *
 *  @param page The GncPluginPage that should be added to the window.
 *  The visible widget for this plugin must have already been created.
 *
 *  @param tab_widget The widget that should be added into the
 *  notebook tab for this page.  Generally this is a GtkLabel, but
 *  could also be a GtkHBox containing an icon and a label.
 *
 *  @internal
 */
static void
gnc_main_window_connect (GncMainWindow *window,
			 GncPluginPage *page,
			 GtkWidget *tab_hbox,
			 GtkWidget *menu_label)
{
	GtkNotebook *notebook;

	page->window = GTK_WIDGET(window);
	notebook = GTK_NOTEBOOK (window->priv->notebook);
	window->priv->installed_pages =
	  g_list_append (window->priv->installed_pages, page);
	gtk_notebook_append_page_menu (notebook, page->notebook_page,
				       tab_hbox, menu_label);
	gnc_plugin_page_inserted (page);
	gtk_notebook_set_current_page (notebook, -1);
	if (GNC_PLUGIN_PAGE_GET_CLASS(page)->window_changed)
	  (GNC_PLUGIN_PAGE_GET_CLASS(page)->window_changed)(page, GTK_WIDGET(window));
	g_signal_emit (window, main_window_signals[PAGE_ADDED], 0, page);

	g_signal_connect(G_OBJECT(page->notebook_page), "popup-menu",
			 G_CALLBACK(gnc_main_window_popup_menu_cb), page);
	g_signal_connect_after(G_OBJECT(page->notebook_page), "button-press-event",
			 G_CALLBACK(gnc_main_window_button_press_cb), page);
}


/** Disconnect a GncPluginPage page from the window.  If this page is
 *  currently foremost in the window's notebook, its user interface
 *  actions will be disconnected and the page's summarybar widget (if
 *  any) will be removed.  The page is then removed from the window's
 *  notebook and its list of active pages.
 *
 *  @param window The window the page should be removed from.
 *
 *  @param page The GncPluginPage that should be removed from the
 *  window.
 *
 *  @internal
 */
static void
gnc_main_window_disconnect (GncMainWindow *window,
			    GncPluginPage *page)
{
	GtkNotebook *notebook;
	gint page_num;

	/* Disconnect the callbacks */
	g_signal_handlers_disconnect_by_func(G_OBJECT(page->notebook_page),
			 G_CALLBACK(gnc_main_window_popup_menu_cb), page);
	g_signal_handlers_disconnect_by_func(G_OBJECT(page->notebook_page),
			 G_CALLBACK(gnc_main_window_button_press_cb), page);

	/* Disconnect the page and summarybar from the window */
	if (window->priv->current_page == page) {
		gnc_plugin_page_unmerge_actions (page, window->ui_merge);
		gnc_plugin_page_unselected (page);
		window->priv->current_page = NULL;

		if (page->summarybar) {
			gtk_container_remove(GTK_CONTAINER(window->priv->summarybar_dock),
					     page->summarybar);
		}
	}

	/* Remove it from the list of pages in the window */
	window->priv->installed_pages =
	  g_list_remove (window->priv->installed_pages, page);


	/* Remove the page from the notebook */
	notebook = GTK_NOTEBOOK (window->priv->notebook);
	page_num =  gtk_notebook_page_num(notebook, page->notebook_page);
	gtk_notebook_remove_page (notebook, page_num);

	if ( gtk_notebook_get_current_page(notebook) == -1) {
	  /* Need to synthesize a page changed signal when the last
	   * page is removed.  The notebook doesn't generate a signal
	   * for this, therefore the switch_page code in this file
	   * never gets called to generate this signal. */
	  gnc_main_window_switch_page(notebook, NULL, -1, window);
	  //g_signal_emit (window, main_window_signals[PAGE_CHANGED], 0, NULL);
	}

	gnc_plugin_page_removed (page);

	gtk_ui_manager_ensure_update (window->ui_merge);
	gnc_window_set_status (GNC_WINDOW(window), page, NULL);
}


/************************************************************
 *                                                          *
 ************************************************************/


void
gnc_main_window_display_page (GncPluginPage *page)
{
	GncMainWindow *window;
	GtkNotebook *notebook;
	gint page_num;

	window = GNC_MAIN_WINDOW (page->window);
	notebook = GTK_NOTEBOOK (window->priv->notebook);
	page_num = gtk_notebook_page_num(notebook, page->notebook_page);
	gtk_notebook_set_current_page (notebook, page_num);
	gtk_window_present(GTK_WINDOW(window));
}


/*  Display a data plugin page in a window.  If the page already
 *  exists in any window, then that window will be brought to the
 *  front and the notebook switch to display the specified page.  If
 *  the page is new then it will be added to the specified window.  If
 *  the window is NULL, the new page will be added to the first
 *  window.
 */
void
gnc_main_window_open_page (GncMainWindow *window,
			   GncPluginPage *page)
{
	GtkWidget *tab_hbox;
	GtkWidget *label;
	const gchar *icon;
	GtkWidget *image;
	gboolean immutable = FALSE;

	if (window)
	  g_return_if_fail (GNC_IS_MAIN_WINDOW (window));
	g_return_if_fail (GNC_IS_PLUGIN_PAGE (page));
	g_return_if_fail (gnc_plugin_page_has_books(page));

	if (gnc_main_window_page_exists(page)) {
	  gnc_main_window_display_page(page);
	  return;
	}

	if (gnc_plugin_page_get_use_new_window(page)) {
	  window = gnc_main_window_new ();
	  gtk_widget_show(GTK_WIDGET(window));
	} else if ((window == NULL) && active_windows) {
	  window = active_windows->data;
	}

	/* Is this the first page in the first window? */
	if ((window == active_windows->data) &&
	    (window->priv->installed_pages == NULL)) {
	  immutable = TRUE;
	  g_object_set_data (G_OBJECT (page), PLUGIN_PAGE_IMMUTABLE,
			     GINT_TO_POINTER(1));
	}

	page->window = GTK_WIDGET(window);
	page->notebook_page = gnc_plugin_page_create_widget (page);
	g_object_set_data (G_OBJECT (page->notebook_page),
			   PLUGIN_PAGE_LABEL, page);

	/*
	 * The page tab.
	 */
	icon = GNC_PLUGIN_PAGE_GET_CLASS(page)->tab_icon;
	label = gtk_label_new (gnc_plugin_page_get_page_name(page));
	gtk_widget_show (label);

	tab_hbox = gtk_hbox_new (FALSE, 6);
	gtk_widget_show (tab_hbox);

	if (icon != NULL) {
		image = gtk_image_new_from_stock (icon, GTK_ICON_SIZE_MENU);
		gtk_widget_show (image);
		gtk_box_pack_start (GTK_BOX (tab_hbox), image, FALSE, FALSE, 0);
	} 

	gtk_box_pack_start (GTK_BOX (tab_hbox), label, TRUE, TRUE, 0);
  
	/* Add close button - Not for immutable pages */
	if (!immutable) {
	  GtkWidget *close_image, *close_button;
	  
	  close_button = gtk_button_new();
	  gtk_button_set_relief(GTK_BUTTON(close_button), GTK_RELIEF_NONE);
	  close_image=gtk_image_new_from_stock(GTK_STOCK_CLOSE, GTK_ICON_SIZE_MENU);
	  gtk_widget_show(close_image);
	  gtk_container_add(GTK_CONTAINER(close_button), close_image);
	  if (gnc_gconf_get_bool(GCONF_GENERAL, KEY_SHOW_CLOSE_BUTTON, NULL))
	    gtk_widget_show (close_button);
	  else
	    gtk_widget_hide (close_button);
     
	  g_signal_connect_swapped (G_OBJECT (close_button), "clicked",
                      G_CALLBACK(gnc_main_window_close_page), page);

	  gtk_box_pack_start (GTK_BOX (tab_hbox), close_button, FALSE, FALSE, 0);

	  g_object_set_data (G_OBJECT (page), PLUGIN_PAGE_CLOSE_BUTTON, close_button);
	}

	/*
	 * The popup menu
	 */
	label = gtk_label_new (gnc_plugin_page_get_page_name(page));

	/*
	 * Now install it all in the window.
	 */
	gnc_main_window_connect(window, page, tab_hbox, label);
}


/*  Remove a data plugin page from a window and display the previous
 *  page.  If the page removed was the last page in the window, and
 *  there is more than one window open, then the entire window will be
 *  destroyed.
 */
void
gnc_main_window_close_page (GncPluginPage *page)
{
	GncMainWindow *window;

	if (!page->notebook_page)
		return;

	window = GNC_MAIN_WINDOW (page->window);
	if (!window) {
	  g_warning("Page is not in a window.");
	  return;
	}

	gnc_main_window_disconnect(window, page);
	gnc_plugin_page_destroy_widget (page);
	g_object_unref(page);

	/* If this isn't the last window, go ahead and destroy the window. */
	if (window->priv->installed_pages == NULL) {
		if (g_list_length(active_windows) > 1) {
			gtk_widget_destroy(GTK_WIDGET(window));
		}
	}
}


/*  Retrieve a pointer to the page that is currently at the front of
 *  the specified window.  Any plugin that needs to manipulate its
 *  menus based upon the currently selected menu page should connect
 *  to the "page_changed" signal on a window.  The callback function
 *  from that signal can then call this function to obtain a pointer
 *  to the current page.
 */
GncPluginPage *
gnc_main_window_get_current_page (GncMainWindow *window)
{
	return window->priv->current_page;
}


/*  Manually add a set of actions to the specified window.  Plugins
 *  whose user interface is not hard coded (e.g. the menu-additions *
 *  plugin) must create their actions at run time, then use this *
 *  function to install them into the window.
 */
void
gnc_main_window_manual_merge_actions (GncMainWindow *window,
				      const gchar *group_name,
				      GtkActionGroup *group,
				      guint merge_id)
{
	MergedActionEntry *entry;

	g_return_if_fail (GNC_IS_MAIN_WINDOW (window));
	g_return_if_fail (group_name != NULL);
	g_return_if_fail (GTK_IS_ACTION_GROUP(group));
	g_return_if_fail (merge_id > 0);

	entry = g_new0 (MergedActionEntry, 1);
	entry->action_group = group;
	entry->merge_id = merge_id;
	gtk_ui_manager_ensure_update (window->ui_merge);
	g_hash_table_insert (window->priv->merged_actions_table, g_strdup (group_name), entry);
}


/*  Add a set of actions to the specified window.  This function
 *  should not need to be called directly by plugin implementors.
 *  Correctly assigning values to the GncPluginClass fields during
 *  plugin initialization will cause this routine to be automatically
 *  called.
 */
void
gnc_main_window_merge_actions (GncMainWindow *window,
			       const gchar *group_name,
			       GtkActionEntry *actions,
			       guint n_actions,
			       const gchar *filename,
			       gpointer user_data)
{
	GncMainWindowActionData *data;
	MergedActionEntry *entry;
	GError *error = NULL;
	gchar *pathname;

	g_return_if_fail (GNC_IS_MAIN_WINDOW (window));
	g_return_if_fail (group_name != NULL);
	g_return_if_fail (actions != NULL);
	g_return_if_fail (n_actions > 0);
	g_return_if_fail (filename != NULL);

	data = g_new0 (GncMainWindowActionData, 1);
	data->window = window;
	data->data = user_data;

	pathname = gnc_gnome_locate_ui_file (filename);
	if (pathname == NULL)
	  return;

	entry = g_new0 (MergedActionEntry, 1);
	entry->action_group = gtk_action_group_new (group_name);
	gtk_action_group_add_actions (entry->action_group, actions, n_actions, data);
	gtk_ui_manager_insert_action_group (window->ui_merge, entry->action_group, 0);
	entry->merge_id = gtk_ui_manager_add_ui_from_file (window->ui_merge, pathname, &error);
	g_assert(entry->merge_id || error);
	if (entry->merge_id) {
	  gtk_ui_manager_ensure_update (window->ui_merge);
	  g_hash_table_insert (window->priv->merged_actions_table, g_strdup (group_name), entry);
	} else {
	  g_critical("Failed to load ui file.\n  Filename %s\n  Error %s",
		     filename, error->message);
	  g_error_free(error);
	  g_free(entry);
	}
	g_free(pathname);
}


/*  Remove a set of actions from the specified window.  This function
 *  should not need to be called directly by plugin implementors.  It
 *  will automatically be called when a plugin is removed from a
 *  window.
 */
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

	gtk_ui_manager_remove_action_group (window->ui_merge, entry->action_group);
	gtk_ui_manager_remove_ui (window->ui_merge, entry->merge_id);
	gtk_ui_manager_ensure_update (window->ui_merge);

	g_hash_table_remove (window->priv->merged_actions_table, group_name);
}


/*  Force a full update of the user interface for the specified
 *  window.  This can be an expensive function, but is needed because
 *  the gtk ui manager doesn't always seem to update properly when
 *  actions are changed.
 */
void
gnc_main_window_actions_updated (GncMainWindow *window)
{
	GtkActionGroup *force;

	g_return_if_fail (GNC_IS_MAIN_WINDOW (window));

	/* Unfortunately gtk_ui_manager_ensure_update doesn't work
	 * here.  Force a full update by adding and removing an empty
	 * action group.
	 */
	force = gtk_action_group_new("force_update");
	gtk_ui_manager_insert_action_group (window->ui_merge, force, 0);
	gtk_ui_manager_ensure_update (window->ui_merge);
	gtk_ui_manager_remove_action_group (window->ui_merge, force);
	g_object_unref(force);
}


/*  Retrieve a specific set of user interface actions from a window.
 *  This function can be used to get an group of action to be
 *  manipulated when the front page of a window has changed.
 */
GtkActionGroup *
gnc_main_window_get_action_group (GncMainWindow *window,
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
gnc_main_window_add_plugin (gpointer plugin,
			    gpointer window)
{
	g_return_if_fail (GNC_IS_MAIN_WINDOW (window));
	g_return_if_fail (GNC_IS_PLUGIN (plugin));

	gnc_plugin_add_to_window (GNC_PLUGIN (plugin),
				  GNC_MAIN_WINDOW (window),
				  window_type);
}

static void
gnc_main_window_update_toolbar (GncMainWindow *window)
{
	GtkToolbarStyle style;
	GSList *list;

	ENTER("window %p", window);

	style = gnc_get_toolbar_style();
	list = gtk_ui_manager_get_toplevels(window->ui_merge, GTK_UI_MANAGER_TOOLBAR);
	g_slist_foreach(list, (GFunc)gtk_toolbar_set_style, GINT_TO_POINTER(style));
	g_slist_free(list);
	LEAVE("");
}

static void
gnc_main_window_gconf_changed (GConfClient *client,
			       guint cnxn_id,
			       GConfEntry *entry,
			       gpointer user_data)
{
	GncMainWindow *window;
	GConfValue *value;
	const gchar *key, *key_tail;

	window = GNC_MAIN_WINDOW(user_data);

	key = gconf_entry_get_key(entry);
	value = gconf_entry_get_value(entry);
	if (!key || !value)
	  return;

	key_tail = rindex(key, '/');
	if (key_tail != NULL)
	  key_tail++;
	if (strcmp(key_tail, KEY_TOOLBAR_STYLE) == 0) {
	  gnc_main_window_update_toolbar(window);
	}
}

static void
gnc_main_window_setup_window (GncMainWindow *window)
{
	GncMainWindowPrivate *priv;
	GtkWidget *main_vbox;
	guint merge_id;
	GncPluginManager *manager;
	GList *plugins;
	GError *error = NULL;
	gchar *filename;
	SCM debugging;

	/* Catch window manager delete signal */
	g_signal_connect (G_OBJECT (window), "delete-event",
			  G_CALLBACK (gnc_main_window_delete_event), window);

	/* Create widgets and add them to the window */
	main_vbox = gtk_vbox_new (FALSE, 0);
	gtk_widget_show (main_vbox);
	gtk_container_add (GTK_CONTAINER (window), main_vbox);

	priv = window->priv;
	priv->menu_dock = gtk_vbox_new (FALSE, 0);
	gtk_widget_show (priv->menu_dock);
	gtk_box_pack_start (GTK_BOX (main_vbox), priv->menu_dock,
			    FALSE, TRUE, 0);

	priv->notebook = gtk_notebook_new ();
	g_object_set(G_OBJECT(priv->notebook),
			      "scrollable", TRUE,
			      "enable-popup", TRUE,
			      NULL);
	gtk_widget_show (priv->notebook);
	g_signal_connect (G_OBJECT (priv->notebook), "switch-page",
			  G_CALLBACK (gnc_main_window_switch_page), window);
	gtk_box_pack_start (GTK_BOX (main_vbox), priv->notebook,
			    TRUE, TRUE, 0);

	priv->show_summarybar = TRUE;
	priv->summarybar_dock = gtk_vbox_new (FALSE, 0);
	gtk_widget_show (priv->summarybar_dock);
	gtk_box_pack_start (GTK_BOX (main_vbox), priv->summarybar_dock,
			    FALSE, TRUE, 0);

	priv->statusbar = gtk_statusbar_new ();
	gtk_widget_show (priv->statusbar);
	gtk_box_pack_start (GTK_BOX (main_vbox), priv->statusbar,
	                           FALSE, TRUE, 0);
        gtk_statusbar_set_has_resize_grip( GTK_STATUSBAR(priv->statusbar), TRUE );

	priv->progressbar = gtk_progress_bar_new ();
	gtk_widget_show (priv->progressbar);
	gtk_box_pack_start (GTK_BOX (priv->statusbar), priv->progressbar,
			    FALSE, TRUE, 0);

	window->ui_merge = gtk_ui_manager_new ();

	/* Create menu and toolbar information */
	priv->action_group = gtk_action_group_new ("MainWindowActions");
	gtk_action_group_add_actions (priv->action_group, gnc_menu_actions,
				      gnc_menu_n_actions, window);
	gtk_action_group_add_toggle_actions (priv->action_group, 
					     toggle_actions, n_toggle_actions, 
					     window);
	gtk_action_group_add_radio_actions (priv->action_group,
					    radio_entries, n_radio_entries,
					    0,
					    G_CALLBACK(gnc_main_window_cmd_window_raise),
					    window);
	gnc_plugin_update_actions(window->priv->action_group,
				  always_insensitive_actions,
				  "sensitive", FALSE);
	gtk_ui_manager_insert_action_group (window->ui_merge, priv->action_group, 0);

	g_signal_connect (G_OBJECT (window->ui_merge), "add_widget",
			  G_CALLBACK (gnc_main_window_add_widget), window);
	filename = gnc_gnome_locate_ui_file("gnc-main-window-ui.xml");

	/* Can't do much without a ui. */
	g_assert (filename);

	merge_id = gtk_ui_manager_add_ui_from_file (window->ui_merge,
						    filename, &error);
	g_assert(merge_id || error);
	if (merge_id) {
	  gtk_window_add_accel_group (GTK_WINDOW (window),
				      gtk_ui_manager_get_accel_group(window->ui_merge));
	  gtk_ui_manager_ensure_update (window->ui_merge);
	} else {
	  g_critical("Failed to load ui file.\n  Filename %s\n  Error %s",
		     filename, error->message);
	  g_error_free(error);
	  g_assert(merge_id != 0);
	}
	g_free(filename);

	gnc_gconf_add_notification(G_OBJECT(window), GCONF_GENERAL,
				   gnc_main_window_gconf_changed);
	gnc_gconf_add_notification(G_OBJECT(window), DESKTOP_GNOME_INTERFACE,
				   gnc_main_window_gconf_changed);
	gnc_main_window_update_toolbar(window);

        /* Testing */
	/* Now update the "eXtensions" menu */
	debugging = scm_c_eval_string("(gnc:debugging?)");
	if (debugging == SCM_BOOL_F) {
	  GtkAction*  action;

	  action = gtk_action_group_get_action(priv->action_group,"ExtensionsAction");
	  g_object_set(G_OBJECT(action), "visible", FALSE, NULL);
	}

	/* GncPluginManager stuff */
	manager = gnc_plugin_manager_get ();
	plugins = gnc_plugin_manager_get_plugins (manager);
        g_list_foreach (plugins, gnc_main_window_add_plugin, window);
        g_list_free (plugins);

	g_signal_connect (G_OBJECT (manager), "plugin-added",
			  G_CALLBACK (gnc_main_window_plugin_added), window);
	g_signal_connect (G_OBJECT (manager), "plugin-removed",
			  G_CALLBACK (gnc_main_window_plugin_removed), window);

}

static void
gnc_main_window_add_widget (GtkUIManager *merge,
			    GtkWidget *widget,
			    GncMainWindow *window)
{
	if (GTK_IS_TOOLBAR (widget)) {
		window->priv->toolbar_dock = widget;
	}

	gtk_box_pack_start (GTK_BOX (window->priv->menu_dock), widget, FALSE, FALSE, 0);
	gtk_widget_show (widget);
}

/** This function is invoked when the GtkNotebook switches pages.  It
 *  is responsible for updating the rest of the window contents
 *  outside of the notebook.  I.E. Updating the user interface, the
 *  summary bar, etc.  This function also emits the "page_changed"
 *  signal from the window so that any plugin can also learn about the
 *  fact that the page has changed.
 *
 *  @internal
 */
static void
gnc_main_window_switch_page (GtkNotebook *notebook,
			     GtkNotebookPage *notebook_page,
			     gint pos,
			     GncMainWindow *window)
{
	GtkWidget *child, *summarybar, *summarybar_dock;
	GncPluginPage *page;
	gboolean immutable;

	ENTER("Notebook %p, page, %p, index %d, window %p",
	       notebook, notebook_page, pos, window);
	g_return_if_fail (GNC_IS_MAIN_WINDOW (window));

	summarybar_dock = window->priv->summarybar_dock;

	if (window->priv->current_page != NULL) {
		page = window->priv->current_page;
		gnc_plugin_page_unmerge_actions (page, window->ui_merge);
		gnc_plugin_page_unselected (page);

		/* Remove old page's summarybar too */
		if (page->summarybar) {
			gtk_container_remove(GTK_CONTAINER(summarybar_dock),
					     page->summarybar);
		}
	}

	child = gtk_notebook_get_nth_page (notebook, pos);
	if (child) {
		page = g_object_get_data (G_OBJECT (child), PLUGIN_PAGE_LABEL);
	} else {
		page = NULL;
	}

	window->priv->current_page = page;

	if (page != NULL) {
		/* Update the user interface (e.g. menus and toolbars */
		gnc_plugin_page_merge_actions (page, window->ui_merge);

		/* install new summarybar (if any) */
		summarybar = page->summarybar;
		if (summarybar) {
		  if (GTK_OBJECT_FLOATING(summarybar)) {
		    /* Own this object. This will prevent it from being deleted by
		     * gtk when it is removed from the summarybar. */
		    g_object_ref (summarybar);
		    gtk_object_sink (GTK_OBJECT (summarybar));
		  }

		  if (window->priv->show_summarybar)
		    gtk_widget_show(summarybar);
		  else
		    gtk_widget_hide(summarybar);
		  gtk_box_pack_start(GTK_BOX(summarybar_dock), summarybar,
				     FALSE, TRUE, 0 );
		}

		/* Allow page specific actions */
		gnc_plugin_page_selected (page);
		gnc_window_update_status (GNC_WINDOW(window), page);
	}

	/* Update the menus based upon whether this is an "immutable" page. */
	immutable = page &&
	  g_object_get_data (G_OBJECT (page), PLUGIN_PAGE_IMMUTABLE);
	gnc_plugin_update_actions(window->priv->action_group,
				  immutable_page_actions,
				  "sensitive", !immutable);
	gnc_plugin_update_actions(window->priv->action_group,
				  multiple_page_actions,
				  "sensitive",
				  g_list_length(window->priv->installed_pages) > 1);

	gnc_main_window_update_title(window);
	gnc_main_window_update_menu_item(window);

	g_signal_emit (window, main_window_signals[PAGE_CHANGED], 0, page);
	LEAVE(" ");
}

static void
gnc_main_window_plugin_added (GncPlugin *manager,
			      GncPlugin *plugin,
			      GncMainWindow *window)
{
	g_return_if_fail (GNC_IS_MAIN_WINDOW (window));
	g_return_if_fail (GNC_IS_PLUGIN (plugin));

	gnc_plugin_add_to_window (plugin, window, window_type);
}

static void
gnc_main_window_plugin_removed (GncPlugin *manager,
				GncPlugin *plugin,
				GncMainWindow *window)
{
	g_return_if_fail (GNC_IS_MAIN_WINDOW (window));
	g_return_if_fail (GNC_IS_PLUGIN (plugin));

	gnc_plugin_remove_from_window (plugin, window, window_type);
}


/* Command callbacks */
static void
gnc_main_window_cmd_file_properties (GtkAction *action, GncMainWindow *window)
{
  SCM func = scm_c_eval_string("gnc:main-window-properties-cb");
  if (!SCM_PROCEDUREP (func)) {
      PERR ("not a procedure\n");
      return;
  }
  scm_call_0(func);
}

static void
gnc_main_window_cmd_file_close (GtkAction *action, GncMainWindow *window)
{
	g_return_if_fail(GNC_IS_MAIN_WINDOW(window));

	if (window->priv->current_page != NULL) {
		gnc_main_window_close_page (window->priv->current_page);
	}
}

static void
gnc_main_window_cmd_file_quit (GtkAction *action, GncMainWindow *window)
{
	QofSession *session;
	session = qof_session_get_current_session();
	if (qof_book_not_saved(qof_session_get_book(session))) {
	  if (gnc_main_window_prompt_for_save(GTK_WIDGET(window))) {
	    /* User cancelled */
	    return;
	  }
	}

	gnc_shutdown (0);
}

static void
gnc_main_window_cmd_edit_preferences (GtkAction *action, GncMainWindow *window)
{
	gnc_preferences_dialog ();
}

static void
gnc_main_window_cmd_view_refresh (GtkAction *action, GncMainWindow *window)
{
}

static void
gnc_main_window_cmd_actions_reset_warnings (GtkAction *action, GncMainWindow *window)
{
  gnc_reset_warnings_dialog(GTK_WIDGET(window));
}

static void
gnc_main_window_cmd_view_toolbar (GtkAction *action, GncMainWindow *window)
{
	if (gtk_toggle_action_get_active(GTK_TOGGLE_ACTION(action))) {
		gtk_widget_show (window->priv->toolbar_dock);
	} else {
		gtk_widget_hide (window->priv->toolbar_dock);
	}
}

static void
gnc_main_window_cmd_view_summary (GtkAction *action, GncMainWindow *window)
{
	if (gtk_toggle_action_get_active(GTK_TOGGLE_ACTION(action))) {
		gtk_widget_show (window->priv->summarybar_dock);
	} else {
		gtk_widget_hide (window->priv->summarybar_dock);
	}
}

static void
gnc_main_window_cmd_view_statusbar (GtkAction *action, GncMainWindow *window)
{
	if (gtk_toggle_action_get_active(GTK_TOGGLE_ACTION(action))) {
		gtk_widget_show (window->priv->statusbar);
	} else {
		gtk_widget_hide (window->priv->statusbar);
	}
}

static void
gnc_main_window_cmd_window_new (GtkAction *action, GncMainWindow *window)
{
	GncMainWindow *new_window;

	/* Create the new window */
	ENTER(" ");
	new_window = gnc_main_window_new ();
	gtk_widget_show(GTK_WIDGET(new_window));
	LEAVE(" ");
}

static void
gnc_main_window_cmd_window_move_page (GtkAction *action, GncMainWindow *window)
{
	GncMainWindowPrivate *priv;
	GncMainWindow *new_window;
	GncPluginPage *page;
	GtkNotebook *notebook;
	GtkWidget *tab_widget, *menu_widget;

	/* Setup */
	priv = window->priv;
	if (priv->current_page == NULL)
		return;
	notebook = GTK_NOTEBOOK (priv->notebook);
	page = priv->current_page;
	tab_widget = gtk_notebook_get_tab_label (notebook, page->notebook_page);
	menu_widget = gtk_notebook_get_menu_label (notebook, page->notebook_page);

	/* Ref the page components, then remove it from its old window */
	g_object_ref(page);
	g_object_ref(tab_widget);
	g_object_ref(menu_widget);
	g_object_ref(page->notebook_page);
	gnc_main_window_disconnect(window, page);

	/* Create the new window */
	new_window = gnc_main_window_new ();
	gtk_widget_show(GTK_WIDGET(new_window));

	/* Now add the page to the new window */
	gnc_main_window_connect (new_window, page, tab_widget, menu_widget);

	/* Unref the page components now that we're done */
	g_object_unref(page->notebook_page);
	g_object_unref(menu_widget);
	g_object_unref(tab_widget);
	g_object_unref(page);

	/* just a little debugging. :-) */
	DEBUG("Moved page %p (sb %p) from window %p to new window %p",
	      page, page->summarybar, window, new_window);
	DEBUG("Old window current is %p, new window current is %p",
	      window->priv->current_page, new_window->priv->current_page);
}

static void
gnc_main_window_cmd_window_raise (GtkAction *action,
				  GtkRadioAction *current,
				  GncMainWindow *unused)
{
	GncMainWindow *window;
	gint value;

	g_return_if_fail(GTK_IS_ACTION(action));
	g_return_if_fail(GTK_IS_RADIO_ACTION(current));
	g_return_if_fail(GNC_IS_MAIN_WINDOW(unused));
	
	ENTER("action %p, current %p, window %p", action, current, unused);
	value = gtk_radio_action_get_current_value(current);
	window = g_list_nth_data(active_windows, value);
	gtk_window_present(GTK_WINDOW(window));
	LEAVE(" ");
}

static void
gnc_main_window_cmd_help_tutorial (GtkAction *action, GncMainWindow *window)
{
	gnc_gnome_help (HF_GUIDE, NULL);
}

static void
gnc_main_window_cmd_help_contents (GtkAction *action, GncMainWindow *window)
{
	gnc_gnome_help (HF_HELP, NULL);
}

static void
gnc_main_window_cmd_test( GtkAction *action, GncMainWindow *window )
{
        GtkWindow *w = GTK_WINDOW(gtk_window_new( GTK_WINDOW_TOPLEVEL ));
        gnc_html *gnchtml = gnc_html_new( w );
        gchar *html = "<html><head><title>testing</title></head><body><h1>testing</h1><h2>testing 2</h2> <p>Tes<br />ting<object classid=\"gnc-guppi-pie\" width=\"300\" height=\"200\">No pie for you!</object></p></body></html>";
        gtk_container_add( GTK_CONTAINER(w), GTK_WIDGET(gnc_html_get_widget(gnchtml)) );

        gnc_html_show_data( gnchtml, html, strlen( html ) );
        
        gtk_widget_show_all( GTK_WIDGET(w) );
}

static void
gnc_main_window_cmd_help_about (GtkAction *action, GncMainWindow *window)
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
	GdkPixbuf *logo;

	logo = gnc_gnome_get_gdkpixbuf ("appicon.png");

	about = gnome_about_new ("GnuCash", VERSION, copyright, message, authors, documenters,
				 strcmp (translator_credits, "translator_credits") != 0 ? translator_credits : NULL,
				 logo);

	gdk_pixbuf_unref (logo);
	gtk_dialog_run (GTK_DIALOG (about));
}


/************************************************************
 *                                                          *
 ************************************************************/

gncUIWidget
gnc_ui_get_toplevel (void)
{
  if (active_windows)
    return active_windows->data;
  return NULL;
}

static GtkWindow *
gnc_main_window_get_gtk_window (GncWindow *window)
{
  g_return_val_if_fail (GNC_IS_MAIN_WINDOW (window), NULL);
  return GTK_WINDOW(window);
}

static GtkWidget *
gnc_main_window_get_statusbar (GncWindow *window_in)
{
  GncMainWindowPrivate *priv;
  GncMainWindow *window;

  g_return_val_if_fail (GNC_IS_MAIN_WINDOW (window_in), NULL);

  window = GNC_MAIN_WINDOW(window_in);
  priv = window->priv;
  return priv->statusbar;
}

static GtkWidget *
gnc_main_window_get_progressbar (GncWindow *window_in)
{
  GncMainWindowPrivate *priv;
  GncMainWindow *window;

  g_return_val_if_fail (GNC_IS_MAIN_WINDOW (window_in), NULL);

  window = GNC_MAIN_WINDOW(window_in);
  priv = window->priv;
  return priv->progressbar;
}

static void
gnc_window_main_window_init (GncWindowIface *iface)
{
	iface->get_gtk_window  = gnc_main_window_get_gtk_window;
	iface->get_statusbar   = gnc_main_window_get_statusbar;
	iface->get_progressbar = gnc_main_window_get_progressbar;
}


/*  Set the window where all progressbar updates should occur.  This
 *  is a wrapper around the gnc_window_set_progressbar_window()
 *  function.
 */
void
gnc_main_window_set_progressbar_window (GncMainWindow *window)
{
  GncWindow *gncwin;
  gncwin = GNC_WINDOW(window);
  gnc_window_set_progressbar_window(gncwin);
}


/** Popup a contextual menu.  This function ends up being called when
 *  the user right-clicks in the context of a window, or uses the
 *  keyboard context-menu request key combination (Shift-F10 by
 *  default).
 *
 *  @param page This is the GncPluginPage corresponding to the visible
 *  page.
 *
 *  @param event The event parameter passed to the "button-press"
 *  callback.  May be null if there was no event (aka keyboard
 *  request).
 */
static void
do_popup_menu(GncPluginPage *page, GdkEventButton *event)
{
  GtkUIManager *ui_merge;
  GtkWidget *menu;
  int button, event_time;

  g_return_if_fail(GNC_IS_PLUGIN_PAGE(page));

  ENTER("page %p, event %p", page, event);
  ui_merge = gnc_plugin_page_get_ui_merge(page);
  if (ui_merge == NULL) {
    LEAVE("no ui merge");
    return;
  }

  menu = gtk_ui_manager_get_widget(ui_merge, "/MainPopup");
  if (!menu) {
    LEAVE("no menu");
    return;
  }

  if (event) {
    button = event->button;
    event_time = event->time;
  } else {
    button = 0;
    event_time = gtk_get_current_event_time ();
  }

  gtk_menu_popup(GTK_MENU(menu), NULL, NULL, NULL, NULL, button, event_time);
  LEAVE(" ");
}


/** Callback function invoked when the user requests that Gnucash
 *  popup the contextual menu via the keyboard context-menu request
 *  key combination (Shift-F10 by default).
 *
 *  @param page This is the GncPluginPage corresponding to the visible
 *  page.
 *
 *  @param widget Whatever widget had focus when the user issued the
 *  keyboard context-menu request.
 *
 *  @return Always returns TRUE to indicate that the menu request was
 *  handled.
 */
static gboolean
gnc_main_window_popup_menu_cb (GtkWidget *widget,
			       GncPluginPage *page)
{
  ENTER("widget %p, page %p", widget, page);
  do_popup_menu(page, NULL);
  LEAVE(" ");
  return TRUE;
}


/*  Callback function invoked when the user clicks in the content of
 *  any Gnucash window.  If this was a "right-click" then Gnucash will
 *  popup the contextual menu.
 */
gboolean
gnc_main_window_button_press_cb (GtkWidget *whatever,
				 GdkEventButton *event,
				 GncPluginPage *page)
{
  g_return_val_if_fail(GNC_IS_PLUGIN_PAGE(page), FALSE);

  ENTER("widget %p, event %p, page %p", whatever, event, page);
  /* Ignore double-clicks and triple-clicks */
  if (event->button == 3 && event->type == GDK_BUTTON_PRESS) {
    do_popup_menu(page, event);
    LEAVE("menu shown");
    return TRUE;
  }

  LEAVE("other click");
  return FALSE;
}


/** @} */
/** @} */
