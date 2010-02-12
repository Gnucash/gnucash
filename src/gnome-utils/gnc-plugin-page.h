/* 
 * gnc-plugin-page.h -- A page, which can be added to the
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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 */

/** @addtogroup ContentPlugins
    @{ */
/** @addtogroup ContentPluginBase Common object and functions
    @{ */
/** @file gnc-plugin-page.h
    @brief Functions for adding plugins to a GnuCash window.
    @author Copyright (C) 2003 Jan Arne Petersen
    @author Copyright (C) 2003,2005 David Hampton <hampton@employees.org>
*/

#ifndef __GNC_PLUGIN_PAGE_H
#define __GNC_PLUGIN_PAGE_H

#include <glib.h>
#include "qof.h"

G_BEGIN_DECLS

#define KEY_SUMMARYBAR_POSITION "summarybar_position"

/* type macros */
#define GNC_TYPE_PLUGIN_PAGE            (gnc_plugin_page_get_type ())
#define GNC_PLUGIN_PAGE(o)              (G_TYPE_CHECK_INSTANCE_CAST ((o), GNC_TYPE_PLUGIN_PAGE, GncPluginPage))
#define GNC_PLUGIN_PAGE_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GNC_TYPE_PLUGIN_PAGE, GncPluginPageClass))
#define GNC_IS_PLUGIN_PAGE(o)           (G_TYPE_CHECK_INSTANCE_TYPE ((o), GNC_TYPE_PLUGIN_PAGE))
#define GNC_IS_PLUGIN_PAGE_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GNC_TYPE_PLUGIN_PAGE))
#define GNC_PLUGIN_PAGE_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GNC_PLUGIN_PAGE, GncPluginPageClass))

/* typedefs & structures */

/** The instance data structure for a content plugin. */
typedef struct GncPluginPage {
	GObject gobject;		/**< The parent object data. */

	GtkWidget *window;		/**< The window that contains the
					 *   display widget for this plugin.
					 *   This field is private to the
					 *   gnucash window management
					 *   code.  */
	GtkWidget *notebook_page;	/**< The display widget for this
					 *   plugin.  This field is private to
					 *   the gnucash window management
					 *   code.  */
	GtkWidget *summarybar;		/**< The summary bar widget (if any)
					 *   that is associated with this
					 *   plugin.  This field is private to
					 *   the gnucash window management
					 *   code.  */
} GncPluginPage;


/** The class data structure for a content plugin. */
typedef struct {
	GObjectClass gobject;

	/** The relative name of the icon that should be shown on the
	 *  tab for this page. */
	const gchar *tab_icon;
	/** The textual name of this plugin. */
	const gchar *plugin_name;

	/* Signals */
	void (* inserted) (GncPluginPage *plugin_page);
	void (* removed) (GncPluginPage *plugin_page);
	void (* selected) (GncPluginPage *plugin_page);
	void (* unselected) (GncPluginPage *plugin_page);

	/* Virtual Table */

	/** Function called to create the display widget for a
	 *  particular type of plugin.  The returned widget should
	 *  encompass all information that goes with this page,
	 *  including scroll bars, a summary bar, etc.
	 *
	 *  @param plugin_page A pointer to the plugin for which a
	 *  display widget should be created.
	 *
	 *  @return A displayable gtk widget. */
	GtkWidget *(* create_widget) (GncPluginPage *plugin_page);
	/** Function called to destroy the display widget for a
	 *  particular type of plugin.
	 *
	 *  @param plugin_page A pointer to the plugin whose display
	 *  widget should be destroyed. */
	void (* destroy_widget) (GncPluginPage *plugin_page);

	/** Save enough information about this page so that it can be
	 *  recreated next time the user starts gnucash.
	 *  
	 *  @param page The page to save.
	 *
	 *  @param key_file A pointer to the GKeyFile data structure where the
	 *  page information should be written.
	 *
	 *  @param group_name The group name to use when writing data.
	 *  The name is specific to this page instance. */
	void (* save_page) (GncPluginPage *page, GKeyFile *file, 
                            const gchar *group);

	/** Create a new page based on the information saved during a
	 *  previous instantiation of gnucash.  This function may or
	 *  may not install the new page in the window as it sees fit.
	 *  Generally the function will install the page int the
	 *  window in order to manipulate the menu items that are
	 *  created at install time.
	 *
	 *  @param window The window where this new page will be
	 *  installed.
	 *
	 *  @param key_file A pointer to the GKeyFile data structure where the
	 *  page information should be retrieved.
	 *
	 *  @param group_name The group name to use when retrieving
	 *  data.  The name is specific to this page instance.
	 *
	 *  @return A pointer to the new page. */
        GncPluginPage * (* recreate_page) (GtkWidget *window, GKeyFile *file, 
                                           const gchar *group);

	/** Perform plugin specific actions when a page is added to a
	 *  window (or has been removed from one window and added to a
	 *  new window).  This function is called after the page is
	 *  installed in the window, just before the window's
	 *  PAGE_ADDED signal is generated.
	 *  
	 *  @param page The page that was added to a window.
	 *
	 *  @param window The window where the page was added. */
	void (* window_changed) (GncPluginPage *plugin_page, GtkWidget *window);

	/** This function vector allows page specific actions to occur
	 *  when the page name is changed.
	 *  
	 *  @param page The page to update.
	 *  
	 *  @param name The new name for this page. */
	void (* page_name_changed) (GncPluginPage *plugin_page, 
                                    const gchar *name);

	/** This function vector allows page specific actions to
	 *  override the generic code for setting the sensitivity of
	 *  items in the Edit menu.
	 *  
	 *  @param page The front page in a main window..
	 *  
	 *  @param hide Whether the widgets should be shown or
	 *  hidden. */
	void (* update_edit_menu_actions) (GncPluginPage *plugin_page, gboolean hide);

	/** This function vector is called to finish any outstanding
	 *  activities.  It will be called for such things as closing a
	 *  page, saving the data file, etc.
	 *  
	 *  @param page The page in a main window.
	 *
	 *  @return FALSE if the page could not or would not comply,
	 *  which should cancel the pending operation.  TRUE
	 *  otherwise */
	gboolean (* finish_pending) (GncPluginPage *plugin_page);
} GncPluginPageClass;


/** Get the type of a content plugin.
 *
 *  @return A GType.
 */
GType gnc_plugin_page_get_type (void);


/** Create the display widget that corresponds to this plugin.  This
 *  function will be called by the main/embedded window manipulation
 *  code to create a widget that they can display.  The returned
 *  widget should encompass all information that goes with this page,
 *  including scroll bars, a summary bar, etc.
 *
 *  @param plugin_page A pointer to the plugin for which a display
 *  widget should be created.
 *
 *  @return A displayable gtk widget. */
GtkWidget *gnc_plugin_page_create_widget (GncPluginPage *plugin_page);


/** Destroy the display widget that corresponds to this plugin.  This
 *  function will be called by the main/embedded window manipulation
 *  code when a page is closed.
 *
 *  @param plugin_page A pointer to the plugin whose display widget
 *  should be destroyed. */
void gnc_plugin_page_destroy_widget (GncPluginPage *plugin_page);


/** Show/hide the summarybar associated with this page.
 *
 *  @param page The page whose summarybar visibility should be changed.
 *
 *  @param visible Whether or not the summarybar should be shown..
 */
void gnc_plugin_page_show_summarybar (GncPluginPage *page, gboolean visible);


/** Call the plugin specific function that will save the state of a
 *  content page to a disk.  That function must save enough
 *  information about the page that it can be recreated next time the
 *  user starts gnucash.
 *
 *  @param page The page to save.
 *
 *  @param key_file A pointer to the GKeyFile data structure where the
 *  page information should be written.
 *
 *  @param group_name The group name to use when saving data. */
void gnc_plugin_page_save_page (GncPluginPage *page,
				GKeyFile *key_file,
				const gchar *group_name);


/** This function looks up a specific plugin type by name, and then
 *  calls a plugin specific function to create a new page and restore
 *  its content to a previous state.
 *
 *  @param window The window where this page should be installed.
 *
 *  @param page_type The name of the page type to create.
 *
 *  @param key_file A pointer to the GKeyFile data structure where the
 *  page information should be read.
 *
 *  @param group_name The group name to use when restoring data. */
GncPluginPage *gnc_plugin_page_recreate_page (GtkWidget *window,
					      const gchar *page_type,
					      GKeyFile *key_file,
					      const gchar *group_name);


/** Add the actions for a content page to the specified window.
 *
 *  @param plugin_page A pointer to the page whose actions should be
 *  added to the user interface.
 *
 *  @param merge A pointer to the UI manager data structure for a
 *  window. */
void gnc_plugin_page_merge_actions (GncPluginPage *plugin_page, 
                                    GtkUIManager *merge);


/** Remove the actions for a content page from the specified window.
 *
 *  @param plugin_page A pointer to the page whose actions should be
 *  removed from the user interface.
 *
 *  @param merge A pointer to the UI manager data structure for a
 *  window. */
void gnc_plugin_page_unmerge_actions (GncPluginPage *plugin_page, 
                                      GtkUIManager *merge);


/** Retrieve the textual name of a plugin.
 *
 *  @param plugin_page A pointer to the page whose actions name
 *  should be retrieved.
 *
 *  @return The name of this plugin.  This string is owned by the
 *  plugin. */
const gchar *gnc_plugin_page_get_plugin_name (GncPluginPage *plugin_page);


/** Add a book reference to the specified page.
 *
 *  @param page The page to be modified.
 *
 *  @param book The book referenced by this page.
 */
void gnc_plugin_page_add_book (GncPluginPage *page, QofBook *book);


/** Query a page to see if it has a reference to a given book.  This
 *  function takes a guid instead of a QofBook because that's what the
 *  engine event mechanism provides.
 *
 *  @param page The page to query.
 *
 *  @param book The guid of the book in question.
 *
 *  @return TRUE if the page refers to the specified book. FALSE
 *  otherwise.
 */
gboolean gnc_plugin_page_has_book (GncPluginPage *page, QofBook *book);


/** Query a page to see if it has a reference to any book.
 *
 *  @param page The page to query.
 *
 *  @return TRUE if the page references any books. FALSE otherwise.
 */
gboolean gnc_plugin_page_has_books (GncPluginPage *page);


/** Retrieve a pointer to the GncMainWindow (GtkWindow) containing
 *  this page.
 *
 *  @param page The page whose window should be retrieved.
 *
 *  @return A pointer to the window.
 */
GtkWidget *gnc_plugin_page_get_window (GncPluginPage *page);


/** Retrieve the name of this page.  This is the string used in the
 *  window title, and in the notebook tab and page selection menus.
 *
 *  @param page The page whose name should be retrieved.
 *
 *  @return The page's name.  This string is owned by the page and
 *  should not be freed by the caller.
 */
const gchar *gnc_plugin_page_get_page_name (GncPluginPage *page);


/** Set the name of this page.  This is the string used in the window
 *  title, and in the notebook tab and page selection menus.
 *
 *  @param page The page whose name should be set.
 *
 *  @param name The new string for the name.
 */
void gnc_plugin_page_set_page_name (GncPluginPage *page, const char *name);


/** Retrieve the long name of this page.  This is the string used in
 *  the tooltip that is attached to the pate name in the notebook
 *  tab.
 *
 *  @param page The page whose name should be retrieved.
 *
 *  @return The page's name.  This string is owned by the page and
 *  should not be freed by the caller.
 */
const gchar *gnc_plugin_page_get_page_long_name (GncPluginPage *page);


/** Set the long name of this page.  This is the string used in the
 *  tooltip that is attached to the pate name in the notebook tab.
 *
 *  @param page The page whose name should be set.
 *
 *  @param name The new string for the name.
 */
void gnc_plugin_page_set_page_long_name (GncPluginPage *page, const char *name);


/** Retrieve the color of this page. This is the color string used
 *  in the notebook tab.
 *
 *  @param page The page whose name should be retrieved.
 *
 *  @return The color for this page.  This string is owned by the page and
 *  should not be freed by the caller.
 */
const gchar *gnc_plugin_page_get_page_color (GncPluginPage *page);


/** Set the color of this page. This is the color string used
 *  in the notebook tab.
 *
 *  @param page The page whose name should be retrieved.
 *
 *  @return The color for this page.  This string is owned by the page and
 *  should not be freed by the caller.
 */
void gnc_plugin_page_set_page_color (GncPluginPage *page, const char *color);


/** Retrieve the Uniform Resource Identifier for this page.
 *
 *  @param page The page whose URI should be retrieved.
 *
 *  @return The URI for this page.  This string is owned by the page and
 *  should not be freed by the caller.
 */
const gchar *gnc_plugin_page_get_uri (GncPluginPage *page);


/** Set the Uniform Resource Identifier for this page.
 *
 *  @param page The page whose URI should be set.
 *
 *  @param name The new URI for the page.
 */
void gnc_plugin_page_set_uri (GncPluginPage *page, const char *name);


/** Retrieve the statusbar text associated with this page.
 *
 *  @param page The page whose statusbar should text be retrieved.
 *
 *  @return A pointer to the statusbar text for this page.  This
 *  string is owned by the page and should not be freed by the caller.
 */
const gchar *gnc_plugin_page_get_statusbar_text (GncPluginPage *page);


/** Set the statusbar text associated with this page.
 *
 *  @param page The page whose statusbar text should be set.
 *
 *  @param name The new statusbar text for the page.
 */
void gnc_plugin_page_set_statusbar_text (GncPluginPage *page, 
                                         const char *name);


/** Retrieve the "use new window" setting associated with this page.
 *
 *  @param page The page whose setting should be retrieved.
 *
 *  @return Whether this page should be created in a new window.
 */
gboolean gnc_plugin_page_get_use_new_window (GncPluginPage *page);


/** Set the "use new window" setting associated with this page.  If
 *  this setting is TRUE, the page will be installed into a new
 *  window.  Otherwise the page will be installed into an existing
 *  window.
 *
 *  @param page The page whose setting should be updated.
 *
 *  @param use_new The new value for this setting.
 */
void gnc_plugin_page_set_use_new_window (GncPluginPage *page, 
                                         gboolean use_new);


/** Retrieve the name of the XML UI file associated with this page.
 *
 *  @param page The page whose setting should be retrieved.
 *
 *  @return A pointer to the filename used for the UI.  This
 *  string is owned by the page and should not be freed by the caller.
 */
const char *gnc_plugin_page_get_ui_description (GncPluginPage *page);


/** Set an alternate UI for the specified page.  This alternate ui
 *  may only use actions specified in the source for the page.
 *
 *  @note This function must be called before the page is installed
 *  into a window.
 *
 *  @param page The page to modify.
 *
 *  @param ui_filename The filename (no path) of the alternate UI.
 */
void gnc_plugin_page_set_ui_description (GncPluginPage *page, 
                                         const char *ui_filename);


/** Retrieve the GtkUIManager object associated with this page.
 *
 *  @param page The page whose UI information should be retrieved.
 *
 *  @return A pointer to the GtkUIManager object for this page. */
GtkUIManager *gnc_plugin_page_get_ui_merge (GncPluginPage *page);


/** Retrieve the GtkActionGroup object associated with this page.
 *
 *  @param page The page whose menu/toolbar action group should be
 *  retrieved.
 *
 *  @return A pointer to the GtkActionGroup object for this page. */
GtkActionGroup *gnc_plugin_page_get_action_group (GncPluginPage *page);


/** Create the GtkActionGroup object associated with this page.
 *
 *  @param page The page whose menu/toolbar action group should be
 *  created.
 *
 *  @param group_name The name associate with this action group.  The
 *  name is used to associate key bindings with actions, so it should
 *  be consistent across all pages of the same type.
 *
 *  @return A pointer to the newly created GtkActionGroup object for
 *  this page. */
GtkActionGroup * gnc_plugin_page_create_action_group (GncPluginPage *page, 
                                                      const gchar *group_name);

/** Retrieve a GtkAction object associated with this page.
 *
 *  @param page The page whose menu/toolbar action group should be
 *  retrieved.
 *
 *  @param name The name of the GtkAction to find.
 *
 *  @return A pointer to the retuested GtkAction object or NULL. */
GtkAction *gnc_plugin_page_get_action (GncPluginPage *page, const gchar *name);

/* Signals */
void gnc_plugin_page_inserted (GncPluginPage *plugin_page);
void gnc_plugin_page_removed (GncPluginPage *plugin_page);
void gnc_plugin_page_selected (GncPluginPage *plugin_page);
void gnc_plugin_page_unselected (GncPluginPage *plugin_page);

/** Tell a page to finish any outstanding activities.
 *  
 *  @param plugin_page A page.
 *
 *  @return FALSE if the page could not or would not comply, which
 *  should cancel the pending operation.  TRUE otherwise */
gboolean gnc_plugin_page_finish_pending (GncPluginPage *plugin_page);

G_END_DECLS

#endif /* __GNC_PLUGIN_PAGE_H */
/** @} */
/** @} */
