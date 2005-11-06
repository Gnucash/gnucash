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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
 * Boston, MA  02111-1307,  USA       gnu@gnu.org
 */

/** @addtogroup ContentPlugins
    @{ */
/** @addtogroup ContentPluginBase Common object and functions
    @{ */
/** @file gnc-plugin-page.h
    @brief Functions for adding plugins to a Gnucash window.
    @author Copyright (C) 2003 Jan Arne Petersen
    @author Copyright (C) 2003,2005 David Hampton <hampton@employees.org>
*/

#ifndef __GNC_PLUGIN_PAGE_H
#define __GNC_PLUGIN_PAGE_H

#include "guid.h"
#include "qofbook.h"

G_BEGIN_DECLS

/* type macros */
#define GNC_TYPE_PLUGIN_PAGE            (gnc_plugin_page_get_type ())
#define GNC_PLUGIN_PAGE(o)              (G_TYPE_CHECK_INSTANCE_CAST ((o), GNC_TYPE_PLUGIN_PAGE, GncPluginPage))
#define GNC_PLUGIN_PAGE_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GNC_TYPE_PLUGIN_PAGE, GncPluginPageClass))
#define GNC_IS_PLUGIN_PAGE(o)           (G_TYPE_CHECK_INSTANCE_TYPE ((o), GNC_TYPE_PLUGIN_PAGE))
#define GNC_IS_PLUGIN_PAGE_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GNC_TYPE_PLUGIN_PAGE))
#define GNC_PLUGIN_PAGE_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GNC_PLUGIN_PAGE, GncPluginPageClass))

/* typedefs & structures */
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

typedef struct {
	GObjectClass gobject;

	const gchar *tab_icon;
	const gchar *plugin_name;

	/* Signals */
	void (* inserted) (GncPluginPage *plugin_page);
	void (* removed) (GncPluginPage *plugin_page);
	void (* selected) (GncPluginPage *plugin_page);
	void (* unselected) (GncPluginPage *plugin_page);

	/* Virtual Table */
	GtkWidget *(* create_widget) (GncPluginPage *plugin_page);
	void (* destroy_widget) (GncPluginPage *plugin_page);
	void (* window_changed) (GncPluginPage *plugin_page, GtkWidget *window);
} GncPluginPageClass;

/* function prototypes */
GType                 gnc_plugin_page_get_type        (void);

GtkWidget            *gnc_plugin_page_create_widget   (GncPluginPage *plugin_page);
void                  gnc_plugin_page_destroy_widget  (GncPluginPage *plugin_page);

/** Show the summarybar associated with this page.
 *
 *  @param page The page whose summarybar visibility should be changed.
 *
 *  @param visible Whether or not the summarybar should be shown..
 */
void gnc_plugin_page_show_summarybar (GncPluginPage *page, gboolean visible);

void                  gnc_plugin_page_merge_actions   (GncPluginPage *plugin_page,
                                                       GtkUIManager *merge);
void                  gnc_plugin_page_unmerge_actions (GncPluginPage *plugin_page,
                                                       GtkUIManager *merge);

const gchar * gnc_plugin_page_get_plugin_name (GncPluginPage *plugin_page);

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
gboolean gnc_plugin_page_has_book (GncPluginPage *page, GUID *book);

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

/** Set the name of this page.  This is the string used in the
 *  window title, and in the notebook tab and page selection menus.
 *
 *  @param page The page whose name should be set.
 *
 *  @param name The new string for the name.
 */
void gnc_plugin_page_set_page_name (GncPluginPage *page, const char *name);

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
void gnc_plugin_page_set_statusbar_text (GncPluginPage *page, const char *name);


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
void gnc_plugin_page_set_use_new_window (GncPluginPage *page, gboolean use_new);


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
void gnc_plugin_page_set_ui_description (GncPluginPage *page, const char *ui_filename);


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
 *  name is used to associate keybindings with actions, so it should
 *  be consistent across all pages of the same type.
 *
 *  @return A pointer to the newly created GtkActionGroup object for
 *  this page. */
GtkActionGroup * gnc_plugin_page_create_action_group (GncPluginPage *page, const gchar *group_name);


/* Signals */
void                  gnc_plugin_page_inserted        (GncPluginPage *plugin_page);
void                  gnc_plugin_page_removed         (GncPluginPage *plugin_page);
void                  gnc_plugin_page_selected        (GncPluginPage *plugin_page);
void                  gnc_plugin_page_unselected      (GncPluginPage *plugin_page);


G_END_DECLS

#endif /* __GNC_PLUGIN_PAGE_H */
/** @} */
/** @} */
