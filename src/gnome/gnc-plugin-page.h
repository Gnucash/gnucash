/* 
 * gnc-plugin-page.h -- A page, which can be added to the
 *	GnuCash main window.
 *
 * Copyright (C) 2003 Jan Arne Petersen <jpetersen@uni-bonn.de>
 * Copyright (C) 2003 David Hampton <hampton@employees.org>
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

#ifndef __GNC_PLUGIN_PAGE_H
#define __GNC_PLUGIN_PAGE_H

#include "egg-menu-merge.h"
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
typedef struct GncPluginPagePrivate GncPluginPagePrivate;

typedef struct GncPluginPage {
	GObject parent;
	GncPluginPagePrivate *priv;

	GtkWidget *window;
	GtkWidget *notebook_page;

	gchar *title;
	gchar *tab_name;
	gchar *uri;
} GncPluginPage;

typedef struct {
	GObjectClass parent;

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

	void (* merge_actions) (GncPluginPage *plugin_page, EggMenuMerge *merge);
	void (* unmerge_actions) (GncPluginPage *plugin_page, EggMenuMerge *merge);
} GncPluginPageClass;

/* function prototypes */
GType                 gnc_plugin_page_get_type        (void);

GtkWidget            *gnc_plugin_page_create_widget   (GncPluginPage *plugin_page);
void                  gnc_plugin_page_destroy_widget  (GncPluginPage *plugin_page);

void                  gnc_plugin_page_merge_actions   (GncPluginPage *plugin_page,
                                                       EggMenuMerge *merge);
void                  gnc_plugin_page_unmerge_actions (GncPluginPage *plugin_page,
                                                       EggMenuMerge *merge);

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
gboolean              gnc_plugin_page_has_book        (GncPluginPage *page, GUID *book);

/** Query a page to see if it has a reference to any book.
 *
 *  @param page The page to query.
 *
 *  @return TRUE if the page references any books. FALSE otherwise.
 */
gboolean              gnc_plugin_page_has_books       (GncPluginPage *page);


/* Signals */
void                  gnc_plugin_page_inserted        (GncPluginPage *plugin_page);
void                  gnc_plugin_page_removed         (GncPluginPage *plugin_page);
void                  gnc_plugin_page_selected        (GncPluginPage *plugin_page);
void                  gnc_plugin_page_unselected      (GncPluginPage *plugin_page);

G_END_DECLS

#endif /* __GNC_PLUGIN_PAGE_H */
