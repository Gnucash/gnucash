/* 
 * gnc-plugin_page.c -- 
 *
 * Copyright (C) 2003 Jan Arne Petersen
 * Author: Jan Arne Petersen <jpetersen@uni-bonn.de>
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

#include "gnc-plugin-page.h"

static gpointer         parent_class = NULL;

static void gnc_plugin_page_class_init (GncPluginPageClass *klass);
static void gnc_plugin_page_init       (GncPluginPage *plugin_page);
static void gnc_plugin_page_finalize   (GObject *object);

enum {
	INSERTED,
	REMOVED,
	SELECTED,
	UNSELECTED,
	LAST_SIGNAL
};

static guint signals[LAST_SIGNAL] = { 0 };

struct GncPluginPagePrivate
{
	GList *books;
};

GType
gnc_plugin_page_get_type (void)
{
	static GType gnc_plugin_page_type = 0;

	if (gnc_plugin_page_type == 0) {
		static const GTypeInfo our_info = {

			sizeof (GncPluginPageClass),
			NULL,		/* base_init */
			NULL,		/* base_finalize */
			(GClassInitFunc) gnc_plugin_page_class_init,
			NULL,		/* class_finalize */
			NULL,		/* class_data */
			sizeof (GncPluginPage),
			0,		/* n_preallocs */
			(GInstanceInitFunc) gnc_plugin_page_init,
		};

		gnc_plugin_page_type = g_type_register_static (G_TYPE_OBJECT,
							       "GncPluginPage",
    							       &our_info, 0);
	}

	return gnc_plugin_page_type;
}

GtkWidget *
gnc_plugin_page_create_widget (GncPluginPage *plugin_page)
{
	GncPluginPageClass *klass;
	GtkWidget *widget;

	g_return_val_if_fail (GNC_IS_PLUGIN_PAGE (plugin_page), NULL);

	klass = GNC_PLUGIN_PAGE_GET_CLASS (plugin_page);
	g_return_val_if_fail (klass != NULL, NULL);
	g_return_val_if_fail (klass->create_widget != NULL, NULL);

	widget = klass->create_widget (plugin_page);

	/*
	 * If there is a destroy function, add a ref so that the
	 * widgets will exists when the destroy function is called.
	 * Otherwise it will be destroyed when it is removed from the
	 * main notebook for the window.
	 */
	if (klass->destroy_widget)
		g_object_ref(widget);

	return widget;
}

void
gnc_plugin_page_destroy_widget (GncPluginPage *plugin_page)
{
	GncPluginPageClass *klass;

	g_return_if_fail (GNC_IS_PLUGIN_PAGE (plugin_page));

	klass = GNC_PLUGIN_PAGE_GET_CLASS (plugin_page);
	g_return_if_fail (klass != NULL);
	g_return_if_fail (klass->destroy_widget != NULL);

	return klass->destroy_widget (plugin_page);
}

void
gnc_plugin_page_merge_actions (GncPluginPage *plugin_page,
			       EggMenuMerge *ui_merge)
{
	GncPluginPageClass *klass;

	g_return_if_fail (GNC_IS_PLUGIN_PAGE (plugin_page));

	klass = GNC_PLUGIN_PAGE_GET_CLASS (plugin_page);
	g_return_if_fail (klass != NULL);
	g_return_if_fail (klass->merge_actions != NULL);

	klass->merge_actions (plugin_page, ui_merge);
}

void
gnc_plugin_page_unmerge_actions (GncPluginPage *plugin_page,
				 EggMenuMerge *ui_merge)
{
	GncPluginPageClass *klass;

	g_return_if_fail (GNC_IS_PLUGIN_PAGE (plugin_page));

	klass = GNC_PLUGIN_PAGE_GET_CLASS (plugin_page);
	g_return_if_fail (klass != NULL);
	g_return_if_fail (klass->unmerge_actions != NULL);

	klass->unmerge_actions (plugin_page, ui_merge);
}

const gchar *
gnc_plugin_page_get_name (GncPluginPage *plugin_page)
{
	GncPluginPageClass *klass;

	g_return_val_if_fail (GNC_IS_PLUGIN_PAGE (plugin_page), NULL);

	klass = GNC_PLUGIN_PAGE_GET_CLASS (plugin_page);
	g_return_val_if_fail (klass != NULL, NULL);

	return (klass->plugin_name);
}


/* Signals */
void
gnc_plugin_page_inserted (GncPluginPage *plugin_page)
{
	g_return_if_fail (GNC_IS_PLUGIN_PAGE (plugin_page));

	g_signal_emit (G_OBJECT (plugin_page), signals[INSERTED], 0);
}

void
gnc_plugin_page_removed (GncPluginPage *plugin_page)
{
	g_return_if_fail (GNC_IS_PLUGIN_PAGE (plugin_page));

	g_signal_emit (G_OBJECT (plugin_page), signals[REMOVED], 0);
}

void
gnc_plugin_page_selected (GncPluginPage *plugin_page)
{
	g_return_if_fail (GNC_IS_PLUGIN_PAGE (plugin_page));

	g_signal_emit (G_OBJECT (plugin_page), signals[SELECTED], 0);
}

void
gnc_plugin_page_unselected (GncPluginPage *plugin_page)
{
	g_return_if_fail (GNC_IS_PLUGIN_PAGE (plugin_page));

	g_signal_emit (G_OBJECT (plugin_page), signals[UNSELECTED], 0);
}

static void
gnc_plugin_page_class_init (GncPluginPageClass *klass)
{
	GObjectClass *gobject_class = G_OBJECT_CLASS (klass);

	parent_class = g_type_class_peek_parent (klass);
	gobject_class->finalize = gnc_plugin_page_finalize;

	klass->tab_icon    = NULL;
	klass->plugin_name = NULL;

 	signals[INSERTED] = g_signal_new ("inserted",
					  G_OBJECT_CLASS_TYPE (klass),
					  G_SIGNAL_RUN_FIRST,
					  G_STRUCT_OFFSET (GncPluginPageClass, inserted),
					  NULL, NULL,
					  g_cclosure_marshal_VOID__VOID,
					  G_TYPE_NONE,
					  0);
	signals[REMOVED] = g_signal_new ("removed",
					 G_OBJECT_CLASS_TYPE (klass),
					 G_SIGNAL_RUN_FIRST,
					 G_STRUCT_OFFSET (GncPluginPageClass, removed),
					 NULL, NULL,
					 g_cclosure_marshal_VOID__VOID,
					 G_TYPE_NONE,
					 0);
	signals[SELECTED] = g_signal_new ("selected",
					  G_OBJECT_CLASS_TYPE (klass),
					  G_SIGNAL_RUN_FIRST,
					  G_STRUCT_OFFSET (GncPluginPageClass, inserted),
					  NULL, NULL,
					  g_cclosure_marshal_VOID__VOID,
					  G_TYPE_NONE,
					  0);
	signals[UNSELECTED] = g_signal_new ("unselected",
					    G_OBJECT_CLASS_TYPE (klass),
					    G_SIGNAL_RUN_FIRST,
					    G_STRUCT_OFFSET (GncPluginPageClass, removed),
					    NULL, NULL,
					    g_cclosure_marshal_VOID__VOID,
					    G_TYPE_NONE,
					    0);
}

static void
gnc_plugin_page_init (GncPluginPage *plugin_page)
{
	GncPluginPagePrivate *priv;

	priv = plugin_page->priv = g_new0 (GncPluginPagePrivate, 1);

	plugin_page->title       = NULL;
	plugin_page->tab_name    = NULL;
	plugin_page->uri         = NULL;

	plugin_page->window      = NULL;
}

static void
gnc_plugin_page_finalize (GObject *object)
{
  GncPluginPagePrivate *priv;
  GncPluginPage *page;
  GList *item;

  page = GNC_PLUGIN_PAGE (object);
  if (page->statusbar_text)
    g_free (page->statusbar_text);
  if (page->title)
	g_free(page->title);
  if (page->tab_name)
	g_free(page->tab_name);
  if (page->uri)
	g_free(page->uri);

  priv = page->priv;
  if (priv->books) {
    for (item = priv->books; item; item = g_list_next(item)) {
      guid_free (item->data);
    }
    g_list_free(priv->books);
    priv->books = NULL;
  }
  g_free (priv);
  page->priv = NULL;

  page->window = NULL; // Don't need to free it.

  G_OBJECT_CLASS (parent_class)->finalize (object);
}

void
gnc_plugin_page_add_book (GncPluginPage *page, QofBook *book)
{
  GncPluginPagePrivate *priv;
  GUID *guid;

  g_return_if_fail (GNC_IS_PLUGIN_PAGE (page));
  g_return_if_fail (book != NULL);

  priv = page->priv;

  guid = guid_malloc();
  *guid = *qof_book_get_guid(book);
  priv->books = g_list_append(priv->books, guid);
}

gboolean
gnc_plugin_page_has_book (GncPluginPage *page, GUID *entity)
{
  GncPluginPagePrivate *priv;
  GList *item;

  g_return_val_if_fail (GNC_IS_PLUGIN_PAGE (page), FALSE);
  g_return_val_if_fail (entity != NULL, FALSE);

  priv = page->priv;
  for (item = priv->books; item; item = g_list_next(item)) {
    if (guid_equal((GUID*)item->data, entity)) {
      return TRUE;
    }
  }
  return FALSE;
}

gboolean
gnc_plugin_page_has_books (GncPluginPage *page)
{
  g_return_val_if_fail (GNC_IS_PLUGIN_PAGE (page), FALSE);

  return (page->priv->books != NULL);
}
