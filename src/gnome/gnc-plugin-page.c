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

static void gnc_plugin_page_base_init (gpointer klass);

enum {
	INSERTED,
	REMOVED,
	SELECTED,
	UNSELECTED,
	LAST_SIGNAL
};

static guint signals[LAST_SIGNAL] = { 0 };

GType
gnc_plugin_page_get_type (void)
{
	static GType gnc_plugin_page_type = 0;

	if (gnc_plugin_page_type == 0) {
		static const GTypeInfo our_info = {
			sizeof (GncPluginPageIface),
			gnc_plugin_page_base_init,
			NULL,
			NULL,
			NULL,
			NULL,
			0,
			0,
			NULL
		};

		gnc_plugin_page_type = g_type_register_static (G_TYPE_INTERFACE,
							       "GncPluginPage",
    							       &our_info, 0);
		g_type_interface_add_prerequisite (gnc_plugin_page_type, G_TYPE_OBJECT);
	}

	return gnc_plugin_page_type;
}

GtkWidget *
gnc_plugin_page_create_widget (GncPluginPage *plugin_page)
{
	GtkWidget *widget;

	g_return_val_if_fail (GNC_IS_PLUGIN_PAGE (plugin_page), NULL);
	g_return_val_if_fail (GNC_PLUGIN_PAGE_GET_IFACE (plugin_page)->create_widget != NULL, NULL);

	widget = GNC_PLUGIN_PAGE_GET_IFACE (plugin_page)->create_widget (plugin_page);

	/*
	 * If there is a destroy function, add a ref so that the
	 * widgets will exists when the destroy function is called.
	 * Otherwise it will be destroyed when it is removed from the
	 * main notebook for the window.
	 */
	if (GNC_PLUGIN_PAGE_GET_IFACE (plugin_page)->destroy_widget)
		g_object_ref(widget);

	return widget;
}

void
gnc_plugin_page_destroy_widget (GncPluginPage *plugin_page)
{
	g_return_if_fail (GNC_IS_PLUGIN_PAGE (plugin_page));
	g_return_if_fail (GNC_PLUGIN_PAGE_GET_IFACE (plugin_page)->destroy_widget != NULL);

	return GNC_PLUGIN_PAGE_GET_IFACE (plugin_page)->destroy_widget (plugin_page);
}

void
gnc_plugin_page_merge_actions (GncPluginPage *plugin_page,
			       EggMenuMerge *ui_merge)
{
	g_return_if_fail (GNC_IS_PLUGIN_PAGE (plugin_page));
	g_return_if_fail (GNC_PLUGIN_PAGE_GET_IFACE (plugin_page)->merge_actions != NULL);

	GNC_PLUGIN_PAGE_GET_IFACE (plugin_page)->merge_actions (plugin_page, ui_merge);
}

void
gnc_plugin_page_unmerge_actions (GncPluginPage *plugin_page,
				 EggMenuMerge *ui_merge)
{
	g_return_if_fail (GNC_IS_PLUGIN_PAGE (plugin_page));
	g_return_if_fail (GNC_PLUGIN_PAGE_GET_IFACE (plugin_page)->unmerge_actions != NULL);

	GNC_PLUGIN_PAGE_GET_IFACE (plugin_page)->unmerge_actions (plugin_page, ui_merge);
}

gchar *
gnc_plugin_page_get_title  (GncPluginPage *plugin_page)
{
	g_return_val_if_fail (GNC_IS_PLUGIN_PAGE (plugin_page), NULL);
	g_return_val_if_fail (GNC_PLUGIN_PAGE_GET_IFACE (plugin_page)->get_title != NULL, NULL);

	return GNC_PLUGIN_PAGE_GET_IFACE (plugin_page)->get_title (plugin_page);
}

gchar *
gnc_plugin_page_get_tab_name  (GncPluginPage *plugin_page)
{
	g_return_val_if_fail (GNC_IS_PLUGIN_PAGE (plugin_page), NULL);
	g_return_val_if_fail (GNC_PLUGIN_PAGE_GET_IFACE (plugin_page)->get_tab_name != NULL, NULL);

	return GNC_PLUGIN_PAGE_GET_IFACE (plugin_page)->get_tab_name (plugin_page);
}

G_CONST_RETURN gchar *
gnc_plugin_page_get_tab_icon (GncPluginPage *plugin_page)
{
	g_return_val_if_fail (GNC_IS_PLUGIN_PAGE (plugin_page), NULL);
	g_return_val_if_fail (GNC_PLUGIN_PAGE_GET_IFACE (plugin_page)->get_tab_icon != NULL, NULL);

	return GNC_PLUGIN_PAGE_GET_IFACE (plugin_page)->get_tab_icon (plugin_page);
}

G_CONST_RETURN gchar *
gnc_plugin_page_get_plugin_name (GncPluginPage *plugin_page)
{
	g_return_val_if_fail (GNC_IS_PLUGIN_PAGE (plugin_page), NULL);
	g_return_val_if_fail (GNC_PLUGIN_PAGE_GET_IFACE (plugin_page)->get_plugin_name != NULL, NULL);

	return GNC_PLUGIN_PAGE_GET_IFACE (plugin_page)->get_plugin_name (plugin_page);
}

G_CONST_RETURN gchar *
gnc_plugin_page_get_uri (GncPluginPage *plugin_page)
{
	g_return_val_if_fail (GNC_IS_PLUGIN_PAGE (plugin_page), NULL);
	g_return_val_if_fail (GNC_PLUGIN_PAGE_GET_IFACE (plugin_page)->get_uri != NULL, NULL);

	return GNC_PLUGIN_PAGE_GET_IFACE (plugin_page)->get_uri (plugin_page);
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

GtkWidget *
gnc_plugin_page_get_window  (GncPluginPage *plugin_page)
{
	g_return_val_if_fail (GNC_IS_PLUGIN_PAGE (plugin_page), NULL);

	return GNC_PLUGIN_PAGE_GET_IFACE (plugin_page)->window;
}

void
gnc_plugin_page_set_window  (GncPluginPage *plugin_page, GtkWidget *window)
{
	g_return_if_fail (GNC_IS_PLUGIN_PAGE (plugin_page));

	GNC_PLUGIN_PAGE_GET_IFACE (plugin_page)->window = window;
}

static void
gnc_plugin_page_base_init (gpointer klass)
{
	static gboolean initialized = FALSE;

	if (!initialized) {
		initialized = TRUE;

		signals[INSERTED] = g_signal_new ("inserted",
						  G_OBJECT_CLASS_TYPE (klass),
						  G_SIGNAL_RUN_FIRST,
						  G_STRUCT_OFFSET (GncPluginPageIface, inserted),
						  NULL, NULL,
						  g_cclosure_marshal_VOID__VOID,
						  G_TYPE_NONE,
						  0);
		signals[REMOVED] = g_signal_new ("removed",
						 G_OBJECT_CLASS_TYPE (klass),
						 G_SIGNAL_RUN_FIRST,
						 G_STRUCT_OFFSET (GncPluginPageIface, removed),
						 NULL, NULL,
						 g_cclosure_marshal_VOID__VOID,
						 G_TYPE_NONE,
						 0);
		signals[SELECTED] = g_signal_new ("selected",
						  G_OBJECT_CLASS_TYPE (klass),
						  G_SIGNAL_RUN_FIRST,
						  G_STRUCT_OFFSET (GncPluginPageIface, inserted),
						  NULL, NULL,
						  g_cclosure_marshal_VOID__VOID,
						  G_TYPE_NONE,
						  0);
		signals[UNSELECTED] = g_signal_new ("unselected",
						    G_OBJECT_CLASS_TYPE (klass),
   						    G_SIGNAL_RUN_FIRST,
   						    G_STRUCT_OFFSET (GncPluginPageIface, removed),
   						    NULL, NULL,
   						    g_cclosure_marshal_VOID__VOID,
   						    G_TYPE_NONE,
   						    0);
	}
}
