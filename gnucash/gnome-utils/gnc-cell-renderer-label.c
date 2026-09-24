/********************************************************************
 * Copyright (C) 2026 GnuCash contributors
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
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02110-1301, USA.
 *******************************************************************/

#include <config.h>
#include "gnc-cell-renderer-label.h"
static void
setup (GtkSignalListItemFactory *factory, GtkListItem *item, gpointer data)
{
    GtkWidget *label = gtk_label_new (NULL);
    gtk_label_set_selectable (GTK_LABEL (label), TRUE);
    gtk_widget_set_halign (label, GTK_ALIGN_START);
    gtk_list_item_set_child (item, label);
    (void)factory;
    (void)data;
}
GtkListItemFactory *
gnc_cell_renderer_label_new (void)
{
    GtkListItemFactory *factory = gtk_signal_list_item_factory_new ();
    g_signal_connect (factory, "setup", G_CALLBACK (setup), NULL);
    return factory;
}
