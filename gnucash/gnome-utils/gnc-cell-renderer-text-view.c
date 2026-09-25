/*
 * Copyright (C) 2020 Robert Fewell
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with this program; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *************************************************************************/

#include <config.h>
#include "gnc-cell-renderer-text-view.h"
static void
setup (GtkSignalListItemFactory *factory, GtkListItem *item, gpointer data)
{
    GtkWidget *label = gtk_editable_label_new (NULL);
    gtk_editable_set_enable_undo (GTK_EDITABLE (label), TRUE);
    gtk_widget_set_halign (label, GTK_ALIGN_START);
    gtk_widget_set_valign (label, GTK_ALIGN_START);
    gtk_list_item_set_child (item, label);
    (void)factory;
    (void)data;
}
GtkListItemFactory *
gnc_cell_renderer_text_view_new (void)
{
    GtkListItemFactory *factory = gtk_signal_list_item_factory_new ();
    g_signal_connect (factory, "setup", G_CALLBACK (setup), NULL);
    return factory;
}
