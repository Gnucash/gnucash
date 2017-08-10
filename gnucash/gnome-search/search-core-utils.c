/*
 * search-core-utils.c -- common functions for search code
 * Copyright (C) 2006 David Hampton <hampton@employees.org>
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
 *
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <gtk/gtk.h>
#include <glib/gi18n.h>

#include "search-core-utils.h"


static void
search_combo_changed (GtkWidget *widget, gint *value)
{
    GtkTreeModel *model;
    GtkTreeIter iter;

    g_return_if_fail(GTK_IS_COMBO_BOX(widget));
    g_return_if_fail(value);

    model = gtk_combo_box_get_model(GTK_COMBO_BOX(widget));
    if (!gtk_combo_box_get_active_iter(GTK_COMBO_BOX(widget), &iter))
        return;

    gtk_tree_model_get(model, &iter,
                       GNC_COMBO_SEARCH_COL_VALUE, value,
                       -1);
}

GtkWidget *
gnc_combo_box_new_search (void)
{
    GtkWidget *combo;
    GtkListStore *store;
    GtkCellRenderer *renderer;

    store = gtk_list_store_new(NUM_GNC_COMBO_SEARCH_COLS, G_TYPE_STRING, G_TYPE_UINT);
    combo = gtk_combo_box_new_with_model(GTK_TREE_MODEL(store));
    g_object_unref(store);

    renderer = gtk_cell_renderer_text_new ();
    gtk_cell_layout_pack_start (GTK_CELL_LAYOUT (combo), renderer, TRUE);
    gtk_cell_layout_set_attributes (GTK_CELL_LAYOUT (combo), renderer,
                                    "text", GNC_COMBO_SEARCH_COL_TEXT,
                                    NULL);
    return combo;
}

void
gnc_combo_box_search_add (GtkComboBox *combo, const gchar *text, guint value)
{
    GtkListStore *store;
    GtkTreeIter iter;

    g_return_if_fail(GTK_IS_COMBO_BOX(combo));
    g_return_if_fail(text);

    store = GTK_LIST_STORE(gtk_combo_box_get_model(combo));
    gtk_list_store_append(store, &iter);
    gtk_list_store_set(store, &iter,
                       GNC_COMBO_SEARCH_COL_TEXT, text,
                       GNC_COMBO_SEARCH_COL_VALUE, value,
                       -1);
}

guint
gnc_combo_box_search_get_active (GtkComboBox *combo)
{
    GtkTreeModel *model;
    GtkTreeIter iter;
    guint value;

    g_return_val_if_fail(GTK_IS_COMBO_BOX(combo), 0);

    model = gtk_combo_box_get_model(combo);
    if (!gtk_combo_box_get_active_iter(combo, &iter))
        return 0;

    gtk_tree_model_get(model, &iter,
                       GNC_COMBO_SEARCH_COL_VALUE, &value,
                       -1);
    return value;
}

void
gnc_combo_box_search_set_active (GtkComboBox *combo, guint value)
{
    GtkTreeModel *model;
    GtkTreeIter iter;
    guint row_value = 0;

    g_return_if_fail(GTK_IS_COMBO_BOX(combo));

    model = gtk_combo_box_get_model(combo);
    if (!gtk_tree_model_get_iter_first(model, &iter))
        return;

    do
    {
        gtk_tree_model_get(model, &iter,
                           GNC_COMBO_SEARCH_COL_VALUE, &row_value,
                           -1);
        if (value == row_value)
        {
            gtk_combo_box_set_active_iter(combo, &iter);
            return;
        }
    }
    while (gtk_tree_model_iter_next(model, &iter));

    /* No match found. Select the first item. */
    gtk_combo_box_set_active(combo, 0);
}

void
gnc_combo_box_search_changed(GtkComboBox *combo, guint *value)
{
    g_signal_connect (combo, "changed",
                      G_CALLBACK (search_combo_changed), value);
}
