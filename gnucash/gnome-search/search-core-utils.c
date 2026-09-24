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
#include <config.h>
#endif

#include <gtk/gtk.h>

#include "search-core-utils.h"
#include "gnc-gtk-utils.h"

#define SEARCH_VALUE_DATA "gnc-search-value"

static guint
search_drop_down_value_for_item (GObject *item)
{
    return GPOINTER_TO_UINT (g_object_get_data (item, SEARCH_VALUE_DATA));
}

static void
search_drop_down_selection_changed (GtkDropDown *drop_down, GParamSpec *pspec,
                                    gpointer user_data)
{
    GObject *item;
    guint *value = user_data;

    g_return_if_fail (value);
    item = gtk_drop_down_get_selected_item (drop_down);
    if (item)
    {
        *value = search_drop_down_value_for_item (item);
    }
    (void)pspec;
}

GtkWidget *
gnc_search_drop_down_new (void)
{
    GtkStringList *model = gtk_string_list_new (NULL);
    GtkDropDown *drop_down = GTK_DROP_DOWN (
        gnc_gtk_drop_down_new (G_LIST_MODEL (model), NULL));

    return GTK_WIDGET (drop_down);
}

void
gnc_search_drop_down_add (GtkDropDown *drop_down, const gchar *text,
                          guint value)
{
    GtkStringList *model;
    GObject *item;
    guint position;

    g_return_if_fail (GTK_IS_DROP_DOWN (drop_down));
    g_return_if_fail (text);

    model = GTK_STRING_LIST (gtk_drop_down_get_model (drop_down));
    g_return_if_fail (GTK_IS_STRING_LIST (model));
    gtk_string_list_append (model, text);
    position = g_list_model_get_n_items (G_LIST_MODEL (model)) - 1;
    item = g_list_model_get_item (G_LIST_MODEL (model), position);
    g_object_set_data (item, SEARCH_VALUE_DATA, GUINT_TO_POINTER (value));
    g_object_unref (item);
}

guint
gnc_search_drop_down_get_active (GtkDropDown *drop_down)
{
    GObject *item;
    guint value = 0;

    g_return_val_if_fail (GTK_IS_DROP_DOWN (drop_down), 0);

    item = gtk_drop_down_get_selected_item (drop_down);
    if (item)
    {
        value = search_drop_down_value_for_item (item);
    }
    return value;
}

void
gnc_search_drop_down_set_active (GtkDropDown *drop_down, guint value)
{
    GListModel *model;
    guint count;

    g_return_if_fail (GTK_IS_DROP_DOWN (drop_down));

    model = gtk_drop_down_get_model (drop_down);
    count = g_list_model_get_n_items (model);
    for (guint position = 0; position < count; position++)
    {
        GObject *item = g_list_model_get_item (model, position);
        gboolean matches = search_drop_down_value_for_item (item) == value;

        g_object_unref (item);
        if (matches)
        {
            gtk_drop_down_set_selected (drop_down, position);
            return;
        }
    }

    gtk_drop_down_set_selected (drop_down,
                                count ? 0 : GTK_INVALID_LIST_POSITION);
}

void
gnc_search_drop_down_changed (GtkDropDown *drop_down, guint *value)
{
    g_return_if_fail (GTK_IS_DROP_DOWN (drop_down));
    g_return_if_fail (value);

    g_signal_connect (drop_down, "notify::selected",
                      G_CALLBACK (search_drop_down_selection_changed), value);
}
