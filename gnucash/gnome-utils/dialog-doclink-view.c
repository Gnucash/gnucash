/********************************************************************\
 * dialog-doclink-view.c -- Document link dialog Columnview         *
 * Copyright (C) 2024 Robert Fewell                                 *
 *                                                                  *
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
 *                                                                  *
 * This program is distributed in the hope that it will be useful,  *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of   *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    *
 * GNU General Public License for more details.                     *
 *                                                                  *
 * You should have received a copy of the GNU General Public License*
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
\********************************************************************/

#include <config.h>

#include <gtk/gtk.h>
#include <glib/gi18n.h>

#include "dialog-doclink-view.h"

G_DEFINE_TYPE (DoclinkViewItem, doclink_view_item, G_TYPE_OBJECT);
static GParamSpec *properties[N_PROPS_DOCLINK] = { NULL, };

static void
doclink_view_item_get_property (GObject *object,
                                guint property_id,
                                GValue *value,
                                GParamSpec *pspec)
{
    DoclinkViewItem *item = (DoclinkViewItem*) object;

    switch (property_id)
    {
        case PROP_DOCLINK_ITEM_DATE:
            g_value_set_string (value, item->item_date);
            break;

        case PROP_DOCLINK_ITEM_TIME64:
            g_value_set_int64 (value, item->item_time64);
            break;

        case PROP_DOCLINK_INVOICE_ID:
            g_value_set_string (value, item->invoice_id);
            break;

        case PROP_DOCLINK_DESCRIPTION:
            g_value_set_string (value, item->description);
            break;

        case PROP_DOCLINK_DISPLAY_URI:
            g_value_set_string (value, item->display_uri);
            break;

        case PROP_DOCLINK_AVAILABLE:
            g_value_set_string (value, item->available);
            break;

        case PROP_DOCLINK_ITEM_POINTER:
            g_value_set_pointer (value, item->item_pointer);
            break;

        case PROP_DOCLINK_URI:
            g_value_set_string (value, item->uri);
            break;

        case PROP_DOCLINK_URI_RELATIVE:
            g_value_set_boolean (value, item->uri_relative);
            break;

        case PROP_DOCLINK_URI_RELATIVE_PIX:
            g_value_set_string (value, item->uri_relative_pix);
            break;

        default:
            G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
            break;
    }
}

static void
doclink_view_item_init (DoclinkViewItem *item)
{
}

static void
doclink_view_item_finalize (GObject *object)
{
    DoclinkViewItem *item = DOCLINKVIEW_ITEM(object);

    g_free (item->item_date);
    g_free (item->invoice_id);
    g_free (item->description);
    g_free (item->display_uri);
    g_free (item->available);
    g_free (item->uri);
    g_free (item->uri_relative_pix);

    G_OBJECT_CLASS(doclink_view_item_parent_class)->finalize (object);
}

static void
doclink_view_item_class_init (DoclinkViewItemClass *klass)
{
    GObjectClass *gobject_class = G_OBJECT_CLASS(klass);

    gobject_class->finalize = doclink_view_item_finalize;
    gobject_class->get_property = doclink_view_item_get_property;

    properties[PROP_DOCLINK_ITEM_DATE] =
        g_param_spec_string ("item-date", NULL, NULL, NULL, G_PARAM_READABLE);

    properties[PROP_DOCLINK_ITEM_TIME64] =
        g_param_spec_int64 ("item-time64", NULL, NULL, 0, G_MAXINT64, 0, G_PARAM_READABLE);

    properties[PROP_DOCLINK_INVOICE_ID] =
         g_param_spec_string ("invoice-id", NULL, NULL, NULL, G_PARAM_READABLE);

    properties[PROP_DOCLINK_DESCRIPTION] =
        g_param_spec_string ("description", NULL, NULL, NULL, G_PARAM_READABLE);

    properties[PROP_DOCLINK_DISPLAY_URI] =
        g_param_spec_string ("display-uri", NULL, NULL, NULL, G_PARAM_READABLE);

    properties[PROP_DOCLINK_AVAILABLE] =
        g_param_spec_string ("available", NULL, NULL, NULL, G_PARAM_READABLE);

    properties[PROP_DOCLINK_ITEM_POINTER] =
         g_param_spec_pointer ("item-pointer", NULL, NULL, G_PARAM_READABLE);

    properties[PROP_DOCLINK_URI] =
        g_param_spec_string ("uri", NULL, NULL, NULL, G_PARAM_READABLE);

    properties[PROP_DOCLINK_URI_RELATIVE] =
        g_param_spec_boolean ("uri-relative", NULL, NULL, FALSE, G_PARAM_READABLE);

    properties[PROP_DOCLINK_URI_RELATIVE_PIX] =
        g_param_spec_string ("uri-relative-pix", NULL, NULL, NULL, G_PARAM_READABLE);

    g_object_class_install_properties (gobject_class, N_PROPS_DOCLINK, properties);
}

static void
factory_normal_label_setup (GtkListItemFactory *factory, GtkListItem *list_item, gpointer user_data)
{
    GtkWidget *label = gtk_label_new ("");
    gtk_label_set_ellipsize (GTK_LABEL(label), PANGO_ELLIPSIZE_NONE);
    gtk_label_set_xalign (GTK_LABEL(label), 0.0);

    g_object_set_data (G_OBJECT(label), "prop-position", user_data);

    gtk_widget_add_css_class (GTK_WIDGET(label), "gnc-class-doclink-row");

    gtk_list_item_set_child (list_item, label);
}

static void
factory_ld_label_setup (GtkListItemFactory *factory, GtkListItem *list_item, gpointer user_data)
{
    GtkWidget *label = gtk_label_new ("");
    gtk_label_set_ellipsize (GTK_LABEL(label), PANGO_ELLIPSIZE_START);
    gtk_label_set_xalign (GTK_LABEL(label), 0.0);

    g_object_set_data (G_OBJECT(label), "prop-position", user_data);

    gtk_widget_add_css_class (GTK_WIDGET(label), "gnc-class-doclink-row");

    gtk_list_item_set_child (list_item, label);
}

static void
factory_rel_setup (GtkListItemFactory *factory, GtkListItem *list_item)
{
    GtkWidget *image = gtk_image_new ();

    gtk_widget_add_css_class (GTK_WIDGET(image), "gnc-class-doclink-row");

    gtk_list_item_set_child (list_item, image);
}

static void
factory_date_bind (GtkListItemFactory *factory, GtkListItem *list_item, gpointer user_data)
{
    GtkWidget *label = gtk_list_item_get_child (list_item);

    DoclinkViewItem *item = gtk_list_item_get_item (list_item);
    gtk_label_set_label (GTK_LABEL(label), item->item_date);
}

static void
factory_id_bind (GtkListItemFactory *factory, GtkListItem *list_item, gpointer user_data)
{
    GtkWidget *label = gtk_list_item_get_child (list_item);

    DoclinkViewItem *item = gtk_list_item_get_item (list_item);
    gtk_label_set_label (GTK_LABEL(label), item->invoice_id);
}

static void
factory_type_bind (GtkListItemFactory *factory, GtkListItem *list_item, gpointer user_data)
{
    GtkWidget *label = gtk_list_item_get_child (list_item);

    DoclinkViewItem *item = gtk_list_item_get_item (list_item);
    gtk_label_set_label (GTK_LABEL(label), item->description);
}

static void
factory_ld_bind (GtkListItemFactory *factory, GtkListItem *list_item, gpointer user_data)
{
    GtkWidget *label = gtk_list_item_get_child (list_item);

    DoclinkViewItem *item = gtk_list_item_get_item (list_item);
    gtk_label_set_label (GTK_LABEL(label), item->display_uri);
}

static void
factory_av_bind (GtkListItemFactory *factory, GtkListItem *list_item, gpointer user_data)
{
    GtkWidget *label = gtk_list_item_get_child (list_item);

    DoclinkViewItem *item = gtk_list_item_get_item (list_item);
    gtk_label_set_label (GTK_LABEL(label), item->available);
}

static void
factory_rel_bind (GtkListItemFactory *factory, GtkListItem *list_item, gpointer user_data)
{
    GtkWidget *image = gtk_list_item_get_child (list_item);

    DoclinkViewItem *item = gtk_list_item_get_item (list_item);

    if (item->uri_relative_pix)
        gtk_image_set_from_icon_name (GTK_IMAGE(image), item->uri_relative_pix);
}

GtkWidget *
gnc_doclink_create_column_view (GtkWidget *sw, GListModel *model)
{
    GtkColumnViewColumn* column;

    GtkWidget *view = gtk_column_view_new (NULL);

    GtkSorter *sorter = g_object_ref (gtk_column_view_get_sorter (GTK_COLUMN_VIEW(view)));
    GtkSortListModel *sort_model = gtk_sort_list_model_new (G_LIST_MODEL(model), sorter);

    GtkSingleSelection *selection = gtk_single_selection_new (G_LIST_MODEL(sort_model));
    gtk_single_selection_set_autoselect (selection, TRUE);

    gtk_column_view_set_model (GTK_COLUMN_VIEW(view), GTK_SELECTION_MODEL(selection));


    GtkListItemFactory *factory_date = gtk_signal_list_item_factory_new ();
    g_signal_connect (G_OBJECT(factory_date), "setup", G_CALLBACK(factory_normal_label_setup), NULL);
    g_signal_connect (G_OBJECT(factory_date), "bind", G_CALLBACK(factory_date_bind), NULL);
    column = gtk_column_view_column_new (_("Date"), factory_date);

    GtkSorter *sorter_date = GTK_SORTER(gtk_numeric_sorter_new (
                                        gtk_property_expression_new (DOCLINKVIEW_TYPE_ITEM, NULL, "item-time64")));
    gtk_column_view_column_set_sorter (column, GTK_SORTER(sorter_date));
    g_object_unref (sorter_date);

    gtk_column_view_append_column (GTK_COLUMN_VIEW(view), column);
    gtk_column_view_sort_by_column (GTK_COLUMN_VIEW(view), column, GTK_SORT_ASCENDING);


    GtkListItemFactory *factory_id = gtk_signal_list_item_factory_new ();
    g_signal_connect (G_OBJECT(factory_id), "setup", G_CALLBACK(factory_normal_label_setup), NULL);
    g_signal_connect (G_OBJECT(factory_id), "bind", G_CALLBACK(factory_id_bind), NULL);
    column = gtk_column_view_column_new (_("Id"), factory_id);
    gtk_column_view_append_column (GTK_COLUMN_VIEW(view), column);
    g_object_set_data (G_OBJECT(view), "id-column", column);

    GtkSorter *sorter_id = GTK_SORTER(gtk_string_sorter_new (
                                      gtk_property_expression_new (DOCLINKVIEW_TYPE_ITEM, NULL, "invoice-id")));
    gtk_column_view_column_set_sorter (column, GTK_SORTER(sorter_id));
    g_object_unref (sorter_id);


    GtkListItemFactory *factory_type = gtk_signal_list_item_factory_new ();
    g_signal_connect (G_OBJECT(factory_type), "setup",
                      G_CALLBACK(factory_normal_label_setup), GINT_TO_POINTER(PROP_DOCLINK_DESCRIPTION));
    g_signal_connect (G_OBJECT(factory_type), "bind", G_CALLBACK(factory_type_bind), NULL);
    column = gtk_column_view_column_new (_("Type"), factory_type);
    gtk_column_view_append_column (GTK_COLUMN_VIEW(view), column);
    g_object_set_data (G_OBJECT(view), "type-column", column);

    GtkSorter *sorter_type = GTK_SORTER(gtk_string_sorter_new (
                                        gtk_property_expression_new (DOCLINKVIEW_TYPE_ITEM, NULL, "description")));
    gtk_column_view_column_set_sorter (column, GTK_SORTER(sorter_type));
    g_object_unref (sorter_type);


    GtkListItemFactory *factory_ld = gtk_signal_list_item_factory_new ();
    g_signal_connect (G_OBJECT(factory_ld), "setup",
                      G_CALLBACK(factory_ld_label_setup), GINT_TO_POINTER(PROP_DOCLINK_DISPLAY_URI));
    g_signal_connect (G_OBJECT(factory_ld), "bind", G_CALLBACK(factory_ld_bind), NULL);
    column = gtk_column_view_column_new (_("Linked Document"), factory_ld);
    gtk_column_view_append_column (GTK_COLUMN_VIEW(view), column);
    gtk_column_view_column_set_expand (column, TRUE);

    GtkSorter *sorter_ld = GTK_SORTER(gtk_string_sorter_new (
                                      gtk_property_expression_new (DOCLINKVIEW_TYPE_ITEM, NULL, "display-uri")));
    gtk_column_view_column_set_sorter (column, GTK_SORTER(sorter_ld));
    g_object_unref (sorter_ld);


    GtkListItemFactory *factory_av = gtk_signal_list_item_factory_new ();
    g_signal_connect (G_OBJECT(factory_av), "setup",
                      G_CALLBACK(factory_normal_label_setup), GINT_TO_POINTER(PROP_DOCLINK_AVAILABLE));
    g_signal_connect (G_OBJECT(factory_av), "bind", G_CALLBACK(factory_av_bind), NULL);
    column = gtk_column_view_column_new (_("Available"), factory_av);
    gtk_column_view_append_column (GTK_COLUMN_VIEW(view), column);

    GtkSorter *sorter_avail = GTK_SORTER(gtk_string_sorter_new (
                                         gtk_property_expression_new (DOCLINKVIEW_TYPE_ITEM, NULL, "available")));
    gtk_column_view_column_set_sorter (column, GTK_SORTER(sorter_avail));
    g_object_unref (sorter_avail);


    GtkListItemFactory *factory_rel = gtk_signal_list_item_factory_new ();
    g_signal_connect (G_OBJECT(factory_rel), "setup", G_CALLBACK(factory_rel_setup), NULL);
    g_signal_connect (G_OBJECT(factory_rel), "bind", G_CALLBACK(factory_rel_bind), NULL);
    column = gtk_column_view_column_new (_("Relative"), factory_rel);
    gtk_column_view_append_column (GTK_COLUMN_VIEW(view), column);

    GtkSorter *sorter_rel = GTK_SORTER(gtk_numeric_sorter_new (
                                       gtk_property_expression_new (DOCLINKVIEW_TYPE_ITEM, NULL, "uri-relative")));
    gtk_column_view_column_set_sorter (column, GTK_SORTER(sorter_rel));
    g_object_unref (sorter_rel);

    gtk_scrolled_window_set_child (GTK_SCROLLED_WINDOW(sw), GTK_WIDGET(view));

    return view;
}
