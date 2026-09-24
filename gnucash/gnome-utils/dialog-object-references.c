/********************************************************************\
 * dialog-object-references.c -- dialog for displaying a list of    *
 *                               objects which refer to another     *
 *                               specific object                    *
 *                                                                  *
 * Copyright (C) 2010 Phil Longstaff (plongstaff@rogers.com)        *
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

#include "gnc-ui.h"
#include "dialog-utils.h"
#include "dialog-object-references.h"

static QofLogModule log_module = GNC_MOD_GUI;

static void
object_reference_item_setup (GtkListItemFactory *factory, GtkListItem *item,
                             gpointer user_data)
{
    GtkWidget *label = gtk_label_new (NULL);

    (void)factory;
    (void)user_data;
    gtk_label_set_xalign (GTK_LABEL (label), 0.0);
    gtk_label_set_ellipsize (GTK_LABEL (label), PANGO_ELLIPSIZE_END);
    gtk_list_item_set_child (item, label);
}

static void
object_reference_item_bind (GtkListItemFactory *factory, GtkListItem *item,
                            gpointer user_data)
{
    GtkStringObject *row = GTK_STRING_OBJECT (gtk_list_item_get_item (item));

    (void)factory;
    (void)user_data;
    gtk_label_set_text (GTK_LABEL (gtk_list_item_get_child (item)),
                        gtk_string_object_get_string (row));
}

static void
object_references_dialog_close_clicked_cb (GtkButton *button,
                                           GtkWindow *dialog)
{
    gtk_window_destroy (dialog);
    (void)button;
}

void
gnc_ui_object_references_show( const gchar* explanation_text, GList* objlist )
{
    GtkWidget* dialog;
    GtkBuilder* builder;
    GtkWidget* box;
    GList* node;
    GtkLabel* explanation;
    GListStore* store;
    GtkNoSelection* selection;
    GtkColumnView* listview;
    GtkListItemFactory* factory;
    GtkColumnViewColumn* column;

    ENTER("");

    /* Open the dialog */
    builder = gtk_builder_new();
    gnc_builder_add_from_file (builder, "dialog-object-references.glade", "object_references_dialog" );
    dialog = GTK_WIDGET(gtk_builder_get_object (builder, "object_references_dialog" ));

    // Set the name for this dialog so it can be easily manipulated with css
    gtk_widget_set_name (GTK_WIDGET(dialog), "gnc-id-object-reference");

    explanation = GTK_LABEL(gtk_builder_get_object (builder, "lbl_explanation" ));
    gtk_label_set_text( explanation, explanation_text );

    /* The model owns only the display names. The caller retains the instances. */
    store = g_list_store_new (GTK_TYPE_STRING_OBJECT);
    for ( node = objlist; node != NULL; node = node->next )
    {
        QofInstance* inst = node->data;
        GtkStringObject* row = gtk_string_object_new (qof_instance_get_display_name (inst));

        g_list_store_append (store, row);
        g_object_unref (row);
    }

    /* Set up the GTK4 list view. Object references are informational, so it
     * deliberately has no selection model. */
    selection = gtk_no_selection_new (G_LIST_MODEL (store));
    listview = GTK_COLUMN_VIEW (gtk_column_view_new (GTK_SELECTION_MODEL (selection)));
    factory = gtk_signal_list_item_factory_new ();
    g_signal_connect (factory, "setup", G_CALLBACK (object_reference_item_setup), NULL);
    g_signal_connect (factory, "bind", G_CALLBACK (object_reference_item_bind), NULL);
    column = gtk_column_view_column_new (_("Object"), factory);
    gtk_column_view_column_set_expand (column, TRUE);
    gtk_column_view_append_column (listview, column);
    g_object_unref (column);

    box = GTK_WIDGET(gtk_builder_get_object (builder, "hbox_list" ));
    gtk_box_prepend (GTK_BOX(box), GTK_WIDGET(listview));
    gtk_window_set_default_widget (
        GTK_WINDOW (dialog),
        GTK_WIDGET (gtk_builder_get_object (builder, "okbutton")));
    g_signal_connect (gtk_builder_get_object (builder, "okbutton"), "clicked",
                      G_CALLBACK (object_references_dialog_close_clicked_cb),
                      dialog);

    g_object_unref(G_OBJECT(builder));
    gtk_window_set_modal (GTK_WINDOW(dialog), TRUE);
    gtk_window_present (GTK_WINDOW(dialog));

    LEAVE("");
}
