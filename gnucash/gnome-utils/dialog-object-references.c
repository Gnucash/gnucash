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

void
gnc_ui_object_references_show( const gchar* explanation_text, GList* objlist )
{
    GtkWidget* dialog;
    GtkBuilder* builder;
    GtkWidget* box;
    GList* node;
    GtkLabel* explanation;
    GtkListStore* store;
    GtkWidget* listview;
    GtkTreeViewColumn* column;
    GtkCellRenderer* renderer;

    ENTER("");

    /* Open the dialog */
    builder = gtk_builder_new();
    gnc_builder_add_from_file (builder, "dialog-object-references.glade", "object_references_dialog" );
    dialog = GTK_WIDGET(gtk_builder_get_object (builder, "object_references_dialog" ));

    // Set the style context for this dialog so it can be easily manipulated with css
    gnc_widget_set_style_context (GTK_WIDGET(dialog), "GncObjectRefDialog");

    explanation = GTK_LABEL(gtk_builder_get_object (builder, "lbl_explanation" ));
    gtk_label_set_text( explanation, explanation_text );

    /* Set up the list store */
    store = gtk_list_store_new( 1, G_TYPE_STRING );
    for ( node = objlist; node != NULL; node = node->next )
    {
        QofInstance* inst = node->data;
        GtkTreeIter iter;

        gtk_list_store_append( store, &iter );
        gtk_list_store_set( store, &iter, 0, qof_instance_get_display_name( inst ), -1 );
    }

    /* Set up the list view */
    listview = gtk_tree_view_new_with_model( GTK_TREE_MODEL(store) );
    renderer = gtk_cell_renderer_text_new();
    column = gtk_tree_view_column_new_with_attributes( "Object", renderer, "text", 0, NULL );
    gtk_tree_view_append_column( GTK_TREE_VIEW(listview), column );

    box = GTK_WIDGET(gtk_builder_get_object (builder, "hbox_list" ));
    gtk_container_add( GTK_CONTAINER(box), listview );

    /* Autoconnect signals */
    gtk_builder_connect_signals_full (builder, gnc_builder_connect_full_func, dialog);

    /* Run the dialog */
    gtk_widget_show_all( dialog );
    gtk_dialog_run( GTK_DIALOG(dialog) );
    g_object_unref(G_OBJECT(builder));
    gtk_widget_destroy( dialog );

    LEAVE("");
}
