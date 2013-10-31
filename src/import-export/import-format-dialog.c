/*
 * import-format-dialog.c -- provides a UI to ask for users to resolve
 *                           ambiguities.
 *
 * Created by:	Derek Atkins <derek@ihtfp.com>
 * Copyright (c) 2003 Derek Atkins <warlord@MIT.EDU>
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
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include "dialog-utils.h"
#include "import-parse.h"
#include "gnc-ui-util.h"

#define MAX_CHOICES 6

static void
option_changed_cb (GtkWidget *widget, gpointer index_p)
{
    gint *my_index = index_p;
    *my_index = gtk_combo_box_get_active(GTK_COMBO_BOX(widget));
}


static GncImportFormat
add_menu_and_run_dialog(GtkWidget *dialog, GtkWidget *menu_box, GncImportFormat fmt)
{
    GtkComboBox  *combo;
    GtkListStore *store;
    GtkTreeIter iter;
    GtkCellRenderer *cell;
    gint index = 0, count = 0;
    gint *index_p = &index;
    GncImportFormat formats[MAX_CHOICES];

    store = gtk_list_store_new(1, G_TYPE_STRING);

    if (fmt & GNCIF_NUM_PERIOD)
    {
        gtk_list_store_append (store, &iter);
        gtk_list_store_set (store, &iter, 0, _("Period: 123,456.78"), -1);
        formats[count] = GNCIF_NUM_PERIOD;
        count++;
    }

    if (fmt & GNCIF_NUM_COMMA)
    {
        gtk_list_store_append (store, &iter);
        gtk_list_store_set (store, &iter, 0, _("Comma: 123.456,78"), -1);
        formats[count] = GNCIF_NUM_COMMA;
        count++;
    }

    if (fmt & GNCIF_DATE_MDY)
    {
        gtk_list_store_append (store, &iter);
        gtk_list_store_set (store, &iter, 0, _("m/d/y"), -1);
        formats[count] = GNCIF_DATE_MDY;
        count++;
    }

    if (fmt & GNCIF_DATE_DMY)
    {
        gtk_list_store_append (store, &iter);
        gtk_list_store_set (store, &iter, 0, _("d/m/y"), -1);
        formats[count] = GNCIF_DATE_DMY;
        count++;
    }

    if (fmt & GNCIF_DATE_YMD)
    {
        gtk_list_store_append (store, &iter);
        gtk_list_store_set (store, &iter, 0, _("y/m/d"), -1);
        formats[count] = GNCIF_DATE_YMD;
        count++;
    }

    if (fmt & GNCIF_DATE_YDM)
    {
        gtk_list_store_append (store, &iter);
        gtk_list_store_set (store, &iter, 0, _("y/d/m"), -1);
        formats[count] = GNCIF_DATE_YDM;
        count++;
    }

    g_assert(count > 1);

    combo = GTK_COMBO_BOX(gtk_combo_box_new_with_model(GTK_TREE_MODEL(store)));
    g_object_unref(store);

    /* Create cell renderer. */
    cell = gtk_cell_renderer_text_new();

    /* Pack it to the combo box. */
    gtk_cell_layout_pack_start( GTK_CELL_LAYOUT( combo ), cell, FALSE );

    /* Connect renderer to data source */
    gtk_cell_layout_set_attributes( GTK_CELL_LAYOUT( combo ), cell, "text", 0, NULL );

    g_signal_connect(G_OBJECT(combo), "changed",
                     G_CALLBACK(option_changed_cb), index_p);

    gtk_box_pack_start(GTK_BOX(menu_box), GTK_WIDGET(combo), TRUE, TRUE, 0);

    gtk_widget_show_all(dialog);
    gtk_window_set_modal(GTK_WINDOW(dialog), TRUE);
    gtk_dialog_run(GTK_DIALOG(dialog));
    gtk_widget_destroy(dialog);

    return formats[index];
}


GncImportFormat
gnc_import_choose_fmt(const char* msg, GncImportFormat fmts, gpointer data)
{
    GtkBuilder *builder;
    GtkWidget *dialog;
    GtkWidget *widget;

    g_return_val_if_fail(fmts, FALSE);

    /* if there is only one format availble, just return it */
    if (!(fmts & (fmts - 1)))
    {
        return fmts;
    }
    /* Open the Glade Builder file */
    builder = gtk_builder_new();
    gnc_builder_add_from_file (builder, "dialog-import.glade", "format_picker");
    dialog = GTK_WIDGET(gtk_builder_get_object (builder, "format_picker"));
    widget = GTK_WIDGET(gtk_builder_get_object (builder, "msg_label"));
    gtk_label_set_text(GTK_LABEL(widget), msg);

    widget = GTK_WIDGET(gtk_builder_get_object (builder, "menu_box"));

    g_object_unref(G_OBJECT(builder));

    return add_menu_and_run_dialog(dialog, widget, fmts);
}
