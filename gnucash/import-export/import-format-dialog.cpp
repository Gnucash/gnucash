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
#include <config.h>
#endif

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include "dialog-utils.h"
#include "gnc-gui-query.h"
#include "import-parse.h"
#include "gnc-ui-util.h"

#define MAX_CHOICES 6

typedef struct
{
    GncImportFormatCallback completed;
    gpointer user_data;
    GncImportFormat formats[MAX_CHOICES];
    GncImportFormat result;
} ImportFormatRequest;

static void
format_response_captured ([[maybe_unused]] GtkDialog *dialog, gint response,
                          gpointer user_data)
{
    auto request = static_cast<ImportFormatRequest*>(user_data);
    if (response != GTK_RESPONSE_CANCEL && response != GTK_RESPONSE_DELETE_EVENT &&
        response != GTK_RESPONSE_NONE)
    {
        auto combo = GTK_COMBO_BOX (g_object_get_data (G_OBJECT (dialog),
                                                      "format-combo"));
        auto index = gtk_combo_box_get_active (combo);
        if (index >= 0)
            request->result = request->formats[index];
    }
}

static void
format_dialog_completed (GtkWindow *parent, gint response, gpointer user_data)
{
    auto request = static_cast<ImportFormatRequest*>(user_data);
    request->completed (request->result, request->user_data);
    delete request;
}

static void
add_menu_to_dialog(GtkWidget *dialog, GtkWidget *menu_box, GncImportFormat fmt,
                   ImportFormatRequest *request)
{
    GtkComboBox  *combo;
    GtkListStore *store;
    GtkTreeIter iter;
    GtkCellRenderer *cell;
    gint count = 0;

    store = gtk_list_store_new(1, G_TYPE_STRING);

    if (fmt & GNCIF_NUM_PERIOD)
    {
        gtk_list_store_append (store, &iter);
        gtk_list_store_set (store, &iter, 0, _("Period: 123,456.78"), -1);
        request->formats[count] = GNCIF_NUM_PERIOD;
        count++;
    }

    if (fmt & GNCIF_NUM_COMMA)
    {
        gtk_list_store_append (store, &iter);
        gtk_list_store_set (store, &iter, 0, _("Comma: 123.456,78"), -1);
        request->formats[count] = GNCIF_NUM_COMMA;
        count++;
    }

    if (fmt & GNCIF_DATE_MDY)
    {
        gtk_list_store_append (store, &iter);
        gtk_list_store_set (store, &iter, 0, _("m/d/y"), -1);
        request->formats[count] = GNCIF_DATE_MDY;
        count++;
    }

    if (fmt & GNCIF_DATE_DMY)
    {
        gtk_list_store_append (store, &iter);
        gtk_list_store_set (store, &iter, 0, _("d/m/y"), -1);
        request->formats[count] = GNCIF_DATE_DMY;
        count++;
    }

    if (fmt & GNCIF_DATE_YMD)
    {
        gtk_list_store_append (store, &iter);
        gtk_list_store_set (store, &iter, 0, _("y/m/d"), -1);
        request->formats[count] = GNCIF_DATE_YMD;
        count++;
    }

    if (fmt & GNCIF_DATE_YDM)
    {
        gtk_list_store_append (store, &iter);
        gtk_list_store_set (store, &iter, 0, _("y/d/m"), -1);
        request->formats[count] = GNCIF_DATE_YDM;
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

    g_object_set_data (G_OBJECT (dialog), "format-combo", combo);
    gtk_combo_box_set_active (combo, 0);

    gtk_box_pack_start(GTK_BOX(menu_box), GTK_WIDGET(combo), TRUE, TRUE, 0);

    gtk_widget_show_all(dialog);
    gtk_window_set_modal(GTK_WINDOW(dialog), TRUE);
    g_signal_connect (dialog, "response",
                      G_CALLBACK (format_response_captured), request);
    gnc_dialog_run_async (GTK_DIALOG (dialog), NULL,
                          format_dialog_completed, request);
}

void
gnc_import_choose_fmt_async (const char* msg, GncImportFormat fmts,
                             GncImportFormatCallback completed,
                             gpointer data)
{
    GtkBuilder *builder;
    GtkWidget *dialog;
    GtkWidget *widget;

    g_return_if_fail (fmts && completed);

    /* if there is only one format available, just return it */
    if (!(fmts & (fmts - 1)))
    {
        completed (fmts, data);
        return;
    }
    /* Open the Glade Builder file */
    builder = gtk_builder_new();
    gnc_builder_add_from_file (builder, "dialog-import.glade", "format_picker_dialog");
    dialog = GTK_WIDGET(gtk_builder_get_object (builder, "format_picker_dialog"));
    widget = GTK_WIDGET(gtk_builder_get_object (builder, "msg_label"));
    gtk_label_set_text(GTK_LABEL(widget), msg);

    widget = GTK_WIDGET(gtk_builder_get_object (builder, "menu_box"));

    g_object_unref(G_OBJECT(builder));

    auto request = new ImportFormatRequest{};
    request->completed = completed;
    request->user_data = data;
    request->result = GNCIF_NONE;
    add_menu_to_dialog(dialog, widget, fmts, request);
}
