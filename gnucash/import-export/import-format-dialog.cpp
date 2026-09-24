/*
 * import-format-dialog.cpp -- provides a GTK4 UI to resolve import format
 *                             ambiguities without a nested event loop.
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
#include "gnc-gtk-utils.h"
#include "import-parse.h"

#define MAX_CHOICES 6

struct FormatPicker
{
    GtkWidget *dialog;
    GtkDropDown *dropdown;
    GncImportFormat formats[MAX_CHOICES];
    guint count;
    GncImportFormatChosenCB chosen_cb;
    gpointer user_data;
    gboolean finished;
};

static void
format_picker_finish (FormatPicker *picker, gint response)
{
    if (!picker || picker->finished)
        return;
    picker->finished = TRUE;

    auto format = GNCIF_NONE;
    if (response == GTK_RESPONSE_OK)
    {
        auto selected = gtk_drop_down_get_selected (picker->dropdown);
        if (selected < picker->count)
            format = picker->formats[selected];
    }

    auto chosen_cb = picker->chosen_cb;
    auto user_data = picker->user_data;
    gtk_window_destroy (GTK_WINDOW (picker->dialog));
    g_free (picker);
    if (chosen_cb)
        chosen_cb (format, user_data);
}

static void
format_picker_ok_clicked_cb (GtkButton *button, FormatPicker *picker)
{
    (void)button;
    format_picker_finish (picker, GTK_RESPONSE_OK);
}

static gboolean
format_picker_close_request_cb (GtkWindow *window, FormatPicker *picker)
{
    (void)window;
    format_picker_finish (picker, GTK_RESPONSE_CANCEL);
    return TRUE;
}

static void
format_picker_add_choice (FormatPicker *picker, const char **labels,
                          GncImportFormat format, const char *label)
{
    picker->formats[picker->count] = format;
    labels[picker->count] = label;
    ++picker->count;
}

void
gnc_import_choose_fmt_async (GtkWindow *parent, const char *msg,
                             GncImportFormat fmts,
                             GncImportFormatChosenCB chosen_cb,
                             gpointer user_data)
{
    g_return_if_fail (fmts);

    if (!(fmts & (fmts - 1)))
    {
        if (chosen_cb)
            chosen_cb (fmts, user_data);
        return;
    }

    auto picker = g_new0 (FormatPicker, 1);
    picker->chosen_cb = chosen_cb;
    picker->user_data = user_data;
    const char *labels[MAX_CHOICES + 1] {};
    if (fmts & GNCIF_NUM_PERIOD)
        format_picker_add_choice (picker, labels, GNCIF_NUM_PERIOD, _("Period: 123,456.78"));
    if (fmts & GNCIF_NUM_COMMA)
        format_picker_add_choice (picker, labels, GNCIF_NUM_COMMA, _("Comma: 123.456,78"));
    if (fmts & GNCIF_DATE_MDY)
        format_picker_add_choice (picker, labels, GNCIF_DATE_MDY, _("m/d/y"));
    if (fmts & GNCIF_DATE_DMY)
        format_picker_add_choice (picker, labels, GNCIF_DATE_DMY, _("d/m/y"));
    if (fmts & GNCIF_DATE_YMD)
        format_picker_add_choice (picker, labels, GNCIF_DATE_YMD, _("y/m/d"));
    if (fmts & GNCIF_DATE_YDM)
        format_picker_add_choice (picker, labels, GNCIF_DATE_YDM, _("y/d/m"));
    g_assert (picker->count > 1);

    auto builder = gtk_builder_new ();
    gnc_builder_add_from_file (builder, "dialog-import.glade", "format_picker_dialog");
    picker->dialog = GTK_WIDGET (gtk_builder_get_object (builder, "format_picker_dialog"));
    auto message = GTK_LABEL (gtk_builder_get_object (builder, "msg_label"));
    auto menu_box = GTK_BOX (gtk_builder_get_object (builder, "menu_box"));
    auto ok_button = GTK_BUTTON (gtk_builder_get_object (builder, "okbutton1"));
    g_return_if_fail (picker->dialog && message && menu_box && ok_button);

    gtk_label_set_text (message, msg);
    picker->dropdown = gnc_gtk_drop_down_new_from_strings (labels);
    gtk_box_append (menu_box, GTK_WIDGET (picker->dropdown));
    if (parent)
        gtk_window_set_transient_for (GTK_WINDOW (picker->dialog), parent);
    gtk_window_set_modal (GTK_WINDOW (picker->dialog), TRUE);
    g_signal_connect (ok_button, "clicked", G_CALLBACK (format_picker_ok_clicked_cb), picker);
    g_signal_connect (picker->dialog, "close-request", G_CALLBACK (format_picker_close_request_cb), picker);
    gtk_window_set_default_widget (GTK_WINDOW (picker->dialog), GTK_WIDGET (ok_button));
    g_object_unref (builder);
    gtk_widget_set_visible (picker->dialog, TRUE);
}
