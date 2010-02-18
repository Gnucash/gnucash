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
#include <glade/glade.h>

#include "import-parse.h"
#include "dialog-utils.h"
#include "gnc-ui-util.h"

#define MAX_CHOICES 6

static void
choice_option_changed (GtkWidget *widget, gint index, gpointer index_p)
{
    gint *my_index = index_p;
    *my_index = index;
}

static GncImportFormat
add_menu_and_run_dialog(GtkWidget *dialog, GtkWidget *menu_box, GncImportFormat fmt)
{
    GtkWidget *menu;
    gint index = 0, count = 0;
    GncImportFormat formats[MAX_CHOICES];
    GNCOptionInfo menus[MAX_CHOICES];

    memset(&menus, 0, sizeof(menus));

    if (fmt & GNCIF_NUM_PERIOD)
    {
        formats[count] = GNCIF_NUM_PERIOD;
        menus[count].name = _("Period: 123,456.78");
        menus[count].callback = choice_option_changed;
        menus[count].user_data = &index;
        count++;
    }

    if (fmt & GNCIF_NUM_COMMA)
    {
        formats[count] = GNCIF_NUM_COMMA;
        menus[count].name = _("Comma: 123.456,78");
        menus[count].callback = choice_option_changed;
        menus[count].user_data = &index;
        count++;
    }

    if (fmt & GNCIF_DATE_MDY)
    {
        formats[count] = GNCIF_DATE_MDY;
        menus[count].name = _("m/d/y");
        menus[count].callback = choice_option_changed;
        menus[count].user_data = &index;
        count++;
    }

    if (fmt & GNCIF_DATE_DMY)
    {
        formats[count] = GNCIF_DATE_DMY;
        menus[count].name = _("d/m/y");
        menus[count].callback = choice_option_changed;
        menus[count].user_data = &index;
        count++;
    }

    if (fmt & GNCIF_DATE_YMD)
    {
        formats[count] = GNCIF_DATE_YMD;
        menus[count].name = _("y/m/d");
        menus[count].callback = choice_option_changed;
        menus[count].user_data = &index;
        count++;
    }

    if (fmt & GNCIF_DATE_YDM)
    {
        formats[count] = GNCIF_DATE_YDM;
        menus[count].name = _("y/d/m");
        menus[count].callback = choice_option_changed;
        menus[count].user_data = &index;
        count++;
    }

    g_assert(count > 1);
    menu = gnc_build_option_menu(menus, count);
    gtk_box_pack_start(GTK_BOX(menu_box), menu, TRUE, TRUE, 0);

    gtk_widget_show_all(dialog);
    gtk_window_set_modal(GTK_WINDOW(dialog), TRUE);
    gtk_dialog_run(GTK_DIALOG(dialog));
    gtk_widget_destroy(dialog);

    return formats[index];
}

GncImportFormat
gnc_import_choose_fmt(const char* msg, GncImportFormat fmts, gpointer data)

{
    GladeXML *xml;
    GtkWidget *dialog;
    GtkWidget *widget;

    g_return_val_if_fail(fmts, FALSE);

    /* if there is only one format availble, just return it */
    if (!(fmts & (fmts - 1)))
    {
        return fmts;
    }

    xml = gnc_glade_xml_new("generic-import.glade", "format_picker");
    dialog = glade_xml_get_widget(xml, "format_picker");
    widget = glade_xml_get_widget(xml, "msg_label");
    gtk_label_set_text(GTK_LABEL(widget), msg);

    widget = glade_xml_get_widget(xml, "menu_box");
    return add_menu_and_run_dialog(dialog, widget, fmts);
}
