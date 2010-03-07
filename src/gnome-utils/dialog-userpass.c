/********************************************************************\
 * dialog-userpass.c -- dialog for username/password entry          *
 * Copyright (C) 2001 Gnumatic, Inc.                                *
 * Author: Dave Peticolas <dave@krondo.com>                         *
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

#include "config.h"

#include <gnome.h>

#include "dialog-utils.h"
#include "gnc-ui.h"


gboolean
gnc_get_username_password (GtkWidget *parent,
                           const char *heading,
                           const char *initial_username,
                           const char *initial_password,
                           char **username,
                           char **password)
{
    GtkWidget *dialog;
    GtkWidget *heading_label;
    GtkWidget *username_entry;
    GtkWidget *password_entry;
    GladeXML *xml;
    gint result;

    g_return_val_if_fail (username != NULL, FALSE);
    g_return_val_if_fail (password != NULL, FALSE);

    xml = gnc_glade_xml_new ("userpass.glade", "Username Password Dialog");

    dialog = glade_xml_get_widget (xml, "Username Password Dialog");

    if (parent)
        gtk_window_set_transient_for (GTK_WINDOW (dialog), GTK_WINDOW (parent));

    heading_label  = glade_xml_get_widget (xml, "heading_label");
    username_entry = glade_xml_get_widget (xml, "username_entry");
    password_entry = glade_xml_get_widget (xml, "password_entry");

    if (heading)
        gtk_label_set_text (GTK_LABEL (heading_label), heading);

    if (initial_username)
        gtk_entry_set_text (GTK_ENTRY (username_entry), initial_username);
    gtk_editable_select_region (GTK_EDITABLE (username_entry), 0, -1);

    if (initial_password)
        gtk_entry_set_text (GTK_ENTRY (password_entry), initial_password);

    result = gtk_dialog_run(GTK_DIALOG (dialog));
    gtk_widget_hide(dialog);

    if (result == GTK_RESPONSE_OK)
    {
        *username = gtk_editable_get_chars (GTK_EDITABLE (username_entry), 0, -1);
        *password = gtk_editable_get_chars (GTK_EDITABLE (password_entry), 0, -1);

        gtk_widget_destroy(dialog);
        return TRUE;
    }

    *username = NULL;
    *password = NULL;

    gtk_widget_destroy(dialog);
    return FALSE;
}
