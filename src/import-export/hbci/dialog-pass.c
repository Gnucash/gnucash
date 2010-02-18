/********************************************************************\
 * dialog-pass.c -- dialog for password entry                       *
 * Copyright (C) 2002 Christian Stimming                            *
 * heavily copied from Dave Peticolas <dave@krondo.com>             *
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

#include <gtk/gtk.h>
#include <glib/gi18n.h>

#include "dialog-utils.h"
#include "gnc-ui.h"
#include "dialog-pass.h"

gboolean
gnc_hbci_get_password (GtkWidget *parent,
                       const char *windowtitle,
                       const char *heading,
                       const char *initial_password,
                       char **password,
                       gboolean hide_input)
{
    GtkWidget *dialog;
    GtkWidget *heading_label;
    GtkWidget *password_entry;
    GladeXML *xml;
    gint result;

    g_return_val_if_fail (password != NULL, FALSE);

    xml = gnc_glade_xml_new ("hbcipass.glade", "Password Dialog");

    dialog = glade_xml_get_widget (xml, "Password Dialog");

    if (parent)
        gtk_window_set_transient_for (GTK_WINDOW (dialog), GTK_WINDOW (parent));

    heading_label  = glade_xml_get_widget (xml, "heading_label");
    password_entry = glade_xml_get_widget (xml, "password_entry");
    g_assert(heading_label && password_entry);

    gtk_dialog_set_default_response (GTK_DIALOG (dialog), 1);

    gtk_entry_set_activates_default (GTK_ENTRY (password_entry), TRUE);

    if (windowtitle)
        gtk_window_set_title (GTK_WINDOW (dialog), windowtitle);

    if (heading)
        gtk_label_set_text (GTK_LABEL (heading_label), heading);

    if (initial_password)
        gtk_entry_set_text (GTK_ENTRY (password_entry), initial_password);
    gtk_entry_set_visibility (GTK_ENTRY (password_entry), !hide_input);

    result = gtk_dialog_run (GTK_DIALOG (dialog));

    if (result == 1) /* the hand-assigned response value */
    {
        *password = g_strdup (gtk_entry_get_text (GTK_ENTRY (password_entry)) );
        gtk_widget_destroy(dialog);
        return TRUE;
    }
    gtk_widget_destroy(dialog);

    *password = NULL;
    return FALSE;
}


gboolean
gnc_hbci_get_initial_password (GtkWidget *parent,
                               const char *windowtitle,
                               const char *heading,
                               char **password)
{
    GtkWidget *dialog;
    GtkWidget *heading_label;
    GtkWidget *password_entry;
    GtkWidget *confirm_entry;
    GladeXML *xml;
    gint result;

    g_return_val_if_fail (password != NULL, FALSE);

    xml = gnc_glade_xml_new ("hbcipass.glade", "Initial Password Dialog");

    dialog = glade_xml_get_widget (xml, "Initial Password Dialog");

    if (parent)
        gtk_window_set_transient_for (GTK_WINDOW (dialog), GTK_WINDOW (parent));

    heading_label  = glade_xml_get_widget (xml, "heading_label");
    password_entry = glade_xml_get_widget (xml, "password_entry");
    confirm_entry = glade_xml_get_widget (xml, "confirm_entry");
    g_assert(heading_label && password_entry && confirm_entry);

    gtk_dialog_set_default_response (GTK_DIALOG (dialog), 1);

    gtk_entry_set_activates_default (GTK_ENTRY (password_entry), FALSE);
    gtk_entry_set_activates_default (GTK_ENTRY (confirm_entry), TRUE);

    if (windowtitle)
        gtk_window_set_title (GTK_WINDOW (dialog), windowtitle);

    if (heading)
        gtk_label_set_text (GTK_LABEL (heading_label), heading);

    while (TRUE)
    {
        result = gtk_dialog_run (GTK_DIALOG (dialog));

        if (result == 1) /* the hand-assigned response value */
        {
            const char *pw = gtk_entry_get_text (GTK_ENTRY (password_entry));
            const char *confirm = gtk_entry_get_text (GTK_ENTRY (confirm_entry));
            if (strcmp (pw, confirm) == 0)
            {
                *password = g_strdup(pw);
                gtk_widget_destroy (GTK_WIDGET (dialog));
                return TRUE;
            }
        }
        else
            break;

        /* strings didn't match */
        if (gnc_ok_cancel_dialog (parent, GTK_RESPONSE_OK,
                                  _("The two passwords didn't match. "
                                    "Please try again."))
                != GTK_RESPONSE_OK)
            break;
    }
    *password = NULL;
    gtk_widget_destroy (GTK_WIDGET (dialog));
    return FALSE;
}
