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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
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

  dialog = dialog = glade_xml_get_widget (xml, "Username Password Dialog");

  if (parent)
    gnome_dialog_set_parent (GNOME_DIALOG (dialog), GTK_WINDOW (parent));

  heading_label  = glade_xml_get_widget (xml, "heading_label");
  username_entry = glade_xml_get_widget (xml, "username_entry");
  password_entry = glade_xml_get_widget (xml, "password_entry");

  gnome_dialog_set_default (GNOME_DIALOG (dialog), 0);

  gnome_dialog_editable_enters (GNOME_DIALOG (dialog),
                                GTK_EDITABLE (username_entry));
  gnome_dialog_editable_enters (GNOME_DIALOG (dialog),
                                GTK_EDITABLE (password_entry));

  if (heading)
    gtk_label_set_text (GTK_LABEL (heading_label), heading);

  if (initial_username)
    gtk_entry_set_text (GTK_ENTRY (username_entry), initial_username);

  if (initial_password)
    gtk_entry_set_text (GTK_ENTRY (password_entry), initial_password);

  gtk_widget_grab_focus (username_entry);

  result = gnome_dialog_run_and_close (GNOME_DIALOG (dialog));

  if (result == 0)
  {
    *username = gtk_editable_get_chars (GTK_EDITABLE (username_entry), 0, -1);
    *password = gtk_editable_get_chars (GTK_EDITABLE (password_entry), 0, -1);

    return TRUE;
  }

  *username = NULL;
  *password = NULL;

  return FALSE;
}
