/*
 * gnc-icons.c -- Functions to add icons for GnuCash to use
 * Copyright (C) 2003 Jan Arne Petersen
 * Author: Jan Arne Petersen <jpetersen@uni-bonn.de>
 */
/********************************************************************\
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
 *                                                                  *
\********************************************************************/


#include <config.h>

#include <gtk/gtk.h>
#include <glib/gi18n.h>

#include "gnc-icons.h"
#include "gnc-filepath-utils.h"
#include "gnc-gnome-utils.h"
#include "gnc-path.h"

static QofLogModule log_module = GNC_MOD_GUI;

static gchar *icon_files[] =
{
    GNC_ICON_APP,
    GNC_ICON_ACCOUNT,
    GNC_ICON_ACCOUNT_REPORT,
    GNC_ICON_DELETE_ACCOUNT,
    GNC_ICON_EDIT_ACCOUNT,
    GNC_ICON_NEW_ACCOUNT,
    GNC_ICON_OPEN_ACCOUNT,
    GNC_ICON_TRANSFER,
    GNC_ICON_SCHEDULE,
    GNC_ICON_SPLIT_TRANS,
    GNC_ICON_JUMP_TO,
    GNC_ICON_INVOICE,
    GNC_ICON_INVOICE_PAY,
    GNC_ICON_INVOICE_POST,
    GNC_ICON_INVOICE_UNPOST,
    GNC_ICON_INVOICE_NEW,
    GNC_ICON_INVOICE_EDIT,
    GNC_ICON_INVOICE_DUPLICATE,
    GNC_ICON_PDF_EXPORT,
    NULL
};

void
gnc_load_app_icons (void)
{
    GtkIconTheme *icon_theme = gtk_icon_theme_get_default ();
    const gchar *default_path;
    gchar* pkgdatadir = gnc_path_get_pkgdatadir ();
    gchar* datadir = gnc_path_get_datadir ();
    gchar **path;
    gint n_elements, i;

    default_path = g_build_filename (pkgdatadir, "icons", NULL);
    gtk_icon_theme_append_search_path (icon_theme, default_path);
    default_path = g_build_filename (datadir, "icons", NULL);
    gtk_icon_theme_append_search_path (icon_theme, default_path);
    g_free (pkgdatadir);
    g_free (datadir);

    gtk_icon_theme_get_search_path (icon_theme,
                                    &path,
                                    &n_elements);
    PINFO ("The icon theme search path has %i elements.", n_elements);
    if (n_elements > 0)
    {
        for (i = 0; i < n_elements; i++)
            PINFO ("Path %i: %s", i, path[i]);
    }

    for (i = 0; icon_files[i]; i++)
    {
        gchar *file = icon_files[i];
        // check if we have at least one size for the named icons loaded
        if (!gtk_icon_theme_has_icon (icon_theme, file))
            PWARN ("No icon named '%s' found. Some gui elements may be missing their icons", file);
    }
}
