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


#include "config.h"

#include <gtk/gtk.h>
#include <glib/gi18n.h>

#include "gnc-icons.h"
#include "gnc-filepath-utils.h"
#include "gnc-gnome-utils.h"
#include "gnc-path.h"

typedef enum  {
    APP_ICON,
    ACTION_ICON
} IconFileType;

typedef struct _icon_file
{
    const gchar *icon_name;
    const gchar *filename;
    const IconFileType icon_type;
} icon_file;

static icon_file icon_files[] =
{
    { GNC_ICON_APP,               "gnucash-icon.png",          APP_ICON},
    { GNC_ICON_ACCOUNT,           "gnc-account.png",           ACTION_ICON},
    { GNC_ICON_ACCOUNT_REPORT,    "gnc-account-report.png",    ACTION_ICON},
    { GNC_ICON_DELETE_ACCOUNT,    "gnc-account-delete.png",    ACTION_ICON},
    { GNC_ICON_EDIT_ACCOUNT,      "gnc-account-edit.png",      ACTION_ICON},
    { GNC_ICON_NEW_ACCOUNT,       "gnc-account-new.png",       ACTION_ICON},
    { GNC_ICON_OPEN_ACCOUNT,      "gnc-account-open.png",      ACTION_ICON},
    { GNC_ICON_TRANSFER,          "gnc-transfer.png",          ACTION_ICON},
    { GNC_ICON_SCHEDULE,          "gnc-sx-new.png",            ACTION_ICON},
    { GNC_ICON_SPLIT_TRANS,       "gnc-split-trans.png",       ACTION_ICON},
    { GNC_ICON_JUMP_TO,           "gnc-jumpto.png",            ACTION_ICON},
    { GNC_ICON_INVOICE,           "gnc-invoice.png",           ACTION_ICON},
    { GNC_ICON_INVOICE_PAY,       "gnc-invoice-pay.png",       ACTION_ICON},
    { GNC_ICON_INVOICE_POST,      "gnc-invoice-post.png",      ACTION_ICON},
    { GNC_ICON_INVOICE_UNPOST,    "gnc-invoice-unpost.png",    ACTION_ICON},
    { GNC_ICON_INVOICE_NEW,       "gnc-invoice-new.png",       ACTION_ICON},
    { GNC_ICON_INVOICE_EDIT,      "gnc-invoice-edit.png",      ACTION_ICON},
    { GNC_ICON_INVOICE_DUPLICATE, "gnc-invoice-duplicate.png", ACTION_ICON},
    { GNC_ICON_PDF_EXPORT,        "gnc-gnome-pdf.png",         ACTION_ICON},
    { GNC_ICON_PDF_EXPORT,        "gnc-gnome-pdf.png",         ACTION_ICON},
    { 0 },
};

void
gnc_load_app_icons (void)
{
    GtkIconTheme *icon_theme = gtk_icon_theme_get_default ();
    icon_file *file;
    const gchar *default_path;
    gchar* pkgdatadir = gnc_path_get_pkgdatadir ();
    gchar* datadir = gnc_path_get_datadir ();

    default_path = g_build_filename (pkgdatadir, "icons", NULL);
    g_free (pkgdatadir);
    gtk_icon_theme_append_search_path (icon_theme, default_path);
    default_path = g_build_filename (datadir, "icons", NULL);
    g_free (datadir);
    gtk_icon_theme_append_search_path (icon_theme, default_path);

    for (file = icon_files; file->icon_name; file++)
    {

        gint *icon_sizes = gtk_icon_theme_get_icon_sizes (icon_theme, file->icon_name);

        if ((file->icon_type == ACTION_ICON) &&
            (icon_sizes[0] != 16) && (icon_sizes[1] != 24))
            g_warning ("Required icon size for icon name '%s' not found", file->icon_name);
        else if ((icon_sizes[0] != 16) && (icon_sizes[3] != 32) && (icon_sizes[4] != 48))
            g_warning ("Required icon size for icon name '%s' not found", file->icon_name);
        g_free (icon_sizes);

        // check to see if we have at least one size for the named icons loaded
        g_assert (gtk_icon_theme_has_icon (icon_theme, file->icon_name));
    }
}
