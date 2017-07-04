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

typedef struct _item_file
{
    const gchar *icon_name;
    const gchar *filename;
} item_file;

static item_file item_files[] =
{
    { GNC_ICON_ACCOUNT,           "gnc-account.png"},
    { GNC_ICON_ACCOUNT_REPORT,    "gnc-account-report.png"},
    { GNC_ICON_DELETE_ACCOUNT,    "gnc-account-delete.png"},
    { GNC_ICON_EDIT_ACCOUNT,      "gnc-account-edit.png"},
    { GNC_ICON_NEW_ACCOUNT,       "gnc-account-new.png"},
    { GNC_ICON_OPEN_ACCOUNT,      "gnc-account-open.png"},
    { GNC_ICON_TRANSFER,          "gnc-transfer.png"},
    { GNC_ICON_SCHEDULE,          "gnc-sx-new.png"},
    { GNC_ICON_SPLIT_TRANS,       "gnc-split-trans.png"},
    { GNC_ICON_JUMP_TO,           "gnc-jumpto.png"},
    { GNC_ICON_INVOICE,           "gnc-invoice.png"},
    { GNC_ICON_INVOICE_PAY,       "gnc-invoice-pay.png"},
    { GNC_ICON_INVOICE_POST,      "gnc-invoice-post.png"},
    { GNC_ICON_INVOICE_UNPOST,    "gnc-invoice-unpost.png"},
    { GNC_ICON_INVOICE_NEW,       "gnc-invoice-new.png"},
    { GNC_ICON_INVOICE_EDIT,      "gnc-invoice-edit.png"},
    { GNC_ICON_INVOICE_DUPLICATE, "gnc-invoice-duplicate.png"},
    { GNC_ICON_PDF_EXPORT,        "gnc-gnome-pdf.png"},
    { 0 },
};

void
gnc_load_app_icons (void)
{
    item_file *file;
    const gchar *default_path;
    gchar* pkgdatadir = gnc_path_get_pkgdatadir ();
    default_path = g_build_filename (pkgdatadir, "icons", NULL);
    g_free(pkgdatadir);

    for (file = item_files; file->icon_name; file++)
    {
        GdkPixbuf *pixbuf_sm, *pixbuf_lg;
        char *fullname_sm, *fullname_lg;
        fullname_sm = g_strconcat (default_path, "/hicolor/16x16/actions/", file->filename, NULL);
        fullname_lg = g_strconcat (default_path, "/hicolor/24x24/actions/", file->filename, NULL);

        g_assert (fullname_sm && fullname_lg);

        pixbuf_sm = gnc_gnome_get_gdkpixbuf (fullname_sm);
        pixbuf_lg = gnc_gnome_get_gdkpixbuf (fullname_lg);
        g_assert (pixbuf_sm && pixbuf_lg);

        gtk_icon_theme_add_builtin_icon (file->icon_name, 16, pixbuf_sm);
        gtk_icon_theme_add_builtin_icon (file->icon_name, 24, pixbuf_lg);

        g_object_unref(pixbuf_sm);
        g_object_unref(pixbuf_lg);

        g_free (fullname_sm);
        g_free (fullname_lg);
    }
}
