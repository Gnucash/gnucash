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

typedef struct _item_file
{
    const gchar *icon_name;
    const gchar *filename_lg;
    const gchar *filename_sm;
} item_file;

static item_file item_files[] =
{
    { GNC_ICON_ACCOUNT,           "gnc-account.png",           "gnc-account-16.png"},
    { GNC_ICON_ACCOUNT_REPORT,    "gnc-account-report.png",    "gnc-account-report-16.png"},
    { GNC_ICON_DELETE_ACCOUNT,    "gnc-account-delete.png",    "gnc-account-delete-16.png"},
    { GNC_ICON_EDIT_ACCOUNT,      "gnc-account-edit.png",      "gnc-account-edit-16.png"},
    { GNC_ICON_NEW_ACCOUNT,       "gnc-account-new.png",       "gnc-account-new-16.png"},
    { GNC_ICON_OPEN_ACCOUNT,      "gnc-account-open.png",      "gnc-account-open-16.png"},
    { GNC_ICON_TRANSFER,          "gnc-transfer.png",          "gnc-transfer-16.png"},
    { GNC_ICON_SCHEDULE,          "gnc-sx-new.png",            "gnc-sx-new-16.png"},
    { GNC_ICON_SPLIT_TRANS,       "gnc-split-trans.png",       "gnc-split-trans-16.png"},
    { GNC_ICON_JUMP_TO,           "gnc-jumpto.png",            "gnc-jumpto-16.png"},
    { GNC_ICON_INVOICE,           "gnc-invoice.png",           "gnc-invoice-16.png"},
    { GNC_ICON_INVOICE_PAY,       "gnc-invoice-pay.png",       "gnc-invoice-pay-16.png"},
    { GNC_ICON_INVOICE_POST,      "gnc-invoice-post.png",      "gnc-invoice-post-16.png"},
    { GNC_ICON_INVOICE_UNPOST,    "gnc-invoice-unpost.png",    "gnc-invoice-unpost-16.png"},
    { GNC_ICON_INVOICE_NEW,       "gnc-invoice-new.png",       "gnc-invoice-new-16.png"},
    { GNC_ICON_INVOICE_EDIT,      "gnc-invoice-edit.png",      "gnc-invoice-edit-16.png"},
    { GNC_ICON_INVOICE_DUPLICATE, "gnc-invoice-duplicate.png", "gnc-invoice-duplicate-16.png"},
    { GNC_ICON_PDF_EXPORT,        "gnc-gnome-pdf-24.png",      "gnc-gnome-pdf-16.png"},
    { 0 },
};

void
gnc_load_app_icons (void)
{
    item_file *file;

    for (file = item_files; file->icon_name; file++)
    {
        GdkPixbuf *pixbuf_sm, *pixbuf_lg;
        char *fullname_sm = NULL, *fullname_lg = NULL;

        fullname_sm = gnc_filepath_locate_pixmap (file->filename_sm);
        fullname_lg = gnc_filepath_locate_pixmap (file->filename_lg);
        g_assert (fullname_sm && fullname_lg);

        pixbuf_sm = gnc_gnome_get_gdkpixbuf (fullname_sm);
        pixbuf_lg = gnc_gnome_get_gdkpixbuf (fullname_lg);
        g_assert (pixbuf_sm && pixbuf_lg);

        gtk_icon_theme_add_builtin_icon (file->icon_name, 16, pixbuf_sm);
        gtk_icon_theme_add_builtin_icon (file->icon_name, 24, pixbuf_lg);

        g_object_unref(pixbuf_sm);
        g_object_unref(pixbuf_lg);
    }
}
