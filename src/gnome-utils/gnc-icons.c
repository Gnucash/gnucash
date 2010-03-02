/*
 * gnc-icons.c -- Functions to create a GtkIconFactory for GnuCash
 * Copyright (C) 2003 Jan Arne Petersen
 * Author: Jan Arne Petersen <jpetersen@uni-bonn.de>
 */

#include "config.h"

#include <gtk/gtk.h>
#include <glib/gi18n.h>

#include "gnc-icons.h"
#include "gnc-gnome-utils.h"

static GtkStockItem items[] =
{
    { GNC_STOCK_ACCOUNT,        N_("Account"),         0, 0, NULL },
    { GNC_STOCK_DELETE_ACCOUNT, N_("_Delete Account"), 0, 0, NULL },
    { GNC_STOCK_EDIT_ACCOUNT,   N_("_Edit Account"),   0, 0, NULL },
    { GNC_STOCK_NEW_ACCOUNT,    N_("_New Account"),    0, 0, NULL },
    { GNC_STOCK_OPEN_ACCOUNT,   N_("_Open Account"),   0, 0, NULL },
    { GNC_STOCK_TRANSFER,       N_("_Transfer..."),    0, 0, NULL },
    { GNC_STOCK_SPLIT_TRANS,    N_("S_plit Transaction"), 0, 0, NULL },
    { GNC_STOCK_JUMP_TO,        N_("_Jump"),              0, 0, NULL },
};

typedef struct _item_file
{
    const gchar *stock_name;
    const gchar *filename_lg;
    const gchar *filename_sm;
} item_file;

static item_file item_files[] =
{
    { GNC_STOCK_ACCOUNT,        "gnc-account.png",        "gnc-account-16.png"},
    { GNC_STOCK_DELETE_ACCOUNT, "gnc-account-delete.png", "gnc-account-delete-16.png"},
    { GNC_STOCK_EDIT_ACCOUNT,   "gnc-account-edit.png",   "gnc-account-edit-16.png"},
    { GNC_STOCK_NEW_ACCOUNT,    "gnc-account-new.png",    "gnc-account-new-16.png"},
    { GNC_STOCK_OPEN_ACCOUNT,   "gnc-account-open.png",   "gnc-account-open-16.png"},
    { GNC_STOCK_TRANSFER,       "gnc-transfer.png",       "gnc-transfer-16.png"},
    { GNC_STOCK_SCHEDULE,       "gnc-sx-new.png",         "gnc-sx-new-16.png"},
    { GNC_STOCK_SPLIT_TRANS,    "gnc-split-trans.png",    "gnc-split-trans-16.png"},
    { GNC_STOCK_JUMP_TO,        "gnc-jumpto.png",         "gnc-jumpto-16.png"},
    { GNC_STOCK_INVOICE,        "gnc-invoice.png",        "gnc-invoice-16.png"},
    { GNC_STOCK_INVOICE_POST,   "gnc-invoice-post.png",   "gnc-invoice-post-16.png"},
    { GNC_STOCK_INVOICE_UNPOST, "gnc-invoice-unpost.png", "gnc-invoice-unpost-16.png"},
    { GNC_STOCK_INVOICE_EDIT,   "gnc-invoice-edit.png",   "gnc-invoice-edit-16.png"},
    { 0 },
};

static void
gnc_add_stock_icon_pair (GtkIconFactory *factory,
                         const char *stock,
                         const char *filename1,
                         const char *filename2)
{
    GtkIconSet *set;
    GtkIconSource *source;
    GdkPixbuf *pixbuf1, *pixbuf2;
    char *fullname1, *fullname2;

    /* Find the complete path names for these files */
    fullname1 = gnc_gnome_locate_pixmap (filename1);
    fullname2 = gnc_gnome_locate_pixmap (filename2);
    g_assert (fullname1 && fullname2);

    /* Load the pixbufs */
    pixbuf1 = gnc_gnome_get_gdkpixbuf (filename1);
    pixbuf2 = gnc_gnome_get_gdkpixbuf (filename2);
    g_assert (pixbuf1 && pixbuf2);

    /* Create the icon set */
    set = gtk_icon_set_new ();
    source = gtk_icon_source_new ();
    gtk_icon_source_set_filename (source, fullname1);
    gtk_icon_source_set_pixbuf (source, pixbuf1);
    gtk_icon_set_add_source (set, source);
    gtk_icon_source_free(source);

    source = gtk_icon_source_new ();
    gtk_icon_source_set_filename (source, fullname2);
    gtk_icon_source_set_pixbuf (source, pixbuf2);
    gtk_icon_source_set_size (source, GTK_ICON_SIZE_MENU);
    gtk_icon_source_set_size_wildcarded (source, FALSE);
    gtk_icon_set_add_source (set, source);
    gtk_icon_source_free(source);

    /* Add it to the factory */
    gtk_icon_factory_add (factory, stock, set);

    /* Cleanup */
    g_object_unref (pixbuf2);
    g_object_unref (pixbuf1);
    g_free(fullname2);
    g_free(fullname1);
    gtk_icon_set_unref (set);
}

void
gnc_load_stock_icons (void)
{
    GtkIconFactory *factory;
    item_file *file;

    /* Register our stock items */
    gtk_stock_add (items, G_N_ELEMENTS (items));

    /* Add our custom icon factory to the list of defaults */
    factory = gtk_icon_factory_new ();
    for (file = item_files; file->stock_name; file++)
    {
        gnc_add_stock_icon_pair (factory, file->stock_name,
                                 file->filename_lg, file->filename_sm);
    }

    gtk_icon_factory_add_default (factory);
}
