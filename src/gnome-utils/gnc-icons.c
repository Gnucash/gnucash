/*
 * gnc-icons.c -- Functions to create a GtkIconFactory for GnuCash
 * Copyright (C) 2003 Jan Arne Petersen
 * Author: Jan Arne Petersen <jpetersen@uni-bonn.de>
 */

#include <gtk/gtkiconfactory.h>

#include "gnc-icons.h"

#include "gnc-dir.h"

void
gnc_load_stock_icons (void)
{
	GtkIconFactory *factory;
	GtkIconSet *set;
	GdkPixbuf *pixbuf;
	GtkIconSource *source;

	factory = gtk_icon_factory_new ();

	/* Account */
	set = gtk_icon_set_new ();

	pixbuf = gdk_pixbuf_new_from_file (GNC_PIXMAP_DIR "/account.png", NULL);
	source = gtk_icon_source_new ();
	gtk_icon_source_set_filename (source, GNC_PIXMAP_DIR "/account.png");
	gtk_icon_source_set_pixbuf (source, pixbuf);
	gdk_pixbuf_unref (pixbuf);
	gtk_icon_set_add_source (set, source);

	pixbuf = gdk_pixbuf_new_from_file (GNC_PIXMAP_DIR "/account-16.png", NULL);
	source = gtk_icon_source_new ();
	gtk_icon_source_set_filename (source, GNC_PIXMAP_DIR "/account-16.png");
	gtk_icon_source_set_pixbuf (source, pixbuf);
	gdk_pixbuf_unref (pixbuf);
	gtk_icon_source_set_size (source, GTK_ICON_SIZE_MENU);
	gtk_icon_source_set_size_wildcarded (source, FALSE);
	gtk_icon_set_add_source (set, source);

	gtk_icon_factory_add (factory, GNC_STOCK_ACCOUNT, set);
	gtk_icon_set_unref (set);

	/* Delete Account */
	set = gtk_icon_set_new ();

	pixbuf = gdk_pixbuf_new_from_file (GNC_PIXMAP_DIR "/delete-account.png", NULL);
	source = gtk_icon_source_new ();
	gtk_icon_source_set_filename (source, GNC_PIXMAP_DIR "/delete-account.png");
	gtk_icon_source_set_pixbuf (source, pixbuf);
	gdk_pixbuf_unref (pixbuf);
	gtk_icon_set_add_source (set, source);

	pixbuf = gdk_pixbuf_new_from_file (GNC_PIXMAP_DIR "/delete-account-16.png", NULL);
	source = gtk_icon_source_new ();
	gtk_icon_source_set_filename (source, GNC_PIXMAP_DIR "/delete-account-16.png");
	gtk_icon_source_set_pixbuf (source, pixbuf);
	gdk_pixbuf_unref (pixbuf);
	gtk_icon_source_set_size (source, GTK_ICON_SIZE_MENU);
	gtk_icon_source_set_size_wildcarded (source, FALSE);
	gtk_icon_set_add_source (set, source);

	gtk_icon_factory_add (factory, GNC_STOCK_DELETE_ACCOUNT, set);
	gtk_icon_set_unref (set);

	/* Edit Account */
	set = gtk_icon_set_new ();

	pixbuf = gdk_pixbuf_new_from_file (GNC_PIXMAP_DIR "/edit-account.png", NULL);
	source = gtk_icon_source_new ();
	gtk_icon_source_set_filename (source, GNC_PIXMAP_DIR "/edit-account.png");
	gtk_icon_source_set_pixbuf (source, pixbuf);
	gdk_pixbuf_unref (pixbuf);
	gtk_icon_set_add_source (set, source);

	pixbuf = gdk_pixbuf_new_from_file (GNC_PIXMAP_DIR "/edit-account-16.png", NULL);
	source = gtk_icon_source_new ();
	gtk_icon_source_set_filename (source, GNC_PIXMAP_DIR "/edit-account-16.png");
	gtk_icon_source_set_pixbuf (source, pixbuf);
	gdk_pixbuf_unref (pixbuf);
	gtk_icon_source_set_size (source, GTK_ICON_SIZE_MENU);
	gtk_icon_source_set_size_wildcarded (source, FALSE);
	gtk_icon_set_add_source (set, source);

	gtk_icon_factory_add (factory, GNC_STOCK_EDIT_ACCOUNT, set);
	gtk_icon_set_unref (set);

	/* Open Account */
	set = gtk_icon_set_new ();

	pixbuf = gdk_pixbuf_new_from_file (GNC_PIXMAP_DIR "/open-account.png", NULL);
	source = gtk_icon_source_new ();
	gtk_icon_source_set_filename (source, GNC_PIXMAP_DIR "/open-account.png");
	gtk_icon_source_set_pixbuf (source, pixbuf);
	gdk_pixbuf_unref (pixbuf);
	gtk_icon_set_add_source (set, source);

	pixbuf = gdk_pixbuf_new_from_file (GNC_PIXMAP_DIR "/open-account-16.png", NULL);
	source = gtk_icon_source_new ();
	gtk_icon_source_set_filename (source, GNC_PIXMAP_DIR "/open-account-16.png");
	gtk_icon_source_set_pixbuf (source, pixbuf);
	gdk_pixbuf_unref (pixbuf);
	gtk_icon_source_set_size (source, GTK_ICON_SIZE_MENU);
	gtk_icon_source_set_size_wildcarded (source, FALSE);
	gtk_icon_set_add_source (set, source);

	gtk_icon_factory_add (factory, GNC_STOCK_OPEN_ACCOUNT, set);
	gtk_icon_set_unref (set);

	gtk_icon_factory_add_default (factory);
}
