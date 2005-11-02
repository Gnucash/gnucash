/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * go-gui-utils.c: Misc gtk utilities
 *
 * Copyright (C) 2004 Jody Goldberg (jody@gnome.org)
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of version 2 of the GNU General Public
 * License as published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
 * USA
 */
#include <goffice/goffice-config.h>
#include "go-gui-utils.h"

/* ------------------------------------------------------------------------- */

/**
 * go_gtk_button_new_with_stock_image
 *
 * Code from gedit
 *
 * Creates a new GtkButton with custom label and stock image.
 * 
 * text : button label
 * sotck_id : id for stock icon
 *
 * return : newly created button
 *
 **/

GtkWidget* 
go_gtk_button_new_with_stock_image (char const *text, char const* stock_id)
{
	GtkWidget *button;
	GtkStockItem item;
	GtkWidget *label;
	GtkWidget *image;
	GtkWidget *hbox;
	GtkWidget *align;

	button = gtk_button_new ();

	if (GTK_BIN (button)->child)
		gtk_container_remove (GTK_CONTAINER (button),
				      GTK_BIN (button)->child);

	if (gtk_stock_lookup (stock_id, &item)) {
		label = gtk_label_new_with_mnemonic (text);

		gtk_label_set_mnemonic_widget (GTK_LABEL (label), GTK_WIDGET (button));

		image = gtk_image_new_from_stock (stock_id, GTK_ICON_SIZE_BUTTON);
		hbox = gtk_hbox_new (FALSE, 2);

		align = gtk_alignment_new (0.5, 0.5, 0.0, 0.0);

		gtk_box_pack_start (GTK_BOX (hbox), image, FALSE, FALSE, 0);
		gtk_box_pack_end (GTK_BOX (hbox), label, FALSE, FALSE, 0);

		gtk_container_add (GTK_CONTAINER (button), align);
		gtk_container_add (GTK_CONTAINER (align), hbox);
		gtk_widget_show_all (align);

		return button;
	}

	label = gtk_label_new_with_mnemonic (text);
	gtk_label_set_mnemonic_widget (GTK_LABEL (label), GTK_WIDGET (button));

	gtk_misc_set_alignment (GTK_MISC (label), 0.5, 0.5);

	gtk_widget_show (label);
	gtk_container_add (GTK_CONTAINER (button), label);

	return button;
}

/**
 * go_libglade_new :
 * @gcc : #GOCmdContext
 * @gladefile :
 *
 * Simple utility to open glade files
 **/
GladeXML *
go_libglade_new (char const *gladefile, char const *root,
		 char const *domain, GOCmdContext *gcc)
{
	GladeXML *gui;
	char *f;

	g_return_val_if_fail (gladefile != NULL, NULL);

	if (!g_path_is_absolute (gladefile)) {
		char *d = gnm_sys_glade_dir ();
		f = g_build_filename (d, gladefile, NULL);
		g_free (d);
	} else
		f = g_strdup (gladefile);

	gui = glade_xml_new (f, root, domain);
	if (gui == NULL && gcc != NULL) {
		char *msg = g_strdup_printf (_("Unable to open file '%s'"), f);
		go_cmd_context_error_system (gcc, msg);
		g_free (msg);
	}
	g_free (f);

	return gui;
}

/**
 * go_editable_enters:
 * @window: dialog to affect.
 * @editable: Editable to affect.
 *
 * Normally if there's an editable widget (such as #GtkEntry) in your
 * dialog, pressing Enter will activate the editable rather than the
 * default dialog button. However, in most cases, the user expects to
 * type something in and then press enter to close the dialog. This
 * function enables that behavior.
 **/
void
go_editable_enters (GtkWindow *window, GtkWidget *w)
{
	g_return_if_fail (GTK_IS_WINDOW (window));
	g_signal_connect_swapped (G_OBJECT (w),
		"activate",
		G_CALLBACK (gtk_window_activate_default), window);
}

GdkPixbuf *
go_pixbuf_intelligent_scale (GdkPixbuf *buf, guint width, guint height)
{
	GdkPixbuf *scaled;
	int w, h;
	unsigned long int ow = gdk_pixbuf_get_width (buf);
	unsigned long int oh = gdk_pixbuf_get_height (buf);

	if (ow > width || oh > height) {
		if (ow * height > oh * width) {
			w = width;
			h = width * (((double)oh)/(double)ow);
		} else {
			h = height;
			w = height * (((double)ow)/(double)oh);
		}
			
		scaled = gdk_pixbuf_scale_simple (buf, w, h, GDK_INTERP_BILINEAR);
	} else
		scaled = g_object_ref (buf);
	
	return scaled;
}
