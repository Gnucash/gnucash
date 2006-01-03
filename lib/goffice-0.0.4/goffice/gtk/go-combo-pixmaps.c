/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * go-combo-pixmaps.c - A pixmap selector combo box
 * Copyright 2000-2004, Ximian, Inc.
 *
 * Authors:
 *   Jody Goldberg <jody@gnome.org>
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License, version 2, as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301
 * USA.
 */

#include <goffice/goffice-config.h>
#include "go-combo-pixmaps.h"
#include "go-combo-box.h"

#include <gtk/gtkwindow.h>
#include <gtk/gtktable.h>
#include <gtk/gtktogglebutton.h>
#include <gtk/gtkimage.h>
#include <gtk/gtkmenu.h>
#include <gtk/gtkimagemenuitem.h>
#include <gtk/gtkvbox.h>
#include <gdk/gdkkeysyms.h>

#include <glib/gi18n.h>

#include <gsf/gsf-impl-utils.h>

#define PIXMAP_PREVIEW_WIDTH 15
#define PIXMAP_PREVIEW_HEIGHT 15

struct _GOComboPixmaps {
	GOComboBox     base;

	int selected_index;
	int cols;
	GArray *elements;

	GtkWidget    *table, *preview_button;
	GtkWidget    *preview_image;
	GtkTooltips  *tool_tip;
};

typedef struct {
	GOComboBoxClass base;
	void (* changed) (GOComboPixmaps *pixmaps, int id);
} GOComboPixmapsClass;

enum {
	CHANGED,
	LAST_SIGNAL
};

typedef struct {
	GdkPixbuf *pixbuf;
	int	   id;
} Element;

static guint go_combo_pixmaps_signals [LAST_SIGNAL] = { 0, };
static GObjectClass *go_combo_pixmaps_parent_class;

static void
go_combo_pixmaps_finalize (GObject *object)
{
	GOComboPixmaps *combo = GO_COMBO_PIXMAPS (object);

	if (combo->tool_tip) {
		g_object_unref (combo->tool_tip);
		combo->tool_tip = NULL;
	}

	if (combo->elements) {
		g_array_free (combo->elements, TRUE);
		combo->elements = NULL;
	}

	(*go_combo_pixmaps_parent_class->finalize) (object);
}

static void
cb_screen_changed (GOComboPixmaps *combo, GdkScreen *previous_screen)
{
	GtkWidget *w = GTK_WIDGET (combo);
	GdkScreen *screen = gtk_widget_has_screen (w)
		? gtk_widget_get_screen (w)
		: NULL;

	if (screen) {
		GtkWidget *toplevel = gtk_widget_get_toplevel (combo->table);
		gtk_window_set_screen (GTK_WINDOW (toplevel), screen);
	}
}

static void
emit_change (GOComboPixmaps *combo)
{
	if (_go_combo_is_updating (GO_COMBO_BOX (combo)))
		return;
	g_signal_emit (combo, go_combo_pixmaps_signals [CHANGED], 0,
		g_array_index (combo->elements, Element, combo->selected_index).id);
	go_combo_box_popup_hide (GO_COMBO_BOX (combo));
}

static void
go_combo_pixmaps_init (GOComboPixmaps *combo)
{
	combo->elements = g_array_new (FALSE, FALSE, sizeof (Element));
	combo->table = gtk_table_new (1, 1, 0);

	combo->tool_tip = gtk_tooltips_new ();
	g_object_ref (combo->tool_tip);
	gtk_object_sink (GTK_OBJECT (combo->tool_tip));

	combo->preview_button = gtk_toggle_button_new ();
	combo->preview_image = gtk_image_new ();
	gtk_container_add (GTK_CONTAINER (combo->preview_button),
		GTK_WIDGET (combo->preview_image));

	g_signal_connect (G_OBJECT (combo),
		"screen-changed",
		G_CALLBACK (cb_screen_changed), NULL);
	g_signal_connect_swapped (combo->preview_button,
		"clicked",
		G_CALLBACK (emit_change), combo);

	gtk_widget_show_all (combo->preview_button);
	gtk_widget_show_all (combo->table);
	go_combo_box_construct (GO_COMBO_BOX (combo),
		combo->preview_button, combo->table, combo->table);
}

static void
go_combo_pixmaps_class_init (GObjectClass *gobject_class)
{
	go_combo_pixmaps_parent_class = g_type_class_ref (GO_COMBO_BOX_TYPE);
	gobject_class->finalize = go_combo_pixmaps_finalize;

	go_combo_pixmaps_signals [CHANGED] =
		g_signal_new ("changed",
			      G_OBJECT_CLASS_TYPE (gobject_class),
			      G_SIGNAL_RUN_LAST,
			      G_STRUCT_OFFSET (GOComboPixmapsClass, changed),
			      NULL, NULL,
			      g_cclosure_marshal_VOID__INT,
			      G_TYPE_NONE, 1, G_TYPE_INT);
}

GSF_CLASS (GOComboPixmaps, go_combo_pixmaps,
	   go_combo_pixmaps_class_init, go_combo_pixmaps_init,
	   GO_COMBO_BOX_TYPE)

GOComboPixmaps *
go_combo_pixmaps_new (int ncols)
{
	GOComboPixmaps *combo;

	g_return_val_if_fail (ncols > 0, NULL);

	combo = g_object_new (GO_COMBO_PIXMAPS_TYPE, NULL);
	combo->cols = ncols;
	return combo;
}

static gboolean
swatch_activated (GOComboPixmaps *combo, GtkWidget *button)
{
	go_combo_pixmaps_select_index (combo,
		GPOINTER_TO_INT (g_object_get_data (G_OBJECT (button), "ItemIndex")));
	emit_change (combo);
	return TRUE;
}

static gboolean
cb_swatch_release_event (GtkWidget *button, GdkEventButton *event, GOComboPixmaps *combo)
{
//#warning TODO do I want to check for which button ?
	return swatch_activated (combo, button);
}
static gboolean
cb_swatch_key_press (GtkWidget *button, GdkEventKey *event, GOComboPixmaps *combo)
{
	if (event->keyval == GDK_Return ||
	    event->keyval == GDK_KP_Enter ||
	    event->keyval == GDK_space)
		return swatch_activated (combo, button);
	else
		return FALSE;
}

/**
 * go_combo_pixmaps_add_element :
 * @combo : #GOComboPixmaps
 * @pixbuf : #GdkPixbuf
 * @id : an identifier for the callbacks
 * @tootip : optional
 *
 * Absorbs a ref to the pixbuf.
 **/
void
go_combo_pixmaps_add_element (GOComboPixmaps *combo,
			      GdkPixbuf *pixbuf, int id, char const *tooltip)
{
	GtkWidget *button, *box;
	Element tmp;
	int col, row;

	g_return_if_fail (IS_GO_COMBO_PIXMAPS (combo));

	/* Wrap inside a vbox with a border so that we can see the focus indicator */
	box = gtk_vbox_new (FALSE, 0);
	gtk_box_pack_start (GTK_BOX (box),
			    gtk_image_new_from_pixbuf (pixbuf),
			    TRUE, TRUE, 0);
	g_object_unref (pixbuf);

	button = gtk_button_new ();
	gtk_container_set_border_width (GTK_CONTAINER (box), 2);
	gtk_button_set_relief (GTK_BUTTON (button), GTK_RELIEF_NONE);
	gtk_container_add (GTK_CONTAINER (button), box);

	if (tooltip != NULL)
		gtk_tooltips_set_tip (combo->tool_tip, button,
			tooltip, NULL);

	col = combo->elements->len;
	row = col / combo->cols;
	col = col % combo->cols;

	tmp.pixbuf = pixbuf;
	tmp.id = id;
	g_array_append_val (combo->elements, tmp);
	g_object_set_data (G_OBJECT (button), "ItemIndex",
		GINT_TO_POINTER (combo->elements->len-1));
	gtk_table_attach (GTK_TABLE (combo->table), button,
		col, col + 1, row + 1, row + 2,
		GTK_FILL, GTK_FILL, 1, 1);
	gtk_widget_show_all (button);

	g_object_connect (button,
		"signal::button_release_event", G_CALLBACK (cb_swatch_release_event), combo,
		"signal::key_press_event", G_CALLBACK (cb_swatch_key_press), combo,
		NULL);
}

gboolean
go_combo_pixmaps_select_index (GOComboPixmaps *combo, int i)
{
	g_return_val_if_fail (IS_GO_COMBO_PIXMAPS (combo), FALSE);
	g_return_val_if_fail (0 <= i, FALSE);
	g_return_val_if_fail (i < (int)combo->elements->len, FALSE);

	combo->selected_index = i;
	gtk_image_set_from_pixbuf (GTK_IMAGE (combo->preview_image),
		g_array_index (combo->elements, Element, i).pixbuf);

	return TRUE;
}

gboolean
go_combo_pixmaps_select_id (GOComboPixmaps *combo, int id)
{
	unsigned i;

	g_return_val_if_fail (IS_GO_COMBO_PIXMAPS (combo), FALSE);

	for (i = 0 ; i < combo->elements->len ; i++)
		if (g_array_index (combo->elements, Element, i).id == id)
			break;

	g_return_val_if_fail (i <combo->elements->len, FALSE);

	combo->selected_index = i;
	gtk_image_set_from_pixbuf (GTK_IMAGE (combo->preview_image),
		g_array_index (combo->elements, Element, i).pixbuf);

	return TRUE;
}

int
go_combo_pixmaps_get_selected (GOComboPixmaps const *combo, int *index)
{
	Element *el;

	g_return_val_if_fail (IS_GO_COMBO_PIXMAPS (combo), 0);
	el = &g_array_index (combo->elements, Element, combo->selected_index);

	if (index != NULL)
		*index = combo->selected_index;
	return el->id;
}

GtkWidget *
go_combo_pixmaps_get_preview (GOComboPixmaps const *combo)
{
	g_return_val_if_fail (IS_GO_COMBO_PIXMAPS (combo), NULL);
	return combo->preview_button;
}

/************************************************************************/

struct _GOMenuPixmaps {
	GtkMenu base;
	unsigned cols, n;
};
typedef struct {
	GtkMenuClass	base;
	void (* changed) (GOMenuPixmaps *pixmaps, int id);
} GOMenuPixmapsClass;

static guint go_menu_pixmaps_signals [LAST_SIGNAL] = { 0, };
static void
go_menu_pixmaps_class_init (GObjectClass *gobject_class)
{
	go_menu_pixmaps_signals [CHANGED] =
		g_signal_new ("changed",
			      G_OBJECT_CLASS_TYPE (gobject_class),
			      G_SIGNAL_RUN_LAST,
			      G_STRUCT_OFFSET (GOMenuPixmapsClass, changed),
			      NULL, NULL,
			      g_cclosure_marshal_VOID__INT,
			      G_TYPE_NONE, 1, G_TYPE_INT);
}

static GSF_CLASS (GOMenuPixmaps, go_menu_pixmaps,
		  go_menu_pixmaps_class_init, NULL,
		  GTK_TYPE_MENU)

GOMenuPixmaps *
go_menu_pixmaps_new (int ncols)
{
        GOMenuPixmaps *submenu = g_object_new (go_menu_pixmaps_get_type (), NULL);
	submenu->cols = ncols;
	submenu->n = 0;
	gtk_widget_show (GTK_WIDGET (submenu));
	return submenu;
}

static void
cb_menu_item_activate (GtkWidget *button, GtkWidget *menu)
{
	int id = GPOINTER_TO_INT (g_object_get_data (G_OBJECT (button), "ItemID"));
	g_signal_emit (menu, go_menu_pixmaps_signals [CHANGED], 0, id);
}

void
go_menu_pixmaps_add_element (GOMenuPixmaps *menu,
			     GdkPixbuf *pixbuf, int id)
{
        GtkWidget *button;
	int col, row;

	col = menu->n++;
	row = col / menu->cols;
	col = col % menu->cols;

	button = gtk_image_menu_item_new_with_label (" ");
	gtk_image_menu_item_set_image (GTK_IMAGE_MENU_ITEM (button),
		gtk_image_new_from_pixbuf ((GdkPixbuf *)pixbuf));
	g_object_unref ((GdkPixbuf *)pixbuf);
	g_object_set_data (G_OBJECT (button),
		"ItemID", GINT_TO_POINTER (id));
	gtk_widget_show_all (button);
	gtk_menu_attach (GTK_MENU (menu), button,
		col, col + 1, row + 1, row + 2);
	g_signal_connect (G_OBJECT (button),
		"activate",
		G_CALLBACK (cb_menu_item_activate), menu);
}
