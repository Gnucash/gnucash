/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * color-palette.c - A color selector palette
 * Copyright 2000, 2001, Ximian, Inc.
 *
 * Authors:
 * This code was extracted from widget-color-combo.c
 *   written by Miguel de Icaza (miguel@kernel.org) and
 *   Dom Lachowicz (dominicl@seas.upenn.edu). The extracted
 *   code was re-packaged into a separate object by
 *   Michael Levy (mlevy@genoscope.cns.fr)
 *   And later revised and polished by
 *   Almer S. Tigelaar (almer@gnome.org)
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
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 */

#include <goffice/goffice-config.h>
#include "go-color-palette.h"
#include "go-marshalers.h"

#include <goffice/utils/go-color.h>
#include <gui-util.h>
#include <style-color.h>
#include <gtk/gtklabel.h>
#include <gtk/gtkvbox.h>
#include <gtk/gtktable.h>
#include <gtk/gtkdrawingarea.h>
#include <gtk/gtkimagemenuitem.h>
#include <gtk/gtkimage.h>
#include <gtk/gtkstock.h>
#include <gtk/gtkbutton.h>
#include <gtk/gtkcolorseldialog.h>
#include <gdk/gdkkeysyms.h>
#include <gdk/gdkcolor.h>
#include <glib/gi18n.h>
#include <gsf/gsf-impl-utils.h>


#include <string.h>

typedef struct _ColorNamePair ColorNamePair;
struct _GOColorPalette {
	GtkVBox	base;

	GOColorGroup *group;
	GOColor	      selection, default_color;
	gboolean      current_is_custom;
	gboolean      current_is_default;
	gboolean      allow_alpha;

	/* only for custom colours */
	GtkWidget   *swatches [GO_COLOR_GROUP_HISTORY_SIZE];
	GtkTooltips *tip;

	/* The table with our default color names */
	ColorNamePair const *default_set;
};

typedef struct {
	GtkVBoxClass base;

	/* Signals emited by this widget */
	void (*color_changed) (GOColorPalette *pal, GOColor color,
			       gboolean is_custom, gboolean by_user, gboolean is_default);
	void (*display_custom_dialog) (GOColorPalette *pal, GtkWidget *dialog);
} GOColorPaletteClass;

#define COLOR_PREVIEW_WIDTH 12
#define COLOR_PREVIEW_HEIGHT 12

enum {
	COLOR_CHANGED,
	DISPLAY_CUSTOM_DIALOG,
	LAST_SIGNAL
};

struct _ColorNamePair {
	GOColor	color;
	char const *name;	/* english name - eg. "white" */
};

static ColorNamePair const default_color_set [] = {
	{ RGBA_TO_UINT (0x00, 0x00, 0x00, 0xff), N_("black")},
	{ RGBA_TO_UINT (0x99, 0x33, 0x00, 0xff), N_("light brown")},
	{ RGBA_TO_UINT (0x33, 0x33, 0x00, 0xff), N_("brown gold")},
	{ RGBA_TO_UINT (0x00, 0x33, 0x00, 0xff), N_("dark green #2")},
	{ RGBA_TO_UINT (0x00, 0x33, 0x66, 0xff), N_("navy")},
	{ RGBA_TO_UINT (0x00, 0x00, 0x80, 0xff), N_("dark blue")},
	{ RGBA_TO_UINT (0x33, 0x33, 0x99, 0xff), N_("purple #2")},
	{ RGBA_TO_UINT (0x33, 0x33, 0x33, 0xff), N_("very dark gray")},

	{ RGBA_TO_UINT (0x80, 0x00, 0x00, 0xff), N_("dark red")},
	{ RGBA_TO_UINT (0xFF, 0x66, 0x00, 0xff), N_("red-orange")},
	{ RGBA_TO_UINT (0x80, 0x80, 0x00, 0xff), N_("gold")},
	{ RGBA_TO_UINT (0x00, 0x80, 0x00, 0xff), N_("dark green")},
	{ RGBA_TO_UINT (0x00, 0x80, 0x80, 0xff), N_("dull blue")},
	{ RGBA_TO_UINT (0x00, 0x00, 0xFF, 0xff), N_("blue")},
	{ RGBA_TO_UINT (0x66, 0x66, 0x99, 0xff), N_("dull purple")},
	{ RGBA_TO_UINT (0x80, 0x80, 0x80, 0xff), N_("dark gray")},

	{ RGBA_TO_UINT (0xFF, 0x00, 0x00, 0xff), N_("red")},
	{ RGBA_TO_UINT (0xFF, 0x99, 0x00, 0xff), N_("orange")},
	{ RGBA_TO_UINT (0x99, 0xCC, 0x00, 0xff), N_("lime")},
	{ RGBA_TO_UINT (0x33, 0x99, 0x66, 0xff), N_("dull green")},
	{ RGBA_TO_UINT (0x33, 0xCC, 0xCC, 0xff), N_("dull blue #2")},
	{ RGBA_TO_UINT (0x33, 0x66, 0xFF, 0xff), N_("sky blue #2")},
	{ RGBA_TO_UINT (0x80, 0x00, 0x80, 0xff), N_("purple")},
	{ RGBA_TO_UINT (0x96, 0x96, 0x96, 0xff), N_("gray")},

	{ RGBA_TO_UINT (0xFF, 0x00, 0xFF, 0xff), N_("magenta")},
	{ RGBA_TO_UINT (0xFF, 0xCC, 0x00, 0xff), N_("bright orange")},
	{ RGBA_TO_UINT (0xFF, 0xFF, 0x00, 0xff), N_("yellow")},
	{ RGBA_TO_UINT (0x00, 0xFF, 0x00, 0xff), N_("green")},
	{ RGBA_TO_UINT (0x00, 0xFF, 0xFF, 0xff), N_("cyan")},
	{ RGBA_TO_UINT (0x00, 0xCC, 0xFF, 0xff), N_("bright blue")},
	{ RGBA_TO_UINT (0x99, 0x33, 0x66, 0xff), N_("red purple")},
	{ RGBA_TO_UINT (0xC0, 0xC0, 0xC0, 0xff), N_("light gray")},

	{ RGBA_TO_UINT (0xFF, 0x99, 0xCC, 0xff), N_("pink")},
	{ RGBA_TO_UINT (0xFF, 0xCC, 0x99, 0xff), N_("light orange")},
	{ RGBA_TO_UINT (0xFF, 0xFF, 0x99, 0xff), N_("light yellow")},
	{ RGBA_TO_UINT (0xCC, 0xFF, 0xCC, 0xff), N_("light green")},
	{ RGBA_TO_UINT (0xCC, 0xFF, 0xFF, 0xff), N_("light cyan")},
	{ RGBA_TO_UINT (0x99, 0xCC, 0xFF, 0xff), N_("light blue")},
	{ RGBA_TO_UINT (0xCC, 0x99, 0xFF, 0xff), N_("light purple")},
	{ RGBA_TO_UINT (0xFF, 0xFF, 0xFF, 0xff), N_("white")},

	{ 0, NULL},

	/* Disable these for now, they are mostly repeats */
	{ RGBA_TO_UINT (0x99, 0x99, 0xFF, 0xff), N_("purplish blue")},
	{ RGBA_TO_UINT (0x99, 0x33, 0x66, 0xff), N_("red purple")},
	{ RGBA_TO_UINT (0xFF, 0xFF, 0xCC, 0xff), N_("light yellow")},
	{ RGBA_TO_UINT (0xCC, 0xFF, 0xFF, 0xff), N_("light blue")},
	{ RGBA_TO_UINT (0x66, 0x00, 0x66, 0xff), N_("dark purple")},
	{ RGBA_TO_UINT (0xFF, 0x80, 0x80, 0xff), N_("pink")},
	{ RGBA_TO_UINT (0x00, 0x66, 0xCC, 0xff), N_("sky blue")},
	{ RGBA_TO_UINT (0xCC, 0xCC, 0xFF, 0xff), N_("light purple")},

	{ RGBA_TO_UINT (0x00, 0x00, 0x80, 0xff), N_("dark blue")},
	{ RGBA_TO_UINT (0xFF, 0x00, 0xFF, 0xff), N_("magenta")},
	{ RGBA_TO_UINT (0xFF, 0xFF, 0x00, 0xff), N_("yellow")},
	{ RGBA_TO_UINT (0x00, 0xFF, 0xFF, 0xff), N_("cyan")},
	{ RGBA_TO_UINT (0x80, 0x00, 0x80, 0xff), N_("purple")},
	{ RGBA_TO_UINT (0x80, 0x00, 0x00, 0xff), N_("dark red")},
	{ RGBA_TO_UINT (0x00, 0x80, 0x80, 0xff), N_("dull blue")},
	{ RGBA_TO_UINT (0x00, 0x00, 0xFF, 0xff), N_("blue")},

	{ 0, NULL},
};

static guint go_color_palette_signals [LAST_SIGNAL] = { 0, };

static GObjectClass *go_color_palette_parent_class;

static GtkWidget *
create_color_sel (GObject *action_proxy, GOColor c, GCallback handler, gboolean allow_alpha)
{
	char *title = g_object_get_data (G_OBJECT (action_proxy), "title");
	GtkWidget *w = gtk_color_selection_dialog_new (title);
	GtkColorSelectionDialog *dialog = GTK_COLOR_SELECTION_DIALOG (w);
	GtkColorSelection *colorsel = GTK_COLOR_SELECTION (dialog->colorsel);
	GdkColor gdk;

	gtk_widget_hide (dialog->help_button);
	gtk_color_selection_set_current_color (colorsel,
		go_color_to_gdk (c, &gdk));
	gtk_color_selection_set_has_opacity_control (colorsel, allow_alpha);

	g_signal_connect_object (dialog,
		"response", handler, action_proxy, 0);

	/* require an explicit show _after_ the custom-dialog signal fires */
	return w;
}

static gboolean
handle_color_sel (GtkColorSelectionDialog *dialog,
		  gint response_id, GOColor *res)
{
	if (response_id == GTK_RESPONSE_OK) {
		GdkColor gdk;
		GtkColorSelection *colorsel = GTK_COLOR_SELECTION (dialog->colorsel);
		guint16 alpha = gtk_color_selection_get_current_alpha (colorsel);

		gtk_color_selection_get_current_color (colorsel, &gdk);
		*res = GDK_TO_UINT (gdk);
		alpha >>= 8;
		*res = UINT_RGBA_CHANGE_A (*res, alpha);
	}
	/* destroy _before_ we emit */
	gtk_object_destroy (GTK_OBJECT (dialog));
	return response_id == GTK_RESPONSE_OK;
}

static void
go_color_palette_finalize (GObject *object)
{
	GOColorPalette *pal = GO_COLOR_PALETTE (object);

	if (pal->tip) {
		g_object_unref (pal->tip);
		pal->tip = NULL;
	}

	go_color_palette_set_group (pal, NULL);

	(*go_color_palette_parent_class->finalize) (object);
}

static void
go_color_palette_class_init (GObjectClass *gobject_class)
{
	gobject_class->finalize = go_color_palette_finalize;

	go_color_palette_parent_class = g_type_class_peek_parent (gobject_class);

	go_color_palette_signals [COLOR_CHANGED] =
		g_signal_new ("color_changed",
			      G_OBJECT_CLASS_TYPE (gobject_class),
			      G_SIGNAL_RUN_LAST,
			      G_STRUCT_OFFSET (GOColorPaletteClass, color_changed),
			      NULL, NULL,
			      go__VOID__INT_BOOLEAN_BOOLEAN_BOOLEAN,
			      G_TYPE_NONE, 4, G_TYPE_INT,
			      G_TYPE_BOOLEAN, G_TYPE_BOOLEAN, G_TYPE_BOOLEAN);
	go_color_palette_signals [DISPLAY_CUSTOM_DIALOG] =
		g_signal_new ("display-custom-dialog",
			      G_OBJECT_CLASS_TYPE (gobject_class),
			      G_SIGNAL_RUN_LAST,
			      G_STRUCT_OFFSET (GOColorPaletteClass, display_custom_dialog),
			      NULL, NULL,
			      g_cclosure_marshal_VOID__OBJECT,
			      G_TYPE_NONE, 1, G_TYPE_OBJECT);
}

GSF_CLASS (GOColorPalette, go_color_palette,
	   go_color_palette_class_init, NULL,
	   GTK_TYPE_VBOX)

/*
 * Find out if a color is in the default palette (not in the custom colors!)
 *
 * Utility function
 */
static gboolean
color_in_palette (ColorNamePair const *set, GOColor color)
{
	int i;

	for (i = 0; set[i].name != NULL; i++)
		if (color == set[i].color)
			return TRUE;
	return FALSE;
}

static void
set_color (GOColorPalette *pal, GOColor color, gboolean is_custom,
	   gboolean by_user, gboolean is_default)
{
	if (is_default)
		color = pal->default_color;
	if (!color_in_palette (pal->default_set, color))
		go_color_group_add_color (pal->group, color);
	pal->selection = color;
	pal->current_is_custom = is_custom;
	pal->current_is_default = is_default;
	g_signal_emit (pal, go_color_palette_signals [COLOR_CHANGED], 0,
		       color, is_custom, by_user, is_default);
}

static void
cb_history_changed (GOColorPalette *pal)
{
	int i;
	GdkColor gdk;
	GOColorGroup *group = pal->group;

	for (i = 0 ; i < GO_COLOR_GROUP_HISTORY_SIZE ; i++)
		gtk_widget_modify_bg (pal->swatches [i], GTK_STATE_NORMAL,
			go_color_to_gdk (group->history[i], &gdk));
#if 0
	if (next_swatch != NULL) {
		next_swatch->style->bg[GTK_STATE_NORMAL] = *new_color;
		gnome_color_picker_set_i16 (GNOME_COLOR_PICKER (pal->picker),
			new_color->red, new_color->green, new_color->blue, 0);
	}
#endif
}

static gboolean
cb_default_release_event (GtkWidget *button, GdkEventButton *event, GOColorPalette *pal)
{
	set_color (pal, pal->default_color, FALSE, TRUE, TRUE);
	return TRUE;
}

static void
swatch_activated (GOColorPalette *pal, GtkBin *button)
{
	GList *tmp = gtk_container_get_children (GTK_CONTAINER (gtk_bin_get_child (button)));
	GtkWidget *swatch = (tmp != NULL) ? tmp->data : NULL;

	g_list_free (tmp);

	g_return_if_fail (swatch != NULL);

	set_color (pal, GDK_TO_UINT (swatch->style->bg[GTK_STATE_NORMAL]),
		   FALSE, TRUE, FALSE);
}

static gboolean
cb_swatch_release_event (GtkBin *button, GdkEventButton *event, GOColorPalette *pal)
{
#ifdef GOG_WARN_TODO
#warning TODO do I want to check for which button ?
#endif
	swatch_activated (pal, button);
	return TRUE;
}

static gboolean
cb_swatch_key_press (GtkBin *button, GdkEventKey *event, GOColorPalette *pal)
{
	if (event->keyval == GDK_Return ||
	    event->keyval == GDK_KP_Enter ||
	    event->keyval == GDK_space) {
		swatch_activated (pal, button);
		return TRUE;
	} else
		return FALSE;
}

/*
 * Create the individual color buttons
 *
 * Utility function
 */
static GtkWidget *
go_color_palette_button_new (GOColorPalette *pal, GtkTable* table, GtkTooltips *tip,
			  ColorNamePair const * color_name, gint col, gint row)
{
        GtkWidget *button, *swatch, *box;
	GdkColor   gdk;

	swatch = gtk_drawing_area_new ();
	gtk_widget_modify_bg (swatch, GTK_STATE_NORMAL,
		go_color_to_gdk (color_name->color, &gdk));
	gtk_widget_set_size_request (swatch, COLOR_PREVIEW_WIDTH, COLOR_PREVIEW_HEIGHT);

	/* Wrap inside a vbox with a border so that we can see the focus indicator */
	box = gtk_vbox_new (FALSE, 0);
	gtk_container_set_border_width (GTK_CONTAINER (box), 2);
	gtk_box_pack_start (GTK_BOX (box), GTK_WIDGET (swatch), TRUE, TRUE, 0);

	button = gtk_button_new ();
	gtk_button_set_relief (GTK_BUTTON (button), GTK_RELIEF_NONE);
	gtk_container_add (GTK_CONTAINER (button), box);
	gtk_tooltips_set_tip (tip, button, _(color_name->name), "");

	gtk_table_attach (table, button, col, col+1, row, row+1,
		GTK_FILL, GTK_FILL, 0, 0);

	g_object_connect (button,
		"signal::button_release_event", G_CALLBACK (cb_swatch_release_event), pal,
		"signal::key_press_event", G_CALLBACK (cb_swatch_key_press), pal,
		NULL);
	return swatch;
}

static void
cb_combo_custom_response (GtkColorSelectionDialog *dialog,
			  gint response_id, GOColorPalette *pal)
{
	GOColor c;
	if (handle_color_sel (dialog, response_id, &c))
		set_color (pal, c, TRUE, TRUE, FALSE);
}

static void
cb_combo_custom_clicked (GtkWidget *button, GOColorPalette *pal)
{
	GtkWidget *dialog = create_color_sel (G_OBJECT (pal), pal->selection,
		G_CALLBACK (cb_combo_custom_response), pal->allow_alpha);
	g_signal_emit (pal, go_color_palette_signals [DISPLAY_CUSTOM_DIALOG], 0,
		dialog);
	gtk_widget_show (dialog);
}

void
go_color_palette_set_title (GOColorPalette *pal, char const *title)
{
	g_object_set_data_full (G_OBJECT (pal), "title", 
		g_strdup (title), g_free);
}

/**
 * go_color_palette_set_group :
 * @cg : #GOColorGroup
 *
 * Absorb the reference to the group
 */
void
go_color_palette_set_group (GOColorPalette *pal, GOColorGroup *cg)
{
	if (pal->group == cg)
		return;

	if (pal->group) {
		g_signal_handlers_disconnect_by_func (
			G_OBJECT (pal->group),
			G_CALLBACK (cb_history_changed), pal);
		g_object_unref (G_OBJECT (pal->group));
		pal->group = NULL;
	}
	if (cg != NULL) {
		pal->group = cg;
		g_signal_connect_swapped (G_OBJECT (cg),
			"history-changed",
			G_CALLBACK (cb_history_changed), pal);
	}
}
static GtkWidget *
go_color_palette_setup (GOColorPalette *pal,
		     char const *no_color_label,
		     int cols, int rows,
		     ColorNamePair const *color_names)
{
	GtkWidget	*w, *table;
	GtkTooltips	*tip;
	int pos, row, col = 0;

	table = gtk_table_new (cols, rows, FALSE);

	if (no_color_label != NULL) {
		w = gtk_button_new_with_label (no_color_label);
		gtk_table_attach (GTK_TABLE (table), w,
				  0, cols, 0, 1, GTK_FILL | GTK_EXPAND, 0, 0, 0);
		g_signal_connect (w,
			"button_release_event",
			G_CALLBACK (cb_default_release_event), pal);
	}

	pal->tip = tip = gtk_tooltips_new ();
	g_object_ref (pal->tip);
	gtk_object_sink (GTK_OBJECT (pal->tip));

	for (row = 0; row < rows; row++)
		for (col = 0; col < cols; col++) {
			pos = row * cols + col;
			if (color_names [pos].name == NULL)
				goto custom_colors;
			go_color_palette_button_new ( pal,
				GTK_TABLE (table), GTK_TOOLTIPS (tip),
				&(color_names [pos]), col, row + 1);
		}

custom_colors :
	if (col > 0)
		row++;
	for (col = 0; col < cols && col < GO_COLOR_GROUP_HISTORY_SIZE; col++) {
		ColorNamePair color_name = { 0, N_("custom") };
		color_name.color = pal->group->history [col];
		pal->swatches [col] = go_color_palette_button_new (pal,
			GTK_TABLE (table), GTK_TOOLTIPS (tip),
			&color_name, col, row + 1);
	}

	w = gnumeric_button_new_with_stock_image (_("Custom Color..."),
						  GTK_STOCK_SELECT_COLOR);
	gtk_button_set_alignment (GTK_BUTTON (w), 0., .5);
	gtk_table_attach (GTK_TABLE (table), w, 0, cols,
		row + 2, row + 3, GTK_FILL | GTK_EXPAND, 0, 0, 0);
	g_signal_connect (G_OBJECT (w),
		"clicked",
		G_CALLBACK (cb_combo_custom_clicked), pal);

	return table;
}

void
go_color_palette_set_color_to_default (GOColorPalette *pal)
{
	set_color (pal, pal->default_color, FALSE, TRUE, TRUE);
}

void
go_color_palette_set_current_color (GOColorPalette *pal, GOColor color)
{
	set_color (pal, color,
		   color_in_palette (pal->default_set, color),
		   FALSE, FALSE);
}

GOColor
go_color_palette_get_current_color (GOColorPalette *pal,
				    gboolean *is_default, gboolean *is_custom)
{
	if (is_default != NULL)
		*is_default = pal->current_is_default;
	if (is_custom != NULL)
		*is_custom = pal->current_is_custom;
	return pal->selection;
}

void
go_color_palette_set_allow_alpha (GOColorPalette *pal, gboolean allow_alpha)
{
	pal->allow_alpha = allow_alpha;
}

GtkWidget *
go_color_palette_new (char const *no_color_label,
		   GOColor default_color,
		   GOColorGroup *cg)
{
	GOColorPalette *pal;
	int const cols = 8;
	int const rows = 6;
	ColorNamePair const *color_names = default_color_set;

	pal = g_object_new (GO_COLOR_PALETTE_TYPE, NULL);

	pal->default_set   = color_names;
	pal->default_color = default_color;
	pal->selection	   = default_color;
	pal->current_is_custom  = FALSE;
	pal->current_is_default = TRUE;
	go_color_palette_set_group (pal, cg);

	gtk_container_add (GTK_CONTAINER (pal),
		go_color_palette_setup (pal, no_color_label, cols, rows,
				     pal->default_set));
	return GTK_WIDGET (pal);
}


/***********************************************************************/

typedef struct {
	GtkMenu base;
	gboolean allow_alpha;
	GOColor  selection, default_color;
} GOMenuColor;

typedef struct {
	GtkMenuClass	base;
	void (* color_changed) (GOMenuColor *menu, GOColor color,
				gboolean custom, gboolean by_user, gboolean is_default);
	void (*display_custom_dialog) (GOColorPalette *pal, GtkWidget *dialog);
} GOMenuColorClass;

static guint go_menu_color_signals [LAST_SIGNAL] = { 0, };

static void
go_menu_color_class_init (GObjectClass *gobject_class)
{
	go_menu_color_signals [COLOR_CHANGED] =
		g_signal_new ("color_changed",
			      G_OBJECT_CLASS_TYPE (gobject_class),
			      G_SIGNAL_RUN_LAST,
			      G_STRUCT_OFFSET (GOMenuColorClass, color_changed),
			      NULL, NULL,
			      go__VOID__INT_BOOLEAN_BOOLEAN_BOOLEAN,
			      G_TYPE_NONE, 4, G_TYPE_INT,
			      G_TYPE_BOOLEAN, G_TYPE_BOOLEAN, G_TYPE_BOOLEAN);
	go_menu_color_signals [DISPLAY_CUSTOM_DIALOG] =
		g_signal_new ("display-custom-dialog",
			      G_OBJECT_CLASS_TYPE (gobject_class),
			      G_SIGNAL_RUN_LAST,
			      G_STRUCT_OFFSET (GOColorPaletteClass, display_custom_dialog),
			      NULL, NULL,
			      g_cclosure_marshal_VOID__OBJECT,
			      G_TYPE_NONE, 1, G_TYPE_OBJECT);
}

static GSF_CLASS (GOMenuColor, go_menu_color,
		  go_menu_color_class_init, NULL,
		  GTK_TYPE_MENU)

static GtkWidget *
make_colored_menu_item (char const *label, GOColor c)
{
	GtkWidget *button;
	GdkPixbuf *pixbuf = gdk_pixbuf_new (GDK_COLORSPACE_RGB, TRUE, 8,
		COLOR_PREVIEW_WIDTH, COLOR_PREVIEW_HEIGHT);
	gdk_pixbuf_fill (pixbuf, c);

	button = gtk_image_menu_item_new_with_label (label);
	gtk_image_menu_item_set_image (GTK_IMAGE_MENU_ITEM (button),
		gtk_image_new_from_pixbuf (pixbuf));
	g_object_unref (pixbuf);
	gtk_widget_show_all (button);

	g_object_set_data (G_OBJECT (button), "go_color", GINT_TO_POINTER (c));
	return button;
}

static void
cb_menu_default_activate (GtkWidget *button, GOMenuColor *menu)
{
	menu->selection = menu->default_color;
	g_signal_emit (menu, go_menu_color_signals [COLOR_CHANGED], 0,
		       menu->selection, FALSE, TRUE, TRUE);
}

static void
cb_menu_color_activate (GtkWidget *button, GOMenuColor *menu)
{
	GOColor color = GPOINTER_TO_INT (
		g_object_get_data (G_OBJECT (button), "go_color"));
	menu->selection = color;
	g_signal_emit (menu, go_menu_color_signals [COLOR_CHANGED], 0,
		       color, FALSE, TRUE, FALSE);
}
static void
cb_menu_custom_response (GtkColorSelectionDialog *dialog,
			 gint response_id, GOMenuColor *menu)
{
	GOColor c;
	if (handle_color_sel (dialog, response_id, &c)) {
		menu->selection = c;
		g_signal_emit (menu, go_menu_color_signals [COLOR_CHANGED], 0,
			c, TRUE, TRUE, FALSE);
	}
}

static void
cb_menu_custom_activate (GtkWidget *button, GOMenuColor *menu)
{
	GtkWidget *dialog = create_color_sel (G_OBJECT (menu), menu->selection,
		G_CALLBACK (cb_menu_custom_response), menu->allow_alpha);
	g_signal_emit (menu, go_menu_color_signals [DISPLAY_CUSTOM_DIALOG], 0,
		dialog);
	gtk_widget_show (dialog);
}

/**
 * go_color_palette_make_menu:
 * @no_color_labe :
 * default_color: #GOColor
 * @cg : #GOColorGroup
 * @custom_dialog_title :
 * @current_color : #GOColor
 *
 * Create a submenu with a palette of colours.  Caller is responsible for
 * creating an item to point to the submenu.
 **/
GtkWidget *
go_color_palette_make_menu (char const *no_color_label,
			    GOColor default_color,
			    GOColorGroup *cg,
			    char const *custom_dialog_title,
			    GOColor current_color)
{
	int cols = 8;
	int rows = 6;
	int col, row, pos, table_row = 0;
	ColorNamePair const *color_names = default_color_set;
        GtkWidget *w, *submenu;

	submenu = g_object_new (go_menu_color_get_type (), NULL);

	if (no_color_label != NULL) {
		w = make_colored_menu_item (no_color_label, default_color);
		gtk_menu_attach (GTK_MENU (submenu), w, 0, cols, 0, 1);
		g_signal_connect (G_OBJECT (w),
			"activate",
			G_CALLBACK (cb_menu_default_activate), submenu);
		table_row++;
	}
	for (row = 0; row < rows; row++, table_row++) {
		for (col = 0; col < cols; col++) {
			pos = row * cols + col;
			if (color_names [pos].name == NULL)
				goto custom_colors;
			w = make_colored_menu_item (" ",
				color_names [pos].color);
			gtk_menu_attach (GTK_MENU (submenu), w,
				col, col+1, table_row, table_row+1);
			g_signal_connect (G_OBJECT (w),
				"activate",
				G_CALLBACK (cb_menu_color_activate), submenu);
		}
	}

custom_colors :
	if (col > 0)
		row++;
	for (col = 0; col < cols && col < GO_COLOR_GROUP_HISTORY_SIZE; col++) {
		w = make_colored_menu_item (" ", cg->history[col]);
		gtk_menu_attach (GTK_MENU (submenu), w,
			col, col+1, table_row, table_row+1);
		g_signal_connect (G_OBJECT (w),
			"activate",
			G_CALLBACK (cb_menu_color_activate), submenu);
	}
	w = gtk_image_menu_item_new_with_label (_("Custom Color..."));
	gtk_image_menu_item_set_image (GTK_IMAGE_MENU_ITEM (w),
		gtk_image_new_from_stock (GTK_STOCK_SELECT_COLOR, GTK_ICON_SIZE_MENU));
	gtk_widget_show_all (w);
	gtk_menu_attach (GTK_MENU (submenu), w, 0, cols, row + 2, row + 3);
	g_signal_connect (G_OBJECT (w),
		"activate",
		G_CALLBACK (cb_menu_custom_activate), submenu);

	((GOMenuColor *)submenu)->selection = current_color;
	((GOMenuColor *)submenu)->default_color = default_color;
	g_object_set_data_full (G_OBJECT (submenu), "title", 
		g_strdup (custom_dialog_title), g_free);

	gtk_widget_show (submenu);

	return submenu;
}
