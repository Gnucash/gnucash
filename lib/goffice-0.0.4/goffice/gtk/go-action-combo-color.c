/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * go-action-combo-color.c: A custom GtkAction to handle color selection
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
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301
 * USA
 */
#include <goffice/goffice-config.h>
#include "go-action-combo-color.h"
#include "go-combo-color.h"
#include "go-combo-box.h"
#include "go-color-palette.h"
#include "goffice-gtk.h"

#include <gtk/gtkaction.h>
#include <gtk/gtktoolitem.h>
#include <gtk/gtkicontheme.h>
#include <gtk/gtkiconfactory.h>
#include <gtk/gtkimagemenuitem.h>
#include <gtk/gtkimage.h>
#include <gtk/gtktoolbar.h>
#include <gsf/gsf-impl-utils.h>

#include <glib/gi18n.h>

typedef struct {
	GtkToolItem	 base;
	GOComboColor	*combo;	/* container has a ref, not us */
} GOToolComboColor;
typedef GtkToolItemClass GOToolComboColorClass;

#define GO_TOOL_COMBO_COLOR_TYPE	(go_tool_combo_color_get_type ())
#define GO_TOOL_COMBO_COLOR(o)		(G_TYPE_CHECK_INSTANCE_CAST (o, GO_TOOL_COMBO_COLOR_TYPE, GOToolComboColor))
#define IS_GO_TOOL_COMBO_COLOR(o)	(G_TYPE_CHECK_INSTANCE_TYPE (o, GO_TOOL_COMBO_COLOR_TYPE))

static GType go_tool_combo_color_get_type (void);
static gboolean
go_tool_combo_color_set_tooltip (GtkToolItem *tool_item, GtkTooltips *tooltips,
				 char const *tip_text,
				 char const *tip_private)
{
	GOToolComboColor *self = (GOToolComboColor *)tool_item;
	go_combo_box_set_tooltip (GO_COMBO_BOX (self->combo), tooltips,
				  tip_text, tip_private);
	return TRUE;
}
static void
go_tool_combo_color_class_init (GtkToolItemClass *tool_item_class)
{
	tool_item_class->set_tooltip = go_tool_combo_color_set_tooltip;
}

static GdkPixbuf *
make_icon (GtkAction *a, GtkWidget *tool)
{
	GtkIconSize size;
	gint pixels = 8;
	char *stock_id;
	GdkPixbuf *icon;
	GtkSettings *settings = gtk_widget_get_settings (tool);
	GdkScreen *screen = gtk_widget_get_screen (tool);

	if (tool->parent)
		size = gtk_toolbar_get_icon_size (GTK_TOOLBAR (tool->parent));
	else
		g_object_get (settings,
			      "gtk-toolbar-icon-size", &size,
			      NULL);
	gtk_icon_size_lookup_for_settings (settings, size,
					   &pixels, NULL);
	g_object_get (a, "stock-id", &stock_id, NULL);
	icon = gtk_icon_theme_load_icon
		(gtk_icon_theme_get_for_screen (screen),
		 stock_id, pixels, 0, NULL);
	g_free (stock_id);

	return icon;
}


static GSF_CLASS (GOToolComboColor, go_tool_combo_color,
	   go_tool_combo_color_class_init, NULL,
	   GTK_TYPE_TOOL_ITEM)

/*****************************************************************************/

struct _GOActionComboColor {
	GtkAction	 base;
	GOColorGroup 	*color_group;
	char const 	*default_val_label;
	GOColor		 default_val, current_color;
};
typedef struct {
	GtkActionClass base;
	void (*display_custom_dialog) (GOActionComboColor *caction, GtkWidget *dialog);
} GOActionComboColorClass;

enum {
	DISPLAY_CUSTOM_DIALOG,
	LAST_SIGNAL
};

static guint go_action_combo_color_signals [LAST_SIGNAL] = { 0, };
static GObjectClass *combo_color_parent;

static void
go_action_combo_color_connect_proxy (GtkAction *a, GtkWidget *proxy)
{
	GTK_ACTION_CLASS (combo_color_parent)->connect_proxy (a, proxy);

	if (GTK_IS_IMAGE_MENU_ITEM (proxy)) { /* set the icon */
		GdkPixbuf *icon = make_icon (a, proxy);
		GtkWidget *image = gtk_image_new_from_pixbuf (icon);
		g_object_unref (icon);

		gtk_widget_show (image);
		gtk_image_menu_item_set_image (
			GTK_IMAGE_MENU_ITEM (proxy), image);
	}
}

static void
cb_color_changed (GtkWidget *cc, GOColor color,
		  gboolean is_custom, gboolean by_user, gboolean is_default,
		  GOActionComboColor *caction)
{
	if (!by_user)
		return;
	caction->current_color = is_default ? caction->default_val : color;
	gtk_action_activate (GTK_ACTION (caction));
}

static char *
get_title (GtkAction *a)
{
	char *res;
	g_object_get (G_OBJECT (a), "label", &res, NULL);
	return res;
}

static void
cb_proxy_custom_dialog (G_GNUC_UNUSED GObject *ignored,
			GtkWidget *dialog, GOActionComboColor *caction)
{
	g_signal_emit (caction,
		       go_action_combo_color_signals [DISPLAY_CUSTOM_DIALOG], 0,
		       dialog);
}

static GtkWidget *
go_action_combo_color_create_tool_item (GtkAction *a)
{
	GOActionComboColor *caction = (GOActionComboColor *)a;
	GOToolComboColor *tool = g_object_new (GO_TOOL_COMBO_COLOR_TYPE, NULL);
	char *title;

	/* FIXME: We probably should re-do this when tool changes screen or
	   parent.  */
	GdkPixbuf *icon = make_icon (a, GTK_WIDGET (tool));

	tool->combo = (GOComboColor *)go_combo_color_new (icon,
		caction->default_val_label, caction->default_val,
		caction->color_group);

	go_combo_color_set_instant_apply (GO_COMBO_COLOR (tool->combo), TRUE);
	go_combo_box_set_relief (GO_COMBO_BOX (tool->combo), GTK_RELIEF_NONE);
	title = get_title (a);
	go_combo_box_set_title (GO_COMBO_BOX (tool->combo), title);
	g_free (title);

	go_gtk_widget_disable_focus (GTK_WIDGET (tool->combo));
	gtk_container_add (GTK_CONTAINER (tool), GTK_WIDGET (tool->combo));
	gtk_widget_show (GTK_WIDGET (tool->combo));
	gtk_widget_show (GTK_WIDGET (tool));

	g_object_connect (G_OBJECT (tool->combo),
		"signal::color_changed", G_CALLBACK (cb_color_changed), a,
		"signal::display-custom-dialog", G_CALLBACK (cb_proxy_custom_dialog), a,
		NULL);
	return GTK_WIDGET (tool);
}

static GtkWidget *
go_action_combo_color_create_menu_item (GtkAction *a)
{
	GOActionComboColor *caction = (GOActionComboColor *)a;
	char * title = get_title (a);
	GtkWidget *submenu = go_color_palette_make_menu (
		caction->default_val_label,
		caction->default_val,
		caction->color_group, title, caction->current_color);
	GtkWidget *item = gtk_image_menu_item_new ();

	g_free (title);
	gtk_menu_item_set_submenu (GTK_MENU_ITEM (item), submenu);
	gtk_widget_show (submenu);

	g_object_connect (G_OBJECT (submenu),
		"signal::color_changed", G_CALLBACK (cb_color_changed), a,
		"signal::display-custom-dialog", G_CALLBACK (cb_proxy_custom_dialog), a,
		NULL);
	return item;
}

static void
go_action_combo_color_finalize (GObject *obj)
{
	GOActionComboColor *color = (GOActionComboColor *)obj;
	if (color->color_group != NULL)
		g_object_unref (color->color_group);

	combo_color_parent->finalize (obj);
}

static void
go_action_combo_color_class_init (GtkActionClass *gtk_act_class)
{
	GObjectClass *gobject_class = (GObjectClass *)gtk_act_class;

	combo_color_parent = g_type_class_peek_parent (gobject_class);
	gobject_class->finalize		= go_action_combo_color_finalize;

	gtk_act_class->create_tool_item = go_action_combo_color_create_tool_item;
	gtk_act_class->create_menu_item = go_action_combo_color_create_menu_item;
	gtk_act_class->connect_proxy	= go_action_combo_color_connect_proxy;

	go_action_combo_color_signals [DISPLAY_CUSTOM_DIALOG] =
		g_signal_new ("display-custom-dialog",
			      G_OBJECT_CLASS_TYPE (gobject_class),
			      G_SIGNAL_RUN_LAST,
			      G_STRUCT_OFFSET (GOActionComboColorClass, display_custom_dialog),
			      NULL, NULL,
			      g_cclosure_marshal_VOID__OBJECT,
			      G_TYPE_NONE, 1, G_TYPE_OBJECT);
}

GSF_CLASS (GOActionComboColor, go_action_combo_color,
	   go_action_combo_color_class_init, NULL,
	   GTK_TYPE_ACTION)

GOActionComboColor *
go_action_combo_color_new (char const  *action_name,
			   char const  *stock_id,
			   char const  *default_color_label,
			   GOColor	default_color,
			   gpointer	group_key)
{
	GOActionComboColor *res = g_object_new (go_action_combo_color_get_type (),
					   "name", action_name,
					   "stock-id", stock_id,
					   NULL);
	res->color_group = go_color_group_fetch (action_name, group_key);
	res->default_val_label = g_strdup (default_color_label);
	res->current_color = res->default_val = default_color;

	return res;
}

void
go_action_combo_color_set_group (GOActionComboColor *action, gpointer group_key)
{
//#warning TODO
}

GOColor
go_action_combo_color_get_color (GOActionComboColor *a, gboolean *is_default)
{
	if (is_default != NULL)
		*is_default = (a->current_color == a->default_val);
	return a->current_color;
}

void
go_action_combo_color_set_color (GOActionComboColor *a, GOColor color)
{
	GSList *ptr = gtk_action_get_proxies (GTK_ACTION (a));

	a->current_color = color;
	for ( ; ptr != NULL ; ptr = ptr->next)
		if (IS_GO_TOOL_COMBO_COLOR (ptr->data))
			go_combo_color_set_color (GO_TOOL_COMBO_COLOR (ptr->data)->combo, color);
}
