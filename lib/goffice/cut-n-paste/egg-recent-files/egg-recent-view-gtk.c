/* File import from libegg to gnumeric by import-egg.  Do not edit.  */

#include <goffice/goffice-config.h>
/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/**
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307, USA.
 *
 * Authors:
 *   James Willcox <jwillcox@cs.indiana.edu>
 */

#ifdef HAVE_CONFIG_H
/* #include <config.h> */
#endif

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <gtk/gtk.h>
#include <libgnomevfs/gnome-vfs.h>
#ifndef USE_STABLE_LIBGNOMEUI
#include <libgnomeui/gnome-icon-theme.h>
#endif
#include <gconf/gconf-client.h>
#include "egg-recent-model.h"
#include "egg-recent-view.h"
#include "egg-recent-view-gtk.h"
#include "egg-recent-util.h"
#include "egg-recent-item.h"

struct _EggRecentViewGtk {
	GObject parent_instance;	/* We emit signals */

	GtkWidget *menu;
	GtkWidget *start_menu_item;

	gboolean leading_sep;
	gboolean trailing_sep;

	gulong changed_cb_id;

	gchar *uid;

	gboolean show_icons;
	gboolean show_numbers;
#ifndef USE_STABLE_LIBGNOMEUI
	GnomeIconTheme *theme;
#endif

	GtkTooltips *tooltips;
	EggRecentViewGtkTooltipFunc tooltip_func;
	gpointer tooltip_func_data;

	EggRecentModel *model;
	GConfClient *client;
	GtkIconSize icon_size;
};



struct _EggRecentViewGtkMenuData {
	EggRecentViewGtk *view;
	EggRecentItem *item;
};

typedef struct _EggRecentViewGtkMenuData EggRecentViewGtkMenuData;

enum {
	ACTIVATE,
	LAST_SIGNAL
};

/* GObject properties */
enum {
	PROP_BOGUS,
	PROP_MENU,
	PROP_START_MENU_ITEM,
	PROP_SHOW_ICONS,
	PROP_SHOW_NUMBERS
};

static guint view_signals[LAST_SIGNAL] = { 0 };


static void
egg_recent_view_gtk_clear (EggRecentViewGtk *view)
{
	GList *menu_children;
	GList *p;
	GObject *menu_item;
	gint *menu_data=NULL;

	g_return_if_fail (view->menu != NULL);

	menu_children = gtk_container_get_children (GTK_CONTAINER (view->menu));

	p = menu_children;
	while (p != NULL) {
		menu_item = (GObject *)p->data;

		menu_data = (gint *)g_object_get_data (menu_item,
						       view->uid);

		if (menu_data) {
			gtk_container_remove (GTK_CONTAINER (view->menu),
					     GTK_WIDGET (menu_item));

		}

		p = p->next;
	}
}


static gint
egg_recent_view_gtk_find_menu_offset (EggRecentViewGtk *view)
{
	gint i;
	GList *menu_children;
	GList *p;
	GtkWidget *menu_item;
	gint menu_loc=-1;

	g_return_val_if_fail (view, 0);

	menu_children = GTK_MENU_SHELL (view->menu)->children;

	i = 0;
	p = menu_children;
	while (p != NULL) {
		menu_item = (GtkWidget *)p->data;

		if (menu_item == view->start_menu_item) {
			menu_loc = i;
			break;
		}

		p = p->next;
		i++;
	}

	return menu_loc;
}

static void
egg_recent_view_gtk_menu_cb (GtkWidget *menu, gpointer data)
{
	EggRecentViewGtkMenuData *md = (EggRecentViewGtkMenuData *) data;
	EggRecentItem *item;

	g_return_if_fail (md);
	g_return_if_fail (md->item);
	g_return_if_fail (md->view);
	g_return_if_fail (EGG_IS_RECENT_VIEW_GTK (md->view));

	item = md->item;

	egg_recent_item_ref (item);

	g_signal_emit (G_OBJECT(md->view), view_signals[ACTIVATE], 0,
		       item);

	egg_recent_item_unref (item);
}

static void
egg_recent_view_gtk_destroy_cb (gpointer data, GClosure *closure)
{
	EggRecentViewGtkMenuData *md = data;

	egg_recent_item_unref (md->item);
	g_free (md);
}

static GtkWidget *
egg_recent_view_gtk_new_separator (EggRecentViewGtk *view)
{
	GtkWidget *retval;

	g_return_val_if_fail (view, NULL);

	retval = gtk_separator_menu_item_new ();

	/**
	 * this is a tag so we can distinguish our menu items
	 * from others that may be in the menu.
	 */
	g_object_set_data (G_OBJECT (retval),
			   view->uid,
			   GINT_TO_POINTER (1));


	gtk_widget_show (retval);

	return retval;
}

static GtkWidget *
egg_recent_view_gtk_new_menu_item (EggRecentViewGtk *view,
				   EggRecentItem *item,
				   gint index)
{
	GtkWidget *menu_item;
	EggRecentViewGtkMenuData *md;
	gchar *mime_type;
	GtkWidget *image;
	GdkPixbuf *pixbuf;
	gchar *text;
	gchar *short_name;
	gchar *escaped;

	g_return_val_if_fail (view, NULL);
	g_return_val_if_fail (item, NULL);

	short_name = egg_recent_item_get_short_name (item);
	if (!short_name)
		return NULL;

	escaped = egg_recent_util_escape_underlines (short_name);
	g_free (short_name);

	if (view->show_numbers) {
		/* avoid having conflicting mnemonics */
		if (index >= 10)
			text = g_strdup_printf ("%d.  %s", index,
						escaped);
		else
			text = g_strdup_printf ("_%d.  %s", index,
						escaped);
		g_free (escaped);
	} else {
		text = escaped;
	}

	mime_type = egg_recent_item_get_mime_type (item);
#ifndef USE_STABLE_LIBGNOMEUI
	{
		int width, height;
		gchar *uri;

		gtk_icon_size_lookup_for_settings
			(gtk_widget_get_settings (view->menu),
			 view->icon_size,
			 &width, &height);

		uri = egg_recent_item_get_uri (item);
		pixbuf = egg_recent_util_get_icon (view->theme, uri,
						   mime_type,
						   height);
		g_free (uri);
	}
#else
	pixbuf = NULL;
#endif
	image = gtk_image_new_from_pixbuf (pixbuf);
	if (pixbuf)
		g_object_unref (pixbuf);

	if (view->show_icons)
		gtk_widget_show (image);

	menu_item = gtk_image_menu_item_new_with_mnemonic (text);
	gtk_image_menu_item_set_image (GTK_IMAGE_MENU_ITEM (menu_item),
				       image);

	md = g_new0 (EggRecentViewGtkMenuData, 1);
	md->view = view;
	md->item = egg_recent_item_ref (item);

	g_signal_connect_data (G_OBJECT (menu_item), "activate",
			       G_CALLBACK (egg_recent_view_gtk_menu_cb),
			       md,
			       (GClosureNotify)egg_recent_view_gtk_destroy_cb,
			       0);

	g_free (mime_type);
	g_free (text);

	/**
	 * this is a tag so we can distinguish our menu items
	 * from others that may be in the menu.
	 */
	g_object_set_data (G_OBJECT (menu_item),
			   view->uid,
			   GINT_TO_POINTER (1));


	gtk_widget_show (menu_item);

	return menu_item;
}

static void
egg_recent_view_gtk_add_to_menu (EggRecentViewGtk *view,
				 EggRecentItem *item,
				 gint display,
				 gint index)
{
	GtkWidget *menu_item;
	gint menu_offset;

	g_return_if_fail (view);
	g_return_if_fail (view->menu);

	menu_offset = egg_recent_view_gtk_find_menu_offset (view);

	if (item != NULL)
		menu_item = egg_recent_view_gtk_new_menu_item (view, item, display);
	else
		menu_item = egg_recent_view_gtk_new_separator (view);

	if (view->tooltip_func != NULL && menu_item != NULL) {
		view->tooltip_func (view->tooltips, menu_item,
				    item, view->tooltip_func_data);
	}

	if (menu_item)
		gtk_menu_shell_insert (GTK_MENU_SHELL (view->menu), menu_item,
			       menu_offset+index);
}

static void
egg_recent_view_gtk_set_list (EggRecentViewGtk *view, GList *list)
{
	EggRecentItem *item;
	GList *p;
	gint display=1;
	gint index=1;

	g_return_if_fail (view);

	egg_recent_view_gtk_clear (view);

	if (view->leading_sep) {
		egg_recent_view_gtk_add_to_menu (view, NULL, display, index);
		index++;
	}

	p = list;
	while (p != NULL) {
		item = (EggRecentItem *)p->data;

		egg_recent_view_gtk_add_to_menu (view, item, display, index);

		p = p->next;
		display++;
		index++;
	}

	if (view->trailing_sep)
		egg_recent_view_gtk_add_to_menu (view, NULL, display, index);
}

static void
model_changed_cb (EggRecentModel *model, GList *list, EggRecentViewGtk *view)
{
	if (list != NULL)
		egg_recent_view_gtk_set_list (view, list);
	else
		egg_recent_view_gtk_clear (view);
}

static EggRecentModel *
egg_recent_view_gtk_get_model (EggRecentView *view_parent)
{
	EggRecentViewGtk *view;

	g_return_val_if_fail (view_parent != NULL, NULL);
	view = EGG_RECENT_VIEW_GTK (view_parent);
	return view->model;
}

static void
egg_recent_view_gtk_set_model (EggRecentView *view_parent,
				 EggRecentModel *model)
{
	EggRecentViewGtk *view;

	g_return_if_fail (view_parent != NULL);
	view = EGG_RECENT_VIEW_GTK (view_parent);

	if (view->model != NULL) {
		g_object_unref (view->model);
		g_signal_handler_disconnect (G_OBJECT (model),
					     view->changed_cb_id);
	}

	view->model = model;
	g_object_ref (view->model);

	view->changed_cb_id = g_signal_connect_object (G_OBJECT (model),
						"changed",
						G_CALLBACK (model_changed_cb),
						view, 0);

	egg_recent_model_changed (view->model);
}

void
egg_recent_view_gtk_set_leading_sep (EggRecentViewGtk *view, gboolean val)
{
	view->leading_sep = val;

	egg_recent_view_gtk_clear (view);

	if (view->model)
		egg_recent_model_changed (view->model);
}

void
egg_recent_view_gtk_set_trailing_sep (EggRecentViewGtk *view, gboolean val)
{
	view->trailing_sep = val;

	egg_recent_view_gtk_clear (view);

	if (view->model)
		egg_recent_model_changed (view->model);
}

static void
egg_recent_view_gtk_set_property (GObject *object,
			   guint prop_id,
			   const GValue *value,
			   GParamSpec *pspec)
{
	EggRecentViewGtk *view = EGG_RECENT_VIEW_GTK (object);

	switch (prop_id)
	{
		case PROP_MENU:
			egg_recent_view_gtk_set_menu (view,
					       GTK_WIDGET (g_value_get_object (value)));
		break;
		case PROP_START_MENU_ITEM:
			egg_recent_view_gtk_set_start_menu_item (view,
					g_value_get_object (value));
		break;
		case PROP_SHOW_ICONS:
			egg_recent_view_gtk_show_icons (view,
					g_value_get_boolean (value));
		break;
		case PROP_SHOW_NUMBERS:
			egg_recent_view_gtk_show_numbers (view,
					g_value_get_boolean (value));
		break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
egg_recent_view_gtk_get_property (GObject *object,
			   guint prop_id,
			   GValue *value,
			   GParamSpec *pspec)
{
	EggRecentViewGtk *view = EGG_RECENT_VIEW_GTK (object);

	switch (prop_id)
	{
		case PROP_MENU:
			g_value_set_object (value, view->menu);
		break;
		case PROP_START_MENU_ITEM:
			g_value_set_object (value, view->start_menu_item);
		break;
		case PROP_SHOW_ICONS:
			g_value_set_boolean (value, view->show_icons);
		break;
		case PROP_SHOW_NUMBERS:
			g_value_set_boolean (value, view->show_numbers);
		break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
egg_recent_view_gtk_finalize (GObject *object)
{
	EggRecentViewGtk *view = EGG_RECENT_VIEW_GTK (object);

	g_signal_handler_disconnect (G_OBJECT (view->model),
				     view->changed_cb_id);

	g_free (view->uid);

	g_object_unref (view->menu);
	g_object_unref (view->model);
#ifndef USE_STABLE_LIBGNOMEUI
	g_object_unref (view->theme);
#endif
	g_object_unref (view->client);

	g_object_unref (view->tooltips);
}

static void
egg_recent_view_gtk_class_init (EggRecentViewGtkClass * klass)
{
	GObjectClass *object_class;

	object_class = G_OBJECT_CLASS (klass);

	object_class->set_property = egg_recent_view_gtk_set_property;
	object_class->get_property = egg_recent_view_gtk_get_property;
	object_class->finalize     = egg_recent_view_gtk_finalize;

	view_signals[ACTIVATE] = g_signal_new ("activate",
			G_OBJECT_CLASS_TYPE (object_class),
			G_SIGNAL_RUN_LAST,
			G_STRUCT_OFFSET (EggRecentViewGtkClass, activate),
			NULL, NULL,
			g_cclosure_marshal_VOID__BOXED,
			G_TYPE_NONE, 1,
			EGG_TYPE_RECENT_ITEM);

	g_object_class_install_property (object_class,
					 PROP_MENU,
					 g_param_spec_object ("menu",
						 	      "Menu",
							      "The GtkMenuShell this object will update.",
							      gtk_menu_get_type(),
							      G_PARAM_READWRITE));
	g_object_class_install_property (object_class,
					 PROP_START_MENU_ITEM,
					 g_param_spec_object ("start-menu-item",
						 	      "Start Menu Item",
							      "The menu item that precedes where are menu items will go",
							      gtk_menu_item_get_type (),
							      G_PARAM_READWRITE));

	g_object_class_install_property (object_class,
					 PROP_SHOW_ICONS,
					 g_param_spec_boolean ("show-icons",
					   "Show Icons",
					   "Whether or not to show icons",
					   FALSE,
					   G_PARAM_READWRITE));

	g_object_class_install_property (object_class,
					 PROP_SHOW_NUMBERS,
					 g_param_spec_boolean ("show-numbers",
					   "Show Numbers",
					   "Whether or not to show numbers",
					   TRUE,
					   G_PARAM_READWRITE));

	klass->activate = NULL;
}

static void
egg_recent_view_init (EggRecentViewClass *iface)
{
	iface->do_get_model = egg_recent_view_gtk_get_model;
	iface->do_set_model = egg_recent_view_gtk_set_model;
}

static void
show_menus_changed_cb (GConfClient *client,
		       guint cnxn_id,
		       GConfEntry *entry,
		       EggRecentViewGtk *view)
{
	GConfValue *value;

	value = gconf_entry_get_value (entry);

	g_return_if_fail (value->type == GCONF_VALUE_BOOL);

	egg_recent_view_gtk_show_icons (view,
				gconf_value_get_bool (value));

}

#ifndef USE_STABLE_LIBGNOMEUI
static void
theme_changed_cb (GnomeIconTheme *theme, EggRecentViewGtk *view)
{
	if (view->model != NULL)
		egg_recent_model_changed (view->model);
}
#endif

static void
egg_recent_view_gtk_init (EggRecentViewGtk * view)
{
	view->client = gconf_client_get_default ();

	view->show_icons =
		gconf_client_get_bool (view->client,
			"/desktop/gnome/interface/menus_have_icons",
			NULL);

	gconf_client_add_dir (view->client, "/desktop/gnome/interface",
			      GCONF_CLIENT_PRELOAD_NONE,
			      NULL);
	gconf_client_notify_add (view->client,
			"/desktop/gnome/interface/menus_have_icons",
			(GConfClientNotifyFunc)show_menus_changed_cb,
			view, NULL, NULL);


	view->leading_sep = FALSE;
	view->trailing_sep = FALSE;

	view->uid = egg_recent_util_get_unique_id ();
#ifndef USE_STABLE_LIBGNOMEUI
	view->theme = gnome_icon_theme_new ();
	gnome_icon_theme_set_allow_svg (view->theme, TRUE);
	g_signal_connect_object (view->theme, "changed",
				 G_CALLBACK (theme_changed_cb), view, 0);
#endif
	view->tooltips = gtk_tooltips_new ();
	g_object_ref (view->tooltips);
	gtk_object_sink (GTK_OBJECT (view->tooltips));
	view->tooltip_func = NULL;
	view->tooltip_func_data = NULL;

	view->icon_size = GTK_ICON_SIZE_MENU;
}

void
egg_recent_view_gtk_set_icon_size (EggRecentViewGtk *view,
				   GtkIconSize icon_size)
{
	if (view->icon_size != icon_size) {
		view->icon_size = icon_size;
		egg_recent_model_changed (view->model);
	} else {
		view->icon_size = icon_size;
	}
}

GtkIconSize
egg_recent_view_gtk_get_icon_size (EggRecentViewGtk *view)
{
	return view->icon_size;
}

void
egg_recent_view_gtk_show_icons (EggRecentViewGtk *view, gboolean show)
{
	view->show_icons = show;

	if (view->model)
		egg_recent_model_changed (view->model);
}

void
egg_recent_view_gtk_show_numbers (EggRecentViewGtk *view, gboolean show)
{
	view->show_numbers = show;

	if (view->model)
		egg_recent_model_changed (view->model);
}

void
egg_recent_view_gtk_set_tooltip_func (EggRecentViewGtk *view,
				      EggRecentViewGtkTooltipFunc func,
				      gpointer user_data)
{
	view->tooltip_func = func;
	view->tooltip_func_data = user_data;

	if (view->model)
		egg_recent_model_changed (view->model);
}

/**
 * egg_recent_view_gtk_set_menu:
 * @view: A EggRecentViewGtk object.
 * @menu: The GtkMenuShell to put the menu items in.
 *
 * Use this function to change the GtkMenuShell that the recent
 * documents appear in.
 *
 */
void
egg_recent_view_gtk_set_menu (EggRecentViewGtk *view,
				GtkWidget *menu)
{
	g_return_if_fail (view);
	g_return_if_fail (EGG_IS_RECENT_VIEW_GTK (view));
	g_return_if_fail (menu);

	if (view->menu != NULL)
		g_object_unref (view->menu);

	view->menu = menu;
	g_object_ref (view->menu);
}

/**
 * egg_recent_view_gtk_set_start_menu_item:
 * @view: A EggRecentViewGtk object.
 * @start_menu_item: The menu item that appears just before where our menu
 * items should appear
 *
 */
void
egg_recent_view_gtk_set_start_menu_item (EggRecentViewGtk *view,
					 GtkWidget *menu_item)
{
	g_return_if_fail (view);
	g_return_if_fail (EGG_IS_RECENT_VIEW_GTK (view));

	view->start_menu_item = menu_item;
}

/**
 * egg_recent_view_gtk_get_menu:
 * @view: A EggRecentViewGtk object.
 *
 */
GtkWidget *
egg_recent_view_gtk_get_menu (EggRecentViewGtk *view)
{
	return view->menu;
}

/**
 * egg_recent_view_gtk_get_start_menu_item
 * @view: A EggRecentViewGtk object.
 *
 */
GtkWidget *
egg_recent_view_gtk_get_start_menu_item (EggRecentViewGtk *view)
{
	return view->start_menu_item;
}


/**
 * egg_recent_view_gtk_new:
 * @appname: The name of your application.
 * @limit:  The maximum number of items allowed.
 *
 * This creates a new EggRecentViewGtk object.
 *
 * Returns: a EggRecentViewGtk object
 */
EggRecentViewGtk *
egg_recent_view_gtk_new (GtkWidget *menu, GtkWidget *start_menu_item)
{
	EggRecentViewGtk *view;

	g_return_val_if_fail (menu, NULL);

	view = EGG_RECENT_VIEW_GTK (g_object_new (egg_recent_view_gtk_get_type (),
					   "start-menu-item",
					   start_menu_item,
					   "menu", menu,
					   "show-numbers", TRUE, NULL));

	g_return_val_if_fail (view, NULL);

	return view;
}

/**
 * egg_recent_view_gtk_get_type:
 * @:
 *
 * This returns a GType representing a EggRecentViewGtk object.
 *
 * Returns: a GType
 */
GType
egg_recent_view_gtk_get_type (void)
{
	static GType egg_recent_view_gtk_type = 0;

	if(!egg_recent_view_gtk_type) {
		static const GTypeInfo egg_recent_view_gtk_info = {
			sizeof (EggRecentViewGtkClass),
			NULL, /* base init */
			NULL, /* base finalize */
			(GClassInitFunc)egg_recent_view_gtk_class_init, /* class init */
			NULL, /* class finalize */
			NULL, /* class data */
			sizeof (EggRecentViewGtk),
			0,
			(GInstanceInitFunc) egg_recent_view_gtk_init
		};

		static const GInterfaceInfo view_info =
		{
			(GInterfaceInitFunc) egg_recent_view_init,
			NULL,
			NULL
		};

		egg_recent_view_gtk_type = g_type_register_static (G_TYPE_OBJECT,
							"EggRecentViewGtk",
							&egg_recent_view_gtk_info, 0);
		g_type_add_interface_static (egg_recent_view_gtk_type,
					     EGG_TYPE_RECENT_VIEW,
					     &view_info);
	}

	return egg_recent_view_gtk_type;
}

