/* File import from libegg to gnumeric by import-egg.  Do not edit.  */

/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
#ifndef __EGG_RECENT_VIEW_GTK_H__
#define __EGG_RECENT_VIEW_GTK_H__

G_BEGIN_DECLS

#include <gtk/gtk.h>
#include "egg-recent-item.h"

#define EGG_RECENT_VIEW_GTK(obj)		G_TYPE_CHECK_INSTANCE_CAST (obj, egg_recent_view_gtk_get_type (), EggRecentViewGtk)
#define EGG_RECENT_VIEW_GTK_CLASS(klass) 	G_TYPE_CHECK_CLASS_CAST (klass, egg_recent_view_gtk_get_type (), EggRecentViewGtkClass)
#define EGG_IS_RECENT_VIEW_GTK(obj)		G_TYPE_CHECK_INSTANCE_TYPE (obj, egg_recent_view_gtk_get_type ())

typedef void (*EggRecentViewGtkTooltipFunc) (GtkTooltips *tooltips,
					     GtkWidget *menu,
					     EggRecentItem *item,
					     gpointer user_data);

typedef struct _EggRecentViewGtk EggRecentViewGtk;

typedef struct _EggRecentViewGtkClass EggRecentViewGtkClass;

struct _EggRecentViewGtkClass {
	GObjectClass parent_class;

	void (*activate) (EggRecentViewGtk *view, EggRecentItem *item);
};

GType        egg_recent_view_gtk_get_type (void);

EggRecentViewGtk * egg_recent_view_gtk_new (GtkWidget *menu,
					    GtkWidget *start_menu_item);

void egg_recent_view_gtk_set_menu            (EggRecentViewGtk *view,
						GtkWidget *menu);
GtkWidget * egg_recent_view_gtk_get_menu     (EggRecentViewGtk *view);


void egg_recent_view_gtk_set_start_menu_item (EggRecentViewGtk *view,
					      GtkWidget *menu_item);
GtkWidget *egg_recent_view_gtk_get_start_menu_item     (EggRecentViewGtk *view);

void egg_recent_view_gtk_set_leading_sep     (EggRecentViewGtk *view,
						gboolean val);

void egg_recent_view_gtk_set_trailing_sep    (EggRecentViewGtk *view,
						gboolean val);

void egg_recent_view_gtk_show_icons          (EggRecentViewGtk *view,
					      gboolean show);
void egg_recent_view_gtk_show_numbers        (EggRecentViewGtk *view,
					      gboolean show);

void egg_recent_view_gtk_set_tooltip_func    (EggRecentViewGtk *view,
					      EggRecentViewGtkTooltipFunc func,
					      gpointer user_data);

void egg_recent_view_gtk_set_icon_size       (EggRecentViewGtk *view,
					      GtkIconSize icon_size);
GtkIconSize egg_recent_view_gtk_get_icon_size (EggRecentViewGtk *view);

G_END_DECLS

#endif /* __EGG_RECENT_VIEW_GTK_H__ */
