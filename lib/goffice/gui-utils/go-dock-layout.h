/* File import from bonoboui to gnumeric by import-bonobo.  Do not edit.  */

/* go-dock-layout.c

   Copyright (C) 1998 Free Software Foundation

   All rights reserved.

   The Gnome Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   The Gnome Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with the Gnome Library; see the file COPYING.LIB.  If not,
   write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.

   Author: Ettore Perazzoli <ettore@comm2000.it>
*/
/*
  @NOTATION@
*/

#ifndef _GO_DOCK_LAYOUT_H
#define _GO_DOCK_LAYOUT_H



G_BEGIN_DECLS

#define GO_TYPE_DOCK_LAYOUT            (go_dock_layout_get_type ())
#define GO_DOCK_LAYOUT(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GO_TYPE_DOCK_LAYOUT, GoDockLayout))
#define GO_DOCK_LAYOUT_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GO_TYPE_DOCK_LAYOUT, GoDockLayoutClass))
#define GO_IS_DOCK_LAYOUT(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GO_TYPE_DOCK_LAYOUT))
#define GO_IS_DOCK_LAYOUT_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GO_TYPE_DOCK_LAYOUT))
#define GO_DOCK_LAYOUT_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GO_TYPE_DOCK_LAYOUT, GoDockLayoutClass))

typedef struct _GoDockLayoutItem    GoDockLayoutItem;
typedef struct _GoDockLayoutClass   GoDockLayoutClass;
typedef struct _GoDockLayout        GoDockLayout;
typedef struct _GoDockLayoutPrivate GoDockLayoutPrivate;

#include <goffice/gui-utils/go-dock.h>
#include <goffice/gui-utils/go-dock-item.h>

struct _GoDockLayoutItem
{
  GoDockItem *item;

  GoDockPlacement placement;

  union
  {
    struct
    {
      gint x;
      gint y;
      GtkOrientation orientation;
    } floating;

    struct
    {
      gint band_num;
      gint band_position;
      gint offset;
    } docked;

  } position;
};

struct _GoDockLayout
{
  GObject object;

  GList *items;                 /* GoDockLayoutItem */

  /*< private >*/
  GoDockLayoutPrivate *_priv;
};

struct _GoDockLayoutClass
{
  GObjectClass parent_class;

  gpointer dummy[4];
};

GoDockLayout     *go_dock_layout_new      (void);
GType                 go_dock_layout_get_type (void) G_GNUC_CONST;

gboolean             go_dock_layout_add_item (GoDockLayout *layout,
                                                 GoDockItem *item,
                                                 GoDockPlacement placement,
                                                 gint band_num,
                                                 gint band_position,
                                                 gint offset);

gboolean             go_dock_layout_add_floating_item
                                                (GoDockLayout *layout,
                                                 GoDockItem *item,
                                                 gint x, gint y,
                                                 GtkOrientation orientation);

GoDockLayoutItem *go_dock_layout_get_item (GoDockLayout *layout,
                                                 GoDockItem *item);
GoDockLayoutItem *go_dock_layout_get_item_by_name
                                                (GoDockLayout *layout,
                                                 const gchar *name);

gboolean             go_dock_layout_remove_item
                                                (GoDockLayout *layout,
                                                 GoDockItem *item);
gboolean             go_dock_layout_remove_item_by_name
                                                (GoDockLayout *layout,
                                                 const gchar *name);

gchar               *go_dock_layout_create_string
                                                (GoDockLayout *layout);
gboolean             go_dock_layout_parse_string
                                                (GoDockLayout *layout,
                                                 const gchar *string);

gboolean             go_dock_layout_add_to_dock
                                                (GoDockLayout *layout,
                                                 GoDock *dock);

G_END_DECLS

#endif
