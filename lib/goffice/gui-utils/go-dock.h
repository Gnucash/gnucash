/* File import from bonoboui to gnumeric by import-bonobo.  Do not edit.  */

/* go-dock.h

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

#ifndef _GO_DOCK_H
#define _GO_DOCK_H

#include <glib-object.h>

G_BEGIN_DECLS

#define GO_TYPE_DOCK            (go_dock_get_type ())
#define GO_DOCK(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GO_TYPE_DOCK, GoDock))
#define GO_DOCK_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GO_TYPE_DOCK, GoDockClass))
#define GO_IS_DOCK(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GO_TYPE_DOCK))
#define GO_IS_DOCK_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GO_TYPE_DOCK))
#define GO_DOCK_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GO_TYPE_DOCK, GoDockClass))

typedef enum
{
  GO_DOCK_TOP,
  GO_DOCK_RIGHT,
  GO_DOCK_BOTTOM,
  GO_DOCK_LEFT,
  GO_DOCK_FLOATING
} GoDockPlacement;

typedef struct _GoDock GoDock;
typedef struct _GoDockPrivate GoDockPrivate;
typedef struct _GoDockClass GoDockClass;

#include <goffice/gui-utils/go-dock-band.h>
#include <goffice/gui-utils/go-dock-layout.h>

struct _GoDock
{
  GtkContainer container;

  GtkWidget *client_area;

  /* GoDockBands associated with this dock.  */
  GList *top_bands;
  GList *bottom_bands;
  GList *right_bands;
  GList *left_bands;

  /* Children that are currently not docked.  */
  GList *floating_children;     /* GtkWidget */

  /* Client rectangle before drag.  */
  GtkAllocation client_rect;

  guint floating_items_allowed : 1;

  /*< private >*/
  GoDockPrivate *_priv;
};

struct _GoDockClass
{
  GtkContainerClass parent_class;

  void (* layout_changed) (GoDock *dock);

  gpointer dummy[4];
};

GtkWidget     *go_dock_new               (void);
GtkType        go_dock_get_type          (void) G_GNUC_CONST;

void           go_dock_allow_floating_items
                                            (GoDock *dock,
                                             gboolean enable);

void           go_dock_add_item          (GoDock             *dock,
                                             GoDockItem         *item,
                                             GoDockPlacement  placement,
                                             guint                  band_num,
                                             gint                   position,
                                             guint                  offset,
                                             gboolean               in_new_band);

void           go_dock_add_floating_item (GoDock *dock,
                                             GoDockItem *widget,
                                             gint x, gint y,
                                             GtkOrientation orientation);

void             go_dock_set_client_area   (GoDock             *dock,
						GtkWidget             *widget);

GtkWidget       *go_dock_get_client_area   (GoDock             *dock);

GoDockItem  *go_dock_get_item_by_name  (GoDock *dock,
						const gchar *name,
						GoDockPlacement *placement_return,
						guint *num_band_return,
						guint *band_position_return,
						guint *offset_return);

GoDockLayout *go_dock_get_layout      (GoDock *dock);

gboolean          go_dock_add_from_layout (GoDock *dock,
					       GoDockLayout *layout);

/* protected */
#if 1 /* defined(GO_UI_INTERNAL) */
gint _bonobo_dock_handle_key_nav (GoDock     *dock,
				  GoDockBand *band,
				  GoDockItem *item,
				  GdkEventKey    *event);
#endif /* GO_UI_INTERNAL */

G_END_DECLS

#endif
