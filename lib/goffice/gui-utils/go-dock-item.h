/* File import from bonoboui to gnumeric by import-bonobo.  Do not edit.  */

/* WARNING ____ IMMATURE API ____ liable to change */

/* go-dock-item.h
 *
 * Copyright (C) 1998 Ettore Perazzoli
 * Copyright (C) 1998 Elliot Lee
 * Copyright (C) 1995-1997 Peter Mattis, Spencer Kimball and Josh MacDonald
 * All rights reserved.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */
/*
  @NOTATION@
*/

#ifndef _GO_DOCK_ITEM_H
#define _GO_DOCK_ITEM_H

#include <gtk/gtk.h>

G_BEGIN_DECLS

#define GO_TYPE_DOCK_ITEM            (go_dock_item_get_type())
#define GO_DOCK_ITEM(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GO_TYPE_DOCK_ITEM, GoDockItem))
#define GO_DOCK_ITEM_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GO_TYPE_DOCK_ITEM, GoDockItemClass))
#define GO_IS_DOCK_ITEM(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GO_TYPE_DOCK_ITEM))
#define GO_IS_DOCK_ITEM_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GO_TYPE_DOCK_ITEM))
#define GO_DOCK_ITEM_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GO_TYPE_DOCK_ITEM, GoDockItemClass))

typedef enum
{
  GO_DOCK_ITEM_BEH_NORMAL = 0,
  GO_DOCK_ITEM_BEH_EXCLUSIVE = 1 << 0,
  GO_DOCK_ITEM_BEH_NEVER_FLOATING = 1 << 1,
  GO_DOCK_ITEM_BEH_NEVER_VERTICAL = 1 << 2,
  GO_DOCK_ITEM_BEH_NEVER_HORIZONTAL = 1 << 3,
  GO_DOCK_ITEM_BEH_LOCKED = 1 << 4
  /* MAINT: Update the size of the bit field in the GoDockItem structure if you add items to this */
} GoDockItemBehavior;

/* obsolete, for compatibility; don't use */
#define GO_DOCK_ITEM_BEH_NEVER_DETACH GO_DOCK_ITEM_BEH_NEVER_FLOATING

#define GO_DOCK_ITEM_NOT_LOCKED(x) (! (GO_DOCK_ITEM(x)->behavior \
                                          & GO_DOCK_ITEM_BEH_LOCKED))

typedef struct _GoDockItem        GoDockItem;
typedef struct _GoDockItemPrivate GoDockItemPrivate;
typedef struct _GoDockItemClass   GoDockItemClass;

struct _GoDockItem
{
  GtkBin bin;

  gchar                *name;

  /* <private> */
  GdkWindow            *bin_window; /* parent window for children */
  GdkWindow            *float_window; /* always NULL */
  GtkShadowType         shadow_type;

  /* Start drag position (wrt widget->window).  */
  gint16                  dragoff_x, dragoff_y;

  /* Position of the floating window.  */
  gint16                  float_x, float_y;

  guint                 behavior : 5;
  guint                 orientation : 1;

  guint                 float_window_mapped : 1;
  guint                 is_floating : 1;
  guint                 in_drag : 1;
  /* If TRUE, the pointer must be grabbed on "map_event".  */
  guint                 grab_on_map_event : 1;

  /*< private >*/
  GoDockItemPrivate *_priv;
};

struct _GoDockItemClass
{
  GtkBinClass parent_class;

  void (* dock_drag_begin) (GoDockItem *item);
  void (* dock_drag_motion) (GoDockItem *item, gint x, gint y);
  void (* dock_drag_end) (GoDockItem *item);
  void (* dock_detach) (GoDockItem *item);
  void (* orientation_changed) (GoDockItem *item, GtkOrientation new_orientation);

  gpointer dummy[4];
};

/* Public methods.  */
GtkType        go_dock_item_get_type        (void) G_GNUC_CONST;
GtkWidget     *go_dock_item_new             (const gchar *name,
                                                GoDockItemBehavior behavior);
void           go_dock_item_construct       (GoDockItem *new_dock_item,
						const gchar *name,
						GoDockItemBehavior behavior);

GtkWidget     *go_dock_item_get_child       (GoDockItem *dock_item);

char          *go_dock_item_get_name        (GoDockItem *dock_item);

void           go_dock_item_set_shadow_type (GoDockItem *dock_item,
                                                GtkShadowType type);

GtkShadowType  go_dock_item_get_shadow_type (GoDockItem *dock_item);

gboolean       go_dock_item_set_orientation (GoDockItem *dock_item,
                                                GtkOrientation orientation);

GtkOrientation go_dock_item_get_orientation (GoDockItem *dock_item);

GoDockItemBehavior
               go_dock_item_get_behavior    (GoDockItem *dock_item);

/* Private methods.  */
#if 1 /* defined(GO_UI_INTERNAL) */
void           go_dock_item_set_locked      (GoDockItem *dock_item,
						 gboolean        locked);
gboolean       go_dock_item_detach          (GoDockItem *item,
						 gint x, gint y);

void           go_dock_item_attach          (GoDockItem *item,
						 GtkWidget *parent,
						 gint x, gint y);
void           go_dock_item_unfloat         (GoDockItem *item);

void           go_dock_item_grab_pointer    (GoDockItem *item);

void           go_dock_item_drag_floating   (GoDockItem *item,
						 gint x, gint y);

void           go_dock_item_handle_size_request
                                               (GoDockItem *item,
                                                GtkRequisition *requisition);

void           go_dock_item_get_floating_position
                                               (GoDockItem *item,
                                                gint *x, gint *y);
GtkWidget     *go_dock_item_get_grip       (GoDockItem *item);

#endif /* GO_UI_INTERNAL */

G_END_DECLS

#endif /* _GO_DOCK_ITEM_H */
