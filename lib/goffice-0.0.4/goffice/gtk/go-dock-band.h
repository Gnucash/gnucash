/* File import from bonoboui to libgoffice by import-bonobo.  Do not edit.  */

/* go-dock-band.h

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
   write to the Free Software Foundation, Inc., 51 Franklin St, Fifth
   Floor, Boston, MA  02110-1301 USA.

   Author: Ettore Perazzoli <ettore@comm2000.it>
*/
/*
  @NOTATION@
*/

#ifndef _GO_DOCK_BAND_H
#define _GO_DOCK_BAND_H

#include <glib-object.h>

G_BEGIN_DECLS

#define GO_TYPE_DOCK_BAND            (go_dock_band_get_type ())
#define GO_DOCK_BAND(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GO_TYPE_DOCK_BAND, GoDockBand))
#define GO_DOCK_BAND_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GO_TYPE_DOCK_BAND, GoDockBandClass))
#define GO_IS_DOCK_BAND(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GO_TYPE_DOCK_BAND))
#define GO_IS_DOCK_BAND_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GO_TYPE_DOCK_BAND))
#define GO_DOCK_BAND_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GO_TYPE_DOCK_BAND, GoDockBandClass))

typedef struct _GoDockBand GoDockBand;
typedef struct _GoDockBandPrivate GoDockBandPrivate;
typedef struct _GoDockBandClass GoDockBandClass;
typedef struct _GoDockBandChild GoDockBandChild;

#include <goffice/gui-utils/go-dock.h>
#include <goffice/gui-utils/go-dock-item.h>
#include <goffice/gui-utils/go-dock-layout.h>

struct _GoDockBand
{
  GtkContainer container;

  GList *children;              /* GoDockBandChild */

  GList *floating_child;        /* GoDockBandChild */

  /* This used to remember the allocation before the drag begin: it is
     necessary to do so because we actually decide what docking action
     happens depending on it, instead of using the current allocation
     (which might be constantly changing while the user drags things
     around).  */
  GtkAllocation drag_allocation;

  guint tot_offsets;

  guint max_space_requisition : 16;
  guint num_children : 8;
  guint new_for_drag : 1;
  guint doing_drag : 1;
  guint orientation : 1;

  /*< private >*/
  GoDockBandPrivate *_priv;
};

struct _GoDockBandClass
{
  GtkContainerClass parent_class;

  gpointer dummy[2];
};

struct _GoDockBandChild
{
  GtkWidget *widget;

  GtkAllocation drag_allocation;

  /* Maximum (requested) offset from the previous child.  */
  guint16 offset;

  /* Actual offset.  */
  guint16 real_offset;

  guint16 drag_offset;

  guint16 prev_space, foll_space;
  guint16 drag_prev_space, drag_foll_space;

  guint16 max_space_requisition;
};

GtkWidget     *go_dock_band_new              (void);
GtkType        go_dock_band_get_type         (void) G_GNUC_CONST;

void           go_dock_band_set_orientation  (GoDockBand *band,
                                                 GtkOrientation orientation);
GtkOrientation go_dock_band_get_orientation  (GoDockBand *band);

gboolean       go_dock_band_insert           (GoDockBand *band,
                                                 GtkWidget *child,
                                                 guint offset,
                                                 gint position);
gboolean       go_dock_band_prepend          (GoDockBand *band,
                                                 GtkWidget *child,
                                                 guint offset);
gboolean       go_dock_band_append           (GoDockBand *band,
                                                 GtkWidget *child,
                                                 guint offset);

void           go_dock_band_set_child_offset (GoDockBand *band,
                                                 GtkWidget *child,
                                                 guint offset);
guint          go_dock_band_get_child_offset (GoDockBand *band,
                                                 GtkWidget *child);
void           go_dock_band_move_child       (GoDockBand *band,
                                                 GList *old_child,
                                                 guint new_num);

guint          go_dock_band_get_num_children (GoDockBand *band);

void           go_dock_band_drag_begin       (GoDockBand *band,
                                                 GoDockItem *item);
gboolean       go_dock_band_drag_to          (GoDockBand *band,
                                                 GoDockItem *item,
                                                 gint x, gint y);
void           go_dock_band_drag_end         (GoDockBand *band,
                                                 GoDockItem *item);

GoDockItem *go_dock_band_get_item_by_name (GoDockBand *band,
                                                 const char *name,
                                                 guint *position_return,
                                                 guint *offset_return);

void           go_dock_band_layout_add       (GoDockBand *band,
                                                 GoDockLayout *layout,
                                                 GoDockPlacement placement,
                                                 guint band_num);

#if 1 /* defined(GO_UI_INTERNAL) */
gint _bonobo_dock_band_handle_key_nav (GoDockBand *band,
				      GoDockItem *item,
				      GdkEventKey    *event);
#endif /* GO_UI_INTERNAL */

G_END_DECLS

#endif
