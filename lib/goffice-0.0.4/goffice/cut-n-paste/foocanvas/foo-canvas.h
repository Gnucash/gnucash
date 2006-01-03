/* File import from foocanvas to libgoffice by import-foocanvas.  Do not edit.  */

/* -*- Mode: C; tab-width: 8; indent-tabs-mode: 8; c-basic-offset: 8 -*- */
/*
 * Copyright (C) 1997, 1998, 1999, 2000 Free Software Foundation
 * All rights reserved.
 *
 * This file is part of the Gnome Library.
 *
 * The Gnome Library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License as
 * published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 *
 * The Gnome Library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with the Gnome Library; see the file COPYING.LIB.  If
 * not, write to the Free Software Foundation, Inc., 51 Franklin St,
 * Fifth Floor, Boston, MA  02110-1301 USA.
 */
/*
  @NOTATION@
 */
/* FooCanvas widget - Tk-like canvas widget for Gnome
 *
 * FooCanvas is basically a port of the Tk toolkit's most excellent canvas
 * widget.  Tk is copyrighted by the Regents of the University of California,
 * Sun Microsystems, and other parties.
 *
 *
 * Authors: Federico Mena <federico@nuclecu.unam.mx>
 *          Raph Levien <raph@gimp.org>
 */

#ifndef FOO_CANVAS_H
#define FOO_CANVAS_H

#include <gtk/gtklayout.h>
#include <gdk/gdkevents.h>
#include <stdarg.h>

G_BEGIN_DECLS


/* "Small" value used by canvas stuff */
#define FOO_CANVAS_EPSILON 1e-10


/* Macros for building colors that fit in a 32-bit integer.  The values are in
 * [0, 255].
 */

#define FOO_CANVAS_COLOR(r, g, b) ((((int) (r) & 0xff) << 24)	\
				     | (((int) (g) & 0xff) << 16)	\
				     | (((int) (b) & 0xff) << 8)	\
				     | 0xff)

#define FOO_CANVAS_COLOR_A(r, g, b, a) ((((int) (r) & 0xff) << 24)	\
					  | (((int) (g) & 0xff) << 16)	\
					  | (((int) (b) & 0xff) << 8)	\
					  | ((int) (a) & 0xff))


typedef struct _FooCanvas           FooCanvas;
typedef struct _FooCanvasClass      FooCanvasClass;
typedef struct _FooCanvasItem       FooCanvasItem;
typedef struct _FooCanvasItemClass  FooCanvasItemClass;
typedef struct _FooCanvasGroup      FooCanvasGroup;
typedef struct _FooCanvasGroupClass FooCanvasGroupClass;


/* FooCanvasItem - base item class for canvas items
 *
 * All canvas items are derived from FooCanvasItem.  The only information a
 * FooCanvasItem contains is its parent canvas, its parent canvas item group,
 * and its bounding box in world coordinates.
 *
 * Items inside a canvas are organized in a tree of FooCanvasItemGroup nodes
 * and FooCanvasItem leaves.  Each canvas has a single root group, which can
 * be obtained with the foo_canvas_get_root() function.
 *
 * The abstract FooCanvasItem class does not have any configurable or
 * queryable attributes.
 */

/* Object flags for items */
enum {
	FOO_CANVAS_ITEM_REALIZED         = 1 << 4,
	FOO_CANVAS_ITEM_MAPPED           = 1 << 5,
	FOO_CANVAS_ITEM_ALWAYS_REDRAW    = 1 << 6,
	FOO_CANVAS_ITEM_VISIBLE          = 1 << 7,
	FOO_CANVAS_ITEM_NEED_UPDATE      = 1 << 8,
	FOO_CANVAS_ITEM_NEED_DEEP_UPDATE = 1 << 9
};

/* Update flags for items */
enum {
	FOO_CANVAS_UPDATE_REQUESTED  = 1 << 0,
	FOO_CANVAS_UPDATE_DEEP       = 1 << 1
};

#define FOO_TYPE_CANVAS_ITEM            (foo_canvas_item_get_type ())
#define FOO_CANVAS_ITEM(obj)            (GTK_CHECK_CAST ((obj), FOO_TYPE_CANVAS_ITEM, FooCanvasItem))
#define FOO_CANVAS_ITEM_CLASS(klass)    (GTK_CHECK_CLASS_CAST ((klass), FOO_TYPE_CANVAS_ITEM, FooCanvasItemClass))
#define FOO_IS_CANVAS_ITEM(obj)         (GTK_CHECK_TYPE ((obj), FOO_TYPE_CANVAS_ITEM))
#define FOO_IS_CANVAS_ITEM_CLASS(klass) (GTK_CHECK_CLASS_TYPE ((klass), FOO_TYPE_CANVAS_ITEM))
#define FOO_CANVAS_ITEM_GET_CLASS(obj)  (GTK_CHECK_GET_CLASS ((obj), FOO_TYPE_CANVAS_ITEM, FooCanvasItemClass))


struct _FooCanvasItem {
	GtkObject object;

	/* Parent canvas for this item */
	FooCanvas *canvas;

	/* Parent canvas group for this item (a FooCanvasGroup) */
	FooCanvasItem *parent;

	/* Bounding box for this item (in canvas coordinates) */
	double x1, y1, x2, y2;
};

struct _FooCanvasItemClass {
	GtkObjectClass parent_class;

	/* Tell the item to update itself.  The flags are from the update flags
	 * defined above.  The item should update its internal state from its
	 * queued state, and recompute and request its repaint area. The
	 * update method also recomputes the bounding box of the item.
	 */
	void (* update) (FooCanvasItem *item, double i2w_dx, double i2w_dy, int flags);

	/* Realize an item -- create GCs, etc. */
	void (* realize) (FooCanvasItem *item);

	/* Unrealize an item */
	void (* unrealize) (FooCanvasItem *item);

	/* Map an item - normally only need by items with their own GdkWindows */
	void (* map) (FooCanvasItem *item);

	/* Unmap an item */
	void (* unmap) (FooCanvasItem *item);

	/* Draw an item of this type.  (x, y) are the upper-left canvas pixel
	 * coordinates of the drawable, a temporary pixmap, where things get
	 * drawn.  (width, height) are the dimensions of the drawable.
	 */
	void (* draw) (FooCanvasItem *item, GdkDrawable *drawable, GdkEventExpose *expose);

	/* Calculate the distance from an item to the specified point.  It also
         * returns a canvas item which is the item itself in the case of the
         * object being an actual leaf item, or a child in case of the object
         * being a canvas group.  (cx, cy) are the canvas pixel coordinates that
         * correspond to the item-relative coordinates (x, y).
	 */
	double (* point) (FooCanvasItem *item, double x, double y, int cx, int cy,
			  FooCanvasItem **actual_item);

	void (* translate) (FooCanvasItem *item, double dx, double dy);

	/* Fetch the item's bounding box (need not be exactly tight).  This
	 * should be in item-relative coordinates.
	 */
	void (* bounds) (FooCanvasItem *item, double *x1, double *y1, double *x2, double *y2);

	/* Signal: an event ocurred for an item of this type.  The (x, y)
	 * coordinates are in the canvas world coordinate system.
	 */
	gboolean (* event)                (FooCanvasItem *item, GdkEvent *event);

	/* Reserved for future expansion */
	gpointer spare_vmethods [4];
};


/* Standard Gtk function */
GType foo_canvas_item_get_type (void) G_GNUC_CONST;

/* Create a canvas item using the standard Gtk argument mechanism.  The item is
 * automatically inserted at the top of the specified canvas group.  The last
 * argument must be a NULL pointer.
 */
FooCanvasItem *foo_canvas_item_new (FooCanvasGroup *parent, GType type,
				    const gchar *first_arg_name, ...);

/* Constructors for use in derived classes and language wrappers */
void foo_canvas_item_construct (FooCanvasItem *item, FooCanvasGroup *parent,
				const gchar *first_arg_name, va_list args);

/* Configure an item using the standard Gtk argument mechanism.  The last
 * argument must be a NULL pointer.
 */
void foo_canvas_item_set (FooCanvasItem *item, const gchar *first_arg_name, ...);

/* Used only for language wrappers and the like */
void foo_canvas_item_set_valist (FooCanvasItem *item,
				 const gchar *first_arg_name, va_list args);

/* Move an item by the specified amount */
void foo_canvas_item_move (FooCanvasItem *item, double dx, double dy);

/* Raise an item in the z-order of its parent group by the specified number of
 * positions.
 */
void foo_canvas_item_raise (FooCanvasItem *item, int positions);

/* Lower an item in the z-order of its parent group by the specified number of
 * positions.
 */
void foo_canvas_item_lower (FooCanvasItem *item, int positions);

/* Raise an item to the top of its parent group's z-order. */
void foo_canvas_item_raise_to_top (FooCanvasItem *item);

/* Lower an item to the bottom of its parent group's z-order */
void foo_canvas_item_lower_to_bottom (FooCanvasItem *item);

/* Send an item behind another item */
void foo_canvas_item_send_behind (FooCanvasItem *item,
				  FooCanvasItem *behind_item);


/* Show an item (make it visible).  If the item is already shown, it has no
 * effect.
 */
void foo_canvas_item_show (FooCanvasItem *item);

/* Hide an item (make it invisible).  If the item is already invisible, it has
 * no effect.
 */
void foo_canvas_item_hide (FooCanvasItem *item);

/* Grab the mouse for the specified item.  Only the events in event_mask will be
 * reported.  If cursor is non-NULL, it will be used during the duration of the
 * grab.  Time is a proper X event time parameter.  Returns the same values as
 * XGrabPointer().
 */
int foo_canvas_item_grab (FooCanvasItem *item, unsigned int event_mask,
			  GdkCursor *cursor, guint32 etime);

/* Ungrabs the mouse -- the specified item must be the same that was passed to
 * foo_canvas_item_grab().  Time is a proper X event time parameter.
 */
void foo_canvas_item_ungrab (FooCanvasItem *item, guint32 etime);

/* These functions convert from a coordinate system to another.  "w" is world
 * coordinates and "i" is item coordinates.
 */
void foo_canvas_item_w2i (FooCanvasItem *item, double *x, double *y);
void foo_canvas_item_i2w (FooCanvasItem *item, double *x, double *y);

/* Remove the item from its parent group and make the new group its parent.  The
 * item will be put on top of all the items in the new group.  The item's
 * coordinates relative to its new parent to *not* change -- this means that the
 * item could potentially move on the screen.
 *
 * The item and the group must be in the same canvas.  An item cannot be
 * reparented to a group that is the item itself or that is an inferior of the
 * item.
 */
void foo_canvas_item_reparent (FooCanvasItem *item, FooCanvasGroup *new_group);

/* Used to send all of the keystroke events to a specific item as well as
 * GDK_FOCUS_CHANGE events.
 */
void foo_canvas_item_grab_focus (FooCanvasItem *item);

/* Fetch the bounding box of the item.  The bounding box may not be exactly
 * tight, but the canvas items will do the best they can.  The returned bounding
 * box is in the coordinate system of the item's parent.
 */
void foo_canvas_item_get_bounds (FooCanvasItem *item,
				 double *x1, double *y1, double *x2, double *y2);

/* Request that the update method eventually get called.  This should be used
 * only by item implementations.
 */
void foo_canvas_item_request_update (FooCanvasItem *item);

/* Request a redraw of the bounding box of the canvas item */
void foo_canvas_item_request_redraw (FooCanvasItem *item);

/* FooCanvasGroup - a group of canvas items
 *
 * A group is a node in the hierarchical tree of groups/items inside a canvas.
 * Groups serve to give a logical structure to the items.
 *
 * Consider a circuit editor application that uses the canvas for its schematic
 * display.  Hierarchically, there would be canvas groups that contain all the
 * components needed for an "adder", for example -- this includes some logic
 * gates as well as wires.  You can move stuff around in a convenient way by
 * doing a foo_canvas_item_move() of the hierarchical groups -- to move an
 * adder, simply move the group that represents the adder.
 *
 * The following arguments are available:
 *
 * name		type		read/write	description
 * --------------------------------------------------------------------------------
 * x		double		RW		X coordinate of group's origin
 * y		double		RW		Y coordinate of group's origin
 */


#define FOO_TYPE_CANVAS_GROUP            (foo_canvas_group_get_type ())
#define FOO_CANVAS_GROUP(obj)            (GTK_CHECK_CAST ((obj), FOO_TYPE_CANVAS_GROUP, FooCanvasGroup))
#define FOO_CANVAS_GROUP_CLASS(klass)    (GTK_CHECK_CLASS_CAST ((klass), FOO_TYPE_CANVAS_GROUP, FooCanvasGroupClass))
#define FOO_IS_CANVAS_GROUP(obj)         (GTK_CHECK_TYPE ((obj), FOO_TYPE_CANVAS_GROUP))
#define FOO_IS_CANVAS_GROUP_CLASS(klass) (GTK_CHECK_CLASS_TYPE ((klass), FOO_TYPE_CANVAS_GROUP))
#define FOO_CANVAS_GROUP_GET_CLASS(obj)  (GTK_CHECK_GET_CLASS ((obj), FOO_TYPE_CANVAS_GROUP, FooCanvasGroupClass))


struct _FooCanvasGroup {
	FooCanvasItem item;

	double xpos, ypos;

	/* Children of the group */
	GList *item_list;
	GList *item_list_end;
};

struct _FooCanvasGroupClass {
	FooCanvasItemClass parent_class;
};


/* Standard Gtk function */
GType foo_canvas_group_get_type (void) G_GNUC_CONST;


/*** FooCanvas ***/


#define FOO_TYPE_CANVAS            (foo_canvas_get_type ())
#define FOO_CANVAS(obj)            (GTK_CHECK_CAST ((obj), FOO_TYPE_CANVAS, FooCanvas))
#define FOO_CANVAS_CLASS(klass)    (GTK_CHECK_CLASS_CAST ((klass), FOO_TYPE_CANVAS, FooCanvasClass))
#define FOO_IS_CANVAS(obj)         (GTK_CHECK_TYPE ((obj), FOO_TYPE_CANVAS))
#define FOO_IS_CANVAS_CLASS(klass) (GTK_CHECK_CLASS_TYPE ((klass), FOO_TYPE_CANVAS))
#define FOO_CANVAS_GET_CLASS(obj)  (GTK_CHECK_GET_CLASS ((obj), FOO_TYPE_CANVAS, FooCanvasClass))


struct _FooCanvas {
	GtkLayout layout;

	/* Root canvas group */
	FooCanvasItem *root;

	/* The item containing the mouse pointer, or NULL if none */
	FooCanvasItem *current_item;

	/* Item that is about to become current (used to track deletions and such) */
	FooCanvasItem *new_current_item;

	/* Item that holds a pointer grab, or NULL if none */
	FooCanvasItem *grabbed_item;

	/* If non-NULL, the currently focused item */
	FooCanvasItem *focused_item;

	/* GC for temporary draw pixmap */
	GdkGC *pixmap_gc;

	/* Event on which selection of current item is based */
	GdkEvent pick_event;

	/* Scrolling region */
	double scroll_x1, scroll_y1;
	double scroll_x2, scroll_y2;

	/* Scaling factor to be used for display */
	double pixels_per_unit;

	/* Idle handler ID */
	guint idle_id;

	/* Signal handler ID for destruction of the root item */
	guint root_destroy_id;

	/* Internal pixel offsets when zoomed out */
	int zoom_xofs, zoom_yofs;

	/* Last known modifier state, for deferred repick when a button is down */
	int state;

	/* Event mask specified when grabbing an item */
	guint grabbed_event_mask;

	/* Tolerance distance for picking items */
	int close_enough;

	/* Whether the canvas should center the canvas in the middle of
	 * the window if the scroll region is smaller than the window */
	unsigned int center_scroll_region : 1;

	/* Whether items need update at next idle loop iteration */
	unsigned int need_update : 1;

	/* Are we in the midst of an update */
	unsigned int doing_update : 1;

	/* Whether the canvas needs redrawing at the next idle loop iteration */
	unsigned int need_redraw : 1;

	/* Whether current item will be repicked at next idle loop iteration */
	unsigned int need_repick : 1;

	/* For use by internal pick_current_item() function */
	unsigned int left_grabbed_item : 1;

	/* For use by internal pick_current_item() function */
	unsigned int in_repick : 1;
};

struct _FooCanvasClass {
	GtkLayoutClass parent_class;

	/* Draw the background for the area given.
	 */
	void (* draw_background) (FooCanvas *canvas,
				  int x, int y, int width, int height);

	/* Private Virtual methods for groping the canvas inside bonobo */
	void (* request_update) (FooCanvas *canvas);

	/* Reserved for future expansion */
	gpointer spare_vmethods [4];
};


/* Standard Gtk function */
GType foo_canvas_get_type (void) G_GNUC_CONST;

/* Creates a new canvas.  You should check that the canvas is created with the
 * proper visual and colormap.  Any visual will do unless you intend to insert
 * gdk_imlib images into it, in which case you should use the gdk_imlib visual.
 *
 * You should call foo_canvas_set_scroll_region() soon after calling this
 * function to set the desired scrolling limits for the canvas.
 */
GtkWidget *foo_canvas_new (void);

/* Returns the root canvas item group of the canvas */
FooCanvasGroup *foo_canvas_root (FooCanvas *canvas);

/* Sets the limits of the scrolling region, in world coordinates */
void foo_canvas_set_scroll_region (FooCanvas *canvas,
				   double x1, double y1, double x2, double y2);

/* Gets the limits of the scrolling region, in world coordinates */
void foo_canvas_get_scroll_region (FooCanvas *canvas,
				   double *x1, double *y1, double *x2, double *y2);

/* Sets the number of pixels that correspond to one unit in world coordinates */
void foo_canvas_set_pixels_per_unit (FooCanvas *canvas, double n);

/* Wether the canvas centers the scroll region if it is smaller than the window  */
void foo_canvas_set_center_scroll_region (FooCanvas *canvas, gboolean center_scroll_region);

/* Scrolls the canvas to the specified offsets, given in canvas pixel coordinates */
void foo_canvas_scroll_to (FooCanvas *canvas, int cx, int cy);

/* Returns the scroll offsets of the canvas in canvas pixel coordinates.  You
 * can specify NULL for any of the values, in which case that value will not be
 * queried.
 */
void foo_canvas_get_scroll_offsets (FooCanvas *canvas, int *cx, int *cy);

/* Requests that the canvas be repainted immediately instead of in the idle
 * loop.
 */
void foo_canvas_update_now (FooCanvas *canvas);

/* Returns the item that is at the specified position in world coordinates, or
 * NULL if no item is there.
 */
FooCanvasItem *foo_canvas_get_item_at (FooCanvas *canvas, double x, double y);

/* For use only by item type implementations.  Request that the canvas
 * eventually redraw the specified region, specified in canvas pixel
 * coordinates.  The region contains (x1, y1) but not (x2, y2).
 */
void foo_canvas_request_redraw (FooCanvas *canvas, int x1, int y1, int x2, int y2);

/* These functions convert from a coordinate system to another.  "w" is world
 * coordinates, "c" is canvas pixel coordinates (pixel coordinates that are
 * (0,0) for the upper-left scrolling limit and something else for the
 * lower-left scrolling limit).
 */
void foo_canvas_w2c_rect_d (FooCanvas *canvas,
			    double *x1, double *y1,
			    double *x2, double *y2);
void foo_canvas_w2c (FooCanvas *canvas, double wx, double wy, int *cx, int *cy);
void foo_canvas_w2c_d (FooCanvas *canvas, double wx, double wy, double *cx, double *cy);
void foo_canvas_c2w (FooCanvas *canvas, int cx, int cy, double *wx, double *wy);

/* This function takes in coordinates relative to the GTK_LAYOUT
 * (canvas)->bin_window and converts them to world coordinates.
 * These days canvas coordinates and window coordinates are the same, but
 * these are left for backwards compat reasons.
 */
void foo_canvas_window_to_world (FooCanvas *canvas,
				 double winx, double winy, double *worldx, double *worldy);

/* This is the inverse of foo_canvas_window_to_world() */
void foo_canvas_world_to_window (FooCanvas *canvas,
				 double worldx, double worldy, double *winx, double *winy);

/* Takes a string specification for a color and allocates it into the specified
 * GdkColor.  If the string is null, then it returns FALSE. Otherwise, it
 * returns TRUE.
 */
int foo_canvas_get_color (FooCanvas *canvas, const char *spec, GdkColor *color);

/* Allocates a color from the RGB value passed into this function. */
gulong foo_canvas_get_color_pixel (FooCanvas *canvas,
				   guint        rgba);


/* Sets the stipple origin of the specified gc so that it will be aligned with
 * all the stipples used in the specified canvas.  This is intended for use only
 * by canvas item implementations.
 */
void foo_canvas_set_stipple_origin (FooCanvas *canvas, GdkGC *gc);

G_END_DECLS

#endif
