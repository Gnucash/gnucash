/* File import from foocanvas to libgoffice by import-foocanvas.  Do not edit.  */

#include <goffice/goffice-config.h>
#include <glib/gi18n.h>
#include <gsf/gsf-impl-utils.h>
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
/*
 * FooCanvas widget - Tk-like canvas widget for Gnome
 *
 * FooCanvas is basically a port of the Tk toolkit's most excellent canvas widget.  Tk is
 * copyrighted by the Regents of the University of California, Sun Microsystems, and other parties.
 *
 *
 * Authors: Federico Mena <federico@nuclecu.unam.mx>
 *          Raph Levien <raph@gimp.org>
 */

/*
 * TO-DO list for the canvas:
 *
 * - Allow to specify whether FooCanvasImage sizes are in units or pixels (scale or don't scale).
 *
 * - Implement a flag for foo_canvas_item_reparent() that tells the function to keep the item
 *   visually in the same place, that is, to keep it in the same place with respect to the canvas
 *   origin.
 *
 * - GC put functions for items.
 *
 * - Widget item (finish it).
 *
 * - GList *foo_canvas_gimme_all_items_contained_in_this_area (FooCanvas *canvas, Rectangle area);
 *
 * - Retrofit all the primitive items with microtile support.
 *
 * - Curve support for line item.
 *
 * - Arc item (Havoc has it; to be integrated in FooCanvasEllipse).
 *
 * - Sane font handling API.
 *
 * - Get_arg methods for items:
 *   - How to fetch the outline width and know whether it is in pixels or units?
 */


#include <math.h>
#include <string.h>
#include <stdio.h>
#include <gdk/gdkprivate.h>
#include <gtk/gtkmain.h>
#include <gtk/gtksignal.h>
#include <gtk/gtkaccessible.h>
#include <gtk/gtkwindow.h>
#include "foo-canvas.h"
#include "foo-canvas-i18n.h"

#include "foo-canvas-marshal.h"

static void foo_canvas_request_update (FooCanvas      *canvas);
static void group_add                   (FooCanvasGroup *group,
					 FooCanvasItem  *item);
static void group_remove                (FooCanvasGroup *group,
					 FooCanvasItem  *item);
static void redraw_and_repick_if_mapped (FooCanvasItem *item);

/*** FooCanvasItem ***/

/* Some convenience stuff */
#define GCI_UPDATE_MASK (FOO_CANVAS_UPDATE_REQUESTED | FOO_CANVAS_UPDATE_DEEP)
#define GCI_EPSILON 1e-18

enum {
	ITEM_PROP_0,
	ITEM_PROP_PARENT,
	ITEM_PROP_VISIBLE
};

enum {
	ITEM_EVENT,
	ITEM_LAST_SIGNAL
};

static void foo_canvas_item_class_init     (FooCanvasItemClass *class);
static void foo_canvas_item_init           (FooCanvasItem      *item);
static int  emit_event                       (FooCanvas *canvas, GdkEvent *event);

static guint item_signals[ITEM_LAST_SIGNAL];

static GtkObjectClass *item_parent_class;

static gpointer accessible_item_parent_class;
static gpointer accessible_parent_class;


/**
 * foo_canvas_item_get_type:
 *
 * Registers the &FooCanvasItem class if necessary, and returns the type ID
 * associated to it.
 *
 * Return value:  The type ID of the &FooCanvasItem class.
 **/
GType
foo_canvas_item_get_type (void)
{
	static GType canvas_item_type = 0;

	if (!canvas_item_type) {
		static const GTypeInfo canvas_item_info = {
			sizeof (FooCanvasItemClass),
			(GBaseInitFunc) NULL,
			(GBaseFinalizeFunc) NULL,
			(GClassInitFunc) foo_canvas_item_class_init,
			NULL,           /* class_finalize */
			NULL,           /* class_data */
			sizeof (FooCanvasItem),
			0,              /* n_preallocs */
			(GInstanceInitFunc) foo_canvas_item_init
		};

		canvas_item_type = g_type_register_static (gtk_object_get_type (),
							   "FooCanvasItem",
							   &canvas_item_info,
							   0);
	}

	return canvas_item_type;
}

/* Object initialization function for FooCanvasItem */
static void
foo_canvas_item_init (FooCanvasItem *item)
{
	item->object.flags |= FOO_CANVAS_ITEM_VISIBLE;
}

/**
 * foo_canvas_item_new:
 * @parent: The parent group for the new item.
 * @type: The object type of the item.
 * @first_arg_name: A list of object argument name/value pairs, NULL-terminated,
 * used to configure the item.  For example, "fill_color", "black",
 * "width_units", 5.0, NULL.
 * @Varargs:
 *
 * Creates a new canvas item with @parent as its parent group.  The item is
 * created at the top of its parent's stack, and starts up as visible.  The item
 * is of the specified @type, for example, it can be
 * foo_canvas_rect_get_type().  The list of object arguments/value pairs is
 * used to configure the item.
 *
 * Return value: The newly-created item.
 **/
FooCanvasItem *
foo_canvas_item_new (FooCanvasGroup *parent, GType type, const gchar *first_arg_name, ...)
{
	FooCanvasItem *item;
	va_list args;

	g_return_val_if_fail (FOO_IS_CANVAS_GROUP (parent), NULL);
	g_return_val_if_fail (g_type_is_a (type, foo_canvas_item_get_type ()), NULL);

	item = FOO_CANVAS_ITEM (g_object_new (type, NULL));

	va_start (args, first_arg_name);
	foo_canvas_item_construct (item, parent, first_arg_name, args);
	va_end (args);

	return item;
}


/* Performs post-creation operations on a canvas item (adding it to its parent
 * group, etc.)
 */
static void
item_post_create_setup (FooCanvasItem *item)
{
	GtkObject *obj;

	obj = GTK_OBJECT (item);

	group_add (FOO_CANVAS_GROUP (item->parent), item);

	redraw_and_repick_if_mapped (item);
}

/* Set_property handler for canvas items */
static void
foo_canvas_item_set_property (GObject *gobject, guint param_id,
			      const GValue *value, GParamSpec *pspec)
{
	FooCanvasItem *item;

	g_return_if_fail (FOO_IS_CANVAS_ITEM (gobject));

	item = FOO_CANVAS_ITEM (gobject);

	switch (param_id) {
	case ITEM_PROP_PARENT:
		if (item->parent != NULL) {
		    g_warning ("Cannot set `parent' argument after item has "
			       "already been constructed.");
		} else if (g_value_get_object (value)) {
			item->parent = FOO_CANVAS_ITEM (g_value_get_object (value));
			item->canvas = item->parent->canvas;
			item_post_create_setup (item);
		}
		break;
	case ITEM_PROP_VISIBLE:
		if (g_value_get_boolean (value)) {
			foo_canvas_item_show (item);
		} else {
			foo_canvas_item_hide (item);
		}
		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (gobject, param_id, pspec);
		break;
	}
}

/* Get_property handler for canvas items */
static void
foo_canvas_item_get_property (GObject *gobject, guint param_id,
			      GValue *value, GParamSpec *pspec)
{
	FooCanvasItem *item;

	g_return_if_fail (FOO_IS_CANVAS_ITEM (gobject));

	item = FOO_CANVAS_ITEM (gobject);

	switch (param_id) {
	case ITEM_PROP_VISIBLE:
		g_value_set_boolean (value, item->object.flags & FOO_CANVAS_ITEM_VISIBLE);
		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (gobject, param_id, pspec);
		break;
	}
}

/**
 * foo_canvas_item_construct:
 * @item: An unconstructed canvas item.
 * @parent: The parent group for the item.
 * @first_arg_name: The name of the first argument for configuring the item.
 * @args: The list of arguments used to configure the item.
 *
 * Constructs a canvas item; meant for use only by item implementations.
 **/
void
foo_canvas_item_construct (FooCanvasItem *item, FooCanvasGroup *parent,
			   const gchar *first_arg_name, va_list args)
{
	g_return_if_fail (FOO_IS_CANVAS_GROUP (parent));
	g_return_if_fail (FOO_IS_CANVAS_ITEM (item));

	item->parent = FOO_CANVAS_ITEM (parent);
	item->canvas = item->parent->canvas;

	g_object_set_valist (G_OBJECT (item), first_arg_name, args);

	item_post_create_setup (item);
}


static void
redraw_and_repick_if_mapped (FooCanvasItem *item)
{
	if (item->object.flags & FOO_CANVAS_ITEM_MAPPED) {
		foo_canvas_item_request_redraw (item);
		item->canvas->need_repick = TRUE;
	}
}


/* Standard object dispose function for canvas items */
static void
foo_canvas_item_dispose (GObject *object)
{
	FooCanvasItem *item;

	g_return_if_fail (FOO_IS_CANVAS_ITEM (object));

	item = FOO_CANVAS_ITEM (object);

	foo_canvas_item_request_redraw (item);

	/* Make the canvas forget about us */

	if (item == item->canvas->current_item) {
		item->canvas->current_item = NULL;
		item->canvas->need_repick = TRUE;
	}

	if (item == item->canvas->new_current_item) {
		item->canvas->new_current_item = NULL;
		item->canvas->need_repick = TRUE;
	}

	if (item == item->canvas->grabbed_item) {
		GdkDisplay *display = gtk_widget_get_display (GTK_WIDGET (item->canvas));
		item->canvas->grabbed_item = NULL;
		gdk_display_pointer_ungrab (display, GDK_CURRENT_TIME);
	}

	if (item == item->canvas->focused_item)
		item->canvas->focused_item = NULL;

	/* Normal destroy stuff */

	if (item->object.flags & FOO_CANVAS_ITEM_MAPPED)
		(* FOO_CANVAS_ITEM_GET_CLASS (item)->unmap) (item);

	if (item->object.flags & FOO_CANVAS_ITEM_REALIZED)
		(* FOO_CANVAS_ITEM_GET_CLASS (item)->unrealize) (item);

	if (item->parent)
		group_remove (FOO_CANVAS_GROUP (item->parent), item);

	G_OBJECT_CLASS (item_parent_class)->dispose (object);
}

/* Realize handler for canvas items */
static void
foo_canvas_item_realize (FooCanvasItem *item)
{
	if (item->parent && !(item->parent->object.flags & FOO_CANVAS_ITEM_REALIZED))
		(* FOO_CANVAS_ITEM_GET_CLASS (item->parent)->realize) (item->parent);

	if (item->parent == NULL && !GTK_WIDGET_REALIZED (GTK_WIDGET (item->canvas)))
		gtk_widget_realize (GTK_WIDGET (item->canvas));

	GTK_OBJECT_SET_FLAGS (item, FOO_CANVAS_ITEM_REALIZED);

	foo_canvas_item_request_update (item);
}

/* Unrealize handler for canvas items */
static void
foo_canvas_item_unrealize (FooCanvasItem *item)
{
	if (item->object.flags & FOO_CANVAS_ITEM_MAPPED)
		(* FOO_CANVAS_ITEM_GET_CLASS (item)->unmap) (item);

	GTK_OBJECT_UNSET_FLAGS (item, FOO_CANVAS_ITEM_REALIZED);
}

/* Map handler for canvas items */
static void
foo_canvas_item_map (FooCanvasItem *item)
{
	GTK_OBJECT_SET_FLAGS (item, FOO_CANVAS_ITEM_MAPPED);
}

/* Unmap handler for canvas items */
static void
foo_canvas_item_unmap (FooCanvasItem *item)
{
	GTK_OBJECT_UNSET_FLAGS (item, FOO_CANVAS_ITEM_MAPPED);
}

/* Update handler for canvas items */
static void
foo_canvas_item_update (FooCanvasItem *item, double i2w_dx, double i2w_dy, int flags)
{
	GTK_OBJECT_UNSET_FLAGS (item, FOO_CANVAS_ITEM_NEED_UPDATE);
	GTK_OBJECT_UNSET_FLAGS (item, FOO_CANVAS_ITEM_NEED_DEEP_UPDATE);
}

/*
 * This routine invokes the update method of the item
 * Please notice, that we take parent to canvas pixel matrix as argument
 * unlike virtual method ::update, whose argument is item 2 canvas pixel
 * matrix
 *
 * I will try to force somewhat meaningful naming for affines (Lauris)
 * General naming rule is FROM2TO, where FROM and TO are abbreviations
 * So p2cpx is Parent2CanvasPixel and i2cpx is Item2CanvasPixel
 * I hope that this helps to keep track of what really happens
 *
 */

static void
foo_canvas_item_invoke_update (FooCanvasItem *item,
			       double i2w_dx,
			       double i2w_dy,
			       int flags)
{
	int child_flags;

	child_flags = flags;

	/* apply object flags to child flags */
	child_flags &= ~FOO_CANVAS_UPDATE_REQUESTED;

	if (item->object.flags & FOO_CANVAS_ITEM_NEED_UPDATE)
		child_flags |= FOO_CANVAS_UPDATE_REQUESTED;

	if (item->object.flags & FOO_CANVAS_ITEM_NEED_DEEP_UPDATE)
		child_flags |= FOO_CANVAS_UPDATE_DEEP;

	if (child_flags & GCI_UPDATE_MASK) {
		if (FOO_CANVAS_ITEM_GET_CLASS (item)->update)
			FOO_CANVAS_ITEM_GET_CLASS (item)->update (item, i2w_dx, i2w_dy, child_flags);
	}

	/* If this fail you probably forgot to chain up to
	 * FooCanvasItem::update from a derived class */
 	g_return_if_fail (!(item->object.flags & FOO_CANVAS_ITEM_NEED_UPDATE));
}

/*
 * This routine invokes the point method of the item.
 * The arguments x, y should be in the parent item local coordinates.
 */

static double
foo_canvas_item_invoke_point (FooCanvasItem *item, double x, double y, int cx, int cy, FooCanvasItem **actual_item)
{
	/* Calculate x & y in item local coordinates */

	if (FOO_CANVAS_ITEM_GET_CLASS (item)->point)
		return FOO_CANVAS_ITEM_GET_CLASS (item)->point (item, x, y, cx, cy, actual_item);

	return 1e18;
}

/**
 * foo_canvas_item_set:
 * @item: A canvas item.
 * @first_arg_name: The list of object argument name/value pairs used to configure the item.
 * @Varargs:
 *
 * Configures a canvas item.  The arguments in the item are set to the specified
 * values, and the item is repainted as appropriate.
 **/
void
foo_canvas_item_set (FooCanvasItem *item, const gchar *first_arg_name, ...)
{
	va_list args;

	va_start (args, first_arg_name);
	foo_canvas_item_set_valist (item, first_arg_name, args);
	va_end (args);
}


/**
 * foo_canvas_item_set_valist:
 * @item: A canvas item.
 * @first_arg_name: The name of the first argument used to configure the item.
 * @args: The list of object argument name/value pairs used to configure the item.
 *
 * Configures a canvas item.  The arguments in the item are set to the specified
 * values, and the item is repainted as appropriate.
 **/
void
foo_canvas_item_set_valist (FooCanvasItem *item, const gchar *first_arg_name, va_list args)
{
	g_return_if_fail (FOO_IS_CANVAS_ITEM (item));

	g_object_set_valist (G_OBJECT (item), first_arg_name, args);

#if 0
	/* I commented this out, because item implementations have to schedule update/redraw */
	foo_canvas_item_request_redraw (item);
#endif

	item->canvas->need_repick = TRUE;
}


/**
 * foo_canvas_item_move:
 * @item: A canvas item.
 * @dx: Horizontal offset.
 * @dy: Vertical offset.
 *
 * Moves a canvas item by creating an affine transformation matrix for
 * translation by using the specified values. This happens in item
 * local coordinate system, so if you have nontrivial transform, it
 * most probably does not do, what you want.
 **/
void
foo_canvas_item_move (FooCanvasItem *item, double dx, double dy)
{
        g_return_if_fail (item != NULL);
        g_return_if_fail (FOO_IS_CANVAS_ITEM (item));

        if (!FOO_CANVAS_ITEM_GET_CLASS (item)->translate) {
                g_warning ("Item type %s does not implement translate method.\n",
                           g_type_name (GTK_OBJECT_TYPE (item)));
                return;
        }

        (* FOO_CANVAS_ITEM_GET_CLASS (item)->translate) (item, dx, dy);

	if (item->object.flags & FOO_CANVAS_ITEM_MAPPED)
		item->canvas->need_repick = TRUE;

	if (!(item->object.flags & FOO_CANVAS_ITEM_NEED_DEEP_UPDATE)) {
		item->object.flags |= FOO_CANVAS_ITEM_NEED_DEEP_UPDATE;
		if (item->parent != NULL)
			foo_canvas_item_request_update (item->parent);
		else
			foo_canvas_request_update (item->canvas);
	}

}

/* Convenience function to reorder items in a group's child list.  This puts the
 * specified link after the "before" link. Returns TRUE if the list was changed.
 */
static gboolean
put_item_after (GList *link, GList *before)
{
	FooCanvasGroup *parent;

	if (link == before)
		return FALSE;

	parent = FOO_CANVAS_GROUP (FOO_CANVAS_ITEM (link->data)->parent);

	if (before == NULL) {
		if (link == parent->item_list)
			return FALSE;

		link->prev->next = link->next;

		if (link->next)
			link->next->prev = link->prev;
		else
			parent->item_list_end = link->prev;

		link->prev = before;
		link->next = parent->item_list;
		link->next->prev = link;
		parent->item_list = link;
	} else {
		if ((link == parent->item_list_end) && (before == parent->item_list_end->prev))
			return FALSE;

		if (link->next)
			link->next->prev = link->prev;

		if (link->prev)
			link->prev->next = link->next;
		else {
			parent->item_list = link->next;
			parent->item_list->prev = NULL;
		}

		link->prev = before;
		link->next = before->next;

		link->prev->next = link;

		if (link->next)
			link->next->prev = link;
		else
			parent->item_list_end = link;
	}
	return TRUE;
}


/**
 * foo_canvas_item_raise:
 * @item: A canvas item.
 * @positions: Number of steps to raise the item.
 *
 * Raises the item in its parent's stack by the specified number of positions.
 * If the number of positions is greater than the distance to the top of the
 * stack, then the item is put at the top.
 **/
void
foo_canvas_item_raise (FooCanvasItem *item, int positions)
{
	GList *link, *before;
	FooCanvasGroup *parent;

	g_return_if_fail (FOO_IS_CANVAS_ITEM (item));
	g_return_if_fail (positions >= 0);

	if (!item->parent || positions == 0)
		return;

	parent = FOO_CANVAS_GROUP (item->parent);
	link = g_list_find (parent->item_list, item);
	g_assert (link != NULL);

	for (before = link; positions && before; positions--)
		before = before->next;

	if (!before)
		before = parent->item_list_end;

	if (put_item_after (link, before)) {
		redraw_and_repick_if_mapped (item);
	}
}


/**
 * foo_canvas_item_lower:
 * @item: A canvas item.
 * @positions: Number of steps to lower the item.
 *
 * Lowers the item in its parent's stack by the specified number of positions.
 * If the number of positions is greater than the distance to the bottom of the
 * stack, then the item is put at the bottom.
 **/
void
foo_canvas_item_lower (FooCanvasItem *item, int positions)
{
	GList *link, *before;
	FooCanvasGroup *parent;

	g_return_if_fail (FOO_IS_CANVAS_ITEM (item));
	g_return_if_fail (positions >= 1);

	if (!item->parent || positions == 0)
		return;

	parent = FOO_CANVAS_GROUP (item->parent);
	link = g_list_find (parent->item_list, item);
	g_assert (link != NULL);

	if (link->prev)
		for (before = link->prev; positions && before; positions--)
			before = before->prev;
	else
		before = NULL;

	if (put_item_after (link, before)) {
		redraw_and_repick_if_mapped (item);
	}
}


/**
 * foo_canvas_item_raise_to_top:
 * @item: A canvas item.
 *
 * Raises an item to the top of its parent's stack.
 **/
void
foo_canvas_item_raise_to_top (FooCanvasItem *item)
{
	GList *link;
	FooCanvasGroup *parent;

	g_return_if_fail (FOO_IS_CANVAS_ITEM (item));

	if (!item->parent)
		return;

	parent = FOO_CANVAS_GROUP (item->parent);
	link = g_list_find (parent->item_list, item);
	g_assert (link != NULL);

	if (put_item_after (link, parent->item_list_end)) {
		redraw_and_repick_if_mapped (item);
	}
}


/**
 * foo_canvas_item_lower_to_bottom:
 * @item: A canvas item.
 *
 * Lowers an item to the bottom of its parent's stack.
 **/
void
foo_canvas_item_lower_to_bottom (FooCanvasItem *item)
{
	GList *link;
	FooCanvasGroup *parent;

	g_return_if_fail (FOO_IS_CANVAS_ITEM (item));

	if (!item->parent)
		return;

	parent = FOO_CANVAS_GROUP (item->parent);
	link = g_list_find (parent->item_list, item);
	g_assert (link != NULL);

	if (put_item_after (link, NULL)) {
		redraw_and_repick_if_mapped (item);
	}
}

/**
 * foo_canvas_item_send_behind:
 * @item: A canvas item.
 * @behind_item: The canvas item to put item behind, or NULL
 *
 * Moves item to a in the position in the stacking order so that
 * it is placed immediately below behind_item, or at the top if
 * behind_item is NULL.
 **/
void
foo_canvas_item_send_behind (FooCanvasItem *item,
			     FooCanvasItem *behind_item)
{
	GList *item_list;
	int item_position, behind_position;

	g_return_if_fail (FOO_IS_CANVAS_ITEM (item));

	if (behind_item == NULL) {
		foo_canvas_item_raise_to_top (item);
		return;
	}

	g_return_if_fail (FOO_IS_CANVAS_ITEM (behind_item));
	g_return_if_fail (item->parent == behind_item->parent);

	item_list = FOO_CANVAS_GROUP (item->parent)->item_list;

	item_position = g_list_index (item_list, item);
	g_assert (item_position != -1);
	behind_position = g_list_index (item_list, behind_item);
	g_assert (behind_position != -1);
	g_assert (item_position != behind_position);

	if (item_position == behind_position - 1) {
		return;
	}

	if (item_position < behind_position) {
		foo_canvas_item_raise (item, (behind_position - 1) - item_position);
	} else {
		foo_canvas_item_lower (item, item_position - behind_position);
	}
}

/**
 * foo_canvas_item_show:
 * @item: A canvas item.
 *
 * Shows a canvas item.  If the item was already shown, then no action is taken.
 **/
void
foo_canvas_item_show (FooCanvasItem *item)
{
	g_return_if_fail (FOO_IS_CANVAS_ITEM (item));

	if (!(item->object.flags & FOO_CANVAS_ITEM_VISIBLE)) {
		item->object.flags |= FOO_CANVAS_ITEM_VISIBLE;

		if (!(item->object.flags & FOO_CANVAS_ITEM_REALIZED))
			(* FOO_CANVAS_ITEM_GET_CLASS (item)->realize) (item);

		if (item->parent != NULL) {
			if (!(item->object.flags & FOO_CANVAS_ITEM_MAPPED) &&
			    item->parent->object.flags & FOO_CANVAS_ITEM_MAPPED)
				(* FOO_CANVAS_ITEM_GET_CLASS (item)->map) (item);
		} else {
			if (!(item->object.flags & FOO_CANVAS_ITEM_MAPPED) &&
			    GTK_WIDGET_MAPPED (GTK_WIDGET (item->canvas)))
				(* FOO_CANVAS_ITEM_GET_CLASS (item)->map) (item);
		}

		redraw_and_repick_if_mapped (item);
	}
}


/**
 * foo_canvas_item_hide:
 * @item: A canvas item.
 *
 * Hides a canvas item.  If the item was already hidden, then no action is
 * taken.
 **/
void
foo_canvas_item_hide (FooCanvasItem *item)
{
	g_return_if_fail (FOO_IS_CANVAS_ITEM (item));

	if (item->object.flags & FOO_CANVAS_ITEM_VISIBLE) {
		item->object.flags &= ~FOO_CANVAS_ITEM_VISIBLE;

		redraw_and_repick_if_mapped (item);

		if (item->object.flags & FOO_CANVAS_ITEM_MAPPED)
			(* FOO_CANVAS_ITEM_GET_CLASS (item)->unmap) (item);

		/* No need to unrealize when we just want to hide */
	}
}


/**
 * foo_canvas_item_grab:
 * @item: A canvas item.
 * @event_mask: Mask of events that will be sent to this item.
 * @cursor: If non-NULL, the cursor that will be used while the grab is active.
 * @etime: The timestamp required for grabbing the mouse, or GDK_CURRENT_TIME.
 *
 * Specifies that all events that match the specified event mask should be sent
 * to the specified item, and also grabs the mouse by calling
 * gdk_pointer_grab().  The event mask is also used when grabbing the pointer.
 * If @cursor is not NULL, then that cursor is used while the grab is active.
 * The @etime parameter is the timestamp required for grabbing the mouse.
 *
 * Return value: If an item was already grabbed, it returns %GDK_GRAB_ALREADY_GRABBED.  If
 * the specified item was hidden by calling foo_canvas_item_hide(), then it
 * returns %GDK_GRAB_NOT_VIEWABLE.  Else, it returns the result of calling
 * gdk_pointer_grab().
 **/
int
foo_canvas_item_grab (FooCanvasItem *item, guint event_mask, GdkCursor *cursor, guint32 etime)
{
	int retval;

	g_return_val_if_fail (FOO_IS_CANVAS_ITEM (item), GDK_GRAB_NOT_VIEWABLE);
	g_return_val_if_fail (GTK_WIDGET_MAPPED (item->canvas), GDK_GRAB_NOT_VIEWABLE);

	if (item->canvas->grabbed_item)
		return GDK_GRAB_ALREADY_GRABBED;

	if (!(item->object.flags & FOO_CANVAS_ITEM_MAPPED))
		return GDK_GRAB_NOT_VIEWABLE;

	retval = gdk_pointer_grab (item->canvas->layout.bin_window,
				   FALSE,
				   event_mask,
				   NULL,
				   cursor,
				   etime);

	if (retval != GDK_GRAB_SUCCESS)
		return retval;

	item->canvas->grabbed_item = item;
	item->canvas->grabbed_event_mask = event_mask;
	item->canvas->current_item = item; /* So that events go to the grabbed item */

	return retval;
}


/**
 * foo_canvas_item_ungrab:
 * @item: A canvas item that holds a grab.
 * @etime: The timestamp for ungrabbing the mouse.
 *
 * Ungrabs the item, which must have been grabbed in the canvas, and ungrabs the
 * mouse.
 **/
void
foo_canvas_item_ungrab (FooCanvasItem *item, guint32 etime)
{
	GdkDisplay *display;

	g_return_if_fail (FOO_IS_CANVAS_ITEM (item));

	if (item->canvas->grabbed_item != item)
		return;

	display = gtk_widget_get_display (GTK_WIDGET (item->canvas));
	item->canvas->grabbed_item = NULL;
	gdk_display_pointer_ungrab (display, etime);
}


/**
 * foo_canvas_item_w2i:
 * @item: A canvas item.
 * @x: X coordinate to convert (input/output value).
 * @y: Y coordinate to convert (input/output value).
 *
 * Converts a coordinate pair from world coordinates to item-relative
 * coordinates.
 **/
void
foo_canvas_item_w2i (FooCanvasItem *item, double *x, double *y)
{
	g_return_if_fail (FOO_IS_CANVAS_ITEM (item));
	g_return_if_fail (x != NULL);
	g_return_if_fail (y != NULL);

	item = item->parent;
	while (item) {
		if (FOO_IS_CANVAS_GROUP (item)) {
			*x -= FOO_CANVAS_GROUP (item)->xpos;
			*y -= FOO_CANVAS_GROUP (item)->ypos;
		}

		item = item->parent;
	}
}


/**
 * foo_canvas_item_i2w:
 * @item: A canvas item.
 * @x: X coordinate to convert (input/output value).
 * @y: Y coordinate to convert (input/output value).
 *
 * Converts a coordinate pair from item-relative coordinates to world
 * coordinates.
 **/
void
foo_canvas_item_i2w (FooCanvasItem *item, double *x, double *y)
{
	g_return_if_fail (FOO_IS_CANVAS_ITEM (item));
	g_return_if_fail (x != NULL);
	g_return_if_fail (y != NULL);

	item = item->parent;
	while (item) {
		if (FOO_IS_CANVAS_GROUP (item)) {
			*x += FOO_CANVAS_GROUP (item)->xpos;
			*y += FOO_CANVAS_GROUP (item)->ypos;
		}

		item = item->parent;
	}
}

/* Returns whether the item is an inferior of or is equal to the parent. */
static int
is_descendant (FooCanvasItem *item, FooCanvasItem *parent)
{
	for (; item; item = item->parent)
		if (item == parent)
			return TRUE;

	return FALSE;
}

/**
 * foo_canvas_item_reparent:
 * @item: A canvas item.
 * @new_group: A canvas group.
 *
 * Changes the parent of the specified item to be the new group.  The item keeps
 * its group-relative coordinates as for its old parent, so the item may change
 * its absolute position within the canvas.
 **/
void
foo_canvas_item_reparent (FooCanvasItem *item, FooCanvasGroup *new_group)
{
	g_return_if_fail (FOO_IS_CANVAS_ITEM (item));
	g_return_if_fail (FOO_IS_CANVAS_GROUP (new_group));

	/* Both items need to be in the same canvas */
	g_return_if_fail (item->canvas == FOO_CANVAS_ITEM (new_group)->canvas);

	/* The group cannot be an inferior of the item or be the item itself --
	 * this also takes care of the case where the item is the root item of
	 * the canvas.  */
	g_return_if_fail (!is_descendant (FOO_CANVAS_ITEM (new_group), item));

	/* Everything is ok, now actually reparent the item */

	g_object_ref (GTK_OBJECT (item)); /* protect it from the unref in group_remove */

	foo_canvas_item_request_redraw (item);

	group_remove (FOO_CANVAS_GROUP (item->parent), item);
	item->parent = FOO_CANVAS_ITEM (new_group);
	group_add (new_group, item);

	/* Redraw and repick */

	redraw_and_repick_if_mapped (item);

	g_object_unref (GTK_OBJECT (item));
}

/**
 * foo_canvas_item_grab_focus:
 * @item: A canvas item.
 *
 * Makes the specified item take the keyboard focus, so all keyboard events will
 * be sent to it.  If the canvas widget itself did not have the focus, it grabs
 * it as well.
 **/
void
foo_canvas_item_grab_focus (FooCanvasItem *item)
{
	FooCanvasItem *focused_item;
	GdkEvent ev;

	g_return_if_fail (FOO_IS_CANVAS_ITEM (item));
	g_return_if_fail (GTK_WIDGET_CAN_FOCUS (GTK_WIDGET (item->canvas)));

	focused_item = item->canvas->focused_item;

	if (focused_item) {
		ev.focus_change.type = GDK_FOCUS_CHANGE;
		ev.focus_change.window = GTK_LAYOUT (item->canvas)->bin_window;
		ev.focus_change.send_event = FALSE;
		ev.focus_change.in = FALSE;

		emit_event (item->canvas, &ev);
	}

	item->canvas->focused_item = item;
	gtk_widget_grab_focus (GTK_WIDGET (item->canvas));

	if (focused_item) {
		ev.focus_change.type = GDK_FOCUS_CHANGE;
		ev.focus_change.window = GTK_LAYOUT (item->canvas)->bin_window;
		ev.focus_change.send_event = FALSE;
		ev.focus_change.in = TRUE;

		emit_event (item->canvas, &ev);
	}
}


/**
 * foo_canvas_item_get_bounds:
 * @item: A canvas item.
 * @x1: Leftmost edge of the bounding box (return value).
 * @y1: Upper edge of the bounding box (return value).
 * @x2: Rightmost edge of the bounding box (return value).
 * @y2: Lower edge of the bounding box (return value).
 *
 * Queries the bounding box of a canvas item.  The bounds are returned in the
 * coordinate system of the item's parent.
 **/
void
foo_canvas_item_get_bounds (FooCanvasItem *item, double *x1, double *y1, double *x2, double *y2)
{
	double tx1, ty1, tx2, ty2;

	g_return_if_fail (FOO_IS_CANVAS_ITEM (item));

	tx1 = ty1 = tx2 = ty2 = 0.0;

	/* Get the item's bounds in its coordinate system */

	if (FOO_CANVAS_ITEM_GET_CLASS (item)->bounds)
		(* FOO_CANVAS_ITEM_GET_CLASS (item)->bounds) (item, &tx1, &ty1, &tx2, &ty2);

	/* Return the values */

	if (x1)
		*x1 = tx1;

	if (y1)
		*y1 = ty1;

	if (x2)
		*x2 = tx2;

	if (y2)
		*y2 = ty2;
}


/**
 * foo_canvas_item_request_update
 * @item: A canvas item.
 *
 * To be used only by item implementations.  Requests that the canvas queue an
 * update for the specified item.
 **/
void
foo_canvas_item_request_update (FooCanvasItem *item)
{
	g_return_if_fail (!item->canvas->doing_update);

	if (item->object.flags & FOO_CANVAS_ITEM_NEED_UPDATE)
		return;

	item->object.flags |= FOO_CANVAS_ITEM_NEED_UPDATE;

	if (item->parent != NULL) {
		/* Recurse up the tree */
		foo_canvas_item_request_update (item->parent);
	} else {
		/* Have reached the top of the tree, make sure the update call gets scheduled. */
		foo_canvas_request_update (item->canvas);
	}
}

/**
 * foo_canvas_item_request_update
 * @item: A canvas item.
 *
 * Convenience function that informs a canvas that the specified item needs
 * to be repainted. To be used by item implementations
 **/
void
foo_canvas_item_request_redraw (FooCanvasItem *item)
{
	if (item->object.flags & FOO_CANVAS_ITEM_MAPPED)
		foo_canvas_request_redraw (item->canvas,
					   item->x1, item->y1,
					   item->x2 + 1, item->y2 + 1);
}



/*** FooCanvasGroup ***/


enum {
	GROUP_PROP_0,
	GROUP_PROP_X,
	GROUP_PROP_Y
};


static void foo_canvas_group_class_init  (FooCanvasGroupClass *class);
static void foo_canvas_group_init        (FooCanvasGroup      *group);
static void foo_canvas_group_set_property(GObject               *object,
					    guint                  param_id,
					    const GValue          *value,
					    GParamSpec            *pspec);
static void foo_canvas_group_get_property(GObject               *object,
					    guint                  param_id,
					    GValue                *value,
					    GParamSpec            *pspec);

static void foo_canvas_group_destroy     (GtkObject             *object);

static void   foo_canvas_group_update      (FooCanvasItem *item,
					      double           i2w_dx,
					      double           i2w_dy,
					      int              flags);
static void   foo_canvas_group_unrealize   (FooCanvasItem *item);
static void   foo_canvas_group_map         (FooCanvasItem *item);
static void   foo_canvas_group_unmap       (FooCanvasItem *item);
static void   foo_canvas_group_draw        (FooCanvasItem *item, GdkDrawable *drawable,
					      GdkEventExpose *expose);
static double foo_canvas_group_point       (FooCanvasItem *item, double x, double y,
					      int cx, int cy,
					      FooCanvasItem **actual_item);
static void   foo_canvas_group_translate   (FooCanvasItem *item, double dx, double dy);
static void   foo_canvas_group_bounds      (FooCanvasItem *item, double *x1, double *y1,
					      double *x2, double *y2);


static FooCanvasItemClass *group_parent_class;


/**
 * foo_canvas_group_get_type:
 *
 * Registers the &FooCanvasGroup class if necessary, and returns the type ID
 * associated to it.
 *
 * Return value:  The type ID of the &FooCanvasGroup class.
 **/
GType
foo_canvas_group_get_type (void)
{
	static GType group_type = 0;

	if (!group_type) {
		static const GTypeInfo group_info = {
			sizeof (FooCanvasGroupClass),
			(GBaseInitFunc) NULL,
			(GBaseFinalizeFunc) NULL,
			(GClassInitFunc) foo_canvas_group_class_init,
			NULL,           /* class_finalize */
			NULL,           /* class_data */
			sizeof (FooCanvasGroup),
			0,              /* n_preallocs */
			(GInstanceInitFunc) foo_canvas_group_init


		};

		group_type = g_type_register_static (foo_canvas_item_get_type (),
						     "FooCanvasGroup",
						     &group_info,
						     0);
	}

	return group_type;
}

/* Class initialization function for FooCanvasGroupClass */
static void
foo_canvas_group_class_init (FooCanvasGroupClass *class)
{
	GObjectClass *gobject_class;
	GtkObjectClass *object_class;
	FooCanvasItemClass *item_class;

	gobject_class = (GObjectClass *) class;
	object_class = (GtkObjectClass *) class;
	item_class = (FooCanvasItemClass *) class;

	group_parent_class = gtk_type_class (foo_canvas_item_get_type ());

	gobject_class->set_property = foo_canvas_group_set_property;
	gobject_class->get_property = foo_canvas_group_get_property;

	g_object_class_install_property
		(gobject_class, GROUP_PROP_X,
		 g_param_spec_double ("x",
				      _("X"),
				      _("X"),
				      -G_MAXDOUBLE, G_MAXDOUBLE, 0.0,
				      GSF_PARAM_STATIC | G_PARAM_READWRITE));
	g_object_class_install_property
		(gobject_class, GROUP_PROP_Y,
		 g_param_spec_double ("y",
				      _("Y"),
				      _("Y"),
				      -G_MAXDOUBLE, G_MAXDOUBLE, 0.0,
				      GSF_PARAM_STATIC | G_PARAM_READWRITE));

	object_class->destroy = foo_canvas_group_destroy;

	item_class->update = foo_canvas_group_update;
	item_class->unrealize = foo_canvas_group_unrealize;
	item_class->map = foo_canvas_group_map;
	item_class->unmap = foo_canvas_group_unmap;
	item_class->draw = foo_canvas_group_draw;
	item_class->point = foo_canvas_group_point;
	item_class->translate = foo_canvas_group_translate;
	item_class->bounds = foo_canvas_group_bounds;
}

/* Object initialization function for FooCanvasGroup */
static void
foo_canvas_group_init (FooCanvasGroup *group)
{
	group->xpos = 0.0;
	group->ypos = 0.0;
}

/* Set_property handler for canvas groups */
static void
foo_canvas_group_set_property (GObject *gobject, guint param_id,
			       const GValue *value, GParamSpec *pspec)
{
	FooCanvasItem *item;
	FooCanvasGroup *group;
	double old;
	gboolean moved;

	g_return_if_fail (FOO_IS_CANVAS_GROUP (gobject));

	item = FOO_CANVAS_ITEM (gobject);
	group = FOO_CANVAS_GROUP (gobject);

	moved = FALSE;
	switch (param_id) {
	case GROUP_PROP_X:
		old = group->xpos;
		group->xpos = g_value_get_double (value);
		if (old != group->xpos)
			moved = TRUE;
		break;

	case GROUP_PROP_Y:
		old = group->ypos;
		group->ypos = g_value_get_double (value);
		if (old != group->ypos)
			moved = TRUE;
		break;

	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (gobject, param_id, pspec);
		break;
	}

	if (moved) {
		item->object.flags |= FOO_CANVAS_ITEM_NEED_DEEP_UPDATE;
		if (item->parent != NULL)
			foo_canvas_item_request_update (item->parent);
		else
			foo_canvas_request_update (item->canvas);
	}
}

/* Get_property handler for canvas groups */
static void
foo_canvas_group_get_property (GObject *gobject, guint param_id,
				 GValue *value, GParamSpec *pspec)
{
	FooCanvasItem *item;
	FooCanvasGroup *group;

	g_return_if_fail (FOO_IS_CANVAS_GROUP (gobject));

	item = FOO_CANVAS_ITEM (gobject);
	group = FOO_CANVAS_GROUP (gobject);

	switch (param_id) {
	case GROUP_PROP_X:
		g_value_set_double (value, group->xpos);
		break;

	case GROUP_PROP_Y:
		g_value_set_double (value, group->ypos);
		break;

	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (gobject, param_id, pspec);
		break;
	}
}

/* Destroy handler for canvas groups */
static void
foo_canvas_group_destroy (GtkObject *object)
{
	FooCanvasGroup *group;
	FooCanvasItem *child;
	GList *list;

	g_return_if_fail (FOO_IS_CANVAS_GROUP (object));

	group = FOO_CANVAS_GROUP (object);

	list = group->item_list;
	while (list) {
		child = list->data;
		list = list->next;

		gtk_object_destroy (GTK_OBJECT (child));
	}

	if (GTK_OBJECT_CLASS (group_parent_class)->destroy)
		(* GTK_OBJECT_CLASS (group_parent_class)->destroy) (object);
}

/* Update handler for canvas groups */
static void
foo_canvas_group_update (FooCanvasItem *item, double i2w_dx, double i2w_dy, int flags)
{
	FooCanvasGroup *group;
	GList *list;
	FooCanvasItem *i;
	double bbox_x0, bbox_y0, bbox_x1, bbox_y1;
	gboolean first = TRUE;

	group = FOO_CANVAS_GROUP (item);

	(* group_parent_class->update) (item, i2w_dx, i2w_dy, flags);

	bbox_x0 = 0;
	bbox_y0 = 0;
	bbox_x1 = 0;
	bbox_y1 = 0;

	for (list = group->item_list; list; list = list->next) {
		i = list->data;

		foo_canvas_item_invoke_update (i, i2w_dx + group->xpos, i2w_dy + group->ypos, flags);

		if (first) {
			first = FALSE;
			bbox_x0 = i->x1;
			bbox_y0 = i->y1;
			bbox_x1 = i->x2;
			bbox_y1 = i->y2;
		} else {
			bbox_x0 = MIN (bbox_x0, i->x1);
			bbox_y0 = MIN (bbox_y0, i->y1);
			bbox_x1 = MAX (bbox_x1, i->x2);
			bbox_y1 = MAX (bbox_y1, i->y2);
		}
	}
	item->x1 = bbox_x0;
	item->y1 = bbox_y0;
	item->x2 = bbox_x1;
	item->y2 = bbox_y1;
}

/* Unrealize handler for canvas groups */
static void
foo_canvas_group_unrealize (FooCanvasItem *item)
{
	FooCanvasGroup *group;
	GList *list;
	FooCanvasItem *i;

	group = FOO_CANVAS_GROUP (item);

	/* Unmap group before children to avoid flash */
	if (item->object.flags & FOO_CANVAS_ITEM_MAPPED)
		(* FOO_CANVAS_ITEM_GET_CLASS (item)->unmap) (item);

	for (list = group->item_list; list; list = list->next) {
		i = list->data;

		if (i->object.flags & FOO_CANVAS_ITEM_REALIZED)
			(* FOO_CANVAS_ITEM_GET_CLASS (i)->unrealize) (i);
	}

	(* group_parent_class->unrealize) (item);
}

/* Map handler for canvas groups */
static void
foo_canvas_group_map (FooCanvasItem *item)
{
	FooCanvasGroup *group;
	GList *list;
	FooCanvasItem *i;

	group = FOO_CANVAS_GROUP (item);

	for (list = group->item_list; list; list = list->next) {
		i = list->data;

		if (i->object.flags & FOO_CANVAS_ITEM_VISIBLE &&
		    !(i->object.flags & FOO_CANVAS_ITEM_MAPPED)) {
			if (!(i->object.flags & FOO_CANVAS_ITEM_REALIZED))
				(* FOO_CANVAS_ITEM_GET_CLASS (i)->realize) (i);

			(* FOO_CANVAS_ITEM_GET_CLASS (i)->map) (i);
		}
	}

	(* group_parent_class->map) (item);
}

/* Unmap handler for canvas groups */
static void
foo_canvas_group_unmap (FooCanvasItem *item)
{
	FooCanvasGroup *group;
	GList *list;
	FooCanvasItem *i;

	group = FOO_CANVAS_GROUP (item);

	for (list = group->item_list; list; list = list->next) {
		i = list->data;

		if (i->object.flags & FOO_CANVAS_ITEM_MAPPED)
			(* FOO_CANVAS_ITEM_GET_CLASS (i)->unmap) (i);
	}

	(* group_parent_class->unmap) (item);
}

/* Draw handler for canvas groups */
static void
foo_canvas_group_draw (FooCanvasItem *item, GdkDrawable *drawable,
			 GdkEventExpose *expose)
{
	FooCanvasGroup *group;
	GList *list;
	FooCanvasItem *child = NULL;

	group = FOO_CANVAS_GROUP (item);

	for (list = group->item_list; list; list = list->next) {
		child = list->data;

		if ((child->object.flags & FOO_CANVAS_ITEM_MAPPED) &&
		    (FOO_CANVAS_ITEM_GET_CLASS (child)->draw)) {
			GdkRectangle child_rect;

			child_rect.x = child->x1;
			child_rect.y = child->y1;
			child_rect.width = child->x2 - child->x1 + 1;
			child_rect.height = child->y2 - child->y1 + 1;

			if (gdk_region_rect_in (expose->region, &child_rect) != GDK_OVERLAP_RECTANGLE_OUT)
				(* FOO_CANVAS_ITEM_GET_CLASS (child)->draw) (child, drawable, expose);
		}
	}
}

/* Point handler for canvas groups */
static double
foo_canvas_group_point (FooCanvasItem *item, double x, double y, int cx, int cy,
			FooCanvasItem **actual_item)
{
	FooCanvasGroup *group;
	GList *list;
	FooCanvasItem *child, *point_item;
	int x1, y1, x2, y2;
	double gx, gy;
	double dist, best;
	int has_point;

	group = FOO_CANVAS_GROUP (item);

	x1 = cx - item->canvas->close_enough;
	y1 = cy - item->canvas->close_enough;
	x2 = cx + item->canvas->close_enough;
	y2 = cy + item->canvas->close_enough;

	best = 0.0;
	*actual_item = NULL;

	gx = x - group->xpos;
	gy = y - group->ypos;

	dist = 0.0; /* keep gcc happy */

	for (list = group->item_list; list; list = list->next) {
		child = list->data;

		if ((child->x1 > x2) || (child->y1 > y2) || (child->x2 < x1) || (child->y2 < y1))
			continue;

		point_item = NULL; /* cater for incomplete item implementations */

		if ((child->object.flags & FOO_CANVAS_ITEM_MAPPED)
		    && FOO_CANVAS_ITEM_GET_CLASS (child)->point) {
			dist = foo_canvas_item_invoke_point (child, gx, gy, cx, cy, &point_item);
			has_point = TRUE;
		} else
			has_point = FALSE;

		if (has_point
		    && point_item
		    && ((int) (dist * item->canvas->pixels_per_unit + 0.5)
			<= item->canvas->close_enough)) {
			best = dist;
			*actual_item = point_item;
		}
	}

	return best;
}

void
foo_canvas_group_translate (FooCanvasItem *item, double dx, double dy)
{
        FooCanvasGroup *group;

        group = FOO_CANVAS_GROUP (item);

        group->xpos += dx;
        group->ypos += dy;
}

/* Bounds handler for canvas groups */
static void
foo_canvas_group_bounds (FooCanvasItem *item, double *x1, double *y1, double *x2, double *y2)
{
	FooCanvasGroup *group;
	FooCanvasItem *child;
	GList *list;
	double tx1, ty1, tx2, ty2;
	double minx, miny, maxx, maxy;
	int set;

	group = FOO_CANVAS_GROUP (item);

	/* Get the bounds of the first visible item */

	child = NULL; /* Unnecessary but eliminates a warning. */

	set = FALSE;

	for (list = group->item_list; list; list = list->next) {
		child = list->data;

		if (child->object.flags & FOO_CANVAS_ITEM_MAPPED) {
			set = TRUE;
			foo_canvas_item_get_bounds (child, &minx, &miny, &maxx, &maxy);
			break;
		}
	}

	/* If there were no visible items, return an empty bounding box */

	if (!set) {
		*x1 = *y1 = *x2 = *y2 = 0.0;
		return;
	}

	/* Now we can grow the bounds using the rest of the items */

	list = list->next;

	for (; list; list = list->next) {
		child = list->data;

		if (!(child->object.flags & FOO_CANVAS_ITEM_MAPPED))
			continue;

		foo_canvas_item_get_bounds (child, &tx1, &ty1, &tx2, &ty2);

		if (tx1 < minx)
			minx = tx1;

		if (ty1 < miny)
			miny = ty1;

		if (tx2 > maxx)
			maxx = tx2;

		if (ty2 > maxy)
			maxy = ty2;
	}

	/* Make the bounds be relative to our parent's coordinate system */

	if (item->parent) {
		minx += group->xpos;
		miny += group->ypos;
		maxx += group->xpos;
		maxy += group->ypos;
	}

	*x1 = minx;
	*y1 = miny;
	*x2 = maxx;
	*y2 = maxy;
}

/* Adds an item to a group */
static void
group_add (FooCanvasGroup *group, FooCanvasItem *item)
{
	g_object_ref (GTK_OBJECT (item));
	gtk_object_sink (GTK_OBJECT (item));

	if (!group->item_list) {
		group->item_list = g_list_append (group->item_list, item);
		group->item_list_end = group->item_list;
	} else
		group->item_list_end = g_list_append (group->item_list_end, item)->next;

	if (item->object.flags & FOO_CANVAS_ITEM_VISIBLE &&
	    group->item.object.flags & FOO_CANVAS_ITEM_MAPPED) {
		if (!(item->object.flags & FOO_CANVAS_ITEM_REALIZED))
			(* FOO_CANVAS_ITEM_GET_CLASS (item)->realize) (item);

		if (!(item->object.flags & FOO_CANVAS_ITEM_MAPPED))
			(* FOO_CANVAS_ITEM_GET_CLASS (item)->map) (item);
	}
}

/* Removes an item from a group */
static void
group_remove (FooCanvasGroup *group, FooCanvasItem *item)
{
	GList *children;

	g_return_if_fail (FOO_IS_CANVAS_GROUP (group));
	g_return_if_fail (FOO_IS_CANVAS_ITEM (item));

	for (children = group->item_list; children; children = children->next)
		if (children->data == item) {
			if (item->object.flags & FOO_CANVAS_ITEM_MAPPED)
				(* FOO_CANVAS_ITEM_GET_CLASS (item)->unmap) (item);

			if (item->object.flags & FOO_CANVAS_ITEM_REALIZED)
				(* FOO_CANVAS_ITEM_GET_CLASS (item)->unrealize) (item);

			/* Unparent the child */

			item->parent = NULL;
			g_object_unref (GTK_OBJECT (item));

			/* Remove it from the list */

			if (children == group->item_list_end)
				group->item_list_end = children->prev;

			group->item_list = g_list_remove_link (group->item_list, children);
			g_list_free (children);
			break;
		}
}


/*** FooCanvas ***/


enum {
	DRAW_BACKGROUND,
	LAST_SIGNAL
};

static void foo_canvas_class_init          (FooCanvasClass *class);
static void foo_canvas_init                (FooCanvas      *canvas);
static void foo_canvas_destroy             (GtkObject        *object);
static void foo_canvas_map                 (GtkWidget        *widget);
static void foo_canvas_unmap               (GtkWidget        *widget);
static void foo_canvas_realize             (GtkWidget        *widget);
static void foo_canvas_unrealize           (GtkWidget        *widget);
static void foo_canvas_size_allocate       (GtkWidget        *widget,
					      GtkAllocation    *allocation);
static gint foo_canvas_button              (GtkWidget        *widget,
					      GdkEventButton   *event);
static gint foo_canvas_motion              (GtkWidget        *widget,
					      GdkEventMotion   *event);
static gint foo_canvas_expose              (GtkWidget        *widget,
					      GdkEventExpose   *event);
static gint foo_canvas_key                 (GtkWidget        *widget,
					      GdkEventKey      *event);
static gint foo_canvas_crossing            (GtkWidget        *widget,
					      GdkEventCrossing *event);
static gint foo_canvas_focus_in            (GtkWidget        *widget,
					      GdkEventFocus    *event);
static gint foo_canvas_focus_out           (GtkWidget        *widget,
					      GdkEventFocus    *event);
static void foo_canvas_request_update_real (FooCanvas      *canvas);
static void foo_canvas_draw_background     (FooCanvas      *canvas,
					      int               x,
					      int               y,
					      int               width,
					      int               height);


static GtkLayoutClass *canvas_parent_class;

static guint canvas_signals[LAST_SIGNAL];

/**
 * foo_canvas_get_type:
 *
 * Registers the &FooCanvas class if necessary, and returns the type ID
 * associated to it.
 *
 * Return value:  The type ID of the &FooCanvas class.
 **/
GType
foo_canvas_get_type (void)
{
	static GType canvas_type = 0;

	if (!canvas_type) {
		static const GTypeInfo canvas_info = {
			sizeof (FooCanvasClass),
			(GBaseInitFunc) NULL,
			(GBaseFinalizeFunc) NULL,
			(GClassInitFunc) foo_canvas_class_init,
			NULL,           /* class_finalize */
			NULL,           /* class_data */
			sizeof (FooCanvas),
			0,              /* n_preallocs */
			(GInstanceInitFunc) foo_canvas_init
		};

		canvas_type = g_type_register_static (gtk_layout_get_type (),
						      "FooCanvas",
						      &canvas_info,
						      0);
	}

	return canvas_type;
}

static void
foo_canvas_get_property (GObject    *object,
			   guint       prop_id,
			   GValue     *value,
			   GParamSpec *pspec)
{
	switch (prop_id) {
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
foo_canvas_set_property (GObject      *object,
			   guint         prop_id,
			   const GValue *value,
			   GParamSpec   *pspec)
{
	switch (prop_id) {
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
foo_canvas_accessible_adjustment_changed (GtkAdjustment *adjustment,
					  gpointer       data)
{
	AtkObject *atk_obj;

	/* The scrollbars have changed */
	atk_obj = ATK_OBJECT (data);

	g_signal_emit_by_name (atk_obj, "visible_data_changed");
}

static void
foo_canvas_accessible_initialize (AtkObject *obj,
				  gpointer   data)
{
	FooCanvas *canvas;

	if (ATK_OBJECT_CLASS (accessible_parent_class)->initialize != NULL)
		ATK_OBJECT_CLASS (accessible_parent_class)->initialize (obj, data);

	canvas = FOO_CANVAS (data);
	g_signal_connect (canvas->layout.hadjustment,
			  "value_changed",
			  G_CALLBACK (foo_canvas_accessible_adjustment_changed),
			  obj);
	g_signal_connect (canvas->layout.vadjustment,
			  "value_changed",
			  G_CALLBACK (foo_canvas_accessible_adjustment_changed),
			  obj);

	obj->role = ATK_ROLE_LAYERED_PANE;
}

static gint
foo_canvas_accessible_get_n_children (AtkObject* obj)
{
 	GtkAccessible *accessible;
	GtkWidget *widget;
	FooCanvas *canvas;
	FooCanvasGroup *root_group;

	accessible = GTK_ACCESSIBLE (obj);
	widget = accessible->widget;
	if (widget == NULL) {
		/* State is defunct */
		return 0;
	}

	g_return_val_if_fail (FOO_IS_CANVAS (widget), 0);

	canvas = FOO_CANVAS (widget);
	root_group = foo_canvas_root (canvas);
	g_return_val_if_fail (root_group, 0);
	return 1;
}

static AtkObject*
foo_canvas_accessible_ref_child (AtkObject *obj,
                                 gint       i)
{
	GtkAccessible *accessible;
	GtkWidget *widget;
	FooCanvas *canvas;
	FooCanvasGroup *root_group;
	AtkObject *atk_object;

	/* Canvas only has one child, so return NULL if index is non zero */
	if (i != 0) {
        	return NULL;
	}

	accessible = GTK_ACCESSIBLE (obj);
	widget = accessible->widget;
	if (widget == NULL) {
		/* State is defunct */
		return NULL;
	}

	canvas = FOO_CANVAS (widget);
	root_group = foo_canvas_root (canvas);
	g_return_val_if_fail (root_group, NULL);
	atk_object = atk_gobject_accessible_for_object (G_OBJECT (root_group));
	g_object_ref (atk_object);

	g_warning ("Accessible support for FooGroup needs to be implemented");

	return atk_object;
}

static void
foo_canvas_accessible_class_init (AtkObjectClass *klass)
{
 	accessible_parent_class = g_type_class_peek_parent (klass);

	klass->initialize = foo_canvas_accessible_initialize;
	klass->get_n_children = foo_canvas_accessible_get_n_children;
	klass->ref_child = foo_canvas_accessible_ref_child;
}

static GType
foo_canvas_accessible_get_type (void)
{
	static GType type = 0;

	if (!type) {
		AtkObjectFactory *factory;
		GType parent_atk_type;
		GTypeQuery query;
		GTypeInfo tinfo = { 0 };

		factory = atk_registry_get_factory (atk_get_default_registry(),
						    GTK_TYPE_WIDGET);
		if (!factory) {
			return G_TYPE_INVALID;
		}
		parent_atk_type = atk_object_factory_get_accessible_type (factory);
		if (!parent_atk_type) {
			return G_TYPE_INVALID;
		}
		g_type_query (parent_atk_type, &query);
		tinfo.class_init = (GClassInitFunc) foo_canvas_accessible_class_init;
		tinfo.class_size = query.class_size;
		tinfo.instance_size = query.instance_size;
		type = g_type_register_static (parent_atk_type,
					       "FooCanvasAccessibility",
					       &tinfo, 0);
	}
	return type;
}

static AtkObject *
foo_canvas_accessible_create (GObject *for_object)
{
	GType type;
	AtkObject *accessible;
	FooCanvas *canvas;

	canvas = FOO_CANVAS (for_object);
	g_return_val_if_fail (canvas != NULL, NULL);

	type = foo_canvas_accessible_get_type ();

	if (type == G_TYPE_INVALID) {
		return atk_no_op_object_new (for_object);
	}

	accessible = g_object_new (type, NULL);
	atk_object_initialize (accessible, for_object);
	return accessible;
}

static GType
foo_canvas_accessible_factory_get_accessible_type (void)
{
	return foo_canvas_accessible_get_type ();
}

static AtkObject*
foo_canvas_accessible_factory_create_accessible (GObject *obj)
{
	AtkObject *accessible;

	g_return_val_if_fail (G_IS_OBJECT (obj), NULL);

	accessible = foo_canvas_accessible_create (obj);

	return accessible;
}

static void
foo_canvas_accessible_factory_class_init (AtkObjectFactoryClass *klass)
{
	klass->create_accessible = foo_canvas_accessible_factory_create_accessible;
	klass->get_accessible_type = foo_canvas_accessible_factory_get_accessible_type;
}

static GType
foo_canvas_accessible_factory_get_type (void)
{
	static GType type = 0;

	if (!type) {
		static const GTypeInfo tinfo = {
			sizeof (AtkObjectFactoryClass),
			(GBaseInitFunc) NULL,
			(GBaseFinalizeFunc) NULL,
			(GClassInitFunc) foo_canvas_accessible_factory_class_init,
			NULL,		/* class_finalize */
			NULL,		/* class_data */
			sizeof (AtkObjectFactory),
			0,		/* n_preallocs */
			NULL
		};
		type = g_type_register_static (ATK_TYPE_OBJECT_FACTORY,
					       "FooCanvasAccessibilityFactory",
					       &tinfo, 0);
	}

	return type;
}


/* Class initialization function for FooCanvasClass */
static void
foo_canvas_class_init (FooCanvasClass *klass)
{
	GObjectClass   *gobject_class;
	GtkObjectClass *object_class;
	GtkWidgetClass *widget_class;

	gobject_class = (GObjectClass *)klass;
	object_class  = (GtkObjectClass *) klass;
	widget_class  = (GtkWidgetClass *) klass;

	canvas_parent_class = gtk_type_class (gtk_layout_get_type ());

	gobject_class->set_property = foo_canvas_set_property;
	gobject_class->get_property = foo_canvas_get_property;

	object_class->destroy = foo_canvas_destroy;

	widget_class->map = foo_canvas_map;
	widget_class->unmap = foo_canvas_unmap;
	widget_class->realize = foo_canvas_realize;
	widget_class->unrealize = foo_canvas_unrealize;
	widget_class->size_allocate = foo_canvas_size_allocate;
	widget_class->button_press_event = foo_canvas_button;
	widget_class->button_release_event = foo_canvas_button;
	widget_class->motion_notify_event = foo_canvas_motion;
	widget_class->expose_event = foo_canvas_expose;
	widget_class->key_press_event = foo_canvas_key;
	widget_class->key_release_event = foo_canvas_key;
	widget_class->enter_notify_event = foo_canvas_crossing;
	widget_class->leave_notify_event = foo_canvas_crossing;
	widget_class->focus_in_event = foo_canvas_focus_in;
	widget_class->focus_out_event = foo_canvas_focus_out;

	klass->draw_background = foo_canvas_draw_background;
	klass->request_update = foo_canvas_request_update_real;

	canvas_signals[DRAW_BACKGROUND] =
		g_signal_new ("draw_background",
			      G_TYPE_FROM_CLASS (object_class),
			      G_SIGNAL_RUN_LAST,
			      G_STRUCT_OFFSET (FooCanvasClass, draw_background),
			      NULL, NULL,
			      foo_canvas_marshal_VOID__INT_INT_INT_INT,
			      G_TYPE_NONE, 4,
			      G_TYPE_INT, G_TYPE_INT, G_TYPE_INT, G_TYPE_INT);

	atk_registry_set_factory_type (atk_get_default_registry (),
				       FOO_TYPE_CANVAS,
				       foo_canvas_accessible_factory_get_type ());
}

/* Callback used when the root item of a canvas is destroyed.  The user should
 * never ever do this, so we panic if this happens.
 */
static void
panic_root_destroyed (GtkObject *object, gpointer data)
{
	g_error ("Eeeek, root item %p of canvas %p was destroyed!", object, data);
}

/* Object initialization function for FooCanvas */
static void
foo_canvas_init (FooCanvas *canvas)
{
	GTK_WIDGET_SET_FLAGS (canvas, GTK_CAN_FOCUS);

	gtk_widget_set_redraw_on_allocate (GTK_WIDGET (canvas), FALSE);

	canvas->scroll_x1 = 0.0;
	canvas->scroll_y1 = 0.0;
	canvas->scroll_x2 = canvas->layout.width;
	canvas->scroll_y2 = canvas->layout.height;

	canvas->pixels_per_unit = 1.0;

	canvas->pick_event.type = GDK_LEAVE_NOTIFY;
	canvas->pick_event.crossing.x = 0;
	canvas->pick_event.crossing.y = 0;

	gtk_layout_set_hadjustment (GTK_LAYOUT (canvas), NULL);
	gtk_layout_set_vadjustment (GTK_LAYOUT (canvas), NULL);

	/* Create the root item as a special case */

	canvas->root = FOO_CANVAS_ITEM (g_object_new (foo_canvas_group_get_type (), NULL));
	canvas->root->canvas = canvas;

	g_object_ref (GTK_OBJECT (canvas->root));
	gtk_object_sink (GTK_OBJECT (canvas->root));

	canvas->root_destroy_id = g_signal_connect (GTK_OBJECT (canvas->root), "destroy",
						    (GtkSignalFunc) panic_root_destroyed,
						    canvas);

	canvas->need_repick = TRUE;
	canvas->doing_update = FALSE;
}

/* Convenience function to remove the idle handler of a canvas */
static void
remove_idle (FooCanvas *canvas)
{
	if (canvas->idle_id == 0)
		return;

	g_source_remove (canvas->idle_id);
	canvas->idle_id = 0;
}

/* Removes the transient state of the canvas (idle handler, grabs). */
static void
shutdown_transients (FooCanvas *canvas)
{
	/* We turn off the need_redraw flag, since if the canvas is mapped again
	 * it will request a redraw anyways.  We do not turn off the need_update
	 * flag, though, because updates are not queued when the canvas remaps
	 * itself.
	 */
	if (canvas->need_redraw) {
		canvas->need_redraw = FALSE;
	}

	if (canvas->grabbed_item) {
		GdkDisplay *display = gtk_widget_get_display (GTK_WIDGET (canvas));
		canvas->grabbed_item = NULL;
		gdk_display_pointer_ungrab (display, GDK_CURRENT_TIME);
	}

	remove_idle (canvas);
}

/* Destroy handler for FooCanvas */
static void
foo_canvas_destroy (GtkObject *object)
{
	FooCanvas *canvas;

	g_return_if_fail (FOO_IS_CANVAS (object));

	/* remember, destroy can be run multiple times! */

	canvas = FOO_CANVAS (object);

	if (canvas->root_destroy_id) {
		g_signal_handler_disconnect (GTK_OBJECT (canvas->root), canvas->root_destroy_id);
		canvas->root_destroy_id = 0;
	}
	if (canvas->root) {
		g_object_unref (GTK_OBJECT (canvas->root));
		canvas->root = NULL;
	}

	shutdown_transients (canvas);

	if (GTK_OBJECT_CLASS (canvas_parent_class)->destroy)
		(* GTK_OBJECT_CLASS (canvas_parent_class)->destroy) (object);
}

/**
 * foo_canvas_new:
 * @void:
 *
 * Creates a new empty canvas.  If you wish to use the
 * &FooCanvasImage item inside this canvas, then you must push the gdk_imlib
 * visual and colormap before calling this function, and they can be popped
 * afterwards.
 *
 * Return value: A newly-created canvas.
 **/
GtkWidget *
foo_canvas_new (void)
{
	return GTK_WIDGET (g_object_new (foo_canvas_get_type (), NULL));
}

/* Map handler for the canvas */
static void
foo_canvas_map (GtkWidget *widget)
{
	FooCanvas *canvas;

	g_return_if_fail (FOO_IS_CANVAS (widget));

	/* Normal widget mapping stuff */

	if (GTK_WIDGET_CLASS (canvas_parent_class)->map)
		(* GTK_WIDGET_CLASS (canvas_parent_class)->map) (widget);

	canvas = FOO_CANVAS (widget);

	/* Map items */

	if (canvas->root->object.flags & FOO_CANVAS_ITEM_VISIBLE &&
	    !(canvas->root->object.flags & FOO_CANVAS_ITEM_MAPPED) &&
	    FOO_CANVAS_ITEM_GET_CLASS (canvas->root)->map)
		(* FOO_CANVAS_ITEM_GET_CLASS (canvas->root)->map) (canvas->root);
}

/* Unmap handler for the canvas */
static void
foo_canvas_unmap (GtkWidget *widget)
{
	FooCanvas *canvas;

	g_return_if_fail (FOO_IS_CANVAS (widget));

	canvas = FOO_CANVAS (widget);

	shutdown_transients (canvas);

	/* Unmap items */

	if (FOO_CANVAS_ITEM_GET_CLASS (canvas->root)->unmap)
		(* FOO_CANVAS_ITEM_GET_CLASS (canvas->root)->unmap) (canvas->root);

	/* Normal widget unmapping stuff */

	if (GTK_WIDGET_CLASS (canvas_parent_class)->unmap)
		(* GTK_WIDGET_CLASS (canvas_parent_class)->unmap) (widget);
}

/* Realize handler for the canvas */
static void
foo_canvas_realize (GtkWidget *widget)
{
	FooCanvas *canvas;

	g_return_if_fail (FOO_IS_CANVAS (widget));

	/* Normal widget realization stuff */

	if (GTK_WIDGET_CLASS (canvas_parent_class)->realize)
		(* GTK_WIDGET_CLASS (canvas_parent_class)->realize) (widget);

	canvas = FOO_CANVAS (widget);

	gdk_window_set_events (canvas->layout.bin_window,
			       (gdk_window_get_events (canvas->layout.bin_window)
				 | GDK_EXPOSURE_MASK
				 | GDK_BUTTON_PRESS_MASK
				 | GDK_BUTTON_RELEASE_MASK
				 | GDK_POINTER_MOTION_MASK
				 | GDK_KEY_PRESS_MASK
				 | GDK_KEY_RELEASE_MASK
				 | GDK_ENTER_NOTIFY_MASK
				 | GDK_LEAVE_NOTIFY_MASK
				 | GDK_FOCUS_CHANGE_MASK));

	/* Create our own temporary pixmap gc and realize all the items */

	canvas->pixmap_gc = gdk_gc_new (canvas->layout.bin_window);

	(* FOO_CANVAS_ITEM_GET_CLASS (canvas->root)->realize) (canvas->root);
}

/* Unrealize handler for the canvas */
static void
foo_canvas_unrealize (GtkWidget *widget)
{
	FooCanvas *canvas;

	g_return_if_fail (FOO_IS_CANVAS (widget));

	canvas = FOO_CANVAS (widget);

	shutdown_transients (canvas);

	/* Unrealize items and parent widget */

	(* FOO_CANVAS_ITEM_GET_CLASS (canvas->root)->unrealize) (canvas->root);

	g_object_unref (canvas->pixmap_gc);
	canvas->pixmap_gc = NULL;

	if (GTK_WIDGET_CLASS (canvas_parent_class)->unrealize)
		(* GTK_WIDGET_CLASS (canvas_parent_class)->unrealize) (widget);
}

/* Handles scrolling of the canvas.  Adjusts the scrolling and zooming offset to
 * keep as much as possible of the canvas scrolling region in view.
 */
static void
scroll_to (FooCanvas *canvas, int cx, int cy, gboolean redraw)
{
	int scroll_width, scroll_height;
	int right_limit, bottom_limit;
	int old_zoom_xofs, old_zoom_yofs;
	int changed_x = FALSE, changed_y = FALSE;
	int canvas_width, canvas_height;

	canvas_width = GTK_WIDGET (canvas)->allocation.width;
	canvas_height = GTK_WIDGET (canvas)->allocation.height;

	scroll_width = floor ((canvas->scroll_x2 - canvas->scroll_x1) * canvas->pixels_per_unit + 0.5);
	scroll_height = floor ((canvas->scroll_y2 - canvas->scroll_y1) * canvas->pixels_per_unit + 0.5);

	right_limit = scroll_width - canvas_width;
	bottom_limit = scroll_height - canvas_height;

	old_zoom_xofs = canvas->zoom_xofs;
	old_zoom_yofs = canvas->zoom_yofs;

	if (right_limit < 0) {
		cx = 0;
		if (canvas->center_scroll_region) {
			canvas->zoom_xofs = (canvas_width - scroll_width) / 2;
			scroll_width = canvas_width;
		} else {
			canvas->zoom_xofs = 0;
		}
	} else if (cx < 0) {
		cx = 0;
		canvas->zoom_xofs = 0;
	} else
		canvas->zoom_xofs = 0;

	if (bottom_limit < 0) {
		cy = 0;
		if (canvas->center_scroll_region) {
			canvas->zoom_yofs = (canvas_height - scroll_height) / 2;
			scroll_height = canvas_height;
		} else {
			canvas->zoom_yofs = 0;
		}
	} else if (cy < 0) {
		cy = 0;
		canvas->zoom_yofs = 0;
	} else if (cy > bottom_limit) {
		cy = bottom_limit;
		canvas->zoom_yofs = 0;
	} else
		canvas->zoom_yofs = 0;

	if ((canvas->zoom_xofs != old_zoom_xofs) || (canvas->zoom_yofs != old_zoom_yofs)) {
		/* This can only occur, if either canvas size or widget size changes */
		/* So I think we can request full redraw here */
		/* More stuff - we have to mark root as needing fresh affine (Lauris) */
		if (!(canvas->root->object.flags & FOO_CANVAS_ITEM_NEED_DEEP_UPDATE)) {
			canvas->root->object.flags |= FOO_CANVAS_ITEM_NEED_DEEP_UPDATE;
			foo_canvas_request_update (canvas);
		}
		gtk_widget_queue_draw (GTK_WIDGET (canvas));
	}

	if (((int) canvas->layout.hadjustment->value) != cx) {
		canvas->layout.hadjustment->value = cx;
		changed_x = TRUE;
	}

	if (((int) canvas->layout.vadjustment->value) != cy) {
		canvas->layout.vadjustment->value = cy;
		changed_y = TRUE;
	}

	if ((scroll_width != (int) canvas->layout.width) || (scroll_height != (int) canvas->layout.height)) {
		gtk_layout_set_size (GTK_LAYOUT (canvas), scroll_width, scroll_height);
	}

	/* Signal GtkLayout that it should do a redraw. */
	if (redraw) {
	if (changed_x)
		g_signal_emit_by_name (GTK_OBJECT (canvas->layout.hadjustment), "value_changed");
	if (changed_y)
		g_signal_emit_by_name (GTK_OBJECT (canvas->layout.vadjustment), "value_changed");
}
}

/* Size allocation handler for the canvas */
static void
foo_canvas_size_allocate (GtkWidget *widget, GtkAllocation *allocation)
{
	FooCanvas *canvas;

	g_return_if_fail (FOO_IS_CANVAS (widget));
	g_return_if_fail (allocation != NULL);

	if (GTK_WIDGET_CLASS (canvas_parent_class)->size_allocate)
		(* GTK_WIDGET_CLASS (canvas_parent_class)->size_allocate) (widget, allocation);

	canvas = FOO_CANVAS (widget);

	/* Recenter the view, if appropriate */

	canvas->layout.hadjustment->page_size = allocation->width;
	canvas->layout.hadjustment->page_increment = allocation->width / 2;

	canvas->layout.vadjustment->page_size = allocation->height;
	canvas->layout.vadjustment->page_increment = allocation->height / 2;

	scroll_to (canvas,
		   canvas->layout.hadjustment->value,
		   canvas->layout.vadjustment->value, TRUE);

	g_signal_emit_by_name (GTK_OBJECT (canvas->layout.hadjustment), "changed");
	g_signal_emit_by_name (GTK_OBJECT (canvas->layout.vadjustment), "changed");
}

/* Emits an event for an item in the canvas, be it the current item, grabbed
 * item, or focused item, as appropriate.
 */

static int
emit_event (FooCanvas *canvas, GdkEvent *event)
{
	GdkEvent ev;
	gint finished;
	FooCanvasItem *item;
	FooCanvasItem *parent;
	guint mask;

	/* Could be an old pick event */
	if (!GTK_WIDGET_REALIZED (canvas)) {
		return FALSE;
	}

	/* Perform checks for grabbed items */

	if (canvas->grabbed_item &&
	    !is_descendant (canvas->current_item, canvas->grabbed_item)) {
		return FALSE;
        }

	if (canvas->grabbed_item) {
		switch (event->type) {
		case GDK_ENTER_NOTIFY:
			mask = GDK_ENTER_NOTIFY_MASK;
			break;

		case GDK_LEAVE_NOTIFY:
			mask = GDK_LEAVE_NOTIFY_MASK;
			break;

		case GDK_MOTION_NOTIFY:
			mask = GDK_POINTER_MOTION_MASK;
			break;

		case GDK_BUTTON_PRESS:
		case GDK_2BUTTON_PRESS:
		case GDK_3BUTTON_PRESS:
			mask = GDK_BUTTON_PRESS_MASK;
			break;

		case GDK_BUTTON_RELEASE:
			mask = GDK_BUTTON_RELEASE_MASK;
			break;

		case GDK_KEY_PRESS:
			mask = GDK_KEY_PRESS_MASK;
			break;

		case GDK_KEY_RELEASE:
			mask = GDK_KEY_RELEASE_MASK;
			break;

		default:
			mask = 0;
			break;
		}

		if (!(mask & canvas->grabbed_event_mask))
			return FALSE;
	}

	/* Convert to world coordinates -- we have two cases because of diferent
	 * offsets of the fields in the event structures.
	 */

	ev = *event;

	switch (ev.type)
        {
	case GDK_ENTER_NOTIFY:
	case GDK_LEAVE_NOTIFY:
		foo_canvas_window_to_world (canvas,
					      ev.crossing.x, ev.crossing.y,
					      &ev.crossing.x, &ev.crossing.y);
		break;

	case GDK_MOTION_NOTIFY:
                foo_canvas_window_to_world (canvas,
                                              ev.motion.x, ev.motion.y,
                                              &ev.motion.x, &ev.motion.y);
                break;

	case GDK_BUTTON_PRESS:
	case GDK_2BUTTON_PRESS:
	case GDK_3BUTTON_PRESS:
                foo_canvas_window_to_world (canvas,
                                              ev.motion.x, ev.motion.y,
                                              &ev.motion.x, &ev.motion.y);
                break;

	case GDK_BUTTON_RELEASE:
		foo_canvas_window_to_world (canvas,
					      ev.motion.x, ev.motion.y,
					      &ev.motion.x, &ev.motion.y);
		break;

	default:
		break;
	}

	/* Choose where we send the event */

	item = canvas->current_item;

	if (canvas->focused_item
	    && ((event->type == GDK_KEY_PRESS) ||
		(event->type == GDK_KEY_RELEASE) ||
		(event->type == GDK_FOCUS_CHANGE)))
		item = canvas->focused_item;

	/* The event is propagated up the hierarchy (for if someone connected to
	 * a group instead of a leaf event), and emission is stopped if a
	 * handler returns TRUE, just like for GtkWidget events.
	 */

	finished = FALSE;

	while (item && !finished) {
		g_object_ref (GTK_OBJECT (item));

		g_signal_emit (
		       GTK_OBJECT (item), item_signals[ITEM_EVENT], 0,
			&ev, &finished);

		parent = item->parent;
		g_object_unref (GTK_OBJECT (item));

		item = parent;
	}

	return finished;
}

/* Re-picks the current item in the canvas, based on the event's coordinates.
 * Also emits enter/leave events for items as appropriate.
 */
static int
pick_current_item (FooCanvas *canvas, GdkEvent *event)
{
	int button_down;
	double x, y;
	int cx, cy;
	int retval;

	retval = FALSE;

	/* If a button is down, we'll perform enter and leave events on the
	 * current item, but not enter on any other item.  This is more or less
	 * like X pointer grabbing for canvas items.
	 */
	button_down = canvas->state & (GDK_BUTTON1_MASK
				       | GDK_BUTTON2_MASK
				       | GDK_BUTTON3_MASK
				       | GDK_BUTTON4_MASK
				       | GDK_BUTTON5_MASK);
	if (!button_down)
		canvas->left_grabbed_item = FALSE;

	/* Save the event in the canvas.  This is used to synthesize enter and
	 * leave events in case the current item changes.  It is also used to
	 * re-pick the current item if the current one gets deleted.  Also,
	 * synthesize an enter event.
	 */
	if (event != &canvas->pick_event) {
		if ((event->type == GDK_MOTION_NOTIFY) || (event->type == GDK_BUTTON_RELEASE)) {
			/* these fields have the same offsets in both types of events */

			canvas->pick_event.crossing.type       = GDK_ENTER_NOTIFY;
			canvas->pick_event.crossing.window     = event->motion.window;
			canvas->pick_event.crossing.send_event = event->motion.send_event;
			canvas->pick_event.crossing.subwindow  = NULL;
			canvas->pick_event.crossing.x          = event->motion.x;
			canvas->pick_event.crossing.y          = event->motion.y;
			canvas->pick_event.crossing.mode       = GDK_CROSSING_NORMAL;
			canvas->pick_event.crossing.detail     = GDK_NOTIFY_NONLINEAR;
			canvas->pick_event.crossing.focus      = FALSE;
			canvas->pick_event.crossing.state      = event->motion.state;

			/* these fields don't have the same offsets in both types of events */

			if (event->type == GDK_MOTION_NOTIFY) {
				canvas->pick_event.crossing.x_root = event->motion.x_root;
				canvas->pick_event.crossing.y_root = event->motion.y_root;
			} else {
				canvas->pick_event.crossing.x_root = event->button.x_root;
				canvas->pick_event.crossing.y_root = event->button.y_root;
			}
		} else
			canvas->pick_event = *event;
	}

	/* Don't do anything else if this is a recursive call */

	if (canvas->in_repick)
		return retval;

	/* LeaveNotify means that there is no current item, so we don't look for one */

	if (canvas->pick_event.type != GDK_LEAVE_NOTIFY) {
		/* these fields don't have the same offsets in both types of events */

		if (canvas->pick_event.type == GDK_ENTER_NOTIFY) {
			x = canvas->pick_event.crossing.x;
			y = canvas->pick_event.crossing.y;
		} else {
			x = canvas->pick_event.motion.x;
			y = canvas->pick_event.motion.y;
		}

		/* canvas pixel coords */

		cx = (int) (x + 0.5);
		cy = (int) (y + 0.5);

		/* world coords */
		foo_canvas_c2w (canvas, cx, cy, &x, &y);

		/* find the closest item */
		if (canvas->root->object.flags & FOO_CANVAS_ITEM_MAPPED)
			foo_canvas_item_invoke_point (canvas->root, x, y, cx, cy,
							&canvas->new_current_item);
		else
			canvas->new_current_item = NULL;
	} else
		canvas->new_current_item = NULL;

	if ((canvas->new_current_item == canvas->current_item) && !canvas->left_grabbed_item)
		return retval; /* current item did not change */

	/* Synthesize events for old and new current items */

	if ((canvas->new_current_item != canvas->current_item)
	    && (canvas->current_item != NULL)
	    && !canvas->left_grabbed_item) {
		GdkEvent new_event;
		FooCanvasItem *item;

		item = canvas->current_item;

		new_event = canvas->pick_event;
		new_event.type = GDK_LEAVE_NOTIFY;

		new_event.crossing.detail = GDK_NOTIFY_ANCESTOR;
		new_event.crossing.subwindow = NULL;
		canvas->in_repick = TRUE;
		retval = emit_event (canvas, &new_event);
		canvas->in_repick = FALSE;
	}

	/* new_current_item may have been set to NULL during the call to emit_event() above */

	if ((canvas->new_current_item != canvas->current_item) && button_down) {
		canvas->left_grabbed_item = TRUE;
		return retval;
	}

	/* Handle the rest of cases */

	canvas->left_grabbed_item = FALSE;
	canvas->current_item = canvas->new_current_item;

	if (canvas->current_item != NULL) {
		GdkEvent new_event;

		new_event = canvas->pick_event;
		new_event.type = GDK_ENTER_NOTIFY;
		new_event.crossing.detail = GDK_NOTIFY_ANCESTOR;
		new_event.crossing.subwindow = NULL;
		retval = emit_event (canvas, &new_event);
	}

	return retval;
}

/* Button event handler for the canvas */
static gint
foo_canvas_button (GtkWidget *widget, GdkEventButton *event)
{
	FooCanvas *canvas;
	int mask;
	int retval;

	g_return_val_if_fail (FOO_IS_CANVAS (widget), FALSE);
	g_return_val_if_fail (event != NULL, FALSE);

	retval = FALSE;

	canvas = FOO_CANVAS (widget);

	/*
	 * dispatch normally regardless of the event's window if an item has
	 * has a pointer grab in effect
	 */
	if (!canvas->grabbed_item && event->window != canvas->layout.bin_window)
		return retval;

	switch (event->button) {
	case 1:
		mask = GDK_BUTTON1_MASK;
		break;
	case 2:
		mask = GDK_BUTTON2_MASK;
		break;
	case 3:
		mask = GDK_BUTTON3_MASK;
		break;
	case 4:
		mask = GDK_BUTTON4_MASK;
		break;
	case 5:
		mask = GDK_BUTTON5_MASK;
		break;
	default:
		mask = 0;
	}

	switch (event->type) {
	case GDK_BUTTON_PRESS:
	case GDK_2BUTTON_PRESS:
	case GDK_3BUTTON_PRESS:
		/* Pick the current item as if the button were not pressed, and
		 * then process the event.
		 */
		canvas->state = event->state;
		pick_current_item (canvas, (GdkEvent *) event);
		canvas->state ^= mask;
		retval = emit_event (canvas, (GdkEvent *) event);
		break;

	case GDK_BUTTON_RELEASE:
		/* Process the event as if the button were pressed, then repick
		 * after the button has been released
		 */
		canvas->state = event->state;
		retval = emit_event (canvas, (GdkEvent *) event);
		event->state ^= mask;
		canvas->state = event->state;
		pick_current_item (canvas, (GdkEvent *) event);
		event->state ^= mask;
		break;

	default:
		g_assert_not_reached ();
	}

	return retval;
}

/* Motion event handler for the canvas */
static gint
foo_canvas_motion (GtkWidget *widget, GdkEventMotion *event)
{
	FooCanvas *canvas;

	g_return_val_if_fail (FOO_IS_CANVAS (widget), FALSE);
	g_return_val_if_fail (event != NULL, FALSE);

	canvas = FOO_CANVAS (widget);

	if (event->window != canvas->layout.bin_window)
		return FALSE;

	canvas->state = event->state;
	pick_current_item (canvas, (GdkEvent *) event);
	return emit_event (canvas, (GdkEvent *) event);
}

/* Key event handler for the canvas */
static gint
foo_canvas_key (GtkWidget *widget, GdkEventKey *event)
{
	FooCanvas *canvas;

	g_return_val_if_fail (FOO_IS_CANVAS (widget), FALSE);
	g_return_val_if_fail (event != NULL, FALSE);

	canvas = FOO_CANVAS (widget);

	if (emit_event (canvas, (GdkEvent *) event))
		return TRUE;
	if (event->type == GDK_KEY_RELEASE)
		return GTK_WIDGET_CLASS (canvas_parent_class)->key_release_event (widget, event);
	else
		return GTK_WIDGET_CLASS (canvas_parent_class)->key_press_event (widget, event);
}


/* Crossing event handler for the canvas */
static gint
foo_canvas_crossing (GtkWidget *widget, GdkEventCrossing *event)
{
	FooCanvas *canvas;

	g_return_val_if_fail (FOO_IS_CANVAS (widget), FALSE);
	g_return_val_if_fail (event != NULL, FALSE);

	canvas = FOO_CANVAS (widget);

	if (event->window != canvas->layout.bin_window)
		return FALSE;

	canvas->state = event->state;
	return pick_current_item (canvas, (GdkEvent *) event);
}

/* Focus in handler for the canvas */
static gint
foo_canvas_focus_in (GtkWidget *widget, GdkEventFocus *event)
{
	FooCanvas *canvas;

	GTK_WIDGET_SET_FLAGS (widget, GTK_HAS_FOCUS);

	canvas = FOO_CANVAS (widget);

	if (canvas->focused_item)
		return emit_event (canvas, (GdkEvent *) event);
	else
		return FALSE;
}

/* Focus out handler for the canvas */
static gint
foo_canvas_focus_out (GtkWidget *widget, GdkEventFocus *event)
{
	FooCanvas *canvas;

	GTK_WIDGET_UNSET_FLAGS (widget, GTK_HAS_FOCUS);

	canvas = FOO_CANVAS (widget);

	if (canvas->focused_item)
		return emit_event (canvas, (GdkEvent *) event);
	else
		return FALSE;
}

/* Expose handler for the canvas */
static gint
foo_canvas_expose (GtkWidget *widget, GdkEventExpose *event)
{
	FooCanvas *canvas;

	canvas = FOO_CANVAS (widget);

	if (!GTK_WIDGET_DRAWABLE (widget) || (event->window != canvas->layout.bin_window)) return FALSE;

#ifdef VERBOSE
	g_print ("Expose\n");
#endif
	/* If there are any outstanding items that need updating, do them now */
	if (canvas->idle_id) {
		g_source_remove (canvas->idle_id);
		canvas->idle_id = 0;
	}
	if (canvas->need_update) {
		g_return_val_if_fail (!canvas->doing_update, FALSE);

		canvas->doing_update = TRUE;
		foo_canvas_item_invoke_update (canvas->root, 0, 0, 0);

		g_return_val_if_fail (canvas->doing_update, FALSE);

		canvas->doing_update = FALSE;

		canvas->need_update = FALSE;
	}

	/* Hmmm. Would like to queue antiexposes if the update marked
	   anything that is gonna get redrawn as invalid */


	g_signal_emit (G_OBJECT (canvas), canvas_signals[DRAW_BACKGROUND], 0,
		       event->area.x, event->area.y,
		       event->area.width, event->area.height);

	if (canvas->root->object.flags & FOO_CANVAS_ITEM_MAPPED)
		(* FOO_CANVAS_ITEM_GET_CLASS (canvas->root)->draw) (canvas->root,
								      canvas->layout.bin_window,
								      event);



	/* Chain up to get exposes on child widgets */
	GTK_WIDGET_CLASS (canvas_parent_class)->expose_event (widget, event);

	return FALSE;
}

static void
foo_canvas_draw_background (FooCanvas *canvas,
			    int x, int y, int width, int height)
{
	/* By default, we use the style background. */
	gdk_gc_set_foreground (canvas->pixmap_gc,
			       &GTK_WIDGET (canvas)->style->bg[GTK_STATE_NORMAL]);
	gdk_draw_rectangle (canvas->layout.bin_window,
			    canvas->pixmap_gc,
			    TRUE,
			    x, y,
			    width, height);
}

static void
do_update (FooCanvas *canvas)
{
	/* Cause the update if necessary */

update_again:
	if (canvas->need_update) {
		g_return_if_fail (!canvas->doing_update);

		canvas->doing_update = TRUE;
		foo_canvas_item_invoke_update (canvas->root, 0, 0, 0);

		g_return_if_fail (canvas->doing_update);

		canvas->doing_update = FALSE;

		canvas->need_update = FALSE;
	}

	/* Pick new current item */

	while (canvas->need_repick) {
		canvas->need_repick = FALSE;
		pick_current_item (canvas, &canvas->pick_event);
	}

	/* it is possible that during picking we emitted an event in which
	   the user then called some function which then requested update
	   of something.  Without this we'd be left in a state where
	   need_update would have been left TRUE and the canvas would have
	   been left unpainted. */
	if (canvas->need_update) {
		goto update_again;
	}
}

/* Idle handler for the canvas.  It deals with pending updates and redraws. */
static gint
idle_handler (gpointer data)
{
	FooCanvas *canvas;

	GDK_THREADS_ENTER ();

	canvas = FOO_CANVAS (data);
	do_update (canvas);

	/* Reset idle id */
	canvas->idle_id = 0;

	GDK_THREADS_LEAVE ();

	return FALSE;
}

/* Convenience function to add an idle handler to a canvas */
static void
add_idle (FooCanvas *canvas)
{
	if (!canvas->idle_id) {
		/* We let the update idle handler have higher priority
		 * than the redraw idle handler so the canvas state
		 * will be updated during the expose event.  canvas in
		 * expose_event.
		 */
		canvas->idle_id = g_idle_add_full (GDK_PRIORITY_REDRAW - 20,
						   idle_handler, canvas, NULL);
	}
}

/**
 * foo_canvas_root:
 * @canvas: A canvas.
 *
 * Queries the root group of a canvas.
 *
 * Return value: The root group of the specified canvas.
 **/
FooCanvasGroup *
foo_canvas_root (FooCanvas *canvas)
{
	g_return_val_if_fail (FOO_IS_CANVAS (canvas), NULL);

	return FOO_CANVAS_GROUP (canvas->root);
}


/**
 * foo_canvas_set_scroll_region:
 * @canvas: A canvas.
 * @x1: Leftmost limit of the scrolling region.
 * @y1: Upper limit of the scrolling region.
 * @x2: Rightmost limit of the scrolling region.
 * @y2: Lower limit of the scrolling region.
 *
 * Sets the scrolling region of a canvas to the specified rectangle.  The canvas
 * will then be able to scroll only within this region.  The view of the canvas
 * is adjusted as appropriate to display as much of the new region as possible.
 **/
void
foo_canvas_set_scroll_region (FooCanvas *canvas, double x1, double y1, double x2, double y2)
{
	double wxofs, wyofs;
	int xofs, yofs;

	g_return_if_fail (FOO_IS_CANVAS (canvas));

	if ((canvas->scroll_x1 == x1) && (canvas->scroll_y1 == y1) &&
	    (canvas->scroll_x2 == x2) && (canvas->scroll_y2 == y2)) {
		return;
	}

	/*
	 * Set the new scrolling region.  If possible, do not move the visible contents of the
	 * canvas.
	 */

	foo_canvas_c2w (canvas,
			  GTK_LAYOUT (canvas)->hadjustment->value + canvas->zoom_xofs,
			  GTK_LAYOUT (canvas)->vadjustment->value + canvas->zoom_yofs,
			  /*canvas->zoom_xofs,
			  canvas->zoom_yofs,*/
			  &wxofs, &wyofs);

	canvas->scroll_x1 = x1;
	canvas->scroll_y1 = y1;
	canvas->scroll_x2 = x2;
	canvas->scroll_y2 = y2;

	foo_canvas_w2c (canvas, wxofs, wyofs, &xofs, &yofs);

	scroll_to (canvas, xofs, yofs, TRUE);

	canvas->need_repick = TRUE;

	if (!(canvas->root->object.flags & FOO_CANVAS_ITEM_NEED_DEEP_UPDATE)) {
		canvas->root->object.flags |= FOO_CANVAS_ITEM_NEED_DEEP_UPDATE;
		foo_canvas_request_update (canvas);
	}
}


/**
 * foo_canvas_get_scroll_region:
 * @canvas: A canvas.
 * @x1: Leftmost limit of the scrolling region (return value).
 * @y1: Upper limit of the scrolling region (return value).
 * @x2: Rightmost limit of the scrolling region (return value).
 * @y2: Lower limit of the scrolling region (return value).
 *
 * Queries the scrolling region of a canvas.
 **/
void
foo_canvas_get_scroll_region (FooCanvas *canvas, double *x1, double *y1, double *x2, double *y2)
{
	g_return_if_fail (FOO_IS_CANVAS (canvas));

	if (x1)
		*x1 = canvas->scroll_x1;

	if (y1)
		*y1 = canvas->scroll_y1;

	if (x2)
		*x2 = canvas->scroll_x2;

	if (y2)
		*y2 = canvas->scroll_y2;
}

void
foo_canvas_set_center_scroll_region (FooCanvas *canvas,
				     gboolean center_scroll_region)
{
	g_return_if_fail (FOO_IS_CANVAS (canvas));

	canvas->center_scroll_region = center_scroll_region != 0;

	scroll_to (canvas,
		   canvas->layout.hadjustment->value,
		   canvas->layout.vadjustment->value, TRUE);
}


/**
 * foo_canvas_set_pixels_per_unit:
 * @canvas: A canvas.
 * @n: The number of pixels that correspond to one canvas unit.
 *
 * Sets the zooming factor of a canvas by specifying the number of pixels that
 * correspond to one canvas unit.
 **/
void
foo_canvas_set_pixels_per_unit (FooCanvas *canvas, double n)
{
	GtkWidget *widget;
	double cx, cy;
	int x1, y1;
	int center_x, center_y;
	GdkWindow *window;
	GdkWindowAttr attributes;
	gint attributes_mask;

	g_return_if_fail (FOO_IS_CANVAS (canvas));
	g_return_if_fail (n > FOO_CANVAS_EPSILON);

	widget = GTK_WIDGET (canvas);

	center_x = widget->allocation.width / 2;
	center_y = widget->allocation.height / 2;

	/* Find the coordinates of the screen center in units. */
	cx = (canvas->layout.hadjustment->value + center_x) / canvas->pixels_per_unit + canvas->scroll_x1 + canvas->zoom_xofs;
	cy = (canvas->layout.vadjustment->value + center_y) / canvas->pixels_per_unit + canvas->scroll_y1 + canvas->zoom_yofs;

	/* Now calculate the new offset of the upper left corner. (round not truncate) */
	x1 = ((cx - canvas->scroll_x1) * n) - center_x + .5;
	y1 = ((cy - canvas->scroll_y1) * n) - center_y + .5;

	canvas->pixels_per_unit = n;

	if (!(canvas->root->object.flags & FOO_CANVAS_ITEM_NEED_DEEP_UPDATE)) {
		canvas->root->object.flags |= FOO_CANVAS_ITEM_NEED_DEEP_UPDATE;
		foo_canvas_request_update (canvas);
	}

	/* Map a background None window over the bin_window to avoid
	 * scrolling the window scroll causing exposes.
	 */
	window = NULL;
	if (GTK_WIDGET_MAPPED (widget)) {
		attributes.window_type = GDK_WINDOW_CHILD;
		attributes.x = widget->allocation.x;
		attributes.y = widget->allocation.y;
		attributes.width = widget->allocation.width;
		attributes.height = widget->allocation.height;
		attributes.wclass = GDK_INPUT_OUTPUT;
		attributes.visual = gtk_widget_get_visual (widget);
		attributes.colormap = gtk_widget_get_colormap (widget);
		attributes.event_mask = GDK_VISIBILITY_NOTIFY_MASK;

		attributes_mask = GDK_WA_X | GDK_WA_Y | GDK_WA_VISUAL | GDK_WA_COLORMAP;

		window = gdk_window_new (gtk_widget_get_parent_window (widget),
					 &attributes, attributes_mask);
		gdk_window_set_back_pixmap (window, NULL, FALSE);
		gdk_window_set_user_data (window, widget);

		gdk_window_show (window);
	}

	scroll_to (canvas, x1, y1, FALSE);

	/* If we created a an overlapping background None window, remove it how.
	 *
	 * TODO: We would like to temporarily set the bin_window background to
	 * None to avoid clearing the bin_window to the background, but gdk doesn't
	 * expose enought to let us do this, so we get a flash-effect here. At least
	 * it looks better than scroll + expose.
	 */
	if (window != NULL) {
		gdk_window_hide (window);
		gdk_window_set_user_data (window, NULL);
		gdk_window_destroy (window);
	}

	canvas->need_repick = TRUE;
}

/**
 * foo_canvas_scroll_to:
 * @canvas: A canvas.
 * @cx: Horizontal scrolling offset in canvas pixel units.
 * @cy: Vertical scrolling offset in canvas pixel units.
 *
 * Makes a canvas scroll to the specified offsets, given in canvas pixel units.
 * The canvas will adjust the view so that it is not outside the scrolling
 * region.  This function is typically not used, as it is better to hook
 * scrollbars to the canvas layout's scrolling adjusments.
 **/
void
foo_canvas_scroll_to (FooCanvas *canvas, int cx, int cy)
{
	g_return_if_fail (FOO_IS_CANVAS (canvas));

	scroll_to (canvas, cx, cy, TRUE);
}

/**
 * foo_canvas_get_scroll_offsets:
 * @canvas: A canvas.
 * @cx: Horizontal scrolling offset (return value).
 * @cy: Vertical scrolling offset (return value).
 *
 * Queries the scrolling offsets of a canvas.  The values are returned in canvas
 * pixel units.
 **/
void
foo_canvas_get_scroll_offsets (FooCanvas *canvas, int *cx, int *cy)
{
	g_return_if_fail (FOO_IS_CANVAS (canvas));

	if (cx)
		*cx = canvas->layout.hadjustment->value;

	if (cy)
		*cy = canvas->layout.vadjustment->value;
}

/**
 * foo_canvas_update_now:
 * @canvas: A canvas.
 *
 * Forces an immediate update and redraw of a canvas.  If the canvas does not
 * have any pending update or redraw requests, then no action is taken.  This is
 * typically only used by applications that need explicit control of when the
 * display is updated, like games.  It is not needed by normal applications.
 */
void
foo_canvas_update_now (FooCanvas *canvas)
{
	g_return_if_fail (FOO_IS_CANVAS (canvas));

	if (!(canvas->need_update || canvas->need_redraw))
		return;
	remove_idle (canvas);
	do_update (canvas);
}

/**
 * foo_canvas_get_item_at:
 * @canvas: A canvas.
 * @x: X position in world coordinates.
 * @y: Y position in world coordinates.
 *
 * Looks for the item that is under the specified position, which must be
 * specified in world coordinates.
 *
 * Return value: The sought item, or NULL if no item is at the specified
 * coordinates.
 **/
FooCanvasItem *
foo_canvas_get_item_at (FooCanvas *canvas, double x, double y)
{
	FooCanvasItem *item;
	double dist;
	int cx, cy;

	g_return_val_if_fail (FOO_IS_CANVAS (canvas), NULL);

	foo_canvas_w2c (canvas, x, y, &cx, &cy);

	dist = foo_canvas_item_invoke_point (canvas->root, x, y, cx, cy, &item);
	if ((int) (dist * canvas->pixels_per_unit + 0.5) <= canvas->close_enough)
		return item;
	else
		return NULL;
}

/* Queues an update of the canvas */
static void
foo_canvas_request_update (FooCanvas *canvas)
{
	FOO_CANVAS_GET_CLASS (canvas)->request_update (canvas);
}

static void
foo_canvas_request_update_real (FooCanvas *canvas)
{
	canvas->need_update = TRUE;
	add_idle (canvas);
}

/**
 * foo_canvas_request_redraw:
 * @canvas: A canvas.
 * @x1: Leftmost coordinate of the rectangle to be redrawn.
 * @y1: Upper coordinate of the rectangle to be redrawn.
 * @x2: Rightmost coordinate of the rectangle to be redrawn, plus 1.
 * @y2: Lower coordinate of the rectangle to be redrawn, plus 1.
 *
 * Convenience function that informs a canvas that the specified rectangle needs
 * to be repainted.  The rectangle includes @x1 and @y1, but not @x2 and @y2.
 * To be used only by item implementations.
 **/
void
foo_canvas_request_redraw (FooCanvas *canvas, int x1, int y1, int x2, int y2)
{
	GdkRectangle bbox;

	g_return_if_fail (FOO_IS_CANVAS (canvas));

	if (!GTK_WIDGET_DRAWABLE (canvas) || (x1 >= x2) || (y1 >= y2)) return;

	bbox.x = x1;
	bbox.y = y1;
	bbox.width = x2 - x1;
	bbox.height = y2 - y1;

	gdk_window_invalidate_rect (canvas->layout.bin_window,
				    &bbox, FALSE);
}

/**
 * foo_canvas_w2c:
 * @canvas: A canvas.
 * @wx: World X coordinate.
 * @wy: World Y coordinate.
 * @cx: X pixel coordinate (return value).
 * @cy: Y pixel coordinate (return value).
 *
 * Converts world coordinates into canvas pixel coordinates.
 **/
void
foo_canvas_w2c (FooCanvas *canvas, double wx, double wy, int *cx, int *cy)
{
	double zoom;

	g_return_if_fail (FOO_IS_CANVAS (canvas));

	zoom = canvas->pixels_per_unit;

	if (cx)
		*cx = floor ((wx - canvas->scroll_x1)*zoom + canvas->zoom_xofs + 0.5);
	if (cy)
		*cy = floor ((wy - canvas->scroll_y1)*zoom + canvas->zoom_yofs + 0.5);
}

/**
 * foo_canvas_w2c:
 * @canvas: A canvas.
 * @world: rectangle in world coordinates.
 * @canvas: rectangle in canvase coordinates.
 *
 * Converts rectangles in world coordinates into canvas pixel coordinates.
 **/
void
foo_canvas_w2c_rect_d (FooCanvas *canvas,
			 double *x1, double *y1,
			 double *x2, double *y2)
{
	foo_canvas_w2c_d (canvas,
			    *x1, *y1,
			    x1, y1);
	foo_canvas_w2c_d (canvas,
			    *x2, *y2,
			    x2, y2);
}



/**
 * foo_canvas_w2c_d:
 * @canvas: A canvas.
 * @wx: World X coordinate.
 * @wy: World Y coordinate.
 * @cx: X pixel coordinate (return value).
 * @cy: Y pixel coordinate (return value).
 *
 * Converts world coordinates into canvas pixel coordinates.  This version
 * returns coordinates in floating point coordinates, for greater precision.
 **/
void
foo_canvas_w2c_d (FooCanvas *canvas, double wx, double wy, double *cx, double *cy)
{
	double zoom;

	g_return_if_fail (FOO_IS_CANVAS (canvas));

	zoom = canvas->pixels_per_unit;

	if (cx)
		*cx = (wx - canvas->scroll_x1)*zoom + canvas->zoom_xofs;
	if (cy)
		*cy = (wy - canvas->scroll_y1)*zoom + canvas->zoom_yofs;
}


/**
 * foo_canvas_c2w:
 * @canvas: A canvas.
 * @cx: Canvas pixel X coordinate.
 * @cy: Canvas pixel Y coordinate.
 * @wx: X world coordinate (return value).
 * @wy: Y world coordinate (return value).
 *
 * Converts canvas pixel coordinates to world coordinates.
 **/
void
foo_canvas_c2w (FooCanvas *canvas, int cx, int cy, double *wx, double *wy)
{
	double zoom;

	g_return_if_fail (FOO_IS_CANVAS (canvas));

	zoom = canvas->pixels_per_unit;

	if (wx)
		*wx = (cx - canvas->zoom_xofs)/zoom + canvas->scroll_x1;
	if (wy)
		*wy = (cy - canvas->zoom_yofs)/zoom + canvas->scroll_y1;
}


/**
 * foo_canvas_window_to_world:
 * @canvas: A canvas.
 * @winx: Window-relative X coordinate.
 * @winy: Window-relative Y coordinate.
 * @worldx: X world coordinate (return value).
 * @worldy: Y world coordinate (return value).
 *
 * Converts window-relative coordinates into world coordinates.  You can use
 * this when you need to convert mouse coordinates into world coordinates, for
 * example.
 * Window coordinates are really the same as canvas coordinates now, but this
 * function is here for backwards compatibility reasons.
 **/
void
foo_canvas_window_to_world (FooCanvas *canvas, double winx, double winy,
			      double *worldx, double *worldy)
{
	g_return_if_fail (FOO_IS_CANVAS (canvas));

	if (worldx)
		*worldx = canvas->scroll_x1 + ((winx - canvas->zoom_xofs)
					       / canvas->pixels_per_unit);

	if (worldy)
		*worldy = canvas->scroll_y1 + ((winy - canvas->zoom_yofs)
					       / canvas->pixels_per_unit);
}


/**
 * foo_canvas_world_to_window:
 * @canvas: A canvas.
 * @worldx: World X coordinate.
 * @worldy: World Y coordinate.
 * @winx: X window-relative coordinate.
 * @winy: Y window-relative coordinate.
 *
 * Converts world coordinates into window-relative coordinates.
 * Window coordinates are really the same as canvas coordinates now, but this
 * function is here for backwards compatibility reasons.
 **/
void
foo_canvas_world_to_window (FooCanvas *canvas, double worldx, double worldy,
			    double *winx, double *winy)
{
	g_return_if_fail (FOO_IS_CANVAS (canvas));

	if (winx)
		*winx = (canvas->pixels_per_unit)*(worldx - canvas->scroll_x1) + canvas->zoom_xofs;

	if (winy)
		*winy = (canvas->pixels_per_unit)*(worldy - canvas->scroll_y1) + canvas->zoom_yofs;
}



/**
 * foo_canvas_get_color:
 * @canvas: A canvas.
 * @spec: X color specification, or NULL for "transparent".
 * @color: Returns the allocated color.
 *
 * Allocates a color based on the specified X color specification.  As a
 * convenience to item implementations, it returns TRUE if the color was
 * allocated, or FALSE if the specification was NULL.  A NULL color
 * specification is considered as "transparent" by the canvas.
 *
 * Return value: TRUE if @spec is non-NULL and the color is allocated.  If @spec
 * is NULL, then returns FALSE.
 **/
int
foo_canvas_get_color (FooCanvas *canvas, const char *spec, GdkColor *color)
{
	GdkColormap *colormap;

	g_return_val_if_fail (FOO_IS_CANVAS (canvas), FALSE);
	g_return_val_if_fail (color != NULL, FALSE);

	if (!spec) {
		color->pixel = 0;
		color->red = 0;
		color->green = 0;
		color->blue = 0;
		return FALSE;
	}

	gdk_color_parse (spec, color);

	colormap = gtk_widget_get_colormap (GTK_WIDGET (canvas));

	gdk_rgb_find_color (colormap, color);

	return TRUE;
}

/**
 * foo_canvas_get_color_pixel:
 * @canvas: A canvas.
 * @rgba: RGBA color specification.
 *
 * Allocates a color from the RGBA value passed into this function.  The alpha
 * opacity value is discarded, since normal X colors do not support it.
 *
 * Return value: Allocated pixel value corresponding to the specified color.
 **/
gulong
foo_canvas_get_color_pixel (FooCanvas *canvas, guint rgba)
{
	GdkColormap *colormap;
	GdkColor color;

	g_return_val_if_fail (FOO_IS_CANVAS (canvas), 0);

	color.red = ((rgba & 0xff000000) >> 16) + ((rgba & 0xff000000) >> 24);
	color.green = ((rgba & 0x00ff0000) >> 8) + ((rgba & 0x00ff0000) >> 16);
	color.blue = (rgba & 0x0000ff00) + ((rgba & 0x0000ff00) >> 8);
	color.pixel = 0;

	colormap = gtk_widget_get_colormap (GTK_WIDGET (canvas));

	gdk_rgb_find_color (colormap, &color);

	return color.pixel;
}


/* FIXME: This function is not useful anymore */
/**
 * foo_canvas_set_stipple_origin:
 * @canvas: A canvas.
 * @gc: GC on which to set the stipple origin.
 *
 * Sets the stipple origin of the specified GC as is appropriate for the canvas,
 * so that it will be aligned with other stipple patterns used by canvas items.
 * This is typically only needed by item implementations.
 **/
void
foo_canvas_set_stipple_origin (FooCanvas *canvas, GdkGC *gc)
{
	g_return_if_fail (FOO_IS_CANVAS (canvas));
	g_return_if_fail (GDK_IS_GC (gc));

	gdk_gc_set_ts_origin (gc, 0, 0);
}

static gboolean
boolean_handled_accumulator (GSignalInvocationHint *ihint,
			     GValue                *return_accu,
			     const GValue          *handler_return,
			     gpointer               dummy)
{
	gboolean continue_emission;
	gboolean signal_handled;

	signal_handled = g_value_get_boolean (handler_return);
	g_value_set_boolean (return_accu, signal_handled);
	continue_emission = !signal_handled;

	return continue_emission;
}

static guint
foo_canvas_item_accessible_add_focus_handler (AtkComponent    *component,
                                              AtkFocusHandler handler)
{
 	GSignalMatchType match_type;
	guint signal_id;

	match_type = G_SIGNAL_MATCH_ID | G_SIGNAL_MATCH_FUNC;
	signal_id = g_signal_lookup ("focus-event", ATK_TYPE_OBJECT);

	if (!g_signal_handler_find (component, match_type, signal_id, 0, NULL,
                                    (gpointer) handler, NULL)) {
		return g_signal_connect_closure_by_id (component,
                                                       signal_id, 0,
                                                       g_cclosure_new (
                                                       G_CALLBACK (handler), NULL,
                                                       (GClosureNotify) NULL),
                                                       FALSE);
	}
	return 0;
}

static void
foo_canvas_item_accessible_get_item_extents (FooCanvasItem *item,
                                             GdkRectangle  *rect)
{
 	double bx1, bx2, by1, by2;
	gint scroll_x, scroll_y;
	gint x1, x2, y1, y2;

	foo_canvas_item_get_bounds (item, &bx1, &by1, &bx2, &by2);
	foo_canvas_w2c_rect_d (item->canvas, &bx1, &by1, &bx2, &by2);
	foo_canvas_get_scroll_offsets (item->canvas, &scroll_x, &scroll_y);
	x1 = floor (bx1);
	y1 = floor (by1);
	x2 = ceil (bx2);
	y2 = ceil (by2);
	rect->x = x1 - scroll_x;
	rect->y = y1 - scroll_y;
	rect->width = x2 - x1;
	rect->height = y2 - y1;
}

static gboolean
foo_canvas_item_accessible_is_item_in_window (FooCanvasItem *item,
                                              GdkRectangle  *rect)
{
 	GtkWidget *widget;
	gboolean retval;

	widget = GTK_WIDGET (item->canvas);
	if (widget->window) {
		int window_width, window_height;

		gdk_window_get_geometry (widget->window, NULL, NULL,
                                         &window_width, &window_height, NULL);
		/*
                 * Check whether rectangles intersect
		 */
                if (rect->x + rect->width < 0 ||
                    rect->y + rect->height < 0 ||
                    rect->x > window_width  ||
                    rect->y > window_height) {
			retval = FALSE;
		} else {
                        retval = TRUE;
		}
	} else {
                retval = FALSE;
	}
        return retval;
}


static void
foo_canvas_item_accessible_get_extents (AtkComponent *component,
                                        gint		*x,
                                        gint		*y,
                                        gint		*width,
                                        gint		*height,
                                        AtkCoordType coord_type)
{
 	AtkGObjectAccessible *atk_gobj;
	GObject *obj;
	FooCanvasItem *item;
	gint window_x, window_y;
	gint toplevel_x, toplevel_y;
	GdkRectangle rect;
	GdkWindow *window;
	GtkWidget *canvas;

	atk_gobj = ATK_GOBJECT_ACCESSIBLE (component);
	obj = atk_gobject_accessible_get_object (atk_gobj);

	if (obj == NULL) {
		/* item is defunct */
		return;
	}

        /* Get the CanvasItem */
	item = FOO_CANVAS_ITEM (obj);

	/* If this item has no parent canvas, something's broken */
	g_return_if_fail (GTK_IS_WIDGET (item->canvas));

	foo_canvas_item_accessible_get_item_extents (item, &rect);
	*width = rect.width;
	*height = rect.height;
	if (!foo_canvas_item_accessible_is_item_in_window (item, &rect)) {
		*x = G_MININT;
		*y = G_MININT;
		return;
	}

        canvas = GTK_WIDGET (item->canvas);
	window = gtk_widget_get_parent_window (canvas);
	gdk_window_get_origin (window, &window_x, &window_y);
	*x = rect.x + window_x;
	*y = rect.y + window_y;
	if (coord_type == ATK_XY_WINDOW) {
		window = gdk_window_get_toplevel (canvas->window);
		gdk_window_get_origin (window, &toplevel_x, &toplevel_y);
		*x -= toplevel_x;
		*y -= toplevel_y;
	}
        return;
}

static gint
foo_canvas_item_accessible_get_mdi_zorder (AtkComponent *component)
{
	AtkGObjectAccessible *atk_gobj;
	GObject *g_obj;
	FooCanvasItem *item;

	atk_gobj = ATK_GOBJECT_ACCESSIBLE (component);
	g_obj = atk_gobject_accessible_get_object (atk_gobj);
	if (g_obj == NULL) {
		/* Object is defunct */
		return -1;
	}

	item = FOO_CANVAS_ITEM (g_obj);
	if (item->parent) {
       		return g_list_index (FOO_CANVAS_GROUP (item->parent)->item_list, item);
	} else {
		g_return_val_if_fail (item->canvas->root == item, -1);
		return 0;
	}
}

static gboolean
foo_canvas_item_accessible_grab_focus (AtkComponent *component)
{
 	AtkGObjectAccessible *atk_gobj;
	GObject *obj;
	FooCanvasItem *item;
	GtkWidget *toplevel;

	atk_gobj = ATK_GOBJECT_ACCESSIBLE (component);
	obj = atk_gobject_accessible_get_object (atk_gobj);

	item = FOO_CANVAS_ITEM (obj);
	if (item == NULL) {
		/* item is defunct */
		return FALSE;
	}

        foo_canvas_item_grab_focus (item);
	toplevel = gtk_widget_get_toplevel (GTK_WIDGET (item->canvas));
	if (GTK_WIDGET_TOPLEVEL (toplevel)) {
		gtk_window_present (GTK_WINDOW (toplevel));
	}

	return TRUE;
}

static void
foo_canvas_item_accessible_remove_focus_handler (AtkComponent *component,
                                                 guint		handler_id)
{
 	g_signal_handler_disconnect (component, handler_id);
}

static void
foo_canvas_item_accessible_component_interface_init (AtkComponentIface *iface)
{
	g_return_if_fail (iface != NULL);

	iface->add_focus_handler = foo_canvas_item_accessible_add_focus_handler;
	iface->get_extents = foo_canvas_item_accessible_get_extents;
	iface->get_mdi_zorder = foo_canvas_item_accessible_get_mdi_zorder;
	iface->grab_focus = foo_canvas_item_accessible_grab_focus;
      	iface->remove_focus_handler = foo_canvas_item_accessible_remove_focus_handler;
}

static gboolean
foo_canvas_item_accessible_is_item_on_screen (FooCanvasItem *item)
{
	GdkRectangle rect;

	foo_canvas_item_accessible_get_item_extents (item, &rect);
	return foo_canvas_item_accessible_is_item_in_window (item, &rect);
}

static void
foo_canvas_item_accessible_initialize (AtkObject *obj, gpointer data)
{
	if (ATK_OBJECT_CLASS (accessible_item_parent_class)->initialize != NULL)
		ATK_OBJECT_CLASS (accessible_item_parent_class)->initialize (obj, data);
	g_object_set_data (G_OBJECT (obj), "atk-component-layer",
			   GINT_TO_POINTER (ATK_LAYER_MDI));
}

static AtkStateSet*
foo_canvas_item_accessible_ref_state_set (AtkObject *accessible)
{
 	AtkGObjectAccessible *atk_gobj;
	GObject *obj;
 	FooCanvasItem *item;
	AtkStateSet *state_set;

	state_set = ATK_OBJECT_CLASS (accessible_item_parent_class)->ref_state_set (accessible);
	atk_gobj = ATK_GOBJECT_ACCESSIBLE (accessible);
	obj = atk_gobject_accessible_get_object (atk_gobj);

	item = FOO_CANVAS_ITEM (obj);
	if (item == NULL) {
		atk_state_set_add_state (state_set, ATK_STATE_DEFUNCT);
	} else {
                if (item->object.flags & FOO_CANVAS_ITEM_VISIBLE) {
			atk_state_set_add_state (state_set, ATK_STATE_VISIBLE);

			if (foo_canvas_item_accessible_is_item_on_screen (item)) {
  				atk_state_set_add_state (state_set, ATK_STATE_SHOWING);
       			}
		}
        	if (GTK_WIDGET_CAN_FOCUS (GTK_WIDGET (item->canvas))) {
			atk_state_set_add_state (state_set, ATK_STATE_FOCUSABLE);

			if (item->canvas->focused_item == item) {
				atk_state_set_add_state (state_set, ATK_STATE_FOCUSED);
			}
		}
	}

        return state_set;
}

static void
foo_canvas_item_accessible_class_init (AtkObjectClass *klass)
{
 	accessible_item_parent_class = g_type_class_peek_parent (klass);

	klass->initialize = foo_canvas_item_accessible_initialize;
	klass->ref_state_set = foo_canvas_item_accessible_ref_state_set;
}

static GType
foo_canvas_item_accessible_get_type (void)
{
	static GType type = 0;

	if (!type) {
		static const GInterfaceInfo atk_component_info = {
			(GInterfaceInitFunc) foo_canvas_item_accessible_component_interface_init,
                 	(GInterfaceFinalizeFunc) NULL,
			NULL
		};
		AtkObjectFactory *factory;
		GType parent_atk_type;
		GTypeQuery query;
		GTypeInfo tinfo = { 0 };

		factory = atk_registry_get_factory (atk_get_default_registry(),
						    GTK_TYPE_OBJECT);
		if (!factory) {
			return G_TYPE_INVALID;
		}
		parent_atk_type = atk_object_factory_get_accessible_type (factory);
		if (!parent_atk_type) {
			return G_TYPE_INVALID;
		}
		g_type_query (parent_atk_type, &query);
		tinfo.class_init = (GClassInitFunc) foo_canvas_item_accessible_class_init;
		tinfo.class_size = query.class_size;
		tinfo.instance_size = query.instance_size;
		type = g_type_register_static (parent_atk_type,
					       "FooCanvasItemAccessibility",
					       &tinfo, 0);

		g_type_add_interface_static (type, ATK_TYPE_COMPONENT,
					     &atk_component_info);

	}

	return type;
}

static AtkObject *
foo_canvas_item_accessible_create (GObject *for_object)
{
	GType type;
	AtkObject *accessible;
	FooCanvasItem *item;

	item = FOO_CANVAS_ITEM (for_object);
	g_return_val_if_fail (item != NULL, NULL);

	type = foo_canvas_item_accessible_get_type ();
	if (type == G_TYPE_INVALID) {
		return atk_no_op_object_new (for_object);
	}

        accessible = g_object_new (type, NULL);
	atk_object_initialize (accessible, for_object);
	return accessible;
}

static GType
foo_canvas_item_accessible_factory_get_accessible_type (void)
{
	return foo_canvas_item_accessible_get_type ();
}

static AtkObject*
foo_canvas_item_accessible_factory_create_accessible (GObject *obj)
{
	AtkObject *accessible;

	g_return_val_if_fail (G_IS_OBJECT (obj), NULL);

	accessible = foo_canvas_item_accessible_create (obj);

	return accessible;
}

static void
foo_canvas_item_accessible_factory_class_init (AtkObjectFactoryClass *klass)
{
	klass->create_accessible = foo_canvas_item_accessible_factory_create_accessible;
	klass->get_accessible_type = foo_canvas_item_accessible_factory_get_accessible_type;
}

static GType
foo_canvas_item_accessible_factory_get_type (void)
{
	static GType type = 0;

	if (!type) {
		static const GTypeInfo tinfo = {
			sizeof (AtkObjectFactoryClass),
			(GBaseInitFunc) NULL,
			(GBaseFinalizeFunc) NULL,
			(GClassInitFunc) foo_canvas_item_accessible_factory_class_init,
			NULL,		/* class_finalize */
			NULL,		/* class_data */
			sizeof (AtkObjectFactory),
			0,		/* n_preallocs */
			NULL
		};
		type = g_type_register_static (ATK_TYPE_OBJECT_FACTORY,
					       "FooCanvasItemAccessibilityFactory",
					       &tinfo, 0);
	}

	return type;
}

/* Class initialization function for FooCanvasItemClass */
static void
foo_canvas_item_class_init (FooCanvasItemClass *class)
{
	GObjectClass *gobject_class;

	gobject_class = (GObjectClass *) class;

	item_parent_class = gtk_type_class (gtk_object_get_type ());

	gobject_class->set_property = foo_canvas_item_set_property;
	gobject_class->get_property = foo_canvas_item_get_property;

	g_object_class_install_property
		(gobject_class, ITEM_PROP_PARENT,
		 g_param_spec_object ("parent", NULL, NULL,
				      FOO_TYPE_CANVAS_ITEM,
				      GSF_PARAM_STATIC | G_PARAM_READWRITE));

	g_object_class_install_property
		(gobject_class, ITEM_PROP_VISIBLE,
		 g_param_spec_boolean ("visible", NULL, NULL,
				      TRUE,
				      GSF_PARAM_STATIC | G_PARAM_READWRITE));

	item_signals[ITEM_EVENT] =
		g_signal_new ("event",
			      G_TYPE_FROM_CLASS (class),
			      G_SIGNAL_RUN_LAST,
			      G_STRUCT_OFFSET (FooCanvasItemClass, event),
			      boolean_handled_accumulator, NULL,
			      foo_canvas_marshal_BOOLEAN__BOXED,
			      G_TYPE_BOOLEAN, 1,
			      GDK_TYPE_EVENT | G_SIGNAL_TYPE_STATIC_SCOPE);

	gobject_class->dispose = foo_canvas_item_dispose;

	class->realize = foo_canvas_item_realize;
	class->unrealize = foo_canvas_item_unrealize;
	class->map = foo_canvas_item_map;
	class->unmap = foo_canvas_item_unmap;
	class->update = foo_canvas_item_update;

	atk_registry_set_factory_type (atk_get_default_registry (),
                                       FOO_TYPE_CANVAS_ITEM,
                                       foo_canvas_item_accessible_factory_get_type ());
}
