/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- *//* vim: set sw=8: */
#ifndef GOD_DRAWING_VIEW_H
#define GOD_DRAWING_VIEW_H

/**
 * god-drawing-view.h: MS Office Graphic Object support
 *
 * Author:
 *    Michael Meeks (michael@ximian.com)
 *    Jody Goldberg (jody@gnome.org)
 *    Christopher James Lahey <clahey@ximian.com>
 *
 * (C) 1998-2003 Michael Meeks, Jody Goldberg, Chris Lahey
 **/

#include <glib-object.h>
#include <glib.h>
#include <goffice/drawing/god-drawing.h>
#include <goffice/drawing/god-anchor.h>
#include <gdk/gdkdrawable.h>
#include <gtk/gtkdrawingarea.h>

G_BEGIN_DECLS

#define GOD_DRAWING_VIEW_TYPE		(god_drawing_view_get_type ())
#define GOD_DRAWING_VIEW(o)		(G_TYPE_CHECK_INSTANCE_CAST ((o), GOD_DRAWING_VIEW_TYPE, GodDrawingView))
#define GOD_DRAWING_VIEW_CLASS(k)	(G_TYPE_CHECK_CLASS_CAST ((k), GOD_DRAWING_VIEW_TYPE, GodDrawingViewClass))
#define IS_GOD_DRAWING_VIEW(o)		(G_TYPE_CHECK_INSTANCE_TYPE ((o), GOD_DRAWING_VIEW_TYPE))
#define IS_GOD_DRAWING_VIEW_CLASS(k)	(G_TYPE_CHECK_CLASS_TYPE ((k), GOD_DRAWING_VIEW_TYPE))

typedef struct GodDrawingViewPrivate_ GodDrawingViewPrivate;

typedef struct {
	GtkDrawingArea parent;
	GodDrawingViewPrivate *priv;
} GodDrawingView;

typedef struct {
	GtkDrawingAreaClass parent_class;
} GodDrawingViewClass;

GType           god_drawing_view_get_type     (void);
GodDrawingView *god_drawing_view_new          (void);

/* Return value is reffed. */
GodDrawing     *god_drawing_view_get_drawing  (GodDrawingView *renderer);
void            god_drawing_view_set_drawing  (GodDrawingView *renderer,
					       GodDrawing     *drawing);

/* Return value is reffed. */
GodAnchor      *god_drawing_view_get_extents  (GodDrawingView *renderer);
void            god_drawing_view_set_extents  (GodDrawingView *renderer,
					       GodAnchor      *extents);

G_END_DECLS

#endif /* GOD_DRAWING_VIEW_H */
