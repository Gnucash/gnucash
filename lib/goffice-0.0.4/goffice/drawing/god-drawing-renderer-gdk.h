/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- *//* vim: set sw=8: */
#ifndef GOD_DRAWING_RENDERER_GDK_H
#define GOD_DRAWING_RENDERER_GDK_H

/**
 * god-drawing-renderer-gdk.h: MS Office Graphic Object support
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

G_BEGIN_DECLS

#define GOD_DRAWING_RENDERER_GDK_TYPE		(god_drawing_renderer_gdk_get_type ())
#define GOD_DRAWING_RENDERER_GDK(o)		(G_TYPE_CHECK_INSTANCE_CAST ((o), GOD_DRAWING_RENDERER_GDK_TYPE, GodDrawingRendererGdk))
#define GOD_DRAWING_RENDERER_GDK_CLASS(k)	(G_TYPE_CHECK_CLASS_CAST ((k), GOD_DRAWING_RENDERER_GDK_TYPE, GodDrawingRendererGdkClass))
#define IS_GOD_DRAWING_RENDERER_GDK(o)		(G_TYPE_CHECK_INSTANCE_TYPE ((o), GOD_DRAWING_RENDERER_GDK_TYPE))
#define IS_GOD_DRAWING_RENDERER_GDK_CLASS(k)	(G_TYPE_CHECK_CLASS_TYPE ((k), GOD_DRAWING_RENDERER_GDK_TYPE))

typedef struct GodDrawingRendererGdkPrivate_ GodDrawingRendererGdkPrivate;

typedef struct {
	GObject parent;
	GodDrawingRendererGdkPrivate *priv;
} GodDrawingRendererGdk;

typedef struct {
	GObjectClass parent_class;
} GodDrawingRendererGdkClass;

GType                  god_drawing_renderer_gdk_get_type      (void);
GodDrawingRendererGdk *god_drawing_renderer_gdk_new           (void);

/* Return value is reffed. */
GodDrawing            *god_drawing_renderer_gdk_get_drawing   (GodDrawingRendererGdk *renderer);
void                   god_drawing_renderer_gdk_set_drawing   (GodDrawingRendererGdk *renderer,
							       GodDrawing            *drawing);

/* Return value is reffed. */
GdkDrawable           *god_drawing_renderer_gdk_get_drawable  (GodDrawingRendererGdk *renderer);
void                   god_drawing_renderer_gdk_set_drawable  (GodDrawingRendererGdk *renderer,
							       GdkDrawable           *drawable);

/* Return value is reffed. */
GdkGC                 *god_drawing_renderer_gdk_get_gc        (GodDrawingRendererGdk *renderer);
void                   god_drawing_renderer_gdk_set_gc        (GodDrawingRendererGdk *renderer,
							       GdkGC                 *gc);

/* Return value is reffed. */
GodAnchor             *god_drawing_renderer_gdk_get_extents   (GodDrawingRendererGdk *renderer);
void                   god_drawing_renderer_gdk_set_extents   (GodDrawingRendererGdk *renderer,
							       GodAnchor             *extents);
void                   god_drawing_renderer_gdk_render        (GodDrawingRendererGdk *renderer,
							       GdkRectangle          *area);

G_END_DECLS

#endif /* GOD_DRAWING_RENDERER_GDK_H */
