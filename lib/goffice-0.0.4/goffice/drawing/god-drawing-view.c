/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * god-drawing-view.c: MS Office Graphic Object support
 *
 * Copyright (C) 2000-2002
 *	Jody Goldberg (jody@gnome.org)
 *	Michael Meeks (mmeeks@gnu.org)
 *      Christopher James Lahey <clahey@ximian.com>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
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
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301
 * USA
 */

#include <goffice/goffice-config.h>
#include <goffice/drawing/god-drawing-view.h>
#include <gsf/gsf-impl-utils.h>
#include <goffice/drawing/god-drawing-renderer-gdk.h>

static GObjectClass *parent_class;

struct GodDrawingViewPrivate_ {
	GodDrawingRendererGdk *renderer;
};

GodDrawingView *
god_drawing_view_new (void)
{
	GodDrawingView *view;

	view = g_object_new (GOD_DRAWING_VIEW_TYPE, NULL);

	return view;
}

GodDrawing *
god_drawing_view_get_drawing  (GodDrawingView *view)
{
	return god_drawing_renderer_gdk_get_drawing (view->priv->renderer);
}

void
god_drawing_view_set_drawing  (GodDrawingView *view,
			       GodDrawing *drawing)
{
	god_drawing_renderer_gdk_set_drawing (view->priv->renderer, drawing);
	gtk_widget_queue_draw (GTK_WIDGET (view));
}

GodAnchor *
god_drawing_view_get_extents  (GodDrawingView *view)
{
	return god_drawing_renderer_gdk_get_extents (view->priv->renderer);
}

void
god_drawing_view_set_extents  (GodDrawingView *view,
					  GodAnchor *extents)
{
	god_drawing_renderer_gdk_set_extents (view->priv->renderer, extents);
	gtk_widget_queue_draw (GTK_WIDGET (view));
}

static void
god_drawing_view_init (GObject *object)
{
	GodDrawingView *view = GOD_DRAWING_VIEW (object);

	view->priv = g_new0 (GodDrawingViewPrivate, 1);
	view->priv->renderer = god_drawing_renderer_gdk_new ();
}

static void
god_drawing_view_realize (GtkWidget *widget)
{
	GodDrawingView *view = GOD_DRAWING_VIEW (widget);

	GTK_WIDGET_CLASS(parent_class)->realize (widget);

	god_drawing_renderer_gdk_set_drawable (view->priv->renderer,
					       widget->window);
	god_drawing_renderer_gdk_set_gc (view->priv->renderer,
					 widget->style->fg_gc[0]);
}

static gboolean
god_drawing_view_expose_event (GtkWidget	   *widget,
			       GdkEventExpose      *event)
{
	GodDrawingView *view = GOD_DRAWING_VIEW (widget);
	god_drawing_renderer_gdk_render (view->priv->renderer,
					 &event->area);
	return TRUE;
}

static void
god_drawing_view_dispose (GObject *object)
{
	GodDrawingView *view = GOD_DRAWING_VIEW (object);

	if (view->priv == NULL)
		return;

	if (view->priv->renderer)
		g_object_unref (view->priv->renderer);
	g_free (view->priv);
	view->priv = NULL;

	G_OBJECT_CLASS (parent_class)->dispose (object);
}

static void
god_drawing_view_class_init (GodDrawingViewClass *class)
{
	GObjectClass *object_class;
	GtkWidgetClass *widget_class;

	object_class               = (GObjectClass *) class;
	widget_class               = (GtkWidgetClass *) class;

	parent_class               = g_type_class_peek_parent (class);

	object_class->dispose      = god_drawing_view_dispose;

	widget_class->realize      = god_drawing_view_realize;
	widget_class->expose_event = god_drawing_view_expose_event;
}

GSF_CLASS (GodDrawingView, god_drawing_view,
	   god_drawing_view_class_init, god_drawing_view_init,
	   GTK_TYPE_DRAWING_AREA)
