/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * go-graph-widget.h : 
 *
 * Copyright (C) 2003-2005 Jean Brefort (jean.brefort@normalesup.org)
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Library General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301, USA.
 */

#ifndef _GO_GRAPH_WIDGET_H_
#define _GO_GRAPH_WIDGET_H_

#include <glib-object.h>
#include <gtk/gtkwidget.h>
#include <goffice/graph/gog-chart.h>
#include <goffice/graph/gog-graph.h>

G_BEGIN_DECLS

#define GO_GRAPH_WIDGET_TYPE	(go_graph_widget_get_type ())
#define GO_GRAPH_WIDGET(o)	(G_TYPE_CHECK_INSTANCE_CAST ((o), GO_GRAPH_WIDGET_TYPE, GOGraphWidget))
#define IS_GO_GRAPH_WIDGET(o)	(G_TYPE_CHECK_INSTANCE_TYPE ((o), GO_GRAPH_WIDGET_TYPE))

typedef struct _GOGraphWidget GOGraphWidget;

GType go_graph_widget_get_type (void);
GtkWidget *go_graph_widget_new (void);

GogGraph *go_graph_widget_get_graph (GOGraphWidget *widget);
GogChart *go_graph_widget_get_chart (GOGraphWidget *widget);

G_END_DECLS

#endif  /* _GO_GRAPH_WIDGET_H_ */
