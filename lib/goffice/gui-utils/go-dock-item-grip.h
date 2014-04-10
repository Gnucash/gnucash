/* File import from bonoboui to gnumeric by import-bonobo.  Do not edit.  */

/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/**
 * go-dock-item-grip.h
 *
 * Author:
 *    Michael Meeks
 *
 * Copyright (C) 2002 Sun Microsystems, Inc.
 */

#ifndef _GO_DOCK_ITEM_GRIP_H_
#define _GO_DOCK_ITEM_GRIP_H_

#include <gtk/gtkwidget.h>
#include <goffice/gui-utils/go-dock-item.h>

G_BEGIN_DECLS

#define GO_TYPE_DOCK_ITEM_GRIP            (go_dock_item_grip_get_type())
#define GO_DOCK_ITEM_GRIP(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GO_TYPE_DOCK_ITEM_GRIP, GoDockItemGrip))
#define GO_DOCK_ITEM_GRIP_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GO_TYPE_DOCK_ITEM_GRIP, GoDockItemGripClass))
#define GO_IS_DOCK_ITEM_GRIP(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GO_TYPE_DOCK_ITEM_GRIP))
#define GO_IS_DOCK_ITEM_GRIP_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GO_TYPE_DOCK_ITEM_GRIP))
#define GO_DOCK_ITEM_GRIP_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GO_TYPE_DOCK_ITEM_GRIP, GoDockItemGripClass))

typedef struct {
	GtkWidget parent;

	GoDockItem *item;
} GoDockItemGrip;

typedef struct {
	GtkWidgetClass parent_class;

	void (*activate) (GoDockItemGrip *grip);
} GoDockItemGripClass;

GType      go_dock_item_grip_get_type (void);
GtkWidget *go_dock_item_grip_new      (GoDockItem *item);

G_END_DECLS

#endif /* _GO_DOCK_ITEM_GRIP_H_ */
