/* File import from bonoboui to gnumeric by import-bonobo.  Do not edit.  */

/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/**
 * go-dock-item-grip.c
 *
 * Author:
 *    Michael Meeks
 *
 * Copyright (C) 2002 Sun Microsystems, Inc.
 */

#include "gnumeric-config.h"
#include <glib/gi18n.h>
#include "go-a11y.h"
#include "go-dock-band.h"
#include "go-dock-item-grip.h"
#include <glib-object.h>
#include <atk/atkstateset.h>
#include <gdk/gdkkeysyms.h>
#include <gtk/gtkaccessible.h>
#include <gtk/gtkbindings.h>
#include <libgnome/gnome-macros.h>
#include <glib/gi18n.h>
#include <string.h>

#define DRAG_HANDLE_SIZE 10

enum {
	ACTIVATE,
	LAST_SIGNAL
};
static guint signals [LAST_SIGNAL];

static AtkObjectClass *a11y_parent_class = NULL;

GNOME_CLASS_BOILERPLATE (GoDockItemGrip, go_dock_item_grip,
			 GtkWidget, GTK_TYPE_WIDGET)

static gint
go_dock_item_grip_expose (GtkWidget      *widget,
			      GdkEventExpose *event)
{
	GdkRectangle *clip = &event->area;
	GdkRectangle *rect = &widget->allocation;
	GoDockItemGrip *grip = (GoDockItemGrip *) widget;
	GtkShadowType shadow = GTK_SHADOW_OUT;

	gtk_paint_handle (widget->style,
			  widget->window,
			  GTK_WIDGET_STATE (widget),
			  shadow,
			  clip, widget, "dockitem",
			  rect->x, rect->y, rect->width, rect->height,
			  grip->item->orientation);

	if (GTK_WIDGET_HAS_FOCUS (widget)) {
		gint focus_width;
		gint focus_pad;
		GdkRectangle focus;

		gtk_widget_style_get (GTK_WIDGET (widget),
				      "focus-line-width", &focus_width,
				      "focus-padding", &focus_pad,
				      NULL);

		focus = *rect;
		focus.x += widget->style->xthickness + focus_pad;
		focus.y += widget->style->ythickness + focus_pad;
		focus.width -= 2 * (widget->style->xthickness + focus_pad);
		focus.height -= 2 * (widget->style->xthickness + focus_pad);

		gtk_paint_focus (widget->style, widget->window,
				 GTK_WIDGET_STATE (widget),
				 clip, widget, "dockitem",
				 focus.x, focus.y,
				 focus.width, focus.height);
	}

	return FALSE;
}

static void
grip_item_a11y_initialize (AtkObject *accessible, gpointer widget)
{
	accessible->role = ATK_ROLE_SEPARATOR;
	atk_object_set_name (accessible, "grip");

	a11y_parent_class->initialize (accessible, widget);
}

static AtkStateSet*
grip_item_a11y_ref_state_set (AtkObject *accessible)
{
	AtkStateSet *state_set;
	GoDockItemGrip *grip;
	GtkWidget *widget;

	state_set = a11y_parent_class->ref_state_set (accessible);
	widget = GTK_ACCESSIBLE (accessible)->widget;
	if (widget == NULL)
		return state_set;

	grip = GO_DOCK_ITEM_GRIP (widget);

	if (grip == NULL)
		return state_set;

	if (grip->item->orientation == GTK_ORIENTATION_VERTICAL) {
		atk_state_set_add_state (state_set, ATK_STATE_VERTICAL);
		atk_state_set_remove_state (state_set, ATK_STATE_HORIZONTAL);
	} else {
		atk_state_set_add_state (state_set, ATK_STATE_HORIZONTAL);
		atk_state_set_remove_state (state_set, ATK_STATE_VERTICAL);
	}

	return state_set;
}

static GoDock *
get_dock (GtkWidget *widget)
{
	while (widget && !GO_IS_DOCK (widget))
		widget = widget->parent;

	return (GoDock *) widget;
}

static void
go_dock_item_grip_dock (GoDockItemGrip *grip)
{
	GoDock *dock;
	int placement;

	g_return_if_fail (GO_IS_DOCK_ITEM_GRIP (grip));

	if (!grip->item->is_floating)
		return;

	dock = get_dock (GTK_WIDGET (grip->item));
	g_return_if_fail (dock != NULL);

	go_dock_item_unfloat (grip->item);

	g_object_ref (G_OBJECT (grip->item));
	gtk_container_remove (
		GTK_CONTAINER (
			GTK_WIDGET (grip->item)->parent),
		GTK_WIDGET (grip->item));

	if (grip->item->orientation == GTK_ORIENTATION_HORIZONTAL)
		placement = GO_DOCK_TOP;
	else
		placement = GO_DOCK_LEFT;

	go_dock_add_item (
		dock, grip->item,
		placement, 2, 0, 0, TRUE);
	g_object_unref (G_OBJECT (grip->item));
}

static void
go_dock_item_grip_undock (GoDockItemGrip *grip)
{
	guint x, y;

	g_return_if_fail (GO_IS_DOCK_ITEM_GRIP (grip));

	if (grip->item->is_floating)
		return;

	gdk_window_get_position (
		GTK_WIDGET (grip)->window, &x, &y);

	go_dock_item_detach (grip->item, x, y);
}

enum {
	ACTION_DOCK,
	ACTION_UNDOCK,
	ACTION_LAST
};

static gboolean
go_dock_item_grip_do_action (AtkAction *action,
				 gint       i)
{
	GoDockItemGrip *grip;
	GtkWidget *widget;

	widget = GTK_ACCESSIBLE (action)->widget;
	if (widget == NULL)
		return FALSE;

	grip = GO_DOCK_ITEM_GRIP (widget);

	if (grip->item->behavior & GO_DOCK_ITEM_BEH_LOCKED)
		return FALSE;

	switch (i) {
	case ACTION_DOCK:
		go_dock_item_grip_dock (grip);
		break;
	case ACTION_UNDOCK:
		go_dock_item_grip_undock (grip);
		break;
	default:
		break;
	}
	return FALSE;
}

static gint
go_dock_item_grip_get_n_actions (AtkAction *action)
{
	GoDockItemGrip *grip;
	GtkWidget *widget;

	widget = GTK_ACCESSIBLE (action)->widget;
	if (widget == NULL)
		return 0;

	grip = GO_DOCK_ITEM_GRIP (widget);

	if (grip->item->behavior & GO_DOCK_ITEM_BEH_LOCKED)
		return 0;
	else
		return ACTION_LAST;
}

static void
grip_item_a11y_class_init (AtkObjectClass *klass)
{
	a11y_parent_class = g_type_class_peek_parent (klass);

	klass->initialize = grip_item_a11y_initialize;
	klass->ref_state_set = grip_item_a11y_ref_state_set;
}

static AtkObject *
go_dock_item_grip_get_accessible (GtkWidget *widget)
{
#if 0
	AtkObject *accessible;
	static GType a11y_type = 0;

	if (!a11y_type) {
		AtkActionIface action_if;

		a11y_type = go_a11y_get_derived_type_for (
			GO_TYPE_DOCK_ITEM_GRIP,
			NULL, grip_item_a11y_class_init);

		memset (&action_if, 0, sizeof (AtkActionIface));
		action_if.do_action = go_dock_item_grip_do_action;
		action_if.get_n_actions = go_dock_item_grip_get_n_actions;

		go_a11y_add_actions_interface (
			a11y_type, &action_if,
			ACTION_DOCK,   "dock",   _("Dock the toolbar"),    "<Enter>",
			ACTION_UNDOCK, "undock", _("Un dock the toolbar"), "<Enter>",
			-1);
	}

	if ((accessible = go_a11y_get_atk_object (widget)))
		return accessible;

	return go_a11y_set_atk_object_ret (
		widget, g_object_new (a11y_type, NULL));
#else
	return NULL;
#endif
}

static void
go_dock_item_grip_activate (GoDockItemGrip *grip)
{
	if (grip->item->is_floating)
		go_dock_item_grip_dock (grip);
	else
		go_dock_item_grip_undock (grip);
}

static void
go_dock_item_grip_instance_init (GoDockItemGrip *grip)
{
	GTK_WIDGET_SET_FLAGS (grip, GTK_CAN_FOCUS);
	GTK_WIDGET_SET_FLAGS (grip, GTK_NO_WINDOW);
}

static GoDockBand *
get_dock_band (GtkWidget *widget)
{
	while (widget && !GO_IS_DOCK_BAND (widget))
		widget = widget->parent;

	return (GoDockBand *) widget;
}

static gint
go_dock_item_grip_key_press_event (GtkWidget   *widget,
				       GdkEventKey *event)
{
  gboolean had_focus = GTK_WIDGET_HAS_FOCUS (widget);
  GoDockBand *band = get_dock_band (widget);
  GoDockItemGrip *grip = (GoDockItemGrip *) widget;

  if (!grip->item->is_floating && band &&
      _bonobo_dock_band_handle_key_nav (band, grip->item, event))
    {
      if (had_focus && !GTK_WIDGET_HAS_FOCUS (widget))
        gtk_widget_grab_focus (widget);
      return TRUE;
    }

  return GTK_WIDGET_CLASS (parent_class)->key_press_event (widget, event);
}

static void
go_dock_item_grip_class_init (GoDockItemGripClass *klass)
{
	GtkBindingSet  *binding_set;
	GtkWidgetClass *widget_class = (GtkWidgetClass *) klass;

	parent_class = g_type_class_peek_parent (klass);

	widget_class->expose_event = go_dock_item_grip_expose;
#if 0
	widget_class->get_accessible = go_dock_item_grip_get_accessible;
#endif
	widget_class->key_press_event = go_dock_item_grip_key_press_event;

	klass->activate = go_dock_item_grip_activate;

	binding_set = gtk_binding_set_by_class (klass);

	signals[ACTIVATE] =
		g_signal_new ("activate",
			      G_TYPE_FROM_CLASS (klass),
			      G_SIGNAL_RUN_LAST | G_SIGNAL_ACTION,
			      G_STRUCT_OFFSET (
				      GoDockItemGripClass, activate),
			      NULL, NULL,
			      g_cclosure_marshal_VOID__VOID,
			      G_TYPE_NONE, 0);
	widget_class->activate_signal = signals[ACTIVATE];

	gtk_binding_entry_add_signal (binding_set, GDK_Return, 0,
				      "activate", 0);
	gtk_binding_entry_add_signal (binding_set, GDK_KP_Enter, 0,
				      "activate", 0);
}

GtkWidget *
go_dock_item_grip_new (GoDockItem *item)
{
	GoDockItemGrip *grip = g_object_new (
		GO_TYPE_DOCK_ITEM_GRIP, NULL);

	grip->item = item;

	return GTK_WIDGET (grip);
}
