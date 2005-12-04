/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gnm-combo-box.c - a customizable combobox
 * Copyright 2000, 2001, Ximian, Inc.
 *
 * Authors:
 *   Miguel de Icaza (miguel@gnu.org)
 *   Adrian E Feiguin (feiguin@ifir.edu.ar)
 *   Paolo Molnaro (lupus@debian.org).
 *   Jon K Hellan (hellan@acm.org)
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License, version 2, as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301
 * USA.
 */

#include <goffice/goffice-config.h>
#include "go-combo-box.h"
#include <goffice/utils/go-marshalers.h>

#include <gtk/gtktogglebutton.h>
#include <gtk/gtktearoffmenuitem.h>
#include <gtk/gtkwindow.h>
#include <gtk/gtkmain.h>
#include <gtk/gtkarrow.h>
#include <gtk/gtkeventbox.h>
#include <gtk/gtkvbox.h>
#include <gtk/gtkframe.h>
#include <gdk/gdkkeysyms.h>
#include <glib/gi18n.h>

#include <gsf/gsf-impl-utils.h>

enum {
	POP_DOWN_DONE,
	LAST_SIGNAL
};

struct _GOComboBoxPrivate {
	GtkWidget *popdown_container;
	GtkWidget *popdown_focus;	/* Popup's toplevel when not torn off */
	GtkWidget *display_widget;

	/* Internal widgets used to implement the ComboBox */
	GtkWidget *frame;
	GtkWidget *arrow_button;

	GtkWidget *toplevel;		/* Popup's toplevel when not torn off */
	GtkWidget *tearoff_window;	/* Popup's toplevel when torn off */
	gboolean torn_off;

	GtkWidget *tearable;	/* The tearoff "button" */
	GtkWidget *popup;	/* Popup */

	gboolean   updating_buttons;
};
static GObjectClass *go_combo_box_parent_class;
static guint go_combo_box_signals [LAST_SIGNAL] = { 0, };

static void go_combo_set_tearoff_state (GOComboBox *combo, gboolean torn_off);

/**
 * go_combo_popup_reparent
 * @popup:       Popup
 * @new_parent:  New parent
 * @unrealize:   Unrealize popup if TRUE.
 *
 * Reparent the popup, taking care of the refcounting
 *
 * Compare with gtk_menu_reparent in gtk/gtkmenu.c
 */
static void
go_combo_popup_reparent (GtkWidget *popup,
			  GtkWidget *new_parent,
			  gboolean unrealize)
{
	GtkObject *object = GTK_OBJECT (popup);
	gboolean was_floating = GTK_OBJECT_FLOATING (object);

	g_object_ref (object);
	gtk_object_sink (object);

	if (unrealize) {
		g_object_ref (object);
		gtk_container_remove (GTK_CONTAINER (popup->parent), popup);
		gtk_container_add (GTK_CONTAINER (new_parent), popup);
		g_object_unref (object);
	}
	else
		gtk_widget_reparent (GTK_WIDGET (popup), new_parent);
	gtk_widget_set_size_request (new_parent, -1, -1);

	if (was_floating)
		GTK_OBJECT_SET_FLAGS (object, GTK_FLOATING);
	else
		g_object_unref (object);
}

static void
go_combo_box_finalize (GObject *object)
{
	GOComboBox *combo_box = GO_COMBO_BOX (object);

	g_free (combo_box->priv);

	go_combo_box_parent_class->finalize (object);
}

static void
go_combo_box_destroy (GtkObject *object)
{
	GtkObjectClass *klass = (GtkObjectClass *)go_combo_box_parent_class;
	GOComboBox *combo_box = GO_COMBO_BOX (object);

	if (combo_box->priv->toplevel) {
		gtk_widget_destroy (combo_box->priv->toplevel);
		g_object_unref (combo_box->priv->toplevel);
		combo_box->priv->toplevel = NULL;
	}

	if (combo_box->priv->tearoff_window) {
		gtk_widget_destroy (combo_box->priv->tearoff_window);
		g_object_unref (combo_box->priv->tearoff_window);
		combo_box->priv->tearoff_window = NULL;
	}

	if (klass->destroy)
                klass->destroy (object);
}

/* Cut and paste from gtkwindow.c */
static void
do_focus_change (GtkWidget *widget, gboolean in)
{
	GdkEventFocus fevent;

	g_object_ref (widget);

	if (in)
		GTK_WIDGET_SET_FLAGS (widget, GTK_HAS_FOCUS);
	else
		GTK_WIDGET_UNSET_FLAGS (widget, GTK_HAS_FOCUS);

	fevent.type = GDK_FOCUS_CHANGE;
	fevent.window = widget->window;
	fevent.in = in;

	gtk_widget_event (widget, (GdkEvent *)&fevent);

	g_object_notify (G_OBJECT (widget), "has-focus");

	g_object_unref (widget);
}

static void
set_arrow_state (GOComboBox *combo_box, gboolean state)
{
	GOComboBoxPrivate *priv = combo_box->priv;
	g_return_if_fail (!combo_box->priv->updating_buttons);

	combo_box->priv->updating_buttons = TRUE;
	gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (priv->arrow_button), state);
	if (GTK_IS_TOGGLE_BUTTON (priv->display_widget))
		gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (priv->display_widget), state);
	combo_box->priv->updating_buttons = FALSE;
}

static void
go_combo_box_popup_hide_unconditional (GOComboBox *combo_box)
{
	gboolean popup_info_destroyed = FALSE;

	g_return_if_fail (combo_box != NULL);
	g_return_if_fail (IS_GO_COMBO_BOX (combo_box));

	gtk_widget_hide (combo_box->priv->toplevel);
	gtk_widget_hide (combo_box->priv->popup);
	if (combo_box->priv->torn_off) {
		GTK_TEAROFF_MENU_ITEM (combo_box->priv->tearable)->torn_off
			= FALSE;
		go_combo_set_tearoff_state (combo_box, FALSE);
	}

	do_focus_change (combo_box->priv->toplevel, FALSE);
	gtk_grab_remove (combo_box->priv->toplevel);
	gdk_display_pointer_ungrab (gtk_widget_get_display (combo_box->priv->toplevel),
				    GDK_CURRENT_TIME);

	g_object_ref (combo_box->priv->popdown_container);
	g_signal_emit (combo_box,
		       go_combo_box_signals [POP_DOWN_DONE], 0,
		       combo_box->priv->popdown_container, &popup_info_destroyed);

	if (popup_info_destroyed){
		gtk_container_remove (
			GTK_CONTAINER (combo_box->priv->frame),
			combo_box->priv->popdown_container);
		combo_box->priv->popdown_container = NULL;
	}
	g_object_unref (combo_box->priv->popdown_container);
	set_arrow_state (combo_box, FALSE);
}

static gboolean
cb_arrow_pressed (GOComboBox *combo_box)
{
	if (!combo_box->priv->updating_buttons) {
		if (combo_box->priv->toplevel == NULL ||
		    !GTK_WIDGET_VISIBLE (combo_box->priv->toplevel))
			go_combo_box_popup_display (combo_box);
		else
			go_combo_box_popup_hide_unconditional (combo_box);
	}

	return TRUE;
}

static gboolean
go_combo_box_mnemonic_activate (GtkWidget *w, gboolean group_cycling)
{
	GOComboBox *combo_box = GO_COMBO_BOX (w);
	cb_arrow_pressed (combo_box);
	return TRUE;
}

static void
go_combo_box_style_set (GtkWidget *widget,
			G_GNUC_UNUSED GtkStyle *prev_style)
{
	gboolean add_tearoffs;
	gtk_widget_style_get (widget,
			      "add-tearoffs", &add_tearoffs,
			      NULL);
	go_combo_box_set_tearable (GO_COMBO_BOX (widget), add_tearoffs);
}

static void
go_combo_box_class_init (GObjectClass *object_class)
{
	GtkWidgetClass *widget_class = (GtkWidgetClass *)object_class;
	go_combo_box_parent_class = g_type_class_peek_parent (object_class);

	object_class->finalize = go_combo_box_finalize;
	widget_class->mnemonic_activate = go_combo_box_mnemonic_activate;
	((GtkObjectClass *)object_class)->destroy = go_combo_box_destroy;

	gtk_widget_class_install_style_property
		(widget_class,
		 g_param_spec_boolean ("add-tearoffs",
				       _("Add tearoffs to menus"),
				       _("Whether dropdowns should have a tearoff menu item"),
				       FALSE,
				       GSF_PARAM_STATIC | 
				       G_PARAM_READABLE));
	widget_class->style_set = go_combo_box_style_set;

	go_combo_box_signals [POP_DOWN_DONE] = g_signal_new (
		"pop_down_done",
		G_OBJECT_CLASS_TYPE (object_class),
		G_SIGNAL_RUN_LAST,
		G_STRUCT_OFFSET (GOComboBoxClass, pop_down_done),
		NULL, NULL,
		go__BOOLEAN__OBJECT,
		G_TYPE_BOOLEAN, 1, G_TYPE_OBJECT);
}

gboolean
_go_combo_is_updating (GOComboBox const *combo_box)
{
	return combo_box->priv->updating_buttons;
}

static  gint
cb_combo_keypress (GtkWidget *widget, GdkEventKey *event,
		   GOComboBox *combo_box)
{
	if (event->keyval == GDK_Escape) {
		go_combo_box_popup_hide_unconditional (combo_box);
		return TRUE;
	} else
		return FALSE;
}

/**
 * go_combo_popup_tear_off
 * @combo:         Combo box
 * @set_position:  Set to position of popup shell if true
 *
 * Tear off the popup
 *
 * FIXME:
 * Gtk popup menus are toplevel windows, not dialogs. I think this is wrong,
 * and make the popups dialogs. But may be there should be a way to make
 * them toplevel. We can do this after creating:
 * GTK_WINDOW (tearoff)->type = GTK_WINDOW_TOPLEVEL;
 */
static void
go_combo_popup_tear_off (GOComboBox *combo, gboolean set_position)
{
	int x, y;

	if (!combo->priv->tearoff_window) {
		GtkWidget *tearoff;
		const gchar *title;

		/* FIXME: made this a toplevel, not a dialog ! */
		tearoff = gtk_window_new (GTK_WINDOW_TOPLEVEL);
		gtk_widget_ref (tearoff);
		gtk_object_sink (GTK_OBJECT (tearoff));
		combo->priv->tearoff_window = tearoff;
		gtk_widget_set_app_paintable (tearoff, TRUE);
		g_signal_connect (tearoff, "key_press_event",
				  G_CALLBACK (cb_combo_keypress),
				  combo);
		gtk_widget_realize (tearoff);
		title = go_combo_box_get_title (combo);
		if (title)
			gdk_window_set_title (tearoff->window, title);
		g_object_set (G_OBJECT (tearoff),
			"allow-shrink",	FALSE,
			"allow-grow",	TRUE,
			NULL);
		gtk_window_set_transient_for
			(GTK_WINDOW (tearoff),
			 GTK_WINDOW (gtk_widget_get_toplevel
				     GTK_WIDGET (combo)));
	}

	if (GTK_WIDGET_VISIBLE (combo->priv->popup)) {
		gtk_widget_hide (combo->priv->toplevel);

		gtk_grab_remove (combo->priv->toplevel);
		gdk_display_pointer_ungrab (gtk_widget_get_display (combo->priv->toplevel),
					    GDK_CURRENT_TIME);
	}

	go_combo_popup_reparent (combo->priv->popup,
				  combo->priv->tearoff_window, FALSE);

	/* It may have got confused about size */
	gtk_widget_queue_resize (GTK_WIDGET (combo->priv->popup));

	if (set_position) {
		go_combo_box_get_pos (combo, &x, &y);
		gtk_window_move (GTK_WINDOW (combo->priv->tearoff_window), x, y);
	}
	gtk_widget_show (GTK_WIDGET (combo->priv->popup));
	gtk_widget_show (combo->priv->tearoff_window);

}

/**
 * go_combo_box_popup_hide:
 * @combo_box:  Combo box
 *
 * Hide popup, but not when it is torn off.
 * This is the external interface - for subclasses and apps which expect a
 * regular combo which doesn't do tearoffs.
 */
/* protected */ void
go_combo_box_popup_hide (GOComboBox *combo_box)
{
	if (!combo_box->priv->torn_off)
		go_combo_box_popup_hide_unconditional (combo_box);
	else if (GTK_WIDGET_VISIBLE (combo_box->priv->toplevel)) {
		/* Both popup and tearoff window present. Get rid of just
                   the popup shell. */
		go_combo_popup_tear_off (combo_box, FALSE);
		set_arrow_state (combo_box, FALSE);
	}
}

/*
 * Find best location for displaying
 */
/* protected */ void
go_combo_box_get_pos (GOComboBox *combo_box, int *x, int *y)
{
	GtkWidget *wcombo = GTK_WIDGET (combo_box);
	GdkScreen *screen = gtk_widget_get_screen (wcombo);
	int ph, pw;

	gdk_window_get_origin (wcombo->window, x, y);
	*y += wcombo->allocation.height + wcombo->allocation.y;
	*x += wcombo->allocation.x;

	ph = combo_box->priv->popup->allocation.height;
	pw = combo_box->priv->popup->allocation.width;

	if ((*y + ph) > gdk_screen_get_height (screen))
		*y = gdk_screen_get_height (screen) - ph;

	if ((*x + pw) > gdk_screen_get_width (screen))
		*x = gdk_screen_get_width (screen) - pw;
}

/**
 * go_combo_tearoff_bg_copy
 * @combo_box:  Combo box
 *
 * Copy popup window image to the tearoff window.
 */
static void
go_combo_tearoff_bg_copy (GOComboBox *combo)
{
	GdkPixmap *pixmap;
	GdkGC *gc;
	GdkGCValues gc_values;

	GtkWidget *widget = combo->priv->popup;

	if (combo->priv->torn_off) {
		gc_values.subwindow_mode = GDK_INCLUDE_INFERIORS;
		gc = gdk_gc_new_with_values (widget->window,
					     &gc_values, GDK_GC_SUBWINDOW);

		pixmap = gdk_pixmap_new (widget->window,
					 widget->allocation.width,
					 widget->allocation.height,
					 -1);

		gdk_draw_drawable (pixmap, gc,
				 widget->window,
				 0, 0, 0, 0, -1, -1);
		g_object_unref (gc);

		gtk_widget_set_size_request (combo->priv->tearoff_window,
				      widget->allocation.width,
				      widget->allocation.height);

		gdk_window_set_back_pixmap
			(combo->priv->tearoff_window->window, pixmap, FALSE);
		g_object_unref (pixmap);
	}
}

/* protected */ void
go_combo_box_popup_display (GOComboBox *combo_box)
{
	int x, y;

	g_return_if_fail (GO_COMBO_BOX (combo_box) != NULL);
	g_return_if_fail (combo_box->priv->popdown_container != NULL);

	if (combo_box->priv->torn_off) {
		/* To give the illusion that tearoff still displays the
		 * popup, we copy the image in the popup window to the
		 * background. Thus, it won't be blank after reparenting */
		go_combo_tearoff_bg_copy (combo_box);

		/* We force an unrealize here so that we don't trigger
		 * redrawing/ clearing code - we just want to reveal our
		 * backing pixmap.
		 */
		go_combo_popup_reparent (combo_box->priv->popup,
					  combo_box->priv->toplevel, TRUE);
	}

	go_combo_box_get_pos (combo_box, &x, &y);

	gtk_window_move (GTK_WINDOW (combo_box->priv->toplevel), x, y);
	gtk_widget_realize (combo_box->priv->popup);
	gtk_widget_show (combo_box->priv->popup);
	gtk_widget_realize (combo_box->priv->toplevel);
	gtk_widget_show (combo_box->priv->toplevel);

	gtk_widget_grab_focus (combo_box->priv->toplevel);
	do_focus_change (combo_box->priv->toplevel, TRUE);

	gtk_grab_add (combo_box->priv->toplevel);
	gdk_pointer_grab (combo_box->priv->toplevel->window, TRUE,
			  GDK_BUTTON_PRESS_MASK |
			  GDK_BUTTON_RELEASE_MASK |
			  GDK_POINTER_MOTION_MASK,
			  NULL, NULL, GDK_CURRENT_TIME);
	set_arrow_state (combo_box, TRUE);
}

static  gint
go_combo_box_button_press (GtkWidget *widget, GdkEventButton *event, GOComboBox *combo_box)
{
	GtkWidget *child = gtk_get_event_widget ((GdkEvent *) event);
	if (child != widget){
		while (child){
			if (child == widget)
				return FALSE;
			child = child->parent;
		}
	}

	go_combo_box_popup_hide (combo_box);
	return TRUE;
}

static void
cb_state_change (GtkWidget *widget, GtkStateType old_state, GOComboBox *combo_box)
{
	GtkStateType const new_state = GTK_WIDGET_STATE(widget);
	gtk_widget_set_state (combo_box->priv->display_widget, new_state);
}

static void
go_combo_box_init (GOComboBox *combo_box)
{
	GtkWidget *arrow;
	GdkCursor *cursor;

	combo_box->priv = g_new0 (GOComboBoxPrivate, 1);
	combo_box->priv->updating_buttons = FALSE;

	combo_box->priv->arrow_button = gtk_toggle_button_new ();
	gtk_button_set_relief (GTK_BUTTON (combo_box->priv->arrow_button), GTK_RELIEF_NONE);
	GTK_WIDGET_UNSET_FLAGS (combo_box->priv->arrow_button, GTK_CAN_FOCUS);

	arrow = gtk_arrow_new (GTK_ARROW_DOWN, GTK_SHADOW_IN);
	gtk_container_add (GTK_CONTAINER (combo_box->priv->arrow_button), arrow);
	gtk_box_pack_end (GTK_BOX (combo_box), combo_box->priv->arrow_button, FALSE, FALSE, 0);
	g_signal_connect_swapped (combo_box->priv->arrow_button,
		"button-press-event",
		G_CALLBACK (cb_arrow_pressed), combo_box);
	gtk_widget_show_all (combo_box->priv->arrow_button);

	/*
	 * prelight the display widget when mousing over the arrow.
	 */
	g_signal_connect (combo_box->priv->arrow_button, "state-changed",
			  G_CALLBACK (cb_state_change), combo_box);

	/*
	 * The pop-down container
	 */

	combo_box->priv->toplevel = gtk_window_new (GTK_WINDOW_POPUP);
	gtk_widget_ref (combo_box->priv->toplevel);
	gtk_object_sink (GTK_OBJECT (combo_box->priv->toplevel));
	g_object_set (G_OBJECT (combo_box->priv->toplevel),
		"allow-shrink",	FALSE,
		"allow-grow",	TRUE,
		NULL);

	combo_box->priv->popup = gtk_event_box_new ();
	gtk_container_add (GTK_CONTAINER (combo_box->priv->toplevel),
			   combo_box->priv->popup);
	gtk_widget_show (combo_box->priv->popup);

	gtk_widget_realize (combo_box->priv->popup);
	cursor = gdk_cursor_new_for_display (gtk_widget_get_display (GTK_WIDGET (combo_box)), GDK_TOP_LEFT_ARROW);
	gdk_window_set_cursor (combo_box->priv->popup->window, cursor);
	gdk_cursor_unref (cursor);

	combo_box->priv->torn_off = FALSE;
	combo_box->priv->tearoff_window = NULL;

	combo_box->priv->frame = gtk_frame_new (NULL);
	gtk_container_add (GTK_CONTAINER (combo_box->priv->popup),
			   combo_box->priv->frame);
	gtk_frame_set_shadow_type (GTK_FRAME (combo_box->priv->frame), GTK_SHADOW_OUT);

	g_signal_connect (combo_box->priv->toplevel, "button_press_event",
			  G_CALLBACK (go_combo_box_button_press), combo_box);
	g_signal_connect (combo_box->priv->toplevel, "key_press_event",
			  G_CALLBACK (cb_combo_keypress), combo_box);
}

GSF_CLASS (GOComboBox, go_combo_box,
	   go_combo_box_class_init, go_combo_box_init,
	   GTK_TYPE_HBOX)

/**
 * go_combo_box_set_display:
 * @combo_box: the Combo Box to modify
 * @display_widget: The widget to be displayed

 * Sets the displayed widget for the @combo_box to be @display_widget
 */
/* protected */ void
go_combo_box_set_display (GOComboBox *combo_box, GtkWidget *display_widget)
{
	g_return_if_fail (IS_GO_COMBO_BOX (combo_box));
	g_return_if_fail (GTK_IS_WIDGET (display_widget));

	if (combo_box->priv->display_widget != NULL &&
	    combo_box->priv->display_widget != display_widget)
		gtk_container_remove (GTK_CONTAINER (combo_box),
				      combo_box->priv->display_widget);

	combo_box->priv->display_widget = display_widget;

	gtk_box_pack_start (GTK_BOX (combo_box), display_widget, TRUE, TRUE, 0);
}

static gboolean
cb_tearable_enter_leave (GtkWidget *w, GdkEventCrossing *event, gpointer data)
{
	gboolean const flag = GPOINTER_TO_INT(data);
	gtk_widget_set_state (w, flag ? GTK_STATE_PRELIGHT : GTK_STATE_NORMAL);
	return FALSE;
}

/**
 * go_combo_set_tearoff_state
 * @combo_box:  Combo box
 * @torn_off:   TRUE: Tear off. FALSE: Pop down and reattach
 *
 * Set the tearoff state of the popup
 *
 * Compare with gtk_menu_set_tearoff_state in gtk/gtkmenu.c
 */
static void
go_combo_set_tearoff_state (GOComboBox *combo,
			     gboolean  torn_off)
{
	g_return_if_fail (combo != NULL);
	g_return_if_fail (IS_GO_COMBO_BOX (combo));

	if (combo->priv->torn_off != torn_off) {
		combo->priv->torn_off = torn_off;

		if (combo->priv->torn_off) {
			go_combo_popup_tear_off (combo, TRUE);
			set_arrow_state (combo, FALSE);
		} else {
			gtk_widget_hide (combo->priv->tearoff_window);
			go_combo_popup_reparent (combo->priv->popup,
						  combo->priv->toplevel,
						  FALSE);
		}
	}
}

static gboolean
cb_popup_delete (GOComboBox *combo)
{
	go_combo_box_popup_hide_unconditional (combo);
	return TRUE;
}

static gboolean
cb_tearable_button_release (GtkWidget *w, GdkEventButton *event,
			    GOComboBox *combo)
{
	GtkTearoffMenuItem *tearable;

	g_return_val_if_fail (w != NULL, FALSE);
	g_return_val_if_fail (GTK_IS_TEAROFF_MENU_ITEM (w), FALSE);

	tearable = GTK_TEAROFF_MENU_ITEM (w);
	tearable->torn_off = !tearable->torn_off;

	if (!combo->priv->torn_off) {
		gboolean need_connect;

		need_connect = (!combo->priv->tearoff_window);
		go_combo_set_tearoff_state (combo, TRUE);
		if (need_connect)
			g_signal_connect_swapped (combo->priv->tearoff_window,
				"delete_event",
				G_CALLBACK (cb_popup_delete), combo);
	} else
		go_combo_box_popup_hide_unconditional (combo);

	return TRUE;
}

void
go_combo_box_construct (GOComboBox *combo,
			GtkWidget *display_widget,
			GtkWidget *popdown_container,
			GtkWidget *popdown_focus)
{
	GtkWidget *tearable;
	GtkWidget *vbox;

	g_return_if_fail (IS_GO_COMBO_BOX (combo));
	g_return_if_fail (GTK_IS_WIDGET (display_widget));

	GTK_BOX (combo)->spacing = 0;
	GTK_BOX (combo)->homogeneous = FALSE;

	combo->priv->popdown_container = popdown_container;
	combo->priv->display_widget = NULL;

	vbox = gtk_vbox_new (FALSE, 5);
	tearable = gtk_tearoff_menu_item_new ();
	g_signal_connect (tearable, "enter-notify-event",
			  G_CALLBACK (cb_tearable_enter_leave),
			  GINT_TO_POINTER (TRUE));
	g_signal_connect (tearable, "leave-notify-event",
			  G_CALLBACK (cb_tearable_enter_leave),
			  GINT_TO_POINTER (FALSE));
	g_signal_connect (tearable, "button-release-event",
			  G_CALLBACK (cb_tearable_button_release),
			  (gpointer) combo);
	gtk_box_pack_start (GTK_BOX (vbox), tearable, FALSE, FALSE, 0);
	gtk_box_pack_start (GTK_BOX (vbox), popdown_container, TRUE, TRUE, 0);
	combo->priv->tearable = tearable;
	g_object_set (tearable, "no-show-all", TRUE, NULL);

	go_combo_box_set_tearable (combo, FALSE);

	go_combo_box_set_relief (combo, GTK_RELIEF_NORMAL);

	go_combo_box_set_display (combo, display_widget);
	gtk_container_add (GTK_CONTAINER (combo->priv->frame), vbox);
	gtk_widget_show_all (combo->priv->frame);
}

/**
 * go_combo_box_set_title
 * @combo: Combo box
 * @title: Title
 *
 * Set a title to display over the tearoff window.
 *
 * FIXME:
 *
 * This should really change the title even when the popup is already torn off.
 * I guess the tearoff window could attach a listener to title change or
 * something. But I don't think we need the functionality, so I didn't bother
 * to investigate.
 *
 * MW: Just make it a property.
 */
void
go_combo_box_set_title (GOComboBox *combo, char const *title)
{
	GOComboBoxClass *klass = G_TYPE_INSTANCE_GET_CLASS (combo,
		GO_COMBO_BOX_TYPE, GOComboBoxClass);

	g_return_if_fail (klass != NULL);

	g_object_set_data_full (G_OBJECT (combo), "go-combo-title",
		g_strdup (title), (GDestroyNotify) g_free);

	if (klass->set_title)
		(klass->set_title) (combo, title);
}

char const *
go_combo_box_get_title (GOComboBox *combo)
{
	return g_object_get_data (G_OBJECT (combo), "go-combo-title");
}

/**
 * go_combo_box_set_tearable:
 * @combo: Combo box
 * @tearable: whether to allow the @combo to be tearable
 *
 * controls whether the combo box's pop up widget can be torn off.
 */
void
go_combo_box_set_tearable (GOComboBox *combo, gboolean tearable)
{
	g_return_if_fail (IS_GO_COMBO_BOX (combo));

	if (tearable){
		gtk_widget_show (combo->priv->tearable);
	} else {
		go_combo_set_tearoff_state (combo, FALSE);
		gtk_widget_hide (combo->priv->tearable);
	}
}

void
go_combo_box_set_relief (GOComboBox *combo, GtkReliefStyle relief)
{
	g_return_if_fail (IS_GO_COMBO_BOX (combo));

	gtk_button_set_relief (GTK_BUTTON (combo->priv->arrow_button), relief);
	if (GTK_IS_BUTTON (combo->priv->display_widget))
		gtk_button_set_relief (GTK_BUTTON (combo->priv->display_widget), relief);
}

void
go_combo_box_set_tooltip (GOComboBox *c, GtkTooltips *tips,
			   char const *text, char const *priv_text)
{
//#warning this is ugly the tip moves as we jump from preview to arrow
	gtk_tooltips_set_tip (tips, c->priv->display_widget, text, priv_text);
	gtk_tooltips_set_tip (tips, c->priv->arrow_button, text, priv_text);
}
