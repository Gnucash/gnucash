/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * go-action-combo-stack.c: A custom GtkAction to handle undo/redo menus/toolbars
 *
 * Copyright (C) 2004 Jody Goldberg (jody@gnome.org)
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of version 2 of the GNU General Public
 * License as published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
 * USA
 */
#include <goffice/goffice-config.h>
#include "go-action-combo-stack.h"
#include "go-combo-box.h"
//#include <src/gui-util.h>
#include <gui-util.h>

#include <gtk/gtkaction.h>
#include <gtk/gtktoolitem.h>
#include <gtk/gtkimagemenuitem.h>
#include <gtk/gtkimage.h>
#include <gtk/gtkwindow.h>
#include <gtk/gtkcontainer.h>
#include <gtk/gtktreeview.h>
#include <gtk/gtktreeselection.h>
#include <gtk/gtkliststore.h>
#include <gtk/gtkcellrenderertext.h>
#include <gtk/gtkwidget.h>
#include <gtk/gtktogglebutton.h>
#include <gtk/gtkscrolledwindow.h>

#include <gsf/gsf-impl-utils.h>
#include <glib/gi18n.h>
#include <stdio.h>

////////////////////////////////////////////////////////////////////////////

typedef struct {
	GOComboBox base;

	GtkWidget   *button;
	GtkTreeView *list;
	GtkWidget   *scrolled;

	gpointer last_key;
} GOComboStack;

typedef struct {
	GOComboBoxClass	base;
	void (*pop) (GOComboStack *cbox, gpointer key);
} GOComboStackClass;

enum {
	POP,
	LAST_SIGNAL
};
enum {
	LABEL_COL,
	INDEX_COL,
	KEY_COL
};

#define GO_COMBO_STACK_TYPE	(go_combo_stack_get_type ())
#define GO_COMBO_STACK(o)	G_TYPE_CHECK_INSTANCE_CAST (o, GO_COMBO_STACK_TYPE, GOComboStack)
#define IS_GO_COMBO_STACK(o)	G_TYPE_CHECK_INSTANCE_TYPE (o, GO_COMBO_STACK_TYPE)

static GtkType go_combo_stack_get_type   (void);
static guint go_combo_stack_signals [LAST_SIGNAL] = { 0, };

static void
cb_screen_changed (GOComboStack *cs, GdkScreen *previous_screen)
{
	GtkWidget *w = GTK_WIDGET (cs);
	GdkScreen *screen = gtk_widget_has_screen (w)
		? gtk_widget_get_screen (w)
		: NULL;

	if (screen) {
		GtkWidget *toplevel = gtk_widget_get_toplevel (cs->scrolled
		    ? cs->scrolled : GTK_WIDGET (cs->list));
		gtk_window_set_screen (GTK_WINDOW (toplevel), screen);
	}
}

static gpointer
get_key_at_path (GtkTreeView *view, GtkTreePath	*pos)
{
	gpointer	  res = NULL;
	GtkTreeIter	  iter;
	GtkTreeModel	 *model = gtk_tree_view_get_model (view);
	if (gtk_tree_model_get_iter (model, &iter, pos))
		gtk_tree_model_get (model, &iter, KEY_COL, &res, -1);
	return res;
}

static void
cb_button_clicked (GOComboStack *stack)
{
	if (!_go_combo_is_updating (GO_COMBO_BOX (stack))) {
		GtkTreePath *pos = gtk_tree_path_new_first ();
		gpointer top = get_key_at_path (stack->list, pos);
		gtk_tree_path_free (pos);
		g_signal_emit (stack, go_combo_stack_signals [POP], 0, top);
		go_combo_box_popup_hide (GO_COMBO_BOX (stack));
	}
}

static gboolean
cb_button_release_event (GtkWidget *list, GdkEventButton *e, gpointer data)
{
	GOComboStack *stack = GO_COMBO_STACK (data);

	go_combo_box_popup_hide (GO_COMBO_BOX (stack));

	if (stack->last_key != NULL) {
		gint dummy, w, h;
		gdk_window_get_geometry (e->window, &dummy, &dummy, &w, &h, &dummy);
		if (0 <= e->x && e->x < w && 0 <= e->y && e->y < h)
			g_signal_emit (stack, go_combo_stack_signals [POP], 0,
				       stack->last_key);
	}

	return TRUE;
}

static gboolean
cb_motion_notify_event (GtkWidget *widget, GdkEventMotion *event,
			GOComboStack *stack)
{
	GtkTreePath	 *start, *pos;
	GtkTreeSelection *sel;
	GtkTreeModel	 *model = gtk_tree_view_get_model (stack->list);

	stack->last_key = NULL;
	sel = gtk_tree_view_get_selection (stack->list);
	gtk_tree_selection_unselect_all (sel);

	if (!gtk_tree_view_get_path_at_pos
	    (stack->list, event->x, event->y, &pos, NULL, NULL, NULL)) {
		int n = gtk_tree_model_iter_n_children (model, NULL);
		if (n == 0)
			return TRUE;
		pos = gtk_tree_path_new_from_indices (n - 1, -1);
	}

	stack->last_key = get_key_at_path (stack->list, pos);
	start = gtk_tree_path_new_first ();
	gtk_tree_selection_select_range (sel, start, pos);
	gtk_tree_path_free (start);
	gtk_tree_path_free (pos);

	return TRUE;
}

static gboolean
cb_leave_notify_event (GOComboStack *stack)
{
	stack->last_key = NULL;
	gtk_tree_selection_unselect_all (gtk_tree_view_get_selection (stack->list));
	return FALSE;
}

static void
go_combo_stack_init (GOComboStack *stack)
{
	GtkScrolledWindow *scrolled;
	GtkTreeSelection *selection;

	stack->button = gtk_toggle_button_new ();
	gtk_button_set_relief (GTK_BUTTON (stack->button), GTK_RELIEF_NONE);
	GTK_WIDGET_UNSET_FLAGS (stack->button, GTK_CAN_FOCUS);

	stack->list = (GtkTreeView *)gtk_tree_view_new ();
	selection = gtk_tree_view_get_selection (stack->list);
	gtk_tree_selection_set_mode (selection, GTK_SELECTION_MULTIPLE);

	stack->scrolled = gtk_scrolled_window_new (
		gtk_tree_view_get_hadjustment (stack->list),
		gtk_tree_view_get_vadjustment (stack->list));
	scrolled = GTK_SCROLLED_WINDOW (stack->scrolled);
	gtk_scrolled_window_set_policy (scrolled,
		GTK_POLICY_NEVER, GTK_POLICY_AUTOMATIC);
	gtk_scrolled_window_add_with_viewport (scrolled, GTK_WIDGET (stack->list));
	gtk_widget_set_size_request (stack->scrolled, -1, 200); /* MAGIC NUMBER */

	/* Set up the dropdown list */
	g_signal_connect (G_OBJECT (stack), "screen-changed", G_CALLBACK (cb_screen_changed), NULL);
	g_signal_connect (G_OBJECT (stack->list),
		"button_release_event",
		G_CALLBACK (cb_button_release_event), stack);
	g_signal_connect (G_OBJECT (stack->list),
		"motion_notify_event",
		G_CALLBACK (cb_motion_notify_event), stack);
	g_signal_connect_swapped (G_OBJECT (stack->list),
		"leave_notify_event",
		G_CALLBACK (cb_leave_notify_event), stack);
	g_signal_connect_swapped (stack->button, "clicked",
		G_CALLBACK (cb_button_clicked),
		(gpointer) stack);

	gtk_widget_show (GTK_WIDGET (stack->list));
	gtk_widget_show (stack->scrolled);
	gtk_widget_show (stack->button);
	go_combo_box_construct (GO_COMBO_BOX (stack),
		stack->button, stack->scrolled, GTK_WIDGET (stack->list));
}

static void
go_combo_stack_class_init (GObjectClass *klass)
{
	go_combo_stack_signals [POP] = g_signal_new ("pop",
		G_TYPE_FROM_CLASS (klass),
		G_SIGNAL_RUN_LAST,
		G_STRUCT_OFFSET (GOComboStackClass, pop),
		NULL, NULL,
		g_cclosure_marshal_VOID__POINTER,
		G_TYPE_NONE,
		1, G_TYPE_POINTER);
}

GSF_CLASS (GOComboStack, go_combo_stack,
	   go_combo_stack_class_init, go_combo_stack_init,
	   GO_COMBO_BOX_TYPE)

////////////////////////////////////////////////////////////////////////////

typedef struct {
	GtkToolItem	 base;
	GOComboStack	*combo; /* container has a ref, not us */
} GOToolComboStack;
typedef GtkToolItemClass GOToolComboStackClass;

#define GO_TOOL_COMBO_STACK_TYPE	(go_tool_combo_stack_get_type ())
#define GO_TOOL_COMBO_STACK(o)		(G_TYPE_CHECK_INSTANCE_CAST (o, GO_TOOL_COMBO_STACK_TYPE, GOToolComboStack))
#define IS_GO_TOOL_COMBO_STACK(o)	(G_TYPE_CHECK_INSTANCE_TYPE (o, GO_TOOL_COMBO_STACK_TYPE))

static GType go_tool_combo_stack_get_type (void);
static gboolean
go_tool_combo_stack_set_tooltip (GtkToolItem *tool_item, GtkTooltips *tooltips,
				 char const *tip_text,
				 char const *tip_private)
{
	GOToolComboStack *self = (GOToolComboStack *)tool_item;
	go_combo_box_set_tooltip (GO_COMBO_BOX (self->combo), tooltips,
		tip_text, tip_private);
	return TRUE;
}

static void
go_tool_combo_stack_class_init (GtkToolItemClass *tool_item_klass)
{
	tool_item_klass->set_tooltip = go_tool_combo_stack_set_tooltip;
}

static GSF_CLASS (GOToolComboStack, go_tool_combo_stack,
	   go_tool_combo_stack_class_init, NULL,
	   GTK_TYPE_TOOL_ITEM)

/*****************************************************************************/

struct _GOActionComboStack {
	GtkAction	 base;
	GtkTreeModel	*model;

	gpointer	 last_selection;
};
typedef GtkActionClass GOActionComboStackClass;

static GObjectClass *combo_stack_parent;

static void
cb_tool_popped (GOToolComboStack *tool, gpointer key, GOActionComboStack *a)
{
	/* YUCK
	 * YUCK
	 * YUCK
	 * We really need to return the key in "activate" but can not for now.
	 * as a result people had better call
	 * 	go_action_combo_stack_selection
	 * from with the handler or they will lose the selection from toolitems.
	 * We can not tell whether the activation was a menu or accelerator
	 * which just use the top.  */
	a->last_selection = key;
	gtk_action_activate (GTK_ACTION (a));
	a->last_selection = NULL;
}

static GtkWidget *
go_action_combo_stack_create_tool_item (GtkAction *a)
{
	GOActionComboStack *saction = (GOActionComboStack *)a;
	GtkWidget *image;
	GtkTreeView *tree_view;
	GOToolComboStack *tool = g_object_new (GO_TOOL_COMBO_STACK_TYPE, NULL);
	char *stock_id;
	gboolean is_sensitive = gtk_tree_model_iter_n_children (saction->model, NULL) > 0;

	tool->combo = g_object_new (GO_COMBO_STACK_TYPE, NULL);
	tree_view = GTK_TREE_VIEW (tool->combo->list);
	gtk_tree_view_set_model (tree_view, saction->model);
	gtk_tree_view_set_headers_visible (tree_view, FALSE);
	gtk_tree_view_append_column (tree_view,
		gtk_tree_view_column_new_with_attributes (NULL,
			gtk_cell_renderer_text_new (),
			"text", 0,
			NULL));

	g_object_get (G_OBJECT (a), "stock_id", &stock_id, NULL);
	image = gtk_image_new_from_stock (
		stock_id, GTK_ICON_SIZE_LARGE_TOOLBAR);
	g_free (stock_id);
	gtk_widget_show (image);
	gtk_container_add (GTK_CONTAINER (tool->combo->button), image);

	gtk_widget_set_sensitive (GTK_WIDGET (tool), is_sensitive);

	go_combo_box_set_relief (GO_COMBO_BOX (tool->combo), GTK_RELIEF_NONE);
	go_combo_box_set_tearable (GO_COMBO_BOX (tool->combo), TRUE);
	gnm_widget_disable_focus (GTK_WIDGET (tool->combo));
	gtk_container_add (GTK_CONTAINER (tool), GTK_WIDGET (tool->combo));
	gtk_widget_show (GTK_WIDGET (tool->combo));
	gtk_widget_show (GTK_WIDGET (tool));

	g_signal_connect (G_OBJECT (tool->combo),
		"pop",
		G_CALLBACK (cb_tool_popped), saction);

	return GTK_WIDGET (tool);
}

static GtkWidget *
go_action_combo_stack_create_menu_item (GtkAction *a)
{
	GOActionComboStack *saction = (GOActionComboStack *)a;
	GtkWidget *item = gtk_image_menu_item_new ();
	gboolean is_sensitive = gtk_tree_model_iter_n_children (saction->model, NULL) > 0;
	gtk_widget_set_sensitive (GTK_WIDGET (item), is_sensitive);
	return item;
}

static void
go_action_combo_stack_finalize (GObject *obj)
{
	GOActionComboStack *saction = (GOActionComboStack *)obj;
	g_object_unref (saction->model);
	saction->model = NULL;
	combo_stack_parent->finalize (obj);
}

static void
go_action_combo_stack_class_init (GtkActionClass *gtk_act_klass)
{
	GObjectClass *gobject_klass = (GObjectClass *)gtk_act_klass;

	combo_stack_parent = g_type_class_peek_parent (gobject_klass);

	gobject_klass->finalize = go_action_combo_stack_finalize;
	gtk_act_klass->create_tool_item = go_action_combo_stack_create_tool_item;
	gtk_act_klass->create_menu_item = go_action_combo_stack_create_menu_item;
}

static void
go_action_combo_stack_init (GOActionComboStack *saction)
{
	saction->model = (GtkTreeModel *)
		gtk_list_store_new (3, G_TYPE_STRING, G_TYPE_INT, G_TYPE_POINTER);
	saction->last_selection = NULL;
}

GSF_CLASS (GOActionComboStack, go_action_combo_stack,
	   go_action_combo_stack_class_init, go_action_combo_stack_init,
	   GTK_TYPE_ACTION)

static void
check_sensitivity (GOActionComboStack *saction, unsigned old_count)
{
	unsigned new_count = gtk_tree_model_iter_n_children (saction->model, NULL);

	if ((old_count > 0) ^ (new_count > 0)) {
		GSList *ptr = gtk_action_get_proxies (GTK_ACTION (saction));
		gboolean is_sensitive = (new_count > 0);
		for ( ; ptr != NULL ; ptr = ptr->next)
			gtk_widget_set_sensitive (ptr->data, is_sensitive);
	}
}

/**
 * go_action_combo_stack_push :
 * @act : #GOActionComboStack
 * @str : The label to push
 * @key : a key value to id the pushe item
 **/
void
go_action_combo_stack_push (GOActionComboStack *a,
			    char const *label, gpointer key)
{
	GOActionComboStack *saction = GO_ACTION_COMBO_STACK (a);
	GtkTreeIter iter;
	unsigned old_count = gtk_tree_model_iter_n_children (saction->model, NULL);

	g_return_if_fail (saction != NULL);

	gtk_list_store_insert (GTK_LIST_STORE (saction->model), &iter, 0);
	gtk_list_store_set (GTK_LIST_STORE (saction->model), &iter,
		LABEL_COL,	label,
		KEY_COL,	key,
		-1);
	check_sensitivity (saction, old_count);
}

/**
 * go_action_combo_stack_pop :
 * @act : #GOActionComboStack
 * @n :
 *
 * Shorten list @act by removing @n off the top (or fewer if the list is
 * shorter)
 **/
void
go_action_combo_stack_pop (GOActionComboStack *a, unsigned n)
{
	GOActionComboStack *saction = GO_ACTION_COMBO_STACK (a);
	GtkTreeIter iter;
	unsigned old_count = gtk_tree_model_iter_n_children (saction->model, NULL);

	g_return_if_fail (saction != NULL);

	if (gtk_tree_model_iter_nth_child (saction->model, &iter, NULL, 0))
		while (n-- > 0 &&
		       gtk_list_store_remove (GTK_LIST_STORE (saction->model), &iter))
			;
	check_sensitivity (saction, old_count);
}

/**
 * go_action_combo_stack_truncate :
 * @act : #GOActionComboStack
 * @n : 
 *
 * Ensure that list @act is no longer than @n, dropping any extra off the
 * bottom.
 **/
void
go_action_combo_stack_truncate (GOActionComboStack *a, unsigned n)
{
	GOActionComboStack *saction = GO_ACTION_COMBO_STACK (a);
	GtkTreeIter iter;
	unsigned old_count = gtk_tree_model_iter_n_children (saction->model, NULL);

	g_return_if_fail (saction != NULL);

	if (gtk_tree_model_iter_nth_child (saction->model, &iter, NULL, n))
		while (gtk_list_store_remove (GTK_LIST_STORE (saction->model), &iter))
			;
	check_sensitivity (saction, old_count);
}

/**
 * go_action_combo_stack_selection :
 * @a : #GOActionComboStack
 *
 * Returns the key of the item last selected in one of the proxies.
 * Yes this interface is terrible, but we can't return the key in the activate
 * signal.
 *
 * NOTE : see writeup in cb_tool_popped.
 **/
gpointer
go_action_combo_stack_selection (GOActionComboStack const *a)
{
	gpointer res = NULL;
	GtkTreeIter iter;

	if (a->last_selection != NULL)
		return a->last_selection;
	if (gtk_tree_model_get_iter_first (a->model, &iter))
		gtk_tree_model_get (a->model, &iter,
			KEY_COL, &res,
			-1);
	return res;
}
