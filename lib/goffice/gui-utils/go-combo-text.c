/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gnumeric-combo-text: A combo box for selecting from a list.
 */

#include <goffice/goffice-config.h>
#include "go-combo-text.h"
#include "go-combo-box.h"
#include "go-marshalers.h"

#include <gtk/gtksignal.h>
#include <gtk/gtkentry.h>
#include <gtk/gtktreeview.h>
#include <gtk/gtktreeselection.h>
#include <gtk/gtkliststore.h>
#include <gtk/gtkcellrenderertext.h>
#include <gtk/gtklabel.h>
#include <gtk/gtkscrolledwindow.h>

#include <gsf/gsf-impl-utils.h>

struct _GoComboText {
	GOComboBox parent;

	GCompareFunc cmp_func;

	GtkWidget *entry;
	GtkWidget *list;
	GtkWidget *scroll;
	int rows;
};

typedef struct {
	GOComboBoxClass	base;

	gboolean (* selection_changed)	(GoComboText *ct, GtkTreeSelection *selection);
	gboolean (* entry_changed)	(GoComboText *ct, char const *new_str);
} GoComboTextClass;

#define GO_COMBO_TEXT_CLASS(klass) G_TYPE_CHECK_CLASS_CAST (klass, go_combo_text_get_type (), GoComboTextClass)

enum {
	SELECTION_CHANGED,
	ENTRY_CHANGED,
	LAST_SIGNAL
};
static guint combo_text_signals [LAST_SIGNAL] = { 0 };

/**
 * A utility wrapper around g_signal_emitv because it does not initiialize the
 * result to FALSE if there is no handler.
 */
static gboolean
go_signal_emit (GoComboText *ct, int signal,
		 gconstpointer arg, int default_ret)
{
	gboolean result;
	GValue ret = { 0, };
	GValue instance_and_parm [2] = { { 0, }, { 0, } };

	g_value_init (instance_and_parm + 0, GO_TYPE_COMBO_TEXT);
	g_value_set_instance (instance_and_parm + 0, G_OBJECT (ct));

	g_value_init (instance_and_parm + 1, G_TYPE_POINTER);
	g_value_set_pointer (instance_and_parm + 1, (gpointer)arg);

	g_value_init (&ret, G_TYPE_BOOLEAN);
	g_value_set_boolean  (&ret, default_ret);

	g_signal_emitv (instance_and_parm, combo_text_signals [signal], 0, &ret);
	result = g_value_get_boolean (&ret);

	g_value_unset (instance_and_parm + 0);
	g_value_unset (instance_and_parm + 1);

	return result;
}

static void
cb_entry_activate (GtkWidget *entry, gpointer ct)
{
	char const *text = gtk_entry_get_text (GTK_ENTRY (entry));

	if (go_signal_emit (GO_COMBO_TEXT (ct), ENTRY_CHANGED, text, TRUE))
		go_combo_text_set_text (GO_COMBO_TEXT (ct), text,
			GO_COMBO_TEXT_CURRENT);
}

static void
cb_list_changed (GtkTreeSelection *selection,
		gpointer data)
{
	GoComboText *ct = GO_COMBO_TEXT (data);
	GtkEntry *entry = GTK_ENTRY (ct->entry);
	gboolean accept_change;
	GtkTreeModel *store;
	GtkTreeIter iter;
	char const *text;

	if (gtk_tree_selection_get_selected (selection, &store, &iter))
		gtk_tree_model_get (store, &iter, 0, &text, -1);
	else
		text = "";

	accept_change = TRUE;
	if (go_signal_emit (ct, SELECTION_CHANGED, selection, TRUE))
		accept_change = go_signal_emit (ct, ENTRY_CHANGED, text, TRUE);
	if (accept_change)
		gtk_entry_set_text (entry, text);

	go_combo_box_popup_hide (GO_COMBO_BOX (data));
}

static void
cb_scroll_size_request (GtkWidget *widget, GtkRequisition *requisition,
			GoComboText *ct)
{
	GtkRequisition list_req;
	int mon_width, mon_height;
	GdkRectangle rect;
	GdkScreen    *screen;

	/* In a Xinerama setup, use geometry of the actual display unit.  */
	screen = gtk_widget_get_screen (widget);
	if (screen == NULL)
		/* Looks like this will happen when
		 * embedded as a bonobo component */
		screen = gdk_screen_get_default ();

	gdk_screen_get_monitor_geometry (screen, 0, &rect);
	mon_width  = rect.width;
	mon_height = rect.height;

	gtk_widget_size_request	(ct->list, &list_req);
	if (requisition->height < list_req.height) {
		int height       = list_req.height;
		GtkWidget const *w = ct->list;

		if (w != NULL) {
			/* Make room for a whole number of items which don't
			 * overflow the screen, but no more than 20. */
			int avail_height, nitems;
			
			avail_height = mon_height - 20
				- GTK_CONTAINER (widget)->border_width * 2 + 4;
			nitems = MIN (20, avail_height * ct->rows / w->requisition.height);
			height = nitems *  w->requisition.height / ct->rows;
			if (height > list_req.height)
				height = list_req.height;
		}

		/* FIXME : Why do we need 4 ??
		 * without it things end up scrolling.
		 */
		requisition->height = height +
			GTK_CONTAINER (widget)->border_width * 2 + 4;
	}

	requisition->width  = MAX (requisition->width, 
				   ct->entry->allocation.width + 
				   GTK_CONTAINER (widget)->border_width * 2);
	requisition->width  = MIN (requisition->width, mon_width - 20);
	requisition->height = MIN (requisition->height, mon_height - 20);
}

static void
cb_screen_changed (GoComboText *ct, GdkScreen *previous_screen)
{
	GtkWidget *w = GTK_WIDGET (ct);
	GdkScreen *screen = gtk_widget_has_screen (w)
		? gtk_widget_get_screen (w)
		: NULL;

	if (screen) {
		GtkWidget *toplevel = gtk_widget_get_toplevel (ct->scroll);
		gtk_window_set_screen (GTK_WINDOW (toplevel), screen);
	}	
}

static void
go_combo_text_init (GoComboText *ct)
{
	GtkCellRenderer *renderer;
	GtkListStore *store;
	GtkTreeViewColumn *column;

	ct->rows = 0;
	ct->entry = gtk_entry_new ();
	ct->list = gtk_tree_view_new ();
	g_object_set (G_OBJECT (ct->list), NULL);
	gtk_tree_view_set_headers_visible (GTK_TREE_VIEW (ct->list), FALSE);
	store = gtk_list_store_new (1, G_TYPE_STRING);
	gtk_tree_view_set_model (GTK_TREE_VIEW (ct->list), GTK_TREE_MODEL (store));
	renderer = gtk_cell_renderer_text_new ();
	column = gtk_tree_view_column_new_with_attributes (
								NULL,
								renderer, "text", 0, NULL);
	gtk_tree_view_column_set_expand (column, TRUE);
	gtk_tree_view_append_column (GTK_TREE_VIEW (ct->list), column);
	g_signal_connect (G_OBJECT (gtk_tree_view_get_selection (GTK_TREE_VIEW (ct->list))),
		"changed",
		G_CALLBACK (cb_list_changed), (gpointer) ct);

	ct->scroll = gtk_scrolled_window_new (NULL, NULL);

	gtk_scrolled_window_set_policy (
		GTK_SCROLLED_WINDOW (ct->scroll),
		GTK_POLICY_NEVER, GTK_POLICY_AUTOMATIC);
	gtk_scrolled_window_add_with_viewport (
		GTK_SCROLLED_WINDOW (ct->scroll), ct->list);
	gtk_container_set_focus_hadjustment (
		GTK_CONTAINER (ct->list),
		gtk_scrolled_window_get_hadjustment (
			GTK_SCROLLED_WINDOW (ct->scroll)));
	gtk_container_set_focus_vadjustment (
		GTK_CONTAINER (ct->list),
		gtk_scrolled_window_get_vadjustment (
			GTK_SCROLLED_WINDOW (ct->scroll)));

	g_signal_connect (G_OBJECT (ct->entry),
		"activate",
		GTK_SIGNAL_FUNC (cb_entry_activate), (gpointer) ct);
	g_signal_connect (G_OBJECT (ct->scroll),
		"size_request",
		G_CALLBACK (cb_scroll_size_request), (gpointer) ct);

	gtk_widget_show (ct->entry);
	go_combo_box_construct (GO_COMBO_BOX (ct),
		ct->entry, ct->scroll, ct->list);

	g_signal_connect (G_OBJECT (ct),
		"screen-changed", G_CALLBACK (cb_screen_changed),
		NULL);
}

static void
go_combo_text_destroy (GtkObject *object)
{
	GtkObjectClass *parent;
	GoComboText *ct = GO_COMBO_TEXT (object);

	if (ct->list != NULL) {
		g_signal_handlers_disconnect_by_func (G_OBJECT (ct),
			G_CALLBACK (cb_screen_changed), NULL);
		ct->list = NULL;
	}

	parent = g_type_class_peek (GO_COMBO_BOX_TYPE);
	if (parent && parent->destroy)
		(*parent->destroy) (object);
}

static void
go_combo_text_class_init (GtkObjectClass *klass)
{
	klass->destroy = &go_combo_text_destroy;

	combo_text_signals [SELECTION_CHANGED] = g_signal_new ("selection_changed",
		GO_TYPE_COMBO_TEXT,
		G_SIGNAL_RUN_LAST,
		G_STRUCT_OFFSET (GoComboTextClass, selection_changed),
		(GSignalAccumulator) NULL, NULL,
		go__BOOLEAN__POINTER,
		G_TYPE_BOOLEAN, 1, G_TYPE_POINTER);
	combo_text_signals [ENTRY_CHANGED] = g_signal_new ("entry_changed",
		GO_TYPE_COMBO_TEXT,
		G_SIGNAL_RUN_LAST,
		G_STRUCT_OFFSET (GoComboTextClass, entry_changed),
		(GSignalAccumulator) NULL, NULL,
		go__BOOLEAN__POINTER,
		G_TYPE_BOOLEAN, 1, G_TYPE_POINTER);
}

/**
 * go_combo_text_new :
 * @cmp_func : an optional comparison routine.
 */
GtkWidget*
go_combo_text_new (GCompareFunc cmp_func)
{
	GoComboText *ct;

	if (cmp_func == NULL)
		cmp_func = &g_str_equal;

	ct = g_object_new (GO_TYPE_COMBO_TEXT, NULL);
	ct->cmp_func = cmp_func;
	return GTK_WIDGET (ct);
}

GtkWidget *
go_combo_text_glade_new (void)
{
	return go_combo_text_new (NULL);
}

GSF_CLASS (GoComboText, go_combo_text,
	   go_combo_text_class_init, go_combo_text_init,
	   GO_COMBO_BOX_TYPE)

GtkWidget *
go_combo_text_get_entry (GoComboText *ct)
{
	return ct->entry;
}

/**
 * go_combo_text_set_text :
 * @ct :
 * @text : the label for the new item
 * @start : where to begin the search in the list.
 *
 * return TRUE if the item is found in the list.
 */
gboolean
go_combo_text_set_text (GoComboText *ct, const gchar *text,
			 GoComboTextSearch start)
{
	gboolean found = FALSE, result;
	GtkTreeView   *list = GTK_TREE_VIEW (ct->list);
	GtkTreeIter iter;
	GtkTreeModel *store;
	GtkTreeSelection *selection = gtk_tree_view_get_selection (list);
	char *label;

	/* Be careful */
	result = start != GO_COMBO_TEXT_FROM_TOP &&
				gtk_tree_selection_get_selected (selection, &store, &iter);

	if (result) {
		if (start == GO_COMBO_TEXT_NEXT)
			result = gtk_tree_model_iter_next (store, &iter);
		for (; result ; result = gtk_tree_model_iter_next (store, &iter)) {
			gtk_tree_model_get (store, &iter, 0, &label, -1);
			if (ct->cmp_func (label, text))
				break;
			g_free (label);
		}
	} else
		store = gtk_tree_view_get_model (list);

	if (!result)
		for (result = gtk_tree_model_get_iter_first (store, &iter);
				result;
				result = gtk_tree_model_iter_next (store, &iter)) {
			gtk_tree_model_get (store, &iter, 0, &label, -1);
			if (ct->cmp_func (label, text))
				break;
			g_free (label);
		}

	g_signal_handlers_block_by_func (G_OBJECT (selection),
					  G_CALLBACK (cb_list_changed),
					  (gpointer) ct);
	gtk_tree_selection_unselect_all (selection);

	/* Use visible label rather than supplied text just in case */
	if (result) {
		GtkTreePath *path = gtk_tree_model_get_path (store, &iter);
		gtk_tree_selection_select_iter (selection, &iter);
		gtk_tree_view_set_cursor (GTK_TREE_VIEW (ct->list), path, NULL, FALSE);
		gtk_tree_path_free (path);
		gtk_entry_set_text (GTK_ENTRY (ct->entry), label);
		g_free (label);
		found = TRUE;
	} else
		gtk_entry_set_text (GTK_ENTRY (ct->entry), text);

	g_signal_handlers_unblock_by_func (G_OBJECT (selection),
					  G_CALLBACK (cb_list_changed),
					  (gpointer) ct);
	return found;
}

/**
 * go_combo_text_add_item :
 * @ct : The text combo that will get the new element.
 * @label : the user visible label for the new item
 * @key   : The unique key to identify this item.
 *
 * It is ok to have multiple items with the same label, but the key must be
 * unique.
 */
void
go_combo_text_add_item (GoComboText *ct, char const *label)
{
	GtkListStore *store;
	GtkTreeIter iter;
	g_return_if_fail (label != NULL);

	store = GTK_LIST_STORE (gtk_tree_view_get_model (GTK_TREE_VIEW (ct->list)));
	gtk_list_store_append (store, &iter);
	gtk_list_store_set (store, &iter, 0, label, -1);
	ct->rows++;
}
