/*
 * gnc-general-select.c --  Widget to pop-up a search dialog and show
 *			the selected item.
 *
 * Copyright (C) 2001 Free Software Foundation
 * All rights reserved.
 *
 * Derek Atkins <warlord@MIT.EDU>
 *
 * Gnucash is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * as published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 *
 * Gnucash is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 *
 */
/*
  @NOTATION@
 */

#include "config.h"

#include <gtk/gtk.h>
#include <string.h>
#include <ctype.h>
#include <stdio.h>

#include "gnc-component-manager.h"
#include "QueryCore.h"
#include "QueryObject.h"
#include "gncObject.h"
#include "gnc-general-search.h"

#define GNCGENERALSEARCH_CLASS	"gnc-general-search-widget"

/* Signal codes */
enum
{
	SELECTION_CHANGED,
	LAST_SIGNAL
};


static void gnc_general_search_init         (GNCGeneralSearch      *gsl);
static void gnc_general_search_class_init   (GNCGeneralSearchClass *class);
static void gnc_general_search_destroy      (GtkObject             *object);

typedef struct _GNCGeneralSearchPrivate GNCGeneralSearchPrivate;

struct _GNCGeneralSearchPrivate {
	GUID			guid;
	GNCIdTypeConst		type;
	GNCSearchCB		search_cb;
	gpointer		user_data;
	GNCSearchWindow *	sw;
	const QofParam * get_guid;
	gint			component_id;
};

#define _PRIVATE(o) \
   (G_TYPE_INSTANCE_GET_PRIVATE ((o), GNC_TYPE_GENERAL_SEARCH, GNCGeneralSearchPrivate))

static GtkHBoxClass *parent_class;
static guint general_search_signals[LAST_SIGNAL];


/**
 * gnc_general_search_get_type:
 *
 * Returns the GtkType for the GNCGeneralSearch widget
 */
GType
gnc_general_search_get_type (void)
{
	static GType general_search_type = 0;

	if (!general_search_type){
		static const GTypeInfo our_info = {
			sizeof (GNCGeneralSearchClass),    /* class_size */
			NULL,   			   /* base_init */
			NULL,				   /* base_finalize */
			(GClassInitFunc) gnc_general_search_class_init,
			NULL,				   /* class_finalize */
			NULL,				   /* class_data */
			sizeof (GNCGeneralSearch),	   /* */
			0,				   /* n_preallocs */
			(GInstanceInitFunc) gnc_general_search_init,
		};

		general_search_type = g_type_register_static (GTK_TYPE_HBOX,
							      "GNCGeneralSearch",
							      &our_info, 0);
	}

	return general_search_type;
}

static void
gnc_general_search_class_init (GNCGeneralSearchClass *klass)
{
	GtkObjectClass *object_class = (GtkObjectClass *) klass;

	object_class = (GtkObjectClass*) klass;

	parent_class = gtk_type_class (gtk_hbox_get_type ());

	general_search_signals[SELECTION_CHANGED] =
		g_signal_new("changed",
			     G_TYPE_FROM_CLASS(object_class),
			     G_SIGNAL_RUN_FIRST,
			     G_STRUCT_OFFSET(GNCGeneralSearchClass, changed),
			     NULL, NULL,
			     g_cclosure_marshal_VOID__VOID,
			     G_TYPE_NONE, 0);

	object_class->destroy = gnc_general_search_destroy;

	klass->changed = NULL;

	g_type_class_add_private(klass, sizeof(GNCGeneralSearchPrivate));
}

static void
gnc_general_search_init (GNCGeneralSearch *gsl)
{
	gsl->selected_item = NULL;
}

static void
gnc_general_search_destroy (GtkObject *object)
{
	GNCGeneralSearch *gsl;
	GNCGeneralSearchPrivate *priv;

	g_return_if_fail (object != NULL);
	g_return_if_fail (GNC_IS_GENERAL_SEARCH (object));

	gsl = GNC_GENERAL_SEARCH (object);

	gsl->entry = NULL;
	gsl->button = NULL;

	priv = _PRIVATE(gsl);
	/* Clear the callbacks */
	if (priv->sw) {
		gnc_search_dialog_set_select_cb (priv->sw, NULL, NULL, FALSE);
		gnc_search_dialog_disconnect (priv->sw, gsl);
		priv->sw = NULL;
	}
	if (priv->component_id) {
		/* Unregister ourselves */
		gnc_unregister_gui_component (priv->component_id);
		priv->component_id = 0;
	}

	if (GTK_OBJECT_CLASS (parent_class)->destroy)
		GTK_OBJECT_CLASS (parent_class)->destroy (object);
}

/* The "selection" contents have changed.  Change the text. */
static void
reset_selection_text (GNCGeneralSearch *gsl)
{
	GNCGeneralSearchPrivate *priv;
	const char *text;

	priv = _PRIVATE(gsl);
	if (gsl->selected_item == NULL)
		text = "";
	else
		text = gncObjectPrintable (priv->type, gsl->selected_item);

	gtk_entry_set_text(GTK_ENTRY(gsl->entry), text);
}

/* We've got a refresh event */
static void
refresh_handler (GHashTable *changes, gpointer data)
{
	GNCGeneralSearch *gsl = data;
	GNCGeneralSearchPrivate *priv;
	const EventInfo *info;

	priv = _PRIVATE(gsl);
	if (changes) {
		info = gnc_gui_get_entity_events (changes, &priv->guid);
		if (info) {
			if (info->event_mask & QOF_EVENT_DESTROY)
				gsl->selected_item = NULL;
			reset_selection_text (gsl);
		}
	}
}

/* The user has selected from the search dialog */
static void
new_item_selected_cb (gpointer item, gpointer user_data)
{
	GNCGeneralSearch *gsl = user_data;
	gnc_general_search_set_selected (gsl, item);
}

/* The search dialog has closed; let's forget about her */
static int
on_close_cb (GtkDialog *dialog, gpointer user_data)
{
	GNCGeneralSearch *gsl = user_data;
	GNCGeneralSearchPrivate *priv;

	priv = _PRIVATE(gsl);
	priv->sw = NULL;
	return FALSE;
}

/* The user clicked on the button.  Pop up the selection window */
static void
search_cb(GtkButton * button, gpointer user_data)
{
	GNCGeneralSearch *gsl = user_data;
	GNCGeneralSearchPrivate *priv;
	GNCSearchWindow *sw;

	priv = _PRIVATE(gsl);
	if (priv->sw) {
		gnc_search_dialog_raise (priv->sw);
		return;
	}

	sw = (priv->search_cb)(gsl->selected_item, priv->user_data);

	/* NULL means nothing to 'select' */
	if (sw == NULL)
		return;

	/* Ok, save this search window and setup callbacks */
	priv->sw = sw;

	/* Catch when the search dialog closes */
	gnc_search_dialog_connect_on_close (sw, G_CALLBACK (on_close_cb),
					    gsl);

	/* Catch the selection */
	gnc_search_dialog_set_select_cb (sw, new_item_selected_cb,
					 gsl, gsl->allow_clear);

}

static void
create_children (GNCGeneralSearch *gsl, const char *label)
{
	gsl->entry = gtk_entry_new ();
	gtk_editable_set_editable (GTK_EDITABLE (gsl->entry), FALSE);
	gtk_box_pack_start (GTK_BOX (gsl), gsl->entry, TRUE, TRUE, 0);
	gtk_widget_show (gsl->entry);

	gsl->button = gtk_button_new_with_label (label);
	gtk_box_pack_start (GTK_BOX (gsl), gsl->button, FALSE, FALSE, 0);
	g_signal_connect (G_OBJECT (gsl->button), "clicked",
			  G_CALLBACK (search_cb), gsl);
	gtk_widget_show (gsl->button);
}

/**
 * gnc_general_search_new:
 *
 * Creates a new GNCGeneralSearch widget which can be used to provide
 * an easy way to choose selections
 *
 * Returns a GNCGeneralSearch widget.
 */
GtkWidget *
gnc_general_search_new (GNCIdTypeConst type, const char *label,
			GNCSearchCB search_cb, gpointer user_data)
{
	GNCGeneralSearch *gsl;
	GNCGeneralSearchPrivate *priv;
	const QofParam *get_guid;

	g_return_val_if_fail (type && label && search_cb, NULL);

	get_guid = qof_class_get_parameter (type, QOF_PARAM_GUID);
	g_return_val_if_fail (get_guid, NULL);

	gsl = g_object_new (GNC_TYPE_GENERAL_SEARCH, NULL);

	create_children (gsl, label);

	priv = _PRIVATE(gsl);
	priv->type = type;
	priv->search_cb = search_cb;
	priv->user_data = user_data;
	priv->get_guid = get_guid;
	priv->component_id =
		gnc_register_gui_component (GNCGENERALSEARCH_CLASS,
					    refresh_handler, NULL, gsl);

	return GTK_WIDGET (gsl);
}

/**
 * gnc_general_search_set_selected:
 * @gsl: the general selection widget
 * @selection: the selection to point to
 *
 * Sets the selection value of the widget to a particular pointer.
 *
 * Returns nothing.
 */
void
gnc_general_search_set_selected (GNCGeneralSearch *gsl, gpointer selection)
{
	GNCGeneralSearchPrivate *priv;

	g_return_if_fail(gsl != NULL);
	g_return_if_fail(GNC_IS_GENERAL_SEARCH(gsl));

	priv = _PRIVATE(gsl);
	if (selection != gsl->selected_item) {
		gsl->selected_item = selection;
		reset_selection_text (gsl);
		g_signal_emit(gsl,
			      general_search_signals[SELECTION_CHANGED], 0);
	}

	gnc_gui_component_clear_watches (priv->component_id);

	if (selection) {
		const QofParam *get_guid = priv->get_guid;
		priv->guid = * ((GUID *)(get_guid->param_getfcn
					      (gsl->selected_item, get_guid)));
		gnc_gui_component_watch_entity
			(priv->component_id, &(priv->guid),
			 QOF_EVENT_MODIFY | QOF_EVENT_DESTROY);
	} else
		priv->guid = *xaccGUIDNULL ();
}

/**
 * gnc_general_search_get_selected:
 * @gsl: the general selection widget
 *
 * Returns the current selection by the widget.
 */
gpointer
gnc_general_search_get_selected (GNCGeneralSearch *gsl)
{
	g_return_val_if_fail(gsl != NULL, NULL);
	g_return_val_if_fail(GNC_IS_GENERAL_SEARCH(gsl), NULL);

	return gsl->selected_item;
}

void
gnc_general_search_allow_clear (GNCGeneralSearch *gsl, gboolean allow_clear)
{
	g_return_if_fail (GNC_IS_GENERAL_SEARCH (gsl));
	gsl->allow_clear = allow_clear;
}

/*
  Local Variables:
  c-basic-offset: 8
  End:
*/
