/*
 * gnc-general-select.c --  General Selection Widget
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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
 * Boston, MA  02111-1307,  USA       gnu@gnu.org
 *
 */
/*
  @NOTATION@
 */

#include <config.h>
#include <string.h>
#include <ctype.h>
#include <stdio.h>

#include "gnc-general-select.h"

/* Signal codes */
enum
{
  SELECTION_CHANGED,
  LAST_SIGNAL
};


static void gnc_general_select_init         (GNCGeneralSelect      *gsl);
static void gnc_general_select_class_init   (GNCGeneralSelectClass *class);
static void gnc_general_select_destroy      (GtkObject             *object);

static GtkHBoxClass *parent_class;
static guint general_select_signals[LAST_SIGNAL];


/**
 * gnc_general_select_get_type:
 *
 * Returns the GtkType for the GNCGeneralSelect widget
 */
guint
gnc_general_select_get_type (void)
{
	static guint general_select_type = 0;

	if (!general_select_type){
		GtkTypeInfo general_select_info = {
			"GNCGeneralSelect",
			sizeof (GNCGeneralSelect),
			sizeof (GNCGeneralSelectClass),
			(GtkClassInitFunc) gnc_general_select_class_init,
			(GtkObjectInitFunc) gnc_general_select_init,
			NULL,
			NULL,
		};

		general_select_type = gtk_type_unique (gtk_hbox_get_type (),
                                                       &general_select_info);
	}

	return general_select_type;
}

static void
gnc_general_select_forall (GtkContainer *container, gboolean include_internals,
                           GtkCallback callback, gpointer callback_data)
{
	g_return_if_fail (container != NULL);
	g_return_if_fail (GNC_IS_GENERAL_SELECT (container));
	g_return_if_fail (callback != NULL);

	/* Let GtkBox handle things only if the internal widgets need
	 * to be poked. */
        if (!include_internals)
                return;

        if (!GTK_CONTAINER_CLASS (parent_class)->forall)
                return;

        GTK_CONTAINER_CLASS (parent_class)->forall (container,
                                                    include_internals,
                                                    callback,
                                                    callback_data);
}

static void
gnc_general_select_class_init (GNCGeneralSelectClass *klass)
{
	GtkObjectClass *object_class = (GtkObjectClass *) klass;
	GtkContainerClass *container_class = (GtkContainerClass *) klass;

	object_class = (GtkObjectClass*) klass;

	parent_class = gtk_type_class (gtk_hbox_get_type ());

        general_select_signals[SELECTION_CHANGED] =
                gtk_signal_new("changed",
                               GTK_RUN_FIRST,
                               object_class->type,
                               GTK_SIGNAL_OFFSET(GNCGeneralSelectClass,
                                                 changed),
                               gtk_marshal_NONE__NONE,
                               GTK_TYPE_NONE, 0);

        gtk_object_class_add_signals(object_class,
                                     general_select_signals,
                                     LAST_SIGNAL);

	container_class->forall = gnc_general_select_forall;

	object_class->destroy = gnc_general_select_destroy;

        klass->changed = NULL;
}

static void
gnc_general_select_init (GNCGeneralSelect *gsl)
{
        gsl->selected_item = NULL;
}

static void
gnc_general_select_destroy (GtkObject *object)
{
        GNCGeneralSelect *gsl;

	g_return_if_fail (object != NULL);
	g_return_if_fail (GNC_IS_GENERAL_SELECT (object));

        gsl = GNC_GENERAL_SELECT (object);

        gsl->entry = NULL;
        gsl->button = NULL;

	if (GTK_OBJECT_CLASS (parent_class)->destroy)
		GTK_OBJECT_CLASS (parent_class)->destroy (object);
}

static void
select_cb(GtkButton * button, gpointer user_data)
{
        GNCGeneralSelect *gsl = user_data;
	gpointer new_selection;
        GtkWidget *toplevel;

        toplevel = gtk_widget_get_toplevel (GTK_WIDGET (button));

        new_selection = (gsl->new_select)(gsl->cb_arg, gsl->selected_item,
					  toplevel);

        /* NULL return means cancel; no change */
        if (new_selection == NULL)
                return;

        gnc_general_select_set_selected (gsl, new_selection);
}

static void
create_children (GNCGeneralSelect *gsl, GNCGeneralSelectType type)
{
        gsl->entry = gtk_entry_new ();
        gtk_entry_set_editable (GTK_ENTRY (gsl->entry), FALSE);
	gtk_box_pack_start (GTK_BOX (gsl), gsl->entry, TRUE, TRUE, 0);
        gtk_widget_show (gsl->entry);

	if (type == GNC_GENERAL_SELECT_TYPE_SELECT)
	  gsl->button = gtk_button_new_with_label (_("Select..."));
	else if (type == GNC_GENERAL_SELECT_TYPE_EDIT)
	  gsl->button = gtk_button_new_with_label (_("Edit..."));
	else if (type == GNC_GENERAL_SELECT_TYPE_VIEW)
	  gsl->button = gtk_button_new_with_label (_("View..."));

	gtk_box_pack_start (GTK_BOX (gsl), gsl->button, FALSE, FALSE, 0);
        gtk_signal_connect (GTK_OBJECT (gsl->button), "clicked",
                            select_cb, gsl);
        gtk_widget_show (gsl->button);
}

/**
 * gnc_general_select_new:
 *
 * Creates a new GNCGeneralSelect widget which can be used to provide
 * an easy way to choose selections
 *
 * Returns a GNCGeneralSelect widget.
 */
GtkWidget *
gnc_general_select_new (GNCGeneralSelectType type,
			GNCGeneralSelectGetStringCB get_string,
			GNCGeneralSelectNewSelectCB new_select,
			gpointer cb_arg)
{
	GNCGeneralSelect *gsl;
	g_return_val_if_fail (get_string != NULL, NULL);
	g_return_val_if_fail (new_select != NULL, NULL);

	gsl = gtk_type_new (gnc_general_select_get_type ());

	create_children (gsl, type);
	gsl->get_string = get_string;
	gsl->new_select = new_select;
	gsl->cb_arg = cb_arg;

	return GTK_WIDGET (gsl);
}

/*
 * gnc_general_select_get_printname:
 * @gsl: the general selection widget
 * @selection: the selection to get the printname
 *
 * returns the printable name of the selection
 */
const char *
gnc_general_select_get_printname (GNCGeneralSelect *gsl, gpointer selection)
{
  g_return_val_if_fail (gsl != NULL, NULL);
  g_return_val_if_fail (selection != NULL, NULL);

  return (gsl->get_string)(selection);
}

/**
 * gnc_general_select_set_selected:
 * @gsl: the general selection widget
 * @selection: the selection to point to
 *
 * Sets the selection value of the widget to a particular pointer.
 *
 * Returns nothing.
 */
void
gnc_general_select_set_selected (GNCGeneralSelect *gsl, gpointer selection)
{
        const char *text;

        g_return_if_fail(gsl != NULL);
        g_return_if_fail(GNC_IS_GENERAL_SELECT(gsl));

        gsl->selected_item = selection;

        if (selection == NULL)
                text = "";
        else
                text = gnc_general_select_get_printname(gsl, selection);

        gtk_entry_set_text(GTK_ENTRY(gsl->entry), text);

        gtk_signal_emit(GTK_OBJECT(gsl),
                        general_select_signals[SELECTION_CHANGED]);
}

/**
 * gnc_general_select_get_commodity:
 * @gsl: the general selection widget
 *
 * Returns the current selection by the widget.
 */
gpointer
gnc_general_select_get_selected (GNCGeneralSelect *gsl)
{
        g_return_val_if_fail(gsl != NULL, NULL);
        g_return_val_if_fail(GNC_IS_GENERAL_SELECT(gsl), NULL);

        return gsl->selected_item;
}

/*
  Local Variables:
  c-basic-offset: 8
  End:
*/
