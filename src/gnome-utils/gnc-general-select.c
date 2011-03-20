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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 *
 */
/*
  @NOTATION@
 */

#include "config.h"

#include <gtk/gtk.h>
#include <glib/gi18n.h>
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
static void gnc_general_select_dispose      (GObject               *object);
static void gnc_general_select_finalize     (GObject               *object);

static GtkHBoxClass *parent_class;
static guint general_select_signals[LAST_SIGNAL];


/**
 * gnc_general_select_get_type:
 *
 * Returns the GtkType for the GNCGeneralSelect widget
 */
GType
gnc_general_select_get_type (void)
{
    static GType general_select_type = 0;

    if (general_select_type == 0)
    {
        static const GTypeInfo general_select_info =
        {
            sizeof (GNCGeneralSelectClass),
            NULL,
            NULL,
            (GClassInitFunc) gnc_general_select_class_init,
            NULL,
            NULL,
            sizeof (GNCGeneralSelect),
            0,
            (GInstanceInitFunc) gnc_general_select_init,
            NULL,
        };

        general_select_type = g_type_register_static(GTK_TYPE_HBOX,
                              "GNCGeneralSelect",
                              &general_select_info, 0);
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
    GObjectClass *object_class = (GObjectClass *) klass;
    GtkContainerClass *container_class = (GtkContainerClass *) klass;

    object_class = (GObjectClass*) klass;

    parent_class = g_type_class_ref(GTK_TYPE_HBOX);

    general_select_signals[SELECTION_CHANGED] =
        g_signal_new("changed",
                     G_TYPE_FROM_CLASS(object_class),
                     G_SIGNAL_RUN_FIRST,
                     G_STRUCT_OFFSET(GNCGeneralSelectClass,
                                     changed),
                     NULL, NULL,
                     g_cclosure_marshal_VOID__VOID,
                     G_TYPE_NONE, 0);

    container_class->forall = gnc_general_select_forall;

    object_class->dispose = gnc_general_select_dispose;
    object_class->finalize = gnc_general_select_finalize;

    klass->changed = NULL;
}

static void
gnc_general_select_init (GNCGeneralSelect *gsl)
{
    gsl->disposed = FALSE;
    gsl->selected_item = NULL;
}

static void
gnc_general_select_finalize (GObject *object)
{
    GNCGeneralSelect *gsl;

    g_return_if_fail (object != NULL);
    g_return_if_fail (GNC_IS_GENERAL_SELECT (object));

    gsl = GNC_GENERAL_SELECT (object);


    if (G_OBJECT_CLASS (parent_class)->finalize)
        G_OBJECT_CLASS (parent_class)->finalize (object);
}

static void
gnc_general_select_dispose (GObject *object)
{
    GNCGeneralSelect *gsl;

    g_return_if_fail (object != NULL);
    g_return_if_fail (GNC_IS_GENERAL_SELECT (object));

    gsl = GNC_GENERAL_SELECT (object);

    if (gsl->disposed)
        return;

    gsl->disposed = TRUE;


    gtk_widget_destroy(GTK_WIDGET(gsl->entry));
    gsl->entry = NULL;

    gtk_widget_destroy(GTK_WIDGET(gsl->button));
    gsl->button = NULL;


    if (G_OBJECT_CLASS (parent_class)->dispose)
        G_OBJECT_CLASS (parent_class)->dispose (object);
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
    gtk_editable_set_editable (GTK_EDITABLE (gsl->entry), FALSE);
    gtk_box_pack_start (GTK_BOX (gsl), gsl->entry, TRUE, TRUE, 0);
    gtk_widget_show (gsl->entry);

    if (type == GNC_GENERAL_SELECT_TYPE_SELECT)
        gsl->button = gtk_button_new_with_label (_("Select..."));
    else if (type == GNC_GENERAL_SELECT_TYPE_EDIT)
        gsl->button = gtk_button_new_with_label (_("Edit..."));
    else if (type == GNC_GENERAL_SELECT_TYPE_VIEW)
        gsl->button = gtk_button_new_with_label (_("View..."));

    gtk_box_pack_start (GTK_BOX (gsl), gsl->button, FALSE, FALSE, 0);
    g_signal_connect (G_OBJECT (gsl->button), "clicked",
                      G_CALLBACK (select_cb), gsl);
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

    gsl = g_object_new(GNC_TYPE_GENERAL_SELECT, NULL, NULL);

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

    g_signal_emit(gsl, general_select_signals[SELECTION_CHANGED], 0);
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

/** Sets the editable field from a general selection widget as the
 *  target for the specified label's access key.
 *
 *  @param gde The date editor to set as the target.
 *
 *
 *  @param label The label whose access key should set focus to this
 *  widget. */
void
gnc_general_select_make_mnemonic_target (GNCGeneralSelect *gsl, GtkWidget *label)
{
    g_return_if_fail(gsl);
    g_return_if_fail(GNC_IS_GENERAL_SELECT(gsl));
    g_return_if_fail(label);

    gtk_label_set_mnemonic_widget (GTK_LABEL(label), gsl->entry);
}

/*
  Local Variables:
  c-basic-offset: 8
  End:
*/
