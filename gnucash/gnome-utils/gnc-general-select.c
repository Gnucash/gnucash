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

#include <config.h>

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include <string.h>
#include <ctype.h>
#include <stdio.h>

#include "gnc-general-select.h"
#include "dialog-utils.h"

/* Signal codes */
enum
{
    SELECTION_CHANGED,
    LAST_SIGNAL
};


static void gnc_general_select_dispose (GObject *object);
static void gnc_general_select_finalize (GObject *object);

static guint general_select_signals[LAST_SIGNAL];

typedef struct
{
    GWeakRef select;
    GCancellable *cancellable;
} GNCGeneralSelectRequest;

static void
select_request_free (GNCGeneralSelectRequest *request)
{
    g_weak_ref_clear (&request->select);
    g_clear_object (&request->cancellable);
    g_free (request);
}

static void
gnc_general_select_cancel_selection (GNCGeneralSelect *gsl)
{
    GCancellable *cancellable;

    cancellable = gsl->selection_cancellable;
    gsl->selection_cancellable = NULL;
    if (!cancellable)
        return;

    g_cancellable_cancel (cancellable);
    g_object_unref (cancellable);
}

static void
gnc_general_select_selection_done (gpointer selection, gpointer user_data)
{
    GNCGeneralSelectRequest *request = user_data;
    GNCGeneralSelect *gsl = g_weak_ref_get (&request->select);

    if (gsl && !gsl->disposed &&
        gsl->selection_cancellable == request->cancellable)
    {
        g_clear_object (&gsl->selection_cancellable);
        if (!g_cancellable_is_cancelled (request->cancellable) && selection)
            gnc_general_select_set_selected (gsl, selection);
    }

    g_clear_object (&gsl);
    select_request_free (request);
}

G_DEFINE_TYPE (GNCGeneralSelect, gnc_general_select, GTK_TYPE_BOX)

static void
gnc_general_select_class_init (GNCGeneralSelectClass *klass)
{
    GObjectClass *object_class = (GObjectClass *) klass;
    object_class = (GObjectClass*) klass;

    general_select_signals[SELECTION_CHANGED] =
        g_signal_new("changed",
                     G_TYPE_FROM_CLASS(object_class),
                     G_SIGNAL_RUN_FIRST,
                     G_STRUCT_OFFSET(GNCGeneralSelectClass,
                                     changed),
                     NULL, NULL,
                     g_cclosure_marshal_VOID__VOID,
                     G_TYPE_NONE, 0);

    object_class->dispose = gnc_general_select_dispose;
    object_class->finalize = gnc_general_select_finalize;

    klass->changed = NULL;
}

static void
gnc_general_select_init (GNCGeneralSelect *gsl)
{
    gtk_orientable_set_orientation (GTK_ORIENTABLE(gsl), GTK_ORIENTATION_HORIZONTAL);

    // Set the name for this widget so it can be easily manipulated with css
    gtk_widget_set_name (GTK_WIDGET(gsl), "gnc-id-general-select");

    gsl->disposed = FALSE;
    gsl->selected_item = NULL;
    gsl->selection_cancellable = NULL;
}

static void
gnc_general_select_finalize (GObject *object)
{
    g_return_if_fail (object != NULL);
    g_return_if_fail (GNC_IS_GENERAL_SELECT(object));

    G_OBJECT_CLASS(gnc_general_select_parent_class)->finalize (object);
}

static void
gnc_general_select_dispose (GObject *object)
{
    GNCGeneralSelect *gsl;

    g_return_if_fail (object != NULL);
    g_return_if_fail (GNC_IS_GENERAL_SELECT(object));

    gsl = GNC_GENERAL_SELECT(object);

    if (gsl->disposed)
        return;

    gsl->disposed = TRUE;
    gnc_general_select_cancel_selection (gsl);

    gtk_box_remove (GTK_BOX(gsl), GTK_WIDGET(gsl->entry));
    gsl->entry = NULL;

    gtk_box_remove (GTK_BOX(gsl), GTK_WIDGET(gsl->button));
    gsl->button = NULL;

    G_OBJECT_CLASS(gnc_general_select_parent_class)->dispose (object);
}

static void
select_cb (GtkButton *button, gpointer user_data)
{
    GNCGeneralSelect *gsl = user_data;
    GNCGeneralSelectRequest *request;
    GtkRoot *root;
    GtkWidget *parent = NULL;

    if (gsl->disposed)
        return;

    root = gtk_widget_get_root (GTK_WIDGET (button));
    if (root && GTK_IS_WIDGET (root))
        parent = GTK_WIDGET (root);

    gnc_general_select_cancel_selection (gsl);
    gsl->selection_cancellable = g_cancellable_new ();

    request = g_new0 (GNCGeneralSelectRequest, 1);
    g_weak_ref_init (&request->select, gsl);
    request->cancellable = g_object_ref (gsl->selection_cancellable);

    gsl->new_select (gsl->cb_arg, gsl->selected_item, parent,
                     gsl->selection_cancellable,
                     gnc_general_select_selection_done, request);
}

static void
create_children (GNCGeneralSelect *gsl, GNCGeneralSelectType type)
{
    gsl->entry = gtk_entry_new ();
    gtk_editable_set_editable (GTK_EDITABLE(gsl->entry), FALSE);
    gtk_box_append (GTK_BOX(gsl), GTK_WIDGET(gsl->entry));
    gtk_widget_set_visible (GTK_WIDGET(gsl->entry), TRUE);
    gtk_widget_set_hexpand (GTK_WIDGET(gsl->entry), TRUE);

    if (type == GNC_GENERAL_SELECT_TYPE_SELECT)
        gsl->button = gtk_button_new_with_label (_("Select…"));
    else if (type == GNC_GENERAL_SELECT_TYPE_EDIT)
        gsl->button = gtk_button_new_with_label (_("Edit…"));
    else if (type == GNC_GENERAL_SELECT_TYPE_VIEW)
        gsl->button = gtk_button_new_with_label (_("View…"));

    gtk_box_append (GTK_BOX(gsl), GTK_WIDGET(gsl->button));
    g_signal_connect (G_OBJECT(gsl->button), "clicked",
                      G_CALLBACK(select_cb), gsl);
    gtk_widget_set_visible (GTK_WIDGET(gsl->button), TRUE);
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

    gsl = g_object_new (GNC_TYPE_GENERAL_SELECT, NULL, NULL);

    create_children (gsl, type);
    gsl->get_string = get_string;
    gsl->new_select = new_select;
    gsl->cb_arg = cb_arg;

    return GTK_WIDGET(gsl);
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

    g_return_if_fail (gsl != NULL);
    g_return_if_fail (GNC_IS_GENERAL_SELECT (gsl));

    gnc_general_select_cancel_selection (gsl);
    gsl->selected_item = selection;

    if (selection == NULL)
        text = "";
    else
        text = gnc_general_select_get_printname (gsl, selection);

    gnc_entry_set_text (GTK_ENTRY(gsl->entry), text);

    g_signal_emit (gsl, general_select_signals[SELECTION_CHANGED], 0);
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
    g_return_val_if_fail (gsl != NULL, NULL);
    g_return_val_if_fail (GNC_IS_GENERAL_SELECT(gsl), NULL);

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
    g_return_if_fail (gsl);
    g_return_if_fail (GNC_IS_GENERAL_SELECT(gsl));
    g_return_if_fail (label);

    gtk_label_set_mnemonic_widget (GTK_LABEL(label), gsl->entry);
}
