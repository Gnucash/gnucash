/*
 * gnc-datedelta.c -- Date delta widget
 *
 * Copyright (C) 2000 Free Software Foundation
 * All rights reserved.
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

/*
 * Date delta widget
 *
 * Author: Dave Peticolas <peticola@cs.ucdavis.edu>
 */

#include "config.h"

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include <string.h>
#include <stdlib.h> /* atoi */
#include <ctype.h> /* isdigit */
#include <stdio.h>

#include "gnc-date.h"
#include "gnc-date-delta.h"

#define GDD_LABEL "gdd"

enum
{
    VALUE_CHANGED,
    UNITS_CHANGED,
    POLARITY_CHANGED,
    DELTA_CHANGED,
    LAST_SIGNAL
};

static guint date_delta_signals [LAST_SIGNAL] = { 0 };


static void gnc_date_delta_init       (GNCDateDelta      *gdd);
static void gnc_date_delta_class_init (GNCDateDeltaClass *class);
static void gnc_date_delta_forall     (GtkContainer      *container,
                                       gboolean	          include_internals,
                                       GtkCallback	  callback,
                                       gpointer	          callbabck_data);

static GtkHBoxClass *parent_class;

/**
 * gnc_date_delta_get_type:
 *
 * Returns the GtkType for the GNCDateDelta widget
 */
GType
gnc_date_delta_get_type (void)
{
    static GType date_delta_type = 0;

    if (date_delta_type == 0)
    {
        GTypeInfo date_delta_info =
        {
            sizeof (GNCDateDeltaClass),
            NULL,
            NULL,
            (GClassInitFunc) gnc_date_delta_class_init,
            NULL,
            NULL,
            sizeof (GNCDateDelta),
            0,
            (GInstanceInitFunc) gnc_date_delta_init
        };

        date_delta_type = g_type_register_static (gtk_hbox_get_type (),
                          "GNCDateDelta",
                          &date_delta_info,
                          0);
    }

    return date_delta_type;
}

static void
gnc_date_delta_class_init (GNCDateDeltaClass *klass)
{
    GObjectClass *object_class = G_OBJECT_CLASS (klass);
    GtkContainerClass *container_class = GTK_CONTAINER_CLASS (klass);

    parent_class = g_type_class_peek_parent (klass);

    date_delta_signals [VALUE_CHANGED] =
        g_signal_new ("value_changed",
                      G_OBJECT_CLASS_TYPE (object_class),
                      G_SIGNAL_RUN_FIRST,
                      G_STRUCT_OFFSET (GNCDateDeltaClass, value_changed),
                      NULL,
                      NULL,
                      g_cclosure_marshal_VOID__VOID,
                      G_TYPE_NONE,
                      0);

    date_delta_signals [UNITS_CHANGED] =
        g_signal_new ("units_changed",
                      G_OBJECT_CLASS_TYPE (object_class),
                      G_SIGNAL_RUN_FIRST,
                      G_STRUCT_OFFSET (GNCDateDeltaClass, units_changed),
                      NULL,
                      NULL,
                      g_cclosure_marshal_VOID__VOID,
                      G_TYPE_NONE,
                      0);

    date_delta_signals [POLARITY_CHANGED] =
        g_signal_new ("polarity_changed",
                      G_OBJECT_CLASS_TYPE (object_class),
                      G_SIGNAL_RUN_FIRST,
                      G_STRUCT_OFFSET (GNCDateDeltaClass, polarity_changed),
                      NULL,
                      NULL,
                      g_cclosure_marshal_VOID__VOID,
                      G_TYPE_NONE,
                      0);

    date_delta_signals [DELTA_CHANGED] =
        g_signal_new ("delta_changed",
                      G_OBJECT_CLASS_TYPE (object_class),
                      G_SIGNAL_RUN_FIRST,
                      G_STRUCT_OFFSET (GNCDateDeltaClass, delta_changed),
                      NULL,
                      NULL,
                      g_cclosure_marshal_VOID__VOID,
                      G_TYPE_NONE,
                      0);

    container_class->forall = gnc_date_delta_forall;
}

static void
gnc_date_delta_init (GNCDateDelta *gdd)
{
    gdd->value_spin = NULL;
    gdd->units_combo = NULL;
    gdd->polarity_combo = NULL;

    gdd->units = 0;
    gdd->polarity = 0;

    gdd->show_polarity = FALSE;
}

static void
gnc_date_delta_forall (GtkContainer *container, gboolean include_internals,
                       GtkCallback callback, gpointer callback_data)
{
    g_return_if_fail (container != NULL);
    g_return_if_fail (GNC_IS_DATE_DELTA (container));
    g_return_if_fail (callback != NULL);

    /* Let GtkBox handle things only if the internal widgets need to be
     * poked.
     */
    if (include_internals)
        if (GTK_CONTAINER_CLASS (parent_class)->forall)
            (* GTK_CONTAINER_CLASS (parent_class)->forall)
            (container, include_internals, callback, callback_data);
}

static void
value_changed(GtkEditable *editable, gpointer data)
{
    GNCDateDelta *gdd = GNC_DATE_DELTA(data);

    g_signal_emit(gdd, date_delta_signals [VALUE_CHANGED], 0);
    g_signal_emit(gdd, date_delta_signals [DELTA_CHANGED], 0);
}

static void
set_units (GtkComboBox *combo, GNCDateDelta *gdd)
{
    gint active;

    active = gtk_combo_box_get_active(GTK_COMBO_BOX(gdd->units_combo));
    if ((active < GNC_DATE_DELTA_DAYS) || (active > GNC_DATE_DELTA_YEARS))
        active = GNC_DATE_DELTA_DAYS;
    gdd->units = active;

    g_signal_emit (gdd, date_delta_signals [UNITS_CHANGED], 0);
    g_signal_emit (gdd, date_delta_signals [DELTA_CHANGED], 0);
}

static void
fill_units_combo(GNCDateDelta *gdd)
{
    gtk_combo_box_append_text(GTK_COMBO_BOX(gdd->units_combo),
                              _("Days"));
    gtk_combo_box_append_text(GTK_COMBO_BOX(gdd->units_combo),
                              _("Weeks"));
    gtk_combo_box_append_text(GTK_COMBO_BOX(gdd->units_combo),
                              _("Months"));
    gtk_combo_box_append_text(GTK_COMBO_BOX(gdd->units_combo),
                              _("Years"));

    g_signal_connect (gdd->units_combo, "changed",
                      G_CALLBACK (set_units), gdd);
}

static void
set_polarity (GtkComboBox *combo, GNCDateDelta *gdd)
{
    gint active;

    active = gtk_combo_box_get_active(GTK_COMBO_BOX(gdd->units_combo));
    if ((active < GNC_DATE_DELTA_PAST) || (active > GNC_DATE_DELTA_FUTURE))
        active = GNC_DATE_DELTA_PAST;
    gdd->polarity = active;

    g_signal_emit (gdd, date_delta_signals [POLARITY_CHANGED], 0);
    g_signal_emit (gdd, date_delta_signals [DELTA_CHANGED], 0);
}

static void
fill_polarity_combo(GNCDateDelta *gdd)
{
    gtk_combo_box_append_text(GTK_COMBO_BOX(gdd->polarity_combo),
                              _("Ago"));
    gtk_combo_box_append_text(GTK_COMBO_BOX(gdd->polarity_combo),
                              _("From Now"));

    g_signal_connect (gdd->polarity_combo, "changed",
                      G_CALLBACK(set_polarity), gdd);
}

static void
create_children (GNCDateDelta *gdd)
{
    GtkObject *adj;

    adj = gtk_adjustment_new(1.0, 1.0, 1000.0, 1.0, 5.0, 5.0);
    gdd->value_spin = gtk_spin_button_new(GTK_ADJUSTMENT(adj), 1.0, 0);
    gtk_spin_button_set_numeric(GTK_SPIN_BUTTON(gdd->value_spin), TRUE);
    gtk_box_pack_start(GTK_BOX(gdd), gdd->value_spin, FALSE, FALSE, 0);
    gtk_widget_show(gdd->value_spin);

    g_signal_connect(gdd->value_spin, "changed",
                     G_CALLBACK(value_changed), gdd);

    gdd->units_combo = gtk_combo_box_new_text();
    fill_units_combo(gdd);
    gtk_combo_box_set_active(GTK_COMBO_BOX(gdd->units_combo), 0);
    gtk_box_pack_start(GTK_BOX(gdd), gdd->units_combo, FALSE, FALSE, 0);
    gtk_widget_show(gdd->units_combo);

    gdd->polarity_combo = gtk_combo_box_new_text();
    fill_polarity_combo(gdd);
    gtk_combo_box_set_active(GTK_COMBO_BOX(gdd->polarity_combo), 0);
    gtk_box_pack_start(GTK_BOX(gdd), gdd->polarity_combo, FALSE, FALSE, 0);
    if (gdd->show_polarity)
        gtk_widget_show(gdd->polarity_combo);
}

/**
 * gnc_date_delta_new:
 * @show_polarity: whether 'from now/ago' menu should be displayed.
 *
 * Creates a new GNCDateDelta widget which can be used to provide
 * an easy to use way for entering time deltas in terms of 7 days,
 * 5 weeks, 2 months, etc.
 *
 * Returns a GNCDateDelta widget.
 */
GtkWidget *
gnc_date_delta_new (gboolean show_polarity)
{
    GNCDateDelta *gdd;

    gdd = g_object_new (gnc_date_delta_get_type (), NULL);

    gdd->show_polarity = show_polarity;

    create_children (gdd);

    return GTK_WIDGET (gdd);
}

/**
 * gnc_date_delta_set_value:
 * @gdd: The GNCDateDelta widget
 * @value: The value to set
 *
 * Changes the value of the delta widget to that given.
 */
void
gnc_date_delta_set_value (GNCDateDelta *gdd, int value)
{
    g_return_if_fail(gdd != NULL);
    g_return_if_fail(GNC_IS_DATE_DELTA(gdd));

    gtk_spin_button_set_value(GTK_SPIN_BUTTON(gdd->value_spin), value);
}

/**
 * gnc_date_delta_get_value:
 * @gdd: The GNCDateDelta widget
 *
 * Returns the value of the delta widget.
 */
int
gnc_date_delta_get_value (GNCDateDelta *gdd)
{
    g_return_val_if_fail(gdd != NULL, 0);
    g_return_val_if_fail(GNC_IS_DATE_DELTA(gdd), 0);

    return gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(gdd->value_spin));
}

/**
 * gnc_date_delta_set_units:
 * @gdd: The GNCDateDelta widget
 * @units: The units to set
 *
 * Changes the units of the delta widget to that given.
 */
void
gnc_date_delta_set_units (GNCDateDelta *gdd, GNCDateDeltaUnits units)
{
    g_return_if_fail(gdd != NULL);
    g_return_if_fail(GNC_IS_DATE_DELTA(gdd));
    g_return_if_fail((units >= 0) && (units < GNC_DATE_DELTA_NUM_UNITS));

    gdd->units = units;

    gtk_combo_box_set_active(GTK_COMBO_BOX(gdd->units_combo), units);
}

/**
 * gnc_date_delta_get_units:
 * @gdd: The GNCDateDelta widget
 *
 * Returns the units of the delta widget.
 */
GNCDateDeltaUnits
gnc_date_delta_get_units (GNCDateDelta *gdd)
{
    g_return_val_if_fail(gdd != NULL, GNC_DATE_DELTA_DAYS);
    g_return_val_if_fail(GNC_IS_DATE_DELTA(gdd), GNC_DATE_DELTA_DAYS);

    return gdd->units;
}

/**
 * gnc_date_delta_set_polarity:
 * @gdd: The GNCDateDelta widget
 * @polarity: The polarity to set
 *
 * Changes the polarity of the delta widget to that given.
 */
void
gnc_date_delta_set_polarity (GNCDateDelta *gdd, GNCDateDeltaPolarity polarity)
{
    g_return_if_fail(gdd != NULL);
    g_return_if_fail(GNC_IS_DATE_DELTA(gdd));
    g_return_if_fail((polarity >= 0) &&
                     (polarity < GNC_DATE_DELTA_NUM_POLARITY));

    gdd->polarity = polarity;

    gtk_combo_box_set_active(GTK_COMBO_BOX(gdd->polarity_combo), polarity);
}

/**
 * gnc_date_delta_get_polarity:
 * @gdd: The GNCDateDelta widget
 *
 * Returns the polarity of the delta widget.
 */
GNCDateDeltaPolarity
gnc_date_delta_get_polarity (GNCDateDelta *gdd)
{
    g_return_val_if_fail(gdd != NULL, GNC_DATE_DELTA_PAST);
    g_return_val_if_fail(GNC_IS_DATE_DELTA(gdd), GNC_DATE_DELTA_PAST);

    return gdd->polarity;
}

/**
 * gnc_date_delta_show_polarity:
 * @gdd: The GNCDateDelta widget
 * @show_polarity: boolean flag
 *
 * Show/hide the polarity menu.
 */
void
gnc_date_delta_show_polarity (GNCDateDelta *gdd, gboolean show_polarity)
{
    g_return_if_fail(gdd != NULL);
    g_return_if_fail(GNC_IS_DATE_DELTA(gdd));

    gdd->show_polarity = show_polarity;

    if (show_polarity)
        gtk_widget_show(gdd->polarity_combo);
    else
        gtk_widget_hide(gdd->polarity_combo);
}
