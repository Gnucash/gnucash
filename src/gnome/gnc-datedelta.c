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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
 * Boston, MA  02111-1307,  USA       gnu@gnu.org
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

#include <config.h>
#include <string.h>
#include <stdlib.h> /* atoi */
#include <ctype.h> /* isdigit */
#include <stdio.h>

#include "gnc-datedelta.h"
#include "messages.h"
#include "date.h"


enum
{
  VALUE_CHANGED,
  UNITS_CHANGED,
  POLARITY_CHANGED,
  DELTA_CHANGED,
  LAST_SIGNAL
};

static gint date_delta_signals [LAST_SIGNAL] = { 0 };


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
guint
gnc_date_delta_get_type (void)
{
  static guint date_delta_type = 0;

  if (!date_delta_type){
    GtkTypeInfo date_delta_info = {
      "GNCDateDelta",
      sizeof (GNCDateDelta),
      sizeof (GNCDateDeltaClass),
      (GtkClassInitFunc) gnc_date_delta_class_init,
      (GtkObjectInitFunc) gnc_date_delta_init,
      NULL,
      NULL,
    };

    date_delta_type = gtk_type_unique (gtk_hbox_get_type (),
                                       &date_delta_info);
  }
	
  return date_delta_type;
}

static void
gnc_date_delta_class_init (GNCDateDeltaClass *class)
{
  GtkObjectClass *object_class = (GtkObjectClass *) class;
  GtkContainerClass *container_class = (GtkContainerClass *) class;

  object_class = (GtkObjectClass*) class;

  parent_class = gtk_type_class (gtk_hbox_get_type ());

  date_delta_signals [VALUE_CHANGED] =
    gtk_signal_new ("value_changed",
                    GTK_RUN_FIRST, object_class->type, 
                    GTK_SIGNAL_OFFSET (GNCDateDeltaClass,
                                       value_changed),
                    gtk_signal_default_marshaller,
                    GTK_TYPE_NONE, 0);

  date_delta_signals [UNITS_CHANGED] =
    gtk_signal_new ("units_changed",
                    GTK_RUN_FIRST, object_class->type, 
                    GTK_SIGNAL_OFFSET (GNCDateDeltaClass,
                                       units_changed),
                    gtk_signal_default_marshaller,
                    GTK_TYPE_NONE, 0);

  date_delta_signals [POLARITY_CHANGED] =
    gtk_signal_new ("polarity_changed",
                    GTK_RUN_FIRST, object_class->type, 
                    GTK_SIGNAL_OFFSET (GNCDateDeltaClass,
                                       polarity_changed),
                    gtk_signal_default_marshaller,
                    GTK_TYPE_NONE, 0);

  date_delta_signals [DELTA_CHANGED] =
    gtk_signal_new ("delta_changed",
                    GTK_RUN_FIRST, object_class->type, 
                    GTK_SIGNAL_OFFSET (GNCDateDeltaClass,
                                       delta_changed),
                    gtk_signal_default_marshaller,
                    GTK_TYPE_NONE, 0);

  gtk_object_class_add_signals (object_class, date_delta_signals,
                                LAST_SIGNAL);

  container_class->forall = gnc_date_delta_forall;

  class->value_changed    = NULL;
  class->units_changed    = NULL;
  class->polarity_changed = NULL;
  class->delta_changed    = NULL;
}

static void
gnc_date_delta_init (GNCDateDelta *gdd)
{
  gdd->value_spin = NULL;
  gdd->units_menu = NULL;
  gdd->polarity_menu = NULL;

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

  gtk_signal_emit(GTK_OBJECT (gdd), date_delta_signals [VALUE_CHANGED]);
  gtk_signal_emit(GTK_OBJECT (gdd), date_delta_signals [DELTA_CHANGED]);
}

static void
set_units (GtkWidget *widget, gpointer data)
{
  GNCDateDeltaUnits units;
  GNCDateDelta *gdd;

  units = GPOINTER_TO_INT(data);
  gdd = GNC_DATE_DELTA(gtk_object_get_user_data(GTK_OBJECT(widget)));

  gdd->units = units;

  gtk_signal_emit (GTK_OBJECT (gdd), date_delta_signals [UNITS_CHANGED]);
  gtk_signal_emit (GTK_OBJECT (gdd), date_delta_signals [DELTA_CHANGED]);
}

static void
fill_units_menu(GNCDateDelta *gdd)
{
  GtkWidget *menu;
  GtkWidget *item;
  char *strings[] = {
    _("Days"),
    _("Weeks"),
    _("Months"),
    _("Years"),
    NULL
  };
  gint i;

  menu = gtk_menu_new ();
  gtk_widget_show(menu);

  for (i = 0; strings[i] != NULL; i++)
  {
    item = gtk_menu_item_new_with_label (strings[i]);
    gtk_object_set_user_data(GTK_OBJECT(item), gdd);
    gtk_menu_append (GTK_MENU (menu), item);
    gtk_widget_show(item);

    gtk_signal_connect (GTK_OBJECT (item), "activate",
                        GTK_SIGNAL_FUNC (set_units), GINT_TO_POINTER(i));
  }

  gtk_option_menu_set_menu (GTK_OPTION_MENU (gdd->units_menu), menu);
}

static void
set_polarity (GtkWidget *widget, gpointer data)
{
  GNCDateDeltaPolarity polarity;
  GNCDateDelta *gdd;

  polarity = GPOINTER_TO_INT(data);
  gdd = GNC_DATE_DELTA(gtk_object_get_user_data(GTK_OBJECT(widget)));

  gdd->polarity = polarity;

  gtk_signal_emit (GTK_OBJECT (gdd), date_delta_signals [POLARITY_CHANGED]);
  gtk_signal_emit (GTK_OBJECT (gdd), date_delta_signals [DELTA_CHANGED]);
}

static void
fill_polarity_menu(GNCDateDelta *gdd)
{
  GtkWidget *menu;
  GtkWidget *item;
  char *strings[] = {
    _("Ago"),
    _("From Now"),
    NULL
  };
  gint i;

  menu = gtk_menu_new ();
  gtk_widget_show(menu);

  for (i = 0; strings[i] != NULL; i++)
  {
    item = gtk_menu_item_new_with_label (strings[i]);
    gtk_object_set_user_data(GTK_OBJECT(item), gdd);
    gtk_menu_append (GTK_MENU (menu), item);
    gtk_widget_show(item);

    gtk_signal_connect (GTK_OBJECT (item), "activate",
                        GTK_SIGNAL_FUNC (set_polarity), GINT_TO_POINTER(i));
  }

  gtk_option_menu_set_menu (GTK_OPTION_MENU (gdd->polarity_menu), menu);
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

  gtk_signal_connect(GTK_OBJECT(gdd->value_spin), "changed",
                     GTK_SIGNAL_FUNC(value_changed), gdd);

  gdd->units_menu = gtk_option_menu_new();
  fill_units_menu(gdd);
  gtk_option_menu_set_history(GTK_OPTION_MENU(gdd->units_menu), 0);
  gtk_box_pack_start(GTK_BOX(gdd), gdd->units_menu, FALSE, FALSE, 0);
  gtk_widget_show(gdd->units_menu);

  gdd->polarity_menu = gtk_option_menu_new();
  fill_polarity_menu(gdd);
  gtk_option_menu_set_history(GTK_OPTION_MENU(gdd->polarity_menu), 0);
  gtk_box_pack_start(GTK_BOX(gdd), gdd->polarity_menu, FALSE, FALSE, 0);
  if (gdd->show_polarity)
    gtk_widget_show(gdd->polarity_menu);
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

  gdd = gtk_type_new (gnc_date_delta_get_type ());

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

  gtk_option_menu_set_history(GTK_OPTION_MENU(gdd->units_menu), units);
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

  gtk_option_menu_set_history(GTK_OPTION_MENU(gdd->polarity_menu), polarity);
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
    gtk_widget_show(gdd->polarity_menu);
  else
    gtk_widget_hide(gdd->polarity_menu);
}
