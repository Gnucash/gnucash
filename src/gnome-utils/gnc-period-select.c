/*
 * gnc-period-select.c -- Accounting Period selection widget
 *
 * Copyright (c) 2005 David Hampton <hampton@employees.org>
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
 */

/** @addtogroup GUI
    @{ */
/** @file gnc-period-select.c
    @brief A custom widget for selecting accounting periods.
    @author David Hampton <hampton@employees.org>
*/

#include "config.h"

#include <gtk/gtk.h>
#include <glib/gi18n.h>

#include "gnc-date.h"
#include "gnc-gconf-utils.h"
#include "gnc-period-select.h"

enum {
  PROP_0,
  PROP_FY_END,
  PROP_SHOW_DATE,
  PROP_DATE_BASE,
};

enum {
  CHANGED,
  LAST_SIGNAL
};

static guint signals[LAST_SIGNAL] = { 0 };

/** Declarations *********************************************************/
static void gnc_period_select_init         (GncPeriodSelect      *gce);
static void gnc_period_select_class_init   (GncPeriodSelectClass *class);
static void gnc_period_select_finalize     (GObject *object);

static GtkComboBoxClass *parent_class;


const gchar *start_strings[GNC_ACCOUNTING_PERIOD_LAST] = {
  /* CY Strings */
  N_("Today"),
  N_("Start of this month"),
  N_("Start of previous month"),
  N_("Start of this quarter"),
  N_("Start of previous quarter"),
  N_("Start of this year"),
  N_("Start of previous year"),

  /* FY Strings */
  N_("Start of this fiscal year"),
  N_("Start of previous fiscal year"),
};

const gchar *end_strings[GNC_ACCOUNTING_PERIOD_LAST] = {
  /* CY Strings */
  N_("Today"),
  N_("End of this month"),
  N_("End of previous month"),
  N_("End of this quarter"),
  N_("End of previous quarter"),
  N_("End of this year"),
  N_("End of previous year"),

  /* FY Strings */
  N_("End of this fiscal year"),
  N_("End of previous fiscal year"),
};


/** Private Data Structure ***********************************************/

typedef struct _GncPeriodSelectPrivate GncPeriodSelectPrivate;
struct _GncPeriodSelectPrivate
{
  GtkWidget *selector;

  gboolean start;
  GDate *fy_end;

  GDate     *date_base;
  GtkWidget *date_label;
  GtkWidget *date_align;
};

#define GNC_PERIOD_SELECT_GET_PRIVATE(o)  \
   (G_TYPE_INSTANCE_GET_PRIVATE ((o), GNC_TYPE_PERIOD_SELECT, GncPeriodSelectPrivate))


/************************************************************/
/*                    Signal Functions                      */
/************************************************************/

/*  Tells a GncPeriodSelect object to emit a "changed" signal.
 */
void
gnc_period_select_changed (GncPeriodSelect *period)
{
  g_return_if_fail(GNC_IS_PERIOD_SELECT(period));

  g_signal_emit(G_OBJECT(period), signals[CHANGED], 0);
}


/** Update the user visible sample date label if it exists on this
 *  widget.  This label is for user feedback only.
 *
 *  @param period The GncPeriodSelect object to update.
 */
static void
gnc_period_sample_update_date_label (GncPeriodSelect *period)
{
  GncPeriodSelectPrivate *priv;
  gchar *time_string;
  time_t secs;

  g_return_if_fail(GNC_IS_PERIOD_SELECT(period));
  priv = GNC_PERIOD_SELECT_GET_PRIVATE(period);
  if (!priv->date_label)
    return;
  secs = gnc_period_select_get_time(GNC_PERIOD_SELECT(period));
  time_string = qof_print_date(secs);
  gtk_label_set_label(GTK_LABEL(priv->date_label), time_string);
  g_free(time_string);
}


/** Handle the "changed" signal from the GtkComboBox that is embedded
 *  in this GncPeriodSelect object.  When called, this function
 *  updates the feedback label embedded in the object, then generates
 *  another "changed" signal so that a developer can see that
 *  something in the object changed.
 *
 *  @param box The combo box that changed.
 *
 *  @param period The GncPeriodSelect containing the combo box.
 */
static void
gnc_period_sample_combobox_changed (GtkComboBox *box, GncPeriodSelect *period)
{
  g_return_if_fail(GNC_IS_PERIOD_SELECT(period));

  /* Update this widget */
  gnc_period_sample_update_date_label(period);

  /* Pass it on... */
  gnc_period_select_changed(period);
}


/** Handle an application wide change in the date format.  This
 *  function will be called when the GConf key for the date format is
 *  updated.  It doesn't really care what the new format is, because
 *  the date string is generated elsewhere.  It just needs to know to
 *  update the date label so that it matches the newly selected format.
 *
 *  @param unused The new value for the Gnucash date format.
 *
 *  @param period The GncPeriodSelect that needs to be updated.
 */
static void
gnc_period_sample_new_date_format (GConfEntry *unused,
				   GncPeriodSelect *period)
{
  gnc_period_sample_update_date_label(period);
}


/************************************************************/
/*                   Property Functions                     */
/************************************************************/

/** @name GncPeriodSelect Properties
 @{ */

/*  Get the current value of the fiscal year end setting from a
 *  GncPeriodSelect widget.  If the result is NULL then fiscal years
 *  are not currently supported.
 */
GDate *
gnc_period_select_get_fy_end (GncPeriodSelect *period)
{
  GncPeriodSelectPrivate *priv;
  priv = GNC_PERIOD_SELECT_GET_PRIVATE(period);

  g_return_val_if_fail(period != NULL, NULL);
  g_return_val_if_fail(GNC_IS_PERIOD_SELECT(period), NULL);

  priv = GNC_PERIOD_SELECT_GET_PRIVATE(period);
  if (!priv->fy_end)
    return NULL;
  return g_date_new_dmy(g_date_get_day(priv->fy_end),
			g_date_get_month(priv->fy_end),
			G_DATE_BAD_YEAR);
}


/*  Set the fiscal year end on a GncPeriodSelect widget.  If set to a
 *  value other than NULL then widget will include fiscal accounting
 *  period like "this fiscal year".
 */
void
gnc_period_select_set_fy_end (GncPeriodSelect *period, const GDate *fy_end)
{
  GncPeriodSelectPrivate *priv;
  const gchar *label;
  gint i;

  g_return_if_fail(period != NULL);
  g_return_if_fail(GNC_IS_PERIOD_SELECT(period));

  priv = GNC_PERIOD_SELECT_GET_PRIVATE(period);
  if (priv->fy_end)
    g_date_free(priv->fy_end);

  if (fy_end) {
    priv->fy_end = g_date_new_dmy(g_date_get_day(fy_end),
				  g_date_get_month(fy_end),
				  G_DATE_BAD_YEAR);
  } else {
    priv->fy_end = NULL;
  }

  if (fy_end) {
    for (i = GNC_ACCOUNTING_PERIOD_CYEAR_LAST; i < GNC_ACCOUNTING_PERIOD_FYEAR_LAST; i++) {
      label = priv->start ? _(start_strings[i]) : _(end_strings[i]);
      gtk_combo_box_append_text(GTK_COMBO_BOX(priv->selector), label);
    }
  } else {
    for (i = GNC_ACCOUNTING_PERIOD_FYEAR_LAST - 1; i >= GNC_ACCOUNTING_PERIOD_FYEAR_LAST; i--) {
      gtk_combo_box_remove_text(GTK_COMBO_BOX(priv->selector), i);
    }
  }
}


static void
gnc_period_select_set_date_common (GncPeriodSelect *period, const GDate *date)
{
  GncPeriodSelectPrivate *priv;

  priv = GNC_PERIOD_SELECT_GET_PRIVATE(period);
  if (date) {
    if (priv->date_base)
      g_date_free(priv->date_base);
    priv->date_base = g_date_new_dmy(g_date_get_day(date),
				     g_date_get_month(date),
				     g_date_get_year(date));
    if (priv->date_label == NULL) {
      priv->date_align = gtk_alignment_new(0.5, 0.5, 0, 0);
      gtk_alignment_set_padding(GTK_ALIGNMENT(priv->date_align), 0, 0, 6, 0);
      gtk_box_pack_start_defaults(GTK_BOX(period), priv->date_align);
      priv->date_label = gtk_label_new("");
      gtk_container_add(GTK_CONTAINER(priv->date_align), priv->date_label);
      gtk_widget_show_all(priv->date_align);
    }
    gnc_period_sample_update_date_label(period);
    return;
  }

  if (priv->date_base) {
    g_date_free(priv->date_base);
    priv->date_base = NULL;
    gtk_widget_destroy(priv->date_align);
    priv->date_align = NULL;
    priv->date_label = NULL;
  }
}


/*  Get the current value of the "show date" setting from a
 *  GncPeriodSelect widget.
 */
gboolean
gnc_period_select_get_show_date (GncPeriodSelect *period)
{
  GncPeriodSelectPrivate *priv;

  g_return_val_if_fail(period != NULL, FALSE);
  g_return_val_if_fail(GNC_IS_PERIOD_SELECT(period), FALSE);

  priv = GNC_PERIOD_SELECT_GET_PRIVATE(period);
  return (priv->date_base != NULL);
}

/*  Set the "show date" setting on a GncPeriodSelect widget.  If set
 *  to TRUE then a GtkLabel will be used to show the date
 *  corresponding to the selected time period.
 */
void
gnc_period_select_set_show_date (GncPeriodSelect *period, const gboolean show_date)
{
  GDate date;

  g_return_if_fail(period != NULL);
  g_return_if_fail(GNC_IS_PERIOD_SELECT(period));

  if (show_date) {
    g_date_clear(&date, 1);
    g_date_set_time_t(&date, time (NULL));
    gnc_period_select_set_date_common(period, &date);
  } else {
    gnc_period_select_set_date_common(period, NULL);
  }
}


GDate *
gnc_period_select_get_date_base (GncPeriodSelect *period)
{
  GncPeriodSelectPrivate *priv;

  g_return_val_if_fail(period != NULL, NULL);
  g_return_val_if_fail(GNC_IS_PERIOD_SELECT(period), NULL);

  priv = GNC_PERIOD_SELECT_GET_PRIVATE(period);
  if (!priv->date_base)
    return NULL;
  return g_date_new_dmy(g_date_get_day(priv->date_base),
			g_date_get_month(priv->date_base),
			g_date_get_year(priv->date_base));
}


/*  Set the base date used by a GncPeriodSelect widget.  All example
 *  dates presented by the widget will be computed from this date.
 */
void
gnc_period_select_set_date_base (GncPeriodSelect *period, const GDate *date_base)
{
  g_return_if_fail(period != NULL);
  g_return_if_fail(GNC_IS_PERIOD_SELECT(period));

  gnc_period_select_set_date_common(period, date_base);
}


/** Retrieve a property specific to this GncPeriodSelect object.  This is
 *  nothing more than a dispatch function for routines that can be
 *  called directly.  It has the nice feature of allowing a single
 *  function call to retrieve multiple properties.
 *
 *  @internal
 */
static void
gnc_period_select_get_property (GObject     *object,
			       guint        prop_id,
			       GValue      *value,
			       GParamSpec  *pspec)
{
  GncPeriodSelect *period = GNC_PERIOD_SELECT(object);

  switch (prop_id)
    {
    case PROP_FY_END:
      g_value_set_pointer(value, gnc_period_select_get_fy_end(period));
      break;
    case PROP_SHOW_DATE:
      g_value_set_boolean(value, gnc_period_select_get_show_date(period));
      break;
    case PROP_DATE_BASE:
      g_value_set_pointer(value, gnc_period_select_get_date_base(period));
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID(object, prop_id, pspec);
      break;
    }
}


/** Set a property specific to this GncPeriodSelect object.  This is
 *  nothing more than a dispatch function for routines that can be
 *  called directly.  It has the nice feature of allowing a new widget
 *  to be created with a varargs list specifying the properties,
 *  instead of having to explicitly call each property function.
 *
 *  @internal
 */
static void
gnc_period_select_set_property (GObject      *object,
			       guint         prop_id,
			       const GValue *value,
			       GParamSpec   *pspec)
{
  GncPeriodSelect *period = GNC_PERIOD_SELECT(object);

  switch (prop_id)
    {
    case PROP_FY_END:
      gnc_period_select_set_fy_end(period, g_value_get_pointer(value));
      break;
    case PROP_SHOW_DATE:
      gnc_period_select_set_show_date(period, g_value_get_boolean(value));
      break;
    case PROP_DATE_BASE:
      gnc_period_select_set_date_base(period, g_value_get_pointer(value));
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID(object, prop_id, pspec);
      break;
    }
}

/** @} */

/************************************************************/
/*                    Core Implementation                   */
/************************************************************/

/** @name GncPeriodSelect Core Implementation
 @{ */

/*  Returns the GType of a GncPeriodSelect widget.
 */
GType
gnc_period_select_get_type (void)
{
  static GType period_select_type = 0;

  if (period_select_type == 0) {
    static const GTypeInfo period_select_info = {
      sizeof (GncPeriodSelectClass),
      NULL,
      NULL,
      (GClassInitFunc) gnc_period_select_class_init,
      NULL,
      NULL,
      sizeof (GncPeriodSelect),
      0, /* n_preallocs */
      (GInstanceInitFunc) gnc_period_select_init,
      NULL
    };

    period_select_type = g_type_register_static(GTK_TYPE_HBOX,
					       "GncPeriodSelect",
					       &period_select_info, 0);
  }

  return period_select_type;
}


/** Initialize the class for the a Period Selection widget.  This
 *  will set up any function pointers that override functions in the
 *  parent class, and also installs the proprieties that are unique to
 *  this class.
 *
 *  @param klass The new class structure created by the object system.
 *
 *  @internal
 */
static void
gnc_period_select_class_init (GncPeriodSelectClass *klass)
{
  GObjectClass *gobject_class;

  parent_class = g_type_class_peek_parent(klass);

  gobject_class = G_OBJECT_CLASS(klass);
  gobject_class->set_property = gnc_period_select_set_property;
  gobject_class->get_property = gnc_period_select_get_property;
  gobject_class->finalize = gnc_period_select_finalize;


  signals[CHANGED] = g_signal_new("changed",
				  G_OBJECT_CLASS_TYPE (klass),
				  G_SIGNAL_RUN_FIRST,
				  G_STRUCT_OFFSET(GncPeriodSelectClass, changed),
				  NULL, NULL,
				  g_cclosure_marshal_VOID__VOID,
				  G_TYPE_NONE,
				  0);


  g_object_class_install_property(gobject_class,
				  PROP_FY_END,
				  g_param_spec_pointer("fy-end",
						       "Fiscal Year End",
						       "The fiscal year to use for this widget",
						       G_PARAM_READWRITE));
  g_object_class_install_property(gobject_class,
				  PROP_SHOW_DATE,
				  g_param_spec_boolean("show-date",
						       "Show Date",
						       "Show the start/end date of the accouting period in this widget",
						       FALSE,
						       G_PARAM_READWRITE));
  g_object_class_install_property(gobject_class,
				  PROP_DATE_BASE,
				  g_param_spec_pointer("date-base",
						       "Date Base",
						       "The starting date to use for display calculations",
						       G_PARAM_READWRITE));

  g_type_class_add_private(klass, sizeof(GncPeriodSelectPrivate));
}


/** Initialize a new instance of a gnucash accounting period selection
 *  widget.  This function allocates and initializes the object
 *  private storage space.
 *
 *  @param period The new object instance created by the object system.
 *
 *  @internal
 */
static void
gnc_period_select_init (GncPeriodSelect *period)
{
  GncPeriodSelectPrivate *priv;

  priv = GNC_PERIOD_SELECT_GET_PRIVATE(period);
  priv->start = TRUE;
}


/** Finalize the GncPeriodSelect object.  This function is called from
 *  the G_Object level to complete the destruction of the object.  It
 *  should release any memory not previously released by the destroy
 *  function (i.e. the private data structure), then chain up to the
 *  parent's destroy function.
 *
 *  @param object The object being destroyed.
 *
 *  @internal
 */
static void
gnc_period_select_finalize (GObject *object)
{
  GncPeriodSelectPrivate *priv;
  GncPeriodSelect *period;

  g_return_if_fail (object != NULL);
  g_return_if_fail (GNC_IS_PERIOD_SELECT (object));

  period = GNC_PERIOD_SELECT(object);
  priv = GNC_PERIOD_SELECT_GET_PRIVATE(period);

  /* Stop tracking changes to date formatting */
  gnc_gconf_general_remove_cb(KEY_DATE_FORMAT,
			      (GncGconfGeneralCb)gnc_period_sample_new_date_format, period);

  /* The selector and date_label were added to the hbox.  They will be
   * freed automatically. */
  if (priv->fy_end)
    g_date_free(priv->fy_end);
  if (priv->date_base)
    g_date_free(priv->date_base);

  /* Do not free the private data structure. It is part of a larger
   * memory block allocated by the type system. */

  if (G_OBJECT_CLASS(parent_class)->finalize)
    (* G_OBJECT_CLASS(parent_class)->finalize) (object);
}


/*  Create a new GncPeriodSelect widget which is used to select a
 *  accounting period like "previous month" or "this year".
 *
 *  @param starting_labels If set to TRUE then all the labels will
 *  refer to the "Start of...".  If FALSE, labels will refer to "End
 *  of...".
 *
 *  @return A GncPeriodSelect widget.
 */
GtkWidget *
gnc_period_select_new (gboolean starting_labels)
{
  GncPeriodSelectPrivate *priv;
  GncPeriodSelect *period;
  const gchar *label;
  gint i;

  period = g_object_new(GNC_TYPE_PERIOD_SELECT, NULL);

  /* Set up private data structures */
  priv = GNC_PERIOD_SELECT_GET_PRIVATE(period);
  priv->selector   = gtk_combo_box_new_text();
  priv->start      = starting_labels;

  /* Add the internal widgets to the hbox */
  gtk_box_pack_start_defaults(GTK_BOX(period), priv->selector);
  gtk_widget_show(priv->selector);

  /* Find out when the combo box changes */
  g_signal_connect(G_OBJECT(priv->selector), "changed",
		   G_CALLBACK(gnc_period_sample_combobox_changed), period);

  /* Build all the labels except the fiscal year labels */
  for (i = 0; i < GNC_ACCOUNTING_PERIOD_CYEAR_LAST; i++) {
    label = starting_labels ? _(start_strings[i]) : _(end_strings[i]);
    gtk_combo_box_append_text(GTK_COMBO_BOX(priv->selector), label);
  }

  /* Track changes to date formatting */
  gnc_gconf_general_register_cb(KEY_DATE_FORMAT,
				(GncGconfGeneralCb)gnc_period_sample_new_date_format, period);

  return GTK_WIDGET (period);
}


/*  Create a new GncPeriodSelect widget from a glade file.  The int1
 *  argument passed from glade is used to determine whether the widget
 *  uses labels for start times or end times.  A non-zero int2
 *  argument indicates that an example date should be shown.
 */
GtkWidget *
gnc_period_select_new_glade (gchar *widget_name,
			    gchar *string1, gchar *string2,
			    gint int1, gint int2)
{
  GtkWidget *widget;
  widget = gnc_period_select_new(int1 != 0);
  if (int2)
    gnc_period_select_set_show_date(GNC_PERIOD_SELECT(widget), TRUE);
  gtk_widget_show(widget);
  return widget;
}

/** @} */

/************************************************************/
/*                   Auxiliary Functions                    */
/************************************************************/


/*  Set which item in the GncPeriodSelect is initially selected.  This
 *  is used to set the initial selection before the widget is shown to
 *  the user.
 */
void
gnc_period_select_set_active (GncPeriodSelect *period,
			      GncAccountingPeriod which)
{
  GncPeriodSelectPrivate *priv;

  g_return_if_fail(period != NULL);
  g_return_if_fail(GNC_IS_PERIOD_SELECT(period));
  g_return_if_fail(which >= 0);
  g_return_if_fail(which <  GNC_ACCOUNTING_PERIOD_LAST);

  priv = GNC_PERIOD_SELECT_GET_PRIVATE(period);
  gtk_combo_box_set_active(GTK_COMBO_BOX(priv->selector), which);
}


/*  Get the currently selected accounting period from a
 *  GncPeriodSelect widget.  This is used to retrieve the user's
 *  selection in the form of an enum.
 */
GncAccountingPeriod
gnc_period_select_get_active (GncPeriodSelect *period)
{
  GncPeriodSelectPrivate *priv;

  g_return_val_if_fail(period != NULL, -1);
  g_return_val_if_fail(GNC_IS_PERIOD_SELECT(period), -1);

  priv = GNC_PERIOD_SELECT_GET_PRIVATE(period);
  return gtk_combo_box_get_active(GTK_COMBO_BOX(priv->selector));
}


/*  Get the currently selected accounting period choice from a
 *  GncPeriodSelect widget.  This is used to retrieve the user's
 *  selection in the form of a GDate.
 */
GDate *
gnc_period_select_get_date (GncPeriodSelect *period)
{
  GncPeriodSelectPrivate *priv;
  GncAccountingPeriod which;

  g_return_val_if_fail(period != NULL, 0);
  g_return_val_if_fail(GNC_IS_PERIOD_SELECT(period), 0);

  priv = GNC_PERIOD_SELECT_GET_PRIVATE(period);
  which = gtk_combo_box_get_active(GTK_COMBO_BOX(priv->selector));
  if (which == -1)
    return NULL;

  if (priv->start)
    return gnc_accounting_period_start_gdate(which, priv->fy_end,
					     priv->date_base);
  return gnc_accounting_period_end_gdate(which, priv->fy_end,
					 priv->date_base);
}


/*  Get the currently selected accounting period from a
 *  GncPeriodSelect widget.  This is used to retrieve the user's
 *  selection in the form of an timestamp.
 */
time_t
gnc_period_select_get_time (GncPeriodSelect *period)
{
  GncPeriodSelectPrivate *priv;
  GncAccountingPeriod which;

  g_return_val_if_fail(period != NULL, 0);
  g_return_val_if_fail(GNC_IS_PERIOD_SELECT(period), 0);

  priv = GNC_PERIOD_SELECT_GET_PRIVATE(period);
  which = gtk_combo_box_get_active(GTK_COMBO_BOX(priv->selector));
  if (which == -1)
    return 0;

  if (priv->start)
    return gnc_accounting_period_start_timet(which, priv->fy_end,
					     priv->date_base);
  return gnc_accounting_period_end_timet(which, priv->fy_end,
					 priv->date_base);
}

/** @} */
