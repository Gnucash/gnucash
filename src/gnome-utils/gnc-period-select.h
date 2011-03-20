/*
 * gnc-period-select.h -- Accounting period selection widget
 *
 * Copyright (c) 2005 David Hampton <hampton@employees.org>
 * All rights reserved.
 *
 * GnuCash is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Library General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
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
/** @file gnc-period-select.h
    @brief A custom widget for selecting accounting periods.
    @author David Hampton <hampton@employees.org>
*/

#ifndef GNC_PERIOD_SELECT_H
#define GNC_PERIOD_SELECT_H

#include "gnc-accounting-period.h"

G_BEGIN_DECLS

#define GNC_TYPE_PERIOD_SELECT	    (gnc_period_select_get_type())
#define GNC_PERIOD_SELECT(o)	    (G_TYPE_CHECK_INSTANCE_CAST ((o), GNC_TYPE_PERIOD_SELECT, GncPeriodSelect))
#define GNC_PERIOD_SELECT_CLASS(k)  (G_TYPE_CHECK_CLASS_CAST ((k), GNC_TYPE_PERIOD_SELECT, GncPeriodSelectClass))
#define GNC_IS_PERIOD_SELECT(o)	    (G_TYPE_CHECK_INSTANCE_TYPE ((o), GNC_TYPE_PERIOD_SELECT))

/* typedefs & structures */
typedef struct
{
    GtkHBox hbox;
} GncPeriodSelect;

typedef struct
{
    GtkHBoxClass hbox;

    /* Signals */
    void (*changed) (GncPeriodSelect *period);

} GncPeriodSelectClass;


/** @name GncPeriodSelect Widget Implementation
 @{ */

/** Returns the GType of a GncPeriodSelect widget.
 */
GType gnc_period_select_get_type (void);


/** Tells a GncPeriodSelect object to emit a "changed" signal.
 *
 *  @param period The GncPeriodSelect object that should emit the
 *  signal.
 */
void gnc_period_select_changed (GncPeriodSelect *period);


/** Create a new GncPeriodSelect widget which is used to select a
 *  accounting period like "previous month" or "this year".
 *
 *  @param starting_labels If set to TRUE then all the labels will
 *  refer to the "Start of...".  If FALSE, labels will refer to "End
 *  of...".
 *
 *  @return A newly created GncPeriodSelect widget.
 */
GtkWidget *gnc_period_select_new (gboolean starting_labels);


/** Create a new GncPeriodSelect widget from a glade file.  The int1
 *  argument passed from glade is used to determine whether the widget
 *  uses labels for start times or end times.  A non-zero int2
 *  argument indicates that an example date should be shown.
 *
 *  @return A newly created GncPeriodSelect widget.
 */
GtkWidget * gnc_period_select_new_glade (gchar *widget_name,
        gchar *string1, gchar *string2,
        gint int1, gint int2);
/** @} */


/** @name GncPeriodSelect Properties
 @{ */

/** Set the fiscal year end on a GncPeriodSelect widget.  If set to a
 *  value other than NULL then widget will include fiscal accounting
 *  period like "this fiscal year".
 *
 *  @param period The GncPeriodSelect widget to update.
 *
 *  @param fy_end The new fiscal year end value, or NULL if no fiscal
 *  year is set.  Note that only the month and day fields need be
 *  valid in the provided GDate.
 */
void gnc_period_select_set_fy_end (GncPeriodSelect *period, const GDate *fy_end);


/** Get the current value of the fiscal year end setting from a
 *  GncPeriodSelect widget.  If the result is NULL then fiscal years
 *  are not currently supported.
 *
 *  @param period The GncPeriodSelect widget to query.
 *
 *  @return A pointer to a GDate containing the fiscal year end value,
 *  or NULL if no fiscal year end is set.  Note that only the month
 *  and day fields are valid in the returned GDate.
 */
GDate *gnc_period_select_get_fy_end (GncPeriodSelect *period);


/** Get the current value of the "show sample" setting from a
 *  GncPeriodSelect widget.
 */
gboolean
gnc_period_select_get_show_date (GncPeriodSelect *period);


/** Set the "show sample" setting on a GncPeriodSelect widget.  If set
 *  to TRUE then a GtkLabel will be used to show the date
 *  corresponding to the selected time period.
 */
void gnc_period_select_set_show_date (GncPeriodSelect *period, const gboolean show_date);


GDate *gnc_period_select_get_date_base (GncPeriodSelect *period);

/*  Set the base date used by a GncPeriodSelect widget.  All example
 *  dates presented by the widget will be computed from this date.
 */
void gnc_period_select_set_date_base (GncPeriodSelect *period, const GDate *sample_base);

/** @} */


/** Set which item in the GncPeriodSelect is initially selected.  This
 *  is used to set the initial selection before the widget is shown to
 *  the user.
 *
 *  @param period The selection widget to update.
 *
 *  @param which The accounting period that should be selected.
 */
void gnc_period_select_set_active (GncPeriodSelect *period, GncAccountingPeriod which);


/** Get the currently selected accounting period from a
 *  GncPeriodSelect widget.  This is used to retrieve the user's
 *  selection in the form of an enum.
 *
 *  @param period The selection widget to query.
 *
 *  @return An enum indicating the user's choice of accounting period.
 */
GncAccountingPeriod gnc_period_select_get_active (GncPeriodSelect *period);


/** Get the currently selected accounting period choice from a
 *  GncPeriodSelect widget.  This is used to retrieve the user's
 *  selection in the form of a GDate.
 *
 *  @param period The selection widget to query.
 *
 *  @return The starting/ending time. */
GDate *gnc_period_select_get_date (GncPeriodSelect *period);

/** Get the currently selected accounting period choice from a
 *  GncPeriodSelect widget.  This is used to retrieve the user's
 *  selection in the form of an timestamp.
 *
 *  @param period The selection widget to query.
 *
 *  @return The starting/ending time (in seconds since 1970-01-01) of
 *  the accounting period selected by the user.
 */
time_t gnc_period_select_get_time (GncPeriodSelect *period);

G_END_DECLS

#endif /* GNC_PERIOD_SELECT_H */

/** @} */
