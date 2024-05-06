/********************************************************************\
 * gnc-dense-cal.h : a custom densely-dispalyed calendar widget     *
 * Copyright (C) 2002,2006 Joshua Sled <jsled@asynchronous.org>     *
 *                                                                  *
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation, under version 2 and   *
 *  / or version 3 of the License.                                  *
 *                                                                  *
 * This program is distributed in the hope that it will be useful,  *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of   *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    *
 * GNU General Public License for more details.                     *
 *                                                                  *
 * You should have received a copy of the GNU General Public License*
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
\********************************************************************/

#ifndef _GNC_DENSE_CAL_H
#define _GNC_DENSE_CAL_H

#include <config.h>

#include <glib.h>
#include "gnc-dense-cal-model.h"
#include <gtk/gtk.h>

G_BEGIN_DECLS

#define GNC_TYPE_DENSE_CAL (gnc_dense_cal_get_type ())
G_DECLARE_FINAL_TYPE (GncDenseCal, gnc_dense_cal, GNC, DENSE_CAL, GtkBox)

GtkWidget* gnc_dense_cal_new (GtkWindow *parent);
GtkWidget* gnc_dense_cal_new_with_model (GtkWindow *parent,
                                         GncDenseCalModel *model);
void gnc_dense_cal_set_model (GncDenseCal *cal, GncDenseCalModel *model);

void gnc_dense_cal_set_month (GncDenseCal *dcal, GDateMonth mon);
GDateMonth gnc_dense_cal_get_month (GncDenseCal *dcal);
/**
 * @param year Julian year: 2000 = 2000AD.
 **/
void gnc_dense_cal_set_year (GncDenseCal *dcal, guint year);
GDateYear gnc_dense_cal_get_year (GncDenseCal *dcal);

void gnc_dense_cal_set_num_months (GncDenseCal *dcal, guint num_months);
guint gnc_dense_cal_get_num_months (GncDenseCal *dcal);

void gnc_dense_cal_set_months_per_col (GncDenseCal *dcal, guint monthsPerCol);

G_END_DECLS

#endif /* _GNC_DENSE_CAL_H */
