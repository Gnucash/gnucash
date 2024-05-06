/********************************************************************\
 * gnc-dense-cal-store.h                                            *
 * Copyright (C) 2006 Joshua Sled <jsled@asynchronous.org>          *
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

#ifndef _GNC_DENSE_CAL_STORE_H
#define _GNC_DENSE_CAL_STORE_H

#include <config.h>
#include <glib.h>
#include <glib-object.h>
#include "gnc-dense-cal-model.h"
#include "gnc-dense-cal.h"

G_BEGIN_DECLS

#define GNC_TYPE_DENSE_CAL_STORE (gnc_dense_cal_store_get_type ())
G_DECLARE_FINAL_TYPE(GncDenseCalStore, gnc_dense_cal_store, GNC, DENSE_CAL_STORE, GObject)

typedef enum { NEVER_END, END_ON_DATE, END_AFTER_N_OCCS, BAD_END } gdcs_end_type;

GncDenseCalStore* gnc_dense_cal_store_new (int num_marks);
void gnc_dense_cal_store_clear (GncDenseCalStore *model);
void gnc_dense_cal_store_update_name (GncDenseCalStore *model, const gchar* name);
void gnc_dense_cal_store_update_info (GncDenseCalStore *model, const gchar* info);

void gnc_dense_cal_store_update_recurrences_no_end (GncDenseCalStore *model,
                                                    GDate *start,
                                                    GList *recurrences);
void gnc_dense_cal_store_update_recurrences_count_end (GncDenseCalStore *model,
                                                       GDate *start,
                                                       GList *recurrences,
                                                       int num_occur);
void gnc_dense_cal_store_update_recurrences_date_end (GncDenseCalStore *model,
                                                      GDate *start,
                                                      GList *recurrences,
                                                      GDate *end_date);

G_END_DECLS

#endif // _GNC_DENSE_CAL_STORE_H
