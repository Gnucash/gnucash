/*
 * gnc-datedelta.h -- Date delta widget
 *
 * Copyright (C) 2000 Free Software Foundation
 * All rights reserved.
 *
 * Author: Dave Peticolas <peticola@cs.ucdavis.edu>
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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
 * Boston, MA  02111-1307,  USA       gnu@gnu.org
 *
 */
/*
  @NOTATION@
 */

#ifndef __GNC_DATE_DELTA_H_
#define __GNC_DATE_DELTA_H_ 

#include <gnome.h>

BEGIN_GNOME_DECLS


typedef enum {
  GNC_DATE_DELTA_DAYS,
  GNC_DATE_DELTA_WEEKS,
  GNC_DATE_DELTA_MONTHS,
  GNC_DATE_DELTA_YEARS,
  GNC_DATE_DELTA_NUM_UNITS
} GNCDateDeltaUnits;

typedef enum {
  GNC_DATE_DELTA_PAST,
  GNC_DATE_DELTA_FUTURE,
  GNC_DATE_DELTA_NUM_POLARITY
} GNCDateDeltaPolarity;


#define GNC_DATE_DELTA(obj)          GTK_CHECK_CAST (obj, gnc_date_delta_get_type(), GNCDateDelta)
#define GNC_DATE_DELTA_CLASS(klass)  GTK_CHECK_CLASS_CAST (klass, gnc_date_delta_get_type(), GNCDateDeltaClass)
#define GNC_IS_DATE_DELTA(obj)       GTK_CHECK_TYPE (obj, gnc_date_delta_get_type ())

typedef struct {
  GtkHBox hbox;

  GtkWidget *value_spin;
  GtkWidget *units_menu;
  GtkWidget *polarity_menu;

  GNCDateDeltaUnits units;
  GNCDateDeltaPolarity polarity;

  gboolean show_polarity;
} GNCDateDelta;

typedef struct {
  GtkHBoxClass parent_class;
  void (*value_changed) (GNCDateDelta *gdd);
  void (*units_changed) (GNCDateDelta *gdd);
  void (*polarity_changed) (GNCDateDelta *gdd);
  void (*delta_changed) (GNCDateDelta *gdd);
} GNCDateDeltaClass;

guint gnc_date_delta_get_type (void);

GtkWidget *gnc_date_delta_new (gboolean show_polarity);

void gnc_date_delta_set_value (GNCDateDelta *gdd, int value);
int  gnc_date_delta_get_value (GNCDateDelta *gdd);

void gnc_date_delta_set_units (GNCDateDelta *gdd, GNCDateDeltaUnits units);
GNCDateDeltaUnits gnc_date_delta_get_units (GNCDateDelta *gdd);

void gnc_date_delta_set_polarity (GNCDateDelta *gdd,
                                  GNCDateDeltaPolarity polarity);
GNCDateDeltaPolarity gnc_date_delta_get_polarity (GNCDateDelta *gdd);

void gnc_date_delta_show_polarity (GNCDateDelta *gdd, gboolean show_polarity);

END_GNOME_DECLS

#endif
