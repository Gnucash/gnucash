/*
 * gnc-date-format.c -- Date formator widget
 *
 * Copyright (C) 2003 Derek Atkins  <derek@ihtfp.com>
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
 * */
/*
  @NOTATION@
 */


#ifndef GNC_DATE_FORMAT_H
#define GNC_DATE_FORMAT_H

#include "qof.h"

#ifdef __cplusplus
extern "C" {
#endif

#define GNC_TYPE_DATE_FORMAT          (gnc_date_format_get_type ())
G_DECLARE_FINAL_TYPE (GNCDateFormat, gnc_date_format, GNC, DATE_FORMAT, GtkBox)

GtkWidget *gnc_date_format_new            (void);
GtkWidget *gnc_date_format_new_without_label (void);
GtkWidget *gnc_date_format_new_with_label (const char *label);

void      gnc_date_format_set_format      (GNCDateFormat *gdf, QofDateFormat format);
QofDateFormat gnc_date_format_get_format     (GNCDateFormat *gdf);

void      gnc_date_format_set_months      (GNCDateFormat *gdf,
        GNCDateMonthFormat months);
GNCDateMonthFormat gnc_date_format_get_months (GNCDateFormat *gdf);

void      gnc_date_format_set_years       (GNCDateFormat *gdf,
        gboolean include_century);
gboolean  gnc_date_format_get_years       (GNCDateFormat *gdf);

void      gnc_date_format_set_custom      (GNCDateFormat *gdf, const char *format);
const char* gnc_date_format_get_custom    (GNCDateFormat *gdf);

void      gnc_date_format_refresh         (GNCDateFormat *gdf);

#ifdef __cplusplus
}
#endif

#endif
