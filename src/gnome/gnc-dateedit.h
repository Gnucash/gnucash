/*
 * gnc-dateedit.c -- Date editor widget
 *
 * Copyright (C) 1997, 1998, 1999, 2000 Free Software Foundation
 * All rights reserved.
 *
 * This file was part of the Gnome Library. It was modifed by
 * Dave Peticolas <peticola@cs.ucdavis.edu> for use in GnuCash.
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

#ifndef __GNC_DATE_EDIT_H_
#define __GNC_DATE_EDIT_H_ 

#include <gnome.h>

BEGIN_GNOME_DECLS


typedef enum {
	GNC_DATE_EDIT_SHOW_TIME             = 1 << 0,
	GNC_DATE_EDIT_24_HR                 = 1 << 1,
	GNC_DATE_EDIT_WEEK_STARTS_ON_MONDAY = 1 << 2,
#if 0
        GNC_DATE_EDIT_SHOW_DATE             = 1 << 3,
        GNC_DATE_EDIT_SHOW_DELTA            = 1 << 4
#endif
} GNCDateEditFlags;


#define GNC_DATE_EDIT(obj)          GTK_CHECK_CAST (obj, gnc_date_edit_get_type(), GNCDateEdit)
#define GNC_DATE_EDIT_CLASS(klass)  GTK_CHECK_CLASS_CAST (klass, gnc_date_edit_get_type(), GNCDateEditClass)
#define GNC_IS_DATE_EDIT(obj)       GTK_CHECK_TYPE (obj, gnc_date_edit_get_type ())

typedef struct {
	GtkHBox hbox;

	GtkWidget *date_entry;
	GtkWidget *date_button;

	GtkWidget *time_entry;
	GtkWidget *time_popup;

	GtkWidget *cal_label;
	GtkWidget *cal_popup;
	GtkWidget *calendar;

	time_t    initial_time;

	int       lower_hour;
	int       upper_hour;
	
	int       flags;
} GNCDateEdit;

typedef struct {
	GtkHBoxClass parent_class;
	void (*date_changed) (GNCDateEdit *gde);
	void (*time_changed) (GNCDateEdit *gde);
} GNCDateEditClass;

guint     gnc_date_edit_get_type        (void);

GtkWidget *gnc_date_edit_new            (time_t the_time,
                                         int show_time, int use_24_format);
GtkWidget *gnc_date_edit_new_flags      (time_t the_time,
                                         GNCDateEditFlags flags);

void      gnc_date_edit_set_time        (GNCDateEdit *gde, time_t the_time);

void      gnc_date_edit_set_popup_range (GNCDateEdit *gde,
                                         int low_hour, int up_hour);

time_t    gnc_date_edit_get_date        (GNCDateEdit *gde);
time_t    gnc_date_edit_get_date_end    (GNCDateEdit *gde);

void      gnc_date_edit_set_flags       (GNCDateEdit *gde,
                                         GNCDateEditFlags flags);
int       gnc_date_edit_get_flags       (GNCDateEdit *gde);


END_GNOME_DECLS

#endif
