/*
 * gnc-dateedit.c -- Date editor widget
 *
 * Copyright (C) 1997, 1998, 1999, 2000 Free Software Foundation
 * All rights reserved.
 *
 * This file was part of the Gnome Library. It was modifed by
 * Dave Peticolas <dave@krondo.com> for use in GnuCash.
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

#ifndef GNC_DATE_EDIT_H
#define GNC_DATE_EDIT_H

#include <glib.h>
#include <time.h>
#include "gnc-date.h"

typedef enum
{
    GNC_DATE_EDIT_SHOW_TIME             = 1 << 0,
    GNC_DATE_EDIT_24_HR                 = 1 << 1,
    GNC_DATE_EDIT_WEEK_STARTS_ON_MONDAY = 1 << 2,
} GNCDateEditFlags;

#define GNC_TYPE_DATE_EDIT          (gnc_date_edit_get_type ())
#define GNC_DATE_EDIT(obj)          G_TYPE_CHECK_INSTANCE_CAST (obj, gnc_date_edit_get_type(), GNCDateEdit)
#define GNC_DATE_EDIT_CLASS(klass)  G_TYPE_CHECK_CLASS_CAST (klass, gnc_date_edit_get_type(), GNCDateEditClass)
#define GNC_IS_DATE_EDIT(obj)       G_TYPE_CHECK_INSTANCE_TYPE (obj, gnc_date_edit_get_type ())

/**
 *  \verbatim
 *  * 2001.05.13T1647 [PDT], #gnucash:
 * <jsled> dave_p: So the header for gnc-dateedit.h is a bit light
 *         on _why_ such a thing was done... any help?
 * <dave_p> jsled: gnome date edit isn't i18n'd properly. also, we
 *          added the register date hotkeys.
 *  \endverbatim
 **/
typedef struct
{
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

    int       disposed;

    gboolean  popup_in_progress;
} GNCDateEdit;

typedef struct
{
    GtkHBoxClass parent_class;
    void (*date_changed) (GNCDateEdit *gde);
    void (*time_changed) (GNCDateEdit *gde);
} GNCDateEditClass;

GType     gnc_date_edit_get_type        (void);

GtkWidget *gnc_date_edit_new            (time_t the_time,
        int show_time, int use_24_format);
GtkWidget *gnc_date_edit_new_ts         (Timespec the_time,
        int show_time, int use_24_format);

/**
 * Create a new GncDateEdit widget from a glade file.  The widget
 * generated is set to today's date, and will not show a time as part
 * of the date.  This function does not use any of the arguments
 * passed by glade.
 *
 * @param widget_name This parameter is unused.  The actual widget
 * name will be set by glade so it does not need to be done here.
 *
 * @param string1 Unused.
 * @param string2 Unused.
 * @param int1 Unused.
 * @param int2 Unused.
 *
 * @return A pointer to the newly created GncDateEdit widget.
 */
GtkWidget *gnc_date_edit_new_glade (gchar *widget_name,
                                    gchar *string1, gchar *string2,
                                    gint int1, gint int2);

GtkWidget *gnc_date_edit_new_flags      (time_t the_time,
        GNCDateEditFlags flags);

void      gnc_date_edit_set_gdate       (GNCDateEdit *gde, const GDate *date);
void      gnc_date_edit_set_time        (GNCDateEdit *gde, time_t the_time);
void      gnc_date_edit_set_time_ts     (GNCDateEdit *gde, Timespec the_time);

void      gnc_date_edit_set_popup_range (GNCDateEdit *gde,
        int low_hour, int up_hour);

void      gnc_date_edit_get_gdate       (GNCDateEdit *gde, GDate *date);
time_t    gnc_date_edit_get_date        (GNCDateEdit *gde);
Timespec  gnc_date_edit_get_date_ts     (GNCDateEdit *gde);
time_t    gnc_date_edit_get_date_end    (GNCDateEdit *gde);
Timespec  gnc_date_edit_get_date_end_ts (GNCDateEdit *gde);

void      gnc_date_edit_set_flags       (GNCDateEdit *gde,
        GNCDateEditFlags flags);
int       gnc_date_edit_get_flags       (GNCDateEdit *gde);

void      gnc_date_activates_default    (GNCDateEdit *gde, gboolean state);
void      gnc_date_grab_focus           (GNCDateEdit *gde);

void      gnc_date_make_mnemonic_target (GNCDateEdit *gde, GtkWidget *label);
#endif
