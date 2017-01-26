/********************************************************************\
 * gnucash-date-picker.h -- A popup date picker using gtk_calendar  *
 *                                                                  *
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
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
 *                                                                  *
\********************************************************************/

#ifndef GNUCASH_DATE_PICKER_H
#define GNUCASH_DATE_PICKER_H

#include <libgnomecanvas/libgnomecanvas.h>
#include "gnucash-calendar.h"
/** @ingroup Register
 * @addtogroup Gnome
 * @{
 */
/** @file gnucash-date-picker.h
 * @brief Public declarations for GncDatePicker class
 */
#define GNC_DATE_PICKER(obj)      G_TYPE_CHECK_INSTANCE_CAST((obj), gnc_date_picker_get_type (), GNCDatePicker)
#define GNC_DATE_PICKER_CLASS(k)  G_TYPE_CHECK_CLASS_CAST((k), gnc_date_picker_get_type (), GNCDatePickerClass)
#define IS_GNC_DATE_PICKER(o)     G_TYPE_CHECK_INSTANCE_TYPE((o), gnc_date_picker_get_type ())


typedef struct
{
    GnomeCanvasWidget canvas_widget;

    GncCalendar *calendar;
} GNCDatePicker;


GType gnc_date_picker_get_type (void);

GnomeCanvasItem *gnc_date_picker_new (GnomeCanvasGroup *parent);

/* days are 1-31, mon is 0-11, year 1900 == 1900 */
void gnc_date_picker_set_date (GNCDatePicker *date_picker,
                               guint day, guint mon, guint year);

void gnc_date_picker_get_date (GNCDatePicker *date_picker,
                               guint *day, guint *mon, guint *year);

typedef struct
{
    GnomeCanvasWidgetClass parent_class;

    void (*date_selected) (GNCDatePicker *date_picker);

    void (*date_picked) (GNCDatePicker *date_picker);

    void (*key_press_event) (GNCDatePicker *date_picker,
                             GdkEventKey *event);

} GNCDatePickerClass;

/** @} */
#endif /* GNUCASH_DATE_PICKER_H */
