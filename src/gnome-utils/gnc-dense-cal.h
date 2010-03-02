/********************************************************************\
 * gnc-dense-cal.h : a custom densely-dispalyed calendar widget     *
 * Copyright (C) 2002,2006 Joshua Sled <jsled@asynchronous.org>     *
 *                                                                  *
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation, under version 2 of    *
 * the License.                                                     *
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

#include "config.h"

#include <glib.h>
#include "gnc-dense-cal-model.h"
#include <gtk/gtk.h>

G_BEGIN_DECLS

#define GNC_TYPE_DENSE_CAL          (gnc_dense_cal_get_type ())
#define GNC_DENSE_CAL(obj)          GTK_CHECK_CAST (obj, gnc_dense_cal_get_type (), GncDenseCal)
#define GNC_DENSE_CAL_CLASS(klass)  GTK_CHECK_CLASS_CAST (klass, gnc_dense_cal_get_type (), GncDenseCalClass)
#define GNC_IS_DENSE_CAL(obj)       GTK_CHECK_TYPE (obj, gnc_dense_cal_get_type ())

typedef struct _GncDenseCal        GncDenseCal;
typedef struct _GncDenseCalClass   GncDenseCalClass;

typedef struct _gdc_month_coords
{
    gint x, y;
} gdc_month_coords;

enum GDC_COLORS
{
    MONTH_THIS = 0,
    MONTH_THAT,
    MAX_COLORS
};

struct _GncDenseCal
{
    GtkVBox widget;

    GtkComboBox *view_options;
    GtkDrawingArea *cal_drawing_area;

    GdkPixmap *drawbuf;

    gboolean initialized;

    gboolean showPopup;
    GtkWindow *transPopup;

    gint min_x_scale;
    gint min_y_scale;

    gint x_scale;
    gint y_scale;

    gint numMonths;
    gint monthsPerCol;
    gint num_weeks; /* computed */

    GDateMonth month;
    gint year;
    gint firstOfMonthOffset;

    gint leftPadding;
    gint topPadding;

    gdc_month_coords monthPositions[12];

    GdkColor weekColors[MAX_COLORS];

    guint label_width;
    guint label_height;
    gint dayLabelHeight;

    GncDenseCalModel *model;

    guint lastMarkTag;

    gint week_starts_monday;

    /**
     * A GList of gdc_mark_data structs, one for each active/valid markTag.
     **/
    GList *markData;
    int numMarks;
    /* array of GList*s of per-cell markings. */
    GList **marks;

    int disposed; /* private */
};

struct _GncDenseCalClass
{
    GtkVBoxClass parent_class;
};

typedef struct _gdc_mark_data
{
    gchar *name;
    gchar *info;
    guint tag;
    /**
     * A GList of the dcal->marks indexes containing this mark.
     **/
    GList *ourMarks;
} gdc_mark_data;

GtkWidget*     gnc_dense_cal_new                    (void);
GtkWidget*     gnc_dense_cal_new_with_model         (GncDenseCalModel *model);
GType          gnc_dense_cal_get_type               (void);

void gnc_dense_cal_set_model(GncDenseCal *cal, GncDenseCalModel *model);

void gnc_dense_cal_set_month(GncDenseCal *dcal, GDateMonth mon);
GDateMonth gnc_dense_cal_get_month( GncDenseCal *dcal );
/**
 * @param year Julian year: 2000 = 2000AD.
 **/
void gnc_dense_cal_set_year( GncDenseCal *dcal, guint year );
GDateYear gnc_dense_cal_get_year( GncDenseCal *dcal );

void gnc_dense_cal_set_num_months( GncDenseCal *dcal, guint num_months );
guint gnc_dense_cal_get_num_months( GncDenseCal *dcal );

void gnc_dense_cal_set_months_per_col( GncDenseCal *dcal, guint monthsPerCol );

G_END_DECLS

#endif /* _GNC_DENSE_CAL_H */
