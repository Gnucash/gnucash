/********************************************************************\
 * gnc-dense-cal.h : a custom densely-dispalyed calendar widget     *
 * Copyright (C) 2002,2006 Joshua Sled <jsled@asynchronous.org>     *
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
\********************************************************************/

#ifndef _DENSECAL_H_
#define _DENSECAL_H_

#include <gdk/gdk.h>
#include <gtk/gtkadjustment.h>
#include <gtk/gtkwidget.h>
#include <glib.h>
#include <FreqSpec.h>

G_BEGIN_DECLS

#define GNC_TYPE_DENSE_CAL_MODEL (gnc_dense_cal_model_get_type())
#define GNC_DENSE_CAL_MODEL(obj) (G_TYPE_CHECK_INSTANCE_CAST ((obj), GNC_TYPE_DENSE_CAL_MODEL, GncDenseCalModel))
#define GNC_IS_DENSE_CAL_MODEL(obj) (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GNC_TYPE_DENSE_CAL_MODEL))
#define GNC_DENSE_CAL_MODEL_GET_INTERFACE(inst) (G_TYPE_INSTANCE_GET_INTERFACE ((inst), GNC_TYPE_DENSE_CAL_MODEL, GncDenseCalModelIface))
  
typedef struct _GncDenseCalModel GncDenseCalModel; /* non existant */
typedef struct _GncDenseCalModelIface GncDenseCalModelIface;

struct _GncDenseCalModelIface
{
  GTypeInterface parent;

  /* signals */
  void (*insert)(GncDenseCalModel *mdl, gint tag);
  void (*update)(GncDenseCalModel *mdl, gint tag);
  void (*remove)(GncDenseCalModel *mdl, gint tag);

  /* virtual table */
  GList* (*get_contained)(GncDenseCalModel *model);
  gchar* (*get_name)(GncDenseCalModel *model, guint tag);
  gchar* (*get_info)(GncDenseCalModel *model, guint tag);
  gint (*get_instance_count)(GncDenseCalModel *model, guint tag);
  void (*get_instance)(GncDenseCalModel *model, guint tag, gint instance_index, GDate *date);
};

GType gnc_dense_cal_model_get_type(void);

/* @fixme: glist mem alloc policy... ? */
GList* gnc_dense_cal_model_get_contained(GncDenseCalModel *model);
gchar* gnc_dense_cal_model_get_name(GncDenseCalModel *model, guint tag);
gchar* gnc_dense_cal_model_get_info(GncDenseCalModel *model, guint tag);
gint gnc_dense_cal_model_get_instance_count(GncDenseCalModel *model, guint tag);
void gnc_dense_cal_model_get_instance(GncDenseCalModel *model, guint tag, gint instance_index, GDate *date);

/* ------------------------------------------------------------ */
#define GNC_TYPE_DENSE_CAL_TRANSIENT_MODEL (gnc_dense_cal_transient_model_get_type())
#define GNC_DENSE_CAL_TRANSIENT_MODEL(obj) (G_TYPE_CHECK_INSTANCE_CAST ((obj), GNC_TYPE_DENSE_CAL_TRANSIENT_MODEL, GncDenseCalTransientModel))
#define GNC_DENSE_CAL_TRANSIENT_MODEL_CLASS(klass) (G_TYPE_CHECK_CLASS_CAST ((klass), GNC_TYPE_DENSE_CAL_TRANSIENT_MODEL, GncDenseCalTransientModelClass))
#define GNC_IS_DENSE_CAL_TRANSIENT_MODEL(obj) (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GNC_TYPE_DENSE_CAL_TRANSIENT_MODEL))
#define GNC_IS_DENSE_CAL_TRANSIENT_MODEL_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GNC_TYPE_DENSE_CAL_TRANSIENT_MODEL))
#define GNC_DENSE_CAL_TRANSIENT_MODEL_GET_CLASS(obj) (G_TYPE_INSTANCE_GET_CLASS ((obj), GNC_TYPE_DENSE_CAL_TRANSIENT_MODEL, GncDenseCalTransientModel))
  
typedef enum { NEVER_END, END_ON_DATE, END_AFTER_N_OCCS, BAD_END } gdctm_end_type;

typedef struct _GncDenseCalTransientModel
{
     GObject parent;
     
     GDate start_date;
     gdctm_end_type end_type;
     GDate end_date;
     gint n_occurrences;
     gchar *name;
     gchar *info;
     int num_marks;
     int num_real_marks; 
     GDate **cal_marks;
} GncDenseCalTransientModel;

typedef struct _GncDenseCalTransientModelClass
{
     GObjectClass parent_class;
} GncDenseCalTransientModelClass;

GType gnc_dense_cal_transient_model_get_type(void);
GncDenseCalTransientModel* gnc_dense_cal_transient_model_new(int num_marks);
void gnc_dense_cal_transient_model_clear(GncDenseCalTransientModel *model);
void gnc_dense_cal_transient_model_update_name(GncDenseCalTransientModel *model, gchar* name);
void gnc_dense_cal_transient_model_update_info(GncDenseCalTransientModel *model, gchar* info);
void gnc_dense_cal_transient_model_update_no_end(GncDenseCalTransientModel *model, GDate *start, FreqSpec *fs);
void gnc_dense_cal_transient_model_update_count_end(GncDenseCalTransientModel *model, GDate *start, FreqSpec *fs, int num_occur);
void gnc_dense_cal_transient_model_update_date_end(GncDenseCalTransientModel *model, GDate *start, FreqSpec *fs, GDate *end_date);

/* ------------------------------------------------------------ */

#define GNC_TYPE_DENSE_CAL          (gnc_dense_cal_get_type ()) 
#define GNC_DENSE_CAL(obj)          GTK_CHECK_CAST (obj, gnc_dense_cal_get_type (), GncDenseCal)
#define GNC_DENSE_CAL_CLASS(klass)  GTK_CHECK_CLASS_CAST (klass, gnc_dense_cal_get_type (), GncDenseCalClass)
#define GNC_IS_DENSE_CAL(obj)       GTK_CHECK_TYPE (obj, gnc_dense_cal_get_type ())

typedef struct _GncDenseCal        GncDenseCal;
typedef struct _GncDenseCalClass   GncDenseCalClass;

typedef struct _gdc_month_coords {
        gint x, y;
} gdc_month_coords;

enum GDC_COLORS {
  MONTH_THIS = 0,
  MONTH_THAT,
  MAX_COLORS
};

struct _GncDenseCal
{
        GtkWidget widget;

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

        gboolean needInitMonthLabels;
        gdc_month_coords monthPositions[12];
        GdkFont *monthLabelFont;
        GdkFont *dayLabelFont;
        GdkPixmap *monthLabels[12];

        GdkColor weekColors[MAX_COLORS];

        guint label_lbearing;
        guint label_ascent;
        guint label_width;
        guint label_height;
        guint dayLabelHeight;

  GncDenseCalModel *model;

        guint lastMarkTag;

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
        GtkWidgetClass parent_class;
};

typedef struct _gdc_mark_data {
        gchar *name;
        gchar *info;
        guint tag;
        /* GdkColor markStyle; */
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
/**
 * @param year Julian year: 2000 = 2000AD.
 **/
void gnc_dense_cal_set_year( GncDenseCal *dcal, guint year );
void gnc_dense_cal_set_num_months( GncDenseCal *dcal, guint num_months );
void gnc_dense_cal_set_months_per_col( GncDenseCal *dcal, guint monthsPerCol );

guint gnc_dense_cal_get_num_months( GncDenseCal *dcal );
GDateMonth gnc_dense_cal_get_month( GncDenseCal *dcal );
GDateYear gnc_dense_cal_get_year( GncDenseCal *dcal );

#if 0
guint gnc_dense_cal_mark( GncDenseCal *dcal,
                          guint size, GDate **daysArray,
                          gchar *name, gchar *info );
void gnc_dense_cal_mark_remove( GncDenseCal *dcal, guint markToRemove );
#endif // 0

G_END_DECLS

#endif /* _DENSECAL_H_ */
