/********************************************************************\
 * gnc-dense-cal.c : a custom densely-dispalyed calendar widget     *
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

#include <config.h>

#include "gnc-dense-cal.h"
#include "gnc-dense-cal-model.h"
#include "gnc-engine.h"
#include "gnc-gtk-utils.h"
#include <glib.h>
#include <glib/gi18n.h>
#include <gtk/gtk.h>
#include <pango/pangocairo.h>
#include <math.h>
#include <stdlib.h>
#include "gnc-date.h"
#include "dialog-utils.h"
#include <qoflog.h>

static const QofLogModule log_module = "gnc.gui.dense-cal";

/**
 * Marking ...
 *
 * We want a facility to mark multiple days on the calendar.  This facility
 * should be efficient in display.  It will take an array+count of GDates on
 * which to mark the calendar.  Dates outside of the visible calendar range
 * will be ignored.
 *
 * Markings will be manipulated in tagged sets for markings related to the
 * same event; in order to efficiently process these sets [removal,
 * primarily], we will keep a multiple data structures.
 *
 *
 * We need to be able to perform the following actions:
 * . Add a new mark-set, returning a calendar-unique tag.
 * . Remove a mark-set by tag.
 * . Iterate over all days in the calendar, listing which markings are active
 *   on that day.
 *
 * The markings in the calendar will be internally represented as an array of
 * GLists, with each item in the list pointing to a gdc_mark_data structure.
 * The gdc_mark_data structures will contain:
 * . the external/caller marker tag
 * . the marker indication [color, when supported]
 * . a GList of all instances of the marker in the visible calendar, by
 *   'marks' index.
 *
 * The list of gdc_mark_data structures itself will be a top-level list in the
 * GncDenseCal structure.
 **/

static const int DENSE_CAL_DEFAULT_WIDTH = 15;
static const int DENSE_CAL_DEFAULT_HEIGHT = 105;
static const int MINOR_BORDER_SIZE = 1;
static const int COL_BORDER_SIZE = 3;

static void gnc_dense_cal_finalize (GObject *object);
static void gnc_dense_cal_dispose (GObject *object);
static void gnc_dense_cal_draw_to_buffer (GncDenseCal *dcal);
static void gnc_dense_cal_draw (GtkDrawingArea *area, cairo_t *cr,
                                int width, int height, gpointer user_data);
static void gnc_dense_cal_resize (GtkDrawingArea *area, int width, int height,
                                  gpointer user_data);

static void gdc_reconfig (GncDenseCal *dcal);

static void gdc_free_all_mark_data (GncDenseCal *dcal);

static void _gdc_compute_min_size (GncDenseCal *dcal,
                                   guint *min_width, guint *min_height);
static void _gdc_set_cal_min_size_req (GncDenseCal *dcal);
static void gnc_dense_cal_motion (GtkEventControllerMotion *controller,
                                  double x, double y, gpointer user_data);
static void gnc_dense_cal_click_pressed (GtkGestureClick *gesture,
                                         int n_press, double x, double y,
                                         gpointer user_data);

static void _gdc_view_option_changed (GObject *widget, GParamSpec *pspec,
                                      gpointer user_data);

static inline int day_width_at (GncDenseCal *dcal, guint xScale);
static inline int day_width (GncDenseCal *dcal);
static inline int day_height_at (GncDenseCal *dcal, guint yScale);
static inline int day_height (GncDenseCal *dcal);
static inline int week_width_at (GncDenseCal *dcal, guint xScale);
static inline int week_width (GncDenseCal *dcal);
static inline int week_height_at (GncDenseCal *dcal, guint yScale);
static inline int week_height (GncDenseCal *dcal);
static inline int col_width_at (GncDenseCal *dcal, guint xScale);
static inline int col_width (GncDenseCal *dcal);

static inline int col_height (GncDenseCal *dcal);
static inline int num_cols (GncDenseCal *dcal);

static void _gnc_dense_cal_set_month (GncDenseCal *dcal, GDateMonth mon, gboolean redraw);
static void _gnc_dense_cal_set_year (GncDenseCal *dcal, guint year, gboolean redraw);

/**
 * Returns the total number of weeks to display in the calendar [irrespective
 * of columns/weeks-per-col].
 **/
static inline int num_weeks (GncDenseCal *dcal);
/**
 * Returns the number of weeks per column.  Note that this is the number of
 * weeks needed to display the longest column.
 **/
static int num_weeks_per_col (GncDenseCal *dcal);

/* hotspot calculation */
static gint wheres_this (GncDenseCal *dcal, int x, int y);

static void recompute_x_y_scales (GncDenseCal *dcal);
static void recompute_mark_storage (GncDenseCal *dcal);
static void recompute_extents (GncDenseCal *dcal);
static void populate_hover_window (GncDenseCal *dcal);
static void set_popup_pointing_to (GncDenseCal *dcal, double x, double y);
static void gnc_dense_cal_popup_closed (GtkPopover *popover, GncDenseCal *dcal);

static void month_coords (GncDenseCal *dcal, int monthOfCal, GList **outList);
static void doc_coords (GncDenseCal *dcal, int dayOfCal,
                        int *x1, int *y1, int *x2, int *y2);

static void gdc_mark_add (GncDenseCal *dcal, guint tag, gchar *name,
                          gchar *info, guint size, GDate **dateArray);
static void gdc_mark_remove (GncDenseCal *dcal, guint mark_to_remove, gboolean redraw);

static void gdc_add_tag_markings (GncDenseCal *cal, guint tag);
static void gdc_add_markings (GncDenseCal *cal);
static void gdc_remove_markings (GncDenseCal *cal);

typedef struct _gdc_month_coords
{
    gint x, y;
} gdc_month_coords;

struct _GncDenseCal
{
    GtkBox widget;

    GtkDropDown *view_options;
    GtkOverlay *cal_overlay;
    GtkFixed *cal_background_layer;
    GtkDrawingArea *cal_drawing_area;

    cairo_surface_t *surface;

    gboolean initialized;

    gboolean showPopup;
    GtkPopover *transPopup;
    GtkLabel *popup_date_label;
    GtkListBox *popup_marks;
    gint doc;

    gint min_x_scale;
    gint min_y_scale;

    gint x_scale;
    gint y_scale;

    gint numMonths;
    gint monthsPerCol;
    gint num_weeks; /* computed */

    GDateMonth month;
    guint year;
    gint firstOfMonthOffset;

    gint leftPadding;
    gint topPadding;

    gdc_month_coords monthPositions[12];

    gint label_height; // dense cal label height

    guint month_side_bar_width; // month side bar width
    guint day_top_bar_height; // day top bar height
    guint bar_label_padding; // padding used in top and side bar

    GncDenseCalModel *model;

    guint lastMarkTag;

    GDateWeekday day_of_week_start;

    /**
     * A GList of gdc_mark_data structs, one for each active/valid markTag.
     **/
    GList *markData;
    int numMarks;
    /* array of GList*s of per-cell markings. */
    GList **marks;

    int disposed; /* private */
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

G_DEFINE_TYPE(GncDenseCal, gnc_dense_cal, GTK_TYPE_BOX)

#define MONTH_NAME_BUFSIZE 10

/* Takes the number of months since January, in the range 0 to
 * 11. Returns the abbreviated month name according to the current
 * locale.*/
static const gchar*
month_name (int mon)
{
    static gchar buf[MONTH_NAME_BUFSIZE];
    GDate date;
    gint arbitrary_year = 1977;

    memset (buf, 0, MONTH_NAME_BUFSIZE);
    g_date_clear (&date, 1);

    g_date_set_year (&date, arbitrary_year);
    g_date_set_day (&date, 1);
    // g_date API is 1..12 (not 0..11)
    g_date_set_month (&date, mon + 1);
    g_date_strftime (buf, MONTH_NAME_BUFSIZE, "%b", &date);

    return buf;
}

/* Takes the number of days since Sunday, in the range 0 to 6. Returns
 * the abbreviated weekday name according to the current locale. */
static void
day_label (gchar *buf, int buf_len, int dow)
{
    gnc_dow_abbrev (buf, buf_len, dow);
    /* Use only the first two characters */
    if (g_utf8_strlen (buf, -1) > 2)
    {
        gchar *pointer = g_utf8_offset_to_pointer (buf, 2);
        *pointer = '\0';
    }
}

static void
gnc_dense_cal_class_init (GncDenseCalClass *klass)
{
    GObjectClass *object_class = G_OBJECT_CLASS(klass);

    gtk_widget_class_set_css_name (GTK_WIDGET_CLASS(klass), "calendar");

    object_class->finalize = gnc_dense_cal_finalize;
    object_class->dispose = gnc_dense_cal_dispose;
}

static const guint gdc_view_option_months[] = { 12, 6, 4, 3, 2, 1 };
static const guint gdc_view_option_columns[] = { 3, 2, 2, 2, 1, 1 };

static void
gnc_dense_cal_init (GncDenseCal *dcal)
{

    gtk_orientable_set_orientation (GTK_ORIENTABLE(dcal), GTK_ORIENTATION_VERTICAL);

    // Set the style context for this widget so it can be easily manipulated with css
    gnc_widget_style_context_add_class (GTK_WIDGET(dcal), "calendar");

    // Set the name of this widget so it can be easily manipulated with css
    gtk_widget_set_name (GTK_WIDGET(dcal), "gnc-id-dense-calendar");

    {
        const gchar *options[] = { _("12 months"), _("6 months"), _("4 months"),
                                   _("3 months"), _("2 months"), _("1 month"), NULL };
        GtkStringList *model = gtk_string_list_new (options);

        dcal->view_options = gnc_gtk_drop_down_new (G_LIST_MODEL (model), NULL);
        gtk_drop_down_set_selected (dcal->view_options, 0);
        g_signal_connect (dcal->view_options, "notify::selected",
                          G_CALLBACK (_gdc_view_option_changed), dcal);
    }

    {
        GtkWidget *hbox = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 0);
        GtkWidget *label = gtk_label_new (_("View"));

        gtk_box_set_homogeneous (GTK_BOX(hbox), FALSE);
        gtk_widget_set_halign (label, GTK_ALIGN_END);
        gtk_widget_set_margin_end (label, 5);
        gnc_box_append_full (GTK_BOX(hbox), label, TRUE, TRUE, 0);
        gnc_box_append_full (GTK_BOX(hbox), GTK_WIDGET(dcal->view_options), FALSE, FALSE, 0);

        gnc_box_append_full (GTK_BOX(dcal), GTK_WIDGET(hbox), FALSE, FALSE, 0);
    }
    dcal->cal_overlay = GTK_OVERLAY (gtk_overlay_new ());
    dcal->cal_background_layer = GTK_FIXED (gtk_fixed_new ());
    dcal->cal_drawing_area = GTK_DRAWING_AREA (gtk_drawing_area_new ());
    gtk_widget_set_hexpand (GTK_WIDGET (dcal->cal_overlay), TRUE);
    gtk_widget_set_vexpand (GTK_WIDGET (dcal->cal_overlay), TRUE);
    gtk_overlay_set_child (dcal->cal_overlay,
                           GTK_WIDGET (dcal->cal_background_layer));
    gtk_overlay_add_overlay (dcal->cal_overlay,
                             GTK_WIDGET (dcal->cal_drawing_area));
    gtk_widget_set_hexpand (GTK_WIDGET (dcal->cal_drawing_area), TRUE);
    gtk_widget_set_vexpand (GTK_WIDGET (dcal->cal_drawing_area), TRUE);
    gnc_box_append_full (GTK_BOX (dcal), GTK_WIDGET (dcal->cal_overlay),
                         TRUE, TRUE, 0);
    gtk_drawing_area_set_draw_func (dcal->cal_drawing_area, gnc_dense_cal_draw,
                                    dcal, NULL);
    g_signal_connect (dcal->cal_drawing_area, "resize",
                      G_CALLBACK (gnc_dense_cal_resize), dcal);
    {
        GtkEventController *motion = gtk_event_controller_motion_new ();
        GtkGesture *click = gtk_gesture_click_new ();

        gtk_widget_add_controller (GTK_WIDGET(dcal->cal_drawing_area), motion);
        gtk_widget_add_controller (GTK_WIDGET(dcal->cal_drawing_area),
                                   GTK_EVENT_CONTROLLER (click));
        g_signal_connect (motion, "motion", G_CALLBACK (gnc_dense_cal_motion), dcal);
        g_signal_connect (click, "pressed", G_CALLBACK (gnc_dense_cal_click_pressed), dcal);
    }

    dcal->disposed = FALSE;
    dcal->initialized = FALSE;
    dcal->markData = NULL;
    dcal->numMarks = 0;
    dcal->marks = NULL;
    dcal->lastMarkTag = 0;

    dcal->showPopup = FALSE;

    dcal->transPopup = GTK_POPOVER (gtk_popover_new ());
    {
        GtkWidget *vbox, *hbox;
        GtkWidget *l;

        vbox = gtk_box_new (GTK_ORIENTATION_VERTICAL, 5);
        gtk_box_set_homogeneous (GTK_BOX(vbox), FALSE);
        hbox = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 5);
        gtk_box_set_homogeneous (GTK_BOX(hbox), FALSE);

        gtk_widget_set_name (GTK_WIDGET(dcal->transPopup), "gnc-id-dense-calendar-popup");
        gtk_popover_set_autohide (dcal->transPopup, TRUE);
        gtk_popover_set_position (dcal->transPopup, GTK_POS_BOTTOM);
        gtk_widget_set_parent (GTK_WIDGET(dcal->transPopup),
                               GTK_WIDGET(dcal->cal_drawing_area));
        g_signal_connect (dcal->transPopup, "closed",
                          G_CALLBACK (gnc_dense_cal_popup_closed), dcal);

        l = gtk_label_new (_("Date: "));
        gtk_widget_set_margin_start (l, 5);
        gtk_box_append (GTK_BOX(hbox), l);
        l = gtk_label_new ("YY/MM/DD");
        dcal->popup_date_label = GTK_LABEL (l);
        gtk_box_append (GTK_BOX(hbox), l);
        gtk_box_append (GTK_BOX(vbox), hbox);

        gtk_box_append (GTK_BOX(vbox), gtk_separator_new (GTK_ORIENTATION_HORIZONTAL));

        hbox = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 12);
        l = gtk_label_new (_("Name"));
        gtk_widget_set_hexpand (l, TRUE);
        gtk_label_set_xalign (GTK_LABEL (l), 0.0f);
        gtk_box_append (GTK_BOX(hbox), l);
        l = gtk_label_new (_("Frequency"));
        gtk_label_set_xalign (GTK_LABEL (l), 0.0f);
        gtk_box_append (GTK_BOX(hbox), l);
        gtk_box_append (GTK_BOX(vbox), hbox);

        dcal->popup_marks = GTK_LIST_BOX (gtk_list_box_new ());
        gtk_list_box_set_selection_mode (dcal->popup_marks, GTK_SELECTION_NONE);
        gtk_box_append (GTK_BOX(vbox), GTK_WIDGET(dcal->popup_marks));
        gtk_popover_set_child (dcal->transPopup, vbox);
    }

    dcal->month = G_DATE_JANUARY;
    dcal->year  = 1970;

    dcal->numMonths = 12;
    dcal->monthsPerCol = 3;
    dcal->leftPadding = 4;
    dcal->topPadding = 4;

    {
    GDate now;
    g_date_clear (&now, 1);
        gnc_gdate_set_today (&now);
        _gnc_dense_cal_set_month (dcal, g_date_get_month (&now), FALSE);
        _gnc_dense_cal_set_year (dcal, g_date_get_year (&now), FALSE);
    }

    recompute_extents (dcal);
    recompute_mark_storage (dcal);

    /* Compute initial scaling factors; will be increased when we're
     * allocated enough space to scale up. */
    {
        PangoLayout *layout;
        int width_88, height_88;
        int width_XXX, height_XXX;

        layout = gtk_widget_create_pango_layout (GTK_WIDGET(dcal), NULL);

        pango_layout_set_text (layout, "88", -1);
        pango_layout_get_pixel_size (layout, &width_88, &height_88);

        pango_layout_set_text (layout, "XXX", -1);
        pango_layout_get_pixel_size (layout, &width_XXX, &height_XXX);

        dcal->min_x_scale = dcal->x_scale = width_88 + 2;
        dcal->min_y_scale = dcal->y_scale = MAX(floor ((float)width_XXX / 3.), height_88 + 2);

        dcal->bar_label_padding = 2;

        dcal->month_side_bar_width = height_88 + (dcal->bar_label_padding * 2);
        dcal->day_top_bar_height = height_88 + (dcal->bar_label_padding * 2);

        g_object_unref (layout);
    }

    dcal->initialized = TRUE;

    dcal->day_of_week_start = G_DATE_SUNDAY;

    // Sunday = 1, M = 2, T = 3, W = 4, Th = 5, Fr = 6, Sat = 7
    gint first_day = gnc_start_of_week ();

    // Convert to GDateWeekday 1=Mon,2=Tues,3=Wed,4=Thu,5=Fri,6=Sat,7=Sun
    if (first_day == 1)
        first_day = G_DATE_SUNDAY;
    else
        first_day = first_day - 1;

    if (first_day > 0 && first_day < 8)
        dcal->day_of_week_start = first_day;

    gtk_widget_set_visible (GTK_WIDGET(dcal), TRUE);
}

static void
_gdc_set_cal_min_size_req (GncDenseCal *dcal)
{
    guint min_width, min_height;

    _gdc_compute_min_size (dcal, &min_width, &min_height);
    gtk_widget_set_size_request (GTK_WIDGET (dcal->cal_overlay), min_width, min_height);
}

GtkWidget*
gnc_dense_cal_new (GtkWindow *parent)
{
    GncDenseCal *dcal = g_object_new (GNC_TYPE_DENSE_CAL, NULL);

    (void)parent;

    return GTK_WIDGET(dcal);
}

GtkWidget*
gnc_dense_cal_new_with_model (GtkWindow *parent, GncDenseCalModel *model)
{
    GncDenseCal *cal = GNC_DENSE_CAL(gnc_dense_cal_new (parent));
    gnc_dense_cal_set_model (cal, model);
    return GTK_WIDGET(cal);
}

static void
recompute_first_of_month_offset (GncDenseCal *dcal)
{
    GDate *tmpDate;

    tmpDate = g_date_new_dmy (1, dcal->month, dcal->year);
    dcal->firstOfMonthOffset = g_date_get_weekday (tmpDate) % 7;
    g_date_free (tmpDate);
}

void
gnc_dense_cal_set_month (GncDenseCal *dcal, GDateMonth mon)
{
    _gnc_dense_cal_set_month (dcal, mon, TRUE);
}

static void
_gnc_dense_cal_set_month (GncDenseCal *dcal, GDateMonth mon, gboolean redraw)
{
    if (dcal->month == mon)
        return;

    dcal->month = mon;

    recompute_first_of_month_offset (dcal);

    recompute_extents (dcal);

    if (redraw && gtk_widget_get_realized (GTK_WIDGET(dcal)))
    {
        recompute_x_y_scales (dcal);
        gnc_dense_cal_draw_to_buffer (dcal);
        gtk_widget_queue_draw (GTK_WIDGET(dcal->cal_drawing_area));
    }
}

void
gnc_dense_cal_set_year (GncDenseCal *dcal, guint year)
{
    _gnc_dense_cal_set_year (dcal, year, TRUE);
}

static void
_gnc_dense_cal_set_year (GncDenseCal *dcal, guint year, gboolean redraw)
{
    if (dcal->year == year)
        return;
    dcal->year = year;
    recompute_first_of_month_offset (dcal);
    recompute_extents (dcal);
    if (redraw && gtk_widget_get_realized (GTK_WIDGET(dcal)))
    {
        recompute_x_y_scales (dcal);
        gnc_dense_cal_draw_to_buffer (dcal);
        gtk_widget_queue_draw (GTK_WIDGET(dcal->cal_drawing_area));
    }
}

void
gnc_dense_cal_set_num_months (GncDenseCal *dcal, guint num_months)
{
    guint closest_option = 0;
    int closest_index_distance = G_MAXINT;

    for (guint i = 0; i < G_N_ELEMENTS (gdc_view_option_months); i++)
    {
        gint delta_months = abs ((gint)gdc_view_option_months[i] - (gint)num_months);
        if (delta_months < closest_index_distance)
        {
            closest_option = i;
            closest_index_distance = delta_months;
        }
    }

    // Synchronize the nearest predefined view without re-entering this setter.
    g_signal_handlers_block_by_func (dcal->view_options, _gdc_view_option_changed, dcal);
    gtk_drop_down_set_selected (dcal->view_options, closest_option);
    g_signal_handlers_unblock_by_func (dcal->view_options, _gdc_view_option_changed, dcal);

    dcal->monthsPerCol = gdc_view_option_columns[closest_option];

    dcal->numMonths = num_months;
    recompute_extents (dcal);
    recompute_mark_storage (dcal);
    if (gtk_widget_get_realized (GTK_WIDGET(dcal)))
    {
        recompute_x_y_scales (dcal);
        gnc_dense_cal_draw_to_buffer (dcal);
        gtk_widget_queue_draw (GTK_WIDGET(dcal->cal_drawing_area));
    }
}

guint
gnc_dense_cal_get_num_months (GncDenseCal *dcal)
{
    return dcal->numMonths;
}

void
gnc_dense_cal_set_months_per_col (GncDenseCal *dcal, guint monthsPerCol)
{
    dcal->monthsPerCol = monthsPerCol;
    recompute_x_y_scales (dcal);
}

GDateMonth
gnc_dense_cal_get_month (GncDenseCal *dcal)
{
    return dcal->month;
}

GDateYear
gnc_dense_cal_get_year (GncDenseCal *dcal)
{
    return dcal->year;
}

static void
gnc_dense_cal_dispose (GObject *object)
{
    GncDenseCal *dcal;
    g_return_if_fail (object != NULL);
    g_return_if_fail (GNC_IS_DENSE_CAL(object));

    dcal = GNC_DENSE_CAL(object);

    if (dcal->disposed)
        return;
    dcal->disposed = TRUE;

    if (dcal->transPopup)
    {
        gtk_popover_popdown (dcal->transPopup);
        gtk_widget_unparent (GTK_WIDGET(dcal->transPopup));
        dcal->transPopup = NULL;
    }

    if (dcal->surface)
    {
        cairo_surface_destroy (dcal->surface);
        dcal->surface = NULL;
    }

    /* FIXME: we have a bunch of cleanup to do, here. */

    gdc_free_all_mark_data (dcal);

    if (dcal->model)
    {
        g_object_unref (dcal->model);
        dcal->model = NULL;
    }

    G_OBJECT_CLASS(gnc_dense_cal_parent_class)->dispose(object);
}

static void
gnc_dense_cal_finalize (GObject *object)
{
    g_return_if_fail (object != NULL);
    g_return_if_fail (GNC_IS_DENSE_CAL(object));

    G_OBJECT_CLASS(gnc_dense_cal_parent_class)->finalize(object);
}

static void
gdc_reconfig (GncDenseCal *dcal)
{
    GtkWidget *widget = GTK_WIDGET (dcal->cal_drawing_area);
    int scale = MAX (gtk_widget_get_scale_factor (widget), 1);
    int width = MAX (gtk_widget_get_width (widget), 1);
    int height = MAX (gtk_widget_get_height (widget), 1);

    if (dcal->surface)
        cairo_surface_destroy (dcal->surface);

    dcal->surface = cairo_image_surface_create (CAIRO_FORMAT_ARGB32,
                                                width * scale, height * scale);
    cairo_surface_set_device_scale (dcal->surface, scale, scale);
    gnc_dense_cal_draw_to_buffer (dcal);
}
static void
_gdc_compute_min_size (GncDenseCal *dcal, guint *min_width, guint *min_height)
{
    if (min_width != NULL)
    {
        *min_width =
            (dcal->leftPadding * 2)
            + (num_cols (dcal) * (col_width_at (dcal, dcal->min_x_scale)
                                 + dcal->month_side_bar_width))
            + ((num_cols (dcal) - 1) * COL_BORDER_SIZE);
    }

    if (min_height != NULL)
    {
        *min_height =
            (dcal->topPadding * 2)
            + MINOR_BORDER_SIZE
            + dcal->day_top_bar_height
            + (num_weeks_per_col (dcal)
               * week_height_at (dcal, dcal->min_y_scale));
    }
}

static void
recompute_x_y_scales (GncDenseCal *dcal)
{
    int denom;
    int width, height;

    width = DENSE_CAL_DEFAULT_WIDTH;
    height = DENSE_CAL_DEFAULT_HEIGHT;
    if (dcal->initialized)
    {
        width = gtk_widget_get_width (GTK_WIDGET (dcal->cal_drawing_area));
        height = gtk_widget_get_height (GTK_WIDGET (dcal->cal_drawing_area));
    }

    /* FIXME: there's something slightly wrong in the x_scale computation that
     * lets us draw larger than our area. */
    denom = 7 * num_cols (dcal);
    g_assert (denom != 0);
    dcal->x_scale = ((gint)(width
                            - (dcal->leftPadding * 2)
                            - (num_cols (dcal) * ((8 * MINOR_BORDER_SIZE)
                                    + dcal->month_side_bar_width))
                            - ((num_cols (dcal) - 1) * COL_BORDER_SIZE))
                     / denom);
    dcal->x_scale = MAX(dcal->x_scale, dcal->min_x_scale);

    denom = num_weeks_per_col (dcal);
    g_assert (denom != 0);
    dcal->y_scale = ((gint)(height
                            - (dcal->topPadding * 2)
                            - MINOR_BORDER_SIZE
                            - dcal->day_top_bar_height
                            - (num_weeks_per_col (dcal) - 1
                               * MINOR_BORDER_SIZE))
                     / denom);
    dcal->y_scale = MAX(dcal->y_scale, dcal->min_y_scale);

    _gdc_set_cal_min_size_req (dcal);
}

static void
gdc_free_all_mark_data (GncDenseCal *dcal)
{
    int i;
    GList *l;
    for (i = 0; i < dcal->numMarks; i++)
    {
        /* Each of these just contains an elt of dcal->markData,
         * which we're about to free, below... */
        g_list_free (dcal->marks[i]);
    }
    g_free (dcal->marks);
    dcal->marks = NULL;
    /* Remove the old mark data. */
    for (l = dcal->markData; l; l = l->next)
    {
        gdc_mark_data *mark = l->data;
        g_list_free (mark->ourMarks);
        g_free (mark->name);
        g_free (mark->info);
        g_free (mark);
    }
    g_list_free (dcal->markData);
    dcal->markData = NULL;
}

static void
recompute_mark_storage (GncDenseCal *dcal)
{
    if (dcal->marks == NULL)
        goto createNew;
    gdc_free_all_mark_data (dcal);

createNew:
    dcal->numMarks = num_weeks (dcal) * 7;
    dcal->marks = g_new0 (GList*, dcal->numMarks);
    if (dcal->model)
        gdc_add_markings (dcal);
}

static gint
get_week_of_year (GncDenseCal *dcal, GDate *d)
{
    GDateWeekday fwd, lwd;
    GDateYear year;
    guint day;
    GDate first, last;
    guint ret;
    gint monday_offset = 1;
    gint day_offset = 0;

    g_return_val_if_fail (g_date_valid (d), 0);

    year = g_date_get_year (d);

    if (!d->dmy)
        return 0;

    g_date_clear (&first, 1);
    g_date_set_dmy (&first, 1, 1, year);

    fwd = g_date_get_weekday (&first);

    day_offset = (fwd + 7 - dcal->day_of_week_start) % 7;

    if (dcal->day_of_week_start == G_DATE_SUNDAY) //Su,M,T,W,T,F,Sa
        monday_offset = 1;
    else if (dcal->day_of_week_start == G_DATE_MONDAY) //M,T,W,T,F,Sa,Su
        monday_offset = 0;
    else if (dcal->day_of_week_start == G_DATE_TUESDAY) //T,W,T,F,Sa,Su,M
        monday_offset = 6;
    else if (dcal->day_of_week_start == G_DATE_WEDNESDAY) //W,T,F,Sa,Su,M,T
        monday_offset = 5;
    else if (dcal->day_of_week_start == G_DATE_THURSDAY) //T,F,Sa,Su,M,T,W
        monday_offset = 4;
    else if (dcal->day_of_week_start == G_DATE_FRIDAY) //F,Sa,Su,M,T,W,T
        monday_offset = 3;
    else if (dcal->day_of_week_start == G_DATE_SATURDAY) //Sa,Su,M,T,W,T,F,
        monday_offset = 2;
    else
        monday_offset = 1;

    day = g_date_get_day_of_year (d) - 1;

    g_date_clear (&last, 1);
    g_date_set_dmy (&last, 31, 12, year - 1);
    lwd = g_date_get_weekday (&last);
    gint lday_offset = 6 - ((lwd + 7 - dcal->day_of_week_start) % 7);
    gint addone = 1;

    if (lday_offset)
        addone = 0;

    ret = ((day + day_offset)/7U + ((day_offset <= monday_offset) ? addone : 0));

    return ret;
}

static gint
get_weeks_in_year (GncDenseCal *dcal, GDateYear year)
{
    GDate d;

    g_return_val_if_fail (g_date_valid_year (year), 0);

    g_date_clear (&d, 1);
    g_date_set_dmy (&d, 1, 1, year);
    if (g_date_get_weekday (&d) == dcal->day_of_week_start) return 53;
    g_date_set_dmy (&d, 31, 12, year);
    if (g_date_get_weekday (&d) == dcal->day_of_week_start) return 53;
    if (g_date_is_leap_year (year))
    {
        g_date_set_dmy (&d, 2, 1, year);
        if (g_date_get_weekday (&d) == dcal->day_of_week_start) return 53;
        g_date_set_dmy (&d, 30, 12, year);
        if (g_date_get_weekday (&d) == dcal->day_of_week_start) return 53;
    }
    return 52;
}

static void
recompute_extents (GncDenseCal *dcal)
{
    GDate date;
    gint start_week, end_week;

    g_date_clear (&date, 1);
    g_date_set_dmy (&date, 1, dcal->month, dcal->year);
    start_week = get_week_of_year (dcal, &date);
    g_date_add_months (&date, dcal->numMonths);
    end_week = get_week_of_year (dcal, &date);

    if (g_date_get_year (&date) != dcal->year)
        end_week += get_weeks_in_year (dcal, dcal->year);

    dcal->num_weeks = end_week - start_week + 1;
}

static void
free_rect (gpointer data, gpointer user_data)
{
    g_free ((GdkRectangle*)data);
}

static void
gnc_dense_cal_draw (GtkDrawingArea *area, cairo_t *cr,
                    int width, int height, gpointer user_data)
{
    GncDenseCal *dcal = GNC_DENSE_CAL (user_data);

    g_return_if_fail (GNC_IS_DENSE_CAL (dcal));

    (void)area;
    (void)width;
    (void)height;

    if (!dcal->surface)
        return;

    cairo_save (cr);
    cairo_set_source_surface (cr, dcal->surface, 0, 0);
    cairo_paint (cr);
    cairo_restore (cr);
}

static void
gnc_dense_cal_resize (GtkDrawingArea *area, int width, int height,
                      gpointer user_data)
{
    GncDenseCal *dcal = GNC_DENSE_CAL (user_data);

    g_return_if_fail (GNC_IS_DENSE_CAL (dcal));

    if (width < 1 || height < 1)
        return;

    recompute_x_y_scales (dcal);
    gdc_reconfig (dcal);
    gtk_widget_queue_draw (GTK_WIDGET (area));
}

static void
gdc_clear_background_layer (GncDenseCal *dcal)
{
    GtkWidget *child = gtk_widget_get_first_child (
        GTK_WIDGET (dcal->cal_background_layer));

    while (child)
    {
        GtkWidget *next = gtk_widget_get_next_sibling (child);
        gtk_fixed_remove (dcal->cal_background_layer, child);
        child = next;
    }
}

static void
gdc_add_background_panel (GncDenseCal *dcal, int x, int y, int width,
                          int height, const char *css_class)
{
    GtkWidget *panel;

    if (width <= 0 || height <= 0)
        return;

    panel = gtk_box_new (GTK_ORIENTATION_VERTICAL, 0);
    gtk_widget_add_css_class (panel, css_class);
    gtk_widget_set_size_request (panel, width, height);
    gtk_fixed_put (dcal->cal_background_layer, panel, x, y);
}

static void
gdc_draw_layout (cairo_t *cr, PangoLayout *layout, double x, double y)
{
    cairo_move_to (cr, x, y);
    pango_cairo_show_layout (cr, layout);
}

static void
gnc_dense_cal_draw_to_buffer (GncDenseCal *dcal)
{
    GtkWidget *widget = GTK_WIDGET (dcal->cal_drawing_area);
    GdkRGBA foreground;
    const char *primary_class;
    const char *secondary_class;
    const char *marker_class;
    PangoLayout *layout;
    cairo_t *cr;
    gint i;
    int max_width;

    if (!dcal->surface)
        return;

    cr = cairo_create (dcal->surface);
    cairo_save (cr);
    cairo_set_operator (cr, CAIRO_OPERATOR_CLEAR);
    cairo_paint (cr);
    cairo_restore (cr);

    layout = gtk_widget_create_pango_layout (widget, NULL);
    gtk_widget_get_color (widget, &foreground);
    primary_class = gnc_is_dark_theme (&foreground) ? "primary-dark" : "primary";
    secondary_class = gnc_is_dark_theme (&foreground) ? "secondary-dark" : "secondary";
    marker_class = gnc_is_dark_theme (&foreground) ? "markers-dark" : "markers";
    gdc_clear_background_layer (dcal);

    pango_layout_set_text (layout, "S", -1);
    pango_layout_get_pixel_size (layout, NULL, &dcal->label_height);
    dcal->month_side_bar_width = dcal->label_height + (dcal->bar_label_padding * 2);
    dcal->day_top_bar_height = dcal->label_height + (dcal->bar_label_padding * 2);

    for (i = 0; i < 12; i++)
        dcal->monthPositions[i].x = dcal->monthPositions[i].y = -1;

    for (i = 0; i < dcal->numMonths; i++)
    {
        GList *month_rects = NULL;

        month_coords (dcal, i, &month_rects);
        dcal->monthPositions[i].x = floor (i / dcal->monthsPerCol)
                                   * (col_width (dcal) + COL_BORDER_SIZE);
        dcal->monthPositions[i].y = ((GdkRectangle *)month_rects->next->next->next->data)->y;
        for (GList *iter = month_rects; iter; iter = iter->next)
        {
            GdkRectangle *rect = iter->data;
            gdc_add_background_panel (dcal, rect->x, rect->y, rect->width,
                                      rect->height,
                                      i % 2 == 0 ? primary_class : secondary_class);
        }
        g_list_foreach (month_rects, free_rect, NULL);
        g_list_free (month_rects);
    }

    for (i = 0; i < dcal->numMarks; i++)
    {
        int x1, x2, y1, y2;

        if (!dcal->marks[i])
            continue;
        doc_coords (dcal, i, &x1, &y1, &x2, &y2);
        int radius = MIN ((x2 - x1), (y2 - y1)) * .75;
        int center_x = (x1 + x2) / 2 + ((x2 - x1) % 2 != 0);
        int center_y = (y1 + y2) / 2 + ((y2 - y1) % 2 != 0);
        gdc_add_background_panel (dcal, center_x - (radius + 2),
                                  center_y - radius, (radius * 2) + 4,
                                  radius * 2, marker_class);
    }

    cairo_set_source_rgba (cr, foreground.red, foreground.green,
                           foreground.blue, foreground.alpha);
    cairo_set_line_width (cr, 1.0);

    for (i = 0; i < num_cols (dcal); i++)
    {
        gint x = dcal->leftPadding + i * (col_width (dcal) + COL_BORDER_SIZE)
                 + dcal->month_side_bar_width + 1;
        gint y = dcal->topPadding + dcal->day_top_bar_height;
        gint width = col_width (dcal) - COL_BORDER_SIZE - dcal->month_side_bar_width;
        gint height = col_height (dcal);

        cairo_rectangle (cr, x + 0.5, y + 0.5, width, height);
        cairo_stroke (cr);
        for (gint week = 0; week < num_weeks_per_col (dcal); week++)
        {
            gint week_y = y + week * week_height (dcal);
            cairo_move_to (cr, x, week_y + 0.5);
            cairo_line_to (cr, x + width, week_y + 0.5);
            cairo_stroke (cr);
        }
        for (gint day = 1; day < 7; day++)
        {
            gint day_x = x + day * day_width (dcal);
            cairo_move_to (cr, day_x + 0.5, y);
            cairo_line_to (cr, day_x + 0.5, y + height);
            cairo_stroke (cr);
        }

        pango_layout_set_text (layout, "88", -1);
        pango_layout_get_pixel_size (layout, &max_width, NULL);
        if (dcal->x_scale > max_width)
        {
            gdc_add_background_panel (dcal, x, y - dcal->day_top_bar_height,
                                      day_width (dcal) * 7 + 1,
                                      dcal->day_top_bar_height, "header");
            for (gint day = 0; day < 7; day++)
            {
                gchar label[5] = { 0 };
                int label_width;
                gint label_x;

                day_label (label, sizeof label, (day + dcal->day_of_week_start) % 7);
                pango_layout_set_text (layout, label, -1);
                pango_layout_get_pixel_size (layout, &label_width, NULL);
                label_x = x + day * day_width (dcal) + day_width (dcal) / 2
                          - label_width / 2;
                gdc_draw_layout (cr, layout, label_x,
                                 y - dcal->day_top_bar_height
                                 + dcal->bar_label_padding);
            }
        }
    }

    for (i = 0; i < dcal->numMonths; i++)
    {
        guint idx;

        if (dcal->monthPositions[i].x == -1)
            break;
        gdc_add_background_panel (dcal,
                                  dcal->monthPositions[i].x + dcal->leftPadding,
                                  dcal->topPadding,
                                  dcal->month_side_bar_width + 1,
                                  col_height (dcal) + dcal->day_top_bar_height + 1,
                                  "header");
        idx = (dcal->month - 1 + i) % 12;
        pango_layout_set_text (layout, month_name (idx), -1);
        cairo_save (cr);
        cairo_translate (cr, dcal->monthPositions[i].x + dcal->leftPadding,
                         dcal->monthPositions[i].y);
        cairo_rotate (cr, -G_PI / 2.);
        gdc_draw_layout (cr, layout, 0, dcal->bar_label_padding);
        cairo_restore (cr);
    }

    {
        GDate date, end;
        GDate today;
        gboolean today_found = FALSE;
        gint doc;

        g_date_clear (&today, 1);
        gnc_gdate_set_today (&today);
        g_date_set_dmy (&date, 1, dcal->month, dcal->year);
        end = date;
        g_date_add_months (&end, dcal->numMonths);
        for (doc = 0; g_date_get_julian (&date) < g_date_get_julian (&end);
             g_date_add_days (&date, 1), doc++)
        {
            gchar day_text[4] = { 0 };
            gint x1, x2, y1, y2, text_width, text_height;
            gint width, height;

            doc_coords (dcal, doc, &x1, &y1, &x2, &y2);
            g_snprintf (day_text, sizeof day_text, "%d", g_date_get_day (&date));
            pango_layout_set_text (layout, day_text, -1);
            pango_layout_get_pixel_size (layout, &text_width, &text_height);
            width = x2 - x1 + 1;
            height = y2 - y1 + 1;
            if (!today_found && g_date_compare (&date, &today) == 0)
            {
                today_found = TRUE;
                cairo_rectangle (cr, x1 - day_width (dcal) / 4.0 + 3.5,
                                 y1 - day_height (dcal) / 4.0 + 2.5,
                                 day_width (dcal) - 5,
                                 day_height (dcal) - 5);
                cairo_stroke (cr);
            }
            gdc_draw_layout (cr, layout, x1 + (width - text_width) / 2,
                             y1 + (height - text_height) / 2);
        }
    }

    g_object_unref (layout);
    cairo_destroy (cr);
}
static void
populate_hover_window (GncDenseCal *dcal)
{
    GDate *date;

    if (dcal->doc >= 0)
    {
        GList *l;

        date = g_date_new_dmy (1, dcal->month, dcal->year);
        g_date_add_days (date, dcal->doc);
        /* Note: the ISO date format (%F or equivalently
         * %Y-%m-%d) is not a good idea here since many
         * locales will want to use a very different date
         * format. Please leave the specification of the date
         * format up to the preference.  */
        time64 t64 = gnc_dmy2time64_neutral (g_date_get_day (date),
                                             g_date_get_month (date),
                                             g_date_get_year (date));
        gchar date_buff [MAX_DATE_LENGTH + 1];
        qof_print_date_buff (date_buff, MAX_DATE_LENGTH, t64);
        gtk_label_set_text (dcal->popup_date_label, date_buff);

        GtkWidget *row;
        while ((row = gtk_widget_get_first_child (GTK_WIDGET (dcal->popup_marks))))
            gtk_list_box_remove (dcal->popup_marks, row);

        for (l = dcal->marks[dcal->doc]; l; l = l->next)
        {
            gdc_mark_data *gdcmd = l->data;
            GtkWidget *box = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 12);
            GtkWidget *name = gtk_label_new (gdcmd->name ? gdcmd->name : _("(unnamed)"));
            GtkWidget *info = gtk_label_new (gdcmd->info);

            gtk_widget_set_hexpand (name, TRUE);
            gtk_label_set_xalign (GTK_LABEL (name), 0.0f);
            gtk_label_set_xalign (GTK_LABEL (info), 0.0f);
            gtk_box_append (GTK_BOX (box), name);
            gtk_box_append (GTK_BOX (box), info);
            row = gtk_list_box_row_new ();
            gtk_list_box_row_set_child (GTK_LIST_BOX_ROW (row), box);
            gtk_list_box_append (dcal->popup_marks, row);
        }

        if (!gtk_widget_get_first_child (GTK_WIDGET (dcal->popup_marks)))
        {
            row = gtk_list_box_row_new ();
            gtk_list_box_row_set_child (GTK_LIST_BOX_ROW (row), gtk_label_new (""));
            gtk_list_box_append (dcal->popup_marks, row);
        }

        g_date_free (date);
    }
}

static void
set_popup_pointing_to (GncDenseCal *dcal, double x, double y)
{
    GdkRectangle rect = { (int)x, (int)y, 1, 1 };

    gtk_popover_set_pointing_to (dcal->transPopup, &rect);
}

static void
gnc_dense_cal_popup_closed (GtkPopover *popover, GncDenseCal *dcal)
{
    (void)popover;

    dcal->showPopup = FALSE;
    dcal->doc = -1;
}

static void
gnc_dense_cal_click_pressed (GtkGestureClick *gesture, int n_press,
                             double x, double y, gpointer user_data)
{
    GncDenseCal *dcal = GNC_DENSE_CAL (user_data);

    (void)gesture;
    (void)n_press;

    dcal->doc = wheres_this (dcal, (int)x, (int)y);
    dcal->showPopup = !dcal->showPopup;
    if (dcal->showPopup && dcal->doc >= 0)
    {
        populate_hover_window (dcal);
        set_popup_pointing_to (dcal, x, y);
        gtk_popover_popup (dcal->transPopup);
    }
    else
    {
        dcal->doc = -1;
        gtk_popover_popdown (dcal->transPopup);
    }
}

static void
gnc_dense_cal_motion (GtkEventControllerMotion *controller,
                      double x, double y, gpointer user_data)
{
    GncDenseCal *dcal = GNC_DENSE_CAL (user_data);
    gint doc;

    (void)controller;

    if (!dcal->showPopup)
        return;

    doc = wheres_this (dcal, (int)x, (int)y);
    if (doc >= 0)
    {
        if (dcal->doc != doc)
        {
            dcal->doc = doc;
            populate_hover_window (dcal);
        }
        set_popup_pointing_to (dcal, x, y);
        gtk_popover_popup (dcal->transPopup);
    }
    else
    {
        dcal->doc = -1;
        gtk_popover_popdown (dcal->transPopup);
    }
}


static void
_gdc_view_option_changed (GObject *widget, GParamSpec *pspec, gpointer user_data)
{
    guint selected = gtk_drop_down_get_selected (GTK_DROP_DOWN (widget));

    (void)pspec;

    if (selected == GTK_INVALID_LIST_POSITION ||
        selected >= G_N_ELEMENTS (gdc_view_option_months))
        return;
    DEBUG("changing to %d months", gdc_view_option_months[selected]);
    gnc_dense_cal_set_num_months (GNC_DENSE_CAL(user_data),
                                  gdc_view_option_months[selected]);
}

static inline int
day_width_at (GncDenseCal *dcal, guint xScale)
{
    return xScale + MINOR_BORDER_SIZE;
}

static inline int
day_width (GncDenseCal *dcal)
{
    return day_width_at (dcal, dcal->x_scale);
}

static inline int
day_height_at (GncDenseCal *dcal, guint yScale)
{
    return yScale + MINOR_BORDER_SIZE;
}

static inline int
day_height (GncDenseCal *dcal)
{
    return day_height_at (dcal, dcal->y_scale);
}

static inline int
week_width_at (GncDenseCal *dcal, guint xScale)
{
    return day_width_at (dcal, xScale) * 7;
}

static inline int
week_width (GncDenseCal *dcal)
{
    return week_width_at (dcal, dcal->x_scale);
}

static inline int
week_height_at (GncDenseCal *dcal, guint yScale)
{
    return day_height_at (dcal, yScale);
}

static inline int
week_height (GncDenseCal *dcal)
{
    return week_height_at (dcal, dcal->y_scale);
}

static inline int
col_width_at (GncDenseCal *dcal, guint xScale)
{
    return (week_width_at (dcal, xScale)
            + dcal->month_side_bar_width
            + COL_BORDER_SIZE);
}

static inline int
col_width (GncDenseCal *dcal)
{
    return col_width_at (dcal, dcal->x_scale);
}

static inline int
col_height (GncDenseCal *dcal)
{
    return week_height (dcal) * num_weeks_per_col (dcal);
}

static inline int
num_cols (GncDenseCal *dcal)
{
    return ceil ((float)dcal->numMonths / (float)dcal->monthsPerCol);
}

static inline int
num_weeks (GncDenseCal *dcal)
{
    return dcal->num_weeks;
}

static
int num_weeks_per_col (GncDenseCal *dcal)
{
    int num_weeks_toRet, numCols, i;
    GDate *start, *end;
    int startWeek, endWeek;

    start = g_date_new ();
    end = g_date_new ();

    num_weeks_toRet = 0;
    numCols = num_cols (dcal);

    for (i = 0; i < numCols; i++)
    {
        g_date_set_dmy (start, 1,
                        ((dcal->month - 1
                          + (i * dcal->monthsPerCol)) % 12)
                          + 1,
                        dcal->year + floor ((dcal->month - 1
                                             + (i * dcal->monthsPerCol))
                                             / 12));
        *end = *start;
        /* Add the smaller of (the number of months in the
         * calendar-display, minus the number of months shown in the
         * previous columns) or (the number of months in a column) */
        g_date_add_months (end, MIN(dcal->numMonths,
                                    MIN(dcal->monthsPerCol,
                                        dcal->numMonths
                                        - ((i - 1)
                                           * dcal->monthsPerCol))));
        g_date_subtract_days (end, 1);
        startWeek = get_week_of_year (dcal, start);
        endWeek = get_week_of_year (dcal, end);

        if (endWeek < startWeek)
            endWeek += get_weeks_in_year (dcal, g_date_get_year (start));

        num_weeks_toRet = MAX(num_weeks_toRet, (endWeek - startWeek) + 1);
    }
    g_date_free (start);
    g_date_free (end);
    return num_weeks_toRet;
}

/**
 * @param monthOfCal 0-based; offset of calendar's first month.
 * @param outList A GList in which to place GdkRectangle's of the extents of
 * each week.  4 or 5 GdkRectangle*s will be added to the list, as per the
 * size of the month.
 **/
static void
month_coords (GncDenseCal *dcal, int monthOfCal, GList **outList)
{
    gint weekRow, colNum, previousMonthsInCol, monthOffset;
    gint start;
    GDate *startD, *endD;
    GdkRectangle *rect;
    gint startWk, endWk;

    if (monthOfCal > dcal->numMonths)
        return;

    colNum = floor (monthOfCal / dcal->monthsPerCol);
    monthOffset = colNum * dcal->monthsPerCol;
    previousMonthsInCol = MAX(0, (monthOfCal % dcal->monthsPerCol));

    startD = g_date_new ();
    endD = g_date_new ();

    /* Calculate the number of weeks in the column before the month we're
     * interested in. */
    weekRow = 0;
    if (previousMonthsInCol > 0)
    {
        g_date_set_dmy (startD, 1,
                                ((dcal->month - 1 + monthOffset) % 12) + 1,
                                dcal->year + floor ((dcal->month - 1 + monthOffset) / 12));
        /* get the week of the top of the column */
        startWk = get_week_of_year (dcal, startD);
        /* get the week of the end of the previous months */
        *endD = *startD;
        g_date_add_months (endD, previousMonthsInCol);
        g_date_subtract_days (endD, 1);
        endWk = get_week_of_year (dcal, endD);

        if (endWk < startWk)
             endWk += get_weeks_in_year (dcal, g_date_get_year (startD));

        /* determine how many weeks are before the month we're
         * interested in. */
        weekRow = endWk - startWk;

        gint end_of_week = dcal->day_of_week_start + 6;
        if (end_of_week > 7)
            end_of_week = end_of_week - 7;

        if (g_date_get_weekday (endD) == end_of_week)
            weekRow++;
    }

    g_date_set_dmy (startD, 1,
                            ((dcal->month - 1 + monthOfCal) % 12) + 1,
                            dcal->year + floor ((dcal->month - 1 + monthOfCal) / 12));

    *endD = *startD;
    g_date_add_months (endD, 1);
    g_date_subtract_days (endD, 1);

    /* Get the first week. */
    {
        start = (g_date_get_weekday (startD) + 7 - dcal->day_of_week_start) % 7;

        rect = g_new0 (GdkRectangle, 1);
        rect->x = dcal->leftPadding
                  + MINOR_BORDER_SIZE
                  + (colNum * (col_width (dcal) + COL_BORDER_SIZE))
                  + dcal->month_side_bar_width
                  + (start * day_width (dcal));
        rect->y = dcal->topPadding
                  + dcal->day_top_bar_height
                  + MINOR_BORDER_SIZE
                  + (weekRow * week_height (dcal));
        rect->width = (7 - start) * day_width (dcal);
        rect->height = week_height (dcal);
        *outList = g_list_append (*outList, (gpointer)rect);
        rect = NULL;
    }

    /* Get the middle weeks. */
    {
        gint i;
        gint weekStart = get_week_of_year (dcal, startD) + 1;
        gint weekEnd = get_week_of_year (dcal, endD);

        for (i = weekStart; i < weekEnd; i++)
        {
            rect = g_new0 (GdkRectangle, 1);
            rect->x = dcal->leftPadding
                      + MINOR_BORDER_SIZE
                      + dcal->month_side_bar_width
                      + (colNum * (col_width (dcal) + COL_BORDER_SIZE));
            rect->y = dcal->topPadding
                      + dcal->day_top_bar_height
                      + MINOR_BORDER_SIZE
                      + ((weekRow + (i - weekStart) + 1) * week_height (dcal));
            rect->width  = week_width (dcal);
            rect->height = week_height (dcal);

            *outList = g_list_append (*outList, (gpointer)rect);
            rect = NULL;
        }
    }

    /* Get the last week. */
    {
        gint start_week_of_year = get_week_of_year (dcal, startD);
        gint end_week_of_year = get_week_of_year (dcal, endD);

        rect = g_new0 (GdkRectangle, 1);
        rect->x = dcal->leftPadding
                  + MINOR_BORDER_SIZE
                  + dcal->month_side_bar_width
                  + (colNum * (col_width (dcal) + COL_BORDER_SIZE));
        rect->y = dcal->topPadding
                  + MINOR_BORDER_SIZE
                  + dcal->day_top_bar_height
                  + ((weekRow
                      + (end_week_of_year - start_week_of_year))
                     * week_height (dcal));
        rect->width = (((g_date_get_weekday (endD) + 7 - dcal->day_of_week_start) % 7) + 1) * day_width (dcal);
        rect->height = week_height (dcal);

        *outList = g_list_append (*outList, (gpointer)rect);
        rect = NULL;
    }

    g_date_free (startD);
    g_date_free (endD);
}

/* FIXME: make this more like month_coords */
static void
doc_coords (GncDenseCal *dcal, int dayOfCal,
            int *x1, int *y1, int *x2, int *y2)
{
    GDate d;
    gint docMonth;
    gint d_week_of_cal, top_of_col_week_of_cal;
    gint colNum, dayCol, weekRow;

    /* FIXME: add range checks */
    g_date_set_dmy (&d, 1, dcal->month, dcal->year);
    g_date_add_days (&d, dayOfCal);
    docMonth = g_date_get_month (&d);
    if (g_date_get_year (&d) != dcal->year)
    {
        docMonth += 12;
    }
    colNum  = floor ((float)(docMonth - dcal->month) / (float)dcal->monthsPerCol);
    dayCol = g_date_get_weekday (&d) - dcal->day_of_week_start;

    if (dayCol < 0)
      dayCol = dayCol + 7;

    d_week_of_cal = get_week_of_year (dcal, &d);
    g_date_set_dmy (&d, 1, dcal->month, dcal->year);
    g_date_add_months (&d, (colNum * dcal->monthsPerCol));
    top_of_col_week_of_cal = get_week_of_year (dcal, &d);

    if (d_week_of_cal < top_of_col_week_of_cal)
    {
        gint week_offset = get_weeks_in_year (dcal, dcal->year);
        d_week_of_cal += week_offset;
    }
    weekRow = d_week_of_cal - top_of_col_week_of_cal;

    /* top-left corner */
    /* FIXME: this has the math to make the mark-cells come out right,
     * which it shouldn't. */
    *x1 = dcal->leftPadding
          + MINOR_BORDER_SIZE
          + dcal->month_side_bar_width
          + (colNum * (col_width (dcal) + COL_BORDER_SIZE))
          + (dayCol * day_width (dcal))
          + (day_width (dcal) / 4);
    *y1 = dcal->topPadding
          + MINOR_BORDER_SIZE
          + dcal->day_top_bar_height
          + (weekRow * week_height (dcal))
          + (day_height (dcal) / 4);

    *x2 = *x1 + (day_width (dcal) / 2);
    *y2 = *y1 + (day_height (dcal) / 2);
}

/**
 * Given x,y coordinates, returns the day-of-cal under the mouse; will return
 * '-1' if invalid.
 **/
static gint
wheres_this (GncDenseCal *dcal, int x, int y)
{
    gint colNum, weekRow, dayCol, dayOfCal;
    GDate d, startD;
    int width, height;

    x -= dcal->leftPadding;
    y -= dcal->topPadding;

    if ((x < 0) || (y < 0))
    {
        return -1;
    }
    width = gtk_widget_get_width (GTK_WIDGET (dcal->cal_drawing_area));
    height = gtk_widget_get_height (GTK_WIDGET (dcal->cal_drawing_area));
    if ((x >= width)
            || (y >= height))
    {
        return -1;
    }

    /* "outside of displayed table" check */
    if (x >= (num_cols(dcal) * (col_width (dcal) + COL_BORDER_SIZE)))
    {
        return -1;
    }
    if (y >= dcal->day_top_bar_height + col_height (dcal))
    {
        return -1;
    }

    /* coords -> year-relative-values */
    colNum = floor (x / (col_width (dcal) + COL_BORDER_SIZE));

    x %= (col_width (dcal) + COL_BORDER_SIZE);
    x -= dcal->month_side_bar_width;
    if (x < 0)
    {
        return -1;
    }
    if (x >= day_width (dcal) * 7)
    {
        return -1;
    }

    y -= dcal->day_top_bar_height;
    if (y < 0)
    {
        return -1;
    }

    dayCol = floor ((float)x / (float)day_width (dcal));
    weekRow = floor ((float)y / (float)week_height (dcal));

    g_date_set_dmy (&startD, 1, dcal->month, dcal->year);
    d = startD;
    g_date_add_months (&d, (colNum * dcal->monthsPerCol));

    if (dcal->day_of_week_start == G_DATE_SUNDAY)
        dayCol -= (g_date_get_weekday (&d) - 0) % 7;
    else
        dayCol -= (g_date_get_weekday (&d) - 1) % 7;

    if (weekRow == 0)
    {
        if (dayCol < 0)
        {
            return -1;
        }
    }
    g_date_add_days (&d, dayCol + (weekRow * 7));

    /* Check to make sure we're within the column's displayed range. */
    {
        GDate ccd;
        g_date_set_dmy (&ccd, 1, dcal->month, dcal->year);
        g_date_add_months (&ccd, (colNum + 1) * dcal->monthsPerCol);
        if (g_date_get_julian (&d) >= g_date_get_julian (&ccd))
        {
            return -1;
        }
    }

    dayOfCal = g_date_get_julian (&d) - g_date_get_julian (&startD);

    /* one more check before returning... */
    g_date_subtract_months (&d, dcal->numMonths);
    if (g_date_get_julian (&d) >= g_date_get_julian (&startD))
    {
        /* we're past the end of the displayed calendar, thus -1 */
        DEBUG("%d >= %d", g_date_get_julian (&d), g_date_get_julian (&startD));
        return -1;
    }

    return dayOfCal;
}

static gint
gdc_get_doc_offset (GncDenseCal *dcal, GDate *d)
{
    gint toRet;
    /* soc == start-of-calendar */
    GDate soc;

    g_date_clear (&soc, 1);
    g_date_set_dmy (&soc, 1, dcal->month, dcal->year);
    /* ensure not before calendar start. */
    if (g_date_get_julian (d) < g_date_get_julian (&soc))
        return -1;
    /* do computation here, since we're going to change the
     * start-of-calendar date. */
    toRet = g_date_get_julian (d) - g_date_get_julian (&soc);
    /* ensure not after end of visible calendar. */
    g_date_add_months (&soc, dcal->numMonths);
    if (g_date_get_julian (d) >= g_date_get_julian (&soc))
        return -1;
    /* return pre-computed value. */
    return toRet;
}

static void
gdc_add_tag_markings (GncDenseCal *cal, guint tag)
{
    gchar *name, *info;
    gint num_marks, idx;
    GDate **dates;
    GDate *calDate;

    // copy the values into the old marking function.
    name = gnc_dense_cal_model_get_name (cal->model, tag);
    info = gnc_dense_cal_model_get_info (cal->model, tag);
    num_marks = gnc_dense_cal_model_get_instance_count (cal->model, tag);

    if (num_marks == 0)
        goto cleanup;

    dates = g_new0 (GDate*, num_marks);
    calDate = g_date_new_dmy (1, cal->month, cal->year);

    for (idx = 0; idx < num_marks; idx++)
    {
        dates[idx] = g_date_new ();
        gnc_dense_cal_model_get_instance (cal->model, tag, idx, dates[idx]);

    }
    if (g_date_valid (dates[0]))
    {
        if (g_date_get_julian (dates[0]) < g_date_get_julian (calDate))
        {
            /* Oops, first marking is earlier than months displayed.
             * Choose new first month and recalculate all markings for all
             * tags. Their offsets are all wrong with the newly added month(s).
             */
            _gnc_dense_cal_set_month (cal, g_date_get_month (dates[0]), FALSE);
            _gnc_dense_cal_set_year (cal, g_date_get_year (dates[0]), FALSE);

            gdc_remove_markings (cal);
            gdc_add_markings (cal);
        }
        else
            gdc_mark_add (cal, tag, name, info, num_marks, dates);
    }
    else
    {
        g_warning ("Bad date, skipped.");
    }

    for (idx = 0; idx < num_marks; idx++)
    {
        g_date_free (dates[idx]);
    }
    g_free (dates);
    g_date_free (calDate);

cleanup:
    g_free (info);
}

static void
gdc_add_markings (GncDenseCal *cal)
{
    GList *tags = gnc_dense_cal_model_get_contained (cal->model);

    for (GList *n = tags; n; n = n->next)
        gdc_add_tag_markings (cal, GPOINTER_TO_UINT(n->data));

    g_list_free (tags);
}

static void
gdc_remove_markings (GncDenseCal *cal)
{
    GList *tags = gnc_dense_cal_model_get_contained (cal->model);

    for (GList *n = tags; n; n = n->next)
        gdc_mark_remove (cal, GPOINTER_TO_UINT(n->data), FALSE);

    g_list_free (tags);
}

static void
gdc_model_added_cb (GncDenseCalModel *model, guint added_tag, gpointer user_data)
{
    GncDenseCal *cal = GNC_DENSE_CAL(user_data);
    DEBUG("gdc_model_added_cb update");
    gdc_add_tag_markings (cal, added_tag);
}

static void
gdc_model_update_cb (GncDenseCalModel *model, guint update_tag, gpointer user_data)
{
    GncDenseCal *cal = GNC_DENSE_CAL(user_data);
    gint num_marks = 0;
    DEBUG("gdc_model_update_cb update for tag [%d]", update_tag);
    num_marks = gnc_dense_cal_model_get_instance_count (cal->model, update_tag);
    // We need to redraw if there are no mark, to ensure they're all erased.
    gdc_mark_remove (cal, update_tag, num_marks==0);
    gdc_add_tag_markings (cal, update_tag);

}

static void
gdc_model_removing_cb (GncDenseCalModel *model, guint remove_tag, gpointer user_data)
{
    GncDenseCal *cal = GNC_DENSE_CAL(user_data);
    DEBUG("gdc_model_removing_cb update [%d]", remove_tag);
    gdc_mark_remove (cal, remove_tag, TRUE);
}

void
gnc_dense_cal_set_model (GncDenseCal *cal, GncDenseCalModel *model)
{
    if (cal->model != NULL)
    {
        gdc_remove_markings (cal);
        g_object_unref (G_OBJECT(cal->model));
        cal->model = NULL;
    }
    cal->model = model;
    g_object_ref (G_OBJECT(model));
    g_signal_connect (G_OBJECT(cal->model), "added", (GCallback)gdc_model_added_cb, cal);
    g_signal_connect (G_OBJECT(cal->model), "update", (GCallback)gdc_model_update_cb, cal);
    g_signal_connect (G_OBJECT(cal->model), "removing", (GCallback)gdc_model_removing_cb, cal);

    gdc_add_markings (cal);
}

/**
 * Marks the given array of GDate*s on the calendar with the given name.
 **/
static void
gdc_mark_add (GncDenseCal *dcal,
              guint tag,
              gchar *name,
              gchar *info,
              guint size,
              GDate **dateArray)
{
    guint i;
    gint doc;
    gdc_mark_data *newMark;
    GDate *d;

    if (size == 0)
    {
        g_error ("0 size not allowed");
        return;
    }

    newMark = g_new0 (gdc_mark_data, 1);
    newMark->name = NULL;
    if (name)
        newMark->name = g_strdup (name);
    newMark->info = NULL;
    if (info)
        newMark->info = g_strdup (info);
    newMark->tag = tag;
    newMark->ourMarks = NULL;
    DEBUG("saving mark with tag [%d]", newMark->tag);

    for (i = 0; i < size; i++)
    {
        d = dateArray[i];
        doc = gdc_get_doc_offset (dcal, d);
        if (doc < 0)
            continue;
        if (doc >= dcal->numMarks)
        {
            /* It's not going to get any better, so just
             * stop processing. */
            break;
        }
        dcal->marks[doc] = g_list_append (dcal->marks[doc], newMark);
        newMark->ourMarks = g_list_append (newMark->ourMarks,
                                           GINT_TO_POINTER(doc));
    }
    dcal->markData = g_list_append (dcal->markData, (gpointer)newMark);
    gnc_dense_cal_draw_to_buffer (dcal);
    gtk_widget_queue_draw (GTK_WIDGET(dcal->cal_drawing_area));
}

static void
gdc_mark_remove (GncDenseCal *dcal, guint mark_to_remove, gboolean redraw)
{
    GList *iter, *calendar_marks;
    gint day_of_cal;
    gdc_mark_data *mark_data;

    /* Ignore non-realistic marks */
    if ((gint)mark_to_remove == -1)
    {
        DEBUG("mark_to_remove = -1");
        return;
    }

    mark_data = NULL;
    for (iter = dcal->markData; iter != NULL; iter = iter->next)
    {
        mark_data = (gdc_mark_data*)iter->data;
        if (mark_data->tag == mark_to_remove)
            break;
    }
    if (iter == NULL)
    {
        PINFO("couldn't find tag [%d]", mark_to_remove);
        return;
    }
    if (mark_data == NULL)
    {
        DEBUG("mark_data == null");
        return;
    }

    for (calendar_marks = mark_data->ourMarks; calendar_marks != NULL; calendar_marks = calendar_marks->next)
    {
        day_of_cal = GPOINTER_TO_INT(calendar_marks->data);
        dcal->marks[day_of_cal] = g_list_remove (dcal->marks[day_of_cal], mark_data);
    }
    g_list_free (mark_data->ourMarks);
    dcal->markData = g_list_remove (dcal->markData, mark_data);
    g_free (mark_data->name);
    g_free (mark_data->info);
    g_free (mark_data);

    if (redraw)
    {
        gnc_dense_cal_draw_to_buffer (dcal);
        gtk_widget_queue_draw (GTK_WIDGET(dcal->cal_drawing_area));
    }
}
