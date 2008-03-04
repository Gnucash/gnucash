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

#include "config.h"

#include "glib-compat.h"
#include "gnc-dense-cal.h"
#include "gnc-dense-cal-model.h"
#include "gnc-engine.h"
#include <glib.h>
#include <glib/gi18n.h>
#include <gtk/gtk.h>
#include <math.h>
#include <stdlib.h>

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

static const gchar* MONTH_THIS_COLOR = "lavender";
static const gchar* MONTH_THAT_COLOR = "SlateGray1";

static const gchar* MARK_COLOR = "Yellow";

#undef G_LOG_DOMAIN
#define G_LOG_DOMAIN "gnc.gui.dense-cal"

static void gnc_dense_cal_class_init(GncDenseCalClass *class);
static void gnc_dense_cal_init(GncDenseCal *dcal);
static void gnc_dense_cal_finalize(GObject *object);
static void gnc_dense_cal_dispose(GObject *object);
static void gnc_dense_cal_realize(GtkWidget *widget, gpointer user_data);
static void gnc_dense_cal_configure(GtkWidget *widget, GdkEventConfigure *event, gpointer user_data);
static void gnc_dense_cal_draw_to_buffer(GncDenseCal *dcal);
static gboolean gnc_dense_cal_expose(GtkWidget *widget, GdkEventExpose *event, gpointer user_data);

static void gdc_reconfig(GncDenseCal *dcal);

static void gdc_free_all_mark_data(GncDenseCal *dcal);

static void _gdc_compute_min_size(GncDenseCal *dcal,
                                  guint *min_width, guint *min_height);
static void _gdc_set_cal_min_size_req(GncDenseCal *dcal);
static gint gnc_dense_cal_motion_notify(GtkWidget      *widget,
                                        GdkEventMotion *event);
static gint gnc_dense_cal_button_press(GtkWidget *widget,
                                       GdkEventButton *evt);

static void _gdc_view_option_changed(GtkComboBox *widget, gpointer user_data);

static inline int day_width_at(GncDenseCal *dcal, guint xScale);
static inline int day_width(GncDenseCal *dcal);
static inline int day_height_at(GncDenseCal *dcal, guint yScale);
static inline int day_height(GncDenseCal *dcal);
static inline int week_width_at(GncDenseCal *dcal, guint xScale);
static inline int week_width(GncDenseCal *dcal);
static inline int week_height_at(GncDenseCal *dcal, guint yScale);
static inline int week_height(GncDenseCal *dcal);
static inline int col_width_at(GncDenseCal *dcal, guint xScale);
static inline int col_width(GncDenseCal *dcal);

static inline int col_height(GncDenseCal *dcal);
static inline int num_cols(GncDenseCal *dcal);

static void _gnc_dense_cal_set_month(GncDenseCal *dcal, GDateMonth mon, gboolean redraw);
static void _gnc_dense_cal_set_year(GncDenseCal *dcal, guint year, gboolean redraw);


/**
 * Returns the total number of weeks to display in the calendar [irrespective
 * of columns/weeks-per-col].
 **/
static inline int num_weeks(GncDenseCal *dcal);
/**
 * Returns the number of weeks per column.  Note that this is the number of
 * weeks needed to display the longest column.
 **/
static int num_weeks_per_col(GncDenseCal *dcal);

/* hotspot calculation */
static gint wheres_this(GncDenseCal *dcal, int x, int y);

static void recompute_x_y_scales(GncDenseCal *dcal);
static void recompute_mark_storage(GncDenseCal *dcal);
static void recompute_extents(GncDenseCal *dcal);
static void populate_hover_window(GncDenseCal *dcal, gint doc);

static void month_coords(GncDenseCal *dcal, int monthOfCal, GList **outList);
static void doc_coords(GncDenseCal *dcal, int dayOfCal,
                       int *x1, int *y1, int *x2, int *y2);

static void gdc_mark_add(GncDenseCal *dcal, guint tag, gchar *name, gchar *info, guint size, GDate **dateArray);
static void gdc_mark_remove(GncDenseCal *dcal, guint mark_to_remove, gboolean redraw);

static void gdc_add_tag_markings(GncDenseCal *cal, guint tag);
static void gdc_add_markings(GncDenseCal *cal);
static void gdc_remove_markings(GncDenseCal *cal);

static GObject *parent_class = NULL;

#define MONTH_NAME_BUFSIZE 10

/* Takes the number of months since January, in the range 0 to
 * 11. Returns the abbreviated month name according to the current
 * locale.*/
static const gchar*
month_name(int mon)
{
    static gchar buf[MONTH_NAME_BUFSIZE];
    GDate date;
    gint arbitrary_year = 1977;

    memset(buf, 0, MONTH_NAME_BUFSIZE);
    g_date_clear(&date, 1);

    g_date_set_year(&date, arbitrary_year);
    g_date_set_day(&date, 1);
    // g_date API is 1..12 (not 0..11)
    g_date_set_month(&date, mon+1);
    g_date_strftime(buf, MONTH_NAME_BUFSIZE, "%b", &date);

    return buf;
}

/* Takes the number of days since Sunday, in the range 0 to 6. Returns
 * the abbreviated weekday name according to the current locale. */
static void
day_label(gchar *buf, int buf_len, int dow)
{
    gnc_dow_abbrev(buf, buf_len, dow);
    /* Use only the first two characters */
    if (g_utf8_strlen(buf, -1) > 2) {
        gchar *pointer = g_utf8_offset_to_pointer(buf, 2);
        *pointer = '\0';
    }
}

GType
gnc_dense_cal_get_type()
{
    static GType dense_cal_type = 0;

    if (dense_cal_type == 0) {
        static const GTypeInfo dense_cal_info = {
            sizeof (GncDenseCalClass),
            NULL,
            NULL,
            (GClassInitFunc) gnc_dense_cal_class_init,
            NULL,
            NULL,
            sizeof (GncDenseCal),
            0,
            (GInstanceInitFunc) gnc_dense_cal_init,
            NULL
        };

        dense_cal_type = g_type_register_static(GTK_TYPE_VBOX,
                                                "GncDenseCal",
                                                &dense_cal_info, 0);
    }

    return dense_cal_type;
}

static void
gnc_dense_cal_class_init(GncDenseCalClass *klass)
{
    GObjectClass *object_class;
    GtkWidgetClass *widget_class;

    object_class = G_OBJECT_CLASS (klass);
    widget_class = GTK_WIDGET_CLASS (klass);

    parent_class = g_type_class_peek_parent (klass);

    object_class->finalize = gnc_dense_cal_finalize;
    object_class->dispose = gnc_dense_cal_dispose;

    widget_class->motion_notify_event = gnc_dense_cal_motion_notify;
    widget_class->button_press_event = gnc_dense_cal_button_press;
}

enum _GdcViewOptsColumns
{
    VIEW_OPTS_COLUMN_LABEL = 0,
    VIEW_OPTS_COLUMN_NUM_MONTHS
};

static GtkListStore *_cal_view_options = NULL;
static GtkListStore*
_gdc_get_view_options(void)
{
    if (_cal_view_options == NULL)
    {
        _cal_view_options = gtk_list_store_new(2, G_TYPE_STRING, G_TYPE_INT);
        gtk_list_store_insert_with_values(_cal_view_options, NULL, G_MAXINT, 0, _("12 months"), 1, 12, -1);
        gtk_list_store_insert_with_values(_cal_view_options, NULL, G_MAXINT, 0, _("6 months"), 1, 6, -1);
        gtk_list_store_insert_with_values(_cal_view_options, NULL, G_MAXINT, 0, _("4 months"), 1, 4, -1);
        gtk_list_store_insert_with_values(_cal_view_options, NULL, G_MAXINT, 0, _("3 months"), 1, 3, -1);
        gtk_list_store_insert_with_values(_cal_view_options, NULL, G_MAXINT, 0, _("2 months"), 1, 2, -1);
        gtk_list_store_insert_with_values(_cal_view_options, NULL, G_MAXINT, 0, _("1 month"), 1, 1, -1);
    }

    return _cal_view_options;
}

static void
gnc_dense_cal_init(GncDenseCal *dcal)
{
    gboolean colorAllocSuccess;

    gtk_widget_push_composite_child();

    {
        GtkTreeModel *options;
        GtkCellRenderer *text_rend;
        
        options = GTK_TREE_MODEL(_gdc_get_view_options());
        dcal->view_options = GTK_COMBO_BOX(gtk_combo_box_new_with_model(options));
        gtk_combo_box_set_active(GTK_COMBO_BOX(dcal->view_options), 0);
        text_rend = GTK_CELL_RENDERER(gtk_cell_renderer_text_new());
        gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(dcal->view_options), text_rend, TRUE);
        gtk_cell_layout_add_attribute(GTK_CELL_LAYOUT(dcal->view_options),
                                      text_rend, "text", VIEW_OPTS_COLUMN_LABEL);
        g_signal_connect(G_OBJECT(dcal->view_options), "changed", G_CALLBACK(_gdc_view_option_changed), (gpointer)dcal);
    }

    {
        GtkHBox *hbox = GTK_HBOX(gtk_hbox_new(FALSE, 0));
        GtkAlignment *label_align;
        GtkLabel *label;
        float right_align = 1.0, mid_align = 0.5, fill_x = 0.0, fill_y = 1.0;

        label = GTK_LABEL(gtk_label_new(_("View:")));
        label_align = GTK_ALIGNMENT(gtk_alignment_new(right_align, mid_align, fill_x, fill_y));
        gtk_container_add(GTK_CONTAINER(label_align), GTK_WIDGET(label));
        gtk_box_pack_start(GTK_BOX(hbox), GTK_WIDGET(label_align), TRUE, TRUE, 0);
        gtk_box_pack_start(GTK_BOX(hbox), GTK_WIDGET(dcal->view_options), FALSE, FALSE, 0);
        
        gtk_box_pack_start(GTK_BOX(dcal), GTK_WIDGET(hbox), FALSE, FALSE, 0);
    }
    dcal->cal_drawing_area = GTK_DRAWING_AREA(gtk_drawing_area_new());
    gtk_widget_add_events(GTK_WIDGET(dcal->cal_drawing_area), (GDK_EXPOSURE_MASK
                                                               | GDK_BUTTON_PRESS_MASK
                                                               | GDK_BUTTON_RELEASE_MASK
                                                               | GDK_POINTER_MOTION_MASK
                                                               | GDK_POINTER_MOTION_HINT_MASK));
    gtk_box_pack_start(GTK_BOX(dcal), GTK_WIDGET(dcal->cal_drawing_area), TRUE, TRUE, 0);
    g_signal_connect(G_OBJECT(dcal->cal_drawing_area), "expose_event", G_CALLBACK(gnc_dense_cal_expose), (gpointer)dcal);
    g_signal_connect(G_OBJECT(dcal->cal_drawing_area), "realize", G_CALLBACK(gnc_dense_cal_realize), (gpointer)dcal);
    g_signal_connect(G_OBJECT(dcal->cal_drawing_area), "configure_event", G_CALLBACK(gnc_dense_cal_configure), (gpointer)dcal);

    dcal->disposed = FALSE;
    dcal->initialized = FALSE;
    dcal->markData = NULL;
    dcal->numMarks = 0;
    dcal->marks = NULL;
    dcal->lastMarkTag = 0;

    dcal->showPopup = FALSE;
  
    dcal->transPopup = GTK_WINDOW(gtk_window_new(GTK_WINDOW_POPUP));
    {
        GtkWidget *vbox, *hbox;
        GtkWidget *l;
        GtkListStore *tree_data;
        GtkTreeView *tree_view;

        vbox = gtk_vbox_new(FALSE, 5);
        hbox = gtk_hbox_new(FALSE, 5);

        l = gtk_label_new(_("Date: "));
        gtk_container_add(GTK_CONTAINER(hbox), l);
        l = gtk_label_new("YY/MM/DD");
        g_object_set_data(G_OBJECT(dcal->transPopup), "dateLabel", l);
        gtk_container_add(GTK_CONTAINER(hbox), l);
        gtk_container_add(GTK_CONTAINER(vbox), hbox);

        gtk_container_add(GTK_CONTAINER(vbox), gtk_hseparator_new());

        tree_data = gtk_list_store_new(2, G_TYPE_STRING, G_TYPE_STRING);
        tree_view = GTK_TREE_VIEW(gtk_tree_view_new_with_model(GTK_TREE_MODEL(tree_data)));
        gtk_tree_view_insert_column_with_attributes(tree_view, -1, _("Name"), gtk_cell_renderer_text_new(), "text", 0, NULL);
        gtk_tree_view_insert_column_with_attributes(tree_view, -1, _("Frequency"), gtk_cell_renderer_text_new(), "text", 1, NULL);
        g_object_set_data(G_OBJECT(dcal->transPopup), "model", tree_data);
        gtk_container_add(GTK_CONTAINER(vbox), GTK_WIDGET(tree_view));

        gtk_container_add(GTK_CONTAINER(dcal->transPopup), vbox);

        gtk_window_set_resizable(GTK_WINDOW(dcal->transPopup), FALSE);

        gtk_widget_realize(GTK_WIDGET(dcal->transPopup));
    }

    gdk_color_parse(MONTH_THIS_COLOR,  &dcal->weekColors[MONTH_THIS]);
    gdk_color_parse(MONTH_THAT_COLOR,  &dcal->weekColors[MONTH_THAT]);
    if (gdk_colormap_alloc_colors(gdk_colormap_get_system(),
                                  dcal->weekColors,
                                  MAX_COLORS, TRUE, TRUE,
                                  &colorAllocSuccess) > 0)
    {
        g_error("error allocating colors");
    }

    /* Deal with the various label sizes. */
    {
        gint i;
        gint maxWidth, maxHeight;
        GtkStyle *style;
        PangoLayout *layout;
        PangoFontDescription *font_desc;
        gint font_size;
        gint font_size_reduction_units = 1;

        layout = gtk_widget_create_pango_layout(GTK_WIDGET(dcal), NULL);

        style = gtk_widget_get_style(GTK_WIDGET(dcal));

        font_desc = pango_font_description_copy(style->font_desc);
        font_size = pango_font_description_get_size(font_desc);
        font_size -= font_size_reduction_units * PANGO_SCALE;
        pango_font_description_set_size(font_desc, font_size);
        gtk_widget_modify_font(GTK_WIDGET(dcal), font_desc);
        pango_font_description_free(font_desc);
          
        maxWidth = maxHeight = 0;
        for (i=0; i<12; i++)
        {
            gint w, h;
            pango_layout_set_text(layout, month_name(i), -1);
            pango_layout_get_pixel_size(layout, &w, &h);
            maxWidth = MAX(maxWidth, w);
            maxHeight = MAX(maxHeight, h);
        }

        // these two were reversed, before...
        dcal->label_width    = maxWidth;
        dcal->label_height   = maxHeight;
          
        g_object_unref(layout);
    }

    dcal->month = G_DATE_JANUARY;
    dcal->year  = 1970;

    dcal->numMonths = 12;
    dcal->monthsPerCol = 3;
    dcal->leftPadding = 2;
    dcal->topPadding = 2;

    {
        GDate *now = g_date_new();
        g_date_set_time_t(now, time(NULL));
        _gnc_dense_cal_set_month(dcal, g_date_get_month(now), FALSE);
        _gnc_dense_cal_set_year(dcal, g_date_get_year(now), FALSE);
        g_date_free(now);
    }

    recompute_extents(dcal);
    recompute_mark_storage(dcal);

    /* Compute initial scaling factors; will be increased when we're
     * allocated enough space to scale up. */
    {
        PangoLayout *layout;
        int width_88, height_88;
        int width_XXX, height_XXX;

        layout = gtk_widget_create_pango_layout(GTK_WIDGET(dcal), NULL);

        pango_layout_set_text(layout, "88", -1);
        pango_layout_get_pixel_size(layout, &width_88, &height_88);

        pango_layout_set_text(layout, "XXX", -1);
        pango_layout_get_pixel_size(layout, &width_XXX, &height_XXX);

        dcal->min_x_scale = dcal->x_scale = width_88 + 2;
        dcal->min_y_scale = dcal->y_scale = MAX(floor((float)width_XXX / 3.), height_88 + 2);
               
        dcal->dayLabelHeight = height_88;

        g_object_unref(layout);
    }

    dcal->initialized = TRUE;


    dcal->week_starts_monday = 0;
    {
        gchar **parts;
	const char *week_start_str;

	/* Use this renaming macro to avoid extraction of the message
	   string into the gnucash.pot file when calling xgettext. */
#define dgettext_noextract dgettext
	/* Translators: This string must not show up in gnucash.pot as
	   it is looked up in the "gtk20" translation domain
	   instead. */
	week_start_str = dgettext_noextract("gtk20", "calendar:week_start:0");
#undef dgettext_noextract

        parts = g_strsplit(week_start_str, ":", 3);
        if (parts[0] != NULL
            && parts[1] != NULL
            && parts[2] != NULL)
        {
            if (strcmp("1", parts[2]) == 0)
                dcal->week_starts_monday = 1;
        }
        g_strfreev(parts);
    }

    gtk_widget_pop_composite_child();

    gtk_widget_show_all(GTK_WIDGET(dcal));
}

static void
_gdc_set_cal_min_size_req(GncDenseCal *dcal)
{
    guint min_width, min_height;

    _gdc_compute_min_size(dcal, &min_width, &min_height);
    gtk_widget_set_size_request(GTK_WIDGET(dcal->cal_drawing_area), min_width, min_height);
}

GtkWidget*
gnc_dense_cal_new(void)
{
    GncDenseCal *dcal;
    dcal = g_object_new(GNC_TYPE_DENSE_CAL, NULL);
    return GTK_WIDGET(dcal);
}

GtkWidget*
gnc_dense_cal_new_with_model(GncDenseCalModel *model)
{
    GncDenseCal *cal = GNC_DENSE_CAL(gnc_dense_cal_new());
    gnc_dense_cal_set_model(cal, model);
    return GTK_WIDGET(cal);
}

static void
recompute_first_of_month_offset(GncDenseCal *dcal)
{
    GDate *tmpDate;

    tmpDate = g_date_new_dmy(1, dcal->month, dcal->year);
    dcal->firstOfMonthOffset = g_date_get_weekday(tmpDate) % 7;
    g_date_free(tmpDate);
}

void
gnc_dense_cal_set_month(GncDenseCal *dcal, GDateMonth mon)
{
    _gnc_dense_cal_set_month(dcal, mon, TRUE);
}

static void
_gnc_dense_cal_set_month(GncDenseCal *dcal, GDateMonth mon, gboolean redraw)
{
    GTimer *t = g_timer_new();
    if (dcal->month == mon)
        return;
    dcal->month = mon;
    g_timer_start(t);
    recompute_first_of_month_offset(dcal);
    g_debug("recompute_first_of_month_offset: %f", g_timer_elapsed(t, NULL) * 1000.);
    g_timer_start(t);
    recompute_extents(dcal);
    g_debug("recompute_extents: %f", g_timer_elapsed(t, NULL) * 1000.);
    if (redraw && GTK_WIDGET_REALIZED(dcal))
    {
        g_timer_start(t);
        recompute_x_y_scales(dcal);
        g_debug("recompute_x_y_scales: %f", g_timer_elapsed(t, NULL) * 1000.);
        g_timer_start(t);
        gnc_dense_cal_draw_to_buffer(dcal);
        g_debug("draw_to_buffer: %f", g_timer_elapsed(t, NULL) * 1000.);
        g_timer_start(t);
        gtk_widget_queue_draw(GTK_WIDGET(dcal->cal_drawing_area));
        g_debug("queue_draw: %f", g_timer_elapsed(t, NULL) * 1000.);
    }
    g_timer_stop(t);
    g_timer_destroy(t);
}

void
gnc_dense_cal_set_year(GncDenseCal *dcal, guint year)
{
    return _gnc_dense_cal_set_year(dcal, year, TRUE);
}

static void
_gnc_dense_cal_set_year(GncDenseCal *dcal, guint year, gboolean redraw)
{
    if (dcal->year == year)
        return;
    dcal->year = year;
    recompute_first_of_month_offset(dcal);
    recompute_extents(dcal);
    if (redraw && GTK_WIDGET_REALIZED(dcal))
    {
        recompute_x_y_scales(dcal);
        gnc_dense_cal_draw_to_buffer(dcal);
        gtk_widget_queue_draw(GTK_WIDGET(dcal->cal_drawing_area));
    }
}

void
gnc_dense_cal_set_num_months(GncDenseCal *dcal, guint num_months)
{
    {
        GtkListStore *options = _gdc_get_view_options();
        GtkTreeIter view_opts_iter, iter_closest_to_req;
        int closest_index_distance = G_MAXINT;

        // find closest list value to num_months
        if (!gtk_tree_model_get_iter_first(GTK_TREE_MODEL(options), &view_opts_iter))
        {
            g_critical("no view options?");
            return;
        }
        
        do
        {
            gint months_val, delta_months;

            gtk_tree_model_get(GTK_TREE_MODEL(options), &view_opts_iter, VIEW_OPTS_COLUMN_NUM_MONTHS, &months_val, -1);
            delta_months = abs(months_val - num_months);
            if (delta_months < closest_index_distance)
            {
                iter_closest_to_req = view_opts_iter;
                closest_index_distance = delta_months;
            }
        } while (closest_index_distance != 0
                 && (gtk_tree_model_iter_next(GTK_TREE_MODEL(options), &view_opts_iter)));

        // set iter on view
        g_signal_handlers_block_by_func(dcal->view_options, _gdc_view_option_changed, dcal);
        gtk_combo_box_set_active_iter(GTK_COMBO_BOX(dcal->view_options), &iter_closest_to_req);
        g_signal_handlers_unblock_by_func(dcal->view_options, _gdc_view_option_changed, dcal);
    }

    dcal->numMonths = num_months;
    recompute_extents(dcal);
    recompute_mark_storage(dcal);
    if (GTK_WIDGET_REALIZED(dcal))
    {
        recompute_x_y_scales(dcal);
        gnc_dense_cal_draw_to_buffer(dcal);
        gtk_widget_queue_draw(GTK_WIDGET(dcal->cal_drawing_area));
    }
}

guint
gnc_dense_cal_get_num_months(GncDenseCal *dcal)
{
    return dcal->numMonths;
}

void
gnc_dense_cal_set_months_per_col(GncDenseCal *dcal, guint monthsPerCol)
{
    dcal->monthsPerCol = monthsPerCol;
    recompute_x_y_scales(dcal);
}

GDateMonth
gnc_dense_cal_get_month(GncDenseCal *dcal)
{
    return dcal->month;
}

GDateYear
gnc_dense_cal_get_year(GncDenseCal *dcal)
{
    return dcal->year;
}

static void
gnc_dense_cal_dispose (GObject *object)
{
    GncDenseCal *dcal;
    g_return_if_fail(object != NULL);
    g_return_if_fail(GNC_IS_DENSE_CAL(object));

    dcal = GNC_DENSE_CAL(object);

    if (dcal->disposed)
        return;
    dcal->disposed = TRUE;

    if (GTK_WIDGET_REALIZED(dcal->transPopup))
    {
        gtk_widget_hide(GTK_WIDGET(dcal->transPopup));
        gtk_widget_destroy(GTK_WIDGET(dcal->transPopup));
        dcal->transPopup = NULL;
    }

    if (dcal->drawbuf)
    {
        g_object_unref(dcal->drawbuf);
        dcal->drawbuf = NULL;
    }

    /* FIXME: we have a bunch of cleanup to do, here. */

    gdc_free_all_mark_data(dcal);

    g_object_unref(G_OBJECT(dcal->model));

    if (G_OBJECT_CLASS (parent_class)->dispose)
        G_OBJECT_CLASS(parent_class)->dispose(object);
}

static void
gnc_dense_cal_finalize (GObject *object)
{
    GncDenseCal *dcal;
    g_return_if_fail (object != NULL);
    g_return_if_fail (GNC_IS_DENSE_CAL (object));

    dcal = GNC_DENSE_CAL(object);

    if (G_OBJECT_CLASS (parent_class)->finalize)
        G_OBJECT_CLASS(parent_class)->finalize(object);
}

static void
gnc_dense_cal_configure(GtkWidget *widget,
                        GdkEventConfigure *event,
                        gpointer user_data)
{
    GncDenseCal *dcal = GNC_DENSE_CAL(user_data);
    recompute_x_y_scales(dcal);
    gdc_reconfig(dcal);
    gtk_widget_queue_draw_area(widget,
                               event->x, event->y,
                               event->width, event->height);
}

static void
gnc_dense_cal_realize (GtkWidget *widget, gpointer user_data)
{
    GncDenseCal *dcal;

    g_return_if_fail(widget != NULL);
    g_return_if_fail(GNC_IS_DENSE_CAL (user_data));
    dcal = GNC_DENSE_CAL(user_data);

    recompute_x_y_scales(dcal);
    gdc_reconfig(dcal);

    gtk_style_set_background(widget->style, widget->window, GTK_STATE_ACTIVE);
}

static void
gdc_reconfig(GncDenseCal *dcal)
{
    if (dcal->drawbuf)
        g_object_unref(dcal->drawbuf);
    dcal->drawbuf = gdk_pixmap_new(GTK_WIDGET(dcal->cal_drawing_area)->window,
                                   GTK_WIDGET(dcal->cal_drawing_area)->allocation.width,
                                   GTK_WIDGET(dcal->cal_drawing_area)->allocation.height,
                                   -1);
    gnc_dense_cal_draw_to_buffer(dcal);
}

static void 
_gdc_compute_min_size(GncDenseCal *dcal, guint *min_width, guint *min_height)
{
    if (min_width != NULL)
    {
        *min_width =
            (dcal->leftPadding * 2)
            + (num_cols(dcal)* (col_width_at(dcal, dcal->min_x_scale)
                                + dcal->label_width))
            + ((num_cols(dcal)-1) * COL_BORDER_SIZE);
    }

    if (min_height != NULL)
    {
        *min_height=
            (dcal->topPadding * 2)
            + MINOR_BORDER_SIZE
            + dcal->dayLabelHeight
            + (num_weeks_per_col(dcal)
               * week_height_at(dcal, dcal->min_y_scale));
    }
}

static void
recompute_x_y_scales(GncDenseCal *dcal)
{
    GtkWidget *widget;
    int denom;
    int width, height;

    widget = GTK_WIDGET(dcal);

    width = DENSE_CAL_DEFAULT_WIDTH;
    height = DENSE_CAL_DEFAULT_HEIGHT;
    if (dcal->initialized)
    {
        width  = GTK_WIDGET(dcal->cal_drawing_area)->allocation.width;
        height = GTK_WIDGET(dcal->cal_drawing_area)->allocation.height;
    }

    /* FIXME: there's something slightly wrong in the x_scale computation that
     * lets us draw larger than our area. */
    denom = 7 * num_cols(dcal);
    g_assert(denom != 0);
    dcal->x_scale = ((gint)(width
                            - (dcal->leftPadding * 2)
                            - (num_cols(dcal) * ((8 * MINOR_BORDER_SIZE)
                                                 + dcal->label_width))
                            - ((num_cols(dcal)-1) * COL_BORDER_SIZE))
                     / denom);
    dcal->x_scale = MAX(dcal->x_scale, dcal->min_x_scale);

    denom = num_weeks_per_col(dcal);
    g_assert(denom != 0);
    dcal->y_scale = ((gint)(height
                            - (dcal->topPadding * 2)
                            - MINOR_BORDER_SIZE
                            - dcal->dayLabelHeight
                            - (num_weeks_per_col(dcal)-1
                               * MINOR_BORDER_SIZE))
                     / denom);
    dcal->y_scale = MAX(dcal->y_scale, dcal->min_y_scale);

    _gdc_set_cal_min_size_req(dcal);
}

static void
gdc_free_all_mark_data(GncDenseCal *dcal)
{
    int i;
    GList *l;
    for (i=0; i < dcal->numMarks; i++)
    {
        /* Each of these just contains an elt of dcal->markData,
         * which we're about to free, below... */
        g_list_free(dcal->marks[i]);
    }
    g_free(dcal->marks);
    dcal->marks = NULL;
    /* Remove the old mark data. */
    for (l = dcal->markData; l; l = l->next)
    {
        g_list_free(((gdc_mark_data*)l->data)->ourMarks);
        g_free((gdc_mark_data*)l->data);
    }
    g_list_free(dcal->markData);
    dcal->markData = NULL;
}

static void
recompute_mark_storage(GncDenseCal *dcal)
{
    if (dcal->marks == NULL)
        goto createNew;
    gdc_free_all_mark_data(dcal);

createNew:
    dcal->numMarks = num_weeks(dcal) * 7;
    dcal->marks = g_new0(GList*, dcal->numMarks);
    if (dcal->model)
        gdc_add_markings(dcal);
}

static void
recompute_extents(GncDenseCal *dcal)
{
    GDate date;
    gint start_week, end_week;

    g_date_clear(&date, 1);
    g_date_set_dmy(&date, 1, dcal->month, dcal->year);
    start_week = (dcal->week_starts_monday
                  ? g_date_get_monday_week_of_year(&date)
                  : g_date_get_sunday_week_of_year(&date));
    g_date_add_months(&date, dcal->numMonths);
    end_week = (dcal->week_starts_monday
                ? g_date_get_monday_week_of_year(&date)
                : g_date_get_sunday_week_of_year(&date));
    if (g_date_get_year(&date) != dcal->year)
    {
        end_week += (dcal->week_starts_monday
                     ? g_date_get_monday_weeks_in_year(dcal->year)
                     : g_date_get_sunday_weeks_in_year(dcal->year));
    }
    dcal->num_weeks = end_week - start_week + 1;
}

static void
free_rect(gpointer data, gpointer ud)
{
    g_free((GdkRectangle*)data);
}

static gboolean
gnc_dense_cal_expose(GtkWidget *widget,
                     GdkEventExpose *event,
                     gpointer user_data)
{
    GncDenseCal *dcal;
    GdkGC *gc;

    g_return_val_if_fail(widget != NULL, FALSE);
    g_return_val_if_fail(GNC_IS_DENSE_CAL(user_data), FALSE);
    g_return_val_if_fail(event != NULL, FALSE);

    if (event->count > 0)
        return FALSE;

    dcal = GNC_DENSE_CAL(user_data);
    gc = widget->style->fg_gc[GTK_WIDGET_STATE(widget)];
    gdk_draw_drawable(GDK_DRAWABLE(GTK_WIDGET(dcal->cal_drawing_area)->window),
                      gc, GDK_DRAWABLE(dcal->drawbuf),
                      0, 0, 0, 0, -1, -1);

    return TRUE;
}

#define LOG_AND_RESET(timer, msg) do { g_debug("%s: %f", msg, g_timer_elapsed(timer, NULL) * 1000.); g_timer_reset(timer); } while (0);

static void
gnc_dense_cal_draw_to_buffer(GncDenseCal *dcal)
{
    GtkWidget *widget;
    gint i;
    int maxWidth;
    PangoLayout *layout;
    GTimer *timer;

    timer = g_timer_new();
    g_debug("drawing");
    widget = GTK_WIDGET(dcal);

    if (!dcal->drawbuf)
        return;

    g_timer_start(timer);
    layout = gtk_widget_create_pango_layout(GTK_WIDGET(dcal), NULL);
    LOG_AND_RESET(timer, "create_pango_layout");

    gdk_draw_rectangle(dcal->drawbuf,
                       widget->style->white_gc,
                       TRUE,
                       0, 0,
                       GTK_WIDGET(dcal->cal_drawing_area)->allocation.width,
                       GTK_WIDGET(dcal->cal_drawing_area)->allocation.height);

    /* Fill in alternating month colors. */
    {
        gint i;
        GdkGC *gc;
        GdkRectangle *rect;
        GList *mcList, *mcListIter;

        gc = gdk_gc_new(GTK_WIDGET(dcal)->window);
        gdk_gc_copy(gc, widget->style->fg_gc[GTK_WIDGET_STATE(widget)]);

        /* reset all of the month position offsets. */
        for (i=0; i<12; i++)
        {
            dcal->monthPositions[i].x = dcal->monthPositions[i].y = -1;
        }

        /* Paint the weeks for the upcoming N months. */
        for (i=0; i < dcal->numMonths; i++)
        {
            gdk_gc_set_foreground(gc, &dcal->weekColors[ i % 2 ]);

            mcList = NULL;
            month_coords(dcal, i, &mcList);
            dcal->monthPositions[i].x
                = floor(i/dcal->monthsPerCol)
                * (col_width(dcal) + COL_BORDER_SIZE);
            dcal->monthPositions[i].y = ((GdkRectangle*)mcList->next->data)->y;
            for (mcListIter = mcList; mcListIter != NULL; mcListIter = mcListIter->next)
            {
                rect = (GdkRectangle*)mcListIter->data;
                gdk_draw_rectangle(dcal->drawbuf, gc,
                                   TRUE, rect->x, rect->y,
                                   rect->width, rect->height);
            }
            g_list_foreach(mcList, free_rect, NULL);
            g_list_free(mcList);
        }

        g_object_unref(gc);
    }
    LOG_AND_RESET(timer, "alternating month colors");

    /* Hilight the marked days. */
    {
        int i;
        int x1, x2, y1, y2;
        GdkColor markColor;
        GdkGCValues current_values;

        gdk_gc_get_values(widget->style->fg_gc[widget->state], &current_values);

        gdk_color_parse(MARK_COLOR, &markColor);
        gdk_colormap_alloc_color(gdk_colormap_get_system(), &markColor, TRUE, TRUE);

        gdk_gc_set_foreground(widget->style->fg_gc[widget->state], &markColor);
        for (i=0; i<dcal->numMarks; i++)
        {
            if (dcal->marks[i] != NULL)
            {
                int w,h, x_offset, y_offset, circle_delta;

                doc_coords(dcal, i, &x1, &y1, &x2, &y2);
                w = x2 - x1;
                h = y2 - y1;

                x_offset = x1;
                y_offset = y1;

                circle_delta = ABS(w-h) / 2;
                if (w < h)
                {
                    y_offset += circle_delta;
                }
                else
                {
                    x_offset += circle_delta;
                }

                gdk_draw_arc(dcal->drawbuf,
                             widget->style->fg_gc[widget->state],
                             TRUE,
                             x_offset, y_offset, MIN(w,h), MIN(w,h),
                             0 * 64,
                             360 * 64);
            }
        }

        // reset to the previous foreground color.
        gdk_gc_set_foreground(widget->style->fg_gc[widget->state], &current_values.foreground);
    }
    LOG_AND_RESET(timer, "marked days");

    for (i=0; i < num_cols(dcal); i++)
    {
        gint x, y, w, h;
        gint j;
          
        pango_layout_set_text(layout, "S", -1);
        pango_layout_get_pixel_size(layout, NULL, &dcal->dayLabelHeight);

        x = dcal->leftPadding
            + (i * (col_width(dcal)+COL_BORDER_SIZE))
            + dcal->label_width;
        y = dcal->topPadding + dcal->dayLabelHeight;
        w = col_width(dcal) - COL_BORDER_SIZE - dcal->label_width;
        h = col_height(dcal);

        /* draw the outside border [inside the month labels] */
        gdk_draw_rectangle(dcal->drawbuf,
                           widget->style->fg_gc[widget->state],
                           FALSE, x, y, w, h);
        /* draw the week seperations */
        for (j=0; j < num_weeks_per_col(dcal); j++)
        {
            gint wy = y + (j * week_height(dcal));
            gdk_draw_line(dcal->drawbuf,
                          widget->style->fg_gc[widget->state],
                          x,     wy,
                          x + w, wy);
        }

        /* draw the day seperations */
        for (j=1; j<7; j++)
        {
            gint dx = x + (j * day_width(dcal));
            gdk_draw_line(dcal->drawbuf,
                          widget->style->fg_gc[widget->state],
                          dx, y,
                          dx, y + col_height(dcal));
        }

        /* draw the day labels */
        pango_layout_set_text(layout, "88", -1);
        pango_layout_get_pixel_size(layout, &maxWidth, NULL);
          
        if (dcal->x_scale > maxWidth)
        {
            for (j=0; j<7; j++)
            {
                int day_label_width;
                gint label_x_offset, label_y_offset;
                gint day_label_str_len = 3;
                gchar day_label_str[day_label_str_len+1];

                day_label(day_label_str, day_label_str_len, (j + dcal->week_starts_monday) % 7);
                pango_layout_set_text(layout, day_label_str, -1);
                pango_layout_get_pixel_size(layout, &day_label_width, NULL);
                label_x_offset = x
                    + (j * day_width(dcal))
                    + (day_width(dcal)/2)
                    - (day_label_width / 2);
                label_y_offset = y - dcal->dayLabelHeight;
                pango_layout_set_text(layout, day_label_str, -1);
                gdk_draw_layout(GDK_DRAWABLE(dcal->drawbuf), widget->style->fg_gc[widget->state],
                                label_x_offset, label_y_offset,
                                layout);

            }
        }
    }
    LOG_AND_RESET(timer, "lines and labels");

    /* Month labels. */
    {
        gint i;
        PangoMatrix matrix = PANGO_MATRIX_INIT;

        pango_matrix_rotate(&matrix, 90.);
        pango_context_set_matrix(gtk_widget_get_pango_context(GTK_WIDGET(dcal)), &matrix);

        for (i=0; i<12; i++)
        {
            guint idx;

            if (dcal->monthPositions[i].x == -1)
                break;
            idx = (dcal->month - 1 + i) % 12;
            pango_layout_set_text(layout, month_name(idx), -1);
            gdk_draw_layout(GDK_DRAWABLE(dcal->drawbuf),
                            widget->style->fg_gc[widget->state],
                            dcal->leftPadding + dcal->monthPositions[i].x,
                            dcal->monthPositions[i].y,
                            layout);
        }

        // reset rotation
        pango_matrix_rotate(&matrix, -90.);
        pango_context_set_matrix(gtk_widget_get_pango_context(GTK_WIDGET(dcal)), &matrix);
    }
    LOG_AND_RESET(timer, "month labels");

    /* Try the per-day strings [dates] */
    {
        GDate d, eoc;
        gint doc;
        gchar dayNumBuf[3];
        gint numW, numH;
        gint x1, y1, x2, y2, w, h;

        g_date_set_dmy(&d, 1, dcal->month, dcal->year);
        eoc = d;
        g_date_add_months(&eoc, dcal->numMonths);
        for (doc = 0; g_date_get_julian(&d) < g_date_get_julian(&eoc); g_date_add_days(&d, 1), doc++)
        {
            doc_coords(dcal, doc, &x1, &y1, &x2, &y2);
            memset(dayNumBuf, 0, 3);
            snprintf(dayNumBuf, 3, "%d", g_date_get_day(&d));
            pango_layout_set_text(layout, dayNumBuf, -1);
            pango_layout_get_pixel_size(layout, &numW, &numH);
            w = (x2 - x1)+1;
            h = (y2 - y1)+1;
            gdk_draw_layout(GDK_DRAWABLE(dcal->drawbuf),
                            widget->style->fg_gc[widget->state],
                            x1 + (w/2) - (numW/2),
                            y1 + (h/2) - (numH/2),
                            layout);
        }
    }
    LOG_AND_RESET(timer, "dates");

    gtk_widget_queue_draw_area(GTK_WIDGET(dcal), 
                               widget->allocation.x,
                               widget->allocation.y,
                               widget->allocation.width,
                               widget->allocation.height);

    LOG_AND_RESET(timer, "queue draw");

    g_object_unref(layout);

    g_timer_destroy(timer);
}

static void
populate_hover_window(GncDenseCal *dcal, gint doc)
{
    GtkWidget *w;
    GDate *date;
    static const int MAX_STRFTIME_BUF_LEN = 64;
    gchar strftimeBuf[MAX_STRFTIME_BUF_LEN];

    if (doc >= 0)
    {
        GObject *o;
        GtkListStore *model;
        GList *l;

        w = GTK_WIDGET(g_object_get_data(G_OBJECT(dcal->transPopup), "dateLabel"));
        date = g_date_new_dmy(1, dcal->month, dcal->year);
        g_date_add_days(date, doc);
        /* Note: the ISO date format (%F or equivalently
         * %Y-%m-%d) is not a good idea here since many
         * locales will want to use a very different date
         * format. Please leave the specification of the date
         * format up to the locale and use %x here.  */
        g_date_strftime(strftimeBuf, MAX_STRFTIME_BUF_LEN-1, "%x", date);
        gtk_label_set_text(GTK_LABEL(w), strftimeBuf);

        o = G_OBJECT(dcal->transPopup);
        model = GTK_LIST_STORE(g_object_get_data(o, "model"));
        gtk_list_store_clear(model);
        for (l = dcal->marks[doc]; l; l = l->next)
        {
            GtkTreeIter iter;
            gdc_mark_data *gdcmd;

            gdcmd = (gdc_mark_data*)l->data;
            gtk_list_store_insert(model, &iter, INT_MAX);
            gtk_list_store_set(model, &iter, 0, (gdcmd->name ? gdcmd->name : _("(unnamed)")), 1, gdcmd->info, -1);
        }

        g_date_free(date);
    }
}

static gint
gnc_dense_cal_button_press(GtkWidget *widget,
                           GdkEventButton *evt)
{
    gint doc;
    GncDenseCal *dcal = GNC_DENSE_CAL(widget);

    doc = wheres_this(dcal, evt->x, evt->y);
    dcal->showPopup = ~(dcal->showPopup);
    if (dcal->showPopup && doc >= 0)
    {
        // Do the move twice in case the WM is ignoring the first one
        // because the window hasn't been shown, yet.  The WM is free
        // to ignore our move and place windows according to it's own
        // strategy, but hopefully it'll listen to us.  Certainly the
        // second move after show_all'ing the window should do the
        // trick with a bit of flicker.
        gtk_window_move(GTK_WINDOW(dcal->transPopup), evt->x_root+5, evt->y_root+5);
        populate_hover_window(dcal, doc);
        gtk_widget_queue_resize(GTK_WIDGET(dcal->transPopup));
        gtk_widget_show_all(GTK_WIDGET(dcal->transPopup));
        gtk_window_move(GTK_WINDOW(dcal->transPopup), evt->x_root+5, evt->y_root+5);
    }
    else
    {
        gtk_widget_hide(GTK_WIDGET(dcal->transPopup));
    }
    return FALSE;
}

static gint
gnc_dense_cal_motion_notify(GtkWidget *widget,
                            GdkEventMotion *event)
{
    GncDenseCal *dcal;
    gint doc;
    int unused;
    int x_root_offset, y_root_offset;
    GdkModifierType unused2;

    dcal = GNC_DENSE_CAL(widget);
    if (!dcal->showPopup)
        return FALSE;

    x_root_offset = event->x_root;
    y_root_offset = event->y_root;

    /* As per http://www.gtk.org/tutorial/sec-eventhandling.html */
    if (event->is_hint)
        gdk_window_get_pointer(event->window, &unused, &unused, &unused2);
    gdk_window_move(GTK_WIDGET(dcal->transPopup)->window,
                    x_root_offset+5, y_root_offset+5);
    doc = wheres_this(dcal, event->x, event->y);
    if (doc >= 0)
    {
        populate_hover_window(dcal, doc);
        gtk_widget_queue_resize(GTK_WIDGET(dcal->transPopup));
        gtk_widget_show_all(GTK_WIDGET(dcal->transPopup));
    }
    else
    {
        gtk_widget_hide(GTK_WIDGET(dcal->transPopup));
    }
    return TRUE;
}


static void
_gdc_view_option_changed(GtkComboBox *widget, gpointer user_data)
{
    GtkTreeIter iter;
    GtkTreeModel *model;
    gint months_val;
    
    model = GTK_TREE_MODEL(gtk_combo_box_get_model(widget));
    if (!gtk_combo_box_get_active_iter(widget, &iter))
        return;
    gtk_tree_model_get(model, &iter, VIEW_OPTS_COLUMN_NUM_MONTHS, &months_val, -1);
    g_debug("changing to %d months", months_val);
    gnc_dense_cal_set_num_months(GNC_DENSE_CAL(user_data), months_val);
}

static inline int
day_width_at(GncDenseCal *dcal, guint xScale)
{
    return xScale + MINOR_BORDER_SIZE;
}

static inline int
day_width(GncDenseCal *dcal)
{
    return day_width_at(dcal, dcal->x_scale);
}

static inline int
day_height_at(GncDenseCal *dcal, guint yScale)
{
    return yScale + MINOR_BORDER_SIZE;
}

static inline int
day_height(GncDenseCal *dcal)
{
    return day_height_at(dcal, dcal->y_scale);
}

static inline int
week_width_at(GncDenseCal *dcal, guint xScale)
{
    return day_width_at(dcal, xScale) * 7;
}

static inline int
week_width(GncDenseCal *dcal)
{
    return week_width_at(dcal, dcal->x_scale);
}

static inline int
week_height_at(GncDenseCal *dcal, guint yScale)
{
    return day_height_at(dcal, yScale);
}

static inline int
week_height(GncDenseCal *dcal)
{
    return week_height_at(dcal, dcal->y_scale);
}

static inline int
col_width_at(GncDenseCal *dcal, guint xScale)
{
    return (week_width_at(dcal, xScale)
            + dcal->label_width
            + COL_BORDER_SIZE);
}

static inline int
col_width(GncDenseCal *dcal)
{
    return col_width_at(dcal, dcal->x_scale);
}

static inline int
col_height(GncDenseCal *dcal)
{
    return week_height(dcal) * num_weeks_per_col(dcal);
}

static inline int
num_cols(GncDenseCal *dcal)
{
    return ceil((float)dcal->numMonths / (float)dcal->monthsPerCol);
}

static inline int
num_weeks(GncDenseCal *dcal)
{
    return dcal->num_weeks;
}

static
int num_weeks_per_col(GncDenseCal *dcal)
{
    int num_weeks_toRet, numCols, i;
    GDate *start,*end;
    int startWeek, endWeek;

    start = g_date_new();
    end = g_date_new();

    num_weeks_toRet = 0;
    numCols = num_cols(dcal);

    for (i=0; i<numCols; i++)
    {
        g_date_set_dmy(start, 1,
                       ((dcal->month - 1
                         + (i * dcal->monthsPerCol)) % 12)
                       + 1,
                       dcal->year + floor((dcal->month - 1
                                           + (i*dcal->monthsPerCol))
                                          / 12));
        *end = *start;
        /* Add the smaller of (the number of months in the
         * calendar-display, minus the number of months shown in the
         * previous columns) or (the number of months in a column) */
        g_date_add_months(end, MIN(dcal->numMonths,
                                   MIN(dcal->monthsPerCol,
                                       dcal->numMonths
                                       - ((i-1)
                                          * dcal->monthsPerCol))));
        g_date_subtract_days(end, 1);
        startWeek = (dcal->week_starts_monday
                     ? g_date_get_monday_week_of_year(start)
                     : g_date_get_sunday_week_of_year(start));
        endWeek = (dcal->week_starts_monday
                   ? g_date_get_monday_week_of_year(end)
                   : g_date_get_sunday_week_of_year(end));
        if (endWeek < startWeek)
        {
            endWeek += (dcal->week_starts_monday
                        ? g_date_get_monday_weeks_in_year(g_date_get_year(start))
                        : g_date_get_sunday_weeks_in_year(g_date_get_year(start)));
        }
        num_weeks_toRet = MAX(num_weeks_toRet, (endWeek - startWeek)+1);
    }
    return num_weeks_toRet;
}

/**
 * @param monthOfCal 0-based; offset of calendar's first month.
 * @param outList A GList in which to place GdkRectangle's of the extents of
 * each week.  4 or 5 GdkRectangle*s will be added to the list, as per the
 * size of the month.
 **/
static void
month_coords(GncDenseCal *dcal, int monthOfCal, GList **outList)
{
    gint weekRow, colNum, previousMonthsInCol, monthOffset;
    gint start;
    GDate *startD, *endD;
    GdkRectangle *rect;
    gint startWk, endWk;

    if (monthOfCal > dcal->numMonths)
        return;

    colNum = floor(monthOfCal / dcal->monthsPerCol);
    monthOffset = colNum * dcal->monthsPerCol;
    previousMonthsInCol = MAX(0, (monthOfCal % dcal->monthsPerCol));

    startD = g_date_new();
    endD = g_date_new();

    /* Calculate the number of weeks in the column before the month we're
     * interested in. */
    weekRow = 0;
    if (previousMonthsInCol > 0)
    {
        g_date_set_dmy(startD, 1,
                       ((dcal->month - 1 + monthOffset) % 12) + 1,
                       dcal->year + floor((dcal->month-1+monthOffset)/12));
        /* get the week of the top of the column */
        startWk = (dcal->week_starts_monday
                   ? g_date_get_monday_week_of_year(startD)
                   : g_date_get_sunday_week_of_year(startD));
        /* get the week of the end of the previous months */
        *endD = *startD;
        g_date_add_months(endD, previousMonthsInCol);
        g_date_subtract_days(endD, 1);
        endWk = (dcal->week_starts_monday
                 ? g_date_get_monday_week_of_year(endD)
                 : g_date_get_sunday_week_of_year(endD));
        if (endWk < startWk)
        {
            endWk += (dcal->week_starts_monday
                      ? g_date_get_monday_weeks_in_year(g_date_get_year(startD))
                      : g_date_get_sunday_weeks_in_year(g_date_get_year(startD)));
        }
        /* determine how many weeks are before the month we're
         * interested in. */
        weekRow = endWk - startWk;
        if (g_date_get_weekday(endD) == (dcal->week_starts_monday ? G_DATE_SUNDAY : G_DATE_SATURDAY))
        {
            weekRow++;
        }
    }

    g_date_set_dmy(startD, 1,
                   ((dcal->month - 1 + monthOfCal) % 12) + 1,
                   dcal->year + floor((dcal->month-1+monthOfCal)/12));
    *endD = *startD;
    g_date_add_months(endD, 1);
    g_date_subtract_days(endD, 1);
    /* Get the first week. */
    {
        start = (g_date_get_weekday(startD) - dcal->week_starts_monday) % 7;
        rect = g_new0(GdkRectangle, 1);
        rect->x = dcal->leftPadding
            + MINOR_BORDER_SIZE
            + (colNum * (col_width(dcal) + COL_BORDER_SIZE))
            + dcal->label_width
            + (start * day_width(dcal));
        rect->y = dcal->topPadding
            + dcal->dayLabelHeight
            + MINOR_BORDER_SIZE
            + (weekRow * week_height(dcal));
        rect->width = (7 - start) * day_width(dcal);
        rect->height = week_height(dcal);
        *outList = g_list_append(*outList, (gpointer)rect);
        rect = NULL;
    }

    /* Get the middle weeks. */
    {
        gint i, weekStart, weekEnd;

        weekStart = (dcal->week_starts_monday
                     ? g_date_get_monday_week_of_year(startD)
                     : g_date_get_sunday_week_of_year(startD))+1;
        weekEnd = (dcal->week_starts_monday
                   ? g_date_get_monday_week_of_year(endD)
                   : g_date_get_sunday_week_of_year(endD));
        for (i=weekStart; i<weekEnd; i++) {
            rect = g_new0(GdkRectangle, 1);
            rect->x = dcal->leftPadding
                + MINOR_BORDER_SIZE
                + dcal->label_width
                + (colNum * (col_width(dcal) + COL_BORDER_SIZE));
            rect->y = dcal->topPadding
                + dcal->dayLabelHeight
                + MINOR_BORDER_SIZE
                + ((weekRow + (i-weekStart) + 1) * week_height(dcal));
            rect->width  = week_width(dcal);
            rect->height = week_height(dcal);

            *outList = g_list_append(*outList, (gpointer)rect);
            rect = NULL;
        }
    }
        
    /* Get the last week. */
    {
        gint end_week_of_year = g_date_get_sunday_week_of_year(endD);
        gint start_week_of_year = g_date_get_sunday_week_of_year(startD);
        if (dcal->week_starts_monday == 1)
        {
            end_week_of_year = g_date_get_monday_week_of_year(endD);
            start_week_of_year = g_date_get_monday_week_of_year(startD);
        }

        rect = g_new0(GdkRectangle, 1);
        rect->x = dcal->leftPadding
            + MINOR_BORDER_SIZE
            + dcal->label_width
            + (colNum * (col_width(dcal) + COL_BORDER_SIZE));
        rect->y = dcal->topPadding
            + MINOR_BORDER_SIZE
            + dcal->dayLabelHeight
            + ((weekRow
                + (end_week_of_year - start_week_of_year))
               * week_height(dcal));
        rect->width = (((g_date_get_weekday(endD) - dcal->week_starts_monday) % 7)+1) * day_width(dcal);
        rect->height = week_height(dcal);

        *outList = g_list_append(*outList, (gpointer)rect);
        rect = NULL;
    }

    g_date_free(startD);
    g_date_free(endD);
}

/* FIXME: make this more like month_coords */
static void
doc_coords(GncDenseCal *dcal, int dayOfCal,
           int *x1, int *y1, int *x2, int *y2)
{
    GDate d;
    gint docMonth;
    gint d_week_of_cal, top_of_col_week_of_cal;
    gint colNum, dayCol, weekRow;

    /* FIXME: add range checks */
    g_date_set_dmy(&d, 1, dcal->month, dcal->year);
    g_date_add_days(&d, dayOfCal);
    docMonth = g_date_get_month(&d);
    if (g_date_get_year(&d) != dcal->year)
    {
        docMonth += 12;
    }
    colNum  = floor((float)(docMonth - dcal->month) / (float)dcal->monthsPerCol);
    dayCol  = (g_date_get_weekday(&d) - dcal->week_starts_monday) % 7;
    d_week_of_cal = g_date_get_sunday_week_of_year(&d);
    if (dcal->week_starts_monday == 1)
    {
        d_week_of_cal = g_date_get_monday_week_of_year(&d);
    }
    g_date_set_dmy(&d, 1, dcal->month, dcal->year);
    g_date_add_months(&d, (colNum * dcal->monthsPerCol));
    top_of_col_week_of_cal = (dcal->week_starts_monday
                              ? g_date_get_monday_week_of_year(&d)
                              : g_date_get_sunday_week_of_year(&d));
    if (d_week_of_cal < top_of_col_week_of_cal)
    {
        gint week_offset;
        week_offset = g_date_get_sunday_weeks_in_year(dcal->year);
        if (dcal->week_starts_monday == 1)
        {
            week_offset = g_date_get_monday_weeks_in_year(dcal->year);
        }
        d_week_of_cal += week_offset;
    }
    weekRow = d_week_of_cal - top_of_col_week_of_cal;

    /* top-left corner */
    /* FIXME: this has the math to make the mark-cells come out right,
     * which it shouldn't. */
    *x1 = dcal->leftPadding
        + MINOR_BORDER_SIZE
        + dcal->label_width
        + (colNum * (col_width(dcal) + COL_BORDER_SIZE))
        + (dayCol * day_width(dcal))
        + (day_width(dcal)/4);
    *y1 = dcal->topPadding
        + MINOR_BORDER_SIZE
        + dcal->dayLabelHeight
        + (weekRow * week_height(dcal))
        + (day_height(dcal)/4);

    *x2 = *x1 + (day_width(dcal)/2);
    *y2 = *y1 + (day_height(dcal)/2);
}

/**
 * Given x,y coordinates, returns the day-of-cal under the mouse; will return
 * '-1' if invalid.
 **/
static gint
wheres_this(GncDenseCal *dcal, int x, int y)
{
    gint colNum, weekRow, dayCol, dayOfCal;
    GDate d, startD;

    x -= dcal->leftPadding;
    y -= dcal->topPadding;

    if ((x < 0) || (y < 0))
    {
        return -1;
    }
    if ((x >= GTK_WIDGET(dcal)->allocation.width)
        || (y >= GTK_WIDGET(dcal)->allocation.height))
    {
        return -1;
    }

    /* "outside of displayed table" check */
    if (x >= (num_cols(dcal) * (col_width(dcal) + COL_BORDER_SIZE)))
    {
        return -1;
    }
    if (y >= dcal->dayLabelHeight + col_height(dcal))
    {
        return -1;
    }
        
    /* coords -> year-relative-values */
    colNum = floor(x / (col_width(dcal)+COL_BORDER_SIZE));
     
    x %= (col_width(dcal)+COL_BORDER_SIZE);
    x -= dcal->label_width;
    if (x < 0)
    {
        return -1;
    }
    if (x >= day_width(dcal) * 7)
    {
        return -1;
    }

    y -= dcal->dayLabelHeight;
    if (y < 0)
    {
        return -1;
    }

    dayCol = floor((float)x / (float)day_width(dcal));
    weekRow = floor((float)y / (float)week_height(dcal));

    g_date_set_dmy(&startD, 1, dcal->month, dcal->year);
    d = startD;
    g_date_add_months(&d, (colNum * dcal->monthsPerCol));
    dayCol -= ((g_date_get_weekday(&d) - dcal->week_starts_monday) % 7);
    if (weekRow == 0)
    {
        if (dayCol < 0)
        {
            return -1;
        }
    }
    g_date_add_days(&d, dayCol + (weekRow * 7));

    /* Check to make sure we're within the column's displayed range. */
    {
        GDate ccd;
        g_date_set_dmy(&ccd, 1, dcal->month, dcal->year);
        g_date_add_months(&ccd, (colNum+1) * dcal->monthsPerCol);
        if (g_date_get_julian(&d) >= g_date_get_julian(&ccd))
        {
            return -1;
        }
    }

    dayOfCal = g_date_get_julian(&d) - g_date_get_julian(&startD);

    /* one more check before returning... */
    g_date_subtract_months(&d, dcal->numMonths);
    if (g_date_get_julian(&d) >= g_date_get_julian(&startD))
    {
        /* we're past the end of the displayed calendar, thus -1 */
        g_debug("%d >= %d", g_date_get_julian(&d), g_date_get_julian(&startD));
        return -1;
    }

    return dayOfCal;
}

static gint
gdc_get_doc_offset(GncDenseCal *dcal, GDate *d)
{
    gint toRet;
    /* soc == start-of-calendar */
    GDate soc;

    g_date_clear(&soc, 1);
    g_date_set_dmy(&soc, 1, dcal->month, dcal->year);
    /* ensure not before calendar start. */
    if (g_date_get_julian(d) < g_date_get_julian(&soc))
        return -1;
    /* do computation here, since we're going to change the
     * start-of-calendar date. */
    toRet = g_date_get_julian(d) - g_date_get_julian(&soc);
    /* ensure not after end of visible calendar. */
    g_date_add_months(&soc, dcal->numMonths);
    if (g_date_get_julian(d) >= g_date_get_julian(&soc))
        return -1;
    /* return pre-computed value. */
    return toRet;
}

static void
gdc_add_tag_markings(GncDenseCal *cal, guint tag)
{
    gchar *name, *info;
    gint num_marks, idx;
    GDate **dates;

    // copy the values into the old marking function.
    name = gnc_dense_cal_model_get_name(cal->model, tag);
    info = gnc_dense_cal_model_get_info(cal->model, tag);
    num_marks = gnc_dense_cal_model_get_instance_count(cal->model, tag);

    if (num_marks == 0)
        goto cleanup;

    dates = g_new0(GDate*, num_marks);
    for (idx = 0; idx < num_marks; idx++)
    {
        dates[idx] = g_date_new();
        gnc_dense_cal_model_get_instance(cal->model, tag, idx, dates[idx]);
    }

    _gnc_dense_cal_set_month(cal, g_date_get_month(dates[0]), FALSE);
    _gnc_dense_cal_set_year(cal, g_date_get_year(dates[0]), FALSE);

    gdc_mark_add(cal, tag, name, info, num_marks, dates);

    for (idx = 0; idx < num_marks; idx++)
    {
        g_date_free(dates[idx]);
    }
    g_free(dates);

cleanup:
    g_free(info);
}

static void
gdc_add_markings(GncDenseCal *cal)
{
    GList *tags;
    tags = gnc_dense_cal_model_get_contained(cal->model);
    for (; tags != NULL; tags = tags->next)
    {
        guint tag = GPOINTER_TO_UINT(tags->data);
        gdc_add_tag_markings(cal, tag);
    }
    g_list_free(tags);
}

static void
gdc_remove_markings(GncDenseCal *cal)
{
    GList *tags;
    tags = gnc_dense_cal_model_get_contained(cal->model);
    for (; tags != NULL; tags = tags->next)
    {
        guint tag = GPOINTER_TO_UINT(tags->data);
        gdc_mark_remove(cal, tag, FALSE);
    }
    g_list_free(tags);
}

static void
gdc_model_added_cb(GncDenseCalModel *model, guint added_tag, gpointer user_data)
{
    GncDenseCal *cal = GNC_DENSE_CAL(user_data);
    g_debug("gdc_model_added_cb update\n");
    gdc_add_tag_markings(cal, added_tag);
} 

static void
gdc_model_update_cb(GncDenseCalModel *model, guint update_tag, gpointer user_data)
{
    GncDenseCal *cal = GNC_DENSE_CAL(user_data);
    g_debug("gdc_model_update_cb update for tag [%d]\n", update_tag);
    gdc_mark_remove(cal, update_tag, FALSE);
    gdc_add_tag_markings(cal, update_tag);
    
}

static void
gdc_model_removing_cb(GncDenseCalModel *model, guint remove_tag, gpointer user_data)
{
    GncDenseCal *cal = GNC_DENSE_CAL(user_data);
    g_debug("gdc_model_removing_cb update [%d]\n", remove_tag);
    gdc_mark_remove(cal, remove_tag, TRUE);
}

void
gnc_dense_cal_set_model(GncDenseCal *cal, GncDenseCalModel *model)
{
    if (cal->model != NULL)
    {
        gdc_remove_markings(cal);
        g_object_unref(G_OBJECT(cal->model));
        cal->model = NULL;
    }
    cal->model = model;
    g_object_ref(G_OBJECT(model));
    g_signal_connect(G_OBJECT(cal->model), "added", (GCallback)gdc_model_added_cb, cal);
    g_signal_connect(G_OBJECT(cal->model), "update", (GCallback)gdc_model_update_cb, cal);
    g_signal_connect(G_OBJECT(cal->model), "removing", (GCallback)gdc_model_removing_cb, cal);

    gdc_add_markings(cal);
}

/**
 * Marks the given array of GDate*s on the calendar with the given name.
 **/
static void
gdc_mark_add(GncDenseCal *dcal,
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
        g_error("0 size not allowed\n");
        return;
    }

    newMark = g_new0(gdc_mark_data, 1);
    newMark->name = NULL;
    if (name)
        newMark->name = g_strdup(name);
    newMark->info = NULL;
    if (info)
        newMark->info = g_strdup(info);
    newMark->tag = tag;
    newMark->ourMarks = NULL;
    g_debug("saving mark with tag [%d]\n", newMark->tag);

    for (i=0; i<size; i++)
    {
        d = dateArray[i];
        doc = gdc_get_doc_offset(dcal, d);
        if (doc < 0)
            continue;
        if (doc >= dcal->numMarks)
        {
            /* It's not going to get any better, so just
             * stop processing. */
            break;
        }
        dcal->marks[doc] = g_list_append(dcal->marks[doc], newMark);
        newMark->ourMarks = g_list_append(newMark->ourMarks,
                                          GINT_TO_POINTER(doc));
    }
    dcal->markData = g_list_append(dcal->markData, (gpointer)newMark);
    gnc_dense_cal_draw_to_buffer(dcal);
    gtk_widget_queue_draw(GTK_WIDGET(dcal->cal_drawing_area));
}

static void
gdc_mark_remove(GncDenseCal *dcal, guint mark_to_remove, gboolean redraw)
{
    GList *iter, *calendar_marks;
    gint day_of_cal;
    gdc_mark_data *mark_data;

    /* Ignore non-realistic marks */
    if ((gint)mark_to_remove == -1)
    {
        g_debug("mark_to_remove = -1");
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
        g_message("couldn't find tag [%d]", mark_to_remove);
        return;
    }
    if (mark_data == NULL)
    {
        g_debug("mark_data == null");
        return;
    }

    for (calendar_marks = mark_data->ourMarks; calendar_marks != NULL; calendar_marks = calendar_marks->next)
    {
        day_of_cal = GPOINTER_TO_INT(calendar_marks->data);
        dcal->marks[day_of_cal] = g_list_remove(dcal->marks[day_of_cal], mark_data);
    }
    g_list_free(mark_data->ourMarks);
    dcal->markData = g_list_remove(dcal->markData, mark_data);
    g_free(mark_data);

    if (redraw)
    {
        gnc_dense_cal_draw_to_buffer(dcal);
        gtk_widget_queue_draw(GTK_WIDGET(dcal->cal_drawing_area));
    }
}
