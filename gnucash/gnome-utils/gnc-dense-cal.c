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
static void gnc_dense_cal_resize (GtkDrawingArea *widget,
                                  gint width, gint height,
                                  gpointer user_data);

static void gnc_dense_cal_draw_to_buffer (GncDenseCal *dcal);
static void gnc_dense_cal_draw (GtkDrawingArea *area, cairo_t *cr,
                                int width, int height, gpointer user_data);

static void gdc_reconfig (GncDenseCal *dcal);

static void gdc_free_all_mark_data (GncDenseCal *dcal);

static void _gdc_compute_min_size (GncDenseCal *dcal,
                                   guint *min_width, guint *min_height);
static void _gdc_set_cal_min_size_req(GncDenseCal *dcal);
static void gnc_dense_cal_button_motion (GtkEventControllerMotion *controller,
                                         double x, double y, gpointer user_data);
static void gnc_dense_cal_button_press (GtkGestureClick *gesture,
                                        int n_press,
                                        double x, double y, gpointer user_data);
static void popup_closed_cb (GtkPopover *widget, gpointer user_data);
static void popup_focus_leave (GtkWidget *widget, gpointer user_data);
static gboolean popover_escape_cb (GtkEventControllerKey *key, guint keyval,
                                   guint keycode, GdkModifierType state,
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
    GtkBox           widget;
    GtkWidget       *months_dropdown;

    GtkDrawingArea  *cal_drawing_area;
    gint             cal_drawing_area_width;
    gint             cal_drawing_area_height;

    cairo_surface_t *surface;

    gboolean initialized;

    gboolean showPopup;
    gboolean is_popped;
    GtkWidget *transPopup;
    gint screen_width;
    gint screen_height;
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
    GtkWidgetClass *widget_class = GTK_WIDGET_CLASS(klass);

    gtk_widget_class_set_css_name (GTK_WIDGET_CLASS(klass), "calendar");

    object_class->finalize = gnc_dense_cal_finalize;
    object_class->dispose = gnc_dense_cal_dispose;
}

/***********************************************************************/

#define MONTHS_TYPE_ITEM (months_item_get_type ())
G_DECLARE_FINAL_TYPE (MonthsItem, months_item, MONTHS, ITEM, GObject)

struct _MonthsItem {
    GObject parent_instance;
    gchar *number_of_months_label;
    gint   number_of_months;
    gint   number_of_columns;
};

G_DEFINE_TYPE (MonthsItem, months_item, G_TYPE_OBJECT);

static void
months_item_init (MonthsItem *item)
{
}

static void
months_item_finalize (GObject *object)
{
    MonthsItem *item = MONTHS_ITEM(object);

    g_free (item->number_of_months_label);

    G_OBJECT_CLASS(months_item_parent_class)->finalize (object);
}

static void
months_item_class_init (MonthsItemClass *klass)
{
    GObjectClass *object_class = G_OBJECT_CLASS(klass);
    object_class->finalize = months_item_finalize;
}

static void
months_dropdown_setup_item_single_line_cb (GtkSignalListItemFactory *factory,
                                           GtkListItem *list_item)
{
    GtkWidget *dropdown_label = gtk_label_new ("");

    g_object_set (dropdown_label, "xalign", 0.5, NULL);

    g_object_set_data (G_OBJECT(list_item), "dd-label", dropdown_label);

    gtk_list_item_set_child (list_item, dropdown_label);
}

static void
months_dropdown_selected_item_changed_cb (GtkDropDown *dropdown,
                                          GParamSpec *pspec,
                                          GtkListItem *list_item)
{
    GtkWidget *dropdown_label = g_object_get_data (G_OBJECT(list_item), "dd-label");

// could change the apperence here...

}

static void
months_dropdown_bind_item_cb (GtkSignalListItemFactory *factory,
                              GtkListItem *list_item,
                              gpointer user_data)
{
    GtkDropDown *dropdown = GTK_DROP_DOWN(user_data);

    MonthsItem *item = gtk_list_item_get_item (list_item);
    GtkWidget *dropdown_label = g_object_get_data (G_OBJECT(list_item), "dd-label");
    GtkWidget *popup;

    gtk_label_set_text (GTK_LABEL(dropdown_label), item->number_of_months_label);

    popup = gtk_widget_get_ancestor (dropdown_label, GTK_TYPE_POPOVER);
    if (popup && gtk_widget_is_ancestor (popup, GTK_WIDGET(dropdown)))
    {
        g_signal_connect (G_OBJECT(dropdown), "notify::selected-item",
                          G_CALLBACK(months_dropdown_selected_item_changed_cb), list_item);
        months_dropdown_selected_item_changed_cb (dropdown, NULL, list_item);
    }
}

static void
months_dropdown_unbind_item_cb (GtkSignalListItemFactory *factory,
                                GtkListItem *list_item,
                                gpointer user_data)
{
    GtkDropDown *dropdown = user_data;
    g_signal_handlers_disconnect_by_func (dropdown,
                                          months_dropdown_selected_item_changed_cb,
                                          list_item);
}

static void
months_dropdown_selected_cb (GtkDropDown *widget, GParamSpec *pspec, gpointer user_data)
{
    GncDenseCal *dcal = GNC_DENSE_CAL(user_data);

    MonthsItem *item = gtk_drop_down_get_selected_item (widget);

    if (item)
        gnc_dense_cal_set_num_months (dcal, item->number_of_months);
}

typedef struct
{
    /** The name of the entry. */
    const gchar *month;
    /** The number of months */
    gint month_number;
    /** The number of column to use */
    gint column_number;
} MonthEntries;

/** List of entries. */
static MonthEntries month_labels[] =
{
    { N_("12 months"), 12, 3 },
    { N_("6 months"),   6, 2 },
    { N_("4 months"),   4, 2 },
    { N_("3 months"),   3, 2 },
    { N_("2 month"),    2, 1 },
    { N_("1 month"),    1, 1 },
    { NULL, 0, 0 },
};

static void
months_dropdown_populate_model (GListStore *store, MonthEntries *entries)
{
    for (gint i = 0; (entries[i].month); i++)
    {
        MonthsItem *item = g_object_new (MONTHS_TYPE_ITEM, NULL);

        item->number_of_months_label = g_strdup (entries[i].month);
        item->number_of_months = entries[i].month_number;
        item->number_of_columns = entries[i].column_number;

        g_list_store_append (store, item);
        g_object_unref (item);
    }
}

static GtkWidget *
months_dropdown_new (GncDenseCal *dcal)
{
    GListStore *store = g_list_store_new (MONTHS_TYPE_ITEM);

    months_dropdown_populate_model (store, month_labels);

    GtkWidget *dropdown = g_object_new (GTK_TYPE_DROP_DOWN, "model",
                                        G_LIST_MODEL(store), NULL);

    g_object_unref (store);

    GtkListItemFactory *factory = gtk_signal_list_item_factory_new ();

    g_signal_connect (G_OBJECT(factory), "setup",
                      G_CALLBACK(months_dropdown_setup_item_single_line_cb), dropdown);
    g_signal_connect (G_OBJECT(factory), "bind",
                      G_CALLBACK(months_dropdown_bind_item_cb), dropdown);
    g_signal_connect (G_OBJECT(factory), "unbind",
                      G_CALLBACK(months_dropdown_unbind_item_cb), dropdown);

    g_signal_connect (G_OBJECT(dropdown), "notify::selected",
                      G_CALLBACK(months_dropdown_selected_cb), dcal);

    g_object_set (dropdown,
                  "factory", factory,
                  "list-factory", NULL,
                  NULL);

    g_object_unref (factory);

    return dropdown;
}

/***********************************************************************/

static void
gnc_dense_cal_init (GncDenseCal *dcal)
{
    GtkStyleContext *context = gtk_widget_get_style_context (GTK_WIDGET(dcal));

    gtk_orientable_set_orientation (GTK_ORIENTABLE(dcal), GTK_ORIENTATION_VERTICAL);

    // Set the style context for this widget so it can be easily manipulated with css
    gnc_widget_style_context_add_class (GTK_WIDGET(dcal), "calendar");

    // Set the name of this widget so it can be easily manipulated with css
    gtk_widget_set_name (GTK_WIDGET(dcal), "gnc-id-dense-calendar");

    gtk_style_context_add_class (context, "calendar");
    GtkWidget *hbox = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 0);

    GtkWidget *label = gtk_label_new (_("View"));
    gtk_widget_set_visible (GTK_WIDGET(label), TRUE);
    gtk_box_set_homogeneous (GTK_BOX (hbox), FALSE);
    gtk_widget_set_halign (label, GTK_ALIGN_END);
    gtk_widget_set_margin_end (label, 5);
    gtk_widget_set_hexpand (GTK_WIDGET(label), TRUE);
    gtk_box_append (GTK_BOX(hbox), GTK_WIDGET(label));

    dcal->months_dropdown = months_dropdown_new (dcal);
    gtk_box_append (GTK_BOX(hbox), GTK_WIDGET(dcal->months_dropdown));

    gtk_widget_set_margin_bottom (GTK_WIDGET(hbox), 6);
    gtk_box_append (GTK_BOX(dcal), GTK_WIDGET(hbox));
    gnc_box_set_all_margins (GTK_BOX(dcal), 6);

    dcal->cal_drawing_area = GTK_DRAWING_AREA(gtk_drawing_area_new ());

    dcal->cal_drawing_area_width = 0;
    dcal->cal_drawing_area_height = 0;

    gtk_box_append (GTK_BOX(dcal), GTK_WIDGET(dcal->cal_drawing_area));

    gtk_widget_set_hexpand (GTK_WIDGET(dcal->cal_drawing_area), TRUE);
    gtk_widget_set_vexpand (GTK_WIDGET(dcal->cal_drawing_area), TRUE);

    gtk_drawing_area_set_draw_func (dcal->cal_drawing_area,
                                    gnc_dense_cal_draw, dcal, NULL);

    g_signal_connect (G_OBJECT(dcal->cal_drawing_area), "resize",
                      G_CALLBACK(gnc_dense_cal_resize), (gpointer)dcal);

    GtkGesture *event_gesture = gtk_gesture_click_new ();
    gtk_widget_add_controller (GTK_WIDGET(dcal->cal_drawing_area),
                               GTK_EVENT_CONTROLLER(event_gesture));
    gtk_gesture_single_set_button (GTK_GESTURE_SINGLE(event_gesture), 1);
    g_signal_connect (G_OBJECT(event_gesture), "pressed",
                      G_CALLBACK(gnc_dense_cal_button_press), dcal);

    GtkEventController *event_motion = gtk_event_controller_motion_new ();
    gtk_widget_add_controller (GTK_WIDGET(dcal->cal_drawing_area),
                               GTK_EVENT_CONTROLLER(event_motion));
    g_signal_connect (G_OBJECT(event_motion), "motion",
                      G_CALLBACK(gnc_dense_cal_button_motion), dcal);

    dcal->disposed = FALSE;
    dcal->initialized = FALSE;
    dcal->markData = NULL;
    dcal->numMarks = 0;
    dcal->marks = NULL;
    dcal->lastMarkTag = 0;

    dcal->showPopup = FALSE;
    dcal->is_popped = FALSE;
    dcal->transPopup = gtk_popover_new();

    gtk_popover_set_autohide (GTK_POPOVER(dcal->transPopup), FALSE);

    GtkEventController *focus_controller = gtk_event_controller_focus_new ();
    gtk_widget_add_controller (GTK_WIDGET(dcal->transPopup),
                               GTK_EVENT_CONTROLLER(focus_controller));
    g_signal_connect (G_OBJECT(focus_controller), "leave",
                      G_CALLBACK(popup_focus_leave), dcal);

    g_signal_connect (G_OBJECT(dcal->transPopup), "closed",
                      G_CALLBACK(popup_closed_cb), dcal);

    {
        GtkWidget *vbox, *hbox;
        GtkWidget *l;
        GtkListStore *tree_data;
        GtkTreeView *tree_view;

        vbox = gtk_box_new (GTK_ORIENTATION_VERTICAL, 6);
        gnc_box_set_all_margins (GTK_BOX(vbox), 3);
        gtk_box_set_homogeneous (GTK_BOX(vbox), FALSE);
        hbox = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 6);
        gtk_box_set_homogeneous (GTK_BOX(hbox), FALSE);
        gtk_widget_set_hexpand (GTK_WIDGET(hbox), TRUE);

        gtk_widget_set_name (GTK_WIDGET(dcal->transPopup), "gnc-id-dense-calendar-popup");

        l = gtk_label_new(_("Date:"));
        gtk_widget_set_margin_start (l, 6);
        gtk_box_append (GTK_BOX(hbox), GTK_WIDGET(l));

        l = gtk_label_new ("YY/MM/DD");
        g_object_set_data(G_OBJECT(dcal->transPopup), "dateLabel", l);

        gtk_box_append (GTK_BOX(hbox), GTK_WIDGET(l));
        gtk_box_append (GTK_BOX(vbox), GTK_WIDGET(hbox));
        gtk_box_append (GTK_BOX(vbox), GTK_WIDGET(gtk_separator_new (GTK_ORIENTATION_HORIZONTAL)));
        gtk_widget_set_vexpand (GTK_WIDGET(vbox), TRUE);

        tree_data = gtk_list_store_new (2, G_TYPE_STRING, G_TYPE_STRING);
        tree_view = GTK_TREE_VIEW(gtk_tree_view_new_with_model (GTK_TREE_MODEL(tree_data)));
        gtk_tree_view_insert_column_with_attributes (tree_view, -1, _("Name"),
                                                     gtk_cell_renderer_text_new (), "text", 0, NULL);
        gtk_tree_view_insert_column_with_attributes (tree_view, -1, _("Frequency"),
                                                     gtk_cell_renderer_text_new (), "text", 1, NULL);
        gtk_tree_selection_set_mode (gtk_tree_view_get_selection (GTK_TREE_VIEW(tree_view)), GTK_SELECTION_NONE);
        g_object_set_data (G_OBJECT(dcal->transPopup), "model", tree_data);
        g_object_set_data (G_OBJECT(dcal->transPopup), "tree", tree_view);
        g_object_unref (tree_data);

        // Use this event to capture the escape key being pressed
        GtkEventController *event_controller_tv = gtk_event_controller_key_new ();
        gtk_widget_add_controller (GTK_WIDGET(tree_view),
                                   GTK_EVENT_CONTROLLER(event_controller_tv));
        g_signal_connect (G_OBJECT(event_controller_tv), "key-pressed",
                          G_CALLBACK(popover_escape_cb), dcal);

        gtk_box_append (GTK_BOX(vbox), GTK_WIDGET(tree_view));
        gtk_widget_set_vexpand (GTK_WIDGET(tree_view), TRUE);
        gtk_widget_set_hexpand (GTK_WIDGET(tree_view), TRUE);
        gtk_popover_set_child (GTK_POPOVER(dcal->transPopup), GTK_WIDGET(vbox));
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
        GtkBorder padding;
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

        gtk_style_context_get_padding (context, &padding);
        if ((padding.top + padding.bottom) == 0)
            dcal->bar_label_padding = 2; // px
        else
            dcal->bar_label_padding = (padding.top + padding.bottom) / 2;

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
    gtk_widget_set_size_request (GTK_WIDGET(dcal->cal_drawing_area),
                                 min_width, min_height);
}

GtkWidget*
gnc_dense_cal_new (GtkWindow *parent)
{
    GncDenseCal *dcal = g_object_new (GNC_TYPE_DENSE_CAL, NULL);

    gtk_widget_set_parent (GTK_WIDGET(dcal->transPopup),
                           GTK_WIDGET(parent));

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
    GListModel *months_model = gtk_drop_down_get_model (GTK_DROP_DOWN(dcal->months_dropdown));
    int closest_index_distance = G_MAXINT;
    int index = 0;
    int max_index = g_list_model_get_n_items (months_model);
    int closest_index = 0;
    int months_per_column = 0;

    if (!months_model)
    {
        PWARN("No months model");
        return;
    }

    do
    {
        gint delta_months;
        MonthsItem *item = g_list_model_get_item (months_model, index);

        if (item)
        {
            delta_months = abs (item->number_of_months - (int)num_months);

            if (delta_months < closest_index_distance)
            {
                months_per_column = item->number_of_columns;
                closest_index_distance = delta_months;
                closest_index = index;
            }
            g_object_unref (item);
        }
        index++;
    }
    while ((closest_index_distance != 0) && (index < max_index));

    PINFO("Number of months %d, closest index %d", num_months, closest_index);

    g_signal_handlers_block_by_func (dcal->months_dropdown, months_dropdown_selected_cb, dcal);
    gtk_drop_down_set_selected (GTK_DROP_DOWN(dcal->months_dropdown), closest_index);
    g_signal_handlers_unblock_by_func (dcal->months_dropdown, months_dropdown_selected_cb, dcal);

    // set the number of months per column if found in model
    if (months_per_column != 0)
        dcal->monthsPerCol = months_per_column;

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

    if (dcal->surface)
    {
        cairo_surface_destroy (dcal->surface);
        dcal->surface = NULL;
    }

    /* FIXME: we have a bunch of cleanup to do, here. */

    gdc_free_all_mark_data (dcal);

    g_object_unref (G_OBJECT(dcal->model));

    G_OBJECT_CLASS(gnc_dense_cal_parent_class)->dispose (object);
}

static void
gnc_dense_cal_finalize (GObject *object)
{
    g_return_if_fail (object != NULL);
    g_return_if_fail (GNC_IS_DENSE_CAL(object));

    G_OBJECT_CLASS(gnc_dense_cal_parent_class)->finalize(object);
}

static void
gnc_dense_cal_resize (GtkDrawingArea* widget, gint width, gint height, gpointer user_data)
{
    GncDenseCal *dcal = user_data;

    dcal->cal_drawing_area_height = height;
    dcal->cal_drawing_area_width = width;

    recompute_x_y_scales (dcal);
    gdc_reconfig (dcal);
}

static void
gdc_reconfig (GncDenseCal *dcal)
{
    if (dcal->cal_drawing_area_width > 0)
        dcal->surface = cairo_image_surface_create (CAIRO_FORMAT_ARGB32,
                                                    dcal->cal_drawing_area_width,
                                                    dcal->cal_drawing_area_height);
    else
        dcal->surface = cairo_image_surface_create (CAIRO_FORMAT_ARGB32,
                                                    DENSE_CAL_DEFAULT_WIDTH,
                                                    DENSE_CAL_DEFAULT_HEIGHT);
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

    if (dcal->cal_drawing_area_width > 0)
    {
        width = dcal->cal_drawing_area_width;
        height = dcal->cal_drawing_area_height;
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
    GncDenseCal *dcal = GNC_DENSE_CAL(user_data);

    g_return_if_fail (GNC_IS_DENSE_CAL(user_data));

    cairo_save (cr);
    cairo_set_source_surface (cr, dcal->surface, 0, 0);
    cairo_paint (cr);
    cairo_restore (cr);
}

static void
gnc_dense_cal_draw_to_buffer (GncDenseCal *dcal)
{
    GtkWidget *widget;
    GtkStyleContext *stylectxt;
    GtkStateFlags state_flags;
    GtkAllocation alloc;
    gint i;
    int maxWidth;
    PangoLayout *layout;
    cairo_t *cr;
    gchar *primary_color_class, *secondary_color_class, *marker_color_class;

    DEBUG("drawing");
    widget = GTK_WIDGET(dcal);

    if (!dcal->surface)
        return;

    cr = cairo_create (dcal->surface);
    layout = gtk_widget_create_pango_layout (GTK_WIDGET(dcal), NULL);

    gtk_widget_get_allocation (GTK_WIDGET(dcal->cal_drawing_area), &alloc);
    stylectxt = gtk_widget_get_style_context (GTK_WIDGET(dcal->cal_drawing_area));
    state_flags = gtk_style_context_get_state (stylectxt);

    gtk_style_context_add_class (stylectxt, "background");
    gtk_style_context_add_class (stylectxt, "calendar");

    gtk_render_background (stylectxt, cr, 0, 0,
                           cairo_image_surface_get_width (dcal->surface),
                           cairo_image_surface_get_height (dcal->surface));

    gtk_style_context_remove_class (stylectxt, "background");

    /* get the colors */
    {
         GdkRGBA color;
         gchar *class_extension = NULL;

         gtk_style_context_get_color (stylectxt, &color);

          if (gnc_is_dark_theme (&color))
              class_extension = "-dark";

          primary_color_class = g_strconcat ("primary", class_extension, NULL);
          secondary_color_class = g_strconcat ("secondary", class_extension, NULL);
          marker_color_class = g_strconcat ("markers", class_extension, NULL);
    }

    /* lets confirm text height size */
    pango_layout_set_text (layout, "S", -1);
    pango_layout_get_pixel_size (layout, NULL, &dcal->label_height);
    dcal->month_side_bar_width = dcal->label_height + (dcal->bar_label_padding * 2);
    dcal->day_top_bar_height = dcal->label_height + (dcal->bar_label_padding * 2);

    /* Fill in alternating month colors. */
    {
        gint i;
        GdkRectangle *rect;
        GList *mcList, *mcListIter;

        /* reset all of the month position offsets. */
        for (i = 0; i < 12; i++)
        {
            dcal->monthPositions[i].x = dcal->monthPositions[i].y = -1;
        }

        gtk_style_context_save (stylectxt);

        /* Paint the weeks for the upcoming N months. */
        for (i = 0; i < dcal->numMonths; i++)
        {
            mcList = NULL;
            month_coords (dcal, i, &mcList);
            dcal->monthPositions[i].x = floor (i / dcal->monthsPerCol)
                                            * (col_width (dcal) + COL_BORDER_SIZE);
            dcal->monthPositions[i].y = ((GdkRectangle*)mcList->next->next->next->data)->y;
            for (mcListIter = mcList; mcListIter != NULL; mcListIter = mcListIter->next)
            {
                rect = (GdkRectangle*)mcListIter->data;
                gtk_style_context_save (stylectxt);

                if (i % 2 == 0)
                    gtk_style_context_add_class (stylectxt, primary_color_class);
                else
                    gtk_style_context_add_class (stylectxt, secondary_color_class);

                gtk_render_background (stylectxt, cr, rect->x, rect->y, rect->width, rect->height);
                gtk_style_context_restore (stylectxt);
            }
            g_list_foreach (mcList, free_rect, NULL);
            g_list_free (mcList);
        }
        gtk_style_context_restore (stylectxt);
    }

    /* Highlight the marked days. */
    {
        int i;
        int x1, x2, y1, y2;

        gtk_style_context_save (stylectxt);
        gtk_style_context_add_class (stylectxt, marker_color_class);
        gtk_style_context_add_class (stylectxt, "view");
        gtk_style_context_set_state (stylectxt, GTK_STATE_FLAG_SELECTED);

        for (i = 0; i < dcal->numMarks; i++)
        {
            if (dcal->marks[i] != NULL)
            {
                int center_x, center_y, radius;

                doc_coords(dcal, i, &x1, &y1, &x2, &y2);
                center_x = (x1 + x2 ) / 2;
                center_y = (y1 + y2 ) / 2;
                radius = MIN((x2 - x1), (y2 - y1)) * .75;

                // try to compensate for row height/width being odd or even
                if (((y2 - y1) % 2) != 0)
                    center_y = center_y + 1;

                if (((x2 - x1) % 2) != 0)
                    center_x = center_x + 1;

                gtk_render_background (stylectxt, cr,
                                       center_x - (radius + 2), center_y - radius,
                                        (radius * 2) + 4, radius * 2);
            }
        }
        gtk_style_context_restore (stylectxt);
    }

    for (i = 0; i < num_cols (dcal); i++)
    {
        GdkRGBA color;
        gint x, y, w, h;
        gint j;

        cairo_save (cr);
        gdk_rgba_parse (&color, "black");

        x = dcal->leftPadding
            + (i * (col_width (dcal) + COL_BORDER_SIZE))
            + dcal->month_side_bar_width + 1;
        y = dcal->topPadding + dcal->day_top_bar_height;
        w = col_width (dcal) - COL_BORDER_SIZE - dcal->month_side_bar_width;
        h = col_height (dcal);

        gtk_style_context_save (stylectxt);

        /* draw the outside border [inside the month labels] */
        gtk_style_context_add_class (stylectxt, "frame");

        gtk_render_frame (stylectxt, cr, x, y, w + 1, h + 1);

        cairo_set_source_rgb (cr, color.red, color.green, color.blue);
        cairo_set_line_width (cr, 1);

        /* draw the week separations */
        for (j = 0; j < num_weeks_per_col (dcal); j++)
        {
            gint wy = y + (j * week_height (dcal));
            cairo_move_to (cr, x, wy + 0.5);
            cairo_line_to (cr, x + w, wy + 0.5);
            cairo_stroke (cr);
        }

        /* draw the day separations */
        for (j = 1; j < 7; j++)
        {
            gint dx = x + (j * day_width (dcal));
            cairo_move_to (cr, dx + 0.5, y);
            cairo_line_to (cr, dx + 0.5, y + col_height (dcal));
            cairo_stroke (cr);
        }
        cairo_restore (cr);
        gtk_style_context_restore (stylectxt);


        /* draw the day of the week labels */
        pango_layout_set_text (layout, "88", -1);
        pango_layout_get_pixel_size (layout, &maxWidth, NULL);

        if (dcal->x_scale > maxWidth)
        {
            gtk_style_context_save (stylectxt);
            gtk_style_context_add_class (stylectxt, "header");

            gtk_render_background (stylectxt, cr, x,
                                                  y - dcal->day_top_bar_height,
                                                  (day_width(dcal) * 7) + 1,
                                                  dcal->day_top_bar_height);

            for (j = 0; j < 7; j++)
            {
                int day_label_width;
                gint label_x_offset, label_y_offset;
                gint day_label_str_len = 4;
                gchar day_label_str[day_label_str_len + 1];
                day_label (day_label_str, day_label_str_len, (j + dcal->day_of_week_start) % 7);
                pango_layout_set_text (layout, day_label_str, -1);
                pango_layout_get_pixel_size (layout, &day_label_width, NULL);
                label_x_offset = x
                                 + (j * day_width (dcal))
                                 + (day_width (dcal) / 2)
                                 - (day_label_width / 2);
                label_y_offset = y - dcal->day_top_bar_height + dcal->bar_label_padding;
                pango_layout_set_text (layout, day_label_str, -1);
                gtk_render_layout (stylectxt, cr, label_x_offset, label_y_offset, layout);
            }
            gtk_style_context_restore (stylectxt);
        }
    }

    /* Month labels. */
    {
        gint i;
        gint x_offset = dcal->leftPadding;

        gtk_style_context_save (stylectxt);
        gtk_style_context_add_class (stylectxt, "header");

        for (i = 0; i < 12; i++)
        {
            if (dcal->monthPositions[i].x == -1)
                break;

            gtk_render_background (stylectxt, cr, dcal->monthPositions[i].x + x_offset,
                                                  dcal->topPadding,
                                                  dcal->month_side_bar_width + 1,
                                                  col_height(dcal) + dcal->day_top_bar_height + 1);
        }

        for (i = 0; i < 12; i++)
        {
            guint idx;

            if (dcal->monthPositions[i].x == -1)
                break;
            idx = (dcal->month - 1 + i) % 12;
            pango_layout_set_text (layout, month_name (idx), -1);
            cairo_save (cr);
            cairo_translate (cr, dcal->monthPositions[i].x + x_offset, dcal->monthPositions[i].y);
            cairo_rotate (cr, -G_PI / 2.);
            gtk_render_layout (stylectxt, cr, 0, dcal->bar_label_padding, layout);
            cairo_restore (cr);
        }
        gtk_style_context_restore (stylectxt);
    }

    /* Day number strings [dates] */
    {
        GDate d, eoc;
        gint doc;
        gchar dayNumBuf[4];
        gint numW, numH;
        gint x1, y1, x2, y2, w, h;

        GDate now;
        g_date_clear (&now, 1);
        gnc_gdate_set_today (&now);
        gboolean today_found = FALSE;

        gtk_style_context_save (stylectxt);
        gtk_style_context_add_class (stylectxt, "day-number");

        cairo_save (cr);
        g_date_set_dmy (&d, 1, dcal->month, dcal->year);
        eoc = d;
        g_date_add_months (&eoc, dcal->numMonths);
        for (doc = 0; g_date_get_julian (&d) < g_date_get_julian (&eoc); g_date_add_days (&d, 1), doc++)
        {
            doc_coords (dcal, doc, &x1, &y1, &x2, &y2);
            memset (dayNumBuf, 0, 4);
            snprintf (dayNumBuf, 4, "%d", g_date_get_day(&d));
            pango_layout_set_text (layout, dayNumBuf, -1);
            pango_layout_get_pixel_size (layout, &numW, &numH);
            w = (x2 - x1) + 1;
            h = (y2 - y1) + 1;

            if (!today_found && g_date_compare (&d, &now) == 0)
            {
                GtkBorder border;

                gtk_style_context_save (stylectxt);
                gtk_style_context_add_class (stylectxt, marker_color_class);
                gtk_style_context_add_class (stylectxt, "frame");

                gtk_style_context_get_border (stylectxt, &border);

                today_found = TRUE;

                if (border.left + border.right != 0)
                {
                    GtkCssProvider *provider = gtk_css_provider_new ();
                    gchar *frame_css = ".marker-border {\n  border-color:black;\n}\n";

                    gint dayw = day_width (dcal);
                    gint dayh = day_height (dcal);
                    gint bw = (border.left + border.right) / 2;

                    gtk_css_provider_load_from_data (provider, frame_css, -1);
                    gtk_style_context_add_provider (stylectxt, GTK_STYLE_PROVIDER(provider),
                                                    GTK_STYLE_PROVIDER_PRIORITY_APPLICATION);
                    g_object_unref (provider);

                    gtk_style_context_add_class (stylectxt, "marker-border");

                    gtk_render_frame (stylectxt, cr, x1 - (dayw / 4) + 3,
                                                     y1 - (dayh / 4) + 2,
                                                     dayw - 4 - bw,
                                                     dayh - 4 - bw);

                    gtk_style_context_remove_class (stylectxt, "marker-border");
                }
                gtk_style_context_restore (stylectxt);
            }
            gtk_render_layout (stylectxt, cr, x1 + (w / 2) - (numW / 2), y1 + (h / 2) - (numH / 2), layout);
        }
        cairo_restore (cr);
        gtk_style_context_restore (stylectxt);
    }

    gtk_widget_get_allocation (widget, &alloc);
    gtk_widget_queue_draw (GTK_WIDGET(dcal));

    g_free (primary_color_class);
    g_free (secondary_color_class);
    g_free (marker_color_class);

    g_object_unref (layout);
    cairo_destroy (cr);
}

static void
populate_hover_window (GncDenseCal *dcal)
{
    GtkWidget *w;
    GDate *date;

    if (dcal->doc >= 0)
    {
        GObject *o;
        GtkListStore *model;
        GList *l;

        w = GTK_WIDGET(g_object_get_data (G_OBJECT(dcal->transPopup), "dateLabel"));
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
        gtk_label_set_text (GTK_LABEL(w), date_buff);

        o = G_OBJECT(dcal->transPopup);
        model = GTK_LIST_STORE(g_object_get_data (o, "model"));
        gtk_list_store_clear (model);
        for (l = dcal->marks[dcal->doc]; l; l = l->next)
        {
            GtkTreeIter iter;
            gdc_mark_data *gdcmd;

            gdcmd = (gdc_mark_data*)l->data;
            gtk_list_store_insert (model, &iter, INT_MAX);
            gtk_list_store_set (model, &iter, 0, (gdcmd->name ? gdcmd->name : _("(unnamed)")),
                                                                1, gdcmd->info, -1);
        }

        // if there are no rows, add one
        if (gtk_tree_model_iter_n_children (GTK_TREE_MODEL(model), NULL) == 0)
        {
            GtkTreeIter iter;
            gtk_list_store_insert (model, &iter, -1);
        }

        // make sure all pending events are processed
        while (g_main_context_pending (NULL))
            g_main_context_iteration (NULL, TRUE);

        g_date_free (date);
    }
}

static gboolean
compute_popup_position_and_populate (GncDenseCal *dcal, gint x, gint y)
{
    GtkRoot *root = gtk_widget_get_root (GTK_WIDGET(dcal->cal_drawing_area));
    graphene_matrix_t matrix;
    float x_translation = 0.0;
    float y_translation = 0.0;
    GdkRectangle rect;

    if (gtk_widget_compute_transform (GTK_WIDGET(dcal->cal_drawing_area), GTK_WIDGET(root), &matrix))
    {
        x_translation = graphene_matrix_get_x_translation (&matrix);
        y_translation = graphene_matrix_get_y_translation (&matrix);
    }
    else
        return FALSE;

    populate_hover_window (dcal);

    GtkAllocation alloc;
    gtk_widget_get_allocation (GTK_WIDGET(dcal->transPopup), &alloc);

    rect.x = x + x_translation - 10; // need space for the move
    rect.y = y + y_translation;
    rect.width = 2;
    rect.height = 2;

    if (alloc.width - 10 >= rect.x)
    {
        rect.x = rect.x + 10;
        gtk_popover_set_position (GTK_POPOVER(dcal->transPopup), GTK_POS_RIGHT);
    }
    else
        gtk_popover_set_position (GTK_POPOVER(dcal->transPopup), GTK_POS_LEFT);

    gtk_popover_set_pointing_to (GTK_POPOVER(dcal->transPopup), &rect);

    gtk_widget_queue_resize (GTK_WIDGET(dcal->transPopup));

    return TRUE;
}

static void
gnc_dense_cal_button_press (GtkGestureClick *gesture,
                            int n_press,
                            double x,
                            double y,
                            gpointer user_data)
{
    GncDenseCal *dcal = user_data;

    if (dcal->is_popped)
    {
        gtk_popover_popdown (GTK_POPOVER(dcal->transPopup));
        return;
    }

    dcal->doc = wheres_this (dcal, x, y);
    if (dcal->doc >= 0)
    {
        if (compute_popup_position_and_populate (dcal, x, y))
        {
            gtk_popover_popup (GTK_POPOVER(dcal->transPopup));

            gtk_widget_grab_focus (GTK_WIDGET(g_object_get_data (
                                   G_OBJECT(dcal->transPopup), "tree")));

            dcal->is_popped = TRUE;
         }
    }
    else
    {
        dcal->doc = -1;
        gtk_popover_popdown (GTK_POPOVER(dcal->transPopup));
        dcal->is_popped = FALSE;
    }
}


static void
popup_focus_leave (GtkWidget *widget, gpointer user_data)
{
    GncDenseCal *dcal = user_data;

    if (dcal->is_popped)
    {
        dcal->doc = -1;
        gtk_popover_popdown (GTK_POPOVER(dcal->transPopup));
    }
}

static void
popup_closed_cb (GtkPopover *widget, gpointer user_data)
{
    GncDenseCal *dcal = user_data;
    dcal->is_popped = FALSE;
}

static gboolean
popover_escape_cb (GtkEventControllerKey *key, guint keyval,
                   guint keycode, GdkModifierType state,
                   gpointer user_data)
{
    GncDenseCal *dcal = user_data;

    if (keyval == GDK_KEY_Escape)
    {
        dcal->doc = -1;
        gtk_popover_popdown (GTK_POPOVER(dcal->transPopup));
        dcal->is_popped = FALSE;
        return TRUE;
    }
    else
        return FALSE;
}
//FIXME gtk4 this sometimes locks up, possibly pointer under popup
static void
gnc_dense_cal_button_motion (GtkEventControllerMotion *controller,
                             double x, double y, gpointer user_data)
{
    GncDenseCal *dcal = user_data;

    if (!dcal->is_popped)
        return;

    dcal->doc = wheres_this (dcal, x, y);
    if (dcal->doc >= 0)
    {
        compute_popup_position_and_populate (dcal, x, y);
    }
    else
    {
        dcal->doc = -1;
        gtk_popover_popdown (GTK_POPOVER(dcal->transPopup));
    }
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
    GtkAllocation alloc;

    x -= dcal->leftPadding;
    y -= dcal->topPadding;

    if ((x < 0) || (y < 0))
    {
        return -1;
    }
    gtk_widget_get_allocation (GTK_WIDGET(dcal), &alloc);
    if ((x >= alloc.width)
            || (y >= alloc.height))
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
