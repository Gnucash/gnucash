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

static QofLogModule log_module = GNC_MOD_SX;

static void gnc_dense_cal_class_init (GncDenseCalClass *class);
static void gnc_dense_cal_init (GncDenseCal *dcal);
static void gnc_dense_cal_finalize (GObject *object);
static void gnc_dense_cal_dispose (GObject *object);
static void gnc_dense_cal_realize (GtkWidget *widget);
static void gnc_dense_cal_draw_to_buffer(GncDenseCal *dcal);
static gint gnc_dense_cal_expose(GtkWidget      *widget,
                                  GdkEventExpose *event);

static void gdc_reconfig(GncDenseCal *dcal);

static void gdc_free_all_mark_data(GncDenseCal *dcal);

static void gnc_dense_cal_size_request(GtkWidget      *widget,
                                        GtkRequisition *requisition);
static void gnc_dense_cal_size_allocate(GtkWidget     *widget,
                                         GtkAllocation *allocation);
static gint gnc_dense_cal_motion_notify(GtkWidget      *widget,
                                         GdkEventMotion *event);
static gint gnc_dense_cal_button_press(GtkWidget *widget,
                                        GdkEventButton *evt);

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
static void gdc_mark_remove(GncDenseCal *dcal, guint mark_to_remove);

static void gdc_add_tag_markings(GncDenseCal *cal, guint tag);
static void gdc_add_markings(GncDenseCal *cal);
static void gdc_remove_markings(GncDenseCal *cal);

static GtkWidgetClass *parent_class = NULL;

#define MONTH_NAME_BUFSIZE 5
/* Takes the number of months since January, in the range 0 to
 * 11. Returns the abbreviated month name according to the current
 * locale.*/
static const gchar *month_name(int mon) 
{
     static gchar buf[MONTH_NAME_BUFSIZE];
     GDate date;

     memset(buf, 0, MONTH_NAME_BUFSIZE);
     g_date_clear(&date, 1);
     g_date_set_time_t(&date, time(NULL));
     // g_date API is 1..12 (not 0..11)
     g_date_set_month(&date, mon+1);
     g_date_strftime(buf, MONTH_NAME_BUFSIZE-1, "%b", &date);
     
     return buf;
}
/* Takes the number of days since Sunday, in the range 0 to 6. Returns
 * the abbreviated weekday name according to the current locale. */
static const gchar *day_label(int wday)
{
    static gchar buf[MONTH_NAME_BUFSIZE];
    struct tm my_tm;
    int i;
    
    memset(buf, 0, MONTH_NAME_BUFSIZE);
    memset(&my_tm, 0, sizeof(struct tm));
    my_tm.tm_wday = wday;
    i = strftime (buf, MONTH_NAME_BUFSIZE-1, "%a", &my_tm);
    /* Wild hack to use only the first two letters */
    buf[2]='\0';
    return buf;
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

          dense_cal_type = g_type_register_static(GTK_TYPE_WIDGET,
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

     widget_class->realize = gnc_dense_cal_realize;
     widget_class->expose_event = gnc_dense_cal_expose;
     widget_class->size_request = gnc_dense_cal_size_request;
     widget_class->size_allocate = gnc_dense_cal_size_allocate;
     widget_class->motion_notify_event = gnc_dense_cal_motion_notify;
     widget_class->button_press_event = gnc_dense_cal_button_press;
}

static void
gnc_dense_cal_init(GncDenseCal *dcal)
{
     gboolean colorAllocSuccess;

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

          gtk_widget_realize(GTK_WIDGET(dcal->transPopup));
     }

     gdk_color_parse(MONTH_THIS_COLOR,  &dcal->weekColors[MONTH_THIS]);
     gdk_color_parse(MONTH_THAT_COLOR,  &dcal->weekColors[MONTH_THAT]);
     if (gdk_colormap_alloc_colors(gdk_colormap_get_system(),
                                   dcal->weekColors,
                                   MAX_COLORS, TRUE, TRUE,
                                   &colorAllocSuccess) > 0)
     {
          /* FIXME : handle [more] properly */
          PERR("Error allocating colors\n");
     }
     
     /* Deal with the various label sizes. */
     {
          gint i;
          gint maxWidth, maxHeight, maxAscent, maxLBearing;
          gint lbearing, rbearing, width, ascent, descent;
          GtkStyle *style;

          /* @FIXME: GNOME 2 port (rework the complete font code) */
          style = gtk_widget_get_style(GTK_WIDGET(dcal));

          dcal->dayLabelFont = gtk_style_get_font(style);
          gdk_font_ref(dcal->dayLabelFont);
          g_assert(dcal->dayLabelFont);

          dcal->monthLabelFont = gtk_style_get_font(style);
          g_assert(dcal->monthLabelFont);
          gdk_font_ref(dcal->monthLabelFont);

          maxWidth = maxHeight = maxAscent = maxLBearing = 0;
          for (i=0; i<12; i++)
          {
               gint w, h;
               gdk_string_extents(dcal->monthLabelFont, month_name(i),
                                  &lbearing, &rbearing, &width,
                                  &ascent, &descent);
               w = rbearing - lbearing + 1;
               h = ascent + descent;
               maxLBearing = MAX(maxLBearing, ABS(lbearing));
               maxWidth = MAX(maxWidth, w);
               maxHeight = MAX(maxHeight, h);
               maxAscent = MAX(maxAscent, ascent);
          }
          dcal->label_width    = maxHeight + 1;
          dcal->label_height   = maxWidth;
          dcal->label_lbearing = maxLBearing;
          dcal->label_ascent   = maxAscent;
          dcal->needInitMonthLabels = TRUE;
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
          gnc_dense_cal_set_month(dcal, g_date_get_month(now));
          gnc_dense_cal_set_year(dcal, g_date_get_year(now));
          g_date_free(now);
     }

     recompute_extents(dcal);
     recompute_mark_storage(dcal);

     /* Now that we're "sure" of our configuration, compute initial
      * scaling factors; will be increased when we're allocated enough
      * space to scale up. */
     dcal->min_x_scale = dcal->x_scale =
          MAX(gdk_string_width(dcal->monthLabelFont, "88"),
              gdk_string_width(dcal->dayLabelFont, "88") + 2);
     dcal->min_y_scale = dcal->y_scale =
          MAX(floor((float)gdk_string_width(dcal->monthLabelFont,
                                            "XXX")
                    / 3.0),
              gdk_string_height(dcal->dayLabelFont, "88") + 2);
     dcal->dayLabelHeight = gdk_string_height(dcal->monthLabelFont, "88");
     dcal->initialized = TRUE;
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
     dcal->month = mon;
     recompute_first_of_month_offset(dcal);
     recompute_extents(dcal);
     if (GTK_WIDGET_REALIZED(dcal))
     {
          recompute_x_y_scales(dcal);
          gnc_dense_cal_draw_to_buffer(dcal);
          gtk_widget_queue_draw(GTK_WIDGET(dcal));
     }
}

void
gnc_dense_cal_set_year(GncDenseCal *dcal, guint year)
{
     dcal->year = year;
     recompute_first_of_month_offset(dcal);
     recompute_extents(dcal);
     if (GTK_WIDGET_REALIZED(dcal))
     {
          recompute_x_y_scales(dcal);
          gnc_dense_cal_draw_to_buffer(dcal);
          gtk_widget_queue_draw(GTK_WIDGET(dcal));
     }
}

void
gnc_dense_cal_set_num_months(GncDenseCal *dcal, guint num_months)
{
     dcal->numMonths = num_months;
     recompute_extents(dcal);
     recompute_mark_storage(dcal);
     if (GTK_WIDGET_REALIZED(dcal))
     {
          recompute_x_y_scales(dcal);
          gnc_dense_cal_draw_to_buffer(dcal);
          gtk_widget_queue_draw(GTK_WIDGET(dcal));
     }
}

void
gnc_dense_cal_set_months_per_col(GncDenseCal *dcal, guint monthsPerCol)
{
     dcal->monthsPerCol = monthsPerCol;
     recompute_x_y_scales(dcal);
}

guint
gnc_dense_cal_get_num_months(GncDenseCal *dcal)
{
     return dcal->numMonths;
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
     int i;
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
     /* monthLabelFont, dayLabelFont */
     if (dcal->monthLabelFont)
     {
          gdk_font_unref(dcal->monthLabelFont);
          dcal->monthLabelFont = NULL;
     }

     if (dcal->dayLabelFont)
     {
          gdk_font_unref(dcal->dayLabelFont);
          dcal->dayLabelFont = NULL;
     }

     /* month labels */
     if (dcal->monthLabels[0])
     {
          for (i=0; i < 12; i++)
          {
               g_object_unref(dcal->monthLabels[i]);
               dcal->monthLabels[i] = NULL;
          }
     }
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
gnc_dense_cal_realize (GtkWidget *widget)
{
     GncDenseCal *dcal;
     GdkWindowAttr attributes;
     gint attributes_mask;

     g_return_if_fail(widget != NULL);
     g_return_if_fail(GNC_IS_DENSE_CAL (widget));

     GTK_WIDGET_SET_FLAGS(widget, GTK_REALIZED);
     dcal = GNC_DENSE_CAL(widget);

     attributes.x = widget->allocation.x;
     attributes.y = widget->allocation.y;
     attributes.width = widget->allocation.width;
     attributes.height = widget->allocation.height;
     attributes.wclass = GDK_INPUT_OUTPUT;
     attributes.window_type = GDK_WINDOW_CHILD;
     attributes.event_mask =
          gtk_widget_get_events(widget)
          | GDK_EXPOSURE_MASK
          | GDK_BUTTON_PRESS_MASK
          | GDK_BUTTON_RELEASE_MASK
          | GDK_POINTER_MOTION_MASK
          | GDK_POINTER_MOTION_HINT_MASK;
     attributes.visual = gtk_widget_get_visual(widget);
     attributes.colormap = gtk_widget_get_colormap(widget);

     attributes_mask = GDK_WA_X | GDK_WA_Y | GDK_WA_VISUAL | GDK_WA_COLORMAP;

     widget->window = gdk_window_new(widget->parent->window, &attributes, attributes_mask);

     widget->style = gtk_style_attach(widget->style, widget->window);

     gdk_window_set_user_data(widget->window, widget);

     gdc_reconfig(dcal);

     gtk_style_set_background(widget->style, widget->window, GTK_STATE_ACTIVE);
}

static void
gdc_reconfig(GncDenseCal *dcal)
{
     GtkWidget *widget = GTK_WIDGET(dcal);

     if (dcal->drawbuf)
          g_object_unref(dcal->drawbuf);
     dcal->drawbuf = gdk_pixmap_new(widget->window,
                                    widget->allocation.width,
                                    widget->allocation.height,
                                    -1);
     gnc_dense_cal_draw_to_buffer(dcal);
}

static void 
gnc_dense_cal_size_request(GtkWidget *widget,
                           GtkRequisition *requisition)
{
     GncDenseCal *dcal = GNC_DENSE_CAL(widget);
     if (!dcal->initialized)
     {
          PERR("Uninitialized size request\n");
          requisition->width  = DENSE_CAL_DEFAULT_WIDTH;
          requisition->height = DENSE_CAL_DEFAULT_HEIGHT;
          return;
     }
     requisition->width =
          (dcal->leftPadding * 2)
          + (num_cols(dcal)* (col_width_at(dcal, dcal->min_x_scale)
                              + dcal->label_width))
          + ((num_cols(dcal)-1) * COL_BORDER_SIZE);
     requisition->height =
          (dcal->topPadding * 2)
          + MINOR_BORDER_SIZE
          + dcal->dayLabelHeight
          + (num_weeks_per_col(dcal)
             * week_height_at(dcal, dcal->min_y_scale));
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
          width  = widget->allocation.width;
          height = widget->allocation.height;
     }

     /* FIXME: there's something slightly wrong in the x_scale computation that
      * lets us draw larger than our area. */
     denom = 7 * num_cols(dcal);
     g_assert(denom != 0);
     dcal->x_scale = (gint)((width
                             - (dcal->leftPadding * 2)
                             - (num_cols(dcal) * ((8 * MINOR_BORDER_SIZE)
                                                  + dcal->label_width))
                             - ((num_cols(dcal)-1) * COL_BORDER_SIZE))
                            / denom);
     dcal->x_scale = MAX(dcal->x_scale, dcal->min_x_scale);

     denom = num_weeks_per_col(dcal);
     g_assert(denom != 0);
     dcal->y_scale = (gint)((height
                             - (dcal->topPadding * 2)
                             - MINOR_BORDER_SIZE
                             - dcal->dayLabelHeight
                             - (num_weeks_per_col(dcal)-1
                                * MINOR_BORDER_SIZE))
                            / denom);
     dcal->y_scale = MAX(dcal->y_scale, dcal->min_y_scale);
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
     start_week = g_date_get_sunday_week_of_year(&date);
     g_date_add_months(&date, dcal->numMonths);
     end_week = g_date_get_sunday_week_of_year(&date);
     if (g_date_get_year(&date) != dcal->year)
          end_week += g_date_get_sunday_weeks_in_year(dcal->year);
     dcal->num_weeks = end_week - start_week + 1;
}

static void
gnc_dense_cal_size_allocate(GtkWidget *widget,
                            GtkAllocation *allocation)
{
     GncDenseCal *dcal;

     g_return_if_fail(widget != NULL);
     g_return_if_fail(GNC_IS_DENSE_CAL (widget));
     g_return_if_fail(allocation != NULL);

     dcal = GNC_DENSE_CAL(widget);

     widget->allocation = *allocation;

     if (GTK_WIDGET_REALIZED(widget)) {
          gdk_window_move_resize(widget->window,
                                 allocation->x, allocation->y,
                                 allocation->width,
                                 allocation->height);
          
          /* We want to know how many px we can increase every day
           * [width] or week [height]. */
          recompute_x_y_scales(dcal);

          gdc_reconfig(dcal);
     }
}

static void
free_rect(gpointer data, gpointer ud)
{
     g_free((GdkRectangle*)data);
}

static gint
gnc_dense_cal_expose(GtkWidget *widget,
                      GdkEventExpose *event)
{
     GncDenseCal *dcal;
     GdkGC *gc;

     g_return_val_if_fail(widget != NULL, FALSE);
     g_return_val_if_fail(GNC_IS_DENSE_CAL(widget), FALSE);
     g_return_val_if_fail(event != NULL, FALSE);

     if (event->count > 0)
          return FALSE;

     dcal = GNC_DENSE_CAL(widget);
     gc = widget->style->fg_gc[GTK_WIDGET_STATE(widget)];
     gdk_draw_drawable(GDK_DRAWABLE(widget->window),
                       gc,
                       GDK_DRAWABLE(dcal->drawbuf),
                       0, 0, 0, 0,
                       widget->allocation.width,
                       widget->allocation.height);

     return FALSE;
}

static void
gnc_dense_cal_draw_to_buffer(GncDenseCal *dcal)
{
     GtkWidget *widget;
     gint i;
     int maxWidth;

     widget = &dcal->widget;

     if (!dcal->drawbuf)
          return;

     gdk_draw_rectangle(dcal->drawbuf,
                        widget->style->white_gc,
                        TRUE,
                        0, 0,
                        widget->allocation.width,
                        widget->allocation.height);


     if (dcal->needInitMonthLabels)
     {
          /* Create the month labels */
          gint i;
          GdkPixmap *tmpPix;
          GdkImage *tmpImg;
          GdkGC *gc;
          GdkColor black;

          gc = widget->style->fg_gc[widget->state];
          tmpPix = gdk_pixmap_new(NULL,
                                  dcal->label_height,
                                  dcal->label_width,
                                  gdk_visual_get_system()->depth);
          black.pixel = gdk_rgb_xpixel_from_rgb(0);
          for (i=0; i<12; i++)
          {
               guint x,y;
               /* these are going to be rotated, so transpose width
                * and height */
               dcal->monthLabels[i] =
                    gdk_pixmap_new(widget->window,
                                   dcal->label_width,
                                   dcal->label_height, -1);
               gdk_draw_rectangle(dcal->monthLabels[i],
                                  widget->style->white_gc,
                                  TRUE, 0, 0,
                                  dcal->label_width,
                                  dcal->label_height);

               gdk_draw_rectangle(tmpPix,
                                  widget->style->white_gc,
                                  TRUE, 0, 0,
                                  dcal->label_height,
                                  dcal->label_width);

               gdk_draw_string(tmpPix, dcal->monthLabelFont, gc,
                               dcal->label_lbearing,
                               dcal->label_ascent,
                               month_name(i));

               tmpImg = gdk_image_get(tmpPix, 0, 0,
                                      dcal->label_height,
                                      dcal->label_width);

               /* now, (transpose the pixel matrix)==(do a 90-degree
                * counter-clockwise rotation) */
               for (x=0; x < dcal->label_height; x++)
               {
                    for (y=0; y < dcal->label_width; y++)
                    {
                         if (gdk_image_get_pixel(tmpImg, x, y) != black.pixel)
                              continue;
                         gdk_draw_point(dcal->monthLabels[i],
                                        gc, y,
                                        dcal->label_height - x);
                    }
               }
               gdk_image_destroy(tmpImg);
          }
          dcal->needInitMonthLabels = FALSE;
     }

     /* Fill in alternating month colors. */
     {
          gint i;
          GdkGC *gc;
          GdkRectangle *rect;
          GList *mcList, *mcListIter;

          gc = gdk_gc_new(dcal->widget.window);
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
                                       rect->width - 2, rect->height);
               }
               g_list_foreach(mcList, free_rect, NULL);
               g_list_free(mcList);
          }

          gdk_gc_destroy(gc);
     }

     /* Hilight the marked days. */
     {
          int i;
          int x1, x2, y1, y2;
          GdkColor markColor, black;

          gdk_color_parse(MARK_COLOR, &markColor);
          gdk_colormap_alloc_color(gdk_colormap_get_system(), &markColor, TRUE, TRUE);
          gdk_color_black(gdk_colormap_get_system(), &black);

          /* FIXME: use a different GC for this */
          gdk_gc_set_foreground(widget->style->fg_gc[widget->state], &markColor);
          for (i=0; i<dcal->numMarks; i++)
          {
               if (dcal->marks[i] != NULL)
               {
                    doc_coords(dcal, i, &x1, &y1, &x2, &y2);
                    gdk_draw_rectangle(dcal->drawbuf,
                                       widget->style->fg_gc[widget->state],
                                       TRUE, x1, y1, (x2-x1), (y2-y1));
               }
          }
          gdk_gc_set_foreground(widget->style->fg_gc[widget->state], &black);
     }

     for (i=0; i < num_cols(dcal); i++)
     {
          gint x, y, w, h;
          gint j;

          dcal->dayLabelHeight = gdk_string_height(dcal->monthLabelFont, "S");

          x = dcal->leftPadding
               + (i * (col_width(dcal)+COL_BORDER_SIZE))
               + dcal->label_width;
          y = dcal->topPadding + dcal->dayLabelHeight;
          w = col_width(dcal) - COL_BORDER_SIZE - dcal->label_width - 2;
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
          maxWidth = gdk_string_width(dcal->monthLabelFont, "88");
          if (dcal->x_scale > maxWidth)
          {
               for (j=0; j<7; j++)
               {
                    gint dx = x
                         + (j * day_width(dcal))
                         + (day_width(dcal)/2)
                         - (gdk_string_width(dcal->monthLabelFont,
                                             day_label(j)) / 2);
                    gint dy = y - 2;
                    gdk_draw_string(dcal->drawbuf,
                                    dcal->monthLabelFont,
                                    widget->style->fg_gc[widget->state],
                                    dx, dy, day_label(j));
               }
          }
     }

     /* Try some pixmap copying for the month labels. */
     {
          gint i, idx;

          for (i=0; i<12; i++)
          {
               if (dcal->monthPositions[i].x == -1)
                    break;
               idx = (dcal->month - 1 + i) % 12;
               gdk_draw_drawable(GDK_DRAWABLE(dcal->drawbuf),
                                 widget->style->fg_gc[widget->state],
                                 GDK_DRAWABLE(dcal->monthLabels[idx]),
                                 0, 0,
                                 dcal->leftPadding
                                 + dcal->monthPositions[i].x,
                                 dcal->monthPositions[i].y,
                                 dcal->label_width, dcal->label_height);
          }
     }

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
               numW = gdk_string_width(dcal->dayLabelFont, dayNumBuf);
               numH = gdk_string_height(dcal->dayLabelFont, dayNumBuf);
               w = (x2 - x1)+1;
               h = (y2 - y1)+1;
               gdk_draw_string(dcal->drawbuf,
                               dcal->dayLabelFont,
                               widget->style->fg_gc[widget->state],
                               x1 + (w/2) - (numW/2),
                               y1 + (h/2) + (numH/2),
                               dayNumBuf);
          }
     }

     {
          GdkRectangle update_rect;
          update_rect.x = 0;
          update_rect.y = 0;
          update_rect.width  = widget->allocation.width;
          update_rect.height = widget->allocation.height;
          gtk_widget_draw(GTK_WIDGET(dcal), &update_rect);
     }
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
          gtk_widget_show_all(GTK_WIDGET(dcal->transPopup));
     }
     else
     {
          gtk_widget_hide(GTK_WIDGET(dcal->transPopup));
     }
     return TRUE;
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
          startWeek = g_date_get_sunday_week_of_year(start);
          endWeek = g_date_get_sunday_week_of_year(end);
          if (endWeek < startWeek)
          {
               endWeek += g_date_get_sunday_weeks_in_year(g_date_get_year(start));
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
     // FIXME: clean these up?

     /* Calculate the number of weeks in the column before the month we're
      * interested in. */
     weekRow = 0;
     if (previousMonthsInCol > 0)
     {
          g_date_set_dmy(startD, 1,
                         ((dcal->month - 1 + monthOffset) % 12) + 1,
                         dcal->year + floor((dcal->month-1+monthOffset)/12));
          /* get the week of the top of the column */
          startWk = g_date_get_sunday_week_of_year(startD);
          /* get the week of the end of the previous months */
          *endD = *startD;
          g_date_add_months(endD, previousMonthsInCol);
          g_date_subtract_days(endD, 1);
          endWk = g_date_get_sunday_week_of_year(endD);
          if (endWk < startWk)
          {
               endWk += g_date_get_sunday_weeks_in_year(g_date_get_year(startD));
          }
          /* determine how many weeks are before the month we're
           * interested in. */
          weekRow = endWk - startWk;
          if (g_date_get_weekday(endD) == G_DATE_SATURDAY)
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
          start = g_date_get_weekday(startD) % 7;
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

          weekStart = g_date_get_sunday_week_of_year(startD)+1;
          weekEnd = g_date_get_sunday_week_of_year(endD);
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
          rect = g_new0(GdkRectangle, 1);
          rect->x = dcal->leftPadding
               + MINOR_BORDER_SIZE
               + dcal->label_width
               + (colNum * (col_width(dcal) + COL_BORDER_SIZE));
          rect->y = dcal->topPadding
               + MINOR_BORDER_SIZE
               + dcal->dayLabelHeight
               + ((weekRow
                   + (g_date_get_sunday_week_of_year(endD)
                      - g_date_get_sunday_week_of_year(startD)))
                  * week_height(dcal));
          rect->width = ((g_date_get_weekday(endD) % 7)+1) * day_width(dcal);
          rect->height = week_height(dcal);

          *outList = g_list_append(*outList, (gpointer)rect);
          rect = NULL;
     }
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
     dayCol  = g_date_get_weekday(&d) % 7;
     d_week_of_cal = g_date_get_sunday_week_of_year(&d);
     g_date_set_dmy(&d, 1, dcal->month, dcal->year);
     g_date_add_months(&d, (colNum * dcal->monthsPerCol));
     top_of_col_week_of_cal = g_date_get_sunday_week_of_year(&d);
     if (d_week_of_cal < top_of_col_week_of_cal)
     {
          d_week_of_cal += g_date_get_sunday_weeks_in_year(dcal->year);
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
          /* DEBUG("x(%d) or y(%d) < 0", x, y); */
          return -1;
     }
     if ((x >= GTK_WIDGET(dcal)->allocation.width)
         || (y >= GTK_WIDGET(dcal)->allocation.height))
     {
          /*DEBUG("x(%d) > allocation.width(%d) or y(%d) > allocation->height(%d)",
            x, y,
            GTK_WIDGET(dcal)->allocation.width,
            GTK_WIDGET(dcal)->allocation.height);*/
          return -1;
     }

     /* "outside of displayed table" check */
     if (x >= (num_cols(dcal) * (col_width(dcal) + COL_BORDER_SIZE)))
     {
          /*DEBUG("x(%d) > (col_width(%d) * num_cols(%d))",
            x, col_width(dcal), num_cols(dcal));*/
          return -1;
     }
     if (y >= col_height(dcal))
     {
          /*DEBUG("y(%d) > col_height(%d)",
            y, col_height(dcal));*/
          return -1;
     }
        
     /* coords -> year-relative-values */
     colNum = floor(x / (col_width(dcal)+COL_BORDER_SIZE));
     
     x %= (col_width(dcal)+COL_BORDER_SIZE);
     x -= dcal->label_width;
     if (x < 0)
     {
          /* DEBUG("X is over the label.");*/
          return -1;
     }
     if (x >= day_width(dcal) * 7)
     {
          /*DEBUG("X is in the col_border space.");*/
          return -1;
     }

     y -= dcal->dayLabelHeight;
     if (y < 0)
     {
          /*DEBUG("Y is over the label.");*/
          return -1;
     }

     dayCol = floor((float)x / (float)day_width(dcal));
     weekRow = floor((float)y / (float)week_height(dcal));

     g_date_set_dmy(&startD, 1, dcal->month, dcal->year);
     d = startD;
     g_date_add_months(&d, (colNum * dcal->monthsPerCol));
     dayCol -= (g_date_get_weekday(&d) % 7);
     if (weekRow == 0)
     {
          if (dayCol < 0)
          {
               /*DEBUG("Before the beginning of the first month.");*/
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
               /*DEBUG("%d outside of column range [%d]",
                 g_date_get_julian(&d), g_date_get_julian(&ccd));*/
               return -1;
          }
     }

     dayOfCal = g_date_get_julian(&d) - g_date_get_julian(&startD);

     /* one more check before returning... */
     g_date_subtract_months(&d, dcal->numMonths);
     if (g_date_get_julian(&d) >= g_date_get_julian(&startD))
     {
          /* we're past the end of the displayed calendar, thus -1 */
          DEBUG("%d >= %d", g_date_get_julian(&d), g_date_get_julian(&startD));
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
     if (g_date_get_julian(d) > g_date_get_julian(&soc))
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
          return;

     dates = g_new0(GDate*, num_marks);
     for (idx = 0; idx < num_marks; idx++)
     {
          dates[idx] = g_date_new();
          gnc_dense_cal_model_get_instance(cal->model, tag, idx, dates[idx]);
     }

     gdc_mark_add(cal, tag, name, info, num_marks, dates);

     for (idx = 0; idx < num_marks; idx++)
     {
          g_date_free(dates[idx]);
     }
     g_free(dates);
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
}

static void
gdc_remove_markings(GncDenseCal *cal)
{
     GList *tags;
     tags = gnc_dense_cal_model_get_contained(cal->model);
     for (; tags != NULL; tags = tags->next)
     {
          guint tag = GPOINTER_TO_UINT(tags->data);
          gdc_mark_remove(cal, tag);
     }
}

static void
gdc_model_added_cb(GncDenseCalModel *model, guint added_tag, gpointer user_data)
{
     GncDenseCal *cal = GNC_DENSE_CAL(user_data);
     printf("gdc_model_added_cb update\n");
     gdc_add_tag_markings(cal, added_tag);
} 

static void
gdc_model_update_cb(GncDenseCalModel *model, guint update_tag, gpointer user_data)
{
     GncDenseCal *cal = GNC_DENSE_CAL(user_data);
     printf("gdc_model_update_cb update for tag [%d]\n", update_tag);
     gdc_mark_remove(cal, update_tag);
     gdc_add_tag_markings(cal, update_tag);
}

static void
gdc_model_removing_cb(GncDenseCalModel *model, guint remove_tag, gpointer user_data)
{
     GncDenseCal *cal = GNC_DENSE_CAL(user_data);
     printf("gdc_model_removing_cb update [%d]\n", remove_tag);
     gdc_mark_remove(cal, remove_tag);
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
          PERR("0 size not allowed\n");
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
     printf("saving mark with tag [%d]\n", newMark->tag);

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
     gtk_widget_queue_draw(GTK_WIDGET(dcal));
}

static void
gdc_mark_remove(GncDenseCal *dcal, guint mark_to_remove)
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
          DEBUG("couldn't find tag [%d]", mark_to_remove);
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
          dcal->marks[day_of_cal] = g_list_remove(dcal->marks[day_of_cal], mark_data);
     }
     g_list_free(mark_data->ourMarks);
     dcal->markData = g_list_remove(dcal->markData, mark_data);
     g_free(mark_data);
     gnc_dense_cal_draw_to_buffer(dcal);
     gtk_widget_queue_draw(GTK_WIDGET(dcal));
}
