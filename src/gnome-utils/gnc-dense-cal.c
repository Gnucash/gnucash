/********************************************************************\
 * gnc-dense-cal.c : a custom densely-dispalyed calendar widget     *
 * Copyright (C) 2002 Joshua Sled <jsled@asynchronous.org>          *
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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
\********************************************************************/

#include "config.h"

#include <stdio.h>
#include <math.h>
#include <time.h>
#include <string.h>
#include <gtk/gtk.h>
#include "gnc-dense-cal.h"

/**
 * Todo:
 * . marking
 *   . color-per-marker (configurable)
 *   X all-or-nothing
 * . handle errors properly
 * X mouse-over -> "hottip"
 * X rotated month labels
 * X weeksPerCol -> monthsPerCol
 **/

/**
 * Marking ...
 *
 * We want a facility to mark multiple days on the calendar.  This facility
 * should be efficient in display.  It will take an array+count of GDates on
 * which to mark the calendar.  Dates outside of the visible calendar range
 * will be ignored.

 * Markings will be manipulated in tagged sets for markings related to the
 * same event; in order to efficiently process these sets [removal,
 * primarily], we will keep a multiple data structures.
 *

 * We need to be able to perform the following actions:
 * . Add a new mark-set, returning a calendar-unique tag.
 * . Remove a mark-set by tag.
 * . Iterate over all days in the calendar, listing which markings are active
 *   on that day.

 * The markings in the calendar will be internally represented as an array of
 * GLists, with each item in the list pointing to a gdc_mark_data structure.
 * The gdc_mark_data structures will contain:
 * . the external/caller marker tag
 * . the marker indication [color, when supported]
 * . a GList of all instances of the marker in the visible calendar, by
 *   'marks' index.

 * The list of gdc_mark_data structures itself will be a top-level list in the
 * GncDenseCal structure.

 *
 **/

static const int DENSE_CAL_DEFAULT_WIDTH = 15;
static const int DENSE_CAL_DEFAULT_HEIGHT = 105;
static const int MINOR_BORDER_SIZE = 1;
static const int COL_BORDER_SIZE = 3;

static const gchar* MONTH_NAMES[] = {
        "Jan", "Feb", "Mar", "Apr", "May", "Jun",
        "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
};

static const gchar* MONTH_THIS_COLOR = "lavender";
static const gchar* MONTH_THAT_COLOR = "SlateGray1";

static const gchar* MARK_COLOR = "Yellow";
static const gchar* LABEL_FONT_NAME = "-Adobe-Helvetica-Bold-R-Normal--10-100-75-75-P-60-ISO8859-15";

static const gchar* MARKS_LOST_SIGNAL_NAME = "marks_lost";

/* SIGNALS */
enum gnc_dense_cal_signal_enum {
  MARKS_LOST_SIGNAL,
  LAST_SIGNAL
};

GtkWidget *gnc_dense_cal_new();

static gint gnc_dense_cal_signals[LAST_SIGNAL] = { 0 };

static void gnc_dense_cal_class_init (GncDenseCalClass *class);
static void gnc_dense_cal_init (GncDenseCal *dcal);
static void gnc_dense_cal_destroy (GtkObject *object);
static void gnc_dense_cal_realize (GtkWidget *widget);
static gint gnc_dense_cal_expose( GtkWidget      *widget,
                                  GdkEventExpose *event );

static void gnc_dense_cal_size_request( GtkWidget      *widget,
                                        GtkRequisition *requisition);
static void gnc_dense_cal_size_allocate( GtkWidget     *widget,
                                         GtkAllocation *allocation );
static gint gnc_dense_cal_motion_notify( GtkWidget      *widget,
                                         GdkEventMotion *event );
static gint gnc_dense_cal_button_press( GtkWidget *widget,
                                        GdkEventButton *evt );

static const inline int day_width_at( GncDenseCal *dcal, guint xScale );
static const inline int day_width( GncDenseCal *dcal );
static const inline int day_height_at( GncDenseCal *dcal, guint yScale );
static const inline int day_height( GncDenseCal *dcal );
static const inline int week_width_at( GncDenseCal *dcal, guint xScale );
static const inline int week_width( GncDenseCal *dcal );
static const inline int week_height_at( GncDenseCal *dcal, guint yScale );
static const inline int week_height( GncDenseCal *dcal );
static const inline int col_width_at( GncDenseCal *dcal, guint xScale );
static const inline int col_width( GncDenseCal *dcal );

static const inline int col_height( GncDenseCal *dcal );
static const inline int num_cols( GncDenseCal *dcal );
/**
 * Returns the total number of weeks to display in the calendar [irrespective
 * of columns/weeks-per-col].
 **/
static const inline int num_weeks( GncDenseCal *dcal );
/**
 * Returns the number of weeks per column.  Note that this is the number of
 * weeks needed to display the longest column.
 **/
static const int num_weeks_per_col( GncDenseCal *dcal );

/** hotspot calculation **/
static gint wheres_this( GncDenseCal *dcal, int x, int y );

static void recompute_x_y_scales( GncDenseCal *dcal );
static void recompute_mark_storage( GncDenseCal *dcal );
static void recompute_extents( GncDenseCal *dcal );
static void populate_hover_window( GncDenseCal *dcal, gint doc );

static void month_coords( GncDenseCal *dcal, int monthOfCal, GList **outList );
static void doc_coords( GncDenseCal *dcal, int dayOfCal,
                        int *x1, int *y1, int *x2, int *y2 );

static GtkWidgetClass *parent_class = NULL;

GtkType
gnc_dense_cal_get_type ()
{
        static GtkType dense_cal_type = 0;

        if (!dense_cal_type)
        {
                static const GtkTypeInfo dense_cal_info =
                        {
                                "GncDenseCal",
                                sizeof (GncDenseCal),
                                sizeof (GncDenseCalClass),
                                (GtkClassInitFunc) gnc_dense_cal_class_init,
                                (GtkObjectInitFunc) gnc_dense_cal_init,
                                /* reserved_1 */ NULL,
                                /* reserved_1 */ NULL,
                                (GtkClassInitFunc) NULL
                        };

                dense_cal_type =
                        gtk_type_unique (GTK_TYPE_WIDGET, &dense_cal_info);
        }

        return dense_cal_type;
}

static void
gnc_dense_cal_class_init (GncDenseCalClass *class)
{
        GtkObjectClass *object_class;
        GtkWidgetClass *widget_class;

        object_class = (GtkObjectClass*) class;
        widget_class = (GtkWidgetClass*) class;
        parent_class = gtk_type_class (gtk_widget_get_type ());

        gnc_dense_cal_signals[MARKS_LOST_SIGNAL] =
                gtk_signal_new( MARKS_LOST_SIGNAL_NAME,
                                GTK_RUN_FIRST,
                                object_class->type,
                                GTK_SIGNAL_OFFSET( GncDenseCalClass, marks_lost_cb ),
                                gtk_signal_default_marshaller, GTK_TYPE_NONE, 0 );
        gtk_object_class_add_signals (object_class, gnc_dense_cal_signals, LAST_SIGNAL);

        object_class->destroy = gnc_dense_cal_destroy;
        widget_class->realize = gnc_dense_cal_realize;
        widget_class->expose_event = gnc_dense_cal_expose;
        widget_class->size_request = gnc_dense_cal_size_request;
        widget_class->size_allocate = gnc_dense_cal_size_allocate;
        widget_class->motion_notify_event = gnc_dense_cal_motion_notify;
        widget_class->button_press_event = gnc_dense_cal_button_press;
}

static void
gnc_dense_cal_init (GncDenseCal *dcal)
{
        gboolean colorAllocSuccess;

        dcal->initialized = FALSE;
        dcal->markData = NULL;
        dcal->numMarks = 0;
        dcal->marks = NULL;
        dcal->lastMarkTag = 0;

        dcal->showPopup = FALSE;
        gtk_widget_set_events( GTK_WIDGET(dcal),
                               GDK_EXPOSURE_MASK
                               | GDK_BUTTON_RELEASE_MASK
                               | GDK_POINTER_MOTION_MASK
                               | GDK_POINTER_MOTION_HINT_MASK );

        dcal->transPopup = GTK_WINDOW( gtk_window_new( GTK_WINDOW_POPUP ) );
        {
                GtkWidget *vbox, *hbox;
                GtkWidget *l;
                GtkCList *cl;
                static const gchar *CLIST_TITLES[] = {
                        "Name", "Info"
                };

                vbox = gtk_vbox_new( FALSE, 5 );
                hbox = gtk_hbox_new( FALSE, 5 );

                l = gtk_label_new( "Date: " );
                gtk_container_add( GTK_CONTAINER(hbox), l );
                l = gtk_label_new( "YY/MM/DD" );
                gtk_object_set_data( GTK_OBJECT(dcal->transPopup),
                                     "dateLabel", (gpointer)l );
                gtk_container_add( GTK_CONTAINER(hbox), l );
                gtk_container_add( GTK_CONTAINER(vbox), hbox );

                gtk_container_add( GTK_CONTAINER(vbox), gtk_hseparator_new() );

                cl = GTK_CLIST(gtk_clist_new_with_titles(2, (gchar**)CLIST_TITLES));
                gtk_clist_set_column_auto_resize( cl, 0, TRUE );
                gtk_clist_set_column_auto_resize( cl, 1, TRUE );
                gtk_object_set_data( GTK_OBJECT(dcal->transPopup),
                                     "clist", (gpointer)cl );
                gtk_container_add( GTK_CONTAINER(vbox), GTK_WIDGET(cl) );

                gtk_container_add( GTK_CONTAINER(dcal->transPopup), vbox );

                gtk_widget_realize( GTK_WIDGET(dcal->transPopup) );
        }

        gdk_color_parse( MONTH_THIS_COLOR,  &dcal->weekColors[MONTH_THIS] );
        gdk_color_parse( MONTH_THAT_COLOR,  &dcal->weekColors[MONTH_THAT] );
        if ( gdk_colormap_alloc_colors( gdk_colormap_get_system(),
                                        dcal->weekColors,
                                        MAX_COLORS, TRUE, TRUE,
                                        &colorAllocSuccess ) > 0 ) {
                /* FIXME : handle properly */
                printf( "Error allocating colors\n" );
        }

        /* Deal with the various label sizes. */
        {
                gint i;
                guint maxWidth, maxHeight, maxAscent, maxLBearing;
                gint lbearing, rbearing, width, ascent, descent;

                dcal->monthLabelFont = gdk_font_load( LABEL_FONT_NAME );
                if ( dcal->monthLabelFont == NULL ) {
                        printf( "Couldn't load font \"%s\"\n", LABEL_FONT_NAME );
                        gtk_main_quit();
                }

                dcal->dayLabelFont = GTK_WIDGET(dcal)->style->font;
                if ( dcal->dayLabelFont == NULL ) {
                        printf( "Couldn't get day-label font from widget->style\n" );
                        gtk_main_quit();
                }

                maxWidth = maxHeight = maxAscent = maxLBearing = 0;
                for ( i=0; i<12; i++ ) {
                        gint w, h;
                        gdk_string_extents( dcal->monthLabelFont, MONTH_NAMES[i],
                                            &lbearing, &rbearing, &width,
                                            &ascent, &descent );
                        w = rbearing - lbearing + 1;
                        h = ascent + descent;
                        maxLBearing = MAX( maxLBearing, ABS(lbearing) );
                        maxWidth = MAX( maxWidth, w );
                        maxHeight = MAX( maxHeight, h );
                        maxAscent = MAX( maxAscent, ascent );
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
                GDate *tmpDate;

                tmpDate = g_date_new();
                g_date_set_time( tmpDate, time(NULL) );
                gnc_dense_cal_set_month( dcal, g_date_month(tmpDate) );
                gnc_dense_cal_set_year( dcal, g_date_year(tmpDate) );
                g_date_free( tmpDate );
        }

        recompute_extents( dcal );
        recompute_mark_storage( dcal );

        /* Now that we're "sure" of our configuration, compute initial
         * scaling factors; will be increased when we're allocated enough
         * space to scale up. */
        dcal->min_x_scale = dcal->x_scale =
                MAX( gdk_string_width( dcal->monthLabelFont, "88" ),
                     gdk_string_width( dcal->dayLabelFont, "88" ) + 2 );
        dcal->min_y_scale = dcal->y_scale =
                MAX( floor( (float)gdk_string_width( dcal->monthLabelFont,
                                                     "XXX" )
                            / 3.0 ),
                     gdk_string_height( dcal->dayLabelFont, "88" ) + 2 );
        dcal->dayLabelHeight = gdk_string_height( dcal->monthLabelFont, "88" );
        
        dcal->initialized = TRUE;
}

GtkWidget*
gnc_dense_cal_new()
{
        GncDenseCal *dcal;
        dcal = gtk_type_new (gnc_dense_cal_get_type ());

        return GTK_WIDGET (dcal);
}

static void
recompute_first_of_month_offset( GncDenseCal *dcal )
{
        GDate *tmpDate;

        tmpDate = g_date_new_dmy( 1, dcal->month, dcal->year );
        dcal->firstOfMonthOffset = g_date_weekday( tmpDate ) % 7;
        g_date_free( tmpDate );
}

void
gnc_dense_cal_set_month( GncDenseCal *dcal, GDateMonth mon )
{
        dcal->month = mon;
        recompute_first_of_month_offset( dcal );
        recompute_extents( dcal );
        if ( GTK_WIDGET_REALIZED( dcal ) ) {
                recompute_x_y_scales( dcal );
        }
        gtk_widget_queue_draw( GTK_WIDGET(dcal) );
}

void
gnc_dense_cal_set_year( GncDenseCal *dcal, guint year )
{
        dcal->year = year;
        recompute_first_of_month_offset( dcal );
        recompute_extents( dcal );
        if ( GTK_WIDGET_REALIZED( dcal ) ) {
                recompute_x_y_scales( dcal );
                gtk_widget_queue_draw( GTK_WIDGET(dcal) );
        }
}

void
gnc_dense_cal_set_num_months( GncDenseCal *dcal, guint num_months )
{
        dcal->numMonths = num_months;
        recompute_extents( dcal );
        recompute_mark_storage( dcal );
        if ( GTK_WIDGET_REALIZED( dcal ) ) {
                recompute_x_y_scales( dcal );
                gtk_widget_queue_draw( GTK_WIDGET(dcal) );
        }
}

void
gnc_dense_cal_set_months_per_col( GncDenseCal *dcal, guint monthsPerCol )
{
        dcal->monthsPerCol = monthsPerCol;
        recompute_x_y_scales(dcal);
}

guint
gnc_dense_cal_get_num_months( GncDenseCal *dcal )
{
        return dcal->numMonths;
}

GDateMonth
gnc_dense_cal_get_month( GncDenseCal *dcal )
{
        return dcal->month;
}

GDateYear
gnc_dense_cal_get_year( GncDenseCal *dcal )
{
        return dcal->year;
}

static void
gnc_dense_cal_destroy (GtkObject *object)
{
        g_return_if_fail (object != NULL);
        g_return_if_fail (GNC_IS_DENSE_CAL (object));

        if (GTK_OBJECT_CLASS (parent_class)->destroy)
                (* GTK_OBJECT_CLASS (parent_class)->destroy) (object);
}

static void
gnc_dense_cal_realize (GtkWidget *widget)
{
        GncDenseCal *dcal;
        GdkWindowAttr attributes;
        gint attributes_mask;

        g_return_if_fail (widget != NULL);
        g_return_if_fail (GNC_IS_DENSE_CAL (widget));

        GTK_WIDGET_SET_FLAGS (widget, GTK_REALIZED);
        dcal = GNC_DENSE_CAL (widget);

        attributes.x = widget->allocation.x;
        attributes.y = widget->allocation.y;
        attributes.width = widget->allocation.width;
        attributes.height = widget->allocation.height;
        attributes.wclass = GDK_INPUT_OUTPUT;
        attributes.window_type = GDK_WINDOW_CHILD;
        attributes.event_mask = gtk_widget_get_events (widget) | 
                GDK_EXPOSURE_MASK | GDK_BUTTON_PRESS_MASK | 
                GDK_BUTTON_RELEASE_MASK | GDK_POINTER_MOTION_MASK |
                GDK_POINTER_MOTION_HINT_MASK;
        attributes.visual = gtk_widget_get_visual (widget);
        attributes.colormap = gtk_widget_get_colormap (widget);

        attributes_mask = GDK_WA_X | GDK_WA_Y | GDK_WA_VISUAL | GDK_WA_COLORMAP;
        widget->window = gdk_window_new (widget->parent->window, &attributes, attributes_mask);

        widget->style = gtk_style_attach (widget->style, widget->window);

        gdk_window_set_user_data (widget->window, widget);

        gtk_style_set_background (widget->style, widget->window, GTK_STATE_ACTIVE);
}

static void 
gnc_dense_cal_size_request( GtkWidget      *widget,
                            GtkRequisition *requisition )
{
        GncDenseCal *dcal = GNC_DENSE_CAL(widget);
        if ( !dcal->initialized ) {
                printf( "Uninitialized size request\n" );
                requisition->width  = DENSE_CAL_DEFAULT_WIDTH;
                requisition->height = DENSE_CAL_DEFAULT_HEIGHT;
                return;
        }
        requisition->width =
                (dcal->leftPadding * 2)
                + (num_cols(dcal) * (col_width_at(dcal, dcal->min_x_scale)
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
recompute_x_y_scales( GncDenseCal *dcal )
{
        GtkWidget *widget;
        int denom;
        int width, height;

        widget = GTK_WIDGET(dcal);

        width = DENSE_CAL_DEFAULT_WIDTH;
        height = DENSE_CAL_DEFAULT_HEIGHT;
        if ( dcal->initialized ) {
                width  = widget->allocation.width;
                height = widget->allocation.height;
        }

        /* FIXME: there's something slightly wrong in the x_scale computation that
         * lets us draw larger than our area. */
        denom = 7 * num_cols(dcal);
        g_assert( denom != 0 );
        dcal->x_scale = (gint)((width
                                - (dcal->leftPadding * 2)
                                - (num_cols(dcal) * ( (8 * MINOR_BORDER_SIZE)
                                                      + dcal->label_width ))
                                - ((num_cols(dcal)-1) * COL_BORDER_SIZE))
                               / denom);
        dcal->x_scale = MAX( dcal->x_scale, dcal->min_x_scale );

        denom = num_weeks_per_col(dcal);
        g_assert( denom != 0 );
        dcal->y_scale = (gint)((height
                                - (dcal->topPadding * 2)
                                - MINOR_BORDER_SIZE
                                - dcal->dayLabelHeight
                                - (num_weeks_per_col(dcal)-1
                                   * MINOR_BORDER_SIZE))
                               / denom );
        dcal->y_scale = MAX( dcal->y_scale, dcal->min_y_scale );
}

static void
recompute_mark_storage( GncDenseCal *dcal )
{
        gint i;
        GList *l;
        int oldNumMarks;

        oldNumMarks = dcal->numMarks;
        dcal->numMarks = num_weeks(dcal) * 7;
        if ( dcal->marks == NULL ) {
                goto createNew;
        }

        dcal->markData = NULL;
        for ( i=0; i<oldNumMarks; i++ ) {
                /* Each of these just contains an elt of dcal->markData,
                 * which we're about to free... */
                g_list_free( dcal->marks[i] );
        }
        g_free( dcal->marks );
        dcal->marks = NULL;
        /* Remove the old mark data. */
        for ( l = dcal->markData; l; l = l->next ) {
                g_list_free( ((gdc_mark_data*)l->data)->ourMarks );
                g_free( (gdc_mark_data*)l->data );
        }
        dcal->markData = NULL;

 createNew:
        dcal->marks = g_new0( GList*, dcal->numMarks );
        gtk_signal_emit_by_name( GTK_OBJECT(dcal), MARKS_LOST_SIGNAL_NAME, NULL );
}

static void
recompute_extents( GncDenseCal *dcal )
{
        GDate date;
        gint start_week, end_week;

        g_date_clear( &date, 1 );
        g_date_set_dmy( &date, 1, dcal->month, dcal->year );
        start_week = g_date_sunday_week_of_year(&date);
        g_date_add_months( &date, dcal->numMonths );
        end_week = g_date_sunday_week_of_year(&date);
        if ( g_date_year(&date) != dcal->year ) {
                end_week += g_date_sunday_weeks_in_year( dcal->year );
        }
        dcal->num_weeks = end_week - start_week + 1;
}

static void
gnc_dense_cal_size_allocate( GtkWidget     *widget,
                             GtkAllocation *allocation )
{
        GncDenseCal *dcal;

        g_return_if_fail (widget != NULL);
        g_return_if_fail (GNC_IS_DENSE_CAL (widget));
        g_return_if_fail (allocation != NULL);

        dcal = GNC_DENSE_CAL(widget);

        widget->allocation = *allocation;

        if (GTK_WIDGET_REALIZED (widget)) {
                gdk_window_move_resize (widget->window,
                                        allocation->x, allocation->y,
                                        allocation->width,
                                        allocation->height);
                /* We want to know how many px we can increase every day
                 * [width] or week [height]. */
                recompute_x_y_scales( dcal );
        }
}

static void
free_rect( gpointer data, gpointer ud )
{
        g_free( (GdkRectangle*)data );
}

static gint
gnc_dense_cal_expose( GtkWidget *widget,
                      GdkEventExpose *event )
{
        GncDenseCal *dcal;
        gint i;
        int maxWidth;

        g_return_val_if_fail (widget != NULL, FALSE);
        g_return_val_if_fail (GNC_IS_DENSE_CAL (widget), FALSE);
        g_return_val_if_fail (event != NULL, FALSE);

        if (event->count > 0)
                return FALSE;
  
        dcal = GNC_DENSE_CAL (widget);

        gdk_window_clear_area (widget->window,
                               0, 0,
                               widget->allocation.width,
                               widget->allocation.height);

        if ( dcal->needInitMonthLabels ) {
                /* Create the month labels */
                gint i;
                GdkPixmap *tmpPix;
                GdkImage *tmpImg;
                GdkGC *gc;
                GdkColor black;

                gc = widget->style->fg_gc[widget->state];
                tmpPix = gdk_pixmap_new( widget->window,
                                         dcal->label_height,
                                         dcal->label_width, -1 );
                black.pixel = gdk_rgb_xpixel_from_rgb(0);
                for ( i=0; i<12; i++ ) {
                        gint x,y;
                        /* these are going to be rotated, so transpose width
                         * and height */
                        dcal->monthLabels[i] =
                                gdk_pixmap_new( widget->window,
                                                dcal->label_width,
                                                dcal->label_height, -1 );
                        gdk_draw_rectangle( dcal->monthLabels[i],
                                            widget->style->bg_gc[widget->state],
                                            TRUE, 0, 0,
                                            dcal->label_width,
                                            dcal->label_height );

                        gdk_draw_rectangle( tmpPix,
                                            widget->style->bg_gc[widget->state],
                                            TRUE, 0, 0,
                                            dcal->label_height,
                                            dcal->label_width );

                        gdk_draw_string( tmpPix, dcal->monthLabelFont, gc,
                                         dcal->label_lbearing,
                                         dcal->label_ascent,
                                         MONTH_NAMES[i] );

                        tmpImg = gdk_image_get( tmpPix, 0, 0,
                                                dcal->label_height,
                                                dcal->label_width );
                        /* now, (transpose the pixel matrix)==(do a 90-degree
                         * counter-clocwise rotation) */
                        for ( x=0; x<dcal->label_height; x++ ) {
                                for ( y=0; y<dcal->label_width; y++ ) {
                                        if ( gdk_image_get_pixel( tmpImg, x, y )
                                             != black.pixel ) {
                                                continue;
                                        }
                                        gdk_draw_point( dcal->monthLabels[i],
                                                        gc, y,
                                                        dcal->label_height - x );
                                }
                        }
                        gdk_image_destroy( tmpImg );
                }
                dcal->needInitMonthLabels = FALSE;
        }

        /* Fill in alternating month colors. */
        {
                gint i;
                GdkGC *gc;
                GdkRectangle *rect;
                GList *mcList, *mcListIter;

                gc = gdk_gc_new( widget->window );
                gdk_gc_copy( gc, widget->style->fg_gc[widget->state] );

                /* reset all of the month position offsets. */
                for ( i=0; i<12; i++ ) {
                        dcal->monthPositions[i].x = dcal->monthPositions[i].y = -1;
                }

                /* Paint the weeks for the upcoming N months. */
                for ( i=0; i < dcal->numMonths; i++ ) {
                        gdk_gc_set_foreground( gc, &dcal->weekColors[ i % 2 ] );

                        mcList = NULL;
                        month_coords( dcal, i, &mcList );
                        dcal->monthPositions[i].x = 
                                floor(i/dcal->monthsPerCol)
                                * (col_width(dcal) + COL_BORDER_SIZE);
                        dcal->monthPositions[i].y = ((GdkRectangle*)mcList->next->data)->y;
                        for ( mcListIter = mcList; mcListIter;
                              mcListIter = mcListIter->next ) {
                                rect = (GdkRectangle*)mcListIter->data;
                                gdk_draw_rectangle( widget->window, gc,
                                                    TRUE, rect->x, rect->y,
                                                    rect->width - 2, rect->height );
                        }
                        g_list_foreach( mcList, free_rect, NULL );
                        g_list_free( mcList );
                }

                gdk_gc_destroy( gc );
        }

        /* Hilight the marked days. */
        {
                int i;
                int x1, x2, y1, y2;
                GdkColor markColor, black;
                GList *l;
                gdc_mark_data *gdcmd;

                gdk_color_parse( MARK_COLOR, &markColor );
                gdk_colormap_alloc_color( gdk_colormap_get_system(), &markColor, TRUE, TRUE );
                gdk_color_black( gdk_colormap_get_system(), &black );

                /* FIXME: use a different GC for this */
                gdk_gc_set_foreground( widget->style->fg_gc[widget->state], &markColor );
                for ( i=0; i<num_weeks(dcal)*7; i++ ) {
                        l = dcal->marks[i];
                        for ( ; l ; l = l->next ) {
                                gdcmd = (gdc_mark_data*)l->data;
                                doc_coords( dcal, i, &x1, &y1, &x2, &y2 );
                                gdk_draw_rectangle( widget->window,
                                                    widget->style->fg_gc[widget->state],
                                                    TRUE, x1, y1, (x2-x1), (y2-y1) );
                        }
                }
                gdk_gc_set_foreground( widget->style->fg_gc[widget->state], &black );
        }

        for ( i=0; i<num_cols(dcal); i++ ) {
                gint x, y, w, h;
                gint j;

                dcal->dayLabelHeight = gdk_string_height( dcal->monthLabelFont, "S" );

                x = dcal->leftPadding
                        + ( i * (col_width(dcal)+COL_BORDER_SIZE) )
                        + dcal->label_width;
                y = dcal->topPadding + dcal->dayLabelHeight;
                w = col_width(dcal) - COL_BORDER_SIZE - dcal->label_width - 2;
                h = col_height(dcal);

                /* draw the outside border [inside the month labels] */
                gdk_draw_rectangle( widget->window,
                                    widget->style->fg_gc[widget->state],
                                    FALSE, x, y, w, h );
                /* draw the week seperations */
                {
                        for ( j=0; j < num_weeks_per_col(dcal); j++ ) {
                                gint wy = y + (j * week_height(dcal));
                                gdk_draw_line( widget->window,
                                               widget->style->fg_gc[widget->state],
                                               x,     wy,
                                               x + w, wy );
                        }
                }
                /* draw the day seperations */
                {
                        for ( j=1; j<7; j++ ) {
                                gint dx = x + (j * day_width(dcal));
                                gdk_draw_line( widget->window,
                                               widget->style->fg_gc[widget->state],
                                               dx, y,
                                               dx, y + col_height(dcal) );
                        }
                }
                /* draw the day labels */
                maxWidth = gdk_string_width( dcal->monthLabelFont, "88" );
                if ( dcal->x_scale > maxWidth ) {
                        static const gchar *dayLabels[7] = {
                                "Su", "M", "Tu", "W", "Th", "F", "Sa"
                        };
                        for ( j=0; j<7; j++ ) {
                                gint dx = x
                                        + (j * day_width(dcal))
                                        + (day_width(dcal)/2)
                                        - ( gdk_string_width(dcal->monthLabelFont,
                                                             dayLabels[j]) / 2 );
                                gint dy = y - 2;
                                gdk_draw_string( widget->window,
                                                 dcal->monthLabelFont,
                                                 widget->style->fg_gc[widget->state],
                                                 dx, dy, dayLabels[j] );
                        }
                }
        }

  /* Try some pixmap copying for the month labels. */
  {
          gint i, idx;

          for ( i=0; i<12; i++ ) {
                  if ( dcal->monthPositions[i].x == -1 ) {
                          break;
                  }
                  idx = (dcal->month - 1 + i) % 12;
                  gdk_draw_pixmap( widget->window,
                                   widget->style->fg_gc[widget->state],
                                   dcal->monthLabels[idx],
                                   0, 0,
                                   dcal->leftPadding
                                   + dcal->monthPositions[i].x,
                                   dcal->monthPositions[i].y,
                                   dcal->label_width, dcal->label_height );
          }
  }

  /* Try the per-day strings [dates] */
  {
          GDate d, eoc;
          gint doc;
          gchar dayNumBuf[3];
          gint numW, numH;
          gint x1, y1, x2, y2, w, h;

          g_date_set_dmy( &d, 1, dcal->month, dcal->year );
          eoc = d;
          g_date_add_months( &eoc, dcal->numMonths );
          for ( doc = 0; g_date_julian(&d) < g_date_julian(&eoc);
                g_date_add_days( &d, 1 ), doc++ ) {
                  doc_coords( dcal, doc, &x1, &y1, &x2, &y2 );
                  memset( dayNumBuf, 0, 3 );
                  sprintf( dayNumBuf, "%d", g_date_day( &d ) );
                  numW = gdk_string_width( dcal->dayLabelFont, dayNumBuf );
                  numH = gdk_string_height( dcal->dayLabelFont, dayNumBuf );
                  w = (x2 - x1)+1;
                  h = (y2 - y1)+1;
                  gdk_draw_string( widget->window,
                                   dcal->dayLabelFont,
                                   widget->style->fg_gc[widget->state],
                                   x1 + (w/2) - (numW/2),
                                   y1 + (h/2) + (numH/2),
                                   dayNumBuf );
          }
  }

  return FALSE;
}

static void
populate_hover_window( GncDenseCal *dcal, gint doc )
{
        GtkWidget *w;
        GDate *date;
        static const int MAX_STRFTIME_BUF_LEN = 64;
        gchar strftimeBuf[MAX_STRFTIME_BUF_LEN];

        if ( doc >= 0 ) {
                GtkObject *o;
                GtkCList *cl;
                GList *l;
                gchar *rowText[2];
                gint row = 0;
                gdc_mark_data *gdcmd;

                w = GTK_WIDGET( gtk_object_get_data( GTK_OBJECT(dcal->transPopup),
                                                     "dateLabel" ) );
                date = g_date_new_dmy( 1, dcal->month, dcal->year );
                g_date_add_days( date, doc );
                g_date_strftime( strftimeBuf, MAX_STRFTIME_BUF_LEN-1, "%Y-%m-%d", date );
                gtk_label_set_text( GTK_LABEL(w), strftimeBuf );

                o = GTK_OBJECT(dcal->transPopup);
                cl = GTK_CLIST( gtk_object_get_data(o, "clist" ) );
                gtk_clist_clear( cl );
                for ( l = dcal->marks[doc]; l; l = l->next ) {
                        gdcmd = (gdc_mark_data*)l->data;
                        rowText[0] = gdcmd->name;
                        rowText[1] = gdcmd->info;
                        gtk_clist_insert( cl, row++, rowText );
                }
        }
}

static gint
gnc_dense_cal_button_press( GtkWidget *widget,
                            GdkEventButton *evt )
{
        gint doc;
        GncDenseCal *dcal = GNC_DENSE_CAL(widget);

        doc = wheres_this( dcal, evt->x, evt->y );
        dcal->showPopup = ~(dcal->showPopup);
        if ( dcal->showPopup && doc >= 0 ) {
                gdk_window_move( GTK_WIDGET(dcal->transPopup)->window,
                                 evt->x_root+1, evt->y_root+1 );
                populate_hover_window( dcal, doc );
                gtk_widget_show_all( GTK_WIDGET(dcal->transPopup) );
        } else {
                gtk_widget_hide( GTK_WIDGET(dcal->transPopup) );
        }
        return FALSE;
}

static gint
gnc_dense_cal_motion_notify( GtkWidget      *widget,
                             GdkEventMotion *event )
{
        GncDenseCal *dcal;
        gint doc;
        int unused;
        int x_root_offset, y_root_offset;
        GdkModifierType unused2;

        dcal = GNC_DENSE_CAL(widget);
        if ( ! dcal->showPopup )
                return FALSE;

        x_root_offset = event->x_root;
        y_root_offset = event->y_root;

        /* As per http://www.gtk.org/tutorial/sec-eventhandling.html */
        if ( event->is_hint ) {
                gdk_window_get_pointer( event->window, &unused, &unused, &unused2 );
        }
        gdk_window_move( GTK_WIDGET(dcal->transPopup)->window,
                         x_root_offset+1, y_root_offset+1 );
        doc = wheres_this( dcal, event->x, event->y );
        if ( doc >= 0 ) {
                populate_hover_window( dcal, doc );
                gtk_widget_show_all( GTK_WIDGET(dcal->transPopup) );
        } else {
                gtk_widget_hide( GTK_WIDGET(dcal->transPopup) );
        }
        return TRUE;
}

static const inline int
day_width_at( GncDenseCal *dcal, guint xScale )
{
        return xScale + MINOR_BORDER_SIZE;
}

static const inline int
day_width( GncDenseCal *dcal )
{
        return day_width_at( dcal, dcal->x_scale );
}

static const inline int
day_height_at( GncDenseCal *dcal, guint yScale )
{
        return yScale + MINOR_BORDER_SIZE;
}

static const inline int
day_height( GncDenseCal *dcal )
{
        return day_height_at( dcal, dcal->y_scale );
}

static const inline int
week_width_at( GncDenseCal *dcal, guint xScale )
{
        return day_width_at(dcal, xScale) * 7;
}

static const inline int
week_width( GncDenseCal *dcal )
{
        return week_width_at( dcal, dcal->x_scale );
}

static const inline int
week_height_at( GncDenseCal *dcal, guint yScale )
{
        return day_height_at(dcal, yScale);
}

static const inline int
week_height( GncDenseCal *dcal )
{
        return week_height_at(dcal, dcal->y_scale);
}

static const inline int
col_width_at( GncDenseCal *dcal, guint xScale )
{
        return (week_width_at(dcal, xScale)
                + dcal->label_width
                + COL_BORDER_SIZE);
}

static const inline int
col_width( GncDenseCal *dcal )
{
        return col_width_at( dcal, dcal->x_scale );
}

static const inline int
col_height( GncDenseCal *dcal )
{
        return week_height(dcal)
                * num_weeks_per_col(dcal);
}

static const inline int
num_cols( GncDenseCal *dcal )
{
        return ceil( (float)dcal->numMonths / (float)dcal->monthsPerCol );
}

static const inline int
num_weeks( GncDenseCal *dcal )
{
        /* FIXME: calculate, remove 'recompute_extents' */
        return dcal->num_weeks;
}

static const
int num_weeks_per_col( GncDenseCal *dcal )
{
        int num_weeks_toRet, numCols, i;
        GDate *start,*end;
        int startWeek, endWeek;

        start = g_date_new();
        end = g_date_new();

        num_weeks_toRet = 0;
        numCols = num_cols(dcal);

        for ( i=0; i<numCols; i++ ) {
                g_date_set_dmy( start, 1,
                                ((dcal->month - 1 +
                                  (i * dcal->monthsPerCol)) % 12)
                                + 1,
                                dcal->year + floor((dcal->month - 1
                                                    + (i*dcal->monthsPerCol))
                                                   / 12) );
                *end = *start;
                /* Add the smaller of (the number of months in the
                 * calendar-display, minus the number of months shown in the
                 * previous columns) or (the number of months in a column) */
                g_date_add_months( end, MIN( dcal->numMonths,
                                             MIN( dcal->monthsPerCol,
                                                  dcal->numMonths
                                                  - ((i-1)
                                                     * dcal->monthsPerCol) ) ) );
                g_date_subtract_days( end, 1 );
                startWeek = g_date_sunday_week_of_year( start );
                endWeek = g_date_sunday_week_of_year( end );
                if ( endWeek < startWeek ) {
                        endWeek += g_date_sunday_weeks_in_year( g_date_year(start) );
                }
                num_weeks_toRet = MAX( num_weeks_toRet, (endWeek - startWeek)+1 );
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
month_coords( GncDenseCal *dcal, int monthOfCal, GList **outList )
{
        gint weekRow, colNum, previousMonthsInCol, monthOffset;
        gint start;
        GDate *startD, *endD;
        GdkRectangle *rect;
        gint startWk, endWk;

        if ( monthOfCal > dcal->numMonths ) {
                return;
        }
        colNum = floor(monthOfCal / dcal->monthsPerCol);
        monthOffset = colNum * dcal->monthsPerCol;
        previousMonthsInCol = MAX( 0, (monthOfCal % dcal->monthsPerCol) );

        startD = g_date_new();
        endD = g_date_new();

        /* Calculate the number of weeks in the column before the month we're
         * interested in. */
        weekRow = 0;
        if ( previousMonthsInCol > 0 ) {
                g_date_set_dmy( startD, 1,
                                ((dcal->month - 1 + monthOffset) % 12) + 1,
                                dcal->year + floor((dcal->month-1+monthOffset)/12) );
                // get the week of the top of the column
                startWk = g_date_sunday_week_of_year( startD );
                // get the week of the end of the previous months
                *endD = *startD;
                g_date_add_months( endD, previousMonthsInCol );
                g_date_subtract_days( endD, 1 );
                endWk = g_date_sunday_week_of_year( endD );
                if ( endWk < startWk ) {
                        endWk += g_date_sunday_weeks_in_year( g_date_year(startD) );
                }
                // determine how many weeks are before the month we're interested in.
                weekRow = endWk - startWk;
                if ( g_date_weekday(endD) == G_DATE_SATURDAY ) {
                        weekRow++;
                }
        }

        g_date_set_dmy( startD, 1,
                        ((dcal->month - 1 + monthOfCal) % 12) + 1,
                        dcal->year + floor((dcal->month-1+monthOfCal)/12) );
        *endD = *startD;
        g_date_add_months( endD, 1 );
        g_date_subtract_days( endD, 1 );
        /* Get the first week. */
        {
                start = g_date_weekday( startD ) % 7;
                rect = g_new0( GdkRectangle, 1 );
                rect->x = dcal->leftPadding
                        + MINOR_BORDER_SIZE
                        + (colNum * (col_width(dcal) + COL_BORDER_SIZE))
                        + dcal->label_width
                        + (start * day_width(dcal));
                rect->y = dcal->topPadding
                        + dcal->dayLabelHeight
                        + MINOR_BORDER_SIZE
                        + (weekRow * week_height(dcal) );
                rect->width = (7 - start) * day_width(dcal);
                rect->height = week_height(dcal);
                *outList = g_list_append(*outList, (gpointer)rect );
                rect = NULL;
        }

        /* Get the middle weeks. */
        {
                gint i, weekStart, weekEnd;

                weekStart = g_date_sunday_week_of_year(startD)+1;
                weekEnd = g_date_sunday_week_of_year(endD);
                for ( i=weekStart; i<weekEnd; i++ ) {
                        rect = g_new0( GdkRectangle, 1 );
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

                        *outList = g_list_append( *outList, (gpointer)rect );
                        rect = NULL;
                }
        }
        
        /* Get the last week. */
        {
                rect = g_new0( GdkRectangle, 1 );
                rect->x = dcal->leftPadding
                        + MINOR_BORDER_SIZE
                        + dcal->label_width
                        + (colNum * (col_width(dcal) + COL_BORDER_SIZE));
                rect->y = dcal->topPadding
                        + MINOR_BORDER_SIZE
                        + dcal->dayLabelHeight
                        + ((weekRow
                            + (g_date_sunday_week_of_year(endD)
                               - g_date_sunday_week_of_year(startD)))
                           * week_height(dcal));
                rect->width = ((g_date_weekday(endD) % 7)+1) * day_width(dcal);
                rect->height = week_height(dcal);

                *outList = g_list_append( *outList, (gpointer)rect );
                rect = NULL;
        }
}

/* FIXME: make this more like month_coords */
static void
doc_coords( GncDenseCal *dcal, int dayOfCal,
            int *x1, int *y1, int *x2, int *y2 )
{
        GDate d;
        gint docMonth;
        gint d_week_of_cal, top_of_col_week_of_cal;
        gint colNum, dayCol, weekRow;

        /* FIXME: add range checks */
        g_date_set_dmy( &d, 1, dcal->month, dcal->year );
        g_date_add_days( &d, dayOfCal );
        docMonth = g_date_month( &d );
        if ( g_date_year( &d ) != dcal->year ) {
                docMonth += 12;
        }
        colNum  = floor( (float)(docMonth - dcal->month) / (float)dcal->monthsPerCol );
        dayCol  = g_date_weekday( &d ) % 7;
        d_week_of_cal = g_date_sunday_week_of_year( &d );
        g_date_set_dmy( &d, 1, dcal->month, dcal->year );
        g_date_add_months( &d, (colNum * dcal->monthsPerCol) );
        top_of_col_week_of_cal = g_date_sunday_week_of_year( &d );
        if ( d_week_of_cal < top_of_col_week_of_cal ) {
                d_week_of_cal +=
                        g_date_sunday_weeks_in_year( dcal->year );
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
wheres_this( GncDenseCal *dcal, int x, int y )
{
        gint colNum, weekRow, dayCol, dayOfCal;
        GDate d, startD;

        x -= dcal->leftPadding;
        y -= dcal->topPadding;

        if ( (x < 0) || (y < 0) ) {
                printf( "x(%d) or y(%d) < 0\n", x, y );
                return -1;
        }
        if ( (x >= GTK_WIDGET(dcal)->allocation.width)
             || (y >= GTK_WIDGET(dcal)->allocation.height) ) {
                printf( "x(%d) > allocation.width(%d) or y(%d) > allocation->height(%d)\n",
                        x, y,
                        GTK_WIDGET(dcal)->allocation.width,
                        GTK_WIDGET(dcal)->allocation.height );
                return -1;
        }

        /* "outside of displayed table" check */
        if ( x >= (num_cols(dcal) * (col_width(dcal) + COL_BORDER_SIZE)) ) {
                printf( "x(%d) > ( col_width(%d) * num_cols(%d) )\n",
                        x, col_width(dcal), num_cols(dcal) );
                return -1;
        }
        if ( y >= col_height(dcal) ) {
                printf( "y(%d) > col_height(%d)\n",
                        y, col_height(dcal) );
                return -1;
        }
        
        /* coords -> year-relative-values */
        colNum = floor( x / (col_width(dcal)+COL_BORDER_SIZE) );

        x %= (col_width(dcal)+COL_BORDER_SIZE);
        x -= dcal->label_width;
        if ( x < 0 ) {
                printf( "X is over the label.\n" );
                return -1;
        }
        if ( x >= day_width(dcal) * 7 ) {
                printf( "X is in the col_border space.\n" );
                return -1;
        }

        y -= dcal->dayLabelHeight;
        if ( y < 0 ) {
                printf( "Y is over the label.\n" );
                return -1;
        }

        dayCol = floor( (float)x / (float)day_width(dcal) );
        weekRow = floor( (float)y / (float)week_height(dcal) );

        g_date_set_dmy( &startD, 1, dcal->month, dcal->year );
        d = startD;
        g_date_add_months( &d, (colNum * dcal->monthsPerCol) );
        dayCol -= (g_date_weekday(&d) % 7);
        if ( weekRow == 0 ) {
                if ( dayCol < 0 ) {
                        printf( "Before the beginning of the first month.\n" );
                        return -1;
                }
        }
        g_date_add_days( &d, dayCol + (weekRow * 7) );

        /* Check to make sure we're within the column's displayed range. */
        {
                GDate ccd;
                g_date_set_dmy( &ccd, 1, dcal->month, dcal->year );
                g_date_add_months( &ccd, (colNum+1) * dcal->monthsPerCol );
                if ( g_date_julian(&d) >= g_date_julian(&ccd) ) {
                        printf( "%d outside of column range [%d]\n",
                                g_date_julian(&d), g_date_julian(&ccd) );
                        return -1;
                }
        }

        dayOfCal = g_date_julian(&d) - g_date_julian(&startD);

        /* one more check before returning... */
        g_date_subtract_months( &d, dcal->numMonths );
        if ( g_date_julian(&d) >= g_date_julian(&startD) ) {
                /* we're past the end of the displayed calendar, thus -1 */
                printf( "%d >= %d\n",
                        g_date_julian( &d ), g_date_julian( &startD ) );
                return -1;
        }

        return dayOfCal;
}

static gint
gdc_get_doc_offset( GncDenseCal *dcal, GDate *d )
{
        gint toRet;
        /* soc == start-of-calendar */
        GDate soc;

        g_date_set_dmy( &soc, 1, dcal->month, dcal->year );
        /* ensure not before calendar start. */
        if ( g_date_julian(d) < g_date_julian(&soc) ) {
                return -1;
        }
        /* do computation here, since we're going to change the
         * start-of-calendar date. */
        toRet = g_date_julian(d) - g_date_julian(&soc);
        /* ensure not after end of visible calendar. */
        g_date_add_months( &soc, dcal->numMonths );
        if ( g_date_julian(d) > g_date_julian(&soc) ) {
                return -1;
        }
        /* return pre-computed value. */
        return toRet;
}

/**
 * Marks the given array of GDate*s on the calendar with the given name.
 **/
guint
gnc_dense_cal_mark( GncDenseCal *dcal,
                    guint size, GDate **dateArray,
                    gchar *name, gchar *info )
{
        gint i, doc;
        gdc_mark_data *newMark;
        GDate *d;
        GList *l;

        if ( size == 0 ) {
                printf( "0 size not allowed\n" );
                return -1;
        }

        if ( name == NULL
             || strlen(name) == 0 ) {
                printf( "name must be reasonable\n" );
                return -1;
        }

        newMark = g_new0( gdc_mark_data, 1 );
        newMark->name     = g_strdup(name);
        newMark->info     = NULL;
        if ( info ) {
                newMark->info = g_strdup(info);
        }
        newMark->tag      = dcal->lastMarkTag++;
        newMark->ourMarks = NULL;

        for ( i=0; i<size; i++ ) {
                d = dateArray[i];
                doc = gdc_get_doc_offset( dcal, d );
                if ( doc < 0 ) {
                        printf( "This is TheErrorThatShouldBeAppropriately"
                                "Handeled @ %d for \"%s\" with %d\n",
                                i, name, doc );
                        continue;
                }
                l = g_list_alloc();
                l->data = newMark;
                dcal->marks[doc] = g_list_concat( dcal->marks[doc], l );
                newMark->ourMarks = g_list_append( newMark->ourMarks,
                                                   GINT_TO_POINTER(doc) );
        }
        dcal->markData = g_list_append( dcal->markData, (gpointer)newMark );
        return newMark->tag;
}

void
gnc_dense_cal_mark_remove( GncDenseCal *dcal, guint markToRemove )
{
        GList *l, *calMarkL;
        gint doc;
        gdc_mark_data *gdcmd;

        gdcmd = NULL;
        for ( l = dcal->markData; l; l=l->next ) {
                gdcmd = (gdc_mark_data*)l->data;
                if ( gdcmd->tag == markToRemove )
                        break;
        }
        g_assert( l != NULL );
        if ( l == NULL ) {
                return;
        }
        g_assert( gdcmd != NULL );

        l = NULL;
        for ( calMarkL = gdcmd->ourMarks;
              calMarkL;
              calMarkL = calMarkL->next ) {
                doc = GPOINTER_TO_INT(calMarkL->data);
                dcal->marks[doc] = g_list_remove( dcal->marks[doc], gdcmd );
        }
        g_list_free( gdcmd->ourMarks );
        dcal->markData = g_list_remove( dcal->markData, gdcmd );
        g_free( gdcmd );
}
