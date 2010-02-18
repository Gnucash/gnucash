/********************************************************************
 * gnc-html-graph-gog_webkit.c -- GNC/HTML Graphing support via GOG *
 *                                                                  *
 * Copyright (C) 2005 Joshua Sled <jsled@asynchronous.org>          *
 * Copyright (C) 2009 Phil Longstaff <plongstaff@rogers.com>        *
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
 ********************************************************************/

#include "config.h"

#include <gtk/gtk.h>
#include <webkit/webkit.h>
#include <string.h>
#include <stdlib.h>

#include "gnc-ui-util.h"
#include "gnc-html-graph-gog.h"
#include "gnc-html-graph-gog-webkit.h"
#include "gnc-html-graph-gog-extras.h"
#include "gnc-html.h"
#include "gnc-engine.h"

/**
 * TODO:
 * - scatter-plot marker selection
 * - series-color, piecharts (hard, not really supported by GOG)
 *   and scatters (or drop feature)
 * - title-string freeing (fixmes)
 * - general graph cleanup
 **/

/* indicates the debugging module that this .o belongs to.  */
#undef G_LOG_DOMAIN
#define G_LOG_DOMAIN "gnc.html.graph.gog.webkit"
static QofLogModule log_module = GNC_MOD_HTML;

static int handle_piechart( GncHtml* html, gpointer eb, gpointer d );
static int handle_barchart( GncHtml* html, gpointer eb, gpointer d );
static int handle_linechart( GncHtml* html, gpointer eb, gpointer d );
static int handle_scatter( GncHtml* html, gpointer eb, gpointer d );

static double * read_doubles(const char * string, int nvalues);

void
gnc_html_graph_gog_webkit_init( void )
{
    gnc_html_graph_gog_init();

    gnc_html_register_object_handler( GNC_CHART_PIE, handle_piechart );
    gnc_html_register_object_handler( GNC_CHART_BAR, handle_barchart );
    gnc_html_register_object_handler( GNC_CHART_SCATTER, handle_scatter );
    gnc_html_register_object_handler( GNC_CHART_LINE, handle_linechart );
}

static double *
read_doubles(const char * string, int nvalues)
{
    int    n;
    gchar *next;
    double * retval = g_new0(double, nvalues);

    // guile is going to (puts ...) the elements of the double array
    // together. In non-POSIX locales, that will be in a format that
    // the locale-specific sscanf will not be able to parse.
    gnc_push_locale("C");
    {
        for (n = 0; n < nvalues; n++)
        {
            retval[n] = strtod(string, &next);
            string = next;
        }
    }
    gnc_pop_locale();

    return retval;
}

static char **
read_strings(const char * string, int nvalues)
{
    int n;
    int choffset = 0;
    int accum = 0;
    char ** retval = g_new0(char *, nvalues);
    char thischar;
    const char * inptr = string;
    int escaped = FALSE;

    for (n = 0; n < nvalues; n++)
    {
        retval[n] = g_new0(char, strlen(string + accum) + 1);
        retval[n][0] = 0;
        choffset = 0;
        while ((thischar = *inptr) != 0)
        {
            if (thischar == '\\')
            {
                escaped = TRUE;
                inptr++;
            }
            else if ((thischar != ' ') || escaped)
            {
                retval[n][choffset] = thischar;
                retval[n][choffset+1] = 0;
                choffset++;
                escaped = FALSE;
                inptr++;
            }
            else
            {
                /* an unescaped space */
                escaped = FALSE;
                inptr++;
                break;
            }
        }
        accum += choffset;
        /* printf("retval[%d] = '%s'\n", n, retval[n]); */
    }

    return retval;
}

static int
get_int_value( gchar** str, const gchar* name )
{
    gchar* p;
    gchar* tag_name;
    int val = -1;

    tag_name = g_strdup_printf( "%s=", name );
    p = g_strstr_len( *str, -1, tag_name );
    if ( p != NULL )
    {
        val = atoi( p + strlen( tag_name ) );
        *str = p + strlen( tag_name );
    }
    g_free( tag_name );

    return val;
}

static int
get_int_param( gchar** str, const gchar* name )
{
    gchar* p;
    gchar* tag_string;
    int val = -1;

    tag_string = g_strdup_printf( "<param name=\"%s\" value=\"", name );
    p = g_strstr_len( *str, -1, tag_string );
    if ( p != NULL )
    {
        val = atoi( p + strlen( tag_string ) );
        *str = p + strlen( tag_string );
    }
    g_free( tag_string );

    return val;
}

static gchar*
get_string_param( gchar** str, const gchar* name )
{
    gchar* p;
    gchar* p_end;
    gchar* tag_string;
    gchar* val = NULL;

    tag_string = g_strdup_printf( "<param name=\"%s\" value=\"", name );
    p = g_strstr_len( *str, -1, tag_string );
    if ( p != NULL )
    {
        p += strlen( tag_string );
        p_end = g_strstr_len( p, -1, "\">\n" );
        val = g_strndup( p, (p_end - p) );
        *str = p_end + strlen( "\">\n" );
    }
    g_free( tag_string );

    return val;
}

static gchar*
convert_pixbuf_to_base64_string( GdkPixbuf* pixbuf )
{
    gchar* pixel_buffer;
    gsize pixel_buffer_size;
    GError* error = NULL;
    gchar* base64_buf;

    if ( !gdk_pixbuf_save_to_buffer( pixbuf, &pixel_buffer, &pixel_buffer_size, "png",
                                     &error, NULL ) )
    {
        PERR( "Unable to save pixbuf to buffer: %s\n", error->message );
        return NULL;
    }

    base64_buf = g_base64_encode( (guchar *)pixel_buffer, pixel_buffer_size );
    g_free( pixel_buffer );
    return base64_buf;
}

/*
 * Handle the following parameters:
 * title: text
 * subtitle: text
 * datasize: (length data), sscanf( .., %d, (int)&datasize )
 * data: (foreach (lambda (datum) (push datum) (push " ")) data)
 * colors: string; space-seperated?
 * labels: string; space-seperated?
 * slice_urls_[123]: ?
 * legend_urls_[123]: ?
 */
static gboolean
handle_piechart( GncHtml* html, gpointer eb, gpointer d )
{
    gchar* object_info = (gchar*)eb;
    gchar** pResult = (gchar**)d;
    GncHtmlPieChartInfo pieChartInfo;
    GdkPixbuf* pixbuf;
    gchar* p;
    gchar* p_end;
    gchar* temp_str;
    gchar* base64_buf;

    pieChartInfo.width = get_int_value( &object_info, "width" );
    pieChartInfo.height = get_int_value( &object_info, "height" );
    pieChartInfo.title = get_string_param( &object_info, "title" );
    pieChartInfo.subtitle = get_string_param( &object_info, "subtitle" );
    pieChartInfo.datasize = get_int_param( &object_info, "datasize" );
    temp_str = get_string_param( &object_info, "data" );
    if ( temp_str != NULL )
    {
        pieChartInfo.data = read_doubles( temp_str, pieChartInfo.datasize );
    }
    temp_str = get_string_param( &object_info, "colors" );
    if ( temp_str != NULL )
    {
        pieChartInfo.colors = read_strings( temp_str, pieChartInfo.datasize );
        g_free( temp_str );
    }
    temp_str = get_string_param( &object_info, "labels" );
    if ( temp_str != NULL )
    {
        pieChartInfo.labels = read_strings( temp_str, pieChartInfo.datasize );
        g_free( temp_str );
    }

    pixbuf = gnc_html_graph_gog_create_piechart( &pieChartInfo );
    if ( pieChartInfo.title != NULL ) g_free( (gchar*)pieChartInfo.title );
    if ( pieChartInfo.subtitle != NULL ) g_free( (gchar*)pieChartInfo.subtitle );

    base64_buf = convert_pixbuf_to_base64_string( pixbuf );
    if ( base64_buf == NULL )
    {
        return FALSE;
    }

    *pResult = g_strdup_printf( "<img src=\"data:image/png;base64,%s \" alt=\"Cannot display piechart\"/>", base64_buf );
    g_free( base64_buf );

    g_debug("piechart rendered.");
    return TRUE;
}

/**
 * data_rows:int
 * data_cols:int
 * data:doubles[], data_rows*data_cols
 * x_axis_label:string
 * y_axis_label:string
 * row_labels:string[]
 * col_labels:string[]
 * col_colors:string[]
 * rotate_row_labels:boolean
 * stacked:boolean
 **/
static gboolean
handle_barchart( GncHtml* html, gpointer eb, gpointer d )
{
    gchar* object_info = (gchar*)eb;
    gchar** pResult = (gchar**)d;
    GncHtmlBarChartInfo barChartInfo;
    GdkPixbuf* pixbuf;
    gchar* p;
    gchar* p_end;
    gchar* temp_str;
    gchar* base64_buf;

    barChartInfo.width = get_int_value( &object_info, "width" );
    barChartInfo.height = get_int_value( &object_info, "height" );
    barChartInfo.title = get_string_param( &object_info, "title" );
    barChartInfo.subtitle = get_string_param( &object_info, "subtitle" );
    barChartInfo.data_rows = get_int_param( &object_info, "data_rows" );
    barChartInfo.data_cols = get_int_param( &object_info, "data_cols" );
    temp_str = get_string_param( &object_info, "data" );
    if ( temp_str != NULL )
    {
        barChartInfo.data = read_doubles( temp_str, barChartInfo.data_rows * barChartInfo.data_cols );
    }
    barChartInfo.x_axis_label = get_string_param( &object_info, "x_axis_label" );
    barChartInfo.y_axis_label = get_string_param( &object_info, "y_axis_label" );
    temp_str = get_string_param( &object_info, "col_colors" );
    if ( temp_str != NULL )
    {
        barChartInfo.col_colors = read_strings( temp_str, barChartInfo.data_cols );
        g_free( temp_str );
    }
    temp_str = get_string_param( &object_info, "row_labels" );
    if ( temp_str != NULL )
    {
        barChartInfo.row_labels = read_strings( temp_str, barChartInfo.data_rows );
        g_free( temp_str );
    }
    temp_str = get_string_param( &object_info, "col_labels" );
    if ( temp_str != NULL )
    {
        barChartInfo.col_labels = read_strings( temp_str, barChartInfo.data_cols );
        g_free( temp_str );
    }
    barChartInfo.rotate_row_labels = get_int_param( &object_info, "rotate_row_labels" );
    barChartInfo.stacked = get_int_param( &object_info, "stacked" );

    pixbuf = gnc_html_graph_gog_create_barchart( &barChartInfo );
    if ( barChartInfo.title != NULL ) g_free( (gchar*)barChartInfo.title );
    if ( barChartInfo.subtitle != NULL ) g_free( (gchar*)barChartInfo.subtitle );
    if ( barChartInfo.x_axis_label != NULL ) g_free( (gchar*)barChartInfo.x_axis_label );
    if ( barChartInfo.y_axis_label != NULL ) g_free( (gchar*)barChartInfo.y_axis_label );

    base64_buf = convert_pixbuf_to_base64_string( pixbuf );
    if ( base64_buf == NULL )
    {
        return FALSE;
    }

    *pResult = g_strdup_printf( "<img src=\"data:image/png;base64,%s \" alt=\"Cannot display barchart\"/>", base64_buf );

    g_debug("barchart rendered.");
    return TRUE;
}


/**
 * data_rows:int
 * data_cols:int
 * data:doubles[], data_rows*data_cols
 * x_axis_label:string
 * y_axis_label:string
 * row_labels:string[]
 * col_labels:string[]
 * col_colors:string[]
 * rotate_row_labels:boolean
 * stacked:boolean
 * markers:boolean
 * major_grid:boolean
 * minor_grid:boolean
 **/
static gboolean
handle_linechart( GncHtml* html, gpointer eb, gpointer d )
{
    gchar* object_info = (gchar*)eb;
    gchar** pResult = (gchar**)d;
    GncHtmlLineChartInfo lineChartInfo;
    GdkPixbuf* pixbuf;
    gchar* p;
    gchar* p_end;
    gchar* temp_str;
    gchar* base64_buf;

    lineChartInfo.width = get_int_value( &object_info, "width" );
    lineChartInfo.height = get_int_value( &object_info, "height" );
    lineChartInfo.title = get_string_param( &object_info, "title" );
    lineChartInfo.subtitle = get_string_param( &object_info, "subtitle" );
    lineChartInfo.data_rows = get_int_param( &object_info, "data_rows" );
    lineChartInfo.data_cols = get_int_param( &object_info, "data_cols" );
    temp_str = get_string_param( &object_info, "data" );
    if ( temp_str != NULL )
    {
        lineChartInfo.data = read_doubles( temp_str, lineChartInfo.data_rows * lineChartInfo.data_cols );
    }
    lineChartInfo.x_axis_label = get_string_param( &object_info, "x_axis_label" );
    lineChartInfo.y_axis_label = get_string_param( &object_info, "y_axis_label" );
    temp_str = get_string_param( &object_info, "col_colors" );
    if ( temp_str != NULL )
    {
        lineChartInfo.col_colors = read_strings( temp_str, lineChartInfo.data_cols );
        g_free( temp_str );
    }
    temp_str = get_string_param( &object_info, "row_labels" );
    if ( temp_str != NULL )
    {
        lineChartInfo.row_labels = read_strings( temp_str, lineChartInfo.data_rows );
        g_free( temp_str );
    }
    temp_str = get_string_param( &object_info, "col_labels" );
    if ( temp_str != NULL )
    {
        lineChartInfo.col_labels = read_strings( temp_str, lineChartInfo.data_cols );
        g_free( temp_str );
    }
    lineChartInfo.rotate_row_labels = get_int_param( &object_info, "rotate_row_labels" );
    lineChartInfo.stacked = get_int_param( &object_info, "stacked" );
    lineChartInfo.markers = get_int_param( &object_info, "markers" );
    lineChartInfo.major_grid = get_int_param( &object_info, "major_grid" );
    lineChartInfo.minor_grid = get_int_param( &object_info, "minor_grid" );

    pixbuf = gnc_html_graph_gog_create_linechart( &lineChartInfo );
    if ( lineChartInfo.title != NULL ) g_free( (gchar*)lineChartInfo.title );
    if ( lineChartInfo.subtitle != NULL ) g_free( (gchar*)lineChartInfo.subtitle );
    if ( lineChartInfo.x_axis_label != NULL ) g_free( (gchar*)lineChartInfo.x_axis_label );
    if ( lineChartInfo.y_axis_label != NULL ) g_free( (gchar*)lineChartInfo.y_axis_label );

    base64_buf = convert_pixbuf_to_base64_string( pixbuf );
    if ( base64_buf == NULL )
    {
        return FALSE;
    }

    *pResult = g_strdup_printf( "<img src=\"data:image/png;base64,%s \" alt=\"Cannot display linechart\"/>", base64_buf );

    g_debug("linechart rendered.");
    return TRUE;
}


static gboolean
handle_scatter( GncHtml* html, gpointer eb, gpointer d )
{
    gchar* object_info = (gchar*)eb;
    gchar** pResult = (gchar**)d;
    GncHtmlScatterPlotInfo scatterPlotInfo;
    GdkPixbuf* pixbuf;
    gchar* p;
    gchar* p_end;
    gchar* temp_str;
    gchar* base64_buf;

    scatterPlotInfo.width = get_int_value( &object_info, "width" );
    scatterPlotInfo.height = get_int_value( &object_info, "height" );
    scatterPlotInfo.title = get_string_param( &object_info, "title" );
    scatterPlotInfo.subtitle = get_string_param( &object_info, "subtitle" );
    scatterPlotInfo.x_axis_label = get_string_param( &object_info, "x_axis_label" );
    scatterPlotInfo.y_axis_label = get_string_param( &object_info, "y_axis_label" );
    scatterPlotInfo.marker_str = get_string_param( &object_info, "marker" );
    scatterPlotInfo.color_str = get_string_param( &object_info, "color" );
    scatterPlotInfo.datasize = get_int_param( &object_info, "datasize" );
    temp_str = get_string_param( &object_info, "x_data" );
    if ( temp_str != NULL )
    {
        scatterPlotInfo.xData = read_doubles( temp_str, scatterPlotInfo.datasize );
    }
    temp_str = get_string_param( &object_info, "y_data" );
    if ( temp_str != NULL )
    {
        scatterPlotInfo.yData = read_doubles( temp_str, scatterPlotInfo.datasize );
    }

    pixbuf = gnc_html_graph_gog_create_scatterplot( &scatterPlotInfo );
    if ( scatterPlotInfo.title != NULL ) g_free( (gchar*)scatterPlotInfo.title );
    if ( scatterPlotInfo.subtitle != NULL ) g_free( (gchar*)scatterPlotInfo.subtitle );

    base64_buf = convert_pixbuf_to_base64_string( pixbuf );
    if ( base64_buf == NULL )
    {
        return FALSE;
    }

    *pResult = g_strdup_printf( "<img src=\"data:image/png;base64,%s \" alt=\"Cannot display scatterplot\"/>", base64_buf );

    g_debug("scatterplot rendered.");
    return TRUE;
}
