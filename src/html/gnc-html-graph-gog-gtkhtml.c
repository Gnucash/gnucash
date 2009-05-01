/********************************************************************
 * gnc-html-graph-gog.c -- GNC/HTML Graphing support via GOG        *
 *                                                                  *
 * Copyright (C) 2005 Joshua Sled <jsled@asynchronous.org>          *
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
#include <gtkhtml/gtkhtml.h>
#include <gtkhtml/gtkhtml-embedded.h>
#include <string.h>

#include "gnc-ui-util.h"
#include "gnc-html-graph-gog.h"
#include "gnc-html-graph-gog-gtkhtml.h"
#include "gnc-html-graph-gog-extras.h"
#include "gnc-html.h"
#include "gnc-engine.h"
#include <goffice/goffice.h>
#include <goffice/graph/gog-chart.h>
#include <goffice/graph/gog-graph.h>
#include <goffice/graph/gog-object.h>
#if defined(HAVE_GOFFICE_0_5)
#    include <goffice/graph/gog-renderer.h>
#elif defined(GOFFICE_WITH_CAIRO)
#    include <goffice/graph/gog-renderer-cairo.h>
#else
#    include <goffice/graph/gog-renderer-pixbuf.h>
#endif
#ifndef GTKHTML_USES_GTKPRINT
#    include <goffice/graph/gog-renderer-gnome-print.h>
#endif
/* everything inside the following #ifndef can be safely removed when gnucash
requires libgoffice >= 0.7.5. */
#ifndef GOG_TYPE_GRAPH
#	define GOG_TYPE_GRAPH GOG_GRAPH_TYPE
#	define GOG_TYPE_RENDERER GOG_RENDERER_TYPE
#endif
#include <goffice/graph/gog-styled-object.h>
#include <goffice/graph/gog-plot.h>
#include <goffice/graph/gog-series.h>
#include <goffice/utils/go-color.h>
#include <goffice/utils/go-marker.h>
#include <goffice/graph/gog-data-set.h>
#include <goffice/data/go-data-simple.h>
#include <goffice/app/go-plugin.h>
#include <goffice/app/go-plugin-loader-module.h>

/**
 * TODO:
 * - scatter-plot marker selection
 * - series-color, piecharts (hard, not really supported by GOG)
 *   and scatters (or drop feature)
 * - title-string freeing (fixmes)
 * - general graph cleanup
 **/

#undef G_LOG_DOMAIN
#define G_LOG_DOMAIN "gnc.html.graph.gog.gtkhtml"

static int handle_piechart( GncHtml* html, gpointer eb, gpointer d );
static int handle_barchart( GncHtml* html, gpointer eb, gpointer d );
static int handle_linechart( GncHtml* html, gpointer eb, gpointer d );
static int handle_scatter( GncHtml* html, gpointer eb, gpointer d );

#ifdef GTKHTML_USES_GTKPRINT
static void draw_print_cb(GtkHTMLEmbedded *eb, cairo_t *cr, gpointer graph);
#else
static void draw_print_cb(GtkHTMLEmbedded *eb, GnomePrintContext *context, gpointer graph);
#endif

static gboolean create_basic_plot_elements(const char *plot_type, GogObject **out_graph, GogObject **out_chart, GogPlot **out_plot);

static double * read_doubles(const char * string, int nvalues);

static void set_chart_titles_from_hash(GogObject *chart, gpointer eb);
static void set_chart_titles(GogObject *chart, const char *title, const char* sub_title);
static void set_chart_axis_labels_from_hash(GogObject *chart, gpointer eb);
static void set_chart_axis_labels(GogObject *chart, const char *x_axis_label, const char* y_axis_label);

void
gnc_html_graph_gog_gtkhtml_init( void )
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
    for (n=0; n<nvalues; n++) {
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
  int choffset=0;
  int accum = 0;
  char ** retval = g_new0(char *, nvalues);
  char thischar;
  const char * inptr = string;
  int escaped = FALSE;

  for (n=0; n < nvalues; n++) {
    retval[n] = g_new0(char, strlen(string+accum)+1);
    retval[n][0] = 0;
    choffset = 0;
    while ((thischar = *inptr) != 0) {
      if (thischar == '\\') {
        escaped = TRUE;
        inptr++;
      }
      else if ((thischar != ' ') || escaped) {
        retval[n][choffset] = thischar;
        retval[n][choffset+1] = 0;    
        choffset++;
        escaped = FALSE;
        inptr++;
      }
      else {
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

static void
add_pixbuf_graph_widget( GtkHTMLEmbedded *eb, GdkPixbuf* buf )
{
  GtkWidget *widget;
  gboolean update_status;
  GogGraph *graph = GOG_GRAPH(g_object_get_data( G_OBJECT(buf), "graph" ));

  widget = gtk_image_new_from_pixbuf (buf);
  gtk_widget_set_size_request (widget, eb->width, eb->height);
  gtk_widget_show_all (widget);
  gtk_container_add (GTK_CONTAINER (eb), widget);

  // blindly copied from gnc-html-guppi.c..
  gtk_widget_set_size_request (GTK_WIDGET (eb), eb->width, eb->height);

  g_object_set_data_full (G_OBJECT (eb), "graph", graph, g_object_unref);
  g_signal_connect (G_OBJECT (eb), "draw_print",
		    G_CALLBACK (draw_print_cb), NULL);
}

static gboolean
create_basic_plot_elements(const char *plot_type_name,
                           GogObject **out_graph,
                           GogObject **out_chart,
                           GogPlot **out_plot)
{
  *out_graph = g_object_new(GOG_TYPE_GRAPH, NULL);
  *out_chart = gog_object_add_by_name(*out_graph, "Chart", NULL);
  *out_plot = gog_plot_new_by_name(plot_type_name);
  if (!*out_plot)
  {
    // FIXME - log betterer; should probably use GError?
    g_warning("gog: unable to load %s plugin", plot_type_name);
    return FALSE;
  }
  gog_object_add_by_name(*out_chart, "Plot", GOG_OBJECT(*out_plot) );
  return TRUE;
}

static void
set_chart_titles_from_hash(GogObject *chart, gpointer eb)
{
  set_chart_titles(chart,
                   (const char *)gnc_html_get_embedded_param(eb, "title"), 
                   (const char *)gnc_html_get_embedded_param(eb, "subtitle"));
}

static void
set_chart_titles(GogObject *chart, const char *title, const char* sub_title)
{
  gchar *my_sub_title, *total_title;
  GOData *title_scalar;
  GogObject *tmp;

  if (sub_title)
    my_sub_title = g_strdup_printf("%s(%s)", title ? " " : "", sub_title);
  else
    my_sub_title = g_strdup("");

  total_title = g_strdup_printf("%s%s", title ? title : "", my_sub_title);

  tmp = gog_object_add_by_name(chart, "Title", NULL);
  title_scalar = go_data_scalar_str_new(total_title, TRUE);
  gog_dataset_set_dim(GOG_DATASET(tmp), 0, title_scalar, NULL);

  g_free(my_sub_title);
}

static void
set_chart_axis_labels_from_hash(GogObject *chart, gpointer eb)
{
  set_chart_axis_labels(chart,
                        gnc_html_get_embedded_param(eb, "x_axis_label"),
                        gnc_html_get_embedded_param(eb, "y_axis_label"));
}

static void
set_chart_axis_labels(GogObject *chart, const char *x_axis_label, const char* y_axis_label)
{
  if (x_axis_label != NULL)
  {
    GogObject *xaxis, *label;
    GOData *data;
    xaxis = gog_object_get_child_by_role(chart, gog_object_find_role_by_name(chart, "X-Axis"));
    label = gog_object_add_by_name(xaxis, "Label", NULL);
    data = go_data_scalar_str_new(x_axis_label, FALSE);
    gog_dataset_set_dim(GOG_DATASET(label), 0, data, NULL);
  }

  if (y_axis_label != NULL)
  {
    GogObject *yaxis, *label;
    GOData *data;
    yaxis = gog_object_get_child_by_role(chart, gog_object_find_role_by_name(chart, "Y-Axis"));
    label = gog_object_add_by_name(yaxis, "Label", NULL);
    data = go_data_scalar_str_new(y_axis_label, FALSE);
    gog_dataset_set_dim(GOG_DATASET(label), 0, data, NULL);
  }
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
handle_piechart( GncHtml* html, gpointer eb, gpointer unused )
{
  GncHtmlPieChartInfo pieChartInfo;

  // parse data from the text-ized params.
  {
    const char *datasizeStr, *dataStr, *labelsStr, *colorStr;

    datasizeStr = gnc_html_get_embedded_param(eb, "datasize");
    dataStr = gnc_html_get_embedded_param(eb, "data" );
    labelsStr = gnc_html_get_embedded_param(eb, "labels");
    colorStr = gnc_html_get_embedded_param(eb, "colors");
    g_return_val_if_fail( datasizeStr != NULL
                          && dataStr != NULL
                          && labelsStr != NULL
                          && colorStr != NULL, FALSE );
    pieChartInfo.datasize = atoi( datasizeStr );
    pieChartInfo.data = read_doubles( dataStr, pieChartInfo.datasize );
    pieChartInfo.labels = read_strings( labelsStr, pieChartInfo.datasize );
    pieChartInfo.colors = read_strings( colorStr, pieChartInfo.datasize );
  }

  pieChartInfo.title = (const char *)gnc_html_get_embedded_param(eb, "title"); 
  pieChartInfo.subtitle = (const char *)gnc_html_get_embedded_param(eb, "subtitle");
  pieChartInfo.width = ((GtkHTMLEmbedded*)eb)->width;
  pieChartInfo.height = ((GtkHTMLEmbedded*)eb)->height;

  add_pixbuf_graph_widget( eb, gnc_html_graph_gog_create_piechart( &pieChartInfo ) );

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
handle_barchart( GncHtml* html, gpointer eb, gpointer unused )
{
  GncHtmlBarChartInfo barChartInfo;

  // parse data from the text-ized params
  // series => bars [gnc:cols]
  // series-elements => segments [gnc:rows]
  {
    const char *data_rows_str, *data_cols_str, *data_str, *col_labels_str, *row_labels_str;
    const char *col_colors_str, *rotate_row_labels_str = NULL, *stacked_str = NULL;

    data_rows_str         = gnc_html_get_embedded_param (eb, "data_rows");
    data_cols_str         = gnc_html_get_embedded_param (eb, "data_cols");
    data_str              = gnc_html_get_embedded_param (eb, "data" );
    row_labels_str        = gnc_html_get_embedded_param (eb, "row_labels");
    col_labels_str        = gnc_html_get_embedded_param (eb, "col_labels");
    col_colors_str        = gnc_html_get_embedded_param (eb, "col_colors");
    rotate_row_labels_str = gnc_html_get_embedded_param (eb, "rotate_row_labels");
    stacked_str           = gnc_html_get_embedded_param (eb, "stacked");

    barChartInfo.rotate_row_labels     = (gboolean) atoi (rotate_row_labels_str);
    barChartInfo.stacked               = (gboolean) atoi (stacked_str);

#if 0 // too strong at the moment.
    g_return_val_if_fail (data_rows_str != NULL
                          && data_cols_str != NULL
                          && data_str != NULL
                          && col_labels_str != NULL
                          && row_labels_str != NULL
                          && col_colors_str != NULL, FALSE );
#endif // 0
    barChartInfo.data_rows = atoi (data_rows_str);
    barChartInfo.data_cols = atoi (data_cols_str);
    barChartInfo.data = read_doubles (data_str, barChartInfo.data_rows*barChartInfo.data_cols);
    barChartInfo.row_labels = read_strings (row_labels_str, barChartInfo.data_rows);
    barChartInfo.col_labels = read_strings (col_labels_str, barChartInfo.data_cols);
    barChartInfo.col_colors = read_strings (col_colors_str, barChartInfo.data_cols);
  }

  barChartInfo.title = (const char *)gnc_html_get_embedded_param(eb, "title"); 
  barChartInfo.subtitle = (const char *)gnc_html_get_embedded_param(eb, "subtitle");
  barChartInfo.width = ((GtkHTMLEmbedded*)eb)->width;
  barChartInfo.height = ((GtkHTMLEmbedded*)eb)->height;
  barChartInfo.x_axis_label = gnc_html_get_embedded_param(eb, "x_axis_label"),
  barChartInfo.y_axis_label = gnc_html_get_embedded_param(eb, "y_axis_label");

  add_pixbuf_graph_widget( eb, gnc_html_graph_gog_create_barchart( &barChartInfo ) );

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
handle_linechart( GncHtml* html, gpointer eb, gpointer unused )
{
  GncHtmlLineChartInfo lineChartInfo;

  // parse data from the text-ized params
  // series => lines [gnc:cols]
  // series-elements => segments [gnc:rows]
  {
    const char *data_rows_str, *data_cols_str, *data_str, *col_labels_str, *row_labels_str;
    const char *col_colors_str, *rotate_row_labels_str = NULL, *stacked_str = NULL, *markers_str = NULL;
    const char *major_grid_str = NULL, *minor_grid_str = NULL;

    data_rows_str         = gnc_html_get_embedded_param (eb, "data_rows");
    data_cols_str         = gnc_html_get_embedded_param (eb, "data_cols");
    data_str              = gnc_html_get_embedded_param (eb, "data" );
    row_labels_str        = gnc_html_get_embedded_param (eb, "row_labels");
    col_labels_str        = gnc_html_get_embedded_param (eb, "col_labels");
    col_colors_str        = gnc_html_get_embedded_param (eb, "col_colors");
    rotate_row_labels_str = gnc_html_get_embedded_param (eb, "rotate_row_labels");
    stacked_str           = gnc_html_get_embedded_param (eb, "stacked");
    markers_str           = gnc_html_get_embedded_param (eb, "markers");
    major_grid_str        = gnc_html_get_embedded_param (eb, "major_grid");
    minor_grid_str        = gnc_html_get_embedded_param (eb, "minor_grid");

    lineChartInfo.rotate_row_labels     = (gboolean) atoi (rotate_row_labels_str);
    lineChartInfo.stacked               = (gboolean) atoi (stacked_str);
    lineChartInfo.markers               = (gboolean) atoi (markers_str);
    lineChartInfo.major_grid            = (gboolean) atoi (major_grid_str);
    lineChartInfo.minor_grid            = (gboolean) atoi (minor_grid_str);

    lineChartInfo.data_rows = atoi (data_rows_str);
    lineChartInfo.data_cols = atoi (data_cols_str);
    lineChartInfo.data = read_doubles (data_str, lineChartInfo.data_rows*lineChartInfo.data_cols);
    lineChartInfo.row_labels = read_strings (row_labels_str, lineChartInfo.data_rows);
    lineChartInfo.col_labels = read_strings (col_labels_str, lineChartInfo.data_cols);
    lineChartInfo.col_colors = read_strings (col_colors_str, lineChartInfo.data_cols);
  }

  lineChartInfo.title = (const char *)gnc_html_get_embedded_param(eb, "title"); 
  lineChartInfo.subtitle = (const char *)gnc_html_get_embedded_param(eb, "subtitle");
  lineChartInfo.width = ((GtkHTMLEmbedded*)eb)->width;
  lineChartInfo.height = ((GtkHTMLEmbedded*)eb)->height;
  lineChartInfo.x_axis_label = gnc_html_get_embedded_param(eb, "x_axis_label"),
  lineChartInfo.y_axis_label = gnc_html_get_embedded_param(eb, "y_axis_label");

  add_pixbuf_graph_widget( eb, gnc_html_graph_gog_create_linechart( &lineChartInfo ) );

  g_debug("linechart rendered.");
  return TRUE;
}


static gboolean
handle_scatter( GncHtml* html, gpointer eb, gpointer unused )
{
  GncHtmlScatterPlotInfo scatterPlotInfo;

  {
    const char *datasizeStr, *xDataStr, *yDataStr;

    datasizeStr = gnc_html_get_embedded_param( eb, "datasize" );
    scatterPlotInfo.datasize = atoi( datasizeStr );

    xDataStr = gnc_html_get_embedded_param( eb, "x_data" );
    scatterPlotInfo.xData = read_doubles( xDataStr, scatterPlotInfo.datasize );

    yDataStr = gnc_html_get_embedded_param( eb, "y_data" );
    scatterPlotInfo.yData = read_doubles( yDataStr, scatterPlotInfo.datasize );

    scatterPlotInfo.marker_str = gnc_html_get_embedded_param(eb, "marker");
    scatterPlotInfo.color_str = gnc_html_get_embedded_param(eb, "color");
  }

  scatterPlotInfo.title = (const char *)gnc_html_get_embedded_param(eb, "title"); 
  scatterPlotInfo.subtitle = (const char *)gnc_html_get_embedded_param(eb, "subtitle");
  scatterPlotInfo.width = ((GtkHTMLEmbedded*)eb)->width;
  scatterPlotInfo.height = ((GtkHTMLEmbedded*)eb)->height;
  scatterPlotInfo.x_axis_label = gnc_html_get_embedded_param(eb, "x_axis_label"),
  scatterPlotInfo.y_axis_label = gnc_html_get_embedded_param(eb, "y_axis_label");

  add_pixbuf_graph_widget( eb, gnc_html_graph_gog_create_scatterplot( &scatterPlotInfo ) );

  return TRUE;
}

#ifdef GTKHTML_USES_GTKPRINT
static void
draw_print_cb(GtkHTMLEmbedded *eb, cairo_t *cr, gpointer unused)
{
  GogGraph *graph = GOG_GRAPH(g_object_get_data(G_OBJECT(eb), "graph"));
#    ifdef HAVE_GOFFICE_0_5
  GogRenderer *rend = g_object_new(GOG_TYPE_RENDERER, "model", graph, NULL);
#    else
  GogRendererCairo *rend = g_object_new(GOG_RENDERER_CAIRO_TYPE, "model", graph,
                                        "cairo", cr, "is-vector", TRUE, NULL);
#    endif

  /* assuming pixel size is 0.5, cf. gtkhtml/src/htmlprinter.c */
  cairo_scale(cr, 0.5, 0.5);

  cairo_translate(cr, 0, -eb->height);

#    ifdef HAVE_GOFFICE_0_5
  gog_renderer_render_to_cairo(rend, cr, eb->width, eb->height);
#    else
  gog_renderer_cairo_update(rend, eb->width, eb->height, 1.0);
#    endif
  g_object_unref(rend);
}

#else /* !GTKHTML_USES_GTKPRINT */
static void
draw_print_cb(GtkHTMLEmbedded *eb, GnomePrintContext *context, gpointer unused)
{
  GogGraph *graph = GOG_GRAPH (g_object_get_data (G_OBJECT (eb), "graph"));

  /* assuming pixel size is 0.5, cf. gtkhtml/src/htmlprinter.c */
  gnome_print_scale (context, 0.5, 0.5);

  gnome_print_translate (context, 0, eb->height);
  gog_graph_print_to_gnome_print (graph, context, eb->width, eb->height);
}
#endif /* GTKHTML_USES_GTKPRINT */
