/********************************************************************
 * gnc-html-graph-gog-gtkmozembed.c -- GNC/HTML Graphing support via*
 *                                     GOG                          *
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
#include <gtkmozembed.h>
#include <string.h>

#include "gnc-ui-util.h"
#include "gnc-html-graph-gog-gtkmozembed.h"
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
#include <goffice/graph/gog-style.h>
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
#define G_LOG_DOMAIN "gnc.html.graph.gog.gtkmozembed"

static int handle_piechart( GncHtml* html, gpointer eb, gpointer d );
static int handle_barchart( GncHtml* html, gpointer eb, gpointer d );
static int handle_linechart( GncHtml* html, gpointer eb, gpointer d );
static int handle_scatter( GncHtml* html, gpointer eb, gpointer d );

#if 0
#ifdef GTKHTML_USES_GTKPRINT
static void draw_print_cb(GtkHTMLEmbedded *eb, cairo_t *cr, gpointer graph);
#else
static void draw_print_cb(GtkHTMLEmbedded *eb, GnomePrintContext *context, gpointer graph);
#endif
#endif

static gboolean create_basic_plot_elements(const char *plot_type, GogObject **out_graph, GogObject **out_chart, GogPlot **out_plot);

static double * read_doubles(const char * string, int nvalues);

static void set_chart_titles_from_hash(GogObject *chart, gpointer eb);
static void set_chart_titles(GogObject *chart, const char *title, const char* sub_title);
static void set_chart_axis_labels_from_hash(GogObject *chart, gpointer eb);
static void set_chart_axis_labels(GogObject *chart, const char *x_axis_label, const char* y_axis_label);

void
gnc_html_graph_gog_gtkmozembed_init( void )
{
  g_debug( "init gog graphing" );
  
  libgoffice_init();
  
  /* Initialize plugins manager */
  go_plugins_init( NULL, NULL, NULL, NULL, TRUE, GO_PLUGIN_LOADER_MODULE_TYPE );

  gnc_html_register_object_handler( "gnc-guppi-pie", handle_piechart );
  gnc_html_register_object_handler( "gnc-guppi-bar", handle_barchart );
  gnc_html_register_object_handler( "gnc-guppi-scatter", handle_scatter );
  gnc_html_register_object_handler( "gnc-guppi-line", handle_linechart );
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

#if 0
static void
add_pixbuf_graph_widget( GtkHTMLEmbedded *eb, GogObject *graph )
{
  GtkWidget *widget;
#if defined(HAVE_GOFFICE_0_5)
  GogRenderer *renderer;
#elif defined(GOFFICE_WITH_CAIRO)
  GogRendererCairo *cairo_renderer;
#else
  GogRendererPixbuf *pixbuf_renderer;
#endif
  GdkPixbuf *buf;
  gboolean update_status;

  // Note that this shouldn't be necessary as per discussion with Jody...
  // ... but it is because we don't embed in a control which passes the
  // update requests back to the graph widget, a-la the foo-canvas that
  // gnumeric uses.  We probably _should_ do something like that, though.
  gog_object_update (GOG_OBJECT (graph));

#if defined(HAVE_GOFFICE_0_5)
  renderer = GOG_RENDERER (g_object_new (GOG_RENDERER_TYPE,
					 "model", graph,
					 NULL));
  update_status = gog_renderer_update (renderer, eb->width, eb->height);
  buf = gog_renderer_get_pixbuf (renderer);
#elif defined(GOFFICE_WITH_CAIRO)
  cairo_renderer = GOG_RENDERER_CAIRO (g_object_new (GOG_RENDERER_CAIRO_TYPE,
						     "model", graph,
						     NULL));
  update_status = gog_renderer_cairo_update (cairo_renderer,
					     eb->width, eb->height, 1.0);
  buf = gog_renderer_cairo_get_pixbuf (cairo_renderer);
#else
  pixbuf_renderer = GOG_RENDERER_PIXBUF (g_object_new (GOG_RENDERER_PIXBUF_TYPE,
						       "model", graph,
						       NULL));
  update_status = gog_renderer_pixbuf_update (pixbuf_renderer,
					      eb->width, eb->height, 1.0);
  buf = gog_renderer_pixbuf_get (pixbuf_renderer);
#endif

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
#endif

static gboolean
create_basic_plot_elements(const char *plot_type_name,
                           GogObject **out_graph,
                           GogObject **out_chart,
                           GogPlot **out_plot)
{
  *out_graph = g_object_new(GOG_GRAPH_TYPE, NULL);
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
  GogObject *graph, *chart;
  GogPlot *plot;
  GogSeries *series;
  GOData *labelData, *sliceData;
  int datasize;
  double *data = NULL;
  char **labels = NULL, **colors = NULL;

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
    datasize = atoi( datasizeStr );
    data = read_doubles( dataStr, datasize );
    labels = read_strings( labelsStr, datasize );
    colors = read_strings( colorStr, datasize );
  }

  if (!create_basic_plot_elements("GogPiePlot", &graph, &chart, &plot))
  {
    return FALSE;
  }
  gog_object_add_by_name(chart, "Legend", NULL);

  GOG_STYLED_OBJECT(graph)->style->outline.width = 5;
  GOG_STYLED_OBJECT(graph)->style->outline.color = RGBA_BLACK;

  series = gog_plot_new_series(plot);
  labelData = go_data_vector_str_new((char const * const *)labels, datasize, NULL);
  gog_series_set_dim(series, 0, labelData, NULL);
  go_data_emit_changed(GO_DATA(labelData));

  sliceData = go_data_vector_val_new(data, datasize, NULL);
  gog_series_set_dim(series, 1, sliceData, NULL);
  go_data_emit_changed(GO_DATA(sliceData));

  // fixme: colors
  set_chart_titles_from_hash(chart, eb);

//FIXME  add_pixbuf_graph_widget (eb, graph);

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
  GogObject *graph, *chart;
  GogPlot *plot;
  GogSeries *series;
  GogStyle *style;
  GOData *label_data, *slice_data;
  int data_rows, data_cols;
  double *data = NULL;
  char **col_labels = NULL, **row_labels = NULL, **col_colors = NULL;
  gboolean rotate_row_labels = FALSE;
  gboolean stacked = FALSE;
  char *bar_type = "normal";
  int bar_overlap = 0 /*percent*/; // seperate bars; no overlap.

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

    rotate_row_labels     = (gboolean) atoi (rotate_row_labels_str);
    stacked               = (gboolean) atoi (stacked_str);

#if 0 // too strong at the moment.
    g_return_val_if_fail (data_rows_str != NULL
                          && data_cols_str != NULL
                          && data_str != NULL
                          && col_labels_str != NULL
                          && row_labels_str != NULL
                          && col_colors_str != NULL, FALSE );
#endif // 0
    data_rows = atoi (data_rows_str);
    data_cols = atoi (data_cols_str);
    data = read_doubles (data_str, data_rows*data_cols);
    row_labels = read_strings (row_labels_str, data_rows);
    col_labels = read_strings (col_labels_str, data_cols);
    col_colors = read_strings (col_colors_str, data_cols);
  }

  if (!create_basic_plot_elements("GogBarColPlot", &graph, &chart, &plot)) {
    return FALSE;
  }
  gog_object_add_by_name(chart, "Legend", NULL);

  if ( stacked ) {
    // when stacked, we want the bars on _top_ of eachother.
    bar_type = "stacked";
    bar_overlap = 100 /*percent*/;
  }

  g_object_set (G_OBJECT (plot),
                //"vary_style_by_element",	TRUE,
                "type",                         bar_type,
                "overlap_percentage",           bar_overlap, 
                NULL);
  label_data = go_data_vector_str_new ((char const * const *)row_labels, data_rows, NULL);
  {
    // foreach row:
    //   series = row
    GdkColor color;
    int i;
    for (i = 0; i < data_cols; i++) {
      GError *err = NULL;

      series = gog_plot_new_series (plot);
      gog_object_set_name (GOG_OBJECT (series), col_labels[i], &err);
      if (err != NULL)
      {
           g_warning("error setting name [%s] on series [%d]: [%s]",
                     col_labels[i], i, err->message);
      }

      g_object_ref (label_data);
      gog_series_set_dim (series, 0, label_data, NULL);
      go_data_emit_changed (GO_DATA (label_data));

      slice_data = go_data_vector_val_new (data + (i*data_rows), data_rows, NULL);
      gog_series_set_dim (series, 1, slice_data, NULL);
      go_data_emit_changed (GO_DATA (slice_data));

      style = gog_styled_object_get_style (GOG_STYLED_OBJECT (series));
      style->fill.type = GOG_FILL_STYLE_PATTERN;
      if (gdk_color_parse (col_colors[i], &color)) {
           style->fill.auto_back = FALSE;
           go_pattern_set_solid (&style->fill.pattern, GDK_TO_UINT (color));
      } else {
           g_warning("cannot parse color [%s]", col_colors[i]);
      }
    }
  }

  if (rotate_row_labels) {
    GogObject *object = gog_object_get_child_by_role (
      chart, gog_object_find_role_by_name (chart, "X-Axis"));
    style = gog_styled_object_get_style (GOG_STYLED_OBJECT (object));
    gog_style_set_text_angle (style, 90.0);
  }

  set_chart_titles_from_hash (chart, eb);
  set_chart_axis_labels_from_hash (chart, eb);

  // we need to do this twice for the barchart... :p
  gog_object_update (GOG_OBJECT (graph));

//FIXME  add_pixbuf_graph_widget (eb, graph);

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
  GogObject *graph, *chart;
  GogPlot *plot;
  GogSeries *series;
  GogStyle *style;
  GOData *label_data, *slice_data;
  int data_rows, data_cols;
  double *data = NULL;
  char **col_labels = NULL, **row_labels = NULL, **col_colors = NULL;
  gboolean rotate_row_labels = FALSE;
  gboolean stacked = FALSE;
  gboolean markers = FALSE;
  gboolean major_grid = FALSE;
  gboolean minor_grid = FALSE;
  char *line_type = "normal";

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

    rotate_row_labels     = (gboolean) atoi (rotate_row_labels_str);
    stacked               = (gboolean) atoi (stacked_str);
    markers               = (gboolean) atoi (markers_str);
    major_grid            = (gboolean) atoi (major_grid_str);
    minor_grid            = (gboolean) atoi (minor_grid_str);

#if 0 // too strong at the moment.
    g_return_val_if_fail (data_rows_str != NULL
                          && data_cols_str != NULL
                          && data_str != NULL
                          && col_labels_str != NULL
                          && row_labels_str != NULL
                          && col_colors_str != NULL, FALSE );
#endif // 0

    data_rows = atoi (data_rows_str);
    data_cols = atoi (data_cols_str);
    data = read_doubles (data_str, data_rows*data_cols);
    row_labels = read_strings (row_labels_str, data_rows);
    col_labels = read_strings (col_labels_str, data_cols);
    col_colors = read_strings (col_colors_str, data_cols);
  }

  if (!create_basic_plot_elements("GogLinePlot", &graph, &chart, &plot)) {
    return FALSE;
  }
  gog_object_add_by_name(chart, "Legend", NULL);

  if ( stacked ) {
    // when stacked, we want the lines on _top_ of eachother.
    line_type = "stacked";
  }

  g_object_set (G_OBJECT (plot),
                //"vary_style_by_element",	TRUE,
                "type",                         line_type,
                "default-style-has-markers",	markers,
                NULL);
  label_data = go_data_vector_str_new ((char const * const *)row_labels, data_rows, NULL);
  {
    // foreach row:
    //   series = row
    GdkColor color;
    int i;
    for (i = 0; i < data_cols; i++) {
      GError *err = NULL;

      series = gog_plot_new_series (plot);
      gog_object_set_name (GOG_OBJECT (series), col_labels[i], &err);
      if (err != NULL)
      {
           g_warning("error setting name [%s] on series [%d]: [%s]",
                     col_labels[i], i, err->message);
      }

      g_object_ref (label_data);
      gog_series_set_dim (series, 0, label_data, NULL);
      go_data_emit_changed (GO_DATA (label_data));

      slice_data = go_data_vector_val_new (data + (i*data_rows), data_rows, NULL);
      gog_series_set_dim (series, 1, slice_data, NULL);
      go_data_emit_changed (GO_DATA (slice_data));

      style = gog_styled_object_get_style (GOG_STYLED_OBJECT (series));
      style->fill.type = GOG_FILL_STYLE_PATTERN;
      if (gdk_color_parse (col_colors[i], &color)) {
           style->fill.auto_back = FALSE;
           go_pattern_set_solid (&style->fill.pattern, GDK_TO_UINT (color));
      } else {
           g_warning("cannot parse color [%s]", col_colors[i]);
      }
    }
  }

  if (rotate_row_labels) {
    GogObject *object = gog_object_get_child_by_role (
      chart, gog_object_find_role_by_name (chart, "X-Axis"));
    style = gog_styled_object_get_style (GOG_STYLED_OBJECT (object));
    gog_style_set_text_angle (style, 90.0);
  }

  if (major_grid || minor_grid) {
    GogObject *object;
    gog_object_add_by_name(chart,"Grid", NULL);
    object = gog_object_get_child_by_role (chart, gog_object_find_role_by_name (chart, "Y-Axis"));
    if (major_grid)
      gog_object_add_by_name (GOG_OBJECT (object),"MajorGrid", NULL);
    if (minor_grid)
      gog_object_add_by_name (GOG_OBJECT (object),"MinorGrid", NULL);
  }

  set_chart_titles_from_hash (chart, eb);
  set_chart_axis_labels_from_hash (chart, eb);

  // we need to do this twice for the linechart... :p
  gog_object_update (GOG_OBJECT (graph));

//FIXME  add_pixbuf_graph_widget (eb, graph);

  g_debug("linechart rendered.");
  return TRUE;
}


static gboolean
handle_scatter( GncHtml* html, gpointer eb, gpointer unused )
{
  GogObject *graph, *chart;
  GogPlot *plot;
  GogSeries *series;
  GOData *sliceData;
  GogStyle *style;
  int datasize;
  double *xData, *yData;
  const gchar *marker_str, *color_str;
  gboolean fill = FALSE;

  {
    const char *datasizeStr, *xDataStr, *yDataStr;

    datasizeStr = gnc_html_get_embedded_param( eb, "datasize" );
    datasize = atoi( datasizeStr );

    xDataStr = gnc_html_get_embedded_param( eb, "x_data" );
    xData = read_doubles( xDataStr, datasize );

    yDataStr = gnc_html_get_embedded_param( eb, "y_data" );
    yData = read_doubles( yDataStr, datasize );

    marker_str = gnc_html_get_embedded_param(eb, "marker");
    color_str = gnc_html_get_embedded_param(eb, "color");
  }

  if (!create_basic_plot_elements("GogXYPlot", &graph, &chart, &plot))
  {
    return FALSE;
  }

  series = gog_plot_new_series( plot );
  style = gog_styled_object_get_style(GOG_STYLED_OBJECT(series));

  sliceData = go_data_vector_val_new( xData, datasize, NULL );
  gog_series_set_dim( series, 0, sliceData, NULL );
  go_data_emit_changed (GO_DATA (sliceData));

  sliceData = go_data_vector_val_new( yData, datasize, NULL );
  gog_series_set_dim( series, 1, sliceData, NULL );
  go_data_emit_changed (GO_DATA (sliceData));

  /* set marker shape */
  if (marker_str) {
    GOMarkerShape shape;

    if (g_str_has_prefix(marker_str, "filled ")) {
      fill = TRUE;
      marker_str += 7;
    }
    shape = go_marker_shape_from_str(marker_str);
    if (shape != GO_MARKER_NONE) {
      style->marker.auto_shape = FALSE;
      go_marker_set_shape(style->marker.mark, shape);
    } else {
      g_warning("cannot parse marker shape [%s]", marker_str);
    }
  }

  /* set marker and line colors */
  if (color_str) {
    GdkColor color;
    if (gdk_color_parse(color_str, &color)) {
      style->marker.auto_outline_color = FALSE;
      go_marker_set_outline_color(style->marker.mark, GDK_TO_UINT(color));
      style->line.auto_color = FALSE;
      style->line.color = GDK_TO_UINT(color);
    } else {
      g_warning("cannot parse color [%s]", color_str);
    }
  }

  /* set marker fill colors */
  if (fill) {
    style->marker.auto_fill_color = style->marker.auto_outline_color;
    go_marker_set_fill_color(style->marker.mark,
                             go_marker_get_outline_color(style->marker.mark));
  } else {
    GogStyle *chart_style =
      gog_styled_object_get_style(GOG_STYLED_OBJECT(chart));

    if (chart_style->fill.type == GOG_FILL_STYLE_PATTERN
        && chart_style->fill.pattern.pattern == GO_PATTERN_SOLID) {
      style->marker.auto_fill_color = FALSE;
      go_marker_set_fill_color(style->marker.mark,
                               chart_style->fill.pattern.back);
    } else if (chart_style->fill.type == GOG_FILL_STYLE_PATTERN
               && chart_style->fill.pattern.pattern
               == GO_PATTERN_FOREGROUND_SOLID) {
      style->marker.auto_fill_color = FALSE;
      go_marker_set_fill_color(style->marker.mark,
                               chart_style->fill.pattern.fore);
    } else {
      g_warning("fill color of markers can only be set like a solid fill "
                "pattern of the chart");
    }
  }

  set_chart_titles_from_hash(chart, eb);
  set_chart_axis_labels_from_hash(chart, eb);

  // And twice for the scatter, too... :p
  gog_object_update(GOG_OBJECT(graph));

//FIXME  add_pixbuf_graph_widget (eb, graph);

  return TRUE;
}

#if 0
#ifdef GTKHTML_USES_GTKPRINT
static void
draw_print_cb(GtkHTMLEmbedded *eb, cairo_t *cr, gpointer unused)
{
  GogGraph *graph = GOG_GRAPH(g_object_get_data(G_OBJECT(eb), "graph"));
#    ifdef HAVE_GOFFICE_0_5
  GogRenderer *rend = g_object_new(GOG_RENDERER_TYPE, "model", graph, NULL);
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
#endif

#else /* !GTKHTML_USES_GTKPRINT */
#if 0
static void
draw_print_cb(GtkHTMLEmbedded *eb, GnomePrintContext *context, gpointer unused)
{
  GogGraph *graph = GOG_GRAPH (g_object_get_data (G_OBJECT (eb), "graph"));

  /* assuming pixel size is 0.5, cf. gtkhtml/src/htmlprinter.c */
  gnome_print_scale (context, 0.5, 0.5);

  gnome_print_translate (context, 0, eb->height);
  gog_graph_print_to_gnome_print (graph, context, eb->width, eb->height);
}
#endif
#endif /* GTKHTML_USES_GTKPRINT */
