#include "config.h"

#include <string.h>

#include <gtkhtml/gtkhtml.h>
#include <gtkhtml/gtkhtml-embedded.h>

#include "gnc-html-graph-gog.h"
#include "gnc-html.h"
#include "gnc-trace.h"

#include "goffice.h"
#include "graph/gog-graph.h"
#include "graph/gog-object.h"
#include "graph/gog-plot.h"
#include "graph/gog-series.h"
#include "graph/go-data-simple.h"
#include "utils/go-color.h"
#include "graph/gog-renderer-pixbuf.h"
#include "graph/gog-renderer-svg.h"
#include "graph/gog-data-set.h"
#include "graph/gog-styled-object.h"
#include "graph/gog-style.h"

#include <gsf/gsf.h>
#include <gsf/gsf-output-memory.h>

static short module = MOD_GUI;

static int handle_piechart(gnc_html * html, GtkHTMLEmbedded * eb, gpointer d);
static int handle_barchart(gnc_html * html, GtkHTMLEmbedded * eb, gpointer d);
static int handle_scatter(gnc_html * html, GtkHTMLEmbedded * eb, gpointer d);

void
gnc_html_graph_gog_init(void) {

  PINFO( "init gog graphing" );
  
  libgoffice_init();

  gnc_html_register_object_handler( "gnc-guppi-pie", handle_piechart );
  gnc_html_register_object_handler( "gnc-guppi-bar", handle_barchart );
  gnc_html_register_object_handler( "gnc-guppi-scatter", handle_scatter );
}

static double * 
read_doubles(const char * string, int nvalues) {
  int    n;
  int    choffset=0;
  int    accum = 0;
  double * retval = g_new0(double, nvalues);

  //gnc_push_locale ("C");

  for(n=0; n<nvalues; n++) {
    sscanf(string+accum, "%le%n", &retval[n], &choffset);
    accum += choffset;
  }

  //gnc_pop_locale ();

  return retval;
}

static char ** 
read_strings(const char * string, int nvalues) {
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
free_strings(char ** strings, int nstrings) {
  int count;

  if (!strings) return;

  for (count=0; count < nstrings; count++) {
    g_free(strings[count]);
    strings[count] = NULL;
  }
  g_free(strings);
}

static void
addPixbufGraphWidget( GtkHTMLEmbedded *eb, GogObject *graph )
{
  GtkWidget *widget;
  GogRenderer *pixbufRend;
  GdkPixbuf *buf;
  gboolean updateStatus;

  // Note that this shouldn't be necessary as per discussion with Jody...
  // ... but it is because we don't embed in a control which passes the
  // update requests back to the graph widget, a-la the foo-canvas that
  // gnumeric uses.  We probably _should_ do something like that, though.
  gog_object_update( GOG_OBJECT(graph) );

#if 0
  // example SVG use.  Also, nice for debugging.
  {
    GsfOutput *mem;
    gboolean output;

    mem = gsf_output_memory_new();
    output = gog_graph_export_to_svg( graph, mem, eb->width, eb->height, 1. );
    printf( "svg: [%s]\n", (guchar*)gsf_output_memory_get_bytes( GSF_OUTPUT_MEMORY(mem) ) );
  }
#endif // 0

  pixbufRend = g_object_new( GOG_RENDERER_PIXBUF_TYPE,
                             "model", graph,
                             NULL );
  updateStatus = gog_renderer_pixbuf_update( GOG_RENDERER_PIXBUF(pixbufRend), eb->width, eb->height, 1. );
  buf = gog_renderer_pixbuf_get(GOG_RENDERER_PIXBUF(pixbufRend));
  widget = gtk_image_new_from_pixbuf( buf );
  gtk_widget_set_size_request( widget, eb->width, eb->height );
  gtk_widget_show_all( widget );
  gtk_container_add( GTK_CONTAINER(eb), widget );
  // blindly copied from gnc-html-guppi.c
  gtk_widget_set_usize(GTK_WIDGET(eb), eb->width, eb->height);
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
static int
handle_piechart(gnc_html * html, GtkHTMLEmbedded * eb, gpointer d)
{
  GogObject *graph, *chart, *tmp, *legend;
  GogPlot *plot;
  GogSeries *series;
  GOData *labelData, *sliceData;
  int datasize;
  double *data = NULL;
  char **labels = NULL, **colors = NULL;

  // First, parse data from the text-ized params.
  {
    char *datasizeStr, *dataStr, *labelsStr, *colorStr;

    datasizeStr = g_hash_table_lookup(eb->params, "datasize");
    dataStr = g_hash_table_lookup(eb->params, "data" );
    labelsStr = g_hash_table_lookup(eb->params, "labels");
    colorStr = g_hash_table_lookup(eb->params, "colors");
    g_return_val_if_fail( datasizeStr != NULL
                          && dataStr != NULL
                          && labelsStr != NULL
                          && colorStr != NULL, FALSE );
    sscanf( datasizeStr, "%d", &datasize );
    data = read_doubles( dataStr, datasize );
    labels = read_strings( labelsStr, datasize );
    colors = read_strings( colorStr, datasize );
  }

  graph = g_object_new( GOG_GRAPH_TYPE, NULL );
  GOG_STYLED_OBJECT(graph)->style->outline.width = 5;
  GOG_STYLED_OBJECT(graph)->style->outline.color = RGBA_BLACK;

  chart = gog_object_add_by_name( graph, "Chart", NULL );
  plot = gog_plot_new_by_name( "GogPiePlot" );
  if ( !plot )
  {
    // FIXME - log betterer
    printf( "plugin not loaded" );
    return FALSE;
  }
  g_object_set (G_OBJECT (plot),
		NULL);
  gog_object_add_by_name( chart, "Plot", GOG_OBJECT(plot) );
  series = gog_plot_new_series( plot );
  labelData = go_data_vector_str_new( labels, datasize );
  gog_series_set_dim( series, 0, labelData, NULL );
  go_data_emit_changed (GO_DATA (labelData));

  sliceData = go_data_vector_val_new( data, datasize );
  gog_series_set_dim( series, 1, sliceData, NULL );
  go_data_emit_changed (GO_DATA (sliceData));

  // fixme: colors
  {
    char *titleParam;
    GOData *title;

    titleParam = g_hash_table_lookup( eb->params, "title" );

    tmp = gog_object_add_by_name (chart, "Title", NULL);
    gog_object_set_pos (tmp, GOG_POSITION_N | GOG_POSITION_ALIGN_START);
    title = go_data_scalar_str_new (titleParam, FALSE);
    gog_dataset_set_dim (GOG_DATASET (tmp), 0, title, NULL);
  }

  legend = gog_object_add_by_name( chart, "Legend", NULL );

  addPixbufGraphWidget( eb, graph );

  PINFO( "piechart rendered" );
  return TRUE;
}

/**
 * datarows:int
 * datacols:int
 * data:doubles[], datarows*datacols
 * x_axis_label:string
 * y_axis_label:string
 * col_labels:string[]
 * row_labels:string[]
 * col_colors:string
 * rotate_row_labels:boolean
 * stacked:boolean
 **/
static int
handle_barchart(gnc_html * html, GtkHTMLEmbedded * eb, gpointer d)
{
  GogObject *graph, *chart, *tmp, *legend;
  GogPlot *plot;
  GogSeries *series;
  GOData *labelData, *sliceData;
  int datarows, datacols;
  double *data = NULL;
  char **col_labels = NULL, **row_labels = NULL, **col_colors = NULL;
  //char *x_axis_label, *y_axis_label;
  //gboolean rotate_row_labels;
  gboolean stacked = FALSE;
  char *barType = "normal";
  int barOverlap = 0 /*percent*/; // seperate bars; no overlap.

  // First, parse data from the text-ized params.
  {
    char *datarowsStr, *datacolsStr, *dataStr, *colLabelsStr, *rowLabelsStr, *colColorsStr, *stackedStr;
    gint stackedInt;

    datarowsStr  = g_hash_table_lookup(eb->params, "data_rows");
    datacolsStr  = g_hash_table_lookup(eb->params, "data_cols");
    dataStr      = g_hash_table_lookup(eb->params, "data" );
    colLabelsStr = g_hash_table_lookup(eb->params, "col_labels");
    rowLabelsStr = g_hash_table_lookup(eb->params, "row_labels");
    colColorsStr = g_hash_table_lookup(eb->params, "col_colors");
    stackedStr = NULL;
    stackedStr   = g_hash_table_lookup(eb->params, "stacked");
    sscanf( stackedStr, "%d", &stackedInt );
    stacked = (gboolean)stackedInt;

#if 0 // too strong at the moment.
    g_return_val_if_fail( datarowsStr != NULL
                          && datacolsStr != NULL
                          && dataStr != NULL
                          && colLabelsStr != NULL
                          && rowLabelsStr != NULL
                          && colColorsStr != NULL, FALSE );
#endif // 0
    sscanf( datarowsStr, "%d", &datarows );
    sscanf( datacolsStr, "%d", &datacols );
    data = read_doubles( dataStr, datarows*datacols );
    row_labels = read_strings( rowLabelsStr, datarows );
    col_labels = read_strings( colLabelsStr, datacols );
    col_colors = read_strings( colColorsStr, datacols );
  }

  graph = g_object_new( GOG_GRAPH_TYPE, NULL );
  chart = gog_object_add_by_name( graph, "Chart", NULL );
  // series => bars [gnc:cols]
  // elements => segments [gnc:rows]
  plot = gog_plot_new_by_name( "GogBarColPlot" );
  if ( !plot )
  {
    // FIXME - log betterer
    printf( "plugin not loaded" );
    return FALSE;
  }
  if ( stacked )
  {
    barType = "stacked";
    barOverlap = 100 /*percent*/;
    // when stacked, we want the bars on _top_ of eachother.
  }
  g_object_set (G_OBJECT (plot),
                "vary_style_by_element",	TRUE,
                "type",                         barType,
                "overlap_percentage",           barOverlap, 
		NULL);
  gog_object_add_by_name( chart, "Plot", GOG_OBJECT(plot) );

  labelData = go_data_vector_str_new(  (char const * const *)col_labels, datacols );
  {
    int i;
    // foreach row:
    //   series = row

    for ( i = 0; i < datacols; i++ )
    {
      series = gog_plot_new_series( plot );

      g_object_ref( labelData );
      gog_series_set_dim( series, 0, labelData, NULL );
      go_data_emit_changed (GO_DATA (labelData));

      sliceData = go_data_vector_val_new( data + (i*datarows), datarows );
      gog_series_set_dim( series, 1, sliceData, NULL );
      go_data_emit_changed (GO_DATA (sliceData));
    }
  }

  // fixme: colors
  {
    char *titleParam;
    GOData *title;

    titleParam = g_hash_table_lookup( eb->params, "title" );

    tmp = gog_object_add_by_name (chart, "Title", NULL);
    gog_object_set_pos (tmp, GOG_POSITION_N | GOG_POSITION_ALIGN_START);
    title = go_data_scalar_str_new (titleParam, FALSE);
    gog_dataset_set_dim (GOG_DATASET (tmp), 0, title, NULL);
  }

  legend = gog_object_add_by_name( chart, "Legend", NULL );

  // we need to do this twice for the barchart... :p
  gog_object_update( GOG_OBJECT(graph) );

  addPixbufGraphWidget( eb, graph );

  PINFO( "barchart rendered." );
  return TRUE;
}

static int
handle_scatter(gnc_html * html, GtkHTMLEmbedded * eb, gpointer d)
{
  GogObject *graph, *chart, *legend;
  GogPlot *plot;
  GogSeries *series;
  GOData *sliceData;
  char *title, *subtitle, *xAxisLabel, *yAxisLabel;
  int datasize;
  double *xData, *yData;

  {
    char *datasizeStr, *xDataStr, *yDataStr;

    title = g_hash_table_lookup( eb->params, "title" );
    subtitle = g_hash_table_lookup( eb->params, "subtitle" );

    datasizeStr = g_hash_table_lookup( eb->params, "datasize" );
    sscanf( datasizeStr, "%d", &datasize );

    xDataStr = g_hash_table_lookup( eb->params, "x_data" );
    xData = read_doubles( xDataStr, datasize );

    yDataStr = g_hash_table_lookup( eb->params, "y_data" );
    yData = read_doubles( yDataStr, datasize );

    xAxisLabel = g_hash_table_lookup( eb->params, "x_axis_label" );
    yAxisLabel = g_hash_table_lookup( eb->params, "y_axis_label" );
  }

  graph = g_object_new( GOG_GRAPH_TYPE, NULL );
  chart = gog_object_add_by_name( graph, "Chart", NULL );
  plot = gog_plot_new_by_name( "GogXYPlot" );
  if ( !plot )
  {
    // FIXME - log betterer
    printf( "plugin not loaded" );
    return FALSE;
  }
  //g_object_set (G_OBJECT (plot), NULL);
  gog_object_add_by_name( chart, "Plot", GOG_OBJECT(plot) );

  series = gog_plot_new_series( plot );

  sliceData = go_data_vector_val_new( xData, datasize );
  gog_series_set_dim( series, 0, sliceData, NULL );
  go_data_emit_changed (GO_DATA (sliceData));

  sliceData = go_data_vector_val_new( yData, datasize );
  gog_series_set_dim( series, 1, sliceData, NULL );
  go_data_emit_changed (GO_DATA (sliceData));

  // fixme: colors
  // fixme: title, subtitle
  // fixme: axis labels

  //legend = gog_object_add_by_name( chart, "Legend", NULL );

  // And twice for the scatter, too... :p
  gog_object_update( GOG_OBJECT(graph) );

  addPixbufGraphWidget( eb, graph );

  PINFO( "scatter rendered." );

  return TRUE;
}
