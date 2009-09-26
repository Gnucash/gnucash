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
#include <string.h>

#include "gnc-html-graph-gog.h"
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
#if !WANT_WEBKIT && !defined(GTKHTML_USES_GTKPRINT)
#    include <goffice/graph/gog-renderer-gnome-print.h>
#endif
/* everything inside the following #ifndef can be safely removed when gnucash
requires libgoffice >= 0.7.5, the contents of the #else block must stay. */
#ifndef GOG_TYPE_GRAPH
#	define GOG_TYPE_GRAPH GOG_GRAPH_TYPE
#	define GO_TYPE_PLUGIN_LOADER_MODULE GO_PLUGIN_LOADER_MODULE_TYPE
#	define GOG_TYPE_RENDERER GOG_RENDERER_TYPE
#	include <goffice/graph/gog-style.h>
#	define GOStyle GogStyle
#	define go_styled_object_get_style gog_styled_object_get_style
#	define GO_STYLED_OBJECT GOG_STYLED_OBJECT
#	define GO_STYLE_FILL_PATTERN GOG_FILL_STYLE_PATTERN
#	define go_style_set_text_angle gog_style_set_text_angle
#else
#	include <goffice/utils/go-style.h>
#	include <goffice/utils/go-styled-object.h>
#endif
#ifndef GO_COLOR_FROM_GDK
#	define GO_COLOR_FROM_GDK GDK_TO_UINT
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
#define G_LOG_DOMAIN "gnc.html.graph.gog"

static gboolean create_basic_plot_elements(const char *plot_type, GogObject **out_graph, GogObject **out_chart, GogPlot **out_plot);

static void set_chart_titles(GogObject *chart, const char *title, const char* sub_title);
static void set_chart_axis_labels(GogObject *chart, const char *x_axis_label, const char* y_axis_label);

void
gnc_html_graph_gog_init( void )
{
	static gboolean initialized = FALSE;

	if( !initialized ) {
		g_debug( "init gog graphing" );

		libgoffice_init();
  
		/* Initialize plugins manager */
		go_plugins_init( NULL, NULL, NULL, NULL, TRUE, GO_TYPE_PLUGIN_LOADER_MODULE );

		initialized = TRUE;
	}
}

static GdkPixbuf*
create_graph_pixbuf( GogObject *graph, int width, int height )
{
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
	renderer = GOG_RENDERER(g_object_new( GOG_TYPE_RENDERER, "model", graph, NULL ));
	update_status = gog_renderer_update( renderer, width, height );
	buf = gog_renderer_get_pixbuf( renderer );
#elif defined(GOFFICE_WITH_CAIRO)
	cairo_renderer = GOG_RENDERER_CAIRO(g_object_new( GOG_RENDERER_CAIRO_TYPE,
													"model", graph,
													NULL ));
	update_status = gog_renderer_cairo_update( cairo_renderer, width, height, 1.0 );
	buf = gog_renderer_cairo_get_pixbuf( cairo_renderer );
#else
	pixbuf_renderer = GOG_RENDERER_PIXBUF(g_object_new( GOG_RENDERER_PIXBUF_TYPE,
														"model", graph,
														NULL));
	update_status = gog_renderer_pixbuf_update( pixbuf_renderer, width, height, 1.0 );
	buf = gog_renderer_pixbuf_get( pixbuf_renderer );
#endif

	g_object_set_data_full( G_OBJECT(buf), "graph", graph, g_object_unref );
	return buf;
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
GdkPixbuf*
gnc_html_graph_gog_create_piechart( GncHtmlPieChartInfo* info )
{
	GogObject *graph, *chart;
	GogPlot *plot;
	GogSeries *series;
	GOData *labelData, *sliceData;
	GdkPixbuf* pixbuf;

	if( !create_basic_plot_elements( "GogPiePlot", &graph, &chart, &plot ) ) {
		return NULL;
	}
	gog_object_add_by_name( chart, "Legend", NULL );

#ifdef GO_COLOR_BLACK
	GOG_STYLED_OBJECT(graph)->style->line.width = 5;
	GOG_STYLED_OBJECT(graph)->style->line.color = GO_COLOR_BLACK;
#else
	GOG_STYLED_OBJECT(graph)->style->outline.width = 5;
	GOG_STYLED_OBJECT(graph)->style->outline.color = RGBA_BLACK;
#endif

	series = gog_plot_new_series( plot );
	labelData = go_data_vector_str_new( (gchar const * const *)info->labels, info->datasize, NULL );
	gog_series_set_dim( series, 0, labelData, NULL );
	go_data_emit_changed( GO_DATA(labelData) );

	sliceData = go_data_vector_val_new( info->data, info->datasize, NULL );
	gog_series_set_dim( series, 1, sliceData, NULL );
	go_data_emit_changed( GO_DATA(sliceData) );

	// fixme: colors
	set_chart_titles( chart, info->title, info->subtitle );

	pixbuf = create_graph_pixbuf( graph, info->width, info->height );

	return pixbuf;
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
GdkPixbuf*
gnc_html_graph_gog_create_barchart( GncHtmlBarChartInfo* info )
{
	GogObject *graph, *chart;
	GogPlot *plot;
	GogSeries *series;
	GOStyle *style;
	GOData *label_data, *slice_data;
	char *bar_type = "normal";
	int bar_overlap = 0 /*percent*/; // seperate bars; no overlap.
	GdkPixbuf* pixbuf;

	if( !create_basic_plot_elements( "GogBarColPlot", &graph, &chart, &plot ) ) {
		return FALSE;
	}
	gog_object_add_by_name( chart, "Legend", NULL );

	if( info->stacked ) {
		// when stacked, we want the bars on _top_ of eachother.
		bar_type = "stacked";
		bar_overlap = 100 /*percent*/;
	}

	g_object_set( G_OBJECT(plot),
					//"vary_style_by_element",	TRUE,
					"type",                     bar_type,
					"overlap_percentage",       bar_overlap, 
					NULL);
	label_data = go_data_vector_str_new( (gchar const * const *)info->row_labels, info->data_rows, NULL );
	{
		// foreach row:
		//   series = row
		GdkColor color;
		int i;
		for( i = 0; i < info->data_cols; i++ ) {
			GError *err = NULL;

			series = gog_plot_new_series( plot );
			gog_object_set_name( GOG_OBJECT(series), info->col_labels[i], &err );
			if( err != NULL ) {
				g_warning( "error setting name [%s] on series [%d]: [%s]",
							info->col_labels[i], i, err->message);
			}

			g_object_ref( label_data );
			gog_series_set_dim( series, 0, label_data, NULL );
			go_data_emit_changed( GO_DATA(label_data) );

			slice_data = go_data_vector_val_new( info->data + (i*info->data_rows), info->data_rows, NULL );
			gog_series_set_dim( series, 1, slice_data, NULL );
			go_data_emit_changed( GO_DATA(slice_data) );

			style = go_styled_object_get_style( GO_STYLED_OBJECT(series) );
			style->fill.type = GO_STYLE_FILL_PATTERN;
			if( gdk_color_parse( info->col_colors[i], &color ) ) {
				style->fill.auto_back = FALSE;
				go_pattern_set_solid( &style->fill.pattern, GO_COLOR_FROM_GDK(color) );
			} else {
				g_warning( "cannot parse color [%s]", info->col_colors[i] );
			}
		}
	}

	if( info->rotate_row_labels ) {
		GogObject *object = gog_object_get_child_by_role(
									chart, gog_object_find_role_by_name( chart, "X-Axis" ) );
		style = go_styled_object_get_style( GO_STYLED_OBJECT(object) );
		go_style_set_text_angle( style, 90.0 );
	}

	set_chart_titles( chart, info->title, info->subtitle );
	set_chart_axis_labels( chart, info->x_axis_label, info->y_axis_label );

	// we need to do this twice for the barchart... :p
	gog_object_update( GOG_OBJECT(graph) );

	pixbuf = create_graph_pixbuf( graph, info->width, info->height );
	g_debug( "barchart rendered." );

	return pixbuf;
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
GdkPixbuf*
gnc_html_graph_gog_create_linechart( GncHtmlLineChartInfo* info )
{
	GogObject *graph, *chart;
	GogPlot *plot;
	GogSeries *series;
	GOStyle *style;
	GOData *label_data, *slice_data;
	gchar* line_type = "normal";
	GdkPixbuf* pixbuf;

	if( !create_basic_plot_elements( "GogLinePlot", &graph, &chart, &plot ) ) {
		return NULL;
	}
	gog_object_add_by_name( chart, "Legend", NULL );

	if( info->stacked ) {
		// when stacked, we want the lines on _top_ of eachother.
		line_type = "stacked";
	}

	g_object_set( G_OBJECT(plot),
					//"vary_style_by_element",   TRUE,
					"type",                      line_type,
					"default-style-has-markers", info->markers,
					NULL);
	label_data = go_data_vector_str_new( (gchar const * const *)info->row_labels, info->data_rows, NULL );
	{
		// foreach row:
		//   series = row
		GdkColor color;
		int i;
		for( i = 0; i < info->data_cols; i++ ) {
			GError *err = NULL;

			series = gog_plot_new_series( plot );
			gog_object_set_name( GOG_OBJECT(series), info->col_labels[i], &err );
			if( err != NULL ) {
				g_warning( "error setting name [%s] on series [%d]: [%s]",
							info->col_labels[i], i, err->message );
			}

			g_object_ref( label_data );
			gog_series_set_dim( series, 0, label_data, NULL );
			go_data_emit_changed( GO_DATA(label_data) );

			slice_data = go_data_vector_val_new( info->data + (i* info->data_rows),  info->data_rows, NULL );
			gog_series_set_dim( series, 1, slice_data, NULL );
			go_data_emit_changed( GO_DATA(slice_data) );

			style = go_styled_object_get_style( GO_STYLED_OBJECT(series) );
			style->fill.type = GO_STYLE_FILL_PATTERN;
			if( gdk_color_parse( info->col_colors[i], &color ) ) {
				style->fill.auto_back = FALSE;
				go_pattern_set_solid( &style->fill.pattern, GO_COLOR_FROM_GDK(color) );
			} else {
				g_warning( "cannot parse color [%s]", info->col_colors[i] );
			}
		}
	}

	if( info->rotate_row_labels ) {
		GogObject *object = gog_object_get_child_by_role(
								chart, gog_object_find_role_by_name( chart, "X-Axis" ) );
		style = go_styled_object_get_style( GO_STYLED_OBJECT(object) );
		go_style_set_text_angle( style, 90.0 );
	}

	if( info->major_grid ||  info->minor_grid ) {
		GogObject *object;
		gog_object_add_by_name( chart,"Grid", NULL );
		object = gog_object_get_child_by_role( chart,
										gog_object_find_role_by_name( chart, "Y-Axis" ) );
		if( info->major_grid ) {
			gog_object_add_by_name( GOG_OBJECT(object), "MajorGrid", NULL );
		}
		if( info->minor_grid ) {
			gog_object_add_by_name( GOG_OBJECT (object), "MinorGrid", NULL );
		}
	}

	set_chart_titles( chart, info->title, info->subtitle );
	set_chart_axis_labels( chart, info->x_axis_label, info->y_axis_label );

	// we need to do this twice for the linechart... :p
	gog_object_update( GOG_OBJECT(graph) );

	pixbuf = create_graph_pixbuf( graph, info->width, info->height );
	g_debug( "linechart rendered." );

	return pixbuf;
}

GdkPixbuf*
gnc_html_graph_gog_create_scatterplot( GncHtmlScatterPlotInfo* info )
{
	GogObject *graph, *chart;
	GogPlot *plot;
	GogSeries *series;
	GOData *sliceData;
	GOStyle *style;
	gboolean fill = FALSE;

	if( !create_basic_plot_elements( "GogXYPlot", &graph, &chart, &plot ) ) {
		return NULL;
	}

	series = gog_plot_new_series( plot );
	style = go_styled_object_get_style( GO_STYLED_OBJECT(series) );

	sliceData = go_data_vector_val_new( info->xData, info->datasize, NULL );
	gog_series_set_dim( series, 0, sliceData, NULL );
	go_data_emit_changed( GO_DATA(sliceData) );

	sliceData = go_data_vector_val_new( info->yData, info->datasize, NULL );
	gog_series_set_dim( series, 1, sliceData, NULL );
	go_data_emit_changed( GO_DATA(sliceData) );

	/* set marker shape */
	if( info->marker_str != NULL ) {
		GOMarkerShape shape;

		if( g_str_has_prefix( info->marker_str, "filled ") ) {
			fill = TRUE;
			info->marker_str += 7;
		}
		shape = go_marker_shape_from_str( info->marker_str );
		if( shape != GO_MARKER_NONE ) {
			style->marker.auto_shape = FALSE;
			go_marker_set_shape( style->marker.mark, shape );
		} else {
			g_warning( "cannot parse marker shape [%s]", info->marker_str );
		}
	}

	/* set marker and line colors */
	if( info->color_str != NULL ) {
		GdkColor color;
		if( gdk_color_parse( info->color_str, &color ) ) {
			style->marker.auto_outline_color = FALSE;
			go_marker_set_outline_color( style->marker.mark, GO_COLOR_FROM_GDK(color) );
			style->line.auto_color = FALSE;
			style->line.color = GO_COLOR_FROM_GDK(color);
		} else {
			g_warning( "cannot parse color [%s]", info->color_str );
		}
	}

	/* set marker fill colors */
	if( fill ) {
		style->marker.auto_fill_color = style->marker.auto_outline_color;
		go_marker_set_fill_color( style->marker.mark,
								go_marker_get_outline_color( style->marker.mark ) );
	} else {
		GOStyle *chart_style = go_styled_object_get_style( GO_STYLED_OBJECT(chart) );

		if( chart_style->fill.type == GO_STYLE_FILL_PATTERN
				&& chart_style->fill.pattern.pattern == GO_PATTERN_SOLID ) {
			style->marker.auto_fill_color = FALSE;
			go_marker_set_fill_color( style->marker.mark, chart_style->fill.pattern.back );
		} else if( chart_style->fill.type == GO_STYLE_FILL_PATTERN
				&& chart_style->fill.pattern.pattern == GO_PATTERN_FOREGROUND_SOLID ) {
			style->marker.auto_fill_color = FALSE;
			go_marker_set_fill_color( style->marker.mark, chart_style->fill.pattern.fore );
		} else {
			g_warning( "fill color of markers can only be set like a solid fill "
						"pattern of the chart" );
		}
	}

	set_chart_titles( chart, info->title, info->subtitle );
	set_chart_axis_labels( chart, info->x_axis_label, info->y_axis_label );

	// And twice for the scatter, too... :p
	gog_object_update( GOG_OBJECT(graph) );

	return create_graph_pixbuf( graph, info->width, info->height );
}
