/********************************************************************
 * gnc-html_graph_gog.h -- display html with gnc special            *
 *									tags			                *
 * Copyright (C) 2000 Bill Gribble <grib@billgribble.com>           *
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
\********************************************************************/

#ifndef GNC_HTML_GRAPH_GOG_H
#define GNC_HTML_GRAPH_GOG_H 1

typedef struct
{
    gint width;
    gint height;
    const gchar* title;
    const gchar* subtitle;
    gint datasize;
    gdouble* data;
    gchar** labels;
    gchar** colors;
} GncHtmlPieChartInfo;

typedef struct
{
    gint width;
    gint height;
    const gchar* title;
    const gchar* subtitle;
    gint data_rows;
    gint data_cols;
    gdouble* data;
    gchar** col_labels;
    gchar** row_labels;
    gchar** col_colors;
    const gchar* x_axis_label;
    const gchar* y_axis_label;
    gboolean rotate_row_labels;
    gboolean stacked;
} GncHtmlBarChartInfo;

typedef struct
{
    gint width;
    gint height;
    const gchar* title;
    const gchar* subtitle;
    gint data_rows;
    gint data_cols;
    gdouble* data;
    gchar** col_labels;
    gchar** row_labels;
    gchar** col_colors;
    gboolean rotate_row_labels;
    gboolean stacked;
    gboolean markers;
    gboolean major_grid;
    gboolean minor_grid;
    const gchar* x_axis_label;
    const gchar* y_axis_label;
} GncHtmlLineChartInfo;

typedef struct
{
    gint width;
    gint height;
    const gchar* title;
    const gchar* subtitle;
    const gchar* x_axis_label;
    const gchar* y_axis_label;
    gint datasize;
    gdouble* xData;
    gdouble* yData;
    const gchar* marker_str;
    const gchar* color_str;
} GncHtmlScatterPlotInfo;

void gnc_html_graph_gog_init( void );
GdkPixbuf* gnc_html_graph_gog_create_piechart( GncHtmlPieChartInfo* info );
GdkPixbuf* gnc_html_graph_gog_create_barchart( GncHtmlBarChartInfo* info );
GdkPixbuf* gnc_html_graph_gog_create_linechart( GncHtmlLineChartInfo* info );
GdkPixbuf* gnc_html_graph_gog_create_scatterplot( GncHtmlScatterPlotInfo* info );

#endif /* GNC_HTML_GRAPH_GOG_H */
