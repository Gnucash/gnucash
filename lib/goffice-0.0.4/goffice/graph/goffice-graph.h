/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * goffice-graph.h: 
 *
 * Copyright (C) 2003-2004 Jody Goldberg (jody@gnome.org)
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of version 2 of the GNU General Public
 * License as published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301
 * USA
 */

#ifndef GOFFICE_GRAPH_H
#define GOFFICE_GRAPH_H

#include <glib.h>

G_BEGIN_DECLS

typedef struct _GogObject	 GogObject;
typedef struct _GogObjectRole	 GogObjectRole;
typedef struct _GogView		 GogView;	 /* view of an Object */

typedef struct _GogGraph	 GogGraph;	/* collection of charts */
typedef struct _GogChart	 GogChart;	/* collection of plots */
typedef struct _GogPlot		 GogPlot;	/* abstract base for plots */
typedef struct _GogPlotType	 GogPlotType;	/* visible characterization */
typedef struct _GogPlotFamily	 GogPlotFamily; /* a group of plot types */
typedef struct _GogPlotDesc	 GogPlotDesc;	/* data/axis requirements */
typedef struct _GogSeries	 GogSeries;	/* single plotable entity */
typedef struct _GogSeriesDesc	 GogSeriesDesc; /* data requirements */
typedef struct _GogSeriesDimDesc GogSeriesDimDesc; /* dimension of a series */

/* Useful objects */
typedef struct _GogLegend	GogLegend;
typedef struct _GogLabel	GogLabel;
typedef struct _GogStyledObject	GogStyledObject;
typedef struct _GogAxis		GogAxis;
typedef struct _GogAxisLine	GogAxisLine;
typedef struct _GogGrid		GogGrid;
typedef struct _GogGridLine	GogGridLine;
typedef struct _GogErrorBar	GogErrorBar;
typedef struct _GogRegCurve	GogRegCurve;
typedef struct _GogRegEqn	GogRegEqn;
typedef struct _GogRegCurveType	GogRegCurveType;

/* formating */
typedef struct _GogTheme	GogTheme;
typedef struct _GogStyle	GogStyle;
typedef GSList 			GogSeriesElementStyleList;

/* Data */
typedef struct _GogDataAllocator GogDataAllocator;
typedef struct _GogDataset	 GogDataset;

/* Rendering */
typedef struct _GogRenderer	 GogRenderer;

typedef struct {
	double w, h;
} GogViewRequisition;

typedef struct {
	double w, h;
	double x, y;
} GogViewAllocation;

typedef struct {
	double wr, hb;
	double wl, ht;
} GogViewPadding;

typedef void (*GogEnumFunc) (unsigned i, GogStyle *style,
			     char const *name, gpointer data);

typedef enum {
	GOG_AXIS_UNKNOWN = -1,
	GOG_AXIS_X	 = 0,
	GOG_AXIS_Y,
	GOG_AXIS_Z,
	GOG_AXIS_CIRCULAR,
	GOG_AXIS_RADIAL,
	GOG_AXIS_PSEUDO_3D,
	GOG_AXIS_TYPES
} GogAxisType;
typedef enum {
	GOG_AXIS_SET_UNKNOWN 	  = -1,
	GOG_AXIS_SET_NONE   	  = 0,
	GOG_AXIS_SET_X		  = (1 << GOG_AXIS_X),
	GOG_AXIS_SET_XY		  = (1 << GOG_AXIS_X) | (1 << GOG_AXIS_Y),
	GOG_AXIS_SET_XY_pseudo_3d = (1 << GOG_AXIS_X) | (1 << GOG_AXIS_Y) | (1 << GOG_AXIS_PSEUDO_3D),
	GOG_AXIS_SET_XYZ	  = (1 << GOG_AXIS_X) | (1 << GOG_AXIS_Y) | (1 << GOG_AXIS_Z),
	GOG_AXIS_SET_RADAR        = (1 << GOG_AXIS_CIRCULAR) | (1 << GOG_AXIS_RADIAL),
	GOG_AXIS_SET_ALL	  = ((1 << GOG_AXIS_TYPES) -1)
} GogAxisSet;

typedef enum {
	GOG_DIM_INVALID = -1,
	GOG_DIM_LABEL = 0,
	GOG_DIM_INDEX,
	GOG_DIM_VALUE,
	GOG_DIM_MATRIX,
	GOG_DIM_TYPES
} GogDimType;

typedef enum {
	GOG_DATA_SCALAR,
	GOG_DATA_VECTOR,
	GOG_DATA_MATRIX
}	GogDataType;

/* A helper enum to simplify import/export from MS Excel (tm) which uses the
 * same logical dim names for all plot types.  Do _NOT_ reorder, or change the
 * enumeration without checking the xls code */
typedef enum {
	GOG_MS_DIM_LABELS	= 0,
	GOG_MS_DIM_VALUES	= 1,
	GOG_MS_DIM_CATEGORIES	= 2,
	GOG_MS_DIM_BUBBLES	= 3, /* undocumented */
	GOG_MS_DIM_TYPES,
	GOG_MS_DIM_ERR_plus1,	/* we made it up */
	GOG_MS_DIM_ERR_minus1,	/* we made it up */
	GOG_MS_DIM_ERR_plus2,	/* we made it up */
	GOG_MS_DIM_ERR_minus2,	/* we made it up */
	GOG_MS_DIM_START,	/* we made it up for dropbars*/
	GOG_MS_DIM_END,	/* we made it up for dropbars */
	GOG_MS_DIM_LOW,	/* we made it up for hi-lo*/
	GOG_MS_DIM_HIGH,	/* we made it up for hi-lo */
} GogMSDimType;

typedef enum {
	GOG_POSITION_AUTO	= 0,
	GOG_POSITION_N		= 1 << 0,	/* can be used with E or W */
	GOG_POSITION_S		= 1 << 1,	/* can be used with E or W */
	GOG_POSITION_E		= 1 << 2,
	GOG_POSITION_W		= 1 << 3,
	GOG_POSITION_COMPASS	= 0x0f,

	/* modifiers for compass */
	GOG_POSITION_ALIGN_FILL	  = 0 << 4,
	GOG_POSITION_ALIGN_START  = 1 << 4,
	GOG_POSITION_ALIGN_END	  = 2 << 4,
	GOG_POSITION_ALIGN_CENTER = 3 << 4,
	GOG_POSITION_ALIGNMENT	  = 0x30,

	GOG_POSITION_SPECIAL	= 1 << 6,

	GOG_POSITION_MANUAL	  = 1 << 7,
	GOG_POSITION_MANUAL_X_ABS = 1 << 8, /* abs vs relative pos */
	GOG_POSITION_MANUAL_Y_ABS = 1 << 9,
	GOG_POSITION_MANUAL_X_END = 1 << 10, /* pos relative to start or end */
	GOG_POSITION_MANUAL_Y_END = 1 << 11,

	/* modifiers for manual */
	GOG_POSITION_ANCHOR_NW       = 0 << 12,
	GOG_POSITION_ANCHOR_N	     = 1 << 12,
	GOG_POSITION_ANCHOR_NE	     = 2 << 12,
	GOG_POSITION_ANCHOR_E        = 3 << 12,
	GOG_POSITION_ANCHOR_SE       = 4 << 12,
	GOG_POSITION_ANCHOR_S        = 5 << 12,
	GOG_POSITION_ANCHOR_SW       = 6 << 12,
	GOG_POSITION_ANCHOR_W        = 7 << 12,
	GOG_POSITION_ANCHOR_CENTER   = 8 << 12,
	GOG_POSITION_ANCHOR          = 0xf000,
	
	GOG_POSITION_ANY_MANUAL      = 0xff80,
       	
	GOG_POSITION_PADDING	     = 1 << 16
} GogObjectPosition;

#define GOG_POSITION_IS_SPECIAL(pos) (((pos) & GOG_POSITION_SPECIAL)&&(!((pos) & GOG_POSITION_MANUAL)))
#define GOG_POSITION_IS_PADDING(pos) (((pos) & GOG_POSITION_PADDING)&&(!((pos) & GOG_POSITION_MANUAL)))

/* #define NO_DEBUG_CHARTS */
#ifndef NO_DEBUG_CHARTS
#define gog_debug(level, code)	do { if (goffice_graph_debug_level > level) { code } } while (0)
#else
#define gog_debug(level, code)
#endif
extern int goffice_graph_debug_level;

G_END_DECLS

#endif /* GOFFICE_GRAPH_H */
