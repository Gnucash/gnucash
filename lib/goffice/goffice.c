/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * goffice.c : a bogus little init file to pull all the parts together
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
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
 * USA
 */

#include <goffice/goffice-config.h>
#include <goffice/goffice.h>
#include "plugin-service.h"
#include "command-context.h"
#include "command-context-stderr.h"
#include <goffice/graph/gog-series.h>
#include <goffice/graph/gog-plot.h>
#include <goffice/graph/gog-plot-engine.h>
#include <goffice/graph/gog-chart.h>
#include <goffice/graph/gog-graph.h>
#include <goffice/graph/gog-axis.h>
#include <goffice/graph/gog-legend.h>
#include <goffice/graph/gog-label.h>
#include <goffice/graph/gog-grid.h>
#include <goffice/graph/gog-grid-line.h>
#include <goffice/graph/gog-theme.h>
#include <goffice/graph/gog-error-bar.h>
#include <goffice/graph/go-data-simple.h>
#include <goffice/utils/go-font.h>
#include <goffice/utils/go-math.h>
#include <gsf/gsf-utils.h>
#include "gnumeric-gconf.h"
#include "str.h"
#include "mstyle.h"

int goffice_graph_debug_level = 1;

void
libgoffice_init (void)
{
	GnmCmdContext *ctx;

	plugin_services_init();

	// effective: +libgoffice_init...
	go_font_init ();
	go_math_init ();
	gsf_init ();

	/* keep trigger happy linkers from leaving things out */
	gog_plugin_services_init ();
	(void) GOG_GRAPH_TYPE;
	(void) GOG_CHART_TYPE;
	(void) GOG_PLOT_TYPE;
	(void) GOG_SERIES_TYPE;
	(void) GOG_SERIES_ELEMENT_TYPE;
	(void) GOG_LEGEND_TYPE;
	(void) GOG_AXIS_TYPE;
	(void) GOG_LABEL_TYPE;
	(void) GOG_GRID_TYPE;
	(void) GOG_GRID_LINE_TYPE;
	(void) GOG_ERROR_BAR_TYPE;
	(void) GO_DATA_SCALAR_VAL_TYPE;
	(void) GO_DATA_SCALAR_STR_TYPE;
	gog_themes_init	();
	// -libgoffice_init

	gnm_string_init();
	mstyle_init();
	gnm_conf_init( FALSE );

	ctx = cmd_context_stderr_new();
	plugins_init( ctx );
}

void
libgoffice_shutdown (void)
{
	gog_themes_shutdown ();
	go_font_shutdown ();
	gog_plugin_services_shutdown ();
}
