/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * go-line.c :
 *
 * Copyright (C) 2004 Emmanuel Pacaud (emmanuel.pacaud@univ-poitiers.fr)
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

#include "go-color.h"
#include "go-line.h"

#ifdef WITH_GTK
#include <goffice/gui-utils/go-combo-pixmaps.h>
#endif

#include <string.h>
#include <glib/gi18n.h>

typedef struct {
	int 		 n_dash;
	double 		 dash[6];
} GOLineDashDesc;

static const GOLineDashDesc line_dash_desc =		{2,	{ 18, 6 } };
static const GOLineDashDesc line_dot_desc = 		{2,	{ 3, 3 } };
static const GOLineDashDesc line_dash_dot_desc =	{4, 	{ 9, 3, 6, 3 } };
static const GOLineDashDesc line_dash_dot_dot_desc =  {6, 	{ 9, 3, 3, 3, 3, 3 } };	

struct {
	GOLineDashType type;
	char const *label;
	char const *name;
	const GOLineDashDesc *dash_desc;
} line_dashes[GO_LINE_MAX] = 
{
	{ GO_LINE_NONE,		N_("None"), 		"none",		NULL },
	{ GO_LINE_SOLID,	N_("Solid"),		"solid",	NULL },
	{ GO_LINE_DASH,		N_("Dash"), 		"dash",		&line_dash_desc },
	{ GO_LINE_DOT,		N_("Dot"),		"dot",		&line_dot_desc},
	{ GO_LINE_DASH_DOT,	N_("Dash dot"),		"dash-dot",	&line_dash_dot_desc },
	{ GO_LINE_DASH_DOT_DOT,	N_("Dash dot dot"),	"dash-dot-dot",	&line_dash_dot_dot_desc }
};
	
GOLineDashType
go_line_dash_from_str (char const *name)
{
	unsigned i;
	GOLineDashType ret = GO_LINE_NONE;

	for (i = 0; i < GO_LINE_MAX; i++) {
		if (strcmp (line_dashes[i].name, name) == 0) {
			ret = line_dashes[i].type;
			break;
		}
	}
	return ret;
}

char const *
go_line_dash_as_str (GOLineDashType type)
{
	unsigned i;
	char const *ret = "none";

	for (i = 0; i < GO_LINE_MAX; i++) {
		if (line_dashes[i].type == type) {
			ret = line_dashes[i].name;
			break;
		}
	}
	return ret;
}

void
go_line_vpath_dash_free (ArtVpathDash *dash)
{
	if (dash != NULL)
		g_free (dash->dash);
	g_free (dash);
}

ArtVpathDash *
go_line_get_vpath_dash (GOLineDashType type, double scale)
{
	int i;
	ArtVpathDash *dash = NULL;
	const GOLineDashDesc *dash_desc;

	if (type < 0 || type >= G_N_ELEMENTS (line_dashes))
		return NULL;
	
	dash_desc = line_dashes[type].dash_desc;
	if (dash_desc != NULL) {
		dash = g_new (ArtVpathDash, 1);
		dash->offset = 0.5;
		dash->n_dash = dash_desc->n_dash;
		dash->dash = g_new (double, dash->n_dash);
		for (i = 0; i < dash->n_dash; i++) 
			dash->dash[i] = scale * dash_desc->dash[i];
	}

	return dash;
}

/* Liang-Barsky line clipping */
ArtVpath *
go_line_clip_vpath (ArtVpath const *vpath, GogViewAllocation const *clip_area)
{
	double x1, y1, x2, y2;
	double p[4], q[4], r, t1 = 0., t2 = 1., delta_x, delta_y;
	double x_min, x_max, y_min, y_max;
	unsigned i = 0, j;
	gboolean clip_last, clip_first;
	ArtVpath *result_path;
	int n_result, n_result_max;
	
	x_min = clip_area->x;
	x_max = x_min + clip_area->w;
	y_min = clip_area->y;
	y_max = y_min + clip_area->h;

	n_result = 0;
	n_result_max = 16;
	result_path = art_new (ArtVpath, n_result_max);

	/* TODO clip_first computation isn't needed if previous clip_last was FALSE */
	while (vpath[i].code != ART_END) {
		gboolean reject = FALSE;
		clip_last = TRUE;
		while (vpath[i+1].code == ART_LINETO) {

			t1 = 0.;
			t2 = 1.;

			x1 = vpath[i].x;
			y1 = vpath[i].y;
			x2 = vpath[i+1].x;
			y2 = vpath[i+1].y;

			delta_x = x2 - x1;
			delta_y = y2 - y1;

			p[0] = - delta_x;
			q[0] = x1 - x_min;
			p[1] = delta_x;
			q[1] = x_max - x1;
			p[2] = - delta_y;
			q[2] = y1 - y_min;
			p[3] = delta_y;
			q[3] = y_max - y1;

			clip_last = FALSE;
			clip_first = FALSE;

			for (j = 0; j < 4; j++) {
				if (p[j] < 0.) {
					r =  q[j] / p[j];
					if (r > t1) {
						t1 = r;
						clip_first = TRUE;
					} 
				}
				else if (p[j] > 0.) {
					r =  q[j] / p[j];
					if (r < t2) {
						t2 = r;
						clip_last = TRUE;
					} 
				}
			}

			if (t1 <= t2) {
				reject = FALSE;
				if (clip_first) 
					art_vpath_add_point (&result_path, &n_result, &n_result_max,
							     ART_MOVETO,
							     x1 + t1 * delta_x,
							     y1 + t1 * delta_y);
				else
					art_vpath_add_point (&result_path, &n_result, &n_result_max,
							     vpath[i].code,
							     vpath[i].x,
							     vpath[i].y);
				if (clip_last)
					art_vpath_add_point (&result_path, &n_result, &n_result_max,
							     ART_LINETO,
							     x1 + t2 * delta_x,
							     y1 + t2 * delta_y);
			} else 
				reject = TRUE;		     
			
			i++;
		}
		if (!clip_last && !reject)
			art_vpath_add_point (&result_path, &n_result, &n_result_max,
					     ART_LINETO, vpath[i].x, vpath[i].y);

		i++;
	}
	art_vpath_add_point (&result_path, &n_result, &n_result_max,
			     ART_END, 0., 0.);

	return result_path;
}

ArtVpath *
go_line_dash_vpath (ArtVpath const *path, 
		    ArtVpathDash const *dash,
		    GogViewAllocation const *bbox)
{
	ArtVpath *dashed;

	if (dash == NULL) 
		return NULL;

	if (bbox != NULL) {
		ArtVpath *clipped = go_line_clip_vpath (path, bbox);
		dashed = art_vpath_dash (clipped, dash);
		g_free (clipped);
	} else
		dashed = art_vpath_dash (path, dash);

	return dashed;
}
					 
#ifdef WITH_GTK

#define LINE_SAMPLE_HEIGHT	5
#define LINE_SAMPLE_WIDTH	60

gpointer	 
go_line_dash_selector (GOLineDashType default_type)
{
	static GOLineDashType const elements[] = {
		GO_LINE_NONE,
		GO_LINE_SOLID,
		GO_LINE_DASH,
		GO_LINE_DOT,
		GO_LINE_DASH_DOT,
		GO_LINE_DASH_DOT_DOT,
		GO_LINE_MAX
	};

	unsigned	 i;
	gboolean	 is_auto;
	GOComboPixmaps	*w;
	GdkPixbuf	*pixbuf;
	ArtVpathDash	*dash;
	GOLineDashType	 dash_type;
	ArtVpath	 line[3], *path;
	ArtSVP		*svp;
	GogViewAllocation bbox;

	bbox.x = 0;
	bbox.y = 0;
	bbox.w = LINE_SAMPLE_WIDTH;
	bbox.h = LINE_SAMPLE_HEIGHT;

	line[0].code = ART_MOVETO;
	line[1].code = ART_LINETO;
	line[2].code = ART_END;
	line[0].x = 0.5;
	line[1].x = LINE_SAMPLE_WIDTH - 0.5;
	line[0].y = line[1].y = LINE_SAMPLE_HEIGHT / 2.0;

	w = go_combo_pixmaps_new (1);
	for (i = 0; i < G_N_ELEMENTS (elements); i++) {
		pixbuf = gdk_pixbuf_new (GDK_COLORSPACE_RGB, TRUE, 8, 
					 LINE_SAMPLE_WIDTH, LINE_SAMPLE_HEIGHT);
		gdk_pixbuf_fill (pixbuf, 0); /* in case the fill colours have alpha = 0 */
		is_auto = elements[i] == GO_LINE_MAX;
		dash_type = is_auto ? default_type : i;
		if (dash_type != GO_LINE_NONE) {
			dash = go_line_get_vpath_dash (dash_type, 1.0);
			path = dash != NULL ? art_vpath_dash (line, dash) : line;
			svp = art_svp_vpath_stroke (path,
						    ART_PATH_STROKE_JOIN_MITER, ART_PATH_STROKE_CAP_ROUND,
						    1.0, 4.0, 0.5);
			if (dash != NULL) {
				go_line_vpath_dash_free (dash);
				g_free (path);
			}
			go_color_render_svp (0x000000FF, svp, 0, 0, LINE_SAMPLE_WIDTH, LINE_SAMPLE_HEIGHT,
					     gdk_pixbuf_get_pixels (pixbuf),
					     gdk_pixbuf_get_rowstride (pixbuf));
			art_svp_free (svp);
		}
		if (is_auto) {
			/* xgettext : this will appear as 'Automatic (patternname)' */
			char *name = g_strdup_printf (_("Automatic (%s)"),
						      _(line_dashes [default_type].label));
			go_combo_pixmaps_add_element (w, pixbuf, -default_type, name);
			g_free (name);
		} else
			go_combo_pixmaps_add_element (w, pixbuf, dash_type,
						      _(line_dashes[dash_type].label));
	}
	return w;
}
#endif /*WITH_GTK*/
