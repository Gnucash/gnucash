/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gog-renderer-svg.c :
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
#include <goffice/graph/gog-renderer-svg.h>
#include <goffice/graph/gog-renderer-impl.h>
#include <goffice/graph/gog-style.h>
#include <goffice/graph/gog-view.h>
#include <goffice/utils/go-color.h>
#include <goffice/utils/go-font.h>
#include <goffice/utils/go-marker.h>
#include <goffice/utils/go-units.h>

#include <gsf/gsf-libxml.h>
#include <gsf/gsf-impl-utils.h>
#include <pango/pangoft2.h>

#include <libxml/tree.h>

#include <locale.h>
#include <math.h>

#define CC2XML(s) ((const xmlChar *)(s))

#define GOG_RENDERER_SVG_TYPE	(gog_renderer_svg_get_type ())
#define GOG_RENDERER_SVG(o)	(G_TYPE_CHECK_INSTANCE_CAST ((o), GOG_RENDERER_SVG_TYPE, GogRendererSvg))
#define IS_GOG_RENDERER_SVG(o)	(G_TYPE_CHECK_INSTANCE_TYPE ((o), GOG_RENDERER_SVG_TYPE))

typedef struct _GogRendererSvg GogRendererSvg;

struct _GogRendererSvg {
	GogRenderer base;

	xmlDocPtr doc;
	xmlNodePtr defs;
	xmlNodePtr current_node;
	GHashTable *table;
	gint grad, pat, img;
	unsigned clip_counter;

	PangoContext *pango_context;
};

typedef GogRendererClass GogRendererSvgClass;

static GObjectClass *parent_klass;

static GType gog_renderer_svg_get_type (void);

static void
gog_renderer_svg_finalize (GObject *obj)
{
	GogRendererSvg *prend = GOG_RENDERER_SVG (obj);

	if (prend->pango_context != NULL) {
		g_object_unref (prend->pango_context);
		prend->pango_context = NULL;
	}

	(*parent_klass->finalize) (obj);
}

static void
gog_renderer_svg_clip_push (GogRenderer *rend, GogRendererClip *clip)
{
	GogRendererSvg *prend = GOG_RENDERER_SVG (rend);
	char *buf;
	xmlNodePtr child;
	xmlNodePtr node;
	char *old_num_locale = g_strdup (setlocale (LC_NUMERIC, NULL));

	prend->clip_counter++;
	
	setlocale (LC_NUMERIC, "C");
	node = xmlNewDocNode (prend->doc, NULL, CC2XML("clipPath"), NULL);
	xmlAddChild (prend->defs, node);
	buf = g_strdup_printf ("clip%i", prend->clip_counter);
	xmlNewProp (node, CC2XML("id"), CC2XML(buf));
	g_free (buf);
	child = xmlNewDocNode (prend->doc, NULL, CC2XML("rect"), NULL);
	xmlAddChild (node, child);
	buf = g_strdup_printf ("%g", clip->area.x);
	xmlNewProp (child, CC2XML("x"), CC2XML(buf));
	g_free (buf);
	buf = g_strdup_printf ("%g", clip->area.y);
	xmlNewProp (child, CC2XML("y"), CC2XML(buf));
	g_free (buf);
	buf = g_strdup_printf ("%g", clip->area.w);
	xmlNewProp (child, CC2XML("width"), CC2XML(buf));
	g_free (buf);
	buf = g_strdup_printf ("%g", clip->area.h);
	xmlNewProp (child, CC2XML("height"), CC2XML(buf));
	g_free (buf);
	
	node = xmlNewDocNode (prend->doc, NULL, CC2XML("g"), NULL);
	xmlAddChild (prend->current_node, node);
	buf = g_strdup_printf ("url(#clip%i)", prend->clip_counter);
	xmlNewProp (node, CC2XML ("clip-path"), CC2XML (buf));
	g_free (buf);
	setlocale (LC_NUMERIC, old_num_locale);
	g_free (old_num_locale);

	prend->current_node = node;
}

static void
gog_renderer_svg_clip_pop (GogRenderer *rend, GogRendererClip *clip)
{
	GogRendererSvg *prend = GOG_RENDERER_SVG (rend);
	
	prend->current_node = prend->current_node->parent;
}

static void
draw_path (GogRendererSvg *prend, ArtVpath const *path, GString *string)
{
	for ( ; path->code != ART_END ; path++)
		switch (path->code) {
		case ART_MOVETO_OPEN :
		case ART_MOVETO :
			g_string_append_printf (string, "M%g %g", path->x, path->y);
			break;
		case ART_LINETO :
			g_string_append_printf (string, "L%g %g", path->x, path->y);
			break;
		default :
			break;
		}
}

static void
stroke_dasharray (xmlNodePtr node, ArtVpathDash *dash)
{
	GString *string;
	int i;

	if (dash == NULL || dash->n_dash < 1)
		return;

	string = g_string_new ("");
	for (i = 0; i < dash->n_dash; i++) 
		g_string_append_printf (string, i == 0 ? "%g" : " %g", dash->dash[i]);
	xmlNewProp (node, CC2XML ("stroke-dasharray"), CC2XML (string->str));
	g_string_free (string, TRUE);
}

static void
gog_renderer_svg_draw_path (GogRenderer *renderer, ArtVpath const *path,
			    GogViewAllocation const *bound)
{
	GogRendererSvg *prend = GOG_RENDERER_SVG (renderer);
	GogStyle const *style = renderer->cur_style;
	xmlNodePtr node;
	GString *string;
	char *buf;
	int opacity;
	char *old_num_locale;

	if (style->line.dash_type == GO_LINE_NONE)
		return;
	
	node = xmlNewDocNode (prend->doc, NULL, "path", NULL);
	old_num_locale = g_strdup (setlocale (LC_NUMERIC, NULL));

	setlocale (LC_NUMERIC, "C");
	xmlAddChild (prend->current_node, node);
	string = g_string_new ("");
	draw_path (prend, path, string);
	xmlNewProp (node, CC2XML ("d"), CC2XML (string->str));
	g_string_free (string, TRUE);
	xmlNewProp (node, CC2XML ("fill"), CC2XML ("none"));
	buf = g_strdup_printf ("%g", gog_renderer_line_size (renderer, style->line.width));
	xmlNewProp (node, CC2XML ("stroke-width"), CC2XML (buf));
	g_free (buf);
	/* TODO: clip dashed lines to prevent possible rsvg crash */
	stroke_dasharray (node, renderer->line_dash);
	buf = g_strdup_printf ("#%06x", style->line.color >> 8);
	xmlNewProp (node, CC2XML ("stroke"), CC2XML (buf));
	g_free (buf);
	opacity = style->line.color & 0xff;
	if (opacity != 255) {
		buf = g_strdup_printf ("%g", (double) opacity / 255.);
		xmlNewProp (node, CC2XML ("stroke-opacity"), CC2XML (buf));
		g_free (buf);
	}
	setlocale (LC_NUMERIC, old_num_locale);
	g_free (old_num_locale);
}

static void
gog_renderer_svg_draw_polygon (GogRenderer *renderer, ArtVpath const *path, 
			       gboolean narrow, GogViewAllocation const *bound)
{
	GogRendererSvg *prend = GOG_RENDERER_SVG (renderer);
	GogStyle const *style = renderer->cur_style;
	gboolean with_outline = (!narrow && style->outline.dash_type != GO_LINE_NONE);
	xmlNodePtr node;
	char *buf, *name, *id;
	int opacity;
	char *old_num_locale = g_strdup (setlocale (LC_NUMERIC, NULL));

	setlocale (LC_NUMERIC, "C");
	if (style->fill.type != GOG_FILL_STYLE_NONE || with_outline) {
		GString *string = g_string_new ("");
		node = xmlNewDocNode (prend->doc, NULL, "path", NULL);
		xmlAddChild (prend->current_node, node);
		draw_path (prend, path, string);
		g_string_append (string, "z");
		xmlNewProp (node, CC2XML ("d"), CC2XML (string->str));
		g_string_free (string, TRUE);
	} else
		return;

	if (style->fill.type != GOG_FILL_STYLE_NONE) {

		switch (style->fill.type) {
		case GOG_FILL_STYLE_PATTERN: {
			GOColor color;
			if (go_pattern_is_solid (&style->fill.pattern, &color)) {
				buf = g_strdup_printf ("#%06x", color >> 8);
				xmlNewProp (node, CC2XML ("fill"), CC2XML (buf));
				g_free (buf);
				opacity = color & 0xff;
				if (opacity != 255) {
					buf = g_strdup_printf ("%g", (double) opacity / 255.);
					xmlNewProp (node, CC2XML ("fill-opacity"), CC2XML (buf));
					g_free (buf);
				}
			}
			break;
		}

		case GOG_FILL_STYLE_GRADIENT:
			id = g_strdup_printf ("g_%x_%x_%x", style->fill.gradient.dir,
				style->fill.pattern.back, style->fill.pattern.fore);
			name = (char*) g_hash_table_lookup (prend->table, id);
			if (!name) {
				double x1, y1, x2, y2;
				GOColor start, end;
				xmlNodePtr child, stop;
				name = g_strdup_printf ("grad%d", prend->grad++);
				g_hash_table_insert (prend->table, id, name);
				if (style->fill.gradient.dir < 4) {
					x1 = y1 = x2 = 0;
					y2 = 1;
				} else if (style->fill.gradient.dir < 8) {
					x1 = y1 = y2 = 0;
					x2 = 1;
				} else if (style->fill.gradient.dir < 12) {
					x1 = y1 = 0;
					x2 = y2 = 1;
				} else {
					x1 = y2 = 1;
					x2 = y1 = 0;
				}
				child = xmlNewDocNode (prend->doc, NULL, CC2XML ("linearGradient"), NULL);
				xmlAddChild (prend->defs, child);
				xmlNewProp (child, CC2XML ("id"), CC2XML (name));
				xmlNewProp (child, CC2XML ("gradientUnits"), CC2XML ("objectBoundingBox"));
				switch (style->fill.gradient.dir % 4) {
				case 0:
					buf = (char*) "pad";
					start = style->fill.pattern.fore;
					end = style->fill.pattern.back;
					break;
				case 1:
					buf = (char*) "pad";
					start = style->fill.pattern.back;
					end = style->fill.pattern.fore;
					break;
				case 2:
					buf = (char*) "reflect";
					start = style->fill.pattern.fore;
					end = style->fill.pattern.back;
					x2 = x1 + (x2 - x1) / 2;
					y2 = y1 + (y2 - y1) / 2;
					break;
				default:
					buf = (char*) "reflect";
					start = style->fill.pattern.back;
					end = style->fill.pattern.fore;
					x2 = x1 + (x2 - x1) / 2;
					y2 = y1 + (y2 - y1) / 2;
					break;
				}
				xmlNewProp (child, CC2XML ("spreadMethod"), CC2XML (buf));
				buf = g_strdup_printf ("%g", x1);
				xmlNewProp (child, CC2XML ("x1"), CC2XML (buf));
				g_free (buf);
				buf = g_strdup_printf ("%g", y1);
				xmlNewProp (child, CC2XML ("y1"), CC2XML (buf));
				g_free (buf);
				buf = g_strdup_printf ("%g", x2);
				xmlNewProp (child, CC2XML ("x2"), CC2XML (buf));
				g_free (buf);
				buf = g_strdup_printf ("%g", y2);
				xmlNewProp (child, CC2XML ("y2"), CC2XML (buf));
				g_free (buf);
				stop = xmlNewDocNode (prend->doc, NULL, CC2XML ("stop"), NULL);
				xmlAddChild (child, stop);
				xmlNewProp (stop, CC2XML ("offset"), CC2XML ("0"));
				buf = g_strdup_printf ("#%06x", start >> 8);
				xmlNewProp (stop, CC2XML ("stop-color"), CC2XML (buf));
				g_free (buf);
				opacity = start & 0xff;
				if (opacity != 255) {
					buf = g_strdup_printf ("%g", (double) opacity / 255.);
					xmlNewProp (stop, CC2XML ("stop-opacity"), CC2XML (buf));
					g_free (buf);
				}
				stop = xmlNewDocNode (prend->doc, NULL, CC2XML ("stop"), NULL);
				xmlAddChild (child, stop);
				xmlNewProp (stop, CC2XML ("offset"), CC2XML ("1"));
				buf = g_strdup_printf ("#%06x", end >> 8);
				xmlNewProp (stop, CC2XML ("stop-color"), CC2XML (buf));
				g_free (buf);
				opacity = end & 0xff;
				if (opacity != 255) {
					buf = g_strdup_printf ("%g", (double) opacity / 255.);
					xmlNewProp (stop, CC2XML ("stop-opacity"), CC2XML (buf));
					g_free (buf);
				}
				buf = g_strdup_printf ("url(#%s)", name);
			} else {
				buf = g_strdup_printf ("url(#%s)", name);
				g_free (id);
			}
			xmlNewProp (node, CC2XML ("fill"), CC2XML (buf));
			g_free (buf);
			break;

		case GOG_FILL_STYLE_IMAGE:
			break;

		case GOG_FILL_STYLE_NONE:
			break; /* impossible */
		}
	}
	else
		xmlNewProp (node, CC2XML ("fill"), CC2XML ("none"));

	if (with_outline) {
		/* TODO: clip dashed lines to prevent possible rsvg crash */
		stroke_dasharray (node, renderer->outline_dash);
		buf = g_strdup_printf ("%g",  gog_renderer_line_size (renderer, style->outline.width));
		xmlNewProp (node, CC2XML ("stroke-width"), CC2XML (buf));
		g_free (buf);
		buf = g_strdup_printf ("#%06x", style->outline.color >> 8);
		xmlNewProp (node, CC2XML ("stroke"), CC2XML (buf));
		g_free (buf);
		opacity = style->outline.color & 0xff;
		if (opacity != 255) {
			buf = g_strdup_printf ("%g", (double) opacity / 255.);
			xmlNewProp (node, CC2XML ("stroke-opacity"), CC2XML (buf));
			g_free (buf);
		}
	} else
		xmlNewProp (node, CC2XML ("stroke"), CC2XML ("none"));
	setlocale (LC_NUMERIC, old_num_locale);
	g_free (old_num_locale);
}

static void
gog_renderer_svg_draw_marker (GogRenderer *rend, double x, double y)
{
	GogRendererSvg *prend = GOG_RENDERER_SVG (rend);
	GOMarker *marker = rend->cur_style->marker.mark;
	ArtVpath const *outline_path_raw, *fill_path_raw;
	ArtVpath *outline_path, *fill_path;
	double scaling[6], translation[6], affine[6];
	double half_size;
	xmlNodePtr node;
	GString *string;
	char *buf;
	int opacity;
	char *old_num_locale = g_strdup (setlocale (LC_NUMERIC, NULL));

	setlocale (LC_NUMERIC, "C");
	g_return_if_fail (marker != NULL);

	go_marker_get_paths (marker, &outline_path_raw, &fill_path_raw);

	if ((outline_path_raw == NULL) ||
	    (fill_path_raw == NULL))
		return;

	half_size = gog_renderer_line_size (rend, marker->size) / 2.0;
	art_affine_scale (scaling, half_size, half_size);
	art_affine_translate (translation, x, y);
	art_affine_multiply (affine, scaling, translation);

	outline_path = art_vpath_affine_transform (outline_path_raw, affine);
	fill_path = art_vpath_affine_transform (fill_path_raw, affine);

	node = xmlNewDocNode (prend->doc, NULL, "path", NULL);
	xmlAddChild (prend->current_node, node);
	string = g_string_new ("");
	draw_path (prend, fill_path, string);
	g_string_append (string, "z");
	xmlNewProp (node, CC2XML ("d"), CC2XML (string->str));
	g_string_free (string, TRUE);
	buf = g_strdup_printf ("#%06x", marker->fill_color >> 8);
	xmlNewProp (node, CC2XML ("fill"), CC2XML (buf));
	g_free (buf);
	xmlNewProp (node, CC2XML ("stroke"), CC2XML ("none"));
	opacity = marker->fill_color & 0xff;
	if (opacity != 255) {
		buf = g_strdup_printf ("%g", (double) opacity / 255.);
		xmlNewProp (node, CC2XML ("fill-opacity"), CC2XML (buf));
		g_free (buf);
	}

	node = xmlNewDocNode (prend->doc, NULL, "path", NULL);
	xmlAddChild (prend->current_node, node);
	string = g_string_new ("");
	draw_path (prend, outline_path, string);
	g_string_append (string, "z");
	xmlNewProp (node, CC2XML ("d"), CC2XML (string->str));
	g_string_free (string, TRUE);
	xmlNewProp (node, CC2XML ("fill"), CC2XML ("none"));
	xmlNewProp (node, CC2XML ("stroke-linecap"), CC2XML ("round"));
	buf = g_strdup_printf ("%g",  gog_renderer_line_size (rend, go_marker_get_outline_width (marker)));
	xmlNewProp (node, CC2XML ("stroke-width"), CC2XML (buf));
	g_free (buf);
	buf = g_strdup_printf ("#%06x", marker->outline_color >> 8);
	xmlNewProp (node, CC2XML ("stroke"), CC2XML (buf));
	g_free (buf);
	opacity = marker->outline_color & 0xff;
	if (opacity != 255) {
		buf = g_strdup_printf ("%g", (double) opacity / 255.);
		xmlNewProp (node, CC2XML ("stroke-opacity"), CC2XML (buf));
		g_free (buf);
	}

	g_free (outline_path);
	g_free (fill_path);
	setlocale (LC_NUMERIC, old_num_locale);
	g_free (old_num_locale);
}

static PangoLayout *
make_layout (GogRenderer *rend, char const *text)
{
	GogRendererSvg *prend = GOG_RENDERER_SVG (rend);
	PangoLayout *layout;
	PangoFontDescription const *fd = rend->cur_style->font.font->desc;

	if (prend->pango_context == NULL) {
		PangoFT2FontMap *font_map = PANGO_FT2_FONT_MAP (pango_ft2_font_map_new ());
		/*assume horizontal and vertical resolutions are the same
		 * Why ? */
		pango_ft2_font_map_set_resolution (font_map,
				GO_IN_TO_PT((double)1. / gog_renderer_pt2r (rend, 1.0)),
				GO_IN_TO_PT((double)1. / gog_renderer_pt2r (rend, 1.0)));
		prend->pango_context = pango_ft2_font_map_create_context  (font_map);
		g_object_unref (font_map);
	}

	gog_debug (0, {
		char *msg = pango_font_description_to_string (fd);
		g_warning (msg);
		g_free (msg);
	});

	layout = pango_layout_new (prend->pango_context);
	pango_layout_set_font_description (layout, fd);

	pango_layout_set_text (layout, text, -1);

	return layout;
}

static void
gog_renderer_svg_measure_text (GogRenderer *rend,
			       char const *text, GogViewRequisition *size)
{
	PangoRectangle  rect;
	PangoLayout    *layout = make_layout (rend, text);
	pango_layout_get_pixel_extents (layout, NULL, &rect);
	g_object_unref (layout);
	size->w = gog_renderer_pt2r (rend, rect.width);
	size->h = gog_renderer_pt2r (rend, rect.height);
}

static void
gog_renderer_svg_draw_text (GogRenderer *rend, char const *text,
			    GogViewAllocation const *pos, GtkAnchorType anchor,
			    GogViewAllocation *result)
{
	GogRendererSvg *prend = GOG_RENDERER_SVG (rend);
	xmlNodePtr node;
	char *buf;
	double x, y;
	int baseline;
	char *old_num_locale;
	PangoRectangle  rect;
	PangoLayout* layout = make_layout (rend, "lp");
	PangoFontDescription const *fd = rend->cur_style->font.font->desc;
	PangoLayoutIter* iter =pango_layout_get_iter(layout);
	pango_layout_get_pixel_extents (layout, NULL, &rect);
	x = pos->x;
	/* adjust to the base line */
	y = pos->y;
	baseline = pango_layout_iter_get_baseline(iter);
	pango_layout_iter_get_run_extents(iter, NULL, &rect);
	y += gog_renderer_pt2r(rend, (baseline - rect.y) / PANGO_SCALE);
	pango_layout_iter_free(iter);
	g_object_unref (layout);

	switch (anchor) {
	case GTK_ANCHOR_CENTER : case GTK_ANCHOR_E : case GTK_ANCHOR_W :
		y -= gog_renderer_pt2r(rend, (double) (rect.height / 2) / PANGO_SCALE);
		break;
	case GTK_ANCHOR_SE : case GTK_ANCHOR_S : case GTK_ANCHOR_SW :
		y -= gog_renderer_pt2r(rend, (double)rect.height / PANGO_SCALE);
		break;
	default :
		break;
	}

	node = xmlNewDocNode (prend->doc, NULL, "text", NULL);
	xmlNodeSetContent (node, CC2XML (text));
	old_num_locale = g_strdup (setlocale (LC_NUMERIC, NULL));
	setlocale (LC_NUMERIC, "C");
	xmlAddChild (prend->current_node, node);
	buf = g_strdup_printf ("%g", x);
	xmlNewProp (node, CC2XML ("x"), CC2XML (buf));
	g_free (buf);
	buf = g_strdup_printf ("%g", y);
	xmlNewProp (node, CC2XML ("y"), CC2XML (buf));
	g_free (buf);
	switch (anchor) {
	case GTK_ANCHOR_CENTER : case GTK_ANCHOR_N : case GTK_ANCHOR_S :
		xmlNewProp (node, CC2XML ("text-anchor"), CC2XML ("middle"));
		break;
	case GTK_ANCHOR_NE : case GTK_ANCHOR_SE : case GTK_ANCHOR_E :
		xmlNewProp (node, CC2XML ("text-anchor"), CC2XML ("end"));
		break;
	default : break;
	}
	xmlNewProp (node, CC2XML ("font-family"), CC2XML (pango_font_description_get_family (fd)));
	buf = g_strdup_printf ("%d", (int)(rint (gog_renderer_pt2r(rend, pango_font_description_get_size (fd) / PANGO_SCALE))));
	xmlNewProp (node, CC2XML ("font-size"), CC2XML (buf));
	g_free (buf);
	switch (pango_font_description_get_weight (fd)) {
	case PANGO_WEIGHT_BOLD:
		xmlNewProp (node, CC2XML ("font-weight"), CC2XML ("bold"));
		break;
	case PANGO_WEIGHT_NORMAL: break;
	default:
		buf = g_strdup_printf ("%d", pango_font_description_get_weight (fd));
		xmlNewProp (node, CC2XML ("font-weight"), CC2XML (buf));
		g_free (buf);
		break;
	}
	switch (pango_font_description_get_style (fd)) {
	case PANGO_STYLE_ITALIC:
		xmlNewProp (node, CC2XML ("font-syle"), CC2XML ("italic"));
		break;
	case PANGO_STYLE_OBLIQUE:
		xmlNewProp (node, CC2XML ("font-syle"), CC2XML ("oblique"));
		break;
	default: break;
	}
	setlocale (LC_NUMERIC, old_num_locale);
	g_free (old_num_locale);
}

static void
gog_renderer_svg_class_init (GogRendererClass *rend_klass)
{
	GObjectClass *gobject_klass   = (GObjectClass *) rend_klass;

	parent_klass = g_type_class_peek_parent (rend_klass);
	gobject_klass->finalize	  	= gog_renderer_svg_finalize;
	rend_klass->clip_push		= gog_renderer_svg_clip_push;
	rend_klass->clip_pop	 	= gog_renderer_svg_clip_pop;
	rend_klass->draw_path	  	= gog_renderer_svg_draw_path;
	rend_klass->draw_polygon  	= gog_renderer_svg_draw_polygon;
	rend_klass->draw_text	  	= gog_renderer_svg_draw_text;
	rend_klass->draw_marker	  	= gog_renderer_svg_draw_marker;
	rend_klass->measure_text  	= gog_renderer_svg_measure_text;
}

static GSF_CLASS (GogRendererSvg, gog_renderer_svg,
		  gog_renderer_svg_class_init, NULL,
		  GOG_RENDERER_TYPE)

/**
 * gog_graph_export_to_svg :
 * @graph  : #GogGraph
 * @output : #GsfOutput
 * @width  :
 * @height :
 *
 * Renders @graph as SVG and stores it in @output.
 *
 * Returns TRUE on success.
 **/
gboolean
gog_graph_export_to_svg (GogGraph *graph, GsfOutput *output,
			 double width, double height, double scale)
{
	GogViewAllocation allocation;
	GogRendererSvg *prend;
	xmlNsPtr namespace;
	gboolean success = TRUE;
	char *buf;
	char *old_num_locale = g_strdup (setlocale (LC_NUMERIC, NULL));
	setlocale (LC_NUMERIC, "C");

	prend = g_object_new (GOG_RENDERER_SVG_TYPE,
			      "model", graph,
			      NULL);
	prend->base.scale = scale;
	prend->doc = xmlNewDoc (CC2XML ("1.0"));

	xmlNewDtd (prend->doc,
		   CC2XML ("svg"), CC2XML ("-//W3C//DTD SVG 1.1//EN"),
		   CC2XML ("http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd"));
	prend->doc->children = xmlNewDocNode (prend->doc, NULL, CC2XML ("svg"), NULL);
	prend->current_node = prend->doc->children;
	prend->defs = xmlNewDocNode (prend->doc, NULL, CC2XML ("defs"), NULL);
	xmlAddChild (prend->doc->children, prend->defs);
	prend->table = g_hash_table_new_full (g_str_hash, g_str_equal, g_free, g_free);
	prend->grad = prend->pat = prend->img = 0;
	
	namespace = xmlNewNs (prend->doc->children, CC2XML ("http://www.w3.org/2000/svg"), NULL);
	xmlSetNs (prend->doc->children, namespace);
	xmlNewProp (prend->doc->children, CC2XML ("version"), CC2XML ("1.1"));

	namespace = xmlNewNs (prend->doc->children, CC2XML ("http://www.w3.org/1999/xlink"), CC2XML ("xlink"));

	buf = g_strdup_printf ("%g", width);
	xmlNewProp (prend->doc->children, CC2XML ("width"), CC2XML (buf));
	g_free (buf);
	buf = g_strdup_printf ("%g", height);
	xmlNewProp (prend->doc->children, CC2XML ("height"), CC2XML (buf));
	g_free (buf);
	setlocale (LC_NUMERIC, old_num_locale);
	g_free (old_num_locale);

	prend->clip_counter = 0;
	allocation.x = 0.;
	allocation.y = 0.;
	allocation.w = width;
	allocation.h = height;
	gog_view_size_allocate (prend->base.view, &allocation);
	gog_view_render	(prend->base.view, NULL);

	if ((!g_hash_table_size (prend->table)) &&
	    (prend->clip_counter == 0)) {
		xmlUnlinkNode (prend->defs);
		xmlFreeNode (prend->defs);
	}
	xmlIndentTreeOutput = TRUE;
	if (gsf_xmlDocFormatDump (output, prend->doc, "UTF-8", TRUE) < 0)
		success = FALSE;

	xmlFreeDoc (prend->doc);
	g_hash_table_destroy (prend->table);
	g_object_unref (prend);

	return success;
}
