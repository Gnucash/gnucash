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
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301
 * USA
 */

#include <goffice/goffice-config.h>
#include <goffice/graph/gog-graph-impl.h>
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
set_double_prop (xmlNodePtr node, const char *name, double value)
{
	char buffer[G_ASCII_DTOSTR_BUF_SIZE];

	g_ascii_dtostr (buffer, sizeof (buffer), value);
	xmlNewProp (node, CC2XML(name), CC2XML(buffer));
}

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
draw_path (ArtVpath const *path, GString *string)
{
	char buffer[G_ASCII_DTOSTR_BUF_SIZE];
	
	for ( ; path->code != ART_END ; path++)
		switch (path->code) {
		case ART_MOVETO_OPEN :
		case ART_MOVETO :
			g_string_append_c (string, 'M');
			g_string_append (string, g_ascii_dtostr (buffer, sizeof (buffer), path->x));
			g_string_append_c (string, ' ');
			g_string_append (string, g_ascii_dtostr (buffer, sizeof (buffer), path->y));
			break;
		case ART_LINETO :
			g_string_append_c (string, 'L');
			g_string_append (string, g_ascii_dtostr (buffer, sizeof (buffer), path->x));
			g_string_append_c (string, ' ');
			g_string_append (string, g_ascii_dtostr (buffer, sizeof (buffer), path->y));
			break;
		default :
			break;
		}
}

static void
gog_renderer_svg_push_clip (GogRenderer *rend, GogRendererClip *clip)
{
	GogRendererSvg *prend = GOG_RENDERER_SVG (rend);
	char *buf;
	xmlNodePtr child;
	xmlNodePtr node;
	GString *string;

	prend->clip_counter++;
	
	node = xmlNewDocNode (prend->doc, NULL, CC2XML("clipPath"), NULL);
	xmlAddChild (prend->defs, node);

	buf = g_strdup_printf ("clip%i", prend->clip_counter);
	xmlNewProp (node, CC2XML("id"), CC2XML(buf));
	g_free (buf);
	
	child = xmlNewChild (node, NULL, CC2XML ("path"), NULL);
	string = g_string_new ("");
	draw_path (clip->path, string);
	xmlNewProp (child, CC2XML ("d"), CC2XML (string->str));
	g_string_free (string, TRUE);
	
	node = xmlNewDocNode (prend->doc, NULL, CC2XML("g"), NULL);
	xmlAddChild (prend->current_node, node);
	
	buf = g_strdup_printf ("url(#clip%i)", prend->clip_counter);
	xmlNewProp (node, CC2XML ("clip-path"), CC2XML (buf));
	g_free (buf);

	prend->current_node = node;
}

static void
gog_renderer_svg_pop_clip (GogRenderer *rend, GogRendererClip *clip)
{
	GogRendererSvg *prend = GOG_RENDERER_SVG (rend);
	
	prend->current_node = prend->current_node->parent;
}

static void
stroke_dasharray (xmlNodePtr node, ArtVpathDash *dash)
{
	GString *string;
	char buffer[G_ASCII_DTOSTR_BUF_SIZE];
	int i;

	if (dash == NULL || dash->n_dash < 1)
		return;

	string = g_string_new ("");
	for (i = 0; i < dash->n_dash; i++) {
	       if (i != 0) g_string_append_c (string, ' ');
	       g_string_append (string, g_ascii_dtostr (buffer, sizeof (buffer), dash->dash[i]));
	}
	xmlNewProp (node, CC2XML ("stroke-dasharray"), CC2XML (string->str));
	g_string_free (string, TRUE);
}

#define SVG_PATTERN_SCALE 2.0

static void
fill_properties (GogRenderer *renderer, xmlNodePtr node, gboolean narrow)
{
	GogRendererSvg *prend = GOG_RENDERER_SVG (renderer);
	GogStyle const *style = renderer->cur_style;
	gboolean with_outline = (!narrow && style->outline.dash_type != GO_LINE_NONE);
	char *buf, *name, *id;
	int opacity;

	if (style->fill.type != GOG_FILL_STYLE_NONE) {

		switch (style->fill.type) {
		case GOG_FILL_STYLE_PATTERN: {
			GOColor color;
			if (go_pattern_is_solid (&style->fill.pattern, &color)) {
				buf = g_strdup_printf ("#%06x", color >> 8);
				xmlNewProp (node, CC2XML ("fill"), CC2XML (buf));
				g_free (buf);
				opacity = color & 0xff;
				if (opacity != 255) 
					set_double_prop (node, "fill-opacity", (double) opacity / 255.0);
			} else {
				xmlNodePtr child, pat_node;
				id = g_strdup (go_pattern_as_str (style->fill.pattern.pattern));
				name = (char *) g_hash_table_lookup (prend->table, id);

				if (!name) {
					double height, width;
					char *svg_path = go_pattern_get_svg_path (&style->fill.pattern, 
										  &width, &height);
					char buffer[G_ASCII_DTOSTR_BUF_SIZE];

					if (svg_path == NULL) {
						g_free (id);
						break;
					}

					name = g_strdup (id);
					g_hash_table_insert (prend->table, id, name);
					pat_node = xmlNewChild (prend->defs, NULL, CC2XML ("pattern"), NULL);
					xmlSetProp (pat_node, CC2XML ("x"), CC2XML ("0"));
					xmlSetProp (pat_node, CC2XML ("y"), CC2XML ("0"));
					set_double_prop (pat_node, "width", width * SVG_PATTERN_SCALE);
					set_double_prop (pat_node, "height", height * SVG_PATTERN_SCALE);
					xmlSetProp (pat_node, CC2XML ("id"), CC2XML (name));
					xmlSetProp (pat_node, CC2XML ("patternUnits"), CC2XML ("userSpaceOnUse"));
					
					child = xmlNewChild (pat_node, NULL, CC2XML ("rect"), NULL);
					xmlSetProp (child, CC2XML ("x"), CC2XML ("-0.1"));
					xmlSetProp (child, CC2XML ("y"), CC2XML ("-0.1"));
					set_double_prop (child, "width", width * SVG_PATTERN_SCALE + 0.2);
					set_double_prop (child, "height", height * SVG_PATTERN_SCALE + 0.2);
					g_ascii_dtostr (buffer, sizeof (buffer), 
							(double) (style->fill.pattern.back & 0xff) / 255.0);
					buf = g_strdup_printf ("stroke:none;fill:#%06x;fill-opacity:%s;",
							       style->fill.pattern.back >> 8, buffer);
					xmlSetProp (child, CC2XML ("style"), CC2XML (buf));
					g_free (buf);

					child = xmlNewChild (pat_node, NULL, CC2XML ("path"), NULL);
					xmlSetProp (child, CC2XML ("d"), CC2XML (svg_path));
					g_ascii_dtostr (buffer, sizeof (buffer), 
							(double) (style->fill.pattern.fore & 0xff) / 255.0);
					buf = g_strdup_printf ("stroke:none;fill:#%06x;fill-opacity:%s;", 
							       style->fill.pattern.fore >> 8, buffer);
					xmlSetProp (child, CC2XML ("style"), CC2XML (buf));
					g_free (buf);
					buf = g_strdup_printf ("scale(%g)", SVG_PATTERN_SCALE);
					xmlSetProp (child, CC2XML ("transform"), CC2XML (buf)); 
					g_free (buf);
					g_free (svg_path);
				} else {
					g_free (id);
				}
				buf = g_strdup_printf ("url(#%s)", name);
				xmlSetProp (node, CC2XML ("fill"), CC2XML (buf));
				g_free (buf);
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
				set_double_prop (child, "x1", x1);
				set_double_prop (child, "y1", y1);
				set_double_prop (child, "x2", x2);
				set_double_prop (child, "y2", y2);
				
				stop = xmlNewDocNode (prend->doc, NULL, CC2XML ("stop"), NULL);
				xmlAddChild (child, stop);
				xmlNewProp (stop, CC2XML ("offset"), CC2XML ("0"));
				buf = g_strdup_printf ("#%06x", start >> 8);
				xmlNewProp (stop, CC2XML ("stop-color"), CC2XML (buf));
				g_free (buf);
				opacity = start & 0xff;
				if (opacity != 255) 
					set_double_prop (stop, "stop-opacity", (double) opacity / 255.0);
				
				stop = xmlNewDocNode (prend->doc, NULL, CC2XML ("stop"), NULL);
				xmlAddChild (child, stop);
				xmlNewProp (stop, CC2XML ("offset"), CC2XML ("1"));
				buf = g_strdup_printf ("#%06x", end >> 8);
				xmlNewProp (stop, CC2XML ("stop-color"), CC2XML (buf));
				g_free (buf);
				opacity = end & 0xff;
				if (opacity != 255) 
					set_double_prop (stop, "stop-opacity", (double) opacity / 255.0);
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
		set_double_prop (node, "stroke-width", gog_renderer_line_size (renderer, style->outline.width));
		buf = g_strdup_printf ("#%06x", style->outline.color >> 8);
		xmlNewProp (node, CC2XML ("stroke"), CC2XML (buf));
		g_free (buf);
		opacity = style->outline.color & 0xff;
		if (opacity != 255) 
			set_double_prop (node, "stroke-opacity", (double) opacity / 255.0);
	} else
		xmlNewProp (node, CC2XML ("stroke"), CC2XML ("none"));
}

static void
gog_renderer_svg_draw_path (GogRenderer *renderer, ArtVpath const *path)
{
	GogRendererSvg *prend = GOG_RENDERER_SVG (renderer);
	GogStyle const *style = renderer->cur_style;
	xmlNodePtr node;
	GString *string;
	char *buf;
	int opacity;

	if (style->line.dash_type == GO_LINE_NONE)
		return;
	
	node = xmlNewDocNode (prend->doc, NULL, "path", NULL);

	xmlAddChild (prend->current_node, node);
	
	string = g_string_new ("");
	draw_path (path, string);
	xmlNewProp (node, CC2XML ("d"), CC2XML (string->str));
	g_string_free (string, TRUE);
	
	xmlNewProp (node, CC2XML ("fill"), CC2XML ("none"));

	set_double_prop (node, "stroke-width", gog_renderer_line_size (renderer, style->line.width));
	stroke_dasharray (node, renderer->line_dash);
	
	buf = g_strdup_printf ("#%06x", style->line.color >> 8);
	xmlNewProp (node, CC2XML ("stroke"), CC2XML (buf));
	g_free (buf);
	
	opacity = style->line.color & 0xff;
	if (opacity != 255) 
		set_double_prop (node, "stroke-opacity", (double) opacity / 255.0);
}

static void
gog_renderer_svg_draw_polygon (GogRenderer *renderer, ArtVpath const *path, 
			       gboolean narrow)
{
	GogRendererSvg *prend = GOG_RENDERER_SVG (renderer);
	GogStyle const *style = renderer->cur_style;
	gboolean with_outline = (!narrow && style->outline.dash_type != GO_LINE_NONE);
	xmlNodePtr node;

	if (style->fill.type != GOG_FILL_STYLE_NONE || with_outline) {
		GString *string = g_string_new ("");
		node = xmlNewDocNode (prend->doc, NULL, "path", NULL);
		xmlAddChild (prend->current_node, node);
		draw_path (path, string);
		g_string_append_c (string, 'z');
		xmlNewProp (node, CC2XML ("d"), CC2XML (string->str));
		g_string_free (string, TRUE);
	} else
		return;

	fill_properties (renderer, node, narrow);
}

static void
draw_bezier_path (ArtBpath const *path, GString *string)
{
	char buffer[G_ASCII_DTOSTR_BUF_SIZE];
	
	for ( ; path->code != ART_END ; path++)
		switch (path->code) {
		case ART_MOVETO_OPEN :
		case ART_MOVETO :
			g_string_append_c (string, 'M');
			g_string_append (string, g_ascii_dtostr (buffer, sizeof (buffer), path->x3));
			g_string_append_c (string, ' ');
			g_string_append (string, g_ascii_dtostr (buffer, sizeof (buffer), path->y3));
			break;
		case ART_LINETO :
			g_string_append_c (string, 'L');
			g_string_append (string, g_ascii_dtostr (buffer, sizeof (buffer), path->x3));
			g_string_append_c (string, ' ');
			g_string_append (string, g_ascii_dtostr (buffer, sizeof (buffer), path->y3));
			break;
		case ART_CURVETO :
			g_string_append_c (string, 'C');
			g_string_append (string, g_ascii_dtostr (buffer, sizeof (buffer), path->x1));
			g_string_append_c (string, ' ');
			g_string_append (string, g_ascii_dtostr (buffer, sizeof (buffer), path->y1));
			g_string_append_c (string, ' ');
			g_string_append (string, g_ascii_dtostr (buffer, sizeof (buffer), path->x2));
			g_string_append_c (string, ' ');
			g_string_append (string, g_ascii_dtostr (buffer, sizeof (buffer), path->y2));
			g_string_append_c (string, ' ');
			g_string_append (string, g_ascii_dtostr (buffer, sizeof (buffer), path->x3));
			g_string_append_c (string, ' ');
			g_string_append (string, g_ascii_dtostr (buffer, sizeof (buffer), path->y3));
			break;
		default :
			break;
		}
}

static void
gog_renderer_svg_draw_bezier_path (GogRenderer *rend, ArtBpath const *path)
{
	GogRendererSvg *prend = GOG_RENDERER_SVG (rend);
	GogStyle const *style = rend->cur_style;
	xmlNodePtr node;
	GString *string;
	char *buf;
	int opacity;

	if (style->line.dash_type == GO_LINE_NONE)
		return;
	
	node = xmlNewDocNode (prend->doc, NULL, "path", NULL);

	xmlAddChild (prend->current_node, node);
	string = g_string_new ("");
	draw_bezier_path (path, string);

	xmlNewProp (node, CC2XML ("d"), CC2XML (string->str));
	g_string_free (string, TRUE);
	xmlNewProp (node, CC2XML ("fill"), CC2XML ("none"));
	set_double_prop (node, "stroke-width", gog_renderer_line_size (rend, style->line.width));
	stroke_dasharray (node, rend->line_dash);
	buf = g_strdup_printf ("#%06x", style->line.color >> 8);
	xmlNewProp (node, CC2XML ("stroke"), CC2XML (buf));
	g_free (buf);
	opacity = style->line.color & 0xff;
	if (opacity != 255) 
		set_double_prop (node, "stroke-opacity", (double) opacity / 255.0);
}

static void
gog_renderer_svg_draw_bezier_polygon (GogRenderer *renderer, ArtBpath const *path, 
				      gboolean narrow)
{
	GogRendererSvg *prend = GOG_RENDERER_SVG (renderer);
	GogStyle const *style = renderer->cur_style;
	gboolean with_outline = (!narrow && style->outline.dash_type != GO_LINE_NONE);
	xmlNodePtr node;

	if (style->fill.type != GOG_FILL_STYLE_NONE || with_outline) {
		GString *string = g_string_new ("");
		node = xmlNewDocNode (prend->doc, NULL, "path", NULL);
		xmlAddChild (prend->current_node, node);
		draw_bezier_path (path, string);
		g_string_append_c (string, 'z');
		xmlNewProp (node, CC2XML ("d"), CC2XML (string->str));
		g_string_free (string, TRUE);
	} else
		return;

	fill_properties (renderer, node, narrow);
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
	draw_path (fill_path, string);
	g_string_append_c (string, 'z');
	xmlNewProp (node, CC2XML ("d"), CC2XML (string->str));
	g_string_free (string, TRUE);
	buf = g_strdup_printf ("#%06x", marker->fill_color >> 8);
	xmlNewProp (node, CC2XML ("fill"), CC2XML (buf));
	g_free (buf);
	xmlNewProp (node, CC2XML ("stroke"), CC2XML ("none"));
	opacity = marker->fill_color & 0xff;
	if (opacity != 255) 
		set_double_prop (node, "fill-opacity", (double) opacity / 255.0);

	node = xmlNewDocNode (prend->doc, NULL, "path", NULL);
	xmlAddChild (prend->current_node, node);
	string = g_string_new ("");
	draw_path (outline_path, string);
	g_string_append_c (string, 'z');
	xmlNewProp (node, CC2XML ("d"), CC2XML (string->str));
	g_string_free (string, TRUE);
	xmlNewProp (node, CC2XML ("fill"), CC2XML ("none"));
	xmlNewProp (node, CC2XML ("stroke-linecap"), CC2XML ("round"));
	set_double_prop (node, "stroke-width", 
		gog_renderer_line_size (rend, go_marker_get_outline_width (marker)));
	buf = g_strdup_printf ("#%06x", marker->outline_color >> 8);
	xmlNewProp (node, CC2XML ("stroke"), CC2XML (buf));
	g_free (buf);
	opacity = marker->outline_color & 0xff;
	if (opacity != 255) 
		set_double_prop (node, "stroke-opacity", (double) opacity / 255.0);

	g_free (outline_path);
	g_free (fill_path);
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
gog_renderer_svg_get_text_OBR (GogRenderer *rend, char const *text, GOGeometryOBR *obr)
{
	PangoRectangle  rect;
	PangoLayout    *layout = make_layout (rend, text);
	
	pango_layout_get_pixel_extents (layout, NULL, &rect);
	g_object_unref (layout);
	obr->w = gog_renderer_pt2r (rend, rect.width);
	obr->h = gog_renderer_pt2r (rend, rect.height);
}

static void
gog_renderer_svg_draw_text (GogRenderer *rend, char const *text,
			    GogViewAllocation const *pos, GtkAnchorType anchor,
			    GogViewAllocation *result)
{
	GogRendererSvg *prend = GOG_RENDERER_SVG (rend);
	GogStyle const *style = rend->cur_style;
	PangoRectangle  rect;
	PangoLayout* layout = make_layout (rend, text);
	PangoFontDescription const *fd = style->font.font->desc;
	PangoLayoutIter* iter =pango_layout_get_iter(layout);
	GOGeometryOBR obr;
	GOGeometryAABR aabr;
	GString *string;
	xmlNodePtr node;
	char buffer[G_ASCII_DTOSTR_BUF_SIZE];
	char *buf;
	double y_offset;
	int baseline;

	baseline = pango_layout_iter_get_baseline (iter);
	pango_layout_iter_get_run_extents (iter, NULL, &rect);
	pango_layout_iter_free(iter);
	g_object_unref (layout);
	obr.w = gog_renderer_pt2r (rend, rect.width / PANGO_SCALE);
	obr.h = gog_renderer_pt2r (rend, rect.height / PANGO_SCALE);
	obr.alpha = rend->cur_style->text_layout.angle * M_PI / 180.0;
	obr.x = pos->x;
	obr.y = pos->y;
	go_geometry_OBR_to_AABR (&obr, &aabr);
	y_offset = gog_renderer_pt2r(rend, (baseline - rect.y) / PANGO_SCALE) - obr.h / 2.0;

	switch (anchor) {
		case GTK_ANCHOR_NW: case GTK_ANCHOR_W: case GTK_ANCHOR_SW:
			obr.x += aabr.w / 2.0;
			break;
		case GTK_ANCHOR_NE: case GTK_ANCHOR_E: case GTK_ANCHOR_SE:
			obr.x -= aabr.w / 2.0;
		default:
			break;
	}
			
	switch (anchor) {
		case GTK_ANCHOR_NW: case GTK_ANCHOR_N: case GTK_ANCHOR_NE:
			obr.y += aabr.h / 2.0;
			break;
		case GTK_ANCHOR_SW: case GTK_ANCHOR_S: case GTK_ANCHOR_SE:
			obr.y -= aabr.h / 2.0;
		default:
			break;
	}
	
	node = xmlNewDocNode (prend->doc, NULL, "text", NULL);
	xmlNodeSetContent (node, CC2XML (text));
	xmlAddChild (prend->current_node, node);
	set_double_prop (node, "x", obr.x);
	set_double_prop (node, "y", obr.y + y_offset);
	xmlNewProp (node, CC2XML ("text-anchor"), CC2XML ("middle"));
	string = g_string_new ("rotate(");
	g_string_append (string, g_ascii_dtostr (buffer, sizeof (buffer), -rend->cur_style->text_layout.angle));
	g_string_append_c (string, ',');
	g_string_append (string, g_ascii_dtostr (buffer, sizeof (buffer), obr.x));
	g_string_append_c (string, ',');
	g_string_append (string, g_ascii_dtostr (buffer, sizeof (buffer), obr.y));
	g_string_append_c (string, ')');
	xmlNewProp (node, CC2XML ("transform"), CC2XML (string->str));
       	g_string_free (string, TRUE);	
	
	buf = g_strdup_printf ("#%06x", style->font.color >> 8);
	xmlNewProp (node, CC2XML ("fill"), CC2XML (buf));
	g_free (buf);
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
}

static void
gog_renderer_svg_class_init (GogRendererClass *rend_klass)
{
	GObjectClass *gobject_klass   = (GObjectClass *) rend_klass;

	parent_klass = g_type_class_peek_parent (rend_klass);
	gobject_klass->finalize	  	= gog_renderer_svg_finalize;
	rend_klass->push_clip		= gog_renderer_svg_push_clip;
	rend_klass->pop_clip	 	= gog_renderer_svg_pop_clip;
	rend_klass->draw_path	  	= gog_renderer_svg_draw_path;
	rend_klass->draw_polygon  	= gog_renderer_svg_draw_polygon;
	rend_klass->draw_bezier_path 	= gog_renderer_svg_draw_bezier_path;
	rend_klass->draw_bezier_polygon = gog_renderer_svg_draw_bezier_polygon;
	rend_klass->draw_text	  	= gog_renderer_svg_draw_text;
	rend_klass->draw_marker	  	= gog_renderer_svg_draw_marker;
	rend_klass->get_text_OBR	= gog_renderer_svg_get_text_OBR;
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

	gog_graph_force_update (graph);

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

	set_double_prop (prend->doc->children, "width", width);
	set_double_prop (prend->doc->children, "height", height);

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
