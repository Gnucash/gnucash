#ifndef GNUMERIC_STYLE_BORDER_H
#define GNUMERIC_STYLE_BORDER_H

#include "gnumeric.h"
#include <gdk/gdkgc.h>
#include <libgnomeprint/gnome-print.h>

typedef enum {
	STYLE_BORDER_HORIZONTAL,
	STYLE_BORDER_VERTICAL,
	STYLE_BORDER_DIAGONAL
} StyleBorderOrientation;

typedef enum {
 	STYLE_BORDER_NONE			= 0x0,
 	STYLE_BORDER_THIN			= 0x1,
 	STYLE_BORDER_MEDIUM			= 0x2,
 	STYLE_BORDER_DASHED			= 0x3,
 	STYLE_BORDER_DOTTED			= 0x4,
 	STYLE_BORDER_THICK			= 0x5,
 	STYLE_BORDER_DOUBLE			= 0x6,
 	STYLE_BORDER_HAIR			= 0x7,
	STYLE_BORDER_MEDIUM_DASH		= 0x8,
	STYLE_BORDER_DASH_DOT			= 0x9,
	STYLE_BORDER_MEDIUM_DASH_DOT		= 0xa,
	STYLE_BORDER_DASH_DOT_DOT		= 0xb,
	STYLE_BORDER_MEDIUM_DASH_DOT_DOT	= 0xc,
	STYLE_BORDER_SLANTED_DASH_DOT		= 0xd,

	/* ONLY for internal use */
	STYLE_BORDER_INCONSISTENT		= 0xe,

 	STYLE_BORDER_MAX
} StyleBorderType;

/* The order corresponds to the border_buttons name list
 * in dialog_cell_format_impl */
typedef enum _StyleBorderLocation {
	STYLE_BORDER_TOP,	STYLE_BORDER_BOTTOM,
	STYLE_BORDER_LEFT,	STYLE_BORDER_RIGHT,
	STYLE_BORDER_REV_DIAG,	STYLE_BORDER_DIAG,

	/* These are special.
	 * They are logical rather than actual borders, however, they
	 * require extra lines to be drawn so they need to be here.
	 */
	STYLE_BORDER_HORIZ, STYLE_BORDER_VERT,

	STYLE_BORDER_EDGE_MAX
} StyleBorderLocation;

struct _GnmBorder {
	/* Key elements */
	StyleBorderType	 line_type;
	GnmColor     	*color;
	int		 begin_margin, end_margin, width;

	/* Private */
	GdkGC	        *gc;
	GdkScreen       *gc_screen;
	gint	        ref_count;
};

void	      style_border_unref (GnmBorder *border);
GnmBorder  *style_border_ref   (GnmBorder *border);

#define	style_border_is_blank(b) ((b) == NULL || (b)->line_type == STYLE_BORDER_NONE)
GnmBorder  *style_border_none  (void);
void          style_border_none_set_color (GnmColor *color);

GnmBorder  *style_border_fetch (StyleBorderType const	 line_type,
				  GnmColor 			*color,
				  StyleBorderOrientation       orientation);
gboolean style_border_visible_in_blank (GnmBorder const *border);

StyleBorderOrientation style_border_get_orientation (StyleBorderLocation type);

gint   style_border_get_width   (StyleBorderType const line_type);
void   style_border_set_gc_dash (GdkGC *gc, StyleBorderType const line_type);

void style_borders_row_draw  (GnmBorder const * const * prev_vert,
			      GnmRow const *sr,
			      GdkDrawable *drawable,
			      int x, int y1, int y2,
			      int *colwidths, gboolean draw_vertical);
void style_border_draw_diag  (GnmStyle const *style,
			      GdkDrawable *drawable,
			      int x1, int y1, int x2, int y2);

void style_borders_row_print (GnmBorder const * const * prev_vert,
			      GnmRow const *sr,
			      GnomePrintContext *context,
			      float x, float y1, float y2,
			      Sheet const *sheet, gboolean draw_vertical);
void style_border_print_diag (GnmStyle const *style,
			      GnomePrintContext *context,
			      float x1, float y1, float x2, float y2);

#endif /* GNUMERIC_STYLE_BORDER_H */
