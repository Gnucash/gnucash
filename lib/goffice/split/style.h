#ifndef GNUMERIC_STYLE_H
#define GNUMERIC_STYLE_H

#include <gdk/gdk.h>
#include <libgnomeprint/gnome-font.h>
#include <gnumeric.h>
#include <pango/pango.h>

#define DEFAULT_FONT "Sans"
#define DEFAULT_SIZE 10.0

/* Alignment definitions */
/* Do not change these flags they are used as keys in the 1.0.x xml format.  */
typedef enum _StyleHAlignFlags {
	HALIGN_GENERAL =  0x01,
	HALIGN_LEFT    =  0x02,
	HALIGN_RIGHT   =  0x04,
	HALIGN_CENTER  =  0x08,
	HALIGN_FILL    =  0x10,
	HALIGN_JUSTIFY =  0x20,
	HALIGN_CENTER_ACROSS_SELECTION =  0x40
} StyleHAlignFlags;

typedef enum _StyleVAlignFlags {
	VALIGN_TOP     = 1,
	VALIGN_BOTTOM  = 2,
	VALIGN_CENTER  = 4,
	VALIGN_JUSTIFY = 8
} StyleVAlignFlags;

typedef enum _StyleUnderlineType {
	UNDERLINE_NONE   = 0,
	UNDERLINE_SINGLE = 1,
	UNDERLINE_DOUBLE = 2
} StyleUnderlineType;

struct _GnmFont {
	int	 ref_count;
	char	*font_name;
	double	 size_pts;
	double	 scale;
	struct {
		/* This does not belong here.  */
		struct {
			double digit, decimal, sign, E, e, hash;
		} pixels, pts;
	} approx_width;
	double	 height;
	struct {
		PangoContext		*context;
		PangoFontMetrics	*metrics;
		PangoFont	  	*font;
		PangoFontDescription  	*font_descr;
		PangoLayout		*layout;
	} pango;

	GnomeFont *gnome_print_font;

	unsigned int is_bold:1;
	unsigned int is_italic:1;
};

void           style_init  	      (void);
void	       style_shutdown         (void);

GnmFont     *style_font_new         (PangoContext *context,
				       const char *font_name,
				       double size_pts, double scale,
				       gboolean bold, gboolean italic);
void style_font_ref          (GnmFont *sf);
void style_font_unref        (GnmFont *sf);

guint          style_font_hash_func (gconstpointer v);
gint           style_font_equal (gconstpointer v, gconstpointer v2);

SpanCalcFlags	 required_updates_for_style (GnmStyle const *style);
StyleHAlignFlags style_default_halign (GnmStyle const *mstyle, GnmCell const *c);

extern double gnumeric_default_font_width;

GnomeFont *gnm_font_find_closest_from_weight_slant (const guchar *family, 
						    GnomeFontWeight weight, 
						    gboolean italic, 
						    gdouble size);
PangoContext *gnm_pango_context_get (void);

// #include "mstyle.h"

#endif /* GNUMERIC_STYLE_H */
