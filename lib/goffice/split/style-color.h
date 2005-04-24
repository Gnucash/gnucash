#ifndef GNUMERIC_STYLE_COLOR_H
#define GNUMERIC_STYLE_COLOR_H

#include "gnumeric.h"
#include <goffice/utils/go-color.h>
#include <gdk/gdkcolor.h>

struct _GnmColor {
	GdkColor color, selected_color;
	char     *name;
	int      ref_count;
	gboolean is_auto;
};

/* Colors used by any GnumericSheet item */
extern GdkColor gs_white, gs_light_gray, gs_dark_gray, gs_black, gs_lavender, gs_yellow;

GnmColor *style_color_new_go   (GOColor c);
GnmColor *style_color_new_name  (char const *name);
GnmColor *style_color_new       (gushort red, gushort green, gushort blue);
GnmColor *style_color_new_i8    (guint8 red, guint8 green, guint8 blue);
GnmColor *style_color_new_pango (PangoColor *c);
GnmColor *style_color_auto_font (void);
GnmColor *style_color_auto_back (void);
GnmColor *style_color_auto_pattern (void);
GnmColor *style_color_ref      (GnmColor *sc);
void        style_color_unref    (GnmColor *sc);
gint        style_color_equal (const GnmColor *k1, const GnmColor *k2);
GnmColor *style_color_black    (void);
GnmColor *style_color_white    (void);
GnmColor *style_color_grid     (void);

void gnumeric_color_init     (void);
void gnumeric_color_shutdown (void);

#endif /* GNUMERIC_STYLE_COLOR_H */
