#ifndef GNUMERIC_MSTYLE_H
#define GNUMERIC_MSTYLE_H

#include "gnumeric.h"
#include "style.h"

/*
 * Keep element_size up to date.
 */
typedef enum {
	/* Delimiter */
	MSTYLE_ELEMENT_UNSET = 0,
	/* When there is a conflict in a merge */
	MSTYLE_ELEMENT_CONFLICT,
	/* Types that are visible in blank cells */
		MSTYLE_COLOR_BACK,
		MSTYLE_COLOR_PATTERN,

	        MSTYLE_BORDER_TOP,
	        MSTYLE_BORDER_BOTTOM,
	        MSTYLE_BORDER_LEFT,
	        MSTYLE_BORDER_RIGHT,
	        MSTYLE_BORDER_REV_DIAGONAL,
	        MSTYLE_BORDER_DIAGONAL,

		MSTYLE_PATTERN,
	/* Delimiter */
	MSTYLE_ELEMENT_MAX_BLANK,
	/* Normal types */
	        MSTYLE_COLOR_FORE,
		MSTYLE_FONT_NAME,
		MSTYLE_FONT_BOLD,
		MSTYLE_FONT_ITALIC,
		MSTYLE_FONT_UNDERLINE,
		MSTYLE_FONT_STRIKETHROUGH,
	        MSTYLE_FONT_SIZE,

		MSTYLE_FORMAT,

	        MSTYLE_ALIGN_V,
	        MSTYLE_ALIGN_H,
	        MSTYLE_INDENT,
		MSTYLE_ROTATION,
		MSTYLE_WRAP_TEXT,
		MSTYLE_SHRINK_TO_FIT,

	        MSTYLE_CONTENT_LOCKED,
	        MSTYLE_CONTENT_HIDDEN,

	/* Things not in MS Excel's Style */
	        MSTYLE_VALIDATION,
	        MSTYLE_HLINK,		/* patch equal_XL if this is changed */
	        MSTYLE_INPUT_MSG,	/* patch equal_XL if this is changed */
	/* Delimiter */
	MSTYLE_ELEMENT_MAX
} MStyleElementType;

GnmStyle     *mstyle_new           (void);
GnmStyle     *mstyle_new_default   (void);
GnmStyle     *mstyle_copy          (const GnmStyle *st);
GnmStyle	   *mstyle_copy_merge	 (const GnmStyle *orig, const GnmStyle *overlay);
void        mstyle_ref           (GnmStyle *st);
void        mstyle_unref         (GnmStyle *st);

GnmStyle   *mstyle_link_sheet    (GnmStyle *st, Sheet *sheet);
void        mstyle_link          (GnmStyle *st);
void        mstyle_link_multiple (GnmStyle *st, int count);
void        mstyle_unlink        (GnmStyle *st);

gboolean    mstyle_equal         (GnmStyle const *a, GnmStyle const *b);
gboolean    mstyle_equal_XL	 (GnmStyle const *a, GnmStyle const *b);
gboolean    mstyle_verify        (GnmStyle const *st);
guint       mstyle_hash          (gconstpointer st);
guint       mstyle_hash_XL	 (gconstpointer st);
gboolean    mstyle_empty         (const GnmStyle *st);

/*
 * Wafer thin element access functions.
 */
gboolean            mstyle_is_element_set  (const GnmStyle *st, MStyleElementType t);
gboolean            mstyle_is_element_conflict (const GnmStyle *st, MStyleElementType t);
void                mstyle_compare             (GnmStyle *a, const GnmStyle *b);
void                mstyle_unset_element   (GnmStyle *st, MStyleElementType t);
void                mstyle_replace_element (GnmStyle *src, GnmStyle *dst, MStyleElementType t);
void                mstyle_set_color       (GnmStyle *st, MStyleElementType t,
					    GnmColor *col);
GnmColor         *mstyle_get_color       (const GnmStyle *st, MStyleElementType t);
void                mstyle_set_border      (GnmStyle *st, MStyleElementType t,
					    GnmBorder *border);
GnmBorder	   *mstyle_get_border      (const GnmStyle *st, MStyleElementType t);
void                mstyle_set_pattern     (GnmStyle *st, int pattern);
int                 mstyle_get_pattern     (const GnmStyle *st);
void                mstyle_set_font_name   (GnmStyle *st, const char *name);
const char         *mstyle_get_font_name   (const GnmStyle *st);
void                mstyle_set_font_bold   (GnmStyle *st, gboolean bold);
gboolean            mstyle_get_font_bold   (const GnmStyle *st);
void                mstyle_set_font_italic (GnmStyle *st, gboolean italic);
gboolean            mstyle_get_font_italic (const GnmStyle *st);
void                mstyle_set_font_uline  (GnmStyle *st, StyleUnderlineType const t);
StyleUnderlineType  mstyle_get_font_uline  (const GnmStyle *st);
void                mstyle_set_font_strike (GnmStyle *st, gboolean strikethrough);
gboolean            mstyle_get_font_strike (const GnmStyle *st);
void                mstyle_set_font_size   (GnmStyle *st, double size);
double              mstyle_get_font_size   (const GnmStyle *st);

/* this font must be unrefd after use */
GnmFont          *mstyle_get_font        (const GnmStyle *st,
					    PangoContext *context,
					    double zoom);
void                mstyle_set_format      (GnmStyle *st, GnmFormat *);
void                mstyle_set_format_text (GnmStyle *st, const char *format);
GnmFormat          *mstyle_get_format      (const GnmStyle *st);
void                mstyle_set_align_h     (GnmStyle *st, StyleHAlignFlags a);
StyleHAlignFlags    mstyle_get_align_h     (const GnmStyle *st);
void                mstyle_set_align_v     (GnmStyle *st, StyleVAlignFlags a);
StyleVAlignFlags    mstyle_get_align_v     (const GnmStyle *st);
void                mstyle_set_indent	   (GnmStyle *st, int i);
int		    mstyle_get_indent	   (const GnmStyle *st);

void                mstyle_set_rotation	   (GnmStyle *st, int r);
int            	    mstyle_get_rotation    (const GnmStyle *st);

void                mstyle_set_wrap_text   (GnmStyle *st, gboolean f);
gboolean            mstyle_get_wrap_text   (const GnmStyle *st);
gboolean            mstyle_get_effective_wrap_text   (const GnmStyle *st);
void                mstyle_set_shrink_to_fit (GnmStyle *st, gboolean f);
gboolean            mstyle_get_shrink_to_fit (const GnmStyle *st);

void                mstyle_set_content_locked (GnmStyle *st, gboolean f);
gboolean            mstyle_get_content_locked (const GnmStyle *st);
void                mstyle_set_content_hidden (GnmStyle *st, gboolean f);
gboolean            mstyle_get_content_hidden (const GnmStyle *st);

void                mstyle_set_validation	(GnmStyle *st, GnmValidation *v);
GnmValidation      *mstyle_get_validation	(const GnmStyle *st);

void                mstyle_set_hlink		(GnmStyle *st, GnmHLink *link);
GnmHLink	   *mstyle_get_hlink		(const GnmStyle *st);

void                mstyle_set_input_msg	(GnmStyle *st, GnmInputMsg *msg);
GnmInputMsg   	   *mstyle_get_input_msg	(const GnmStyle *st);

gboolean            mstyle_visible_in_blank (const GnmStyle *st);

PangoAttrList      *mstyle_generate_attrs_full (const GnmStyle *st);
PangoAttrList      *mstyle_get_pango_attrs     (const GnmStyle *st,
						PangoContext *context,
						double zoom);
void	    	    mstyle_set_from_pango_attribute (GnmStyle *style,
						     PangoAttribute const *attr);

char       *mstyle_to_string   (const GnmStyle *st); /* Debug only ! leaks like a sieve */
void        mstyle_dump        (const GnmStyle *st);

void        mstyle_init (void);
void        mstyle_shutdown (void);

#endif /* GNUMERIC_MSTYLE_H */
