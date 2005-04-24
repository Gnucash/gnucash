/* vim: set sw=8: */
/*
 * GnmStyle.c: The guts of the style engine.
 *
 * Authors:
 *   Michael Meeks <mmeeks@gnu.org>
 *   Almer S. Tigelaar <almer@gnome.org>
 *   Jody Goldberg <jody@gnome.org>
 *   Morten Welinder <terra@gnome.org>
 */
#include <config.h>
#include "gnumeric.h"
#include "mstyle.h"

#include "str.h"
#include "style-border.h"
#include "style-color.h"
//#include "validation.h"
//#include "pattern.h"
#include "format.h"
//#include "sheet-style.h"
#include "application.h"
#include "gutils.h"
#include "gnumeric-gconf.h"

#include <stdio.h>

#ifndef USE_MSTYLE_POOL
#define USE_MSTYLE_POOL 1
#endif

#if USE_MSTYLE_POOL
/* Memory pool for mstyles.  */
static GnmMemChunk *mstyle_pool;
#define CHUNK_ALLOC(T,p) ((T*)gnm_mem_chunk_alloc (p))
#define CHUNK_ALLOC0(T,p) ((T*)gnm_mem_chunk_alloc0 (p))
#define CHUNK_FREE(p,v) gnm_mem_chunk_free ((p), (v))
#else
#define CHUNK_ALLOC(T,c) g_new (T,1)
#define CHUNK_ALLOC0(T,c) g_new0 (T,1)
#define CHUNK_FREE(p,v) g_free ((v))
#endif

typedef struct {
	MStyleElementType type;
	union {
		union {
			GnmColor *any;
			GnmColor *fore;
			GnmColor *back;
			GnmColor *pattern;
		}                color;
		union {
			GnmBorder *top;
			GnmBorder *bottom;
			GnmBorder *left;
			GnmBorder *right;
			GnmBorder *diagonal;
			GnmBorder *rev_diagonal;

			/* Used for loading */
			GnmBorder *any;
		}                border;
		guint32          pattern;

		union {
			GnmString *name;
			gboolean  bold;
			gboolean  italic;
			StyleUnderlineType  underline;
			gboolean  strikethrough;
			float     size;
		}                font;
		GnmFormat     *format;
		union {
			guint16   v;
			guint16   h;
		}                align;
		int		 indent;
		int		 rotation;
		gboolean         wrap_text;
		gboolean         shrink_to_fit;
		gboolean         content_locked;
		gboolean         content_hidden;

		GnmValidation   *validation;
		GnmHLink        *hlink;
		GnmInputMsg	*input_msg;

		/* Convenience members */
		gpointer         any_pointer;
		gboolean         any_boolean;
		float            any_float;
		guint16          any_guint16;
		guint32          any_guint32;
	} u;
} MStyleElement;

struct _GnmStyle {
	guint32        ref_count;
	guint32        link_count;
	Sheet	      *linked_sheet;
	MStyleElement  elements[MSTYLE_ELEMENT_MAX];
	PangoAttrList *pango_attrs;
	double         pango_attrs_zoom;
	GnmFont     *font;
	double         font_zoom;
};

#define MSTYLE_ANY_COLOR             MSTYLE_COLOR_FORE: \
				case MSTYLE_COLOR_BACK: \
				case MSTYLE_COLOR_PATTERN

#define MSTYLE_ANY_BORDER            MSTYLE_BORDER_TOP: \
				case MSTYLE_BORDER_BOTTOM: \
				case MSTYLE_BORDER_LEFT: \
				case MSTYLE_BORDER_RIGHT: \
				case MSTYLE_BORDER_DIAGONAL: \
				case MSTYLE_BORDER_REV_DIAGONAL

#define MSTYLE_ANY_POINTER           MSTYLE_FONT_NAME: \
				case MSTYLE_FORMAT: \
				case MSTYLE_VALIDATION: \
				case MSTYLE_HLINK: \
				case MSTYLE_INPUT_MSG

#define MSTYLE_ANY_BOOLEAN           MSTYLE_FONT_BOLD: \
				case MSTYLE_FONT_ITALIC: \
				case MSTYLE_FONT_STRIKETHROUGH: \
				case MSTYLE_WRAP_TEXT:\
				case MSTYLE_SHRINK_TO_FIT:\
				case MSTYLE_CONTENT_LOCKED:\
				case MSTYLE_CONTENT_HIDDEN

#define MSTYLE_ANY_GUINT16           MSTYLE_ALIGN_V: \
                                case MSTYLE_ALIGN_H

#define MSTYLE_ANY_GUINT32           MSTYLE_PATTERN: \
				case MSTYLE_ROTATION

#define MSTYLE_ANY_FLOAT             MSTYLE_FONT_SIZE


static const char *
mstyle_names[MSTYLE_ELEMENT_MAX] = {
	"--UnSet--",
	"--Conflict--",
	"Color.Back",
	"Color.Pattern",
	"Border.Top",
	"Border.Bottom",
	"Border.Left",
	"Border.Right",
	"Border.RevDiagonal",
	"Border.Diagonal",
	"Pattern",
	"--MaxBlank--",
	"Color.Fore",
	"Font.Name",
	"Font.Bold",
	"Font.Italic",
	"Font.Underline",
	"Font.Strikethrough",
	"Font.Size",
	"Format",
	"Align.v",
	"Align.h",
	"Indent",
	"Rotation",
	"WrapText",
	"ShrinkToFit",
	"Content.Locked",
	"Content.Hidden",
	"Validation",
	"Hyper Link",
	"Input Msg"
};

/* Some ref/link count debugging */
#if 0
#define d(arg)	printf arg
#else
#define d(arg)	do { } while (0)
#endif

static guint
mstyle_hash_internal (gconstpointer st, int i)
{
	const GnmStyle *mstyle = (const GnmStyle *)st;
	guint32 hash = 0;

	while (i-- > (MSTYLE_ELEMENT_CONFLICT + 1)) {
		const MStyleElement *e = &mstyle->elements[i];
		hash = (hash << 7) ^ (hash >> (sizeof (hash) * 8 - 7));
		switch (i) {
		case MSTYLE_ANY_COLOR:
			/* auto colours break things */
			if (!e->u.color.any->is_auto)
				hash = hash ^ GPOINTER_TO_UINT (e->u.color.any);
			break;
		case MSTYLE_ANY_BORDER:
			hash = hash ^ GPOINTER_TO_UINT (e->u.border.any);
			break;
		case MSTYLE_ANY_POINTER:
			/*
			 * FIXME FIXME FIXME
			 * Will someone please convince me that it is safe
			 * to use the raw pointers here?  -- MW.
			 */
			hash = hash ^ GPOINTER_TO_UINT (e->u.any_pointer);
			break;
		case MSTYLE_ELEMENT_MAX_BLANK: /* A dummy element */
			break;
		case MSTYLE_ANY_BOOLEAN:
			if (e->u.any_boolean)
				hash = hash ^ 0x1379;
			break;
		case MSTYLE_ANY_FLOAT:
			hash = hash ^ ((int)(e->u.any_float * 97));
			break;
		case MSTYLE_ANY_GUINT16:
			hash = hash ^ e->u.any_guint16;
			break;
		case MSTYLE_ANY_GUINT32:
			hash = hash ^ e->u.any_guint32;
			break;
		case MSTYLE_INDENT:
			hash = hash ^ e->u.indent;
			break;
		case MSTYLE_FONT_UNDERLINE:
			hash = hash ^ e->u.font.underline;
			break;

#ifndef DEBUG_SWITCH_ENUM
		default:
			g_assert_not_reached ();
			break;
#endif
		}
	}

	return hash;
}

guint
mstyle_hash_XL (gconstpointer st)
{
	return mstyle_hash_internal (st, MSTYLE_VALIDATION);
}

guint
mstyle_hash (gconstpointer st)
{
	return mstyle_hash_internal (st, MSTYLE_ELEMENT_MAX);
}


static char *
mstyle_element_dump (const MStyleElement *e)
{
	GString *ans = g_string_new (NULL);
	char    *txt_ans;

	/* This leaks ans from above.  Let's consider that a feature.  */
	g_return_val_if_fail (e != NULL, g_strdup ("Duff element"));

	switch (e->type) {
	case MSTYLE_ELEMENT_UNSET:
		g_string_printf (ans, "\tUnset\n");
		break;
	case MSTYLE_COLOR_BACK:
		g_string_printf (ans, "\tbackground col %hx:%hx:%hx\n",
				 e->u.color.any->color.red,
				 e->u.color.any->color.green,
				 e->u.color.any->color.blue);
		break;
	case MSTYLE_COLOR_PATTERN:
		g_string_printf (ans, "\tpattern col %hx:%hx:%hx\n",
				 e->u.color.any->color.red,
				 e->u.color.any->color.green,
				 e->u.color.any->color.blue);
		break;
	case MSTYLE_BORDER_TOP:
	case MSTYLE_BORDER_BOTTOM:
	case MSTYLE_BORDER_LEFT:
	case MSTYLE_BORDER_RIGHT:
	case MSTYLE_BORDER_DIAGONAL:
	case MSTYLE_BORDER_REV_DIAGONAL:
		if (e->u.border.any)
			g_string_printf (ans, "\t%s %d\n", mstyle_names[e->type], e->u.border.any->line_type);
		else
			g_string_printf (ans, "\t%s blank\n", mstyle_names[e->type]);
		break;

	case MSTYLE_PATTERN :
		g_string_printf (ans, "\tpattern %d\n", e->u.pattern);
		break;

	case MSTYLE_COLOR_FORE:
		g_string_printf (ans, "\tforegnd col %hx:%hx:%hx\n",
				 e->u.color.any->color.red,
				 e->u.color.any->color.green,
				 e->u.color.any->color.blue);
		break;
	case MSTYLE_FONT_NAME:
		g_string_printf (ans, "\tname '%s'\n", e->u.font.name->str);
		break;
	case MSTYLE_FONT_BOLD:
		if (e->u.font.bold)
			g_string_printf (ans, "\tbold\n");
		else
			g_string_printf (ans, "\tnot bold\n");
		break;
	case MSTYLE_FONT_ITALIC:
		if (e->u.font.italic)
			g_string_printf (ans, "\titalic\n");
		else
			g_string_printf (ans, "\tnot italic\n");
		break;
	case MSTYLE_FONT_UNDERLINE:
		switch (e->u.font.underline) {
		default :
		case UNDERLINE_NONE :
			g_string_printf (ans, "\tnot underline\n");
		case UNDERLINE_SINGLE :
			g_string_printf (ans, "\tsingle underline\n");
		case UNDERLINE_DOUBLE :
			g_string_printf (ans, "\tdouble underline\n");
		};
		break;
	case MSTYLE_FONT_STRIKETHROUGH:
		if (e->u.font.strikethrough)
			g_string_printf (ans, "\tstrikethrough\n");
		else
			g_string_printf (ans, "\tnot strikethrough\n");
		break;
	case MSTYLE_FONT_SIZE:
		g_string_printf (ans, "\tsize %f\n", e->u.font.size);
		break;

	case MSTYLE_FORMAT: {
		char *fmt = style_format_as_XL (e->u.format, TRUE);
		g_string_printf (ans, "\tformat '%s'\n", fmt);
		g_free (fmt);
		break;
	}

	case MSTYLE_ALIGN_V:
		g_string_printf (ans, "\tvalign %hd\n", e->u.align.v);
		break;
	case MSTYLE_ALIGN_H:
		g_string_printf (ans, "\thalign %hd\n", e->u.align.h);
		break;
	case MSTYLE_INDENT:
		g_string_printf (ans, "\tindent %d\n", e->u.indent);
		break;
	case MSTYLE_ROTATION:
		g_string_printf (ans, "\trotation %d\n", e->u.rotation);
		break;

	case MSTYLE_WRAP_TEXT :
		g_string_printf (ans, "\twrap text %d\n", e->u.wrap_text);
		break;
	case MSTYLE_SHRINK_TO_FIT :
		g_string_printf (ans, "\tshrink to fit %d\n", e->u.shrink_to_fit);
		break;
	case MSTYLE_CONTENT_LOCKED :
		g_string_printf (ans, "\tlocked %d\n", e->u.content_locked);
		break;
	case MSTYLE_CONTENT_HIDDEN :
		g_string_printf (ans, "\thidden %d\n", e->u.content_hidden);
		break;
	case MSTYLE_VALIDATION :
		g_string_printf (ans, "\tvalidation %p\n", e->u.validation);
		break;

	case MSTYLE_HLINK :
		g_string_printf (ans, "\thlink %p\n", e->u.hlink);
		break;

	case MSTYLE_INPUT_MSG :
		g_string_printf (ans, "\tinput msg %p\n", e->u.input_msg);
		break;

	default:
		g_string_printf (ans, "\t%s\n", mstyle_names[e->type]);
		break;
	}

	txt_ans = ans->str;
	g_string_free (ans, FALSE);

	return txt_ans;
}

static gboolean
mstyle_element_equal (MStyleElement const *a,
		      MStyleElement const *b)
{
	if ((a->type == MSTYLE_ELEMENT_UNSET ||
	     b->type == MSTYLE_ELEMENT_UNSET) && a->type != b->type)
		return FALSE;

	g_return_val_if_fail (a->type == b->type, FALSE);

	switch (a->type) {
	case MSTYLE_ANY_COLOR:
		return (a->u.color.any == b->u.color.any ||
			(a->u.color.any->is_auto && b->u.color.any->is_auto));
	case MSTYLE_ANY_BORDER:
		return (a->u.border.any == b->u.border.any);
	case MSTYLE_PATTERN:
		return (a->u.pattern == b->u.pattern);
	case MSTYLE_FONT_NAME:
		return (a->u.font.name == b->u.font.name);
	case MSTYLE_FONT_BOLD:
		return (a->u.font.bold == b->u.font.bold);
	case MSTYLE_FONT_ITALIC:
		return (a->u.font.italic == b->u.font.italic);
	case MSTYLE_FONT_UNDERLINE:
		return (a->u.font.underline == b->u.font.underline);
	case MSTYLE_FONT_STRIKETHROUGH:
		return (a->u.font.strikethrough == b->u.font.strikethrough);
	case MSTYLE_FONT_SIZE:
		return (a->u.font.size == b->u.font.size);
	case MSTYLE_FORMAT:
		return (a->u.format == b->u.format);
	case MSTYLE_ALIGN_V:
		return (a->u.align.v == b->u.align.v);
	case MSTYLE_ALIGN_H:
		return (a->u.align.h == b->u.align.h);
	case MSTYLE_INDENT:
		return (a->u.indent == b->u.indent);
	case MSTYLE_ROTATION:
		return (a->u.rotation == b->u.rotation);
	case MSTYLE_WRAP_TEXT:
		return (a->u.wrap_text == b->u.wrap_text);
	case MSTYLE_SHRINK_TO_FIT:
		return (a->u.shrink_to_fit == b->u.shrink_to_fit);
	case MSTYLE_CONTENT_LOCKED:
		return (a->u.content_locked == b->u.content_locked);
	case MSTYLE_CONTENT_HIDDEN:
		return (a->u.content_hidden == b->u.content_hidden);
	case MSTYLE_VALIDATION:
		return (a->u.validation == b->u.validation);
	case MSTYLE_HLINK:
		return (a->u.hlink == b->u.hlink);
	case MSTYLE_INPUT_MSG:
		return (a->u.input_msg == b->u.input_msg);
	default:
		return TRUE;
	}

	return FALSE;
}

static inline MStyleElement
mstyle_element_ref (const MStyleElement *e)
{
	switch (e->type) {
	case MSTYLE_ANY_COLOR:
		style_color_ref (e->u.color.any);
		break;
	case MSTYLE_ANY_BORDER:
		style_border_ref (e->u.border.any);
		break;
	case MSTYLE_FONT_NAME:
		gnm_string_ref (e->u.font.name);
		break;
	case MSTYLE_FORMAT:
		style_format_ref (e->u.format);
		break;
#if 0
	case MSTYLE_VALIDATION:
		if (e->u.validation)
			validation_ref (e->u.validation);
		break;
#endif //0
	case MSTYLE_HLINK:
		if (e->u.hlink)
			g_object_ref (G_OBJECT (e->u.hlink));
		break;
	case MSTYLE_INPUT_MSG:
		if (e->u.input_msg)
			g_object_ref (G_OBJECT (e->u.input_msg));
		break;
	default:
		break;
	}
	return *e;
}

static inline void
mstyle_element_unref (MStyleElement e)
{
	switch (e.type) {
	case MSTYLE_ANY_COLOR:
		style_color_unref (e.u.color.any);
		break;
	case MSTYLE_ANY_BORDER:
		style_border_unref (e.u.border.any);
		break;
	case MSTYLE_FONT_NAME:
		gnm_string_unref (e.u.font.name);
		break;
	case MSTYLE_FORMAT:
		style_format_unref (e.u.format);
		break;
#if 0
	case MSTYLE_VALIDATION:
		if (e.u.validation)
			validation_unref (e.u.validation);
		break;
#endif // 0
	case MSTYLE_HLINK:
		if (e.u.hlink)
			g_object_unref (G_OBJECT (e.u.hlink));
		break;
	case MSTYLE_INPUT_MSG:
		if (e.u.input_msg)
			g_object_unref (G_OBJECT (e.u.input_msg));
		break;
	default:
		break;
	}
}

/**
 * mstyle_elements_compare:
 * @a: style to be tagged
 * @b: style to compare.
 *
 * Compares styles and tags conflicts into a.
 **/
static inline void
mstyle_elements_compare (MStyleElement *a,
			 const MStyleElement *b)
{
	int i;

	g_return_if_fail (a != NULL);
	g_return_if_fail (b != NULL);

	for (i = 0; i < MSTYLE_ELEMENT_MAX; i++) {
		if (b[i].type == MSTYLE_ELEMENT_UNSET ||
		    b[i].type == MSTYLE_ELEMENT_CONFLICT ||
		    a[i].type == MSTYLE_ELEMENT_CONFLICT)
			continue;
		if (a[i].type == MSTYLE_ELEMENT_UNSET) {
			mstyle_element_ref (&b[i]);
			a[i] = b[i];
		} else if (!mstyle_element_equal (a+i, b+i)) {
			mstyle_element_unref (a[i]);
			a[i].type = MSTYLE_ELEMENT_CONFLICT;
		}
	}

}

void
mstyle_compare (GnmStyle *a, const GnmStyle *b)
{
	mstyle_elements_compare (a->elements,
				 b->elements);
}

static void
mstyle_elements_unref (MStyleElement *e)
{
	int i;

	if (e)
		for (i = 0; i < MSTYLE_ELEMENT_MAX; i++) {
			mstyle_element_unref (e[i]);
			e[i].type = MSTYLE_ELEMENT_UNSET;
		}
}

static void
mstyle_elements_copy (GnmStyle *new_style, const GnmStyle *old_style)
{
	int                  i;
	MStyleElement       *ans;
	const MStyleElement *e;

	e   = old_style->elements;
	ans = new_style->elements;

	for (i = 0; i < MSTYLE_ELEMENT_MAX; i++) {
		mstyle_element_ref (&e[i]);
		ans[i] = e[i];
	}
}

static inline void
mstyle_pango_clear (GnmStyle *mstyle)
{
	if (mstyle->pango_attrs) {
		pango_attr_list_unref (mstyle->pango_attrs);
		mstyle->pango_attrs = NULL;
	}
}


static inline void
mstyle_font_clear (GnmStyle *mstyle)
{
	if (mstyle->font) {
		style_font_unref (mstyle->font);
		mstyle->font = NULL;
	}
}


GnmStyle *
mstyle_new (void)
{
	GnmStyle *style = CHUNK_ALLOC0 (GnmStyle, mstyle_pool);

	style->ref_count = 1;
	style->link_count = 0;
	style->linked_sheet = NULL;
	style->pango_attrs = NULL;
	style->font = NULL;
	d(("new %p\n", style));

	return style;
}

GnmStyle *
mstyle_copy (const GnmStyle *style)
{
	GnmStyle *new_style = CHUNK_ALLOC (GnmStyle, mstyle_pool);

	new_style->ref_count = 1;
	new_style->link_count = 0;
	new_style->linked_sheet = NULL;
	mstyle_elements_copy (new_style, style);

	if ((new_style->pango_attrs = style->pango_attrs))
		pango_attr_list_ref (new_style->pango_attrs);
	if ((new_style->font = style->font)) {
		style_font_ref (new_style->font);
		new_style->font_zoom = style->font_zoom;
	}

	d(("copy %p\n", new_style));
	return new_style;
}

GnmStyle *
mstyle_copy_merge (const GnmStyle *orig, const GnmStyle *overlay)
{
	int i;
	GnmStyle *res = CHUNK_ALLOC0 (GnmStyle, mstyle_pool);

	MStyleElement       *res_e;
	const MStyleElement *orig_e;
	const MStyleElement *overlay_e;

	res->ref_count = 1;
	res->link_count = 0;
	res->linked_sheet = NULL;
	res_e = res->elements;
	orig_e = orig->elements;
	overlay_e = overlay->elements;

	for (i = 0; i < MSTYLE_ELEMENT_MAX; i++)
		res_e[i] = mstyle_element_ref (
			(overlay_e[i].type ? overlay_e : orig_e) + i);

	d(("copy merge %p\n", res));
	return res;
}

/**
 * mstyle_new_default:
 *
 * Return the default style,
 * this should _never_ _ever_ have any of its elements
 * set.
 *
 * Return value: the default style.
 **/

GnmStyle *
mstyle_new_default (void)
{
	GnmStyle *mstyle = mstyle_new ();

	mstyle_set_font_name	(mstyle, gnm_app_prefs->default_font.name);
	mstyle_set_font_size	(mstyle, gnm_app_prefs->default_font.size);
	mstyle_set_font_bold	(mstyle, gnm_app_prefs->default_font.is_bold);
	mstyle_set_font_italic	(mstyle, gnm_app_prefs->default_font.is_italic);

	mstyle_set_format_text (mstyle, "General");
	mstyle_set_align_v     (mstyle, VALIGN_BOTTOM);
	mstyle_set_align_h     (mstyle, HALIGN_GENERAL);
	mstyle_set_indent      (mstyle, 0);
	mstyle_set_rotation    (mstyle, 0);
	mstyle_set_wrap_text   (mstyle, FALSE);
	mstyle_set_shrink_to_fit (mstyle, FALSE);
	mstyle_set_content_locked (mstyle, TRUE);
	mstyle_set_content_hidden (mstyle, FALSE);
	mstyle_set_font_uline  (mstyle, UNDERLINE_NONE);
	mstyle_set_font_strike (mstyle, FALSE);

	mstyle_set_hlink       (mstyle, NULL);
	mstyle_set_input_msg   (mstyle, NULL);
	mstyle_set_validation  (mstyle, NULL);

	mstyle_set_color       (mstyle, MSTYLE_COLOR_FORE,
				style_color_black ());
	mstyle_set_color       (mstyle, MSTYLE_COLOR_BACK,
				style_color_white ());
	mstyle_set_color       (mstyle, MSTYLE_COLOR_PATTERN,
				style_color_black ());

	/* To negate borders */
	mstyle_set_border      (mstyle, MSTYLE_BORDER_TOP,
				style_border_ref (style_border_none ()));
	mstyle_set_border      (mstyle, MSTYLE_BORDER_LEFT,
				style_border_ref (style_border_none ()));
	mstyle_set_border      (mstyle, MSTYLE_BORDER_BOTTOM,
				style_border_ref (style_border_none ()));
	mstyle_set_border      (mstyle, MSTYLE_BORDER_RIGHT,
				style_border_ref (style_border_none ()));
	mstyle_set_border      (mstyle, MSTYLE_BORDER_DIAGONAL,
				style_border_ref (style_border_none ()));
	mstyle_set_border      (mstyle, MSTYLE_BORDER_REV_DIAGONAL,
				style_border_ref (style_border_none ()));

	/* This negates the back and pattern colors */
	mstyle_set_pattern     (mstyle, 0);

	return mstyle;
}

void
mstyle_ref (GnmStyle *style)
{
	g_return_if_fail (style->ref_count > 0);

	style->ref_count++;
	d(("ref %p = %d\n", style, style->ref_count));
}

void
mstyle_unref (GnmStyle *style)
{
	g_return_if_fail (style->ref_count > 0);

	d(("unref %p = %d\n", style, style->ref_count-1));
	if (style->ref_count-- <= 1) {
		g_return_if_fail (style->link_count == 0);
		g_return_if_fail (style->linked_sheet == NULL);

		if (style->elements)
			mstyle_elements_unref (style->elements);
		mstyle_pango_clear (style);
		mstyle_font_clear (style);

		CHUNK_FREE (mstyle_pool, style);
	}
}

/**
 * Replace auto pattern color in style with sheet's auto pattern color.
 * make_copy tells if we are allowed to modify the style in place or we must
 * make a copy first.
 */
static GnmStyle *
link_pattern_color (GnmStyle *style, GnmColor *auto_color, gboolean make_copy)
{
	MStyleElementType etype = MSTYLE_COLOR_PATTERN;
	GnmColor *pattern_color = style->elements[etype].u.color.any;

	if (pattern_color->is_auto && auto_color != pattern_color) {
		style_color_ref (auto_color);
		if (make_copy) {
			GnmStyle *orig = style;
			style = mstyle_copy (style);
			mstyle_unref (orig);
		}
		mstyle_set_color (style, etype, auto_color);
	}
	return style;
}

/**
 * Replace auto border colors in style with sheet's auto pattern
 * color. (pattern is *not* a typo.)
 * make_copy tells if we are allowed to modify the style in place or we must
 * make a copy first.
 *
 * FIXME: We conjecture that XL color 64 in border should change with the
 * pattern, but not color 127. That distinction is not yet represented in
 * our data structures.
 */
static GnmStyle *
link_border_colors (GnmStyle *style, GnmColor *auto_color, gboolean make_copy)
{
	GnmBorder *border;
	GnmColor *color;
	int i;

	for (i = MSTYLE_BORDER_TOP ; i <= MSTYLE_BORDER_DIAGONAL ; ++i) {
		if (mstyle_is_element_set (style, i)) {
			border = style->elements[i].u.border.any;
			color = border->color;
			if (color->is_auto && auto_color != color) {
				GnmBorder *new_border;
				StyleBorderOrientation orientation;

				switch (i) {
				case MSTYLE_BORDER_LEFT:
				case MSTYLE_BORDER_RIGHT:
					orientation = STYLE_BORDER_VERTICAL;
					break;
				case MSTYLE_BORDER_REV_DIAGONAL:
				case MSTYLE_BORDER_DIAGONAL:
					orientation = STYLE_BORDER_DIAGONAL;
					break;
				case MSTYLE_BORDER_TOP:
				case MSTYLE_BORDER_BOTTOM:
				default:
					orientation = STYLE_BORDER_HORIZONTAL;
					break;
				}
				style_color_ref (auto_color);
				new_border = style_border_fetch (
					border->line_type, auto_color,
					orientation);

				if (make_copy) {
					GnmStyle *orig = style;
					style = mstyle_copy (style);
					mstyle_unref (orig);
					make_copy = FALSE;
				}
				mstyle_set_border (style, i, new_border);
			}
		}
	}
	return style;
}

/**
 * mstyle_link_sheet :
 * @style :
 * @sheet :
 *
 * ABSORBS a reference to the style and sets the link count to 1.
 *
 * Where auto pattern color occurs in the style (it may for pattern and
 * borders), it is replaced with the sheet's auto pattern color. We make
 * sure that we do not modify the style which was passed in to us, but also
 * that we don't copy more than once. The final argument to the
 * link_xxxxx_color functions tell whether or not to copy.
 */
GnmStyle *
mstyle_link_sheet (GnmStyle *style, Sheet *sheet)
{
	GnmColor *auto_color;
	gboolean style_is_orig = TRUE;

	if (style->linked_sheet != NULL) {
		GnmStyle *orig = style;
		style = mstyle_copy (style);
		mstyle_unref (orig);
		style_is_orig = FALSE;

		/* safety test */
		g_return_val_if_fail (style->linked_sheet != sheet, style);
	}

	g_return_val_if_fail (style->link_count == 0, style);
	g_return_val_if_fail (style->linked_sheet == NULL, style);

	//auto_color = sheet_style_get_auto_pattern_color (sheet);
        auto_color = style_color_black();
	if (mstyle_is_element_set (style, MSTYLE_COLOR_PATTERN))
		style = link_pattern_color (style, auto_color, style_is_orig);
	style = link_border_colors (style, auto_color, style_is_orig);
	style_color_unref (auto_color);

	style->linked_sheet = sheet;
	style->link_count = 1;

#if 0
	/* Not needed for validation anymore, leave it as template for conditionals */
	if (mstyle_is_element_set (style, MSTYLE_VALIDATION))
		validation_link (style->elements[MSTYLE_VALIDATION].u.validation, sheet);
#endif

	d(("link sheet %p = 1\n", style));
	return style;
}

void
mstyle_link (GnmStyle *style)
{
	g_return_if_fail (style->link_count > 0);

	style->link_count++;
	d(("link %p = %d\n", style, style->link_count));
}

void
mstyle_link_multiple (GnmStyle *style, int count)
{
	g_return_if_fail (style->link_count > 0);

	style->link_count += count;
	d(("multiple link %p + %d = %d\n", style, count, style->link_count));
}

void
mstyle_unlink (GnmStyle *style)
{
	g_return_if_fail (style->link_count > 0);

	d(("unlink %p = %d\n", style, style->link_count-1));
	if (style->link_count-- == 1) {
#if 0
		/* Not needed for validation anymore, leave it as template for conditionals */
		if (mstyle_is_element_set (style, MSTYLE_VALIDATION))
			validation_unlink (style->elements[MSTYLE_VALIDATION].u.validation);
#endif
		//sheet_style_unlink (style->linked_sheet, style);
		style->linked_sheet = NULL;
		mstyle_unref (style);
	}
}

char *
mstyle_to_string (const GnmStyle *style)
{
	guint i;
	GString *ans;
	char *txt_ans;

	g_return_val_if_fail (style != NULL, g_strdup ("(null)"));

	ans = g_string_new ("Elements : ");
	for (i = 0; i < MSTYLE_ELEMENT_MAX; i++) {
		char *txt;

		if (style->elements[i].type) {
			txt = mstyle_element_dump (&style->elements[i]);
			g_string_append_printf (ans, "%s ", txt);
			g_free (txt);
		} else
			g_string_append_printf (ans, ".\n");
	}
	txt_ans = ans->str;
	g_string_free (ans, FALSE);

	return txt_ans;
}

void
mstyle_dump (const GnmStyle *style)
{
	char *txt;

	fprintf (stderr, "Style Refs %d\n",
		 style->ref_count);
	txt = mstyle_to_string (style);
	fprintf (stderr, "%s\n", txt);
	g_free (txt);
}

gboolean
mstyle_equal (const GnmStyle *a, const GnmStyle *b)
{
	int i;
	MStyleElement const *ea, *eb;

	g_return_val_if_fail (a != NULL, FALSE);
	g_return_val_if_fail (b != NULL, FALSE);

	if (a == b)
		return TRUE;

	ea = a->elements;
	eb = b->elements;
	for (i = 1; i < MSTYLE_ELEMENT_MAX; i++) {
		/* Elements in the same position should have the same types */
		if (ea[i].type != eb[i].type) {
			if (ea[i].type != MSTYLE_ELEMENT_UNSET &&
			    eb[i].type != MSTYLE_ELEMENT_UNSET)
				g_warning ("%s mismatched types.", mstyle_names[i]);
			return FALSE;
		}

		if (!mstyle_element_equal (ea+i, eb+i))
			return FALSE;
	}

	return TRUE;
}

gboolean
mstyle_equal_XL (const GnmStyle *a, const GnmStyle *b)
{
	int i;
	MStyleElement const *ea, *eb;

	g_return_val_if_fail (a != NULL, FALSE);
	g_return_val_if_fail (b != NULL, FALSE);

	if (a == b)
		return TRUE;

	ea = a->elements;
	eb = b->elements;
	for (i = 1; i < MSTYLE_VALIDATION; i++) {
		/* Elements in the same position should have the same types */
		if (ea[i].type != eb[i].type) {
			if (ea[i].type != MSTYLE_ELEMENT_UNSET &&
			    eb[i].type != MSTYLE_ELEMENT_UNSET)
				g_warning ("%s mismatched types.", mstyle_names[i]);
			return FALSE;
		}

		if (!mstyle_element_equal (ea+i, eb+i))
			return FALSE;
	}

	return TRUE;
}

gboolean
mstyle_empty (const GnmStyle *style)
{
	int i;

	g_return_val_if_fail (style != NULL, FALSE);

	for (i = 0; i < MSTYLE_ELEMENT_MAX; i++)
		if (style->elements[i].type)
			return FALSE;
	return TRUE;
}

gboolean
mstyle_verify (const GnmStyle *style)
{
	int j;

	for (j = 0; j < MSTYLE_ELEMENT_MAX; j++) {
		MStyleElement e = style->elements[j];

		g_return_val_if_fail (e.type <  MSTYLE_ELEMENT_MAX, FALSE);
		g_return_val_if_fail (e.type != MSTYLE_ELEMENT_CONFLICT, FALSE);
	}
	return TRUE;
}

gboolean
mstyle_is_element_set (const GnmStyle *st, MStyleElementType t)
{
	g_return_val_if_fail (st != NULL, FALSE);
	g_return_val_if_fail (t > 0 && t < MSTYLE_ELEMENT_MAX, FALSE);

	return  st->elements[t].type != MSTYLE_ELEMENT_UNSET &&
		st->elements[t].type != MSTYLE_ELEMENT_CONFLICT;
}

gboolean
mstyle_is_element_conflict (const GnmStyle *st, MStyleElementType t)
{
	g_return_val_if_fail (st != NULL, FALSE);
	g_return_val_if_fail (t > 0 && t < MSTYLE_ELEMENT_MAX, FALSE);

	return st->elements[t].type == MSTYLE_ELEMENT_CONFLICT;
}

void
mstyle_unset_element (GnmStyle *st, MStyleElementType t)
{
	g_return_if_fail (st != NULL);
	g_return_if_fail (t > 0 && t < MSTYLE_ELEMENT_MAX);

	mstyle_element_unref (st->elements[t]);
	st->elements[t].type = MSTYLE_ELEMENT_UNSET;
}

/**
 * mstyle_replace_element:
 * @src: Source mstyle
 * @dst: Destination mstyle
 * @t: Element to replace
 *
 * This function replaces element 't' in mstyle 'dst' with element 't'
 * in mstyle 'src'. (If element 't' was already set in mstyle 'dst' then
 * the element will first be unset)
 **/
void
mstyle_replace_element (GnmStyle *src, GnmStyle *dst, MStyleElementType t)
{
	g_return_if_fail (src != NULL);
	g_return_if_fail (dst != NULL);

	mstyle_element_ref (&src->elements[t]);

	if (mstyle_is_element_set (dst, t))
		mstyle_unset_element (dst, t);

	dst->elements[t] = src->elements[t];
}

void
mstyle_set_color (GnmStyle *st, MStyleElementType t,
		  GnmColor *col)
{
	g_return_if_fail (st != NULL);
	g_return_if_fail (col != NULL);

	switch (t) {
	case MSTYLE_ANY_COLOR:
		mstyle_element_unref (st->elements[t]);
		st->elements[t].type = t;
		st->elements[t].u.color.any = col;
		mstyle_pango_clear (st);
		break;
	default:
		g_warning ("Not a color element");
		break;
	}
}

GnmColor *
mstyle_get_color (GnmStyle const *st, MStyleElementType t)
{
	g_return_val_if_fail (mstyle_is_element_set (st, t), NULL);

	switch (t) {
	case MSTYLE_ANY_COLOR:
		return st->elements[t].u.color.any;

	default:
		g_warning ("Not a color element");
		return NULL;
	}
}

void
mstyle_set_border (GnmStyle *st, MStyleElementType t,
		   GnmBorder *border)
{
	g_return_if_fail (st != NULL);

	/* NOTE : It is legal for border to be NULL */
	switch (t) {
	case MSTYLE_ANY_BORDER:
		mstyle_element_unref (st->elements[t]);
		st->elements[t].type = t;
		st->elements[t].u.border.any = border;
		break;
	default:
		g_warning ("Not a border element");
		break;
	}

}

GnmBorder *
mstyle_get_border (const GnmStyle *st, MStyleElementType t)
{
	switch (t) {
	case MSTYLE_ANY_BORDER:
		return st->elements[t].u.border.any;

	default:
		g_warning ("Not a border element");
		return NULL;
	}
}

void
mstyle_set_pattern (GnmStyle *st, int pattern)
{
	g_return_if_fail (st != NULL);
	g_return_if_fail (pattern >= 0);
	//g_return_if_fail (pattern <= GNUMERIC_SHEET_PATTERNS);

	st->elements[MSTYLE_PATTERN].type = MSTYLE_PATTERN;
	st->elements[MSTYLE_PATTERN].u.pattern = pattern;
}

int
mstyle_get_pattern (const GnmStyle *style)
{
	g_return_val_if_fail (mstyle_is_element_set (style, MSTYLE_PATTERN), 0);

	return style->elements[MSTYLE_PATTERN].u.pattern;
}

GnmFont *
mstyle_get_font (const GnmStyle *style, PangoContext *context, double zoom)
{
	g_return_val_if_fail (style != NULL, NULL);

	if (!style->font || style->font_zoom != zoom) {
		const gchar *name;
		gboolean bold, italic;
		double size;

		mstyle_font_clear ((GnmStyle *)style);

		if (mstyle_is_element_set (style, MSTYLE_FONT_NAME))
			name = mstyle_get_font_name (style);
		else
			name = DEFAULT_FONT;

		if (mstyle_is_element_set (style, MSTYLE_FONT_BOLD))
			bold = mstyle_get_font_bold (style);
		else
			bold = FALSE;

		if (mstyle_is_element_set (style, MSTYLE_FONT_ITALIC))
			italic = mstyle_get_font_italic (style);
		else
			italic = FALSE;

		if (mstyle_is_element_set (style, MSTYLE_FONT_SIZE))
			size = mstyle_get_font_size (style);
		else
			size = DEFAULT_SIZE;

		((GnmStyle *)style)->font =
			style_font_new (context, name, size,
					zoom, bold, italic);
		((GnmStyle *)style)->font_zoom = zoom;
	}

	style_font_ref (style->font);
	return style->font;
}

void
mstyle_set_font_name (GnmStyle *style, const char *name)
{
	g_return_if_fail (name != NULL);
	g_return_if_fail (style != NULL);

	mstyle_element_unref (style->elements[MSTYLE_FONT_NAME]);
	style->elements[MSTYLE_FONT_NAME].type = MSTYLE_FONT_NAME;
	style->elements[MSTYLE_FONT_NAME].u.font.name = gnm_string_get (name);
	mstyle_font_clear (style);
	mstyle_pango_clear (style);
}

const char *
mstyle_get_font_name (const GnmStyle *style)
{
	g_return_val_if_fail (mstyle_is_element_set (style, MSTYLE_FONT_NAME), NULL);

	return style->elements[MSTYLE_FONT_NAME].u.font.name->str;
}

void
mstyle_set_font_bold (GnmStyle *style, gboolean bold)
{
	g_return_if_fail (style != NULL);

	style->elements[MSTYLE_FONT_BOLD].type = MSTYLE_FONT_BOLD;
	style->elements[MSTYLE_FONT_BOLD].u.font.bold = bold;
	mstyle_font_clear (style);
	mstyle_pango_clear (style);
}

gboolean
mstyle_get_font_bold (const GnmStyle *style)
{
	g_return_val_if_fail (mstyle_is_element_set (style, MSTYLE_FONT_BOLD), FALSE);

	return style->elements[MSTYLE_FONT_BOLD].u.font.bold;
}

void
mstyle_set_font_italic (GnmStyle *style, gboolean italic)
{
	g_return_if_fail (style != NULL);

	style->elements[MSTYLE_FONT_ITALIC].type = MSTYLE_FONT_ITALIC;
	style->elements[MSTYLE_FONT_ITALIC].u.font.italic = italic;
	mstyle_font_clear (style);
	mstyle_pango_clear (style);
}

gboolean
mstyle_get_font_italic (const GnmStyle *style)
{
	g_return_val_if_fail (mstyle_is_element_set (style, MSTYLE_FONT_ITALIC), FALSE);

	return style->elements[MSTYLE_FONT_ITALIC].u.font.italic;
}

void
mstyle_set_font_uline (GnmStyle *style, StyleUnderlineType const underline)
{
	g_return_if_fail (style != NULL);

	style->elements[MSTYLE_FONT_UNDERLINE].type = MSTYLE_FONT_UNDERLINE;
	style->elements[MSTYLE_FONT_UNDERLINE].u.font.underline = underline;
	mstyle_pango_clear (style);
}

StyleUnderlineType
mstyle_get_font_uline (const GnmStyle *style)
{
	g_return_val_if_fail (mstyle_is_element_set (style, MSTYLE_FONT_UNDERLINE), FALSE);

	return style->elements[MSTYLE_FONT_UNDERLINE].u.font.underline;
}

void
mstyle_set_font_strike (GnmStyle *style, gboolean const strikethrough)
{
	g_return_if_fail (style != NULL);

	style->elements[MSTYLE_FONT_STRIKETHROUGH].type = MSTYLE_FONT_STRIKETHROUGH;
	style->elements[MSTYLE_FONT_STRIKETHROUGH].u.font.strikethrough = strikethrough;
	mstyle_pango_clear (style);
}

gboolean
mstyle_get_font_strike (const GnmStyle *style)
{
	g_return_val_if_fail (mstyle_is_element_set (style, MSTYLE_FONT_STRIKETHROUGH), FALSE);

	return style->elements[MSTYLE_FONT_STRIKETHROUGH].u.font.strikethrough;
}
void
mstyle_set_font_size (GnmStyle *style, double size)
{
	g_return_if_fail (style != NULL);
	g_return_if_fail (size >= 1.);

	style->elements[MSTYLE_FONT_SIZE].type = MSTYLE_FONT_SIZE;
	style->elements[MSTYLE_FONT_SIZE].u.font.size = size;
	mstyle_font_clear (style);
	mstyle_pango_clear (style);
}

double
mstyle_get_font_size (const GnmStyle *style)
{
	g_return_val_if_fail (mstyle_is_element_set (style, MSTYLE_FONT_SIZE), 12.0);

	return style->elements[MSTYLE_FONT_SIZE].u.font.size;
}

void
mstyle_set_format (GnmStyle *style, GnmFormat *format)
{
	g_return_if_fail (style != NULL);
	g_return_if_fail (format != NULL);

	style_format_ref (format);
	mstyle_element_unref (style->elements[MSTYLE_FORMAT]);
	style->elements[MSTYLE_FORMAT].type = MSTYLE_FORMAT;
	style->elements[MSTYLE_FORMAT].u.format = format;
}

void
mstyle_set_format_text (GnmStyle *style, const char *format)
{
	GnmFormat *sf;

	g_return_if_fail (style != NULL);
	g_return_if_fail (format != NULL);

	/* FIXME FIXME FIXME : This is a potential problem
	 * I am not sure people are feeding us only translated formats.
	 * This entire function should be deleted.
	 */
	sf = style_format_new_XL (format, FALSE);
	mstyle_set_format (style, sf);
	style_format_unref (sf);
}

GnmFormat *
mstyle_get_format (GnmStyle const *style)
{
	g_return_val_if_fail (mstyle_is_element_set (style, MSTYLE_FORMAT), NULL);

	return style->elements[MSTYLE_FORMAT].u.format;
}

void
mstyle_set_align_h (GnmStyle *style, StyleHAlignFlags a)
{
	g_return_if_fail (style != NULL);

	style->elements[MSTYLE_ALIGN_H].type = MSTYLE_ALIGN_H;
	style->elements[MSTYLE_ALIGN_H].u.align.h = a;
}

StyleHAlignFlags
mstyle_get_align_h (const GnmStyle *style)
{
	g_return_val_if_fail (mstyle_is_element_set (style, MSTYLE_ALIGN_H), 0);

	return style->elements[MSTYLE_ALIGN_H].u.align.h;
}

void
mstyle_set_align_v (GnmStyle *style, StyleVAlignFlags a)
{
	g_return_if_fail (style != NULL);

	style->elements[MSTYLE_ALIGN_V].type = MSTYLE_ALIGN_V;
	style->elements[MSTYLE_ALIGN_V].u.align.v = a;
}

StyleVAlignFlags
mstyle_get_align_v (const GnmStyle *style)
{
	g_return_val_if_fail (mstyle_is_element_set (style, MSTYLE_ALIGN_V), 0);

	return style->elements[MSTYLE_ALIGN_V].u.align.v;
}

void
mstyle_set_indent (GnmStyle *style, int i)
{
	g_return_if_fail (style != NULL);

	style->elements[MSTYLE_INDENT].type = MSTYLE_INDENT;
	style->elements[MSTYLE_INDENT].u.indent = i;
}

int
mstyle_get_indent (const GnmStyle *style)
{
	g_return_val_if_fail (mstyle_is_element_set (style, MSTYLE_INDENT), 0);

	return style->elements[MSTYLE_INDENT].u.indent;
}

void
mstyle_set_rotation (GnmStyle *style, int rot_deg)
{
	g_return_if_fail (style != NULL);

	style->elements[MSTYLE_ROTATION].type = MSTYLE_ROTATION;
	style->elements[MSTYLE_ROTATION].u.rotation = rot_deg;
}

int
mstyle_get_rotation (const GnmStyle *style)
{
	g_return_val_if_fail (mstyle_is_element_set (style, MSTYLE_ROTATION), 0);

	return style->elements[MSTYLE_ROTATION].u.rotation;
}

void
mstyle_set_wrap_text (GnmStyle *style, gboolean f)
{
	g_return_if_fail (style != NULL);

	style->elements[MSTYLE_WRAP_TEXT].type = MSTYLE_WRAP_TEXT;
	style->elements[MSTYLE_WRAP_TEXT].u.wrap_text = f;
}

gboolean
mstyle_get_wrap_text (const GnmStyle *style)
{
	g_return_val_if_fail (mstyle_is_element_set (style, MSTYLE_WRAP_TEXT), FALSE);

	return style->elements[MSTYLE_WRAP_TEXT].u.wrap_text;
}

/*
 * Same as mstyle_get_wrap_text except that if either halign or valign
 * is _JUSTIFY, the result will be TRUE.
 */
gboolean
mstyle_get_effective_wrap_text (const GnmStyle *style)
{
	g_return_val_if_fail (mstyle_is_element_set (style, MSTYLE_WRAP_TEXT), FALSE);
	g_return_val_if_fail (mstyle_is_element_set (style, MSTYLE_ALIGN_V), FALSE);
	g_return_val_if_fail (mstyle_is_element_set (style, MSTYLE_ALIGN_H), FALSE);

	/* Note: HALIGN_GENERAL never expands to HALIGN_JUSTIFY.  */
	return (style->elements[MSTYLE_WRAP_TEXT].u.wrap_text ||
		style->elements[MSTYLE_ALIGN_V].u.align.v == VALIGN_JUSTIFY ||
		style->elements[MSTYLE_ALIGN_H].u.align.h == HALIGN_JUSTIFY);
}

void
mstyle_set_shrink_to_fit (GnmStyle *style, gboolean f)
{
	g_return_if_fail (style != NULL);

	style->elements[MSTYLE_SHRINK_TO_FIT].type = MSTYLE_SHRINK_TO_FIT;
	style->elements[MSTYLE_SHRINK_TO_FIT].u.wrap_text = f;
}

gboolean
mstyle_get_shrink_to_fit (const GnmStyle *style)
{
	g_return_val_if_fail (mstyle_is_element_set (style, MSTYLE_SHRINK_TO_FIT), FALSE);

	return style->elements[MSTYLE_SHRINK_TO_FIT].u.wrap_text;
}
void
mstyle_set_content_locked (GnmStyle *style, gboolean f)
{
	g_return_if_fail (style != NULL);

	style->elements[MSTYLE_CONTENT_LOCKED].type = MSTYLE_CONTENT_LOCKED;
	style->elements[MSTYLE_CONTENT_LOCKED].u.content_locked = f;
}

gboolean
mstyle_get_content_locked (const GnmStyle *style)
{
	g_return_val_if_fail (mstyle_is_element_set (style, MSTYLE_CONTENT_LOCKED), FALSE);

	return style->elements[MSTYLE_CONTENT_LOCKED].u.content_locked;
}
void
mstyle_set_content_hidden (GnmStyle *style, gboolean f)
{
	g_return_if_fail (style != NULL);

	style->elements[MSTYLE_CONTENT_HIDDEN].type = MSTYLE_CONTENT_HIDDEN;
	style->elements[MSTYLE_CONTENT_HIDDEN].u.content_hidden = f;
}

gboolean
mstyle_get_content_hidden (const GnmStyle *style)
{
	g_return_val_if_fail (mstyle_is_element_set (style, MSTYLE_CONTENT_HIDDEN), FALSE);

	return style->elements[MSTYLE_CONTENT_HIDDEN].u.content_hidden;
}

void
mstyle_set_validation (GnmStyle *style, GnmValidation *v)
{
	g_return_if_fail (style != NULL);

	mstyle_element_unref (style->elements[MSTYLE_VALIDATION]);
	style->elements[MSTYLE_VALIDATION].type = MSTYLE_VALIDATION;
	style->elements[MSTYLE_VALIDATION].u.validation = v;
}

GnmValidation *
mstyle_get_validation (const GnmStyle *style)
{
	g_return_val_if_fail (mstyle_is_element_set (style, MSTYLE_VALIDATION), NULL);

	return style->elements[MSTYLE_VALIDATION].u.validation;
}

void
mstyle_set_hlink (GnmStyle *style, GnmHLink *link)
{
	g_return_if_fail (style != NULL);

	mstyle_element_unref (style->elements[MSTYLE_HLINK]);
	style->elements[MSTYLE_HLINK].type = MSTYLE_HLINK;
	style->elements[MSTYLE_HLINK].u.hlink = link;
}

GnmHLink *
mstyle_get_hlink (const GnmStyle *style)
{
	g_return_val_if_fail (mstyle_is_element_set (style, MSTYLE_HLINK), NULL);

	return style->elements[MSTYLE_HLINK].u.hlink;
}

void
mstyle_set_input_msg (GnmStyle *style, GnmInputMsg *msg)
{
	g_return_if_fail (style != NULL);

	mstyle_element_unref (style->elements[MSTYLE_INPUT_MSG]);
	style->elements[MSTYLE_INPUT_MSG].type = MSTYLE_INPUT_MSG;
	style->elements[MSTYLE_INPUT_MSG].u.input_msg = msg;
}

GnmInputMsg *
mstyle_get_input_msg (const GnmStyle *style)
{
	g_return_val_if_fail (mstyle_is_element_set (style, MSTYLE_INPUT_MSG), NULL);

	return style->elements[MSTYLE_INPUT_MSG].u.input_msg;
}

gboolean
mstyle_visible_in_blank (const GnmStyle *st)
{
	MStyleElementType i;

	if (mstyle_is_element_set (st, MSTYLE_PATTERN) &&
	    mstyle_get_pattern (st) > 0)
		return TRUE;

	for (i = MSTYLE_BORDER_TOP ; i <= MSTYLE_BORDER_DIAGONAL ; ++i)
		if (mstyle_is_element_set (st, i) &&
		    style_border_visible_in_blank (mstyle_get_border (st, i)))
			return TRUE;

	return FALSE;
}

static void
add_attr (PangoAttrList *attrs, PangoAttribute *attr)
{
	attr->start_index = 0;
	attr->end_index = G_MAXINT;
	pango_attr_list_insert (attrs, attr);
}

/**
 * mstyle_get_pango_attrs :
 * @style : #GnmStyle
 **/
PangoAttrList *
mstyle_get_pango_attrs (const GnmStyle *mstyle,
			PangoContext *context,
			double zoom)
{
	PangoAttrList *l;

	if (mstyle->pango_attrs) {
		if (zoom == mstyle->pango_attrs_zoom) {
			pango_attr_list_ref (mstyle->pango_attrs);
			return mstyle->pango_attrs;
		}
		pango_attr_list_unref (((GnmStyle *)mstyle)->pango_attrs);
	}

	((GnmStyle *)mstyle)->pango_attrs = l = pango_attr_list_new ();
	((GnmStyle *)mstyle)->pango_attrs_zoom = zoom;

	/* Foreground colour.  */
	/* See http://bugzilla.gnome.org/show_bug.cgi?id=105322 */
	if (0) {
		const GnmColor *fore = mstyle_get_color (mstyle, MSTYLE_COLOR_FORE);
		add_attr (l, pango_attr_foreground_new (
			fore->color.red, fore->color.green, fore->color.blue));
	}

	/* Handle underlining.  */
	switch (mstyle_get_font_uline (mstyle)) {
	case UNDERLINE_SINGLE :
		add_attr (l, pango_attr_underline_new (PANGO_UNDERLINE_SINGLE));
		break;
	case UNDERLINE_DOUBLE :
		add_attr (l, pango_attr_underline_new (PANGO_UNDERLINE_DOUBLE));
		break;
	default :
		break;
	}

	if (mstyle_get_font_strike (mstyle))
		add_attr (l, pango_attr_strikethrough_new (TRUE));

	{
		GnmFont *font = mstyle_get_font (mstyle, context, zoom);
		add_attr (l, pango_attr_font_desc_new (font->pango.font_descr));
		style_font_unref (font);
	}

	pango_attr_list_ref (l);
	return l;
}

PangoAttrList *
mstyle_generate_attrs_full (GnmStyle const *st)
{
	GnmColor *fore = mstyle_get_color (st, MSTYLE_COLOR_FORE);
	PangoAttrList *l = pango_attr_list_new ();

	add_attr (l, pango_attr_family_new (mstyle_get_font_name (st)));
	add_attr (l, pango_attr_size_new (mstyle_get_font_size (st) * PANGO_SCALE));
	add_attr (l, pango_attr_style_new (mstyle_get_font_italic (st)
		? PANGO_STYLE_ITALIC : PANGO_STYLE_NORMAL));
	add_attr (l, pango_attr_weight_new (mstyle_get_font_bold (st)
		? PANGO_WEIGHT_BOLD : PANGO_WEIGHT_NORMAL));
	add_attr (l, pango_attr_foreground_new (
		fore->color.red, fore->color.green, fore->color.blue));
	add_attr (l, pango_attr_strikethrough_new (mstyle_get_font_strike (st)));
	switch (mstyle_get_font_uline (st)) {
	case UNDERLINE_SINGLE :
		add_attr (l, pango_attr_underline_new (PANGO_UNDERLINE_SINGLE));
		break;
	case UNDERLINE_DOUBLE :
		add_attr (l, pango_attr_underline_new (PANGO_UNDERLINE_DOUBLE));
		break;
	default :
		add_attr (l, pango_attr_underline_new (PANGO_UNDERLINE_NONE));
		break;
	}

	return l;
}

void
mstyle_set_from_pango_attribute (GnmStyle *style, PangoAttribute const *attr)
{
	switch (attr->klass->type) {
	case PANGO_ATTR_FAMILY :
		mstyle_set_font_name (style, ((PangoAttrString *)attr)->value);
		break;
	case PANGO_ATTR_SIZE :
		mstyle_set_font_size (style,
			(double )(((PangoAttrInt *)attr)->value) / PANGO_SCALE);
		break;
	case PANGO_ATTR_STYLE :
		mstyle_set_font_italic (style,
			((PangoAttrInt *)attr)->value == PANGO_STYLE_ITALIC);
		break;
	case PANGO_ATTR_WEIGHT :
		mstyle_set_font_bold (style,
			((PangoAttrInt *)attr)->value >= PANGO_WEIGHT_BOLD);
		break;
	case PANGO_ATTR_FOREGROUND :
		mstyle_set_color (style, MSTYLE_COLOR_FORE,
			style_color_new_pango (
			&((PangoAttrColor *)attr)->color));
		break;
	case PANGO_ATTR_UNDERLINE :
		switch (((PangoAttrInt *)attr)->value) {
		case PANGO_UNDERLINE_NONE :
			mstyle_set_font_uline (style, UNDERLINE_NONE);
			break;
		case PANGO_UNDERLINE_SINGLE :
			mstyle_set_font_uline (style, UNDERLINE_SINGLE);
			break;
		case PANGO_UNDERLINE_DOUBLE :
			mstyle_set_font_uline (style, UNDERLINE_DOUBLE);
			break;
		}
		break;
	case PANGO_ATTR_STRIKETHROUGH :
		mstyle_set_font_strike (style,
			((PangoAttrInt *)attr)->value != 0);
		break;
	default :
		break; /* ignored */
	}
}

/* ------------------------------------------------------------------------- */

void
mstyle_init (void)
{
#if USE_MSTYLE_POOL
	mstyle_pool =
		gnm_mem_chunk_new ("mstyle pool",
				   sizeof (GnmStyle),
				   16 * 1024 - 128);
#endif
}

#if USE_MSTYLE_POOL
static void
cb_mstyle_pool_leak (gpointer data, gpointer user)
{
	GnmStyle *mstyle = data;
	fprintf (stderr, "Leaking mstyle at %p.\n", mstyle);
	mstyle_dump (mstyle);
}
#endif

void
mstyle_shutdown (void)
{
#if USE_MSTYLE_POOL
	gnm_mem_chunk_foreach_leak (mstyle_pool, cb_mstyle_pool_leak, NULL);
	gnm_mem_chunk_destroy (mstyle_pool, FALSE);
	mstyle_pool = NULL;
#endif
}
