#ifndef GO_FORMAT_IMPL_H
#define GO_FORMAT_IMPL_H

#include <goffice/utils/go-format.h>

G_BEGIN_DECLS

struct _GOFormatElement {
        char const *format;
        char        restriction_type;
        double	    restriction_value;
	GOColor     go_color;

	/* fmt contains an '@' that stringifies things */
	gboolean    forces_text;

	gboolean    want_am_pm;
	gboolean    has_fraction;
	gboolean    suppress_minus;
	gboolean    elapsed_time;

	GOFormat	*container;
	char		*regexp_str;
	GByteArray	*match_tags;
	GORegexp	 regexp;
};

void go_fmt_general_int    (GString *result, int val, int col_width);
void go_fmt_general_float  (GString *result, double val, double col_width);
void go_format_number      (GString *result,
			    double number, int col_width, GOFormatElement const *elem,
			    GODateConventions const *date_conv);
#ifdef GOFFICE_WITH_LONG_DOUBLE
void go_fmt_general_floatl (GString *result, long double val, double col_width);
void go_format_numberl     (GString *result,
			    long double number, int col_width, GOFormatElement const *elem,
			    GODateConventions const *date_conv);
#endif

G_END_DECLS

#endif /* GO_FORMAT_IMPL_H */
