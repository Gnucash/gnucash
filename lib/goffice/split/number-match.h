#ifndef GNUMERIC_NUMBER_MATCH_H
#define GNUMERIC_NUMBER_MATCH_H

#include "gnumeric.h"

gboolean format_match_create  (GnmFormat *fmt);
void	 format_match_release (GnmFormat *fmt);

GnmValue   *format_match_simple (char const *s);
GnmValue   *format_match        (char const *s, GnmFormat *cur_fmt,
				 GnmDateConventions const *date_conv);
GnmValue   *format_match_number (char const *s, GnmFormat *cur_fmt,
				 GnmDateConventions const *date_conv);

void format_match_init   (void);
void format_match_finish (void);

#endif /* GNUMERIC_NUMBER_MATCH_H */
