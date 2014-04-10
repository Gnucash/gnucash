#ifndef GNUMERIC_STRING_H
#define GNUMERIC_STRING_H

#include "gnumeric.h"

struct _GnmString {
	int        ref_count;
	char       *str;
};

void    gnm_string_init       (void);
void    gnm_string_shutdown   (void);

GnmString *gnm_string_get        (char const *s);
GnmString *gnm_string_get_nocopy (char *s);
GnmString *gnm_string_ref        (GnmString *);
void       gnm_string_unref      (GnmString *);

#endif /* GNUMERIC_STRING_H */
