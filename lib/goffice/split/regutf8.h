#ifndef GNUMERIC_REGUTF8_H
#define GNUMERIC_REGUTF8_H

#include <glib.h>
#include <sys/types.h>
#include <goffice/cut-n-paste/pcre/pcreposix.h>

/* -------------------------------------------------------------------------- */

#ifndef REG_EPAREN
#define REG_EPAREN REG_BADPAT
#endif

#ifndef REG_EBRACE
#define REG_EBRACE REG_BADPAT
#endif

#ifndef REG_EESCAPE
#define REG_EESCAPE REG_BADPAT
#endif

#ifndef REG_NOERROR
#define REG_NOERROR 0
#endif

#ifndef REG_OK
#define REG_OK REG_NOERROR
#endif

int gnumeric_regcomp_XL (go_regex_t *preg, char const *pattern, int cflags);

const char *gnumeric_regexp_quote1 (GString *target, const char *s);
void gnumeric_regexp_quote (GString *target, const char *s);

#endif
