#ifndef GNUMERIC_PLUGIN_UTIL_H
#define GNUMERIC_PLUGIN_UTIL_H

#include "gnumeric.h"
#include "error-info.h"

#include <stdio.h>

FILE *gnumeric_fopen_error_info (char const *file_name, char const *mode,
				 ErrorInfo **ret_error);

#endif /* GNUMERIC_PLUGIN_UTIL_H */
