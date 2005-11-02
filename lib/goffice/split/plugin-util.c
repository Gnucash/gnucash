/*
 * plugin-util.c: Utility functions for gnumeric plugins
 *
 * Authors:
 *  Almer. S. Tigelaar. <almer1@dds.nl>
 *  Zbigniew Chyla <cyba@gnome.pl>
 *
 */

#include <config.h>
#include <glib/gi18n.h>
#include "gnumeric.h"
#include "plugin-util.h"

#include "command-context.h"
#include "io-context.h"

#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>
#include <string.h>

/**
 * gnumeric_fopen_error_info:
 * @file_name: the file to open
 * @mode: the file mode
 * @ret_error: ErrorInfo to fill when error occurs
 *
 * a wrapper around fopen ().
 * It will fill ErrorInfo struct for you.
 * for more info on the parameters see 'man 3 fopen'
 *
 * Return value: a pointer to a FILE struct if successful or NULL if not
 **/
FILE *
gnumeric_fopen_error_info (const char *file_name, const char *mode, ErrorInfo **ret_error)
{
	FILE *f;

	g_return_val_if_fail (file_name != NULL, NULL);
	g_return_val_if_fail (mode != NULL, NULL);
	g_return_val_if_fail (ret_error != NULL, NULL);

	*ret_error = NULL;
	f = fopen (file_name, mode);
	if (f == NULL) {
		if (strchr (mode, 'w') != NULL && strchr (mode, 'r') == NULL) {
			*ret_error = error_info_new_printf (
			             _("Error while opening file \"%s\" for writing."),
			             file_name);
		} else {
			*ret_error = error_info_new_printf (
			             _("Error while opening file \"%s\" for reading."),
			             file_name);
		}
		error_info_add_details (*ret_error, error_info_new_from_errno ());
	}

	return f;
}
