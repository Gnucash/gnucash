#ifndef GLIB24_26_COMPAT_H
#define GLIB24_26_COMPAT_H

#include <glib.h>

/* from glib-2.6[.6] gdate.h */
guint        g_date_get_iso8601_week_of_year (const GDate *date);

/* from glib-2.6[.6] gutils.h */
G_CONST_RETURN gchar* G_CONST_RETURN * g_get_language_names (void);

#ifdef G_OS_WIN32

/* On Win32, the canonical directory separator is the backslash, and
 * the search path separator is the semicolon. Note that also the
 * (forward) slash works as directory separator.
 */
#define G_DIR_SEPARATOR '\\'
#define G_DIR_SEPARATOR_S "\\"
#define G_IS_DIR_SEPARATOR(c) ((c) == G_DIR_SEPARATOR || (c) == '/')
#define G_SEARCHPATH_SEPARATOR ';'
#define G_SEARCHPATH_SEPARATOR_S ";"

#else  /* !G_OS_WIN32 */

/* Unix */

#define G_DIR_SEPARATOR '/'
#define G_DIR_SEPARATOR_S "/"
#define G_IS_DIR_SEPARATOR(c) ((c) == G_DIR_SEPARATOR)
#define G_SEARCHPATH_SEPARATOR ':'
#define G_SEARCHPATH_SEPARATOR_S ":"

#endif /* !G_OS_WIN32 */

/* from glib-2.6[.6] gstrfuncs.h */
guint                 g_strv_length    (gchar       **str_array);

#endif // GLIB24_26_COMPAT_H
