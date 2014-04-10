/* File import from libegg to gnumeric by import-egg.  Do not edit.  */


#ifndef __EGG_RECENT_UTIL__
#define __EGG_RECENT_UTIL__

#include <gtk/gtk.h>
#ifndef USE_STABLE_LIBGNOMEUI
#include <libgnomeui/gnome-icon-theme.h>
#endif

G_BEGIN_DECLS

gchar * egg_recent_util_escape_underlines (const gchar *uri);
gchar * egg_recent_util_get_unique_id (void);
#ifndef USE_STABLE_LIBGNOMEUI
GdkPixbuf * egg_recent_util_get_icon (GnomeIconTheme *theme,
				      const gchar *uri,
				      const gchar *mime_type,
				      int size);
#endif

G_END_DECLS

#endif /* __EGG_RECENT_UTIL__ */
