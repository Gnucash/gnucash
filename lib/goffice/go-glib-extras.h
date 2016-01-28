#ifndef GO_GLIB_EXTRAS_H
#define GO_GLIB_EXTRAS_H

#include <glib.h>

G_BEGIN_DECLS

guint go_ascii_strcase_hash(gconstpointer v);
gint go_ascii_strcase_equal(gconstpointer v, gconstpointer v2);

char const *go_guess_encoding(char const *raw, gsize len,
                              char const *user_guess, char **utf8_str);

G_END_DECLS

#endif /* GO_GLIB_EXTRAS_H */
