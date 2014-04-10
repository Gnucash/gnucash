#ifndef GO_REGUTF8_H
#define GO_REGUTF8_H

#include <glib.h>
#include <glib-object.h>
#include <goffice/cut-n-paste/pcre/pcreposix.h>

/* -------------------------------------------------------------------------- */

G_BEGIN_DECLS

#define GO_SEARCH_REPLACE_TYPE        (go_search_replace_get_type ())
#define GO_SEARCH_REPLACE(o)          (G_TYPE_CHECK_INSTANCE_CAST ((o), GO_SEARCH_REPLACE_TYPE, GoSearchReplace))
#define GO_IS_SEARCH_REPLACE(o)       (G_TYPE_CHECK_INSTANCE_TYPE ((o), GO_SEARCH_REPLACE_TYPE))

typedef struct _GoSearchReplace {
	GObject base;

	/*< public >*/
	char *search_text;
	char *replace_text;

	GORegexp *comp_search;
	gboolean is_regexp;	/* Search text is a regular expression.  */
	gboolean ignore_case;	/* Consider "a" and "A" the same.  */
	gboolean preserve_case;	/* Like Emacs' case-replace.  */
	gboolean match_words;	/* Like grep -w.  */

	/*< private >*/
	gboolean plain_replace;
} GoSearchReplace;

typedef struct {
	GObjectClass g_object_class;
} GoSearchReplaceClass;


GQuark           go_search_replace_error_quark (void);
GType            go_search_replace_get_type (void);

gboolean         go_search_replace_verify (GoSearchReplace *sr, gboolean repl, GError **err);

gboolean         go_search_match_string (GoSearchReplace *sr, const char *src);
char *           go_search_replace_string (GoSearchReplace *sr, const char *src);

const char *go_regexp_quote1 (GString *target, const char *s);
void go_regexp_quote (GString *target, const char *s);

G_END_DECLS

#endif /* GO_REGUTF8_H */
