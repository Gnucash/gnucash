#ifndef GO_GLIB_EXTRAS_H
#define GO_GLIB_EXTRAS_H

#include <goffice/goffice.h>

G_BEGIN_DECLS

/* Misc convenience routines that would be nice to have in glib */

typedef gpointer (*GOMapFunc) (gpointer value);

void	 go_ptr_array_insert	(GPtrArray *array, gpointer value, int index);

GSList	*go_hash_keys		(GHashTable *hash);
GSList	*go_hash_values		(GHashTable *hash);

GSList	*go_slist_map		(GSList const *list, GOMapFunc map_func);
GSList	*go_slist_create	(gpointer item1, ...);
void	 go_slist_free_custom	(GSList *list, GFreeFunc free_func);
#define	 go_string_slist_copy(list) go_slist_map (list, (GOMapFunc) g_strdup)
GSList	*go_strsplit_to_slist	(char const *str, gchar delimiter);
#define GO_SLIST_FOREACH(list,valtype,val,stmnt) \
G_STMT_START { \
	GSList const *go_l; \
	for (go_l = (list); go_l != NULL; go_l = go_l->next) { \
		valtype *val = go_l->data; \
		stmnt \
		; \
	} \
} G_STMT_END
#define GO_SLIST_PREPEND(list,item) \
	(list = g_slist_prepend (list, item))
#define GO_SLIST_APPEND(list,item) \
	(list = g_slist_append (list, item))
#define GO_SLIST_REMOVE(list,item) \
	(list = g_slist_remove (list, item))
#define GO_SLIST_CONCAT(list_a,list_b) \
	(list_a = g_slist_concat (list_a, list_b))
#define GO_SLIST_REVERSE(list) \
	(list = g_slist_reverse (list))
#define GO_SLIST_SORT(list,cmp_func) \
	(list = g_slist_sort (list, cmp_func))

gint go_list_index_custom (GList *list, gpointer data, GCompareFunc cmp_func);
void go_list_free_custom  (GList *list, GFreeFunc free_func);
#define GO_LIST_FOREACH(list,valtype,val,stmnt) \
G_STMT_START { \
	GList *go_l; \
	for (go_l = (list); go_l != NULL; go_l = go_l->next) { \
		valtype *val = go_l->data; \
		stmnt \
		; \
	} \
} G_STMT_END
#define GO_LIST_PREPEND(list,item) \
	(list = g_list_prepend (list, item))
#define GO_LIST_APPEND(list,item) \
	(list = g_list_append (list, item))
#define GO_LIST_REMOVE(list,item) \
	(list = g_list_remove (list, item))
#define GO_LIST_CONCAT(list_a,list_b) \
	(list_a = g_list_concat (list_a, list_b))
#define GO_LIST_REVERSE(list) \
	(list = g_list_reverse (list))
#define GO_LIST_SORT(list,cmp_func) \
	(list = g_list_sort (list, cmp_func))

int	    go_str_compare		(void const *x, void const *y);
guint	    go_ascii_strcase_hash	(gconstpointer v);
gint	    go_ascii_strcase_equal	(gconstpointer v, gconstpointer v2);
gint	    go_utf8_collate_casefold	(char const *a, char const *b);
char	   *go_utf8_strcapital		(char const *p, gssize len);
void	    go_strescape		(GString *target, char const *str);
char const *go_strunescape		(GString *target, char const *str);
void	    go_string_append_gstring	(GString *target, const GString *src);
void        go_string_append_c_n        (GString *target, char c, gsize n);
void        go_string_replace           (GString *target,
					 gsize pos, gssize oldlen,
					 const char *txt, gssize newlen);

char const *go_guess_encoding		(char const *raw, gsize len,
					 char const *user_guess,
					 char **utf8_str);

char const *go_get_real_name		(void);
void	    go_destroy_password	(char *passwd);

GOMemChunk  *go_mem_chunk_new		(char const *name, gsize user_atom_size, gsize chunk_size);
void	     go_mem_chunk_destroy	(GOMemChunk *chunk, gboolean expect_leaks);
gpointer     go_mem_chunk_alloc		(GOMemChunk *chunk);
gpointer     go_mem_chunk_alloc0	(GOMemChunk *chunk);
void         go_mem_chunk_free		(GOMemChunk *chunk, gpointer mem);
void         go_mem_chunk_foreach_leak	(GOMemChunk *chunk, GFunc cb, gpointer user);

void	go_object_toggle             (gpointer object,
				      const gchar *property_name);
gboolean go_object_set_property (GObject *obj, const char *property_name,
				 const char *user_prop_name, const char *value,
				 GError **err,
				 const char *error_template);
GSList *go_object_properties_collect (GObject *obj);
void    go_object_properties_apply   (GObject *obj,
				      GSList *props,
				      gboolean changed_only);
void    go_object_properties_free    (GSList *props);

typedef gboolean (*GOParseKeyValueFunc) (const char *name,
		  const char *value,
		  GError **err,
		  gpointer user);

gboolean go_parse_key_value (const char *options,
			     GError **err,
			     GOParseKeyValueFunc handler,
			     gpointer user);

G_END_DECLS

#endif /* GO_GLIB_EXTRAS_H */
