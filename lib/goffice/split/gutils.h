#ifndef GNUMERIC_UTILS_H
#define GNUMERIC_UTILS_H

#include "gnumeric.h"
#include "numbers.h"
#include <sys/types.h>

/* Misc convenience routines that would be nice to have in glib */

typedef gpointer (*GnmMapFunc) (gpointer value);

GSList	*gnm_hash_keys		(GHashTable *hash);
GSList	*gnm_hash_values	(GHashTable *hash);

GSList	*gnm_slist_map		(GSList const *list, GnmMapFunc map_func);
GSList	*gnm_slist_create	(gpointer item1, ...);
void	 gnm_slist_free_custom	(GSList *list, GFreeFunc free_func);
#define	 gnm_string_slist_copy(list) gnm_slist_map (list, (GnmMapFunc) g_strdup)
GSList	*gnm_strsplit_to_slist	(char const *str, char const *delimiter);
#define GNM_SLIST_FOREACH(list,valtype,val,stmnt) \
G_STMT_START { \
	GSList const *gnm_l; \
	for (gnm_l = (list); gnm_l != NULL; gnm_l = gnm_l->next) { \
		valtype *val = gnm_l->data; \
		stmnt \
		; \
	} \
} G_STMT_END
#define GNM_SLIST_PREPEND(list,item) \
	(list = g_slist_prepend (list, item))
#define GNM_SLIST_APPEND(list,item) \
	(list = g_slist_append (list, item))
#define GNM_SLIST_REMOVE(list,item) \
	(list = g_slist_remove (list, item))
#define GNM_SLIST_CONCAT(list_a,list_b) \
	(list_a = g_slist_concat (list_a, list_b))
#define GNM_SLIST_REVERSE(list) \
	(list = g_slist_reverse (list))
#define GNM_SLIST_SORT(list,cmp_func) \
	(list = g_slist_sort (list, cmp_func))

void	  gnm_ptr_array_insert	(GPtrArray *array, gpointer value, int index);
gint      gnm_list_index_custom (GList *list, gpointer data, GCompareFunc cmp_func);
void      gnm_list_free_custom (GList *list, GFreeFunc free_func);
#define GNM_LIST_FOREACH(list,valtype,val,stmnt) \
G_STMT_START { \
	GList *gnm_l; \
	for (gnm_l = (list); gnm_l != NULL; gnm_l = gnm_l->next) { \
		valtype *val = gnm_l->data; \
		stmnt \
		; \
	} \
} G_STMT_END
#define GNM_LIST_PREPEND(list,item) \
	(list = g_list_prepend (list, item))
#define GNM_LIST_APPEND(list,item) \
	(list = g_list_append (list, item))
#define GNM_LIST_REMOVE(list,item) \
	(list = g_list_remove (list, item))
#define GNM_LIST_CONCAT(list_a,list_b) \
	(list_a = g_list_concat (list_a, list_b))
#define GNM_LIST_REVERSE(list) \
	(list = g_list_reverse (list))
#define GNM_LIST_SORT(list,cmp_func) \
	(list = g_list_sort (list, cmp_func))

int	    gnm_str_compare		(void const *x, void const *y);
guint	    gnm_ascii_strcase_hash	(gconstpointer v);
gint	    gnm_ascii_strcase_equal	(gconstpointer v, gconstpointer v2);
gint	    gnm_utf8_collate_casefold	(char const *a, char const *b);
char	   *gnm_utf8_strcapital		(char const *p, ssize_t len);
void	    gnm_strescape		(GString *target, char const *str);
char const *gnm_strunescape		(GString *target, char const *str);
void	    gnm_string_append_gstring	(GString *target, const GString *src);
char const *gnm_guess_encoding		(char const *raw, size_t len,
					 char const *user_guess,
					 char **utf8_str);

char const *gnm_get_real_name		(void);
void	    gnm_destroy_password	(char *passwd);

/* System and user paths */
char	*gnm_sys_lib_dir    (char const *subdir);
char	*gnm_sys_data_dir   (char const *subdir);
char	*gnm_sys_glade_dir  (void);
char	*gnm_sys_plugin_dir (void);
char	*gnm_usr_dir	    (char const *subdir);
char	*gnm_usr_plugin_dir (void);

GnmMemChunk *gnm_mem_chunk_new		(char const *, size_t, size_t);
void	     gnm_mem_chunk_destroy	(GnmMemChunk *, gboolean);
gpointer     gnm_mem_chunk_alloc	(GnmMemChunk *);
gpointer     gnm_mem_chunk_alloc0	(GnmMemChunk *);
void         gnm_mem_chunk_free		(GnmMemChunk *, gpointer);
void         gnm_mem_chunk_foreach_leak (GnmMemChunk *, GFunc, gpointer);

void    gnm_time_counter_push (void);
gdouble gnm_time_counter_pop  (void);

#endif /* GNUMERIC_UTILS_H */
