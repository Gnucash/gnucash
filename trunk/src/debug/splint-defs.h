/********************************************************************
 * splint-defs.h: declarations from system include files needed to  *
 *                provide splint with memory usage patterns.        *
 *                                                                  *
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
 *                                                                  *
 * This program is distributed in the hope that it will be useful,  *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of   *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    *
 * GNU General Public License for more details.                     *
 *                                                                  *
 * You should have received a copy of the GNU General Public License*
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
\********************************************************************/

/* Only need this if running splint.  This file contains lines copied from the other
include files, but with the splint annotations added.  Yes, this is dangerous if the
definitions change.  However, these definitions aren't used during regular compilation,
only when running splint, so the danger is manageable. */

#if !defined( SPLINT_DEFS_H ) && defined( S_SPLINT_S )
#define SPLINT_DEFS_H

/*@ -incondefs @*/

/* Each section is surrounded by the include guard of the appropriate include file */

/* dbi.h */

#ifdef __DBI_H__
void dbi_conn_close( /*@ only @*/ dbi_conn );
int dbi_conn_error( dbi_conn, /*@ out @*/ const char** );
dbi_result dbi_conn_get_table_list( dbi_conn, const char*, /*@ null @*/ const char* );
/*@ dependent @*/
const char* dbi_conn_get_option( dbi_conn, const char* );
int dbi_result_free( /*@ only @*/ dbi_result );
/*@ dependent @*/
const char* dbi_result_get_string_idx( dbi_result, unsigned int );
dbi_driver dbi_driver_list( /*@ null @*/ dbi_driver );
size_t dbi_conn_quote_string_copy( dbi_conn, const char*, /*@ out @*/ char** );
/*@ dependent @*/
dbi_driver_get_name( dbi_driver );
#endif

/* gdate.h */

#ifdef __G_DATE_H__
void g_date_free( /*@ only @*/ GDate* );
#endif

/* gmem.h */

#ifdef __G_MEM_H__
/*@ null @*/ /*@ only @*/ /*@ out @*/ gpointer g_malloc(gsize n_bytes);
/*@ null @*/ /*@ only @*/ /*@ out @*/
gpointer g_malloc0(gsize n_bytes);
void g_free( /*@ only @*/ gpointer );
#endif

/* gvalue.h */

#ifdef __G_VALUE_H__
GValue* g_value_init( /*@ out @*/ GValue*, GType );
/*@ dependent @*/
const gchar* g_value_get_string(const GValue *value);
void g_value_take_string( GValue*, /*@ only @*/ const gchar *);
#endif

/* gstring.h */

#ifdef __G_STRING_H__
gchar* g_string_free( /*@ only @*/ GString*, gboolean );
#endif

/* ghash.h */

#ifdef __G_HASH_H__
GHashTable* g_hash_table_new_full( GHashFunc, GEqualFunc, /*@ null @*/ GDestroyNotify, /*@ null @*/ GDestroyNotify );
void g_hash_table_insert(GHashTable *hash_table, /*@ only @*/ gpointer key, gpointer value);
/*@ dependent @*/
g_hash_table_lookup( GHashTable* hash_table, gpointer key );
#endif

/* glist.h */

#ifdef __G_LIST_H__
GList* g_list_append( /*@ returned @*//*@ null @*/ GList*, /*@ keep @*/ gpointer );
GList* g_list_prepend( /*@ returned @*//*@ null @*/ GList*, /*@ keep @*/ gpointer );
void g_list_free( /*@ only @*/ GList* );
#endif

/* gslist.h */

#ifdef __G_SLIST_H__
GSList* g_slist_append( /*@ returned @*//*@ null @*/ GSList*, /*@ keep @*/ gpointer );
void g_slist_free( /*@ only @*/ GSList* );
#endif

/* gstrfuncs.h */

#ifdef __G_STRFUNCS_H__
gint64 g_ascii_strtoll( const gchar*, /*@ null @*/ gchar**, guint );
#endif

/* gtype.h */

#ifdef __G_TYPE_H__
/*@ dependent @*/ GTypeInstance* g_type_check_instance_cast(GTypeInstance *instance,
        GType iface_type);
#endif

/* gtestutils.h */

#ifdef __G_TEST_UTILS_H__
#undef g_assert
#define g_assert assert
#endif

/*@ +incondefs @*/
#endif /* SPLINT_DEFS_H */
