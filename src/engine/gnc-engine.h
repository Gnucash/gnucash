/********************************************************************
 * gnc-engine.h  -- top-level include file for Gnucash Engine       *
 * Copyright 2000 Bill Gribble <grib@billgribble.com>               *
 * Copyright 2001 Linas Vepstas <linas@linas.org>                   *
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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
 *                                                                  *
 ********************************************************************/

#ifndef GNC_ENGINE_H
#define GNC_ENGINE_H

#include <glib.h>

/** TYPES **********************************************************/

typedef struct account_s             Account;
typedef struct account_group_s       AccountGroup;
typedef struct split_s               Split;
typedef struct transaction_s         Transaction;
typedef struct gnc_book_struct       GNCBook;
typedef struct gnc_session_struct    GNCSession;
typedef struct gnc_commodity_s       gnc_commodity;
typedef struct gnc_commodity_table_s gnc_commodity_table;

typedef GList                  AccountList;
typedef GList                  BookList;
typedef GList                  SplitList;
typedef GList                  TransList;

typedef GList                  AccountGUIDList;
typedef GList                  BookGUIDList;

typedef void (* gnc_engine_init_hook_t)(int, char **);


/** PROTOTYPES ******************************************************/

/* GnuCash version number infomation. */
unsigned int gnucash_major_version (void);
unsigned int gnucash_minor_version (void);
unsigned int gnucash_micro_version (void);

/* gnc_engine_init MUST be called before gnc engine functions can 
 * be used. */
void gnc_engine_init(int argc, char ** argv);

/* called to shutdown the engine */
void gnc_engine_shutdown (void);

/* pass a function pointer to gnc_engine_add_init_hook and 
 * it will be called during the evaluation of gnc_engine_init */
void gnc_engine_add_init_hook(gnc_engine_init_hook_t hook);

/* Many strings used throughout the engine are likely to be duplicated.
 * So we provide a reference counted cache system for the strings, which
 * shares strings whenever possible.
 *
 * Use g_cache_insert to insert a string into the cache (it will return a
 * pointer to the cached string).
 * Basically you should use this instead of g_strdup.
 *
 * Use g_cache_remove (giving it a pointer to a cached string) if the string
 * is unused.  If this is the last reference to the string it will be
 * removed from the cache, otherwise it will just decrement the
 * reference count.
 * Basically you should use this instead of g_free.
 *
 * Note that all the work is done when inserting or removing.  Once
 * cached the strings are just plain C strings.
 */

/* get the gnc_string_cache.  Create it if it doesn't exist already */
GCache* gnc_engine_get_string_cache(void);

#endif
