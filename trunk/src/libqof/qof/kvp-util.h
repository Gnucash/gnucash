/********************************************************************\
 * kvp-util.h -- misc KVP utilities                                 *
 * Copyright (C) 2003 Linas Vepstas <linas@linas.org>               *
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

/** @addtogroup KVP
    @{
*/
/** @file kvp-util.h
    @brief QOF KVP utility functions
 */
/**  @name Hash Utilities
 @{
*/

#ifndef GNC_KVP_UTIL_H
#define GNC_KVP_UTIL_H

typedef struct
{
    gpointer key;
    gpointer value;
} GHashTableKVPair;

/**
  Returns a GSList* of all the
  keys and values in a given hash table.  Data elements of lists are
  actual hash elements, so be careful, and deallocation of the
  GHashTableKVPairs in the result list are the caller's
  responsibility.  A typical sequence might look like this:

    GSList *kvps = g_hash_table_key_value_pairs(hash);
    ... use kvps->data->key and kvps->data->val, etc. here ...
    g_slist_foreach(kvps, g_hash_table_kv_pair_free_gfunc, NULL);
    g_slist_free(kvps);

*/

GSList *g_hash_table_key_value_pairs(GHashTable *table);
void g_hash_table_kv_pair_free_gfunc(gpointer data, gpointer user_data);

/***********************************************************************/

/** @} */
/** @} */
#endif /* GNC_KVP_UTIL_H */
