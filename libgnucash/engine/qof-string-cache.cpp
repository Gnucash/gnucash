/********************************************************************\
 * qof-string-cache.c -- QOF string cache functions                 *
 * Copyright (C) 1997 Robin D. Clark                                *
 * Copyright (C) 1997-2001,2004 Linas Vepstas <linas@linas.org>     *
 * Copyright 2006  Neil Williams  <linux@codehelp.co.uk>            *
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
 *                                                                  *
 *   Author: Rob Clark (rclark@cs.hmc.edu)                          *
 *   Author: Linas Vepstas (linas@linas.org)                        *
 *   Author: Phil Longstaff (phil.longstaff@yahoo.ca)               *
\********************************************************************/

extern "C"
{
#include <config.h>

#include <ctype.h>
#include <glib.h>
#include <stdlib.h>
#include <string.h>
#include "qof.h"
}

/* Uncomment if you need to log anything.
static QofLogModule log_module = QOF_MOD_UTIL;
*/
/* =================================================================== */
/* The QOF string cache                                                */
/*                                                                     */
/* The cache is a GHashTable where a copy of the string is the key,    */
/* and a ref count is the value                                        */
/* =================================================================== */

static GHashTable* qof_string_cache = NULL;

static GHashTable*
qof_get_string_cache(void)
{
    if (!qof_string_cache)
    {
        qof_string_cache = g_hash_table_new_full(
                               g_str_hash,               /* hash_func          */
                               g_str_equal,              /* key_equal_func     */
                               g_free,                   /* key_destroy_func   */
                               g_free);                  /* value_destroy_func */
    }
    return qof_string_cache;
}

void
qof_string_cache_init(void)
{
    (void)qof_get_string_cache();
}

void
qof_string_cache_destroy (void)
{
    if (qof_string_cache)
    {
        g_hash_table_destroy(qof_string_cache);
    }
    qof_string_cache = NULL;
}

/* If the key exists in the cache, check the refcount.  If 1, just
 * remove the key.  Otherwise, decrement the refcount */
void
qof_string_cache_remove(gconstpointer key)
{
    if (key)
    {
        GHashTable* cache = qof_get_string_cache();
        gpointer value;
        gpointer cache_key;
        if (g_hash_table_lookup_extended(cache, key, &cache_key, &value))
        {
            guint* refcount = (guint*)value;
            if (*refcount == 1)
            {
                g_hash_table_remove(cache, key);
            }
            else
            {
                --(*refcount);
            }
        }
    }
}

/* If the key exists in the cache, increment the refcount.  Otherwise,
 * add it with a refcount of 1. */
gpointer
qof_string_cache_insert(gconstpointer key)
{
    if (key)
    {
        GHashTable* cache = qof_get_string_cache();
        gpointer value;
        gpointer cache_key;
        if (g_hash_table_lookup_extended(cache, key, &cache_key, &value))
        {
            guint* refcount = (guint*)value;
            ++(*refcount);
            return cache_key;
        }
        else
        {
            gpointer new_key = g_strdup(static_cast<const char*>(key));
            guint* refcount = static_cast<unsigned int*>(g_malloc(sizeof(guint)));
            *refcount = 1;
            g_hash_table_insert(cache, new_key, refcount);
            return new_key;
        }
    }
    return NULL;
}

void
qof_string_cache_replace(gconstpointer * dst, gconstpointer src)
{
    gpointer tmp {qof_string_cache_insert(src)};
    qof_string_cache_remove(&dst);
    *dst = tmp;
}
/* ************************ END OF FILE ***************************** */
