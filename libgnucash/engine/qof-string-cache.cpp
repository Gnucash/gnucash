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
#include <unordered_map>
#include <string>

extern "C"
{
#include <config.h>

#include "qof.h"
}

/* Uncomment if you need to log anything.
static QofLogModule log_module = QOF_MOD_UTIL;
*/
/* =================================================================== */
/* The QOF string cache                                                */
/*                                                                     */
/* The cache is a std:unordered_map where a std::string is the key,    */
/* and a ref count is the value                                        */
/* =================================================================== */

static std::unordered_map<std::string_view, int> cache;

/* If the key exists in the cache, check the refcount.  If 1, just
 * remove the key.  Otherwise, decrement the refcount */
void
qof_string_cache_remove(const char * key)
{
    if (!key || !key[0])
        return;

    auto map_iter = cache.find (key);
    if (map_iter == cache.end())
        return;

    if (map_iter->second == 1)
    {
        g_free ((gpointer)map_iter->first.data());
        cache.erase (map_iter);
    }
    else
        map_iter->second--;
}

/* If the key exists in the cache, increment the refcount.  Otherwise,
 * add it with a refcount of 1. */
const char *
qof_string_cache_insert(const char * key)
{
    if (!key)
        return nullptr;

    if (!key[0])
        return "";

    auto map_iter = cache.find (key);
    if (map_iter == cache.end())
    {
        std::string_view skey { g_strdup (key) };
        map_iter = cache.emplace (skey, 1).first;
    }
    else
        map_iter->second++;

    return map_iter->first.data();
}

const char *
qof_string_cache_replace(char const * dst, char const * src)
{
    const char * tmp {qof_string_cache_insert (src)};
    qof_string_cache_remove (dst);
    return tmp;
}
/* ************************ END OF FILE ***************************** */
