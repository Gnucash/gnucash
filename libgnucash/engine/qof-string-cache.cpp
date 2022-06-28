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

#include <string>
#include <unordered_map>

extern "C"
{
#include <config.h>

#include <ctype.h>
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
/* The cache is an unordered_map where a copy of the string is the     */
/* key, and a ref count is the value                                   */
/* =================================================================== */

static std::unordered_map<std::string, guint> qof_string_cache;

void
qof_string_cache_clear (void)
{
    qof_string_cache.clear();
}

/* If the key exists in the cache, check the refcount.  If 1, just
 * remove the key.  Otherwise, decrement the refcount */
void
qof_string_cache_remove(const char * key)
{
    if (!key || !key[0])
        return;

    auto cache_iter = qof_string_cache.find (key);
    if (cache_iter == qof_string_cache.end())
        return;

    auto& refcount = cache_iter->second;
    if (refcount == 1)
        qof_string_cache.erase (cache_iter);
    else
        refcount--;
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

    auto [cache_iter, inserted] = qof_string_cache.insert ({key, 0});
    (cache_iter->second)++;

    return cache_iter->first.c_str();
}

const char *
qof_string_cache_replace(char const * dst, char const * src)
{
    const char * tmp {qof_string_cache_insert (src)};
    qof_string_cache_remove (dst);
    return tmp;
}
/* ************************ END OF FILE ***************************** */
