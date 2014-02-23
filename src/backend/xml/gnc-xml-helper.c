/********************************************************************\
 * gnc-xml-helper.h -- api for xml helpers                          *
 *                                                                  *
 * Copyright (C) 2001 James LewisMoss <dres@debian.org>             *
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
\********************************************************************/

#include <glib.h>
#include "gnc-xml-helper.h"

xmlChar*
checked_char_cast (gchar *val)
{
    const int length = -1; /* Assumes val is null-terminated */
    gchar *end;
    if (val == NULL) return NULL;
    /* Replace any invalid UTF-8 characters with a sequence of '?' */
    while (!g_utf8_validate (val, length, (const gchar**)(&end)))
        *end = '?';
    /* Replace any invalid (for XML) control characters (everything < 0x20
     * except \n, \t, and \r) with '?'. Technically we should replace
     * these with a numeric entity, but that will blow up the libxml
     * functions that expect raw text. It seems unlikely that anyone
     * would use intentionally use one of these characters anyway.
     */

     for (end = val; *end; ++end)
	if (*end > 0 && *end < 0x20 && *end != 0x09 &&
	    *end != 0x0a && *end != 0x0d)
	    *end = '?';
    return (xmlChar*)(val);
}

