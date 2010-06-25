/********************************************************************\
 * escape.c : escape SQL reserved characters                        *
 * Copyright (C) 2001 Linas Vepstas <linas@linas.org>               *
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

/*
 * FILE:
 * esacpe.c
 *
 * FUNCTION:
 * Escapes the ' and \ characters in a string
 */

#include "config.h"
#include <glib.h>
#include <string.h>

#include "gnc-engine.h"
#include "escape.h"

static QofLogModule log_module = GNC_MOD_BACKEND;

/* ================================================ */

struct _escape
{
    /* pointer to memory used for escaping arguments */
    char * escape;
    size_t esc_buflen;
};

/* ================================================ */
/* escape single-quote marks and backslashes so that the
 * database SQL parser doesn't puke on the query string
 */

const char *
sqlEscapeString (sqlEscape *b, const char *str)
{
    const char *p, *src_head;
    char *dst_tail;
    size_t len, slen;

    ENTER("str = %s", str);

    if (!b || !str)
    {
        LEAVE("(null) args");
        return NULL;
    }

    /* if a string is escaped twice, just return the first */
    if (b->escape == str)
    {
        LEAVE("%s: already escaped", str);
        return str;
    }

    /* if nothing to escape, just return */
    len = strlen (str);
    slen = strcspn (str, "\\\'");
    if (len == slen)
    {
        LEAVE("nothing to escape");
        return str;
    }

    /* count to see how much space we'll need */
    p = str + slen + 1;
    while (*p)
    {
        len ++;
        p += 1 + strcspn (p, "\\\'");
    }

    /* get more space, if needed */
    if (len >= b->esc_buflen)
    {
        b->escape = g_realloc(b->escape, len + 100);
        b->esc_buflen = len + 100;
    }

    /* copy and escape */
    src_head = (char *) str;
    dst_tail = b->escape;
    p = src_head + strcspn (src_head, "\\\'");
    while (*p)
    {
        size_t cp_len = p - src_head;

        strncpy (dst_tail, src_head, cp_len);
        dst_tail += cp_len;
        *dst_tail = '\\';
        dst_tail ++;
        *dst_tail = *p;
        dst_tail ++;

        src_head = p + 1;
        p = src_head + strcspn (src_head, "\\\'");
    }
    if (p != src_head)
    {
        size_t cp_len = p - src_head;

        strncpy (dst_tail, src_head, cp_len);
        dst_tail += cp_len;
    }
    *dst_tail = 0;

    LEAVE("b->escape = %s", b->escape);
    return b->escape;
}

/* ================================================ */

#define INITIAL_BUFSZ 2000

sqlEscape *
sqlEscape_new (void)
{
    sqlEscape *b = g_new (sqlEscape, 1);

    b->escape = g_malloc (INITIAL_BUFSZ);
    b->esc_buflen = INITIAL_BUFSZ;
    return (b);
}

/* ================================================ */

void
sqlEscape_destroy (sqlEscape *b)
{
    ENTER(" ");
    if (!b)
    {
        LEAVE("b is (null)");
        return;
    }
    g_free (b->escape);
    b->escape = NULL;
    g_free (b);
    LEAVE(" ");
}

/* ================ END OF FILE ==================== */
