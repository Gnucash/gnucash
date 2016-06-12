/********************************************************************\
 * qofutil.c -- QOF utility functions                               *
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
\********************************************************************/

#include "config.h"

#include <ctype.h>
#include <glib.h>
#include <stdlib.h>
#include <string.h>
#include "qof.h"
#include "qofbackend-p.h"

G_GNUC_UNUSED static QofLogModule log_module = QOF_MOD_UTIL;

void
g_hash_table_foreach_sorted(GHashTable *hash_table, GHFunc func, gpointer user_data, GCompareFunc compare_func)
{
    GList *iter;
    GList *keys = g_list_sort(g_hash_table_get_keys(hash_table), compare_func);

    for (iter = keys; iter; iter = iter->next)
    {
        func(iter->data, g_hash_table_lookup(hash_table, iter->data), user_data);
    }

    g_list_free(keys);
}

gboolean
qof_utf8_substr_nocase (const gchar *haystack, const gchar *needle)
{
    gchar *haystack_casefold, *haystack_normalized;
    gchar *needle_casefold, *needle_normalized;
    gchar *p;

    g_return_val_if_fail (haystack && needle, FALSE);

    haystack_casefold = g_utf8_casefold (haystack, -1);
    haystack_normalized = g_utf8_normalize (haystack_casefold, -1,
                                            G_NORMALIZE_ALL);
    g_free (haystack_casefold);

    needle_casefold = g_utf8_casefold (needle, -1);
    needle_normalized = g_utf8_normalize (needle_casefold, -1, G_NORMALIZE_ALL);
    g_free (needle_casefold);

    p = strstr (haystack_normalized, needle_normalized);
    g_free (haystack_normalized);
    g_free (needle_normalized);

    return p != NULL;
}

/** Use g_utf8_casefold and g_utf8_collate to compare two utf8 strings,
 * ignore case. Return < 0 if da compares before db, 0 if they compare
 * equal, > 0 if da compares after db. */
static gint
qof_utf8_strcasecmp (const gchar *da, const gchar *db)
{
    gchar *da_casefold, *db_casefold;
    gint retval;

    g_return_val_if_fail (da != NULL, 0);
    g_return_val_if_fail (db != NULL, 0);

    da_casefold = g_utf8_casefold (da, -1);
    db_casefold = g_utf8_casefold (db, -1);
    retval = g_utf8_collate (da_casefold, db_casefold);
    g_free (da_casefold);
    g_free (db_casefold);

    return retval;
}

gint
safe_strcasecmp (const gchar * da, const gchar * db)
{
    if ((da) && (db))
    {
        if ((da) != (db))
        {
            gint retval = qof_utf8_strcasecmp ((da), (db));
            /* if strings differ, return */
            if (retval) return retval;
        }
    }
    else if ((!(da)) && (db))
    {
        return -1;
    }
    else if ((da) && (!(db)))
    {
        return +1;
    }
    return 0;
}

gint
null_strcmp (const gchar * da, const gchar * db)
{
    if (da && db) return strcmp (da, db);
    if (!da && db && 0 == db[0]) return 0;
    if (!db && da && 0 == da[0]) return 0;
    if (!da && db) return -1;
    if (da && !db) return +1;
    return 0;
}

#define MAX_DIGITS 50

/* inverse of strtoul */
gchar *
ultostr (gulong val, gint base)
{
    gchar buf[MAX_DIGITS];
    gulong broke[MAX_DIGITS];
    gint i;
    gulong places = 0, reval;

    if ((2 > base) || (36 < base)) return NULL;

    /* count digits */
    places = 0;
    for (i = 0; i < MAX_DIGITS; i++)
    {
        broke[i] = val;
        places ++;
        val /= base;
        if (0 == val) break;
    }

    /* normalize */
    reval = 0;
    for (i = places - 2; i >= 0; i--)
    {
        reval += broke[i+1];
        reval *= base;
        broke[i] -= reval;
    }

    /* print */
    for (i = 0; i < (gint)places; i++)
    {
        if (10 > broke[i])
        {
            buf[places-1-i] = 0x30 + broke[i];  /* ascii digit zero */
        }
        else
        {
            buf[places-1-i] = 0x41 - 10 + broke[i];  /* ascii capital A */
        }
    }
    buf[places] = 0x0;

    return g_strdup (buf);
}

/* =================================================================== */
/* returns TRUE if the string is a number, possibly with whitespace */
/* =================================================================== */

gboolean
gnc_strisnum(const gchar *s)
{
    if (s == NULL) return FALSE;
    if (*s == 0) return FALSE;

    while (*s && isspace(*s))
        s++;

    if (*s == 0) return FALSE;
    if (!isdigit(*s)) return FALSE;

    while (*s && isdigit(*s))
        s++;

    if (*s == 0) return TRUE;

    while (*s && isspace(*s))
        s++;

    if (*s == 0) return TRUE;

    return FALSE;
}

/* =================================================================== */
/* Return NULL if the field is whitespace (blank, tab, formfeed etc.)
 * Else return pointer to first non-whitespace character. */
/* =================================================================== */

G_GNUC_UNUSED static const gchar *
qof_util_whitespace_filter (const gchar * val)
{
    size_t len;
    if (!val) return NULL;

    len = strspn (val, "\a\b\t\n\v\f\r ");
    if (0 == val[len]) return NULL;
    return val + len;
}

#ifdef THESE_CAN_BE_USEFUL_FOR_DEGUGGING
static guint g_str_hash_KEY(gconstpointer v)
{
    return g_str_hash(v);
}
static guint g_str_hash_VAL(gconstpointer v)
{
    return g_str_hash(v);
}
static gpointer g_strdup_VAL(gpointer v)
{
    return g_strdup(v);
}
static gpointer g_strdup_KEY(gpointer v)
{
    return g_strdup(v);
}
static void g_free_VAL(gpointer v)
{
    return g_free(v);
}
static void g_free_KEY(gpointer v)
{
    return g_free(v);
}
static gboolean qof_util_str_equal(gconstpointer v, gconstpointer v2)
{
    return (v && v2) ? g_str_equal(v, v2) : FALSE;
}
#endif

void
qof_init (void)
{
#ifndef HAVE_GLIB_2_36
    g_type_init(); /* Automatic as of GLib 2.36 */
#endif
    qof_log_init();
    qof_string_cache_init();
    qof_object_initialize ();
    qof_query_init ();
    qof_book_register ();
}

void
qof_close(void)
{
    qof_query_shutdown ();
    qof_object_shutdown ();
    qof_finalize_backend_libraries();
    qof_string_cache_destroy ();
    qof_log_shutdown();
}

/* ************************ END OF FILE ***************************** */
