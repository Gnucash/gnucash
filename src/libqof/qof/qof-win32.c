/*
 * qof-win32.c
 *
 * Copyright (C) 2007 Andreas Koehler <andi5.py@gmx.net>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of version 2 of the GNU General Public
 * License as published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 */

#include "config.h"

#include <glib.h>
#include "gnc-date-p.h"
#include "strptime.h"
#include <windows.h>
#include <stdlib.h>

static GHashTable *picture_to_format = NULL;
G_LOCK_DEFINE_STATIC(picture_to_format);

gchar *
qof_time_format_from_utf8(const gchar *utf8_format)
{
    gunichar2 *utf16_format;
    gchar *retval;
    gsize count;

    utf16_format = g_utf8_to_utf16(utf8_format, -1, NULL, NULL, NULL);
    if (!utf16_format)
        return NULL;

    /* get number of resulting wide characters */
    count = wcstombs(NULL, utf16_format, 0);
    if (count <= 0)
        return NULL;

    /* malloc and convert */
    retval = g_malloc((count + 1) * sizeof(gchar));
    count = wcstombs(retval, utf16_format, count + 1);
    g_free(utf16_format);
    if (count <= 0)
    {
        g_free(retval);
        return NULL;
    }

    return retval;
}

gchar *
qof_formatted_time_to_utf8(const gchar *locale_string)
{
    gunichar2 *utf16_string;
    gchar *retval;
    gsize count;

    /* get number of resulting wide characters */
    count = mbstowcs(NULL, locale_string, 0);
    if (count <= 0)
        return NULL;

    /* malloc and convert */
    utf16_string = g_malloc((count + 1) * sizeof(gunichar2));
    count = mbstowcs(utf16_string, locale_string, count + 1);
    if (count <= 0)
    {
        g_free(utf16_string);
        return NULL;
    }

    retval = g_utf16_to_utf8(utf16_string, -1, NULL, NULL, NULL);
    g_free(utf16_string);

    return retval;
}

const char *
qof_win32_get_time_format(QofWin32Picture picture)
{
    gchar *locale_string, *format;
    gchar *tmp1, *tmp2;

    switch (picture)
    {
    case QOF_WIN32_PICTURE_DATE:
        locale_string = get_win32_locale_string(LOCALE_SSHORTDATE);
        break;
    case QOF_WIN32_PICTURE_TIME:
        locale_string = get_win32_locale_string(LOCALE_STIMEFORMAT);
        break;
    case QOF_WIN32_PICTURE_DATETIME:
        tmp1 = get_win32_locale_string(LOCALE_SSHORTDATE);
        tmp2 = get_win32_locale_string(LOCALE_STIMEFORMAT);
        locale_string = g_strconcat(tmp1, " ", tmp2);
        g_free(tmp1);
        g_free(tmp2);
        break;
    default:
        g_assert_not_reached();
    }

    G_LOCK(picture_to_format);
    if (!picture_to_format)
        picture_to_format = g_hash_table_new_full(g_str_hash, g_str_equal,
                            NULL, g_free);
    format = g_hash_table_lookup(picture_to_format, locale_string);
    if (!format)
    {
        format = translate_win32_picture(locale_string);
        g_hash_table_insert(picture_to_format, g_strdup(locale_string), format);
    }
    G_UNLOCK(picture_to_format);
    g_free(locale_string);

    return format;
}
