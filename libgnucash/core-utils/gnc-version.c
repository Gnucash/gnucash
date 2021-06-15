/********************************************************************\
 * gnc-version.cpp -- functions to query the build-time version info  *
 *                                                                  *
 * Copyright (C) 2019 Geert Janssens <geert@kobaltwit.be>           *
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
 ********************************************************************/

#include "gnc-version.h"
#include <config.h>

#ifdef GNC_VCS
    #define vcs GNC_VCS " "
#else
    #define vcs ""
#endif
#define dflt_build_id vcs GNC_VCS_REV "(" GNC_VCS_REV_DATE ")"

const char *gnc_version(void)
{
    return PROJECT_VERSION;
}

const char *gnc_build_id(void)
{
    /* GNUCASH_BUILD_ID can be set by the builder prior to compiling to anything
     * the builder sees fit (eg distributions may want to print a package source
     * version number (rpm, dpkg,...)
     * If not set by a builder it will be set by default to our
     * git revision ("git706a3b (<commit-date>)"
     */
    if (GNUCASH_BUILD_ID[0] != '\0')
        return GNUCASH_BUILD_ID;
    else
        return dflt_build_id;

}

const char *gnc_vcs_rev(void)
{
    return GNC_VCS_REV;
}

const char *gnc_vcs_rev_date(void)
{
    return GNC_VCS_REV_DATE;
}

const int gnc_gnucash_major_version(void)
{
    return PROJECT_VERSION_MAJOR;
}

static GList * developer_log = NULL;

gboolean gnc_developer_log_empty (void)
{
    return (!developer_log);
}

void gnc_add_developer_log_entry (gchar *str)
{
    developer_log = g_list_prepend (developer_log, g_strdup (str));
}

void gnc_clear_developer_log (void)
{
    g_list_free_full (developer_log, g_free);
    developer_log = NULL;
}

static char * mystrcat (char* dest, const char* src)
{
     while (*dest) dest++;
     while ((*dest++ = *src++));
     return --dest;
}

static gchar * string_join (GList *strings, const gchar *sep)
{
    gchar *outstr, *p;
    gint len = 1, seplen = strlen (sep);

    for (GList *n = strings; n; n = n->next)
        len += strlen (n->data) + seplen;

    if (len > 4000)
        return g_strdup ("Devlog exceeds 4Kb.");

    outstr = g_new0 (gchar, len);
    p = outstr;

    for (GList *n = strings; n; n = n->next)
    {
        p = mystrcat (p, n->data);
        if (n->next)
            p = mystrcat (p, sep);
    }

    return outstr;
}

gchar * gnc_get_developer_log (void)
{
    return string_join (g_list_reverse (developer_log), "\n");
}
