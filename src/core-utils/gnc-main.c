/*
 * gnc-main.c:
 *
 * Copyright (C) 2006 Chris Shoemaker <c.shoemaker@cox.net>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
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
#include "gnc-main.h"

static gchar *namespace_regexp = NULL;
static gboolean is_debugging = 0;
static gboolean extras_enabled = 0;
static const gchar *gconf_path;

void
gnc_main_set_namespace_regexp(const gchar *str)
{
    if (namespace_regexp)
        g_free(namespace_regexp);

    if (str)
        namespace_regexp = g_strdup(str);
}

const gchar *
gnc_main_get_namespace_regexp(void)
{
    return namespace_regexp;
}

gboolean
gnc_is_debugging(void)
{
    return is_debugging;
}

void
gnc_set_debugging(gboolean d)
{
    is_debugging = d;
}

gboolean
gnc_is_extra_enabled(void)
{
    return extras_enabled;
}

void
gnc_set_extra(gboolean enabled)
{
    extras_enabled = enabled;
}

void
gnc_set_gconf_path (const gchar *path)
{
    gconf_path = path;
}

const gchar *
gnc_get_gconf_path (void)
{
    return gconf_path;
}
