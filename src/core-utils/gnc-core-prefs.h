/*
 * gnc-core-prefs.h:
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

#ifndef GNC_MAIN_H
#define GNC_MAIN_H

#include <glib.h>

#define GCONF_PATH "/apps/gnucash"

const gchar *gnc_core_prefs_get_namespace_regexp(void);
void gnc_core_prefs_set_namespace_regexp(const gchar *str);

gboolean gnc_core_prefs_is_debugging_enabled(void);
void gnc_core_prefs_set_debugging(gboolean d);

gboolean gnc_core_prefs_is_extra_enabled(void);
void gnc_core_prefs_set_extra(gboolean enabled);

gboolean gnc_core_prefs_get_file_save_compressed(void);
void gnc_core_prefs_set_file_save_compressed(gboolean compressed);

gint gnc_core_prefs_get_file_retention_policy(void);
void gnc_core_prefs_set_file_retention_policy(gint policy);

gint gnc_core_prefs_get_file_retention_days(void);
void gnc_core_prefs_set_file_retention_days(gint days);

const gchar *gnc_gconf_get_path_prefix(void);
void gnc_gconf_set_path_prefix(const gchar *prefix);

guint gnc_core_prefs_get_long_version( void );

#endif /* GNC_MAIN_H */
