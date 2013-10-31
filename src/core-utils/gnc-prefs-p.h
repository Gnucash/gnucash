/********************************************************************\
 * gnc-prefs.h Api to load and store preferences to a configurable  *
 *             backend                                              *
 *                                                                  *
 * Copyright (C) 2013 Geert Janssens <geert@kobaltwit.be>           *
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

#ifndef GNC_PREFS_P_H_
#define GNC_PREFS_P_H_

#include "gnc-prefs.h"


typedef struct
{
    gulong (*register_cb) (const char *group,
                           const gchar *pref_name,
                           gpointer func,
                           gpointer user_data);

    void (*remove_cb_by_func) (const gchar *group,
                               const gchar *pref_name,
                               gpointer func,
                               gpointer user_data);

    void (*remove_cb_by_id) (const gchar *group,
                             guint id);

    guint (*register_group_cb) (const gchar *group,
                                gpointer func,
                                gpointer user_data);

    void (*remove_group_cb_by_func) (const gchar *group,
                                     gpointer func,
                                     gpointer user_data);

    void (*bind) (const gchar *group,
                  /*@ null @*/ const gchar *pref_name,
                  gpointer object,
                  const gchar *property);

    gboolean (*get_bool) (const gchar *group,
                          /*@ null @*/ const gchar *pref_name);

    gint (*get_int) (const gchar *group,
                     const gchar *pref_name);

    gdouble (*get_float) (const gchar *group,
                          const gchar *pref_name);

    gchar *(*get_string) (const gchar *group,
                          const gchar *pref_name);

    gint (*get_enum) (const gchar *group,
                      const gchar *pref_name);

    GVariant *(*get_value) (const gchar *group,
                            const gchar *pref_name);

    gboolean (*set_bool) (const gchar *group,
                          const gchar *pref_name,
                          gboolean value);

    gboolean (*set_int) (const gchar *group,
                         const gchar *pref_name,
                         gint value);

    gboolean (*set_float) (const gchar *group,
                           const gchar *pref_name,
                           gdouble value);

    gboolean (*set_string) (const gchar *group,
                            const gchar *pref_name,
                            const gchar *value);

    gboolean (*set_enum) (const gchar *group,
                          const gchar *pref_name,
                          gint value);

    gboolean (*set_value) (const gchar *group,
                           const gchar *pref_name,
                           GVariant *value);

    void (*reset) (const gchar *group,
                   const gchar *pref_name);

    void (*reset_group) (const gchar *group);

} PrefsBackend;

extern PrefsBackend prefsbackend;

#endif /* GNC_PREFS_P_H_ */
