/********************************************************************\
 * global-options.h -- GNOME global option handling                 *
 * Copyright (C) 1998,1999 Linas Vepstas                            *
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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
\********************************************************************/

#ifndef GLOBAL_OPTIONS_H
#define GLOBAL_OPTIONS_H

#include "config.h"

#include <glib.h>

#include "gnc-common.h"
#include "option-util.h"


void gnc_options_init(void);
void gnc_options_shutdown(void);

SCM gnc_register_option_change_callback(OptionChangeCallback callback,
                                        gpointer user_data,
                                        char *section,
                                        char *name);

void gnc_unregister_option_change_callback_id(SCM callback_id);

GNCOption * gnc_get_option_by_name(const char *section_name, const char *name);
GNCOption * gnc_get_option_by_SCM(SCM guile_option);

SCM gnc_lookup_option(const char *section, const char *name,
                      SCM default_value);

gboolean gnc_lookup_boolean_option(const char *section, const char *name,
				   gboolean default_value);

char * gnc_lookup_string_option(const char *section, const char *name,
				const char *default_value);

char * gnc_lookup_font_option(const char *section, const char *name,
                              const char *default_value);

char * gnc_lookup_multichoice_option(const char *section, const char *name,
                                     const char *default_value);

gdouble gnc_lookup_number_option(const char *section, const char *name,
                                 gdouble default_value);

gboolean gnc_lookup_color_option(const char *section, const char *name,
                                 gdouble *red, gdouble *green,
                                 gdouble *blue, gdouble *alpha);

guint32 gnc_lookup_color_option_argb(const char *section, const char *name,
                                     guint32 default_value);

GSList * gnc_lookup_list_option(const char *section, const char *name,
                                GSList *default_value);

gnc_commodity *
gnc_lookup_currency_option(const char *section,
                           const char *name,
                           gnc_commodity *default_value);

gnc_commodity * gnc_default_currency (void);

void gnc_set_option_default(const char *section, const char *name);

gboolean gnc_set_option(const char *section, const char *name, SCM value);

gboolean gnc_set_number_option(const char *section, const char *name,
                               gdouble value);

gboolean gnc_set_boolean_option(const char *section, const char *name,
                                gboolean value);
    
void gnc_option_refresh_ui_by_name(const char *section_name, 
				   const char *name);

void gnc_set_option_selectable_by_name(const char *section, 
                                       const char *name,
                                       gboolean selectable);

/* private */

void _gnc_option_refresh_ui(SCM option);
GNCOptionDB * gnc_get_global_options(void);

#endif /* GLOBAL_OPTIONS_H */
