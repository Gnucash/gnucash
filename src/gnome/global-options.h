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

#ifndef __GLOBAL_OPTIONS_H__
#define __GLOBAL_OPTIONS_H__

#include "config.h"

#include <gnome.h>

#include "gnc-common.h"
#include "option-util.h"


void gnc_options_init();
void gnc_options_shutdown();
void gnc_show_options_dialog();

SCM gnc_register_option_change_callback(OptionChangeCallback callback,
                                        void *user_data,
                                        char *section,
                                        char *name);

void gnc_unregister_option_change_callback_id(SCM callback_id);

GNCOption * gnc_get_option_by_name(const char *section_name, const char *name);
GNCOption * gnc_get_option_by_SCM(SCM guile_option);

gboolean gnc_lookup_boolean_option(const char *section, const char *name,
				   gboolean default_value);

char * gnc_lookup_string_option(const char *section, const char *name,
				char *default_value);

char * gnc_lookup_multichoice_option(const char *section, const char *name,
                                     char *default_value);

gdouble gnc_lookup_number_range_option(const char *section, const char *name,
                                       gdouble default_value);

gboolean gnc_lookup_color_option(const char *section, const char *name,
                                 gdouble *red, gdouble *green,
                                 gdouble *blue, gdouble *alpha);

uint32 gnc_lookup_color_option_argb(const char *section, const char *name,
                                    uint32 default_value);

GSList * gnc_lookup_list_option(const char *section, const char *name,
                                GSList *default_value);

void gnc_set_option_default(const char *section, const char *name);

gboolean gnc_set_number_range_option(const char *section, const char *name,
                                     gdouble value);

/* private */

void _gnc_option_refresh_ui(SCM option);


#endif /* __GLOBAL_OPTIONS_H__ */
