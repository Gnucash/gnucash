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
 * along with this program; if not, write to the Free Software      *
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.        *
\********************************************************************/

#ifndef __GLOBAL_OPTIONS_H__
#define __GLOBAL_OPTIONS_H__

#include <gnome.h>

#include "option-util.h"


void gnc_options_init();
void gnc_options_shutdown();
void gnc_show_options_dialog();

SCM gnc_register_option_change_callback(OptionChangeCallback callback,
                                        void *user_data,
                                        char *section,
                                        char *name);

void gnc_unregister_option_change_callback_id(SCM callback_id);

GNCOption * gnc_get_option_by_name(char *section_name, char *name);
GNCOption * gnc_get_option_by_SCM(SCM guile_option);

gboolean gnc_lookup_boolean_option(char *section, char *name,
				   gboolean default_value);

char * gnc_lookup_string_option(char *section, char *name,
				char *default_value);

char * gnc_lookup_multichoice_option(char *section, char *name,
                                     char *default_value);

/* private */

void _gnc_option_refresh_ui(SCM option);


#endif /* __GLOBAL_OPTIONS_H__ */
