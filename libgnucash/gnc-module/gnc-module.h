/*************************************************************
 * gnc-module.h -- loadable plugin/module system for gnucash
 * Copyright 2001 Linux Developers Group, Inc.
 *************************************************************/
/********************************************************************\
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
\********************************************************************/


#ifndef GNC_MODULE_H
#define GNC_MODULE_H

#include <glib.h>

typedef void * GNCModule;

#define DEFAULT_MODULE_PATH "/usr/local/gnucash/lib/modules"
#define GNC_MODULE_PREFIX "libgncmod"

/* the basics: initialize the module system, refresh its module
 * database, and get a list of all known modules */
void            gnc_module_system_init(void);
void            gnc_module_system_refresh(void);
GList         * gnc_module_system_modinfo(void);

/* load and unload a module.  gnc_module_system_init() must be called
 * before loading and unloading.
 */
/*@ dependent @*/
GNCModule       gnc_module_load(const gchar * module_name, gint iface);
GNCModule       gnc_module_load_optional(const gchar * module_name, gint iface);
int             gnc_module_unload(GNCModule mod);

#endif
