/*************************************************************
 * gnc-module.h -- loadable plugin/module system for gnucash
 * Copyright 2001 Linux Developers Group, Inc.
 *************************************************************/
/*
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 */
/** @addtogroup gnc_module GnuCash-specific shared libraries

\note HACK ALERT: These are \b NOT genuine libtool modules! You can
currently link against these libraries with impunity and large
sections of the codebase do exactly that, especially the test routines.
This may change if the load mechanism is upgraded to use GModule. For
comparison, see the Makefile.am changes required to test the
genuine GModule backends in src/backend/postgres/test/Makefile.am
The -module flag was removed from all these libraries due to
portability problems arising from this confused naming and the consequent
build assumptions. http://svn.gnucash.org/trac/changeset/11462
@{
*/
/** @file gnc-module.h
	@brief GnuCash-specific module interface
	@author Copyright 2001 Linux Developers Group, Inc.
*/

#ifndef GNC_MODULE_H
#define GNC_MODULE_H

#include <glib.h>
#include <ltdl.h>
#include <libguile.h>

#ifndef lt_ptr
#define lt_ptr lt_ptr_t
#endif

typedef void * GNCModule;

typedef struct {
  char * module_path;
  char * module_description;
  char * module_filepath;
  int    module_interface;
  int    module_age;
  int    module_revision;
} GNCModuleInfo;

#define DEFAULT_MODULE_PATH "/usr/local/gnucash/lib/modules"

/** the current gnc-module interface version. */
#define GNC_MOD_INTERFACE_VERSION 0

/** the basics: initialize the module system, refresh its module 
 * database, and get a list of all known modules */
void            gnc_module_system_init(void);
void            gnc_module_system_refresh(void);
GList         * gnc_module_system_modinfo(void);

/** catch-all routine to load all available modules.

If a gnc-module has been built for the specified module
interface, this function will load it. */
void            gnc_module_load_all(gint interface);

/** load and unload a module.  gnc_module_system_init() must be called
 * before loading and unloading. 
 *
 */
GNCModule       gnc_module_load(const gchar * module_name, gint interface);
GNCModule       gnc_module_load_optional(const gchar * module_name, gint interface);
int             gnc_module_unload(GNCModule mod);
GNCModuleInfo * gnc_module_get_info(const char * lib_path);
int             gnc_module_use_scm_module(gchar * module_path);

/** gnc_module_lookup locates the given 'symbol' in module
 * 'mod'.  'mod' must be previously loaded. */
void          * gnc_module_lookup(GNCModule mod, gchar * symbol);

/** @} */
#endif  /* GNC_MODULE_H */
