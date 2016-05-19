/********************************************************************
 * gnc-backend-dbi.h: load and save data to SQL via libdbi          *
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
\********************************************************************/
/** @file gnc-backend-dbi.h
 *  @brief load and save data to SQL via libdbi
 *  @author Copyright (c) 2006-2008 Phil Longstaff <plongstaff@rogers.com>
 *
 * This file implements the top-level QofBackend API for saving/
 * restoring data to/from an SQL database via libdbi
 */

#ifndef GNC_BACKEND_DBI_H_
#define GNC_BACKEND_DBI_H_

#include <gmodule.h>

/** Initialization function which can be used when this module is
 * statically linked into the application. */
void gnc_module_init_backend_dbi(void);
/** Shutdown function which can be used when this module is
 * statically linked into the application. */
void gnc_module_finalize_backend_dbi(void);

#ifndef GNC_NO_LOADABLE_MODULES
/** This is the standarized initialization function of a qof_backend
 * GModule, but compiling this can be disabled by defining
 * GNC_NO_LOADABLE_MODULES. */
G_MODULE_EXPORT void qof_backend_module_init(void);
G_MODULE_EXPORT void qof_backend_module_finalize(void);
#endif

#endif /* GNC_BACKEND_DBI_H_ */
