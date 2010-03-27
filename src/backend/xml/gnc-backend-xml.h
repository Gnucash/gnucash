/********************************************************************
 * gnc-backend-xml.h: load and save data to XML files               *
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
/** @file gnc-backend-xml.h
 *  @brief load and save data to files
 *  @author Copyright (c) 2000 Gnumatic Inc.
 *  @author Copyright (c) 2002 Derek Atkins <warlord@MIT.EDU>
 *  @author Copyright (c) 2003 Linas Vepstas <linas@linas.org>
 *
 * This file implements the top-level QofBackend API for saving/
 * restoring data to/from an ordinary Unix filesystem file.
 */

#ifndef GNC_BACKEND_XML_H_
#define GNC_BACKEND_XML_H_

#include "qof.h"
#include <gmodule.h>

#include "qofbackend-p.h"
struct FileBackend_struct
{
    QofBackend be;

    char *dirname;
    char *fullpath;  /* Fully qualified path to book */
    char *lockfile;
    char *linkfile;
    int lockfd;

    QofBook *primary_book;  /* The primary, main open book */

    int file_retention_days;
    gboolean file_compression;
};

typedef struct FileBackend_struct FileBackend;

// This is now a static inside the module
//QofBackend * libgncmod_backend_file_LTX_gnc_backend_new(void);

/** Initialization function which can be used when this module is
 * statically linked into the application. */
void gnc_module_init_backend_xml(void);

#ifndef GNC_NO_LOADABLE_MODULES
/** This is the standarized initialization function of a qof_backend
 * GModule, but compiling this can be disabled by defining
 * GNC_NO_LOADABLE_MODULES. This one simply calls
 * gnc_module_init_backend_file(). */
G_MODULE_EXPORT
void qof_backend_module_init(void);
#endif

#endif /* GNC_BACKEND_XML_H_ */
