/********************************************************************
 * gnc-backend-file.h: load and save data to files                  *
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
/** @file gnc-backend-file.h
 *  @brief load and save data to files 
 *  @author Copyright (c) 2000 Gnumatic Inc.
 *  @author Copyright (c) 2002 Derek Atkins <warlord@MIT.EDU>
 *  @author Copyright (c) 2003 Linas Vepstas <linas@linas.org>
 *
 * This file implements the top-level QofBackend API for saving/
 * restoring data to/from an ordinary Unix filesystem file.
 */

#ifndef GNC_BACKEND_FILE_H_
#define GNC_BACKEND_FILE_H_

#include "qof.h"

struct FileBackend_struct
{
    QofBackend be;

    char *dirname;
    char *fullpath;  /* Fully qualified path to book */
    char *lockfile;
    char *linkfile;
    int lockfd;
   /** \deprecated 
	XXX price_lookup should be removed during the redesign
   * of the SQL backend... prices can now be queried using
   * the generic query mechanism.
   *
   * Note the correct signature for this call is
   * void (*price_lookup) (QofBackend *, GNCPriceLookup *);
   * we use gpointer to avoid an unwanted include file dependency.
   */  
    void (*price_lookup) (QofBackend *, gpointer);
  /** XXX Export should really _NOT_ be here, but is left here for now.
   * I'm not sure where this should be going to. It should be
   * removed ASAP.   This is a temporary hack-around until period-closing
   * is fully implemented.
   */
    void (*export) (QofBackend *, QofBook *);

    QofBook *primary_book;  /* The primary, main open book */
};

typedef struct FileBackend_struct FileBackend;

typedef enum 
{
    GNC_BOOK_NOT_OURS,
    GNC_BOOK_BIN_FILE,
    GNC_BOOK_XML1_FILE,
    GNC_BOOK_XML2_FILE,
    QSF_GNC_OBJECT,
    QSF_OBJECT,
    QSF_MAP,
} QofBookFileType;

QofBackend * libgncmod_backend_file_LTX_gnc_backend_new(void);

void gnc_provider_init(void);

#endif /* GNC_BACKEND_FILE_H_ */
