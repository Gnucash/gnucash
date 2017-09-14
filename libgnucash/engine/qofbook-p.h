/********************************************************************\
 * qof-book-p.h -- private functions for QOF books.                 *
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
\********************************************************************/
/** @addtogroup Object
    @{ */
/** @addtogroup Object_Private
    Private interfaces, not meant to be used by applications.
    @{ */
/** @name  Book_Private
    @{ */
/*
 * HISTORY:
 * Created 2001 by Rob Browning
 * Copyright (c) 2001 Rob Browning
 * Copyright (c) 2001,2003 Linas Vepstas <linas@linas.org>
 */

#ifndef QOF_BOOK_P_H
#define QOF_BOOK_P_H

#include "qofbackend.h"
#include "qofbook.h"
#include "qofid.h"
#include "qofid-p.h"
#include "qofinstance-p.h"
#ifdef __cplusplus
extern "C"
{
#endif

/*
 *    qof_book_set_backend() is used by backends to
 *    initialize the pointers in the book structure to
 *    something that contains actual data.  These routines
 *    should not be used otherwise.  (Its somewhat questionable
 *    if the backends should even be doing this much, but for
 *    backwards compatibility, we leave these here.)
 */
void qof_book_set_backend (QofBook *book, QofBackend *be);

/* Register books with the engine */
gboolean qof_book_register (void);

/** @deprecated use qof_instance_set_guid instead but only in
backends (when reading the GncGUID from the data source). */
#define qof_book_set_guid(book,guid)    \
         qof_instance_set_guid(QOF_INSTANCE(book), guid)

/** Validate a counter format string with a given format specifier.
 *    If valid, returns a normalized format string,
 *    that is whatever long int specifier was used will be replaced with the value of
 *    the posix "PRIx64" macro.
 *    If not valid returns NULL and optionally set an error message is a non-null
 *    err_msg parameter was passed.
 *    The caller should free the returned format string and  error message with g_free.
 */
gchar *qof_book_normalize_counter_format_internal(const gchar *p,
        const gchar* gint64_format, gchar **err_msg);

/** This debugging function can be used to traverse the book structure
 *    and all subsidiary structures, printing out which structures
 *    have been marked dirty.
 */
void qof_book_print_dirty (const QofBook *book);

/* @} */
/* @} */
/* @} */
#ifdef __cplusplus
}
#endif

#endif /* QOF_BOOK_P_H */
