/********************************************************************\
 * gnc-engine-guile.h -- engine helper functions for guile          *
 * Copyright (C) 2000 Linas Vepstas <linas@linas.org>               *
 * Copyright (C) 2001 Linux Developers Group, Inc.                  *
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

#ifndef GNC_ENGINE_GUILE_H
#define GNC_ENGINE_GUILE_H

#include <glib.h>
#include <libguile.h>

#include "gnc-engine.h"
#include <gncTaxTable.h>    /* for GncAccountValue */
#include "gnc-hooks.h"

#ifdef __cplusplus
extern "C"
{
#endif
/* Helpers for various conversions to and from guile */

GDate gnc_time64_to_GDate(SCM x);

SCM gnc_guid2scm(GncGUID guid);

GncGUID gnc_scm2guid(SCM guid_scm);

int gnc_guid_p(SCM guid_scm);

/* for a list of strings */
GSList* gnc_query_scm2path(SCM path_scm);

/* These two functions convert a query object into a scheme
 * representation of the query and vice-versa. They do not
 * simply convert a query pointer to a guile query pointer! */
SCM gnc_query2scm(QofQuery* q);

QofQuery* gnc_scm2query(SCM query_scm);

SCM gnc_numeric_to_scm(gnc_numeric arg);

gnc_numeric gnc_scm_to_numeric(SCM arg);

gnc_commodity* gnc_scm_to_commodity(SCM scm);

SCM gnc_commodity_to_scm(const gnc_commodity* commodity);

SCM gnc_book_to_scm(const QofBook* book);

/* Conversion routines used with tax tables */
GncAccountValue* gnc_scm_to_account_value_ptr(SCM valuearg);

SCM gnc_account_value_ptr_to_scm(GncAccountValue*);

/**
 * add Scheme-style danglers from a hook
 */
void gnc_hook_add_scm_dangler(const gchar* name, SCM proc);

/** Convert a time string to calendar time representation.  Combine strptime and
 *  mktime into a single function to avoid the need to wrap struct tm *.
 *
 *  @param s String representation of time.
 *
 *  @param format Format specification.
 *
 *  @return The time in seconds since unix epoch, or -1 on error */
time64 gnc_parse_time_to_time64(const gchar* s, const gchar* format);

#ifdef __cplusplus
}
#endif
#endif
