/********************************************************************\
 * engine-helpers.h -- gnucash engine helper functions              *
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

#ifndef ENGINE_HELPERS_H
#define ENGINE_HELPERS_H

#include <glib.h>
#include <libguile.h>

#include "gnc-engine.h"
#include "Account.h"
#include "Query.h"
#include "Transaction.h"

Timespec gnc_transaction_get_date_posted(const Transaction *t);
Timespec gnc_transaction_get_date_entered(const Transaction *t);

Timespec gnc_split_get_date_reconciled(const Split *s);

void gnc_transaction_set_date_posted(Transaction *t, const Timespec d);
void gnc_transaction_set_date_entered(Transaction *t, const Timespec d);

void gnc_transaction_set_date(Transaction *t, Timespec ts);

/* Helpers for various types */

SCM      gnc_timespec2timepair(Timespec t);
Timespec gnc_timepair2timespec(SCM x);
int      gnc_timepair_p(SCM x);

SCM  gnc_guid2scm(GncGUID guid);
GncGUID gnc_scm2guid(SCM guid_scm);
int  gnc_guid_p(SCM guid_scm);

/* for a list of strings */
GSList * gnc_query_scm2path (SCM path_scm);

/* These two functions convert a query object into a scheme
 * representation of the query and vice-versa. They do not
 * simply convert a query pointer to a guile query pointer! */
SCM gnc_query2scm (Query * q);
Query * gnc_scm2query (SCM query_scm);

SCM gnc_gint64_to_scm(const gint64 x);
gint64 gnc_scm_to_gint64(SCM num);
int gnc_gh_gint64_p(SCM num);

SCM gnc_numeric_to_scm(gnc_numeric arg);
gnc_numeric gnc_scm_to_numeric(SCM arg);
int gnc_numeric_p(SCM arg);
gnc_commodity * gnc_scm_to_commodity(SCM scm);
SCM gnc_commodity_to_scm (const gnc_commodity *commodity);
SCM gnc_book_to_scm (const QofBook *book);
SCM qof_session_to_scm (const QofSession *session);

#endif
