/********************************************************************\
 * engine-helpers.h -- gnucash g-wrap helper functions              *
 * Copyright (C) 2000 Linus Vepstas                                 *
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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

#ifndef ENGINE_HELPERS_H
#define ENGINE_HELPERS_H

#include <gnc-engine.h>
#include <gnc-book.h>
#include <Transaction.h>
#include <Account.h>
#include <GNCId.h>
#include <Query.h>
#include <guile/gh.h>
#include <glib.h>

Timespec gnc_transaction_get_date_posted(Transaction *t);
Timespec gnc_transaction_get_date_entered(Transaction *t);

Timespec gnc_split_get_date_reconciled(Split *s);

void gnc_transaction_set_date_posted(Transaction *t, const Timespec d);
void gnc_transaction_set_date_entered(Transaction *t, const Timespec d);

void gnc_transaction_set_date(Transaction *t, Timespec ts);

char * gnc_gettext_helper(const char *string);


/* Helpers for various types */

SCM      gnc_timespec2timepair(Timespec t);
Timespec gnc_timepair2timespec(SCM x);
int      gnc_timepair_p(SCM x);

SCM  gnc_guid2scm(GUID guid);
GUID gnc_scm2guid(SCM guid_scm);
int  gnc_guid_p(SCM guid_scm);

/* These two functions convert a query object into a scheme
 * representation of the query and vice-versa. They do not
 * simply convert a query pointer to a g-wrapped query pointer! */
SCM gnc_query2scm (Query * q);
Query * gnc_scm2query (SCM query_scm);

/* See Group.h for info about traversals. */

gboolean gnc_scmGroupStagedTransactionTraversal(AccountGroup *grp,
                                                unsigned int stage,
                                                SCM thunk);

gboolean gnc_scmAccountStagedTransactionTraversal(Account *a,
                                                  unsigned int stage,
                                                  SCM thunk);
SCM gnc_gint64_to_scm(const gint64 x);
gint64 gnc_scm_to_gint64(SCM num);
int gnc_gh_gint64_p(SCM num);

SCM gnc_numeric_to_scm(gnc_numeric arg);
gnc_numeric gnc_scm_to_numeric(SCM arg);
int gnc_numeric_p(SCM arg);
gnc_commodity * gnc_scm_to_commodity(SCM scm);
SCM gnc_commodity_to_scm (const gnc_commodity *commodity);
SCM gnc_book_to_scm (GNCBook *book);

/* The GList is freed */
SCM     gnc_glist_account_ptr_to_scm(GList *account_list);
/* The GList is not freed */
SCM     gnc_glist_account_ptr_to_scm_no_free (GList *account_list);
GList * gnc_scm_to_glist_account_ptr(SCM scm_list);
int     gnc_glist_account_ptr_p(SCM scm_list);

/* The GList is freed */
SCM     gnc_glist_commodity_ptr_to_scm(GList * list);
GList * gnc_scm_to_glist_commodity_ptr(SCM list);
int     gnc_glist_commodity_ptr_p(SCM list);

/* The GList is freed */
SCM     gnc_glist_price_ptr_to_scm(GList * list);
GList * gnc_scm_to_glist_price_ptr(SCM list);
int     gnc_glist_price_ptr_p(SCM list);

#endif
