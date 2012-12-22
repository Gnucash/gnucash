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

typedef void (*GncBOCb)    (gpointer new_val, gpointer user_data);

Timespec gnc_transaction_get_date_posted(const Transaction *t);
Timespec gnc_transaction_get_date_entered(const Transaction *t);

Timespec gnc_split_get_date_reconciled(const Split *s);

void gnc_transaction_set_date(Transaction *t, Timespec ts);

/** Gets the transaction Number or split Action based on book option:
  * if the book option is TRUE (split action is used for NUM) and a
  * split is provided, split-action is returned; if book option is FALSE
  * (tran-num is used for NUM) and a trans is provided, transaction-num
  * is returned; if split is provided and tran is NULL, split-action is
  * returned; if tran is provided and split is NULL, transaction-num is
  * returned. Otherwise NULL is returned.*/
const char *  gnc_get_num_action (const Transaction *trans, const Split *split);

/** Opposite of 'gnc_get_num_action'; if the book option is TRUE (split action
  * is used for NUM) and a trans is provided, transaction-num is returned; if
  * book option is FALSE (tran-num is used for NUM) and a split is provided,
  * split-action is returned; if split is provided and tran is NULL,
  * split-action is returned; if tran is provided and split is NULL,
  * transaction-num is returned. Otherwise NULL is returned.*/
const char *  gnc_get_action_num (const Transaction *trans, const Split *split);

/** Sets the transaction Number and/or split Action based on book option:
  * if the book option is TRUE (split action is to be used for NUM) then 'num'
  * sets split-action and, if 'tran' and 'action' are provided, 'action'
  * sets transaction-num; if book option is FALSE (tran-num is to be used for NUM)
  * then 'num' sets transaction-num and, if 'split' and 'action' are
  * provided, 'action' sets 'split-action'. If any arguments are NULL (#f, for
  * the guile version), no change is made to the field that would otherwise be
  * affected. If 'tran' and 'num' are passed with 'split and 'action' set to
  * NULL, it is xaccTransSetNum (trans, num). Likewise, if 'split and 'action'
  * are passed with 'tran' and 'num' set to NULL, it is xaccSplitSetAction (split,
  * action). */
void gnc_set_num_action (Transaction *trans, Split *split,
                            const char *num, const char *action);

/** Calls registered callbacks when num_field_source book option changes so that
  * registers/reports can update themselves */
void
gnc_book_option_num_field_source_change (gboolean num_action);

/** Registers callbacks to be called when the book option changes for the
  * specified book option key */
void
gnc_book_option_register_cb (gchar *key, GncBOCb func, gpointer user_data);

/** Removes previously registered callbacks for the specified book option key */
void
gnc_book_option_remove_cb (gchar *key, GncBOCb func, gpointer user_data);

/* Helpers for various types */

SCM      gnc_timespec2timepair(Timespec t);
Timespec gnc_timepair2timespec(SCM x);
GDate    gnc_timepair_to_GDate(SCM x);
int      gnc_timepair_p(SCM x);

SCM  gnc_guid2scm(GncGUID guid);
GncGUID gnc_scm2guid(SCM guid_scm);
int  gnc_guid_p(SCM guid_scm);

/* for a list of strings */
GSList * gnc_query_scm2path (SCM path_scm);

/* These two functions convert a query object into a scheme
 * representation of the query and vice-versa. They do not
 * simply convert a query pointer to a guile query pointer! */
SCM gnc_query2scm (QofQuery * q);
QofQuery * gnc_scm2query (SCM query_scm);

int gnc_gh_gint64_p(SCM num);

SCM gnc_numeric_to_scm(gnc_numeric arg);
gnc_numeric gnc_scm_to_numeric(SCM arg);
int gnc_numeric_p(SCM arg);
gnc_commodity * gnc_scm_to_commodity(SCM scm);
SCM gnc_commodity_to_scm (const gnc_commodity *commodity);
SCM gnc_book_to_scm (const QofBook *book);

#endif
