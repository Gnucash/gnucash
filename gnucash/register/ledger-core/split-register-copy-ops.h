/********************************************************************\
 * split-register-copy-ops.h -- implement copy/paste semantics for  *
 *                              transactions and splits             *
 * Copyright (C) 1999 Linas Vepstas                                 *
 * Copyright (C) 2017 Aaron Laws                                    *
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

#ifndef SPLIT_REGISTER_COPY_OPS_H
#define SPLIT_REGISTER_COPY_OPS_H

#include <glib.h>
#include <libguile.h>

#include "qof.h"
#include "Account.h"
#include "gnc-guile-utils.h"

/* The next set of functions is for manipulating scheme
 * representations of splits and transactions. */

SCM    gnc_copy_split(Split *split, gboolean use_cut_semantics);
void   gnc_copy_split_scm_onto_split(SCM split_scm, Split *split,
                                     QofBook *book);

void   gnc_split_scm_set_account(SCM split_scm, Account *account);
void   gnc_split_scm_set_memo(SCM split_scm, const char *memo);
void   gnc_split_scm_set_action(SCM split_scm, const char *action);
void   gnc_split_scm_set_reconcile_state(SCM split_scm, char reconcile_state);
void   gnc_split_scm_set_amount(SCM split_scm, gnc_numeric amount);
void   gnc_split_scm_set_value(SCM split_scm, gnc_numeric value);

gnc_numeric gnc_split_scm_get_amount(SCM split_scm);
gnc_numeric gnc_split_scm_get_value(SCM split_scm);

SCM    gnc_copy_trans(Transaction *trans, gboolean use_cut_semantics);
void   gnc_copy_trans_scm_onto_trans(SCM trans_scm, Transaction *trans,
                                     gboolean do_commit, QofBook *book);
void   gnc_copy_trans_scm_onto_trans_swap_accounts(SCM trans_scm,
        Transaction *trans,
        const GncGUID *guid_1,
        const GncGUID *guid_2,
        gboolean do_commit,
        QofBook *book);

void   gnc_trans_scm_append_split_scm(SCM trans_scm, SCM split_scm);

SCM    gnc_trans_scm_get_split_scm(SCM trans_scm, int index);
SCM    gnc_trans_scm_get_other_split_scm(SCM trans_scm, SCM split_scm);
int    gnc_trans_scm_get_num_splits(SCM trans_scm);

#endif
