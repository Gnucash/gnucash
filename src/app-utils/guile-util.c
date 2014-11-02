/********************************************************************\
 * guile-util.c -- utility functions for using guile for GnuCash    *
 * Copyright (C) 1999 Linas Vepstas                                 *
 * Copyright (C) 2000 Dave Peticolas                                *
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
 * along with this program; if not, write to the Free Software      *
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.        *
\********************************************************************/

#include "config.h"

#include <glib.h>
#include <glib/gi18n.h>
#include <string.h>
#include "swig-runtime.h"
#include <libguile.h>
#include <errno.h>
#ifdef HAVE_UNISTD_H
# include <unistd.h>
#else
# include <io.h>
# define close _close
#endif
#ifndef HAVE_STRPTIME
#    include "strptime.h"
#endif

#include "qof.h"
#include "engine-helpers-guile.h"
#include "glib-helpers.h"
#include "gnc-glib-utils.h"
#include "gnc-guile-utils.h"
#include "gnc-prefs.h"
#include "guile-util.h"
#include "guile-mappings.h"

/* This static indicates the debugging module this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_GUILE;


struct _setters
{
    SCM split_scm_account_guid;
    SCM split_scm_memo;
    SCM split_scm_action;
    SCM split_scm_reconcile_state;
    SCM split_scm_amount;
    SCM split_scm_value;

    SCM trans_scm_date;
    SCM trans_scm_num;
    SCM trans_scm_description;
    SCM trans_scm_notes;
    SCM trans_scm_append_split_scm;
} setters;

struct _getters
{
    SCM split_scm_memo;
    SCM split_scm_action;
    SCM split_scm_amount;
    SCM split_scm_value;

    SCM trans_scm_split_scms;
    SCM trans_scm_split_scm;
    SCM trans_scm_other_split_scm;

    SCM debit_string;
    SCM credit_string;
} getters;

struct _predicates
{
    SCM is_split_scm;
    SCM is_trans_scm;
} predicates;

struct _Process
{
    GPid pid;
    gint fd_stdin;
    gint fd_stdout;
    gint fd_stderr;
    gboolean dead;
    gboolean detached;
};


static void
initialize_scm_functions()
{
    static gboolean scm_funcs_inited = FALSE;

    if (scm_funcs_inited)
        return;

    setters.split_scm_account_guid =
        scm_c_eval_string("gnc:split-scm-set-account-guid");
    setters.split_scm_memo = scm_c_eval_string("gnc:split-scm-set-memo");
    setters.split_scm_action = scm_c_eval_string("gnc:split-scm-set-action");
    setters.split_scm_reconcile_state =
        scm_c_eval_string("gnc:split-scm-set-reconcile-state");
    setters.split_scm_amount = scm_c_eval_string("gnc:split-scm-set-amount");
    setters.split_scm_value = scm_c_eval_string("gnc:split-scm-set-value");

    setters.trans_scm_date = scm_c_eval_string("gnc:transaction-scm-set-date-posted");
    setters.trans_scm_num = scm_c_eval_string("gnc:transaction-scm-set-num");
    setters.trans_scm_description =
        scm_c_eval_string("gnc:transaction-scm-set-description");
    setters.trans_scm_notes = scm_c_eval_string("gnc:transaction-scm-set-notes");
    setters.trans_scm_append_split_scm =
        scm_c_eval_string("gnc:transaction-scm-append-split-scm");

    getters.split_scm_memo = scm_c_eval_string("gnc:split-scm-get-memo");
    getters.split_scm_action = scm_c_eval_string("gnc:split-scm-get-action");
    getters.split_scm_amount = scm_c_eval_string("gnc:split-scm-get-amount");
    getters.split_scm_value = scm_c_eval_string("gnc:split-scm-get-value");

    getters.trans_scm_split_scms =
        scm_c_eval_string("gnc:transaction-scm-get-split-scms");
    getters.trans_scm_split_scm =
        scm_c_eval_string("gnc:transaction-scm-get-split-scm");
    getters.trans_scm_other_split_scm =
        scm_c_eval_string("gnc:transaction-scm-get-other-split-scm");

    getters.debit_string = scm_c_eval_string("gnc:get-debit-string");
    getters.credit_string = scm_c_eval_string("gnc:get-credit-string");

    predicates.is_split_scm = scm_c_eval_string("gnc:split-scm?");
    predicates.is_trans_scm = scm_c_eval_string("gnc:transaction-scm?");

    scm_funcs_inited = TRUE;
}


/********************************************************************\
  gnc_scm_lookup

    returns the SCM binding associated with the given symbol function,
    or SCM_UNDEFINED if it couldn't be retrieved.

    Don't use this to get hold of symbols that are considered private
    to a given module unless the C code you're writing is considered
    part of that module.

  Args:

    module - where to lookup the symbol, something like "ice-9 debug"
    symbol - what to look up.

  Returns: value bound to the symbol, if any.
\********************************************************************/

#if 0

************ NOT TESTED YET **************

SCM
gnc_scm_lookup(const char *module, const char *symbol)
{
    SCM scm_module = scm_c_resolve_module(module);
    SCM value = scm_c_module_lookup(scm_module, symbol);
    return value;
}

#endif

/********************************************************************\
 * gnc_copy_split                                                   *
 *   returns a scheme representation of a split. If the split is    *
 *   NULL, SCM_UNDEFINED is returned.                               *
 *                                                                  *
 * Args: split             - the split to copy                      *
 *       use_cut_semantics - if TRUE, copy is for a 'cut' operation *
 * Returns: SCM representation of split or SCM_UNDEFINED            *
\********************************************************************/
SCM
gnc_copy_split(Split *split, gboolean use_cut_semantics)
{
    static swig_type_info *split_type = NULL;
    SCM func;
    SCM arg;

    if (split == NULL)
        return SCM_UNDEFINED;

    func = scm_c_eval_string("gnc:split->split-scm");
    if (!scm_is_procedure(func))
        return SCM_UNDEFINED;

    if (!split_type)
        split_type = SWIG_TypeQuery("_p_Split");

    arg = SWIG_NewPointerObj(split, split_type, 0);

    return scm_call_2(func, arg, SCM_BOOL(use_cut_semantics));
}


/********************************************************************\
 * gnc_copy_split_scm_onto_split                                    *
 *   copies a scheme representation of a split onto an actual split.*
 *                                                                  *
 * Args: split_scm - the scheme representation of a split           *
 *       split     - the split to copy onto                         *
 * Returns: Nothing                                                 *
\********************************************************************/
void
gnc_copy_split_scm_onto_split(SCM split_scm, Split *split,
                              QofBook * book)
{
    static swig_type_info *split_type = NULL;
    SCM result;
    SCM func;
    SCM arg;

    if (split_scm == SCM_UNDEFINED)
        return;

    if (split == NULL)
        return;

    g_return_if_fail (book);

    func = scm_c_eval_string("gnc:split-scm?");
    if (!scm_is_procedure(func))
        return;

    result = scm_call_1(func, split_scm);
    if (!scm_is_true(result))
        return;

    func = scm_c_eval_string("gnc:split-scm-onto-split");
    if (!scm_is_procedure(func))
        return;

    if (!split_type)
        split_type = SWIG_TypeQuery("_p_Split");

    arg = SWIG_NewPointerObj(split, split_type, 0);

    scm_call_3(func, split_scm, arg, gnc_book_to_scm (book));
}


/********************************************************************\
 * gnc_is_split_scm                                                 *
 *   returns true if the scm object is a scheme split               *
 *                                                                  *
 * Args: scm - a scheme object                                      *
 * Returns: true if scm is a scheme split                           *
\********************************************************************/
gboolean
gnc_is_split_scm(SCM scm)
{
    initialize_scm_functions();

    return scm_is_true(scm_call_1(predicates.is_split_scm, scm));
}


/********************************************************************\
 * gnc_is_trans_scm                                                 *
 *   returns true if the scm object is a scheme transaction         *
 *                                                                  *
 * Args: scm - a scheme object                                      *
 * Returns: true if scm is a scheme transaction                     *
\********************************************************************/
gboolean
gnc_is_trans_scm(SCM scm)
{
    initialize_scm_functions();

    return scm_is_true(scm_call_1(predicates.is_trans_scm, scm));
}


/********************************************************************\
 * gnc_split_scm_set_account                                        *
 *   set the account of a scheme representation of a split.         *
 *                                                                  *
 * Args: split_scm - the scheme split                               *
 *       account   - the account to set                             *
 * Returns: Nothing                                                 *
\********************************************************************/
void
gnc_split_scm_set_account(SCM split_scm, Account *account)
{
    gchar guid_string[GUID_ENCODING_LENGTH+1];
    SCM arg;

    initialize_scm_functions();

    if (!gnc_is_split_scm(split_scm))
        return;
    if (account == NULL)
        return;

    guid_to_string_buff(xaccAccountGetGUID(account), guid_string);
    if (guid_string == NULL)
        return;

    arg = scm_from_utf8_string(guid_string);

    scm_call_2(setters.split_scm_account_guid, split_scm, arg);
}


/********************************************************************\
 * gnc_split_scm_set_memo                                           *
 *   set the memo of a scheme representation of a split.            *
 *                                                                  *
 * Args: split_scm - the scheme split                               *
 *       memo      - the memo to set                                *
 * Returns: Nothing                                                 *
\********************************************************************/
void
gnc_split_scm_set_memo(SCM split_scm, const char *memo)
{
    SCM arg;

    initialize_scm_functions();

    if (!gnc_is_split_scm(split_scm))
        return;
    if (memo == NULL)
        return;

    arg = scm_from_utf8_string(memo);

    scm_call_2(setters.split_scm_memo, split_scm, arg);
}


/********************************************************************\
 * gnc_split_scm_set_action                                         *
 *   set the action of a scheme representation of a split.          *
 *                                                                  *
 * Args: split_scm - the scheme split                               *
 *       action    - the action to set                              *
 * Returns: Nothing                                                 *
\********************************************************************/
void
gnc_split_scm_set_action(SCM split_scm, const char *action)
{
    SCM arg;

    initialize_scm_functions();

    if (!gnc_is_split_scm(split_scm))
        return;
    if (action == NULL)
        return;

    arg = scm_from_utf8_string(action);

    scm_call_2(setters.split_scm_action, split_scm, arg);
}


/********************************************************************\
 * gnc_split_scm_set_reconcile_state                                *
 *   set the reconcile state of a scheme split.                     *
 *                                                                  *
 * Args: split_scm       - the scheme split                         *
 *       reconcile_state - the reconcile state to set               *
 * Returns: Nothing                                                 *
\********************************************************************/
void
gnc_split_scm_set_reconcile_state(SCM split_scm, char reconcile_state)
{
    SCM arg;

    initialize_scm_functions();

    if (!gnc_is_split_scm(split_scm))
        return;

    arg = SCM_MAKE_CHAR(reconcile_state);

    scm_call_2(setters.split_scm_reconcile_state, split_scm, arg);
}


/********************************************************************\
 * gnc_split_scm_set_amount                                         *
 *   set the amount of a scheme split                               *
 *                                                                  *
 * Args: split_scm - the scheme split                               *
 *       amount    - the amount to set                              *
 * Returns: Nothing                                                 *
\********************************************************************/
void
gnc_split_scm_set_amount(SCM split_scm, gnc_numeric amount)
{
    SCM arg;

    initialize_scm_functions();

    if (!gnc_is_split_scm(split_scm))
        return;

    arg = gnc_numeric_to_scm(amount);
    scm_call_2(setters.split_scm_amount, split_scm, arg);
}


/********************************************************************\
 * gnc_split_scm_set_value                                          *
 *   set the value of a scheme split                                *
 *                                                                  *
 * Args: split_scm - the scheme split                               *
 *       value     - the value to set                               *
 * Returns: Nothing                                                 *
\********************************************************************/
void
gnc_split_scm_set_value(SCM split_scm, gnc_numeric value)
{
    SCM arg;

    initialize_scm_functions();

    if (!gnc_is_split_scm(split_scm))
        return;

    arg = gnc_numeric_to_scm(value);
    scm_call_2(setters.split_scm_value, split_scm, arg);
}


/********************************************************************\
 * gnc_split_scm_get_memo                                           *
 *   return the newly allocated memo of a scheme split, or NULL.    *
 *                                                                  *
 * Args: split_scm - the scheme split                               *
 * Returns: newly allocated memo string, must be freed with g_free  *
\********************************************************************/
char *
gnc_split_scm_get_memo(SCM split_scm)
{
    SCM result;

    initialize_scm_functions();

    if (!gnc_is_split_scm(split_scm))
        return NULL;

    result = scm_call_1(getters.split_scm_memo, split_scm);
    if (!scm_is_string(result))
        return NULL;

    return gnc_scm_to_utf8_string(result);
}


/**********************************************************************\
 * gnc_split_scm_get_action                                           *
 *   return the newly allocated action of a scheme split, or NULL.    *
 *                                                                    *
 * Args: split_scm - the scheme split                                 *
 * Returns: newly allocated action string, must be freed with g_free  *
\**********************************************************************/
char *
gnc_split_scm_get_action(SCM split_scm)
{
    SCM result;

    initialize_scm_functions();

    if (!gnc_is_split_scm(split_scm))
        return NULL;

    result = scm_call_1(getters.split_scm_action, split_scm);
    if (!scm_is_string(result))
        return NULL;

    return gnc_scm_to_utf8_string(result);
}


/********************************************************************\
 * gnc_split_scm_get_amount                                         *
 *   return the amount of a scheme split                            *
 *                                                                  *
 * Args: split_scm - the scheme split                               *
 * Returns: amount of scheme split                                  *
\********************************************************************/
gnc_numeric
gnc_split_scm_get_amount(SCM split_scm)
{
    SCM result;

    initialize_scm_functions();

    if (!gnc_is_split_scm(split_scm))
        return gnc_numeric_zero ();

    result = scm_call_1(getters.split_scm_amount, split_scm);
    if (!gnc_numeric_p(result))
        return gnc_numeric_zero ();

    return gnc_scm_to_numeric(result);
}


/********************************************************************\
 * gnc_split_scm_get_value                                          *
 *   return the value of a scheme split                             *
 *                                                                  *
 * Args: split_scm - the scheme split                               *
 * Returns: value of scheme split                                   *
\********************************************************************/
gnc_numeric
gnc_split_scm_get_value(SCM split_scm)
{
    SCM result;

    initialize_scm_functions();

    if (!gnc_is_split_scm(split_scm))
        return gnc_numeric_zero ();

    result = scm_call_1(getters.split_scm_value, split_scm);
    if (!gnc_numeric_p(result))
        return gnc_numeric_zero ();

    return gnc_scm_to_numeric(result);
}


/********************************************************************\
 * gnc_copy_trans                                                   *
 *   returns a scheme representation of a transaction. If the       *
 *   transaction is NULL, SCM_UNDEFINED is returned.                *
 *                                                                  *
 * Args: trans             - the transaction to copy                *
 *       use_cut_semantics - if TRUE, copy is for a 'cut' operation *
 * Returns: SCM representation of transaction or SCM_UNDEFINED      *
\********************************************************************/
SCM
gnc_copy_trans(Transaction *trans, gboolean use_cut_semantics)
{
    static swig_type_info *trans_type = NULL;
    SCM func;
    SCM arg;

    if (trans == NULL)
        return SCM_UNDEFINED;

    func = scm_c_eval_string("gnc:transaction->transaction-scm");
    if (!scm_is_procedure(func))
        return SCM_UNDEFINED;

    if (!trans_type)
        trans_type = SWIG_TypeQuery("_p_Transaction");

    arg = SWIG_NewPointerObj(trans, trans_type, 0);

    return scm_call_2(func, arg, SCM_BOOL(use_cut_semantics));
}


/********************************************************************\
 * gnc_copy_trans_scm_onto_trans                                    *
 *   copies a scheme representation of a transaction onto           *
 *   an actual transaction.                                         *
 *                                                                  *
 * Args: trans_scm - the scheme representation of a transaction     *
 *       trans     - the transaction to copy onto                   *
 * Returns: Nothing                                                 *
\********************************************************************/
void
gnc_copy_trans_scm_onto_trans(SCM trans_scm, Transaction *trans,
                              gboolean do_commit, QofBook *book)
{
    gnc_copy_trans_scm_onto_trans_swap_accounts(trans_scm, trans, NULL, NULL,
            do_commit, book);
}


/********************************************************************\
 * gnc_copy_trans_scm_onto_trans_swap_accounts                      *
 *   copies a scheme representation of a transaction onto           *
 *   an actual transaction. If guid_1 and guid_2 are not NULL,      *
 *   the account guids of the splits are swapped accordingly.       *
 *                                                                  *
 * Args: trans_scm - the scheme representation of a transaction     *
 *       trans     - the transaction to copy onto                   *
 *       guid_1    - account guid to swap with guid_2               *
 *       guid_2    - account guid to swap with guid_1               *
 *       do_commit - whether to commit the edits                    *
 * Returns: Nothing                                                 *
\********************************************************************/
void
gnc_copy_trans_scm_onto_trans_swap_accounts(SCM trans_scm,
        Transaction *trans,
        const GncGUID *guid_1,
        const GncGUID *guid_2,
        gboolean do_commit,
        QofBook *book)
{
    static swig_type_info *trans_type = NULL;
    SCM result;
    SCM func;
    SCM arg;

    if (trans_scm == SCM_UNDEFINED)
        return;

    if (trans == NULL)
        return;

    g_return_if_fail (book);

    func = scm_c_eval_string("gnc:transaction-scm?");
    if (!scm_is_procedure(func))
        return;

    result = scm_call_1(func, trans_scm);
    if (!scm_is_true(result))
        return;

    func = scm_c_eval_string("gnc:transaction-scm-onto-transaction");
    if (!scm_is_procedure(func))
        return;

    if (!trans_type)
        trans_type = SWIG_TypeQuery("_p_Transaction");

    arg = SWIG_NewPointerObj(trans, trans_type, 0);

    if ((guid_1 == NULL) || (guid_2 == NULL))
    {
        SCM args = SCM_EOL;
        SCM commit;

        commit = SCM_BOOL(do_commit);

        args = scm_cons(gnc_book_to_scm (book), args);
        args = scm_cons(commit, args);
        args = scm_cons(SCM_EOL, args);
        args = scm_cons(arg, args);
        args = scm_cons(trans_scm, args);

        scm_apply(func, args, SCM_EOL);
    }
    else
    {
        gchar guidstr[GUID_ENCODING_LENGTH+1];
        SCM from, to;
        SCM map = SCM_EOL;
        SCM args = SCM_EOL;
        SCM commit;

        args = scm_cons(gnc_book_to_scm (book), args);

        commit = SCM_BOOL(do_commit);

        args = scm_cons(commit, args);

        guid_to_string_buff(guid_1, guidstr);
        from = scm_from_utf8_string(guidstr);
        guid_to_string_buff(guid_2, guidstr);
        to = scm_from_utf8_string(guidstr);

        map = scm_cons(scm_cons(from, to), map);
        map = scm_cons(scm_cons(to, from), map);

        args = scm_cons(map, args);
        args = scm_cons(arg, args);
        args = scm_cons(trans_scm, args);

        scm_apply(func, args, SCM_EOL);
    }
}

/********************************************************************\
 * gnc_trans_scm_set_date                                           *
 *   set the date of a scheme transaction.                          *
 *                                                                  *
 * Args: trans_scm - the scheme transaction                         *
 *       ts        - the time to set                                *
 * Returns: Nothing                                                 *
\********************************************************************/
void
gnc_trans_scm_set_date(SCM trans_scm, Timespec *ts)
{
    SCM arg;

    initialize_scm_functions();

    if (!gnc_is_trans_scm(trans_scm))
        return;
    if (ts == NULL)
        return;

    arg = gnc_timespec2timepair(*ts);

    scm_call_2(setters.trans_scm_date, trans_scm, arg);
}


/********************************************************************\
 * gnc_trans_scm_set_num                                            *
 *   set the num of a scheme transaction.                           *
 *                                                                  *
 * Args: trans_scm - the scheme transaction                         *
 *       num       - the num to set                                 *
 * Returns: Nothing                                                 *
\********************************************************************/
void
gnc_trans_scm_set_num(SCM trans_scm, const char *num)
{
    SCM arg;

    initialize_scm_functions();

    if (!gnc_is_trans_scm(trans_scm))
        return;
    if (num == NULL)
        return;

    arg = scm_from_utf8_string(num);

    scm_call_2(setters.trans_scm_num, trans_scm, arg);
}


/********************************************************************\
 * gnc_trans_scm_set_description                                    *
 *   set the description of a scheme transaction.                   *
 *                                                                  *
 * Args: trans_scm   - the scheme transaction                       *
 *       description - the description to set                       *
 * Returns: Nothing                                                 *
\********************************************************************/
void
gnc_trans_scm_set_description(SCM trans_scm, const char *description)
{
    SCM arg;

    initialize_scm_functions();

    if (!gnc_is_trans_scm(trans_scm))
        return;
    if (description == NULL)
        return;

    arg = scm_from_utf8_string(description);

    scm_call_2(setters.trans_scm_description, trans_scm, arg);
}


/********************************************************************\
 * gnc_trans_scm_set_notes                                          *
 *   set the notes of a scheme transaction.                         *
 *                                                                  *
 * Args: trans_scm - the scheme transaction                         *
 *       notes     - the notes to set                               *
 * Returns: Nothing                                                 *
\********************************************************************/
void
gnc_trans_scm_set_notes(SCM trans_scm, const char *notes)
{
    SCM arg;

    initialize_scm_functions();

    if (!gnc_is_trans_scm(trans_scm))
        return;
    if (notes == NULL)
        return;

    arg = scm_from_utf8_string(notes);

    scm_call_2(setters.trans_scm_notes, trans_scm, arg);
}


/********************************************************************\
 * gnc_trans_scm_append_split_scm                                   *
 *   append the scheme split onto the scheme transaction            *
 *                                                                  *
 * Args: trans_scm - the scheme transaction                         *
 *       split_scm - the scheme split to append                     *
 * Returns: Nothing                                                 *
\********************************************************************/
void
gnc_trans_scm_append_split_scm(SCM trans_scm, SCM split_scm)
{
    initialize_scm_functions();

    if (!gnc_is_trans_scm(trans_scm))
        return;
    if (!gnc_is_split_scm(split_scm))
        return;

    scm_call_2(setters.trans_scm_append_split_scm, trans_scm, split_scm);
}


/********************************************************************\
 * gnc_trans_scm_get_split_scm                                      *
 *   get the indexth scheme split of a scheme transaction.          *
 *                                                                  *
 * Args: trans_scm - the scheme transaction                         *
 *       index     - the index of the split to get                  *
 * Returns: scheme split to get, or SCM_UNDEFINED if none           *
\********************************************************************/
SCM
gnc_trans_scm_get_split_scm(SCM trans_scm, int index)
{
    SCM arg;

    initialize_scm_functions();

    if (!gnc_is_trans_scm(trans_scm))
        return SCM_UNDEFINED;

    arg = scm_from_int (index);

    return scm_call_2(getters.trans_scm_split_scm, trans_scm, arg);
}


/********************************************************************\
 * gnc_trans_scm_get_other_split_scm                                *
 *   get the other scheme split of a scheme transaction.            *
 *                                                                  *
 * Args: trans_scm - the scheme transaction                         *
 *       split_scm - the split not to get                           *
 * Returns: other scheme split, or SCM_UNDEFINED if none            *
\********************************************************************/
SCM
gnc_trans_scm_get_other_split_scm(SCM trans_scm, SCM split_scm)
{
    SCM result;

    initialize_scm_functions();

    if (!gnc_is_trans_scm(trans_scm))
        return SCM_UNDEFINED;
    if (!gnc_is_split_scm(split_scm))
        return SCM_UNDEFINED;

    result = scm_call_2(getters.trans_scm_other_split_scm, trans_scm, split_scm);

    if (!gnc_is_split_scm(result))
        return SCM_UNDEFINED;

    return result;
}


/********************************************************************\
 * gnc_trans_scm_get_num_splits                                     *
 *   get the number of scheme splits in a scheme transaction.       *
 *                                                                  *
 * Args: trans_scm - the scheme transaction                         *
 * Returns: number of scheme splits in the transaction              *
\********************************************************************/
int
gnc_trans_scm_get_num_splits(SCM trans_scm)
{
    SCM result;

    initialize_scm_functions();

    if (!gnc_is_trans_scm(trans_scm))
        return 0;

    result = scm_call_1(getters.trans_scm_split_scms, trans_scm);

    if (!scm_is_list(result))
        return 0;

    return scm_to_int(scm_length(result));
}


/********************************************************************\
 * gnc_get_debit_string                                             *
 *   return a debit string for a given account type                 *
 *                                                                  *
 * Args: account_type - type of account to get debit string for     *
 * Return: g_malloc'd debit string or NULL                          *
\********************************************************************/
char *
gnc_get_debit_string(GNCAccountType account_type)
{
    SCM result;
    SCM arg;

    initialize_scm_functions();

    if (gnc_prefs_get_bool(GNC_PREFS_GROUP_GENERAL, GNC_PREF_ACCOUNTING_LABELS))
        return g_strdup(_("Debit"));

    if ((account_type < ACCT_TYPE_NONE) || (account_type >= NUM_ACCOUNT_TYPES))
        account_type = ACCT_TYPE_NONE;

    arg = scm_from_long (account_type);

    result = scm_call_1(getters.debit_string, arg);
    if (!scm_is_string(result))
        return NULL;

    return scm_to_utf8_string(result);
}


/************************************************************************\
 * gnc_get_credit_string                                                *
 *   return a credit string for a given account type                    *
 *                                                                      *
 * Args: account_type - type of account to get credit string for        *
 * Return: g_malloc'd credit string or NULL, must be freed with g_free  *
\************************************************************************/
char *
gnc_get_credit_string(GNCAccountType account_type)
{
    SCM result;
    SCM arg;

    initialize_scm_functions();

    if (gnc_prefs_get_bool(GNC_PREFS_GROUP_GENERAL, GNC_PREF_ACCOUNTING_LABELS))
        return g_strdup(_("Credit"));

    if ((account_type < ACCT_TYPE_NONE) || (account_type >= NUM_ACCOUNT_TYPES))
        account_type = ACCT_TYPE_NONE;

    arg = scm_from_long (account_type);

    result = scm_call_1(getters.credit_string, arg);
    if (!scm_is_string(result))
        return NULL;

    return gnc_scm_to_utf8_string(result);
}


static void
on_child_exit (GPid pid, gint status, gpointer data)
{
    Process *proc = data;
    g_return_if_fail (proc && proc->pid == pid);

    g_spawn_close_pid (proc->pid);

    /* free if the process is both dead and detached */
    if (!proc->detached)
        proc->dead = TRUE;
    else
        g_free (proc);
}

Process *
gnc_spawn_process_async (GList *argl, const gboolean search_path)
{
    gboolean retval;
    Process *proc;
    GList *l_iter;
    guint argc;
    gchar **argv, **v_iter;
    GSpawnFlags flags;
    GError *error = NULL;

    proc = g_new0 (Process, 1);

    argc = g_list_length (argl);
    argv = g_malloc ((argc + 1) * sizeof(gchar*));

    for (l_iter = argl, v_iter = argv; l_iter; l_iter = l_iter->next, v_iter++)
    {
        *v_iter = (gchar*) l_iter->data;
    }
    *v_iter = NULL;
    g_list_free (argl);

    flags = G_SPAWN_DO_NOT_REAP_CHILD;
    if (search_path)
        flags |= G_SPAWN_SEARCH_PATH;

    retval = g_spawn_async_with_pipes (
                 NULL, argv, NULL, flags, NULL, NULL, &proc->pid,
                 &proc->fd_stdin, &proc->fd_stdout, &proc->fd_stderr, &error);

    if (retval)
    {
        g_child_watch_add (proc->pid, on_child_exit, proc);
    }
    else
    {
        g_warning ("Could not spawn %s: %s", *argv ? *argv : "(null)",
                   error->message ? error->message : "(null)");
        g_free (proc);
        proc = NULL;
    }
    g_strfreev (argv);

    return proc;
}

gint
gnc_process_get_fd (const Process *proc, const gint std_fd)
{
    const gint *retptr = NULL;
    g_return_val_if_fail (proc, -1);

    if (std_fd == 0)
        retptr = &proc->fd_stdin;
    else if (std_fd == 1)
        retptr = &proc->fd_stdout;
    else if (std_fd == 2)
        retptr = &proc->fd_stderr;
    else
        g_return_val_if_reached (-1);

    if (*retptr == -1)
        g_warning ("Pipe to childs file descriptor %d is -1", std_fd);
    return *retptr;
}

void
gnc_detach_process (Process *proc, const gboolean kill_it)
{
    g_return_if_fail (proc && proc->pid);

    errno = 0;
    close (proc->fd_stdin);
    if (errno)
    {
        g_message ("Close of childs stdin (%d) failed: %s", proc->fd_stdin,
                   g_strerror (errno));
        errno = 0;
    }
    close (proc->fd_stdout);
    if (errno)
    {
        g_message ("Close of childs stdout (%d) failed: %s", proc->fd_stdout,
                   g_strerror(errno));
        errno = 0;
    }
    close (proc->fd_stderr);
    if (errno)
    {
        g_message ("Close of childs stderr (%d) failed: %s", proc->fd_stderr,
                   g_strerror(errno));
        errno = 0;
    }

    if (kill_it && !proc->dead)
    {
        /* give it a chance to die */
        while (g_main_context_iteration (NULL, FALSE) && !proc->dead)
            ;
        if (!proc->dead)
            gnc_gpid_kill (proc->pid);
    }

    /* free if the process is both dead and detached */
    if (!proc->dead)
        proc->detached = TRUE;
    else
        g_free (proc);
}


time64
gnc_parse_time_to_time64 (const gchar *s, const gchar *format)
{
    struct tm tm;

    g_return_val_if_fail(s && format, -1);

    if (!strptime(s, format, &tm))
        return -1;

    return gnc_mktime(&tm);
}
