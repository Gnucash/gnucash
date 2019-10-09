/********************************************************************\
 * split-register-copy-ops.c -- implement copy/paste semantics for  *
 *                              transactions and splits             *
 * Copyright (C) 1999 Linas Vepstas                                 *
 * Copyright (C) 2000 Dave Peticolas                                *
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
 * along with this program; if not, write to the Free Software      *
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.        *
\********************************************************************/

#include <config.h>

#include "swig-runtime.h"
#include <platform.h>
#if PLATFORM(WINDOWS)
#include <winsock.h>
#include <windows.h>
#endif

#include <glib.h>
#include <glib/gi18n.h>
#include <string.h>
#ifdef __MINGW32__
#define _GL_UNISTD_H //Deflect poisonous define of close in Guile's GnuLib
#endif
#include <libguile.h>
#include <errno.h>
#ifdef HAVE_UNISTD_H
# ifdef close
#  undef close
# endif
# include <unistd.h>
#else
# include <io.h>
# define close _close
#endif
#ifndef HAVE_STRPTIME
#    include "strptime.h"
#endif

#include "qof.h"
#include "gnc-engine-guile.h"
#include "glib-guile.h"
#include "gnc-glib-utils.h"
#include "gnc-guile-utils.h"
#include "gnc-prefs.h"
#include "guile-mappings.h"
#include "split-register-copy-ops.h"

#define UNUSED_VAR     __attribute__ ((unused))

/* This static indicates the debugging module this .o belongs to.  */
static QofLogModule UNUSED_VAR log_module = GNC_MOD_GUILE;

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

    predicates.is_split_scm = scm_c_eval_string("gnc:split-scm?");
    predicates.is_trans_scm = scm_c_eval_string("gnc:transaction-scm?");

    scm_funcs_inited = TRUE;
}

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
static gboolean
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
static gboolean
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
    if (strlen(guid_string) == 0)
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
    if (!scm_rational_p(result))
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
    if (!scm_rational_p(result))
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
