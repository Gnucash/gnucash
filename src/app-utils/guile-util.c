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

#include <string.h>

#include "global-options.h"
#include "gnc-engine-util.h"
#include "engine-helpers.h"
#include "glib-helpers.h"
#include "guile-util.h"
#include "messages.h"

#include <g-wrap-runtime-guile.h>

/* This static indicates the debugging module this .o belongs to.  */
static short module = MOD_GUILE;


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


static void
initialize_scm_functions()
{
  static gboolean scm_funcs_inited = FALSE;

  if (scm_funcs_inited)
    return;

  setters.split_scm_account_guid =
    gh_eval_str("gnc:split-scm-set-account-guid");
  setters.split_scm_memo = gh_eval_str("gnc:split-scm-set-memo");
  setters.split_scm_action = gh_eval_str("gnc:split-scm-set-action");
  setters.split_scm_reconcile_state =
    gh_eval_str("gnc:split-scm-set-reconcile-state");
  setters.split_scm_amount = gh_eval_str("gnc:split-scm-set-amount");
  setters.split_scm_value = gh_eval_str("gnc:split-scm-set-value");

  setters.trans_scm_date = gh_eval_str("gnc:transaction-scm-set-date-posted");
  setters.trans_scm_num = gh_eval_str("gnc:transaction-scm-set-num");
  setters.trans_scm_description =
    gh_eval_str("gnc:transaction-scm-set-description");
  setters.trans_scm_notes = gh_eval_str("gnc:transaction-scm-set-notes");
  setters.trans_scm_append_split_scm =
    gh_eval_str("gnc:transaction-scm-append-split-scm");

  getters.split_scm_memo = gh_eval_str("gnc:split-scm-get-memo");
  getters.split_scm_action = gh_eval_str("gnc:split-scm-get-action");
  getters.split_scm_amount = gh_eval_str("gnc:split-scm-get-amount");
  getters.split_scm_value = gh_eval_str("gnc:split-scm-get-value");

  getters.trans_scm_split_scms =
    gh_eval_str("gnc:transaction-scm-get-split-scms");
  getters.trans_scm_split_scm =
    gh_eval_str("gnc:transaction-scm-get-split-scm");
  getters.trans_scm_other_split_scm =
    gh_eval_str("gnc:transaction-scm-get-other-split-scm");

  getters.debit_string = gh_eval_str("gnc:get-debit-string");
  getters.credit_string = gh_eval_str("gnc:get-credit-string");

  predicates.is_split_scm = gh_eval_str("gnc:split-scm?");
  predicates.is_trans_scm = gh_eval_str("gnc:transaction-scm?");

  scm_funcs_inited = TRUE;
}


/********************************************************************\
 * gnc_guile_call1_to_string                                        *
 *   returns the malloc'ed string returned by the guile function    *
 *   or NULL if it can't be retrieved                               *
 *                                                                  *
 * Args: func - the guile function to call                          *
 *       arg  - the single function argument                        *
 * Returns: malloc'ed char * or NULL                                *
\********************************************************************/
char *
gnc_guile_call1_to_string(SCM func, SCM arg)
{
  SCM value;

  if (gh_procedure_p(func))
  {
    value = gh_call1(func, arg);

    if (gh_string_p(value))
      return gh_scm2newstr(value, NULL);
    else
    {
      PERR("bad value\n");
    }
  }
  else
  {
    PERR("not a procedure\n");
  }

  return NULL;
}


/********************************************************************\
 * gnc_guile_call1_symbol_to_string                                 *
 *   returns the malloc'ed string returned by the guile function    *
 *   or NULL if it can't be retrieved. The return value of the      *
 *   function should be a symbol.                                   *
 *                                                                  *
 * Args: func - the guile function to call                          *
 *       arg  - the single function argument                        *
 * Returns: malloc'ed char * or NULL                                *
\********************************************************************/
char *
gnc_guile_call1_symbol_to_string(SCM func, SCM arg)
{
  SCM value;

  if (gh_procedure_p(func))
  {
    value = gh_call1(func, arg);

    if (gh_symbol_p(value))
      return gh_symbol2newstr(value, NULL);
    else
    {
      PERR("bad value\n");
    }
  }
  else
  {
    PERR("not a procedure\n");
  }

  return NULL;
}


/********************************************************************\
 * gnc_guile_call1_to_procedure                                     *
 *   returns the SCM handle to the procedure returned by the guile  *
 *   function, or SCM_UNDEFINED if it couldn't be retrieved.        *
 *                                                                  *
 * Args: func - the guile function to call                          *
 *       arg  - the single function argument                        *
 * Returns: SCM function handle or SCM_UNDEFINED                    *
\********************************************************************/
SCM
gnc_guile_call1_to_procedure(SCM func, SCM arg)
{
  SCM value;

  if (gh_procedure_p(func))
  {
    value = gh_call1(func, arg);

    if (gh_procedure_p(value))
      return value;
    else
    {
      PERR("bad value\n");
    }
  }
  else
  {
    PERR("not a procedure\n");
  }

  return SCM_UNDEFINED;
}


/********************************************************************\
 * gnc_guile_call1_to_list                                          *
 *   returns the SCM handle to the list returned by the guile       *
 *   function, or SCM_UNDEFINED if it couldn't be retrieved.        *
 *                                                                  *
 * Args: func - the guile function to call                          *
 *       arg  - the single function argument                        *
 * Returns: SCM list handle or SCM_UNDEFINED                        *
\********************************************************************/
SCM
gnc_guile_call1_to_list(SCM func, SCM arg)
{
  SCM value;

  if (gh_procedure_p(func))
  {
    value = gh_call1(func, arg);

    if (gh_list_p(value))
      return value;
    else
    {
      PERR("bad value\n");
    }
  }
  else
  {
    PERR("not a procedure\n");
  }

  return SCM_UNDEFINED;
}


/********************************************************************\
 * gnc_guile_call1_to_vector                                        *
 *   returns the SCM handle to the vector returned by the guile     *
 *   function, or SCM_UNDEFINED if it couldn't be retrieved.        *
 *                                                                  *
 * Args: func - the guile function to call                          *
 *       arg  - the single function argument                        *
 * Returns: SCM vector handle or SCM_UNDEFINED                      *
\********************************************************************/
SCM
gnc_guile_call1_to_vector(SCM func, SCM arg)
{
  SCM value;

  if (gh_procedure_p(func))
  {
    value = gh_call1(func, arg);

    if (gh_vector_p(value))
      return value;
    else
    {
      PERR("bad value\n");
    }
  }
  else
  {
    PERR("not a procedure\n");
  }

  return SCM_UNDEFINED;
}


/********************************************************************\
 * gnc_depend                                                       *
 *   ensure the given scm file has been loaded, or return FALSE     *
 *   if it cannot be loaded for any reason.                         *
 *                                                                  *
 * Args: scm_file - the file to load if it hasn't been already      *
 * Returns: true if the file has been loaded, false otherwise       *
\********************************************************************/
gboolean
gnc_depend(const char *scm_file)
{
  static SCM depend_func = SCM_UNDEFINED;
  SCM arg;

  if (scm_file == NULL)
    return FALSE;

  if (depend_func == SCM_UNDEFINED)
    depend_func = gh_eval_str("gnc:depend");

  if (!gh_procedure_p(depend_func))
    return FALSE;

  arg = gh_str02scm(scm_file);

  return gh_scm2bool(gh_call1(depend_func, arg));
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
  static SCM split_type = SCM_UNDEFINED;
  SCM func;
  SCM arg;

  if (split == NULL)
    return SCM_UNDEFINED;

  func = gh_eval_str("gnc:split->split-scm");
  if (!gh_procedure_p(func))
    return SCM_UNDEFINED;

  if(split_type == SCM_UNDEFINED) {
    split_type = gh_eval_str("<gnc:Split*>");
    /* don't really need this - types are bound globally anyway. */
    if(split_type != SCM_UNDEFINED) scm_protect_object(split_type);
  }

  arg = gw_wcp_assimilate_ptr(split, split_type);

  return gh_call2(func, arg, gh_bool2scm(use_cut_semantics));
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
gnc_copy_split_scm_onto_split(SCM split_scm, Split *split)
{
  static SCM split_type = SCM_UNDEFINED;
  SCM result;
  SCM func;
  SCM arg;

  if (split_scm == SCM_UNDEFINED)
    return;

  if (split == NULL)
    return;

  func = gh_eval_str("gnc:split-scm?");
  if (!gh_procedure_p(func))
    return;

  result = gh_call1(func, split_scm);
  if (!gh_scm2bool(result))
    return;

  func = gh_eval_str("gnc:split-scm-onto-split");
  if (!gh_procedure_p(func))
    return;

  if(split_type == SCM_UNDEFINED) {
    split_type = gh_eval_str("<gnc:Split*>");
    /* don't really need this - types are bound globally anyway. */
    if(split_type != SCM_UNDEFINED) scm_protect_object(split_type);
  }

  arg = gw_wcp_assimilate_ptr(split, split_type);
  gh_call2(func, split_scm, arg);
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

  return gh_scm2bool(gh_call1(predicates.is_split_scm, scm));
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

  return gh_scm2bool(gh_call1(predicates.is_trans_scm, scm));
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
  char *guid_string;
  SCM arg;

  initialize_scm_functions();

  if (!gnc_is_split_scm(split_scm))
    return;
  if (account == NULL)
    return;

  guid_string = guid_to_string(xaccAccountGetGUID(account));
  if (guid_string == NULL)
    return;

  arg = gh_str02scm(guid_string);

  gh_call2(setters.split_scm_account_guid, split_scm, arg);

  g_free(guid_string);
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

  arg = gh_str02scm(memo);

  gh_call2(setters.split_scm_memo, split_scm, arg);
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

  arg = gh_str02scm(action);

  gh_call2(setters.split_scm_action, split_scm, arg);
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

  arg = gh_char2scm(reconcile_state);

  gh_call2(setters.split_scm_reconcile_state, split_scm, arg);
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
  gh_call2(setters.split_scm_amount, split_scm, arg);
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
  gh_call2(setters.split_scm_value, split_scm, arg);
}


/********************************************************************\
 * gnc_split_scm_get_memo                                           *
 *   return the newly allocated memo of a scheme split, or NULL.    *
 *                                                                  *
 * Args: split_scm - the scheme split                               *
 * Returns: newly allocated memo string                             *
\********************************************************************/
char *
gnc_split_scm_get_memo(SCM split_scm)
{
  SCM result;

  initialize_scm_functions();

  if (!gnc_is_split_scm(split_scm))
    return NULL;

  result = gh_call1(getters.split_scm_memo, split_scm);
  if (!gh_string_p(result))
    return NULL;

  return gh_scm2newstr(result, NULL);
}


/********************************************************************\
 * gnc_split_scm_get_action                                         *
 *   return the newly allocated action of a scheme split, or NULL.  *
 *                                                                  *
 * Args: split_scm - the scheme split                               *
 * Returns: newly allocated action string                           *
\********************************************************************/
char *
gnc_split_scm_get_action(SCM split_scm)
{
  SCM result;

  initialize_scm_functions();

  if (!gnc_is_split_scm(split_scm))
    return NULL;

  result = gh_call1(getters.split_scm_action, split_scm);
  if (!gh_string_p(result))
    return NULL;

  return gh_scm2newstr(result, NULL);
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

  result = gh_call1(getters.split_scm_amount, split_scm);
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

  result = gh_call1(getters.split_scm_value, split_scm);
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
  static SCM trans_type = SCM_UNDEFINED;
  SCM func;
  SCM arg;

  if (trans == NULL)
    return SCM_UNDEFINED;

  func = gh_eval_str("gnc:transaction->transaction-scm");
  if (!gh_procedure_p(func))
    return SCM_UNDEFINED;

  if(trans_type == SCM_UNDEFINED) {
    trans_type = gh_eval_str("<gnc:Transaction*>");
    /* don't really need this - types are bound globally anyway. */
    if(trans_type != SCM_UNDEFINED) scm_protect_object(trans_type);
  }

  arg = gw_wcp_assimilate_ptr(trans, trans_type);

  return gh_call2(func, arg, gh_bool2scm(use_cut_semantics));
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
                              gboolean do_commit, GNCSession *session)
{
  gnc_copy_trans_scm_onto_trans_swap_accounts(trans_scm, trans, NULL, NULL,
                                              do_commit, session);
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
                                            const GUID *guid_1,
                                            const GUID *guid_2,
                                            gboolean do_commit,
                                            GNCSession *session)
{
  static SCM trans_type = SCM_UNDEFINED;
  SCM result;
  SCM func;
  SCM arg;

  if (trans_scm == SCM_UNDEFINED)
    return;

  if (trans == NULL)
    return;

  g_return_if_fail (session);

  func = gh_eval_str("gnc:transaction-scm?");
  if (!gh_procedure_p(func))
    return;

  result = gh_call1(func, trans_scm);
  if (!gh_scm2bool(result))
    return;

  func = gh_eval_str("gnc:transaction-scm-onto-transaction");
  if (!gh_procedure_p(func))
    return;

  if(trans_type == SCM_UNDEFINED) {
    trans_type = gh_eval_str("<gnc:Transaction*>");
    /* don't really need this - types are bound globally anyway. */
    if(trans_type != SCM_UNDEFINED) scm_protect_object(trans_type);
  }

  arg = gw_wcp_assimilate_ptr(trans, trans_type);

  if ((guid_1 == NULL) || (guid_2 == NULL))
  {
    SCM args = SCM_EOL;
    SCM commit;

    commit = gh_bool2scm(do_commit);

    args = gh_cons(gnc_session_to_scm (session), args);
    args = gh_cons(commit, args);
    args = gh_cons(SCM_EOL, args);
    args = gh_cons(arg, args);
    args = gh_cons(trans_scm, args);

    gh_apply(func, args);
  }
  else
  {
    SCM from, to;
    SCM map = SCM_EOL;
    SCM args = SCM_EOL;
    SCM commit;
    char *guid_str;

    args = gh_cons(gnc_session_to_scm (session), args);

    commit = gh_bool2scm(do_commit);

    args = gh_cons(commit, args);

    guid_str = guid_to_string(guid_1);
    from = gh_str02scm(guid_str);
    g_free (guid_str);

    guid_str = guid_to_string(guid_2);
    to = gh_str02scm(guid_str);
    g_free (guid_str);

    map = gh_cons(gh_cons(from, to), map);
    map = gh_cons(gh_cons(to, from), map);

    args = gh_cons(map, args);
    args = gh_cons(arg, args);
    args = gh_cons(trans_scm, args);

    gh_apply(func, args);
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

  gh_call2(setters.trans_scm_date, trans_scm, arg);
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

  arg = gh_str02scm(num);

  gh_call2(setters.trans_scm_num, trans_scm, arg);
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

  arg = gh_str02scm(description);

  gh_call2(setters.trans_scm_description, trans_scm, arg);
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

  arg = gh_str02scm(notes);

  gh_call2(setters.trans_scm_notes, trans_scm, arg);
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

  gh_call2(setters.trans_scm_append_split_scm, trans_scm, split_scm);
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

  arg = gh_int2scm(index);

  return gh_call2(getters.trans_scm_split_scm, trans_scm, arg);
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

  result = gh_call2(getters.trans_scm_other_split_scm, trans_scm, split_scm);

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

  result = gh_call1(getters.trans_scm_split_scms, trans_scm);

  if (!gh_list_p(result))
    return 0;

  return gh_length(result);
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
  char *type_string;
  char *string;
  char *temp;
  SCM result;
  SCM arg;

  initialize_scm_functions();

  if (gnc_lookup_boolean_option("General", "Use accounting labels", FALSE))
    return g_strdup(_("Debit"));

  if ((account_type < NO_TYPE) || (account_type >= NUM_ACCOUNT_TYPES))
    account_type = NO_TYPE;

  type_string = xaccAccountTypeEnumAsString(account_type);

  arg = gh_symbol2scm(type_string);

  result = gh_call1(getters.debit_string, arg);
  if (!gh_string_p(result))
    return NULL;

  string = gh_scm2newstr(result, NULL);
  if (string)
  {
    temp = g_strdup (string);
    free (string);
  }
  else
    temp = NULL;

  return temp;
}


/********************************************************************\
 * gnc_get_credit_string                                            *
 *   return a credit string for a given account type                *
 *                                                                  *
 * Args: account_type - type of account to get credit string for    *
 * Return: g_malloc'd credit string or NULL                         *
\********************************************************************/
char *
gnc_get_credit_string(GNCAccountType account_type)
{
  char *type_string;
  char *string;
  char *temp;
  SCM result;
  SCM arg;

  initialize_scm_functions();

  if (gnc_lookup_boolean_option("General", "Use accounting labels", FALSE))
    return g_strdup(_("Credit"));

  if ((account_type < NO_TYPE) || (account_type >= NUM_ACCOUNT_TYPES))
    account_type = NO_TYPE;

  type_string = xaccAccountTypeEnumAsString(account_type);

  arg = gh_symbol2scm(type_string);

  result = gh_call1(getters.credit_string, arg);
  if (!gh_string_p(result))
    return NULL;

  string = gh_scm2newstr(result, NULL);
  if (string)
  {
    temp = g_strdup (string);
    free (string);
  }
  else
    temp = NULL;

  return temp;
}


