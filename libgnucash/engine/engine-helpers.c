/********************************************************************\
 * engine-helpers.c -- gnucash engine helper functions              *
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

#include <config.h>

#include <string.h>

#include "Account.h"
#include "engine-helpers.h"
#include "gnc-date.h"
#include "gnc-engine.h"
#include "gnc-session.h"
#include <qof.h>
#include <qofbookslots.h>

/** \todo Code dependent on the private query headers
qofquery-p.h and qofquerycore-p.h may need to be modified.
These files are temporarily exported for QOF 0.6.0 but
cannot be considered "standard" or public parts of QOF. */
#include "qofquery-p.h"
#include "qofquerycore-p.h"

#define FUNC_NAME G_STRFUNC

static QofLogModule log_module = GNC_MOD_ENGINE;

/** Gets the transaction Number or split Action based on book option:
  * if the book option is TRUE (split action is used for NUM) and a
  * split is provided, split-action is returned; if book option is FALSE
  * (tran-num is used for NUM) and a trans is provided, transaction-num
  * is returned; if split is provided and tran is NULL, split-action is
  * returned; if tran is provided and split is NULL, transaction-num is
  * returned. Otherwise NULL is returned.*/
const char *
gnc_get_num_action (const Transaction *trans, const Split *split)
{
    if (trans && !split)
        return xaccTransGetNum(trans);
    if (split && !trans)
        return xaccSplitGetAction(split);
    if (trans && split)
    {
        QofBook* book = qof_session_get_book(gnc_get_current_session ());
        if (!book)
        {
            PERR("Session has no book but has a transaction or split!");
            return NULL;
        }
        if (qof_book_use_split_action_for_num_field (book))
            return xaccSplitGetAction(split);
        else
            return xaccTransGetNum(trans);
    }
    else return NULL;
}

/** Opposite of 'gnc_get_num_action'; if the book option is TRUE (split action
  * is used for NUM) and a trans is provided, transaction-num is returned; if
  * book option is FALSE (tran-num is used for NUM) and a split is provided,
  * split-action is returned; if split is provided and tran is NULL,
  * split-action is returned; if tran is provided and split is NULL,
  * transaction-num is returned. Otherwise NULL is returned.*/
const char *
gnc_get_action_num (const Transaction *trans, const Split *split)
{
    gboolean num_action = qof_book_use_split_action_for_num_field
                           (qof_session_get_book(gnc_get_current_session ()));

    if (trans && !split)
        return xaccTransGetNum(trans);
    if (split && !trans)
        return xaccSplitGetAction(split);
    if (trans && split)
    {
        if (num_action)
            return xaccTransGetNum(trans);
        else
            return xaccSplitGetAction(split);
    }
    else return NULL;
}

/** Sets the transaction Number and/or split Action based on book option:
  * if the book option is TRUE (split action is to be used for NUM) then 'num'
  * sets split-action and, if 'tran' and 'action' are provided, 'action'
  * sets transaction-num; if book option is FALSE (tran-num is to be used for NUM)
  * then 'num' sets transaction-num and, if 'split' and 'action' are
  * provided, 'action' sets 'split-action'. If any arguments are NULL,
  * no change is made to the field that would otherwise be
  * affected. If 'tran' and 'num' are passed with 'split and 'action' set to
  * NULL, it is xaccTransSetNum (trans, num). Likewise, if 'split and 'action'
  * are passed with 'tran' and 'num' set to NULL, it is xaccSplitSetAction (split,
  * action). */
void
gnc_set_num_action (Transaction *trans, Split *split,
                    const char *num, const char *action)
{
    gboolean num_action = qof_book_use_split_action_for_num_field
                           (qof_session_get_book(gnc_get_current_session ()));

    if (trans && num && !split && !action)
    {
        xaccTransSetNum (trans, num);
        return;
    }

    if (!trans && !num && split && action)
    {
        xaccSplitSetAction (split, action);
        return;
    }

    if (trans)
    {
        if (!num_action && num)
            xaccTransSetNum (trans, num);
        if (num_action && action)
            xaccTransSetNum (trans, action);
    }

    if (split)
    {
        if (!num_action && action)
            xaccSplitSetAction (split, action);
        if (num_action && num)
           xaccSplitSetAction (split, num);
    }
}

/************************************************************/
/*           Notification of Book Option Changes            */
/************************************************************/

static GOnce bo_init_once = G_ONCE_INIT;
static GHashTable *bo_callback_hash = NULL;
static GHookList *bo_final_hook_list = NULL;

static gpointer
bo_init (gpointer unused)
{
    bo_callback_hash = g_hash_table_new(g_str_hash, g_str_equal);

    bo_final_hook_list = g_malloc(sizeof(GHookList));
    g_hook_list_init(bo_final_hook_list, sizeof(GHook));
    return NULL;
}

static void
bo_call_hook (GHook *hook, gpointer data)
{
    ((GFunc)hook->func)(data, hook->data);
}

/** Calls registered callbacks when num_field_source book option changes so that
  * registers/reports can update themselves */
void
gnc_book_option_num_field_source_change (gboolean num_action)
{
    GHookList *hook_list;
    const gchar *key = OPTION_NAME_NUM_FIELD_SOURCE;

    g_once(&bo_init_once, bo_init, NULL);

    hook_list = g_hash_table_lookup(bo_callback_hash, key);
    if (hook_list != NULL)
        g_hook_list_marshal(hook_list, TRUE, bo_call_hook, &num_action);
    g_hook_list_invoke(bo_final_hook_list, TRUE);
}

void
gnc_book_option_register_cb (const gchar *key, GncBOCb func, gpointer user_data)
{
    GHookList *hook_list;
    GHook *hook;

    g_once(&bo_init_once, bo_init, NULL);
    hook_list = g_hash_table_lookup(bo_callback_hash, key);
    if (hook_list == NULL)
    {
        hook_list = g_malloc(sizeof(GHookList));
        g_hook_list_init(hook_list, sizeof(GHook));
        g_hash_table_insert(bo_callback_hash, (gpointer)key, hook_list);
    }

    hook = g_hook_find_func_data(hook_list, TRUE, func, user_data);
    if (hook != NULL)
    {
        return;
    }

    hook = g_hook_alloc(hook_list);
    hook->func = func;
    hook->data = user_data;
    g_hook_append(hook_list, hook);
}

void
gnc_book_option_remove_cb (const gchar *key, GncBOCb func, gpointer user_data)
{
    GHookList *hook_list;
    GHook *hook;

    g_once(&bo_init_once, bo_init, NULL);
    hook_list = g_hash_table_lookup(bo_callback_hash, key);
    if (hook_list == NULL)
        return;
    hook = g_hook_find_func_data(hook_list, TRUE, func, user_data);
    if (hook == NULL)
        return;

    g_hook_destroy_link(hook_list, hook);
    if (hook_list->hooks == NULL)
    {
        g_hash_table_remove(bo_callback_hash, key);
        g_free(hook_list);
    }
}


