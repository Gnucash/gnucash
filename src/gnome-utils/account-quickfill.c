/********************************************************************\
 * account-quickfill.h -- Create an account-name quick-fill         *
 * Copyright (C) 2004 Linas Vepstas <linas@linas.org>               *
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

#include "config.h"
#include "account-quickfill.h"
#include "gnc-engine-util.h"
#include "gnc-event.h"
#include "gnc-ui-util.h"
#include "dialog-utils.h"

/* This static indicates the debugging module that this .o belongs to. */
static short module = MOD_REGISTER;


QuickFill *
gnc_quickfill_get_string_match_mb (QuickFill *qf, const char *str)
{
  GdkWChar *wc_text;

  if (NULL == qf) return NULL;
  if (NULL == str) return NULL;

  /* The match routines all expect wide-chars. */
  if (gnc_mbstowcs (&wc_text, str) < 0)
  {
    PERR ("bad text conversion");
    return NULL;
  }
  return gnc_quickfill_get_string_len_match (qf, wc_text, gnc_wcslen (wc_text));
}

/* ===================================================================== */
/* In order to speed up register starts for registers htat have a huge
 * number of accounts in them (where 'huge' is >500) we build a quickfill
 * cache of account names.  This cache is needed because some users on 
 * some machines experience register open times in the tens of seconds
 * type timescales.  Building the quickfill list accounts for almost
 * all of that cpu time (about 90% of the xfer_cell build time for 600 
 * accounts).
 */

typedef struct {
  QuickFill *qf;
  GNCBook *book;
  gint  listener;
  AccountBoolCB dont_add_cb;
  gpointer dont_add_data;
} QFB;

static void 
shared_quickfill_destroy (GNCBook *book, gpointer user_data)
{
  QFB *qfb = user_data;
  gnc_quickfill_destroy (qfb->qf);
  gnc_engine_unregister_event_handler (qfb->listener);
  g_free (qfb);
}

/* Since we are maintaining a 'global' quickfill list, we need to 
 * update it whenever the user creates a new account.  So listen
 * for account modification events, and add new accounts.
 */

static void
listen_for_account_events (GUID *guid, 
                           GNCEngineEventType event_type, 
                           gpointer user_data)
{
  QFB *qfb = user_data;
  QuickFill *qf = qfb->qf;
  QuickFill *match;
  char * name;
  const char *match_str;
  Account *account;
  GNCIdType type;

  if (! (event_type & GNC_EVENT_MODIFY)) return;
  type = xaccGUIDType (guid, qfb->book);
  if (safe_strcmp (type, GNC_ID_ACCOUNT)) return;

  account = xaccAccountLookup (guid, qfb->book);

  /* Not every new account is eligable for the menu */
  if (qfb->dont_add_cb)
  {
     gboolean skip = (qfb->dont_add_cb) (account, qfb->dont_add_data);
     if (skip) return;
  }

  name = xaccAccountGetFullName (account, gnc_get_account_separator ());
  if (NULL == name) return;

  match = gnc_quickfill_get_string_match_mb (qf, name);
  if (!match) goto add_string;
  match_str = gnc_quickfill_string (match);
  if (!match_str) goto add_string;
  if (safe_strcmp (match_str, name)) goto add_string;

  PINFO ("got match for %s", name);
  goto done;

add_string:
  PINFO ("insert new account %s into qf=%p\n", name, qf);
  gnc_quickfill_insert (qf, name, QUICKFILL_ALPHA);
done:
  g_free(name);
}

/* Splat the account name into the shared quickfill object */
static gpointer
load_shared_qf_cb (Account *account, gpointer data)
{
  QFB *qfb = data;
  char *name;

  if (qfb->dont_add_cb)
  {
     gboolean skip = (qfb->dont_add_cb) (account, qfb->dont_add_data);
     if (skip) return NULL;
  }

  name = xaccAccountGetFullName (account, gnc_get_account_separator ());
  if (NULL == name) return NULL;
  gnc_quickfill_insert (qfb->qf, name, QUICKFILL_ALPHA);
  g_free(name);

  return NULL;
}

/* Build the quickfill list out of account names. 
 * Essentially same loop as in gnc_load_xfer_cell() above.
 */
static QuickFill *
build_shared_quickfill (GNCBook *book, AccountGroup *group, 
                        const char * key,
                        AccountBoolCB cb, gpointer data)
{
  QFB *qfb;

  qfb = g_new0(QFB, 1);
  qfb->qf = gnc_quickfill_new ();
  qfb->book = book;
  qfb->listener = 0;
  qfb->dont_add_cb = cb;
  qfb->dont_add_data = data;

  xaccGroupForEachAccount (group, load_shared_qf_cb, qfb, TRUE);

  qfb->listener = 
     gnc_engine_register_event_handler (listen_for_account_events, qfb);

  gnc_book_set_shared_quickfill_hack (book, qfb, shared_quickfill_destroy);

  return qfb->qf;
}

QuickFill *
gnc_get_shared_account_name_quickfill (AccountGroup *group, const char * key,
                                       AccountBoolCB cb, gpointer cb_data)
{
  QFB *qfb;
  GNCBook *book;

  book = xaccGroupGetBook (group);
  qfb = gnc_book_get_shared_quickfill_hack (book);

  if (qfb) return qfb->qf;

  return build_shared_quickfill (book, group, key, cb, cb_data);
}

/* ====================== END OF FILE ================================== */
