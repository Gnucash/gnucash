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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

#include "config.h"
#include "account-quickfill.h"
#include "gnc-gconf-utils.h"
#include "gnc-engine.h"
#include "gnc-ui-util.h"

/* This static indicates the debugging module that this .o belongs to. */
static QofLogModule log_module = GNC_MOD_REGISTER;

static void shared_quickfill_gconf_changed (GConfEntry *entry, gpointer qfb);

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
  GtkListStore *list_store;
  QofBook *book;
  AccountGroup *group;
  gint  listener;
  AccountBoolCB dont_add_cb;
  gpointer dont_add_data;
} QFB;

static void 
shared_quickfill_destroy (QofBook *book, gpointer key, gpointer user_data)
{
  QFB *qfb = user_data;
  gnc_gconf_general_remove_cb(KEY_ACCOUNT_SEPARATOR,
			      shared_quickfill_gconf_changed,
			      qfb);
  gnc_quickfill_destroy (qfb->qf);
  g_object_unref(qfb->list_store);
  qof_event_unregister_handler (qfb->listener);
  g_free (qfb);
}

/* Since we are maintaining a 'global' quickfill list, we need to 
 * update it whenever the user creates a new account.  So listen
 * for account modification events, and add new accounts.
 */
static void
listen_for_account_events (GUID *guid, QofIdType type, 
                           QofEventId event_type, 
                           gpointer user_data)
{
  QFB *qfb = user_data;
  QuickFill *qf = qfb->qf;
  QuickFill *match;
  char * name;
  const char *match_str;
  QofCollection *col;
  Account *account;
  GtkTreeIter iter;

  if (! (event_type & QOF_EVENT_MODIFY)) return;
  if (QSTRCMP (type, GNC_ID_ACCOUNT)) return;

  col = qof_book_get_collection (qfb->book, GNC_ID_ACCOUNT);
  account = GNC_ACCOUNT (qof_collection_lookup_entity (col, guid));

  /* Not every new account is eligable for the menu */
  if (qfb->dont_add_cb)
  {
     gboolean skip = (qfb->dont_add_cb) (account, qfb->dont_add_data);
     if (skip) return;
  }

  name = xaccAccountGetFullName (account);
  if (NULL == name) return;

  match = gnc_quickfill_get_string_match (qf, name);
  if (!match) goto add_string;
  match_str = gnc_quickfill_string (match);
  if (!match_str) goto add_string;
  if (safe_strcmp (match_str, name)) goto add_string;

  PINFO ("got match for %s", name);
  goto done;

add_string:
  PINFO ("insert new account %s into qf=%p\n", name, qf);
  gnc_quickfill_insert (qf, name, QUICKFILL_ALPHA);
  gtk_list_store_append (qfb->list_store, &iter);
  gtk_list_store_set (qfb->list_store, &iter, 0, name, -1);
done:
  g_free(name);
}

/* Splat the account name into the shared quickfill object */
static gpointer
load_shared_qf_cb (Account *account, gpointer data)
{
  QFB *qfb = data;
  char *name;
  GtkTreeIter iter;

  if (qfb->dont_add_cb)
  {
     gboolean skip = (qfb->dont_add_cb) (account, qfb->dont_add_data);
     if (skip) return NULL;
  }

  name = xaccAccountGetFullName (account);
  if (NULL == name) return NULL;
  gnc_quickfill_insert (qfb->qf, name, QUICKFILL_ALPHA);
  gtk_list_store_append (qfb->list_store, &iter);
  gtk_list_store_set (qfb->list_store, &iter, 0, name, -1);
  g_free(name);

  return NULL;
}

static void
shared_quickfill_gconf_changed (GConfEntry *entry, gpointer user_data)
{
  QFB *qfb = user_data;

  /* Reload the quickfill */
  gnc_quickfill_purge(qfb->qf);
  xaccGroupForEachAccount (qfb->group, load_shared_qf_cb, qfb, TRUE);
}


/* Build the quickfill list out of account names. 
 * Essentially same loop as in gnc_load_xfer_cell() above.
 */
static QFB *
build_shared_quickfill (QofBook *book, AccountGroup *group, const char * key,
                        AccountBoolCB cb, gpointer data)
{
  QFB *qfb;

  qfb = g_new0(QFB, 1);
  qfb->qf = gnc_quickfill_new ();
  qfb->book = book;
  qfb->group = group;
  qfb->listener = 0;
  qfb->dont_add_cb = cb;
  qfb->dont_add_data = data;
  qfb->list_store = gtk_list_store_new (1, G_TYPE_STRING);

  gnc_gconf_general_register_cb(KEY_ACCOUNT_SEPARATOR,
				shared_quickfill_gconf_changed,
				qfb);

  xaccGroupForEachAccount (group, load_shared_qf_cb, qfb, TRUE);

  qfb->listener = 
     qof_event_register_old_handler (listen_for_account_events, qfb);

  qof_book_set_data_fin (book, key, qfb, shared_quickfill_destroy);

  return qfb;
}

QuickFill *
gnc_get_shared_account_name_quickfill (AccountGroup *group, 
                                       const char * key, 
                                       AccountBoolCB cb, gpointer cb_data)
{
  QFB *qfb;
  QofBook *book;

  book = xaccGroupGetBook (group);
  qfb = qof_book_get_data (book, key);

  if (qfb) return qfb->qf;

  qfb = build_shared_quickfill (book, group, key, cb, cb_data);
  return qfb->qf;
}

GtkListStore *
gnc_get_shared_account_name_list_store (AccountGroup *group, 
					const char * key, 
					AccountBoolCB cb, gpointer cb_data)
{
  QFB *qfb;
  QofBook *book;

  book = xaccGroupGetBook (group);
  qfb = qof_book_get_data (book, key);

  if (qfb) return qfb->list_store;

  qfb = build_shared_quickfill (book, group, key, cb, cb_data);
  return qfb->list_store;
}

/* ====================== END OF FILE ================================== */
