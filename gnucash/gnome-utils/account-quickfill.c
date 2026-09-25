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

#include <config.h>
#include "account-quickfill.h"
#include "gnc-engine.h"
#include "gnc-prefs.h"
#include "gnc-ui-util.h"

static void shared_quickfill_pref_changed (gpointer prefs, gchar* pref,
                                           gpointer qfb);
static void listen_for_account_events (QofInstance* entity,
                                       QofEventId event_type,
                                       gpointer user_data, gpointer event_data);

struct _GncAccountListItem
{
    GObject parent_instance;
    Account *account;
    gchar *name;
};

G_DEFINE_FINAL_TYPE (GncAccountListItem, gnc_account_list_item, G_TYPE_OBJECT)

static void
gnc_account_list_item_finalize (GObject *object)
{
    GncAccountListItem *item = GNC_ACCOUNT_LIST_ITEM (object);

    g_free (item->name);
    G_OBJECT_CLASS (gnc_account_list_item_parent_class)->finalize (object);
}

static void
gnc_account_list_item_class_init (GncAccountListItemClass *klass)
{
    G_OBJECT_CLASS (klass)->finalize = gnc_account_list_item_finalize;
}

static void
gnc_account_list_item_init (GncAccountListItem *item)
{
    (void)item;
}

static GncAccountListItem*
gnc_account_list_item_new (Account *account, const gchar *name)
{
    GncAccountListItem *item = g_object_new (GNC_TYPE_ACCOUNT_LIST_ITEM, NULL);

    item->account = account;
    item->name = g_strdup (name);
    return item;
}

Account*
gnc_account_list_item_get_account (GncAccountListItem *item)
{
    g_return_val_if_fail (GNC_IS_ACCOUNT_LIST_ITEM (item), NULL);
    return item->account;
}

const gchar*
gnc_account_list_item_get_name (GncAccountListItem *item)
{
    g_return_val_if_fail (GNC_IS_ACCOUNT_LIST_ITEM (item), NULL);
    return item->name;
}

/* ===================================================================== */
/* In order to speed up register starts for registers that have a huge
 * number of accounts in them (where 'huge' is >500) we build a quickfill
 * cache of account names.  This cache is needed because some users on
 * some machines experience register open times in the tens of seconds
 * type timescales.  Building the quickfill list accounts for almost
 * all of that cpu time (about 90% of the xfer_cell build time for 600
 * accounts).
 */

typedef struct
{
    QuickFill* qf;
    GListStore* account_list;
    Account* root;
    gint  listener;
    AccountBoolCB dont_add_cb;
    gpointer dont_add_data;
} QFB;

static void
shared_quickfill_destroy (QofBook* book, gpointer key, gpointer user_data)
{
    QFB* qfb = user_data;

    (void)book;
    (void)key;
    gnc_prefs_remove_cb_by_func (GNC_PREFS_GROUP_GENERAL,
                                 GNC_PREF_ACCOUNT_SEPARATOR,
                                 shared_quickfill_pref_changed,
                                 qfb);
    gnc_prefs_remove_cb_by_func (GNC_PREFS_GROUP_GENERAL_REGISTER,
                                 GNC_PREF_SHOW_LEAF_ACCT_NAMES,
                                 shared_quickfill_pref_changed,
                                 qfb);
    gnc_quickfill_destroy (qfb->qf);
    g_object_unref (qfb->account_list);
    qof_event_unregister_handler (qfb->listener);
    g_free (qfb);
}


/* Build QuickFill and the visible GTK4 model from one account traversal. */
typedef struct
{
    QFB *qfb;
    GPtrArray *items;
} AccountCacheLoadData;

static void
load_shared_account_cache_cb (Account *account, gpointer user_data)
{
    AccountCacheLoadData *data = user_data;
    QFB *qfb = data->qfb;
    GncAccountListItem *item;
    gchar *name;

    if (qfb->dont_add_cb && qfb->dont_add_cb (account, qfb->dont_add_data))
        return;

    name = gnc_get_account_name_for_register (account);
    if (!name)
        return;

    gnc_quickfill_insert (qfb->qf, name, QUICKFILL_ALPHA);
    item = gnc_account_list_item_new (account, name);
    g_ptr_array_add (data->items, item);
    g_free (name);
}

static void
shared_quickfill_reload (QFB *qfb)
{
    AccountCacheLoadData data = { 0 };
    guint old_length;

    g_return_if_fail (qfb != NULL);

    data.qfb = qfb;
    data.items = g_ptr_array_new_with_free_func (g_object_unref);
    gnc_quickfill_purge (qfb->qf);
    gnc_account_foreach_descendant (qfb->root, load_shared_account_cache_cb,
                                    &data);

    old_length = g_list_model_get_n_items (G_LIST_MODEL (qfb->account_list));
    g_list_store_splice (qfb->account_list, 0, old_length,
                         (gpointer *)data.items->pdata, data.items->len);
    g_ptr_array_unref (data.items);
}

static void
shared_quickfill_pref_changed (gpointer prefs, gchar* pref, gpointer user_data)
{
    QFB* qfb = user_data;

    (void)prefs;
    (void)pref;

    shared_quickfill_reload (qfb);
}
/* Build the quickfill list out of account names.
 * Essentially same loop as in gnc_load_xfer_cell() above.
 */
static QFB*
build_shared_quickfill (QofBook* book, Account* root, const char* key,
                        AccountBoolCB cb, gpointer data)
{
    QFB* qfb;

    qfb = g_new0 (QFB, 1);
    qfb->qf = gnc_quickfill_new();
    qfb->root = root;
    qfb->listener = 0;
    qfb->dont_add_cb = cb;
    qfb->dont_add_data = data;
    qfb->account_list = g_list_store_new (GNC_TYPE_ACCOUNT_LIST_ITEM);

    gnc_prefs_register_cb (GNC_PREFS_GROUP_GENERAL,
                           GNC_PREF_ACCOUNT_SEPARATOR,
                           shared_quickfill_pref_changed,
                           qfb);

    gnc_prefs_register_cb (GNC_PREFS_GROUP_GENERAL_REGISTER,
                           GNC_PREF_SHOW_LEAF_ACCT_NAMES,
                           shared_quickfill_pref_changed,
                           qfb);

    shared_quickfill_reload (qfb);

    qfb->listener = qof_event_register_handler (listen_for_account_events, qfb);

    qof_book_set_data_fin (book, key, qfb, shared_quickfill_destroy);

    return qfb;
}

QuickFill*
gnc_get_shared_account_name_quickfill (Account* root, const char* key,
                                       AccountBoolCB cb, gpointer cb_data)
{
    QFB* qfb;
    QofBook* book;

    book = gnc_account_get_book (root);
    qfb = qof_book_get_data (book, key);

    if (qfb)
        return qfb->qf;

    qfb = build_shared_quickfill (book, root, key, cb, cb_data);
    return qfb->qf;
}

GListModel*
gnc_get_shared_account_name_list_model (Account *root, const char *key,
                                        AccountBoolCB cb, gpointer cb_data)
{
    QFB *qfb;
    QofBook *book;

    book = gnc_account_get_book (root);
    qfb = qof_book_get_data (book, key);

    if (!qfb)
        qfb = build_shared_quickfill (book, root, key, cb, cb_data);

    return G_LIST_MODEL (qfb->account_list);
}

/* Since we are maintaining a 'global' quickfill list, we need to
 * update it whenever the user creates a new account.  So listen
 * for account modification events, and add new accounts.
 */
static void
listen_for_account_events (QofInstance* entity, QofEventId event_type,
                           gpointer user_data, gpointer event_data)
{
    QFB* qfb = user_data;
    Account* account;

    (void)event_data;
    if (!(event_type & (QOF_EVENT_MODIFY | QOF_EVENT_ADD | QOF_EVENT_REMOVE)))
        return;

    if (!GNC_IS_ACCOUNT (entity))
        return;

    account = GNC_ACCOUNT (entity);
    if (gnc_account_get_root (account) != qfb->root)
        return;

    /* Account events alter display names, visibility and the QuickFill tree.
     * Rebuild both derived representations in one traversal so they cannot
     * diverge while a register keeps the shared model open. */
    shared_quickfill_reload (qfb);
}

/* ====================== END OF FILE ================================== */
