/*
  TODO: remove bsplit_node, bsplit_parent_node: would that really be simpler?

*/

/********************************************************************\
 * gnc-tree-model-transaction.c -- GtkTreeModel implementation to   *
 *                        display Transactions in a GtkTreeView.    *
 * Copyright (C) 2006-2007 Chris Shoemaker <c.shoemaker@cox.net>    *
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
#include <string.h>
#include <glib/gi18n.h>

#include "gnc-tree-model-transaction.h"
#include "gnc-component-manager.h"
#include "Transaction.h"
#include "TransactionP.h" //what a shame
#include "Scrub.h"
#include "gnc-commodity.h"
//#include "gnc-engine-util.h"
#include "gnc-gobject-utils.h"
#include "gnc-ui-util.h"
#include "gnc-event.h"

/** Static Globals *******************************************************/
static QofLogModule log_module = GNC_MOD_GUI;

struct GncTreeModelTransactionPrivate
{
    QofBook *book;
    Query *query;
    Account *anchor;
    gboolean include_subacc;

    GList *tlist;

    Transaction *btrans;
    Split *bsplit;
    GList *bsplit_node;  /* never added to any list, just for
                            representation of the iter */
    GList *bsplit_parent_node;

    gint event_handler_id;
};

#define TREE_MODEL_TRANSACTION_CM_CLASS "tree-model-transactions"
#define BLANK 0x2
#define SPLIT 0x1
#define IS_BLANK(x) (GPOINTER_TO_INT((x)->user_data) & BLANK)
#define IS_SPLIT(x) (GPOINTER_TO_INT((x)->user_data) & SPLIT)
#define IS_BLANK_SPLIT(x) (IS_BLANK(x) && IS_SPLIT(x))
#define IS_BLANK_TRANS(x) (IS_BLANK(x) && !IS_SPLIT(x))

/* Meaning of user_data fields in iter struct:
 *
 * user_data:  a bitfield for BLANK, SPLIT
 * user_data2: a pointer to a node in a GList of Transactions
 *            if this is a Split, then this points to the GList node of the
 *            parent transaction
 * user_data3: a pointer to a node in a GList of Splits
 *            NULL if this is a transaction.
 */
#define VALID_ITER(model, iter) \
    (GNC_IS_TREE_MODEL_TRANSACTION(model) &&                            \
     ((iter) && (iter)->user_data2) &&                                  \
     ((iter)->stamp == (model)->stamp) &&                               \
     (!IS_SPLIT(iter) ^ ((iter)->user_data3 != NULL)) &&                \
     (!IS_BLANK_SPLIT(iter) ||                                          \
      ((iter)->user_data2 == (model)->priv->bsplit_parent_node))        \
     )

static GtkTreeIter
make_iter(GncTreeModelTransaction *model, gint f, GList *tnode, GList *snode)
{
    GtkTreeIter iter, *iter_p;
    iter_p = &iter;
    iter.stamp = model->stamp;
    iter.user_data = GINT_TO_POINTER(f);
    iter.user_data2 = tnode;
    iter.user_data3 = snode;
    if (!VALID_ITER(model, &iter)) PERR("Making invalid iter");
    return iter;
}

/** Declarations *********************************************************/
static void gnc_tree_model_transaction_class_init (
    GncTreeModelTransactionClass *klass);
static void gnc_tree_model_transaction_init (GncTreeModelTransaction *model);
static void gnc_tree_model_transaction_finalize (GObject *object);

/** Implementation of GtkTreeModel  **************************************/
static void gnc_tree_model_transaction_tree_model_init (
    GtkTreeModelIface *iface);
static guint gnc_tree_model_transaction_get_flags (GtkTreeModel *model);
static int gnc_tree_model_transaction_get_n_columns (GtkTreeModel *model);
static GType gnc_tree_model_transaction_get_column_type (
    GtkTreeModel *model, int index);
static gboolean gnc_tree_model_transaction_get_iter (
    GtkTreeModel *model, GtkTreeIter *iter, GtkTreePath *path);
static GtkTreePath *gnc_tree_model_transaction_get_path (
    GtkTreeModel *model, GtkTreeIter *iter);
static void gnc_tree_model_transaction_get_value (
    GtkTreeModel *model, GtkTreeIter *iter, int column, GValue *value);
static gboolean	gnc_tree_model_transaction_iter_next (
    GtkTreeModel *model, GtkTreeIter *iter);
static gboolean	gnc_tree_model_transaction_iter_children (
    GtkTreeModel *model, GtkTreeIter *iter, GtkTreeIter *parent);
static gboolean	gnc_tree_model_transaction_iter_has_child (
    GtkTreeModel *model, GtkTreeIter *iter);
static int gnc_tree_model_transaction_iter_n_children (
    GtkTreeModel *model, GtkTreeIter *iter);
static gboolean	gnc_tree_model_transaction_iter_nth_child (
    GtkTreeModel *model, GtkTreeIter *iter, GtkTreeIter *parent, int n);
static gboolean	gnc_tree_model_transaction_iter_parent (
    GtkTreeModel *model, GtkTreeIter *iter, GtkTreeIter *child);

/** Helper Functions ****************************************************/

static void gnc_tree_model_transaction_event_handler(
    QofInstance *entity, QofEventId event_type, gpointer tm, gpointer event_data);

/************************************************************/
/*               g_object required functions                */
/************************************************************/

static GtkObjectClass *parent_class = NULL;

GType
gnc_tree_model_transaction_get_type (void)
{
    static GType gnc_tree_model_transaction_type = 0;

    if (gnc_tree_model_transaction_type == 0) {
        static const GTypeInfo our_info = {
            sizeof (GncTreeModelTransactionClass), /* class_size */
            NULL,   			           /* base_init */
            NULL,                                  /* base_finalize */
            (GClassInitFunc) gnc_tree_model_transaction_class_init,
            NULL,                                  /* class_finalize */
            NULL,                                  /* class_data */
            sizeof (GncTreeModelTransaction),      /* */
            0,                                     /* n_preallocs */
            (GInstanceInitFunc) gnc_tree_model_transaction_init
        };

        static const GInterfaceInfo tree_model_info = {
            (GInterfaceInitFunc) gnc_tree_model_transaction_tree_model_init,
            NULL,
            NULL
        };

        gnc_tree_model_transaction_type = g_type_register_static (
            GNC_TYPE_TREE_MODEL, GNC_TREE_MODEL_TRANSACTION_NAME,
            &our_info, 0);

        g_type_add_interface_static (gnc_tree_model_transaction_type,
                                     GTK_TYPE_TREE_MODEL, &tree_model_info);
    }

    return gnc_tree_model_transaction_type;
}

static void
gnc_tree_model_transaction_class_init (GncTreeModelTransactionClass *klass)
{
    GObjectClass *o_class;

    parent_class = g_type_class_peek_parent (klass);

    /* GObject signals */
    o_class = G_OBJECT_CLASS (klass);
    o_class->finalize = gnc_tree_model_transaction_finalize;
}

static void
gnc_tree_model_transaction_init (GncTreeModelTransaction *model)
{
    ENTER("model %p", model);
    while (model->stamp == 0) {
        model->stamp = g_random_int ();
    }

    model->priv = g_new0 (GncTreeModelTransactionPrivate, 1);
    LEAVE(" ");
}

static void
gnc_tree_model_transaction_finalize (GObject *object)
{
    GncTreeModelTransaction *model;
    GncTreeModelTransactionPrivate *priv;

    ENTER("model %p", object);
    g_return_if_fail (object != NULL);
    g_return_if_fail (GNC_IS_TREE_MODEL_TRANSACTION (object));

    model = GNC_TREE_MODEL_TRANSACTION (object);
    priv = model->priv;

    if (priv->event_handler_id) {
        qof_event_unregister_handler(priv->event_handler_id);
        priv->event_handler_id = 0;
    }

    priv->book = NULL;
    g_list_free(priv->tlist);
    priv->tlist = NULL;
    xaccFreeQuery(priv->query);
    priv->query = NULL;
    if (priv->bsplit && !xaccSplitGetParent(priv->bsplit))
        ; //LEAK? xaccFreeSplit(priv->bsplit);
    priv->bsplit = NULL;
    priv->bsplit_node = NULL;
    priv->bsplit_parent_node = NULL;

    //LEAK?: xaccFreeTransaction(priv->btrans);
    priv->btrans = NULL;

    g_free(priv);

    if (G_OBJECT_CLASS (parent_class)->finalize)
        G_OBJECT_CLASS (parent_class)->finalize(object);
    LEAVE(" ");
}

/************************************************************/
/*                   New Model Creation                     */
/************************************************************/
static GncTreeModelTransaction *
gnc_tree_model_transaction_new(GList *tlist)
{
    GncTreeModelTransaction *model;
    GncTreeModelTransactionPrivate *priv;

    ENTER("");

    model = g_object_new(GNC_TYPE_TREE_MODEL_TRANSACTION, NULL);

    priv = model->priv;
    priv->book = gnc_get_current_book();
    priv->tlist = tlist;

    priv->bsplit = xaccMallocSplit(priv->book);
    priv->bsplit_node = g_list_append(NULL, priv->bsplit);
    priv->btrans = xaccMallocTransaction(priv->book);
    priv->tlist = g_list_append(priv->tlist, priv->btrans);

    priv->event_handler_id = qof_event_register_handler(
        gnc_tree_model_transaction_event_handler, model);
    LEAVE("model %p", model);
    return model;
}


GncTreeModelTransaction *
gnc_tree_model_transaction_new_from_query(Query *query)
{
    GncTreeModelTransaction *model;
    GList *tlist;

    tlist = xaccQueryGetTransactions(query, QUERY_TXN_MATCH_ANY);
    model = gnc_tree_model_transaction_new(tlist);
    model->priv->query = query;
    return model;
}

GncTreeModelTransaction *
gnc_tree_model_transaction_new_from_account(Account *acc)
{
    GncTreeModelTransaction *model;
    GList *tlist, *slist;

    slist = xaccAccountGetSplitList(acc);
    tlist = xaccSplitListGetUniqueTransactions(slist);
    model = gnc_tree_model_transaction_new(tlist);
    model->priv->anchor = acc;
    return model;
}

/************************************************************/
/*        Gnc Tree Model Debugging Utility Function         */
/************************************************************/

#define ITER_STRING_LEN 128

static const gchar *
iter_to_string(GtkTreeIter *iter)
{
#ifdef G_THREADS_ENABLED
    static GStaticPrivate gtmits_buffer_key = G_STATIC_PRIVATE_INIT;
    gchar *string;

    string = g_static_private_get (&gtmits_buffer_key);
    if (string == NULL) {
        string = g_malloc(ITER_STRING_LEN + 1);
        g_static_private_set (&gtmits_buffer_key, string, g_free);
    }
#else
    static char string[ITER_STRING_LEN + 1];
#endif

    if (iter)
        snprintf(
            string, ITER_STRING_LEN,
            "[stamp:%x data:%d, %p (%p:%s), %p]",
            iter->stamp, GPOINTER_TO_INT(iter->user_data),
            iter->user_data2,
            iter->user_data2 ? ((GList *) iter->user_data2)->data : 0,
            iter->user_data2 ?
            (QOF_INSTANCE(((GList *) iter->user_data2)->data))->e_type : "",
            iter->user_data3);
    else
        strcpy(string, "(null)");
    return string;
}


/************************************************************/
/*       Gtk Tree Model Required Interface Functions        */
/************************************************************/

static void
gnc_tree_model_transaction_tree_model_init (GtkTreeModelIface *iface)
{
    iface->get_flags       = gnc_tree_model_transaction_get_flags;
    iface->get_n_columns   = gnc_tree_model_transaction_get_n_columns;
    iface->get_column_type = gnc_tree_model_transaction_get_column_type;
    iface->get_iter        = gnc_tree_model_transaction_get_iter;
    iface->get_path        = gnc_tree_model_transaction_get_path;
    iface->get_value       = gnc_tree_model_transaction_get_value;
    iface->iter_next       = gnc_tree_model_transaction_iter_next;
    iface->iter_children   = gnc_tree_model_transaction_iter_children;
    iface->iter_has_child  = gnc_tree_model_transaction_iter_has_child;
    iface->iter_n_children = gnc_tree_model_transaction_iter_n_children;
    iface->iter_nth_child  = gnc_tree_model_transaction_iter_nth_child;
    iface->iter_parent     = gnc_tree_model_transaction_iter_parent;
}

static GtkTreeModelFlags
gnc_tree_model_transaction_get_flags (GtkTreeModel *tm)
{
    return 0;
}

static int
gnc_tree_model_transaction_get_n_columns (GtkTreeModel *tm)
{
    g_return_val_if_fail(GNC_IS_TREE_MODEL_TRANSACTION(tm), -1);
    return GNC_TREE_MODEL_TRANSACTION_NUM_COLUMNS;
}

static GType
gnc_tree_model_transaction_get_column_type (GtkTreeModel *tm, int index)
{
    g_return_val_if_fail (GNC_IS_TREE_MODEL_TRANSACTION (tm),
                          G_TYPE_INVALID);
    g_return_val_if_fail ((index < GNC_TREE_MODEL_TRANSACTION_NUM_COLUMNS) &&
                          (index >= 0), G_TYPE_INVALID);

    switch (index) {
    case GNC_TREE_MODEL_TRANSACTION_COL_GUID:
        return G_TYPE_POINTER;

    case GNC_TREE_MODEL_TRANSACTION_COL_DATE:
        return G_TYPE_ULONG;

    case GNC_TREE_MODEL_TRANSACTION_COL_NUM:
    case GNC_TREE_MODEL_TRANSACTION_COL_DESCRIPTION:
        return G_TYPE_STRING;

    default:
        g_assert_not_reached();
        return G_TYPE_INVALID;
    }
}

static void
gnc_tree_model_transaction_get_value (GtkTreeModel *tm, GtkTreeIter *iter,
                                      int column, GValue *value)
{
    GncTreeModelTransaction *model = GNC_TREE_MODEL_TRANSACTION (tm);
    Split *split = NULL;
    Transaction *trans;
    gboolean is_split, is_blank;
    const GUID * guid;
    GList *node;

    DEBUG("model %p, iter %s, col %d", tm, iter_to_string(iter), column);
    g_assert(VALID_ITER(model, iter));

    is_split = IS_SPLIT(iter);
    is_blank = IS_BLANK(iter);
    node = (GList *) iter->user_data2;
    trans = (Transaction *) node->data;
    if (is_split) {
        node = (GList *) iter->user_data3;
        split = (Split *) node->data;
    }

    g_value_init(value,
                 gnc_tree_model_transaction_get_column_type(tm, column));
    switch (column) {
    case GNC_TREE_MODEL_TRANSACTION_COL_GUID:
        if (is_split)
            guid = qof_entity_get_guid(QOF_INSTANCE(split));
        else
            guid = qof_entity_get_guid(QOF_INSTANCE(trans));
        g_value_set_pointer(value, (gpointer) guid);
        break;
    case GNC_TREE_MODEL_TRANSACTION_COL_DATE:
        if (is_split)
            g_value_set_ulong(value, 0);
        else {
            gulong i = (gulong) xaccTransGetDate(trans);
            if (is_blank && i == 0)
                g_value_set_ulong(value, time(NULL));
            else
                g_value_set_ulong(value, i);
        }
        break;
        /*
    case GNC_TREE_MODEL_TRANSACTION_COL_REC:
        g_value_set_string(value, is_split ? xaccSplitGetReconcile(split) :
                           "");
        break;
        */
    case GNC_TREE_MODEL_TRANSACTION_COL_NUM:
        g_value_set_string(value, is_split ? xaccSplitGetAction(split) :
                           xaccTransGetNum(trans));
        break;
    case GNC_TREE_MODEL_TRANSACTION_COL_DESCRIPTION:
        g_value_set_string(value, is_split ? xaccSplitGetMemo(split) :
                           xaccTransGetDescription(trans));
        break;
    default:
        g_assert_not_reached ();
    }
}

static gboolean
gnc_tree_model_transaction_get_iter(GtkTreeModel *tm, GtkTreeIter *iter,
                                    GtkTreePath *path)
{
    GncTreeModelTransaction *model = GNC_TREE_MODEL_TRANSACTION (tm);
    GList *tnode, *snode, *slist;
    gint depth, *indices, flags;

    g_return_val_if_fail (GNC_IS_TREE_MODEL_TRANSACTION (tm), FALSE);
    DEBUG("model %p, path %s", tm, gtk_tree_path_to_string(path));

    depth = gtk_tree_path_get_depth(path);
    indices = gtk_tree_path_get_indices (path);
    tnode = g_list_nth(model->priv->tlist, indices[0]);
    if (!tnode) {
        DEBUG("path index off end of list");
        goto fail;
    }

    if (depth == 1) { /* Trans */
        snode = NULL;
        /* Check if this is the blank trans */
        if (tnode->data == model->priv->btrans)
            flags = BLANK;
        else flags = 0;
    } else if (depth == 2) {  /* Split */
        Split *split = xaccTransGetSplit(tnode->data, indices[1]);

        slist = xaccTransGetSplitList(tnode->data);
        snode = g_list_find(slist, split);

        flags = SPLIT;

        if (!snode && tnode == model->priv->bsplit_parent_node &&
            xaccTransCountSplits(tnode->data) == indices[1])
            snode = model->priv->bsplit_node;

        if (!snode) goto fail;

        if (snode->data == model->priv->bsplit_node->data)
            flags |= BLANK;

        if (!snode) {
                PERR("Invalid path index: %d", indices[1]);
                goto fail;
        }
    } else {
        DEBUG("Invalid path depth");
        goto fail;
    }

    *iter = make_iter(model, flags, tnode, snode);
    g_assert(VALID_ITER(model, iter));
    return TRUE;
 fail:
    iter->stamp = 0;
    return FALSE;
}

static GtkTreePath *
gnc_tree_model_transaction_get_path (GtkTreeModel *tm, GtkTreeIter *iter)
{
    GncTreeModelTransaction *model = GNC_TREE_MODEL_TRANSACTION (tm);
    GtkTreePath *path;
    gint pos;
    GList *tnode;

    g_assert(VALID_ITER(model, iter));
    path = gtk_tree_path_new();
    tnode = iter->user_data2;

    /* This works fine for the blank trans, too. */
    pos = g_list_position(model->priv->tlist, tnode);
    if (pos == -1)
        goto fail;
    gtk_tree_path_append_index(path, pos);

    if (IS_SPLIT(iter)) {
        Transaction *trans;
        GList *slist;
        Split *split;

        trans = (Transaction *) tnode->data;
        slist = xaccTransGetSplitList(trans);
        split = ((GList*)iter->user_data3)->data;
        pos = xaccTransGetSplitIndex(trans, split);
        if (pos == -1) {
            if (IS_BLANK(iter))
                pos = xaccTransCountSplits(trans);
            else
                goto fail;
        }
        gtk_tree_path_append_index(path, pos);
    }

    return path;
 fail:
    return NULL;
}

static gboolean
gnc_tree_model_transaction_iter_next (GtkTreeModel *tm, GtkTreeIter *iter)
{
    GncTreeModelTransaction *model = GNC_TREE_MODEL_TRANSACTION (tm);
    GList *snode, *tnode;
    gint flags = 0;

    ENTER("model %p, iter %s", tm, iter_to_string(iter));
    g_assert(VALID_ITER(model, iter));

    if (IS_BLANK(iter)) {
        LEAVE("Blanks _never_ have a next");
        goto fail;
    }

    tnode = iter->user_data2;
    if (IS_SPLIT(iter)) {
        flags = SPLIT;
        snode = iter->user_data3;
        do {
            snode = snode->next;
        } while (snode && !xaccTransStillHasSplit(tnode->data, snode->data));
        if (!snode) {
            if (model->priv->bsplit_parent_node == tnode) {
                snode = model->priv->bsplit_node;
                flags |= BLANK;
            } else {
                LEAVE("Last non-blank split has no next");
                goto fail;
            }
        }
    } else {
        flags = 0;
        snode = NULL;
        tnode = tnode->next;

        /* Check if this is the blank trans */
        if (!tnode) {
            LEAVE("last trans has no next");
            goto fail;
        } else if (tnode->data == model->priv->btrans)
            flags |= BLANK;

    }

    *iter = make_iter(model, flags, tnode, snode);
    LEAVE("iter %s", iter_to_string(iter));
    return TRUE;
 fail:
    iter->stamp = 0;
    return FALSE;
}

static gboolean
gnc_tree_model_transaction_iter_children (GtkTreeModel *tm, GtkTreeIter *iter,
                                          GtkTreeIter *parent)
{
    GncTreeModelTransaction *model = GNC_TREE_MODEL_TRANSACTION (tm);
    GList *tnode, *snode = NULL, *slist;
    gint flags = 0;
    Transaction *trans;
    Split *split;

    ENTER("model %p, iter %p (to be filed in), parent %s",
          tm, iter, iter_to_string(parent));

    g_return_val_if_fail (GNC_IS_TREE_MODEL_TRANSACTION (tm), FALSE);

    if (!parent) {
        /* Get the very first iter */
        tnode = model->priv->tlist;
        if (tnode) {
            if (tnode->data == model->priv->btrans)
                flags = BLANK;

            *iter = make_iter(model, flags, tnode, NULL);
            LEAVE("iter (2) %s", iter_to_string(iter));
            return TRUE;
        } else {
            PERR("We should never have a NULL trans list.");
            goto fail;
        }
    }

    g_assert(VALID_ITER(model, parent));

    if (IS_SPLIT(parent))
        goto fail;  /* Splits never have children */

    tnode = parent->user_data2;
    trans = tnode->data;
    slist = xaccTransGetSplitList(trans);
    split = xaccTransGetSplit(trans, 0);
    flags = SPLIT;
    if (model->priv->bsplit_parent_node == tnode && !split) {
        split = (Split *) ((GList *)model->priv->bsplit_node)->data;
    }
    if (split == (Split *) ((GList *)model->priv->bsplit_node)->data) {
        flags |= BLANK;
        snode = model->priv->bsplit_node;
    } else if (split)
        snode = g_list_find(slist, split);

    if (!snode)
        goto fail;

    *iter = make_iter(model, flags, tnode, snode);
    LEAVE("iter %s", iter_to_string(iter));
    return TRUE;
 fail:
    LEAVE("iter has no children");
    iter->stamp = 0;
    return FALSE;
}

static gboolean
gnc_tree_model_transaction_iter_has_child (GtkTreeModel *tm, GtkTreeIter *iter)
{
    GncTreeModelTransaction *model = GNC_TREE_MODEL_TRANSACTION (tm);
    GList *tnode;
    Transaction *trans;
    Split *split;

    g_assert(VALID_ITER(model, iter));
    ENTER("model %p, iter %s", tm, iter_to_string(iter));

    if (IS_SPLIT(iter)) {
        LEAVE(" splits have no children");
        return FALSE;
    }

    tnode = iter->user_data2;
    trans = tnode->data;
    if (!trans) {
        PERR(" The trans data should NEVER be NULL.");
        LEAVE(" trans data was NULL!");
        return FALSE;
    }
    split = xaccTransGetSplit(trans, 0);
    if (split || (model->priv->bsplit_parent_node == tnode)) {
        LEAVE(" yes");
        return TRUE;
    } else {
        LEAVE(" trans has no children");
        return FALSE;
    }
}

static int
gnc_tree_model_transaction_iter_n_children (GtkTreeModel *tm,
                                            GtkTreeIter *iter)
{
    GncTreeModelTransaction *model = GNC_TREE_MODEL_TRANSACTION (tm);
    Transaction *trans;
    GList *tnode;
    int i;

    ENTER("model %p, iter %s", tm, iter_to_string(iter));
    g_return_val_if_fail (GNC_IS_TREE_MODEL_TRANSACTION (tm), -1);

    if (iter == NULL) {
        i = g_list_length(model->priv->tlist);
        LEAVE("toplevel count is %d", i);
        return i;
    }

    g_assert(VALID_ITER(model, iter));

    if (IS_SPLIT(iter)) {
        LEAVE("iter has no children");
        return 0;
    }

    tnode = iter->user_data2;
    trans = tnode->data;
    i = xaccTransCountSplits(trans);
    if (model->priv->bsplit_parent_node == tnode)
        i++;

    LEAVE("iter has %d children", i);
    return i;
}

static gboolean
gnc_tree_model_transaction_iter_nth_child (GtkTreeModel *tm, GtkTreeIter *iter,
                                           GtkTreeIter *parent, int n)
{
    GncTreeModelTransaction *model = GNC_TREE_MODEL_TRANSACTION (tm);
    Transaction *trans;
    Split *split;
    GList *slist, *snode, *tnode;
    gint flags;

    ENTER("model %p, iter %s, n %d", tm, iter_to_string(iter), n);
    g_return_val_if_fail (GNC_IS_TREE_MODEL_TRANSACTION (tm), FALSE);

    if (parent == NULL) {  /* Top-level */
        tnode = g_list_nth(model->priv->tlist, n);

        if (!tnode) {
            PERR("Trans list should never be NULL.");
            goto fail;
        }
        if (tnode->data == model->priv->btrans)
            flags = BLANK;

        *iter = make_iter(model, flags, tnode, NULL);
        LEAVE("iter (2) %s", iter_to_string(iter));
        return TRUE;
    }

    DEBUG("parent iter %s", iter_to_string(parent));
    g_assert(VALID_ITER(model, parent));

    if (IS_SPLIT(parent))
        goto fail;  /* Splits have no children */

    flags = SPLIT;
    tnode = parent->user_data2;
    trans = tnode->data;
    split = xaccTransGetSplit(trans, n);
    slist = xaccTransGetSplitList(trans);
    snode = g_list_find(slist, split);
    if (!snode && model->priv->bsplit_parent_node == tnode &&
        n == xaccTransCountSplits(trans)) {
        snode = model->priv->bsplit_node;
    } else goto fail;

    if (snode->data == model->priv->bsplit_node->data)
        flags |= BLANK;
    *iter = make_iter(model, flags, tnode, snode);
    LEAVE("iter (3) %s", iter_to_string(iter));
    return TRUE;
 fail:
    iter->stamp = 0;
    return FALSE;
}

static gboolean
gnc_tree_model_transaction_iter_parent (GtkTreeModel *tm, GtkTreeIter *iter,
                                        GtkTreeIter *child)
{
    GncTreeModelTransaction *model = GNC_TREE_MODEL_TRANSACTION (tm);
    GList *tnode;
    gint flags = 0;

    ENTER("model %p, child %s", tm, iter_to_string(child));
    g_assert(VALID_ITER(model, child));

    /* Only splits have parents. */
    if (!IS_SPLIT(child)) goto fail;

    tnode = child->user_data2;
    if (tnode->data == model->priv->btrans)
        flags = BLANK;
    *iter = make_iter(model, flags, tnode, NULL);
    LEAVE("iter (3) %s", iter_to_string(iter));
    return TRUE;
 fail:
    iter->stamp = 0;
    return FALSE;
}

/******  End of GtkTreeModel Interface implementation *******/

static void
increment_stamp(GncTreeModelTransaction *model)
{
    do model->stamp++;
    while (model->stamp == 0);
}

static void
update_parent(GncTreeModelTransaction *model, GtkTreePath *path)
{
    GList *tnode;
    GtkTreeIter iter;

    if (gtk_tree_path_up(path) && gnc_tree_model_transaction_get_iter(
            GTK_TREE_MODEL(model), &iter, path)) {
        /* emit changed on the parent because balance may have changed */
        /* This has an undesired side-effect in the sort model.  The
           order of identical sort keys unfortunately changes when
           row_changed is emitted. */
        //What are the side-effects of commenting this out???
        //gtk_tree_model_row_changed(GTK_TREE_MODEL(model), path, &iter);
        tnode = iter.user_data2;

        /* Checkme: Isn't there a simpler condition to check for? */ 
        if (IS_BLANK_TRANS(&iter) && (tnode->data == model->priv->btrans) && 
            (xaccTransCountSplits(model->priv->btrans) == 0)) {
            increment_stamp(model);
            
            PINFO("toggling has_child at row %s\n",
                  gtk_tree_path_to_string(path));
            
            gtk_tree_model_row_has_child_toggled(GTK_TREE_MODEL(model),
                                                 path, &iter);
        }
    }
}

static void
insert_row_at(GncTreeModelTransaction *model, GtkTreeIter *iter)
{
    GtkTreePath *path;

    g_assert(VALID_ITER(model, iter));
    path = gnc_tree_model_transaction_get_path(GTK_TREE_MODEL(model), iter);
    if (!path) PERR("Null path");

    increment_stamp(model);
    if (gnc_tree_model_transaction_get_iter(
            GTK_TREE_MODEL(model), iter, path)) {
        gtk_tree_model_row_inserted(GTK_TREE_MODEL(model), path, iter);
    } else PERR("Tried to insert with invalid iter.");

    update_parent(model, path);
    gtk_tree_path_free(path);
}

static void
delete_row_at_path(GncTreeModelTransaction *model, GtkTreePath *path)
{
    gint depth;

    if (!path) PERR("Null path");
    increment_stamp(model);
    gtk_tree_model_row_deleted(GTK_TREE_MODEL(model), path);

    depth = gtk_tree_path_get_depth(path);
    if (depth == 2) {
        update_parent(model, path);
    } else {
        GtkTreeIter iter;
        if (gnc_tree_model_transaction_get_iter(
                GTK_TREE_MODEL(model), &iter, path)) { 
            GList *tnode = iter.user_data2;
            GncTreeModelTransactionPrivate *priv = model->priv;
            if (tnode == priv->bsplit_parent_node)
                priv->bsplit_parent_node = NULL;
            priv->tlist = g_list_delete_link(priv->tlist, tnode);
        }
    }
}

static void
delete_row_at(GncTreeModelTransaction *model, GtkTreeIter *iter)
{
    GtkTreePath *path;
    g_assert(VALID_ITER(model, iter));
    path = gnc_tree_model_transaction_get_path(GTK_TREE_MODEL(model), iter);
    delete_row_at_path(model, path);
    gtk_tree_path_free(path);
}

static void
changed_row_at(GncTreeModelTransaction *model, GtkTreeIter *iter)
{
    GtkTreePath *path;
    g_assert(VALID_ITER(model, iter));
    path = gnc_tree_model_transaction_get_path(GTK_TREE_MODEL(model), iter);
    if (!path) PERR("Null path");

    increment_stamp(model);
    if (gnc_tree_model_transaction_get_iter(GTK_TREE_MODEL(model), iter, path))
        gtk_tree_model_row_changed(GTK_TREE_MODEL(model), path, iter);
    else PERR("Tried to change with invalid iter.");

    gtk_tree_path_free(path);
}

static void
insert_trans(GncTreeModelTransaction *model, Transaction *trans)
{
    GtkTreeIter iter;
    GList *tnode, *snode;

    model->priv->tlist = g_list_prepend(model->priv->tlist, trans);
    tnode = model->priv->tlist;
    iter = make_iter(model, 0, tnode, NULL);
    insert_row_at(model, &iter);
    for (snode = xaccTransGetSplitList(trans); snode; snode = snode->next) {
        if (xaccTransStillHasSplit(trans, snode->data)) {
            iter = make_iter(model, SPLIT, tnode, snode);
            insert_row_at(model, &iter);
        }
    }

}

/* Moves the blank split to 'trans'.
 */
gboolean
gnc_tree_model_transaction_set_blank_split_parent(
    GncTreeModelTransaction *model, Transaction *trans)
{
    GList *tnode, *bs_parent_node;
    GncTreeModelTransactionPrivate *priv;
    GtkTreeIter iter;
    gboolean moved;

    priv = model->priv;
    tnode = g_list_find(priv->tlist, trans);

    bs_parent_node = priv->bsplit_parent_node;

    if (tnode != bs_parent_node) {
        moved = (bs_parent_node != NULL);
        if (moved) {
            /* Delete the row where the blank split used to be. */
            iter = make_iter(model, SPLIT | BLANK, bs_parent_node,
                              priv->bsplit_node);
            delete_row_at(model, &iter);
            priv->bsplit_parent_node = NULL;
        }

        priv->bsplit_parent_node = tnode;
        iter = make_iter(model, SPLIT | BLANK, tnode, priv->bsplit_node);
        insert_row_at(model, &iter);
    } else
        moved = FALSE;

    return moved;
}

Account *
gnc_tree_model_transaction_get_anchor(GncTreeModelTransaction *model)
{
    g_return_val_if_fail(GNC_IS_TREE_MODEL_TRANSACTION(model), NULL);
    return model->priv->anchor;
}

gboolean
gnc_tree_model_transaction_get_split_and_trans (
    GncTreeModelTransaction *model, GtkTreeIter *iter,
    gboolean *is_split, gboolean *is_blank, Split **split, Transaction **trans)
{
    GList *node;

    g_return_val_if_fail(VALID_ITER(model, iter), FALSE);

    if (is_split)
        *is_split = !!IS_SPLIT(iter);
    if (is_blank)
        *is_blank = !!IS_BLANK(iter);

    if (trans) {
        node = iter->user_data2;
        *trans = node ? (Transaction *) node->data : NULL;
    }
    if (split) {
        node = iter->user_data3;
        *split = node ? (Split *) node->data : NULL;
    }
    return TRUE;
}

static void
make_new_blank_split(GncTreeModelTransaction *model)
{
    GtkTreeIter iter;
    Split *split;
    GList *tnode = model->priv->bsplit_parent_node;

    split = xaccMallocSplit(model->priv->book);
    // This is maybe a BadIdea.
    //if (model->priv->anchor)
    //    xaccSplitSetAccount(split, model->priv->anchor);
    model->priv->bsplit = split;
    model->priv->bsplit_node->data = model->priv->bsplit;

    /* Insert the new row */
    iter = make_iter(model, BLANK|SPLIT, tnode, model->priv->bsplit_node);
    insert_row_at(model, &iter);
}

/* Turn the current blank split into a real split.  This function is
 * never called in response to an engine event.  Instead, this
 * function is called from the treeview to tell the model to commit
 * the blank split.
 */
static void
gnc_tree_model_transaction_commit_blank_split(GncTreeModelTransaction *model)
{
    Split *bsplit;
    Transaction *trans;
    GList *tnode, *snode;
    GtkTreeIter iter;

    tnode = model->priv->bsplit_parent_node;
    bsplit = model->priv->bsplit;
    if (!tnode || !tnode->data) {
        PERR("blank split has no trans");
        return;
    }
    trans = tnode->data;
    if (xaccTransGetSplitIndex(trans, bsplit) == -1) {
        PINFO("blank split has been removed from this trans");
        return;
    }
    snode = g_list_find(xaccTransGetSplitList(trans), bsplit);
    if (!snode) {
        PERR("Failed to turn blank split into real split");
        return;
    }

    /* If we haven't set an amount yet, and there's an imbalance, use that. */
    if (gnc_numeric_zero_p(xaccSplitGetAmount(bsplit))) {
        gnc_numeric imbal = gnc_numeric_neg(xaccTransGetImbalance(trans));
        if (!gnc_numeric_zero_p(imbal)) {
            gnc_numeric amount, rate;
            Account *acct = xaccSplitGetAccount(bsplit);
            xaccSplitSetValue(bsplit, imbal);
            if (gnc_commodity_equal(xaccAccountGetCommodity(acct),
                                    xaccTransGetCurrency(trans)))
                amount = imbal;
            else {
                rate = xaccTransGetAccountConvRate(trans, acct);
                amount = gnc_numeric_mul(
                    imbal, rate,
                    xaccAccountGetCommoditySCU(acct), GNC_RND_ROUND);
            }
            if (gnc_numeric_check(amount) == GNC_ERROR_OK)
                xaccSplitSetAmount(bsplit, amount);
        }
    }
    /* Mark the old blank split as changed */
    iter = make_iter(model, SPLIT, tnode, snode);
    changed_row_at(model, &iter);
    make_new_blank_split(model);
}

void
gnc_tree_model_transaction_commit_split(GncTreeModelTransaction *model,
                                        Split *split)
{
    if (split == model->priv->bsplit) {
        gnc_tree_model_transaction_commit_blank_split(model);
    }
}

#define get_iter gnc_tree_model_transaction_get_iter_from_trans_and_split
gboolean
gnc_tree_model_transaction_get_iter_from_trans_and_split(
    GncTreeModelTransaction *model, Transaction *trans, Split *split, 
    GtkTreeIter *iter)
{
    GncTreeModelTransactionPrivate *priv;
    GList *tnode, *snode = NULL;
    gint flags = 0;

    g_return_val_if_fail(GNC_IS_TREE_MODEL_TRANSACTION(model), FALSE);
    g_return_val_if_fail(iter, FALSE);

    priv = model->priv;
    if (split && !trans) trans = xaccSplitGetParent(split);

    if (trans && priv->book != xaccTransGetBook(trans)) return FALSE;
    if (split && priv->book != xaccSplitGetBook(split)) return FALSE;    
    if (split && !xaccTransStillHasSplit(trans, split)) return FALSE;

    tnode = g_list_find(priv->tlist, trans);
    if (!tnode) return FALSE;

    if (split) {
        GList *slist = xaccTransGetSplitList(trans);
        snode = g_list_find(slist, split);
        flags |= SPLIT;
        if (!snode && split == (Split *) ((GList *)priv->bsplit_node)->data) {
            snode = priv->bsplit_node;
            flags |= BLANK;
        }
        if (!snode) return FALSE;
    }

    if (trans == priv->btrans)
        flags |= BLANK;

    *iter = make_iter(model, flags, tnode, snode);
    return TRUE;
}

gboolean 
gnc_tree_model_transaction_get_blank_trans_iter(GncTreeModelTransaction *model,
                                                GtkTreeIter *iter)
{
    if (!iter) return FALSE;

    g_return_val_if_fail(GNC_IS_TREE_MODEL_TRANSACTION(model), FALSE);
    return get_iter(model, model->priv->btrans, NULL, iter);
}

/* Returns just the path to the transaction if idx_of_split is -1. */
static GtkTreePath *
get_removal_path(GncTreeModelTransaction *model, Transaction *trans,
                 gint idx_of_split)
{
    GncTreeModelTransactionPrivate *priv;
    GList *tnode;
    GtkTreeIter iter;
    GtkTreePath *path;

    g_return_val_if_fail(GNC_IS_TREE_MODEL_TRANSACTION(model), NULL);
    g_return_val_if_fail(trans, NULL);

    priv = model->priv;
    if (priv->book != xaccTransGetBook(trans)) return FALSE;

    tnode = g_list_find(priv->tlist, trans);
    if (!tnode) return FALSE;

    iter = make_iter(model, 0, tnode, NULL);
    path = gnc_tree_model_transaction_get_path(GTK_TREE_MODEL(model), &iter);

    if (idx_of_split >= 0)
        gtk_tree_path_append_index(path, idx_of_split);
    else if (idx_of_split != -1)
        PERR("Invalid idx_of_split");
    return path;
}

/** This function is the handler for all event messages from the
 *  engine.  Its purpose is to update the tree model any time
 *  an split or trans is added to the engine or deleted from the engine.
 *  This change to the model is then propagated to any/all overlying
 *  filters and views.  This function listens to the ADD, REMOVE, and
 *  DESTROY events.
 */
static void
gnc_tree_model_transaction_event_handler(
    QofInstance *entity, QofEventId event_type, gpointer tm, gpointer event_data)
{
    GncTreeModelTransaction *model = (GncTreeModelTransaction *) tm;
    GncTreeModelTransactionPrivate *priv = model->priv;
    GncEventData *ed = event_data;
    GtkTreeIter iter;
    GtkTreePath *path;
    Transaction *trans;
    Split *split = NULL;
    QofIdType type;
    const gchar *name;
    GList *tnode;

    g_return_if_fail(GNC_IS_TREE_MODEL_TRANSACTION(model));
    if (qof_instance_get_book(entity) != priv->book)
        return;
    type = entity->e_type;

    if (safe_strcmp(type, GNC_ID_SPLIT) == 0) {
        /* Get the split.*/
        split = (Split *) entity;
        name = xaccSplitGetMemo(split);

        switch (event_type) {
        case QOF_EVENT_MODIFY:
            if (get_iter(model, NULL, split, &iter)) {
                DEBUG("change split %p (%s)", split, name);
                changed_row_at(model, &iter);
            }
            break;
        default:
            DEBUG("ignored event for %p (%s)", split, name);
        }
    } else if (safe_strcmp(type, GNC_ID_TRANS) == 0) {
        /* Get the trans.*/
        trans = (Transaction *) entity;
        name = xaccTransGetDescription(trans);

        switch (event_type) {
        case GNC_EVENT_ITEM_ADDED:
            split = (Split *) ed->node;
            /* The blank split will be added to the transaction when
               it's first edited.  That will generate an event, but
               we don't want to emit row_inserted because we were
               already showing the blank split. */
            if (split == priv->bsplit) break;

            /* Tell the filters/views where the new row was added. */
            if (get_iter(model, trans, split, &iter)) {
                DEBUG("add split %p (%s)", split, name);
                insert_row_at(model, &iter);
            }
            break;
        case GNC_EVENT_ITEM_REMOVED:
            split = (Split *) ed->node;

            path = get_removal_path(model, trans, ed->idx);
            if (path) {
                DEBUG("remove trans %p (%s)", trans, name);
                delete_row_at_path(model, path);
                gtk_tree_path_free(path);
            }
            if (split == priv->bsplit)
                make_new_blank_split(model);
            break;
        case QOF_EVENT_MODIFY:
            /* The blank trans won't emit MODIFY until it's committed */
            if (priv->btrans == trans) {
                priv->btrans = xaccMallocTransaction(priv->book);
                priv->tlist = g_list_append(priv->tlist, priv->btrans);

                /* Insert a new blank trans */
                iter = make_iter(model, BLANK, priv->tlist, NULL);
                insert_row_at(model, &iter);
            }

            if (get_iter(model, trans, NULL, &iter)) {
                DEBUG("change trans %p (%s)", trans, name);
                changed_row_at(model, &iter);
            }
            break;
        case QOF_EVENT_DESTROY:
            if (priv->btrans == trans) {
                tnode = g_list_find(priv->tlist, priv->btrans);
                priv->btrans = xaccMallocTransaction(priv->book);
                tnode->data = priv->btrans;

                iter = make_iter(model, BLANK, tnode, NULL);
                changed_row_at(model, &iter);
            } else if (get_iter(model, trans, NULL, &iter)) {
                delete_row_at(model, &iter);
                DEBUG("destroy trans %p (%s)", trans, name);
            }
            break;
        default:
            DEBUG("ignored event for %p (%s)", trans, name);
        }
    } else if (safe_strcmp(type, GNC_ID_ACCOUNT) == 0) {
        switch (event_type) {
            Account *acc;
        case GNC_EVENT_ITEM_ADDED:
            split = (Split *) ed;
            acc = xaccSplitGetAccount(split);
            trans = xaccSplitGetParent(split);
            if (!g_list_find(priv->tlist, trans) &&
                ((xaccAccountHasAncestor(acc, priv->anchor) &&
                  priv->include_subacc) || acc == priv->anchor)) {
                insert_trans(model, trans);
            }
            break;
        default:
            ;
        }
    }
}

QofBook *
gnc_tree_model_transaction_get_book(GncTreeModelTransaction *model)
{
    g_return_val_if_fail(GNC_IS_TREE_MODEL_TRANSACTION(model), NULL);
    return model->priv->book;
}

//FIXME: Is this even needed?
gint
gtmt_sort_by_date(GtkTreeModel *tm, GtkTreeIter *a, GtkTreeIter *b,
                  gpointer user_data)
{
    GncTreeModelTransaction *model  = GNC_TREE_MODEL_TRANSACTION(tm);
    GList *tnode;
    time_t i, j;

    /* Games we play here: blank trans is always last */
    if (!VALID_ITER(model, a)) PERR("Invalid a iter.");
    if (!VALID_ITER(model, b)) PERR("Invalid b iter.");

    if (IS_BLANK_TRANS(a)) return 1;
    if (IS_BLANK_TRANS(b)) return -1;

    tnode = a->user_data2;
    i = xaccTransGetDate((Transaction*)tnode->data);
    tnode = b->user_data2;
    j = xaccTransGetDate((Transaction*)tnode->data);

    return (gint)(i - j);
}
