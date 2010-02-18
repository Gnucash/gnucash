/*
 * qif-context.c -- create/destroy QIF Contexts
 *
 * Written By:	Derek Atkins <derek@ihtfp.com>
 * Copyright (c) 2003 Derek Atkins <warlord@MIT.EDU>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <glib.h>

#include "qif-import-p.h"
#include "qif-objects-p.h"

static void qif_object_map_get_helper(gpointer key, gpointer value, gpointer listp);

QifContext
qif_context_new(void)
{
    QifContext ctx = g_new0(struct _QifContext, 1);

    ctx->object_lists = g_hash_table_new(g_str_hash, g_str_equal);
    ctx->object_maps = g_hash_table_new(g_str_hash, g_str_equal);

    return ctx;
}

void
qif_context_destroy(QifContext ctx)
{
    GList *node, *temp;
    QifContext fctx;

    if (!ctx) return;

    /* First, try to destroy all the children contexts */
    for (node = ctx->files; node; node = temp)
    {
        fctx = node->data;
        temp = node->next;
        qif_context_destroy(fctx);
    }

    /* ok, at this point we're actually destroying this context. */

    /* force the end of record */
    if (ctx->handler && ctx->handler->end)
        ctx->handler->end(ctx);

    /* destroy the state objects */
    qif_object_list_destroy(ctx);
    qif_object_map_destroy(ctx);

    /* Remove us from our parent context */
    if (ctx->parent)
        ctx->parent->files = g_list_remove(ctx->parent->files, ctx);

    g_free(ctx->filename);

    g_assert(ctx->files == NULL);
    g_free(ctx);
}

static GList *
qif_context_get_foo_helper(QifContext ctx, GFunc get_helper)
{
    GHashTable *ht;
    GList *node, *list = NULL;
    QifContext fctx;

    g_return_val_if_fail(ctx, NULL);
    g_return_val_if_fail(ctx->parsed, NULL);
    g_return_val_if_fail(get_helper, NULL);

    ht = g_hash_table_new(g_direct_hash, g_direct_equal);

    for (node = ctx->files; node; node = node->next)
    {
        fctx = node->data;
        qif_object_list_foreach(fctx, QIF_O_TXN, get_helper, ht);
    }

    g_hash_table_foreach(ht, qif_object_map_get_helper, &list);
    g_hash_table_destroy(ht);

    return list;
}

static void
qif_get_accts_helper(gpointer obj, gpointer htp)
{
    QifTxn txn = obj;
    QifSplit split;
    GHashTable *ht = htp;
    GList *node;

    if (txn->from_acct)
        g_hash_table_insert(ht, txn->from_acct, txn->from_acct);

    /* The default_split is using the from_acct, so we can ignore it */

    for (node = txn->splits; node; node = node->next)
    {
        split = node->data;
        if (split->cat.obj && split->cat_is_acct)
            g_hash_table_insert(ht, split->cat.acct, split->cat.acct);
    }
}

GList *
qif_context_get_accounts(QifContext ctx)
{
    return qif_context_get_foo_helper(ctx, qif_get_accts_helper);
}

static void
qif_get_cats_helper(gpointer obj, gpointer htp)
{
    QifTxn txn = obj;
    QifSplit split;
    GHashTable *ht = htp;
    GList *node;

    /* default_split uses from_acct, so no categories */

    for (node = txn->splits; node; node = node->next)
    {
        split = node->data;
        if (split->cat.obj && !split->cat_is_acct)
            g_hash_table_insert(ht, split->cat.cat, split->cat.cat);
    }
}

GList *
qif_context_get_categories(QifContext ctx)
{
    return qif_context_get_foo_helper(ctx, qif_get_cats_helper);
}

/*****************************************************************************/

/*
 * Insert and remove a QifObject from the Object Maps in this Qif Context
 */

gint
qif_object_map_count(QifContext ctx, const char *type)
{
    GHashTable *ht;

    g_return_val_if_fail(ctx, 0);
    g_return_val_if_fail(ctx->object_maps, 0);
    g_return_val_if_fail(type, 0);

    ht = g_hash_table_lookup(ctx->object_maps, type);
    if (!ht)
        return 0;

    return g_hash_table_size(ht);
}

void
qif_object_map_foreach(QifContext ctx, const char *type, GHFunc func, gpointer arg)
{
    GHashTable *ht;

    g_return_if_fail(ctx);
    g_return_if_fail(ctx->object_maps);
    g_return_if_fail(type);

    ht = g_hash_table_lookup(ctx->object_maps, type);
    if (ht)
        g_hash_table_foreach(ht, func, arg);
}

void
qif_object_map_insert(QifContext ctx, const char *key, QifObject obj)
{
    GHashTable *ht;

    g_return_if_fail(ctx);
    g_return_if_fail(ctx->object_maps);
    g_return_if_fail(key);
    g_return_if_fail(obj);
    g_return_if_fail(obj->type);

    ht = g_hash_table_lookup(ctx->object_maps, obj->type);
    if (!ht)
    {
        ht = g_hash_table_new(g_str_hash, g_str_equal);
        g_assert(ht);
        g_hash_table_insert(ctx->object_maps, (gpointer)obj->type, ht);
    }

    g_hash_table_insert(ht, (gpointer)key, obj);
}

void
qif_object_map_remove(QifContext ctx, const char *type, const char *key)
{
    GHashTable *ht;

    g_return_if_fail(ctx);
    g_return_if_fail(ctx->object_maps);
    g_return_if_fail(type);
    g_return_if_fail(key);

    ht = g_hash_table_lookup(ctx->object_maps, type);
    if (!ht) return;

    g_hash_table_remove(ht, key);
}

QifObject
qif_object_map_lookup(QifContext ctx, const char *type, const char *key)
{
    GHashTable *ht;

    g_return_val_if_fail(ctx, NULL);
    g_return_val_if_fail(ctx->object_maps, NULL);
    g_return_val_if_fail(type, NULL);
    g_return_val_if_fail(key, NULL);

    ht = g_hash_table_lookup(ctx->object_maps, type);
    if (!ht) return NULL;

    return g_hash_table_lookup(ht, key);
}

/* This GList _SHOULD_ be freed by the caller */

static void
qif_object_map_get_helper(gpointer key, gpointer value, gpointer arg)
{
    GList **listp = arg;
    g_return_if_fail(listp);

    *listp = g_list_prepend(*listp, value);
}

GList *
qif_object_map_get(QifContext ctx, const char *type)
{
    GHashTable *ht;
    GList *list = NULL;

    g_return_val_if_fail(ctx, NULL);
    g_return_val_if_fail(ctx->object_maps, NULL);
    g_return_val_if_fail(type, NULL);

    ht = g_hash_table_lookup(ctx->object_maps, type);
    if (!ht)
        return NULL;

    g_hash_table_foreach(ht, qif_object_map_get_helper, &list);

    return list;
}

static gboolean
qif_object_map_remove_each(gpointer key, gpointer value, gpointer arg)
{
    QifObject obj = value;
    obj->destroy(obj);
    return TRUE;
}

static gboolean
qif_object_map_remove_all(gpointer key, gpointer value, gpointer arg)
{
    GHashTable *ht = value;

    g_hash_table_foreach_remove(ht, qif_object_map_remove_each, NULL);
    g_hash_table_destroy(ht);
    return TRUE;
}

void qif_object_map_destroy(QifContext ctx)
{
    g_return_if_fail(ctx);
    g_return_if_fail(ctx->object_maps);

    g_hash_table_foreach_remove(ctx->object_maps, qif_object_map_remove_all, NULL);
    g_hash_table_destroy(ctx->object_maps);
}

/*****************************************************************************/

/*
 * Insert and remove a QifObject from the Object Lists in this Qif Context
 */

void
qif_object_list_reverse(QifContext ctx, const char *type)
{
    GList *list;

    g_return_if_fail(ctx);
    g_return_if_fail(ctx->object_lists);
    g_return_if_fail(type);

    list = qif_object_list_get(ctx, type);
    list = g_list_reverse(list);
    g_hash_table_insert(ctx->object_lists, (gpointer)type, list);
}

gint
qif_object_list_count(QifContext ctx, const char *type)
{
    GList *list;

    g_return_val_if_fail(ctx, 0);
    g_return_val_if_fail(ctx->object_lists, 0);
    g_return_val_if_fail(type, 0);

    list = g_hash_table_lookup(ctx->object_lists, type);
    return g_list_length(list);
}

void
qif_object_list_foreach(QifContext ctx, const char *type, GFunc func, gpointer arg)
{
    GList *list;

    g_return_if_fail(ctx);
    g_return_if_fail(ctx->object_lists);
    g_return_if_fail(type);

    list = qif_object_list_get(ctx, type);
    g_list_foreach(list, func, arg);
}

void
qif_object_list_insert(QifContext ctx, QifObject obj)
{
    GList *list;

    g_return_if_fail(ctx);
    g_return_if_fail(ctx->object_lists);
    g_return_if_fail(obj);
    g_return_if_fail(obj->type && *obj->type);

    list = g_hash_table_lookup(ctx->object_lists, obj->type);
    list = g_list_prepend(list, obj);
    g_hash_table_insert(ctx->object_lists, (gpointer)obj->type, list);
}

void
qif_object_list_remove(QifContext ctx, QifObject obj)
{
    GList *list;

    g_return_if_fail(ctx);
    g_return_if_fail(ctx->object_lists);
    g_return_if_fail(obj);
    g_return_if_fail(obj->type && *obj->type);

    list = g_hash_table_lookup(ctx->object_lists, obj->type);
    list = g_list_remove(list, obj);
    g_hash_table_insert(ctx->object_lists, (gpointer)obj->type, list);
}

GList *
qif_object_list_get(QifContext ctx, const char *type)
{
    g_return_val_if_fail(ctx, NULL);
    g_return_val_if_fail(ctx->object_lists, NULL);
    g_return_val_if_fail(type, NULL);

    return g_hash_table_lookup(ctx->object_lists, type);
}

static gboolean
qif_object_list_remove_all(gpointer key, gpointer value, gpointer arg)
{
    GList *list = value;
    GList *node;
    QifObject obj;

    for (node = list; node; node = node->next)
    {
        obj = node->data;
        obj->destroy(obj);
    }

    g_list_free(list);
    return TRUE;
}

void
qif_object_list_destroy(QifContext ctx)
{
    g_return_if_fail(ctx);
    g_return_if_fail(ctx->object_lists);

    g_hash_table_foreach_remove(ctx->object_lists, qif_object_list_remove_all, NULL);
    g_hash_table_destroy(ctx->object_lists);
}
