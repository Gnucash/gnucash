/*
 * qif-context.c -- create/destroy QIF Contexts
 *
 * Written By:	Derek Atkins <derek@ihtfp.com>
 *
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <glib.h>

#include "qif-import-p.h"

QifContext
qif_context_new(QifContext parent)
{
  QifContext ctx = g_new0(struct _QifContext, 1);

  if (parent)
    ctx->parent = parent;

  ctx->object_lists = g_hash_table_new(g_str_hash, g_str_equal);
  ctx->object_maps = g_hash_table_new(g_str_hash, g_str_equal);

  /* we should assume that we've got a bank account... just in case.. */
  qif_parse_bangtype(ctx, "!type:bank");

  /* Return the new context */
  return ctx;
}

void
qif_context_destroy(QifContext ctx)
{
  /* force the end of record */
  if (ctx->handler && ctx->handler->end)
    ctx->handler->end(ctx);

  /* destroy the state objects */
  qif_object_list_destroy(ctx);
  qif_object_map_destroy(ctx);

  g_free(ctx);
}

/*****************************************************************************/

/*
 * Insert and remove a QifObject from the Object Maps in this Qif Context
 */

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
  if (!ht) {
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

  g_hash_table_foreach_remove(ctx->object_lists, qif_object_map_remove_all, NULL);
  g_hash_table_destroy(ctx->object_lists);
}

/*****************************************************************************/

/*
 * Insert and remove a QifObject from the Object Lists in this Qif Context
 */

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

  for (node = list; node; node = node->next) {
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

