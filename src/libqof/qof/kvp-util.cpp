/********************************************************************\
 * kvp_util.cpp -- misc odd-job kvp utils                           *
 * Copyright (C) 2001 Linas Vepstas <linas@linas.org>               *
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

#ifdef __cplusplus
extern "C"
{
#endif

#include "config.h"

#include <glib.h>
#include <stdio.h>

#ifdef __cplusplus
}
#endif

#include "kvp_frame.h"
#include "kvp-util.h"
#include "kvp-util-p.h"

/* ================================================================ */

static KvpFrame *
gnc_kvp_array_va (KvpFrame *kvp_root, const char * path,
                  time64 secs, const char * first_name, va_list ap)
{
    KvpFrame *cwd;
    Timespec ts;
    const char *name;

    if (!kvp_root) return NULL;
    if (!first_name) return NULL;

    /* Create subdirectory and put the actual data */
    cwd = kvp_frame_new();

    /* Record the time */
    ts.tv_sec = secs;
    ts.tv_nsec = 0;
    kvp_frame_set_timespec (cwd, "date", ts);

    /* Loop over the args */
    name = first_name;
    while (name)
    {
        const GncGUID *guid;
        guid = va_arg (ap, const GncGUID *);

        kvp_frame_set_guid (cwd, name, guid);

        name = va_arg (ap, const char *);
    }

    /* Attach cwd into the array */
    kvp_frame_add_frame_nc (kvp_root, path, cwd);
    return cwd;
}

/* ================================================================ */

KvpFrame *
gnc_kvp_bag_add (KvpFrame *pwd, const char * path,
                 time64 secs, const char *first_name, ...)
{
    KvpFrame *cwd;
    va_list ap;
    va_start (ap, first_name);
    cwd = gnc_kvp_array_va (pwd, path, secs, first_name, ap);
    va_end (ap);
    return cwd;
}

/* ================================================================ */

#define MATCH_GUID(elt) {                                       \
  KvpFrame *fr = kvp_value_get_frame (elt);                     \
  if (fr) {                                                     \
     GncGUID *guid = kvp_frame_get_guid (fr, guid_name);           \
     if (guid && guid_equal (desired_guid, guid)) return fr;    \
  }                                                             \
}

KvpFrame *
gnc_kvp_bag_find_by_guid (KvpFrame *root, const char * path,
                          const char *guid_name, const GncGUID *desired_guid)
{
    KvpValue *arr;
    KvpValueType valtype;
    GList *node;

    arr = kvp_frame_get_value (root, path);
    valtype = kvp_value_get_type (arr);
    if (KVP_TYPE_FRAME == valtype)
    {
        MATCH_GUID (arr);
        return NULL;
    }

    /* Its gotta be a single isolated frame, or a list of them. */
    if (KVP_TYPE_GLIST != valtype) return NULL;

    for (node = kvp_value_get_glist(arr); node; node = node->next)
    {
        KvpValue *va = static_cast<KvpValue*>(node->data);
        MATCH_GUID (va);
    }
    return NULL;
}

/* ================================================================ */

void
gnc_kvp_bag_remove_frame (KvpFrame *root, const char *path, KvpFrame *fr)
{
    KvpValue *arr;
    KvpValueType valtype;
    GList *node, *listhead;

    arr = kvp_frame_get_value (root, path);
    valtype = kvp_value_get_type (arr);
    if (KVP_TYPE_FRAME == valtype)
    {
        if (fr == kvp_value_get_frame (arr))
        {
            KvpValue *old_val = kvp_frame_replace_value_nc (root, path, NULL);
            kvp_value_replace_frame_nc (old_val, NULL);
            kvp_value_delete (old_val);
        }
        return;
    }

    /* Its gotta be a single isolated frame, or a list of them. */
    if (KVP_TYPE_GLIST != valtype) return;

    listhead = kvp_value_get_glist(arr);
    for (node = listhead; node; node = node->next)
    {
        KvpValue *va = static_cast<KvpValue*>(node->data);
        if (fr == kvp_value_get_frame (va))
        {
            listhead = g_list_remove_link (listhead, node);
            g_list_free_1 (node);
            kvp_value_replace_glist_nc (arr, listhead);
            kvp_value_replace_frame_nc (va, NULL);
            kvp_value_delete (va);
            return;
        }
    }
}

/* ================================================================ */

static KvpFrame *
gnc_kvp_bag_get_first (KvpFrame *root, const char * path)
{
    KvpValue *arr, *va;
    KvpValueType valtype;
    GList *node;

    arr = kvp_frame_get_value (root, path);
    valtype = kvp_value_get_type (arr);
    if (KVP_TYPE_FRAME == valtype)
    {
        return kvp_value_get_frame(arr);
    }

    /* Its gotta be a single isolated frame, or a list of them. */
    if (KVP_TYPE_GLIST != valtype) return NULL;

    node = kvp_value_get_glist(arr);
    if (NULL == node) return NULL;

    va = static_cast<KvpValue*>(node->data);
    return kvp_value_get_frame(va);
}

void
gnc_kvp_bag_merge (KvpFrame *kvp_into, const char *intopath,
                   KvpFrame *kvp_from, const char *frompath)
{
    KvpFrame *fr;

    fr = gnc_kvp_bag_get_first (kvp_from, frompath);
    while (fr)
    {
        gnc_kvp_bag_remove_frame (kvp_from, frompath, fr);
        kvp_frame_add_frame_nc (kvp_into, intopath, fr);
        fr = gnc_kvp_bag_get_first (kvp_from, frompath);
    }
}

/* ================================================================ */
/*
 * See header for docs.
 */

static void
kv_pair_helper(gpointer key, gpointer val, gpointer user_data)
{
    GSList **result = (GSList **) user_data;
    GHashTableKVPair *kvp = g_new(GHashTableKVPair, 1);

    kvp->key = key;
    kvp->value = val;
    *result = g_slist_prepend(*result, kvp);
}

GSList *
g_hash_table_key_value_pairs(GHashTable *table)
{
    GSList *result_list = NULL;
    g_hash_table_foreach(table, kv_pair_helper, &result_list);
    return result_list;
}

void
g_hash_table_kv_pair_free_gfunc(gpointer data, G_GNUC_UNUSED gpointer user_data)
{
    GHashTableKVPair *kvp = (GHashTableKVPair *) data;
    g_free(kvp);
}

/*======================== END OF FILE =============================*/
