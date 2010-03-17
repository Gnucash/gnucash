/********************************************************************
 * kvp_frame.c -- Implements a key-value frame system               *
 * Copyright (C) 2000 Bill Gribble                                  *
 * Copyright (C) 2001,2003 Linas Vepstas <linas@linas.org>          *
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
 ********************************************************************/

#include "config.h"

#include <glib.h>
#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include "qof.h"

/* Note that we keep the keys for this hash table in a GCache
 * (qof_util_string_cache), as it is very likely we will see the
 * same keys over and over again  */

struct _KvpFrame
{
    GHashTable  * hash;
};


typedef struct
{
    void        *data;
    int         datasize;
} KvpValueBinaryData;

struct _KvpValue
{
    KvpValueType type;
    union
    {
        gint64 int64;
        double dbl;
        gnc_numeric numeric;
        gchar *str;
        GUID *guid;
        Timespec timespec;
        KvpValueBinaryData binary;
        GList *list;
        KvpFrame *frame;
        GDate gdate;
    } value;
};

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = QOF_MOD_KVP;

/* *******************************************************************
 * KvpFrame functions
 ********************************************************************/

static guint
kvp_hash_func(gconstpointer v)
{
    return g_str_hash(v);
}

static gint
kvp_comp_func(gconstpointer v, gconstpointer v2)
{
    return g_str_equal(v, v2);
}

static gboolean
init_frame_body_if_needed(KvpFrame *f)
{
    if (!f->hash)
    {
        f->hash = g_hash_table_new(&kvp_hash_func, &kvp_comp_func);
    }
    return(f->hash != NULL);
}

KvpFrame *
kvp_frame_new(void)
{
    KvpFrame * retval = g_new0(KvpFrame, 1);

    /* Save space until the frame is actually used */
    retval->hash = NULL;
    return retval;
}

static void
kvp_frame_delete_worker(gpointer key, gpointer value, gpointer user_data)
{
    qof_util_string_cache_remove(key);
    kvp_value_delete((KvpValue *)value);
}

void
kvp_frame_delete(KvpFrame * frame)
{
    if (!frame) return;

    if (frame->hash)
    {
        /* free any allocated resource for frame or its children */
        g_hash_table_foreach(frame->hash, & kvp_frame_delete_worker,
                             (gpointer)frame);

        /* delete the hash table */
        g_hash_table_destroy(frame->hash);
        frame->hash = NULL;
    }
    g_free(frame);
}

gboolean
kvp_frame_is_empty(const KvpFrame * frame)
{
    if (!frame) return TRUE;
    if (!frame->hash) return TRUE;
    return FALSE;
}

static void
kvp_frame_copy_worker(gpointer key, gpointer value, gpointer user_data)
{
    KvpFrame * dest = (KvpFrame *)user_data;
    g_hash_table_insert(dest->hash,
                        qof_util_string_cache_insert(key),
                        (gpointer)kvp_value_copy(value));
}

KvpFrame *
kvp_frame_copy(const KvpFrame * frame)
{
    KvpFrame * retval = kvp_frame_new();

    if (!frame) return retval;

    if (frame->hash)
    {
        if (!init_frame_body_if_needed(retval)) return(NULL);
        g_hash_table_foreach(frame->hash,
                             & kvp_frame_copy_worker,
                             (gpointer)retval);
    }
    return retval;
}

/* Replace the old value with the new value.  Return the old value.
 * Passing in a null value into this routine has the effect of
 * removing the key from the KVP tree.
 */
KvpValue *
kvp_frame_replace_slot_nc (KvpFrame * frame, const char * slot,
                           KvpValue * new_value)
{
    gpointer orig_key;
    gpointer orig_value = NULL;
    int      key_exists;

    if (!frame || !slot) return NULL;
    if (!init_frame_body_if_needed(frame)) return NULL; /* Error ... */

    key_exists = g_hash_table_lookup_extended(frame->hash, slot,
                 & orig_key, & orig_value);
    if (key_exists)
    {
        g_hash_table_remove(frame->hash, slot);
        qof_util_string_cache_remove(orig_key);
    }
    else
    {
        orig_value = NULL;
    }

    if (new_value)
    {
        g_hash_table_insert(frame->hash,
                            qof_util_string_cache_insert((gpointer) slot),
                            new_value);
    }

    return (KvpValue *) orig_value;
}

/* Passing in a null value into this routine has the effect
 * of deleting the old value stored at this slot.
 */
static inline void
kvp_frame_set_slot_destructively(KvpFrame * frame, const char * slot,
                                 KvpValue * new_value)
{
    KvpValue * old_value;
    old_value = kvp_frame_replace_slot_nc (frame, slot, new_value);
    kvp_value_delete (old_value);
}

/* ============================================================ */
/* Get the named frame, or create it if it doesn't exist.
 * gcc -O3 should inline it.  It performs no error checks,
 * the caller is responsible of passing good keys and frames.
 */
static inline KvpFrame *
get_or_make (KvpFrame *fr, const char * key)
{
    KvpFrame *next_frame;
    KvpValue *value;

    value = kvp_frame_get_slot (fr, key);
    if (value)
    {
        next_frame = kvp_value_get_frame (value);
    }
    else
    {
        next_frame = kvp_frame_new ();
        kvp_frame_set_slot_nc (fr, key,
                               kvp_value_new_frame_nc (next_frame));
    }
    return next_frame;
}

/* Get pointer to last frame in path. If the path doesn't exist,
 * it is created.  The string stored in keypath will be hopelessly
 * mangled .
 */
static inline KvpFrame *
kvp_frame_get_frame_slash_trash (KvpFrame *frame, char *key_path)
{
    char *key, *next;
    if (!frame || !key_path) return frame;

    key = key_path;
    key --;

    while (key)
    {
        key ++;
        while ('/' == *key)
        {
            key++;
        }
        if (0x0 == *key) break;    /* trailing slash */
        next = strchr (key, '/');
        if (next) *next = 0x0;

        frame = get_or_make (frame, key);
        if (!frame) break;  /* error - should never happen */

        key = next;
    }
    return frame;
}

/* ============================================================ */
/* Get pointer to last frame in path, or NULL if the path doesn't
 * exist. The string stored in keypath will be hopelessly mangled .
 */
static inline const KvpFrame *
kvp_frame_get_frame_or_null_slash_trash (const KvpFrame *frame, char *key_path)
{
    KvpValue *value;
    char *key, *next;
    if (!frame || !key_path) return NULL;

    key = key_path;
    key --;

    while (key)
    {
        key ++;
        while ('/' == *key)
        {
            key++;
        }
        if (0x0 == *key) break;    /* trailing slash */
        next = strchr (key, '/');
        if (next) *next = 0x0;

        value = kvp_frame_get_slot (frame, key);
        if (!value) return NULL;
        frame = kvp_value_get_frame (value);
        if (!frame) return NULL;

        key = next;
    }
    return frame;
}

/* Return pointer to last frame in path, and also store the
 * last dangling part of path in 'end_key'.  If path doesn't
 * exist, it is created.
 */

static inline KvpFrame *
get_trailer_make (KvpFrame * frame, const char * key_path, char **end_key)
{
    char *last_key;

    if (!frame || !key_path || (0 == key_path[0])) return NULL;

    last_key = strrchr (key_path, '/');
    if (NULL == last_key)
    {
        last_key = (char *) key_path;
    }
    else if (last_key == key_path)
    {
        last_key ++;
    }
    else if (0 == last_key[1])
    {
        return NULL;
    }
    else
    {
        char *root, *lkey;
        root = g_strdup (key_path);
        lkey = strrchr (root, '/');
        *lkey = 0;
        frame = kvp_frame_get_frame_slash_trash (frame, root);
        g_free(root);

        last_key ++;
    }

    *end_key = last_key;
    return frame;
}


/* Return pointer to last frame in path, or NULL if the path
 * doesn't exist.  Also store the last dangling part of path
 * in 'end_key'.
 */

static inline const KvpFrame *
get_trailer_or_null (const KvpFrame * frame, const char * key_path, char **end_key)
{
    char *last_key;

    if (!frame || !key_path || (0 == key_path[0])) return NULL;

    last_key = strrchr (key_path, '/');
    if (NULL == last_key)
    {
        last_key = (char *) key_path;
    }
    else if (last_key == key_path)
    {
        last_key ++;
    }
    else if (0 == last_key[1])
    {
        return NULL;
    }
    else
    {
        char *root, *lkey;
        root = g_strdup (key_path);
        lkey = strrchr (root, '/');
        *lkey = 0;
        frame = kvp_frame_get_frame_or_null_slash_trash (frame, root);
        g_free(root);

        last_key ++;
    }

    *end_key = last_key;
    return frame;
}

/* ============================================================ */

void
kvp_frame_set_gint64(KvpFrame * frame, const char * path, gint64 ival)
{
    KvpValue *value;
    value = kvp_value_new_gint64 (ival);
    frame = kvp_frame_set_value_nc (frame, path, value);
    if (!frame) kvp_value_delete (value);
}

void
kvp_frame_set_double(KvpFrame * frame, const char * path, double dval)
{
    KvpValue *value;
    value = kvp_value_new_double (dval);
    frame = kvp_frame_set_value_nc (frame, path, value);
    if (!frame) kvp_value_delete (value);
}

void
kvp_frame_set_numeric(KvpFrame * frame, const char * path, gnc_numeric nval)
{
    KvpValue *value;
    value = kvp_value_new_gnc_numeric (nval);
    frame = kvp_frame_set_value_nc (frame, path, value);
    if (!frame) kvp_value_delete (value);
}

void
kvp_frame_set_string(KvpFrame * frame, const char * path, const char* str)
{
    KvpValue *value;
    value = kvp_value_new_string (str);
    frame = kvp_frame_set_value_nc (frame, path, value);
    if (!frame) kvp_value_delete (value);
}

void
kvp_frame_set_guid(KvpFrame * frame, const char * path, const GUID *guid)
{
    KvpValue *value;
    value = kvp_value_new_guid (guid);
    frame = kvp_frame_set_value_nc (frame, path, value);
    if (!frame) kvp_value_delete (value);
}

void
kvp_frame_set_timespec(KvpFrame * frame, const char * path, Timespec ts)
{
    KvpValue *value;
    value = kvp_value_new_timespec (ts);
    frame = kvp_frame_set_value_nc (frame, path, value);
    if (!frame) kvp_value_delete (value);
}

void
kvp_frame_set_frame(KvpFrame * frame, const char * path, KvpFrame *fr)
{
    KvpValue *value;
    value = kvp_value_new_frame (fr);
    frame = kvp_frame_set_value_nc (frame, path, value);
    if (!frame) kvp_value_delete (value);
}

void
kvp_frame_set_frame_nc(KvpFrame * frame, const char * path, KvpFrame *fr)
{
    KvpValue *value;
    value = kvp_value_new_frame_nc (fr);
    frame = kvp_frame_set_value_nc (frame, path, value);
    if (!frame) kvp_value_delete (value);
}

/* ============================================================ */

KvpFrame *
kvp_frame_set_value_nc (KvpFrame * frame, const char * key_path,
                        KvpValue * value)
{
    char *last_key;

    frame = get_trailer_make (frame, key_path, &last_key);
    if (!frame) return NULL;
    kvp_frame_set_slot_destructively(frame, last_key, value);
    return frame;
}

KvpFrame *
kvp_frame_set_value (KvpFrame * frame, const char * key_path,
                     const KvpValue * value)
{
    KvpValue *new_value = NULL;
    char *last_key;

    frame = get_trailer_make (frame, key_path, &last_key);
    if (!frame) return NULL;

    if (value) new_value = kvp_value_copy(value);
    kvp_frame_set_slot_destructively(frame, last_key, new_value);
    return frame;
}

KvpValue *
kvp_frame_replace_value_nc (KvpFrame * frame, const char * key_path,
                            KvpValue * new_value)
{
    KvpValue * old_value;
    char *last_key;

    last_key = NULL;
    if (new_value)
    {
        frame = get_trailer_make (frame, key_path, &last_key);
    }
    else
    {
        frame = (KvpFrame *) get_trailer_or_null (frame, key_path, &last_key);
    }
    if (!frame) return NULL;

    old_value = kvp_frame_replace_slot_nc (frame, last_key, new_value);
    return old_value;
}

/* ============================================================ */

KvpFrame *
kvp_frame_add_value_nc(KvpFrame * frame, const char * path, KvpValue *value)
{
    char *key = NULL;
    KvpValue *oldvalue;
    KvpFrame* orig_frame = frame;

    frame = (KvpFrame *) get_trailer_or_null (frame, path, &key);
    oldvalue = kvp_frame_get_slot (frame, key);

    ENTER ("old frame=%s", kvp_frame_to_string(frame));
    if (oldvalue)
    {
        /* If already a glist here, just append */
        if (KVP_TYPE_GLIST == oldvalue->type)
        {
            GList * vlist = oldvalue->value.list;
            vlist = g_list_append (vlist, value);
            oldvalue->value.list = vlist;
        }
        else
            /* If some other value, convert it to a glist */
        {
            KvpValue *klist;
            GList *vlist = NULL;

            vlist = g_list_append (vlist, oldvalue);
            vlist = g_list_append (vlist, value);
            klist = kvp_value_new_glist_nc (vlist);

            kvp_frame_replace_slot_nc (frame, key, klist);
        }
        LEAVE ("new frame=%s", kvp_frame_to_string(frame));
        return frame;
    }

    /* Hmm, if we are here, the path doesn't exist. We need to
     * create the path, add the value to it. */
    frame = orig_frame;
    frame = kvp_frame_set_value_nc (frame, path, value);
    LEAVE ("new frame=%s", kvp_frame_to_string(frame));
    return frame;
}

KvpFrame *
kvp_frame_add_value(KvpFrame * frame, const char * path, KvpValue *value)
{
    value = kvp_value_copy (value);
    frame = kvp_frame_add_value_nc (frame, path, value);
    if (!frame) kvp_value_delete (value);
    return frame;
}

void
kvp_frame_add_gint64(KvpFrame * frame, const char * path, gint64 ival)
{
    KvpValue *value;
    value = kvp_value_new_gint64 (ival);
    frame = kvp_frame_add_value_nc (frame, path, value);
    if (!frame) kvp_value_delete (value);
}

void
kvp_frame_add_double(KvpFrame * frame, const char * path, double dval)
{
    KvpValue *value;
    value = kvp_value_new_double (dval);
    frame = kvp_frame_add_value_nc (frame, path, value);
    if (!frame) kvp_value_delete (value);
}

void
kvp_frame_add_numeric(KvpFrame * frame, const char * path, gnc_numeric nval)
{
    KvpValue *value;
    value = kvp_value_new_gnc_numeric (nval);
    frame = kvp_frame_add_value_nc (frame, path, value);
    if (!frame) kvp_value_delete (value);
}

void
kvp_frame_add_string(KvpFrame * frame, const char * path, const char* str)
{
    KvpValue *value;
    value = kvp_value_new_string (str);
    frame = kvp_frame_add_value_nc (frame, path, value);
    if (!frame) kvp_value_delete (value);
}

void
kvp_frame_add_guid(KvpFrame * frame, const char * path, const GUID *guid)
{
    KvpValue *value;
    value = kvp_value_new_guid (guid);
    frame = kvp_frame_add_value_nc (frame, path, value);
    if (!frame) kvp_value_delete (value);
}

void
kvp_frame_add_timespec(KvpFrame * frame, const char * path, Timespec ts)
{
    KvpValue *value;
    value = kvp_value_new_timespec (ts);
    frame = kvp_frame_add_value_nc (frame, path, value);
    if (!frame) kvp_value_delete (value);
}

void
kvp_frame_add_frame(KvpFrame * frame, const char * path, KvpFrame *fr)
{
    KvpValue *value;
    value = kvp_value_new_frame (fr);
    frame = kvp_frame_add_value_nc (frame, path, value);
    if (!frame) kvp_value_delete (value);
}

void
kvp_frame_add_frame_nc(KvpFrame * frame, const char * path, KvpFrame *fr)
{
    KvpValue *value;
    value = kvp_value_new_frame_nc (fr);
    frame = kvp_frame_add_value_nc (frame, path, value);
    if (!frame) kvp_value_delete (value);
}

/* ============================================================ */

void
kvp_frame_set_slot(KvpFrame * frame, const char * slot,
                   const KvpValue * value)
{
    KvpValue *new_value = NULL;

    if (!frame) return;

    g_return_if_fail (slot && *slot != '\0');

    if (value) new_value = kvp_value_copy(value);
    kvp_frame_set_slot_destructively(frame, slot, new_value);
}

void
kvp_frame_set_slot_nc(KvpFrame * frame, const char * slot,
                      KvpValue * value)
{
    if (!frame) return;

    g_return_if_fail (slot && *slot != '\0');

    kvp_frame_set_slot_destructively(frame, slot, value);
}

KvpValue *
kvp_frame_get_slot(const KvpFrame * frame, const char * slot)
{
    KvpValue *v;
    if (!frame) return NULL;
    if (!frame->hash) return NULL;  /* Error ... */
    v = g_hash_table_lookup(frame->hash, slot);
    return v;
}

/* ============================================================ */

void
kvp_frame_set_slot_path (KvpFrame *frame,
                         const KvpValue *new_value,
                         const char *first_key, ...)
{
    va_list ap;
    const char *key;

    if (!frame) return;

    g_return_if_fail (first_key && *first_key != '\0');

    va_start (ap, first_key);

    key = first_key;

    while (TRUE)
    {
        KvpValue *value;
        const char *next_key;

        next_key = va_arg (ap, const char *);
        if (!next_key)
        {
            kvp_frame_set_slot (frame, key, new_value);
            break;
        }

        g_return_if_fail (*next_key != '\0');

        value = kvp_frame_get_slot (frame, key);
        if (!value)
        {
            KvpFrame *new_frame = kvp_frame_new ();
            KvpValue *frame_value = kvp_value_new_frame (new_frame);

            kvp_frame_set_slot_nc (frame, key, frame_value);

            value = kvp_frame_get_slot (frame, key);
            if (!value) break;
        }

        frame = kvp_value_get_frame (value);
        if (!frame) break;

        key = next_key;
    }

    va_end (ap);
}

void
kvp_frame_set_slot_path_gslist (KvpFrame *frame,
                                const KvpValue *new_value,
                                GSList *key_path)
{
    if (!frame || !key_path) return;

    while (TRUE)
    {
        const char *key = key_path->data;
        KvpValue *value;

        if (!key)
            return;

        g_return_if_fail (*key != '\0');

        key_path = key_path->next;
        if (!key_path)
        {
            kvp_frame_set_slot (frame, key, new_value);
            return;
        }

        value = kvp_frame_get_slot (frame, key);
        if (!value)
        {
            KvpFrame *new_frame = kvp_frame_new ();
            KvpValue *frame_value = kvp_value_new_frame (new_frame);

            kvp_frame_set_slot_nc (frame, key, frame_value);

            value = kvp_frame_get_slot (frame, key);
            if (!value)
                return;
        }

        frame = kvp_value_get_frame (value);
        if (!frame)
            return;
    }
}

/* ============================================================ */
/* decode url-encoded string, do it in place
 * + == space
 * %xx == asci char where xx is hexadecimal ascii value
 */

static void
decode (char *enc)
{
    char * p, *w;

    /* Loop, convert +'s to blanks */
    p = strchr (enc, '+');
    while (p)
    {
        *p = ' ';
        p = strchr (p, '+');
    }

    p = strchr (enc, '%');
    w = p;

    while (p)
    {
        int ch, cl;
        p++;
        ch = *p - 0x30;               /* ascii 0 = 0x30 */
        if (9 < ch) ch -= 0x11 - 10;  /* uppercase A = 0x41 */
        if (16 < ch) ch -= 0x20;      /* lowercase a = 0x61 */

        p++;
        cl = *p - 0x30;               /* ascii 0 = 0x30 */
        if (9 < cl) cl -= 0x11 - 10;  /* uppercase A = 0x41 */
        if (16 < cl) cl -= 0x20;      /* lowercase a = 0x61 */

        *w = (char) (ch << 4 | cl);

        do
        {
            ++w;
            ++p;
            *w = *p;
            if (0x0 == *p)
            {
                p = 0;
                break;
            }
            if ('%' == *p)
            {
                break;
            }
        }
        while (*p);
    }
}

void
kvp_frame_add_url_encoding (KvpFrame *frame, const char *enc)
{
    char *buff, *p;
    if (!frame || !enc) return;

    /* Loop over all key-value pairs in the encoded string */
    buff = g_strdup (enc);
    p = buff;
    while (*p)
    {
        char *n, *v;
        n = strchr (p, '&');  /* n = next key-value */
        if (n) *n = 0x0;

        v = strchr (p, '=');  /* v =  pointer to value */
        if (!v) break;
        *v = 0x0;
        v ++;

        decode (p);
        decode (v);
        kvp_frame_set_slot_nc (frame, p, kvp_value_new_string(v));

        if (!n) break; /* no next key, we are done */
        p = ++n;
    }

    g_free(buff);
}

/* ============================================================ */


gint64
kvp_frame_get_gint64(const KvpFrame *frame, const char *path)
{
    char *key = NULL;
    frame = get_trailer_or_null (frame, path, &key);
    return kvp_value_get_gint64(kvp_frame_get_slot (frame, key));
}

double
kvp_frame_get_double(const KvpFrame *frame, const char *path)
{
    char *key = NULL;
    frame = get_trailer_or_null (frame, path, &key);
    return kvp_value_get_double(kvp_frame_get_slot (frame, key));
}

gnc_numeric
kvp_frame_get_numeric(const KvpFrame *frame, const char *path)
{
    char *key = NULL;
    frame = get_trailer_or_null (frame, path, &key);
    return kvp_value_get_numeric(kvp_frame_get_slot (frame, key));
}

const char *
kvp_frame_get_string(const KvpFrame *frame, const char *path)
{
    char *key = NULL;
    frame = get_trailer_or_null (frame, path, &key);
    return kvp_value_get_string(kvp_frame_get_slot (frame, key));
}

GUID *
kvp_frame_get_guid(const KvpFrame *frame, const char *path)
{
    char *key = NULL;
    frame = get_trailer_or_null (frame, path, &key);
    return kvp_value_get_guid(kvp_frame_get_slot (frame, key));
}

void *
kvp_frame_get_binary(const KvpFrame *frame, const char *path,
                     guint64 * size_return)
{
    char *key = NULL;
    frame = get_trailer_or_null (frame, path, &key);
    return kvp_value_get_binary(kvp_frame_get_slot (frame, key), size_return);
}

Timespec
kvp_frame_get_timespec(const KvpFrame *frame, const char *path)
{
    char *key = NULL;
    frame = get_trailer_or_null (frame, path, &key);
    return kvp_value_get_timespec(kvp_frame_get_slot (frame, key));
}

KvpFrame *
kvp_frame_get_frame(const KvpFrame *frame, const char *path)
{
    char *key = NULL;
    frame = get_trailer_or_null (frame, path, &key);
    return kvp_value_get_frame(kvp_frame_get_slot (frame, key));
}

KvpValue *
kvp_frame_get_value(const KvpFrame *frame, const char *path)
{
    char *key = NULL;
    frame = get_trailer_or_null (frame, path, &key);
    return kvp_frame_get_slot (frame, key);
}

/* ============================================================ */

KvpFrame *
kvp_frame_get_frame_gslist (KvpFrame *frame, const GSList *key_path)
{
    if (!frame) return frame;

    while (key_path)
    {
        const char *key = key_path->data;

        if (!key) return frame;  /* an unusual but valid exit for this routine. */

        frame = get_or_make (frame, key);
        if (!frame) return frame;  /* this should never happen */

        key_path = key_path->next;
    }
    return frame;  /* this is the normal exit for this func */
}

KvpFrame *
kvp_frame_get_frame_path (KvpFrame *frame, const char *key,  ...)
{
    va_list ap;
    if (!frame || !key) return frame;

    va_start (ap, key);

    while (key)
    {
        frame = get_or_make (frame, key);
        if (!frame) break;     /* error, should never occur */
        key = va_arg (ap, const char *);
    }

    va_end (ap);
    return frame;
}

KvpFrame *
kvp_frame_get_frame_slash (KvpFrame *frame, const char *key_path)
{
    char *root;
    if (!frame || !key_path) return frame;

    root = g_strdup (key_path);
    frame = kvp_frame_get_frame_slash_trash (frame, root);
    g_free(root);
    return frame;
}

/* ============================================================ */

KvpValue *
kvp_frame_get_slot_path (KvpFrame *frame,
                         const char *first_key, ...)
{
    va_list ap;
    KvpValue *value;
    const char *key;

    if (!frame || !first_key) return NULL;

    va_start (ap, first_key);

    key = first_key;
    value = NULL;

    while (TRUE)
    {
        value = kvp_frame_get_slot (frame, key);
        if (!value) break;

        key = va_arg (ap, const char *);
        if (!key) break;

        frame = kvp_value_get_frame (value);
        if (!frame)
        {
            value = NULL;
            break;
        }
    }

    va_end (ap);

    return value;
}

KvpValue *
kvp_frame_get_slot_path_gslist (KvpFrame *frame,
                                const GSList *key_path)
{
    if (!frame || !key_path) return NULL;

    while (TRUE)
    {
        const char *key = key_path->data;
        KvpValue *value;

        if (!key) return NULL;

        value = kvp_frame_get_slot (frame, key);
        if (!value) return NULL;

        key_path = key_path->next;
        if (!key_path) return value;

        frame = kvp_value_get_frame (value);
        if (!frame) return NULL;
    }
}

/* *******************************************************************
 * kvp glist functions
 ********************************************************************/

void
kvp_glist_delete(GList * list)
{
    GList *node;
    if (!list) return;

    /* Delete the data in the list */
    for (node = list; node; node = node->next)
    {
        KvpValue *val = node->data;
        kvp_value_delete(val);
    }

    /* Free the backbone */
    g_list_free(list);
}

GList *
kvp_glist_copy(const GList * list)
{
    GList * retval = NULL;
    GList * lptr;

    if (!list) return retval;

    /* Duplicate the backbone of the list (this duplicates the POINTERS
     * to the values; we need to deep-copy the values separately) */
    retval = g_list_copy((GList *) list);

    /* This step deep-copies the values */
    for (lptr = retval; lptr; lptr = lptr->next)
    {
        lptr->data = kvp_value_copy(lptr->data);
    }

    return retval;
}

gint
kvp_glist_compare(const GList * list1, const GList * list2)
{
    const GList *lp1;
    const GList *lp2;

    if (list1 == list2) return 0;

    /* Nothing is always less than something */
    if (!list1 && list2) return -1;
    if (list1 && !list2) return 1;

    lp1 = list1;
    lp2 = list2;
    while (lp1 && lp2)
    {
        KvpValue *v1 = (KvpValue *) lp1->data;
        KvpValue *v2 = (KvpValue *) lp2->data;
        gint vcmp = kvp_value_compare(v1, v2);
        if (vcmp != 0) return vcmp;
        lp1 = lp1->next;
        lp2 = lp2->next;
    }
    if (!lp1 && lp2) return -1;
    if (!lp2 && lp1) return 1;
    return 0;
}

/* *******************************************************************
 * KvpValue functions
 ********************************************************************/

KvpValue *
kvp_value_new_gint64(gint64 value)
{
    KvpValue * retval  = g_new0(KvpValue, 1);
    retval->type        = KVP_TYPE_GINT64;
    retval->value.int64 = value;
    return retval;
}

KvpValue *
kvp_value_new_double(double value)
{
    KvpValue * retval  = g_new0(KvpValue, 1);
    retval->type        = KVP_TYPE_DOUBLE;
    retval->value.dbl   = value;
    return retval;
}

KvpValue *
kvp_value_new_numeric(gnc_numeric value)
{
    KvpValue * retval    = g_new0(KvpValue, 1);
    retval->type          = KVP_TYPE_NUMERIC;
    retval->value.numeric = value;
    return retval;
}

KvpValue *
kvp_value_new_string(const char * value)
{
    KvpValue * retval;
    if (!value) return NULL;

    retval = g_new0(KvpValue, 1);
    retval->type       = KVP_TYPE_STRING;
    retval->value.str  = g_strdup(value);
    return retval;
}

KvpValue *
kvp_value_new_guid(const GUID * value)
{
    KvpValue * retval;
    if (!value) return NULL;

    retval = g_new0(KvpValue, 1);
    retval->type       = KVP_TYPE_GUID;
    retval->value.guid = g_new0(GUID, 1);
    memcpy(retval->value.guid, value, sizeof(GUID));
    return retval;
}

KvpValue *
kvp_value_new_timespec(Timespec value)
{
    KvpValue * retval = g_new0(KvpValue, 1);
    retval->type       = KVP_TYPE_TIMESPEC;
    retval->value.timespec = value;
    return retval;
}

KvpValue *
kvp_value_new_gdate(GDate value)
{
    KvpValue * retval = g_new0(KvpValue, 1);
    retval->type       = KVP_TYPE_GDATE;
    retval->value.gdate = value;
    return retval;
}

KvpValue *
kvp_value_new_binary(const void * value, guint64 datasize)
{
    KvpValue * retval;
    if (!value) return NULL;

    retval = g_new0(KvpValue, 1);
    retval->type = KVP_TYPE_BINARY;
    retval->value.binary.data = g_new0(char, datasize);
    retval->value.binary.datasize = datasize;
    memcpy(retval->value.binary.data, value, datasize);
    return retval;
}

KvpValue *
kvp_value_new_binary_nc(void * value, guint64 datasize)
{
    KvpValue * retval;
    if (!value) return NULL;

    retval = g_new0(KvpValue, 1);
    retval->type = KVP_TYPE_BINARY;
    retval->value.binary.data = value;
    retval->value.binary.datasize = datasize;
    return retval;
}

KvpValue *
kvp_value_new_glist(const GList * value)
{
    KvpValue * retval;
    if (!value) return NULL;

    retval = g_new0(KvpValue, 1);
    retval->type       = KVP_TYPE_GLIST;
    retval->value.list = kvp_glist_copy(value);
    return retval;
}

KvpValue *
kvp_value_new_glist_nc(GList * value)
{
    KvpValue * retval;
    if (!value) return NULL;

    retval = g_new0(KvpValue, 1);
    retval->type       = KVP_TYPE_GLIST;
    retval->value.list = value;
    return retval;
}

KvpValue *
kvp_value_new_frame(const KvpFrame * value)
{
    KvpValue * retval;
    if (!value) return NULL;

    retval  = g_new0(KvpValue, 1);
    retval->type        = KVP_TYPE_FRAME;
    retval->value.frame = kvp_frame_copy(value);
    return retval;
}

KvpValue *
kvp_value_new_frame_nc(KvpFrame * value)
{
    KvpValue * retval;
    if (!value) return NULL;

    retval  = g_new0(KvpValue, 1);
    retval->type        = KVP_TYPE_FRAME;
    retval->value.frame = value;
    return retval;
}

void
kvp_value_delete(KvpValue * value)
{
    if (!value) return;

    switch (value->type)
    {
    case KVP_TYPE_STRING:
        g_free(value->value.str);
        break;
    case KVP_TYPE_GUID:
        g_free(value->value.guid);
        break;
    case KVP_TYPE_BINARY:
        g_free(value->value.binary.data);
        break;
    case KVP_TYPE_GLIST:
        kvp_glist_delete(value->value.list);
        break;
    case KVP_TYPE_FRAME:
        kvp_frame_delete(value->value.frame);
        break;

    case KVP_TYPE_GINT64:
    case KVP_TYPE_DOUBLE:
    case KVP_TYPE_NUMERIC:
    case KVP_TYPE_TIMESPEC:
    case KVP_TYPE_GDATE:
        break;
    }
    g_free(value);
}

KvpValueType
kvp_value_get_type(const KvpValue * value)
{
    if (!value) return -1;
    return value->type;
}

gint64
kvp_value_get_gint64(const KvpValue * value)
{
    if (!value) return 0;
    if (value->type == KVP_TYPE_GINT64)
    {
        return value->value.int64;
    }
    else
    {
        return 0;
    }
}

double
kvp_value_get_double(const KvpValue * value)
{
    if (!value) return 0.0;
    if (value->type == KVP_TYPE_DOUBLE)
    {
        return value->value.dbl;
    }
    else
    {
        return 0.0;
    }
}

gnc_numeric
kvp_value_get_numeric(const KvpValue * value)
{
    if (!value) return gnc_numeric_zero ();
    if (value->type == KVP_TYPE_NUMERIC)
    {
        return value->value.numeric;
    }
    else
    {
        return gnc_numeric_zero ();
    }
}

char *
kvp_value_get_string(const KvpValue * value)
{
    if (!value) return NULL;
    if (value->type == KVP_TYPE_STRING)
    {
        return value->value.str;
    }
    else
    {
        return NULL;
    }
}

GUID *
kvp_value_get_guid(const KvpValue * value)
{
    if (!value) return NULL;
    if (value->type == KVP_TYPE_GUID)
    {
        return value->value.guid;
    }
    else
    {
        return NULL;
    }
}

Timespec
kvp_value_get_timespec(const KvpValue * value)
{
    Timespec ts;
    ts.tv_sec = 0;
    ts.tv_nsec = 0;
    if (!value) return ts;
    if (value->type == KVP_TYPE_TIMESPEC)
        return value->value.timespec;
    else
        return ts;
}

GDate
kvp_value_get_gdate(const KvpValue * value)
{
    GDate date;
    g_date_clear(&date, 1);
    if (!value) return date;
    if (value->type == KVP_TYPE_GDATE)
        return value->value.gdate;
    else
        return date;
}

void *
kvp_value_get_binary(const KvpValue * value, guint64 * size_return)
{
    if (!value)
    {
        if (size_return)
            *size_return = 0;
        return NULL;
    }

    if (value->type == KVP_TYPE_BINARY)
    {
        if (size_return)
            *size_return = value->value.binary.datasize;
        return value->value.binary.data;
    }
    else
    {
        if (size_return)
            *size_return = 0;
        return NULL;
    }
}

GList *
kvp_value_get_glist(const KvpValue * value)
{
    if (!value) return NULL;
    if (value->type == KVP_TYPE_GLIST)
    {
        return value->value.list;
    }
    else
    {
        return NULL;
    }
}

KvpFrame *
kvp_value_get_frame(const KvpValue * value)
{
    if (!value) return NULL;
    if (value->type == KVP_TYPE_FRAME)
    {
        return value->value.frame;
    }
    else
    {
        return NULL;
    }
}

KvpFrame *
kvp_value_replace_frame_nc(KvpValue *value, KvpFrame * newframe)
{
    KvpFrame *oldframe;
    if (!value) return NULL;
    if (KVP_TYPE_FRAME != value->type) return NULL;

    oldframe = value->value.frame;
    value->value.frame = newframe;
    return oldframe;
}

GList *
kvp_value_replace_glist_nc(KvpValue * value, GList *newlist)
{
    GList *oldlist;
    if (!value) return NULL;
    if (KVP_TYPE_GLIST != value->type) return NULL;

    oldlist = value->value.list;
    value->value.list = newlist;
    return oldlist;
}

/* manipulators */

KvpValue *
kvp_value_copy(const KvpValue * value)
{
    if (!value) return NULL;

    switch (value->type)
    {
    case KVP_TYPE_GINT64:
        return kvp_value_new_gint64(value->value.int64);
        break;
    case KVP_TYPE_DOUBLE:
        return kvp_value_new_double(value->value.dbl);
        break;
    case KVP_TYPE_NUMERIC:
        return kvp_value_new_gnc_numeric(value->value.numeric);
        break;
    case KVP_TYPE_STRING:
        return kvp_value_new_string(value->value.str);
        break;
    case KVP_TYPE_GUID:
        return kvp_value_new_guid(value->value.guid);
        break;
    case KVP_TYPE_GDATE:
        return kvp_value_new_gdate(value->value.gdate);
        break;
    case KVP_TYPE_TIMESPEC:
        return kvp_value_new_timespec(value->value.timespec);
        break;
    case KVP_TYPE_BINARY:
        return kvp_value_new_binary(value->value.binary.data,
                                    value->value.binary.datasize);
        break;
    case KVP_TYPE_GLIST:
        return kvp_value_new_glist(value->value.list);
        break;
    case KVP_TYPE_FRAME:
        return kvp_value_new_frame(value->value.frame);
        break;
    }
    return NULL;
}

void
kvp_frame_for_each_slot(KvpFrame *f,
                        void (*proc)(const char *key,
                                     KvpValue *value,
                                     gpointer data),
                        gpointer data)
{
    if (!f) return;
    if (!proc) return;
    if (!(f->hash)) return;

    g_hash_table_foreach(f->hash, (GHFunc) proc, data);
}

#ifdef _MSC_VER
# define isnan _isnan
#endif
gint
double_compare(double d1, double d2)
{
    if (isnan(d1) && isnan(d2)) return 0;
    if (d1 < d2) return -1;
    if (d1 > d2) return 1;
    return 0;
}

gint
kvp_value_compare(const KvpValue * kva, const KvpValue * kvb)
{
    if (kva == kvb) return 0;
    /* nothing is always less than something */
    if (!kva && kvb) return -1;
    if (kva && !kvb) return 1;

    if (kva->type < kvb->type) return -1;
    if (kva->type > kvb->type) return 1;

    switch (kva->type)
    {
    case KVP_TYPE_GINT64:
        if (kva->value.int64 < kvb->value.int64) return -1;
        if (kva->value.int64 > kvb->value.int64) return 1;
        return 0;
        break;
    case KVP_TYPE_DOUBLE:
        return double_compare(kva->value.dbl, kvb->value.dbl);
        break;
    case KVP_TYPE_NUMERIC:
        return gnc_numeric_compare (kva->value.numeric, kvb->value.numeric);
        break;
    case KVP_TYPE_STRING:
        return strcmp(kva->value.str, kvb->value.str);
        break;
    case KVP_TYPE_GUID:
        return guid_compare(kva->value.guid, kvb->value.guid);
        break;
    case KVP_TYPE_TIMESPEC:
        return timespec_cmp(&(kva->value.timespec), &(kvb->value.timespec));
        break;
    case KVP_TYPE_GDATE:
        return g_date_compare(&(kva->value.gdate), &(kvb->value.gdate));
        break;
    case KVP_TYPE_BINARY:
        /* I don't know that this is a good compare. Ab is bigger than Acef.
           But I'm not sure that actually matters here. */
        if (kva->value.binary.datasize < kvb->value.binary.datasize) return -1;
        if (kva->value.binary.datasize > kvb->value.binary.datasize) return 1;
        return memcmp(kva->value.binary.data,
                      kvb->value.binary.data,
                      kva->value.binary.datasize);
        break;
    case KVP_TYPE_GLIST:
        return kvp_glist_compare(kva->value.list, kvb->value.list);
        break;
    case KVP_TYPE_FRAME:
        return kvp_frame_compare(kva->value.frame, kvb->value.frame);
        break;
    }
    PERR ("reached unreachable code.");
    return FALSE;
}

typedef struct
{
    gint compare;
    KvpFrame *other_frame;
} kvp_frame_cmp_status;

static void
kvp_frame_compare_helper(const char *key, KvpValue * val, gpointer data)
{
    kvp_frame_cmp_status *status = (kvp_frame_cmp_status *) data;
    if (status->compare == 0)
    {
        KvpFrame *other_frame = status->other_frame;
        KvpValue *other_val = kvp_frame_get_slot(other_frame, key);

        if (other_val)
        {
            status->compare = kvp_value_compare(val, other_val);
        }
        else
        {
            status->compare = 1;
        }
    }
}

gint
kvp_frame_compare(const KvpFrame *fa, const KvpFrame *fb)
{
    kvp_frame_cmp_status status;

    if (fa == fb) return 0;
    /* nothing is always less than something */
    if (!fa && fb) return -1;
    if (fa && !fb) return 1;

    /* nothing is always less than something */
    if (!fa->hash && fb->hash) return -1;
    if (fa->hash && !fb->hash) return 1;

    status.compare = 0;
    status.other_frame = (KvpFrame *) fb;

    kvp_frame_for_each_slot((KvpFrame *) fa, kvp_frame_compare_helper, &status);

    if (status.compare != 0)
        return status.compare;

    status.other_frame = (KvpFrame *) fa;

    kvp_frame_for_each_slot((KvpFrame *) fb, kvp_frame_compare_helper, &status);

    return(-status.compare);
}

gchar*
binary_to_string(const void *data, guint32 size)
{
    GString *output;
    guint32 i;
    guchar *data_str = (guchar*)data;

    output = g_string_sized_new(size * sizeof(char));

    for (i = 0; i < size; i++)
    {
        g_string_append_printf(output, "%02x", (unsigned int) (data_str[i]));
    }

    return output->str;
}

gchar*
kvp_value_glist_to_string(const GList *list)
{
    gchar *tmp1;
    gchar *tmp2;
    const GList *cursor;

    tmp1 = g_strdup_printf("[ ");

    for (cursor = list; cursor; cursor = cursor->next)
    {
        gchar *tmp3;

        tmp3 = kvp_value_to_string((KvpValue *)cursor->data);
        tmp2 = g_strdup_printf("%s %s,", tmp1, tmp3 ? tmp3 : "");
        g_free(tmp1);
        g_free(tmp3);
        tmp1 = tmp2;
    }

    tmp2 = g_strdup_printf("%s ]", tmp1);
    g_free(tmp1);

    return tmp2;
}

static void
kvp_frame_to_bare_string_helper(gpointer key, gpointer value, gpointer data)
{
    gchar **str = (gchar**)data;
    *str = g_strdup_printf("%s", kvp_value_to_bare_string((KvpValue *)value));
}

gchar*
kvp_value_to_bare_string(const KvpValue *val)
{
    gchar *tmp1;
    gchar *tmp2;
    const gchar *ctmp;

    g_return_val_if_fail(val, NULL);
    tmp1 = g_strdup("");
    switch (kvp_value_get_type(val))
    {
    case KVP_TYPE_GINT64:
        return g_strdup_printf("%" G_GINT64_FORMAT, kvp_value_get_gint64(val));
        break;

    case KVP_TYPE_DOUBLE:
        return g_strdup_printf("(%g)", kvp_value_get_double(val));
        break;

    case KVP_TYPE_NUMERIC:
        tmp1 = gnc_numeric_to_string(kvp_value_get_numeric(val));
        tmp2 = g_strdup_printf("%s", tmp1 ? tmp1 : "");
        g_free(tmp1);
        return tmp2;
        break;

    case KVP_TYPE_STRING:
        tmp1 = kvp_value_get_string (val);
        return g_strdup_printf("%s", tmp1 ? tmp1 : "");
        break;

    case KVP_TYPE_GUID:
        ctmp = guid_to_string(kvp_value_get_guid(val));
        tmp2 = g_strdup_printf("%s", ctmp ? ctmp : "");
        return tmp2;
        break;

    case KVP_TYPE_TIMESPEC:
    {
        time_t t;
        t = timespecToTime_t(kvp_value_get_timespec(val));
        qof_date_format_set(QOF_DATE_FORMAT_UTC);
        return qof_print_date(t);
        break;
    }
    case KVP_TYPE_BINARY:
    {
        guint64 len;
        void *data;
        data = kvp_value_get_binary(val, &len);
        tmp1 = binary_to_string(data, len);
        return g_strdup_printf("%s", tmp1 ? tmp1 : "");
    }
    break;

    case KVP_TYPE_GLIST:
        /* borked. kvp_value_glist_to_string is a debug fcn */
    {
        tmp1 = kvp_value_glist_to_string(kvp_value_get_glist(val));
        tmp2 = g_strdup_printf("%s", tmp1 ? tmp1 : "");
        g_free(tmp1);
        return tmp2;
        break;
    }
    case KVP_TYPE_FRAME:
    {
        KvpFrame *frame;

        frame = kvp_value_get_frame(val);
        if (frame->hash)
        {
            tmp1 = g_strdup("");
            g_hash_table_foreach(frame->hash, kvp_frame_to_bare_string_helper, &tmp1);
        }
        return tmp1;
    }
    case KVP_TYPE_GDATE:
        return g_strdup_printf("%04d-%02d-%02d",
                               g_date_get_year(&val->value.gdate),
                               g_date_get_month(&val->value.gdate),
                               g_date_get_day(&val->value.gdate));
    }
    g_assert(FALSE); /* must not be reached */
    return g_strdup("");
}

gchar*
kvp_value_to_string(const KvpValue *val)
{
    gchar *tmp1;
    gchar *tmp2;
    const gchar *ctmp;

    g_return_val_if_fail(val, NULL);

    switch (kvp_value_get_type(val))
    {
    case KVP_TYPE_GINT64:
        return g_strdup_printf("KVP_VALUE_GINT64(%" G_GINT64_FORMAT ")",
                               kvp_value_get_gint64(val));
        break;

    case KVP_TYPE_DOUBLE:
        return g_strdup_printf("KVP_VALUE_DOUBLE(%g)",
                               kvp_value_get_double(val));
        break;

    case KVP_TYPE_NUMERIC:
        tmp1 = gnc_numeric_to_string(kvp_value_get_numeric(val));
        tmp2 = g_strdup_printf("KVP_VALUE_NUMERIC(%s)", tmp1 ? tmp1 : "");
        g_free(tmp1);
        return tmp2;
        break;

    case KVP_TYPE_STRING:
        tmp1 = kvp_value_get_string (val);
        return g_strdup_printf("KVP_VALUE_STRING(%s)", tmp1 ? tmp1 : "");
        break;

    case KVP_TYPE_GUID:
        /* THREAD-UNSAFE */
        ctmp = guid_to_string(kvp_value_get_guid(val));
        tmp2 = g_strdup_printf("KVP_VALUE_GUID(%s)", ctmp ? ctmp : "");
        return tmp2;
        break;

    case KVP_TYPE_TIMESPEC:
        tmp1 = g_new0 (char, 40);
        gnc_timespec_to_iso8601_buff (kvp_value_get_timespec (val), tmp1);
        tmp2 = g_strdup_printf("KVP_VALUE_TIMESPEC(%s)", tmp1);
        g_free(tmp1);
        return tmp2;
        break;

    case KVP_TYPE_BINARY:
    {
        guint64 len;
        void *data;
        data = kvp_value_get_binary(val, &len);
        tmp1 = binary_to_string(data, len);
        return g_strdup_printf("KVP_VALUE_BINARY(%s)", tmp1 ? tmp1 : "");
    }
    break;

    case KVP_TYPE_GLIST:
        tmp1 = kvp_value_glist_to_string(kvp_value_get_glist(val));
        tmp2 = g_strdup_printf("KVP_VALUE_GLIST(%s)", tmp1 ? tmp1 : "");
        g_free(tmp1);
        return tmp2;
        break;

    case KVP_TYPE_FRAME:
        tmp1 = kvp_frame_to_string(kvp_value_get_frame(val));
        tmp2 = g_strdup_printf("KVP_VALUE_FRAME(%s)", tmp1 ? tmp1 : "");
        g_free(tmp1);
        return tmp2;
        break;

    case KVP_TYPE_GDATE:
        return g_strdup_printf("KVP_VALUE_GDATE(%04d-%02d-%02d)",
                               g_date_get_year(&val->value.gdate),
                               g_date_get_month(&val->value.gdate),
                               g_date_get_day(&val->value.gdate));
    }
    g_assert(FALSE); /* must not be reached */
    return g_strdup("");
}

static void
kvp_frame_to_string_helper(gpointer key, gpointer value, gpointer data)
{
    gchar *tmp_val;
    gchar **str = (gchar**)data;
    gchar *old_data = *str;

    tmp_val = kvp_value_to_string((KvpValue *)value);

    *str = g_strdup_printf("%s    %s => %s,\n",
                           *str ? *str : "",
                           key ? (char *) key : "",
                           tmp_val ? tmp_val : "");

    g_free(old_data);
    g_free(tmp_val);
}

gchar*
kvp_frame_to_string(const KvpFrame *frame)
{
    gchar *tmp1;

    g_return_val_if_fail (frame != NULL, NULL);

    tmp1 = g_strdup_printf("{\n");

    if (frame->hash)
        g_hash_table_foreach(frame->hash, kvp_frame_to_string_helper, &tmp1);

    {
        gchar *tmp2;
        tmp2 = g_strdup_printf("%s}\n", tmp1);
        g_free(tmp1);
        tmp1 = tmp2;
    }

    return tmp1;
}

GHashTable*
kvp_frame_get_hash(const KvpFrame *frame)
{
    g_return_val_if_fail (frame != NULL, NULL);
    return frame->hash;
}

/* ========================== END OF FILE ======================= */
