/********************************************************************
 * kvp_frame.cpp -- Implements a key-value frame system             *
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

extern "C"
{
#include "config.h"
#include "qof.h"
#include <glib.h>
#include <stdarg.h>
#include <stdio.h>
#include <string.h>
}

#include "kvp-value.hpp"
#include "kvp_frame.hpp"
#include <typeinfo>
#include <sstream>
#include <algorithm>
#include <vector>

KvpFrameImpl::KvpFrameImpl(const KvpFrameImpl & rhs) noexcept
{
    std::for_each(rhs.m_valuemap.begin(), rhs.m_valuemap.end(),
        [this](const map_type::value_type & a)
        {
            auto key = static_cast<char *>(qof_string_cache_insert(a.first));
            auto val = new KvpValueImpl(*a.second);
            this->m_valuemap.insert({key,val});
        }
    );
}

KvpValueImpl * KvpFrameImpl::replace_nc(const char * key, KvpValueImpl * value) noexcept
{
    if (!key) return nullptr;
    auto spot = m_valuemap.find(key);
    KvpValueImpl * ret {nullptr};
    if (spot != m_valuemap.end())
    {
        qof_string_cache_remove(spot->first);
        ret = spot->second;
        m_valuemap.erase(spot);
    }

    if (value)
    {
        auto cachedkey = static_cast<const char *>(qof_string_cache_insert(key));
        m_valuemap.insert({cachedkey,value});
    }

    return ret;
}

std::string
KvpFrameImpl::to_string() const noexcept
{
    std::ostringstream ret;
    ret << "{\n";

    std::for_each(m_valuemap.begin(), m_valuemap.end(),
        [this,&ret](const map_type::value_type &a)
        {
            ret << "    ";
            if (a.first)
                ret << a.first;
            ret << " => ";
            if (a.second)
                ret << a.second->to_string();
            ret << ",\n";
        }
    );

    ret << "}\n";
    return ret.str();
}

std::vector<std::string>
KvpFrameImpl::get_keys() const noexcept
{
    std::vector<std::string> ret;
    std::for_each(m_valuemap.begin(), m_valuemap.end(),
        [&ret](const KvpFrameImpl::map_type::value_type &a)
        {
            ret.push_back(a.first);
        }
    );
    return ret;
}

void
KvpFrameImpl::for_each_slot(void (*proc)(const char *key, KvpValue *value, void * data),
              void *data) const noexcept
{
    if (!proc) return;
    std::for_each (m_valuemap.begin(), m_valuemap.end(),
        [proc,data](const KvpFrameImpl::map_type::value_type & a)
        {
            proc (a.first, a.second, data);
        }
    );
}

KvpValueImpl *
KvpFrameImpl::get_slot(const char * key) const noexcept
{
    auto spot = m_valuemap.find(key);
    if (spot == m_valuemap.end())
        return nullptr;
    return spot->second;
}

int compare(const KvpFrameImpl * one, const KvpFrameImpl * two) noexcept
{
    if (one && !two) return 1;
    if (!one && two) return -1;
    if (!one && !two) return 0;
    return compare(*one, *two);
}

/**
 * If the first KvpFrameImpl has an item that the second does not, 1 is returned.
 * The first item within the two KvpFrameImpl that is not similar, that comparison is returned.
 * If all the items within the first KvpFrameImpl match items within the second, but the
 *   second has more elements, -1 is returned.
 * Otherwise, 0 is returned.
 */
int compare(const KvpFrameImpl & one, const KvpFrameImpl & two) noexcept
{
    for (const auto & a : one.m_valuemap)
    {
        auto otherspot = two.m_valuemap.find(a.first);
        if (otherspot == two.m_valuemap.end())
        {
            return 1;
        }
        auto comparison = compare(a.second,otherspot->second);

        if (comparison != 0)
            return comparison;
    }

    if (one.m_valuemap.size() < two.m_valuemap.size())
        return -1;
    return 0;
}

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = QOF_MOD_KVP;

KvpFrame *
kvp_frame_new(void)
{
    auto ret = new KvpFrameImpl();
    return static_cast<KvpFrame *>(ret);
}

void
kvp_frame_delete(KvpFrame * frame)
{
    if (!frame) return;
    auto realframe = static_cast<KvpFrameImpl *>(frame);
    delete realframe;
}

const char **
kvp_frame_get_keys(const KvpFrame * frame)
{
    if (!frame) return nullptr;
    auto realframe = static_cast<KvpFrameImpl const *>(frame);
    const auto & keys = realframe->get_keys();
    const char ** ret = g_new(const char *, keys.size() + 1);
    unsigned int spot {0};
    std::for_each(keys.begin(), keys.end(),
        [&ret,&spot](const std::string &a)
        {
            ret[spot++] = g_strdup(a.c_str());
        }
    );
    ret[keys.size()] = nullptr;
    return ret;
}

gboolean
kvp_frame_is_empty(const KvpFrame * frame)
{
    if (!frame) return TRUE;
    auto realframe = static_cast<KvpFrameImpl const *>(frame);
    return realframe->get_keys().size() == 0;
}

KvpFrame *
kvp_frame_copy(const KvpFrame * frame)
{
    auto ret = new KvpFrameImpl(*static_cast<KvpFrameImpl const *>(frame));
    return ret;
}

/* Replace the old value with the new value.  Return the old value.
 * Passing in a null value into this routine has the effect of
 * removing the key from the KVP tree.
 */
static KvpValue *
kvp_frame_replace_slot_nc (KvpFrame * frame, const char * slot,
                           KvpValue * new_value)
{
    if (!frame) return NULL;

    auto realframe = static_cast<KvpFrameImpl *>(frame);
    auto realnewvalue = static_cast<KvpValueImpl *>(new_value);
    return realframe->replace_nc(slot, realnewvalue);
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

    last_key = strrchr (const_cast<char*>(key_path), '/');
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

    last_key = strrchr (const_cast<char*>(key_path), '/');
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
kvp_frame_set_guid(KvpFrame * frame, const char * path, const GncGUID *guid)
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

static KvpFrame *
kvp_frame_add_value_nc(KvpFrame * frame, const char * path, KvpValue *value)
{
    char *key = NULL;
    KvpValueImpl * oldvalue;
    KvpFrame* orig_frame = frame;

    frame = (KvpFrame *) get_trailer_or_null (frame, path, &key);
    oldvalue = static_cast<KvpValueImpl *> (kvp_frame_get_slot (frame, key));
    auto newvalue = static_cast<KvpValueImpl *> (value);

    ENTER ("old frame=%s", kvp_frame_to_string(frame));
    if (oldvalue)
    {
        kvp_frame_replace_slot_nc (frame, key, oldvalue->add (newvalue));
        LEAVE ("new frame=%s", kvp_frame_to_string (frame));
        return frame;
    }

    /* Hmm, if we are here, the path doesn't exist. We need to
     * create the path, add the value to it. */
    frame = orig_frame;
    frame = kvp_frame_set_value_nc (frame, path, value);
    LEAVE ("new frame=%s", kvp_frame_to_string(frame));
    return frame;
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
                   KvpValue * value)
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
    if (!frame) return NULL;
    auto realframe = static_cast<const KvpFrameImpl *>(frame);
    return realframe->get_slot(slot);
}

void
kvp_frame_set_slot_path (KvpFrame *frame,
                         KvpValue * new_value,
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
                                KvpValue * new_value,
                                GSList *key_path)
{
    if (!frame || !key_path) return;

    while (TRUE)
    {
        const char *key = static_cast<char*>(key_path->data);
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

GncGUID *
kvp_frame_get_guid(const KvpFrame *frame, const char *path)
{
    char *key = NULL;
    frame = get_trailer_or_null (frame, path, &key);
    return kvp_value_get_guid(kvp_frame_get_slot (frame, key));
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
        const char *key = static_cast<const char*>(key_path->data);
        KvpValue *value;

        if (!key) break;

        value = kvp_frame_get_slot (frame, key);
        if (!value) break;

        key_path = key_path->next;
        if (!key_path) return value;

        frame = kvp_value_get_frame (value);
        if (!frame) break;
    }
    return NULL;
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
        KvpValue *val = static_cast<KvpValue*>(node->data);
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
        lptr->data = kvp_value_copy(static_cast<KvpValue *>(lptr->data));
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
kvp_value_new_gint64(int64_t value)
{
    return new KvpValueImpl{value};
}

KvpValue *
kvp_value_new_double(double value)
{
    return new KvpValueImpl{value};
}

KvpValue *
kvp_value_new_numeric(gnc_numeric value)
{
    return new KvpValueImpl{value};
}

KvpValue *
kvp_value_new_string(const char * value)
{
    if (!value) return {};
    return new KvpValueImpl{g_strdup(value)};
}

KvpValue *
kvp_value_new_guid(const GncGUID * value)
{
    if (!value) return {};
    return new KvpValueImpl{guid_copy(value)};
}

KvpValue *
kvp_value_new_timespec(Timespec value)
{
    return new KvpValueImpl{value};
}

KvpValue *
kvp_value_new_gdate(GDate value)
{
    return new KvpValueImpl{value};
}

KvpValue *
kvp_value_new_glist(const GList * value)
{
    if (!value) return {};
    return new KvpValueImpl{kvp_glist_copy(value)};
}

KvpValue *
kvp_value_new_glist_nc(GList * value)
{
    if (!value) return {};
    return new KvpValueImpl{value};
}

KvpValue *
kvp_value_new_frame(const KvpFrame * value)
{
    if (!value) return {};
    return new KvpValueImpl{kvp_frame_copy(value)};
}

KvpValue *
kvp_value_new_frame_nc(KvpFrame * value)
{
    if (!value) return {};
    return new KvpValueImpl{value};
}

void
kvp_value_delete(KvpValue * value)
{
    if (!value) return;
    KvpValueImpl * realvalue {static_cast<KvpValueImpl *>(value)};
    delete realvalue;
}

KvpValueType
kvp_value_get_type(const KvpValue * oldval)
{
    if (!oldval) return KVP_TYPE_INVALID;
    const KvpValueImpl * value {static_cast<const KvpValueImpl *>(oldval)};
    return value->get_type();
}

int64_t
kvp_value_get_gint64(const KvpValue * ovalue)
{
    if (!ovalue) return {};
    const KvpValueImpl * value {static_cast<const KvpValueImpl *>(ovalue)};
    return value->get<int64_t>();
}

double
kvp_value_get_double(const KvpValue * ovalue)
{
    if (!ovalue) return {};
    const KvpValueImpl * value {static_cast<const KvpValueImpl *>(ovalue)};
    return value->get<double>();
}

gnc_numeric
kvp_value_get_numeric(const KvpValue * ovalue)
{
    //if (!ovalue) return {}; The code depends on no segfault and zero being returned here.
    if (!ovalue) return gnc_numeric_zero();
    const KvpValueImpl * value {static_cast<const KvpValueImpl *>(ovalue)};
    return value->get<gnc_numeric>();
}

char *
kvp_value_get_string(const KvpValue * ovalue)
{
    if (!ovalue) return {};
    const KvpValueImpl * value {static_cast<const KvpValueImpl *>(ovalue)};
    return value->get<char*>();
}

GncGUID *
kvp_value_get_guid(const KvpValue * ovalue)
{
    if (!ovalue) return {};
    const KvpValueImpl * value {static_cast<const KvpValueImpl *>(ovalue)};
    return value->get<GncGUID*>();
}

Timespec
kvp_value_get_timespec(const KvpValue * ovalue)
{
    if (!ovalue) return {};
    const KvpValueImpl * value {static_cast<const KvpValueImpl *>(ovalue)};
    return value->get<Timespec>();
}

GDate
kvp_value_get_gdate(const KvpValue * ovalue)
{
    if (!ovalue) return {};
    const KvpValueImpl * value {static_cast<const KvpValueImpl *>(ovalue)};
    return value->get<GDate>();
}

GList *
kvp_value_get_glist(const KvpValue * ovalue)
{
    if (!ovalue) return {};
    const KvpValueImpl * value {static_cast<const KvpValueImpl *>(ovalue)};
    return value->get<GList*>();
}

KvpFrame *
kvp_value_get_frame(const KvpValue * ovalue)
{
    if (!ovalue) return {};
    const KvpValueImpl * value {static_cast<const KvpValueImpl *>(ovalue)};
    return value->get<KvpFrame*>();
}

KvpFrame *
kvp_value_replace_frame_nc(KvpValue * ovalue, KvpFrame * newframe)
{
    if (!ovalue) return {};
    KvpValueImpl * value {static_cast<KvpValueImpl *>(ovalue)};
    return value->replace_frame_nc (newframe);
}

GList *
kvp_value_replace_glist_nc(KvpValue * ovalue, GList *newlist)
{
    if (!ovalue) return {};
    KvpValueImpl * value {static_cast<KvpValueImpl *>(ovalue)};
    return value->replace_glist_nc (newlist);
}

KvpValue *
kvp_value_copy(const KvpValue * ovalue)
{
    if (!ovalue) return {};
    auto value = static_cast<const KvpValueImpl *>(ovalue);
    KvpValueImpl * ret = new KvpValueImpl(*value);
    return static_cast<KvpValue *>(ret);
}

void
kvp_frame_for_each_slot(KvpFrame *f,
                        void (*proc)(const char *key,
                                     KvpValue *value,
                                     gpointer data),
                        gpointer data)
{
    if (!f) return;
    auto realframe = static_cast<KvpFrameImpl *>(f);
    realframe->for_each_slot(proc, data);
}

int
kvp_value_compare(const KvpValue * okva, const KvpValue * okvb)
{
    auto kva = static_cast<const KvpValueImpl *>(okva);
    auto kvb = static_cast<const KvpValueImpl *>(okvb);
    return compare(kva, kvb);
}

gint
kvp_frame_compare(const KvpFrame *fa, const KvpFrame *fb)
{
    auto realone = static_cast<const KvpFrameImpl *>(fa);
    auto realtwo = static_cast<const KvpFrameImpl *>(fb);
    return compare(realone, realtwo);
}

char *
kvp_value_to_string(const KvpValue * val)
{
    if (!val) return g_strdup("");
    auto realval = static_cast<const KvpValueImpl *>(val);
    return realval->to_string();
}

#ifdef __cplusplus
extern "C"
{
#endif

void init_static_test_pointers( void );

#ifdef __cplusplus
}
#endif

KvpFrame* ( *p_get_trailer_make )( KvpFrame *frame, const char *key_path, char **end_key );
KvpFrame* ( *p_get_or_make )( KvpFrame *fr, const char * key );
const KvpFrame* ( *p_kvp_frame_get_frame_or_null_slash_trash )( const KvpFrame *frame, char *key_path );
const KvpFrame* ( *p_get_trailer_or_null )( const KvpFrame * frame, const char * key_path, char **end_key );

void
init_static_test_pointers( void )
{
    p_get_trailer_make = get_trailer_make;
    p_get_or_make = get_or_make;
    p_kvp_frame_get_frame_or_null_slash_trash = kvp_frame_get_frame_or_null_slash_trash;
    p_get_trailer_or_null = get_trailer_or_null;
}

char*
kvp_frame_to_string(const KvpFrame *frame)
{
    auto realframe = static_cast<KvpFrameImpl const *>(frame);
    /*We'll use g_strdup
     because it will be freed using g_free.*/
    return g_strdup(realframe->to_string().c_str());
}

static GValue *gvalue_from_kvp_value (KvpValue *);
static KvpValue *kvp_value_from_gvalue (const GValue*);

static void
gvalue_list_from_kvp_value (KvpValue *kval, gpointer pList)
{
    GList **gvlist = NULL;
    GValue *gval = gvalue_from_kvp_value (kval);
    gvlist =  (GList**)pList;
    if (G_VALUE_TYPE (gval))
	*gvlist = g_list_prepend (*gvlist, gval);
}

static void
kvp_value_list_from_gvalue (GValue *gval, gpointer pList)
{
    GList **kvplist = (GList**)pList;
    KvpValue *kvp;
    if (!(gval && G_VALUE_TYPE (gval)))
	return;
    kvp = kvp_value_from_gvalue (gval);
    *kvplist = g_list_prepend (*kvplist, kvp);
}

static GValue*
gvalue_from_kvp_value (KvpValue *kval)
{
    GValue *val;
    gnc_numeric num;
    Timespec tm;
    GDate gdate;

    if (kval == NULL) return NULL;
    val = g_slice_new0 (GValue);

    switch (kvp_value_get_type(kval))
    {
	case KVP_TYPE_GINT64:
	    g_value_init (val, G_TYPE_INT64);
	    g_value_set_int64 (val, kvp_value_get_gint64 (kval));
	    break;
	case KVP_TYPE_DOUBLE:
	    g_value_init (val, G_TYPE_DOUBLE);
	    g_value_set_double (val, kvp_value_get_double (kval));
	    break;
	case KVP_TYPE_NUMERIC:
	    g_value_init (val, GNC_TYPE_NUMERIC);
	    num = kvp_value_get_numeric (kval);
	    g_value_set_boxed (val, &num);
	    break;
	case KVP_TYPE_STRING:
	    g_value_init (val, G_TYPE_STRING);
	    g_value_set_string (val, kvp_value_get_string (kval));
	    break;
	case KVP_TYPE_GUID:
	    g_value_init (val, GNC_TYPE_GUID);
	    g_value_set_boxed (val, kvp_value_get_guid (kval));
	    break;
	case KVP_TYPE_TIMESPEC:
	    g_value_init (val, GNC_TYPE_TIMESPEC);
	    tm = kvp_value_get_timespec (kval);
	    g_value_set_boxed (val, &tm);
	    break;
	case KVP_TYPE_GDATE:
	    g_value_init (val, G_TYPE_DATE);
	    gdate = kvp_value_get_gdate (kval);
	    g_value_set_boxed (val, &gdate);
	    break;
	case KVP_TYPE_GLIST:
	{
	    GList *gvalue_list = NULL;
	    GList *kvp_list = kvp_value_get_glist (kval);
	    g_list_foreach (kvp_list, (GFunc)gvalue_list_from_kvp_value, &gvalue_list);
	    g_value_init (val, GNC_TYPE_VALUE_LIST);
	    gvalue_list = g_list_reverse (gvalue_list);
	    g_value_set_boxed (val, gvalue_list);
	    break;
	}
/* No transfer of KVP frames outside of QofInstance-derived classes! */
	case KVP_TYPE_FRAME:
	    PWARN ("Error! Attempt to transfer KvpFrame!");
	default:
	    PWARN ("Error! Invalid KVP Transfer Request!");
	    g_slice_free (GValue, val);
	    val = NULL;
	    break;
    }
    return val;
}

KvpValue*
kvp_value_from_gvalue (const GValue *gval)
{
    KvpValue *val = NULL;
    GType type = G_VALUE_TYPE (gval);
    g_return_val_if_fail (G_VALUE_TYPE (gval), NULL);

    if (type == G_TYPE_INT64)
	val = kvp_value_new_gint64 (g_value_get_int64 (gval));
    else if (type == G_TYPE_DOUBLE)
	val = kvp_value_new_double (g_value_get_double (gval));
    else if (type == GNC_TYPE_NUMERIC)
	val = kvp_value_new_numeric (*(gnc_numeric*)g_value_get_boxed (gval));
    else if (type == G_TYPE_STRING)
	val = kvp_value_new_string (g_value_get_string (gval));
    else if (type == GNC_TYPE_GUID)
	val = kvp_value_new_guid ((GncGUID*)g_value_get_boxed (gval));
    else if (type == GNC_TYPE_TIMESPEC)
	val = kvp_value_new_timespec (*(Timespec*)g_value_get_boxed (gval));
    else if (type == G_TYPE_DATE)
	val = kvp_value_new_gdate (*(GDate*)g_value_get_boxed (gval));
    else if (type == GNC_TYPE_VALUE_LIST)
    {
	GList *gvalue_list = (GList*)g_value_get_boxed (gval);
	GList *kvp_list = NULL;
	g_list_foreach (gvalue_list, (GFunc)kvp_value_list_from_gvalue, &kvp_list);
	kvp_list = g_list_reverse (kvp_list);
	val = kvp_value_new_glist_nc (kvp_list);
//	g_list_free_full (gvalue_list, (GDestroyNotify)g_value_unset);
//	gvalue_list = NULL;
    }
    else
	PWARN ("Error! Don't know how to make a KvpValue from a %s",
	       G_VALUE_TYPE_NAME (gval));

    return val;
}

GValue*
kvp_frame_get_gvalue (KvpFrame *frame, const gchar *key)
{
    KvpValue *kval = kvp_frame_get_value (frame, key);
    GValue *value = gvalue_from_kvp_value (kval);
    return value;
}

void
kvp_frame_set_gvalue (KvpFrame *frame, const gchar *key, const GValue *value)
{
  kvp_frame_set_value_nc (frame, key, kvp_value_from_gvalue (value));
}

static void
gnc_gvalue_copy (GValue *src, gpointer uData)
{
    GList **new_list = (GList**)uData;
    GValue *dest = g_value_init (g_slice_new0 (GValue), G_VALUE_TYPE (src));
    g_value_copy (src, dest);
    *new_list = g_list_prepend(*new_list, dest);
}

void
gnc_gvalue_free (GValue *val)
{
    if (val == NULL || ! G_IS_VALUE (val)) return;
    g_value_unset (val);
    g_slice_free (GValue, val);
}

static GList*
gnc_value_list_copy (GList *list)
{
    GList *new_list = NULL;
    g_list_foreach (list, (GFunc)gnc_gvalue_copy, &new_list);
    new_list = g_list_reverse (new_list);
    return new_list;
}

static void
gnc_value_list_free (GList *list)
{
    g_list_free_full (list, (GDestroyNotify)gnc_gvalue_free);
}

GType
gnc_value_list_get_type (void)
{
    static GType type = 0;
    if (type == 0)
    {
	type = g_boxed_type_register_static ("gnc_value_list",
					     (GBoxedCopyFunc)gnc_value_list_copy,
					     (GBoxedFreeFunc)gnc_value_list_free);
    }
    return type;
}

/* ========================== END OF FILE ======================= */
