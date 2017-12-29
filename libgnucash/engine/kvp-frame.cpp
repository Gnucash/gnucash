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
#include <config.h>
#include "qof.h"
#include <glib.h>
#include <stdarg.h>
#include <stdio.h>
#include <string.h>
}

#include "kvp-value.hpp"
#include "kvp-frame.hpp"
#include <typeinfo>
#include <sstream>
#include <algorithm>
#include <vector>
#include <numeric>

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = "qof.kvp";

static const char delim = '/';

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

KvpFrameImpl::~KvpFrameImpl() noexcept
{
    std::for_each(m_valuemap.begin(), m_valuemap.end(),
		 [](const map_type::value_type &a){
		      qof_string_cache_remove(a.first);
		      delete a.second;
		  }
	);
    m_valuemap.clear();
}

KvpFrame *
KvpFrame::get_child_frame_or_nullptr (Path const & path) noexcept
{
    if (!path.size ())
        return this;
    auto key = path.front ();
    if (m_valuemap.find (key.c_str ()) == m_valuemap.end ())
        return nullptr;
    auto child = m_valuemap.at (key.c_str ())->get <KvpFrame *> ();
    Path send;
    std::copy (path.begin () + 1, path.end (), std::back_inserter (send));
    return child->get_child_frame_or_nullptr (send);
}

KvpFrame *
KvpFrame::get_child_frame_or_create (Path const & path) noexcept
{
    if (!path.size ())
        return this;
    auto key = path.front ();
    auto spot = m_valuemap.find (key.c_str ());
    if (spot == m_valuemap.end () || spot->second->get_type () != KvpValue::Type::FRAME)
        delete set_impl (key.c_str (), new KvpValue {new KvpFrame});
    Path send;
    std::copy (path.begin () + 1, path.end (), std::back_inserter (send));
    auto child_val = m_valuemap.at (key.c_str ());
    auto child = child_val->get <KvpFrame *> ();
    return child->get_child_frame_or_create (send);
}


KvpValue *
KvpFrame::set_impl (std::string const & key, KvpValue * value) noexcept
{
    KvpValue * ret {};
    auto spot = m_valuemap.find (key.c_str ());
    if (spot != m_valuemap.end ())
    {
        qof_string_cache_remove (spot->first);
        ret = spot->second;
        m_valuemap.erase (spot);
    }
    if (value)
    {
        auto cachedkey = static_cast <char const *> (qof_string_cache_insert (key.c_str ()));
        m_valuemap.emplace (cachedkey, value);
    }
    return ret;
}

KvpValue *
KvpFrameImpl::set (Path path, KvpValue* value) noexcept
{
    auto key = path.back ();
    path.pop_back ();
    auto target = get_child_frame_or_nullptr (path);
    if (!target)
        return nullptr;
    return target->set_impl (key, value);
}

KvpValue *
KvpFrameImpl::set_path (Path path, KvpValue* value) noexcept
{
    auto key = path.back();
    path.pop_back();
    auto target = get_child_frame_or_create (path);
    if (!target)
        return nullptr;
    return target->set_impl (key, value);
}

KvpValue *
KvpFrameImpl::get_slot (Path path) noexcept
{
    auto key = path.back();
    path.pop_back();
    auto target = get_child_frame_or_nullptr (path);
    if (!target)
        return nullptr;
    auto spot = target->m_valuemap.find (key.c_str ());
    if (spot != target->m_valuemap.end ())
        return spot->second;
    return nullptr;
}

std::string
KvpFrameImpl::to_string() const noexcept
{
    return to_string("");
}

std::string
KvpFrameImpl::to_string(std::string const & prefix) const noexcept
{
    if (!m_valuemap.size())
        return prefix;
    std::ostringstream ret;
    std::for_each(m_valuemap.begin(), m_valuemap.end(),
        [this,&ret,&prefix](const map_type::value_type &a)
        {
            std::string new_prefix {prefix};
            if (a.first)
            {
                new_prefix += a.first;
                new_prefix += "/";
            }
            if (a.second)
                ret << a.second->to_string(new_prefix) << "\n";
            else
                ret << new_prefix << "(null)\n";
        }
    );
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

GValue*
gvalue_from_kvp_value (const KvpValue *kval)
{
    GValue *val;
    gnc_numeric num;
    Timespec tm;
    GDate gdate;

    if (kval == NULL) return NULL;
    val = g_slice_new0 (GValue);

    switch (kval->get_type())
    {
        case KvpValue::Type::INT64:
            g_value_init (val, G_TYPE_INT64);
            g_value_set_int64 (val, kval->get<int64_t>());
            break;
        case KvpValue::Type::DOUBLE:
            g_value_init (val, G_TYPE_DOUBLE);
            g_value_set_double (val, kval->get<double>());
            break;
        case KvpValue::Type::NUMERIC:
            g_value_init (val, GNC_TYPE_NUMERIC);
            num = kval->get<gnc_numeric>();
            g_value_set_boxed (val, &num);
            break;
        case KvpValue::Type::STRING:
            g_value_init (val, G_TYPE_STRING);
            g_value_set_string (val, kval->get<const char*>());
            break;
        case KvpValue::Type::GUID:
            g_value_init (val, GNC_TYPE_GUID);
            g_value_set_boxed (val, kval->get<GncGUID*>());
            break;
        case KvpValue::Type::TIMESPEC:
            g_value_init (val, GNC_TYPE_TIMESPEC);
            tm = kval->get<Timespec>();
            g_value_set_boxed (val, &tm);
            break;
        case KvpValue::Type::GDATE:
            g_value_init (val, G_TYPE_DATE);
            gdate = kval->get<GDate>();
            g_value_set_boxed (val, &gdate);
            break;
        case KvpValue::Type::GLIST:
        {
            GList *gvalue_list = NULL;
            GList *kvp_list = kval->get<GList*>();
            g_list_foreach (kvp_list, (GFunc)gvalue_list_from_kvp_value,
                            &gvalue_list);
            g_value_init (val, GNC_TYPE_VALUE_LIST);
            gvalue_list = g_list_reverse (gvalue_list);
            g_value_set_boxed (val, gvalue_list);
            break;
        }
/* No transfer of KVP frames outside of QofInstance-derived classes! */
        case KvpValue::Type::FRAME:
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
    GType type;
    if (gval == NULL)
        return NULL;
    type = G_VALUE_TYPE (gval);
    g_return_val_if_fail (G_VALUE_TYPE (gval), NULL);

    if (type == G_TYPE_INT64)
        val = new KvpValue(g_value_get_int64 (gval));
    else if (type == G_TYPE_DOUBLE)
        val = new KvpValue(g_value_get_double (gval));
    else if (type == G_TYPE_BOOLEAN)
    {
        auto bval = g_value_get_boolean(gval);
        if (bval)
            val = new KvpValue(g_strdup("true"));
    }
    else if (type == GNC_TYPE_NUMERIC)
        val = new KvpValue(*(gnc_numeric*)g_value_get_boxed (gval));
    else if (type == G_TYPE_STRING)
    {
        auto string = g_value_get_string(gval);
        if (string != nullptr)
            val = new KvpValue(g_strdup(string));
    }
    else if (type == GNC_TYPE_GUID)
    {
        auto boxed = g_value_get_boxed(gval);
        if (boxed != nullptr)
            val = new KvpValue(guid_copy(static_cast<GncGUID*>(boxed)));
    }
    else if (type == GNC_TYPE_TIMESPEC)
        val = new KvpValue(*(Timespec*)g_value_get_boxed (gval));
    else if (type == G_TYPE_DATE)
        val = new KvpValue(*(GDate*)g_value_get_boxed (gval));
    else if (type == GNC_TYPE_VALUE_LIST)
    {
        GList *gvalue_list = (GList*)g_value_get_boxed (gval);
        GList *kvp_list = NULL;
        g_list_foreach (gvalue_list, (GFunc)kvp_value_list_from_gvalue,
                        &kvp_list);
        kvp_list = g_list_reverse (kvp_list);
        val = new KvpValue(kvp_list);
//      g_list_free_full (gvalue_list, (GDestroyNotify)g_value_unset);
//      gvalue_list = NULL;
    }
    else
        PWARN ("Error! Don't know how to make a KvpValue from a %s",
               G_VALUE_TYPE_NAME (gval));

    return val;
}
/* The following are required for using KvpValue GLists as GValues */
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

void
KvpFrame::flatten_kvp_impl(std::vector <std::string> path, std::vector <KvpEntry> & entries) const noexcept
{
    for (auto const & entry : m_valuemap)
    {
        std::vector<std::string> new_path {path};
        new_path.push_back("/");
        if (entry.second->get_type() == KvpValue::Type::FRAME)
        {
            new_path.push_back(entry.first);
            entry.second->get<KvpFrame*>()->flatten_kvp_impl(new_path, entries);
        }
        else
        {
            new_path.emplace_back (entry.first);
            entries.emplace_back (new_path, entry.second);
        }
    }
}

std::vector <KvpEntry>
KvpFrame::flatten_kvp(void) const noexcept
{
    std::vector <KvpEntry> ret;
    flatten_kvp_impl({}, ret);
    return ret;
}
