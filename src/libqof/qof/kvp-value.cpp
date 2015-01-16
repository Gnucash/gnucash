/********************************************************************\
 * kvp-value.cpp -- Implements a key-value frame system             *
 * Copyright (C) 2014 Aaron Laws                                    *
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

#include "kvp-value.hpp"
#include "kvp_frame.hpp"
#include <cmath>
#include <sstream>
#include <iomanip>
#include <stdexcept>

KvpValueImpl::KvpValueImpl(KvpValueImpl const & other) noexcept
{
    duplicate(other);
}

KvpValueImpl&
KvpValueImpl::operator=(KvpValueImpl const & other) noexcept
{
    duplicate(other);
    return *this;
}

KvpValueImpl::KvpValueImpl(KvpValueImpl && b) noexcept
{
    datastore = b.datastore;
    b.datastore = INT64_C(0);
}

KvpValueImpl&
KvpValueImpl::operator=(KvpValueImpl && b) noexcept
{
    std::swap (datastore, b.datastore);
    return *this;
}

KvpValueImpl *
KvpValueImpl::add(KvpValueImpl * val) noexcept
{
    /* If already a glist here, just append */
    if (this->datastore.type() == typeid(GList*))
    {
        GList * list = boost::get<GList*>(datastore);
        datastore = g_list_append (list, val);
        return this;
    }
    /* If some other value, convert it to a glist */
    GList *list = nullptr;

    list = g_list_append (list, this);
    list = g_list_append (list, val);
    return new KvpValueImpl(list);
}

KvpValueType
KvpValueImpl::get_type() const noexcept
{
    if (datastore.type() == typeid(int64_t))
        return KvpValueType::KVP_TYPE_GINT64;
    else if (datastore.type() == typeid(double))
        return KvpValueType::KVP_TYPE_DOUBLE;
    else if (datastore.type() == typeid(gnc_numeric))
        return KvpValueType::KVP_TYPE_NUMERIC;
    else if (datastore.type() == typeid(gchar *))
        return KvpValueType::KVP_TYPE_STRING;
    else if (datastore.type() == typeid(GncGUID *))
        return KvpValueType::KVP_TYPE_GUID;
    else if (datastore.type() == typeid(Timespec))
        return KvpValueType::KVP_TYPE_TIMESPEC;
    else if (datastore.type() == typeid(GList *))
        return KvpValueType::KVP_TYPE_GLIST;
    else if (datastore.type() == typeid(KvpFrameImpl *))
        return KvpValueType::KVP_TYPE_FRAME;
    else if (datastore.type() == typeid(GDate))
        return KvpValueType::KVP_TYPE_GDATE;

    return KVP_TYPE_INVALID;
}

KvpFrame *
KvpValueImpl::replace_frame_nc (KvpFrame * new_value) noexcept
{
    if (datastore.type() != typeid(KvpFrame *))
        return {};
    auto ret = boost::get<KvpFrame *>(datastore);
    datastore = new_value;
    return ret;
}

GList *
KvpValueImpl::replace_glist_nc (GList * new_value) noexcept
{
    if (datastore.type() != typeid(GList *))
        return {};
    auto ret = boost::get<GList *>(datastore);
    datastore = new_value;
    return ret;
}

struct to_string_visitor : boost::static_visitor<void>
{
    std::ostringstream & output;

    to_string_visitor(std::ostringstream & val) : output(val){}

    void operator()(int64_t val)
    {
        output << "KVP_VALUE_GINT64(" << val << ")";
    }

    void operator()(KvpFrame * val)
    {
        auto tmp1 = kvp_frame_to_string(val);
        output << "KVP_VALUE_FRAME(";
        if (tmp1)
        {
            output << tmp1;
            g_free(tmp1);
        }
        output << ")";
    }

    void operator()(GDate val)
    {
        output << "KVP_VALUE_GDATE(";
        output << std::setw(4) << g_date_get_year(&val) << '-';
        output << std::setw(2) << g_date_get_month(&val) << '-';
        output << std::setw(2) << g_date_get_day(&val) << ')';
    }

    void operator()(GList * val)
    {
        output << "KVP_VALUE_GLIST(";
        output << "[ ";

        /*Since val is passed by value, we can modify it*/
        for (;val; val = val->next)
        {
            gchar *tmp3;
            auto realvalue = static_cast<const KvpValue *>(val->data);
            tmp3 = realvalue->to_string();
            output << ' ';
            if (tmp3)
            {
                output << tmp3;
                g_free(tmp3);
            }
            output << ',';
        }

        output << " ]";
        output << ")";
    }

    void operator()(Timespec val)
    {
        char tmp1[40] {};
        gnc_timespec_to_iso8601_buff (val, tmp1);
        output << "KVP_VALUE_TIMESPEC(" << tmp1 << ")";
    }

    void operator()(gnc_numeric val)
    {
        auto tmp1 = gnc_numeric_to_string(val);
        output << "KVP_VALUE_NUMERIC(";
        if (tmp1)
        {
            output << tmp1;
            g_free(tmp1);
        }
        output << ")";
    }

    void operator()(GncGUID * val)
    {
        char guidstr[GUID_ENCODING_LENGTH+1];
        output << "KVP_VALUE_GUID(";
        if (val)
        {
            guid_to_string_buff(val,guidstr);
            output << guidstr;
        }
        output << ")";
    }

    void operator()(char * val)
    {
        output << "KVP_VALUE_STRING(" << val << ")";
    }

    void operator()(double val)
    {
        output << "KVP_VALUE_DOUBLE(" << val << ")";
    }
};

char *
KvpValueImpl::to_string() const noexcept
{
    std::ostringstream ret;
    to_string_visitor visitor {ret};
    boost::apply_visitor(visitor, datastore);

    /*We still use g_strdup since the return value will be freed by g_free*/
    return g_strdup(ret.str().c_str());
}

struct compare_visitor : boost::static_visitor<int>
{
    template <typename T, typename U>
    int operator()( T& one, U& two) const
    {
        throw std::invalid_argument{"You may not compare objects of different type."};
    }

    template <typename T>
    int operator()( T & one, T & two) const
    {
        /*This will work for any type that implements < and ==.*/
        if (one < two)
            return -1;
        if (two < one)
            return 1;
        return 0;
    }
};
template <> int compare_visitor::operator()(char * const & one, char * const & two) const
{
    return strcmp(one, two);
}
template <> int compare_visitor::operator()(gnc_numeric const & one, gnc_numeric const & two) const
{
    return gnc_numeric_compare(one, two);
}
template <> int compare_visitor::operator()(GncGUID * const & one, GncGUID * const & two) const
{
    return guid_compare(one, two);
}
template <> int compare_visitor::operator()(Timespec const & one, Timespec const & two) const
{
    return timespec_cmp(&one,&two);
}
template <> int compare_visitor::operator()(GDate const & one, GDate const & two) const
{
    return g_date_compare(&one,&two);
}
template <> int compare_visitor::operator()(GList * const & one, GList * const & two) const
{
    return kvp_glist_compare(one, two);
}
template <> int compare_visitor::operator()(KvpFrame * const & one, KvpFrame * const & two) const
{
    return kvp_frame_compare(one, two);
}
template <> int compare_visitor::operator()(double const & one, double const & two) const
{
    if (std::isnan(one) && std::isnan(two))
        return 0;
    if (one < two) return -1;
    if (two < one) return 1;
    return 0;
}

int compare(const KvpValueImpl & one, const KvpValueImpl & two) noexcept
{
    auto type1 = one.get_type();
    auto type2 = two.get_type();

    if (type1 != type2)
        return type1 < type2 ? -1 : 1;

    compare_visitor comparer;
    return boost::apply_visitor(comparer, one.datastore, two.datastore);
}

int
compare(const KvpValueImpl * one, const KvpValueImpl * two) noexcept
{
    if (one == two) return 0;
    if (one && !two) return 1;
    if (!one && two) return -1;
    return compare(*one, *two);
}

struct delete_visitor : boost::static_visitor<void>
{
    template <typename T> void 
    operator()(T &) { /*do nothing*/ }
};

template <> void
delete_visitor::operator()(GList * & value)
{
    kvp_glist_delete(value);
}
template <> void
delete_visitor::operator()(gchar * & value)
{
    g_free(value);
}
template <> void
delete_visitor::operator()(GncGUID * & value)
{
    guid_free(value);
}
template <> void
delete_visitor::operator()(KvpFrame * & value)
{
    kvp_frame_delete(value);
}

KvpValueImpl::~KvpValueImpl() noexcept
{
    delete_visitor d;
    boost::apply_visitor(d, datastore);
}

void
KvpValueImpl::duplicate(const KvpValueImpl& other) noexcept
{
    if (other.datastore.type() == typeid(gchar *))
        this->datastore = g_strdup(other.get<gchar *>());
    else if (other.datastore.type() == typeid(GncGUID*))
        this->datastore = guid_copy(other.get<GncGUID *>());
    else if (other.datastore.type() == typeid(GList*))
        this->datastore = kvp_glist_copy(other.get<GList *>());
    else if (other.datastore.type() == typeid(KvpFrame*))
        this->datastore = kvp_frame_copy(other.get<KvpFrame *>());
    else
        this->datastore = other.datastore;
}
