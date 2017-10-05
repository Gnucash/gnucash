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
#include "kvp-frame.hpp"
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

KvpValue::Type
KvpValueImpl::get_type() const noexcept
{
    if (datastore.type() == typeid(int64_t))
        return KvpValue::Type::INT64;
    else if (datastore.type() == typeid(double))
        return KvpValue::Type::DOUBLE;
    else if (datastore.type() == typeid(gnc_numeric))
        return KvpValue::Type::NUMERIC;
    else if (datastore.type() == typeid(const gchar *))
        return KvpValue::Type::STRING;
    else if (datastore.type() == typeid(GncGUID *))
        return KvpValue::Type::GUID;
    else if (datastore.type() == typeid(Timespec))
        return KvpValue::Type::TIMESPEC;
    else if (datastore.type() == typeid(GList *))
        return KvpValue::Type::GLIST;
    else if (datastore.type() == typeid(KvpFrameImpl *))
        return KvpValue::Type::FRAME;
    else if (datastore.type() == typeid(GDate))
        return KvpValue::Type::GDATE;

    return KvpValue::Type::INVALID;
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
        output << val << " (64-bit int)";
    }

    void operator()(KvpFrame* val)
    {
        output << val->to_string();
    }

    void operator()(GDate val)
    {
        output << std::setw(4) << g_date_get_year(&val) << '-';
        output << std::setw(2) << g_date_get_month(&val) << '-';
        output << std::setw(2) << g_date_get_day(&val);
        output << " (gdate)";
    }

    void operator()(GList * val)
    {
        output << "KVP_VALUE_GLIST(";
        output << "[ ";
        /*Since val is passed by value, we can modify it*/
        for (;val; val = val->next)
        {
            auto realvalue = static_cast<const KvpValue *>(val->data);
            output << ' ' << realvalue->to_string() << ',';
        }
        output << " ]";
        output << ")";
    }

    void operator()(Timespec val)
    {
        char tmp1[40] {};
        gnc_timespec_to_iso8601_buff (val, tmp1);
        output << tmp1 << " (timespec)";
    }

    void operator()(gnc_numeric val)
    {
        auto tmp1 = gnc_numeric_to_string(val);
        if (tmp1)
        {
            output << tmp1;
            g_free(tmp1);
        }
        else
        {
            output << "(null)";
        }
        output << " (timespec)";
    }

    void operator()(GncGUID * val)
    {
        char guidstr[GUID_ENCODING_LENGTH+1];
        if (val)
        {
            guid_to_string_buff(val,guidstr);
            output << guidstr;
        }
        else
        {
            output << "(null)";
        }
        output << " (guid)";
    }

    void operator()(const char * val)
    {
        output << val << " (char *)";
    }

    void operator()(double val)
    {
        output << val << " (double)";
    }
};

std::string
KvpValueImpl::to_string(std::string const & prefix) const noexcept
{
    if (this->datastore.type() == typeid(KvpFrame*))
        return this->get<KvpFrame*>()->to_string(prefix);
    std::ostringstream ret;
    to_string_visitor visitor {ret};
    boost::apply_visitor(visitor, datastore);
    /*We still use g_strdup since the return value will be freed by g_free*/
    return prefix + ret.str();
}

std::string
KvpValueImpl::to_string() const noexcept
{
    return to_string("");
}

static int
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
        gint vcmp = compare(v1, v2);
        if (vcmp != 0) return vcmp;
        lp1 = lp1->next;
        lp2 = lp2->next;
    }
    if (!lp1 && lp2) return -1;
    if (!lp2 && lp1) return 1;
    return 0;
}

static GList *
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
        lptr->data = new KvpValue(*static_cast<KvpValue *>(lptr->data));
    }

    return retval;
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
template <> int compare_visitor::operator()(const char * const & one, const char * const & two) const
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
    return compare(one, two);
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

static void
destroy_value(void* item)
{
    delete static_cast<KvpValue*>(item);
}

template <> void
delete_visitor::operator()(GList * & value)
{
    g_list_free_full(value, destroy_value);
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
    delete value;
}

KvpValueImpl::~KvpValueImpl() noexcept
{
    delete_visitor d;
    boost::apply_visitor(d, datastore);
}

void
KvpValueImpl::duplicate(const KvpValueImpl& other) noexcept
{
    if (other.datastore.type() == typeid(const gchar *))
        this->datastore = g_strdup(other.get<const gchar *>());
    else if (other.datastore.type() == typeid(GncGUID*))
        this->datastore = guid_copy(other.get<GncGUID *>());
    else if (other.datastore.type() == typeid(GList*))
        this->datastore = kvp_glist_copy(other.get<GList *>());
    else if (other.datastore.type() == typeid(KvpFrame*))
        this->datastore = new KvpFrame(*other.get<KvpFrame *>());
    else
        this->datastore = other.datastore;
}

