/********************************************************************\
 * kvp-value.hpp -- Implements a key-value frame system             *
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

#ifndef GNC_KVP_VALUE_TYPE
#define GNC_KVP_VALUE_TYPE

extern "C"
{
#include "config.h"
#include "qof.h"
}
#include <boost/version.hpp>
#if BOOST_VERSION == 105600
#include <boost/type_traits/is_nothrow_move_assignable.hpp>
#endif
#include <boost/variant.hpp>

struct KvpValueImpl
{
    public:
    /**
     * Performs a deep copy
     */
    KvpValueImpl(KvpValueImpl const &) noexcept;
    KvpValueImpl& operator=(const KvpValueImpl&) noexcept;

    /**
     * Move. The old object's datastore is set to int646_t 0.
     */
    KvpValueImpl(KvpValueImpl && b) noexcept;
    KvpValueImpl& operator=(KvpValueImpl && b) noexcept;

    template <typename T>
    KvpValueImpl(T) noexcept;

    /**
     * Performs a deep delete.
     *
     * The contents of this KvpValueImpl are also deleted.
     */
    ~KvpValueImpl() noexcept;

    /**
     * Replaces the frame within this KvpValueImpl.
     *
     * If this KvpValueImpl doesn't contain a KvpFrame, nullptr
     * is returned. Otherwise, the old KvpFrame * is returned.
     */
    KvpFrame * replace_frame_nc (KvpFrame *) noexcept;

    /**
     * Replaces the glist within this KvpValueImpl.
     *
     * If this KvpValueImpl doesn't contain a GList, nullptr
     * is returned. Otherwise, the old GList * is returned.
     */
    GList * replace_glist_nc (GList *) noexcept;

    /**
     * Adds another value to this KvpValueImpl.
     *
     * If this KvpValueImpl represents a collection (GList),
     * the new value is added to the collection and this
     * is returned.
     *
     * Otherwise, a new KvpValueImpl representing a collection
     * is created, this and the new value are added to it,
     * and it is returned.
     */
    KvpValueImpl * add (KvpValueImpl *) noexcept;

    KvpValueType get_type() const noexcept;

    char * to_string() const noexcept;

    template <typename T>
    T get() const noexcept;

    template <typename T>
    void set(T) noexcept;

    friend int compare(const KvpValueImpl &, const KvpValueImpl &) noexcept;

    private:
    void duplicate(const KvpValueImpl&) noexcept;
    boost::variant<
        int64_t,
        double,
        gnc_numeric,
        char *,
        GncGUID *,
        Timespec,
        GList *,
        KvpFrame *,
        GDate> datastore;
};

int
compare(const KvpValueImpl *, const KvpValue *) noexcept;

template <typename T>
KvpValueImpl::KvpValueImpl(T newvalue) noexcept:
    datastore(newvalue)
{
}

template <typename T> T
KvpValueImpl::get() const noexcept
{
    if (this->datastore.type() != typeid(T)) return {};
    return boost::get<T>(datastore);
}

template <typename T> void
KvpValueImpl::set(T val) noexcept
{
    this->datastore = val;
}

#endif
