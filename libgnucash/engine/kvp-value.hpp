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

//Must be a struct because it's exposed to C so that it can in turn be
//translated to/from Scheme.
/** @addtogroup KVP
 * @{
 */

/** Implements KvpValue using boost::variant. Capable of holding the following
 * types:
 * * int64_t
 * * double
 * * gnc_numeric
 * * const char*
 * * GncGUID*
 * * Timepsec
 * * GList*
 * * KvpFrame*
 * * GDate
 */

struct KvpValueImpl
{
    public:
    enum Type
    {
        INVALID = -1,
        INT64 = 1, /**< QOF_TYPE_INT64  gint64 */
        DOUBLE,     /**< QOF_TYPE_DOUBLE  gdouble */
        NUMERIC,    /**< QOF_TYPE_NUMERIC */
        STRING,     /**< QOF_TYPE_STRING gchar* */
        GUID,       /**< QOF_TYPE_GUID */
        TIMESPEC,   /**< QOF_TYPE_DATE */
        PLACEHOLDER_DONT_USE, /* Replaces KVP_TYPE_BINARY */
        GLIST,      /**< no QOF equivalent. */
        FRAME,      /**< no QOF equivalent. */
        GDATE,      /**< no QOF equivalent. */
    };

    /**
     * Performs a deep copy
     */
    KvpValueImpl(KvpValueImpl const &) noexcept;
    KvpValueImpl& operator=(const KvpValueImpl&) noexcept;

    /**
     * Move. The old object's datastore is set to int64_t 0.
     */
    KvpValueImpl(KvpValueImpl && b) noexcept;
    KvpValueImpl& operator=(KvpValueImpl && b) noexcept;

    /** Create a KvpValue containing the passed in item. Note that for pointer
     * types const char*, KvpFrame*, GncGUID*, and GList* the KvpValue takes
     * ownership of the objcet and will delete/free it when the KvpValue is
     * destroyed. That means these objects must be allocated in the free store
     * or heap as follows:
     * * const char*: GLib string allocation, e.g. g_strdup()/
     * * KvpFrame*: operator new
     * * GncGUID*: guid_new() or guid_copy()
     * * GList*: Uses g_list_free(), so it's up to classes using this to empty
         the list before destroying the KvpValue.
     */

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

    KvpValueImpl::Type get_type() const noexcept;

    std::string to_string() const noexcept;
    std::string to_string(std::string const & prefix) const noexcept;

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
        const char*,
        GncGUID *,
        Timespec,
        GList *,
        KvpFrame *,
        GDate> datastore;
};

int
compare(const KvpValueImpl *, const KvpValue *) noexcept;
/** @} Close Doxygen AddToGroup */
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
/** @ingroup KVP
    @{ */
/** @internal @{ */
/** Convert a kvp_value into a GValue. Frames aren't converted.
 * @param kval: A KvpValue.
 * @return GValue*. Must be freed with g_free().
 */
GValue* gvalue_from_kvp_value (const KvpValue *kval);

/** Convert a gvalue into a kvpvalue.
 * @param gval: A GValue of a type KvpValue can digest.
 * @return KvpValue created from the GValue's contents.
 */
KvpValue* kvp_value_from_gvalue (const GValue *gval);

/**
 * \brief Convenience function to release the value in a GValue
 * acquired by kvp_frame_get_gvalue and to free the GValue.
 * \param value: A GValue* created by kvp_frame_get_gvalue
 */
void gnc_gvalue_free (GValue *value);
/** @} Close Doxygen Internal */
/** @} Close Doxygen Group */
extern "C" GType gnc_value_list_get_type (void);
#define GNC_TYPE_VALUE_LIST (gnc_value_list_get_type ())

#endif
