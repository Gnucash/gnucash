/********************************************************************\
 * kvp-frame.hpp -- Implements a key-value frame system             *
 * Copyright (C) 2014 Aaron Laws                                    *
 * Copyright 2015 John Ralls                                        *
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
/** @addtogroup KVP

 * A KvpFrame is a set of associations between character strings
 * (keys) and KvpValues.  A KvpValue is notionally a union with
 * possible types enumerated in the KvpValue::Type enum, and includes,
 * among other things, ints, doubles, strings, guids, lists, time
 * and numeric values.  KvpValues may also be other frames, so
 * KVP is inherently hierarchical.
 *
 * Values are stored in a 'slot' associated with a key.
 * Pointers passed as arguments into set_slot and get_slot are the
 * responsibility of the caller.  Pointers returned by get_slot are
 * owned by the kvp_frame.  Make copies as needed.
 *
 * A 'path' is a sequence of keys that can be followed to a value.  Paths are
 * passed as either '/'-delimited strings or as std::vectors of keys. Unlike
 * file system paths, the tokens '.' and '..' have no special meaning.
 *
 * KVP is an implementation detail whose direct use should be avoided; create an
 * abstraction object in libqof to keep KVP encapsulated here and ensure that
 * KVP modifications are written to the database. Two generic abstractions are
 * provided:
 *
 * * @ref qof_book_set_option and @ref qof_book_get_option provide similar
 *   access for book options.
 *
 * @ref kvpvalues provides a catolog of KVP entries including what objects
 * they're part of and how they're used.
 *
 * ## Purpose
 * KVP is used to extend the class structure without directly reflecting the
 * extension in the database or xML schema. The backend will directly load and
 * store KVP slots without any checking, which allows older versions of GnuCash
 * to load the database without complaint and without damaging the KVP data that
 * they don't understand.
 *
 * When a feature is entirely implemented in KVP and doesn't affect the meaning
 * of the books or other features, this isn't a problem, but when it's not true
 * then it should be registered in @ref UtilFeature so that older versions of
 * GnuCash will refuse to load the database.
 *
 * ## Policy
 * * Document every KVP slot in src/engine/kvp_doc.txt so that it is presented
 * in @ref kvpvalues.
 * * Register a feature in @ref UtilFeature if the use of the KVP in any way
    affects the books or business computations.
 * * Key strings should be all lower case with '-', not spaces, separating words
    and '/' separating frames. Prefer longer and more descriptive names to
    abbreviations, and define a global const char[] to the key or path string so
    that the compiler will catch any typos.
 * * Make good use of the hierarchical nature of KVP by using frames to group
    related slots.
 * * Don't use the KVP API directly outside of libqof, and prefer to use the
     QofInstance and QofBook functions whenever feasible. If those functions
     aren't feasible write a class in libqof to abstract the use of KVP.
 * * Avoid re-using key names in different contexts (e.g. Transactions and
    Splits) unless the slot is used for the same purpose in both.
 * @{
*/

#ifndef GNC_KVP_FRAME_TYPE
#define GNC_KVP_FRAME_TYPE

#include "kvp-value.hpp"
#include <map>
#include <string>
#include <vector>
#include <cstring>
#include <algorithm>
#include <iostream>
using Path = std::vector<std::string>;
using KvpEntry = std::pair <std::vector <std::string>, KvpValue*>;

/** Implements KvpFrame.
 *  It's a struct because QofInstance needs to use the typename to declare a
 *  KvpFrame* member, and QofInstance's API is C until its children are all
 *  rewritten in C++.
 *
 * N.B.**  Writes to KvpFrames must** be wrapped in BeginEdit and Commit
 * for the containing QofInstance and the QofInstance must be marked dirty. This
 * is not** done by the KvpFrame API. In general Kvp items should be
 * accessed using either QofInstance or QofBook methods in order to ensure that
 * this is done.
 * @{
 */
struct KvpFrameImpl
{
    class cstring_comparer
    {
    public:
	/* Returns true if one is less than two. */
	bool operator()(const char * one, const char * two) const
	    {
		auto ret = std::strcmp(one, two) < 0;
		return ret;
	    }
    };
    using map_type = std::map<const char *, KvpValue*, cstring_comparer>;

    public:
    KvpFrameImpl() noexcept {};

    /**
     * Performs a deep copy.
     */
    KvpFrameImpl(const KvpFrameImpl &) noexcept;

    /**
     * Perform a deep delete.
     */
    ~KvpFrameImpl() noexcept;

    /**
     * Set the value with the key in the immediate frame, replacing and
     * returning the old value if it exists or nullptr if it doesn't. Takes
     * ownership of new value and releases ownership of the returned old
     * value. Values must be allocated on the free store with operator new.
     * @param key: The key to insert/replace.
     * @param newvalue: The value to set at key.
     * @return The old value if there was one or nullptr.
     */
    //KvpValue* set(const char * key, KvpValue* newvalue) noexcept;
    /**
     * Set the value with the key in a subframe following the keys in path,
     * replacing and returning the old value if it exists or nullptr if it
     * doesn't. Takes ownership of new value and releases ownership of the
     * returned old value. Values must be allocated on the free store with
     * operator new.
     * @param key: The key to insert/replace.
     * @throw invalid_argument if the path doesn't exist.
     * @param path: The path of subframes leading to the frame in which to
     * insert/replace.
     * @param newvalue: The value to set at key.
     * @return The old value if there was one or nullptr.
     */
    KvpValue* set(Path path, KvpValue* newvalue) noexcept;
     /**
     * Set the value with the key in a subframe following the keys in path,
     * replacing and returning the old value if it exists or nullptr if it
     * doesn't. Creates any missing intermediate frames.Takes
     * ownership of new value and releases ownership of the returned old
     * value. Values must be allocated on the free store with operator new.
     * @param path: The path of subframes as a std::vector leading to the
     * frame in which to insert/replace.
     * @param newvalue: The value to set at key.
     * @return The old value if there was one or nullptr.
     */
    KvpValue* set_path(Path path, KvpValue* newvalue) noexcept;
    /**
     * Make a string representation of the frame. Mostly useful for debugging.
     * @return A std::string representing the frame and all its children.
     */
    std::string to_string() const noexcept;
    /**
     * Make a string representation of the frame with the specified string
     * prefixed to every item in the frame.
     * @return A std::string representing all the children of the frame.
     */
    std::string to_string(std::string const &) const noexcept;
    /**
     * Report the keys in the immediate frame. Be sensible about using this, it
     * isn't a very efficient way to iterate.
     * @return std::vector of keys as std::strings.
     */
    std::vector<std::string> get_keys() const noexcept;

    /** Get the value for the tail of the path or nullptr if it doesn't exist.
     * @param path: Path of keys leading to the desired value.
     * @return The value at the key or nullptr.
     */
    KvpValue* get_slot(Path keys) noexcept;

    /** The function should be of the form:
     * <anything> func (char const *, KvpValue *, data_type &);
     * Do not pass nullptr as the function.
     */
    template <typename func_type, typename data_type>
    void for_each_slot_temp(func_type const &, data_type &) const noexcept;

    template <typename func_type>
    void for_each_slot_temp(func_type const &) const noexcept;

    /**
     * Like for_each_slot, but doesn't traverse nested values. This will only loop
     * over root-level values whose keys match the specified prefix.
     */
    template <typename func_type, typename data_type>
    void for_each_slot_prefix(std::string const & prefix, func_type const &, data_type &) const noexcept;

    template <typename func_type>
    void for_each_slot_prefix(std::string const & prefix, func_type const &) const noexcept;

    /**
     * Returns all keys and values of this frame recursively, flattening
     * the frame-containing values.
     */
    std::vector <KvpEntry>
    flatten_kvp(void) const noexcept;

    /** Test for emptiness
     * @return true if the frame contains nothing.
     */
    bool empty() const noexcept { return m_valuemap.empty(); }
    friend int compare(const KvpFrameImpl&, const KvpFrameImpl&) noexcept;

    private:
    map_type m_valuemap;

    KvpFrame * get_child_frame_or_nullptr (Path const &) noexcept;
    KvpFrame * get_child_frame_or_create (Path const &) noexcept;
    void flatten_kvp_impl(std::vector <std::string>, std::vector <KvpEntry> &) const noexcept;
    KvpValue * set_impl (std::string const &, KvpValue *) noexcept;
};

template<typename func_type, typename data_type>
void KvpFrame::for_each_slot_prefix(std::string const & prefix,
        func_type const & func, data_type & data) const noexcept
{
    std::for_each (m_valuemap.begin(), m_valuemap.end(),
        [&prefix,&func,&data](const KvpFrameImpl::map_type::value_type & a)
        {
            /* Testing for prefix matching */
            if (strncmp(a.first, prefix.c_str(), prefix.size()) == 0)
                func (&a.first[prefix.size()], a.second, data);
        }
    );
}

template <typename func_type>
void KvpFrame::for_each_slot_temp(func_type const & func) const noexcept
{
    std::for_each (m_valuemap.begin(), m_valuemap.end(),
        [&func](const KvpFrameImpl::map_type::value_type & a)
        {
            func (a.first, a.second);
        }
    );
}

template <typename func_type, typename data_type>
void KvpFrame::for_each_slot_temp(func_type const & func, data_type & data) const noexcept
{
    std::for_each (m_valuemap.begin(), m_valuemap.end(),
        [&func,&data](const KvpFrameImpl::map_type::value_type & a)
        {
            func (a.first, a.second, data);
        }
    );
}

int compare (const KvpFrameImpl &, const KvpFrameImpl &) noexcept;
int compare (const KvpFrameImpl *, const KvpFrameImpl *) noexcept;
/** @} Doxygen Group */

#endif
