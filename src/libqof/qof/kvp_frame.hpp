/********************************************************************\
 * kvp-frame.hpp -- Implements a key-value frame system             *
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

#ifndef GNC_KVP_FRAME_TYPE
#define GNC_KVP_FRAME_TYPE

#include "kvp-value.hpp"
#include <map>
#include <string>
#include <vector>
#include <cstring>

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

using Path = std::vector<std::string>;

struct KvpFrameImpl
{
    typedef std::map<const char *, KvpValue*, cstring_comparer> map_type;

    public:
    KvpFrameImpl() noexcept {};

    /**
     * Performs a deep copy.
     */
    KvpFrameImpl(const KvpFrameImpl &) noexcept;

    /**
     * Set the value with the key in the immediate frame, replacing and
     * returning the old value if it exists or nullptr if it doesn't.
     * @param key: The key to insert/replace.
     * @param newvalue: The value to set at key.
     * @return The old value if there was one or nullptr.
     */
    KvpValue* set(const char * key, KvpValue* newvalue) noexcept;
    /**
     * Set the value with the key in a subframe following the keys in path,
     * replacing and returning the old value if it exists or nullptr if it
     * doesn't.
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
     * doesn't. Creates any missing intermediate frames.
     * @param path: The path of subframes leading to the frame in which to
     * insert/replace.
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
     * Report the keys in the immediate frame. Be sensible about using this, it
     * isn't a very efficient way to iterate.
     * @return std::vector of keys as std::strings.
     */
    std::vector<std::string> get_keys() const noexcept;

    /** Get the value for the key or nullptr if it doesn't exist.
     * @param key: The key.
     * @return The value at the key or nullptr.
     */
    KvpValue* get_slot(const char * key) const noexcept;
    /** Get the value for the tail of the path or nullptr if it doesn't exist.
     * @param path: Path of keys leading to the desired value.
     * @return The value at the key or nullptr.
     */
    KvpValue* get_slot(Path keys) const noexcept;
    /** Convenience wrapper for std::for_each, which should be preferred.
     */
    void for_each_slot(void (*proc)(const char *key, KvpValue *value,
                                    void * data),
                       void *data) const noexcept;

    /** Test for emptiness
     * @return true if the frame contains nothing.
     */
    bool empty() const noexcept { return m_valuemap.empty(); }
    friend int compare(const KvpFrameImpl&, const KvpFrameImpl&) noexcept;

    private:
    map_type m_valuemap;
};

int compare (const KvpFrameImpl &, const KvpFrameImpl &) noexcept;
int compare (const KvpFrameImpl *, const KvpFrameImpl *) noexcept;
#endif
