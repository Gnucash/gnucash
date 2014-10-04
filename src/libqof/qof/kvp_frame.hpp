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

struct KvpFrameImpl
{
    typedef std::map<const char *, KvpValueImpl *, cstring_comparer> map_type;

    public:
    KvpFrameImpl() noexcept {};

    /**
     * Performs a deep copy.
     */
    KvpFrameImpl(const KvpFrameImpl &) noexcept;

    /**
     * Replaces the KvpValueImpl at the specified spot, and returns
     * the old KvpValueImpl which used to occupy the spot.
     *
     * If no KvpValueImpl was at the spot, nullptr is returned.
     */
    KvpValueImpl * replace_nc(const char * key, KvpValueImpl * newvalue) noexcept;

    std::string to_string() const noexcept;

    std::vector<std::string> get_keys() const noexcept;

    KvpValueImpl * get_slot(const char * key) const noexcept;

    void for_each_slot(void (*proc)(const char *key, KvpValue *value, void * data), void *data) const noexcept;

    friend int compare(const KvpFrameImpl &, const KvpFrameImpl &) noexcept;

    private:
    map_type m_valuemap;
};

int compare (const KvpFrameImpl &, const KvpFrameImpl &) noexcept;
int compare (const KvpFrameImpl *, const KvpFrameImpl *) noexcept;
#endif
