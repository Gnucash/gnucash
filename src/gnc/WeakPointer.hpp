/*
 * WeakPointer.hpp
 * Copyright (C) 2010 Christian Stimming
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 */

#ifndef GNC_WEAKPOINTER_HPP
#define GNC_WEAKPOINTER_HPP

#include <string>
#include <cassert>

namespace gnc
{

/** A thin wrapper for a C object which is owned by someone else.
 *
 * This copies the interface of the boost::scoped_ptr, but with the
 * boost::shared_ptr possiblity of a custom deleter function because
 * we need that.
 */
template<class T>
class WeakPointer
{
public:
    typedef T element_type;
private:
    element_type *m_ptr;
    typedef WeakPointer<T> this_type;
public:

    WeakPointer(element_type* ptr = 0)
            : m_ptr(ptr)
    { }

    void reset(element_type* ptr = 0)
    {
        m_ptr = ptr;
    }

    T & operator*() const // never throws
    {
        assert(m_ptr != 0);
        return *m_ptr;
    }

    T * operator->() const // never throws
    {
        assert(m_ptr != 0);
        return m_ptr;
    }

    T * get() const // never throws
    {
        return m_ptr;
    }

    // implicit conversion to "bool"
    typedef T * this_type::*unspecified_bool_type;
    operator unspecified_bool_type() const // never throws
    {
        return m_ptr == 0 ? 0 : &this_type::m_ptr;
    }

    bool operator! () const // never throws
    {
        return m_ptr == 0;
    }

};

} // END namespace gnc

#endif
