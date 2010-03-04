/*
 * ScopedPointer.hpp
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

#ifndef GNC_SCOPEDPOINTER_HPP
#define GNC_SCOPEDPOINTER_HPP

#error "This file requires the Boost library because the boost::shared_ptr is nice and has non-trivial features. However, in the current build this file isn't used anyway, so it should not be used within this project so far."
#include <boost/shared_ptr.hpp>

namespace gnc
{

/** A sole ownership of a single C object.
 *
 * This copies the interface of the boost::scoped_ptr, but with the
 * boost::shared_ptr possiblity of a custom deleter function because
 * we need that.
 */
template<class T>
class ScopedPointer // noncopyable
{
    // Private copy constructor so that this is noncopyable
    ScopedPointer(ScopedPointer const &);
    ScopedPointer& operator=(ScopedPointer const&);
    void operator==(ScopedPointer const&) const;
    void operator!=(ScopedPointer const&) const;
    boost::shared_ptr<T> m_ptr;
    typedef ScopedPointer<T> this_type;
public:
    typedef T element_type;

    ScopedPointer()
            : m_ptr()
    { }

    template<class Y, class D> ScopedPointer(Y * ptr, D deleter)
            : m_ptr(ptr, deleter)
    { }

    void reset() // never throws in 1.30+
    {
        this_type().swap(*this);
    }

    template<class Y, class D> void reset( Y * p, D d )
    {
        this_type( p, d ).swap( *this );
    }

    T & operator*() const // never throws
    {
        assert(m_ptr != 0);
        return *m_ptr.get();
    }

    T * operator->() const // never throws
    {
        assert(m_ptr != 0);
        return m_ptr.get();
    }

    T * get() const // never throws
    {
        return m_ptr.get();
    }

    // implicit conversion to "bool"
    typedef T * (this_type::*unspecified_bool_type)() const;
    operator unspecified_bool_type() const // never throws
    {
        return m_ptr == 0 ? 0 : &this_type::get;
    }

    bool operator! () const // never throws
    {
        return m_ptr == 0;
    }

    void swap(ScopedPointer & other) // never throws
    {
        m_ptr.swap(other.m_ptr);
    }
};

} // END namespace gnc

#endif
