/*
 * Book.hpp
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

#ifndef GNC_BOOK_HPP
#define GNC_BOOK_HPP

// gnucash includes
extern "C"
{
#include "qof.h"
#include "engine/gnc-hooks.h"
#include "engine/Account.h"
}

#include "gnc/GncInstance.hpp"

namespace gnc
{
class Account;

/** Wrapper around a gnucash ::QofBook pointer with C++ methods for
 * easier setter and getter access.
 *
 * Unfortunately this object has no information about whether the
 * underlying gnucash ::QofBook object is still alive or has been
 * deleted.
 */
class Book : public GncInstance< ::QofBook >
{
public:
    typedef GncInstance< ::QofBook > base_class;
    Book(element_type* ptr = 0)
            : base_class(ptr)
    { }
    Book(const base_class& x)
            : base_class(x)
    { }

    Account get_root_account();
};

} // END namespace gnc

#endif
