/*
 * Book.hpp
 * Copyright (C) 2011 Christian Stimming
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

#include <glibmm/object.h>
//#include "GncInstance.hpp"

namespace gnc
{
class Book_Class;
class Book;
class GncInstance;
} // END namespace gnc

namespace gnc
{
class Account;

/** Wrapper around a gnucash ::QofBook pointer with C++ methods for
 * easier setter and getter access.
 */
class Book : public Glib::Object //, public GncInstance
{
#ifndef DOXYGEN_SHOULD_SKIP_THIS
    typedef Book CppObjectType;
    typedef Book_Class CppClassType;
    typedef ::QofBook BaseObjectType;
    typedef ::QofBookClass BaseClassType;

private:
    friend class Book_Class;
    static CppClassType book_class_;

private:
    // noncopyable
    Book(const Book&);
    Book& operator=(const Book&);

protected:
    explicit Book(const Glib::ConstructParams& construct_params);
    explicit Book(QofBook* castitem);

#endif /* DOXYGEN_SHOULD_SKIP_THIS */

public:
    virtual ~Book();
#ifndef DOXYGEN_SHOULD_SKIP_THIS
    static GType get_type()      G_GNUC_CONST;
    static GType get_base_type() G_GNUC_CONST;
#endif

    ///Provides access to the underlying C GObject.
    QofBook*       gobj()
    {
        return reinterpret_cast<QofBook*>(gobject_);
    }

    ///Provides access to the underlying C GObject.
    const QofBook* gobj() const
    {
        return reinterpret_cast<QofBook*>(gobject_);
    }

    ///Provides access to the underlying C instance. The caller is responsible for unrefing it. Use when directly setting fields in structs.
    QofBook* gobj_copy();

private:
public:


    Glib::RefPtr<Account> get_root_account();
};

} // END namespace gnc

namespace Glib
{
/** A Glib::wrap() method for this object.
 *
 * @param object The C instance.
 * @param take_copy False if the result should take ownership of the C instance. True if it should take a new copy or ref.
 * @result A C++ instance that wraps this C instance.
 *
 * @relates Gio::FileInfo
 */
Glib::RefPtr<gnc::Book> wrap(::QofBook* object, bool take_copy = false);
}

#endif
