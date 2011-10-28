/*
 * Book.cpp
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

#include "config.h"
#include "Book.hpp"
#include "private/Book_p.hpp"
#include "Account.hpp"

namespace Glib
{

Glib::RefPtr<gnc::Book> wrap(QofBook* object, bool take_copy)
{
    return Glib::RefPtr<gnc::Book>( dynamic_cast<gnc::Book*> (Glib::wrap_auto ((GObject*)(object), take_copy)) );
    //We use dynamic_cast<> in case of multiple inheritance.
}

} /* namespace Glib */


namespace gnc
{


/* The *_Class implementation: */

const Glib::Class& Book_Class::init()
{
    if (!gtype_) // create the GType if necessary
    {
        // Glib::Class has to know the class init function to clone custom types.
        class_init_func_ = &Book_Class::class_init_function;

        // This is actually just optimized away, apparently with no harm.
        // Make sure that the parent type has been created.
        //CppClassParent::CppObjectType::get_type();

        // Create the wrapper type, with the same class/instance size as the base type.
        register_derived_type(qof_book_get_type());

        // Add derived versions of interfaces, if the C type implements any interfaces:

    }

    return *this;
}


void Book_Class::class_init_function(void* g_class, void* class_data)
{
    BaseClassType *const klass = static_cast<BaseClassType*>(g_class);
    CppClassParent::class_init_function(klass, class_data);
}


Glib::ObjectBase* Book_Class::wrap_new(GObject* object)
{
    return new Book((QofBook*)object);
}


/* The implementation: */

QofBook* Book::gobj_copy()
{
    reference();
    return gobj();
}

Book::Book(const Glib::ConstructParams& construct_params)
    : GncInstance(construct_params)
{

}

Book::Book(QofBook* castitem)
    : GncInstance((::QofInstance*)(castitem))
{}


Book::~Book()
{}


Book::CppClassType Book::book_class_; // initialize static member

GType Book::get_type()
{
    return book_class_.init().get_type();
}


GType Book::get_base_type()
{
    return qof_book_get_type();
}


void Book::string_option_set (const Glib::ustring& opt_name, const Glib::ustring& opt_val)
{
    qof_book_set_string_option(gobj(), opt_name.c_str(), opt_val.c_str());
}

Glib::ustring Book::string_option_get (const Glib::ustring& opt_name) const
{
    const char* r = qof_book_get_string_option(gobj(), opt_name.c_str());
    if (r)
        return r;
    else
        return "";
}
bool Book::string_option_exists (const Glib::ustring& opt_name) const
{
    const char* r = qof_book_get_string_option(gobj(), opt_name.c_str());
    if (r)
        return true;
    else
        return false;
}

Glib::RefPtr<Account> Book::get_root_account()
{
    return Glib::wrap(gnc_book_get_root_account (gobj()));
}

} // END namespace gnc
