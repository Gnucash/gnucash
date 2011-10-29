/*
 * Account.cpp
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
#include "Account.hpp"
#include "private/Account_p.hpp"
#include "Commodity.hpp"

namespace Glib
{

Glib::RefPtr<gnc::Account> wrap(::Account* object, bool refuse_ownership)
{
    return Glib::RefPtr<gnc::Account>( dynamic_cast<gnc::Account*> (Glib::wrap_auto ((GObject*)(object), refuse_ownership)) );
    //We use dynamic_cast<> in case of multiple inheritance.
}

} /* namespace Glib */


namespace gnc
{


/* The *_Class implementation: */

const Glib::Class& Account_Class::init()
{
    if (!gtype_) // create the GType if necessary
    {
        // Glib::Class has to know the class init function to clone custom types.
        class_init_func_ = &Account_Class::class_init_function;

        // This is actually just optimized away, apparently with no harm.
        // Make sure that the parent type has been created.
        //CppClassParent::CppObjectType::get_type();

        // Create the wrapper type, with the same class/instance size as the base type.
        register_derived_type(gnc_account_get_type());

        // Add derived versions of interfaces, if the C type implements any interfaces:

    }

    return *this;
}


void Account_Class::class_init_function(void* g_class, void* class_data)
{
    BaseClassType *const klass = static_cast<BaseClassType*>(g_class);
    CppClassParent::class_init_function(klass, class_data);
}


Glib::ObjectBase* Account_Class::wrap_new(GObject* object)
{
    return new Account((::Account*)object);
}


/* The implementation: */

::Account* Account::gobj_copy()
{
    reference();
    return gobj();
}

Account::Account(const Glib::ConstructParams& construct_params)
    : GncInstance(construct_params)
{

}

Account::Account(::Account* castitem)
    : GncInstance((::QofInstance*)(castitem))
{}


Account::~Account()
{}


Account::CppClassType Account::account_class_; // initialize static member

GType Account::get_type()
{
    return account_class_.init().get_type();
}


GType Account::get_base_type()
{
    return gnc_account_get_type();
}



Glib::RefPtr<Commodity> Account::get_commodity() const
{
    return Glib::wrap(xaccAccountGetCommodity(gobj()));
}
Glib::RefPtr<Account> Account::get_parent() const
{
    return Glib::wrap(gnc_account_get_parent(gobj()));
}
Glib::RefPtr<Account> Account::get_root()
{
    return Glib::wrap(gnc_account_get_root(gobj()));
}
Glib::RefPtr<Account> Account::get_nth_child (gint num) const
{
    return Glib::wrap(gnc_account_nth_child(gobj(), num));
}

} // END namespace gnc
