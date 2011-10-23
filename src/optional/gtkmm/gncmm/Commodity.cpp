/*
 * Commodity.cpp
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

#include "Commodity.hpp"
#include "private/Commodity_p.hpp"


namespace Glib
{

Glib::RefPtr<gnc::Commodity> wrap(gnc_commodity* object, bool take_copy)
{
    return Glib::RefPtr<gnc::Commodity>( dynamic_cast<gnc::Commodity*> (Glib::wrap_auto ((GObject*)(object), take_copy)) );
    //We use dynamic_cast<> in case of multiple inheritance.
}

} /* namespace Glib */


namespace gnc
{


/* The *_Class implementation: */

const Glib::Class& Commodity_Class::init()
{
    if (!gtype_) // create the GType if necessary
    {
        // Glib::Class has to know the class init function to clone custom types.
        class_init_func_ = &Commodity_Class::class_init_function;

        // This is actually just optimized away, apparently with no harm.
        // Make sure that the parent type has been created.
        //CppClassParent::CppObjectType::get_type();

        // Create the wrapper type, with the same class/instance size as the base type.
        register_derived_type(gnc_commodity_get_type());

        // Add derived versions of interfaces, if the C type implements any interfaces:

    }

    return *this;
}


void Commodity_Class::class_init_function(void* g_class, void* class_data)
{
    BaseClassType *const klass = static_cast<BaseClassType*>(g_class);
    CppClassParent::class_init_function(klass, class_data);
}


Glib::ObjectBase* Commodity_Class::wrap_new(GObject* object)
{
    return new Commodity((gnc_commodity*)object);
}


/* The implementation: */

gnc_commodity* Commodity::gobj_copy()
{
    reference();
    return gobj();
}

Commodity::Commodity(const Glib::ConstructParams& construct_params)
    : GncInstance(construct_params)
{

}

Commodity::Commodity(gnc_commodity* castitem)
    : GncInstance((::QofInstance*)(castitem))
{}


Commodity::~Commodity()
{}


Commodity::CppClassType Commodity::commodity_class_; // initialize static member

GType Commodity::get_type()
{
    return commodity_class_.init().get_type();
}


GType Commodity::get_base_type()
{
    return gnc_commodity_get_type();
}



} // END namespace gnc
