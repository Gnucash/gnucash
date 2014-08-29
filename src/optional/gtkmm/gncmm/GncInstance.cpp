/*
 * GncInstance.cpp
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

#include "GncInstance.hpp"
#include "private/GncInstance_p.hpp"
#include "Book.hpp"
#include "qofinstance-p.h"


namespace Glib
{

Glib::RefPtr<gnc::GncInstance> wrap(::QofInstance* object, bool take_copy)
{
    return Glib::RefPtr<gnc::GncInstance>( dynamic_cast<gnc::GncInstance*> (Glib::wrap_auto ((GObject*)(object), take_copy)) );
    //We use dynamic_cast<> in case of multiple inheritance.
}

} /* namespace Glib */


namespace gnc
{


/* The *_Class implementation: */

const Glib::Class& GncInstance_Class::init()
{
    if (!gtype_) // create the GType if necessary
    {
        // Glib::Class has to know the class init function to clone custom types.
        class_init_func_ = &GncInstance_Class::class_init_function;

        // This is actually just optimized away, apparently with no harm.
        // Make sure that the parent type has been created.
        //CppClassParent::CppObjectType::get_type();

        // Create the wrapper type, with the same class/instance size as the base type.
        register_derived_type(qof_instance_get_type());

        // Add derived versions of interfaces, if the C type implements any interfaces:

    }

    return *this;
}


void GncInstance_Class::class_init_function(void* g_class, void* class_data)
{
    BaseClassType *const klass = static_cast<BaseClassType*>(g_class);
    CppClassParent::class_init_function(klass, class_data);
}


Glib::ObjectBase* GncInstance_Class::wrap_new(GObject* object)
{
    return new GncInstance((::QofInstance*)object);
}


/* The implementation: */

::QofInstance* GncInstance::gobj_copy()
{
    reference();
    return gobj();
}

GncInstance::GncInstance(const Glib::ConstructParams& construct_params)
    : Glib::Object(construct_params)
{

}

GncInstance::GncInstance(::QofInstance* castitem)
    : Glib::Object((GObject*)(castitem))
{}


GncInstance::~GncInstance()
{}


GncInstance::CppClassType GncInstance::gncInstance_class_; // initialize static member

GType GncInstance::get_type()
{
    return gncInstance_class_.init().get_type();
}


GType GncInstance::get_base_type()
{
    return qof_instance_get_type();
}

void GncInstance::set_dirty()
{
    return qof_instance_set_dirty(gobj());
}
void GncInstance::mark_clean()
{
    return qof_instance_mark_clean(gobj());
}


// ////////////////////////////////////////

Glib::RefPtr<Book> GncInstance::get_book() const
{
    return Glib::wrap(qof_instance_get_book (gobj()));
}
void GncInstance::set_book(Glib::RefPtr<Book> book)
{
    g_assert (book);
    qof_instance_set_book(gobj(), book->gobj());
}



} // END namespace gnc
