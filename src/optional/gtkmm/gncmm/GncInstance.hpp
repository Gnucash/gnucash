/*
 * GncInstance.hpp
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

#ifndef GNC_GNCINSTANCE_HPP
#define GNC_GNCINSTANCE_HPP

// gnucash includes
#include "config.h"
extern "C"
{
#include "qof.h"
}

#include <glibmm/object.h>

namespace gnc
{
class Book;
class GncInstance;
class GncInstance_Class;
} // END namespace gnc

namespace gnc
{

/** Wrapper for ::QofInstance
 * This
 * base class offers some common methods.
 *
 * We cannot name it QofInstance because those stupid C macros (like
 * QOF_CHECK_TYPE) would always confuse our namespaced declaration
 * with the C declaration. I hate macros!
 */
class GncInstance : public Glib::Object
{
#ifndef DOXYGEN_SHOULD_SKIP_THIS
    typedef GncInstance CppObjectType;
    typedef GncInstance_Class CppClassType;
    typedef ::QofInstance BaseObjectType;
    typedef ::QofInstanceClass BaseClassType;

private:
    friend class GncInstance_Class;
    static CppClassType gncInstance_class_;

private:
    // noncopyable
    GncInstance(const GncInstance&);
    GncInstance& operator=(const GncInstance&);

protected:
    explicit GncInstance(const Glib::ConstructParams& construct_params);
    explicit GncInstance(::QofInstance* castitem);

#endif /* DOXYGEN_SHOULD_SKIP_THIS */

public:
    virtual ~GncInstance();
#ifndef DOXYGEN_SHOULD_SKIP_THIS
    static GType get_type()      G_GNUC_CONST;
    static GType get_base_type() G_GNUC_CONST;
#endif

    ///Provides access to the underlying C GObject.
    ::QofInstance*       gobj()
    {
        return reinterpret_cast< ::QofInstance*>(gobject_);
    }

    ///Provides access to the underlying C GObject.
    const ::QofInstance* gobj() const
    {
        return reinterpret_cast< ::QofInstance*>(gobject_);
    }

    ///Provides access to the underlying C instance. The caller is responsible for unrefing it. Use when directly setting fields in structs.
    ::QofInstance* gobj_copy();

public:

    Glib::RefPtr<Book> get_book() const;
    void set_book(Glib::RefPtr<Book> book);
    const ::GncGUID* get_guid() const
    {
        return qof_entity_get_guid(gobj_const());
    }

    bool is_dirty() const
    {
        return qof_instance_get_dirty(gobj_const());
    }
    void set_dirty();
    void mark_clean();

    //bool check_type(const char* type_id) { return (0 == g_strcmp0(type_id, QOF_INSTANCE(base_class::get())->e_type)); }
    //Slots getSlots() const { return qof_instance_get_slots(QOF_INSTANCE(get())); }

private:
    /*const*/
    ::QofInstance* gobj_const() const
    {
        return const_cast< ::QofInstance*>(gobj());
    }
};

} // END namespace gnc

#endif
