/*
 * GncInstance.hpp
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

#ifndef GNC_GNCINSTANCE_HPP
#define GNC_GNCINSTANCE_HPP

// gnucash includes
#include "config.h"
extern "C"
{
#include "qof.h"
}

#include "gnc/WeakPointer.hpp"
#include <QString>

namespace gnc
{

/** Wrapper around the "base class" QofInstance which already offers
 * some common methods. We cannot name it QofInstance because those
 * stupid C macros (like QOF_CHECK_TYPE) would always confuse our
 * namespaced declaration with the C declaration. I hate macros!
 *
 * Unfortunately this object has no information about whether the
 * underlying gnucash ::Transaction object is still alive or has been
 * deleted.
 */
template<class T>
class GncInstance : public WeakPointer< T >
{
public:
    typedef WeakPointer< T > base_class;
    GncInstance(T* ptr = 0)
            : base_class(ptr)
    { }

    GncInstance< ::QofBook > getBook() const { return qof_instance_get_book (QOF_INSTANCE(base_class::get())); }
    ::GUID getGUID() const { return qof_entity_get_guid(QOF_INSTANCE(base_class::get())); }

    bool is_dirty() const { return qof_instance_get_dirty(QOF_INSTANCE(base_class::get())); }
    void set_dirty() { return qof_instance_set_dirty(QOF_INSTANCE(base_class::get())); }
    void mark_clean() { return qof_instance_mark_clean(QOF_INSTANCE(base_class::get())); }

    //bool check_type(const char* type_id) { return (0 == g_strcmp0(type_id, QOF_INSTANCE(base_class::get())->e_type)); }
    //Slots getSlots() const { return qof_instance_get_slots(QOF_INSTANCE(get())); }
};

} // END namespace gnc

#endif
