/*
 * Commodity.hpp
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

#ifndef GNC_COMMODITY_HPP
#define GNC_COMMODITY_HPP

// gnucash includes
#include "config.h"
extern "C"
{
#include "qof.h"
#include "engine/gnc-commodity.h"
}

#include <glibmm/object.h>
#include <glibmm/ustring.h>
#include "GncInstance.hpp"

namespace gnc
{
class Commodity_Class;
} // END namespace gnc

namespace gnc
{

/** Wrapper around a gnucash \ref gnc_commodity object */
class Commodity : public GncInstance
{
#ifndef DOXYGEN_SHOULD_SKIP_THIS
    typedef Commodity CppObjectType;
    typedef Commodity_Class CppClassType;
    typedef ::gnc_commodity BaseObjectType;
    typedef ::gnc_commodityClass BaseClassType;

private:
    friend class Commodity_Class;
    static CppClassType commodity_class_;

private:
    // noncopyable
    Commodity(const Commodity&);
    Commodity& operator=(const Commodity&);

protected:
    explicit Commodity(const Glib::ConstructParams& construct_params);
    explicit Commodity(gnc_commodity* castitem);

#endif /* DOXYGEN_SHOULD_SKIP_THIS */

public:
    virtual ~Commodity();
#ifndef DOXYGEN_SHOULD_SKIP_THIS
    static GType get_type()      G_GNUC_CONST;
    static GType get_base_type() G_GNUC_CONST;
#endif

    ///Provides access to the underlying C GObject.
    gnc_commodity*       gobj()
    {
        return reinterpret_cast<gnc_commodity*>(gobject_);
    }

    ///Provides access to the underlying C GObject.
    const gnc_commodity* gobj() const
    {
        return reinterpret_cast<gnc_commodity*>(gobject_);
    }

    ///Provides access to the underlying C instance. The caller is responsible for unrefing it. Use when directly setting fields in structs.
    gnc_commodity* gobj_copy();

private:
public:

    Glib::ustring get_mnemonic() const
    {
        return gnc_commodity_get_mnemonic(gobj());
    }
    Glib::ustring get_namespace() const
    {
        return gnc_commodity_get_namespace(gobj());
    }
    Glib::ustring get_fullname() const
    {
        return gnc_commodity_get_fullname(gobj());
    }
    Glib::ustring get_printname() const
    {
        return gnc_commodity_get_printname(gobj());
    }

    int get_fraction() const
    {
        return gnc_commodity_get_fraction(gobj());
    }
    bool get_quote_flag() const
    {
        return gnc_commodity_get_quote_flag(gobj());
    }

    bool is_currency() const
    {
        return gnc_commodity_is_currency(gobj());
    }

    bool equal(const Commodity& other) const
    {
        return gnc_commodity_equal(gobj(), other.gobj());
    }
};

inline bool operator==(const Commodity& a, const Commodity& b)
{
    return a.equal(b);
}

inline bool operator!=(const Commodity& a, const Commodity& b)
{
    return !(a == b);
}

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
Glib::RefPtr<gnc::Commodity> wrap(gnc_commodity* object, bool refuse_ownership = true);
}


#endif
