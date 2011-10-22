/*
 * Commodity.hpp
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

#ifndef GNC_COMMODITY_HPP
#define GNC_COMMODITY_HPP

// gnucash includes
#include "config.h"
extern "C"
{
#include "qof.h"
#include "engine/gnc-commodity.h"
}

#include "gnc/GncInstance.hpp"
#include <QtCore/QMetaType>
#include <QtCore/QString>

/** Wrapper around a gnucash gnc_commodity pointer */
namespace gnc
{

class Commodity : public GncInstance<gnc_commodity>
{
public:
    typedef GncInstance<gnc_commodity> base_class;
    Commodity(element_type *ptr = 0)
            : base_class(ptr)
    {}
    QString get_mnemonic() const { return gnc_commodity_get_mnemonic(get()); }
    QString get_namespace() const { return gnc_commodity_get_namespace(get()); }
    QString get_fullname() const { return gnc_commodity_get_fullname(get()); }
    QString get_printname() const { return gnc_commodity_get_printname(get()); }

    int get_fraction() const { return gnc_commodity_get_fraction(get()); }
    bool get_quote_flag() const { return gnc_commodity_get_quote_flag(get()); }

    bool is_currency() const { return gnc_commodity_is_currency(get()); }

    bool equal(const Commodity& other) const { return gnc_commodity_equal(get(), other.get()); }
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

Q_DECLARE_METATYPE(gnc::Commodity)

#endif
