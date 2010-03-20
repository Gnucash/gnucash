/*
 * Account.hpp
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

#ifndef GNC_ACCOUNT_HPP
#define GNC_ACCOUNT_HPP

// gnucash includes
#include "config.h"
extern "C"
{
#include "qof.h"
#include "engine/Account.h"
}

#include "gnc/GncInstance.hpp"
#include "gnc/Commodity.hpp"
#include "gnc/Numeric.hpp"

#include <QtCore/QString>
#include <QtCore/QList>
#include <QtCore/QMetaType>

namespace gnc
{

typedef QList< ::Account*> AccountQList;


/** Wrapper around a gnucash ::Account pointer with C++ methods for
 * easier setter and getter access.
 *
 * Unfortunately this object has no information about whether the
 * underlying gnucash ::Account object is still alive or has been
 * deleted.
 */
class Account : public GncInstance< ::Account >
{
public:
    typedef GncInstance< ::Account > base_class;
    Account(element_type* ptr = 0)
            : base_class(ptr)
    { }
    QString getName() const { return QString::fromUtf8(xaccAccountGetName(get())); }
    QString getFullName() const { return gchar_to_QString(gnc_account_get_full_name (get())); }
    QString getCode() const { return QString::fromUtf8(xaccAccountGetCode(get())); }
    QString getDescription() const { return QString::fromUtf8(xaccAccountGetDescription(get())); }
    Commodity getCommodity() const { return xaccAccountGetCommodity(get()); }
    int getCommoditySCU() const { return xaccAccountGetCommoditySCU(get()); }

    ::SplitList* getSplitList() const { return xaccAccountGetSplitList(get()); }

    /** @name Account tree traversal */
    //@{

    Account get_parent() const { return gnc_account_get_parent(get()); }
    Account get_root() { return gnc_account_get_root(get()); }
    bool is_root() const { return gnc_account_is_root(get()); }
    gint n_children() const { return gnc_account_n_children(get()); }
    GList *get_children() const { return gnc_account_get_children(get()); }
    GList *get_descendants () const { return gnc_account_get_descendants (get()); }
    Account nth_child (gint num) const { return gnc_account_nth_child(get(), num); }


    /** Return the index of this account in the children's list of its
     * parent account.
     */
    gint child_index () const
    {
        Account parent(get_parent());
        if (parent.get())
            return gnc_account_child_index(parent.get(), get());
        else
            return 0;
    }

    gint get_current_depth () const { return gnc_account_get_current_depth(get()); }
    gint get_tree_depth () const { return gnc_account_get_tree_depth(get()); }
    //@}


    static AccountQList fromGList(GList* glist)
    {
        AccountQList result;
        GList* list = glist;
        while (list)
        {
            result.append(reinterpret_cast< ::Account*>(list->data));
            list = g_list_next(list);
        }
        return result;
    }
};

} // END namespace gnc

Q_DECLARE_METATYPE(gnc::Account)

#endif
