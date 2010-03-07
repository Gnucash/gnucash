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

#include "gnc/WeakPointer.hpp"

#include <QString>
#include <QList>

namespace gnc
{

class Account : public WeakPointer< ::Account >
{
public:
    typedef WeakPointer< ::Account > base_class;
    Account(element_type* ptr = 0)
            : base_class(ptr)
    { }
    QString getName() const { return QString::fromUtf8(xaccAccountGetName(get())); }
    QString getCode() const { return QString::fromUtf8(xaccAccountGetCode(get())); }
    QString getDescription() const { return QString::fromUtf8(xaccAccountGetDescription(get())); }

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


    typedef QList< ::Account*> AccountQList;
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

#endif
