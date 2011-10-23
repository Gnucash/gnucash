/*
 * Account.hpp
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

#ifndef GNC_ACCOUNT_HPP
#define GNC_ACCOUNT_HPP

// gnucash includes
extern "C"
{
#include "qof.h"
#include "engine/Account.h"
}

#include <glibmm/object.h>
#include <glibmm/ustring.h>

#include "Numeric.hpp"
#include "GncInstance.hpp"

namespace gnc
{
class Account_Class;
} // END namespace gnc

namespace gnc
{
class Account;
class Commodity;

/** Wrapper around a gnucash ::Account pointer with C++ methods for
 * easier setter and getter access.
 */
class Account : public GncInstance
{
#ifndef DOXYGEN_SHOULD_SKIP_THIS
    typedef Account CppObjectType;
    typedef Account_Class CppClassType;
    typedef ::Account BaseObjectType;
    typedef ::AccountClass BaseClassType;

private:
    friend class Account_Class;
    static CppClassType account_class_;

private:
    // noncopyable
    Account(const Account&);
    Account& operator=(const Account&);

protected:
    explicit Account(const Glib::ConstructParams& construct_params);
    explicit Account(::Account* castitem);

#endif /* DOXYGEN_SHOULD_SKIP_THIS */

public:
    virtual ~Account();
#ifndef DOXYGEN_SHOULD_SKIP_THIS
    static GType get_type()      G_GNUC_CONST;
    static GType get_base_type() G_GNUC_CONST;
#endif

    ///Provides access to the underlying C GObject.
    ::Account*       gobj()
    {
        return reinterpret_cast< ::Account*>(gobject_);
    }

    ///Provides access to the underlying C GObject.
    const ::Account* gobj() const
    {
        return reinterpret_cast< ::Account*>(gobject_);
    }

    ///Provides access to the underlying C instance. The caller is responsible for unrefing it. Use when directly setting fields in structs.
    ::Account* gobj_copy();

private:
public:


    // actual implementation follows here

    Glib::ustring getName() const
    {
        return (xaccAccountGetName(gobj()));
    }
    Glib::ustring getFullName() const
    {
        return gchar_to_ustring(gnc_account_get_full_name (gobj()));
    }
    Glib::ustring getCode() const
    {
        return (xaccAccountGetCode(gobj()));
    }
    Glib::ustring getDescription() const
    {
        return (xaccAccountGetDescription(gobj()));
    }
    Glib::RefPtr<Commodity> getCommodity() const;
    int getCommoditySCU() const
    {
        return xaccAccountGetCommoditySCU(gobj());
    }

    ::SplitList* getSplitList() const
    {
        return xaccAccountGetSplitList(gobj());
    }

    /** @name Account tree traversal */
    //@{

    Glib::RefPtr<Account> get_parent() const;
    Glib::RefPtr<Account> get_root();
    bool is_root() const
    {
        return gnc_account_is_root(gobj());
    }
    gint n_children() const
    {
        return gnc_account_n_children(gobj());
    }
    GList *get_children() const
    {
        return gnc_account_get_children(gobj());
    }
    GList *get_descendants () const
    {
        return gnc_account_get_descendants (gobj());
    }
    Glib::RefPtr<Account> nth_child (gint num) const;


    /** Return the index of this account in the children's list of its
     * parent account.
     */
    gint child_index () const
    {
        Glib::RefPtr<Account> parent(get_parent());
        if (parent && parent->gobj())
            return gnc_account_child_index(parent->gobj(), gobj());
        else
            return 0;
    }

    gint get_current_depth () const
    {
        return gnc_account_get_current_depth(gobj());
    }
    gint get_tree_depth () const
    {
        return gnc_account_get_tree_depth(gobj());
    }
    //@}


#if 0
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
#endif

};

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
Glib::RefPtr<gnc::Account> wrap(::Account* object, bool take_copy = false);
}

#endif
