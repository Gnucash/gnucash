/*
 * Split.hpp
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

#ifndef GNC_SPLIT_HPP
#define GNC_SPLIT_HPP

// gnucash includes
#include "config.h"
extern "C"
{
#include "qof.h"
#include "engine/Split.h"
}

#include <glibmm/object.h>
#include <glibmm/date.h>
#include <glibmm/ustring.h>
#include <vector>

#include "GncInstance.hpp"
#include "Numeric.hpp"

namespace gnc
{
class Split_Class;
} // END namespace gnc

namespace gnc
{
class Book;
class Account;
class Transaction;
class TmpTransaction;

typedef std::vector< ::Split*> SplitQList;

/** Wrapper around a gnucash ::Split pointer with C++ methods for
 * easier setter and getter access.
 */
class Split : public GncInstance
{
#ifndef DOXYGEN_SHOULD_SKIP_THIS
    typedef Split CppObjectType;
    typedef Split_Class CppClassType;
    typedef ::Split BaseObjectType;
    typedef ::SplitClass BaseClassType;

private:
    friend class Split_Class;
    static CppClassType split_class_;

private:
    // noncopyable
    Split(const Split&);
    Split& operator=(const Split&);

protected:
    explicit Split(const Glib::ConstructParams& construct_params);
    explicit Split(::Split* castitem);

#endif /* DOXYGEN_SHOULD_SKIP_THIS */

public:
    virtual ~Split();
#ifndef DOXYGEN_SHOULD_SKIP_THIS
    static GType get_type()      G_GNUC_CONST;
    static GType get_base_type() G_GNUC_CONST;
#endif

    ///Provides access to the underlying C GObject.
    ::Split*       gobj()
    {
        return reinterpret_cast< ::Split*>(gobject_);
    }

    ///Provides access to the underlying C GObject.
    const ::Split* gobj() const
    {
        return reinterpret_cast< ::Split*>(gobject_);
    }

    ///Provides access to the underlying C instance. The caller is responsible for unrefing it. Use when directly setting fields in structs.
    ::Split* gobj_copy();

private:
public:


    Glib::RefPtr<Account> getAccount() const;
    void setAccount(Glib::RefPtr<Account> acc);
    void setAccount(::Account* acc);

    Glib::RefPtr<Transaction> getParent() const;
    void setParent(Glib::RefPtr<Transaction> trans);
    void setParent(Transaction& trans);

    Glib::ustring getMemo() const
    {
        return xaccSplitGetMemo(gobj());
    }
    void setMemo(const Glib::ustring& v)
    {
        xaccSplitSetMemo(gobj(), v.c_str());
    }

    Glib::ustring getAction() const
    {
        return xaccSplitGetAction(gobj());
    }
    void setAction(const Glib::ustring& v)
    {
        xaccSplitSetAction(gobj(), v.c_str());
    }

    char getReconcile() const
    {
        return xaccSplitGetReconcile(gobj());
    }
    void setReconcile(char v)
    {
        xaccSplitSetReconcile(gobj(), v);
    }

    Glib::RefPtr<Split> getOtherSplit() const;

    Glib::ustring getCorrAccountFullName() const
    {
        return gchar_to_ustring(xaccSplitGetCorrAccountFullName(gobj()));
    }
    Glib::ustring getCorrAccountName() const
    {
        return xaccSplitGetCorrAccountName(gobj());
    }
    Glib::ustring getCorrAccountCode() const
    {
        return xaccSplitGetCorrAccountCode(gobj());
    }

    void setAmount(const Numeric& amount)
    {
        xaccSplitSetAmount(gobj(), amount);
    }
    Numeric getAmount() const
    {
        return xaccSplitGetAmount(gobj());
    }
    void setValue(const Numeric& value)
    {
        xaccSplitSetValue(gobj(), value);
    }
    Numeric getValue() const
    {
        return xaccSplitGetValue(gobj());
    }
    Numeric getSharePrice() const
    {
        return xaccSplitGetSharePrice(gobj());
    }
    Numeric getBalance() const
    {
        return xaccSplitGetBalance(gobj());
    }
    Numeric getClearedBalance() const
    {
        return xaccSplitGetClearedBalance(gobj());
    }
    Numeric getReconciledBalance() const
    {
        return xaccSplitGetReconciledBalance(gobj());
    }


    static SplitQList fromGList(GList* glist)
    {
        return gnc::fromGList<SplitQList>(glist);
    }
};


/** This is a temporary split which belongs to a temporary transaction
 * (class gnc::TmpTransaction). Each of this tmp splits has all data
 * fields just like a "real" split, but it is not (yet) added to the
 * respective Account and Book. In other words, it is not stored in
 * the book yet.
 *
 * For this reason this class supports a full copy-by-value, which
 * will create new independent instances of all data fields. */
class TmpSplit
{
public:
    /** Creates a new tmp split whose content is copied from the given
     * real transaction and it should belong to the given
     * TmpTransaction (but it is not added to the TmpTransaction's
     * split list here). */
    TmpSplit(const Glib::RefPtr<Split>& s, const TmpTransaction* parent_trans);

    /** Creates a new empty tmp split, with the Account pointer
     * initialized with the given value. */
    TmpSplit(::Account* account = NULL);

    /** Clears all data fields of this split. */
    void clear(::Account* account = NULL);

    /** Copies the content of this tmp split into the given real
     * transaction by allocating a new real gnc::Split and adding it
     * to the given real gnc::Transaction. */
    void copyInto(Glib::RefPtr<Transaction> t) const;

    ::Account* getAccount() const
    {
        return m_account;
    }
    void setAccount(::Account* v)
    {
        m_account = v;
    }

    const TmpTransaction* getParent() const
    {
        return m_parent;
    }
    void setParent(const TmpTransaction* v)
    {
        m_parent = v;
    }

    /** Returns a pointer to the "Other" split if it exists, or NULL
     * if none or multiple of them exist. */
    TmpSplit* getOtherSplit() const;

    Glib::ustring getMemo() const
    {
        return m_memo;
    }
    void setMemo(const Glib::ustring& v)
    {
        m_memo = v;
    }

    Glib::ustring getAction() const
    {
        return m_action;
    }
    void setAction(const Glib::ustring& v)
    {
        m_action = v;
    }

    char getReconcile() const
    {
        return m_reconcile;
    }
    void setReconcile(char v)
    {
        m_reconcile = v;
    }

    Numeric getAmount() const
    {
        return m_amount;
    }
    void setAmount(const Numeric& v)
    {
        m_amount = v;
    }

    Numeric getValue() const
    {
        return m_value;
    }
    void setValue(const Numeric& v)
    {
        m_value = v;
    }

private:
    ::Account* m_account;
    const TmpTransaction* m_parent;
    Glib::ustring m_memo;
    Glib::ustring m_action;
    char m_reconcile;
    Numeric m_amount;
    Numeric m_value;
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
Glib::RefPtr<gnc::Split> wrap(::Split* object, bool take_copy = false);
}


#endif
