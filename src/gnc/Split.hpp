/*
 * Split.hpp
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

#ifndef GNC_SPLIT_HPP
#define GNC_SPLIT_HPP

// gnucash includes
#include "config.h"
extern "C"
{
#include "qof.h"
#include "engine/Split.h"
}

#include <QtCore/QList>
#include <QtCore/QMetaType>
#include <QtCore/QString>

#include "gnc/GncInstance.hpp"
#include "gnc/Numeric.hpp"

namespace gnc
{
class Book;
class Account;
class Transaction;
class TmpTransaction;

typedef QList< ::Split*> SplitQList;


/** Wrapper around a gnucash ::Split pointer with C++ methods for
 * easier setter and getter access.
 *
 * Unfortunately this object has no information about whether the
 * underlying gnucash ::Split object is still alive or has been
 * deleted.
 */
class Split : public GncInstance< ::Split >
{
public:
    typedef GncInstance< ::Split > base_class;
    Split(element_type* ptr = 0)
            : base_class(ptr)
    { }

    Account getAccount() const;
    void setAccount(Account acc);
    void setAccount(::Account* acc);

    Transaction getParent() const;
    void setParent(Transaction& trans);

    QString getMemo() const { return QString::fromUtf8(xaccSplitGetMemo(get())); }
    void setMemo(const QString& v) { xaccSplitSetMemo(get(), v.toUtf8()); }

    QString getAction() const { return QString::fromUtf8(xaccSplitGetAction(get())); }
    void setAction(const QString& v) { xaccSplitSetAction(get(), v.toUtf8()); }

    char getReconcile() const { return xaccSplitGetReconcile(get()); }
    void setReconcile(char v) { xaccSplitSetReconcile(get(), v); }

    Split getOtherSplit() const { return xaccSplitGetOtherSplit(get()); }

    QString getCorrAccountFullName() const
    {
        return gchar_to_QString(xaccSplitGetCorrAccountFullName(get()));
    }
    QString getCorrAccountName() const { return QString::fromUtf8(xaccSplitGetCorrAccountName(get())); }
    QString getCorrAccountCode() const { return QString::fromUtf8(xaccSplitGetCorrAccountCode(get())); }

    void setAmount(const Numeric& amount) { xaccSplitSetAmount(get(), amount); }
    Numeric getAmount() const { return xaccSplitGetAmount(get()); }
    void setValue(const Numeric& value) { xaccSplitSetValue(get(), value); }
    Numeric getValue() const { return xaccSplitGetValue(get()); }
    Numeric getSharePrice() const { return xaccSplitGetSharePrice(get()); }
    Numeric getBalance() const { return xaccSplitGetBalance(get()); }
    Numeric getClearedBalance() const { return xaccSplitGetClearedBalance(get()); }
    Numeric getReconciledBalance() const { return xaccSplitGetReconciledBalance(get()); }



    static SplitQList fromGList(GList* glist)
    {
        SplitQList result;
        GList* list = glist;
        while (list)
        {
            result.append(reinterpret_cast< ::Split*>(list->data));
            list = g_list_next(list);
        }
        return result;
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
    TmpSplit(const Split& s, const TmpTransaction* parent_trans);

    /** Creates a new empty tmp split, with the Account pointer
     * initialized with the given value. */
    TmpSplit(::Account* account = NULL);

    /** Clears all data fields of this split. */
    void clear(::Account* account = NULL);

    /** Copies the content of this tmp split into the given real
     * transaction by allocating a new real gnc::Split and adding it
     * to the given real gnc::Transaction. */
    void copyInto(Transaction& t) const;

    ::Account* getAccount() const { return m_account; }
    void setAccount(::Account* v) { m_account = v;}

    const TmpTransaction* getParent() const { return m_parent; }
    void setParent(const TmpTransaction* v) { m_parent = v; }

    /** Returns a pointer to the "Other" split if it exists, or NULL
     * if none or multiple of them exist. */
    TmpSplit* getOtherSplit() const;

    QString getMemo() const { return m_memo; }
    void setMemo(const QString& v) { m_memo = v; }

    QString getAction() const { return m_action; }
    void setAction(const QString& v) { m_action = v; }

    char getReconcile() const { return m_reconcile; }
    void setReconcile(char v) { m_reconcile = v; }

    Numeric getAmount() const { return m_amount; }
    void setAmount(const Numeric& v) { m_amount = v; }

    Numeric getValue() const { return m_value; }
    void setValue(const Numeric& v) { m_value = v; }

private:
    ::Account* m_account;
    const TmpTransaction* m_parent;
    QString m_memo;
    QString m_action;
    char m_reconcile;
    Numeric m_amount;
    Numeric m_value;
};

} // END namespace gnc

Q_DECLARE_METATYPE(gnc::Split)
Q_DECLARE_METATYPE(gnc::TmpSplit)

#endif
