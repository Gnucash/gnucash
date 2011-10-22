/*
 * Transaction.hpp
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

#ifndef GNC_TRANSACTION_HPP
#define GNC_TRANSACTION_HPP

// gnucash includes
#include "config.h"
extern "C"
{
#include "qof.h"
#include "engine/Transaction.h"
}

#include "gnc/Account.hpp"
#include "gnc/Book.hpp"
#include "gnc/Commodity.hpp"
#include "gnc/Numeric.hpp"
#include "gnc/GncInstance.hpp"

#include <QtCore/QString>
#include <QtCore/QList>
#include <QtCore/QMetaType>

namespace gnc
{

class Split;
class TmpSplit;

/** Wrapper around a gnucash ::Transaction pointer with C++ methods for
 * easier setter and getter access.
 *
 * Unfortunately this object has no information about whether the
 * underlying gnucash ::Transaction object is still alive or has been
 * deleted.
 */
class Transaction : public GncInstance< ::Transaction >
{
public:
    typedef GncInstance< ::Transaction > base_class;
    Transaction(element_type* ptr = 0)
            : base_class(ptr)
    { }

    void beginEdit() { xaccTransBeginEdit(gobj()); }
    void commitEdit() { xaccTransCommitEdit(gobj()); }
    void rollbackEdit() { xaccTransRollbackEdit(gobj()); }
    bool isOpen() const { return xaccTransIsOpen(gobj()); }


    QString getNum() const { return QString::fromUtf8(xaccTransGetNum(gobj())); }
    void setNum(const QString& v) { xaccTransSetNum(gobj(), v.toUtf8()); }

    QString getDescription() const { return QString::fromUtf8(xaccTransGetDescription(gobj())); }
    void setDescription(const QString& v) { xaccTransSetDescription(gobj(), v.toUtf8()); }

    QString getNotes() const { return QString::fromUtf8(xaccTransGetNotes(gobj())); }
    void setNotes(const QString& v) { xaccTransSetNotes(gobj(), v.toUtf8()); }

    int countSplits() const { return xaccTransCountSplits(gobj()); }
    Split findSplitByAccount(const Account& acc) const;
    void appendSplit(Split& split);
    Split getSplit(int i) const;
    int getSplitIndex(const Split& split) const;
    ::SplitList* getSplitList() const { return xaccTransGetSplitList(gobj()); }

    Commodity getCurrency() const { return xaccTransGetCurrency(gobj()); }
    void setCurrency(const Commodity& c) { xaccTransSetCurrency(gobj(), c.gobj()); }

    Numeric getImbalanceValue() const { return xaccTransGetImbalanceValue(gobj()); }
    bool isBalanced() const { return xaccTransIsBalanced(gobj()); }
    Numeric getAccountConvRate(const Account& acc) const { return xaccTransGetAccountConvRate(gobj(), acc.gobj()); }

    void setDatePosted(const QDate& d) { xaccTransSetDate(gobj(), d.day(), d.month(), d.year()); }
    void setDateEntered(const QDateTime& t) { xaccTransSetDateEnteredSecs(gobj(), t.toTime_t()); }
    QDate getDatePosted() const { return toQDate(xaccTransGetDatePostedGDate(gobj())); }
    QDateTime getDateEntered() const { return toQDateTime(xaccTransRetDateEnteredTS(gobj())); }

    static element_type* newInstance(const Book& b);
};


/** This is a temporary transaction. Each of this tmp transactions has
 * all data fields just like a "real" transaction, but it is not (yet)
 * added to the respective Account and Book. In other words, it is not
 * stored in the book yet.
 *
 * For this reason this class supports a full copy-by-value, which
 * will create new independent instances of all data fields. */
class TmpTransaction
{
public:
    typedef QList<TmpSplit> TmpSplitQList;

    /** Creates an empty tmp transaction */
    TmpTransaction();

    /** Creates a tmp transaction whose content is copied from the
     * given real transaction */
    TmpTransaction(const Transaction& t);

    /** Clears all data fields of this transaction, including deletion
     * of all splits stored here. */
    void clear();

    /** Clears all data fields, but does not delete the splits and
     * instead only resets the data fields of all splits */
    void resetContent();

    /** Copies the content of this tmp transaction into the given real
     * transaction. */
    void copyTo(Transaction& t) const;

    /** Allocates a new real transaction in the Book and Account as
     * stored in the tmp transaction, copies the content of this tmp
     * transaction into the newly allocated one, and returns the
     * pointer to the newly created real transaction. */
    Transaction createAsReal() const;

    QString getNum() const { return m_num; }
    void setNum(const QString& v) { m_num = v; }

    QString getDescription() const { return m_description; }
    void setDescription(const QString& v) { m_description = v; }

    void push_back(const TmpSplit& s);
    const TmpSplitQList& getSplits() const { return m_splits; }
    TmpSplitQList& getSplits() { return m_splits; }
    int countSplits() const { return m_splits.size(); }

    Commodity getCommodity() const { return m_commodity; }
    void setCommodity(const Commodity& v) { m_commodity = v; }

    QDate getDatePosted() const { return m_datePosted; }
    void setDatePosted(const QDate& v) { m_datePosted = v; }

    QDateTime getDateEntered() const { return m_dateTimeEntered; }
    void setDateEntered(const QDateTime& v) { m_dateTimeEntered = v; }

private:
    QString m_num;
    QString m_description;
    QString m_notes;
    TmpSplitQList m_splits;
    Commodity m_commodity;
    QDate m_datePosted;
    QDateTime m_dateTimeEntered;
};

} // END namespace gnc

Q_DECLARE_METATYPE(gnc::Transaction)
Q_DECLARE_METATYPE(gnc::TmpTransaction)

#endif
