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

#include <QString>
#include <QList>

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

    void beginEdit() { xaccTransBeginEdit(get()); }
    void commitEdit() { xaccTransCommitEdit(get()); }
    void rollbackEdit() { xaccTransRollbackEdit(get()); }
    bool isOpen() const { return xaccTransIsOpen(get()); }


    QString getNum() const { return QString::fromUtf8(xaccTransGetNum(get())); }
    void setNum(const QString& v) { xaccTransSetNum(get(), v.toUtf8()); }

    QString getDescription() const { return QString::fromUtf8(xaccTransGetDescription(get())); }
    void setDescription(const QString& v) { xaccTransSetDescription(get(), v.toUtf8()); }

    QString getNotes() const { return QString::fromUtf8(xaccTransGetNotes(get())); }
    void setNotes(const QString& v) { xaccTransSetNotes(get(), v.toUtf8()); }

    int countSplits() const { return xaccTransCountSplits(get()); }
    Split findSplitByAccount(const Account& acc) const;
    void appendSplit(Split& split);
    Split getSplit(int i) const;
    int getSplitIndex(const Split& split) const;
    ::SplitList* getSplitList() const { return xaccTransGetSplitList(get()); }

    Commodity getCurrency() const { return xaccTransGetCurrency(get()); }
    void setCurrency(const Commodity& c) { xaccTransSetCurrency(get(), c.get()); }

    Numeric getImbalanceValue() const { return xaccTransGetImbalanceValue(get()); }
    bool isBalanced() const { return xaccTransIsBalanced(get()); }
    Numeric getAccountConvRate(const Account& acc) const { return xaccTransGetAccountConvRate(get(), acc.get()); }

    void setDatePosted(const QDate& d) { xaccTransSetDate(get(), d.day(), d.month(), d.year()); }
    void setDateEntered(const QDateTime& t) { xaccTransSetDateEnteredSecs(get(), t.toTime_t()); }
    QDate getDatePosted() const { return toQDate(xaccTransGetDatePostedGDate(get())); }
    QDateTime getDateEntered() const { return toQDateTime(xaccTransRetDateEnteredTS(get())); }

    static element_type* newInstance(const Book& b);
};

class TmpTransaction
{
public:
    typedef QList<TmpSplit> TmpSplitQList;

    TmpTransaction();
    TmpTransaction(const Transaction& t);

    void clear();
    void copyTo(Transaction& t) const;
    Transaction createAsReal() const;

    QString getNum() const { return num; }
    void setNum(const QString& v) { num = v; }

    QString getDescription() const { return description; }
    void setDescription(const QString& v) { description = v; }

    void push_back(const TmpSplit& s);
    const TmpSplitQList& getSplits() const { return splits; }
    TmpSplitQList& getSplits() { return splits; }
    int countSplits() const { return splits.size(); }

    Commodity getCommodity() const { return commodity; }
    void setCommodity(const Commodity& v) { commodity = v; }

    QDate getDatePosted() const { return datePosted; }
    void setDatePosted(const QDate& v) { datePosted = v; }

    QDateTime getDateEntered() const { return dateTimeEntered; }
    void setDateEntered(const QDateTime& v) { dateTimeEntered = v; }

private:
    QString num;
    QString description;
    QString notes;
    TmpSplitQList splits;
    Commodity commodity;
    QDate datePosted;
    QDateTime dateTimeEntered;
};

} // END namespace gnc

#endif
