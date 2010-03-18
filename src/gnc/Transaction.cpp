/*
 * Transaction.cpp
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

#include "Transaction.hpp"
#include "gnc/Split.hpp"

namespace gnc
{

Split Transaction::findSplitByAccount(const Account& acc) const
{
    return xaccTransFindSplitByAccount(get(), acc.get());
}
Split Transaction::getSplit(int i) const
{
    return xaccTransGetSplit(get(), i);
}
void Transaction::appendSplit(Split& split)
{
    xaccSplitSetParent(split.get(), get());
}
int Transaction::getSplitIndex(const Split& split) const
{
    return xaccTransGetSplitIndex(get(), split.get());
}
Transaction::element_type* Transaction::newInstance(const Book& b)
{
    return xaccMallocTransaction (b.get());
}

// ////////////////////////////////////////////////////////////

TmpTransaction::TmpTransaction()
{
    clear();
}
TmpTransaction::TmpTransaction(const Transaction& t)
        : num(t.getNum())
        , description(t.getDescription())
        , notes(t.getNotes())
        , commodity(t.getCurrency())
        , datePosted(t.getDatePosted())
        , dateTimeEntered(t.getDateEntered())
{
    SplitQList slist = Split::fromGList(t.getSplitList());
    Q_FOREACH(Split s, slist)
    {
        splits.push_back(TmpSplit(s, this));
    }
}
void TmpTransaction::clear()
{
    num.clear();
    description.clear();
    notes.clear();
    commodity.reset();
    datePosted = QDate();
    dateTimeEntered = QDateTime();
    splits.clear();
}
void TmpTransaction::copyTo(Transaction& t) const
{
    t.setNum(num);
    t.setDescription(description);
    if (!notes.isEmpty())
        t.setNotes(notes);
    t.setCurrency(commodity);
    t.setDatePosted(datePosted);
    t.setDateEntered(dateTimeEntered);
    Q_FOREACH(TmpSplit s, splits)
    {
        s.copyInto(t);
    }
}
void TmpTransaction::createAsReal() const
{
    Q_ASSERT (!splits.isEmpty());
    Account acc(splits.front().getAccount());
    Q_ASSERT (acc);
    Book book(acc.getBook());
    Q_ASSERT (book);
    Transaction trans(Transaction::newInstance(book));
    trans.beginEdit();
    copyTo(trans);
    trans.commitEdit();
}
void TmpTransaction::push_back(const TmpSplit& s)
{
    splits.push_back(s);
    splits.back().setParent(this);
}

} // END namespace gnc
