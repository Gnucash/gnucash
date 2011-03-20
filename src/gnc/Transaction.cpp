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
        : m_num(t.getNum())
        , m_description(t.getDescription())
        , m_notes(t.getNotes())
        , m_commodity(t.getCurrency())
        , m_datePosted(t.getDatePosted())
        , m_dateTimeEntered(t.getDateEntered())
{
    SplitQList slist = Split::fromGList(t.getSplitList());
    Q_FOREACH(Split s, slist)
    {
        m_splits.push_back(TmpSplit(s, this));
    }
}

void TmpTransaction::clear()
{
    m_splits.clear();
    resetContent();
}

void TmpTransaction::resetContent()
{
    m_num.clear();
    m_description.clear();
    m_notes.clear();
    m_commodity.reset();
    m_datePosted = QDate();
    m_dateTimeEntered = QDateTime();
    for (int i = 0; i < m_splits.size(); ++i)
    {
        TmpSplit& split = m_splits[i];
        split.clear();
        split.setParent(this);
    }
}

void TmpTransaction::copyTo(Transaction& t) const
{
    t.setNum(m_num);
    t.setDescription(m_description);
    if (!m_notes.isEmpty())
        t.setNotes(m_notes);
    t.setCurrency(m_commodity);
    t.setDatePosted(m_datePosted);
    t.setDateEntered(m_dateTimeEntered);
    for (int i = 0; i < m_splits.size(); ++i)
    {
        m_splits[i].copyInto(t);
    }
}

Transaction TmpTransaction::createAsReal() const
{
    Q_ASSERT (!m_splits.isEmpty());
    Account acc(m_splits.front().getAccount());
    Q_ASSERT (acc);
    Book book(acc.getBook());
    Q_ASSERT (book);
    Transaction trans(Transaction::newInstance(book));
    trans.beginEdit();
    copyTo(trans);
    trans.commitEdit();
    return trans;
}

void TmpTransaction::push_back(const TmpSplit& s)
{
    m_splits.push_back(s);
    m_splits.back().setParent(this);
}

} // END namespace gnc
