/*
 * Split.cpp
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

#include "Split.hpp"

#include "gnc/Account.hpp"
#include "gnc/Book.hpp"
#include "gnc/Transaction.hpp"

namespace gnc
{

Account Split::getAccount() const { return xaccSplitGetAccount(get()); }
void Split::setAccount(Account acc) { xaccSplitSetAccount(get(), acc.get()); }
void Split::setAccount(::Account* acc) { xaccSplitSetAccount(get(), acc); }


Transaction Split::getParent() const { return xaccSplitGetParent(get()); }
void Split::setParent(Transaction& trans) { xaccSplitSetParent(get(), trans.get()); }


TmpSplit::TmpSplit(const Split& s, const TmpTransaction* parent_trans)
        : m_account(s.getAccount().get())
        , m_parent(parent_trans)
        , m_memo(s.getMemo())
        , m_action(s.getAction())
        , m_reconcile(s.getReconcile())
        , m_amount(s.getAmount())
        , m_value(s.getValue())
{}

TmpSplit::TmpSplit(::Account* account)
{
    clear(account);
}

TmpSplit* TmpSplit::getOtherSplit() const
{
    if (!m_parent)
        return NULL;
    const TmpTransaction& p = *m_parent;
    if (p.countSplits() != 2)
        return NULL;
    TmpTransaction::TmpSplitQList& splits = const_cast<TmpTransaction&>(p).getSplits();
    if (splits.front().getAccount() != m_account)
        return &splits.front();
    else
        return &splits.back();
}

void TmpSplit::clear(::Account* account)
{
    m_account = account;
    m_parent = NULL;
    m_memo.clear();
    m_action.clear();
    m_reconcile = NREC;
    m_amount = Numeric::zero();
    m_value = Numeric::zero();
}

void TmpSplit::copyInto(Transaction& t) const
{
    Split s(xaccMallocSplit(t.getBook().get()));
    s.setAccount(m_account);
    s.setParent(t);
    s.setMemo(m_memo);
    s.setAction(m_action);
    s.setReconcile(m_reconcile);
    s.setAmount(m_amount);
    s.setValue(m_value);
}

} // END namespace gnc
