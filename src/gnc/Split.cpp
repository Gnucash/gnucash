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
        : account(s.getAccount().get())
        , parent(parent_trans)
        , memo(s.getMemo())
        , action(s.getAction())
        , reconcile(s.getReconcile())
        , amount(s.getAmount())
        , value(s.getValue())
{}

TmpSplit::TmpSplit(::Account* _account)
{
    clear(_account);
}

TmpSplit* TmpSplit::getOtherSplit() const
{
    if (!parent)
        return NULL;
    const TmpTransaction& p = *parent;
    if (p.countSplits() != 2)
        return NULL;
    TmpTransaction::TmpSplitQList& splits = const_cast<TmpTransaction&>(p).getSplits();
    if (splits.front().getAccount() != account)
        return &splits.front();
    else
        return &splits.back();
}

void TmpSplit::clear(::Account* _account)
{
    account = _account;
    parent = NULL;
    memo.clear();
    action.clear();
    reconcile = '\0';
    amount = Numeric::zero();
    value = Numeric::zero();
}

void TmpSplit::copyInto(Transaction& t)
{
    Split s(xaccMallocSplit(t.getBook().get()));
    s.setAccount(account);
    s.setParent(t);
    s.setMemo(memo);
    s.setAction(action);
    s.setReconcile(reconcile);
    s.setAmount(amount);
    s.setValue(value);
}

} // END namespace gnc
