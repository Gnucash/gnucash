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

#include <QString>
#include <QList>

namespace gnc
{
class Split;
typedef QList< ::Split*> SplitQList;
}

#include "gnc/WeakPointer.hpp"
#include "gnc/Account.hpp"
#include "gnc/Book.hpp"
#include "gnc/Transaction.hpp"

namespace gnc
{

class Split : public WeakPointer< ::Split >
{
public:
    typedef WeakPointer< ::Split > base_class;
    Split(element_type* ptr = 0)
            : base_class(ptr)
    { }

    Book getBook() const { return xaccSplitGetBook(get()); }
    Account getAccount() const { return xaccSplitGetAccount(get()); }
    void setAccount(Account& acc) { xaccSplitSetAccount(get(), acc.get()); }

    Transaction getParent() const { return xaccSplitGetParent(get()); }
    void setParent(Transaction& trans) { xaccSplitSetParent(get(), trans.get()); }

    QString getMemo() const { return QString::fromUtf8(xaccSplitGetMemo(get())); }
    void setMemo(const QString& v) { xaccSplitSetMemo(get(), v.toUtf8()); }

    QString getAction() const { return QString::fromUtf8(xaccSplitGetAction(get())); }
    void setAction(const QString& v) { xaccSplitSetAction(get(), v.toUtf8()); }

    char getReconcile() const { return xaccSplitGetReconcile(get()); }
    void setReconcile(char v) { xaccSplitSetReconcile(get(), v); }

    Split getOtherSplit() const { return xaccSplitGetOtherSplit(get()); }

    QString getCorrAccountFullName() const
    {
        char * r = xaccSplitGetCorrAccountFullName(get());
        QString result = QString::fromUtf8(r);
        g_free (r);
        return result;
    }
    QString getCorrAccountName() const { return QString::fromUtf8(xaccSplitGetCorrAccountName(get())); }
    QString getCorrAccountCode() const { return QString::fromUtf8(xaccSplitGetCorrAccountCode(get())); }


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

} // END namespace gnc

#endif
