/*
 * Transaction.cpp
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

#include "Transaction.hpp"
#include "Split.hpp"
#include <cassert>
#if HAVE_GLIBMM_VECTORUTILS_H
// new in glibmm-2.29
#include <glibmm/vectorutils.h>
#endif
#include "private/Transaction_p.hpp"

namespace Glib
{

Glib::RefPtr<gnc::Transaction> wrap(::Transaction* object, bool take_copy)
{
    return Glib::RefPtr<gnc::Transaction>( dynamic_cast<gnc::Transaction*> (Glib::wrap_auto ((GObject*)(object), take_copy)) );
    //We use dynamic_cast<> in case of multiple inheritance.
}

} /* namespace Glib */

namespace gnc
{

/* The *_Class implementation: */

const Glib::Class& Transaction_Class::init()
{
    if (!gtype_) // create the GType if necessary
    {
        // Glib::Class has to know the class init function to clone custom types.
        class_init_func_ = &Transaction_Class::class_init_function;

        // This is actually just optimized away, apparently with no harm.
        // Make sure that the parent type has been created.
        //CppClassParent::CppObjectType::get_type();

        // Create the wrapper type, with the same class/instance size as the base type.
        register_derived_type(gnc_transaction_get_type());

        // Add derived versions of interfaces, if the C type implements any interfaces:

    }

    return *this;
}


void Transaction_Class::class_init_function(void* g_class, void* class_data)
{
    BaseClassType *const klass = static_cast<BaseClassType*>(g_class);
    CppClassParent::class_init_function(klass, class_data);
}


Glib::ObjectBase* Transaction_Class::wrap_new(GObject* object)
{
    return new Transaction((::Transaction*)object);
}


/* The implementation: */

::Transaction* Transaction::gobj_copy()
{
    reference();
    return gobj();
}

Transaction::Transaction(const Glib::ConstructParams& construct_params)
    : GncInstance(construct_params)
{

}

Transaction::Transaction(::Transaction* castitem)
    : GncInstance((::QofInstance*)(castitem))
{}


Transaction::~Transaction()
{}


Transaction::CppClassType Transaction::transaction_class_; // initialize static member

GType Transaction::get_type()
{
    return transaction_class_.init().get_type();
}


GType Transaction::get_base_type()
{
    return gnc_transaction_get_type();
}



Glib::RefPtr<Split> Transaction::findSplitByAccount(const Account& acc) const
{
    return Glib::wrap(xaccTransFindSplitByAccount(gobj(), acc.gobj()));
}
Glib::RefPtr<Split> Transaction::getSplit(int i) const
{
    return Glib::wrap(xaccTransGetSplit(gobj(), i));
}
void Transaction::appendSplit(Split& split)
{
    xaccSplitSetParent(split.gobj(), gobj());
}
int Transaction::getSplitIndex(const Split& split) const
{
    return xaccTransGetSplitIndex(gobj(), split.gobj());
}
::Transaction* Transaction::newInstance(const ::QofBook* b)
{
    return xaccMallocTransaction (const_cast< ::QofBook*>(b));
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
    //, m_dateTimeEntered(t.getDateEntered())
{
    SplitQList slist
#if HAVE_GLIBMM_VECTORUTILS_H
    = Glib::ListHandlier<Glib::RefPtr<Split> >::list_to_vector(t.getSplitList());
#else
    ;
#endif
    for (SplitQList::const_iterator iter = slist.begin(); iter != slist.end(); ++iter)
    {
        m_splits.push_back(TmpSplit(Glib::wrap(*iter), this));
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
    m_datePosted = Glib::Date();
    //m_dateTimeEntered = QDateTime();
    for (int i = 0; i < m_splits.size(); ++i)
    {
        TmpSplit& split = m_splits[i];
        split.clear();
        split.setParent(this);
    }
}

void TmpTransaction::copyTo(Glib::RefPtr<Transaction> t) const
{
    assert(t);
    t->setNum(m_num);
    t->setDescription(m_description);
    if (!m_notes.empty())
        t->setNotes(m_notes);
    t->setCurrency(m_commodity);
    t->setDatePosted(m_datePosted);
    //t->setDateEntered(m_dateTimeEntered);
    for (int i = 0; i < m_splits.size(); ++i)
    {
        //m_splits[i].copyInto(t);
    }
}
#if 0
Glib::RefPtr<Transaction> TmpTransaction::createAsReal() const
{
    assert (!m_splits.empty());
    Glib::RefPtr<Account> acc(Glib::wrap(m_splits.front().getAccount()));
    assert (acc);
    Glib::RefPtr<Book> book(acc->getBook());
    assert (book);
    Glib::RefPtr<Transaction> trans(Glib::wrap(Transaction::newInstance(book->gobj())));
    trans->beginEdit();
    copyTo(trans);
    trans->commitEdit();
    return trans;
}
#endif
void TmpTransaction::push_back(const TmpSplit& s)
{
    m_splits.push_back(s);
    m_splits.back().setParent(this);
}

} // END namespace gnc
