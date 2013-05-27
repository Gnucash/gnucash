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



Glib::RefPtr<Split> Transaction::find_split_by_account(const Account& acc) const
{
    return Glib::wrap(xaccTransFindSplitByAccount(gobj(), acc.gobj()));
}
Glib::RefPtr<Split> Transaction::get_split(int i) const
{
    return Glib::wrap(xaccTransGetSplit(gobj(), i));
}
void Transaction::append_split(Glib::RefPtr<Split> split)
{
    g_assert(split);
    xaccSplitSetParent(split->gobj(), gobj());
}
int Transaction::get_split_index(const Split& split) const
{
    return xaccTransGetSplitIndex(gobj(), split.gobj());
}
::Transaction* Transaction::new_instance(const Glib::RefPtr<Book> b)
{
    if (b)
        return xaccMallocTransaction (const_cast< ::QofBook*>(b->gobj()));
    else
        return NULL;
}

// ////////////////////////////////////////////////////////////

TmpTransaction::TmpTransaction()
{
    clear();
}
TmpTransaction::TmpTransaction(const Transaction& t)
    : m_num(t.get_num())
    , m_description(t.get_description())
    , m_notes(t.get_notes())
    , m_commodity(t.get_currency())
    , m_datePosted(t.get_date_posted())
    , m_dateTimeEntered(t.get_date_entered_tt())
{
    SplitQList slist = Split::from_glist(t.get_split_list());
    for (SplitQList::const_iterator iter = slist.begin(); iter != slist.end(); ++iter)
    {
        m_splits.push_back(TmpSplit(Glib::wrap(*iter), this));
    }
}

void TmpTransaction::clear()
{
    m_splits.clear();
    reset_content();
}

void TmpTransaction::reset_content()
{
    m_num.clear();
    m_description.clear();
    m_notes.clear();
    m_commodity.reset();
    m_datePosted = Glib::Date();
    m_dateTimeEntered = 0;
    for (int i = 0; i < m_splits.size(); ++i)
    {
        TmpSplit& split = m_splits[i];
        split.clear();
        split.set_parent(this);
    }
}

void TmpTransaction::copy_to(Glib::RefPtr<Transaction> t) const
{
    assert(t);
    t->set_num(m_num);
    t->set_description(m_description);
    if (!m_notes.empty())
        t->set_notes(m_notes);
    t->set_currency(m_commodity);
    t->set_date_posted(m_datePosted);
    t->set_date_entered(m_dateTimeEntered);
    for (int i = 0; i < m_splits.size(); ++i)
    {
        m_splits[i].copy_into(t);
    }
}

Glib::RefPtr<Transaction> TmpTransaction::create_as_real() const
{
    assert (!m_splits.empty());
    Glib::RefPtr<Account> acc(Glib::wrap(m_splits.front().get_account()));
    assert (acc);
    Glib::RefPtr<Book> book(acc->get_book());
    assert (book);
    Glib::RefPtr<Transaction> trans(Glib::wrap(Transaction::new_instance(book)));
    trans->begin_edit();
    copy_to(trans);
    trans->commit_edit();
    return trans;
}

void TmpTransaction::push_back(const TmpSplit& s)
{
    m_splits.push_back(s);
    m_splits.back().set_parent(this);
}

} // END namespace gnc
