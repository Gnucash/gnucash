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

#include "Account.hpp"
#include "Book.hpp"
#include "Transaction.hpp"
#include "private/Split_p.hpp"

namespace Glib
{

Glib::RefPtr<gnc::Split> wrap(::Split* object, bool take_copy)
{
    return Glib::RefPtr<gnc::Split>( dynamic_cast<gnc::Split*> (Glib::wrap_auto ((GObject*)(object), take_copy)) );
    //We use dynamic_cast<> in case of multiple inheritance.
}

} /* namespace Glib */

namespace gnc
{

/* The *_Class implementation: */

const Glib::Class& Split_Class::init()
{
    if (!gtype_) // create the GType if necessary
    {
        // Glib::Class has to know the class init function to clone custom types.
        class_init_func_ = &Split_Class::class_init_function;

        // This is actually just optimized away, apparently with no harm.
        // Make sure that the parent type has been created.
        //CppClassParent::CppObjectType::get_type();

        // Create the wrapper type, with the same class/instance size as the base type.
        register_derived_type(gnc_split_get_type());

        // Add derived versions of interfaces, if the C type implements any interfaces:

    }

    return *this;
}


void Split_Class::class_init_function(void* g_class, void* class_data)
{
    BaseClassType *const klass = static_cast<BaseClassType*>(g_class);
    CppClassParent::class_init_function(klass, class_data);
}


Glib::ObjectBase* Split_Class::wrap_new(GObject* object)
{
    return new Split((::Split*)object);
}


/* The implementation: */

::Split* Split::gobj_copy()
{
    reference();
    return gobj();
}

Split::Split(const Glib::ConstructParams& construct_params)
    : GncInstance(construct_params)
{

}

Split::Split(::Split* castitem)
    : GncInstance((::QofInstance*)(castitem))
{}


Split::~Split()
{}


Split::CppClassType Split::split_class_; // initialize static member

GType Split::get_type()
{
    return split_class_.init().get_type();
}


GType Split::get_base_type()
{
    return gnc_split_get_type();
}



Glib::RefPtr<Account> Split::get_account() const
{
    return Glib::wrap(xaccSplitGetAccount(gobj()));
}
void Split::set_account(Glib::RefPtr<Account> acc)
{
    if (acc) xaccSplitSetAccount(gobj(), acc->gobj());
}
void Split::set_account(::Account* acc)
{
    xaccSplitSetAccount(gobj(), acc);
}


Glib::RefPtr<Transaction> Split::get_parent() const
{
    return Glib::wrap(xaccSplitGetParent(gobj()));
}
void Split::set_parent(Glib::RefPtr<Transaction> trans)
{
    if (trans) xaccSplitSetParent(gobj(), trans->gobj());
}
void Split::set_parent(Transaction& trans)
{
    xaccSplitSetParent(gobj(), trans.gobj());
}

Glib::RefPtr<Split> Split::get_other_split() const
{
    return Glib::wrap(xaccSplitGetOtherSplit(gobj()));
}


TmpSplit::TmpSplit(const Glib::RefPtr<Split>& s, const TmpTransaction* parent_trans)
    : m_account(s->get_account()->gobj())
    , m_parent(parent_trans)
    , m_memo(s->get_memo())
    , m_action(s->get_action())
    , m_reconcile(s->get_reconcile())
    , m_amount(s->get_amount())
    , m_value(s->get_value())
{}

TmpSplit::TmpSplit(::Account* account)
{
    clear(account);
}

TmpSplit* TmpSplit::get_other_split() const
{
    if (!m_parent)
        return NULL;
    const TmpTransaction& p = *m_parent;
    if (p.get_num_splits() != 2)
        return NULL;
    TmpTransaction::TmpSplitList& splits = const_cast<TmpTransaction&>(p).get_splits();
    if (splits.front().get_account() != m_account)
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

void TmpSplit::copy_into(Glib::RefPtr<Transaction> t) const
{
    g_assert(t);
    Glib::RefPtr<Split> s(Glib::wrap(xaccMallocSplit(t->get_book()->gobj())));
    s->set_account(m_account);
    s->set_parent(t);
    s->set_memo(m_memo);
    s->set_action(m_action);
    s->set_reconcile(m_reconcile);
    s->set_amount(m_amount);
    s->set_value(m_value);
}

} // END namespace gnc
