/*
 * Cmd.hpp
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

#include "Cmd.hpp"
#include "gncmm/Split.hpp"
#include "gncmm/Account.hpp"
#include "gncmm/Transaction.hpp"
#include "gnc/conv.hpp"
#include <QObject>

namespace gnc
{

// Explicit instantiations to check for compiler errors
template class Cmd<Account, QString>;

namespace cmd
{

// ////////////////////////////////////////////////////////////

QUndoCommand* setSplitMemo(Glib::RefPtr<Split> t, const QString& newValue)
{
    return new Cmd<Split, Glib::ustring>(QObject::tr("Edit Split Memo"),
                                         t, &Split::set_memo,
                                         t->get_memo(), q2g(newValue));
}

QUndoCommand* setSplitAction(Glib::RefPtr<Split> t, const QString& newValue)
{
    return new Cmd<Split, Glib::ustring>(QObject::tr("Edit Split Action"),
                                         t, &Split::set_action,
                                         t->get_action(), q2g(newValue));
}

QUndoCommand* setSplitReconcile(Glib::RefPtr<Split> t, char newValue)
{
    if (newValue == t->get_reconcile())
        return NULL;
    // Special third argument: The setter function takes a value
    // directly, instead of a const-reference, so the template type
    // must be given explicitly.
    return new Cmd<Split, char, void (Split::*)(char)>(QObject::tr("Edit Split Reconcile"),
            t, &Split::set_reconcile,
            t->get_reconcile(), newValue);
}

QUndoCommand* setSplitAccount(Glib::RefPtr<Split> t, Glib::RefPtr<Account> newValue)
{
    // Temporary function pointer "tmp" to resolve the ambiguous
    // overload "setAccount()".
    void (Split::*tmp)(Glib::RefPtr<Account>) = &Split::set_account;
    return new Cmd<Split, Glib::RefPtr<Account>, void (Split::*)(Glib::RefPtr<Account>)>(QObject::tr("Edit Split Account"),
            t, tmp,
            t->get_account(), newValue);
}

QUndoCommand* setSplitAmount(Glib::RefPtr<Split> t, const Numeric& newValue)
{
    return new Cmd<Split, Numeric>(QObject::tr("Edit Split Amount"),
                                   t, &Split::set_amount,
                                   t->get_amount(), newValue);
}

QUndoCommand* setSplitValue(Glib::RefPtr<Split> t, const Numeric& newValue)
{
    return new Cmd<Split, Numeric>(QObject::tr("Edit Split Value"),
                                   t, &Split::set_value,
                                   t->get_value(), newValue);
}

// ////////////////////////////////////////////////////////////

class SplitValueAndAmountCmd : public QUndoCommand
{
public:
    typedef QUndoCommand base_class;
    typedef Split target_type;
    typedef Numeric value_type;

    /** Constructor without a getter-function but instead the previous
     * value given directly.
     */
    SplitValueAndAmountCmd(const QString& text,
                           Glib::RefPtr<Split> targetPtr,
                           const value_type& previousValue,
                           const value_type& newValue,
                           QUndoCommand *parent = 0)
        : base_class(text, parent)
        , m_target(targetPtr)
        , m_previousValue(previousValue)
        , m_newValue(newValue)
    {
        Q_ASSERT(m_target);
    }

    virtual void redo()
    {
        set(m_newValue);
    }
    virtual void undo()
    {
        set(m_previousValue);
    }
private:
    void set(const value_type& value)
    {
        Glib::RefPtr<Transaction> trans = m_target->get_parent();
        if (!trans || trans->get_num_splits() != 2)
            return;
        Glib::RefPtr<Split> other = m_target->get_other_split();
        Q_ASSERT(other);
        Glib::RefPtr<Commodity> originCommodity = m_target->get_account()->get_commodity();
        Glib::RefPtr<Commodity> transCommodity = trans->get_currency();
        Glib::RefPtr<Commodity> otherCommodity = other->get_account()->get_commodity();
        if (originCommodity != transCommodity
                || transCommodity != otherCommodity)
            return;

        trans->begin_edit();
        m_target->set_value(value);
        m_target->set_amount(value);
        Numeric valueNeg = value.neg();
        other->set_amount(valueNeg);
        other->set_value(valueNeg);
        trans->commit_edit();
    }

protected:
    Glib::RefPtr<target_type> m_target;
    value_type m_previousValue;
    value_type m_newValue;

};

QUndoCommand* setSplitValueAndAmount(Glib::RefPtr<Split> t, const Numeric& newValue)
{
    return new SplitValueAndAmountCmd(QObject::tr("Edit Transaction Value"),
                                      t, t->get_value(), newValue);
}

// ////////////////////////////////////////////////////////////

QUndoCommand* setTransactionNum(Glib::RefPtr<Transaction> t, const QString& newValue)
{
    if (newValue == g2q(t->get_num()))
        return NULL;
    return new Cmd<Transaction, Glib::ustring>(QObject::tr("Edit Transaction Number"),
            t, &Transaction::set_num,
            t->get_num(), q2g(newValue));
}

QUndoCommand* setTransactionDescription(Glib::RefPtr<Transaction> t, const QString& newValue)
{
    if (newValue == g2q(t->get_description()))
        return NULL;
    return new Cmd<Transaction, Glib::ustring>(QObject::tr("Edit Transaction Description"),
            t, &Transaction::set_description,
            t->get_description(), q2g(newValue));
}

QUndoCommand* setTransactionNotes(Glib::RefPtr<Transaction> t, const QString& newValue)
{
    return new Cmd<Transaction, Glib::ustring>(QObject::tr("Edit Transaction Notes"),
            t, &Transaction::set_notes,
            t->get_notes(), q2g(newValue));
}

QUndoCommand* setTransactionDate(Glib::RefPtr<Transaction> t, const QDate& newValue)
{
    if (newValue == g2q(t->get_date_posted()))
        return NULL;
    return new Cmd<Transaction, Glib::Date>(QObject::tr("Edit Transaction Date"),
                                            t, &Transaction::set_date_posted,
                                            t->get_date_posted(), q2g(newValue));
}

// ////////////////////////////////////////////////////////////

class TransactionDestroyCmd : public QUndoCommand
{
public:
    typedef QUndoCommand base_class;
    typedef Transaction target_type;

    /** Constructor
     */
    TransactionDestroyCmd(const QString& text,
                          Glib::RefPtr<target_type>& targetPtr,
                          QUndoCommand *parent = 0)
        : base_class(text, parent)
        , m_target(targetPtr)
        , m_previousValue(*targetPtr.operator->())
        , m_book(m_target->get_book())
    {
        Q_ASSERT(m_target);
    }

    virtual void redo()
    {
        xaccTransDestroy(m_target->gobj());
        m_target.reset();
    }

    virtual void undo()
    {
        m_target = Glib::wrap(Transaction::new_instance(m_book));
        m_target->begin_edit();
        m_previousValue.copy_to(m_target);
        m_target->commit_edit();
        // Could also use m_previousValue.createAsReal()
    }

protected:
    Glib::RefPtr<Transaction> m_target;
    TmpTransaction m_previousValue;
    Glib::RefPtr<Book> m_book;
};

QUndoCommand* destroyTransaction(Glib::RefPtr<Transaction> t)
{
    return new TransactionDestroyCmd(QObject::tr("Delete Transaction"),
                                     t);
}

// ////////////////////////////////////////////////////////////

/** This is another templated implementation of a QUndoCommand
 * class, this time with keeping a direct reference to the target object.
 *
 * This implements the Command pattern: Any instance of this
 * class represents the change of one member variable (of type ValueT)
 * in an instance of a TargetT object.
 *
 * As we need to keep a reference to the original object, it is
 * relevant who owns the life cycle of that original object. For
 * simplicity, we constrain this current implementation only to
 * classes which are implementations of a WeakPointer<...> which by
 * definition keeps a C pointer to the original object, and we hope
 * that one lives longer than we do.
 */
template < class TargetT, class ValueT, typename SetterFunc = void (TargetT::*)(const ValueT&) >
class CmdRef : public QUndoCommand
{
public:
    /// The base class
    typedef QUndoCommand base_class;

    /// Type of the target object on which this command is applied
    typedef TargetT target_type;

    /// Type of the value that is set by this command
    typedef ValueT value_type;

    /// Type of the setter function to set the value in the target object
    typedef SetterFunc setter_func;

    /** Constructor with the previous value given directly.
     *
     * @param text The QUndoCommand's text which will be displayed in the Undo action.
     * @param targetRef Reference to the target object on which this command is applied.
     * @param setter Pointer to function which sets the value in the target object
     * @param previousValue The previous value, in case this command needs to be undone
     * @param newValue The new value to be set
     * @param parent The parent QUndoCommand instance, or NULL.
     */
    CmdRef(const QString& text,
           TargetT& targetRef,
           setter_func setter,
           const value_type& previousValue,
           const value_type& newValue,
           QUndoCommand *parent = 0)
        : base_class(text, parent)
        , m_target(targetRef)
        , m_setter(setter)
        , m_previousValue(previousValue)
        , m_newValue(newValue)
    {
        Q_ASSERT(m_setter);
    }

    virtual void redo()
    {
        set(m_newValue);
    }
    virtual void undo()
    {
        set(m_previousValue);
    }

private:
    void set(const value_type& value)
    {
        // Call the pointer-to-member which is stored in m_setter.
        (m_target.*m_setter)(value);
    }

protected:
    target_type& m_target;
    setter_func m_setter;
    value_type m_previousValue;
    value_type m_newValue;
};


QUndoCommand* setSplitAccount(TmpSplit& t, Glib::RefPtr<Account> newValue)
{
    return new CmdRef<TmpSplit, ::Account*, void(TmpSplit::*)(::Account*)>(QObject::tr("Edit Split Account"),
            t, &TmpSplit::set_account,
            t.get_account(), newValue->gobj());
}
QUndoCommand* setSplitReconcile(TmpSplit& t, char newValue)
{
    // Special third argument: The setter function takes a value
    // directly, instead of a const-reference, so the template type
    // must be given explicitly.
    return new CmdRef<TmpSplit, char, void (TmpSplit::*)(char)>(QObject::tr("Edit Split Reconcile"),
            t, &TmpSplit::set_reconcile,
            t.get_reconcile(), newValue);
}
QUndoCommand* setTransactionNum(TmpTransaction& t, const QString& newValue)
{
    return new CmdRef<TmpTransaction, Glib::ustring>(QObject::tr("Edit Transaction Number"),
            t, &TmpTransaction::set_num,
            t.get_num(), q2g(newValue));
}
QUndoCommand* setTransactionDescription(TmpTransaction& t, const QString& newValue)
{
    return new CmdRef<TmpTransaction, Glib::ustring>(QObject::tr("Edit Transaction Description"),
            t, &TmpTransaction::set_description,
            t.get_description(), q2g(newValue));
}
QUndoCommand* setTransactionDate(TmpTransaction& t, const QDate& newValue)
{
    return new CmdRef<TmpTransaction, Glib::Date>(QObject::tr("Edit Transaction Date"),
            t, &TmpTransaction::set_date_posted,
            t.get_date_posted(), q2g(newValue));
}

// ////////////////////////////////////////////////////////////

class TmpSplitValueAndAmountCmd : public QUndoCommand
{
public:
    typedef QUndoCommand base_class;
    typedef TmpSplit target_type;
    typedef Numeric value_type;

    /** Constructor without a getter-function but instead the previous
     * value given directly.
     */
    TmpSplitValueAndAmountCmd(const QString& text,
                              target_type& targetRef,
                              const value_type& previousValue,
                              const value_type& newValue,
                              QUndoCommand *parent = 0)
        : base_class(text, parent)
        , m_target(targetRef)
        , m_previousValue(previousValue)
        , m_newValue(newValue)
    {
        Q_ASSERT(m_target.get_parent());
        Q_ASSERT(m_target.get_other_split());
        Q_ASSERT(m_target.get_account());
    }

    virtual void redo()
    {
        set(m_newValue);
    }
    virtual void undo()
    {
        set(m_previousValue);
    }
private:
    void set(const value_type& value)
    {
        Q_ASSERT(m_target.get_parent());
        const TmpTransaction& trans = *m_target.get_parent();
        if (trans.get_num_splits() != 2)
            return;
        TmpSplit* p_other = m_target.get_other_split();
        Q_ASSERT(p_other);
        TmpSplit& other = *p_other;
        Glib::RefPtr<Commodity> originCommodity = Glib::wrap(m_target.get_account())->get_commodity();
        Glib::RefPtr<Commodity> transCommodity = trans.get_commodity();
        Q_ASSERT(other.get_account());
        Glib::RefPtr<Commodity> otherCommodity = Glib::wrap(other.get_account())->get_commodity();
        if (originCommodity != transCommodity
                || transCommodity != otherCommodity)
            return;

        m_target.set_value(value);
        m_target.set_amount(value);
        Numeric valueNeg = value.neg();
        other.set_amount(valueNeg);
        other.set_value(valueNeg);
    }

protected:
    target_type& m_target;
    value_type m_previousValue;
    value_type m_newValue;

};


QUndoCommand* setSplitValueAndAmount(TmpSplit& t, const Numeric& newValue)
{
    return new TmpSplitValueAndAmountCmd(QObject::tr("Edit Transaction Value"),
                                         t, t.get_value(), newValue);
}

// ////////////////////////////////////////////////////////////



class TransactionCreateCmd : public QUndoCommand
{
public:
    typedef QUndoCommand base_class;

    /** Constructor
     */
    TransactionCreateCmd(const QString& text,
                         const TmpTransaction& target,
                         QUndoCommand *parent = 0)
        : base_class(text, parent)
        , m_template(target)
        , m_created(NULL)
    {}

    virtual void redo()
    {
        m_created = m_template.create_as_real();
    }

    virtual void undo()
    {
        xaccTransDestroy(m_created->gobj());
        m_created.reset();
    }

protected:
    TmpTransaction m_template;
    Glib::RefPtr<Transaction> m_created;
};

QUndoCommand* commitNewTransaction(const TmpTransaction& t)
{
    return new TransactionCreateCmd(QObject::tr("Create Transaction"), t);
}



} // END namespace cmd


} // END namespace gnc
