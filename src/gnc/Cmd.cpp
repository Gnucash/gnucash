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
#include "gnc/Split.hpp"
#include "gnc/Account.hpp"
#include "gnc/Transaction.hpp"
#include <QObject>

namespace gnc
{

// Explicit instantiations to check for compiler errors
template class Cmd<Account, QString>;

namespace cmd
{

// ////////////////////////////////////////////////////////////

QUndoCommand* setSplitMemo(Split& t, const QString& newValue)
{
    return new Cmd<Split, QString>(QObject::tr("Edit Split Memo"),
                                   t, &Split::setMemo,
                                   t.getMemo(), newValue);
}

QUndoCommand* setSplitAction(Split& t, const QString& newValue)
{
    return new Cmd<Split, QString>(QObject::tr("Edit Split Action"),
                                   t, &Split::setAction,
                                   t.getAction(), newValue);
}

QUndoCommand* setSplitReconcile(Split& t, char newValue)
{
    // Special third argument: The setter function takes a value
    // directly, instead of a const-reference, so the template type
    // must be given explicitly.
    return new Cmd<Split, char, void (Split::*)(char)>(QObject::tr("Edit Split Reconcile"),
            t, &Split::setReconcile,
            t.getReconcile(), newValue);
}

QUndoCommand* setSplitAmount(Split& t, const Numeric& newValue)
{
    return new Cmd<Split, Numeric>(QObject::tr("Edit Split Amount"),
                                   t, &Split::setAmount,
                                   t.getAmount(), newValue);
}

QUndoCommand* setSplitValue(Split& t, const Numeric& newValue)
{
    return new Cmd<Split, Numeric>(QObject::tr("Edit Split Value"),
                                   t, &Split::setValue,
                                   t.getValue(), newValue);
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
                           WeakPointer<target_type::element_type>& targetPtr,
                           const value_type& previousValue,
                           const value_type& newValue,
                           QUndoCommand *parent = 0)
            : base_class(text, parent)
            , m_target(targetPtr.get())
            , m_previousValue(previousValue)
            , m_newValue(newValue)
    {
        Q_ASSERT(m_target);
    }

    virtual void redo() { set(m_newValue); }
    virtual void undo() { set(m_previousValue); }
private:
    void set(const value_type& value)
    {
        Transaction trans = m_target.getParent();
        if (trans.countSplits() != 2)
            return;
        Split other = m_target.getOtherSplit();
        Q_ASSERT(other);
        Commodity originCommodity = m_target.getAccount().getCommodity();
        Commodity transCommodity = trans.getCurrency();
        Commodity otherCommodity = other.getAccount().getCommodity();
        if (originCommodity != transCommodity
                || transCommodity != otherCommodity)
            return;

        trans.beginEdit();
        m_target.setValue(value);
        m_target.setAmount(value);
        Numeric valueNeg = value.neg();
        other.setAmount(valueNeg);
        other.setValue(valueNeg);
        trans.commitEdit();
    }

protected:
    target_type m_target;
    value_type m_previousValue;
    value_type m_newValue;

};

QUndoCommand* setSplitValueAndAmount(Split& t, const Numeric& newValue)
{
    return new SplitValueAndAmountCmd(QObject::tr("Edit Transaction Value"),
                                      t, t.getValue(), newValue);
}

// ////////////////////////////////////////////////////////////

QUndoCommand* setTransactionNum(Transaction& t, const QString& newValue)
{
    return new Cmd<Transaction, QString>(QObject::tr("Edit Transaction Number"),
                                         t, &Transaction::setNum,
                                         t.getNum(), newValue);
}

QUndoCommand* setTransactionDescription(Transaction& t, const QString& newValue)
{
    return new Cmd<Transaction, QString>(QObject::tr("Edit Transaction Description"),
                                         t, &Transaction::setDescription,
                                         t.getDescription(), newValue);
}

QUndoCommand* setTransactionNotes(Transaction& t, const QString& newValue)
{
    return new Cmd<Transaction, QString>(QObject::tr("Edit Transaction Notes"),
                                         t, &Transaction::setNotes,
                                         t.getNotes(), newValue);
}

QUndoCommand* setTransactionDate(Transaction& t, const QDate& newValue)
{
    return new Cmd<Transaction, QDate>(QObject::tr("Edit Transaction Date"),
                                       t, &Transaction::setDatePosted,
                                       t.getDatePosted(), newValue);
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
                          WeakPointer<target_type::element_type>& targetPtr,
                          QUndoCommand *parent = 0)
            : base_class(text, parent)
            , m_target(targetPtr.get())
            , m_previousValue(m_target)
            , m_book(m_target.getBook())
    {
        Q_ASSERT(m_target);
    }

    virtual void redo()
    {
        xaccTransDestroy(m_target.get());
        m_target.reset();
    }

    virtual void undo()
    {
        m_target.reset(Transaction::newInstance(m_book));
        m_target.beginEdit();
        m_previousValue.copyTo(m_target);
        m_target.commitEdit();
        // Could also use m_previousValue.createAsReal()
    }

protected:
    target_type m_target;
    TmpTransaction m_previousValue;
    Book m_book;
};

QUndoCommand* destroyTransaction(Transaction& t)
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
template<class TargetT, class ValueT, typename SetterFunc = void (TargetT::*)(const ValueT&)>
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

    virtual void redo() { set(m_newValue); }
    virtual void undo() { set(m_previousValue); }

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


QUndoCommand* setSplitReconcile(TmpSplit& t, char newValue)
{
    // Special third argument: The setter function takes a value
    // directly, instead of a const-reference, so the template type
    // must be given explicitly.
    return new CmdRef<TmpSplit, char, void (TmpSplit::*)(char)>(QObject::tr("Edit Split Reconcile"),
            t, &TmpSplit::setReconcile,
            t.getReconcile(), newValue);
}
QUndoCommand* setTransactionNum(TmpTransaction& t, const QString& newValue)
{
    return new CmdRef<TmpTransaction, QString>(QObject::tr("Edit Transaction Number"),
            t, &TmpTransaction::setNum,
            t.getNum(), newValue);
}
QUndoCommand* setTransactionDescription(TmpTransaction& t, const QString& newValue)
{
    return new CmdRef<TmpTransaction, QString>(QObject::tr("Edit Transaction Description"),
            t, &TmpTransaction::setDescription,
            t.getDescription(), newValue);
}
QUndoCommand* setTransactionDate(TmpTransaction& t, const QDate& newValue)
{
    return new CmdRef<TmpTransaction, QDate>(QObject::tr("Edit Transaction Date"),
            t, &TmpTransaction::setDatePosted,
            t.getDatePosted(), newValue);
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
    }

    virtual void redo() { set(m_newValue); }
    virtual void undo() { set(m_previousValue); }
private:
    void set(const value_type& value)
    {
        const TmpTransaction* p_trans = m_target.getParent();
//         if (trans.countSplits() != 2)
//             return;
//         Split other = m_target.getOtherSplit();
//         Q_ASSERT(other);
//         Commodity originCommodity = m_target.getAccount().getCommodity();
//         Commodity transCommodity = trans.getCurrency();
//         Commodity otherCommodity = other.getAccount().getCommodity();
//         if (originCommodity != transCommodity
//                 || transCommodity != otherCommodity)
//             return;

//         trans.beginEdit();
        m_target.setValue(value);
        m_target.setAmount(value);
//         Numeric valueNeg = value.neg();
//         other.setAmount(valueNeg);
//         other.setValue(valueNeg);
//         trans.commitEdit();
    }

protected:
    target_type& m_target;
    value_type m_previousValue;
    value_type m_newValue;

};


QUndoCommand* setSplitValueAndAmount(TmpSplit& t, const Numeric& newValue)
{
    return new TmpSplitValueAndAmountCmd(QObject::tr("Edit Transaction Value"),
                                         t, t.getValue(), newValue);
}

// ////////////////////////////////////////////////////////////



class TransactionCreateCmd : public QUndoCommand
{
public:
    typedef QUndoCommand base_class;
    typedef TmpTransaction target_type;

    /** Constructor
     */
    TransactionCreateCmd(const QString& text,
                         const target_type target,
                         QUndoCommand *parent = 0)
            : base_class(text, parent)
            , m_template(target)
            , m_created(NULL)
    {}

    virtual void redo()
    {
        m_created = m_template.createAsReal();
    }

    virtual void undo()
    {
        xaccTransDestroy(m_created.get());
        m_created.reset();
    }

protected:
    target_type m_template;
    Transaction m_created;
};

QUndoCommand* commitNewTransaction(const TmpTransaction& t)
{
    return new TransactionCreateCmd(QObject::tr("Create Transaction"), t);
}



} // END namespace cmd


} // END namespace gnc
