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

#ifndef GNC_CMD_HPP
#define GNC_CMD_HPP

#include <QUndoCommand>
#include <gnc/WeakPointer.hpp>
#include <gnc/Numeric.hpp>

namespace gnc
{

class Split;
class Account;
class Transaction;

/** This is a templated implementation of a QUndoCommand class. This
 * implements the Command pattern: Any instance of this class
 * represents the change of one member variable (of type ValueT) in an
 * instance of a TargetT object.
 *
 * The fun part is that we can pass pointers to the getter and setter
 * functions into this class' constructor, and the redo()/undo()
 * implementations follow naturally. Hence, this class is already
 * sufficient for any Command that is executed by a particular Setter
 * function.
 *
 * As we need to keep a reference to the original object, it is
 * relevant who owns the life cycle of that original object. For
 * simplicity, we constrain this current implementation only to
 * classes which are implementations of a WeakPointer<...> which by
 * definition keeps a C pointer to the original object, and we hope
 * that one lives longer than we do.
 */
template<class TargetT, class ValueT, typename SetterFunc = void (TargetT::*)(const ValueT&)>
class Cmd : public QUndoCommand
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

    /// Type of the getter function to retrieve the current value from the target object
    typedef value_type (target_type::*getter_func)() const;

    /** Constructor.
     * @param text The QUndoCommand's text which will be displayed in the Undo action.
     * @param targetPtr Reference to the target object on which this command is applied.
     * @param setter Pointer to function which sets the value in the target object
     * @param getter Pointer to function which returns the current value from the target object
     * @param newValue The new value to be set
     * @param parent The parent QUndoCommand instance, or NULL.
     */
    Cmd(const QString& text,
        WeakPointer<typename target_type::element_type>& targetPtr,
        setter_func setter,
        getter_func getter,
        const value_type& newValue,
        QUndoCommand *parent = 0)
            : base_class(text, parent)
            , m_target(targetPtr.get())
            , m_setter(setter)
            , m_previousValue((m_target.*getter)())
            , m_newValue(newValue)
    {
        Q_ASSERT(m_target);
        Q_ASSERT(m_setter);
    }

    /** Overloaded constructor without a getter-function but instead
     * the previous value given directly.
     *
     * @param text The QUndoCommand's text which will be displayed in the Undo action.
     * @param targetPtr Reference to the target object on which this command is applied.
     * @param setter Pointer to function which sets the value in the target object
     * @param previousValue The previous value, in case this command needs to be undone
     * @param newValue The new value to be set
     * @param parent The parent QUndoCommand instance, or NULL.
     */
    Cmd(const QString& text,
        WeakPointer<typename target_type::element_type>& targetPtr,
        setter_func setter,
        const value_type& previousValue,
        const value_type& newValue,
        QUndoCommand *parent = 0)
            : base_class(text, parent)
            , m_target(targetPtr.get())
            , m_setter(setter)
            , m_previousValue(previousValue)
            , m_newValue(newValue)
    {
        Q_ASSERT(m_target);
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
        // Uh oh.  The calling syntax for pointer-to-member
        // variables (here: m_setter) looks rather weird:
        (m_target.*m_setter)(value);
    }

protected:
    target_type m_target;
    setter_func m_setter;
    value_type m_previousValue;
    value_type m_newValue;

};

namespace cmd
{

// This is the collection of command objects which are already
// provided for the different data types and their simple
// members. Just create one of those, add it to a QUndoStack, and
// magically the values will change with undo/redo back and
// forth. Spooky, IMHO.
QUndoCommand* setSplitMemo(Split& split, const QString& newValue);
QUndoCommand* setSplitAction(Split& t, const QString& newValue);
QUndoCommand* setSplitReconcile(Split& t, char newValue);
QUndoCommand* setSplitAmount(Split& t, const Numeric& newValue);
QUndoCommand* setSplitValue(Split& t, const Numeric& newValue);
QUndoCommand* setTransactionNum(Transaction& t, const QString& newValue);
QUndoCommand* setTransactionDescription(Transaction& t, const QString& newValue);
QUndoCommand* setTransactionNotes(Transaction& t, const QString& newValue);
QUndoCommand* setTransactionDate(Transaction& t, const QDate& newValue);
QUndoCommand* setSplitValueAndAmount(Split& t, const Numeric& newValue);
QUndoCommand* destroyTransaction(Transaction& t);

} // END namespace cmd

} // END namespace gnc

#endif
