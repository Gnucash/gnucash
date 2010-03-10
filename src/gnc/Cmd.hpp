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
#include <gnc/Split.hpp>

namespace gnc
{

template<class TargetT, class ValueT>
class Cmd : public QUndoCommand
{
public:
    typedef TargetT target_type;
    typedef ValueT value_type;
    typedef void (TargetT::*setter_func)(const value_type&);
    typedef value_type (TargetT::*getter_func)() const;

    Cmd(const QString& text,
        TargetT& target, setter_func setter,
        const value_type& previousValue,
        const value_type& newValue,
        QUndoCommand *parent = 0)
            : QUndoCommand(text, parent)
            , m_target(target)
            , m_setter(setter)
            , m_previousValue(previousValue)
            , m_newValue(newValue)
    {
    }

    Cmd(const QString& text,
        TargetT& target, setter_func setter,
        getter_func getter,
        const value_type& newValue,
        QUndoCommand *parent = 0)
            : QUndoCommand(text, parent)
            , m_target(target)
            , m_setter(setter)
            , m_previousValue((m_target.*getter)())
            , m_newValue(newValue)
    {
    }

    virtual void redo()
    {
        // Uh oh.  The calling syntax for pointer-to-member variables
        // (here: m_setter) looks rather weird:
        (m_target.*m_setter)(m_newValue);
    }
    virtual void undo()
    {
        (m_target.*m_setter)(m_previousValue);
    }


protected:
    TargetT& m_target;
    setter_func m_setter;
    value_type m_previousValue;
    value_type m_newValue;
};

namespace cmd
{

QUndoCommand* setSplitMemo(Split& split, const QString& newValue)
{
    return new Cmd<Split, QString>(QWidget::tr("Edit Split Memo"),
                                   split, &Split::setMemo,
                                   &Split::getMemo, newValue);
}

} // END namespace cmd

} // END namespace gnc

#endif
