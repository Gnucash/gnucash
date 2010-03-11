/*
 * SplitListModel.cpp
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

#include "SplitListModel.hpp"
#include "gnc/Transaction.hpp"
#include "gnc/Cmd.hpp"
#include <QDebug>
#include <QUndoStack>

namespace gnc
{

SplitListModel::SplitListModel(const SplitQList splits, QUndoStack* undoStack, QObject *parent)
        : QAbstractItemModel(parent)
        , m_list(splits)
        , m_undoStack(undoStack)
{
}

QModelIndex SplitListModel::index(int row, int column,
                                  const QModelIndex &parent) const
{
    //qDebug() << "index(), " << row << column << parent;
    if (!hasIndex(row, column, parent) || row >= m_list.size())
        return QModelIndex();

    Split childItem = m_list.at(row);
    if (childItem.get())
    {
        //qDebug() << "returning" << childItem.getName();
        return createIndex(row, column, childItem.get());
    }
    else
        return QModelIndex();
}

int SplitListModel::columnCount(const QModelIndex& parent) const
{
    //qDebug() << "columnCount()" << parent;
//     if (!parent.isValid())
//         return 0;
//     else
    return 8; // Fixed number for now
}

QVariant SplitListModel::data(const QModelIndex& index, int role) const
{
    //qDebug() << "data(), " << index;
    if (!index.isValid())
        return QVariant();

    Split split(static_cast< ::Split*>(index.internalPointer()));
    Transaction trans(split.getParent());
    if (role == Qt::DisplayRole)
    {
        Numeric amount = split.getAmount(); // Alternatively: xaccSplitConvertAmount(split.get(), split.getAccount().get());
        PrintAmountInfo printInfo(split.get(), false);
        switch (index.column())
        {
        case 0:
            return trans.getDatePosted().date().toString(Qt::ISODate);
        case 1:
            return trans.getNum();
        case 2:
            return trans.getDescription();
        case 3:
            return split.getCorrAccountFullName();
        case 4:
            return QChar(split.getReconcile());
        case 5:
            if (amount.positive_p())
                return amount.printAmount(printInfo);
            else
                return QString();
        case 6:
            if (amount.positive_p())
                return QString();
            else
                return amount.neg().printAmount(printInfo);
        case 7:
            return split.getBalance().printAmount(printInfo);
        default:
            return QVariant();
        }
    }
    else
        return QVariant();
}

Qt::ItemFlags SplitListModel::flags(const QModelIndex &index) const
{
    //qDebug() << "flags()" << index;
    if (!index.isValid())
        return 0;

    Qt::ItemFlags result = Qt::ItemIsEnabled | Qt::ItemIsSelectable;
    switch (index.column())
    {
    case 0:
    case 1:
    case 2:
    case 4:
        //case 5:
        //case 6:
        // Allow write access as well
        return result | Qt::ItemIsEditable;
    default:
        // Ensure read-only access only
        return result;
    }
}

QVariant SplitListModel::headerData(int section, Qt::Orientation orientation, int role) const
{
    //qDebug() << "headerData()" << section;
    if (role != Qt::DisplayRole)
        return QVariant();
    if (orientation == Qt::Horizontal)
    {
        switch (section)
        {
        case 0:
            return tr("Date");
        case 1:
            return tr("Num");
        case 2:
            return tr("Description");
        case 3:
            return tr("Transfer");
        case 4:
            return tr("R?");
        case 5:
            return tr("Increase");
        case 6:
            return tr("Decrease");
        case 7:
            return tr("Balance");
        default:
            return QVariant();
        }
    }
    else
        return QString("%1").arg(1 + section);
}

bool SplitListModel::setData(const QModelIndex &index, const QVariant &value, int role)
{
    QUndoCommand* cmd = NULL;
    if (index.isValid() && role == Qt::EditRole)
    {
        Split split(static_cast< ::Split*>(index.internalPointer()));
        Transaction trans(split.getParent());

        // "Editing" is done by creating a Cmd-object and adding it to
        // the undo stack. That's in fact all that was needed to
        // create an *undoable* edit of this data cell. Seems almost
        // spooky, doesn't it?
        switch (index.column())
        {
        case 0:
        {
            QDateTime date = value.toDateTime();
            if (date.isValid())
            {
                cmd = cmd::setTransactionDate(trans, date);
            }
            break;
        }
        case 1:
            cmd = cmd::setTransactionNum(trans, value.toString());
            break;
        case 2:
            cmd = cmd::setTransactionDescription(trans, value.toString());
            break;
        case 4:
            cmd = cmd::setSplitReconcile(split, value.toChar().toLatin1());
            break;
        case 5:
        {
            bool x;
            double v = value.toDouble(&x);
            if (!x)
            {
                qDebug() << "Cannot convert string to gnc_numeric:" << value.toString();
            }
            else
            {
                Numeric n(v, 100, GNC_HOW_RND_ROUND);
                qDebug() << "Does setting numeric work? numeric=" << n.to_string();
                cmd = cmd::setSplitAmount(split, n);
            }
            // Sigh. This doesn't seem to work so far.
            break;
        }
//         case 6:
//             cmd = cmd::setSplitAmount(split, Numeric(value.toString()).neg());
//             break;

        default:
            break;
        }
    }
    if (cmd)
    {
        m_undoStack->push(cmd);
        emit dataChanged(index, index);
        return true;
    }
    return false;
}

} // END namespace gnc
