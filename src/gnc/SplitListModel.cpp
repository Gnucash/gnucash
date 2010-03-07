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
#include <QDebug>

namespace gnc
{

SplitListModel::SplitListModel(const SplitQList splits, QObject *parent)
        : QAbstractItemModel(parent)
        , m_list(splits)
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
    return 4; // Fixed number for now
}

QVariant SplitListModel::data(const QModelIndex& index, int role) const
{
    //qDebug() << "data(), " << index;
    if (!index.isValid())
        return QVariant();
    if (role == Qt::DisplayRole)
    {
        Split split(static_cast< ::Split*>(index.internalPointer()));
        Transaction trans(split.getParent());
        switch (index.column())
        {
        case 0:
            return trans.getNum();
        case 1:
            return trans.getDescription();
        case 2:
            return split.getCorrAccountFullName();
        case 3:
            return QChar(split.getReconcile());
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

    // Ensure read-only access only
    return Qt::ItemIsEnabled | Qt::ItemIsSelectable;
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
            return QString("Num");
        case 1:
            return QString("Description");
        case 2:
            return QString("Account");
        case 3:
            return QString("Reconciled?");
        default:
            return QVariant();
        }
    }
    else
        return QString("%1").arg(1 + section);
}


} // END namespace gnc
