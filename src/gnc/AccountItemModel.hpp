/*
 * AccountItemModel.hpp
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

#ifndef GNC_ACCOUNTITEMMODEL_HPP
#define GNC_ACCOUNTITEMMODEL_HPP

#include "gnc/Account.hpp"

#include <QAbstractItemModel>
#include <QAbstractTableModel>

namespace gnc
{

class AccountItemModel : public QAbstractTableModel
{
    Q_OBJECT
public:
    AccountItemModel(Account rootaccount, QObject *parent = 0)
            : QAbstractTableModel(parent)
            , m_root(rootaccount)
            , m_acclist(Account::fromGList(m_root.get_descendants()))
    {
    }

    int rowCount(const QModelIndex& parent = QModelIndex()) const { return m_acclist.size(); }
    int columnCount(const QModelIndex& parent = QModelIndex()) const { return 3; }
    QVariant data(const QModelIndex& index, int role) const
    {
        if (!index.isValid())
            return QVariant();
        if (index.row() > rowCount(index))
            return QVariant();
        if (role == Qt::DisplayRole)
        {
            switch (index.column())
            {
            case 0:
                return Account(m_acclist.at(index.row())).getName();
            case 1:
                return Account(m_acclist.at(index.row())).getCode();
            case 2:
                return Account(m_acclist.at(index.row())).getDescription();
            default:
                return QVariant();
            }
        }
        else
            return QVariant();
    }
    QVariant headerData(int section, Qt::Orientation orientation, int role) const
    {
        if (role != Qt::DisplayRole)
            return QVariant();
        if (orientation == Qt::Horizontal)
        {
            switch (section)
            {
            case 0:
                return QString("Name");
            case 1:
                return QString("Code");
            case 2:
                return QString("Description");
            default:
                return QVariant();
            }
        }
        else
            return QString("%1").arg(1 + section);
    }
private:
    Account m_root;
    Account::AccountQList m_acclist;
};

} // END namespace gnc

#endif
