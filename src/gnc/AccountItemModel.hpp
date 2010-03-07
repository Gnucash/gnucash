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

namespace gnc
{

/** This is the data model for an account tree.
 *
 * It was written following the "Simple Tree Model Example" in the qt4
 * documentation. In particular, the trick is that each returned
 * QModelIndex of our model implementation which is valid will have a
 * ::Account* pointer in its QModelIndex::internalPointer() member,
 * which can then be used in the data() method and the other
 * methods. This trick works quite nice because our C structures
 * ::Account* already have all the methods to iterate the tree back
 * and forth, and our C++ wrapper gnc::Account doesn't add any
 * functionality except for some nicer method names.
 */
class AccountTreeModel : public QAbstractItemModel
{
    Q_OBJECT
public:
    AccountTreeModel(Account rootaccount, QObject *parent = 0);

    int rowCount(const QModelIndex& parent = QModelIndex()) const;
    int columnCount(const QModelIndex& parent = QModelIndex()) const;
    QModelIndex parent(const QModelIndex &index) const;
    QModelIndex index(int row, int column,
                      const QModelIndex &parent = QModelIndex()) const;
    Qt::ItemFlags flags(const QModelIndex &index) const;

    QVariant data(const QModelIndex& index, int role) const;
    QVariant headerData(int section, Qt::Orientation orientation, int role) const;

protected:
    Account m_root;
};



/** Specialization of the account tree model for when all accounts
 * should be viewed as a flat list instead of a tree. Only the index()
 * and parent() functions had to be overridden - quite easy. */
class AccountListModel : public AccountTreeModel
{
    Q_OBJECT
public:
    AccountListModel(Account rootaccount, QObject *parent = 0)
            : AccountTreeModel(rootaccount, parent)
            , m_list(Account::fromGList(rootaccount.get_descendants()))
    {
    }

    int rowCount(const QModelIndex& parent = QModelIndex()) const { return m_list.size(); }

    QModelIndex index(int row, int column,
                      const QModelIndex &parent = QModelIndex()) const;

    QModelIndex parent(const QModelIndex &index) const { return QModelIndex(); }

private:
    Account::AccountQList m_list;
};



} // END namespace gnc

#endif
