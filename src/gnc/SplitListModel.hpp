/*
 * SplitListModel.hpp
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

#ifndef GNC_SPLITLISTMODEL_HPP
#define GNC_SPLITLISTMODEL_HPP

#include "gnc/Split.hpp"

#include <QAbstractItemModel>
class QUndoStack;

namespace gnc
{

/** This is the data model for a list of splits.
 */
class SplitListModel : public QAbstractItemModel
{
    Q_OBJECT
public:
    SplitListModel(const SplitQList splits, QUndoStack* undoStack, QObject *parent = 0);

    QModelIndex parent(const QModelIndex &index) const { return QModelIndex(); }
    int rowCount(const QModelIndex& parent = QModelIndex()) const { return m_list.size(); }
    int columnCount(const QModelIndex& parent = QModelIndex()) const;
    QModelIndex index(int row, int column,
                      const QModelIndex &parent = QModelIndex()) const;
    Qt::ItemFlags flags(const QModelIndex &index) const;

    QVariant data(const QModelIndex& index, int role) const;
    QVariant headerData(int section, Qt::Orientation orientation, int role) const;
    bool setData(const QModelIndex &index, const QVariant &value, int role);


protected:
    SplitQList m_list;
    QUndoStack* m_undoStack;
};

} // END namespace gnc

#endif
