/*
 * AccountSelectionDelegate.hpp
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

#ifndef GNC_ACCOUNTSELECTIONDELEGATE_HPP
#define GNC_ACCOUNTSELECTIONDELEGATE_HPP

#include "gnc/Account.hpp"

#include <QtGui/QStyledItemDelegate>
#include <QDebug>

namespace gnc
{

class AccountSelectionDelegate : public QStyledItemDelegate
{
    Q_OBJECT
public:
    typedef QStyledItemDelegate base_class;

    AccountSelectionDelegate(QObject* parent = 0);

    virtual QWidget *createEditor(QWidget *parent, const QStyleOptionViewItem &option,
                                  const QModelIndex &index) const;
    virtual QString displayText(const QVariant& value, const QLocale& locale) const;
    virtual void setEditorData(QWidget *editor, const QModelIndex &index) const;
    virtual void setModelData(QWidget *editor, QAbstractItemModel *model,
                              const QModelIndex &index) const;

};


} // END namespace gnc

#endif
