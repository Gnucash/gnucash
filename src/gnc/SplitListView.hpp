/*
 * SplitListView.hpp
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

#ifndef GNC_SPLITLISTVIEW_HPP
#define GNC_SPLITLISTVIEW_HPP

#include "gnc/Account.hpp"
#include "gnc/SplitListModel.hpp"
#include "gnc/QofEventWrapper.hpp"

#include <QtGui/QTableView>
#include <QtGui/QAbstractItemDelegate>
#include <QUndoStack>
#include <QDebug>

namespace gnc
{

class SplitListView : public QTableView
{
    Q_OBJECT
public:
    typedef QTableView base_class;
    SplitListView(Account account, QUndoStack* undoStack, QWidget* parent = 0);

signals:
    void editorClosed(const QModelIndex& index, QAbstractItemDelegate::EndEditHint hint);

public slots:
    void closeEditor(QWidget* editor, QAbstractItemDelegate::EndEditHint hint);
    void accountEvent( ::Account* v, QofEventId event_type);
    void bookEvent( ::QofBook* v, QofEventId event_type);

private:
    Account m_account;
    QofEventWrapper<SplitListView, ::Account*> m_eventWrapperAccount;
    QofEventWrapper<SplitListView, ::QofBook*> m_eventWrapperBook;
};

}

#endif
