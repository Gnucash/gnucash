/*
 * AccountSelectionDelegate.cpp
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

#include "AccountSelectionDelegate.hpp"

#include "gnc/AccountItemModel.hpp"
#include "gnc/Book.hpp"
#include "gnc/Split.hpp"
#include "gnc/SplitListModel.hpp"

#include <QDebug>
#include <QComboBox>

namespace gnc
{

AccountSelectionDelegate::AccountSelectionDelegate(QObject* parent)
        : base_class(parent)
{}

QString AccountSelectionDelegate::displayText(const QVariant& value, const QLocale& locale) const
{
    if (value.canConvert<Account>())
    {
        Account acc = value.value<Account>();
        return acc.getFullName();
    }
    else
    {
        return base_class::displayText(value, locale);
    }
}

QWidget *AccountSelectionDelegate::createEditor(QWidget *parent,
        const QStyleOptionViewItem &option,
        const QModelIndex &index) const
{
    QComboBox* comboBox = new QComboBox(parent);

    Q_ASSERT(index.isValid());
    const SplitListModel* smodel = dynamic_cast<const SplitListModel*>(index.model());
    if (smodel)
    {
        Account modelAccount = smodel->getAccount();
        Q_ASSERT(modelAccount);
        Book book = modelAccount.getBook();
        Q_ASSERT(book);
        Account rootaccount = book.get_root_account();
        Q_ASSERT(rootaccount);
        AccountListModel* model = new AccountListNamesModel(rootaccount, comboBox);
        comboBox->setModel(model);
    }

    return comboBox;
}

void AccountSelectionDelegate::setEditorData(QWidget *editor, const QModelIndex &index) const
{
    QComboBox* comboBox = dynamic_cast<QComboBox*>(editor);
    Q_ASSERT(comboBox);

    Q_ASSERT(index.isValid());

    QVariant value = index.model()->data(index, Qt::EditRole);
    if (value.canConvert<Account>())
    {
        Account acc = value.value<Account>();
        if (acc)
        {
            const AccountListModel* amodel = dynamic_cast<const AccountListModel*>(comboBox->model());
            Q_ASSERT(amodel);
            comboBox->setCurrentIndex(amodel->indexOf(acc.get()));
        }
    }
    else
    {
        qDebug() << "AccountSelectionDelegate::setEditorData: oops, item returns no account in the column";
    }
}

void AccountSelectionDelegate::setModelData(QWidget *editor, QAbstractItemModel *model,
        const QModelIndex &index) const
{
    QComboBox* comboBox = dynamic_cast<QComboBox*>(editor);
    Q_ASSERT(comboBox);

    int currentIndex = comboBox->currentIndex();
    if (currentIndex == -1)
        return;
    const AccountListModel* amodel = dynamic_cast<const AccountListModel*>(comboBox->model());
    Q_ASSERT(amodel);
    Account acc(amodel->at(currentIndex));
    if (acc)
    {
        model->setData(index, QVariant::fromValue(acc), Qt::EditRole);
    }
}


} // END namespace gnc
