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
#include "gnc/Session.hpp"
#include <QDebug>
#include <QUndoStack>
#include <QBrush>
#include <QMessageBox>

#include "app-utils/gnc-ui-util.h" // for gnc_get_reconcile_str

namespace gnc
{



SplitListModel::SplitListModel(const Account& acc, QUndoStack* undoStack, QObject *parent)
        : QAbstractItemModel(parent)
        , m_account(acc)
        , m_list(Split::fromGList(acc.getSplitList()))
        , m_undoStack(undoStack)
        , m_eventWrapper(*this, &SplitListModel::transactionModified,
                         GNC_ID_TRANS, QOF_EVENT_MODIFY)
{
    // Cache the mapping of transactions to split in the m_hash
    for (int k = 0; k < m_list.size(); ++k)
    {
        m_hash.insert(Split(m_list[k]).getParent().get(), k);
    }
}

SplitListModel::~SplitListModel()
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
    case 5:
    case 6:
        // Allow write access as well
        return result | Qt::ItemIsEditable;
    default:
        // Ensure read-only access only
        return result;
    }
}

QVariant SplitListModel::data(const QModelIndex& index, int role) const
{
    //qDebug() << "data(), " << index;
    if (!index.isValid())
        return QVariant();

    Split split(static_cast< ::Split*>(index.internalPointer()));
    Transaction trans(split.getParent());
    Numeric amount = split.getValue(); // Alternatively: xaccSplitConvertAmount(split.get(), split.getAccount().get());
    PrintAmountInfo printInfo(split.get(), false);

    switch (index.column())
    {
    case 0:
        switch (role)
        {
        case Qt::DisplayRole:
        case Qt::EditRole:
            return trans.getDatePosted();
        default:
            return QVariant();
        }
    case 1:
        switch (role)
        {
        case Qt::DisplayRole:
        case Qt::EditRole:
            return trans.getNum();
        default:
            return QVariant();
        }
    case 2:
        switch (role)
        {
        case Qt::DisplayRole:
        case Qt::EditRole:
            return trans.getDescription();
        default:
            return QVariant();
        }
    case 3:
        switch (role)
        {
        case Qt::DisplayRole:
            return split.getCorrAccountFullName();
        default:
            return QVariant();
        }
    case 4:
        switch (role)
        {
        case Qt::DisplayRole:
        case Qt::EditRole:
            return QString::fromUtf8(gnc_get_reconcile_str(split.getReconcile()));
        default:
            return QVariant();
        }
    case 5:
        switch (role)
        {
        case Qt::DisplayRole:
        case Qt::EditRole:
            if (amount.positive_p())
                return amount.printAmount(printInfo);
            else
                return QString();
        default:
            return QVariant();
        }
    case 6:
        switch (role)
        {
        case Qt::DisplayRole:
        case Qt::EditRole:
            if (amount.positive_p())
                return QString();
            else
                return amount.neg().printAmount(printInfo);
        default:
            return QVariant();
        }
    case 7:
        switch (role)
        {
        case Qt::DisplayRole:
            return split.getBalance().printAmount(printInfo);
        case Qt::ForegroundRole:
            return split.getBalance().negative_p()
                   ? QBrush(Qt::red)
                   : QBrush();
        default:
            return QVariant();
        }
    default:
        return QVariant();
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
            QDate date = value.toDate();
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
        {
            QString str(value.toString());
            if (str.size() > 0)
            {
                char recn = str[0].toLatin1();
                switch (recn)
                {
                case NREC:
                case CREC:
                case YREC:
                case FREC:
                case VREC:
                    cmd = cmd::setSplitReconcile(split, recn);
                    break;
                default:
                    qDebug() << "Unknown reconcile string:" << str;
                }
            }
            break;
        }
        case 5:
        case 6:
        {
            QString str(value.toString().simplified());
            Numeric n;
            QString errmsg = n.parse(str);
            if (errmsg.isEmpty())
            {
                qDebug() << "Does setting numeric work? numeric=" << n.to_string();
                if (index.column() == 6)
                    n = n.neg();
                // Check whether we have the simple case here
                if (split.getParent().countSplits() != 2)
                {
                    QMessageBox::warning(NULL, tr("Unimplemented"),
                                         tr("Sorry, but editing a transaction with more than two splits (here: %1) is not yet implemented.").arg(split.getParent().countSplits()));
                }
                else
                {
                    Transaction trans = split.getParent();
                    Split other = split.getOtherSplit();
                    Q_ASSERT(other);
                    Commodity originCommodity = split.getAccount().getCommodity();
                    Commodity transCommodity = trans.getCurrency();
                    Commodity otherCommodity = other.getAccount().getCommodity();
                    if (originCommodity != transCommodity
                            || transCommodity != otherCommodity)
                    {
                        QMessageBox::warning(NULL, tr("Unimplemented"),
                                             tr("Sorry, but editing a transaction with different accounts is not yet implemented."));
                    }
                    else
                    {
                        // This is the really simple case which we can
                        // handle now.
                        cmd = cmd::setSplitValueAndAmount(split, n);
                    }
                }
            }
            else
            {
                qDebug() << "Cannot convert this string to gnc_numeric:" << str;
            }
            break;
        }
        default:
            break;
        }
    }
    if (cmd)
    {
        m_undoStack->push(cmd);
        // No dataChanged() signal here because it is emitted from the
        // transactionChanged slot.
        return true;
    }
    return false;
}

void SplitListModel::transactionModified( ::Transaction* trans)
{
    if (m_hash.contains(trans))
    {
        int row = m_hash.value(trans);
        emit dataChanged(index(row, 0), index(row, columnCount() - 1));
    }

}

} // END namespace gnc
