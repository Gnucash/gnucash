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
#include "engine/gnc-event.h" // for GNC_EVENT_ITEM_ADDED
#include "gncmm/Transaction.hpp"
#include "gnc/Cmd.hpp"
#include "gnc/Session.hpp"
#include "gnc/metatype.hpp"
#include <QDebug>
#include <QUndoStack>
#include <QBrush>
#include <QMessageBox>
#include <QDateTime>

#include "app-utils/gnc-ui-util.h" // for gnc_get_reconcile_str

namespace gnc
{



SplitListModel::SplitListModel(const Glib::RefPtr<Account> acc, QUndoStack* undoStack, QObject *parent)
        : QAbstractItemModel(parent)
        , m_account(acc)
        , m_list()
        , m_undoStack(undoStack)
        , m_eventWrapper(*this, &SplitListModel::transactionEvent)
        , m_eventWrapperAccount(*this, &SplitListModel::accountEvent)
        , m_enableNewTransaction(true)
{
    recreateCache();

    m_tmpTransaction.push_back(TmpSplit(NULL));
    m_tmpTransaction.push_back(TmpSplit(NULL));
    recreateTmpTrans();
}

void SplitListModel::recreateCache()
{
    SplitQList newSplits = Split::from_glist(m_account->get_split_list());
    bool doReset = (newSplits.size() != m_list.size());

    m_list = newSplits;

    // Cache the mapping of transactions to split in the m_hash
    m_hash.clear();
    for (int k = 0; k < m_list.size(); ++k)
    {
        m_hash.insert(Glib::wrap(m_list[k])->get_parent()->gobj(), k);
    }

    if (doReset)
        reset();
}

void SplitListModel::recreateTmpTrans()
{
    m_tmpTransaction.reset_content();
    while (m_tmpTransaction.get_num_splits() > 2)
        m_tmpTransaction.get_splits().pop_back();
    Q_ASSERT(m_tmpTransaction.get_num_splits() == 2);

    m_tmpTransaction.set_commodity(m_account->get_commodity());
    m_tmpTransaction.set_date_posted(q2g(QDate::currentDate()));
    TmpSplit& oursplit = m_tmpTransaction.get_splits().front();
    TmpSplit& othersplit = m_tmpTransaction.get_splits().back();
    oursplit.set_account(m_account->gobj());

    Q_ASSERT(m_tmpTransaction.get_num_splits() == 2);
    Q_ASSERT(oursplit.get_account() == m_account->gobj());
    Q_ASSERT(othersplit.get_account() == NULL);
}

SplitListModel::~SplitListModel()
{
}

QModelIndex SplitListModel::index(int row, int column,
                                  const QModelIndex &parent) const
{
    //qDebug() << "index(), " << row << column << parent;
    if (!hasIndex(row, column, parent) || row >= rowCount())
        return QModelIndex();

    if (m_enableNewTransaction && row == m_list.size())
        return createIndex(row, column, (void*)NULL);

    Glib::RefPtr<Split> childItem = Glib::wrap(m_list.at(row));
    if (childItem)
    {
        //qDebug() << "returning" << childItem.getName();
        return createIndex(row, column, childItem->gobj());
    }
    else
        return QModelIndex();
}

bool SplitListModel::removeRows(int position, int rows, const QModelIndex &index)
{
    for (int row = position; row < position + rows; ++row)
    {
        Glib::RefPtr<Split> s = Glib::wrap(m_list.at(row));
        Q_ASSERT(s);
        Glib::RefPtr<Transaction> t = s->get_parent();
        Q_ASSERT(t);
        QUndoCommand* cmd = cmd::destroyTransaction(t);
        m_undoStack->push(cmd);
    }
    // No beginInsertRows/endInsertRows because reset() is called in
    // recreateCache() anyway.
    return true;
}

int SplitListModel::rowCount(const QModelIndex& parent) const
{
    return m_list.size() + (m_enableNewTransaction ? 1 : 0);
}

int SplitListModel::columnCount(const QModelIndex& parent) const
{
    //qDebug() << "columnCount()" << parent;
//     if (!parent.isValid())
//         return 0;
//     else
    return COLUMN_LAST; // Fixed number for now
}

Qt::ItemFlags SplitListModel::flags(const QModelIndex &index) const
{
    //qDebug() << "flags()" << index;
    if (!index.isValid())
        return 0;

    Qt::ItemFlags result = Qt::ItemIsEnabled | Qt::ItemIsSelectable;
    switch (index.column())
    {
    case COLUMN_DATE:
    case COLUMN_NUM:
    case COLUMN_DESC:
    case COLUMN_RECONCILE:
    case COLUMN_INCREASE:
    case COLUMN_DECREASE:
    case COLUMN_ACCOUNT:
        // Allow write access as well
        return result | Qt::ItemIsEditable;
    case COLUMN_BALANCE:
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

    if (m_enableNewTransaction && index.row() == m_list.size())
    {
        // Special case: We are in the last row which represents the
        // newly entered txn.

        const TmpSplit& split(m_tmpTransaction.get_splits().front());
        const TmpTransaction& trans = m_tmpTransaction;
        Numeric amount = split.get_value();
        PrintAmountInfo printInfo(m_account, false);
        switch (index.column())
        {
        case COLUMN_DATE:
            switch (role)
            {
            case Qt::DisplayRole:
            case Qt::EditRole:
                return g2q(trans.get_date_posted());
            default:
                return QVariant();
            }
        case COLUMN_NUM:
            switch (role)
            {
            case Qt::DisplayRole:
            case Qt::EditRole:
                return g2q(trans.get_num());
            default:
                return QVariant();
            }
        case COLUMN_DESC:
            switch (role)
            {
            case Qt::DisplayRole:
            case Qt::EditRole:
                return g2q(trans.get_description());
            default:
                return QVariant();
            }
        case COLUMN_ACCOUNT:
            switch (role)
            {
            case Qt::DisplayRole:
            case Qt::EditRole:
                if (trans.get_num_splits() == 2)
                    return QVariant::fromValue(trans.get_splits().back().get_account());
                else
                    return QVariant(); // FIXME: Multi-split txn here
            default:
                return QVariant();
            }
        case COLUMN_RECONCILE:
            switch (role)
            {
            case Qt::DisplayRole:
            case Qt::EditRole:
                return QString::fromUtf8(gnc_get_reconcile_str(split.get_reconcile()));
            default:
                return QVariant();
            }
        case COLUMN_INCREASE:
            switch (role)
            {
            case Qt::DisplayRole:
            case Qt::EditRole:
                if (!amount.zero_p() && amount.positive_p())
                    return g2q(amount.printAmount(printInfo));
                else
                    return QString();
            default:
                return QVariant();
            }
        case COLUMN_DECREASE:
            switch (role)
            {
            case Qt::DisplayRole:
            case Qt::EditRole:
                if (amount.zero_p() || amount.positive_p())
                    return QString();
                else
                    return g2q(amount.neg().printAmount(printInfo));
            default:
                return QVariant();
            }
        case COLUMN_BALANCE:
        default:
            return QVariant();
        }


    }
    else
    {
        // Normal case: We are in a row that displays a normal
        // transaction and split

        Glib::RefPtr<Split> split = Glib::wrap(static_cast< ::Split*>(index.internalPointer()));
        Glib::RefPtr<Transaction> trans(split->get_parent());
        Numeric amount = split->get_value(); // Alternatively: xaccSplitConvertAmount(split.gobj(), split.getAccount().gobj());
        PrintAmountInfo printInfo(split, false);

        switch (index.column())
        {
        case COLUMN_DATE:
            switch (role)
            {
            case Qt::DisplayRole:
            case Qt::EditRole:
                return g2q(trans->get_date_posted());
            default:
                return QVariant();
            }
        case COLUMN_NUM:
            switch (role)
            {
            case Qt::DisplayRole:
            case Qt::EditRole:
                return g2q(trans->get_num());
            default:
                return QVariant();
            }
        case COLUMN_DESC:
            switch (role)
            {
            case Qt::DisplayRole:
            case Qt::EditRole:
                return g2q(trans->get_description());
            default:
                return QVariant();
            }
        case COLUMN_ACCOUNT:
            switch (role)
            {
            case Qt::DisplayRole:
            case Qt::EditRole:
                if (trans->get_num_splits() == 2)
                    return QVariant::fromValue(split->get_other_split()->get_account()->gobj());
                else
                    return g2q(split->get_corr_account_full_name());
            default:
                return QVariant();
            }
        case COLUMN_RECONCILE:
            switch (role)
            {
            case Qt::DisplayRole:
            case Qt::EditRole:
                return QString::fromUtf8(gnc_get_reconcile_str(split->get_reconcile()));
            default:
                return QVariant();
            }
        case COLUMN_INCREASE:
            switch (role)
            {
            case Qt::DisplayRole:
            case Qt::EditRole:
                if (amount.positive_p())
                    return g2q(amount.printAmount(printInfo));
                else
                    return QString();
            default:
                return QVariant();
            }
        case COLUMN_DECREASE:
            switch (role)
            {
            case Qt::DisplayRole:
            case Qt::EditRole:
                if (amount.positive_p())
                    return QString();
                else
                    return g2q(amount.neg().printAmount(printInfo));
            default:
                return QVariant();
            }
        case COLUMN_BALANCE:
            switch (role)
            {
            case Qt::DisplayRole:
                return g2q(split->get_balance().printAmount(printInfo));
            case Qt::ForegroundRole:
                return split->get_balance().negative_p()
                       ? QBrush(Qt::red)
                       : QBrush();
            default:
                return QVariant();
            }
        default:
            return QVariant();
        }
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
        case COLUMN_DATE:
            return tr("Date");
        case COLUMN_NUM:
            return tr("Num");
        case COLUMN_DESC:
            return tr("Description");
        case COLUMN_ACCOUNT:
            return tr("Transfer");
        case COLUMN_RECONCILE:
            return tr("R?");
        case COLUMN_INCREASE:
            return tr("Increase");
        case COLUMN_DECREASE:
            return tr("Decrease");
        case COLUMN_BALANCE:
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
    if (!index.isValid() || role != Qt::EditRole)
        return false;

    QUndoCommand* cmd = NULL;
    if (m_enableNewTransaction && index.row() == m_list.size())
    {
        // Special case: We are in the last row which represents the
        // newly entered txn.

        TmpTransaction& trans = m_tmpTransaction;
        TmpSplit& split = trans.get_splits().front();
        Q_ASSERT(split.get_account() == m_account->gobj());
        Q_ASSERT(trans.get_num_splits() == 2);
        TmpSplit& other = trans.get_splits().back();

        // "Editing" is done by creating a Cmd-object and adding it to
        // the undo stack. That's in fact all that was needed to
        // create an *undoable* edit of this data cell. Seems almost
        // spooky, doesn't it?
        switch (index.column())
        {
        case COLUMN_DATE:
        {
            QDate date = value.toDate();
            if (date.isValid())
            {
                cmd = cmd::setTransactionDate(trans, date);
            }
            break;
        }
        case COLUMN_NUM:
            cmd = cmd::setTransactionNum(trans, value.toString());
            break;
        case COLUMN_DESC:
            cmd = cmd::setTransactionDescription(trans, value.toString());
            break;
        case COLUMN_ACCOUNT:
            if (value.canConvert< ::Account*>())
            {
                if (trans.get_num_splits() == 2)
                {
                    cmd = cmd::setSplitAccount(other, Glib::wrap(value.value< ::Account*>()));
                }
                else
                    QMessageBox::warning(NULL, tr("Unimplemented"),
                                         tr("Sorry, but editing a transaction with more than two splits (here: %1) is not yet implemented.").arg(trans.get_num_splits()));
            }
            break;
        case COLUMN_RECONCILE:
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
        case COLUMN_INCREASE:
        case COLUMN_DECREASE:
        {
            QString str(value.toString().simplified());
            if (str.isEmpty())
                break;
            Numeric n;
            QString errmsg = g2q(n.parse(q2g(str)));
            if (errmsg.isEmpty())
            {
                qDebug() << "Successfully parsed string to numeric=" << g2q(n.to_string());
                if (index.column() == COLUMN_DECREASE)
                    n = n.neg();
                // Check whether we have the simple case here
                if (trans.get_num_splits() != 2)
                {
                    QMessageBox::warning(NULL, tr("Unimplemented"),
                                         tr("Sorry, but editing a transaction with more than two splits (here: %1) is not yet implemented.").arg(trans.get_num_splits()));
                }
                else
                {
                    Glib::RefPtr<Commodity> originCommodity = m_account->get_commodity();
                    Glib::RefPtr<Commodity> transCommodity = trans.get_commodity();
                    bool sameCommodities = (originCommodity == transCommodity);
                    if (other.get_account())
                    {
                        Glib::RefPtr<Commodity> otherCommodity = Glib::wrap(other.get_account())->get_commodity();
                        sameCommodities = sameCommodities && (transCommodity == otherCommodity);
                    }
                    if (!sameCommodities)
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
    else
    {
        // Normal case: We are in a row that displays a normal
        // transaction and split

        Glib::RefPtr<Split> split = Glib::wrap(static_cast< ::Split*>(index.internalPointer()));
        Glib::RefPtr<Transaction> trans(split->get_parent());
        QVariant y(trans);

        // "Editing" is done by creating a Cmd-object and adding it to
        // the undo stack. That's in fact all that was needed to
        // create an *undoable* edit of this data cell. Seems almost
        // spooky, doesn't it?
        switch (index.column())
        {
        case COLUMN_DATE:
        {
            QDate date = value.toDate();
            if (date.isValid())
            {
                cmd = cmd::setTransactionDate(trans, date);
            }
            break;
        }
        case COLUMN_NUM:
            cmd = cmd::setTransactionNum(trans, value.toString());
            break;
        case COLUMN_DESC:
            cmd = cmd::setTransactionDescription(trans, value.toString());
            break;
        case COLUMN_ACCOUNT:
            if (value.canConvert< ::Account*>())
            {
                if (trans->get_num_splits() == 2)
                {
                    Glib::RefPtr<Split> other = split->get_other_split();
                    cmd = cmd::setSplitAccount(other, Glib::wrap(value.value< ::Account*>()));
                }
                else
                    QMessageBox::warning(NULL, tr("Unimplemented"),
                                         tr("Sorry, but editing a transaction with more than two splits "
                                            "(here: %1) is not yet implemented.").arg(trans->get_num_splits()));
            }
            break;
        case COLUMN_RECONCILE:
        {
            QString str(value.toString());
            if (!str.isEmpty())
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
        case COLUMN_INCREASE:
        case COLUMN_DECREASE:
        {
            QString str(value.toString().simplified());
            if (str.isEmpty())
                break;
            Numeric n;
            QString errmsg = g2q(n.parse(q2g(str)));
            if (errmsg.isEmpty())
            {
                qDebug() << "Successfully parsed string to numeric=" << g2q(n.to_string());
                if (index.column() == COLUMN_DECREASE)
                    n = n.neg();
                // Check whether we have the simple case here
                if (split->get_parent()->get_num_splits() != 2)
                {
                    QMessageBox::warning(NULL, tr("Unimplemented"),
                                         tr("Sorry, but editing a transaction with more than two splits "
                                            "(here: %1) is not yet implemented.").arg(split->get_parent()->get_num_splits()));
                }
                else
                {
                    Glib::RefPtr<Transaction> trans = split->get_parent();
                    Glib::RefPtr<Split> other = split->get_other_split();
                    Q_ASSERT(other);
                    Glib::RefPtr<Commodity> originCommodity = split->get_account()->get_commodity();
                    Glib::RefPtr<Commodity> transCommodity = trans->get_currency();
                    Glib::RefPtr<Commodity> otherCommodity = other->get_account()->get_commodity();
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

void SplitListModel::transactionEvent( ::Transaction* trans, QofEventId event_type)
{
    //qDebug() << "SplitListModel::transactionEvent, id=" << qofEventToString(event_type);
    switch (event_type)
    {
    case QOF_EVENT_MODIFY:
        if (m_hash.contains(trans))
        {
            int row = m_hash.value(trans);
            Q_EMIT dataChanged(index(row, 0), index(row, columnCount() - 1));
        }
        break;
    case GNC_EVENT_ITEM_REMOVED:
    case GNC_EVENT_ITEM_ADDED:
        // This event is triggered by a split being added (or removed)
        // to a transaction. Ignored because we already reacted upon
        // the MODIFY event.
        break;
    case QOF_EVENT_CREATE:
        // This event is triggered by a newly created transaction, but
        // we reacted on this in the accountEvent handler already.
        break;
    default:
        qDebug() << "SplitListModel::transactionEvent, ignored event id=" << qofEventToString(event_type);
        break;
    }

}

void SplitListModel::accountEvent( ::Account* acc, QofEventId event_type)
{
    if (acc != m_account->gobj())
        return;
    //qDebug() << "SplitListModel::accountEvent, id=" << qofEventToString(event_type);

    switch (event_type)
    {
    case GNC_EVENT_ITEM_REMOVED:
    case GNC_EVENT_ITEM_ADDED:
        recreateCache();
        break;
    case QOF_EVENT_MODIFY:
    case GNC_EVENT_ITEM_CHANGED:
        // These events are also triggered e.g. by a newly added
        // transaction/split in this account. However, we already
        // reacted on this by the above events, so we can ignore them
        // here.
        break;
    default:
        qDebug() << "SplitListModel::accountEvent, ignored event id=" << qofEventToString(event_type);
        break;
    }
}

void SplitListModel::editorClosed(const QModelIndex& index,
                                  QAbstractItemDelegate::EndEditHint hint)
{
    if (!index.isValid())
        return;

    if (m_enableNewTransaction && index.row() == m_list.size())
    {
        // Special case: The line with the new transaction

        switch (hint)
        {
        case QAbstractItemDelegate::SubmitModelCache:
            switch (index.column())
            {
            case COLUMN_DATE:
            case COLUMN_NUM:
            case COLUMN_DESC:
            case COLUMN_ACCOUNT:
            case COLUMN_RECONCILE:
                break; // we want to do nothing
            case COLUMN_INCREASE:
            case COLUMN_DECREASE:
            {
                // Commit the new transaction
                //qDebug() << "Commit the new transaction as a real one";
                m_tmpTransaction.set_date_entered(QDateTime::currentDateTime().toTime_t());
                QUndoCommand* cmd = cmd::commitNewTransaction(m_tmpTransaction);
                recreateTmpTrans();
                m_undoStack->push(cmd);
                break;
            }
            default:
                break; // nothing to do
            }
        case QAbstractItemDelegate::RevertModelCache:
            recreateTmpTrans();
            break;
        }
    }
    else
    {
        // We are in the line of an existing transaction - no function
        // implemented yet.
    }

}

} // END namespace gnc
