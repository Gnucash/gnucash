#ifndef GNC_ACCOUNTITEMMODEL_HPP
#define GNC_ACCOUNTITEMMODEL_HPP

#include "gnc/Account.hpp"

#include <QAbstractItemModel>
#include <QAbstractListModel>

namespace gnc
{

class AccountItemModel : public QAbstractListModel
{
    Q_OBJECT
public:
    AccountItemModel(Account rootaccount, QObject *parent = 0)
            : QAbstractListModel(parent)
            , m_root(rootaccount)
    {
        GList* list = m_root.get_descendants();
        while (list)
        {
            m_acclist.append(reinterpret_cast< ::Account*>(list->data));
            list = g_list_next(list);
        }
    }

    int rowCount(const QModelIndex& parent = QModelIndex()) const { return m_acclist.size(); }
    QVariant data(const QModelIndex& index, int role) const
    {
        if (!index.isValid())
            return QVariant();
        if (index.row() > rowCount(index))
            return QVariant();
        if (role == Qt::DisplayRole)
            return QString::fromStdString(Account(m_acclist.at(index.row())).getName());
        else
            return QVariant();
    }
    QVariant headerData(int section, Qt::Orientation orientation, int role) const
    {
        if (role != Qt::DisplayRole)
            return QVariant();
        if (orientation == Qt::Horizontal)
            return QString("Account Name");
        else
            return QString("#%1").arg(1 + section);
    }
private:
    Account m_root;
    QList< ::Account*> m_acclist;
};

} // END namespace gnc

#endif
