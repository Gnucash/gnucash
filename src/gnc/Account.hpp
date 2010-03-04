#ifndef GNC_ACCOUNT_HPP
#define GNC_ACCOUNT_HPP

// gnucash includes
#include "config.h"
extern "C"
{
#include "qof.h"
#include "engine/Account.h"
}

#include "gnc/WeakPointer.hpp"

#include <QString>
#include <QList>

namespace gnc
{

class Account : public WeakPointer< ::Account >
{
public:
    typedef WeakPointer< ::Account > base_class;
    Account(element_type* ptr = 0)
            : base_class(ptr)
    { }
    QString getName() const { return QString::fromUtf8(xaccAccountGetName(get())); }
    QString getCode() const { return QString::fromUtf8(xaccAccountGetCode(get())); }
    QString getDescription() const { return QString::fromUtf8(xaccAccountGetDescription(get())); }
    Account get_parent() const { return gnc_account_get_parent(get()); }
    Account get_root() { return gnc_account_get_root(get()); }
    bool is_root() const { return gnc_account_is_root(get()); }
    gint n_children() const { return gnc_account_n_children(get()); }
    GList *get_children() const { return gnc_account_get_children(get()); }
    GList * get_descendants () const { return gnc_account_get_descendants (get()); }
    Account nth_child (gint num) const { return gnc_account_nth_child(get(), num); }

    typedef QList< ::Account*> AccountQList;
    static AccountQList fromGList(GList* glist)
    {
        AccountQList result;
        GList* list = glist;
        while (list)
        {
            result.append(reinterpret_cast< ::Account*>(list->data));
            list = g_list_next(list);
        }
        return result;
    }
};

} // END namespace gnc

#endif
