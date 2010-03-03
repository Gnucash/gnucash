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

#include <QAbstractItemModel>

namespace gnc
{

class Account : public WeakPointer< ::Account >
{
public:
    typedef WeakPointer< ::Account > base_class;
    Account(element_type* ptr = 0)
            : base_class(ptr)
    { }
    std::string getName() const { return xaccAccountGetName(get()); }
    Account get_parent() const { return gnc_account_get_parent(get()); }
    Account get_root() { return gnc_account_get_root(get()); }
    bool is_root() const { return gnc_account_is_root(get()); }
    gint n_children() const { return gnc_account_n_children(get()); }
    GList *get_children() const { return gnc_account_get_children(get()); }
    GList * get_descendants () const { return gnc_account_get_descendants (get()); }
    Account nth_child (gint num) const { return gnc_account_nth_child(get(), num); }

};

} // END namespace gnc

#endif
