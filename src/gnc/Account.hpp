#ifndef GNC_ACCOUNT_HPP
#define GNC_ACCOUNT_HPP

// gnucash includes
extern "C"
{
#include "qof.h"
#include "engine/Account.h"
}

#include "gnc/WeakPointer.hpp"

namespace gnc
{

class Account : public WeakPointer< ::Account >
{
public:
    typedef WeakPointer< ::Account > base_class;
    Account(element_type* ptr = 0)
            : base_class(ptr)
    { }
};

} // END namespace gnc

#endif
