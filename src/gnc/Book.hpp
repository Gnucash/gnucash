#ifndef GNC_BOOK_HPP
#define GNC_BOOK_HPP

// gnucash includes
extern "C"
{
#include "qof.h"
#include "engine/gnc-hooks.h"
#include "engine/Account.h"
}

#include "gnc/WeakPointer.hpp"

namespace gnc
{
class Account;

class Book : public WeakPointer< ::QofBook >
{
public:
    typedef WeakPointer< ::QofBook > base_class;
    Book(element_type* ptr = 0)
            : base_class(ptr)
    { }

    Account get_root_account();
};

} // END namespace gnc

#endif
