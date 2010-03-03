#include "config.h"
#include "gnc/Book.hpp"
#include "gnc/Account.hpp"

namespace gnc
{
Account Book::get_root_account()
{
    return Account(gnc_book_get_root_account (get()));
}

} // END namespace gnc
