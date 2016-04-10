/********************************************************************\
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
 *                                                                  *
 * This program is distributed in the hope that it will be useful,  *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of   *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    *
 * GNU General Public License for more details.                     *
 *                                                                  *
 * You should have received a copy of the GNU General Public License*
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/


#include <glib.h>

// Disable the 'const' function attribute of the get_type() functions.
// GCC would optimize them out because we don't use the return value.
#undef  G_GNUC_CONST
#define G_GNUC_CONST /* empty */

//#include <giomm/wrap_init.h>
#include <glibmm/error.h>
#include <glibmm/object.h>

// #include the widget headers so that we can call the get_type() static methods:

#include "config.h"
#include "Account.hpp"
#include "Book.hpp"
#include "Commodity.hpp"
#include "Split.hpp"
#include "Transaction.hpp"

extern "C"
{

//Declarations of the *_get_type() functions:

    GType gnc_account_get_type(void);
    GType qof_book_get_type(void);
    GType gnc_commodity_get_type(void);
    GType gnc_split_get_type(void);
    GType gnc_transaction_get_type(void);
    GType qof_instance_get_type (void);
} // extern "C"

//Declarations of the *_Class::wrap_new() methods, instead of including all the private headers:

namespace gnc {  class Account_Class { public: static Glib::ObjectBase* wrap_new(GObject*); };  }
namespace gnc {  class Book_Class { public: static Glib::ObjectBase* wrap_new(GObject*); };  }
namespace gnc {  class Commodity_Class { public: static Glib::ObjectBase* wrap_new(GObject*); };  }
namespace gnc {  class GncInstance_Class { public : static Glib::ObjectBase* wrap_new(GObject*); }; }
namespace gnc {  class Split_Class { public: static Glib::ObjectBase* wrap_new(GObject*); };  }
namespace gnc {  class Transaction_Class { public: static Glib::ObjectBase* wrap_new(GObject*); };  }

namespace gnc
{
void wrap_init()
{
// Map gtypes to gtkmm wrapper-creation functions:
    Glib::wrap_register(gnc_account_get_type(), &gnc::Account_Class::wrap_new);
    Glib::wrap_register(qof_book_get_type(), &gnc::Book_Class::wrap_new);
    Glib::wrap_register(gnc_commodity_get_type(), &gnc::Commodity_Class::wrap_new);
    Glib::wrap_register(qof_instance_get_type(), &gnc::GncInstance_Class::wrap_new);
    Glib::wrap_register(gnc_split_get_type(), &gnc::Split_Class::wrap_new);
    Glib::wrap_register(gnc_transaction_get_type(), &gnc::Transaction_Class::wrap_new);

    // Register our gtkmm gtypes:
    gnc::Account::get_type();
    gnc::Book::get_type();
    gnc::Commodity::get_type();
    gnc::GncInstance::get_type();
    gnc::Split::get_type();
    gnc::Transaction::get_type();
} // wrap_init()
} // END namespace gnc

