# gnucash_core.py -- High level python wrapper classes for the core parts
#                    of GnuCash
#
# Copyright (C) 2008 ParIT Worker Co-operative <paritinfo@parit.ca>
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation; either version 2 of
# the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, contact:
# Free Software Foundation           Voice:  +1-617-542-5942
# 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
# Boston, MA  02110-1301,  USA       gnu@gnu.org
#
# @author Mark Jenkins, ParIT Worker Co-operative <mark@parit.ca>
# @author Jeff Green,   ParIT Worker Co-operative <jeff@parit.ca>

import gnucash_core_c

from function_class import \
     ClassFromFunctions, extract_attributes_with_prefix, \
     default_arguments_decorator, method_function_returns_instance, \
     methods_return_instance

class GnuCashCoreClass(ClassFromFunctions):
    _module = gnucash_core_c

class GnuCashBackendException(Exception):
    def __init__(self, msg, errors):
        Exception.__init__(self, msg)
        self.errors = errors

class Session(GnuCashCoreClass):
    """A GnuCash book editing session

    To commit changes to the session you may need to call save,
    (this is allways the case with the file backend).
    
    When you're down with a session you may need to call end()

    Every Session has a Book in the book attribute, which you'll definetely
    be interested in, as every GnuCash entity (Transaction, Split, Vendor,
    Invoice..) is associated with a particular book where it is stored.
    """

    def __init__(self, book_uri=None, is_new=False):
        """A convienent contructor that allows you to specify a book URI,
        begin the session, and load the book.

        This can give you the power of calling
        qof_session_new, qof_session_begin, and qof_session_load all in one!

        book_uri can be None to skip the calls to qof_session_begin and
        qof_session_load, or it can be a string like "file:/test.xac"

        qof_session_load is only called if is_new is set to False

        is_new is passed to qof_session_begin as the
        argument create_if_nonexistent

        This function can raise a GnuCashBackendException. If it does,
        you don't need to cleanup and call end() and destroy(), that is handled
        for you, and the exception is raised.
        """
        GnuCashCoreClass.__init__(self)
        if book_uri is not None:
            try:
                self.begin(book_uri, False, is_new)
                if not is_new:
                    self.load()
            except GnuCashBackendException, backend_exception:
                self.end()
                self.destroy()
                raise

    def raise_backend_errors(self, called_function="qof_session function"):
        """Raises a GnuCashBackendException if there are outstanding
        QOF_BACKEND errors.

        set called_function to name the function that was last called
        """
        errors = self.pop_all_errors()
        if errors != ():
            raise GnuCashBackendException(
                "call to %s resulted in the "
                "following errors, %s" % (called_function, errors),
                errors )        

    def generate_errors(self):
        """A generator that yeilds any outstanding QofBackend errors 
        """
        while self.get_error() is not ERR_BACKEND_NO_ERR:
            error = self.pop_error()
            yield error

    def pop_all_errors(self):
        """Returns any accumulated qof backend errors as a tuple
        """
        return tuple( self.generate_errors() )

    # STATIC METHODS
    @staticmethod
    def raise_backend_errors_after_call(function):
        """A function decorator that results in a call to  
        raise_backend_errors after execution.
        """
        def new_function(self, *args):
            return_value = function(self, *args)
            self.raise_backend_errors(function.__name__)
            return return_value
        return new_function

class Book(GnuCashCoreClass):
    """A Book encapsulates all of the GnuCash data, it is the place where
    all GnuCash entities (Transaction, Split, Vendor, Invoice...), are
    stored. You'll notice that all of the constructors for those entities
    need a book to be associated with.

    The most common way to get a book is through the book property in the
    Session class, that is, create a session that connects to some storage,
    such as through 'my_session = Session('file:my_books.xac')', and access
    the book via the book property, 'my_session.book'

    If you would like to create a Book without any backing storage, call the
    Book constructor wihout any parameters, 'Book()'. You can later merge
    such a book into a book with actual store by using merge_init.

    Methods of interest
    get_root_account -- Returns the root level Account
    get_table -- Returns a commodity lookup table, of type GncCommodityTable
    """
    pass

class GncNumeric(GnuCashCoreClass):
    """Object used by GnuCash to store all numbers. Always consists of a
    numerator and denominator.
    """

    def __init__(self, num=0, denom=1, **kargs):
        """Constructor that allows you to set the numerator and denominator or
        leave them blank with a default value of 0 (not a good idea since there
        is currently no way to alter the value after instantiation)
        """
        GnuCashCoreClass.__init__(self, num, denom, **kargs)
        #if INSTANCE_ARG in kargs:
        #    GnuCashCoreClass.__init__(**kargs)
        #else:
        #    self.set_denom(denom) # currently undefined
        #    self.set_num(num)     # currently undefined

class GncCommodity(GnuCashCoreClass): pass

class GncCommodityTable(GnuCashCoreClass):
    """A CommodityTable provides a way to store and lookup commoditys.
    Commoditys are primarily currencies, but other tradable things such as
    stocks, mutual funds, and material substances are posible.

    Users of this library should not create thier own CommodityTable, instead
    the get_table method from the Book class should be used.

    This table is automatically populated with the GnuCash default commodity's
    which includes most of the world's currencies.
    """
    pass

class GncLot(GnuCashCoreClass):
    pass

class Transaction(GnuCashCoreClass):
    """A GnuCash Transaction
    
    Consists of at least one (generally two) splits to represent a transaction
    between two accounts.
    """
    _new_instance = 'xaccMallocTransaction'
    def GetNthSplit(self, n):
        return self.GetSplitList().pop(n)

class Split(GnuCashCoreClass):
    """A GnuCash Split

    The most basic representation of a movement of currency from one account to
    another.
    """
    _new_instance = 'xaccMallocSplit'

class Account(GnuCashCoreClass):
    """A GnuCash Account.

    A fundamental entity in accounting, an Account provides representation
    for a financial object, such as a BANK account, an ASSET (like a building),
    a LIABILITY (such as a bank loan), a summary of some type of EXPENSE, or
    a summary of some source of INCOME.

    The words in upper case are the constants that GnuCash and this library uses
    to describe account type. Here is the full list:
    BANK, CASH, CREDIT, ASSET, LIABILITY, STOCK, MUTUAL
    CURRENCY, INCOME, EXPENSE, EQUITY, RECEIVABLE, PAYABLE, 
    CHECKING, SAVINGS, MONEYMRKT, CREDITLINE

    These are not strings, they are attributes you can import from this
    module
    """
    _new_instance = 'xaccMallocAccount'

class GUID(GnuCashCoreClass):
    _new_instance = 'guid_new_return'

# Session
Session.add_constructor_and_methods_with_prefix('qof_session_', 'new')

def one_arg_default_none(function):
    return default_arguments_decorator(function, None, None)
Session.decorate_functions(one_arg_default_none, "load", "save")

Session.decorate_functions( Session.raise_backend_errors_after_call,
                            "begin", "load", "save", "end")
Session.get_book = method_function_returns_instance(
    Session.get_book, Book )

Session.book = property( Session.get_book )

# import all of the session backend error codes into this module
this_module_dict = globals()
for error_name, error_value, error_name_after_prefix in \
    extract_attributes_with_prefix(gnucash_core_c, 'ERR_'):
    this_module_dict[ error_name ] = error_value

#Book
Book.add_constructor_and_methods_with_prefix('qof_book_', 'new')
Book.add_method('gnc_book_get_root_account', 'get_root_account')
Book.add_method('gnc_book_set_root_account', 'set_root_account')
Book.add_method('gnc_commodity_table_get_table', 'get_table')
#Functions that return Account
Book.get_root_account = method_function_returns_instance(
    Book.get_root_account, Account )
#Functions that return GncCommodityTable
Book.get_table = method_function_returns_instance(
    Book.get_table, GncCommodityTable ) 

# GncNumeric
GncNumeric.add_constructor_and_methods_with_prefix('gnc_numeric_', 'create')

gncnumeric_dict =   { 
                        'same' : GncNumeric, 
                        'add' : GncNumeric, 
                        'sub' : GncNumeric, 
                        'mul' : GncNumeric, 
                        'div' : GncNumeric, 
                        'neg' : GncNumeric, 
                        'abs' : GncNumeric, 
                        'add_fixed' : GncNumeric, 
                        'sub_fixed' : GncNumeric, 
                        'add_with_error' : GncNumeric, 
                        'sub_with_error' : GncNumeric, 
                        'mul_with_error' : GncNumeric, 
                        'div_with_error' : GncNumeric, 
                        'convert' : GncNumeric, 
                        'reduce' : GncNumeric 
                    }
methods_return_instance(GncNumeric, gncnumeric_dict)

# GncCommodity
GncCommodity.add_constructor_and_methods_with_prefix('gnc_commodity_', 'new')
#Functions that return GncCommodity
GncCommodity.clone = method_function_returns_instance(
    GncCommodity.clone, GncCommodity )

# GncCommodityTable
GncCommodityTable.add_methods_with_prefix('gnc_commodity_table_')
commoditytable_dict =   { 
                            'lookup' : GncCommodity, 
                            'lookup_unique' : GncCommodity, 
                            'find_full' : GncCommodity, 
                            'insert' : GncCommodity 
                        }
methods_return_instance(GncCommodityTable, commoditytable_dict)

# GncLot
GncLot.add_constructor_and_methods_with_prefix('gnc_lot_', 'new')

gnclot_dict =   { 
                    'get_account' : Account, 
                    'get_book' : Book, 
                    'get_earliest_split' : Split, 
                    'get_latest_split' : Split, 
                    'get_balance' : GncNumeric, 
                    'lookup' : GncLot, 
                    'make_default' : GncLot 
                }
methods_return_instance(GncLot, gnclot_dict)

# Transaction
Transaction.add_methods_with_prefix('xaccTrans')
Transaction.add_method('gncTransGetGUID', 'GetGUID');

trans_dict =    {
                    'GetSplit': Split, 
                    'FindSplitByAccount': Split, 
                    'GetNthSplit': Split, 
                    'Clone': Transaction, 
                    'Reverse': Transaction, 
                    'GetReversedBy': Transaction, 
                    'GetImbalance': GncNumeric, 
                    'GetAccountValue': GncNumeric, 
                    'GetAccountAmount': GncNumeric, 
                    'GetAccountConvRate': GncNumeric, 
                    'GetAccountBalance': GncNumeric, 
                    'GetCurrency': GncCommodity,
                    'GetGUID': GUID
                }
methods_return_instance(Transaction, trans_dict)

# Split
Split.add_methods_with_prefix('xaccSplit')
Split.add_method('gncSplitGetGUID', 'GetGUID');

split_dict =    {
                    'GetBook': Book, 
                    'GetAccount': Account, 
                    'GetParent': Transaction, 
                    'Lookup': Split, 
                    'GetOtherSplit': Split, 
                    'GetAmount': GncNumeric, 
                    'GetValue': GncNumeric, 
                    'GetSharePrice': GncNumeric, 
                    'ConvertAmount': GncNumeric, 
                    'GetBaseValue': GncNumeric, 
                    'GetBalance': GncNumeric, 
                    'GetClearedBalance': GncNumeric, 
                    'GetReconciledBalance': GncNumeric, 
                    'VoidFormerAmount': GncNumeric, 
                    'VoidFormerValue': GncNumeric,
                    'GetGUID': GUID
                }
methods_return_instance(Split, split_dict)

Split.account = property( Split.GetAccount, Split.SetAccount )
Split.parent = property( Split.GetParent, Split.SetParent )

# Account
Account.add_methods_with_prefix('xaccAccount')
Account.add_methods_with_prefix('gnc_account_')
Account.add_method('gncAccountGetGUID', 'GetGUID');

account_dict =  {   
                    'get_book' : Book, 
                    'Lookup' : Account, 
                    'get_parent' : Account, 
                    'get_root' : Account, 
                    'nth_child' : Account, 
                    'lookup_by_name' : Account, 
                    'lookup_by_full_name' : Account, 
                    'FindTransByDesc' : Transaction, 
                    'FindSplitByDesc' : Split, 
                    'get_start_balance' : GncNumeric, 
                    'get_start_cleared_balance' : GncNumeric, 
                    'GetBalance' : GncNumeric, 
                    'GetClearedBalance' : GncNumeric, 
                    'GetReconciledBalance' : GncNumeric, 
                    'GetPresentBalance' : GncNumeric, 
                    'GetProjectedMinimumBalance' : GncNumeric, 
                    'GetBalanceAsOfDate' : GncNumeric, 
                    'ConvertBalanceToCurrency' : GncNumeric, 
                    'ConvertBalanceToCurrencyAsOfDate' : GncNumeric, 
                    'GetBalanceInCurrency' : GncNumeric, 
                    'GetClearedBalanceInCurrency' : GncNumeric, 
                    'GetReconciledBalanceInCurrency' : GncNumeric, 
                    'GetPresentBalanceInCurrency' : GncNumeric, 
                    'GetProjectedMinimumBalanceInCurrency' : GncNumeric, 
                    'GetBalanceAsOfDateInCurrency' : GncNumeric, 
                    'GetBalanceChangeForPeriod' : GncNumeric, 
                    'GetCommodity' : GncCommodity,
                    'GetGUID': GUID 
                }
methods_return_instance(Account, account_dict)

Account.name = property( Account.GetName, Account.SetName )

#GUID
GUID.add_methods_with_prefix('guid_')
GUID.add_method('xaccAccountLookup', 'AccountLookup')
GUID.add_method('xaccTransLookup', 'TransLookup')
GUID.add_method('xaccSplitLookup', 'SplitLookup')

guid_dict = {
                'copy' : GUID,
                'TransLookup': Transaction,
                'AccountLookup': Account,
                'SplitLookup': Split
            }
methods_return_instance(GUID, guid_dict)
                
