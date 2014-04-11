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

class Book(GnuCashCoreClass): pass

class GncNumeric(GnuCashCoreClass):
    def __init__(self, num=0, denom=0, **kargs):
        GnuCashCoreClass.__init__(self, num, denom, **kargs)
        #if INSTANCE_ARG in kargs:
        #    GnuCashCoreClass.__init__(**kargs)
        #else:
        #    self.set_denom(denom) # currently undefined
        #    self.set_num(num)     # currently undefined

class GncCommodity(GnuCashCoreClass):
    def __init__(self, book, name=None, namespace=None, mnemonic=None, cusip=None, fraction=1, **kargs):
        GnuCashCoreClass.__init__(self, book, name, namespace, mnemonic, cusip, fraction, **kargs)

class GncCommodityTable(GnuCashCoreClass): pass

class GncLot(GnuCashCoreClass):
    def __init__(self, book, **kargs):
        GnuCashCoreClass.__init__(self, book, **kargs)

class Transaction(GnuCashCoreClass):
    _new_instance = 'xaccMallocTransaction'
    def GetNthSplit(self, n):
        return self.GetSplitList().pop(n)

class Split(GnuCashCoreClass):
    _new_instance = 'xaccMallocSplit'

class Account(GnuCashCoreClass):
    _new_instance = 'xaccMallocAccount'
    def GetNthChild(self, n):
        return self.get_children().pop(n)

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
#Functions that return Account
Book.get_root_account = method_function_returns_instance(
    Book.get_root_account, Account )

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
GncCommodityTable.add_constructor_and_methods_with_prefix('gnc_commodity_table_', 'get_table')

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
                    'GetCurrency': GncCommodity 
                }
methods_return_instance(Transaction, trans_dict)

# Split
Split.add_methods_with_prefix('xaccSplit')

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
                    'VoidFormerValue': GncNumeric 
                }
methods_return_instance(Split, split_dict)

Split.account = property( Split.GetAccount, Split.SetAccount )
Split.parent = property( Split.GetParent, Split.SetParent )

# Account
Account.add_methods_with_prefix('xaccAccount')
Account.add_methods_with_prefix('gnc_account_')

account_dict =  {   
                    'get_book' : Book, 
                    'Lookup' : Account, 
                    'get_parent' : Account, 
                    'get_root' : Account, 
                    'nth_child' : Account, 
                    'lookup_by_name' : Account, 
                    'lookup_by_full_name' : Account, 
                    'GetNthChild' : Account, 
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
                    'GetCommodity' : GncCommodity 
                }
methods_return_instance(Account, account_dict)

Account.name = property( Account.GetName, Account.SetName )

#GUID
GUID.add_methods_with_prefix('guid_')

