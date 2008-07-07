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
     default_arguments_decorator, method_function_returns_instance

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

class GncCommodityTable(GnuCashCoreClass):
    def __init__(self, book, **kargs):
        GnuCashCoreClass.__init__(self, book, **kargs)

class GncLot(GnuCashCoreClass):
    def __init__(self, book, **kargs):
        GnuCashCoreClass.__init__(self, book, **kargs)

class Transaction(GnuCashCoreClass):
    _new_instance = 'xaccMallocTransaction'

class Split(GnuCashCoreClass):
    _new_instance = 'xaccMallocSplit'

class Account(GnuCashCoreClass):
    _new_instance = 'xaccMallocAccount'

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
#Functions that return Account
Book.get_root_account = method_function_returns_instance(
    Book.get_root_account, Account )

# GncNumeric
GncNumeric.add_constructor_and_methods_with_prefix('gnc_numeric_', 'create')
#Functions that return GncNumeric
GncNumeric.same = method_function_returns_instance(
    GncNumeric.same, GncNumeric )
GncNumeric.add = method_function_returns_instance(
    GncNumeric.add, GncNumeric )
GncNumeric.sub = method_function_returns_instance(
    GncNumeric.sub, GncNumeric )
GncNumeric.mul = method_function_returns_instance(
    GncNumeric.mul, GncNumeric )
GncNumeric.div = method_function_returns_instance(
    GncNumeric.div, GncNumeric )
GncNumeric.neg = method_function_returns_instance(
    GncNumeric.neg, GncNumeric )
GncNumeric.abs = method_function_returns_instance(
    GncNumeric.abs, GncNumeric )
GncNumeric.add_fixed = method_function_returns_instance(
    GncNumeric.add_fixed, GncNumeric )
GncNumeric.sub_fixed = method_function_returns_instance(
    GncNumeric.sub_fixed, GncNumeric )
GncNumeric.add_with_error = method_function_returns_instance(
    GncNumeric.add_with_error, GncNumeric )
GncNumeric.sub_with_error = method_function_returns_instance(
    GncNumeric.sub_with_error, GncNumeric )
GncNumeric.mul_with_error = method_function_returns_instance(
    GncNumeric.mul_with_error, GncNumeric )
GncNumeric.div_with_error = method_function_returns_instance(
    GncNumeric.div_with_error, GncNumeric )
GncNumeric.convert = method_function_returns_instance(
    GncNumeric.convert, GncNumeric )
GncNumeric.reduce = method_function_returns_instance(
    GncNumeric.reduce, GncNumeric )

# GncCommodity
GncCommodity.add_constructor_and_methods_with_prefix('gnc_commodity_', 'new')
#Functions that return GncCommodity
GncCommodity.clone = method_function_returns_instance(
    GncCommodity.clone, GncCommodity )

# GncCommodityTable
GncCommodityTable.add_constructor_and_methods_with_prefix('gnc_commodity_table_', 'get_table')
#Functions that return GncCommodity
GncCommodityTable.lookup = method_function_returns_instance(
    GncCommodityTable.lookup, GncCommodity )
GncCommodityTable.lookup_unique = method_function_returns_instance(
    GncCommodityTable.lookup_unique, GncCommodity )
GncCommodityTable.find_full = method_function_returns_instance(
    GncCommodityTable.find_full, GncCommodity )
GncCommodityTable.insert = method_function_returns_instance(
    GncCommodityTable.insert, GncCommodity )

# GncLot
GncLot.add_constructor_and_methods_with_prefix('gnc_lot_', 'new')
#Functions that return Account
GncLot.get_account = method_function_returns_instance(
    GncLot.get_account, Account )
#Functions that return Book
GncLot.get_book = method_function_returns_instance(
    GncLot.get_book, Book )
#Functions that return Split
GncLot.get_earliest_split = method_function_returns_instance(
    GncLot.get_earliest_split, Split )
GncLot.get_latest_split = method_function_returns_instance(
    GncLot.get_latest_split, Split )
#Functions that return GncNumeric
GncLot.get_balance = method_function_returns_instance(
    GncLot.get_balance, GncNumeric )
#Functions that return GncLot
GncLot.lookup = method_function_returns_instance(
    GncLot.lookup, GncLot )
GncLot.make_default = method_function_returns_instance(
    GncLot.make_default, GncLot )


# Transaction
Transaction.add_methods_with_prefix('xaccTrans')
#Functions that return Split
Transaction.GetSplit = method_function_returns_instance(
    Transaction.GetSplit, Split )
Transaction.FindSplitByAccount = method_function_returns_instance(
    Transaction.FindSplitByAccount, Split )
#Functions that return Transaction
Transaction.Clone = method_function_returns_instance(
    Transaction.Clone, Transaction )
Transaction.Reverse = method_function_returns_instance(
    Transaction.Reverse, Transaction )
Transaction.GetReversedBy = method_function_returns_instance(
    Transaction.GetReversedBy, Transaction )
#Functions that return GncCommodity
Transaction.GetCurrency = method_function_returns_instance(
    Transaction.GetCurrency, GncCommodity )
#Functions that return GncNumeric
Transaction.GetImbalance = method_function_returns_instance(
    Transaction.GetImbalance, GncNumeric )
Transaction.GetAccountValue = method_function_returns_instance(
    Transaction.GetAccountValue, GncNumeric )
Transaction.GetAccountAmount = method_function_returns_instance(
    Transaction.GetAccountAmount, GncNumeric )
Transaction.GetAccountConvRate = method_function_returns_instance(
    Transaction.GetAccountConvRate, GncNumeric )
Transaction.GetAccountBalance = method_function_returns_instance(
    Transaction.GetAccountBalance, GncNumeric )

# Split
Split.add_methods_with_prefix('xaccSplit')
#Functions that return Book
Split.GetBook = method_function_returns_instance(
    Split.GetBook, Book )
#Functions that return Account
Split.GetAccount = method_function_returns_instance(
    Split.GetAccount, Account )
#Functions that return Transaction
Split.GetParent = method_function_returns_instance(
    Split.GetParent, Transaction )
#Functions that return Split
Split.Lookup = method_function_returns_instance(
    Split.Lookup, Split )
Split.GetOtherSplit = method_function_returns_instance(
    Split.GetOtherSplit, Split )
#Functions that return GncNumeric
Split.GetAmount = method_function_returns_instance(
    Split.GetAmount, GncNumeric )
Split.GetValue = method_function_returns_instance(
    Split.GetValue, GncNumeric )
Split.GetSharePrice = method_function_returns_instance(
    Split.GetSharePrice, GncNumeric )
Split.ConvertAmount = method_function_returns_instance(
    Split.ConvertAmount, GncNumeric )
Split.GetBaseValue = method_function_returns_instance(
    Split.GetBaseValue, GncNumeric )
Split.GetBalance = method_function_returns_instance(
    Split.GetBalance, GncNumeric )
Split.GetClearedBalance = method_function_returns_instance(
    Split.GetClearedBalance, GncNumeric )
Split.GetReconciledBalance = method_function_returns_instance(
    Split.GetReconciledBalance, GncNumeric )
Split.VoidFormerAmount = method_function_returns_instance(
    Split.VoidFormerAmount, GncNumeric )
Split.VoidFormerValue = method_function_returns_instance(
    Split.VoidFormerValue, GncNumeric )

Split.account = property( Split.GetAccount, Split.SetAccount )
Split.parent = property( Split.GetParent, Split.SetParent )

# Account
Account.add_methods_with_prefix('xaccAccount')
Account.add_methods_with_prefix('gnc_account_')
#Functions that return Book
Account.get_book = method_function_returns_instance(
    Account.get_book, Book )
#Functions that return Account
Account.Lookup = method_function_returns_instance(
    Account.Lookup, Account )
Account.get_parent = method_function_returns_instance(
    Account.get_parent, Account )
Account.get_root = method_function_returns_instance(
    Account.get_root, Account )
Account.nth_child = method_function_returns_instance(
    Account.nth_child, Account )
Account.lookup_by_name = method_function_returns_instance(
    Account.lookup_by_name, Account )
Account.lookup_by_full_name = method_function_returns_instance(
    Account.lookup_by_full_name, Account )
#Functions that return Transaction
Account.FindTransByDesc = method_function_returns_instance(
    Account.FindTransByDesc, Transaction )
#Functions that return Split
Account.FindSplitByDesc = method_function_returns_instance(
    Account.FindSplitByDesc, Split )
#Functions that return GncNumeric
Account.get_start_balance = method_function_returns_instance(
    Account.get_start_balance, GncNumeric )
Account.get_start_cleared_balance = method_function_returns_instance(
    Account.get_start_cleared_balance, GncNumeric )
Account.GetBalance = method_function_returns_instance(
    Account.GetBalance, GncNumeric )
Account.GetClearedBalance = method_function_returns_instance(
    Account.GetClearedBalance, GncNumeric )
Account.GetReconciledBalance = method_function_returns_instance(
    Account.GetReconciledBalance, GncNumeric )
Account.GetPresentBalance = method_function_returns_instance(
    Account.GetPresentBalance, GncNumeric )
Account.GetProjectedMinimumBalance = method_function_returns_instance(
    Account.GetProjectedMinimumBalance, GncNumeric )
Account.GetBalanceAsOfDate = method_function_returns_instance(
    Account.GetBalanceAsOfDate, GncNumeric )
Account.ConvertBalanceToCurrency = method_function_returns_instance(
    Account.ConvertBalanceToCurrency, GncNumeric )
Account.ConvertBalanceToCurrencyAsOfDate = method_function_returns_instance(
    Account.ConvertBalanceToCurrencyAsOfDate, GncNumeric )
Account.GetBalanceInCurrency = method_function_returns_instance(
    Account.GetBalanceInCurrency, GncNumeric )
Account.GetClearedBalanceInCurrency = method_function_returns_instance(
    Account.GetClearedBalanceInCurrency, GncNumeric )
Account.GetReconciledBalanceInCurrency = method_function_returns_instance(
    Account.GetReconciledBalanceInCurrency, GncNumeric )
Account.GetPresentBalanceInCurrency = method_function_returns_instance(
    Account.GetPresentBalanceInCurrency, GncNumeric )
Account.GetProjectedMinimumBalanceInCurrency = method_function_returns_instance(
    Account.GetProjectedMinimumBalanceInCurrency, GncNumeric )
Account.GetBalanceAsOfDateInCurrency = method_function_returns_instance(
    Account.GetBalanceInCurrency, GncNumeric )
Account.GetBalanceChangeForPeriod = method_function_returns_instance(
    Account.GetBalanceChangeForPeriod, GncNumeric )
#Functions that return GncCommodity
Account.GetCommodity = method_function_returns_instance(
    Account.GetCommodity, GncCommodity )

Account.name = property( Account.GetName, Account.SetName )
