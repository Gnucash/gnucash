# gnucash_business.py -- High level python wrapper classes for the business
#                        parts of GnuCash
#
# Copyright (C) 2008,2010 ParIT Worker Co-operative <paritinfo@parit.ca>
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
##  @file
#   @brief High level python wrapper classes for the business parts of GnuCash
#   @author Mark Jenkins, ParIT Worker Co-operative <mark@parit.ca>
#   @author Jeff Green,   ParIT Worker Co-operative <jeff@parit.ca>
#   @ingroup python_bindings

import gnucash_core_c

from function_class import \
     ClassFromFunctions, extract_attributes_with_prefix, \
     default_arguments_decorator, method_function_returns_instance, \
     methods_return_instance, methods_return_instance_lists

from gnucash_core import \
     GnuCashCoreClass, GncNumeric, GncCommodity, Transaction, \
     Split, Book, GncLot, Account

from gnucash_core_c import GNC_OWNER_CUSTOMER, GNC_OWNER_JOB, \
    GNC_OWNER_EMPLOYEE, GNC_OWNER_VENDOR, \
    GNC_PAYMENT_CASH, GNC_PAYMENT_CARD, \
    GNC_DISC_PRETAX, GNC_DISC_SAMETIME, GNC_DISC_POSTTAX, \
    GNC_TAXINCLUDED_YES, GNC_TAXINCLUDED_NO, GNC_TAXINCLUDED_USEGLOBAL, \
    GNC_AMT_TYPE_VALUE, GNC_AMT_TYPE_PERCENT, GNC_ID_INVOICE

import datetime

class GnuCashBusinessEntity(GnuCashCoreClass):
    def __init__(self, book=None, id=None, currency=None, name=None,
                 instance=None):
        if instance == None:
            if book==None or id==None or currency==None:
                raise Exception(
                    "you must call GnuCashBusinessEntity.__init__ "
                    "with either a book, id, and currency, or an existing "
                    "low level swig proxy in the argument instance")
            GnuCashCoreClass.__init__(self, book)
            self.BeginEdit()
            self.SetID(id)
            self.SetCurrency(currency)
            if name != None:
                self.SetName(name)
            self.CommitEdit()
        else:
            GnuCashCoreClass.__init__(self, instance=instance)

class Customer(GnuCashBusinessEntity): pass
                         
class Employee(GnuCashBusinessEntity): pass

class Vendor(GnuCashBusinessEntity): pass

class Job(GnuCashBusinessEntity):
    # override the superclass contructor, as Job doesn't require
    # a currency but it does require an owner
    def __init__(self, book=None, id=None, owner=None, name=None,
                 instance=None):
        if instance == None:
            if book==None or id==None or owner==None:
                raise Exception(
                    "you must call Job.__init__ "
                    "with either a book, id, and owner or an existing "
                    "low level swig proxy in the argument instance")
            GnuCashCoreClass.__init__(self, book)
            self.SetID(id)
            self.SetOwner(owner)
            if name != None:
                self.SetName(name)
        else:
            GnuCashCoreClass.__init__(self, instance=instance)    

class Address(GnuCashCoreClass): pass
    
class BillTerm(GnuCashCoreClass): pass

class TaxTable(GnuCashCoreClass):
    def __init__(self, book=None, name=None, first_entry=None, instance=None):
        if instance == None:
            if book==None or name==None or first_entry==None:
                raise Exception(
                    "you must call TaxTable.__init__  with either a "
                    "book, name, and first_entry, or an existing "
                    "low level swig proxy in the argument instance")
            GnuCashCoreClass.__init__(self, book)
            self.SetName(name)
            self.AddEntry(first_entry)
        else:
            GnuCashCoreClass.__init__(self, instance=instance)

class TaxTableEntry(GnuCashCoreClass):
    def __init__(self, account=None, percent=True, amount=None, instance=None):
        """TaxTableEntry constructor
        
        You must provide an account, or be initizing this with an existing
        swig proxy object via the instance keyword argument.
        
        You may also optionally set the percent keyword argument to False to get
        a fixed value instead of percentage based tax (which is the default, or
        when percent=True).
        
        The tax will be zero percent or zero unless you set the amount keyword
        argument to a GncNumeric value as well.
        """

        if instance == None:
            if account==None:
                raise Exception(
                    "you must call TaxTableEntry.__init__  with either a "
                    "account or an existing "
                    "low level swig proxy in the argument instance")
            GnuCashCoreClass.__init__(self)
            self.SetAccount(account)
            if percent:
                self.SetType(GNC_AMT_TYPE_PERCENT)
            else:
                self.SetType(GNC_AMT_TYPE_VALUE)
            if amount != None:
                self.SetAmount(amount)
        else:
            GnuCashCoreClass.__init__(self, instance=instance)        

class Invoice(GnuCashCoreClass):
    def __init__(self, book=None, id=None, currency=None, owner=None,
                 date_opened=None, instance=None):
        """Invoice Contstructor

        You must provide a book, id, currency and owner
        (Customer, Job, Employee, Vendor) or an existing swig proxy object
        in the keyword argument instance.

        Optionally, you may provide a date the invoice is opened on
        (datetime.date or datetime.datetime), otherwise today's date is used.
        """
        if instance == None:
            if book==None or id==None or currency==None or owner==None:
                raise Exception(
                    "you must call Invoice.__init__ "
                    "with either a book, id, currency and owner, or an existing"
                    "low level swig proxy in the argument instance")
            GnuCashCoreClass.__init__(self, book)
            self.BeginEdit()
            self.SetID(id)
            self.SetCurrency(currency)
            self.SetOwner(owner)
            if date_opened == None:
                date_opened = datetime.date.today()
            self.SetDateOpened(date_opened)
            self.CommitEdit()
        else:
            GnuCashCoreClass.__init__(self, instance=instance)

class Bill(Invoice):
    pass

def decorate_to_return_instance_instead_of_owner(dec_function):
    def new_get_owner_function(self):
        (owner_type, instance) = dec_function(self)
        if owner_type == GNC_OWNER_CUSTOMER:
            return Customer(instance=instance)
        elif owner_type == GNC_OWNER_JOB:
            return Job(instance=instance)
        elif owner_type == GNC_OWNER_EMPLOYEE:
            return Employee(instance=instance)
        elif owner_type == GNC_OWNER_VENDOR:
            return Vendor(instance=instance)
        else:
            return None
    return new_get_owner_function

class Entry(GnuCashCoreClass):
    def __init__(self, book=None, invoice=None, date=None, instance=None):
        """Invoice Entry constructor
        
        You must provide a book or be initizing this with an existing
        swig proxy object via the instance keyword argument.

        The optional invoice argument can be set to a Bill or Invoice
        that you would like to associate the entry with. You might as well
        assign one now, as an Entry can't exist without one, but you can
        always use Invoice.AddEntry or Bill.AddEntry later on.

        By default, the entry will be set to today's date unless you
        override with the date argument.
        """
        if instance == None:
            if book==None:
                raise Exception(
                    "you must call Entry.__init__  with either a "
                    "book or an existing "
                    "low level swig proxy in the argument instance")
            GnuCashCoreClass.__init__(self, book)

            if date == None:
                date = datetime.date.today()
            self.SetDate(date)
            if invoice != None:
                invoice.AddEntry(self)
        else:

            GnuCashCoreClass.__init__(self, instance=instance)

    def test_type(self, invoice):
        if invoice.GetTypeString() == "Invoice" and self.GetInvoice() == None:
            raise Exception("Entry type error. Check that Entry type matches Invoice.")
        if invoice.GetTypeString() == "Bill" and self.GetBill() == None:
            raise Exception("Entry type error. Check that Entry type matches Bill.")


# Owner
GnuCashBusinessEntity.add_methods_with_prefix('gncOwner')

owner_dict = {
                    'GetCustomer' : Customer,
                    'GetVendor' : Vendor,
                    'GetEmployee' : Employee,
                    'GetJob' : Job,
                    'GetAddr' : Address,
                    'GetCurrency' : GncCommodity,
                    'GetEndOwner': GnuCashBusinessEntity,
                    'GetBalanceInCurrency': GncNumeric,
              }
methods_return_instance(GnuCashBusinessEntity, owner_dict)

methods_return_instance_lists(
    GnuCashBusinessEntity, {
        'GetCommoditiesList': GncCommodity
    })

# Customer
Customer.add_constructor_and_methods_with_prefix('gncCustomer', 'Create')

customer_dict = {
                    'GetAddr' : Address,
                    'GetShipAddr' : Address,
                    'GetDiscount' : GncNumeric,
                    'GetCredit' : GncNumeric,
                    'GetTerms' : BillTerm,
                    'GetCurrency' : GncCommodity,
                    'GetTaxTable': TaxTable,
                }
methods_return_instance(Customer, customer_dict)

# Employee
Employee.add_constructor_and_methods_with_prefix('gncEmployee', 'Create')

employee_dict = {
                    'GetBook' : Book,
                    'GetAddr' : Address,
                    'GetWorkday' : GncNumeric,
                    'GetRate' : GncNumeric,
                    'GetCurrency' : GncCommodity
                }
methods_return_instance(Employee, employee_dict)

# Vendor
Vendor.add_constructor_and_methods_with_prefix('gncVendor', 'Create')

vendor_dict =   {
                    'GetAddr' : Address,
                    'GetTerms' : BillTerm,
                    'GetCurrency' : GncCommodity,
                    'GetTaxTable': TaxTable,
                }
methods_return_instance(Vendor, vendor_dict)

# Job
Job.add_constructor_and_methods_with_prefix('gncJob', 'Create')
Job.decorate_functions(
    decorate_to_return_instance_instead_of_owner,
    'GetOwner')

# Address
Address.add_constructor_and_methods_with_prefix('gncAddress', 'Create')

# BillTerm
BillTerm.add_constructor_and_methods_with_prefix('gncBillTerm', 'Create')

billterm_dict = {
                    'LookupByName' : BillTerm,
                    'GetDiscount' : GncNumeric,
                    'GetParent' : BillTerm,
                    'ReturnChild' : BillTerm
                }
methods_return_instance(BillTerm, billterm_dict)

# TaxTable
TaxTable.add_constructor_and_methods_with_prefix('gncTaxTable', 'Create')

taxtable_dict = {
                    'GetParent': TaxTable,
                }
methods_return_instance(TaxTable, taxtable_dict)

# TaxTableEntry
TaxTableEntry.add_constructor_and_methods_with_prefix(
    'gncTaxTableEntry', 'Create')

taxtableentry_dict = {
                         'GetAccount': Account,
                         'GetAmount': GncNumeric,
                     }

# Invoice
Invoice.add_constructor_and_methods_with_prefix('gncInvoice', 'Create')
methods_return_instance_lists(
    Invoice, { 'GetEntries': Entry })

# Bill
Bill.add_methods_with_prefix('gncBill')

invoice_dict = {
                   'GetTerms': BillTerm,
                   'GetCurrency': GncCommodity,
                   'GetToChargeAmount': GncNumeric,
                   'GetPostedLot': GncLot,
                   'GetPostedTxn': Transaction,
                   'GetPostedAcc': Account,
                   'GetTotal': GncNumeric,
                   'GetTotalOf': GncNumeric,
                   'GetTotalSubtotal': GncNumeric,
                   'GetTotalTax': GncNumeric,
                   'PostToAccount': Transaction,
                   'GetBook': Book,
               }
methods_return_instance(Invoice, invoice_dict)
Invoice.decorate_functions(
    decorate_to_return_instance_instead_of_owner,
    'GetOwner', 'GetBillTo')

# Entry
Entry.add_constructor_and_methods_with_prefix('gncEntry', 'Create')

entry_dict = {
                 'GetQuantity': GncNumeric,
                 'GetInvAccount': Account,
                 'GetInvPrice': GncNumeric,
                 'GetInvDiscount': GncNumeric,
                 'GetInvTaxTable': TaxTable,
                 'GetBillAccount': Account,
                 'GetBillPrice': GncNumeric,
                 'GetBillTaxTable': TaxTable,
                 'Copy': Entry,
                 'GetInvoice': Invoice,
                 'GetBill': Invoice
             }
methods_return_instance(Entry, entry_dict)             
Entry.decorate_functions(
    decorate_to_return_instance_instead_of_owner,
    'GetBillTo' )
