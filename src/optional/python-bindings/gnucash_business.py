# gnucash_business.py -- High level python wrapper classes for the business
#                        parts of GnuCash
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

from gnucash_core import \
     GnuCashCoreClass, GncNumeric, GncCommodity, Transaction, \
     Split, Book

class Customer(GnuCashCoreClass): pass

class Employee(GnuCashCoreClass): pass

class Vendor(GnuCashCoreClass): pass

class Address(GnuCashCoreClass): pass
    
class BillTerm(GnuCashCoreClass): pass

# Customer
Customer.add_constructor_and_methods_with_prefix('gncCustomer', 'Create')

customer_dict = {
                    'GetAddr' : Address,
                    'GetShipAddr' : Address,
                    'GetDiscount' : GncNumeric,
                    'GetCredit' : GncNumeric,
                    'GetTerms' : BillTerm,
                    'GetCurrency' : GncCommodity
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
                    'GetCurrency' : GncCommodity
                }
methods_return_instance(Vendor, vendor_dict)

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
