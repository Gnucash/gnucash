#!/usr/bin/env python

# simple_business_create.py -- Set up a set of books for business feature use
#
# Copyright (C) 2010 ParIT Worker Co-operative <transparency@parit.ca>
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

# Creates a new book file (or *overwrites* an existing one) that has elements
# in it for business use -- intended as a demonstration program.
# Syntax:
# gnucash-env python simple_business_create.py \
#                       sqlite3:///home/blah/blah.gnucash
#
# Specificically, this sets up a simple tree, creates a customer, job,
# employee and vendor, creates an unposted invoice for each,
# and posts the customer invoice with a few entries and a tax table.
#
# argv[1] should be the path the new or to overwrite gnucash file/database
# for a file, simply pass the pathname prefixed with the requested data format
# like:
#   xml:///home/blah/blah.gnucash
#   sqlite3:///home/blah/blah.gnucash
# Paths can also be relative, for example:
#   xml://from-here/to/there/blah.gnucash
# For a database you can use these forms:
#   mysql://user:password@host/dbname
#   postgres://user:password@host[:port]/dbname (the port is optional)
#
# You may also want to look at simple_invoice_insert.py

##  @file
#   @brief Set up a set of books for business feature use
#   @author Mark Jenkins, ParIT Worker Co-operative <mark@parit.ca>
#   @ingroup python_bindings_examples

from os.path import abspath
from sys import argv, exit
import datetime
from datetime import timedelta
from gnucash import Session, Account, GncNumeric
from gnucash.gnucash_business import Customer, Employee, Vendor, Job, \
    Address, Invoice, Entry, TaxTable, TaxTableEntry, GNC_AMT_TYPE_PERCENT, \
    GNC_DISC_PRETAX    
from gnucash.gnucash_core_c import \
    ACCT_TYPE_ASSET, ACCT_TYPE_RECEIVABLE, ACCT_TYPE_INCOME, \
    GNC_OWNER_CUSTOMER, ACCT_TYPE_LIABILITY

if len(argv) < 2:
    print 'not enough parameters'
    print 'usage: simple_business_create.py {new_book_url}'
    print 'example:'
    print "gnucash-env python simple_business_create.py sqlite3:///home/blah/blah.gnucash"
    exit()
    

try:
    s = Session(argv[1], is_new=True)

    book = s.book
    root = book.get_root_account()
    commod_table = book.get_table()
    CAD = commod_table.lookup('CURRENCY', 'CAD')

    a = Account(book)
    root.append_child(a)
    a.SetName('Assets')
    a.SetType(ACCT_TYPE_ASSET)
    a.SetCommodity(CAD)

    a2 = Account(book)
    a.append_child(a2)
    a2.SetName('Receivables')
    a2.SetType(ACCT_TYPE_RECEIVABLE)
    a2.SetCommodity(CAD)

    a3 = Account(book)
    root.append_child(a3)
    a3.SetName('Income')
    a3.SetType(ACCT_TYPE_INCOME)
    a3.SetCommodity(CAD)

    a4 = Account(book)
    root.append_child(a4)
    a4.SetName('Liabilities')
    a4.SetType(ACCT_TYPE_LIABILITY)
    a4.SetCommodity(CAD)

    a5 = Account(book)
    a4.append_child(a5)
    a5.SetName('Tax payable')
    a5.SetType(ACCT_TYPE_LIABILITY)
    a5.SetCommodity(CAD)

    a6 = Account(book)
    a.append_child(a6)
    a6.SetName('Bank')
    a6.SetType(ACCT_TYPE_ASSET)
    a6.SetCommodity(CAD)

    # name isn't required, ID and currency are
    new_customer = Customer(book, "1", CAD, "Bill & Bob Industries")

    # not required, but a good idea because the GUI insists on basic address info
    address = new_customer.GetAddr()
    address.SetName("Bill & Bob")
    address.SetAddr1("201 Nowhere street")

    new_employee = Employee(book, "2", CAD, "Reliable employee")

    new_vendor = Vendor(book, "3", CAD, "Dependable vendor")

    new_job = Job(book, "4", new_vendor, "Good clean, fun")

    # 7% tax
    tax_table = TaxTable(book, "good tax",
                         TaxTableEntry(a5, True, GncNumeric(700000, 100000) ) )


    invoice_customer = Invoice(book, "5", CAD, new_customer)
    customer_extract = invoice_customer.GetOwner()
    assert( isinstance(customer_extract, Customer) )
    assert( customer_extract.GetName() == new_customer.GetName() )

    invoice_employee = Invoice(book, "6", CAD, new_employee)
    employee_extract = invoice_employee.GetOwner()
    assert( isinstance(employee_extract, Employee) )
    assert( employee_extract.GetName() == new_employee.GetName() )

    invoice_vendor = Invoice(book, "7", CAD, new_vendor)
    vendor_extract = invoice_vendor.GetOwner()
    assert( isinstance(vendor_extract, Vendor) )
    assert( vendor_extract.GetName() == new_vendor.GetName() )

    invoice_job = Invoice(book, "8", CAD, new_job)
    job_extract = invoice_job.GetOwner()
    assert( isinstance(job_extract, Job) )
    assert( job_extract.GetName() == new_job.GetName() )


    invoice_entry = Entry(book, invoice_customer)
    invoice_entry.SetInvTaxTable(tax_table)
    invoice_entry.SetInvTaxIncluded(False)
    invoice_entry.SetDescription("excellent product")
    invoice_entry.SetQuantity( GncNumeric(1) )
    invoice_entry.SetInvAccount(a3)
    invoice_entry.SetInvPrice(GncNumeric(1) )
    invoice_entry.SetDateEntered(datetime.datetime.now())

    invoice_customer.PostToAccount(a2, datetime.date.today(), datetime.date.today(),
                                   "the memo", True, False)

    new_customer.ApplyPayment(None, None, a2, a6, GncNumeric(100,100),
                              GncNumeric(1), datetime.date.today(), "", "", True)

    invoice_customer.ApplyPayment(None, a6, GncNumeric(7,100),
                                  GncNumeric(1), datetime.date.today(), "", "")

    vendor_bill_returns = book.BillLoookupByID("7")
    assert( vendor_bill_returns.GetID() == "7" )
    vendor_extract = vendor_bill_returns.GetOwner()
    assert( vendor_extract.GetName() == new_vendor.GetName() )
    customer_invoice_returns = book.InvoiceLookupByID("5")
    assert( customer_invoice_returns.GetID() == "5" )
    customer_returns = book.CustomerLookupByID("1")
    assert( customer_returns.GetName() == new_customer.GetName() )

    s.save()

    s.end()
except:
    if "s" in locals():
        s.end()
    raise
