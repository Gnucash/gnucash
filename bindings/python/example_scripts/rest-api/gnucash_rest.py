#!/usr/bin/python

'''

gnucash_rest.py -- A Flask app which responds to REST requests
with JSON responses

Copyright (C) 2013 Tom Lofts <dev@loftx.co.uk>

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as
published by the Free Software Foundation; either version 2 of
the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, contact:

Free Software Foundation Voice: +1-617-542-5942
51 Franklin Street, Fifth Floor Fax: +1-617-542-2652
Boston, MA 02110-1301, USA gnu@gnu.org

@author Tom Lofts <dev@loftx.co.uk>

'''

import gnucash
import gnucash_simple
import json
import atexit
from flask import Flask, abort, request, Response
import sys
import getopt

from decimal import Decimal

from gnucash.gnucash_business import Vendor, Bill, Entry, GncNumeric, \
    Customer, Invoice, Split, Account, Transaction

import datetime

from gnucash import \
    QOF_QUERY_AND, \
    QOF_QUERY_OR, \
    QOF_QUERY_NAND, \
    QOF_QUERY_NOR, \
    QOF_QUERY_XOR

from gnucash import \
    QOF_STRING_MATCH_NORMAL, \
    QOF_STRING_MATCH_CASEINSENSITIVE

from gnucash import \
    QOF_COMPARE_LT, \
    QOF_COMPARE_LTE, \
    QOF_COMPARE_EQUAL, \
    QOF_COMPARE_GT, \
    QOF_COMPARE_GTE, \
    QOF_COMPARE_NEQ

from gnucash import \
    INVOICE_TYPE

from gnucash import \
    INVOICE_IS_PAID

app = Flask(__name__)
app.debug = True

@app.route('/accounts', methods=['GET', 'POST'])
def api_accounts():

    if request.method == 'GET':

        accounts = getAccounts(session.book)

        return Response(json.dumps(accounts), mimetype='application/json')

    elif request.method == 'POST':

        try:
            account = addAccount(session.books)
        except Error as error:
            return Response(json.dumps({'errors': [{'type' : error.type,
                'message': error.message, 'data': error.data}]}), status=400,
                mimetype='application/json')
        else:
            return Response(json.dumps(account), status=201,
                mimetype='application/json')

    else:
        abort(405)

@app.route('/accounts/<guid>', methods=['GET'])
def api_account(guid):

    account = getAccount(session.book, guid)
    
    if account is None:
        abort(404)
    else:
        return Response(json.dumps(account), mimetype='application/json')

@app.route('/accounts/<guid>/splits', methods=['GET'])
def api_account_splits(guid):

    date_posted_from = request.args.get('date_posted_from', None)
    date_posted_to = request.args.get('date_posted_to', None)

    # check account exists
    account = getAccount(session.book, guid)

    if account is None:
        abort(404)

    splits = getAccountSplits(session.book, guid, date_posted_from,
        date_posted_to)
    
    return Response(json.dumps(splits), mimetype='application/json')


@app.route('/transactions', methods=['POST'])
def api_transactions():

    if request.method == 'POST':
        
        currency = str(request.form.get('currency', ''))
        description = str(request.form.get('description', ''))
        num = str(request.form.get('num', ''))
        date_posted = str(request.form.get('date_posted', ''))

        splitvalue1 = int(request.form.get('splitvalue1', ''))
        splitaccount1 = str(request.form.get('splitaccount1', ''))
        splitvalue2 = int(request.form.get('splitvalue2', ''))
        splitaccount2 = str(request.form.get('splitaccount2', ''))

        splits = [
            {'value': splitvalue1, 'account_guid': splitaccount1},
            {'value': splitvalue2, 'account_guid': splitaccount2}]

        try:
            transaction = addTransaction(session.book, num, description,
                date_posted, currency, splits)
        except Error as error:
            return Response(json.dumps({'errors': [{'type' : error.type,
                'message': error.message, 'data': error.data}]}), status=400,
                mimetype='application/json')
        else:
            return Response(json.dumps(transaction), status=201,
                mimetype='application/json')

    else:
        abort(405)

@app.route('/transactions/<guid>', methods=['GET', 'POST', 'DELETE'])
def api_transaction(guid):

    if request.method == 'GET':

        transaction = getTransaction(session.book, guid)

        if transaction is None:
            abort(404)
        
        return Response(json.dumps(transaction), mimetype='application/json')

    elif request.method == 'POST':

        currency = str(request.form.get('currency', ''))
        description = str(request.form.get('description', ''))
        num = str(request.form.get('num', ''))
        date_posted = str(request.form.get('date_posted', ''))

        splitguid1 = str(request.form.get('splitguid1', ''))
        splitvalue1 = int(request.form.get('splitvalue1', ''))
        splitaccount1 = str(request.form.get('splitaccount1', ''))
        splitguid2 = str(request.form.get('splitguid2', ''))
        splitvalue2 = int(request.form.get('splitvalue2', ''))
        splitaccount2 = str(request.form.get('splitaccount2', ''))

        splits = [
            {'guid': splitguid1,
            'value': splitvalue1,
            'account_guid': splitaccount1},
            {'guid': splitguid2,
            'value': splitvalue2,
            'account_guid': splitaccount2}
        ]

        try:
            transaction = editTransaction(session.book, guid, num, description,
                date_posted, currency, splits)
        except Error as error:
            return Response(json.dumps({'errors': [{'type' : error.type,
                'message': error.message, 'data': error.data}]}), status=400, mimetype='application/json')
        else:
            return Response(json.dumps(transaction), status=200,
                mimetype='application/json')

    elif request.method == 'DELETE':

        deleteTransaction(session.book, guid)

        return Response('', status=200, mimetype='application/json')

    else:
        abort(405)

@app.route('/bills', methods=['GET', 'POST'])
def api_bills():

    if request.method == 'GET':
        
        is_paid = request.args.get('is_paid', None)
        is_active = request.args.get('is_active', None)
        date_opened_to = request.args.get('date_opened_to', None)
        date_opened_from = request.args.get('date_opened_from', None)

        if is_paid == '1':
            is_paid = 1
        elif is_paid == '0':
            is_paid = 0
        else:
            is_paid = None

        if is_active == '1':
            is_active = 1
        elif is_active == '0':
            is_active = 0
        else:
            is_active = None

        bills = getBills(session.book, None, is_paid, is_active,
            date_opened_from, date_opened_to)

        return Response(json.dumps(bills), mimetype='application/json')

    elif request.method == 'POST':

        id = str(request.form.get('id', None))

        if id == '':
            id = None
        elif id != None:
            id = str(id)

        vendor_id = str(request.form.get('vendor_id', ''))
        currency = str(request.form.get('currency', ''))
        date_opened = str(request.form.get('date_opened', ''))
        notes = str(request.form.get('notes', ''))

        try:
            bill = addBill(session.book, id, vendor_id, currency, date_opened,
                notes)
        except Error as error:
            # handle incorrect parameter errors
            return Response(json.dumps({'errors': [{'type' : error.type,
                'message': error.message, 'data': error.data}]}), status=400, mimetype='application/json')
        else:
            return Response(json.dumps(bill), status=201,
                mimetype='application/json')

    else:
        abort(405)

@app.route('/bills/<id>', methods=['GET', 'POST', 'PAY'])
def api_bill(id):

    if request.method == 'GET':

        bill = getBill(session.book, id)
        
        if bill is None:
            abort(404)
        else:
            return Response(json.dumps(bill), mimetype='application/json')

    elif request.method == 'POST':

        vendor_id = str(request.form.get('vendor_id', ''))
        currency = str(request.form.get('currency', ''))
        date_opened = request.form.get('date_opened', None)
        notes = str(request.form.get('notes', ''))
        posted = request.form.get('posted', None)
        posted_account_guid = str(request.form.get('posted_account_guid', ''))
        posted_date = request.form.get('posted_date', '')
        due_date = request.form.get('due_date', '')
        posted_memo = str(request.form.get('posted_memo', ''))
        posted_accumulatesplits = request.form.get('posted_accumulatesplits',
            '')
        posted_autopay = request.form.get('posted_autopay', '')

        if posted == '1':
            posted = 1
        else:
            posted = 0

        if (posted_accumulatesplits == '1'
            or posted_accumulatesplits == 'true'
            or posted_accumulatesplits == 'True'
            or posted_accumulatesplits == True):
            posted_accumulatesplits = True
        else:
            posted_accumulatesplits = False

        if posted_autopay == '1':
            posted_autopay = True
        else:
            posted_autopay = False
        try:
            bill = updateBill(session.book, id, vendor_id, currency,
                date_opened, notes, posted, posted_account_guid, posted_date,
                due_date, posted_memo, posted_accumulatesplits, posted_autopay)
        except Error as error:
            return Response(json.dumps({'errors': [{'type' : error.type,
                'message': error.message, 'data': error.data}]}), status=400,
                mimetype='application/json')
        else:
            return Response(json.dumps(bill), status=200,
                mimetype='application/json')

        if bill is None:
            abort(404)
        else:
            return Response(json.dumps(bill),
                mimetype='application/json')

    elif request.method == 'PAY':
        
        posted_account_guid = str(request.form.get('posted_account_guid', ''))
        transfer_account_guid = str(request.form.get('transfer_account_guid',
            ''))
        payment_date = request.form.get('payment_date', '')
        num = str(request.form.get('num', ''))
        memo = str(request.form.get('posted_memo', ''))
        auto_pay = request.form.get('auto_pay', '')

        try:
            bill = payBill(session.book, id, posted_account_guid,
                transfer_account_guid, payment_date, memo, num, auto_pay)
        except Error as error:
            return Response(json.dumps({'errors': [{'type' : error.type,
                'message': error.message, 'data': error.data}]}), status=400,
            mimetype='application/json')
        else:
            return Response(json.dumps(bill), status=200,
                mimetype='application/json')

    else:
        abort(405)

@app.route('/bills/<id>/entries', methods=['GET', 'POST'])
def api_bill_entries(id):

    bill = getBill(session.book, id)
    
    if bill is None:
        abort(404)
    else:
        if request.method == 'GET':
            return Response(json.dumps(bill['entries']), mimetype='application/json')
        elif request.method == 'POST':

            date = str(request.form.get('date', ''))
            description = str(request.form.get('description', ''))
            account_guid = str(request.form.get('account_guid', ''))
            quantity = str(request.form.get('quantity', ''))
            price = str(request.form.get('price', ''))

            try:
                entry = addBillEntry(session.book, id, date, description,
                    account_guid, quantity, price)
            except Error as error:
                return Response(json.dumps({'errors': [{'type' : error.type,
                    'message': error.message, 'data': error.data}]}),
                    status=400, mimetype='application/json')
            else:
                return Response(json.dumps(entry), status=201,
                    mimetype='application/json')

        else:
            abort(405)

@app.route('/invoices', methods=['GET', 'POST'])
def api_invoices():

    if request.method == 'GET':
        
        is_paid = request.args.get('is_paid', None)
        is_active = request.args.get('is_active', None)
        date_due_to = request.args.get('date_due_to', None)
        date_due_from = request.args.get('date_due_from', None)

        if is_paid == '1':
            is_paid = 1
        elif is_paid == '0':
            is_paid = 0
        else:
            is_paid = None

        if is_active == '1':
            is_active = 1
        elif is_active == '0':
            is_active = 0
        else:
            is_active = None

        invoices = getInvoices(session.book, None, is_paid, is_active,
            date_due_from, date_due_to)

        return Response(json.dumps(invoices), mimetype='application/json')

    elif request.method == 'POST':

        id = str(request.form.get('id', None))

        if id == '':
            id = None
        elif id != None:
            id = str(id)

        customer_id = str(request.form.get('customer_id', ''))
        currency = str(request.form.get('currency', ''))
        date_opened = str(request.form.get('date_opened', ''))
        notes = str(request.form.get('notes', ''))

        try:
            invoice = addInvoice(session.book, id, customer_id, currency,
                date_opened, notes)
        except Error as error:
            return Response(json.dumps({'errors': [{'type' : error.type,
                'message': error.message, 'data': error.data}]}), status=400,
                mimetype='application/json')
        else:
            return Response(json.dumps(invoice), status=201,
                mimetype='application/json')

    else:
        abort(405)

@app.route('/invoices/<id>', methods=['GET', 'POST', 'PAY'])
def api_invoice(id):

    if request.method == 'GET':

        invoice = getInvoice(session.book, id)
        
        if invoice is None:
            abort(404)
        else:
            return Response(json.dumps(invoice), mimetype='application/json')

    elif request.method == 'POST':

        customer_id = str(request.form.get('customer_id', ''))
        currency = str(request.form.get('currency', ''))
        date_opened = request.form.get('date_opened', None)
        notes = str(request.form.get('notes', ''))
        posted = request.form.get('posted', None)
        posted_account_guid = str(request.form.get('posted_account_guid', ''))
        posted_date = request.form.get('posted_date', '')
        due_date = request.form.get('due_date', '')
        posted_memo = str(request.form.get('posted_memo', ''))
        posted_accumulatesplits = request.form.get('posted_accumulatesplits',
            '')
        posted_autopay = request.form.get('posted_autopay', '')

        if posted == '1':
            posted = 1
        else:
            posted = 0

        if (posted_accumulatesplits == '1'
            or posted_accumulatesplits == 'true'
            or posted_accumulatesplits == 'True'
            or posted_accumulatesplits == True):
            posted_accumulatesplits = True
        else:
            posted_accumulatesplits = False

        if posted_autopay == '1':
            posted_autopay = True
        else:
            posted_autopay = False
        try:
            invoice = updateInvoice(session.book, id, customer_id, currency,
                date_opened, notes, posted, posted_account_guid, posted_date,
                due_date, posted_memo, posted_accumulatesplits, posted_autopay)
        except Error as error:
            return Response(json.dumps({'errors': [{'type' : error.type,
                'message': error.message, 'data': error.data}]}), status=400,
                mimetype='application/json')
        else:
            return Response(json.dumps(invoice), status=200,
                mimetype='application/json')

        if invoice is None:
            abort(404)
        else:
            return Response(json.dumps(invoice), mimetype='application/json')

    elif request.method == 'PAY':
        
        posted_account_guid = str(request.form.get('posted_account_guid', ''))
        transfer_account_guid = str(request.form.get('transfer_account_guid',
            ''))
        payment_date = request.form.get('payment_date', '')
        num = str(request.form.get('num', ''))
        memo = str(request.form.get('posted_memo', ''))
        auto_pay = request.form.get('auto_pay', '')

        try:
            invoice = payInvoice(session.book, id, posted_account_guid,
                transfer_account_guid, payment_date, memo, num, auto_pay)
        except Error as error:
            return Response(json.dumps({'errors': [{'type' : error.type,
                'message': error.message, 'data': error.data}]}), status=400,
            mimetype='application/json')
        else:
            return Response(json.dumps(invoice), status=200,
                mimetype='application/json')

    else:
        abort(405)

@app.route('/invoices/<id>/entries', methods=['GET', 'POST'])
def api_invoice_entries(id):

    invoice = getInvoice(session.book, id)
    
    if invoice is None:
        abort(404)
    else:
        if request.method == 'GET':
            return Response(json.dumps(invoice['entries']),
                mimetype='application/json')
        elif request.method == 'POST':

            date = str(request.form.get('date', ''))
            description = str(request.form.get('description', ''))
            account_guid = str(request.form.get('account_guid', ''))
            quantity = str(request.form.get('quantity', ''))
            price = str(request.form.get('price', ''))

            try:
                entry = addEntry(session.book, id, date, description,
                    account_guid, quantity, price)
            except Error as error:
                return Response(json.dumps({'errors': [{'type' : error.type,
                    'message': error.message, 'data': error.data}]}),
                    status=400, mimetype='application/json')
            else:
                return Response(json.dumps(entry), status=201,
                    mimetype='application/json')

        else:
            abort(405)

@app.route('/entries/<guid>', methods=['GET', 'POST', 'DELETE'])
def api_entry(guid):

    entry = getEntry(session.book, guid)
    
    if entry is None:
        abort(404)
    else:
        if request.method == 'GET':
            return Response(json.dumps(entry), mimetype='application/json')
        elif request.method == 'POST':

            date = str(request.form.get('date', ''))
            description = str(request.form.get('description', ''))
            account_guid = str(request.form.get('account_guid', ''))
            quantity = str(request.form.get('quantity', ''))
            price = str(request.form.get('price', ''))

            try:
                entry = updateEntry(session.book, guid, date, description,
                    account_guid, quantity, price)
            except Error as error:
                return Response(json.dumps({'errors': [{'type' : error.type,
                    'message': error.message, 'data': error.data}]}),
                    status=400, mimetype='application/json')
            else:
                return Response(json.dumps(entry), status=200,
                    mimetype='application/json')

        elif request.method == 'DELETE':

            deleteEntry(session.book, guid)

            return Response('', status=201, mimetype='application/json')

        else:
            abort(405)

@app.route('/customers', methods=['GET', 'POST'])
def api_customers(): 

    if request.method == 'GET':
        customers = getCustomers(session.book)
        return Response(json.dumps(customers), mimetype='application/json')
    elif request.method == 'POST':

        id = str(request.form.get('id', None))

        if id == '':
            id = None
        elif id != None:
            id = str(id)

        currency = str(request.form.get('currency', ''))
        name = str(request.form.get('name', ''))
        contact = str(request.form.get('contact', ''))
        address_line_1 = str(request.form.get('address_line_1', ''))
        address_line_2 = str(request.form.get('address_line_2', ''))
        address_line_3 = str(request.form.get('address_line_3', ''))
        address_line_4 = str(request.form.get('address_line_4', ''))
        phone = str(request.form.get('phone', ''))
        fax = str(request.form.get('fax', ''))
        email = str(request.form.get('email', ''))

        try:
            customer = addCustomer(session.book, id, currency, name, contact,
                address_line_1, address_line_2, address_line_3, address_line_4,
                phone, fax, email)
        except Error as error:
            return Response(json.dumps({'errors': [{'type' : error.type,
                'message': error.message, 'data': error.data}]}), status=400,
                mimetype='application/json')
        else:
            return Response(json.dumps(customer), status=201,
                mimetype='application/json')

    else:
        abort(405)

@app.route('/customers/<id>', methods=['GET', 'POST'])
def api_customer(id):

    if request.method == 'GET':

        customer = getCustomer(session.book, id)

        if customer is None:
            abort(404)
        else:
            return Response(json.dumps(customer), mimetype='application/json')

    elif request.method == 'POST':

        id = str(request.form.get('id', None))

        name = str(request.form.get('name', ''))
        contact = str(request.form.get('contact', ''))
        address_line_1 = str(request.form.get('address_line_1', ''))
        address_line_2 = str(request.form.get('address_line_2', ''))
        address_line_3 = str(request.form.get('address_line_3', ''))
        address_line_4 = str(request.form.get('address_line_4', ''))
        phone = str(request.form.get('phone', ''))
        fax = str(request.form.get('fax', ''))
        email = str(request.form.get('email', ''))

        try:
            customer = updateCustomer(session.book, id, name, contact,
                address_line_1, address_line_2, address_line_3, address_line_4,
                phone, fax, email)
        except Error as error:
            if error.type == 'NoCustomer':
                return Response(json.dumps({'errors': [{'type' : error.type,
                    'message': error.message, 'data': error.data}]}),
                    status=404, mimetype='application/json')
            else:
                return Response(json.dumps({'errors': [{'type' : error.type,
                    'message': error.message, 'data': error.data}]}),
                    status=400, mimetype='application/json')
        else:
            return Response(json.dumps(customer), status=200,
                mimetype='application/json')

    else:
        abort(405)

@app.route('/customers/<id>/invoices', methods=['GET'])
def api_customer_invoices(id):

    customer = getCustomer(session.book, id)
    
    if customer is None:
        abort(404)
    
    invoices = getInvoices(session.book, customer['guid'], None, None, None,
        None)
    
    return Response(json.dumps(invoices), mimetype='application/json')

@app.route('/vendors', methods=['GET', 'POST'])
def api_vendors(): 

    if request.method == 'GET':
        vendors = getVendors(session.book)
        return Response(json.dumps(vendors), mimetype='application/json')
    elif request.method == 'POST':

        id = str(request.form.get('id', None))

        if id == '':
            id = None
        elif id != None:
            id = str(id)

        currency = str(request.form.get('currency', ''))
        name = str(request.form.get('name', ''))
        contact = str(request.form.get('contact', ''))
        address_line_1 = str(request.form.get('address_line_1', ''))
        address_line_2 = str(request.form.get('address_line_2', ''))
        address_line_3 = str(request.form.get('address_line_3', ''))
        address_line_4 = str(request.form.get('address_line_4', ''))
        phone = str(request.form.get('phone', ''))
        fax = str(request.form.get('fax', ''))
        email = str(request.form.get('email', ''))

        try:
            vendor = addVendor(session.book, id, currency, name, contact,
                address_line_1, address_line_2, address_line_3, address_line_4,
                phone, fax, email)
        except Error as error:
            return Response(json.dumps({'errors': [{'type' : error.type,
                'message': error.message, 'data': error.data}]}), status=400,
                mimetype='application/json')
        else:
            return Response(json.dumps(vendor), status=201,
                mimetype='application/json')

    else:
        abort(405)

@app.route('/vendors/<id>', methods=['GET', 'POST'])
def api_vendor(id):

    if request.method == 'GET':

        vendor = getVendor(session.book, id)

        if vendor is None:
            abort(404)
        else:
            return Response(json.dumps(vendor), mimetype='application/json')
    else:
        abort(405)

@app.route('/vendors/<id>/bills', methods=['GET'])
def api_vendor_bills(id):

    vendor = getVendor(session.book, id)
    
    if vendor is None:
        abort(404)
    
    bills = getBills(session.book, vendor['guid'], None, None, None, None)
    
    return Response(json.dumps(bills), mimetype='application/json')

def getCustomers(book):

    query = gnucash.Query()
    query.search_for('gncCustomer')
    query.set_book(book)
    customers = []

    for result in query.run():
        customers.append(gnucash_simple.customerToDict(
            gnucash.gnucash_business.Customer(instance=result)))

    query.destroy()

    return customers

def getCustomer(book, id):

    customer = book.CustomerLookupByID(id)

    if customer is None:
        return None
    else:
        return gnucash_simple.customerToDict(customer)

def getVendors(book):

    query = gnucash.Query()
    query.search_for('gncVendor')
    query.set_book(book)
    vendors = []

    for result in query.run():
        vendors.append(gnucash_simple.vendorToDict(
            gnucash.gnucash_business.Vendor(instance=result)))

    query.destroy()

    return vendors

def getVendor(book, id):

    vendor = book.VendorLookupByID(id)

    if vendor is None:
        return None
    else:
        return gnucash_simple.vendorToDict(vendor)

def getAccounts(book):

    accounts = gnucash_simple.accountToDict(book.get_root_account())

    return accounts

def getAccountsFlat(book):

    accounts = gnucash_simple.accountToDict(book.get_root_account())

    flat_accounts = getSubAccounts(accounts)

    for n, account in enumerate(flat_accounts):
        account.pop('subaccounts')

    filtered_flat_account = []

    type_ids = [9]

    for n, account in enumerate(flat_accounts):
        if account['type_id'] in type_ids:
            filtered_flat_account.append(account)
            print account['name'] + ' ' + str(account['type_id'])

    return filtered_flat_account

def getSubAccounts(account):

    flat_accounts = []

    if 'subaccounts' in account.keys():
        for n, subaccount in enumerate(account['subaccounts']):
            flat_accounts.append(subaccount)
            flat_accounts = flat_accounts + getSubAccounts(subaccount)

    return flat_accounts

def getAccount(book, guid):

    account_guid = gnucash.gnucash_core.GUID() 
    gnucash.gnucash_core.GUIDString(guid, account_guid)

    account = account_guid.AccountLookup(book)

    if account is None:
        return None

    account = gnucash_simple.accountToDict(account)

    if account is None:
        return None
    else:
        return account


def getTransaction(book, guid):

    transaction_guid = gnucash.gnucash_core.GUID() 
    gnucash.gnucash_core.GUIDString(guid, transaction_guid)

    transaction = transaction_guid.TransactionLookup(book)

    if transaction is None:
        return None

    transaction = gnucash_simple.transactionToDict(transaction, ['splits'])

    if transaction is None:
        return None
    else:
        return transaction

def getTransactions(book, account_guid, date_posted_from, date_posted_to):

    query = gnucash.Query()

    query.search_for('Trans')
    query.set_book(book)

    transactions = []

    for transaction in query.run():
        transactions.append(gnucash_simple.transactionToDict(
            gnucash.gnucash_business.Transaction(instance=transaction)))

    query.destroy()

    return transactions

def getAccountSplits(book, guid, date_posted_from, date_posted_to):

    account_guid = gnucash.gnucash_core.GUID() 
    gnucash.gnucash_core.GUIDString(guid, account_guid)

    query = gnucash.Query()
    query.search_for('Split')
    query.set_book(book)

    SPLIT_TRANS= 'trans'

    QOF_DATE_MATCH_NORMAL = 1

    TRANS_DATE_POSTED = 'date-posted'

    if date_posted_from != None:
        pred_data = gnucash.gnucash_core.QueryDatePredicate(
            QOF_COMPARE_GTE, QOF_DATE_MATCH_NORMAL, datetime.datetime.strptime(
                date_posted_from, "%Y-%m-%d").date())
        param_list = [SPLIT_TRANS, TRANS_DATE_POSTED]
        query.add_term(param_list, pred_data, QOF_QUERY_AND)

    if date_posted_to != None:
        pred_data = gnucash.gnucash_core.QueryDatePredicate(
            QOF_COMPARE_LTE, QOF_DATE_MATCH_NORMAL, datetime.datetime.strptime(
                date_posted_to, "%Y-%m-%d").date())
        param_list = [SPLIT_TRANS, TRANS_DATE_POSTED]
        query.add_term(param_list, pred_data, QOF_QUERY_AND)
    
    SPLIT_ACCOUNT = 'account'
    QOF_PARAM_GUID = 'guid'

    if guid != None:
        gnucash.gnucash_core.GUIDString(guid, account_guid)
        query.add_guid_match(
            [SPLIT_ACCOUNT, QOF_PARAM_GUID], account_guid, QOF_QUERY_AND)

    splits = []

    for split in query.run():
        splits.append(gnucash_simple.splitToDict(
            gnucash.gnucash_business.Split(instance=split),
            ['account', 'transaction', 'other_split']))

    query.destroy()

    return splits

def getInvoices(book, customer, is_paid, is_active, date_due_from,
    date_due_to):

    query = gnucash.Query()
    query.search_for('gncInvoice')
    query.set_book(book)

    if is_paid == 0:
        query.add_boolean_match([INVOICE_IS_PAID], False, QOF_QUERY_AND)
    elif is_paid == 1:
        query.add_boolean_match([INVOICE_IS_PAID], True, QOF_QUERY_AND)

    # active = JOB_IS_ACTIVE
    if is_active == 0:
        query.add_boolean_match(['active'], False, QOF_QUERY_AND)
    elif is_active == 1:
        query.add_boolean_match(['active'], True, QOF_QUERY_AND)

    QOF_PARAM_GUID = 'guid'
    INVOICE_OWNER = 'owner'

    if customer != None:
        customer_guid = gnucash.gnucash_core.GUID() 
        gnucash.gnucash_core.GUIDString(customer, customer_guid)
        query.add_guid_match(
            [INVOICE_OWNER, QOF_PARAM_GUID], customer_guid, QOF_QUERY_AND)

    if date_due_from != None:
        pred_data = gnucash.gnucash_core.QueryDatePredicate(
            QOF_COMPARE_GTE, 2, datetime.datetime.strptime(
                date_due_from, "%Y-%m-%d").date())
        query.add_term(['date_due'], pred_data, QOF_QUERY_AND)

    if date_due_to != None:
        pred_data = gnucash.gnucash_core.QueryDatePredicate(
            QOF_COMPARE_LTE, 2, datetime.datetime.strptime(
                date_due_to, "%Y-%m-%d").date())
        query.add_term(['date_due'], pred_data, QOF_QUERY_AND)

    # return only invoices (1 = invoices)
    pred_data = gnucash.gnucash_core.QueryInt32Predicate(QOF_COMPARE_EQUAL, 1)
    query.add_term([INVOICE_TYPE], pred_data, QOF_QUERY_AND)

    invoices = []

    for result in query.run():
        invoices.append(gnucash_simple.invoiceToDict(
            gnucash.gnucash_business.Invoice(instance=result)))

    query.destroy()

    return invoices

def getBills(book, customer, is_paid, is_active, date_opened_from,
    date_opened_to):

    query = gnucash.Query()
    query.search_for('gncInvoice')
    query.set_book(book)

    if is_paid == 0:
        query.add_boolean_match([INVOICE_IS_PAID], False, QOF_QUERY_AND)
    elif is_paid == 1:
        query.add_boolean_match([INVOICE_IS_PAID], True, QOF_QUERY_AND)

    # active = JOB_IS_ACTIVE
    if is_active == 0:
        query.add_boolean_match(['active'], False, QOF_QUERY_AND)
    elif is_active == 1:
        query.add_boolean_match(['active'], True, QOF_QUERY_AND)

    QOF_PARAM_GUID = 'guid'
    INVOICE_OWNER = 'owner'

    if customer != None:
        customer_guid = gnucash.gnucash_core.GUID() 
        gnucash.gnucash_core.GUIDString(customer, customer_guid)
        query.add_guid_match(
            [INVOICE_OWNER, QOF_PARAM_GUID], customer_guid, QOF_QUERY_AND)

    if date_opened_from != None:
        pred_data = gnucash.gnucash_core.QueryDatePredicate(
            QOF_COMPARE_GTE, 2, datetime.datetime.strptime(
                date_opened_from, "%Y-%m-%d").date())
        query.add_term(['date_opened'], pred_data, QOF_QUERY_AND)

    if date_opened_to != None:
        pred_data = gnucash.gnucash_core.QueryDatePredicate(
            QOF_COMPARE_LTE, 2, datetime.datetime.strptime(
                date_opened_to, "%Y-%m-%d").date())
        query.add_term(['date_opened'], pred_data, QOF_QUERY_AND)

    # return only bills (2 = bills)
    pred_data = gnucash.gnucash_core.QueryInt32Predicate(QOF_COMPARE_EQUAL, 2)
    query.add_term([INVOICE_TYPE], pred_data, QOF_QUERY_AND)

    bills = []

    for result in query.run():
        bills.append(gnucash_simple.billToDict(
            gnucash.gnucash_business.Bill(instance=result)))

    query.destroy()

    return bills

def getGnuCashInvoice(book ,id):

    # we don't use book.InvoicelLookupByID(id) as this is identical to
    # book.BillLookupByID(id) so can return the same object if they share IDs

    query = gnucash.Query()
    query.search_for('gncInvoice')
    query.set_book(book)

    # return only invoices (1 = invoices)
    pred_data = gnucash.gnucash_core.QueryInt32Predicate(QOF_COMPARE_EQUAL, 1)
    query.add_term([INVOICE_TYPE], pred_data, QOF_QUERY_AND)

    INVOICE_ID = 'id'

    pred_data = gnucash.gnucash_core.QueryStringPredicate(
        QOF_COMPARE_EQUAL, id, QOF_STRING_MATCH_NORMAL, False)
    query.add_term([INVOICE_ID], pred_data, QOF_QUERY_AND)

    invoice = None

    for result in query.run():
        invoice = gnucash.gnucash_business.Invoice(instance=result)

    query.destroy()

    return invoice

def getGnuCashBill(book ,id):

    # we don't use book.InvoicelLookupByID(id) as this is identical to
    # book.BillLookupByID(id) so can return the same object if they share IDs

    query = gnucash.Query()
    query.search_for('gncInvoice')
    query.set_book(book)

    # return only bills (2 = bills)
    pred_data = gnucash.gnucash_core.QueryInt32Predicate(QOF_COMPARE_EQUAL, 2)
    query.add_term([INVOICE_TYPE], pred_data, QOF_QUERY_AND)

    INVOICE_ID = 'id'

    pred_data = gnucash.gnucash_core.QueryStringPredicate(
        QOF_COMPARE_EQUAL, id, QOF_STRING_MATCH_NORMAL, False)
    query.add_term([INVOICE_ID], pred_data, QOF_QUERY_AND)

    bill = None

    for result in query.run():
        bill = gnucash.gnucash_business.Bill(instance=result)

    query.destroy()

    return bill

def getInvoice(book, id):

    return gnucash_simple.invoiceToDict(getGnuCashInvoice(book, id))

def payInvoice(book, id, posted_account_guid, transfer_account_guid,
    payment_date, memo, num, auto_pay):

    invoice = getGnuCashInvoice(book, id)
    
    account_guid2 = gnucash.gnucash_core.GUID() 
    gnucash.gnucash_core.GUIDString(transfer_account_guid, account_guid2)

    xfer_acc = account_guid2.AccountLookup(session.book)

    invoice.ApplyPayment(None, xfer_acc, invoice.GetTotal(), GncNumeric(0),
        datetime.datetime.strptime(payment_date, '%Y-%m-%d'), memo, num)

    return gnucash_simple.invoiceToDict(invoice)    

def payBill(book, id, posted_account_guid, transfer_account_guid, payment_date,
    memo, num, auto_pay):

    bill = getGnuCashBill(book, id)

    account_guid = gnucash.gnucash_core.GUID() 
    gnucash.gnucash_core.GUIDString(transfer_account_guid, account_guid)

    xfer_acc = account_guid.AccountLookup(session.book)

    # We pay the negitive total as the bill as this seemed to cause issues
    # with the split not being set correctly and not being marked as paid
    bill.ApplyPayment(None, xfer_acc, bill.GetTotal().neg(), GncNumeric(0),
        datetime.datetime.strptime(payment_date, '%Y-%m-%d'), memo, num)

    return gnucash_simple.billToDict(bill)

def getBill(book, id):

    return gnucash_simple.billToDict(getGnuCashBill(book, id))

def addVendor(book, id, currency_mnumonic, name, contact, address_line_1,
    address_line_2, address_line_3, address_line_4, phone, fax, email):

    if name == '':
        raise Error('NoVendorName', 'A name must be entered for this company',
            {'field': 'name'})

    if (address_line_1 == ''
        and address_line_2 == ''
        and address_line_3 == ''
        and address_line_4 == ''):
        raise Error('NoVendorAddress',
            'An address must be entered for this company',
            {'field': 'address'})

    commod_table = book.get_table()
    currency = commod_table.lookup('CURRENCY', currency_mnumonic)

    if currency is None:
        raise Error('InvalidVendorCurrency',
            'A valid currency must be supplied for this vendor',
            {'field': 'currency'})

    if id is None:
        id = book.VendorNextID()

    vendor = Vendor(session.book, id, currency, name)

    address = vendor.GetAddr()
    address.SetName(contact)
    address.SetAddr1(address_line_1)
    address.SetAddr2(address_line_2)
    address.SetAddr3(address_line_3)
    address.SetAddr4(address_line_4)
    address.SetPhone(phone)
    address.SetFax(fax)
    address.SetEmail(email)

    return gnucash_simple.vendorToDict(vendor)

def addCustomer(book, id, currency_mnumonic, name, contact, address_line_1,
    address_line_2, address_line_3, address_line_4, phone, fax, email):

    if name == '':
        raise Error('NoCustomerName',
            'A name must be entered for this company', {'field': 'name'})

    if (address_line_1 == ''
        and address_line_2 == ''
        and address_line_3 == ''
        and address_line_4 == ''):
        raise Error('NoCustomerAddress',
            'An address must be entered for this company',
            {'field': 'address'})

    commod_table = book.get_table()
    currency = commod_table.lookup('CURRENCY', currency_mnumonic)

    if currency is None:
        raise Error('InvalidCustomerCurrency',
            'A valid currency must be supplied for this customer',
            {'field': 'currency'})

    if id is None:
        id = book.CustomerNextID()

    customer = Customer(session.book, id, currency, name)

    address = customer.GetAddr()
    address.SetName(contact)
    address.SetAddr1(address_line_1)
    address.SetAddr2(address_line_2)
    address.SetAddr3(address_line_3)
    address.SetAddr4(address_line_4)
    address.SetPhone(phone)
    address.SetFax(fax)
    address.SetEmail(email)

    return gnucash_simple.customerToDict(customer)

def updateCustomer(book, id, name, contact, address_line_1, address_line_2,
    address_line_3, address_line_4, phone, fax, email):

    customer = book.CustomerLookupByID(id)

    if customer is None:
        raise Error('NoCustomer', 'A customer with this ID does not exist',
            {'field': 'id'})

    if name == '':
        raise Error('NoCustomerName',
            'A name must be entered for this company', {'field': 'name'})

    if (address_line_1 == ''
        and address_line_2 == ''
        and address_line_3 == ''
        and address_line_4 == ''):
        raise Error('NoCustomerAddress',
            'An address must be entered for this company',
            {'field': 'address'})

    customer.SetName(name)

    address = customer.GetAddr()
    address.SetName(contact)
    address.SetAddr1(address_line_1)
    address.SetAddr2(address_line_2)
    address.SetAddr3(address_line_3)
    address.SetAddr4(address_line_4)
    address.SetPhone(phone)
    address.SetFax(fax)
    address.SetEmail(email)

    return gnucash_simple.customerToDict(customer)

def addInvoice(book, id, customer_id, currency_mnumonic, date_opened, notes):

    customer = book.CustomerLookupByID(customer_id)

    if customer is None:
        raise Error('NoCustomer',
            'A customer with this ID does not exist', {'field': 'id'})

    if id is None:
        id = book.InvoiceNextID(customer)

    try:
        date_opened = datetime.datetime.strptime(date_opened, "%Y-%m-%d")
    except ValueError:
        raise Error('InvalidDateOpened',
            'The date opened must be provided in the form YYYY-MM-DD',
            {'field': 'date_opened'})

    if currency_mnumonic is None:
        currency_mnumonic = customer.GetCurrency().get_mnemonic()

    commod_table = book.get_table()
    currency = commod_table.lookup('CURRENCY', currency_mnumonic)

    if currency is None:
        raise Error('InvalidCustomerCurrency',
            'A valid currency must be supplied for this customer',
            {'field': 'currency'})

    invoice = Invoice(book, id, currency, customer, date_opened.date())

    invoice.SetNotes(notes)

    return gnucash_simple.invoiceToDict(invoice)

def updateInvoice(book, id, customer_id, currency_mnumonic, date_opened,
    notes, posted, posted_account_guid, posted_date, due_date, posted_memo,
    posted_accumulatesplits, posted_autopay):

    invoice = getGnuCashInvoice(book, id)

    if invoice is None:
        raise Error('NoInvoice',
            'An invoice with this ID does not exist',
            {'field': 'id'})

    customer = book.CustomerLookupByID(customer_id)

    if customer is None:
        raise Error('NoCustomer', 'A customer with this ID does not exist',
            {'field': 'customer_id'})

    try:
        date_opened = datetime.datetime.strptime(date_opened, "%Y-%m-%d")
    except ValueError:
        raise Error('InvalidDateOpened',
            'The date opened must be provided in the form YYYY-MM-DD',
            {'field': 'date_opened'})

    if posted_date == '':
        if posted == 1:
            raise Error('NoDatePosted',
                'The date posted must be supplied when posted=1',
                {'field': 'date_posted'})
    else:
        try:
            posted_date = datetime.datetime.strptime(posted_date, "%Y-%m-%d")
        except ValueError:
            raise Error('InvalidDatePosted',
                'The date posted must be provided in the form YYYY-MM-DD',
                {'field': 'posted_date'})

    if due_date == '':
        if posted == 1:
            raise Error('NoDatePosted',
                'The due date must be supplied when posted=1',
                {'field': 'date_posted'})
    else:
        try:
            due_date = datetime.datetime.strptime(due_date, "%Y-%m-%d")
        except ValueError:
            raise Error('InvalidDatePosted',
                'The due date must be provided in the form YYYY-MM-DD',
                {'field': 'due_date'})

    if posted_account_guid == '':
        if posted == 1:
            raise Error('NoPostedAccountGuid',
                'The posted account GUID must be supplied when posted=1',
                {'field': 'posted_account_guid'})
    else:
        guid = gnucash.gnucash_core.GUID() 
        gnucash.gnucash_core.GUIDString(posted_account_guid, guid)

        posted_account = guid.AccountLookup(book)

        if posted_account is None:
            raise Error('NoAccount',
                'No account exists with the posted account GUID',
                {'field': 'posted_account_guid'})

    invoice.SetOwner(customer)
    invoice.SetDateOpened(date_opened)
    invoice.SetNotes(notes)

    # post if currently unposted and posted=1
    if (invoice.GetDatePosted().strftime('%Y-%m-%d') == '1970-01-01'
        and posted == 1):
        invoice.PostToAccount(posted_account, posted_date, due_date,
            posted_memo, posted_accumulatesplits, posted_autopay)

    return gnucash_simple.invoiceToDict(invoice)

def updateBill(book, id, vendor_id, currency_mnumonic, date_opened, notes,
    posted, posted_account_guid, posted_date, due_date, posted_memo,
    posted_accumulatesplits, posted_autopay):

    bill = getGnuCashBill(book, id)

    if bill is None:
        raise Error('NoBill', 'A bill with this ID does not exist',
            {'field': 'id'})

    vendor = book.VendorLookupByID(vendor_id)

    if vendor is None:
        raise Error('NoVendor',
            'A vendor with this ID does not exist',
            {'field': 'vendor_id'})

    try:
        date_opened = datetime.datetime.strptime(date_opened, "%Y-%m-%d")
    except ValueError:
        raise Error('InvalidDateOpened',
            'The date opened must be provided in the form YYYY-MM-DD',
            {'field': 'date_opened'})

    if posted_date == '':
        if posted == 1:
            raise Error('NoDatePosted',
                'The date posted must be supplied when posted=1',
                {'field': 'date_posted'})
    else:
        try:
            posted_date = datetime.datetime.strptime(posted_date, "%Y-%m-%d")
        except ValueError:
            raise Error('InvalidDatePosted',
                'The date posted must be provided in the form YYYY-MM-DD',
                {'field': 'posted_date'})

    if due_date == '':
        if posted == 1:
            raise Error('NoDatePosted',
                'The due date must be supplied when posted=1',
                {'field': 'date_posted'})
    else:
        try:
            due_date = datetime.datetime.strptime(due_date, "%Y-%m-%d")
        except ValueError:
            raise Error('InvalidDatePosted',
                'The due date must be provided in the form YYYY-MM-DD',
                {'field': 'due_date'})

    if posted_account_guid == '':
        if posted == 1:
            raise Error('NoPostedAccountGuid',
                'The posted account GUID must be supplied when posted=1',
                {'field': 'posted_account_guid'})
    else:
        guid = gnucash.gnucash_core.GUID() 
        gnucash.gnucash_core.GUIDString(posted_account_guid, guid)

        posted_account = guid.AccountLookup(book)

        if posted_account is None:
            raise Error('NoAccount',
                'No account exists with the posted account GUID',
                {'field': 'posted_account_guid'})

    bill.SetOwner(vendor)
    bill.SetDateOpened(date_opened)
    bill.SetNotes(notes)

    # post if currently unposted and posted=1
    if bill.GetDatePosted().strftime('%Y-%m-%d') == '1970-01-01' and posted == 1:
        bill.PostToAccount(posted_account, posted_date, due_date, posted_memo,
            posted_accumulatesplits, posted_autopay)

    return gnucash_simple.billToDict(bill)

def addEntry(book, invoice_id, date, description, account_guid, quantity, price):

    invoice = getGnuCashInvoice(book, invoice_id)

    if invoice is None:
        raise Error('NoInvoice',
            'No invoice exists with this ID', {'field': 'invoice_id'})

    try:
        date = datetime.datetime.strptime(date, "%Y-%m-%d")
    except ValueError:
        raise Error('InvalidDateOpened',
            'The date opened must be provided in the form YYYY-MM-DD',
            {'field': 'date'})

    guid = gnucash.gnucash_core.GUID() 
    gnucash.gnucash_core.GUIDString(account_guid, guid)

    account = guid.AccountLookup(book)

    if account is None:
        raise Error('NoAccount', 'No account exists with this GUID',
            {'field': 'account_guid'})

    try:
        quantity = Decimal(quantity).quantize(Decimal('.01'))
    except ArithmeticError:
        raise Error('InvalidQuantity', 'This quantity is not valid',
            {'field': 'quantity'})

    try:
        price = Decimal(price).quantize(Decimal('.01'))
    except ArithmeticError:
        raise Error('InvalidPrice', 'This price is not valid',
            {'field': 'price'})

    entry = Entry(book, invoice, date.date())
    entry.SetDateEntered(datetime.datetime.now())
    entry.SetDescription(description)
    entry.SetInvAccount(account)
    entry.SetQuantity(gnc_numeric_from_decimal(quantity))
    entry.SetInvPrice(gnc_numeric_from_decimal(price))

    return gnucash_simple.entryToDict(entry)

def addBillEntry(book, bill_id, date, description, account_guid, quantity,
    price):

    bill = getGnuCashBill(book,bill_id)

    if bill is None:
        raise Error('NoBill', 'No bill exists with this ID',
            {'field': 'bill_id'})

    try:
        date = datetime.datetime.strptime(date, "%Y-%m-%d")
    except ValueError:
        raise Error('InvalidDateOpened',
            'The date opened must be provided in the form YYYY-MM-DD',
            {'field': 'date'})

    guid = gnucash.gnucash_core.GUID() 
    gnucash.gnucash_core.GUIDString(account_guid, guid)

    account = guid.AccountLookup(book)

    if account is None:
        raise Error('NoAccount', 'No account exists with this GUID',
            {'field': 'account_guid'})

    try:
        quantity = Decimal(quantity).quantize(Decimal('.01'))
    except ArithmeticError:
        raise Error('InvalidQuantity', 'This quantity is not valid',
            {'field': 'quantity'})

    try:
        price = Decimal(price).quantize(Decimal('.01'))
    except ArithmeticError:
        raise Error('InvalidPrice', 'This price is not valid',
            {'field': 'price'})
    
    entry = Entry(book, bill, date.date())
    entry.SetDateEntered(datetime.datetime.now())
    entry.SetDescription(description)
    entry.SetBillAccount(account)
    entry.SetQuantity(gnc_numeric_from_decimal(quantity))
    entry.SetBillPrice(gnc_numeric_from_decimal(price))

    return gnucash_simple.entryToDict(entry)

def getEntry(book, entry_guid):

    guid = gnucash.gnucash_core.GUID() 
    gnucash.gnucash_core.GUIDString(entry_guid, guid)

    entry = book.EntryLookup(guid)

    if entry is None:
        return None
    else:
        return gnucash_simple.entryToDict(entry)

def updateEntry(book, entry_guid, date, description, account_guid, quantity,
    price):

    guid = gnucash.gnucash_core.GUID() 
    gnucash.gnucash_core.GUIDString(entry_guid, guid)

    entry = book.EntryLookup(guid)

    if entry is None:
        raise Error('NoEntry', 'No entry exists with this GUID',
            {'field': 'entry_guid'})

    try:
        date = datetime.datetime.strptime(date, "%Y-%m-%d")
    except ValueError:
        raise Error('InvalidDateOpened',
            'The date opened must be provided in the form YYYY-MM-DD',
            {'field': 'date'})
 
    gnucash.gnucash_core.GUIDString(account_guid, guid)

    account = guid.AccountLookup(book)

    if account is None:
        raise Error('NoAccount', 'No account exists with this GUID',
            {'field': 'account_guid'})

    entry.SetDate(date.date())
    entry.SetDateEntered(datetime.datetime.now())
    entry.SetDescription(description)
    entry.SetInvAccount(account)
    entry.SetQuantity(
        gnc_numeric_from_decimal(Decimal(quantity).quantize(Decimal('.01'))))
    entry.SetInvPrice(
        gnc_numeric_from_decimal(Decimal(price).quantize(Decimal('.01'))))

    return gnucash_simple.entryToDict(entry)

def deleteEntry(book, entry_guid):

    guid = gnucash.gnucash_core.GUID() 
    gnucash.gnucash_core.GUIDString(entry_guid, guid)

    entry = book.EntryLookup(guid)

    invoice = entry.GetInvoice()
    bill = entry.GetBill()

    if invoice != None and entry != None:
        invoice.RemoveEntry(entry)
    elif bill != None and entry != None:
        bill.RemoveEntry(entry)

    if entry != None:
        entry.Destroy()

def deleteTransaction(book, transaction_guid):

    guid = gnucash.gnucash_core.GUID() 
    gnucash.gnucash_core.GUIDString(transaction_guid, guid)

    transaction = guid.TransLookup(book)

    if transaction != None :
        transaction.Destroy()

def addBill(book, id, vendor_id, currency_mnumonic, date_opened, notes):

    vendor = book.VendorLookupByID(vendor_id)

    if vendor is None:
        raise Error('NoVendor', 'A vendor with this ID does not exist',
            {'field': 'id'})

    if id is None:
        id = book.BillNextID(vendor)

    try:
        date_opened = datetime.datetime.strptime(date_opened, "%Y-%m-%d")
    except ValueError:
        raise Error('InvalidVendorDateOpened',
            'The date opened must be provided in the form YYYY-MM-DD',
            {'field': 'date_opened'})

    if currency_mnumonic is None:
        currency_mnumonic = vendor.GetCurrency().get_mnemonic()

    commod_table = book.get_table()
    currency = commod_table.lookup('CURRENCY', currency_mnumonic)

    if currency is None:
        raise Error('InvalidVendorCurrency',
            'A valid currency must be supplied for this vendor',
            {'field': 'currency'})

    bill = Bill(book, id, currency, vendor, date_opened.date())

    bill.SetNotes(notes)

    return gnucash_simple.billToDict(bill)

def addAccount(book, name, currency_mnumonic, account_guid):

    from gnucash.gnucash_core_c import \
    ACCT_TYPE_ASSET, ACCT_TYPE_RECEIVABLE, ACCT_TYPE_INCOME, \
    GNC_OWNER_CUSTOMER, ACCT_TYPE_LIABILITY

    root_account = book.get_root_account()

    commod_table = book.get_table()
    currency = commod_table.lookup('CURRENCY', currency_mnumonic)

    if currency is None:
        raise Error('InvalidCustomerCurrency',
            'A valid currency must be supplied for this customer',
            {'field': 'currency'})

    account = Account(book)
    root_account.append_child(root_account)
    account.SetName(name)
    account.SetType(ACCT_TYPE_ASSET)
    account.SetCommodity(currency)

def addTransaction(book, num, description, date_posted, currency_mnumonic, splits):

    transaction = Transaction(book)

    transaction.BeginEdit()

    commod_table = book.get_table()
    currency = commod_table.lookup('CURRENCY', currency_mnumonic)

    if currency is None:
        raise Error('InvalidTransactionCurrency',
            'A valid currency must be supplied for this transaction',
            {'field': 'currency'})

    try:
        date_posted = datetime.datetime.strptime(date_posted, "%Y-%m-%d")
    except ValueError:
        raise Error('InvalidDatePosted',
            'The date posted must be provided in the form YYYY-MM-DD',
            {'field': 'date_posted'})


    for split_values in splits:
        account_guid = gnucash.gnucash_core.GUID() 
        gnucash.gnucash_core.GUIDString(split_values['account_guid'], account_guid)

        account = account_guid.AccountLookup(book)

        if account is None:
            raise Error('InvalidSplitAccount',
                'A valid account must be supplied for this split',
                {'field': 'account'})

        split = Split(book)
        split.SetValue(GncNumeric(split_values['value'], 100))
        split.SetAccount(account)
        split.SetParent(transaction)

    transaction.SetCurrency(currency)
    transaction.SetDescription(description)
    transaction.SetNum(num)

    transaction.SetDatePostedTS(date_posted)

    transaction.CommitEdit()

    return gnucash_simple.transactionToDict(transaction, ['splits'])

def getTransaction(book, transaction_guid):

    guid = gnucash.gnucash_core.GUID() 
    gnucash.gnucash_core.GUIDString(transaction_guid, guid)

    transaction = guid.TransLookup(book)

    if transaction is None:
        return None
    else:
        return gnucash_simple.transactionToDict(transaction, ['splits'])

def editTransaction(book, transaction_guid, num, description, date_posted,
    currency_mnumonic, splits):

    guid = gnucash.gnucash_core.GUID() 
    gnucash.gnucash_core.GUIDString(transaction_guid, guid)

    transaction = guid.TransLookup(book)

    if transaction is None:
        raise Error('NoCustomer',
            'A transaction with this GUID does not exist',
            {'field': 'guid'})

    transaction.BeginEdit()

    commod_table = book.get_table()
    currency = commod_table.lookup('CURRENCY', currency_mnumonic)

    if currency is None:
        raise Error('InvalidTransactionCurrency',
            'A valid currency must be supplied for this transaction',
            {'field': 'currency'})


    try:
        date_posted = datetime.datetime.strptime(date_posted, "%Y-%m-%d")
    except ValueError:
        raise Error('InvalidDatePosted',
            'The date posted must be provided in the form YYYY-MM-DD',
            {'field': 'date_posted'})

    for split_values in splits:

        split_guid = gnucash.gnucash_core.GUID() 
        gnucash.gnucash_core.GUIDString(split_values['guid'], split_guid)

        split = split_guid.SplitLookup(book)

        if split is None:
            raise Error('InvalidSplitGuid',
                'A valid guid must be supplied for this split',
                {'field': 'guid'})

        account_guid = gnucash.gnucash_core.GUID() 
        gnucash.gnucash_core.GUIDString(
            split_values['account_guid'], account_guid)

        account = account_guid.AccountLookup(book)

        if account is None:
            raise Error('InvalidSplitAccount',
                'A valid account must be supplied for this split',
                {'field': 'account'})

        split.SetValue(GncNumeric(split_values['value'], 100))
        split.SetAccount(account)
        split.SetParent(transaction)

    transaction.SetCurrency(currency)
    transaction.SetDescription(description)
    transaction.SetNum(num)

    transaction.SetDatePostedTS(date_posted)

    transaction.CommitEdit()

    return gnucash_simple.transactionToDict(transaction, ['splits'])

def gnc_numeric_from_decimal(decimal_value):
    sign, digits, exponent = decimal_value.as_tuple()

    # convert decimal digits to a fractional numerator
    # equivlent to
    # numerator = int(''.join(digits))
    # but without the wated conversion to string and back,
    # this is probably the same algorithm int() uses
    numerator = 0
    TEN = int(Decimal(0).radix()) # this is always 10
    numerator_place_value = 1
    # add each digit to the final value multiplied by the place value
    # from least significant to most sigificant
    for i in xrange(len(digits)-1,-1,-1):
        numerator += digits[i] * numerator_place_value
        numerator_place_value *= TEN

    if decimal_value.is_signed():
        numerator = -numerator

    # if the exponent is negative, we use it to set the denominator
    if exponent < 0 :
        denominator = TEN ** (-exponent)
    # if the exponent isn't negative, we bump up the numerator
    # and set the denominator to 1
    else:
        numerator *= TEN ** exponent
        denominator = 1

    return GncNumeric(numerator, denominator)

def shutdown():
    session.save()
    session.end()
    session.destroy()
    print 'Shutdown'

class Error(Exception):
    """Base class for exceptions in this module."""
    def __init__(self, type, message, data):
        self.type = type
        self.message = message
        self.data = data

try:
    options, arguments = getopt.getopt(sys.argv[1:], 'nh:', ['host=', 'new='])
except getopt.GetoptError as err:
    print str(err) # will print something like "option -a not recognized"
    print 'Usage: python-rest.py <connection string>'
    sys.exit(2)

if len(arguments) == 0:
    print 'Usage: python-rest.py <connection string>'
    sys.exit(2)

#set default host for Flask
host = '127.0.0.1'

#allow host option to be changed
for option, value in options:
    if option in ("-h", "--host"):
        host = value

is_new = False

# allow a new database to be used
for option, value in options:
    if option in ("-n", "--new"):
        is_new = True


#start gnucash session base on connection string argument
if is_new:
    session = gnucash.Session(arguments[0], is_new=True)

    # seem to get errors if we use the session directly, so save it and
    #destroy it so it's no longer new

    session.save()
    session.end()
    session.destroy()

session = gnucash.Session(arguments[0], ignore_lock=True)

# register method to close gnucash connection gracefully
atexit.register(shutdown)

app.debug = False

# log to console
if not app.debug:
    import logging
    from logging import StreamHandler
    stream_handler = StreamHandler()
    stream_handler.setLevel(logging.ERROR)
    app.logger.addHandler(stream_handler)

# start Flask server
app.run(host=host)
