#!/usr/bin/python

import gnucash
import gnucash_simple
import json
import atexit
from flask import Flask, abort, request
import sys
import getopt

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

@app.route('/accounts')
def api_accounts():

	accounts = getAccounts(session.book)

	return accounts

@app.route('/accounts/<guid>')
def api_account(guid):

	account = getAccount(session.book, guid)

	if account is None:
		abort(404)
	else:
		return account

@app.route('/invoices')
def api_invoices():

	is_paid = request.args.get('is_paid', None)
	is_active = request.args.get('is_active', None)

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

	invoices = getInvoices(session.book, is_paid, is_active)

	return invoices

@app.route('/invoices/<id>')
def api_invoice(id):

	invoice = getInvoice(session.book, id)

	if invoice is None:
		abort(404)
	else:
		return invoice


@app.route('/customers')
def api_customers():

	customers = getCustomers(session.book)

	return customers

@app.route('/customers/<id>')
def api_customer(id):

	customer = getCustomer(session.book, id)

	if customer is None:
		abort(404)
	else:
		return customer

def getCustomers(book):

	query = gnucash.Query()
	query.search_for('gncCustomer')
	query.set_book(book)
	customers = []

	for result in query.run():
		customers.append(gnucash_simple.customerToDict(gnucash.gnucash_business.Customer(instance=result)))

	query.destroy()

	return json.dumps(customers)

def getCustomer(book, id):

	customer = book.CustomerLookupByID(id)

	if customer is None:
		return None
	else:
		return json.dumps(gnucash_simple.customerToDict(customer))

def getAccounts(book):

	accounts = gnucash_simple.accountToDict(book.get_root_account())

	return json.dumps(accounts)

def getAccount(book, guid):

	account_guid = gnucash.gnucash_core.GUID()
	gnucash.gnucash_core.GUIDString(guid, account_guid)

	account = gnucash_simple.accountToDict(account_guid.AccountLookup(book))

	if account is None:
		return None
	else:
		return json.dumps(account)


def getInvoices(book, is_paid, is_active):

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

	# return only invoices (1 = invoices)
	pred_data = gnucash.gnucash_core.QueryInt32Predicate(QOF_COMPARE_EQUAL, 1)
	query.add_term([INVOICE_TYPE], pred_data, QOF_QUERY_AND)

	invoices = []

	for result in query.run():
		invoices.append(gnucash_simple.invoiceToDict(gnucash.gnucash_business.Invoice(instance=result)))

	query.destroy()

	return json.dumps(invoices)

def getInvoice(book, id):

	invoice = book.InvoiceLookupByID(id)

	if invoice is None:
		return None
	else:
		#print invoiceToDict(invoice)
		return json.dumps(gnucash_simple.invoiceToDict(invoice))

def shutdown():
	session.end()
	session.destroy()

try:
	options, arguments = getopt.getopt(sys.argv[1:], 'h:', ['host='])
except getopt.GetoptError as err:
	print str(err) # will print something like "option -a not recognized"
	print 'Usage: python-rest.py <connection string>'
	sys.exit(2)

if len(arguments) != 1:
	print 'Usage: python-rest.py <connection string>'
	sys.exit(2)

#set default host for flash
host = '127.0.0.1'

#allow host option to be changed
for option, value in options:
	if option in ("-h", "--host"):
		host = value

#start gnucash session base on connection string argument
session = gnucash.Session(arguments[0], ignore_lock=True)

# register method to close gnucash connection gracefully
atexit.register(shutdown)

# start Flask server
app.run(host=host)
