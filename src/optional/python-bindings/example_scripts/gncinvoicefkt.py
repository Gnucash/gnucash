#!/usr/bin/env python
# -*- coding: UTF-8 -*-

##@file
# @ingroup python_bindings_examples
# @author Christoph Holtermann (c.holtermann (at) gmx.de)
# @date 2014-11
# @brief some help for working with invoices, used in \ref py_invoice_export
#
# Credits to Tom Loft for the query to get_all_invoices
# as used in his REST-Api
#
# Issues:
# - get_all_invoices could be added as a method to book Class
# - get_all_customers should be a query like get_all_invoices

try:
    import gnucash
    from gnucash.gnucash_business import Customer, Employee, Vendor, Job, \
        Address, Invoice, Entry, TaxTable, TaxTableEntry, GNC_AMT_TYPE_PERCENT, \
            GNC_DISC_PRETAX
    import str_methods
except ImportError as import_error:
    print "Problem importing modules."
    print import_error
    sys.exit(2)

def get_all_lots(account):
  """Return all lots in account and descendants"""
  ltotal=[]
  descs = account.get_descendants()
  for desc in descs:
    if type(desc).__name__ == 'SwigPyObject':
        desc = gnucash.Account(instance=desc)
    ll=desc.GetLotList()
    ltotal+=ll
  return ltotal

def get_all_invoices_from_lots(account):
  """Return all invoices in account and descendants

  This is based on lots. So invoices without lots will be missed."""

  lot_list=get_all_lots(account)
  invoice_list=[]
  for lot in lot_list:
    if type(lot).__name__ == 'SwigPyObject':
        lot = gnucash.GncLot(instance=lot)

    invoice=gnucash.gnucash_core_c.gncInvoiceGetInvoiceFromLot(lot.instance)
    if invoice:
      invoice_list.append(Invoice(instance=invoice))
  return invoice_list

def get_all_invoices(book, is_paid=None, is_active=None):
    """Returns a list of all invoices in the book.

    posts a query to search for all invoices.

    arguments:
        book                the gnucash book to work with
    keyword-arguments:
        is_paid     int     1 to search for invoices having been paid, 0 for not, None to ignore.
        is_active   int     1 to search for active invoices
    """

    query = gnucash.Query()
    query.search_for('gncInvoice')
    query.set_book(book)

    if is_paid == 0:
        query.add_boolean_match([gnucash.INVOICE_IS_PAID], False, gnucash.QOF_QUERY_AND)
    elif is_paid == 1:
        query.add_boolean_match([gnucash.INVOICE_IS_PAID], True, gnucash.QOF_QUERY_AND)
    elif is_paid == None:
        pass

    # active = JOB_IS_ACTIVE
    if is_active == 0:
        query.add_boolean_match(['active'], False, gnucash.QOF_QUERY_AND)
    elif is_active == 1:
        query.add_boolean_match(['active'], True, gnucash.QOF_QUERY_AND)
    elif is_active == None:
        pass

    # return only invoices (1 = invoices)
    pred_data = gnucash.gnucash_core.QueryInt32Predicate(gnucash.QOF_COMPARE_EQUAL, 1)
    query.add_term([gnucash.INVOICE_TYPE], pred_data, gnucash.QOF_QUERY_AND)

    invoice_list = []

    result = query.run()
    for result in query.run():
        invoice_list.append(Invoice(instance=result))

    query.destroy()

    return invoice_list

def get_all_customers(book):
    """Returns all customers in book.

    Counts IDs upwards. May miss customers with irregular IDs.
    Should be replaced by query as in get_all_invoices."""

    customer_list = []
    customer = True
    customer_id = 0
    while customer:
        customer_id += 1
        customer = book.CustomerLookupByID('%06d' % customer_id)
        if customer:
            customer_list.append(customer)

    return customer_list
