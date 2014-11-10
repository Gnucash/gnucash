#!/usr/bin/env python
# -*- coding: UTF-8 -*-

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

def get_all_invoices(book):
    """Returns all invoices in the book."""

    invoice_list = []
    invoice = True
    invoice_id = 0
    while invoice:
        invoice_id += 1
        invoice = book.InvoiceLookupByID('%06d' % invoice_id)
        if invoice:
            invoice_list.append(invoice)

    return invoice_list

def get_all_customers(book):
    """Returns all customers in book."""

    customer_list = []
    customer = True
    customer_id = 0
    while customer:
        customer_id += 1
        customer = book.CustomerLookupByID('%06d' % customer_id)
        if customer:
            customer_list.append(customer)

    return customer_list
