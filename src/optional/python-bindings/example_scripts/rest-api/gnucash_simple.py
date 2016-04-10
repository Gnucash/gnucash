'''

gnucash_simple.py -- A helper file to convert Gnucash objects into
dictionaries for easier conversion to JSON

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
from gnucash.gnucash_business import Entry, Split, Account

def addressToDict(address):
    if address is None:
        return None
    else:
        simple_address = {}
        simple_address['name'] = address.GetName();
        simple_address['line_1'] = address.GetAddr1();
        simple_address['line_2'] = address.GetAddr2();
        simple_address['line_3'] = address.GetAddr3();
        simple_address['line_4'] = address.GetAddr4();
        simple_address['phone'] = address.GetPhone();
        simple_address['fax'] = address.GetFax();
        simple_address['email'] = address.GetEmail();

        return simple_address

def vendorToDict(vendor):

    if vendor is None:
        return None
    else:
        simple_vendor = {}
        simple_vendor['name'] = vendor.GetName()
        simple_vendor['id'] = vendor.GetID()
        simple_vendor['guid'] = vendor.GetGUID().to_string()
        simple_vendor['notes'] = vendor.GetNotes()
        simple_vendor['active'] = vendor.GetActive()
        simple_vendor['currency'] = vendor.GetCurrency().get_mnemonic()
        simple_vendor['tax_table_override'] = vendor.GetTaxTableOverride()
        simple_vendor['address'] = addressToDict(vendor.GetAddr())
        simple_vendor['tax_included'] = vendor.GetTaxIncluded()

        return simple_vendor

def customerToDict(customer):

    if customer is None:
        return None
    else:
        simple_customer = {}
        simple_customer['name'] = customer.GetName()
        simple_customer['id'] = customer.GetID()
        simple_customer['guid'] = customer.GetGUID().to_string()
        simple_customer['notes'] = customer.GetNotes()
        simple_customer['active'] = customer.GetActive()
        simple_customer['discount'] = customer.GetDiscount().to_double()
        simple_customer['credit'] = customer.GetCredit().to_double()
        simple_customer['currency'] = customer.GetCurrency().get_mnemonic()
        simple_customer['tax_table_override'] = customer.GetTaxTableOverride()
        simple_customer['address'] = addressToDict(customer.GetAddr())
        simple_customer['shipping_address'] = addressToDict(
            customer.GetShipAddr())
        simple_customer['tax_included'] = customer.GetTaxIncluded()

        return simple_customer

def transactionToDict(transaction, entities):
    if transaction is None:
        return None
    else:
        simple_transaction = {}
        simple_transaction['guid'] = transaction.GetGUID().to_string()
        simple_transaction['num'] = transaction.GetNum()
        simple_transaction['notes'] = transaction.GetNotes()
        simple_transaction['is_closing_txn'] = transaction.GetIsClosingTxn()
        
        if 'splits' in entities:
            simple_transaction['splits'] = []
            for split in transaction.GetSplitList(): 
                if type(split) != Split:
                    split=Split(instance=split) 
                simple_transaction['splits'].append(
                    splitToDict(split, ['account']))

        simple_transaction['count_splits'] = transaction.CountSplits()
        simple_transaction['has_reconciled_splits'] = \
            transaction.HasReconciledSplits()
        simple_transaction['currency'] = transaction.GetCurrency(
            ).get_mnemonic()
        simple_transaction['imbalance_value'] = transaction.GetImbalanceValue(
            ).to_double()
        simple_transaction['is_balanced'] = transaction.IsBalanced()
        simple_transaction['date'] = transaction.GetDate()
        simple_transaction['date_posted'] = transaction.RetDatePostedTS(
            ).strftime('%Y-%m-%d')
        simple_transaction['date_entered'] = transaction.RetDateEnteredTS(
            ).strftime('%Y-%m-%d')
        simple_transaction['date_due'] = transaction.RetDateDueTS().strftime(
            '%Y-%m-%d')
        simple_transaction['void_status'] = transaction.GetVoidStatus()
        simple_transaction['void_time'] = transaction.GetVoidTime().strftime(
            '%Y-%m-%d')

        simple_transaction['description'] = transaction.GetDescription()

        return simple_transaction

def splitToDict(split, entities):
    if split is None:
        return None
    else:
        simple_split = {}
        simple_split['guid'] = split.GetGUID().to_string()
        if 'account' in entities:
            simple_split['account'] = accountToDict(split.GetAccount())
        if 'transaction' in entities:
            simple_split['transaction'] = transactionToDict(
                split.GetParent(), [])      
        if 'other_split' in entities:
            simple_split['other_split'] = splitToDict(
                split.GetOtherSplit(), ['account'])
        simple_split['amount'] = split.GetAmount().to_double()
        simple_split['value'] = split.GetValue().to_double()
        simple_split['balance'] = split.GetBalance().to_double()
        simple_split['cleared_balance'] = split.GetClearedBalance().to_double()
        simple_split['reconciled_balance'] = split.GetReconciledBalance(
            ).to_double()

        return simple_split
def invoiceToDict(invoice):

    if invoice is None:
        return None
    else:
        simple_invoice = {}
        simple_invoice['id'] = invoice.GetID()
        simple_invoice['type'] = invoice.GetType()
        simple_invoice['date_opened'] = invoice.GetDateOpened().strftime(
            '%Y-%m-%d')
        if invoice.GetDatePosted().strftime('%Y-%m-%d') == '1970-01-01':
            simple_invoice['date_posted'] = None
        else:
            simple_invoice['date_posted'] = invoice.GetDatePosted().strftime(
                '%Y-%m-%d')
        if invoice.GetDateDue().strftime('%Y-%m-%d') == '1970-01-01':
            simple_invoice['date_due'] = None
        else:
            simple_invoice['date_due'] = invoice.GetDateDue().strftime(
                '%Y-%m-%d')
        simple_invoice['notes'] = invoice.GetNotes()
        simple_invoice['active'] = invoice.GetActive()
        simple_invoice['currency'] = invoice.GetCurrency().get_mnemonic()
        simple_invoice['owner'] = vendorToDict(invoice.GetOwner()) 
        simple_invoice['owner_type'] = invoice.GetOwnerType()
        simple_invoice['billing_id'] = invoice.GetBillingID()
        simple_invoice['to_charge_amount'] = invoice.GetToChargeAmount().to_double()
        simple_invoice['posted_txn'] = transactionToDict(invoice.GetPostedTxn(), [])
        simple_invoice['total'] = invoice.GetTotal().to_double()
        simple_invoice['total_subtotal'] = invoice.GetTotalSubtotal(
            ).to_double()
        simple_invoice['total_tax'] = invoice.GetTotalTax().to_double()

        simple_invoice['entries'] = []
        for n, entry in enumerate(invoice.GetEntries()):
            if type(entry) != Entry:
                entry=Entry(instance=entry) 
            simple_invoice['entries'].append(entryToDict(entry))

        simple_invoice['posted'] = invoice.IsPosted()
        simple_invoice['paid'] = invoice.IsPaid()

        return simple_invoice

def billToDict(bill):

    if bill is None:
        return None
    else:
        simple_bill = {}
        simple_bill['id'] = bill.GetID()
        simple_bill['type'] = bill.GetType()
        simple_bill['date_opened'] = bill.GetDateOpened().strftime('%Y-%m-%d')
        if bill.GetDatePosted().strftime('%Y-%m-%d') == '1970-01-01':
            simple_bill['date_posted'] = None
        else:
            simple_bill['date_posted'] = bill.GetDatePosted().strftime(
                '%Y-%m-%d')
        if bill.GetDateDue().strftime('%Y-%m-%d') == '1970-01-01':
            simple_bill['date_due'] = None
        else:
            simple_bill['date_due'] = bill.GetDateDue().strftime('%Y-%m-%d')
        simple_bill['notes'] = bill.GetNotes()
        simple_bill['active'] = bill.GetActive()
        simple_bill['currency'] = bill.GetCurrency().get_mnemonic()
        simple_bill['owner'] = vendorToDict(bill.GetOwner()) 
        simple_bill['owner_type'] = bill.GetOwnerType()
        simple_bill['billing_id'] = bill.GetBillingID()
        simple_bill['to_charge_amount'] = bill.GetToChargeAmount().to_double()
        simple_bill['total'] = bill.GetTotal().to_double()
        simple_bill['total_subtotal'] = bill.GetTotalSubtotal().to_double()
        simple_bill['total_tax'] = bill.GetTotalTax().to_double()

        simple_bill['entries'] = []
        for n, entry in enumerate(bill.GetEntries()):
            if type(entry) != Entry:
                entry=Entry(instance=entry) 
            simple_bill['entries'].append(entryToDict(entry))

        simple_bill['posted'] = bill.IsPosted()
        simple_bill['paid'] = bill.IsPaid()

        return simple_bill

def entryToDict(entry):

    if entry is None:
        return None
    else:

        simple_entry = {}
        simple_entry['guid'] = entry.GetGUID().to_string()
        simple_entry['date'] = entry.GetDate().strftime('%Y-%m-%d')
        simple_entry['date_entered'] = entry.GetDateEntered().strftime(
            '%Y-%m-%d')
        simple_entry['description'] = entry.GetDescription()
        simple_entry['action'] = entry.GetAction()
        simple_entry['notes'] = entry.GetNotes()
        simple_entry['quantity'] = entry.GetQuantity().to_double()
        if entry.GetInvAccount() == None:
            simple_entry['inv_account'] = {}
        else: 
            simple_entry['inv_account'] = accountToDict(entry.GetInvAccount())      
        simple_entry['inv_price'] = entry.GetInvPrice().to_double()
        simple_entry['discount'] = entry.GetInvDiscount().to_double()
        simple_entry['discounted_type'] = entry.GetInvDiscountType()
        simple_entry['discounted_how'] = entry.GetInvDiscountHow()
        simple_entry['inv_taxable'] = entry.GetInvTaxable()
        simple_entry['inv_tax_included'] = entry.GetInvTaxIncluded()
        simple_entry['inv_tax_table_override'] = entry.GetInvTaxTable()
        if entry.GetBillAccount() == None:
            simple_entry['bill_account'] = {}
        else: 
            simple_entry['bill_account'] = accountToDict(
                entry.GetBillAccount()) 
        simple_entry['bill_price'] = entry.GetBillPrice().to_double()
        simple_entry['bill_taxable'] = entry.GetBillTaxable()
        simple_entry['bill_tax_included'] = entry.GetBillTaxIncluded()
        simple_entry['bill_tax_table'] = entry.GetBillTaxTable()
        simple_entry['billable'] = entry.GetBillable()
        simple_entry['bill_payment'] = entry.GetBillPayment()
        simple_entry['is_open'] = entry.IsOpen()

        return simple_entry


def accountToDict(account):

    commod_table = account.get_book().get_table()
    gbp = commod_table.lookup('CURRENCY', 'GBP')

    if account is None:
        return None
    else:
        simple_account = {}
        simple_account['name'] = account.GetName()
        simple_account['type_id'] = account.GetType()
        simple_account['description'] = account.GetDescription()
        simple_account['guid'] = account.GetGUID().to_string()
        if account.GetCommodity() == None:
            simple_account['currency'] = ''
        else:
            simple_account['currency'] = account.GetCommodity().get_mnemonic()
        simple_account['subaccounts'] = []
        for n, subaccount in enumerate(account.get_children_sorted()):
            simple_account['subaccounts'].append(accountToDict(subaccount))

        simple_account['balance'] = account.GetBalance().to_double()
        simple_account['balance_gbp'] = account.GetBalanceInCurrency(
            gbp, True).to_double()
        simple_account['placeholder'] = account.GetPlaceholder()

        return simple_account
