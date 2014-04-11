#!/usr/bin/env python
# Creates a basic set of accounts and a couple of transactions

import gnucash

FILE_1 = "/tmp/example.xac"

session = None
session = gnucash.Session("file:%s" % FILE_1, True)

book = session.book
root_acct = gnucash.Account(book)
expenses_acct = gnucash.Account(book)
savings_acct = gnucash.Account(book)
opening_acct = gnucash.Account(book)
trans1 = gnucash.Transaction(book)
trans2 = gnucash.Transaction(book)
split1 = gnucash.Split(book)
split3 = gnucash.Split(book)
comm = gnucash.GncCommodity(book, "Canadian Dollars", "CURRENCY", "CAD", None, 100)
num1 = gnucash.GncNumeric(4, 1)
num2 = gnucash.GncNumeric(100, 1)

#Set new root account
book.set_root_account(root_acct)

#Set up root account and add sub-accounts
root_acct.SetName("Root")
root_acct.SetType(13) #ACCT_TYPE_ROOT = 13
root_acct.append_child(expenses_acct)
root_acct.append_child(savings_acct)
root_acct.append_child(opening_acct)

#Set up Expenses account
expenses_acct.SetCommodity(comm)
expenses_acct.SetName("Expenses")
expenses_acct.SetType(9) #ACCT_TYPE_EXPENSE = 9

#Set up Savings account
savings_acct.SetCommodity(comm)
savings_acct.SetName("Savings")
savings_acct.SetType(0) #ACCT_TYPE_BANK = 0

#Set up Opening Balance account
opening_acct.SetCommodity(comm)
opening_acct.SetName("Opening Balance")
opening_acct.SetType(10) #ACCT_TYPE_EQUITY = 10

split1.SetValue(num1)
split1.SetAccount(expenses_acct)
split1.SetParent(trans1)

split3.SetValue(num2)
split3.SetAccount(savings_acct)
split3.SetParent(trans2)

trans1.SetCurrency(comm)
trans1.SetDescription("Groceries")

trans2.SetCurrency(comm)
trans2.SetDescription("Opening Savings Balance")

split2 = split1.GetOtherSplit()
split2.SetAccount(savings_acct)

split4 = split3.GetOtherSplit()
split4.SetAccount(opening_acct)

book.print_dirty()

book.mark_saved()
book.mark_closed()

book.print_dirty()

session.save()
session.end()
session.destroy()
