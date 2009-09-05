#!/usr/bin/env python
# Creates a basic set of accounts and a couple of transactions

from gnucash import Session, Account, Transaction, Split, GncNumeric

FILE_1 = "/tmp/example.gnucash"

session = Session("xml:%s" % FILE_1, True)

book = session.book
root_acct = Account(book)
expenses_acct = Account(book)
savings_acct = Account(book)
opening_acct = Account(book)
trans1 = Transaction(book)
trans2 = Transaction(book)
split1 = Split(book)
split3 = Split(book)
comm_table = book.get_table()
cad = comm_table.lookup("CURRENCY", "CAD")

num1 = GncNumeric(4, 1)
num2 = GncNumeric(100, 1)

#Set new root account
book.set_root_account(root_acct)

#Set up root account and add sub-accounts
root_acct.SetName("Root")
root_acct.SetType(13) #ACCT_TYPE_ROOT = 13
root_acct.append_child(expenses_acct)
root_acct.append_child(savings_acct)
root_acct.append_child(opening_acct)

#Set up Expenses account
expenses_acct.SetCommodity(cad)
expenses_acct.SetName("Expenses")
expenses_acct.SetType(9) #ACCT_TYPE_EXPENSE = 9

#Set up Savings account
savings_acct.SetCommodity(cad)
savings_acct.SetName("Savings")
savings_acct.SetType(0) #ACCT_TYPE_BANK = 0

#Set up Opening Balance account
opening_acct.SetCommodity(cad)
opening_acct.SetName("Opening Balance")
opening_acct.SetType(10) #ACCT_TYPE_EQUITY = 10

split1.SetValue(num1)
split1.SetAccount(expenses_acct)
split1.SetParent(trans1)

split3.SetValue(num2)
split3.SetAccount(savings_acct)
split3.SetParent(trans2)

trans1.SetCurrency(cad)
trans1.SetDescription("Groceries")

trans2.SetCurrency(cad)
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
