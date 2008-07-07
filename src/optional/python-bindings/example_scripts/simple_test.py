#!/usr/bin/env python

from gnucash import \
     Session, Account, Transaction, Split, GncCommodity, GncNumeric
     

FILE_1 = "/tmp/example.xac"

session = None
session = Session("file:%s" % FILE_1, True)

book = session.book
root_account = book.get_root_account()
acct1 = Account(book)
acct2 = Account(book)
trans = Transaction(book)
split1 = Split(book)
split2 = Split(book)
comm = GncCommodity(book, "Canadian Dollars", "CURRENCY", "CAD", None, 100)
debit_num = GncNumeric(4, 1)
credit_num = debit_num.neg()

acct1.SetCommodity(comm)
acct1.SetName("Savings")
root_account.append_child(acct1)

acct2.SetCommodity(comm)
acct2.SetName("Food expenses")
root_account.append_child(acct2)

split1.SetValue(credit_num)
split1.SetAccount(acct1)
split1.SetParent(trans)

split2.SetValue(debit_num)
split2.SetAccount(acct2)
split2.SetParent(trans)

trans.SetCurrency(comm)
trans.SetDescription("Groceries")

book.print_dirty()

book.mark_saved()
book.mark_closed()

session.save()
session.end()
session.destroy()
