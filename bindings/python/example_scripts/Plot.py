#!/usr/bin/env python3

##  @file
#   @brief Example for plotting with pandas and matplotlib
#   @detail Create a temporary file with two Accounts and
#           100 random transactions in the 20th century
#           Feed dates and balances to pandas and plot
#           using matplotlib
#   @author Christoph Holtermann
#   @date 2022-03-20
#   @ingroup python_bindings_examples

import datetime
import pandas
import matplotlib.dates as mdates
import matplotlib.pyplot as plt
from random import randint
from gnucash import Session, SessionOpenMode, Account, Transaction, Split, \
                    GncNumeric

# name of temporary file
filename = "/tmp/plot.gnucash"

# We need to tell GnuCash the data format to create the new file as (xml://)
uri = "xml://" + filename

def createAccounts(book):
    """create two accounts ('Account A' and 'Account B')"""
    root_account = book.get_root_account()
    commodtable = book.get_table()
    currency = commodtable.lookup("CURRENCY", "EUR")
    ses.save()

    print('Create two accounts ("Account A", "Account B")')
    accountA = Account(book)
    accountA.SetCommodity(currency)
    accountA.SetName("Account A")
    root_account.append_child(accountA)

    accountB = Account(book)
    accountB.SetCommodity(currency)
    accountB.SetName("Account B")
    root_account.append_child(accountB)

    #ses.save()

    return accountA, accountB

def createRandomTransactions(book, accountA, accountB):
    """Create 100 random transactions in the 20th century"""
    split = Split(book)

    currency = book.get_table().lookup("CURRENCY", "EUR")

    print("Create 100 random transactions")
    for i in range(100):

        trans = Transaction(book)
        trans.BeginEdit()
        trans.SetCurrency(currency)
        trans.SetDate(randint(1,28), randint(1,12), randint(1900,2000))
        trans.SetDescription("Transaction "+str(i))

        value = randint(-5000,10000)

        value1 = GncNumeric(value, 100)
        value2 = GncNumeric(-value, 100)

        split1 = Split(book)
        split1.SetValue(value1)
        split1.SetAccount(accountA)
        split1.SetMemo("A" + str(i))
        split1.SetParent(trans)

        split2 = Split(book)
        split2.SetValue(value2)
        split2.SetAccount(accountB)
        split2.SetMemo("B" + str(i))
        split2.SetParent(trans)

        trans.CommitEdit()

def removePreviousFile(filename):
    """Remove previous tempfile if exists"""
    from pathlib import Path

    path = Path(filename)

    if path.is_file():
        import os
        os.remove(filename)

removePreviousFile(filename)

with Session(uri, SessionOpenMode.SESSION_NEW_STORE) as ses:

    book = ses.get_book()
    accountA, accountB = createAccounts(book)
    createRandomTransactions(book, accountA, accountB)

    dates = []
    balances = []

    for split in accountA.GetSplitList():
        dates.append(split.GetParent().GetDate())
        # convert GncNumeric to float
        balances.append(float(split.GetBalance()))

    values = { "dates" : dates,
               "balances" : balances }

    # use pandas as intermediate to matplotlib
    data = pandas.DataFrame(values)
    data.plot(x="dates", y="balances")

    # set descriptions
    plt.title(accountA.GetName())
    plt.xlabel('Date')
    plt.ylabel(f'Balance ({accountA.GetCommodity().get_default_symbol()})')

    # show plot
    plt.show()
