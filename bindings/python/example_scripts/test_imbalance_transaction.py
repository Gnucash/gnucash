#!/usr/bin/env python

# test_imbalance_transaction.py -- Test the transaction imbalace viewing
# mechanisms
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

##  @file
#   @brief Test the transaction imbalace viewing mechanisms
#   @author Mark Jenkins, ParIT Worker Co-operative <mark@parit.ca>
#   @ingroup python_bindings_examples

from sys import argv, exit

from gnucash import Session, Transaction, Split, Account, GncNumeric, \
    GncCommodity

# argv[1] should be the path to an existing gnucash file/database
# for a file, simply pass the pathname, for a database you can use
# these forms:
# mysql://user:password@host/dbname
# postgres://user:password@host[:port]/dbname (the port is optional)
#
# You should try it out with a gnucash file with tranding accounts enabled
# and trading accounts disabled

if len(argv) < 2:    
    print('not enough parameters')
    print('usage: test_imbalance_transaction.py {book_url}')
    print('examples:')
    print("gnucash-env python test_imbalance_transaction.py '/home/username/test.gnucash'")
    exit()


try:
    session = Session(argv[1])

    book = session.book

    root = book.get_root_account()
    root.get_instance()
    commod_tab = session.book.get_table()
    CAD = commod_tab.lookup("ISO4217","CAD")
    USD = commod_tab.lookup("ISO4217","USD")
    account = Account(book)
    account2 = Account(book)
    root.append_child(account)
    root.append_child(account2)
    account.SetCommodity(CAD)
    account.SetName("blahblah")
    account.SetType(3)
    account2.SetCommodity(USD)
    account2.SetName("blahblahsdfs ")
    account2.SetType(3)

    a = Transaction(book)
    a.BeginEdit()

    s = Split(book)
    s.SetParent(a)
    s2 = Split(book)
    s2.SetParent(a)

    a.SetCurrency(CAD)
    s.SetAccount(account)
    s.SetValue(GncNumeric(2))
    s.SetAmount(GncNumeric(2))

    s2.SetAccount(account2)
    s2.SetValue(GncNumeric(4))
    s2.SetAmount(GncNumeric(4))
    print('overall imbalance', a.GetImbalanceValue().to_string())

    print('per-currency imbalances')
    imbalance_list = a.GetImbalance()
    for (commod, value) in imbalance_list:
        print(value.to_string(), commod.get_mnemonic())

    a.CommitEdit()


    session.end()
    session.destroy()
except:
    if "session" in locals():
        session.end()
    raise
