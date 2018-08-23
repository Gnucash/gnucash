#!/usr/bin/env python3

# Test C++
#
# use SWIG C++ wrapper
#
# modified from simple_book.py

import gnucash

# We need to tell GnuCash the data format to create the new file as (xml://)
uri = "xml:///tmp/simple_book.gnucash"
print("uri:", uri)

book = gnucash.Book()
qofsession = gnucash.gnucash_core_cc.QofSessionImpl(book.instance)
qofsession.begin(uri, True, True, True)

print(qofsession.get_backend().get_message())

root_account = book.get_root_account()

description = "TEST"
print("set root account description to", description)
root_account.SetDescription(description)

print("creating account 'test'")
account = gnucash.Account(book)
account.SetName("test")

root_account.append_child(account)

print("saving")
qofsession.save(None)

print("ending session")
qofsession.end()

print("reloading")

book = gnucash.Book()
qofsession = gnucash.gnucash_core_cc.QofSessionImpl(book.instance)
qofsession.begin(uri, False, False, False)
qofsession.load(None)

root_account = book.get_root_account()
description_reloaded = root_account.GetDescription()
print("root_account description:", description_reloaded)

print("accounts:")
accounts = root_account.get_children()
for account in accounts:
    print(account.GetName())

print("ending session")
qofsession.end()
