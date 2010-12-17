#!/usr/bin/env python
##  @file
#   @brief Example Script simple sqlite create 
#   @ingroup python-bindings-examples

from gnucash import Session, Account
from os.path import abspath
from gnucash.gnucash_core_c import ACCT_TYPE_ASSET

s = Session('sqlite3://%s' % abspath('test.blob'), is_new=True)
# this seems to make a difference in more complex cases
s.save()

book = s.book
root = book.get_root_account()
a = Account(book)
root.append_child(a)
a.SetName('wow')
a.SetType(ACCT_TYPE_ASSET)

commod_table = book.get_table()
a.SetCommodity( commod_table.lookup('CURRENCY', 'CAD') )
s.save()

s.end()
