#!/usr/bin/env python
# Test file for price database stuff
# To update the price database call
# $PATH/gnucash  --add-price-quotes $PATHTOFILE
# before running this.
# Adding to a calling bash script would be better
# Although calling it from here would be even better!
#  OR:  export PYTHONPATH=$HOME/progs/lib/python2.6/site-packages
# Then: gnucash-env ipython
# The account file is not saved but always use a disposable copy.
# Change, FILE, CURRENCY and STOCK to those defined in your test account.

##  @file
#   @brief Test file for price database stuff
#   @author Mike Evans 
#   @ingroup python_bindings_examples

from gnucash import Session

# FILE should be the path to your existing gnucash file/database
# For a file, simply pass the pathname, for a database you can use
# these forms:
# mysql://user:password@host/dbname
# postgres://user:password@host[:port]/dbname (the port is optional)
#
FILE = "PATH_TO_YOUR_TEST_FILE"  ## Fail is not saved but use a copy anyway

session = Session(FILE, True, False, False)

root = session.book.get_root_account()
book = session.book
pdb = book.get_price_db()
comm_table = book.get_table()
gbp = comm_table.lookup("CURRENCY", "SOME_CURRENCY")
arm = comm_table.lookup("NASDAQ", "SOME_STOCK")
latest = pdb.lookup_latest(arm,gbp) # from the table, NOT live data
value = latest.get_value()
pl = pdb.get_prices(arm,gbp)
for pr in pl:
   source = pr.get_source()
   time = pr.get_time()
   v=pr.get_value()
   price = float(v.num)/v.denom
   print time, source, price

if len(pl) > 0:
   v0 = pl[0].get_value()
   print arm.get_fullname(), float(v0.num) / float(v0.denom )

session.end()
session.destroy()
quit()
