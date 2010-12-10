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
# Thanks for contributions by Christoph Holtermann

import gnucash
from gnucash.gnucash_core_c import gnc_pricedb_get_db, gnc_pricedb_get_prices, gnc_pricedb_lookup_latest,gnc_pricedb_print_contents
from gnucash.gnucash_core import *

# -------------------------------------------
# Configuration options can be changed here :

cur_mnemonic="EUR"                      # Possibilities include "EUR","GBP"
namespace = "EUREX"                     # Predefined namespaces are "CURRENCY","EUREX","AMEX","template"
show_prices = True
commodity_fullname = "ThyssenKrupp AG"  # If no name is given, all commoditys in namespace will be printed out
FILE = "PATH_TO_YOUR_TEST_FILE"         # File is not saved but use a copy anyway
url = "xml://"+FILE

# Configuration end
# -------------------------------------------

session = Session(url, True, False, False)

root = session.book.get_root_account()
book = session.book
pdb = book.get_price_db()
comm_table = book.get_table()

cur = comm_table.lookup("CURRENCY", cur_mnemonic)
cur_name = cur.get_fullname()

# Get a list of all commodities in namespace
commodities=comm_table.get_commodities(namespace)

if not commodities:
  print "No commodity in namespace "+namespace+" !"
  quit()

if commodity_fullname:
  print "Searching commodity '"+commodity_fullname+"' in namespace "+namespace
else:
  print "Commoditys in namespace "+namespace+":"

for i in range(len(commodities)):
  c=gnucash.GncCommodity(instance=commodities[i])
  commodities[i]=c

  c_fullname = c.get_fullname()

  if not(commodity_fullname) or (commodity_fullname == c_fullname):
    print "["+str(i)+"] Full Name :", c.get_fullname()
    if show_prices:
      pl = pdb.get_prices(c,cur)
      if pl:
        print "{0} {1:20}{2:10} {3}".format("Time      ","Source","Price","Currency")
        for i in pl:
           pr = GncPrice(instance=i)
           source = pr.get_source()
           time = pr.get_time()
           v=pr.get_value()
           price = float(v.num)/v.denom

           print "{0} {1:20}{2:10.4f} {3}".format(time,source,price,cur_name)
           # I didn't find out how to format the time option...

session.end()
session.destroy()
quit()
