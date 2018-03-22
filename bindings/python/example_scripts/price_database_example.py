#!/usr/bin/env python
# Another test file for price database stuff
# To update the price database call
# $PATH/gnucash  --add-price-quotes $PATHTOFILE
# before running this.
# Adding to a calling bash script would be better
# Although calling it from here would be even better!
#  OR:  export PYTHONPATH=$HOME/progs/lib/python2.6/site-packages
# Then: gnucash-env ipython
# The account file is not saved but always use a disposable copy.
# Thanks for contributions by Christoph Holtermann and Mark Jenkins

##  @file
#   @brief Test file for price database stuff
#   @author Mike Evans, Christoph Holtermann, Mark Jenkins
#   @ingroup python_bindings_examples

from gnucash import Session

# -------------------------------------------
# Configuration options can be changed here :

cur_mnemonic="EUR"                      # Currency that prices are shown in. Possibilities include "EUR","GBP",...
namespace_name = ""                     # If no namespace_name is set, all namespaces will be shown
show_prices = True                      # If True, all prices for commodity are shown
commodity_fullname = ""                 # If no name is given, all commoditys in namespace will be shown
FILE = "PATH_TO_YOUR_TEST_FILE"         # File is not saved but use a copy anyway

# Configuration end
# -------------------------------------------

session = Session(FILE, True, False, False)

root = session.book.get_root_account()
book = session.book
pdb = book.get_price_db()
comm_table = book.get_table()

cur = comm_table.lookup("CURRENCY", cur_mnemonic)
cur_name = cur.get_fullname()


if namespace_name != "":                    # Show single namespace
  namespaces [ comm_table.find_namespace(namespace_name) ]

else:                                 # Show all namespaces
  namespaces=comm_table.get_namespaces_list()

for namespace in namespaces:

  namespace_name=namespace.get_name()


  # Get a list of all commodities in namespace
  commodities=comm_table.get_commodities(namespace_name)


  if len(commodities) == 0 :

    print("No commodity in namespace "+namespace_name+".")
  else:
    if commodity_fullname:
      print("Searching commodity '"+commodity_fullname+"' in namespace "+namespace_name)
    else:
      print("Commoditys in namespace "+namespace_name+":")


    for i, c in enumerate(commodities):

      c_fullname = c.get_fullname()

      if not(commodity_fullname) or (commodity_fullname == c_fullname):
        print("["+str(i)+"] Full Name :", c.get_fullname())
        if show_prices:
          pl = pdb.get_prices(c,cur)

          if len(pl) > 0 :
            print("{0} {1:20}{2:>10} {3}".format("Time      ","Source","Price","Currency"))
            for pr in pl:

               source = pr.get_source()
               time = pr.get_time()
               v=pr.get_value()
               price = float(v.num)/v.denom

               print("{0} {1:20}{2:10.4f} {3}".format(time,source,price,cur_name))
               # I didn't find out how to format the time option...

session.end()
session.destroy()
quit()
