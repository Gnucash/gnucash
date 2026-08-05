#!/usr/bin/env python3

# quotes_historic.py -- Example Script to read historic quote data into gnucash
#
 
##  @file
#   @brief Example Script to read historic stock data into gnucash
#   @author Peter Holtermann
#   @date January 2011
#   @ingroup python_bindings_examples
#  
#   Call the perl-script @code 
#   ./get_quotes.pl INTC 
#   @endcode first to achieve data into file INTC which can thereafter be imported to GnuCash using this script.
# 
#   For explanation of use have a look at the wiki:
#   https://wiki.gnucash.org/wiki/Stocks/get_prices
#

from gnucash import Session, GncNumeric, GNC_HOW_DENOM_FIXED, GNC_HOW_RND_ROUND_HALF_UP
from gnucash.gnucash_core_c import COMMODITY_DENOM_MULT
import datetime
from gnc_convenience import find_account

FILE = "./test.gnucash"
url = "xml://"+FILE

# Read data from file
f = open('INTC')
data = []
while 1:
    tmp = f.readline()
    if(len(tmp)<2):
        break
    
    data.append(tmp)

f.close()

stock_date = []
stock_price = []
for i in range(1,len(data)):
    year = int(data[i].rsplit(',')[1].rsplit('/')[0])
    month = int(data[i].rsplit(',')[1].rsplit('/')[1])
    day = int(data[i].rsplit(',')[1].rsplit('/')[2])
    stock_date.append(datetime.datetime(year,month,day))
    stock_price.append(float(data[i].rsplit(',')[5]))

# Initialize Gnucash session
session = Session(url, True, False, False)
root = session.book.get_root_account()
book = session.book
account = book.get_root_account()
pdb = book.get_price_db()
comm_table = book.get_table()
ac = find_account(account,'Intel')[0] 

stock = ac.GetCommodity()
# Add the prices
pdb = book.get_price_db()
if len(ac.GetSplitList())<1:
  print('Need at least one Split to get currency info ... ')
  raise SystemExit
cur = ac.GetSplitList()[0].GetParent().GetCurrency()

# Get stock data
pl = pdb.get_prices(stock,cur)
if len(pl)<1:
  print('Need at least one database entry to clone ...')
  raise SystemExit

pl0 = pl[0]
for i in range(1,len(pl)):
  pdb.remove_price(pl[i])

# Store the price the way GnuCash does internally: a commodity's price in a
# currency uses a fixed denominator of (pricing currency's SCU * 10000). See
# the "Price policy" note and COMMODITY_DENOM_MULT in gnc-pricedb.h. For a
# USD-priced stock this is 100 * 10000 = 1000000, i.e. six decimal places.
price_denom = cur.get_fraction() * COMMODITY_DENOM_MULT
for i in range(0,len(stock_date)):
  p_new = pl0.clone(book)
  print('Adding',i,stock_date[i],stock_price[i])
  p_new.set_time64(stock_date[i])
  p_new.set_value(GncNumeric(stock_price[i], price_denom,
                             GNC_HOW_DENOM_FIXED | GNC_HOW_RND_ROUND_HALF_UP))
  p_new.set_source("Finance::Quotes::Historic")
  pdb.add_price(p_new)

# Clean up
session.save()
session.end()
session.destroy()
