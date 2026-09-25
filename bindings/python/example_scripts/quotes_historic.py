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

from gnucash import Session, GncNumeric, GncPrice, PriceSource, GNC_HOW_DENOM_FIXED, GNC_HOW_RND_ROUND_HALF_UP
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
session = Session(url)
root = session.book.get_root_account()
book = session.book
account = book.get_root_account()
pdb = book.get_price_db()
comm_table = book.get_table()

# This example assumes the Intel holding is priced in USD.
ac = find_account(account, 'Intel')[0]
stock = ac.GetCommodity()
cur = comm_table.lookup("CURRENCY", "USD")

# Construct the price at the precision GnuCash uses for a commodity quoted in a
# currency: the currency's smallest unit (its SCU -- get_fraction() is 100 for
# USD, i.e. cents) times COMMODITY_DENOM_MULT (10000), the extra precision a
# price carries beyond an ordinary amount. See "Price policy" in gnc-pricedb.h.
price_denom = cur.get_fraction() * COMMODITY_DENOM_MULT
for i in range(0, len(stock_date)):
  print('Adding', i, stock_date[i], stock_price[i])
  # --- Create a fresh price object with all properties set ------------------
  p_new = GncPrice(book)
  p_new.set_commodity(stock)
  p_new.set_currency(cur)
  p_new.set_time64(stock_date[i])
  p_new.set_value(GncNumeric(stock_price[i], price_denom,
                             GNC_HOW_DENOM_FIXED | GNC_HOW_RND_ROUND_HALF_UP))
  p_new.set_source(PriceSource.FINANCE_QUOTE)
  p_new.set_typestr("last")                    # bid / ask / last / nav / unknown
  # --------------------------------------------------------------------------
  pdb.add_price(p_new)

# Clean up
session.save()
session.end()
session.destroy()
