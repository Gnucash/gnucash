#!/usr/bin/env python

# account_analysis.py -- Output all the credits and debits on an account
#
# Copyright (C) 2009, 2010 ParIT Worker Co-operative <transparency@parit.ca>
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
#   @brief Output all the credits and debits on an account
#   @author Mark Jenkins, ParIT Worker Co-operative <mark@parit.ca>
#   @ingroup python_bindings_examples

# python imports
from sys import argv, stdout
from datetime import date, timedelta
from bisect import bisect_right
from decimal import Decimal
from math import log10
import csv

# gnucash imports
from gnucash import Session, GncNumeric, Split

# Invoke this script like the following example
# $ gnucash-env python account_analysis.py gnucash_file.gnucash \
#   2010 1 monthly 12 \
#   debits-show credits-show Assets 'Test Account'
#
# That will do an analysis on the account 'Assets:Test Account' from
# gnucash_file.xac, all of the debits and all of the credits will be shown
# and summed on for 12 monthly periods starting from January (1st month) 2010
# 
# if you just want to see the credit and debit sums for each period, use
# the debits-noshow and credits-noshow argument
#
# The output goes to stdout and is in csv format.
#
# Account path arugments are space separated, so you need to quote parts of
# the account path with spaces in them
#
# available period types are monthly quarterly and yearly
#
# At the moment this script only supports GnuCash files of the sqllite3 type
# its an easy edit to switch to xml: etc...


# a dictionary with a period name as key, and number of months in that
# kind of period as the value
PERIODS = {"monthly": 1,
           "quarterly": 3,
           "yearly": 12 }

NUM_MONTHS = 12

ONE_DAY = timedelta(days=1)

DEBITS_SHOW, CREDITS_SHOW = ("debits-show", "credits-show")

ZERO = Decimal(0)

def gnc_numeric_to_python_Decimal(numeric):
    negative = numeric.negative_p()
    if negative:
        sign = 1
    else:
        sign = 0
    copy = GncNumeric(numeric.num(), numeric.denom())
    result = copy.to_decimal(None)
    if not result:
        raise Exception("gnc numeric value %s can't be converted to deciaml" %
                        copy.to_string() )
    digit_tuple = tuple( int(char)
                         for char in str(copy.num())
                         if char != '-' )
    denominator = copy.denom()
    exponent = int(log10(denominator))
    assert( (10 ** exponent) == denominator )
    return Decimal( (sign, digit_tuple, -exponent) )
    

def next_period_start(start_year, start_month, period_type):
    # add numbers of months for the period length
    end_month = start_month + PERIODS[period_type]
    # use integer division to find out if the new end month is in a different
    # year, what year it is, and what the end month number should be changed
    # to.
    # Because this depends on modular arithmatic, we have to curvert the month
    # values from 1-12 to 0-11 by subtracting 1 and putting it back after
    #
    # the really cool part is that this whole thing is implemented without
    # any branching; if end_month > NUM_MONTHS
    #
    # A the super nice thing is that you can add all kinds of period lengths
    # to PERIODS
    end_year = start_year + ( (end_month-1) / NUM_MONTHS )
    end_month = ( (end_month-1) % NUM_MONTHS ) + 1

    return end_year, end_month
    

def period_end(start_year, start_month, period_type):
    if period_type not in PERIODS:
        raise Exception("%s is not a valid period, should be %s" % (
                period_type, str(PERIODS.keys()) ) )

    end_year, end_month = next_period_start(start_year, start_month,
                                            period_type)

    # last step, the end date is day back from the start of the next period
    # so we get a period end like
    # 2010-03-31 for period starting 2010-01 instead of 2010-04-01
    return date(end_year, end_month, 1) - ONE_DAY
    

def generate_period_boundaries(start_year, start_month, period_type, periods):
    for i in xrange(periods):
        yield ( date(start_year, start_month, 1),
                period_end(start_year, start_month, period_type) )
        start_year, start_month = next_period_start(start_year, start_month,
                                                    period_type)

def account_from_path(top_account, account_path, original_path=None):
    if original_path==None: original_path = account_path
    account, account_path = account_path[0], account_path[1:]
    account = top_account.lookup_by_name(account)
    if account.get_instance() == None:
        raise Exception(
            "path " + ''.join(original_path) + " could not be found")
    if len(account_path) > 0 :
        return account_from_path(account, account_path, original_path)
    else:
        return account


def main():
    (gnucash_file, start_year, start_month, period_type, periods,
     debits_show, credits_show) = argv[1:8]
    start_year, start_month, periods = [int(blah)
                                        for blah in (start_year, start_month,
                                                     periods) ]

    debits_show = debits_show == DEBITS_SHOW
    credits_show = credits_show == CREDITS_SHOW

    account_path = argv[8:]

    gnucash_session = Session(gnucash_file, is_new=False)
    root_account = gnucash_session.book.get_root_account()
    account_of_interest = account_from_path(root_account, account_path)

    # a list of all the periods of interest, for each period
    # keep the start date, end date, a list to store debits and credits,
    # and sums for tracking the sum of all debits and sum of all credits
    period_list = [
        [start_date, end_date,
         [], # debits
         [], # credits
         ZERO, # debits sum
         ZERO, # credits sum
         ]
        for start_date, end_date in generate_period_boundaries(
            start_year, start_month, period_type, periods)
        ]
    # a copy of the above list with just the period start dates
    period_starts = [e[0] for e in period_list ]
    
    # insert and add all splits in the periods of interest
    for split in account_of_interest.GetSplitList():
        trans = split.parent
        trans_date = date.fromtimestamp(trans.GetDate())

        # use binary search to find the period that starts before or on
        # the transaction date
        period_index = bisect_right( period_starts, trans_date ) - 1
        
        # ignore transactions with a date before the matching period start
        # (after subtracting 1 above start_index would be -1)
        # and after the last period_end
        if period_index >= 0 and \
                trans_date <= period_list[len(period_list)-1][1]:

            # get the period bucket appropriate for the split in question
            period = period_list[period_index]

            # more specifically, we'd expect the transaction date
            # to be on or after the period start, and  before or on the
            # period end, assuming the binary search (bisect_right)
            # assumptions from above are are right..
            #
            # in other words, we assert our use of binary search
            # and the filtered results from the above if provide all the
            # protection we need
            assert( trans_date>= period[0] and trans_date <= period[1] )
            
            split_amount = gnc_numeric_to_python_Decimal(split.GetAmount())

            # if the amount is negative, this is a credit
            if split_amount < ZERO:
                debit_credit_offset = 1
            # else a debit
            else:
                debit_credit_offset = 0

            # store the debit or credit Split with its transaction, using the
            # above offset to get in the right bucket
            #
            # if we wanted to be really cool we'd keep the transactions
            period[2+debit_credit_offset].append( (trans, split) )
    
            # add the debit or credit to the sum, using the above offset
            # to get in the right bucket
            period[4+debit_credit_offset] += split_amount

    csv_writer = csv.writer(stdout)
    csv_writer.writerow( ('period start', 'period end', 'debits', 'credits') )
    
    def generate_detail_rows(values):
        return (
            ('', '', '', '', trans.GetDescription(),
             gnc_numeric_to_python_Decimal(split.GetAmount()))
            for trans, split in values )
            

    for start_date, end_date, debits, credits, debit_sum, credit_sum in \
            period_list:
        csv_writer.writerow( (start_date, end_date, debit_sum, credit_sum) )

        if debits_show and len(debits) > 0:
            csv_writer.writerow(
                ('DEBITS', '', '', '', 'description', 'value') )
            csv_writer.writerows( generate_detail_rows(debits) )
            csv_writer.writerow( () )
        if credits_show and len(credits) > 0:
            csv_writer.writerow(
                ('CREDITS', '', '', '', 'description', 'value') )
            csv_writer.writerows( generate_detail_rows(credits) )
            csv_writer.writerow( () )

    # no save needed, we're just reading..
    gnucash_session.end()


if __name__ == "__main__": main()


