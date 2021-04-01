#!/usr/bin/env python3

# export_account_totals -- export the account totals of all accounts.

# Copyright (C) 2020 Hong Xu <hong@topbug.net>
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
# @author Hong Xu <hong@topbug.net>

# Usage:
#
#    python3 export_account_totals.py /path/to/gnucash_book_file > output.csv

import copy
import sys

from gnucash import GncNumeric, Session, SessionOpenMode


def get_all_sub_accounts(account, names=[]):
    "Iterate over all sub accounts of a given account."

    for child in account.get_children_sorted():
        child_names = names.copy()
        child_names.append(child.GetName())
        yield child, '::'.join(child_names)
        yield from get_all_sub_accounts(child, child_names)


def to_string_with_decimal_point_placed(number: GncNumeric):
    """Convert a GncNumeric to a string with decimal point placed if permissible.
    Otherwise returns its fractional representation.
    """

    number = copy.copy(number)
    if not number.to_decimal(None):
        return str(number)

    nominator = str(number.num())
    point_place = str(number.denom()).count('0')  # How many zeros in the denominator?
    if point_place == 0:
        return nominator

    if len(nominator) <= point_place:  # prepending zeros if the nominator is too short
        nominator = '0' * (point_place - len(nominator)) + nominator

    return '.'.join([nominator[:-point_place], nominator[-point_place:]])


if __name__ == '__main__':
    print('Name,Commodity,Totals,Totals (USD)')
    with Session(sys.argv[1], SessionOpenMode.SESSION_READ_ONLY) as session:
        commodity_table = session.book.get_table()
        USD = commodity_table.lookup("ISO4217", "USD")
        root_account = session.book.get_root_account()
        for account, account_name in get_all_sub_accounts(root_account):
            print(','.join([account_name,
                            account.GetCommodity().get_mnemonic(),
                            to_string_with_decimal_point_placed(account.GetBalance()),
                            to_string_with_decimal_point_placed(account.GetBalanceInCurrency(USD, True))
                            ]))
