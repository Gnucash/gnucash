# Copyright (C) 2024 Quazgar <quazgar@posteo.de>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

"""A first test script, also for testing the testability in general."""

from gnucash import (
    Account,
    Book,
    GncNumeric,
    Split,
    Transaction,
)

import utils  # set sys.path by importing
from new_year import (
    printify_transaction,
)


def test_printify():
    book = Book()

    # empty transaction
    empty_tr = Transaction(book)
    tr_str = printify_transaction(empty_tr)
    print("\n" + tr_str)
    assert len(tr_str.strip()) == 61
    assert tr_str.strip().split("\n")[-1] == "Imbalance: 0.00"

    # a small transaction
    tbl = book.get_table()
    curr = tbl.lookup("CURRENCY", "EUR")
    acct_root = book.get_root_account()
    acct = Account(book)
    acct_root.append_child(acct)
    acct.SetName("Hello")
    acct.SetCommodity(curr)
    acct.SetType(3)

    spl = Split(book)
    spl.SetAccount(acct)
    spl.SetAmount(GncNumeric(12.34))
    spl.SetValue(GncNumeric(12.34))
    tr = Transaction(book)
    tr.BeginEdit()
    tr.SetCurrency(curr)
    spl.SetParent(tr)

    tr_str = printify_transaction(tr)
    print("\n" + tr_str)
    parts = tr_str.split("\n")
    assert parts[0].strip() == "12.34  |  Hello"
    assert parts[-1] == "Imbalance: 12.34"
