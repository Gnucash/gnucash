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

"""Run the full program."""

import shutil
from pathlib import Path

import pytest
from gnucash import (
    Account,
    Book,
    GncNumeric,
    Session,
    SessionOpenMode,
    Split,
    Transaction,
)

import utils as test_utils  # importing sets sys.path
from new_year import (
    duplicate_with_opening_balance,
    printify_transaction,
)
from gnucash_tools import business, utils


@pytest.fixture
def datadir(tmp_path):
    old_datadir = test_utils.datadir(__file__)
    new_datadir = tmp_path / "data"
    shutil.copytree(old_datadir, new_datadir)
    return new_datadir


def test_call_new_year(tmp_path, datadir):
    """Calling the main function on a simple data file."""
    oldfile = datadir / "transactions_business.gnucash"
    newfile = tmp_path / "new.gnucash"
    target_accounts = {
        "asset": "Opening:Assets",
        "liability": "Opening:Liabilities",
    }
    duplicate_with_opening_balance(old=str(oldfile), target=str(newfile),
                                   balance_accounts=target_accounts)

    expected_balances = {
        "Assets": 1700.00,
        "Assets:Current Assets:Cash in Wallet": 125.00,
        "Equity": 0.00,
        "Liabilities": -1700.00,
        "Liabilities:Loans": -1500.00,
        "Opening:Assets": -1500.00,
        "Opening:Liabilities": 1500.00,
    }
    expected_business_entities = {
        business.Vendor: {
            "Name": "Lieferant 1",
            "ID": "000001",
        },
        business.Customer: {
            "Name": "Kunde1",
            "ID": "000001",
        },
        business.Employee: {
            "Name": "Emma Worker",
            "ID": "000001",
        },
    }

    with Session("sqlite3://" + str(newfile), SessionOpenMode.SESSION_READ_ONLY) as session:
        book = session.get_book()
        # Test balances
        accounts = utils.get_all_accounts(book)
        for fullname, expected in expected_balances.items():
            assert fullname in accounts
            acct = accounts[fullname]
            balance = float(acct.GetBalanceInCurrency(acct.GetCommodity(), include_children=True))
            assert balance == expected, f"Account: {fullname}, balance {balance} != {expected}"
        # Test business entities
        for cls, properties in expected_business_entities.items():
            entity = cls.get_all(book)[0]  # Only consider first entry at the moment.
            for name, value in properties.items():
                assert getattr(entity._base, "Get" + name)() == value
