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

"""Tests for single issues."""

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
import new_year
from new_year import (
    printify_transaction,
)


def test_issue_4():
    """"None" target account leads to exception.  Test for correct removal of empty entries."""
    target_accounts = {
        "asset": "Opening:Assets",
        "empty_string": "",
        "none_value": None,
    }
    new_year.clean_balance_accounts(target_accounts)

    assert "empty_string" not in target_accounts
    assert "none_value" not in target_accounts
    assert target_accounts["asset"] == "Opening:Assets"
    assert len(target_accounts) == 1, f"Should be of length 1: {target_accounts}"
