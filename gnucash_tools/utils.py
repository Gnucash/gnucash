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

"""Utility classes und functions for Gnucash.
"""

from gnucash import (
    Account,
    Book,
)


def get_all_accounts(book: Book) -> dict[str, Account]:
    """Get a dict with all accounts in the book, indexed by their name.

This function's behavior is undefined if there are name duplicates.
    """

    result = {}
    root = book.get_root_account()
    result[""] = root
    prefix = ""

    _add_accounts(parent=root, result=result, prefix=prefix)
    return result


def _add_accounts(parent: Account, result: dict[str, Account], prefix: str) -> None:
    """Recursively add accounts to a result dict.

Parameters
----------
parent : Account
    Start here, add descendents.

result : dict[str, Account]
    Add the accounts here.  Keys are ``prefix + ":" + account_name``

prefix : str
    Use this prefix for the name keys.
    """
    for child in parent.get_children_sorted():
        path = f"{prefix}:{child.name}".strip(":")
        result[path] = child
        _add_accounts(parent=child, result=result, prefix=path)
