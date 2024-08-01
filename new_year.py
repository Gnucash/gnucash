#!/usr/bin/env python3

# Copyright (C) 2009, 2010 Mark Jenkins, ParIT Worker Co-operative <transparency@parit.ca>
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
#

"""
Utility for creating a new book, for example at the beginning of the year.

Based upon `new_book_with_opening_balances.py` from the Gnucash Python examples.

@author Mark Jenkins, ParIT Worker Co-operative <mark@parit.ca>
@author Quazgar <quazgar@posteo.de>

@file
  @brief Replicate the account structure of a
  book and apply basis opening balances from the original
"""

import argparse
import configparser
import os
import sys
from datetime import date
from enum import IntEnum
from typing import Optional

# import piecash

import gnucash
from gnucash import (
    Account,
    Book,
    GncCommodity,
    GncNumeric,
    Transaction,
    Session,
    SessionOpenMode,
    Split,
)
from gnucash.gnucash_core_c import \
    GNC_DENOM_AUTO, GNC_HOW_DENOM_EXACT
# ACCT_TYPE_ASSET, ACCT_TYPE_BANK, ACCT_TYPE_CASH, ACCT_TYPE_CHECKING, \
# ACCT_TYPE_CREDIT, ACCT_TYPE_EQUITY, ACCT_TYPE_EXPENSE, ACCT_TYPE_INCOME, \
# ACCT_TYPE_LIABILITY, ACCT_TYPE_MUTUAL, ACCT_TYPE_PAYABLE, \
# ACCT_TYPE_RECEIVABLE, ACCT_TYPE_STOCK, ACCT_TYPE_ROOT, ACCT_TYPE_TRADING

# autopep8: off
sys.path.append(".")
from gnucash_tools import business
# autopep8: on

# This script takes a gnucash url
# and creates a new file/db at a second url that has the same
# account tree and an equivalent opening balance on all the simple balance
# sheet accounts (not for income and expense accounts)
#
# This is done a per currency basis, one opening balance account for each
#
# For non-simple balance sheet accounts (like payable, recievable, stock,
# mutual, and trading, you'll have to put the opening balance in yourself
#
# Invocation examples:
# python3 new_book_with_opening_balances.py \
#   '/home/mark/test.gnucash'
#   'sqlite3:///home/mark/new_test.gnucash'
#
# python3 new_book_with_opening_balances.py \
#   '/home/mark/test.gnucash' \
#   'xml:///crypthome/mark/parit-financial-system/new_test.gnucash'
#
# Remember that the gnucash python package has to be in your PYTHONPATH
# if you're installed GnuCash in a non-standard location, you'll have to do
# something like this
# export PYTHONPATH=gnucash_install_path/lib/python2.x/site-packages/

# argv[1] should be the path to an existing gnucash file/database
# For a file, simply pass the pathname. GnuCash will determine the data format
# xml or sqlite3 automatically.
# For a database you can use these forms:
#   mysql://user:password@host/dbname
#   postgres://user:password@host[:port]/dbname (the port is optional)
#
# argv[2] should be the path for the new gnucash file/database
# For a file, simply pass the pathname prefixed with the requested data format
# like:
#   xml:///home/blah/blah.gnucash
#   sqlite3:///home/blah/blah.gnucash
# Paths can also be relative, for example:
#   xml://from-here/to/there/blah.gnucash
# For a database you can use these forms:
#   mysql://user:password@host/dbname
#   postgres://user:password@host[:port]/dbname (the port is optional)


OPENING_DATE = (1, 1, 2011)  # day, month, year

# https://code.gnucash.org/docs/STABLE/Account_8h_source.html#l00101


class AcctType(IntEnum):
    # ACCT_TYPE_INVALID = -1
    ACCT_TYPE_NONE = -1
    ACCT_TYPE_BANK = 0
    ACCT_TYPE_CASH = 1
    ACCT_TYPE_CREDIT = 3
    ACCT_TYPE_ASSET = 2
    ACCT_TYPE_LIABILITY = 4
    ACCT_TYPE_STOCK = 5
    ACCT_TYPE_MUTUAL = 6
    ACCT_TYPE_CURRENCY = 7
    ACCT_TYPE_INCOME = 8
    ACCT_TYPE_EXPENSE = 9
    ACCT_TYPE_EQUITY = 10
    ACCT_TYPE_RECEIVABLE = 11
    ACCT_TYPE_PAYABLE = 12
    ACCT_TYPE_ROOT = 13
    ACCT_TYPE_TRADING = 14
    # NUM_ACCOUNT_TYPES = 15
    # bank account types *
    ACCT_TYPE_CHECKING = 15
    ACCT_TYPE_SAVINGS = 16
    ACCT_TYPE_MONEYMRKT = 17
    ACCT_TYPE_CREDITLINE = 18

    @classmethod
    def from_shortname(cls, name: str):
        try:
            return cls[f"ACCT_TYPE_{name.upper()}"]
        except KeyError:
            raise KeyError(name)

    def shortname(self) -> str:
        """Return the shortname for this account type.

For example, ACCT_TYPE_ASSET will return ``asset``.
        """
        prefix = "ACCT_TYPE_"
        if self.name.startswith(prefix):
            return self.name[len(prefix):].lower()
        return self.name


# possible account types of interest for opening balances
ACCOUNT_TYPES_TO_OPEN = set((
    AcctType.ACCT_TYPE_BANK,
    AcctType.ACCT_TYPE_CASH,
    AcctType.ACCT_TYPE_CREDIT,
    AcctType.ACCT_TYPE_ASSET,
    AcctType.ACCT_TYPE_LIABILITY,
    AcctType.ACCT_TYPE_STOCK,
    AcctType.ACCT_TYPE_MUTUAL,
    AcctType.ACCT_TYPE_INCOME,
    AcctType.ACCT_TYPE_EXPENSE,
    AcctType.ACCT_TYPE_EQUITY,
    AcctType.ACCT_TYPE_RECEIVABLE,
    AcctType.ACCT_TYPE_PAYABLE,
    AcctType.ACCT_TYPE_TRADING,
))

# You don't need an opening balance for income and expenses, past income
# and expenses should be in Equity->retained earnings
# so we remove them from the above set
ACCOUNT_TYPES_TO_OPEN = ACCOUNT_TYPES_TO_OPEN.difference(set((
    AcctType.ACCT_TYPE_INCOME,
    AcctType.ACCT_TYPE_EXPENSE,
)))

# This script isn't capable of properly creating the lots required for
# STOCK, MUTUAL, RECEIVABLE, and PAYABLE -- you'll have to create opening
# balances for them manually; so they are not included in the set for
# opening balances
ACCOUNT_TYPES_TO_OPEN = ACCOUNT_TYPES_TO_OPEN.difference(set((
    AcctType.ACCT_TYPE_STOCK,
    AcctType.ACCT_TYPE_MUTUAL,
    AcctType.ACCT_TYPE_RECEIVABLE,
    AcctType.ACCT_TYPE_PAYABLE,
)))

# this script isn't capable of properly setting up the transactions for
# ACCT_TYPE_TRADING, you'll have to create opening balances for them manually;
# so, they are not included in the set of accounts used for opening balances
ACCOUNT_TYPES_TO_OPEN.remove(AcctType.ACCT_TYPE_TRADING)

OPENING_BALANCE_ACCOUNT = ('Equity', 'Opening Balances')

# if possible, this program will try to use the account above for the
# currency listed below, and a variation of the above
# Equity->"Opening Balances Symbol" for all other currencies
PREFERED_CURRENCY_FOR_SIMPLE_OPENING_BALANCE = ("CURRENCY", "CAD")


def printify_transaction(trans: Transaction) -> str:
    """Create a nice string from a transaction.
    """
    lines = []
    for split in trans.GetSplitList():
        acct = split.GetAccount()
        lines.append(f"{float(split.GetValue()): >10.2f}  |  {acct.get_full_name()}")

    result = ("\n".join(lines) + "\n" + "-" * (10+5+30) + "\n"
              + f"Imbalance: {float(trans.GetImbalanceValue()):.2f}")
    # from IPython import embed
    # embed()
    return result


def add_split_to_transaction(book, value, account, trans):
    split = Split(book)
    split.SetValue(value)
    split.SetAccount(account)
    split.SetParent(trans)
    return split


def get_account_recursively(book: Book, account_names: list[str], default_type: AcctType,
                            default_commodity: tuple[str, str],
                            base_account: Optional[Account] = None) -> Account:
    """Get the account at the given path, creating accounts as necessary.

Note that this function assumes that there are no name collisions, it always uses the first
candidate with a matching name.
    """
    if not base_account:
        base_account = book.get_root_account()
    if not account_names:
        return base_account
    account_names = list(account_names)
    assert account_names
    name = account_names.pop(0)

    # Find or create the next account.
    next_account = None
    for child in base_account.get_children():
        if child.name == name:
            next_account = child
            break
    if next_account is None:
        next_account = Account(book)
        base_account.append_child(next_account)
        next_account.SetName(name)
        next_account.SetType(int(default_type))
        table = book.get_table()
        next_account.SetCommodity(table.lookup(*default_commodity))

    return get_account_recursively(book, account_names, default_type, default_commodity,
                                   base_account=next_account)


def record_opening_balance(original_account, new_account, new_book,
                           opening_balance_per_type_and_curr, commodity_tuple
                           ):
    """Store data in ``opening_balance_per_type_and_curr``.

``opening_balance_per_type_and_curr`` is a nested dict with one entry per type-currency tuple.
Every value is a tuple of a transaction in ``new_book`` and the total balance (a GncNumeric) of that
transaction.

    """
    acct_type = AcctType(new_account.GetType())
    # create an opening balance if the account type is right
    if acct_type in ACCOUNT_TYPES_TO_OPEN:
        final_balance = original_account.GetBalance()
        if final_balance.num() != 0:
            # if there is a new currency type, associate with the currency
            # a Transaction which will be the opening transaction for that
            # currency and a GncNumeric value which will be the opening
            # balance account amount
            if acct_type in opening_balance_per_type_and_curr:
                opening_balance_per_currency = opening_balance_per_type_and_curr[acct_type]
            else:
                opening_balance_per_currency = opening_balance_per_type_and_curr["*"]

            # opening_balance_per_currency = opening_balance_per_type_and_curr
            if commodity_tuple not in opening_balance_per_currency:
                trans = Transaction(new_book)
                trans.BeginEdit()
                opening_balance_per_currency[commodity_tuple] = (
                    trans, GncNumeric(0, 1))
            trans, total = opening_balance_per_currency[commodity_tuple]

            new_total = total.sub(final_balance, GNC_DENOM_AUTO, GNC_HOW_DENOM_EXACT)

            add_split_to_transaction(new_book, final_balance, new_account, trans)
            opening_balance_per_currency[commodity_tuple] = (trans, new_total)


def build_balanced_account_tree(original_book: Book,
                                target_book: Book,
                                opening_balance_accounts: dict,
                                ) -> dict[AcctType, dict[tuple, tuple]]:
    """
Duplicate account tree to target book and return transactions for the opening balances.

Parameters
----------
original_book : Book
  The original book, this will be used read-only.
target_book : Book
  Book where the (empty) accounts are written into.
opening_balance_accounts : dict
  Accounts where final balances for specific account types shall be written into.

Returns
-------
opening_balances : dict[AcctType, dict[tuple, tuple]]
  Nested dict with the opening balance transactions.  The keys are
  - Account type
  - Currency tuple
  and the values are tuples with the transaction itself and the imbalance value of the transaction.
    """
    # Initialize opening balance dict
    opening_balance_per_type_and_curr: dict = {
        AcctType.from_shortname(typename): {} for typename in opening_balance_accounts
    }
    opening_balance_per_type_and_curr["*"] = {}

    recursively_build_account_tree(
        original_book.get_root_account(),
        target_book.get_root_account(),
        target_book,
        target_book.get_table(),
        opening_balance_per_type_and_curr,
        account_types_to_open=ACCOUNT_TYPES_TO_OPEN
    )
    for a_type, opening_balance_per_currency in opening_balance_per_type_and_curr.items():
        name = str(a_type)
        if isinstance(a_type, AcctType):
            name = a_type.name
        print(f"\n===============\n{name}\n===============")
        for (ns, mnemonic), (trans, balance) in opening_balance_per_currency.items():
            print(f"== {mnemonic} ==")
            print(f"{float(balance)}")
            if float(balance) and a_type != "*" and a_type.shortname() in opening_balance_accounts:
                bal_acct_names = opening_balance_accounts.get(a_type.shortname()).split(":")
                balance_account = get_account_recursively(target_book, bal_acct_names,
                                                          default_type=a_type,
                                                          default_commodity=(ns, mnemonic))
                add_split_to_transaction(target_book, balance, balance_account, trans)
                balance.get_instance().num = 0

            print(printify_transaction(trans))

    return opening_balance_per_type_and_curr


def recursively_build_account_tree(original_parent_account,
                                   new_parent_account,
                                   new_book,
                                   new_commodity_table,
                                   opening_balance_per_type_and_curr,
                                   account_types_to_open):
    """Create duplicate account tree in new book and store all account values.

This function takes ``original_parent_account`` and recursively creates duplicates of all children
in ``new_book``, in ``new_parent_account``. At the same time the account's amount is stored as a
split in the appropriate transaction in the nested dict ``opening_balance_per_type_and_curr``.
    """

    for child in original_parent_account.get_children():
        original_account = child
        new_account = Account(new_book)
        # attach new account to its parent
        new_parent_account.append_child(new_account)

        # copy simple attributes
        for attribute in ('Name', 'Type', 'Description', 'Notes',
                          'Code', 'Color', 'TaxRelated', 'Placeholder'):
            # new_account.SetAttribute( original_account.GetAttribute() )
            getattr(new_account, 'Set' + attribute)(
                getattr(original_account, 'Get' + attribute)())

        # copy commodity
        orig_commodity = original_account.GetCommodity()
        namespace = orig_commodity.get_namespace()
        mnemonic = orig_commodity.get_mnemonic()
        new_commodity = new_commodity_table.lookup(namespace, mnemonic)
        if new_commodity is None:
            new_commodity = orig_commodity.clone(new_book)
            new_commodity_table.insert(new_commodity)
        new_account.SetCommodity(new_commodity)

        record_opening_balance(original_account, new_account,
                               new_book, opening_balance_per_type_and_curr,
                               (namespace, mnemonic),
                               )

        recursively_build_account_tree(original_account,
                                       new_account,
                                       new_book,
                                       new_commodity_table,
                                       opening_balance_per_type_and_curr,
                                       account_types_to_open=account_types_to_open)


def reconstruct_account_name_with_mnemonic(account_tuple, mnemonic):
    """Append the mnemonic to the last account tuple component and return it (as list)."""
    account_pieces = list(account_tuple)
    account_pieces[len(account_pieces) - 1] += " - " + mnemonic
    return account_pieces


def find_or_make_account(account_tuple, parent_account, book,
                         currency) -> Optional[Account]:
    """Return the account at ``account_tuple``.  Create first recursively, if necessary.

Returns
-------

out : Union[Account, None]
    The retrieved or created account, if the currency/commodity is correct.  If there is a currency
    mismatch, return None instead.
    """
    current_account_name, account_path = account_tuple[0], account_tuple[1:]
    current_account = parent_account.lookup_by_name(current_account_name)
    if current_account == None:
        current_account = Account(book)
        current_account.SetName(current_account_name)
        current_account.SetCommodity(currency)
        parent_account.append_child(current_account)

    if len(account_path) > 0:
        return find_or_make_account(account_path, current_account, book,
                                    currency)
    else:
        account_commod = current_account.GetCommodity()
        if (account_commod.get_mnemonic(),
            account_commod.get_namespace()) == (
                currency.get_mnemonic(),
                currency.get_namespace()):
            return current_account
        else:
            return None


def choke_on_none_for_no_account(opening_account, extra_string):
    """Raise an exception if ``opening_account`` is None.

The raised exception has ``extra_string`` as a suffix.
    """
    if opening_account == None:
        raise Exception("account currency and name mismatch, " + extra_string)


def apply_opening_balance_transaction(commodtable, namespace, mnemonic,
                                      new_book_root, new_book,
                                      opening_trans, opening_amount,
                                      is_main_currency: bool) -> None:
    """Put the opening balance into an account in the new book.

If ``is_main_currency`` is True, the accounts are used as normal.  Otherwise, the currency mnemonic
is appended to the last account component.
    """
    currency = commodtable.lookup(namespace, mnemonic)
    assert (currency.get_instance() is not None)

    mnemonic_account_pieces = reconstruct_account_name_with_mnemonic(
        OPENING_BALANCE_ACCOUNT, mnemonic)
    # from IPython import embed
    # embed()

    if is_main_currency:
        opening_account = find_or_make_account(OPENING_BALANCE_ACCOUNT,
                                               new_book_root, new_book, currency)
        if opening_account is None:
            opening_account = find_or_make_account(mnemonic_account_pieces,
                                                   new_book_root, new_book, currency)
    else:
        opening_account = find_or_make_account(mnemonic_account_pieces,
                                               new_book_root, new_book, currency)
    choke_on_none_for_no_account(opening_account, ', '.join(mnemonic_account_pieces))

    # we don't need to use the opening balance account at all if all
    # the accounts being given an opening balance balance out
    if opening_amount.num() != 0:
        add_split_to_transaction(new_book, opening_amount, opening_account, opening_trans)

    opening_trans.SetDate(*OPENING_DATE)
    opening_trans.SetCurrency(currency)
    opening_trans.SetDescription("Opening Balance")
    opening_trans.CommitEdit()


def get_main_currency(balances: dict) -> GncCommodity:
    """Get the main currency from the balances.

For the time being, this is simply the first one to be found.

Parameters
----------
balances : dict
    A dict of balances.  Balance types are the keys.
    """
    for balance in balances.values():
        if not balance:
            continue
        keys = list(balance.keys())
        for key in keys:
            if key[0] == "CURRENCY":
                return key
    return None


def duplicate_business(old: gnucash.Book, target: gnucash.Book) -> None:
    """Duplicate all customers, vendors etc.


This is done from the ``old`` book to the ``target`` book.

Parameters
----------
old: gnucash.Book
    The original book from where the business entries shall be entered.

target: gnucash.Book
    The target book for the entries.
    """

    for cls in (
            business.Vendor,
            business.Customer,
            business.Employee,
    ):
        all_of_them = cls.get_all(book=old)
        for entity in all_of_them:
            entity.clone_to(other=target)


def duplicate_with_opening_balance(old: str, target: str,
                                   balance_accounts: Optional[dict] = None) -> None:
    """Create a target Gnucash file.

The preferred (or first, if there is no preferred one or that does not match) currency will go
straight into the opening accounts, all other currencies will have their account names suffixed.

Parameters
----------

balance_accounts: dict, optional
    The accounts where opening transactions of some types shall be booked against.

    """
    target = os.path.abspath(target)
    target_sqlite = "sqlite3://" + target

    # Create target if it does not exist yet.
    if not os.path.exists(target):
        target_book_session = Session(target_sqlite, SessionOpenMode.SESSION_NEW_STORE)
        # new_book = new_book_session.get_book()
        # new_book_session.save()
        target_book_session.end()
    else:
        print(f"Warning! File {target} exists already.")

    with (Session(old, SessionOpenMode.SESSION_READ_ONLY) as original_book_session,
          Session(target_sqlite, SessionOpenMode.SESSION_NORMAL_OPEN) as target_book_session):
        # we discovered that if we didn't have this save early on, there would
        # be trouble later
        target_book_session.save()
        target_book = target_book_session.get_book()

        #######################
        # Make dict of balances
        #######################
        opening_balance_per_type_and_curr = build_balanced_account_tree(
            original_book=original_book_session.get_book(),
            target_book=target_book,
            opening_balance_accounts=balance_accounts,
        )

        main_currency = get_main_currency(opening_balance_per_type_and_curr)
        target_book_root = target_book.get_root_account()
        commodtable = target_book.get_table()
        for opening_balance_per_currency in opening_balance_per_type_and_curr.values():
            if not opening_balance_per_currency:
                continue
            if main_currency in opening_balance_per_currency:
                opening_trans, opening_amount = opening_balance_per_currency[main_currency]
                apply_opening_balance_transaction(
                    commodtable, *main_currency,
                    target_book_root, target_book,
                    opening_trans, opening_amount,
                    is_main_currency=True)
                del opening_balance_per_currency[main_currency]

            old_book = original_book_session.get_book()

            for (namespace, mnemonic), (opening_trans, opening_amount) in (
                    opening_balance_per_currency.items()):
                apply_opening_balance_transaction(
                    commodtable, namespace, mnemonic,
                    target_book_root, target_book,
                    opening_trans, opening_amount,
                    is_main_currency=False)

        duplicate_business(old=old_book, target=target_book)


def _parse_arguments():
    """Parse the command line."""
    required = {}
    parser = argparse.ArgumentParser(description="Create a new Gnucash file from the account"
                                     " balances of an existing file.")
    conf_args = {"help": "Config file which for additional options."}
    parser.add_argument('-c', '--conf', **conf_args)
    required["infile"] = parser.add_argument("-i", "--infile",
                                             help="Input file (of the old year / booking period).",
                                             type=str, required=True)
    required["outfile"] = parser.add_argument('-o', '--outfile',
                                              help="Filename where the duplicate shall be written.",
                                              type=str, required=True)
    parser.add_argument('--target-asset',
                        help="Where to book the assets.  For example: 'Opening:Assets'")
    parser.add_argument('--target-liability',
                        help="Where to book the liabilities.  For example: 'Opening:Liabilities'")

    # parse --conf first, but ignore any problems on the way
    class SimpleParser(argparse.ArgumentParser):
        def error(self, message):
            raise NotImplementedError(message)
        def print_usage(self, file=None):
            self.error("print_usage")
        def print_help(self, file=None):
            self.error("print_help")

    conf_parser = SimpleParser()
    conf_parser.add_argument("-c", "--conf", **conf_args)
    try:
        parsed = conf_parser.parse_known_args()[0]
    except NotImplementedError as nie:
        parsed = None

    if parsed and parsed.conf:
        config = configparser.ConfigParser()
        with open(parsed.conf, encoding="utf-8") as conf_file:
            config.read_file(conf_file)
        parser.set_defaults(**config["DEFAULT"])
        for name, arg in required.items():
            if name in config["DEFAULT"]:
                arg.required = False

    parsed = parser.parse_args()

    return parsed


def main():

    args = _parse_arguments()
    target_accounts = {
        "asset": args.target_asset,
        "liability": args.target_liability
    }
    duplicate_with_opening_balance(old=args.infile, target=args.outfile,
                                   balance_accounts=target_accounts)


if __name__ == "__main__":
    main()
