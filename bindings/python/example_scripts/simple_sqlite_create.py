#!/usr/bin/env python3
##  @file
#   @brief Example Script simple sqlite create 
#   @ingroup python_bindings_examples

import os
from tempfile import TemporaryDirectory

from gnucash import Session, Account, SessionOpenMode
from os.path import abspath
from gnucash.gnucash_core_c import ACCT_TYPE_ASSET


def create_new_empty_file(delete=True):
    """Create file "new_empty.gnucash" in temporary directory.

    Parameters
    ----------
    delete: boolean, default=True
      If False, do not delete temporary file
    """

    with TemporaryDirectory(delete=delete) as tmpdir:
        targetfile = f"sqlite3://{tmpdir}/new_empty.gnucash"

        with Session(targetfile, SessionOpenMode.SESSION_NEW_STORE) as session:
            session.book.get_root_account()  # Seems necessary to trigger creation of tables.
        print(f"Gnucash file {targetfile} created.")

        with Session(targetfile, SessionOpenMode.SESSION_NORMAL_OPEN) as session:
            print("Opened again.")
        print("Closed again.")


def create_new_file_with_account(delete=True):
    """Create file "new_with_account.gnucash" in temporary directory.

    Parameters
    ----------
    delete: boolean, default=True
      If False, do not delete temporary file
    """
    with TemporaryDirectory(delete=delete) as tmpdir:

        targetfile = f"sqlite3://{tmpdir}/new_with_account.gnucash"
        print(f"Creating: {targetfile}")
        s = Session(targetfile, SessionOpenMode.SESSION_NEW_STORE)
        # The save() seems to make a difference in more complex cases.
        s.save()

        book = s.book
        root = book.get_root_account()
        a = Account(book)
        root.append_child(a)
        a.SetName('wow')
        a.SetType(ACCT_TYPE_ASSET)

        commod_table = book.get_table()
        a.SetCommodity( commod_table.lookup('CURRENCY', 'CAD') )
        s.save()

        s.end()


create_new_empty_file()

create_new_file_with_account()
