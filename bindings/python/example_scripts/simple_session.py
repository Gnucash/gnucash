#!/usr/bin/env python3
##  @file
#   @brief Example Script simple session
#   @ingroup python_bindings_examples

from gnucash import (
     Session, GnuCashBackendException,
     SessionOpenMode,
     ERR_BACKEND_LOCKED, ERR_FILEIO_FILE_NOT_FOUND
)

FILE_1 = "/tmp/not_there.xac"
FILE_2 = "/tmp/example_file.xac"

# open a file that isn't there, detect the error
session = None
try:
    session = Session(FILE_1)
except GnuCashBackendException as backend_exception:
    assert( ERR_FILEIO_FILE_NOT_FOUND in backend_exception.errors)


# create a new file, this requires a file type specification
with Session("xml://%s" % FILE_2, SessionOpenMode.SESSION_NEW_STORE) as session:
    book = session.book
    root = book.get_root_account()

# open the new file, try to open it a second time, detect the lock
with Session(FILE_2) as session:
    try:
        session_2 = Session(FILE_2)
    except GnuCashBackendException as backend_exception:
        assert( ERR_BACKEND_LOCKED in backend_exception.errors )
