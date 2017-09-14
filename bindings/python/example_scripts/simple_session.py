#!/usr/bin/env python
##  @file
#   @brief Example Script simple session
#   @ingroup python_bindings_examples

from gnucash import \
     Session, GnuCashBackendException, \
     ERR_BACKEND_LOCKED, ERR_FILEIO_FILE_NOT_FOUND

FILE_1 = "/tmp/not_there.xac"
FILE_2 = "/tmp/example_file.xac"

# open a file that isn't there, detect the error
session = None
try:
    session = Session(FILE_1)
except GnuCashBackendException, backend_exception:
    assert( ERR_FILEIO_FILE_NOT_FOUND in backend_exception.errors)


# create a new file, this requires a file type specification
session = Session("xml://%s" % FILE_2, is_new=True)
session.save()
session.end()
session.destroy()

# open the new file, try to open it a second time, detect the lock
session = Session(FILE_2)
try:
    session_2 = Session(FILE_2)
except GnuCashBackendException, backend_exception:
    assert( ERR_BACKEND_LOCKED in backend_exception.errors )
session.end()
session.destroy()

    
